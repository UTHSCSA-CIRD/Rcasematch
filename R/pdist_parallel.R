pdistPara <- function(xx,yy=NULL,cores = 4,memMaxMB = 2024, fileMaxGB = 20, method='binary',...){
  #xx- a data.frame containing 2 columns column 1- ID column 2- factor
  #yy - optional- a second data.frame to compare X to. If not given x is compared to itself
  #cores- the number of cores to use. 
    ##Note: hyperthreading is not utlized by foreach.
  #memMaxMB - this is the maximum memory to be consumed by the DATA SET ITSELF
    ##Note: R will likely use up to 2-3 times this amount when combining the results from foreach.
  #fileMaxGB - the maximum size in GB for the file created by ff. If the file will be greater than that the function stops.
  #method- the method used by dist to calculate the distance between two patients. 
  
  #verify and load required packages
  if(!require(doParallel)){stop("The doParallel package required.")}
  if(!require(iterators)){stop("The iterator package is required.")}
  
  if(is.null(yy)){
    #call submethod .identpara if comparing xx to itself. 
    .identpara(xx, cores,memMaxMB,fileMaxGB, method)
  }else{
    lidxx<-length(idxx <- unique(xx[,1]));
    lidyy<-length(idyy <- unique(yy[,1]));
    # number of pairwise combos
    lidxxyy<-lidxx*lidyy;
    if(fileMaxGB < (lidxxyy*8)/2^30)stop("Resulting file would exceed file maximum size.")
    sumKB = object.size(summary(yy[1,2]))
    #Rounding to double for each value to account for R's slightly larger file size.
    if(lidxxyy >= .Machine$integer.max-1){stop("Resulting vectore greater than integer max not supported for xx by yy")}
    if(memMaxMB < ((lidxxyy*8)/2^20)){
      #throws a warning so that the user knows their return value is a ffdf not a matrix or data.frame
      warning("Memory usage calculated to be greater than max.")
      .highMem(xx,yy,cores,memMaxMB, method)

    }else{
    #make and register cluster for number of cores
    cl = makeCluster(cores)
    registerDoParallel(cl)
    #cache yy
    x = foreach(jj = 1:lidyy) %dopar%{
      summary(yy[idyy[jj]==yy[,1],2])
    }
    # run foreach in parallel- this converts the for loop into an mcapply
    ret = foreach(ii =1:lidxx, .combine = 'c') %dopar%{
      # Setup a matrix to return to foreach after each iteration. 
      res = vector(mode = "numeric", length = lidyy);
      #cache the summary of xx to be used this cycle. 
      iisumm <- summary(xx[idxx[ii]==xx[,1],2]);
      for(jj in 1:lidyy) {
        res[jj] <- dist(rbind(iisumm, x[[jj]]), method=method);
      }
      #remove the 0s (the skip if equal)
      res
    }
    stopCluster(cl)
    invisible(ret)
    }#end not too big for memory
  }#end not identical
}


#################################### HIDDEN ADDITIONAL METHODS USED BY pdistPara ##########################

.identpara = function(xx,cores = 4,memMaxMB = 2024,fileMaxGB = 20, method='binary',...){
  #if xx == yy -- this saves on confusion and reduces the number of if calls that will reduce overall performance 
  lidxx<-length(idxx <- unique(xx[,1]));
  #the number of records produced (n*(n-1)*.5)
  lidxxyy = .5*(lidxx-1)*lidxx
  if(lidxxyy >= .Machine$integer.max-1){stop("MultiFile version not yet supported")}
  if(fileMaxGB < (lidxxyy*8)/2^30)stop("Resulting file would exceed file maximum size.")
  #Rounding to double for each value to account for R's slightly larger file size.
  if(memMaxMB < ((lidxxyy*8)/2^20)){
    #throws a warning so that the user knows their return value is a ffdf not a matrix or data.frame
    warning("Memory usage calculated to be greater than max.")
    .highMemIdent(xx,cores,memMaxMB,method)

  }else{
  #start and register the clusters for the number of cores
  cl = makeCluster(cores)
  registerDoParallel(cl)
  
  #cache the summaries
  x <- foreach(jj = 1:lidxx) %dopar%{
    summary(xx[idxx[jj]==xx[,1],2])
  }
  
  #run foreach in parallel lidxx-1 because the last row would be lidxx*lidxx which won't run anyway
  ret <- foreach(ii =1:(lidxx-1), .combine = 'c') %dopar%{
    jj = ii+1
    kk = 1 #keeps track of the position in the matrix
    #matrix is the size of lidxx - the current record (ii)
    res = vector(mode = "numeric", length =(lidxx-ii))
    #While jj <= lidxx run through each one
    while (jj <= lidxx){
      res[kk] <- dist(rbind(x[[ii]], x[[jj]]), method=method)
      jj = jj+1
      kk=kk+1
    }
    res
  }
  stopCluster(cl)
  invisible(ret)
  }#end not too big for memory. 
}


#################################### HIGH MEMORY METHODS ####################################################


.highMemIdent = function(xx,cores = 4,memMaxMB = 2024, method='binary',...){
  if(!require(ff))stop("ff package required for 'too large for memory' data sets")
  lidxx<-length(idxx <- unique(xx[,1]));
  lidxxyy = .5*(lidxx-1)*lidxx
  
  #estimate how many runs we can store before reaching memMaxMB and needing to add it to the file.
  #7000 approx size of summary -- summary is cached for each record in xx
  ###floor((memMax - cost of caching) / cost to store one row)
  stoSize = floor((memMaxMB-(lidxx*1000)/2^20)/(((lidxx*8)/2^20)))
  if(stoSize<1){
    stop("Cannot fit one length of xx in memMax.")
  }
  #start up clusters
  cl = makeCluster(cores)
  registerDoParallel(cl)
  
  #cache
  x <- foreach(jj = 1:lidxx) %dopar%{
    summary(xx[idxx[jj]==xx[,1],2])
  }
  
  #setup data split and ffdf
  itter <- 1 #start
  itterTo = stoSize #stop
  placeHolder = 1 #holds place in ptdf
  #ff -file name is created with the date and time and .ffdat
  ptdist = ff(vmode="double",length =lidxxyy, filename = paste0("ptdist", format(Sys.time(),"%Y-%b-%d--%H-%M"), ".ffdat"), finonexit = F)
  #this can be reopened with open.ffdf(ptdf) instead of having to open each ff. Also makes it easier to write to all 3 files at once.
  #split 
  while(itter<lidxx){
    #refresh ret at the begining of each while loop
    ret = NULL
    #run foreach in parallel from start to stop (itter to itterTo)
    ret<- foreach(ii=itter:itterTo, .combine = 'c') %dopar%{
      jj = ii+1
      kk = 1
      
      res = vector(mode = "numeric", length = (lidxx-ii))
      while (jj <= lidxx){
        res[kk] <- dist(rbind(x[[ii]], x[[jj]]), method=method)
        jj = jj+1
        kk=kk+1
      }
      res
    }
    #fill in ptdf from placeHolder to placeHolder + size of ret - 1 
    ptdist[placeHolder:(placeHolder+length(ret)-1),] = ret
    #increment placeHolder by size of ret
    placeHolder = placeHolder+length(ret)
    #increment itter and itterTo
    itter = itterTo+1
    itterTo = itter +stoSize
    #if itterTo is past the end of the list, set it to lidxx-1
    if(itterTo>lidxx) itterTo = lidxx-1
  }
  stopCluster(cl)
  invisible(ptdist)
}

.highMem = function(xx, yy,cores = 4,memMaxMB = 2024, method='binary',...){
  if(!require(ff))stop("ff package required for 'too large for memory' data sets")
  
  lidxx<-length(idxx <- unique(xx[,1]));
  lidyy<-length(idyy <- unique(yy[,1]));
  # number of pairwise combos
  lidxxyy<-lidxx*lidyy;
  #estimate how many runs we can store before reaching memMaxMB and needing to add it to the file.
  #7000 approx size of summary -- summary is cached for each record in yy 
  #floor((memMax - cost of caching) / cost to store one row)
  stoSize = floor((memMaxMB-(lidyy*1000)/2^20)/(((lidyy*8)/2^20)))
  if(stoSize<1){
    stop("Cannot fit one length of yy in memMax.")
  }
  #start up clusterr and register it 
  cl = makeCluster(cores)
  registerDoParallel(cl)
  #cache yy summaries
  x = foreach(jj = 1:lidyy) %dopar%{
    summary(yy[idyy[jj]==yy[,1],2])
  }
  
  #setup data split and ffdf
  itter <- 1 #start
  itterTo = stoSize #stop
  placeHolder = 1 #place holder in ptdf
  ptdist = ff(vmode="double",length =lidxxyy, filename = paste0("ptdist", format(Sys.time(),"%Y-%b-%d--%H-%M"), ".ffdat"), finonexit = F)
  #split 
  while(itter<=lidxx){
    ret = NULL
    ret = foreach(ii =itter:itterTo, .combine = 'c') %dopar%{
      
      res = vector(mode = "numeric", length =lidyy)
      iisumm <- summary(xx[idxx[ii]==xx[,1],2]);
      for(jj in 1:lidyy) {
        res[jj] <- dist(rbind(iisumm, x[[jj]]), method=method);
      }
      #remove zeros produced by xx == yy NOTE- there will be zeros at the end of ptdf. 
      res
    }
    #fill in ptdist
    ptdist[placeHolder:(placeHolder+length(ret)-1)] = ret
    #increment placeHolder
    placeHolder = placeHolder + length(ret)
    #increment 
    itter = itterTo+1
    itterTo = itter+stoSize
    if(itterTo>lidxx) itterTo = lidxx
  }
  stopCluster(cl)
  invisible(ptdist)
}
