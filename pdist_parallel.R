pdistPara <- function(xx,yy=NULL,cores = 4,memMaxMB = 2024, fileMaxGB = 100, result="pdist_out",env=.GlobalEnv,method='binary',...){
  if(!require(doParallel)){stop("The doParallel package required.")}
  if(!require(iterators)){stop("The iterator package is required.")}
  if(is.null(yy)){
    .identpara(xx, cores,memMaxMB, result, env, method)
  }else{
    lidxx<-length(idxx <- unique(xx[,1]));
    lidyy<-length(idyy <- unique(yy[,1]));
    # number of pairwise combos
    lidxxyy<-lidxx*lidyy;
    if(fileMaxGB < (lidxxyy*2*8)/2^30)stop("Resulting file would exceed file maximum size.")
    #max mem, cut in half, expanded to KB, divided by 4 
    if(memMaxMB < ((lidxxyy*3*8)/2^20)){
      warning("Memory usage calculated to be greater than max.")
      .highMem(xx,yy,memMaxMB,result,env,method)
      invisible()
    }
    cl = makeCluster(cores)
    registerDoParallel(cl)

    x = foreach(jj = 1:lidyy) %dopar%{
      summary(yy[idyy[jj]==yy[,1],2])
    }
    # calculate distances
    env[[result]] = foreach(ii =1:lidxx, .combine = 'rbind') %dopar%{
      # This, and the subsequent jjsumm are vectors with the same length
      # as the number of distiInct values in env[[xx]][,2] (i.e. the 
      # number of levels in that factor). When you take a summary of a
      # factor you get the number of occurrences for each level, with
      # 0s for the levels that do not occur. This input is compatible
      # with dist (if made into a matrix, by rbind())
      res = matrix(0,nrow=lidyy,ncol=3,dimnames=list(c(),c('p1','p2','dist')));
      iisumm <- summary(xx[idxx[ii]==xx[,1],2]);
      for(jj in 1:lidyy) {
        if(idxx[ii]==idyy[jj]) next;
        res[jj,] <- c(idxx[ii], idyy[jj], dist(rbind(iisumm, x[[jj]]), method=method));
      }
      res
    }
    stopCluster(cl)
    env[[result]]  = env[[result]][env[[result]][,1]!=0,]
    gc()
  }#end not identical
}


###########################################################################################################
#################################### HIDDEN ADDITIONAL METHODS USED BY pdistPara ##########################
###########################################################################################################

.identpara = function(xx,cores = 4,memMaxMB = 2024,fileMaxGB = 100, result="pdist_out",env=.GlobalEnv,method='binary',...){
  #if xx == yy -- this saves on confusion and reduces the number of if calls that will reduce overall performance 
  lidxx<-length(idxx <- unique(xx[,1]));
  lidxxyy = .5*(lidxx-1)*lidxx
  if(fileMaxGB < (lidxxyy*2*8)/2^30)stop("Resulting file would exceed file maximum size.")
  #max mem, cut in half, expanded to KB, divided by 4 
  if(memMaxMB < ((lidxxyy*3*8)/2^20)){
    warning("Memory usage calculated to be greater than max.")
    .highMemIdent(xx,memMaxMB,result,env,method)
    invisible()
  }
  
  cl = makeCluster(cores)
  registerDoParallel(cl)
  x <- foreach(jj = 1:lidxx) %dopar%{
    summary(xx[idxx[jj]==xx[,1],2])
  }

  env[[result]] <- foreach(ii =1:(lidxx-1), .combine = 'rbind') %dopar%{
    jj = ii+1
    kk = 1
    
    res = matrix(0,nrow=(lidxx-jj),ncol=3,dimnames=list(c(),c('p1','p2','dist')));
    while (jj < lidxx){
      res[kk,] <- c(idxx[ii], idxx[jj], dist(rbind(x[[ii]], x[[jj]]), method=method));
      jj = jj+1
      kk=kk+1
    }
    res
  }
  stopCluster(cl)
  gc()
}





#############################################################################################################
#################################### HIGH MEMORY METHODS ####################################################
#############################################################################################################


.highMemIdent = function(xx,cores = 4,memMaxMB = 2024, result="pdist_out",env=.GlobalEnv,method='binary',...){
  if(!require(ffbase))stop("ffbase package required for 'too large for memory' data sets")
  if(!require(ff))stop("ff package required for 'too large for memory' data sets")
  
  lidxx<-length(idxx <- unique(xx[,1]));
  
  #estimate how many runs we can store before reaching memMaxMB and needing to add it to the file.
  #848- size of summary -- summary is cached for each record in xx
  ###memMax - cost of caching
  ###new mem divided by lidxx
  stoSize = floor((memMaxMB-(lidxx*848)/2^20)/(((lidxx*3*8)/2^20)))
  if(stoSize<1){
    stop("Cannot fit one length of xx in memMax.")
  }
  
  
  cl = makeCluster(cores)
  registerDoParallel(cl)
  x <- foreach(jj = 1:lidxx) %dopar%{
    summary(xx[idxx[jj]==xx[,1],2])
  }
  
  #setup data split and ffdf
  itter <- 1
  itterTo = itter+stoSize
  
  
  #split 
  while(itter<=lidxx){
    ret = NULL
    ret<- foreach(ii =itter:itterTo, .combine = 'rbind') %dopar%{
      jj = ii+1
      kk = 1
      
      res = matrix(0,nrow=(lidxx-jj),ncol=3,dimnames=list(c(),c('p1','p2','dist')));
      while (jj < lidxx){
        res[kk,] <- c(idxx[ii], idxx[jj], dist(rbind(x[[ii]], x[[jj]]), method=method));
        jj = jj+1
        kk=kk+1
      }
      res
    }
    if(itter==1){
      #init the files 
      pt1 = ff(ret[,1],filename = paste("pt1", format(Sys.time(),"%Y-%b-%d-%H:%M")))
      pt2 = ff(ret[,2], filename = paste("pt2", format(Sys.time(),"%Y-%b-%d-%H:%M")))
      ptdist = ff(ret[,3], filename = paste("ptdist", format(Sys.time(),"%Y-%b-%d-%H:%M")))
      ptdf = ffdf(pt1,pt2,ptdist)
    }else{
      ffappend(ptdf, ret)
    }

    itter = itterTo+1
    itterTo = itter +stoSize
    if(itterTo>lidxx) itterTo = lidxx
  }
  stopCluster(cl)
  env[[result]]  = ptdf
  gc()
}

.highMem = function(xx, yy,cores = 4,memMaxMB = 2024, result="pdist_out", env =.GlobalEnv,method='binary',...){
  if(!require(ffbase))stop("ffbase package required for 'too large for memory' data sets")
  if(!require(ff))stop("ff package required for 'too large for memory' data sets")
    #this function is used if the memory usage for this function exceeds 50% of the available memory.
  lidxx<-length(idxx <- unique(xx[,1]));
  lidyy<-length(idyy <- unique(yy[,1]));
  # number of pairwise combos
  lidxxyy<-lidxx*lidyy;
  #estimate how many runs we can store before reaching memMaxMB and needing to add it to the file.
  #848- size of summary -- summary is cached for each record in yy 
     ###memMax - cost of caching
     ###new mem divided by lidyy
  stoSize = floor((memMaxMB-(lidyy*848)/2^20)/(((lidyy*3*8)/2^20)))
  if(stoSize<1){
    stop("Cannot fit one length of yy in memMax.")
  }
  cl = makeCluster(cores)
  registerDoParallel(cl)
  
  x = foreach(jj = 1:lidyy) %dopar%{
    summary(yy[idyy[jj]==yy[,1],2])
  }
  
  #setup data split and ffdf
  itter <- 1
  itterTo = itter+stoSize
  
  
  #split 
  while(itter<=lidxx){
    ret = NULL
    ret = foreach(ii =itter:itterTo, .combine = 'rbind') %dopar%{
      # This, and the subsequent jjsumm are vectors with the same length
      # as the number of distiInct values in env[[xx]][,2] (i.e. the 
      # number of levels in that factor). When you take a summary of a
      # factor you get the number of occurrences for each level, with
      # 0s for the levels that do not occur. This input is compatible
      # with dist (if made into a matrix, by rbind())
      res = matrix(0,nrow=lidyy,ncol=3,dimnames=list(c(),c('p1','p2','dist')));
      iisumm <- summary(xx[idxx[ii]==xx[,1],2]);
      for(jj in 1:lidyy) {
        if(idxx[ii]==idyy[jj]) next;
        res[jj,] <- c(idxx[ii], idyy[jj], dist(rbind(iisumm, x[[jj]]), method=method));
      }
      res[res[,1]!=0,]
    }
    if(itter==1){
      #init the files 
      pt1 = ff(ret[,1],filename = paste("pt1", format(Sys.time(),"%Y-%b-%d-%H:%M")))
      pt2 = ff(ret[,2], filename = paste("pt2", format(Sys.time(),"%Y-%b-%d-%H:%M")))
      ptdist = ff(ret[,3], filename = paste("ptdist", format(Sys.time(),"%Y-%b-%d-%H:%M")))
      ptdf = ffdf(pt1,pt2,ptdist)
    }else{
      #
      ffappend(ptdf, ret)
    }
    #itterate
    itter = itterTo+1
    itterTo = itter+stoSize
    if(itterTo>lidxx) itterTo = lidxx
  }
  stopCluster(cl)
  env[[result]]  = ptdf
  gc()
}
