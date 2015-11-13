pdistopt <- function(xx,yy,result="pdist_out",env=.GlobalEnv,method='binary',...){
  # xx :    STRING the name of a data.frame in .GlobalEnv with PATIENT_ID 
  #         in first column and a concept code in the second, and 
  #         the second column is a factor. Values of both columns 
  #         can repeat. Left side of comparison
  # yy :    STRING like xx, but the right side of a comparison
  # result: what to name the result matrix in .GlobalEnv
  #         for now, will get blown away if it already exists
  # env :   in what environment to look for xx and create result
  # prog :  whether to show progress bar
  
  # Deliberately forcing function to take values by reference rather
  # than the default R behavior of taking them by value because of 
  # the anticipated size of these things
  
  # Function takes table of patients and concepts (both may repeat)
  # and returns a table with a distance (in the third column) for
  # every pairwise combination of xx and yy
  
  # get the unique IDs (idxx) and also their length (lidxx)
  # same for yy
  
  
  #higher memory, lower CPU requirments. If memory is a concern, make the second dataframe be the smaller one.
  lidxx<-length(idxx <- unique(env[[xx]][,1]));
  lidyy<-length(idyy <- unique(env[[yy]][,1]));
  # number of pairwise combos
  lidxxyy<-lidxx*lidyy;
  # initialize result matrix such that rows = number of non
  # redundant pairwise combinations of IDs, and save that 
  # number while we're at it
  env[[result]] <- matrix(0,nrow=lidxxyy,ncol=3,dimnames=list(c(),c('p1','p2','dist')));
  kk <- 1;
  #reduce the amount of work done each loop by calling summary outside of the loop
  #and using resulting matrix x -- x[,jj]
  x=NULL
  for(jj in 1:lidyy) {
    x[[jj]] = summary(env[[yy]][idyy[jj]==env[[yy]][,1],2])
  }
  # calculate distances
  for(ii in idxx) {
    # This, and the subsequent jjsumm are vectors with the same length
    # as the number of distiInct values in env[[xx]][,2] (i.e. the 
    # number of levels in that factor). When you take a summary of a
    # factor you get the number of occurrences for each level, with
    # 0s for the levels that do not occur. This input is compatible
    # with dist (if made into a matrix, by rbind())
    iisumm <- summary(env[[xx]][ii==env[[xx]][,1],2]);
    for(jj in 1:lidyy) {
      if(ii==idyy[jj]) next;
      env[[result]][kk,] <- c(ii, idyy[jj], 
                              dist(rbind(iisumm, x[[jj]]), method=method));
      kk <- kk + 1;
    }
  }
}