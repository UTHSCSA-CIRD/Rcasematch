

pdist <- function(xx,yy,result="pdist_out",env=.GlobalEnv,method='binary',prog=T,...){
  # xx :    the name of a data.frame in .GlobalEnv with PATIENT_ID 
  #         in first column and a concept code in the second, and 
  #         the second column is a factor. Values of both columns 
  #         can repeat. Left side of comparison
  # yy :    like xx, but the right side of a comparison
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
  lidxx<-length(idxx <- unique(env[[xx]][,1]));
  lidyy<-length(idyy <- unique(env[[yy]][,1]));
  # number of pairwise combos
  lidxxyy<-lidxx*lidyy;
  # initialize result matrix such that rows = number of non
  # redundant pairwise combinations of IDs, and save that 
  # number while we're at it
  env[[result]] <- matrix(0,nrow=lidxxyy,ncol=3,dimnames=list(c(),c('p1','p2','dist')));
  kk <- 1;
  if(prog) pbar <- txtProgressBar(max=lidxxyy,style=3);
  # calculate distances
  for(ii in idxx) {
    # This, and the subsequent jjsumm are vectors with the same length
    # as the number of distinct values in env[[xx]][,2] (i.e. the 
    # number of levels in that factor). When you take a summary of a
    # factor you get the number of occurrences for each level, with
    # 0s for the levels that do not occur. This input is compatible
    # with dist (if made into a matrix, by rbind())
    iisumm <- summary(env[[xx]][ii==env[[xx]][,1],2]);
    for(jj in idyy) {
      if(ii==jj) next;
      env[[result]][kk,] <- c(ii,jj,dist(
        rbind(iisumm,summary(env[[yy]][jj==env[[yy]][,1],2]))
        ,method=method));
      kk <- kk + 1;
    }
    if(prog) setTxtProgressBar(pbar,kk);
  }
  if(prog) close(pbar);
  print(paste("Pairwise distances saved to ",result));
}

# This function can be used with binrows() as follows...
# Let's say you have a bunch of IDs and codes from a pool of potential 
# controls called controldat, and another data.frame expdat in the same 
# format with your experimental cohort. Let's say there are 100 unique 
# IDs in expdat and 200 unique IDs in controldat. You want to run the 
# distance calculations in chunks less than or equal to 4000 results 
# each. 100*200 = 20000 > 4000. So, you send both ID vectors to binrows()
#   expbins <- binrows(expdat[,1],controldat[,1],4000);
#   sapply(expbins,length);
#
# Each of the vectors in the expbins list has a length of 20 or less. If
# these are row-sets of the original 100*200 matrix, each is now <= 20*200
# = 4000 in size. Now you can do something like... 
#
#  for(ii in seq_along(expbins)){
#     expchunk<-expdat[expdat[,1]%in%expbins[[ii]],];
#     result<-paste0("res_",ii);
#    pdist("expchunk","controldat",result);
#   }
