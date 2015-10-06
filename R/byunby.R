byunby <- function(data,indices,FUN,...){
  # all arguments passed to by() and mean the same thing as 
  # by()'s corresponding arguments
  dsplit <- by(data,indices,FUN,...,simplify=F);
  do.call(rbind,dsplit);
}
