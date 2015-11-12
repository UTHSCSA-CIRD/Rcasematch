binrows <- function(rows,cols,maxcombos=40000,...){
  # rows :      vector of unique values that index rows
  # cols :      vector of unique values that index columns
  # maxcombos : largest number of pairwise combinations 
  #             of rows and columns that we want to allow
  
  # returns a list of sub-vectors of rows, each of which is
  # of a length that multiplied by columns will now be below
  # the limit set by maxcombos
  lc<-length(cols); if(lc!=length(unique(cols))){
    cols <- unique(cols); lc<-length(cols);
    warning('Discarded duplicate values in cols');
  }
  lr<-length(rows); if(lr!=length(unique(rows))){
    rows <- unique(rows); lc<-length(rows);
    warning('Discarded duplicate values in rows');
  }
  split(rows,sort(rep(1:ceiling(lr*lc/maxcombos),len=lr)));
}
