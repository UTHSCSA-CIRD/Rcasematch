#Patient wrapper contains a set of methods to "wrap" the data set created by pdist_parallel
#for triangular matrixes only 
pWrapGetIndex = function(r,c, d, len){
  #r = row
  #c = column
  #d = data set (this should be either a matrix or an ff)
  #len = the number of patients (width/height of the strictly upper triangular matrix)
  d[(len*2-r)*(r-1)/2 - r + c]
}
pWrapGetTPT = function(ptID, ptList, d){
  if(!require(foreach)){stop("This function utilizes foreach")}
  #ptID = the ID of the patient you wish to obtain the list for
  #ptList = the vector containing patient IDs
  #d = the data set. This should be either a matrix of an ff.
  #assumes the patients were run ascending numerical order.
  ind = which(ptList == ptID)[[1]]
  l = length(ptList)#caching the length 
  indLoc = (l*2-ind)*(ind-1)/2 - ind + (ind+1) #caching the first indicy of the row (ind == diagonal ind+1= next col)
  #initialize vector
  ret = vector(mode="numeric", length = l-1 )
  if(ind<l){
    ret[ind:(l-1)]= d[indLoc:(indLoc+(l-ind-1))]
  }
  if(ind>1){
    a = ind - 1
    rettmp = foreach(i = 1:(ind-1), .combine = 'rbind')%do%{
      x = d[a]
      a = a+l-i-1
      x
    }
    ret[1:ind-1]=rettmp
  }
  ret
}