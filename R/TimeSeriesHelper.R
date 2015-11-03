
getTimeSeriesBases <- function (entireList, idsWeWant, ptIdent = "patient_num", ...){
  ## we pass the entire list in case the ethnicity is in a record outside those selected for analysis. 

  x = entireList[(entireList[,ptIdent] %in% idsWeWant),]
  byunby(x, x[ , ptIdent], FUN = createTimeSeriesBase)

}
createTimeSeriesBase <- function(xx, ptIdent = "patient_num", vars = c("sex_cd", "language_cd", "race_cd", "v007_Ethnct"), varConst = c("c", "c", "c", "n")){
  #this function can be used with byundby to create the "base" dimension for the time series analysis
  #xx - a subset of the patient list containing just the records for one patient. 
  #ptIdent = the column that identifies the patient.
  #vars- the variables to be included in baseline analysis. 
  ###Note: This is a Baseline value and should remain constant for the duration of the time series
  #varConst- variable consistency. This will hopefully not be required in the future, but as Ethnicity only appears once
  ###instead of on every instance of the patient record, something other than "c" (constant) should be used to signify that 
  ##validate vars and varConst are the same length.
  if(length(vars) != length(varConst)){
    stop("Error, vars and varConst are not the same length!")
  }
  #create ret and add the patient identifier
  ret = data.frame(xx[1,ptIdent])
  colnames(ret) <- c(ptIdent)
  it = 1
  while (it <= length(vars)){
    
    if(vars[it] %in% colnames(xx)){
      if(varConst[it] == "c"){
        #nice and easy, juts like ptIdent
        ret[1,vars[it]] = xx[1,vars[it]]
      }else{
        iit = 1
        while(iit <= nrow(xx)){
          if(!is.na(xx[iit, vars[it]]) && !is.null(xx[iit, vars[it]]) && xx[iit,vars[it]] != ""){
            ret[1,vars[it]] = xx[iit, vars[it]]
            break;
          }
          iit = iit + 1
        }
        if(! vars[it] %in% colnames(ret)){
          ret[1,vars[it]] = NA
        }
      }
    }else{
      warning(paste("Column", vars[i], "does not exist in this subset. Skipping."))
    }
  
    it = it +1
  }
  ret
}