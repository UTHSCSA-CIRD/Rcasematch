sampler <- function(sampleSize, matches, case, control, matchType = 'R'){
  #Samplesize- the number of samples to obtain
  #matches- matcher$matches
  #Case  the list that matches the first column of matches
  #Control - the list that matches the second column of matches
  #Currently assumes patient number (ctrl_pn in matches) and age_tr_fac
  #matchType R- random per group. A- all per group. AA- all visits for that patient
     # R and A require sqldf to be installed. 
  #obtain the samples

  samples = sort(sample(nrow(matches), sampleSize))
  
  col1 = case[case$patient_num%in%matches$patient_num[samples],]
  col1$CaseControl = "Case"
  #control
  m = matches[samples,]
  if(matchType == 'AA'){
    col2 = control[control$patientnum%in%m$ctrl_pn,]
  }else if(matchType == 'A' || matchType == 'R'){
    #check if required package is available, load
    if(!require(sqldf)){stop('Match types A and R require sqldf to operate. Please install sqldf and try again')}
    
    col2 = sqldf("SELECT control.* FROM control JOIN m ON (control.patient_num == m.ctrl_pn AND control.age_tr_fac == m.age_tr_fac)")
    if(matchType == 'R'){
      col2 = byunby(col2, col2[ , "patient_num"], FUN = controlSample )
    }
  }else { 
    stop("ERROR! Invalid matchType. Valid options are: AA, A, R")
  }
  
  col2$CaseControl = "Control"
  ret = rbind(col1,col2)
  return(ret)
}
controlSample <- function(samples){
  #randomly selects one item from each group and returns it.

  return (samples[sample(nrow(samples), 1),])
}