sampler <- function(sampleSize, matchList, case, control, patient="patient_num", grouping = c("age_tr_fac"), matchType = 'R', inclusion = 'A'){
  #Samplesize- the number of samples to obtain
  #matches- matcher$matches
  #Case  the list that matches the first column of matches
  #Control - the list that matches the second column of matches
  #Currently assumes patient number (ctrl_pn in matches) and age_tr_fac
  #matchType R- random per group. A- all per group. AA- all visits for that patient
  #inclusion - lists whether or not all values in "matches" are going to be available in case and control - e.g. not all matched cases are "valid"
    #A- All included
    #non A- non guaranteed
  #obtain the samples
  #check that sqldr is installed. Method will not operate without it
  if(!require(sqldf)){stop('Package sqldf is required for this method to operate. Please install sqldf and try again')}
  #Check inclusion- if not 'A' our matchList may contain IDs that are not in case and control
  if(inclusion =='A'){
    matches = matchList
  }else{
    #Our case and control may be missing values in the matchlist. Remove matches where either
    #the case or the control is missing. 
    sqlstr =paste0("SELECT DISTINCT matchList.* FROM matchList JOIN [case] ON (matchList.", patient, " == [case].", patient, ") JOIN control ON (matchList.ctrl_pn == control.", patient, ")")
    groups = 1
    while(groups <= length(grouping)){
      paste0(sqlstr, " AND (control.", groups, "== matchList.", groups, ")")
      groups = groups + 1
    }
    matches = sqldf(sqlstr)
  }
  #verify that we have enough matches to cover the requested sampleSize.
  if(nrow(matches) <= sampleSize){
    stop("Not enough valid case control matches to pull the sample size.")
  }
  #select and sort random samples of sampleSize without replacement
  samples = sort(sample(nrow(matches), sampleSize))
  #Collect and label the "case" matches
  col1 = case[case[,patient]%in%matches[,patient][samples],]
  col1$CaseControl = "Case"
  #control
  m = matches[samples,]
  
  #Check the matchType to see what information to pull on each control
  if(matchType == 'AA'){
    #Pull all visits associated with the ID
    col2 = control[control[,patient]%in%m$ctrl_pn,]
  }else if(matchType == 'A' || matchType == 'R'){
    #select all or a random visit from a bin indicated by grouping
    #set up SQL select statement to select all controls in the bin
    sqlstr = paste0("SELECT control.* FROM control JOIN m ON (control.", patient," == m.ctrl_pn)")
    group = 1
    while(group <= length(grouping)){
      sqlstr = paste0(sqlstr, " AND (control.", grouping[group], " == m.", grouping[group], ")")
      group = group +1
    }
    col2 = sqldf(sqlstr)
    #if we need only a random visit for each patient, use byunby and controlSample to pull
    if(matchType == 'R'){
      col2 = byunby(col2, col2[ , patient], FUN = controlSample )
    }
  }else { 
    stop("ERROR! Invalid matchType. Valid options are: AA, A, R")
  }
  #label col2 as Control and bind the two tables together with rbind. 
  col2$CaseControl = "Control"
  ret = rbind(col1,col2)
  return(ret)
}
controlSample <- function(samples){
  #randomly selects one item from each group and returns it. Used to select a random visit
  

  return (samples[sample(nrow(samples), 1),])
}