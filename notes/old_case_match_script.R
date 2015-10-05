# updated version

#1) how to read in data
#frd <- read.csv("~/obese_frac/fracture_data.csv",na.strings="");
#2) how to pull out just the visits involving fractures
### Oops. Below is where my mistake was. My bad.
#frd_event<-subset(frd,v000_FRCTR_LWR_LMB!=''|v001_Sprns_strns_kn!=''|v002_Sprns_strns_ankl!='');

nthby <- function(data,groups,varcol,nth=1){
 dsplit<-by(data,groups,function(xx) {
     xx<-xx[order(xx[[varcol]]),];
     if(nth=='last') {
       nth <- nrow(xx);
     } else {
       if(nth>nrow(xx)) return(NULL);
     };
     xx[nth,];
   });
 out <- do.call(rbind,dsplit);
}

# looks like we'll be using the above by()/do.call(rbind,...)
# pattern a lot, so let's create a convenience function for it
byunby <- function(data,indices,FUN,...){
  # all arguments passed to by() and mean the same thing as 
  # by()'s corresponding arguments
  dsplit <- by(data,indices,FUN,...,simplify=F);
  do.call(rbind,dsplit);
}


#3) how to pull out just the first fracture from all fracture visits
# firstfrac <- nthby(frd_event,frd_event$patient_num,'age_at_visit_days');

#4) how to bin by age_at_visit
# fr1stbin<-transform(firstfrac,
# agebin=cut(age_at_visit_days,c(0,730,2190,4380,6570,7665,17885,Inf),labels=c('0-2','2-6','6-12','12-18','18-21','21-49','49+')))

# how to read in well-visit data:
# use read.csv and skip steps 2 and 3

#5) with binned (in this case, by age_at_visit_days) data.frames
# you are ready to run matcher(). Use the fractures binned data.frame
# as cases

matcher <- function(cases,controls,groupby,patient='patient_num',...){
 # cases,controls: data.frames (should both contain columns specified by groupby and patient)
 # groupby: a vector of column names or numbers to group by
 # patient: the name of the column containing patient_num's
 # or equivalent

 # matches represents the unique cases (i.e. ill patients) to find matches for
 matches <- unique(cases[,c(patient,groupby)]);
 # record the number of cases
 nm <- nrow(matches);
 # create a blank column to store the matched control patient IDs
 matches$ctrl_pn<-NA;
 # initialize an empty list with an entry for each case
 outprep <- vector('list',nm);
 # initialize progress bar
 pb <- txtProgressBar(0,nm,style=3); counter <- 0;
 # seq(nm) = 1:nm, and we wrap it in sample() to randomize the order to mitigate bias
 for(ii in sample(seq(nm),nm)){
   # iimatched consists of the controls (of those still available) matching the ii-th
   # case, with a few extra columns
   iimatched <- merge(matches[ii,],controls,by=groupby,all.y=F,suffixes=c('.x',''));
   # if there are NO matching controls, skip the next few lines
   if(nrow(iimatched)>0){
     # otherwise, save ONE randomly selected matching control
     outprep[[ii]]<-iimatched[sample(seq(nrow(iimatched)),1),names(controls)];
     # record that control's patient number in the matches table and as a local variable
     iipat<-matches[ii,'ctrl_pn']<-outprep[[ii]][,patient];
     # use that local variable to remove records from that patient from the controls
     # data.frame for subsequent iterations of this loop (so we don't get redundant controls)
     controls<-controls[iipat!=controls[[patient]],];
   }
   # update the progress bar
   counter <- counter + 1; setTxtProgressBar(pb,counter);
 }
 # finish the progress bar
 close(pb);
 # outprep is a list of either NULLs or data.frames; out will be a single data.frame
 out <- do.call(rbind,outprep);
 # return a list object containing the matched controls and the log of what
 # case got match to what control (if any). Where no matches were found,
 # matches$ctrl_pn will have a value of NA.
 # We use invisible to avoid spamming the console in case you forget to capture output
 invisible(list(control_matched=out,matches=matches));
}

prepost <- function(xx,ftest,which=1,nth=1,val=F,...){
  # xx: a subsettable object
  # ftest: a function that takes the subsettable object as its first arg and returns a boolean vector
  # which: an integer or number of integers indicating which entries to select relative to the nth TRUE value returned by ftest
  #        0 = the nth matching entry itself
  #        1 = the first entry after the nth matching entry
  #        -2 = two entries before the nth matching entry
  #        ...and so on
  # nth:   positive integer indicating which matching entry should be used as the reference point for what to return
  # val:   whether to return values (vector-like objects only) or indexes
  refpt <- (fout<-which(ftest(xx,...)))[nth];
  if(is.na(refpt)) return(NULL) else {
  sel <- which + refpt;
    if(any(sel<1)||any(sel>length(fout))) return(NULL) else {
      if(!val||!is.vector(xx)) return(sel) else {
          return(xx[sel]);
      }
    }
  }
}

# What findrange() can do...
# 1. everything prepost() can do (I'll eventually replace it with a wrapper for findrange)
# 2. if wrapped in byunby(), everything nthby can do (ditto)
# 3. all entries ocurring between fstart and fend, with optional lead and lag patterns (e.g. followup visits)
# 4. all entries in a given time window after an fstart event (fend defines the end of the time-window)
# 5. all the entries preceding an fend event (fstart simply returns TRUE)
# 6. look for an nth event before starting or ending a sequence of selected entries
# 7. (maybe) be a framework for a collection of predefined fstart and fend functions targeted at common use cases (like the above)

rangeAfter <- function(xx, fstart, change, rangeColumn, compare = '<=', requireVisit = F, clip = F, continuous = T, fend = NULL, val = T,... ){

      # TODO (Add in a second parameter. E.G. BMI change within 60 days of fend.)

  
      #Range After utilizes the "findrange" function and then allows the user to search for visits X days after
    #e.g. search for all visits within 60 days of the last bone fracture visit. 
  #change - a numeric/integer value that indicates the number of days after the last event in the returned dataframe has occured.
  #rangeColumn - character array containing the name of the column which contains the comparison value e.g. daysColumnName = "AgeAtVisit"
    #support for date and numerical fields only. Returns NULL if attempting to use a non numerical or a factor that cannot be converted into date. 
  #compare - looking for all values after fend (original 'compare' original + change) 
    #e.g. looking for visits within 60 days of event ending- clip = T, change = default.
  #requireVisit - If True rangeAfter will return NULL if there are no visits within daysAfter days of last event.
    # If F- rangeAfter will return regardless of whether or not there are visits within range after. 
    #Note: if the initial search for a visit from findrange returns NULL, rangeAfter will return NULL.
  #clip - if T the final element in the subset returned by findrange will be clipped (if it matches fend && fend != NULL)
    #if F no action is taken. 
    # e.g. if you are searching for follow up visits after an event where there may be multiple consecutive visits with that event.
      #you could make fstart = event and fend = noEvent. findrange would return the visit with no event in the last index. clip will trim that last index if it matches fend
    #Note: if clip is T and requireVisit is F, strict should be F unless there is a reason why records would not be valid if there is no non event visits (e.g.: survival is questioned)
  #continuous - T - show all visits from fend till compare no longer met. F = show only visits where compare  has been met
  #val - used in the same was as findrange if T it returns a subset of the initial dataframe. If F it returns the verticies
  #xx, fstart and fend are required parameters of findrange. Additional parameters may be passed in the ... Please reference findrange for more information on these parameters.
  #Note: you can use rangeAfter to find changing numerical values, e.g. daysAfter = 1 daysColumnName = "BMI" all visits until BMI is > (eventBMI +1)

  #Checking Compare (we return if compare is invalid)
   if(class(xx[ ,rangeColumn]) == "numeric"){
    if(!(compare == "<" | compare == "<=" | compare == "==" | compare == ">=" | compare == ">")){
      print("Invalid compare. Compare must be =, <, <=, ==, >=, >, or !=")
      return(NULL)
    }
  }else{
    tryCatch({
      xx[ ,rangeColumn] <- as.Date(xx[,rangeColumn])
                                   }, error = function(e) 
                                     {print("rangeColumn non numeric, non date");return(NULL);})
    
    if(!(compare == "<" | compare == "<=" | compare == "==" | compare == ">=" | compare == ">" | compare == "!=" )){
      print("Invalid compare. Compare must be =, <, <=, ==, >=, >, or !=")
      return(NULL)
    }
  }
  sub1 = findrange(xx, fstart, fend, trail=0, val = F)
  if(is.null(sub1))return(NULL)
  
  #if clip and fend eval True on the last element, clip the last element
  if(clip){
    if(eval(fend, xx[sub1[length(sub1)],])) sub1 = sub1[1:(length(sub1)-1)]
  }
  #Check if we are on the last visit for patient X. 
  if(sub1[length(sub1)] == nrow(xx)){
    sub2 = NULL
  }else{
    sub2 = xx[(sub1[length(sub1)]+1):nrow(xx),]
    original = xx[sub1[length(sub1)], rangeColumn]
    if(class(original) == "numeric"){
      newStart = paste(rangeColumn, compare,  (original + change))
      newEnd = paste("!(", rangeColumn, compare, (original + change), ")")
      
    }else{
      if(class(original) == "Date"){
        newStart = paste0(rangeColumn, compare,  "as.Date('", (original + change), "')")
        newEnd = paste0("!(", rangeColumn, compare, "as.Date('",(original + change), "'))")
      }else{
        #error, invalid test type
        return(NULL)
      }
    }
    sub2 = findrange(sub2, newStart, newEnd, 0, 0, 1, 1, F, F)
    
    #clip the newEnd value if exists. 
    if(!is.null(sub2)){
      sub2 = sub2 + sub1[length(sub1)]
      e = eval(parse(text=newEnd)[[1]], xx[sub2[length(sub2)],])
      #e handles the issues that arise from incomplete data sets. 
      if(!is.na(e)){
        if(e){
          sub2 = sub2[1:length(sub2)-1]
        }
      }
    }
  }
  if(is.null(sub2)){
    if(requireVisit){
      return(NULL)
    }else{
      if(val){
        return (xx[sub1,])
      }else{
        return(sub1)
      }
    }
  }
  indicies = NULL
  if(continuous){
    indicies = c(sub1[1] : sub2[length(sub2)])
  }else{
    indicies = c(sub1, sub2)
  }
  if(val){
    return (xx[indicies,])
  }else{
    return(indicies)
  }
}

findrange <- function(xx,fstart,fend = NULL,lead=0,trail=0,nthstart=1,nthend=1,val=T,strict=F){
  #xx is a dataframe containing the set of records to be considered "one patient" or one set of data to analyse
  #fstart is a criterion that should calculate a boolean value when applied to the dataframe
  # accepts string or expression.
  #fend -using the index found in fstart, utilize this new criterion to find the last valid visit
  # accepts string or expression.
  #if left blank, end will equal start
  #nthstart = the index of "true" values you wish to use- e.g. the second occurance of a fracture, default first occurance
  #strict: T/F, if TRUE returns NULL when all criteria cannot be satisfied. Otherwise return as much of the range as does satisfy criteria; if an nthstart value cannot be found, however, a NULL is still returned
  #val: When true val returns a subset of xx, when false val returns a vector list. (affected by strict)

  tstart = substitute(fstart);
  tend = substitute(fend);
  if(class(tstart) == "call"){fstart = tstart;}
  if(class(tstart) == "name"){
    fstart = eval(tstart)
  }
  if(class(fstart)=="character"){
    fstart = parse(text=fstart)[[1]]; 
  }
  
  if(class(tend) == "call"){fend = tend;}
  if(class(tend) == "name"){
    fend = eval(tend);
  }
  if(class(fend)=="character"){
    fend = parse(text=fend)[[1]]; 
  }
  
  if(is.null(fend)){fend=fstart}
  
  #Find all values that evaluate to TRUE with fstart, take the nthstart one and assign the index to start
  start = which(eval(fstart,xx))[nthstart];
  
  #if no index, we don't have an nthstart that evaluates to true
  if(is.na(start)) return(NULL);
  
  #Run an eval on a subset of xx that starts from index start
  #Take the nthend of the evals (this index is offset by start -1)
  end = which(eval(fend, xx[(start):(nrow(xx)), ]))[nthend]
  
  #if we don't find the fend, we check to see if it's strict. If strict return null, else return all after start
  if(is.na(end)){
    if(strict){return (NULL);}
    else { 
      end = nrow(xx);
    } #assigning the last value to end and proceeding to adding beginning offsets
    #ending offsets are out of bounds
  }else{
    # if end is not null, we will add the start offset
    end = end + start - 1;
  }
  if(val){
    tmp = xx[start:end, ];
  }else{
    tmp = c(start:end);
  }
  
  if(lead[1] != 0){
    leadi = sort(lead)+start;
    if(leadi[1] < 1 && strict) return(NULL);
    leadi = leadi[leadi > 0];
    #Check if we can account for all requested indexes, if not return null if strict
    if(length(leadi) != 0){
      if(val){
        tmp = rbind(xx[leadi, ], tmp);
      }else{
        tmp = c(leadi, tmp);
      }
      
    }else{
      if(strict) return(NULL);
    }
  }
  if(trail[1] != 0){
    traili = sort(trail) + end;
    if(traili[length(traili)] > nrow(xx) && strict) return(NULL);
    traili = traili[traili<=nrow(xx)];
    if(length(traili) != 0){
      if(val){
        tmp = rbind(tmp, xx[traili,]);
      }else{
        tmp = c(tmp, traili);
      }
    }
  }
  return (tmp);
}



#casebin: dataframe, with cases binned by age
#ctrlbin: dataframe in the list returned by matcher()$control_matched, which is binned and trimmed to the same size as the binned cases
#dx_col is a vector of diagnosis columns used to define cases c()

# In principle useful to be able to run some quality/validity checks on casebin and ctrlbin
# But do NOT use this function, write one that checks for real problems you might observe
# colcheck <- function(casebin, ctrlbin) {
#   #checks for column names+content, checks that controls do not have diagnosis used to define controls
#   #hard coded a seed; consider dynamically/randomly generating the seed
#   set.seed(41679)
#   #check columns to make sure they have the same column names in the same order
#   case_col<-colnames(casebin)
#   ctrl_col<-colnames(ctrlbin)
#   if(!identical(case_col, ctrl_col)) stop('column names are not matched')
#   #check side-by-side
#   ctrl_nas<-(cbind(sapply(ctrlbin, function(xx) sum(!is.na(xx)))))
#   case_nas<-(cbind(sapply(casebin, function(xx) sum(!is.na(xx)))))
#   cbind(ctrl_nas,case_nas)
# }


#casebin: dataframe, with cases binned by age
#ctrlbin: dataframe in the list returned by matcher()$control_matched, which is binned and trimmed to the same size as the binned cases

samplegroup <- function(casebin, ctrlbin, n1=80, n2=n1) {
  #take random sample ~10% of each cohort and combine them into a sample dataset for training/modeling
  case_idx<-sample(nrow(casebin), n1)
  case_sample<-casebin[case_idx,]
  case_sample$group<-'case'
  ctrl_idx<-sample(nrow(ctrlbin),n2)
  ctrl_sample<-ctrlbin[ctrl_idx,]
  ctrl_sample$group<-'control'
  sample<-rbind(case_sample,ctrl_sample)
  # The below should NOT be hardcoded, and should be in a different function
  # altogether
  #sample$injury<-apply(sample,1,function(xx) {any(xx[c(8,10,12)]!="")})
  invisible(sample)
}

# How to convert to percentiles
# install.packages('AGD')
# library(AGD)
# below takes care of everything!
# sample$prc_bmi <- pnorm(y2z(sample$v005_Bd_Ms_Indx_num,sample$age_at_visit_days/365.25,sex=toupper(sample$sex_cd),ref=cdc.bmi))

