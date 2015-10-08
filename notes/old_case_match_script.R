# updated version

#1) how to read in data
#frd <- read.csv("~/obese_frac/fracture_data.csv",na.strings="");
#X) how to pull out just the visits involving fractures
# this step no longer needed with corrected input datasets

#2) Defining some needed transformations and creating a data dictionary

# AGD provides everything needed to convert raw BMIs to z-scores

# alist is just like a list, except the values assigned to its keys do not get 
# evaluated, they remain as call or name objects (unless of course they are raw
# numbers or character strings). If the input comes by way of DataFinisher, we 
# can count on it having age_at_visit_days, start_date, and birth_date
# (though the cutpoints might vary for different projects)

# ...we use the above alist as follows:
# frd <- do.call(transform,c(list(frd),datatrans));
# When you do c(A,B) where A and B are lists, you get a longer list containing 
# the elements of A followed by the elements of B. However, since a data.frame
# is a type of list, you cannot do c(frd,datatrans) because you then get a list
# of each of the elements of frd (columns) followed by each of the elements of 
# datatrans. So you wrap it in one extra level of list to protect it.

# create a basic data-dictionary: column names and data-types
#frdict <- data.frame(name=colnames(frd),class=sapply(frd,function(ii) class(ii)[[1]]),stringsAsFactors = F);
#frdict$role <- frdict$class;
# now, for the new role column, remove all the variables that aren't going to
# be used in fitting a statistical model...
#frdict[grepl(".*(_info|_inactive)",frd$name)|frd$name %in% c('start_date','birth_date'),'role']<-NA;
# also, mark patient_num as the grouping variable, for future use in nlme and 
# survival models.
#frdict[frdict$name=='patient_num','role']<-'groupingvar';

# Now, instead of managing lists of lengthy column names, you can do, e.g.,
# plot(frd[,frdict$role=='numeric'])

#3) how to pull out just the first fracture or sprain for patient 204
# Notice that we first transformed the data.frame, and now the fracsprain
# column makes it easier to subset
# findrange(subset(frd,patient_num==204),fracsprain=='TRUE',rep_len(T,length(patient_num)),val=T)
# Therefore, to pull out just the first fracture or sprain for all patients you do...
# firstfrac <- byunby(frd,list(frd$patient_num),findrange,fstart=fracsprain=='TRUE',fend=rep_len(T,length(patient_num)),val=T)
# all until first injury (tricky, have to prepend an F so that the event itself gets included):
# untilfrac <- byunby(fftr,list(fftr$patient_num),findrange,fstart=rep_len(T,length(age_at_visit_days)),fend=c(F,fracsprain=='TRUE'),val=T)
# sixty-day followup after first injury:
# follow60 <- byunby(fftr,list(fftr$patient_num),findrange,fstart=fracsprain=='TRUE',fend=age_at_visit_days>(age_at_visit_days[1]+60),val=T)

#X) how to bin by age_at_visit (already accomplished in step 2)

# how to read in well-visit data:
# use read.csv and skip step 3

#5) with binned (in this case, by age_at_visit_days) data.frames
# you are ready to run matcher(). Use the fractures binned data.frame
# as cases

# nthby() is no longer used

# BELOW OBSOLETED BY src/byunby.R
# looks like we'll be using the by()/do.call(rbind,...)
# pattern a lot, so let's create a convenience function for it
#byunby <- function(data,indices,FUN,...){
#  # all arguments passed to by() and mean the same thing as 
#  # by()'s corresponding arguments
#  dsplit <- by(data,indices,FUN,...,simplify=F);
#  do.call(rbind,dsplit);
#}

# USE src/matcher.R instead
# matcher <- function(cases,controls,groupby,patient='patient_num',...){
#  # cases,controls: data.frames (should both contain columns specified by groupby and patient)
#  # groupby: a vector of column names or numbers to group by
#  # patient: the name of the column containing patient_num's
#  # or equivalent
# 
#  # matches represents the unique cases (i.e. ill patients) to find matches for
#  matches <- unique(cases[,c(patient,groupby)]);
#  # record the number of cases
#  nm <- nrow(matches);
#  # create a blank column to store the matched control patient IDs
#  matches$ctrl_pn<-NA;
#  # initialize an empty list with an entry for each case
#  outprep <- vector('list',nm);
#  # initialize progress bar
#  pb <- txtProgressBar(0,nm,style=3); counter <- 0;
#  # seq(nm) = 1:nm, and we wrap it in sample() to randomize the order to mitigate bias
#  for(ii in sample(seq(nm),nm)){
#    # iimatched consists of the controls (of those still available) matching the ii-th
#    # case, with a few extra columns
#    iimatched <- merge(matches[ii,],controls,by=groupby,all.y=F,suffixes=c('.x',''));
#    # if there are NO matching controls, skip the next few lines
#    if(nrow(iimatched)>0){
#      # otherwise, save ONE randomly selected matching control
#      outprep[[ii]]<-iimatched[sample(seq(nrow(iimatched)),1),names(controls)];
#      # record that control's patient number in the matches table and as a local variable
#      iipat<-matches[ii,'ctrl_pn']<-outprep[[ii]][,patient];
#      # use that local variable to remove records from that patient from the controls
#      # data.frame for subsequent iterations of this loop (so we don't get redundant controls)
#      controls<-controls[iipat!=controls[[patient]],];
#    }
#    # update the progress bar
#    counter <- counter + 1; setTxtProgressBar(pb,counter);
#  }
#  # finish the progress bar
#  close(pb);
#  # outprep is a list of either NULLs or data.frames; out will be a single data.frame
#  out <- do.call(rbind,outprep);
#  # return a list object containing the matched controls and the log of what
#  # case got match to what control (if any). Where no matches were found,
#  # matches$ctrl_pn will have a value of NA.
#  # We use invisible to avoid spamming the console in case you forget to capture output
#  invisible(list(control_matched=out,matches=matches));
# }

# BELOW NO LONGER NEEDED
# prepost <- function(xx,ftest,which=1,nth=1,val=F,...){
#   # xx: a subsettable object
#   # ftest: a function that takes the subsettable object as its first arg and returns a boolean vector
#   # which: an integer or number of integers indicating which entries to select relative to the nth TRUE value returned by ftest
#   #        0 = the nth matching entry itself
#   #        1 = the first entry after the nth matching entry
#   #        -2 = two entries before the nth matching entry
#   #        ...and so on
#   # nth:   positive integer indicating which matching entry should be used as the reference point for what to return
#   # val:   whether to return values (vector-like objects only) or indexes
#   refpt <- (fout<-which(ftest(xx,...)))[nth];
#   if(is.na(refpt)) return(NULL) else {
#   sel <- which + refpt;
#     if(any(sel<1)||any(sel>length(fout))) return(NULL) else {
#       if(!val||!is.vector(xx)) return(sel) else {
#           return(xx[sel]);
#       }
#     }
#   }
# }

# What findrange() can do...
# 1. everything prepost() can do (I'll eventually replace it with a wrapper for findrange)
# 2. if wrapped in byunby(), everything nthby can do (ditto)
# 3. all entries ocurring between fstart and fend, with optional lead and lag patterns (e.g. followup visits)
# 4. all entries in a given time window after an fstart event (fend defines the end of the time-window)
# 5. all the entries preceding an fend event (fstart simply returns TRUE)
# 6. look for an nth event before starting or ending a sequence of selected entries
# 7. (maybe) be a framework for a collection of predefined fstart and fend functions targeted at common use cases (like the above)

# This copy of findrange obsoleted. Shortened and corrected one with additional comments
# has been put into findrange.R and that one should be edited from now on
# findrange <- function(xx,fstart,fend,lead=0,trail=0,nthstart=1,nthend=1,val=F,strict=F,...){
#   # xx:   a subsettable object
#   # fstart,fend: functions taking xx as first arg, first one returns starting reference point, second returns ending one (and takes the starting offset as its second arg)
#   #             hint... fend should be the first ocurrence where you want to stop retrieving further data
#   # lead,trail: integers or vectors indicating sequences of entries preceeding the start and following the end, respectively, relative to start and end respectively, can be negative or 0
#   # nthstart,nthend: integers indicating which starting positive value of fstart is the first reference point; the ending reference point is nth relative to whatever turns out to be the starting one
#   # val: whether to return values (if possible, not yet implemented)
#   # strict: T/F, if TRUE returns NULL when all criteria cannot be satisfied. Otherwise return as much of the range as does satisfy criteria; if an nthstart value cannot be found, however, a NULL is still returned
#   # TODO: tests on argument values
#   lead<-sort(lead); trail<-sort(trail);
#   ptstart <- which(stout<-fstart(xx,...))[nthstart];
#   # TODO: (more thorough) tests on ptstart
#   if(is.na(ptstart)) return(NULL);
#   ptend <- which((stend<-fend(xx,ptstart,...))[ptstart:length(stout)])[nthend]+ptstart-1;
#   # TODO: (more thorough) tests on ptend
#   if(is.na(ptend)) if(strict) return(NULL) else ptend<-length(stout);
#   leadidx <- lead+ptstart; trailidx <- trail+ptend;
#   out <- sort(unique(c(leadidx,(max(leadidx)+1):(min(trailidx)-1),trailidx)));
#   if(any(is.na(out)|out<1)) if(strict) return(NULL);
#   return(na.omit(out[out>0]));
#   # TODO: check for val argument, check for vectorness
# }

rangeAfter <- function(xx, fstart, change, rangeColumn, compare = '<=', requireVisit = F, continuous = T, fend = NULL, val = T,... ){
  
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
  #if F no action is taken. 
  # e.g. if you are searching for follow up visits after an event where there may be multiple consecutive visits with that event.
  #you could make fstart = event and fend = noEvent. findrange would return the visit with no event in the last index. clip will trim that last index if it matches fend
  #Note: if clip is T and requireVisit is F, strict should be F unless there is a reason why records would not be valid if there is no non event visits (e.g.: survival is questioned)
  #continuous - T - show all visits from fend till compare no longer met. F = show only visits where compare  has been met
  #val - used in the same was as findrange if T it returns a subset of the initial dataframe. If F it returns the verticies
  #xx, fstart and fend are required parameters of findrange. Additional parameters may be passed in the ... Please reference findrange for more information on these parameters.
  #Note: you can use rangeAfter to find changing numerical values, e.g. daysAfter = 1 daysColumnName = "BMI" all visits until BMI is > (eventBMI +1)

  #Checking Compare (we return if compare is invalid)
  if(class(xx[ ,rangeColumn]) == "numeric" | class(xx[ ,rangeColumn]) == "Date"){
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
  sub1 = lmfindrange(xx, fstart, fend, trail=0, val = F)
  if(is.null(sub1))return(NULL)
  
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
    sub2 = lmfindrange(sub2, newStart, newEnd, 0, 0, 1, 1, F, F, clip = T)
    
    #clip the newEnd value if exists. 
    if(!is.null(sub2)){
      sub2 = sub2 + sub1[length(sub1)]
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

rangeAfterRecord <- function(xx, change, rangeColumn, compare = '<=', val = T,... ){
  # TODO (Add in a second parameter. E.G. BMI change within 60 days of fend.)
  
  #Range After Record utilizes the "findrange" function and then allows the user to search for visits 
    #within a "range" after the first record. rangeAfterRecord will "walk" through the vector list
    #until either there are not enough records in the list (returns NULL) or findrange returns a valid match
  #change - a numeric/integer value that indicates the number of days after the last event in the returned dataframe has occured.
  #rangeColumn - character array containing the name of the column which contains the comparison value e.g. daysColumnName = "AgeAtVisit"
  #support for date and numerical fields only. Returns NULL if attempting to use a non numerical or a factor that cannot be converted into date. 
  #compare - looking for all values after fend (original 'compare' original + change) 
  #e.g. looking for visits within 60 days of event ending- clip = T, change = default.
  
  #val - used in the same was as findrange if T it returns a subset of the initial dataframe. If F it returns the verticies
  
  #Checking Compare (we return if compare is invalid)
  if(class(xx[ ,rangeColumn]) == "numeric" | class(xx[ ,rangeColumn]) == "Date"){
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
  
  while(nrow(xx) > 1){
    original = xx[1, rangeColumn]
    if(class(original) == "numeric"){
      newStart = paste(rangeColumn, compare,  (original + change))
      newEnd = paste("!(", rangeColumn, compare, (original + change), ")")
      
    }else{
      if(class(original) == "Date"){
        newStart = paste0(rangeColumn, compare,  "as.Date('", (original + change), "')")
        newEnd = paste0("!(", rangeColumn, compare, "as.Date('",(original + change), "'))")
      }else{
        warning("Invalid range column type")
        return(NULL)
      }
    }
    ret = lmfindrange(xx[2:nrow(xx),], newStart, newEnd, 0, 0, 1, 1, F, F, clip = T)
    if(is.null(ret)){
      #clip one, repeat the while which will analyze off the new first indicie. 
      xx = xx[2:nrow(xx),]
    }else{
      if(val){
        return (xx[1:(ret[length(ret)]+1),])
      }else{
        return (c(1:(ret[length(ret)]+1)))
      }
      
    }
  }
  #If the while statement does not produce a valid value, we ran out of records, return NULL.
  return (NULL)
}
lmfindrange <- function(xx,fstart,fend,lead=0,trail=0,nthstart=1,nthend=1,val=F,strict=F, clip = T){
  # xx:   a subsettable object
  # fstart,fend: functions taking xx as first arg, first one returns starting reference point, second returns ending one (and takes the starting offset as its second arg)
  #             hint... fend should be the first ocurrence where you want to stop retrieving further data
  # lead,trail: integers or vectors indicating sequences of entries preceeding the start and following the end, respectively, relative to start and end respectively, can be negative or 0
  # nthstart,nthend: integers indicating which starting positive value of fstart is the first reference point; the ending reference point is nth relative to whatever turns out to be the starting one
  # val: whether to return values (if possible, not yet implemented)
  # strict: T/F, if TRUE returns NULL when all criteria cannot be satisfied. Otherwise return as much of the range as does satisfy criteria; if an nthstart value cannot be found, however, a NULL is still returned
  # clip if T - will clip the last record if it matches fend. Use if fend = !fstart.
  # TODO: tests on argument values
  
  # grab the fstart and fend arguments before they can get evaluated
  
  # Previously we tested for fend being NULL, but the default state of fend
  # is not null-- it's technically a mandatory argument and when it's not specified
  # it's missing. Here we catch that behavior and set it to whatever fstart was set to
  # Whether this is a good idea or not needs further thought. Perhaps a better default
  # value is one that immediately ceases accumulating output so that by default the first
  # matching result for fstart is returned and no further ones.
  if(missing(fend)) fend <- fstart 
  
  # TODO: after code walk-through with LSMM to explain the higher priority
  # cleaned up stuff, remove the extra tstart/tend variables and as time 
  # permits go over that.
  
  # if the start expression is a call, leave it alone, that's what we want
  #if(is.call(tstart)){fstart <- tstart;}
  # if it's a name, evaluate it
  if(is.name(fstart)){ fstart <- eval(fstart);}
  # if it evaluates to a character 
  # (or if it was all along and thus did not meet the above checks) 
  # try parsing it to turn it into a call
  if(is.character(fstart)){ fstart = parse(text=fstart)[[1]];}
  # ...it might still not be a valid call at that point, but hey, we tried
  if(!is.call(fstart)&&!is.logical(fstart)){stop('fstart does not evaluate to a call')}
  # TODO: put some final test here that will give the user some actionable error message if
  # despite our best efforts they still managed to pass something that doesn't evaluate as
  # an R call
  
  # repeat the above for fend
  #if(is.call(tend)){fend <- tend;} else 
  if(is.name(fend)){fend <- eval(fend);}
  if(is.character(fend)){fend <- parse(text=fend)[[1]];}
  if(!is.call(fend)&&!is.logical(fend)){stop('fend does not evaluate to a call')}
  #Find all values that evaluate to TRUE with fstart, take the nthstart one and
  # assign that index to start
  start <- which(eval(fstart,xx))[nthstart];
  #if no index, we don't have an nthstart that evaluates to true
  if(is.na(start)) return(NULL);
  
  #Run an eval on a subset of xx that starts from index start
  #Take the nthend of the evals (this index is will be offset by start -2)
  end <- which(eval(fend, xx[(start):(nrow(xx)), ]))[nthend]
  
  #if we don't find the fend, we check to see if it's strict. If strict return null, else return all after start
  if(is.na(end)){
    if(strict){return (NULL);} else { 
      end <- nrow(xx);
    } #assigning the last value to end and proceeding to adding beginning offsets
    #ending offsets are out of bounds
    # if end == 1, special case, accrual of results immediately exits
    # if end == 2, we stop before the second result, so outcome same as above
  }else {
    end = end + start - 1
    if(clip && eval(fend, xx[end,])){end = end - 1}
  }
  
  
  # TODO: decide whether the helpfile should say that lead are negative offsets
  # or that both lead and trail are positive offsets, and internally convert
  # lead to negative. In either case, the "wrong" sign of the offset should
  # just be ignored perhaps? For now, assuming that lead are negative 
  # Note: pushing 0 to the end of the lead and trail vectors so that we never
  # end up with a NULL vector which would cause an error when adding to start
  # or to end
  # test that lead and trail are integers
  if(any(!is.numeric(lead))){warning("non-numeric values in lead were removed"); lead = lead[is.numeric(lead)];}
  if(any(!is.numeric(trail))){warning("non-numeric values in trail were removed"); trail = trail[is.numeric(trail)];}
  
  # TODO: emit warnings when correcting redundancies, illegal offsets, etc.
  if(any(lead>0)){warning("Only negative values are acceptable for lead. Removing positive indicies.")}
  if(any(trail<0)){warning("Only positive values are acceptable for trail. Removing positive indicies.")}
  
  lead <- c(lead[lead<0],0); trail <- c(trail[trail>0],0);
  lead <- lead+start; trail <- trail+end;
  if(strict){
    # If the strict argument is true, return NULL if the leading or trailing 
    # offsets cannot fully fit within the input dataset xx
    if(any(lead<=0)||any(trail>nrow(xx))) return(NULL);
  } else {
    # Otherwise, just fix them (if necessary, if not, it's a nullop anyway)
    # as below
    lead <- lead[lead>0]; trail <- trail[trail<=nrow(xx)];
  }
  
  # Who cares if lead and trail overlap the body of the interval? Just let 
  # unique() adjudicate that, and wrap sort() around it for good measure
  idxs <- sort(unique(c(lead,start:end,trail)));
  
  # the below was moved here from earlier in the code and modified
  # Note: invisible() used instead of return() so we don't spam the user's 
  # console. Note also, lack of curly brackets not an error-- if only one
  # statement in the body of if or else, brackets can be omitted in R
  if(val) invisible(xx[idxs,]) else return(idxs);
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

# BELOW NO LONGER USED
# samplegroup <- function(casebin, ctrlbin, n1=80, n2=n1) {
#   #take random sample ~10% of each cohort and combine them into a sample dataset for training/modeling
#   case_idx<-sample(nrow(casebin), n1)
#   case_sample<-casebin[case_idx,]
#   case_sample$group<-'case'
#   ctrl_idx<-sample(nrow(ctrlbin),n2)
#   ctrl_sample<-ctrlbin[ctrl_idx,]
#   ctrl_sample$group<-'control'
#   sample<-rbind(case_sample,ctrl_sample)
#   # The below should NOT be hardcoded, and should be in a different function
#   # altogether
#   #sample$injury<-apply(sample,1,function(xx) {any(xx[c(8,10,12)]!="")})
#   invisible(sample)
# }
