# updated version

#1) how to read in data
#frd <- read.csv("~/obese_frac/fracture_data.csv",na.strings="");
#X) how to pull out just the visits involving fractures
# this step no longer needed with corrected input datasets

#2) Defining some needed transformations and creating a data dictionary

# AGD provides everything needed to convert raw BMIs to z-scores
install.packages('AGD');
library(AGD);

# alist is just like a list, except the values assigned to its keys do not get 
# evaluated, they remain as call or name objects (unless of course they are raw
# numbers or character strings). If the input comes by way of DataFinisher, we 
# can count on it having age_at_visit_days, start_date, and birth_date
# (though the cutpoints might vary for different projects)
datatrans <- alist(
  agebin = cut(age_at_visit_days,c(0,730,2190,4380,6570,7665,17885,Inf),labels=c('0-2','2-6','6-12','12-18','18-21','21-49','49+')),
  start_date = as.Date(start_date),
  birth_date = as.Date(birth_date),
  # this one will vary from project to project, because BMI won't always have 
  # the prefix v005 (though the abbreviated name following that prefix should
  # be stable for a given site unless they change the NAME_CHAR in the ontology
  # zbmi_num are the BMIs converted to z-scores normalized to age and sex
  zbmi_num = pnorm(y2z(v005_Bd_Ms_Indx_num,age_at_visit_days/365.25,sex=toupper(sex_cd),ref=cdc.bmi)),
  # this one is highly project specific; we are creating a single T/F factor out
  # of three columns of factors each of which have _many_ levels.
  fracsprain = factor(v000_FRCTR_LWR_LMB!=''|v001_Sprns_strns_kn!=''|v002_Sprns_strns_ankl!='')
  );

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
