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
