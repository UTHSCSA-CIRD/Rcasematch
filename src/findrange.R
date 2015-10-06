# What findrange() can do...
# 1. everything prepost() can do (I'll eventually replace it with a wrapper for findrange)
# 2. if wrapped in byunby(), everything nthby can do (ditto)
# 3. all entries ocurring between fstart and fend, with optional lead and lag patterns (e.g. followup visits)
# 4. all entries in a given time window after an fstart event (fend defines the end of the time-window)
# 5. all the entries preceding an fend event (fstart simply returns TRUE)
# 6. look for an nth event before starting or ending a sequence of selected entries
# 7. (maybe) be a framework for a collection of predefined fstart and fend functions targeted at common use cases (like the above)


findrange <- function(xx,fstart,fend,lead=0,trail=0,nthstart=1,nthend=1,val=F,strict=F,...){
  # xx:   a subsettable object
  # fstart,fend: functions taking xx as first arg, first one returns starting reference point, second returns ending one (and takes the starting offset as its second arg)
  #             hint... fend should be the first ocurrence where you want to stop retrieving further data
  # lead,trail: integers or vectors indicating sequences of entries preceeding the start and following the end, respectively, relative to start and end respectively, can be negative or 0
  # nthstart,nthend: integers indicating which starting positive value of fstart is the first reference point; the ending reference point is nth relative to whatever turns out to be the starting one
  # val: whether to return values (if possible, not yet implemented)
  # strict: T/F, if TRUE returns NULL when all criteria cannot be satisfied. Otherwise return as much of the range as does satisfy criteria; if an nthstart value cannot be found, however, a NULL is still returned
  # TODO: tests on argument values
  
  # grab the fstart and fend arguments before they can get evaluated
  tstart <- substitute(fstart);
  
  # Previously we tested for fend being NULL, but the default state of fend
  # is not null-- it's technically a mandatory argument and when it's not specified
  # it's missing. Here we catch that behavior and set it to whatever fstart was set to
  # Whether this is a good idea or not needs further thought. Perhaps a better default
  # value is one that immediately ceases accumulating output so that by default the first
  # matching result for fstart is returned and no further ones.
  if(missing(fend)) tend <- tstart else tend <- substitute(fend);  
  
  # if the start expression is a call, leave it alone, that's what we want
  if(is.call(tstart)){fstart <- tstart;}
  # if it's a name, evaluate it
  else if(is.name(tstart)){ fstart <- eval(tstart);}
  # if it evaluates to a character 
  # (or if it was all along and thus did not meet the above checks) 
  # try parsing it to turn it into a call
  if(is.character(fstart)){ fstart = parse(text=fstart)[[1]];}
  # ...it might still not be a valid call at that point, but hey, we tried
  # TODO: put some final test here that will give the user some actionable error message if
  # despite our best efforts they still managed to pass something that doesn't evaluate as
  # an R call
  
  # repeat the above for fend
  if(is.call(tend)){fend <- tend;} else if(is.name(tend)){fend <- eval(tend);}
  if(is.character(fend)){fend = parse(text=fend)[[1]];}
  
  lead<-sort(lead); trail<-sort(trail);
  ptstart <- which(stout<-fstart(xx,...))[nthstart];
  # TODO: (more thorough) tests on ptstart
  if(is.na(ptstart)) return(NULL);
  ptend <- which((stend<-fend(xx,ptstart,...))[ptstart:length(stout)])[nthend]+ptstart-1;
  # TODO: (more thorough) tests on ptend
  if(is.na(ptend)) if(strict) return(NULL) else ptend<-length(stout);
  leadidx <- lead+ptstart; trailidx <- trail+ptend;
  out <- sort(unique(c(leadidx,(max(leadidx)+1):(min(trailidx)-1),trailidx)));
  if(any(is.na(out)|out<1)) if(strict) return(NULL);
  return(na.omit(out[out>0]));
  # TODO: check for val argument, check for vectorness
}
