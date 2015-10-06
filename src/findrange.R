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
  
  # TODO: after code walk-through with LSMM to explain the higher priority
  # cleaned up stuff, remove the extra tstart/tend variables and as time 
  # permits go over that.
  
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
  if(is.character(fend)){fend <- parse(text=fend)[[1]];}
  
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
  }else if(end<3){end<-start;} else {
    # if end is not null, we will add the start offset
    # We used to think the offset was start - 1, but it's not
    # Let's say raw end = 3. That means: stop accruing before the 3rd result
    # the first accrual is always equal to start, so we need to subtract 1
    # and, we need to stop _before_ we get to third result, so subtract 1 more
    end <- end + start - 2;
  }
  
  # TODO: decide whether the helpfile should say that lead are negative offsets
  # or that both lead and trail are positive offsets, and internally convert
  # lead to negative. In either case, the "wrong" sign of the offset should
  # just be ignored perhaps? For now, assuming that lead are negative 
  # Note: pushing 0 to the end of the lead and trail vectors so that we never
  # end up with a NULL vector which would cause an error when adding to start
  # or to end
  lead <- c(lead[lead<=0],0); trail <- c(trail[trail>=0],0);
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
