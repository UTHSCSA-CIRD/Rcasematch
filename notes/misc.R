if(!require(AGD)) install.packages('AGD');
library(AGD);
## File locations (change to correct local paths)
#fracfile <- 'manuells_fracV2.2.csv';
#wellfile <- 'manuells_WellVisitV2.2.csv';
fracfile <- '../from_g_drive//fracture_data.csv';
wellfile <- '../from_g_drive//wellvisit_data.csv';


## You'll want to edit these column names for each project
respvars <- 'zbmi_tr_num';
groupvars <- 'patient_num';
## Especially these!!
rawvars <- c('v001_FRCTR_LWR_LMB','v003_Sprns_strns_ankl','v004_Sprns_strns_kn','v005_Bd_Ms_Indx_num','v007_Ethnct','v008_Hght_cm_num','v011_Wght_oz_num')
## but these are probably pretty stable for DataFinisher output
metavarsmatch <- ".*(_info|_inactive)";
metavars <- c('start_date','birth_date');

# variables to analyze
varsofinterest <- c(groupvars,'sex_cd','language_cd','race_cd','v007_Ethnct','v000_Pls_num','v002_Rsprtn_Rt_num','v006_Dstlc_Prsr_num','v009_Sstlc_Prsr_num','v011_Wght_oz_num','v008_Hght_cm_num','v010_Tmprtr_F_num');
# transformed variables to analyze
trvarsofinterest <- c('age_tr_fac','zbmi_tr_num');
# time variable
timevar <- 'age_at_visit_days';
eventvar <- 'fracsprain_tr_fac';

## Transformations to do on ALL data.frames in this project
datatrans <- alist(
  age_tr_fac = cut(timevar,c(0,730,2190,4380,6570,7665,17885,Inf),labels=c('0-2','2-6','6-12','12-18','18-21','21-49','49+')),
  start_date = as.Date(start_date),
  birth_date = as.Date(birth_date),
  numvis_tr_num = ave(age_at_visit_days,patient_num,FUN=length),
  histlen_tr_num = ave(age_at_visit_days,patient_num,FUN=function(ii) if(length(ii)==1) 0 else diff(range(ii))),
  # these will vary from project to project, because BMI won't always have 
  # the prefix v005 (though the abbreviated name following that prefix should
  # be stable for a given site unless they change the NAME_CHAR in the ontology
  # in this one we are culling out the impossible BMI values likely caused by 
  # unit errors
  v005_Bd_Ms_Indx_num = ifelse(v005_Bd_Ms_Indx_num<12|v005_Bd_Ms_Indx_num>75,NA,v005_Bd_Ms_Indx_num),
  # zbmi_num are the BMIs converted to z-scores normalized to age and sex
  zbmi_tr_num = ifelse(v005_Bd_Ms_Indx_num<12|v005_Bd_Ms_Indx_num>75,NA,
                       y2z(v005_Bd_Ms_Indx_num,age_at_visit_days/365.25,
                           sex=toupper(sex_cd),ref=cdc.bmi)),
  # this one is highly project specific; we are creating a single T/F factor out
  # of three columns of factors each of which have _many_ levels.
  fracsprain_tr_fac = factor(v001_FRCTR_LWR_LMB!=''|v004_Sprns_strns_kn!=''|v003_Sprns_strns_ankl!='')
);


frac <- read.csv(fracfile,header = T);
well <- read.csv(wellfile,header = T);

fractr <- do.call(transform,c(list(frac),datatrans));
welltr <- do.call(transform,c(list(well),datatrans));

# preliminary, very dirty, data dictionary; maybe more general methods will emerge
frdict <- data.frame(name=colnames(fractr),class=sapply(fractr,function(ii) class(ii)[[1]])
                     ,stringsAsFactors = F);
# initialize the role column
frdict$role <- frdict$class;
# change the roles for the known to be not-really-variables columns to 'meta'
frdict[grepl(metavarsmatch,frdict$name)|frdict$name %in% metavars,'role']<-'meta';
frdict[frdict$name %in% groupvars] <- 'grouping';
frdict[frdict$name %in% rawvars] <- 'raw';
frdict[frdict$name %in% respvars] <- 'response';

follow60 <- byunby(fractr,list(fractr$patient_num),findrange,fstart=fracsprain_tr_fac=='TRUE',fend=age_at_visit_days>(age_at_visit_days[1]+60),val=T);
untilfrac <- byunby(fractr,list(fractr$patient_num),findrange,fstart=T,fend=c(F,fracsprain_tr_fac=='TRUE'),val=T);

# function to collapse first visit and followup for 'simple' time-to-event
# (for use within byunby)
to1event<-function(xx,vars=c(varsofinterest,trvarsofinterest)
                        ,tvar=timevar,rvar=eventvar
                        ,trueevent='TRUE'){
  # xx  : data-frame with multiple visits per subject
  # vars : vector of strings naming columns of interest
  # timevar : name of the column containing the time variable (absolute)
  # respvar : name of the column containing the response variable (event)
  # trueevent : what the the event needs to equal to be non-censored
  if(nrow(xx)<1) browser();
  startage<-min(xx[,tvar]);
  events<-xx[,rvar]==trueevent;
  followup<-if(any(events)) c(event=min(xx[events,tvar])-startage,cen=1) else{
    c(event=max(xx[,tvar])-startage,cen=0);
  }
  return(data.frame(c(xx[xx[,tvar]==startage,vars],startage=startage,as.list(followup))));
}
# here is how we use it with byunby to create a coxph-ready data frame:
# (for TSCI5050)
# foo<-byunby(untilfrac,untilfrac[,'patient_num'],to1event);
# frethnos<-subset(fractr[,c('patient_num','v007_Ethnct')],v007_Ethnct!='');
# for(ii in foo$patient_num) 
#   foo[foo$patient_num==ii,'v007_Ethnct']<-frethnos[frethnos$patient_num==ii,'v007_Ethnct'];
# levels(foo$v007_Ethnct)<-gsub('\"','',gsub('\\"DEM\\|ETHNICITY:','',levels(foo$v007_Ethnc)));
# ...and the equivalent for non-fracture patients, using welltr after doing the following...
# welltr$fracsprain_tr_fac<-'FALSE'
# goodfoo <- na.omit(subset(foo,event>2));
# then sample from bar made from welltr as above and rbind
# and sample with replacement again from combined dataframe
# goodfoobar<-goodfoobar[sample(1:nrow(goodfoobar),500,rep=T),];
# obfuscate numeric vars...
# the ranges on the random perturbations are partly trial and error, partly from this...
# sapply(goodfoobar[,c('v000_Pls_num','v002_Rsprtn_Rt_num','v006_Dstlc_Prsr_num','v009_Sstlc_Prsr_num','v011_Wght_oz_num','v008_Hght_cm_num','v010_Tmprtr_F_num','zbmi_tr_num','startage','event')],function(xx) {uq<-unique(diff(sort(xx)));min(uq[uq>0])})

# goodfoobar$v000_Pls_num <-goodfoobar$v000_Pls_num+sample(-1:1,nrow(goodfoobar),rep=T);
# goodfoobar$v002_Rsprtn_Rt_num <-goodfoobar$v002_Rsprtn_Rt_num+sample(-.5:.5,nrow(goodfoobar),rep=T);
# goodfoobar$v006_Dstlc_Prsr_num <-goodfoobar$v006_Dstlc_Prsr_num+sample(-.5:.5,nrow(goodfoobar),rep=T);
# goodfoobar$v009_Sstlc_Prsr_num <-goodfoobar$v009_Sstlc_Prsr_num+sample(-1:1,nrow(goodfoobar),rep=T);
# goodfoobar$v010_Tmprtr_F_num <-goodfoobar$v010_Tmprtr_F_num+sample(-.05:.05,nrow(goodfoobar),rep=T);
# goodfoobar$event<-goodfoobar$event+sample(-5:5,nrow(goodfoobar),rep=T);
# goodfoobar$startage<-goodfoobar$startage+sample(-4:4,nrow(goodfoobar),rep=T);
# where to stop followup?
# max(subset(goodfoobar,cen==1)$event)

# useful little diagnostic plot for survival
binsurv<-function(xx,binvar,fn=survfit,event='event',cen='cen',...){
  xx$bin<-cut(xx[,binvar],c(0,median(xx[,binvar]),Inf));
  form<-formula(paste0('Surv(',event,',',cen,')~bin'));
  out<- fn(form,xx); out$call$formula <- eval(form);
  out;
}

tmp = quote(v000_FRCTR_LWR_LMB != ""| v001_Sprns_strns_kn != ""| v002_Sprns_strns_ankl != "");
ntmp = quote(!(v000_FRCTR_LWR_LMB != "" | v001_Sprns_strns_kn != "" | v002_Sprns_strns_ankl !=""))
tmp2 = quote(age_at_visit_days > 6000);

#by
by(goo, goo[,"patient_num"], FUN = findrange, fstart = tmp2 )
x = byunby(frac, frac[ , "patient_num"], FUN = findrange, fstart = tmp2, lead = -1);
View(x)

#findrange
x = byunby(frac, frac[ , "patient_num"], FUN = findrange, fstart = tmp2,fend = "v003_Bd_Ms_Indx_num < 26", strict = T)

#rangeAfter
x = byunby(frac, frac[ , "patient_num"], FUN = rangeAfter, 60, "start_date", "<=", T, T, fstart = tmp, fend= ntmp, clip = T, strict = T)
y = byunby(frac, frac[ , "patient_num"], FUN = rangeAfter, 60, "age_at_visit_days", "<=", T, T, fstart = tmp, fend= ntmp, clip = T, strict = T)
all.equal(x, y) #all but the "start_date" column which has been converted from a facto to a date.
#rangeAfter- BMI (to handle adjustments for NAs)
x = byunby(frac, frac[ , "patient_num"], FUN = rangeAfter, 2, "v003_Bd_Ms_Indx_num", ">=", T, T, fstart = tmp, fend= ntmp, clip = T, strict = T)
