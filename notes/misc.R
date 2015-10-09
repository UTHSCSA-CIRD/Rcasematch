if(!require(AGD)) install.packages('AGD');
library(AGD);
library(Rcasematch)
## File locations (change to correct local paths)
fracfile <- 'manuells_fracV2.2.csv';
wellfile <- 'manuells_WellVisitV2.2.csv';


## You'll want to edit these column names for each project
respvars <- 'zbmi_tr_num';
groupvars <- 'patient_num';
## Especially these!!
rawvars <- c('v001_FRCTR_LWR_LMB','v003_Sprns_strns_ankl','v004_Sprns_strns_kn','v005_Bd_Ms_Indx_num','v007_Ethnct','v008_Hght_cm_num','v011_Wght_oz_num')
## but these are probably pretty stable for DataFinisher output
metavarsmatch <- ".*(_info|_inactive)";
metavars <- c('start_date','birth_date');

## Transformations to do on ALL data.frames in this project
datatrans <- alist(
  age_tr_fac = cut(age_at_visit_days,c(0,730,2190,4380,6570,7665,17885,Inf),labels=c('0-2','2-6','6-12','12-18','18-21','21-49','49+')),
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

tmp = quote(fracsprain_tr_fac=='TRUE');
ntmp = quote(fracsprain_tr_fac=='FALSE')
tmp2 = quote(age_at_visit_days > 6000);

#by
by(goo, goo[,"patient_num"], FUN = findrange, fstart = tmp2 )
x = byunby(frac, frac[ , "patient_num"], FUN = findrange, fstart = tmp2, lead = -1);
View(x)

#findrange
#Clip F
x = byunby(fractr, fractr[ , "patient_num"], FUN = findrange, fstart = tmp2,fend = "v005_Bd_Ms_Indx_num < 26", strict = T, clip = F, val = T)
x = byunby(fractr, fractr[ , "patient_num"], FUN = findrange, fstart = tmp,fend = fstart, strict = T, clip = F, val = T, trail = 2)
#clip T
x = byunby(fractr, fractr[ , "patient_num"], FUN = findrange, fstart = tmp,fend = ntmp, strict = T, clip = T, val = T, trail = 1)
x = byunby(fractr, fractr[ , "patient_num"], FUN = findrange, fstart = tmp,fend = ntmp, strict = T, clip = T, val = T)
sum(x$fracsprain_tr_fac== FALSE)

#rangeAfter
#x = byunby(frac, frac[ , "patient_num"], FUN = rangeAfter, 60, "start_date", "<=", T, T, fstart = tmp, fend= ntmp, clip = T, strict = T)
#y = byunby(frac, frac[ , "patient_num"], FUN = rangeAfter, 60, "age_at_visit_days", "<=", T, T, fstart = tmp, fend= ntmp, clip = T, strict = T)
#all.equal(x, y) #all but the "start_date" column which has been converted from a facto to a date.
#rangeAfter- BMI (to handle adjustments for NAs)
#x = byunby(frac, frac[ , "patient_num"], FUN = rangeAfter, 2, "v003_Bd_Ms_Indx_num", ">=", T, T, fstart = tmp, fend= ntmp, clip = T, strict = T)

##  MATCHING WELL TO FRACTURES  ##
firstfrac = byunby(fractr, fractr[ , "patient_num"], FUN = findrange, fstart = tmp,fend = T, clip = F, val = T)

matched = matcher(firstfrac,welltr,c("sex_cd", "age_tr_fac"))
matchFirstFrac = sampler(100,matched$matches,firstfrac,welltr)


followupFrac = byunby(fractr, fractr[ , "patient_num"], FUN = rangeAfter, fstart = "fracsprain_tr_fac == 'TRUE'", 60, "age_at_visit_days", fend = T, clip = F, requireVisit = T)
followupWell = byunby(welltr, welltr[ , c("patient_num","age_tr_fac")], FUN = rangeAfterRecord, 60, "age_at_visit_days")
matchFollowup = sampler(100, matched$matches,followupFrac, followupWell, matchType = 'A', inclusion = "N")

#Time until
fracTimeUntil = x = byunby(fractr, fractr[ , "patient_num"], FUN = findrange, fstart = T,fend = tmp, strict = T, clip = F, val = T)
matchFracUntil = sampler(100,matched$matches,fracTimeUntil,welltr, matchType = 'A')

wellBMI = welltr[!is.na(welltr$v005_Bd_Ms_Indx_num),]


##########################################
######   Begin Analysis Code #############
##########################################

#find the variable inflation value of independant variables
#Pull usable data
matchFFnum =  matchFirstFrac[,subset(frdict,role =='numeric')$name]

vif(matchFFnum)
#vif shows no significant inflation between unexpected variables. (expected BMI, Height, Weight, zBMI)
analyze.Heat(matchFFnum)
analyze.Constellation(matchFFnum)
analyze.Pairwise(matchFFnum)