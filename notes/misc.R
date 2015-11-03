if(!require(AGD)) install.packages('AGD');
library(AGD);
library(Rcasematch)
library(sqldf)
library(survival)
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
  age_tr_fac = cut(age_at_visit_days,c(0,1095,2190,3285,4380,5475,6570,7665,17885,Inf),labels=c('0-3','3-6','6-9','9-12','12-15','15-18','18-21','21-49','49+')),
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
  centered_age = scale(age_at_visit_days,scale=F)[,1],
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
## TEMPORARY WORK AROUND -- R is not importing the fracture columns for the well visits as strings, but as boolean values.
welltr$fracsprain_tr_fac = as.factor("FALSE")

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
count(matchFirstFrac, c("CaseControl","age_tr_fac"))

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
frdict$name <- rownames(frdict)
matchFFnum =  matchFirstFrac[,subset(frdict,role =='numeric')$name]
x = tabler(matchFFnum) #produces N, Mean, SD, and Range
View(x)
vif(matchFFnum)
#vif shows no significant inflation between unexpected variables. (expected BMI, Height, Weight, zBMI)
analyze.Heat(matchFFnum)
analyze.Constellation(matchFFnum)
analyze.Pairwise(matchFFnum)
#reduce problem: 
matchFFFin = matchFirstFrac[, c("zbmi_tr_num", "v000_Pls_num", "age_at_visit_days", "CaseControl")]
ls =lm(formula(matchFFFin), matchFFFin)
summary(ls)
plot(ls)

matchFUnum =  matchFollowup[,subset(frdict,role =='numeric')$name]
x = tabler(matchFUnum)

matchFUnum$patient_num = matchFollowup$patient_num
matchFUnum$CaseControl = matchFollowup$CaseControl

matchFUnum = byunby(matchFUnum, matchFUnum[ , "patient_num"], FUN = timeLapse, "age_at_visit_days")
ggplot(matchFUnum) + geom_line(aes(Lapse, zbmi_tr_num, group = patient_num, color = CaseControl))
ggplot(matchFUnum) + geom_line(aes(Lapse, zbmi_tr_num, group = patient_num, color = CaseControl))
ls = lm(zbmi_tr_num ~ Lapse + CaseControl, matchFUnum)
summary(ls)
plot(ls)
ls = lm(zbmi_tr_num ~ CaseControl, matchFUnum)
summary(ls)
scatterplot(zbmi_tr_num ~ Lapse, data=matchFUnum)
y = matchFUnum[!is.na(matchFUnum$zbmi_tr_num),]
ls = lm(y$zbmi_tr_num ~ y$CaseControl + factor(y$patient_num))

pls = plm(zbmi_tr_num ~ CaseControl, data = y, index = c("patient_num","Lapse"), model="within")

zz = sqldf("SELECT * FROM matchFUnum WHERE matchFUnum.patient_num IN (SELECT patient_num FROM(SELECT patient_num, COUNT(*) AS cnt FROM matchFUnum WHERE zbmi_tr_num IS NOT 'NA' GROUP BY patient_num) WHERE cnt >2)")

pls = plm(zbmi_tr_num ~ CaseControlnum, data - zz, index = c("patient_num","Lapse"), model="within")

###### Survival Analysis
matchFracUntil = byunby(matchFracUntil, matchFracUntil[ , "patient_num"], FUN = timeLapse, "age_at_visit_days")
ggplot(matchFracUntil) + geom_line(aes(Lapse, zbmi_tr_num, group = patient_num, color = CaseControl))
ggplot(matchFracUntil) + geom_line(aes(age_at_visit_days, zbmi_tr_num, group = patient_num, color = CaseControl))

timeBase = getTimeSeriesBases(rbind(fractr, welltr), matchFracUntil$patient_num)
tmp = byunby(matchFracUntil, matchFracUntil[ , "patient_num"], FUN = timeLapse, type = 'S', eventCol = "fracsprain_tr_fac")
timeline = data.frame(patient_num = tmp$patient_num, tstart = tmp$tstart, tstop = tmp$tend, event = tmp$event)
newd = tmerge(data1 = timeBase, data2 = timeline, id=patient_num, tstart = tstart, tstop = tstop)
newd2 = tmerge(newd, timeline, id = patient_num, fract = event(event))
attr(newd2, "tcount")