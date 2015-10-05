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
