tmp = quote(v000_FRCTR_LWR_LMB != ""| v001_Sprns_strns_kn != ""| v002_Sprns_strns_ankl != "");
tmp2 = quote(age_at_visit_days > 6000);


by(goo, goo[,"patient_num"], FUN = findrange, fstart = tmp2 )
x = byunby(frac, frac[ , "patient_num"], FUN = findrange, fstart = tmp2, lead = -1);
View(x)

x = byunby(frac, frac[ , "patient_num"], FUN = findrange, fstart = tmp2,fend = "v003_Bd_Ms_Indx_num < 26", strict = T)
