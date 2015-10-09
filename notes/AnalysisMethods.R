library(ggplot2)
library(spdep)
library(reshape)
library(fmsb)
library(psy)

plotlgb2toScatter<- function(var1, var2, title,labels = c("X Axis", "Y Axis"), sub = ""){
  #plots two variables in a scatter plot- log transforms the varaibles first.
  lgVar1 = log(var1)
  lgVar2 = log(var2)
  #clean up infinities - convert to 0
  lgVar1 = replace(lgVar1, lgVar1 == -Inf, 0)
  lgVar2 = replace(lgVar2, lgVar2 == -Inf, 0)
  plot(lgVar1, lgVar2, main = title, sub = sub, xlab = labels[1], ylab = labels[2])
  abline(lm(lgVar2 ~ lgVar1), col = "violet")
  lines(lowess(lgVar1, lgVar2), col = "skyblue")
  data.frame(cor(lgVar1, lgVar2),  cov(lgVar1, lgVar2))
}

lmCE <- function(var1, varOn, logTransform = T){
  if(logTransform){
    ls <- lm(formula = replace(log(varOn), log(varOn)== -Inf, 0) ~ replace(log(var1), log(var1)== -Inf, 0))
  }else{
    ls = lm(formula = varOn ~ var1)
  }
  ls
}
#fillLRTable <- function(listlm, varOn){
#  x = matrix(nrow = (length(listlm) + 1), ncol = 2)
#  for( i in 1:length(listlm)){
#    #' step through them running both a log and a non log version of the single variable
#    #' analysis. Can utilize lmCE2toStr1 to print coefficients and p value all in one neat package. :)
#    ls = lmCE(varOn, listlm[[i]], logTransform = T)
#    x[i+1,1] = lmCE2toStr1(ls)
#    ls = llmCE(listlm[[i]], varOn, logTransform = F)
#    x[i+1,2] = lmCE2toStr1(ls)
#  }
#  x
#}
llmCE2 <- function(var1, var2, varOn, logTransform = T){
  if(logTransform){
    lsd <- lm(formula = replace(log(varOn), log(varOn)== -Inf, 0) ~ replace(log(var1), log(var1)== -Inf, 0) + replace(log(var2), log(var2)== -Inf, 0))
  }else{
    lsd <- lm(formula = varOn ~ var1 + var2)
  }
  lsd
}
lmCE2toStr1 <- function(ls){
  #Pulls the first variable in a 2 variable lm(y~ x +z) (x) and returns (Coefficient, (confidence interval) P Value)
  p = ''
  if(paste(round(summary(ls)$coefficients[2,4], digits=3)) == '0'){
    p= 'P < .001'
  }else{
    p = paste("P = ", round(summary(ls)$coefficients[2,4], digits=3))
  }
  b = paste(round(summary(ls)$coefficients[2,1], digits=2), "(", round(confint(ls)[2, 1], digits=2), ",", round(confint(ls)[2,2], digits=2), ") ", p, sep = "")
  b
}
lmCE2toStr2 <- function(ls){
  p = ''
  if(paste(round(summary(ls)$coefficients[3,4], digits=3)) == '0'){
    p= 'P < .001'
  }else{
    p = paste("P = ", round(summary(ls)$coefficients[3,4], digits=3))
  }
  c = paste(round(summary(ls)$coefficients[3,1], digits=2), "(", round(confint(ls)[3, 1], digits=2), ",", round(confint(ls)[3,2], digits=2), ") ", p, sep = "")
  c
}

fillLmCE2Table <- function(listlm, lmOn, logTransform = T){
  #run a linear regression model on all items in listlm, printing them out in a matrix.
  x = matrix(nrow = length(listlm), ncol = length(listlm))
  for(i in 1:length(listlm)){
    j = 1+i
    while(j <= length(listlm)){
      ls = llmCE2(listlm[[i]], listlm[[j]], lmOn, logTransform)
      x[i,j] = lmCE2toStr1(ls)
      x[j,i] = lmCE2toStr2(ls)
      j = j + 1
    }
  }
  x
}
delchars = function(str,n,lead=TRUE){
  dots = paste(rep('.',n),collapse='')
  pat = if(lead)paste('^',dots,sep='') else paste(dots,'$',sep='')
  sub(pat,'',str)
}
coefLM = function(ls){
  summary(ls)$coefficients[2,]
}
confIntLM= function(ls){
  confint(ls)[2, ]
}
tableLM = function(lmList){
  coef = apply(lmList,lmList, FUN = coefLM(lmList))
  cint = apply(lmList,lmList, FUN =confIntLM(lmList))
  list <- data.frame(coef, cint)
}  
tabler = function(inframe){
  #' This takes a dataframe of values and prints out their names
  #' N, mean, sd and range in different collumns. 
  a = matrix(ncol = 5, nrow = ncol(inframe))
  i = 1
  for(var in names(inframe)){
    a[i,1] = var
    a[i,2] = sum(complete.cases(inframe[,i]))
    a[i,3] = mean(inframe[,i], na.rm = TRUE)
    a[i,4] = sd(inframe[,i], na.rm = TRUE)
    a[i,5] = paste(min(inframe[,i], na.rm = TRUE), ' - ', max(inframe[,i], na.rm = TRUE))
    i = i+1
  }
  a
}
analyze.Heat <- function(frame, ...){
  #accepts a dataframe, converts it into a data matrix and prints it in a heat map
  dbcor <- cor(data.matrix(frame), use='pairwise')
  heatmap(dbcor,symm=T)
}
analyze.Constellation <- function(frame, ...){
  dbnum <- frame
  dbnum[is.na(dbnum)]<-0
  sphpca(data.matrix(dbnum),nbsphere=1,v=200)
}
analyze.Pairwise<- function(frame, ...){
  plot(frame, pch=16, cex=.3, col="#00000010")
}

vif <- function(dem.lm){
    #accepts a dataframe and will return the VIF value for all numerical values in the dataframe
    #non-numerical values will return NA
  vif_vals <-NULL
  for(val in names(dem.lm)){
    lmfunc <- formula(paste(val,'~ .'))
    vif_add <- VIF(lm(lmfunc, data=dem.lm))
    vif_vals <- rbind(vif_vals,c(val,vif_add))}
  vif_vals
}