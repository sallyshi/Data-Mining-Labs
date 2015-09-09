#getwd()
setwd('/Users/sallyshi/Dropbox (Personal)/ORIE 4740')
olive_train=read.table('olive-train.dat')
olive_test=read.table('olive-test.dat')
#ls()
#r=4
#rm(r)
#head(olive_train)
#tail(olive_train)
#olive_train[1:10,]
#colnames(olive_train)=c(5:14)
#colnames(olive_train)=c(9:12)
#qwe=read.table('olive-train.dat',col.names=c(11:20))
#qwe=read.table('olive-train.dat',T)

colnames(olive_train)=c("Region","Area","Palmitic_Acid_Pt","Palmitoleic_Acid_Pt","Stearic_Acid_Pt","Oleic_Acid_Pt","Linoleic_Acid_Pt","Linolenic_Acid_Pt","Arachidic_Acid_Pt","Eicosenoic_Acid_Pt")
#dim(olive_train)
olive_train$Region=as.factor(olive_train$Region)
olive_train$Area=as.factor(olive_train$Area)
#is.numeric(olive_train$Palmitic_Acid_Pt)
#is.numeric(olive_train$Palmitoleic_Acid_Pt)
#is.numeric(olive_train$Stearic_Acid_Pt)
#is.numeric(olive_train$Oleic_Acid_Pt)
#is.numeric(olive_train$Linoleic_Acid_Pt)
#is.numeric(olive_train$Linolenic_Acid_Pt)
#is.numeric(olive_train$Arachidic_Acid_Pt)
#is.numeric(olive_train$Eicosenoic_Acid_Pt)
#is.character(olive_train$Palmitic_Acid_Pt)
#is.factor(olive_train$Region)
#is.factor(olive_train$Area)
#levels(olive_train$Region)
#is.na(olive_train$Region)
#is.na(olive_train$Oleic_Acid_Pt)
#sum(is.na(olive))
#summary(olive$Region)
#summary(olive$Area)

d=density(olive_train$Palmitic_Acid_Pt)
plot(d, xlab='Palmitic Acid %',type="l", main='Palmitic Acid Density Plot')
e=density(olive_train$Palmitoleic_Acid_Pt)
plot(e, xlab='Palmitoleic Acid %',type="l", main='Palmitoleic Acid Density Plot')
f=density(olive_train$Stearic_Acid_Pt)
plot(f,  xlab='Stearic Acid %',type="l", main='Stearic Acid Density Plot')
g=density(olive$Oleic_Acid_Pt)
plot(g,  xlab='Oleic Acid %',type="l", main='Oleic Acid Density Plot')
h=density(olive$Linoleic_Acid_Pt)
plot(h,  xlab='Linoleic Acid %',type="l", main='Linoleic Acid Density Plot')
i=density(olive$Linolenic_Acid_Pt)
plot(i,  xlab='Linolenic Acid %',type="l", main='Linolenic Acid Density Plot')
j=density(olive$Arachidic_Acid_Pt)
plot(j,  xlab='Arachidic Acid %',type="l", main='Arachidic Acid Density Plot')
k=density(olive$Eicosenoic_Acid_Pt)
plot(k, xlab='Eicosenoic Acid %',type="l", main='Eicosenoic Acid Density Plot')


par(mfrow=c(3,1))
d1=density(olive_train$Eicosenoic_Acid_Pt[olive_train$Region==1])
plot(d1, xlab='Eicosenoic %',type="l", xlim=c(0,60), main='Eicosenoic Acid Density Plot - Region 1')
d2=density(olive_train$Eicosenoic_Acid_Pt[olive_train$Region==2])
plot(d2, xlab='Eicosenoic Acid %',type="l", xlim=c(0,60), main='Eicosenoic Acid Density Plot - Region 2')
d3=density(olive_train$Eicosenoic_Acid_Pt[olive_train$Region==3])
plot(d3, xlab='Eicosenoic Acid %',type="l", xlim=c(0,60), main='Eicosenoic Acid Density Plot - Region 3')



olivePred= function(dat) {
  nData = dim(dat)[1]
  predRegion = rep(0, nData)
  for (i in (1:nData)) {
    predRegion[i]=
      if (dat[i,6]>7500){ 
        predRegion[i]=3;
      }
    else if (dat[i,6]<7200) { 
      predRegion[i]= 1;
    }
    else {
      predRegion[i]=2;
  }}
  return (predRegion)
}
olivePred(olive_train)



oliveAccuracy=function(dat){
  pred=olivePred(dat)
  regTable=table(dat[,1] , pred)
  errorRate = (regTable[1,2] + regTable[1,3] + regTable[2,1] + regTable[2,3]+regTable[3,1]+regTable[3,2])/sum(regTable)
    return(list(accuracyTable=regTable, errorRate = errorRate))
}
oliveAccuracy(olive)


oliveRegRem = olive[olive$Region !=1,]
#2=red, 3=green
plot(oliveRegRem$Arachidic_Acid_Pt, oliveRegRem$Linoleic_Acid_Pt, col=oliveRegRem$Region, xlab= 'Arachidic Acid %', ylab='Linoleic Acid %', main = 'Scatter Plot of Arachidic Acid vs Linoleic Acid' )

legend("topleft", legend=c("Region 2","Region 3"), pch=1)

#intercept, slope
abline(1150, -2)



olivePred=function(dat) {
  nData = dim(dat)[1]
  predRegion = rep(0, nData)
  for (i in (1:nData)) {
    predRegion[i]=
      if (dat[i,10]>5) {
        predRegion[i]=1;
      }
       else if (dat[i,7]>-2*(dat[i,9]) + 1150){ 
        predRegion[i]=2;
      }
    else {
      predRegion[i]=3;
    }}
  return (predRegion)
}
olivePred(olive)



oliveAccuracy=function(dat){
  pred=olivePred(dat)
  regTable=table(dat[,1] , pred)
  errorRate = (regTable[1,2] + regTable[1,3] + regTable[2,1] + regTable[2,3]+regTable[3,1]+regTable[3,2])/sum(regTable)
  return(list(accuracyTable=regTable, errorRate = errorRate))
}
oliveAccuracy(olive)
