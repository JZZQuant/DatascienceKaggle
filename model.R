#unzip("kaggletrain.csv.zip")
df<-read.csv("train.csv",stringsAsFactors=T)
df.numeric<-df[,sapply(df, class) =="numeric"]
df.integer<-df[,sapply(df, class) =="integer"]
df.taget<-df.integer[,1869]
df.integer<-df.integer[,1:1868]
df.factor<-df[,sapply(df, class) =="factor"]
df.factor<-data.frame(sapply(df.factor,function(f) as.integer(f)))
df.data<-cbind(df.numeric,df.integer,df.factor)
rm(df,df.numeric,df.integer,df.factor)

#dimensionality reduction
df.data<-df.data[,sapply(df.data, function(v) var(v, na.rm=TRUE)!=0)]
df.data<-df.data[,sapply(df.data, function(v) abs(cor(v,df.taget, use="pairwise.complete.obs"))>0.1)]
for(i in 1:297)
{
  df.temp<-df.data[,(i+1):ncol(df.data)]
  df.hit<- df.data[,i]
  temp<-sapply(df.temp, function(v) abs(cor(v,df.hit, use="complete.obs"))>0.90)
  if(sum(temp,na.rm=T)>0)
  {temp[is.na(temp)]<-0
  temp<-as.logical(temp)
  df.data<-df.data[,!(names(df.data) %in% names(df.temp)[temp])]}
  if(ncol(df.data)<=i+2) break
}
df.data<-amelia(df.data,incheck = FALSE,m=1)$imputations$imp1
prc <- prcomp(df.data, center=TRUE, scale=TRUE)
varimax7 <- varimax(prc$rotation[,1:15])
newData <- scale(x) %*% varimax7$loadings
newData <- as.data.frame(cbind(newData,df.taget))

#unzip("test.csv.zip")
df.test<-read.csv("test.csv",stringsAsFactors=T)
submission <- data.frame(ID=df.test$ID)
df.numeric.test<-df.test[,sapply(df.test, class) =="numeric"]
df.integer.test<-df.test[,sapply(df.test, class) =="integer"]
df.factor.test<-df.test[,sapply(df.test, class) =="factor"]
df.factor.test<-data.frame(sapply(df.factor.test,function(f) as.integer(f)))
df.data.test<-cbind(df.numeric.test,df.integer.test,df.factor.test)
rm(df.test,df.numeric.test,df.integer.test,df.factor.test)
#dimensionality reduction on test data
df.data.test<-df.data.test[,names(df.data)]
df.data.test<-amelia(df.data.test,incheck = FALSE,m=1)$imputations$imp1
newData.test <- scale(df.data.test) %*% varimax7$loadings
newData.test <- as.data.frame(newData.test)

n <- names(newData)
f <- as.formula(paste("df.taget ~", paste(n[!n %in% "df.taget"], collapse = " + ")))
mylogit <- randomForest(f, data = newData)

submission$target <- predict(mylogit,newData.test)

#submission
submission$target<-as.numeric(submission$target >0.25)
write.csv(submission, "decision.csv", row.names = FALSE)
