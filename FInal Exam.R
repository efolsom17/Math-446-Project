#Porblem 2
#Initializing Data
Hunter_L_x=c(46.4,45.9,41.9,53.3,42.7,45.1,50.7,44.2,60.1)
Panel_Score_y=c(2.6,3.1,2.5,5,3.6,3.8,5.2,3.2,3.8)
cor(Hunter_L_x,Panel_Score_y,method=c("pearson"))
cor(Hunter_L_x,Panel_Score_y,method=c("spearman"))
mean(Hunter_L_x)
mean(Panel_Score_y)
cov(Hunter_L_x,Panel_Score_y)
var(Hunter_L_x)
var(Panel_Score_y)
  

#Problem 3
library(dplyr)
trt1<-c(8,10,12,13,11)
trt2<-c(2,6,7,11,5)
trt3<-c(4,10,9,8,10)
trt4<-c(3,5,9,10,6)
trt5<-c(9,7,5,5,3)
Xi<-c(10.8,6.2,8.2,6.6,5.8)
Xj<-c(5.2,7.6,8.4,9.4,7)
X<-7.5
data<-as.data.frame(rbind(trt1,trt2,trt3,trt4,trt5))
colnames(data)<-c("Block1","Block2","Block3","Block4","Block5")
View(data)
#Fobs=SSTM
Fobs=SSTM=sum(Xi^2)
Fobs
#storage vector to speed up the process
Fstars<-double(length(data$Block1))
#permuting the observations within each block
for(i in 1:length(data$Block1)){
  block<-data[,i]
  Fstar<-sum(block^2)
  Fstars[i]<- Fstar
  
}
pval<-sum(Fstars>Fobs)/length(data$Block1)

pval