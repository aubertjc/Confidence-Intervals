# Confidence-Intervals
http://rpubs.com/jaubert148/132497
library(downloader) 
#LCXY is a file on my computer
LCXY<-read.csv(file.choose(), header=TRUE)
head(LCXY)
names(LCXY)
Luncap<-select(LCXY,LungCapacity)%>%unlist
mu_LC<-mean(Luncap)
print(mu_LC)
N<-30
LC<-sample(Luncap, N)
print(mean(LC))
se<-sd(LC/sqrt(N))
print(se)
Q<-qnorm(1-0.05/2)
interval<-c(mean(LC)-Q*se, mean(LC)+Q*se)
interval
interval[1]<mu_LC&interval[2]>mu_LC
B<-250
plot(mean(Luncap)+c(-7,7),c(1,1),type="n", xlab = "LungCapacity", ylab="interval",xlim=c(5000, 5800), ylim=c(1,B), main = "Conf. Int")
abline(v=mean(Luncap))
for(i in 1:B){
  LC<-sample(Luncap, N)
  se<-sd(LC/sqrt(N))
  interval<-c(mean(LC)-Q*se, mean(LC)+Q*se)
  cobered<-
    mean(LC)<=interval[2] & mean(LC)>=interval[1]
  color<-ifelse(covered,1,2)
  lines(interval, c(i,i), col=color)
  }
