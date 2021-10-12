#Target 1: calculate monthly average temperature
# Total sites: 3403
# Total days from 1981-2019: 14975
library(data.table)
library(epitools)
library(lubridate)

setwd("3_results/Weather data/SLC_Weather_WRH")
site.name<-list.files(pattern="*.txt",recursive=TRUE)
#print(object.size(a),units="Mb")

n.site<-3403
day.t<-14975

#find the station ID list
name.list<-strsplit(site.name,split = "/")
#remove to first two years, ie. 1979-1980, only 39 years to use (1981-2019)
yr <- data.frame(start=c(3L,3L+41L*(1L:3402L)), len=rep(39,3403))
yrs<-sequence(yr$len) + rep(yr$start-1, yr$len)
#obtain stations' ID
name.l<-vector()
for (i in yrs){
  name.l[i]<-name.list[[i]][1]
}
name.l<-unique(name.l)[2:3404]
####

#Create list to store temperature data
temp<- vector("list", length(site.name))
####

#read data
 time.s<-Sys.time()
for (i in yrs) {
  temp[[i]] <- fread(site.name[i],select=c(1:3)) # read data from txt
}
 time.f<-Sys.time()
 # time.f-time.s For 30% files, it was 3.4 mins
 #for all files, it was 12.2 mins
DF<-rbindlist(temp)
setDF(DF) #To convert to data.frame
colnames(DF)<-c("date","maxT","minT")
####

#replace J day to date and obtain month
dates<-rep(seq(as.Date("1981-1-1"), as.Date("2019-12-31"), by = "days"), n.site)
aa<-format(dates,format="%m-%d")
DF[,1]<-dates
DF[,4]<-month(as.POSIXlt(DF[,1],format="%Y-%m-%d"))
DF[,5]<-aa 

#calculate daily mean temperature
DF[,1]<-(DF[,2]+DF[,3])/2
DF<-subset(DF,select=-c(maxT,minT))

#assign station ID to the DF
DF[,4]<-rep(name.l,each=14244)
colnames(DF)<-c("avgT","month","monthdate","ID")

#calculate monthly average temperature
T.avg<-vector()
T.avg<-tapply(DF$avgT,list(DF$month,DF$ID),mean)

#calculate daily average temperature
T.avg.d<-tapply(DF$avgT,list(DF$monthdate,DF$ID),mean)

#export
write.table(T.avg,file="C:/AAFC/Project 1_MCF/3_results/Weather data/monthly avg T.txt", sep="\t")
write.table(T.avg.d,file="C:/AAFC/Project 1_MCF/3_results/Weather data/daily avg T.txt", sep="\t")

