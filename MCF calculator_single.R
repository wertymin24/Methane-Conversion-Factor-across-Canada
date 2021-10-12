#library(weathermetrics)
#This is a calculator for single station.
#function to calculate f, rounded
v.hoff<- function(x){
  round(exp((f_Ea*(x-f_T1))/(f_R*x*f_T1)),3)
}
#convert air temp to manure temp.
T.m<-vector()
if (sum(M.rm)>1) {
  for (i in 1:12){
    T.m[i]<-max(T.sel[i],f_Tmin)  
  } 
}else {
  for (i in 1:12){
    T.m[i]<-max((T.sel[i]-f_T2d),f_Tmin)
  }
} 
T.m<-c(T.m[12],T.m[1:11])# a month lag assumed in IPCC 2019.
#conver C to K
T.m.K<-celsius.to.kelvin(T.m)
f.m<-v.hoff(T.m.K)
f.m[13:36]<-rep(f.m[1:12])

#Vs excreted an loaded
VS_month<-rep(VS_Yr/12,each=36)
VS_loaded<-VS_month*(VS_LQD/100)
VS_ava<-vector()
VS_con<-vector()
CH4<-vector()
temp<-vector() 

#Calculate CH4 produced and MCF
for (i in 1:36) {
  if (Manure.rm[i]==0){
    if (i == 1){
      VS_ava[i]<-VS_loaded[i]
      VS_con[i]<-VS_ava[i]*f.m[i]
    } else {
      VS_ava[i]<-VS_loaded[i]+VS_ava[i-1]-VS_con[i-1]
      VS_con[i]<-VS_ava[i]*f.m[i]
    } 
  } else {
    VS_emp<-(VS_ava[i-1]-VS_con[i-1])*(E_eff/100)
    VS_ava[i]<-VS_loaded[i]+((VS_ava[i-1]-VS_con[i-1])*(1-(E_eff/100)))
    VS_con[i]<-VS_ava[i]*f.m[i]
  }
  temp[i]<-VS_con[i]*B0
  CH4.potential<-VS_Yr*B0
  CH4_sel<-round(sum(temp[25:36]),3)
  MCF<-round(CH4_sel/CH4.potential,3)
}

#To know the CH4 production for every month
#print(paste("monthly CH4 in third year",month.abb,round(temp[25:36],1)))
#To print the result
cat(paste("total CH4 in third year:",CH4_sel,"\n"
          ,"MCF:",MCF))