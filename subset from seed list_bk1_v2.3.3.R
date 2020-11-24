
site<-'bk1' ##change site
period<-0
fd<-paste0('c:/SILVA/',site)
setwd(fd)
treedata <- read.delim(paste0(fd,'/',site,'_list_',period), header=TRUE)
treedata1<-subset(treedata,Per==1)
ba <-within(treedata1,ba<-(BHD/2)^2*pi/10000/0.15)
s.ba<-sum(ba$ba)
max.dbh<-max(treedata1$BHD)
#subset hight competition
#tolerance distance
ax=10
bx=50
ay=10
by=30

treedata2<-subset(treedata,Per==1 & x>5 &x<55&y>5 &y<35 &BHD>=7)
treedata.sm<-subset(treedata,Per==1& x>5 &x<55&y>5 &y<35 &BHD<7)
treedata.s20<-subset(treedata,Per==1& x>5 &x<55&y>5 &y<35 &BHD>=7 &BHD<=20)
treedata.s20.1<-subset(treedata,Per==1& x>ax &x<bx&y>ay &y<by &BHD>=7 &BHD<=20)
treedata.gr20<-subset(treedata,Per==1& x>5 &x<55&y>5 &y<35 &BHD>20)
treedata.g20<-subset(treedata,Per==1& x>ax &x<bx&y>ay &y<by &BHD>20)


#####7<DBH<20 control 
#a0<-rbind(treedata.s20.1,all.corner)
a0<-treedata.s20
a1<-subset(a0,BHD<=10)
a2<-subset(a0,BHD>10)

#if (nrow(treedata.s20)<40) {
#treedata.s20<-treedata.s20 
#} else treedata.s20<-subset(treedata.s20, KKL<mean(treedata.s20$KKL)*1.5& Nr>100)

if (nrow(a2)<40 & nrow(treedata.gr20)<30) {
a1.1<-subset(a1,KKL<mean(a1$KKL)*1.2 | Nr<100)#*0.5)
a2.1<-subset(a2,KKL<mean(a2$KKL)*1.6 | Nr<100)
a3<-rbind(a2.1,a1.1)
} else 
  #if (nrow(a2)>50) {
a1.1<-subset(a1,KKL<mean(a1$KKL)*1.2 | Nr<100)
a2.1<-subset(a2,KKL<mean(a2$KKL)*1.4 | Nr<100)
a3<-rbind(a2.1,a1.1)
#} else a3<-a0

result0<-rbind(treedata.gr20,a3) #from a3 to a0
#result0<-rbind(treedata.gr20,all.corner,treedata.s20)
#####
#treedata.small<-subset(treedata, Per==1 & x>5 &x<55&y>5 &y<35 &BHD>=7 &BHD<=20)
#nu.small<-nrow(treedata.small)

if (nrow(treedata.sm)>150) {
mean.ci<-mean(treedata.sm$KKL)
mean.ci2<-mean.ci*0.7
mean.ci3<-mean.ci*1.8
max.ci<-max(treedata.sm$KKL)*0.8
max.ci2<-max(treedata.sm$KKL)*0.98 

} else if (nrow(treedata.g20)<20) {
mean.ci<-mean(treedata.sm$KKL)
mean.ci2<-mean.ci*0.7
mean.ci3<-mean.ci*1.5
max.ci<-max(treedata.sm$KKL)*0.8
max.ci2<-max(treedata.sm$KKL)*0.98 
} else {
mean.ci<-mean(treedata.sm$KKL)
mean.ci2<-mean.ci*0.7
mean.ci3<-mean.ci*1.5
max.ci<-max(treedata.sm$KKL)*0.8 #from 0.98
max.ci2<-max(treedata.sm$KKL)*0.98 
} 

#if (s.ba<50 & max.dbh<=88) {
#CI<-20
#} else CI<-200
#<7cm control
#if (nrow(treedata.s20)<40) {
treedata3<-subset(treedata.sm,KKL<mean.ci3|KKL>max.ci & KKL<max.ci2) # KKL>max.ci & KKL<max.ci2|
#} else treedata3<-subset(treedata.sm,KKL<0.5) #from 0.1 to 0.2
#nr
#CI
#nrow(treedata3)
result<-rbind(result0,treedata3)

##if volume is too large, then remove small trees due to stong ci
test<-subset(result,BHD>=5 & BHD <10)
#nrow(test)
if (nrow(test)<60 & nrow(treedata.gr20)<40) {
result<-result
} else result<-result[!(result$BHD>=5.0 & result$BHD<10 & result$KKL>mean(test$KKL)),]

a=15.589*10^-3
b=0.01696*10^-3
c=0.01883*10^-3
d=0.01883*10^-3
vol=with(result,(a+b*BHD*h^2+d*BHD^3)/0.15*0.6)


##if volume is over 450
#vol=with(data.s2,(a+b*BHD*h^2+d*BHD^3)/0.15*0.6)

############################


big.tree<-subset(treedata, Per==1 &BHD>100)
notrees=nrow(big.tree)
tree.Nr3=c()
tree.Nr4=c()
treedata5=c()

data<-big.tree


# Set number of seedlings to produce per tree
seedlings=1000
# Set seedlings mortality
if (notrees<5 &s.ba<50){
mortality=0.997 
} else mortality=0.999
# Calculate Seedlings surviving per tree
noseedlings=seedlings*(1-mortality)
# Set Period
#period=trees$Per[1]
# Set minimum tree age for reproduction
minage=40
# Set parameters for Gamma function
# parameters should be set to more useful values, e.g. trees$kd
#a=7.5
#b=1

# Set number of trees to use
#treedata<-subset(trees,trees$Per==period)
#data<-subset(trees,trees$Per==period)
seeddata<-c()
 #notrees=nrow(treedata)
#notrees=66

# Plot trees before seedlings

# Calculate tree age
# Set morphometric factor BHD->age
morpfac<-4.143
treeage<-c()
treeage<-data$BHD*morpfac

# For all trees
for (treenumber in 1:notrees) {
# For single tree
{
# Read tree information
tree.Nr<-data$Nr[treenumber]
tree.BHD<-data$BHD[treenumber]
tree.kd<-data$kd[treenumber]
tree.x<-data$x[treenumber]
tree.y<-data$y[treenumber]
seeds<-c()
# parameters should be set to more useful values, e.g. trees$kd
a=tree.kd/2*1.5
b=1
# Produce seedlings according to Gamma distribution
xseed<-c()
yseed<-c()

for (j in 1:noseedlings) {
xseed<-rbind(xseed,rgamma(1, shape = a, scale = b))
yseed<-rbind(yseed,rgamma(1, shape = a, scale = b))
}

# Produce random 0,1 for x and y
rand01x<-c()
rand01y<-c()
for (j in 1:noseedlings) {
  rand01x<-rbind(rand01x,rbinom(1,1,0.5))
  rand01y<-rbind(rand01y,rbinom(1,1,0.5))
}

# Randomly change sign of x- and y-coordinates for seedlings
for (i in 1:noseedlings) {
  if (rand01x[i,1]==0) {
    xseed[i,1]=-xseed[i,1]
  }
  if (rand01y[i,1]==0) {
    yseed[i,1]=-yseed[i,1]
  }
}

# Calculate actual seedling coordinates relative to site geometry
# too simplified, because coordinates should relate to crown not to stem
xseed2<-xseed
for (i in 1:noseedlings) {
  xseed2[i,1]<-xseed[i,1]+data$x[treenumber]
}

yseed2<-yseed
for (i in 1:noseedlings) {
  yseed2[i,1]<-yseed[i,1]+data$y[treenumber]
}
}

# Compile rows to add to tree list
#Per<-c()
Nr<-c()
Art<-c()
BHD<-c()
h<-c()
kra<-c()
kd<-c()
x<-c()
y<-c()
kenn<-c()
NatSchBaum<-c()
#mort<-c()
#KKL<-c()
#NatSchBaum<-c()

for (j in 1:noseedlings) {
#Per<-rbind(Per,period)
Nr<-rbind(Nr,100*tree.Nr+j+period*1000+20)
Art<-rbind(Art,5)
BHD2 = 0.01623
BHD<-rbind(BHD,0.01623) # adjust to realistic value
kenn<-rbind(kenn,0) # removal identification
NatSchBaum<-rbind(NatSchBaum,0)
#mort<-rbind(mort,0) 
#KKL<-rbind(KKL,0) # crown competition for light
#NatSchBaum<-rbind(NatSchBaum,0)
}

h<-rbind(h,0.3) # regression, adjust? H=2.7658*D^0.539
kra<-rbind(kra,BHD*0.522*1.762) # crownbase= h(1-e^(-(a+b(h/dbh)+c*dbh))
kd<-rbind(kd,BHD*0.14+2.66) # crown diameter=e(a+bln(dbh)+ch+dln(h/dbh)
#changed lists
seeds<-data.frame(Nr,Art,BHD,h,kra,kd,x=xseed2,y=yseed2,kenn,NatSchBaum)
seeddata<-rbind(seeddata,seeds)
#seeddata2<-unname(seeddata)
} # End of all trees
############################
## load ztr
tree.ztr <- read.delim(paste0(fd,'/',site,'_',period,'.ztr'), header=FALSE)
ztr<-c()

for (i in 1:nrow(tree.ztr)) {
  tem<-subset(treedata1,Nr==tree.ztr[i,])
  ztr<-rbind(ztr,tem)
} 

#remove some saplings based on mean CI
#pick for remove some saplings(bigger one) based on mean CI
f.count<-subset(result,BHD>=5.5 & BHD<10)
f.sm<-subset(result,BHD>4 & BHD<6.5)
#nrow(f.count)
f.smci<-mean(f.sm$KKL)*1.0

a=15.589*10^-3
b=0.01696*10^-3
c=0.01883*10^-3
d=0.01883*10^-3
vol=with(result,(a+b*BHD*h^2+d*BHD^3)/0.15*0.6)


if (nrow(f.count)>=50 & nrow(f.count)<80 & sum(vol)<200) {
  f.smsub<-subset(f.sm,KKL<f.smci)
} else if (nrow(f.count)>=80 & sum(vol)<300) {
  f.smsub<-subset(f.sm,KKL<(f.smci*1.2))
} else if ( sum(vol)>300) {
  f.smsub<-subset(f.sm,KKL<(f.smci*1.5))
  } else f.smsub<-c()
#nrow(result0.1)
aa<-f.smsub$Nr
#result3<-result2[!(result2$Nr==f.smsub$Nr),]
#result0.1<-result[!(result$Nr=aa),]
#result0.1<-result[!(result$Nr=aa),]
##keeps <- apply(result,1, function(x) !any(x %in% aa))
##result[keeps,]

result0.1<-result[ !rowSums( sapply( result, function(x) x %in% aa) ) , ]

#bb<-subset(result, Nr!=aa)
#nrow(cc)
data.s<-c()
# Export data table
if (nrow(big.tree)==0){
seeddata<-c() }
headline1<-c('Bk100000 60.0 40.0 0.0 per1','','','','','','','','','') ##change site
headline2<-c('1 7.09.03	 49.0 800.0 0.0 0.0 1999 5 3','','','','','','','','','')
result2<-rbind(result0.1,ztr)

result3<-c()
#shelterwood cutting


a=15.589*10^-3
b=0.01696*10^-3
c=0.01883*10^-3
d=0.01883*10^-3




data.s<-data.frame(result2[,2:10] ,result2$NatSchBaum)
colnames(data.s)<-c('Nr','Art','BHD','h','kra','kd','x','y','kenn','NatSchBaum')
#colnames(ztr)<-colnames(data.s)
#data.s2<-rbind(data.s,ztr)
#duplicated(data.s)
#data.s[duplicated(data.s),]
data.s2<-data.s[!duplicated(data.s),]
data.s2<-rbind(data.s2,seeddata)
#a=15.589*10^-3
#b=0.01696*10^-3
#c=0.01883*10^-3
#d=0.01883*10^-3
#vol=with(data.s2,(a+b*BHD*h^2+d*BHD^3)/0.15*0.6)

#if (sum(vol)>550) {
#data.s2<-data.s2[!(data.s2$BHD==max(data.s2$BHD)),]
# }

##if volume is over 450
#vol=with(data.s2,(a+b*BHD*h^2+d*BHD^3)/0.15*0.6)
#if (sum(vol)>=900) {
#data.s2<-data.s2[!(data.s2$BHD==max(data.s2$BHD)),]
#}

#vol=with(data.s2,(a+b*BHD*h^2+d*BHD^3)/0.15*0.6)
#if (sum(vol)>=370) {
#data.s2<-data.s2[!(data.s2$BHD>=75),]  #added for remove big trees
#}
#data.s2<-data.s2[!(data.s2$BHD>70 |data.s2$BHD==max(data.s2$BHD)),]
#}
#if (sum(vol)>=500) {
#data.s2<-data.s2[!(data.s2$BHD==max(data.s2$BHD)),]
#data.s2<-data.s2[!(data.s2$BHD>=70),]
#}
data<-rbind (headline1, headline2, data.s2)


write.table(data,paste('sub_seed_',site,'_',period,'.slv',sep=''),dec='.',sep='\t',quote=FALSE,row.names=FALSE,col.names=FALSE)
