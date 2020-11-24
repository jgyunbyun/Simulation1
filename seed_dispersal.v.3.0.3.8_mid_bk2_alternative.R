
#skip two rows
site<-'bk2'
setwd(paste0('c:/SILVA/',site))
period<-0
no.parent<-0
treedata <- read.delim(paste('c:/SILVA/',site,'/',site,'_',period,'02','.slv',sep=''), header=FALSE,skip=2)
sub.treedata<-subset(treedata, V3<7)
sub.treedata2<-subset(treedata, V3> 40/4.143)
check.nu<-nrow(sub.treedata) #trees below 7cm
check.nu2<-nrow(sub.treedata2) #trees possible to produce seeds ; over 40 years old
#finding big trees for designating F-tree
#F tree control using 'if'

ba <-within(treedata,ba<-(V3/2)^2*pi/10000/0.15)

s.ba<-sum(ba$ba)

m.period<-period


big.tree1 <- read.delim(paste('c:/SILVA/',site,'/',site,'_',m.period,'02','.slv',sep=''), header=FALSE,skip=2)
big.tree<-subset(big.tree1,V3>=7 & V3<130) #from 7 to 5
conser.tree<-subset(big.tree1,V3>=10 & V3<40)
#nrow(big.tree)
a=15.589*10^-3
b=0.01696*10^-3
c=0.01883*10^-3
d=0.01883*10^-3

vol=with(big.tree,(a+b*V3*V4^2+d*V3^3)/0.15*0.6)

#if (sum(vol)>600) {
#  big.tree<-big.tree[!(big.tree$V3==max(big.tree$V3)),]
#}

#vol=with(big.tree,(a+b*V3*V4^2+d*V3^3)/0.15*0.6)

#if (sum(vol)>600) {
#  big.tree<-big.tree[!(big.tree$V3==max(big.tree$V3)),]
#}


#if (nrow(big.tree)<60) {
#big.tree<-subset(big.tree1,V3>=7 & V3<150)
#} else big.tree<-subset(big.tree1,V3>=10 & V3<150)
tree.nu<-big.tree$V1

notrees=nrow(big.tree)


tree.Nr3=c()
tree.Nr4=c()
tree.Nr5=c()
tree.Nr6=c()
for (i in 1:notrees){

tree.Nr=big.tree$V1[i]
tree.d=big.tree$V3[i]
tree.h=big.tree$V4[i]
tree.kd=big.tree$V6[i]
tree.x=big.tree$V7[i]
tree.y=big.tree$V8[i]

for (j in 1:notrees) {

tree.Nr2=big.tree$V1[j]
tree.d2=big.tree$V3[j]
tree.h2=big.tree$V4[j]
tree.kd2=big.tree$V6[j]
tree.x2=big.tree$V7[j]
tree.y2=big.tree$V8[j]

#calculate distance
rx<-tree.x-tree.x2
ry<-tree.y-tree.y2
#distance
di=sqrt(rx^2+ry^2)


#if (di<5.0 & tree.h2<tree.h & tree.d<50 & tree.d>=30 & tree.d2<15) {
#  tree.Nr3<-rbind(tree.Nr3,tree.Nr2)
#}
#if (di<5.0 & (tree.h2/tree.d2)>(tree.h/tree.d) & tree.d>=30 &tree.d<50 & tree.d2<40) {
#  tree.Nr4<-rbind(tree.Nr4,tree.Nr2)
#}
if (di<7.8 & tree.h2<tree.h ){
  tree.Nr6<-rbind(tree.Nr6,tree.Nr2)
}
#if (di<5.0 & abs(tree.kd+tree.kd2)/2>6.0 & tree.h2<tree.h) {
#  tree.Nr6<-rbind(tree.Nr6,tree.Nr2)
#}
#if (di<7.0 & sum(vol)>500 & tree.h2<tree.h & tree.d2<10) {
#  tree.Nr3<-rbind(tree.Nr3,tree.Nr2)
#}

}
}

######################
big.tree<-subset(big.tree1,V3>15 & V3<45) #from 7 to 5
#conser.tree<-subset(big.tree1,V3>=10 & V3<40)
#nrow(big.tree)
a=15.589*10^-3
b=0.01696*10^-3
c=0.01883*10^-3
d=0.01883*10^-3

vol=with(big.tree,(a+b*V3*V4^2+d*V3^3)/0.15*0.6)

#if (sum(vol)>600) {
#  big.tree<-big.tree[!(big.tree$V3==max(big.tree$V3)),]
#}

#vol=with(big.tree,(a+b*V3*V4^2+d*V3^3)/0.15*0.6)

#if (sum(vol)>600) {
#  big.tree<-big.tree[!(big.tree$V3==max(big.tree$V3)),]
#}


#if (nrow(big.tree)<60) {
#big.tree<-subset(big.tree1,V3>=7 & V3<150)
#} else big.tree<-subset(big.tree1,V3>=10 & V3<150)
#tree.nu<-big.tree1$V1

notrees=nrow(big.tree)


for (i in 1:notrees){
  
  tree.Nr=big.tree$V1[i]
  tree.d=big.tree$V3[i]
  tree.h=big.tree$V4[i]
  tree.kd=big.tree$V6[i]
  tree.x=big.tree$V7[i]
  tree.y=big.tree$V8[i]
  
  for (j in 1:notrees) {
    
    tree.Nr2=big.tree$V1[j]
    tree.d2=big.tree$V3[j]
    tree.h2=big.tree$V4[j]
    tree.kd2=big.tree$V6[j]
    tree.x2=big.tree$V7[j]
    tree.y2=big.tree$V8[j]
    
    #calculate distance
    rx<-tree.x-tree.x2
    ry<-tree.y-tree.y2
    #distance
    di=sqrt(rx^2+ry^2)
    
    
    #if (di<5.0 & tree.h2<tree.h & tree.d<50 & tree.d>=30 & tree.d2<15) {
    #  tree.Nr3<-rbind(tree.Nr3,tree.Nr2)
    #}
    #if (di<5.0 & (tree.h2/tree.d2)>(tree.h/tree.d) & tree.d>=30 &tree.d<50 & tree.d2<40) {
    #  tree.Nr4<-rbind(tree.Nr4,tree.Nr2)
    #}
    if (di<9.0 & tree.h2<tree.h ){
      tree.Nr4<-rbind(tree.Nr4,tree.Nr2)
    }
    
    
  }
}

#tree.nu0<-setdiff(tree.nu,tree.Nr3)
tree.nu1<-setdiff(big.tree$V1,tree.Nr4)
#tree.nu2<-setdiff(tree.nu1,tree.Nr5)
tree.nu2<-setdiff(tree.nu,tree.Nr6)
tree.nu2<-c(tree.nu2,tree.nu1)
#treeA.nu3<-c(tree.nu2,conser.tree$V1)
#tree.nu2<-tree.nu3[!duplicated(tree.nu3)]
#write table
#if (length(tree.nu2)<=60) {
write.table(tree.nu2, paste0(site,'_',period,'.ztr'), dec='.',sep='\t',quote=FALSE,row.names=FALSE,col.names=FALSE)
#} else write.table(tree.nu3, paste0(site,'_',period,'.ztr'), dec='.',sep='\t',quote=FALSE,row.names=FALSE,col.names=FALSE)




#View(bk1_slv1)
#ordered numbering
notrees=nrow(treedata)
#number<-c(1:notrees)
#data<-data.frame(number,treedata)
data<-treedata
#remove second column 
#data<-data[-2]
#give a header
names(treedata) <- c("Nr","Art","BHD","h","kra","kd","x","y","kenn","Nat")
names(data) <- c("Nr","Art","BHD","h","kra","kd","x","y","kenn","Nat")
# Set number of seedlings to produce per tree
seedlings=1000
# Set seedlings mortality
if (check.nu<80 & s.ba<55 &check.nu2<=20 ) { ##changed a little
mortality=0.98
} else if (check.nu<100 & check.nu2>20 & s.ba<55 ) {
mortality=0.995
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
# Calculate cex-values for plot proportional to BHD and kd
cexbhd<-data$BHD/100
cexkd<-data$kd/1
# bmp('input1.bmp')
#bmp(paste('input_',site,'_',period,'.bmp',sep=''))
maxx2<-max(data$x)
minx2<-min(data$x)
maxy2<-max(data$y)
miny2<-min(data$y)
#plot(treedata$x,treedata$y,col='green',cex=cexbhd,pch=16,xlim=c(minx2,maxx2),ylim=c(miny2,maxy2),xlab='x',ylab='y')
#points(treedata$x,treedata$y,col='green',cex=cexkd)
# dev.off()

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
a=tree.kd/2*1.4
b=1
# Produce seedlings according to Gamma distribution
xseed<-c()
yseed<-c()
if (treeage[treenumber]>minage) {
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
Nat<-c()
#mort<-c()
#KKL<-c()
#NatSchBaum<-c()

if (treeage[treenumber]>minage) {
no.parent<-no.parent+1
for (j in 1:noseedlings) {
#Per<-rbind(Per,period)
Nr<-rbind(Nr,no.parent*30+j+(period+1)*2000)
Art<-rbind(Art,5)
BHD2 = 0.01623
BHD<-rbind(BHD,0.01623) # adjust to realistic value
kenn<-rbind(kenn,0) # removal identification
Nat<-rbind(Nat,0)
#mort<-rbind(mort,0) 
#KKL<-rbind(KKL,0) # crown competition for light
#NatSchBaum<-rbind(NatSchBaum,0)
}

h<-rbind(h,0.3) # regression, adjust? H=2.7658*D^0.539
kra<-rbind(kra,BHD*0.522*1.762) # crownbase= h(1-e^(-(a+b(h/dbh)+c*dbh))
kd<-rbind(kd,BHD*0.14+2.66) # crown diameter=e(a+bln(dbh)+ch+dln(h/dbh)

}
#changed lists
if (treeage[treenumber]>minage) {
seeds<-data.frame(Nr,Art,BHD,h,kra,kd,x=xseed2,y=yseed2,kenn,Nat)
}
} # End of single tree

seeddata<-rbind(seeddata,seeds)

} # End of all trees
#data<-subset(trees,trees$Per==period)
#treedata<-subset(trees,trees$Per==period)
data<-rbind(data,seeddata)
 #----------------------------------------------------
# Plot
# Calculate cex-values for plot proportional to BHD and kd
cexbhd<-data$BHD/100
cexkd<-data$kd/1

bmp(paste('input_',site,'_',period,'.bmp',sep=''))
maxx2<-65
minx2<--5
maxy2<-45
miny2<--5
plot(treedata$x,treedata$y,col='green',cex=cexbhd,pch=16,xlim=c(minx2,maxx2),ylim=c(miny2,maxy2),xlab='x',ylab='y')
points(treedata$x,treedata$y,col='green',cex=cexkd)
dev.off()

# Plot trees and adjusted seedlings
bmp(paste('output_',site,'_',period,'.bmp',sep=''))
maxx2<-65
minx2<--5
maxy2<-45
miny2<--5
plot(seeddata$x,seeddata$y,xlim=c(minx2,maxx2),ylim=c(miny2,maxy2),xlab='x',ylab='y')
points(treedata$x,treedata$y,col='green',cex=cexbhd,pch=16)
points(treedata$x,treedata$y,col='green',cex=cexkd)
points(treedata[treeage<minage,7],treedata[treeage<minage,8],col='red',cex=cexkd) # new:  colored trees that were too young differently JH
dev.off()

# Export data table
headline1<-c('Bk100000 60.0 40.0 0.0 per1','','','','','','','','','')
headline2<-c('1 7.09.03	 49.0 800.0 0.0 0.0 1999 5 3','','','','','','','','','')

data<-rbind (headline1, headline2, data)

write.table(data,paste('seed_',site,'_',period,'.slv',sep=''),dec='.',sep='\t',quote=FALSE,row.names=FALSE,col.names=FALSE)
