#Graphical analysis of MTE algorithm

#Erros base fotos
cub=read.csv2('..\\dados\\AEC0144_diam_final.csv')
cub=read.csv2('..\\dados\\IPB1_diam_final.csv')
cub=read.csv2('..\\dados\\VT01_diam_final.csv')

n=nrow(cub)
#diametro
#1 kozak: 5.099507 e 0.3888216
(rmsek=with(cub,sqrt(sum((disc-discm1)^2)/(n-1))/mean(disc)*100))
(MAB=with(cub,sum(abs(disc-discm1))/n))
#2 5grau: 6.218938 e 0.4957415
(rmse5=with(cub,sqrt(sum((disc-discm2)^2)/(n-1))/mean(disc)*100))
(MAB=with(cub,sum(abs(disc-discm2))/n))
#3 Muhairwe: 5.164583 e 0.3944742
(rmsemu=with(cub,sqrt(sum((disc-discm3)^2)/(n-1))/mean(disc)*100))
(MAB=with(cub,sum(abs(disc-discm3))/n))
#4 Max: 
(rmsemeth=with(cub,sqrt(sum((disc-discm4)^2)/(n-1))/mean(disc)*100))
(MAB=with(cub,sum(abs(disc-discm4))/n))
#MTE: 4.998605 e 0.3812423
(rmsemte=with(cub,sqrt(sum((disc-dmte)^2)/(n-1))/mean(disc)*100))
(MAB=with(cub,sum(abs(disc-dmte))/n))

#AEC 1044
#Muhairwe: 
#RMSE: 3.368817
#MAB: 0.3186419
#Kozak:
#RMSE: 3.479889
#MAB: 0.3264357
#Max:
#RMSE: 3.99181
#MAB: 0.3928794
#5grau:
#RMSE: 4.111967
#MAB: 0.4062209
#MTE:
#RMSE: 3.408817
#MAB: 0.3269396


#Gráfico de resíduos
res.mte=((cub$disc-cub$dmte)/(mean(cub$disc))*100)
res.koz=with(cub,((disc-discm1)/mean(disc))*100)
res.scho=with(cub,((disc-discm2)/mean(disc))*100)
res.muh=with(cub,((disc-discm3)/mean(disc))*100)
res.meth=with(cub,((disc-discm4)/mean(disc))*100)

cub$rmse.koz=with(cub,sqrt(sum((disc-discm1)^2)/n)*100)
cub$rmse.scho=with(cub,sqrt(sum((disc-discm2)^2)/n)*100)
cub$rmse.muh=with(cub,sqrt(sum((disc-discm3)^2)/n)*100)
cub$rmse.meth=with(cub,sqrt(sum((disc-discm4)^2)/n)*100)
cub$rmse.mte=with(cub,sqrt(sum((disc-dmte)^2)/n)*100)

View(cub)

cub=subset(cub,!arv==20)
cub=subset(cub,!arv==8)
cub=subset(cub,!arv==63)

pdiscresunid<-function(hi, res, modelo, xlab='hi', ylim=range(-12,12),rmse){
  plot(hi,res, pch=20, xlab = xlab,cex.lab=1.4,ylab="Resíduos (%)", 
       main=modelo,cex.main=1.2,ylim=ylim,col='black');
  abline(h=0);
  abline(h=5, lty=2,col='black');
  abline(h=-5, lty=2,col='black');
  #lines(lowess(hi, res), col='red', lwd=2, lty=1);
}

w=1500
h=400
x11(width=w,height=h);
par(mfrow=c(2,3));
pdiscresunid(cub$hi,res.koz,'Kozak');
pdiscresunid(cub$hi,res.scho,'Schoepfer');
pdiscresunid(cub$hi,res.muh,'Muhairwe');
pdiscresunid(cub$hi,res.meth,'Max e Burkhart');
pdiscresunid(cub$hi,res.mte,'MTE')

View(cub)
ii<-identify(cub$hi,res.mte);
cub[ii,];

dir()

library(fBasics);
library(ggplot2);
library(reshape2);
library(cmrinvflor);


#### Forma do fuste
## Preciso calcular: dicc/2; kozak/2, muhairwe/2 e mte/2
cub$Kozak<-cub$discm1/2
cub$Schoepfer<-cub$discm2/2
cub$Muhairwe<-cub$discm3/2
cub$Max<-cub$discm4/2
cub$MTE<-cub$dmte/2
cub$Diametro<-cub$disc/2
names(cub)
cub$arvore=cub$arv

View(cub)

cub2=subset(cub,arvore==73)

arvore<-(cub2$arvore)

forma_fuste<-melt(cub2,measure.vars = c('Kozak','Diametro'),id.vars = c('hi','arvore'))
forma_fuste<-melt(cub2,measure.vars = c('Schoepfer','Diametro'),id.vars = c('hi','arvore'))
forma_fuste<-melt(cub2,measure.vars = c('Muhairwe','Diametro'),id.vars = c('hi','arvore'))
forma_fuste<-melt(cub2,measure.vars = c('Max','Diametro'),id.vars = c('hi','arvore'))
forma_fuste<-melt(cub2,measure.vars = c('MTE','Diametro'),id.vars = c('hi','arvore'))


x11()
ggplot(forma_fuste, aes(x = value, y = hi, color=variable))+ theme_bw()+geom_point()+
  geom_point(aes(x=-value,y=hi,color=variable))+
  geom_vline(xintercept = 0,colour='red',linetype="dashed", size=0.5)+
  labs(x="Raio (cm)", y="Altura (m)",color="")+
  facet_wrap(~arvore,nrow=4)+
  scale_y_continuous(breaks=seq(0,30,2), limits = c(0,18), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(-12,12,2), limits = c(-7,7), expand = c(0,0))




#volume
cub=read.csv2('..\\dados\\AEC0144_final.csv')
cub=read.csv2('..\\dados\\IPB1_final.csv')
cub=read.csv2('..\\dados\\VT01_final.csv')

cub=subset(cub,!arv==7)
View(cub)
n=nrow(cub)

#volume
#1 kozak: 5.099507 e 0.3888216
(rmsek=with(cub,sqrt(sum((xvisc-vm1)^2)/(n-1))/mean(xvisc)*100))
(MAB=with(cub,sum(abs(xvisc-vm1))/n))
#2 5grau: 6.218938 e 0.4957415
(rmse5=with(cub,sqrt(sum((xvisc-vm2)^2)/(n-1))/mean(xvisc)*100))
(MAB=with(cub,sum(abs(xvisc-vm2))/n))
#3 Muhairwe: 5.164583 e 0.3944742
(rmsemu=with(cub,sqrt(sum((xvisc-vm3)^2)/(n-1))/mean(xvisc)*100))
(MAB=with(cub,sum(abs(xvisc-vm3))/n))
#4 Max: 
(rmsemeth=with(cub,sqrt(sum((xvisc-vm4)^2)/(n-1))/mean(xvisc)*100))
(MAB=with(cub,sum(abs(xvisc-vm4))/n))
#MTE: 4.998605 e 0.3812423
(rmsemte=with(cub,sqrt(sum((xvisc-vmte)^2)/(n-1))/mean(xvisc)*100))
(MAB=with(cub,sum(abs(xvisc-vmte))/n))

#Gráfico de resíduos
res.mte=((cub$xvisc-cub$vmte)/mean(cub$xvisc))*100
res.koz=with(cub,((xvisc-vm1)/mean(xvisc))*100)
res.scho=with(cub,((xvisc-vm2)/mean(xvisc))*100)
res.muh=with(cub,((xvisc-vm3)/mean(xvisc))*100)
res.meth=with(cub,((xvisc-vm4)/mean(xvisc))*100)

cub$rmse.koz=with(cub,sqrt(sum((xvisc-vm1)^2)/n))
cub$rmse.scho=with(cub,sqrt(sum((xvisc-vm2)^2)/n))
cub$rmse.muh=with(cub,sqrt(sum((xvisc-vm3)^2)/n))
cub$rmse.meth=with(cub,sqrt(sum((xvisc-vm4)^2)/n))
cub$rmse.mte=with(cub,sqrt(sum((xvisc-vmte)^2)/n))

pdiscresunid<-function(vol, res, modelo, xlab='hi', ylim=range(-15,15),rmse){
  plot(vol,res, pch=20, xlab = xlab,cex.lab=1.4,ylab="Resíduos (m³)", 
       main=modelo,cex.main=1.2,ylim=ylim,col='black');
  abline(h=0);
  abline(h=5, lty=2,col='black');
  abline(h=-5, lty=2,col='black');
  #lines(lowess(vol, res), col='red', lwd=2, lty=1);
}

w=1500
h=400
x11(width=w,height=h);
par(mfrow=c(2,3));
pdiscresunid(cub$hi,res.koz,'Kozak',rmse=mean(cub$rmse.koz));
pdiscresunid(cub$hi,res.scho,'Schoepfer',rmse=mean(cub$rmse.scho));
pdiscresunid(cub$hi,res.muh,'Muhairwe',rmse=mean(cub$rmse.muh));
pdiscresunid(cub$hi,res.meth,'Max e Burkhart',rmse=mean(cub$rmse.meth));
pdiscresunid(cub$hi,res.mte,'MTE',rmse=mean(cub$rmse.mte))

ii<-identify(cub$hi,res.scho);
cub[ii,];

View(cub)



