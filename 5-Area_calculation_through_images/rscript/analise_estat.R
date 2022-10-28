dados<-read.csv2('../dados/vol_comparativo.csv')
cub<-read.csv2('../dados/cubagem_final.csv')

library(cmrinvflor)

?smalian
names(cub)
cub_fita<- cub %>%
  select(arvore,cdap,ht,hi,cicc,espcasca)

cub_suta<- cub %>%
  select(arvore,dap,ht,hi,dicc,espcasca)

fita_smal<-smalian(cub_fita,dcoms=5,htoco=10,comcasca=F,di_ou_ci='ci');
suta_smal<-smalian(cub_suta,dcoms=7,htoco=10,comcasca=F,di_ou_ci='di');

fita_smal<-fita_smal %>%
  select(arvore,vtsc)
suta_smal<-suta_smal %>%
  select(arvore,vtsc)

names(fita_smal)<-c("arv","fita_vtsc")
names(suta_smal)<-c("arv","suta_vtsc")

dados<-merge(dados,fita_smal)
dados<-merge(dados,suta_smal)


names(dados)<-c("arv","v_xil","v_tri","v_fita","v_suta")

for(i in 1:nrow(dados)){
  dados$dif_tri[i]<-ifelse(dados$v_xil[i]<dados$v_tri[i],dados$v_xil[i]/dados$v_tri[i],dados$v_tri[i]/dados$v_xil[i])
  dados$dif_fita[i]<-ifelse(dados$v_xil[i]<dados$v_fita[i],dados$v_xil[i]/dados$v_fita[i],dados$v_fita[i]/dados$v_xil[i])
  dados$dif_suta[i]<-ifelse(dados$v_xil[i]<dados$v_suta[i],dados$v_xil[i]/dados$v_suta[i],dados$v_suta[i]/dados$v_xil[i])
  dados$dif_tri_suta[i]<-ifelse(dados$v_suta[i]<dados$v_tri[i],dados$v_suta[i]/dados$v_tri[i],dados$v_tri[i]/dados$v_suta[i])
}

dados<-subset(dados,!arv=='62')
summary(dados$dif_tri_suta)

names(dados)
dados$res_tri<-with(dados,v_xil-v_tri)
dados$res_fit<-with(dados,v_xil-v_fita)
dados$res_sut<-with(dados,v_xil-v_suta)


pdiccresunid<-function(vol, res, modelo, xlab='vol', ylim=range(-0.06,0.06)){
  plot(vol,res, pch=19, xlab = xlab,cex.lab=1.4,ylab="Resíduos (m³)", 
       main=modelo,cex.main=1.2,ylim=ylim,col='black');
  abline(h=0);
  tvalor<-qt(0.995, length(res)-1)
  abline(h=tvalor, lty=2,col='black');
  abline(h=-tvalor, lty=2,col='black');
}

w=1366
h=768
x11(width=w,height=h);
par(oma=c(3,3,0,0),mar=c(3,3,2,2),mfrow=c(1,3))
#par(cex=2);
pdiccresunid(dados$v_xil,dados$res_tri,'Triangulação de Delaunayn');
pdiccresunid(dados$v_xil,dados$res_fit,'Método de Smalian com Fita');
pdiccresunid(dados$v_xil,dados$res_sut,'Método de Smalian com Suta');

mtext(text="Volume (m³)",side=1,line=0,outer=TRUE)
mtext(text="Resíduo (m³)",side=2,line=0,outer=TRUE)



n=nrow(dados)
(rmse_tri=with(dados,(sqrt(sum((v_xil-v_tri)^2)/(n-1))/mean(v_xil))*100)) 
(rmse_fita=with(dados,(sqrt(sum((v_xil-v_fita)^2)/(n-1))/mean(v_xil))*100))
(rmse_suta=with(dados,(sqrt(sum((v_xil-v_suta)^2)/(n-1))/mean(v_xil))*100))


(mab_tri=with(dados,sum(abs(v_xil-v_tri))/n))
(mab_fita=with(dados,sum(abs(v_xil-v_fita))/n))
(mab_suta=with(dados,sum(abs(v_xil-v_suta))/n))

(ef_tri=with(dados,1-(sum((v_xil-v_tri)^2)/sum((v_xil-mean(v_xil))^2))))   
(ef_fita=with(dados,1-(sum((v_xil-v_fita)^2)/sum((v_xil-mean(v_xil))^2))))   
(ef_suta=with(dados,1-(sum((v_xil-v_suta)^2)/sum((v_xil-mean(v_xil))^2))))



