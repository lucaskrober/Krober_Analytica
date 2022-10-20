graphics.off(); rm(list=ls(all=T));

library(raster);
library(nnet);
library(rgdal);
library(magick);

arq_sfp<-'..\\dados\\sel_fotos_poligonizar.csv';
#rede_rda<-'..\\dados\\rede_14_1.rda';
rede_rda<-'..\\dados\\rede_2.rda';
arq_resproc<-'..\\resultados\\gi_foto_alta_res.csv';

#area_ref<-0.04*0.04; 
area_ref<-0.0452*0.0452; 

source('f_poly_foto.r');

load(rede_rda);

#resproc<-read.csv2(arq_resproc, stringsAsFactors = F);
#resproc<-data.frame(arv=NA,hi=NA,perimetro=NA,gi=NA);
resproc<-NULL;
resproc<-rbind(resproc,read.csv2(arq_resproc, stringsAsFactors = F));

selfotos<-read.csv2(arq_sfp,stringsAsFactors = F);

memory.size(max=T);
for(i in 1:nrow(selfotos)){
  arv<-selfotos$arv[i];
  hi<-selfotos$hi[i];
  cat(paste('Árvore: ',arv,' - hi = ',hi,'m',sep=''),'\n');
  ii<-resproc$arv==arv & resproc$hi==as.numeric(hi);
  resproc<-resproc[!ii,];
  resproc<-rbind(resproc,f_poly_foto(arv,hi,area_ref,rn=nn));
}
#resproc<-resproc[!is.na(resproc$arv),];
resproc<-resproc[order(resproc$arv,resproc$hi),];
write.csv2(resproc,arq_resproc,row.names = F);

