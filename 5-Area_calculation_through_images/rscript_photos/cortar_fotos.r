rm(list=ls(all=T));

library(raster);   # pacote para manipular rasteres
library(magick);

#narvs<-c(1:7,9:18,20:40,42:43,45:64,66:101,190,240);


dir_jpg<-'..\\dados\\fotos\\jpg1\\';
dir_jpg2<-'..\\dados\\fotos\\jpg2\\';
sfotos_csv<-'..\\dados\\fotos_discos.csv';

sfotos<-read.csv2(sfotos_csv<-'..\\dados\\fotos_discos.csv');
sfotos<-subset(sfotos,foto==1);

#narvs<-unique(sfotos$arv); 
#narvs<-narvs[!narvs %in% 1:4]
#narvs<-sort(narvs);

narvs<-20;
seldiscos<-c(2);

graphics.off(); x11(400,400); 
par(mai=c(0.25,0.25,0.5,0)); #reduzindo as margens do gráfico

for(arv in narvs){
 fdiscos<-dir(paste(dir_jpg,arv,sep=''),full.names = T);
 hi<-as.vector(sfotos$secao[sfotos$arv==arv]);
 #seldiscos<-1:length(hi);
 #seldiscos<-c(18,19);
 
 for(d in seldiscos){
   cat(paste('Árvore: ',arv,' - Disco: ', d, ' de ', length(seldiscos),' (hi = ',hi[d],'m)',sep=''),'\n');
   img <- brick(fdiscos[d]); #Ler arquivo multispectral

   pnt<-coordinates(img);

   img_jpg<-image_read(fdiscos[d]);
   
   tp<-try(plot(img_jpg),silent=T);
   
   if(class(tp)=='try-error'){
     plot(img[[1]]);
   }
   
   ncor<-2;
   fim<-0;
   while(fim==0){
     pbox<-identify(pnt[,1],pnt[,2], labels='+', col=ncor); #Pontos do disco
     if(length(pbox)!=0){
       nbox<-pnt[pbox,c(1,2)];
       
       nbox<-rbind(
         c(max(nbox[,1]),max(nbox[,2])),
         c(max(nbox[,1]),min(nbox[,2])),
         c(min(nbox[,1]),min(nbox[,2])),
         c(min(nbox[,1]),max(nbox[,2])),
         c(max(nbox[,1]),max(nbox[,2]))
       )
       polygon(nbox, lwd=2, border=ncor);     
       ncor<-ncor+2
     }else{
       fim<-1;
     }
   }
   nimg <- crop(img, nbox);
   
   #plot(nimg[[1]]);

   arq_tif<-tempfile(fileext = ".tif");
   
   writeRaster(nimg,arq_tif,"GTiff", overwrite=TRUE);
   
   arq_jpg2<-paste(dir_jpg2,arv,'\\','arv_',arv,'_hi_',gsub('[.]','_',hi[d]),'.jpg',sep='');
   
   comm <- paste('gdal_translate -of JPEG -unscale -co worldfile=yes ', arq_tif, arq_jpg2);
   system(comm);
   unlink(arq_tif);
 }
 remover<-dir(paste(dir_jpg2,arv,sep=''),pattern = '.xml')
 remover<-as.vector(cbind(remover,dir(paste(dir_jpg2,arv,sep=''),pattern = '.wld')))
 remover<-paste0(dir_jpg2,arv,'\\',remover);
 file.remove(remover);
}
graphics.off();
rm(list=ls(all=T));
