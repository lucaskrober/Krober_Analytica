graphics.off();rm(list=ls(all=T));

library(raster);   # pacote para manipular rasteres
library(nnet);
library(magick);

dir_jpg<-'..\\dados\\fotos\\jpg1\\';
arq_rede<-'..\\dados\\dados_rede_foto_original.csv';
rede_rda<-'..\\dados\\rede_foto_original.rda';

sfotos_csv<-'..\\dados\\fotos_discos.csv';

sfotos<-read.csv2(sfotos_csv<-'..\\dados\\fotos_discos.csv');
sfotos<-subset(sfotos,foto==1);

narvs<-unique(sfotos$arv); narvs<-narvs[!narvs %in% 1:4];narvs<-sort(narvs);
#narvs<-c(1:4);

nselarv<-10;
nseldisco<-1;

selarvs<-sample(narvs,nselarv,replace=F);
#selarvs<-1; arv<-1;

#dfrede<-data.frame(arv=NA,hi=NA,r=NA,g=NA,b=NA,disco=NA);
#dfrede<-rbind(dfrede,read.csv2(arq_rede,stringsAsFactors = F));
dfrede<-NULL;

graphics.off(); x11(400,400); 
par(mai=c(0.25,0.25,0.25,0.25)); #reduzindo as margens do gráfico

for(arv in selarvs){
  fdiscos<-dir(paste(dir_jpg,arv,sep=''),full.names = T);
  ndiscos<-length(fdiscos);
  seldiscos<-sort(sample(1:ndiscos,nseldisco,replace=F));

  hi<-as.vector(sfotos$secao[sfotos$arv==arv]);
  
  for(d in 1:nseldisco){
    cat(paste('Árvore: ',arv,' - hi = ',hi[seldiscos[d]],'m',sep=''),'\n');
    
    img <- brick(fdiscos[seldiscos[d]]); #Ler arquivo multispectral
    names(img)<-c('r','g','b')
    #graphics.off(); x11(); plot(img);
    
    img_jpg <- image_read(fdiscos[seldiscos[d]]);

    pnt<-as.data.frame(rasterToPoints(img));
    #names(pnt)<-c('x','y','r','g','b');
    
    plot(img_jpg);
    #image(img, main=paste('Árvore ',arv,' - hi = ',hi[seldiscos[d]],'m',sep=''));
    
    for(etapa in 0:2){
      if (etapa==0){
        cat('Selecione polígonos na área do disco...','\n');
        ncor<-1;
      }else{
        if (etapa==1){
          cat('Selecione polígonos na área de referência...','\n');
          ncor<-4;
        } else{
          cat('Selecione polígonos na área fora do disco','\n');
          ncor<-2;
        }
      }
      fim<-0;
      while(fim==0){
        pbox<-with(pnt,identify(x,y, labels='+', col=ncor)); #Pontos do disco
        if(length(pbox)!=0){
          nbox<-pnt[pbox,c('x','y')];
          
          #nbox<-rbind(nbox,nbox[1,]);
          
          nbox<-rbind(
            c(max(nbox$x),max(nbox$y)),
            c(max(nbox$x),min(nbox$y)),
            c(min(nbox$x),min(nbox$y)),
            c(min(nbox$x),max(nbox$y)),
            c(max(nbox$x),max(nbox$y))
          )
          
          polygon(nbox, lwd=2, border=ncor);     
          #ncor<-ncor+1
          nimg <- crop(img, nbox);
          selpnt<-as.data.frame(rasterToPoints(nimg))[,3:5];
          selpnt$disco<-etapa;
          selpnt$arv<-arv;
          selpnt$hi<-hi[seldiscos[d]];
          dfrede<-rbind(dfrede,selpnt);
        }else{
          fim<-1;
        }
      }
    }
  }
}
graphics.off();

#dfrede<-dfrede[-1,];

write.csv2(dfrede,arq_rede,row.names=F);
#dfrede<-read.csv2(arq_rede,stringsAsFactors = F);

train<-dfrede[,1:4];

varbin <- class.ind(train$disco); #Criar variáveis binárias a partir de uma variável categórica
nn <- nnet(train[,-4], varbin[,], size=10, softmax=TRUE);

save(nn, file=rede_rda);

nc<-as.integer(predict(nn, pnt[,c('r','g','b')], type="class"));

nc[nc==2]<-NA;

rst<-raster(img);
values(rst)<-nc;
graphics.off(); x11(); plot(rst, col=c('#d8b365','red'),legend=F);

#writeRaster(rst,arq_res_tif,"GTiff", overwrite=TRUE);

