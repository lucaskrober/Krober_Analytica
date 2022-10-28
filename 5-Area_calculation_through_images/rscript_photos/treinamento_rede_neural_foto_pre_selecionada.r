graphics.off();rm(list=ls(all=T));

library(raster);   # pacote para manipular rasteres
library(nnet);
library(magick);

dir_jpg2<-'..\\dados\\fotos\\jpg2\\';
#dir_tif<-'..\\dados\\fotos\\tif\\';
arq_rede1<-'..\\dados\\dados_rede_1.csv';
arq_rede2<-'..\\dados\\dados_rede_2.csv';
rede_rda<-'..\\dados\\rede_2.rda';
arq_sel_foto<-'..\\dados\\sel_fotos_treinamento.csv';

#dfrede<-data.frame(arv=NA,hi=NA,r=NA,g=NA,b=NA,disco=NA);
dfrede<-NULL;
dfrede<-rbind(dfrede,read.csv2(arq_rede1,stringsAsFactors = F));
dfrede<-dfrede[sample(1:nrow(dfrede),1000000,F),];

graphics.off(); x11(400,400); 
par(mai=c(0.25,0.25,0.25,0.25)); #reduzindo as margens do gráfico

#source('sel_fotos_treinamento.r');

selfotos<-read.csv2(arq_sel_foto,stringsAsFactors = F);

for(i in 1:nrow(selfotos)){
  arv<-selfotos$arv[i];
  hi<-selfotos$hi[i];
  
  cat(paste('Disco:',i,'de',nrow(selfotos)),'\n');
  cat(paste('Árvore: ',arv,' - hi = ',hi,'m',sep=''),'\n');
  
  #arq_tif<-paste(dir_tif,arv,'\\','arv_',arv,'_hi_',gsub('[.]','_',hi),'.tif',sep='');
  #img <- brick(arq_tif); #Ler arquivo multispectral
  
  arq_jpg2<-paste(dir_jpg2,arv,'\\','arv_',arv,'_hi_',gsub('[.]','_',hi),'.jpg',sep='');
  img <- brick(arq_jpg2); #Ler arquivo multispectral
  names(img)<-c('r','g','b')
  #graphics.off(); x11(); plot(img);
  
  #arq_jpg2<-paste(dir_jpg2,arv,'\\','arv_',arv,'_hi_',gsub('[.]','_',hi),'.jpg',sep='');
  img_jpg <- image_read(arq_jpg2);

  pnt<-as.data.frame(rasterToPoints(img));
  #names(pnt)<-c('x','y','r','g','b');
  
  plot(img_jpg);
  #image(img, main=paste('Árvore ',arv,' - hi = ',hi,'m',sep=''));
  
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
        selpnt$hi<-hi;
        dfrede<-rbind(dfrede,selpnt);
      }else{
        fim<-1;
      }
    }
  }
}
graphics.off();

#dfrede<-dfrede[-1,];

#ii<-dfrede$arv==52
#dfrede<-subset(dfrede,arv!=7)

write.csv2(dfrede,arq_rede2,row.names=F);
#dfrede<-read.csv2(arq_rede2,stringsAsFactors = F);

train<-dfrede[,1:4];

varbin <- class.ind(train$disco); #Criar variáveis binárias a partir de uma variável categórica
nn <- nnet(train[,-4], varbin[,], size=10, maxit=200, softmax=TRUE);

save(nn, file=rede_rda);

nc<-as.integer(predict(nn, pnt[,c('r','g','b')], type="class"));

nc[nc==2]<-NA;

rst<-raster(img);
values(rst)<-nc;
graphics.off(); x11(); plot(rst, col=c('#d8b365','red'),legend=F);

#writeRaster(rst,arq_res_tif,"GTiff", overwrite=TRUE);

