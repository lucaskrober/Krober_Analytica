#source('cortar_fotos_usando_redes.r')

graphics.off();rm(list=ls(all=T));

library(raster);
library(nnet);
library(rgdal);
library(magick);

dir_jpg<-'..\\dados\\fotos\\jpg1\\';
dir_jpg2<-'..\\dados\\fotos\\jpg2\\';
#rede_rda<-'..\\dados\\rede1.rda';
rede_rda<-'..\\dados\\rede_foto_original.rda';

sfotos_csv<-'..\\dados\\fotos_discos.csv';

sfotos<-read.csv2(sfotos_csv<-'..\\dados\\fotos_discos.csv');
sfotos<-subset(sfotos,foto==1);

plotar<-F;

#narvs<-unique(sfotos$arv); narvs<-narvs[!narvs %in% 1:4];narvs<-sort(narvs);
#narvs<-c(1:4);
narvs<-37;

load(rede_rda);

if(plotar){
  graphics.off(); x11(400,400); 
  par(mai=c(0.25,0.25,0.25,0.25)); #reduzindo as margens do gráfico
}

for(arv in narvs){
  fdiscos<-dir(paste(dir_jpg,arv,sep=''),full.names = T);
  seldiscos<-1:length(fdiscos)
  hi<-as.vector(sfotos$secao[sfotos$arv==arv]);
  for(d in seldiscos){
    cat(paste('Árvore: ',arv,' - hi = ',hi[d],'m',sep=''),'\n');
    
    img <- brick(fdiscos[d]); #Ler arquivo multispectral
    names(img)<-c('r','g','b')

    nbox<-as.data.frame(t(bbox(img)));
    names(nbox)<-c('x','y');
    nbox[1,]<-nbox[1,]+100+d*15;
    nbox[2,]<-nbox[2,]+100+d*15;
    
    nbox<-rbind(
      c(max(nbox$x),max(nbox$y)),
      c(max(nbox$x),min(nbox$y)),
      c(min(nbox$x),min(nbox$y)),
      c(min(nbox$x),max(nbox$y)),
      c(max(nbox$x),max(nbox$y))
    )
    
    img <- crop(img, nbox);
    
    nc<-as.integer(predict(nn, as.data.frame(values(img)), type="class"));
    
    nc[nc==2]<-NA;
    rst<-raster(img);
    values(rst)<-nc;
    
    arq_tif2<-tempfile(fileext = ".tif");
    arq_shp<-tempfile(fileext = ".shp");

    writeRaster(rst,arq_tif2,"GTiff", overwrite=TRUE);

    comm <- paste('python ..\\pyscript\\polygonize.py', arq_tif2, '-f "ESRI Shapefile" -8  -q -nomask -b 1',arq_shp);
    system(comm, show.output.on.console = F);
    #system(comm);
    
    shp <- readOGR(arq_shp,ogrListLayers(arq_shp)[1]);
    
    unlink(arq_tif2);
    unlink(arq_shp);
    
    #x11(); plot(shp); box(); axis(1); axis(2); grid();
    #graphics.off();

    #shp<-aggregate(shp,by='DN');

    ##Selecionando os shapes do disco e do objeto de referência
    ashp<-area(shp);
    
    selpoly<-match(max(ashp),ashp);
    
    nspoly<-length(shp@polygons[[selpoly]]@plotOrder);
    spoly<-data.frame(np=1:nspoly,area=NA);
    for(i in 1:nspoly){
      spoly$area[i]<-shp@polygons[[selpoly]]@Polygons[[i]]@area
    }
    spoly<-spoly[order(spoly$area, decreasing = T),];
    
    xy<-shp@polygons[[selpoly]]@Polygons[[spoly$np[1]]]@coords;
    p_xy<-Polygons(srl=list(Polygon(xy)),ID='0')
    sp_xy<-SpatialPolygons(Srl=list(p_xy),pO = as.integer(1)); #pO: plotOrder
    #dados<-data.frame(DN=0,row.names = '0');
    #spd_xy<-SpatialPolygonsDataFrame(sp_xy,data=dados);
    
    nbox<-as.data.frame(bbox(sp_xy));
    sx<-ceiling((nbox[1,2]-nbox[1,1])*0.05);
    sy<-ceiling((nbox[2,2]-nbox[2,1])*0.05);
    
    nbox$min[1]<-nbox$min[1]-sx;
    nbox$max[1]<-nbox$max[1]+sx;

    nbox$min[2]<-nbox$min[2]-sy;
    nbox$max[2]<-nbox$max[2]+sy;
    
    nbox<-rbind(
      c(nbox$max[1],nbox$max[2]),
      c(nbox$max[1],nbox$min[2]),
      c(nbox$min[1],nbox$min[2]),
      c(nbox$min[1],nbox$max[2]),
      c(nbox$max[1],nbox$max[2])
    )
    
    if(plotar){
      img_jpg <- image_read(fdiscos[d]);
      plot(img_jpg);
      
      pnt<-coordinates(img);
      
      ncor<-2;
      polygon(nbox, lwd=2, border=ncor);     
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
      cat('Digite 1 para cortar ou 2 para parar','\n'); 
      r<-as.vector(scan(what = 'integer', nmax = 1)); 
    }else{
      r<-1;
    }

    if(r==2){break}
    
    nimg <- crop(img, nbox);
    
    arq_tif<-tempfile(fileext = ".tif");
    
    writeRaster(nimg,arq_tif,"GTiff", overwrite=TRUE);
    
    arq_jpg2<-paste(dir_jpg2,arv,'\\','arv_',arv,'_hi_',gsub('[.]','_',hi[d]),'.jpg',sep='');
    
    #comm <- paste('gdal_translate -of JPEG -ot UInt16 -unscale -co worldfile=yes ', arq_tif, arq_jpg2);
    comm <- paste('gdal_translate -of JPEG -unscale -co worldfile=yes ', arq_tif, arq_jpg2);
    system(comm, show.output.on.console = F);
    
    unlink(arq_tif);
    remover<-dir(paste(dir_jpg2,arv,sep=''),pattern = '.xml')
    remover<-as.vector(cbind(remover,dir(paste(dir_jpg2,arv,sep=''),pattern = '.wld')))
    remover<-paste0(dir_jpg2,arv,'\\',remover);
    file.remove(remover);
  }
  if(r==2){break}
  # remover<-dir(paste(dir_jpg2,arv,sep=''),pattern = '.xml')
  # remover<-as.vector(cbind(remover,dir(paste(dir_jpg2,arv,sep=''),pattern = '.wld')))
  # remover<-paste0(dir_jpg2,arv,'\\',remover);
  # file.remove(remover);
}
