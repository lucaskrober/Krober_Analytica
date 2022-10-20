graphics.off();rm(list=ls(all=T));

library(raster);
library(nnet);
library(rgdal);
library(magick);

dir_jpg2<-'..\\dados\\fotos\\jpg2\\';
dir_jpg3<-'..\\dados\\fotos\\jpg3\\';
#dir_tif2<-'..\\dados\\fotos\\tif2\\';
#dir_shp<-'..\\dados\\fotos\\shp\\';
arq_shp<-'..\\dados\\temp.shp';
dir_shp2<-'..\\dados\\fotos\\shp2\\';
#rede_rda<-'..\\dados\\rede_14_1.rda';
rede_rda<-'..\\dados\\rede_1.rda';
arq_resproc<-'..\\resultados\\gi_foto_alta_res.csv';
arq_descarte<-'..\\resultados\\descarte_foto_alta_res.csv';

sfotos_csv<-'..\\dados\\fotos_discos.csv';
sfotos<-read.csv2(sfotos_csv<-'..\\dados\\fotos_discos.csv');
sfotos<-subset(sfotos,foto==1);

narvs<-unique(sfotos$arv); narvs<-narvs[!narvs %in% 1:4];narvs<-sort(narvs);
#narvs<-c(1:4);
#narvs<-c(1);

#area_ref<-0.04*0.04; 
area_ref<-0.0452*0.0452; 

plotar<-F;

resproc<-NULL;
descarte<-NULL;

resproc<-rbind(resproc,read.csv2(arq_resproc, stringsAsFactors = F));
#resproc<-subset(resproc,arv!=narvs);

#descarte<-rbind(descarte,read.csv2(arq_descarte, stringsAsFactors = F));

fhi<-function(narq){
  hi<-unlist(strsplit(narq, '[_.]')); 
  if(length(hi)==5){
    hi<-as.numeric(hi[4]);
  }else{
    hi<-as.numeric(paste(hi[4],'.',hi[5],sep=''));
  }
  return(hi)
}

load(rede_rda);
memory.size(max=T);
for(arv in narvs){
  discos<-dir(paste(dir_jpg2,arv,sep=''), pattern='.jpg');
  for(d in 1:length(discos)){
    hi<-fhi(discos[d]);
    cat(paste('Árvore: ',arv,' - hi = ',hi,'m',sep=''),'\n');
    arq_jpg2<-paste(dir_jpg2,arv,'\\',discos[d],sep='');
    
    img <- brick(arq_jpg2); #Ler arquivo multispectral
    names(img)<-c('r','g','b');
    
    pnt<-as.data.frame(values(img));
    
    #pnt<-as.data.frame(rasterToPoints(img))[,3:5];

    nc<-as.integer(predict(nn, pnt, type="class"));
    
    nc[nc==2]<-NA;
    rst<-raster(img);
    values(rst)<-nc;
    
    arq_tif2<-tempfile(fileext = ".tif");
    #arq_tif2<-paste(dir_tif2,arv,'\\','arv_',arv,'_hi_',gsub('[.]','_',hi),'.tif',sep='');
    writeRaster(rst,arq_tif2,"GTiff", overwrite=TRUE);
    
    #shp<-rasterToPolygons(rst, na.rm=TRUE, digits=1, dissolve=T);
    #arq_shp<-paste(dir_shp,arv,'\\','arv_',arv,'_hi_',gsub('[.]','_',hi),'.shp',sep='');
    comm <- paste('python ..\\pyscript\\polygonize.py', arq_tif2, '-f "ESRI Shapefile" -8  -q -nomask -b 1',arq_shp);

    #system(comm, show.output.on.console = F);
    system(comm);
    unlink(arq_tif2);
    
    #python polygonize.py "idwg.tif" -f "ESRI Shapefile" -8  -q -nomask -b 1 "idwg.shp"
    #src_filename: idwg.tif
    #ogr_format: ESRI Shapefile
    #options: 8CONNECTED=8
    #quiet_flag: 1
    #nomask: none
    #b: 1
    #dst_filename: idwg.shp
    
    shp <- readOGR(arq_shp,ogrListLayers(arq_shp)[1]);
    #shp<-aggregate(shp,by='DN');

    ##Selecionando os shapes do disco e do objeto de referência
    
    npoly<-nrow(shp)

    poly<-data.frame(np=1:npoly,id=NA,area=NA);
    for(i in 1:npoly){
      poly$id[i]<-shp@polygons[[i]]@ID
      poly$area[i]<-shp@polygons[[i]]@area
    }
    
    poly<-poly[order(poly$area, decreasing = T),];
    
    npd<-poly$np[1];
    npr<-poly$np[2];
    
    dshp<-shp[poly$np[1],];
    rshp<-shp[poly$np[2],];
    
    ##Calculando a área do disco e do objeto de referência
    
    objetos<-c('disco','objref');
    
    for (obj in objetos){
      if(obj=='disco'){sshp<-dshp}else{sshp<-rshp}

      nspoly<-length(sshp@polygons[[1]]@plotOrder);
      spoly<-data.frame(np=1:nspoly,area=NA);
      for(i in 1:nspoly){
        spoly$area[i]<-sshp@polygons[[1]]@Polygons[[i]]@area
      }
      spoly<-spoly[order(spoly$area, decreasing = T),];
      
      areasel<-spoly$area[1];

      i<-1;
      nrm<-1;
      while(i<=nspoly){
        area<-sshp@polygons[[1]]@Polygons[[nrm]]@area
        if(area==areasel){
          nrm<-2;
        }else{
          sshp@polygons[[1]]@Polygons[[nrm]]<-NULL
        }
        i<-i+1;
      }
      
      if(obj=='disco'){dshp<-sshp}else{rshp<-sshp}
      
    }
    
    area_disco<-area(dshp);
    area_objref<-area(rshp);
    area_disco_m2<-area_disco/area_objref*area_ref;

    if(plotar){
      graphics.off(); x11(400,400); 
      par(mai=c(0.25,0.25,0.25,0.25)); #reduzindo as margens do gráfico
      img_jpg <- image_read(arq_jpg2);
      #plot(img_jpg, main=paste('Árvore ',arv,' - hi = ',hi,'m',sep=''));
      plot(img_jpg);
      plot(dshp,add=T, border='red', lwd=3);
      text(extent(rshp)[2],extent(rshp)[4],paste('Área do disco:', round(area_disco_m2,4), 'm²'), pos=3, cex=1.3);
      cat('Digite 1 para salvar, 0 para descartar o disco ou 2 para parar','\n'); 
      r<-as.vector(scan(what = 'integer', nmax = 1)); 
      #r<-2;
    }else{
      r<-1;
    }

    if(r==2){break}
    
    if(r==1){    

      #arq_jpg3<-paste(dir_jpg3,arv,'\\',discos[d],sep='');
      arq_jpg3<-paste(dir_jpg3,discos[d],sep='');
      jpeg(file = arq_jpg3,units='cm', width=15, height = 15, res=72, quality=100);
      par(mai=c(0.25,0.25,0.25,0.25)); #reduzindo as margens do gráfico
      img_jpg <- image_read(arq_jpg2);
      plot(img_jpg);
      plot(dshp,add=T, border='red', lwd=2);
      plot(rshp,add=T, border='green', lwd=2);
      text(extent(rshp)[2],extent(rshp)[4],paste('Área do disco:', round(area_disco_m2,4), 'm²'), pos=3, cex=1.3);
      dev.off(); graphics.off();
      
      fnesc<-sqrt(area_objref/area_ref); #Fator para cálculo da nova escala em m²
      #sprintf("%f", fnesc);
  
      nxy<-dshp@polygons[[1]]@Polygons[[1]]@coords/fnesc;
      p_xy<-Polygons(srl=list(Polygon(nxy)),ID='ID')
      sp_xy<-SpatialPolygons(Srl=list(p_xy),pO = as.integer(1))
      dados<-data.frame(DN=0,row.names = 'ID');
      spd_xy<-SpatialPolygonsDataFrame(sp_xy,data=dados);

      #54021: World_Polyconic
      proj4string(spd_xy)<-'+proj=poly +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'

      ###Exportanto os resultados no formato SHP
      arq_shp2<-paste(dir_shp2,arv,'\\','arv_',arv,'_hi_',gsub('[.]','_',hi),'.shp',sep='');
      writeOGR(obj=spd_xy, dsn=arq_shp2, layer='shp', morphToESRI=T, overwrite_layer=T, delete_dsn=T, driver="ESRI Shapefile");
      ##-----
      
      #area_disco_2<-sf::st_area(sf::st_polygon(list(spd_xy@polygons[[1]]@Polygons[[1]]@coords)));
      perimetro<-as.vector(sf::st_length(sf::st_sfc(sf::st_linestring(spd_xy@polygons[[1]]@Polygons[[1]]@coords), crs = 54021))*100);
      
      #shp_latlong<-spTransform(spd_xy,CRS('+init=epsg:4326'));
      #perimetro<-geosphere::perimeter(shp_latlong@polygons[[1]]@Polygons[[1]]@coords)*100;
      #area_disco_2<-geosphere::areaPolygon(shp_latlong@polygons[[1]]@Polygons[[1]]@coords)
      resproc<-rbind(resproc,data.frame(arv=arv,hi=hi,perimetro=perimetro,gi=area_disco_m2));
    }else{
      descarte<-rbind(descarte,data.frame(arv=arv,hi=hi));
    }
  }
  if(r==2){break}
}

if(!is.null(resproc)){
  #resproc<-resproc[-1,];
  write.csv2(resproc,arq_resproc,row.names = F);
}

if(!is.null(descarte)){
  #descarte<-descarte[-1,];
  write.csv2(descarte,arq_descarte,row.names = F);
}
file.remove('..\\dados\\temp.shp');
file.remove('..\\dados\\temp.dbf');
file.remove('..\\dados\\temp.shx');
