f_poly_foto<-function(arv,hi,area_ref,rn){
  arq_jpg2<-paste('..\\dados\\fotos\\jpg2\\',arv,'\\','arv_',arv,'_hi_',gsub('[.]','_',hi),'.jpg',sep='');
  arq_shp<-'..\\dados\\temp.shp';
  arq_shp2<-paste('..\\dados\\fotos\\shp2\\',arv,'\\','arv_',arv,'_hi_',gsub('[.]','_',hi),'.shp',sep='');
  arq_jpg3<-paste('..\\dados\\fotos\\jpg3\\arv_',arv,'_hi_',gsub('[.]','_',hi),'.jpg',sep='');

  img <- brick(arq_jpg2); #Ler arquivo multispectral
  names(img)<-c('r','g','b');
  
  pnt<-as.data.frame(values(img));
  
  nc<-as.integer(predict(rn, pnt, type="class"));
  
  nc[nc==2]<-NA;
  rst<-raster(img);
  values(rst)<-nc;
  
  arq_tif2<-tempfile(fileext = ".tif");
  writeRaster(rst,arq_tif2,"GTiff", overwrite=TRUE);
  
  comm <- paste('python ..\\pyscript\\polygonize.py', arq_tif2, '-f "ESRI Shapefile" -8  -q -nomask -b 1',arq_shp);
  #system(comm, show.output.on.console = F);
  system(comm);

  unlink(arq_tif2);
  
  shp <- readOGR(arq_shp,ogrListLayers(arq_shp)[1]);

  file.remove('..\\dados\\temp.shp');
  file.remove('..\\dados\\temp.dbf');
  file.remove('..\\dados\\temp.shx');
  
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
    
  jpeg(file = arq_jpg3,units='cm', width=15, height = 15, res=72, quality=100);
  par(mai=c(0.25,0.25,0.25,0.25)); #reduzindo as margens do gráfico
  img_jpg <- image_read(arq_jpg2);
  plot(img_jpg);
  plot(dshp,add=T, border='red', lwd=2);
  plot(rshp,add=T, border='green', lwd=2);
  text(extent(rshp)[2],extent(rshp)[4],paste('Área do disco:', round(area_disco_m2,4), 'm²'), pos=3, cex=1.3);
  dev.off(); graphics.off();
  
  fnesc<-sqrt(area_objref/area_ref); #Fator para cálculo da nova escala em m²

  nxy<-dshp@polygons[[1]]@Polygons[[1]]@coords/fnesc;
  p_xy<-Polygons(srl=list(Polygon(nxy)),ID='ID')
  sp_xy<-SpatialPolygons(Srl=list(p_xy),pO = as.integer(1))
  dados<-data.frame(DN=0,row.names = 'ID');
  spd_xy<-SpatialPolygonsDataFrame(sp_xy,data=dados);
  
  #54021: World_Polyconic
  proj4string(spd_xy)<-'+proj=poly +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
  
  ###Exportanto os resultados no formato SHP
  writeOGR(obj=spd_xy, dsn=arq_shp2, layer='shp', morphToESRI=T, overwrite_layer=T, delete_dsn=T, driver="ESRI Shapefile");
  ##-----
  
  perimetro<-as.vector(sf::st_length(sf::st_sfc(sf::st_linestring(spd_xy@polygons[[1]]@Polygons[[1]]@coords), crs = 54021))*100);
  
  return(data.frame(arv=arv,hi=as.numeric(hi),perimetro=perimetro,gi=area_disco_m2));
}
