setwd("~/crthiersch_servidor/backup/2019_01/trab_diversos/lacan_cubagem/dados/fotos/shp2")

library(rgdal);

arv<-sort(as.integer(dir()));

arv_hi_xy<-NULL;

for(i in 1:length(arv)){
  shps<-dir(file.path('.',arv[i]), pattern = ".shp")

  for(j in 1:length(shps)){
    hi<-Reduce('c',strsplit(shps[j],'[_.]'))
    hi<-ifelse(length(hi)>5,paste0(hi[4],".",hi[5]),hi[4]);
    
    xy<-readOGR(file.path('.',arv[i],shps[j]))
    xy<-as.data.frame(xy@polygons[[1]]@Polygons[[1]]@coords)
    
    xy<-cbind(arv[i],hi,xy)
    names(xy)<-c('arv','hi','x','y')
    arv_hi_xy<-rbind(arv_hi_xy,xy)
  }
}

write.csv2(arv_hi_xy,'arv_hi_xy.csv',row.names=F)


