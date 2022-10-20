setwd("/shared/ggdrive/lb_sample/dados/shp/2")
#setwd("G:/Drives compartilhados/lb_sample/dados/shp/2")

shp1<-rgdal::readOGR('arv_2_hi_0_2.shp')
shp2<-rgdal::readOGR('arv_2_hi_0_7.shp')

library(dplyr)
library(ggplot2)

#fortify: extrair latitude e longitude em escala de cm para as coordenadas do shape
shp1 <- ggplot2::fortify(shp1)
shp1 <- shp1 %>% dplyr::select(long,lat)

shp2 <- ggplot2::fortify(shp2) 
shp2 <- shp2 %>% dplyr::select(long,lat)

shp1$z<-0.2
shp2$z<-0.7

sec1<-rbind(shp1,shp2)
names(sec1)<-c("Var1","Var2","Var3")
sec1<-as.matrix(sec1)

#output.options="Fa" pra retornar a opção de calcular as isoáreas, o somatório delas é igual ao volume
#do tetraedro resultante
tora <- geometry::delaunayn(sec1, output.options="Fa")
vol_1<-sum(tora$areas);vol_1
#visualizar tora resultante (não abriu no servidor mas abre no rstudio do pc)
tora3d <- geometry::delaunayn(sec1)
rgl::rgl.viewpoint(60)
rgl::rgl.light(120,60)
geometry::tetramesh(tora3d,sec1, alpha=0.9)
