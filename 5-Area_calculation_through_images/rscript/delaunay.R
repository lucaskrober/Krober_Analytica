dir()
arv<-read.csv2('arv_hi_xy.csv')

setwd("/shared/ggdrive/lb_sample/dados")
dir()
arv<-read.csv2('arv_hi_xy.csv')

library(geometry)

arv1<-subset(arv,arv==2)
unique(arv1$hi)
seg1<-subset(arv,hi<=0.7)

dpts <- ggplot2::fortify(cir1)
cir1 <- dpts %>% 
  dplyr::select(long,lat)


seg1$arv<-NULL
seg1$Var3<-seg1$hi
seg1$hi<-NULL
names(seg1)<-c("Var1","Var2","Var3")
seg1$Var3<-as.numeric(seg1$Var3)
seg1$Var3<-seg1$Var3*100
seg1<-as.matrix(seg1)

?delaunayn
tr<-geometry::delaunayn(seg1,output.options="Fa")
vol<-sum(tr$areas)


library(berryFunctions)
library(geometry)
library(maptools);
library(raster);
library(geoR);
library(rgdal)
library(dplyr)

str(arv)
arv$hi<-as.numeric(arv$hi)
list_arv<-split(arv,arv$arv)
list_cir<-NULL

for(i in 1:length(list_arv)){
  list_cir[[i]]<-split(list_arv[[i]],list_arv[[i]]$hi)
}


lista2 <- vector("list",length(list_cir))
for (i in 1:length(list_cir)){
  for (j in 1:(length(list_cir[[i]])-1)){
    lista2[[i]][[j]] <- rbind(list_cir[[i]][[j]], list_cir[[i]][[j+1]])
  }
}

for (i in 1:length(lista2)){
  for (j in 1:(length(lista2[[i]]))){
    lista2[[i]][[j]]$arv<-NULL
    lista2[[i]][[j]]$Var1<-lista2[[i]][[j]]$x
    lista2[[i]][[j]]$Var2<-lista2[[i]][[j]]$y
    lista2[[i]][[j]]$Var3<-lista2[[i]][[j]]$hi*100
    lista2[[i]][[j]]$hi<-NULL
    lista2[[i]][[j]]$x<-NULL
    lista2[[i]][[j]]$y<-NULL
  }
}


triang<-vector("list",length(lista2))
tr_del<-vector("list",length(triang))
lista_vest<-vector("list",length(list_cir))
for (i in 1:length(lista2)){
  for (j in 1:(length(lista2[[i]]))){
    triang[[i]][[j]]<-as.matrix(lista2[[i]][[j]])
    tr_del[[i]][[j]]$vdel<-delaunayn(triang[[i]][[j]],output.options="Fa")
    lista_vest[[i]][[j]]<-sum(tr_del[[i]][[j]][["vdel"]]$areas)/1000000
    paste(j)
  }
}

#pc é a matriz dos dados var 1, 2 e 3 
#tc é o arquivo com a triangulação

tc1 <- delaunayn(pc1)
tc2 <- delaunayn(pc2)

#plota o 3D interativo da seção do tronco
rgl::rgl.viewpoint(60)
rgl::rgl.light(120,60)
tetramesh(tc1,pc1, alpha=0.9)
tetramesh(tc2,pc2, alpha=0.9)