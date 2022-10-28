#library(dplyr)

dir_arvs<-list.dirs('../dados/shp', full.names=T, recursive=F);
#arvore 43 (que está na posição 36) tem um shape com hi NA, além disso, hi 17 e 18 não existem
#remover árvore 43:
dir_arvs<-dir_arvs[-36]

fshp<-function(shp){
  s<-Reduce('c',strsplit(shp,'[_.]'))
  return(data.frame(
    arv=as.integer(s[2]),
    hi=as.numeric(ifelse(length(s)>5,paste0(s[4],".",s[5]),s[4])),
    shp=file.path('..','dados','shp',s[2],shp),stringsAsFactors = F
  ))
}

fahs<-function(dir_arv) Reduce('rbind',lapply(dir(dir_arv, pattern = ".shp"),fshp))

ahs<-fahs(dir_arvs);
ahs<-ahs[order(ahs$arv,ahs$hi),];

#ahs<-subset(ahs,arv<=5)

fvol<-function(t){
  cat(paste0('arv=',t$arv[1],' - hi1=',t$hi[1],' - hi2=',t$hi[2]),'\n')
  shp1<-rgdal::readOGR(t$shp[1])
  shp2<-rgdal::readOGR(t$shp[2])
  
  shp1 <- ggplot2::fortify(shp1)
  #shp1 <- shp1 %>% dplyr::select(long,lat)
  shp1 <- shp1[,c('long','lat')]
  
  shp2 <- ggplot2::fortify(shp2) 
  #shp2 <- shp2 %>% dplyr::select(long,lat)
  shp2 <- shp2[,c('long','lat')]
  
  shp1$z<-t$hi[1]
  shp2$z<-t$hi[2]
  
  sec1<-rbind(shp1,shp2)
  names(sec1)<-c("Var1","Var2","Var3")
  sec1<-as.matrix(sec1)
  
  #output.options="Fa" pra retornar a opção de calcular as isoáreas, 
  #o somatório delas é igual ao volume do tetraedro resultante
  return(data.frame(
    arv=t$arv[1],
    hi1=t$hi[1],
    hi2=t$hi[2],
    vol=sum(geometry::delaunayn(sec1, output.options="Fa")$areas)  
  ))
}
  
fvtoras<-function(ahs) Reduce("rbind",lapply(split(ahs,ahs$arv), function(hs) Reduce("rbind",cmrsample::seqApply(hs,fvol))))
voltoras<-fvtoras(ahs)

volarv<-with(voltoras, aggregate(list(vol=vol),list(arv=arv),sum))

write.csv2(volarv,'vol_triang.csv',row.names=F)

dir()

volarv<-read.csv2('../dados/vol_triang.csv')
comp<-read.csv2('../dados/cubagem_final.csv')
names(comp)

library(dplyr)
comp2<-subset(comp,!duplicated(comp$arvore))
comp3<-comp2 %>%
  dplyr::select(arvore,xvisc)
names(volarv)
names(comp3)[1]<-'arv'
comp4<-NULL
comp4<-merge(comp3,volarv)



for(i in 1:nrow(comp4)){
  comp4$dif[i]<-ifelse(comp4$xvisc[i]<comp4$vol[i],comp4$xvisc[i]/comp4$vol[i],comp4$vol[i]/comp4$xvisc[i])
}


summary(comp4$dif)
mean(comp4$dif)

write.csv2(comp4,'../dados/vol_comparativo.csv',row.names=F)


# #fortify: extrair latitude e longitude para as coordenadas do shape
# #visualizar tora resultante (não abriu no servidor mas abre no rstudio do pc)
# tora3d <- geometry::delaunayn(sec1)
# rgl::rgl.viewpoint(60)
# rgl::rgl.light(120,60)
# geometry::tetramesh(tora3d,sec1, alpha=0.9)
