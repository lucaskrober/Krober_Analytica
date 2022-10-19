##Aplicação da MTE: Estimativa do diâmetro, área seccional e volume
#Restaurando o objeto models
load(file='..\\dados\\models_AEC0144.rda');
load(file='..\\dados\\models_IPB1.rda');
load(file='..\\dados\\models_VT01.rda');

cub=read.csv2('..\\dados\\visc_seccao_xilometro.csv')
cub=subset(cub,!arv %in% c(13,21,25,27,34,35,36,74,64,66))
cub$hi1=NULL
cub$l1=NULL
cub$l2=NULL

names(cub); #View(cub)

cub2=read.csv2('..\\dados\\cubagem_fotos.csv')
cub2$arv=cub2$arvore
cub2$arvore=NULL
cub2=subset(cub2,!arv %in% c(13,21,25,27,34,35,36,74,64,66))
names(cub2)

cub=merge(cub,cub2)

cub<-cub[order(cub$arv,cub$hi),]

names(cub)

View(cub)

cub=subset(cub,matgen=="AEC 0144")
cub=subset(cub,matgen=="IPB1")
cub=subset(cub,matgen=="VT01")


fdMTE<-function(hi,dap,ht,models){
  ipeso<-models[[1]]$peso$hi>=hi[1];
  ipeso<-(1:length(ipeso))[!duplicated(ipeso) & ipeso==T];
  di<-0;
  for(m in 1:length(models)){
    di<-di+(models[[m]]$fdest(hi,dap,ht,models[[m]]$parms))*models[[m]]$peso$peso[ipeso]
  }
  return(di)
}

fgMTE<-function(hi,dap,ht,models){
  return((pi*fdMTE(hi,dap,ht,models)^2)/40000)
}

fvMTE<-function(hi,dap,ht,models,hi_lower=0.2,subdivisions = 100L){
  return(integrate(fgMTE,lower=hi_lower,upper=hi,dap=dap,ht=ht,models=models, subdivisions=subdivisions)$value)
}

#fdMTE(hi,dap,ht,models);
#fgMTE(hi,dap,ht,models);
#fvMTE(hi,dap,ht,models)

for(i in 1:nrow(cub)){
  cub$dmte[i]<-fdMTE(hi=cub$hi[i],dap=cub$dap[i],ht=cub$ht[i],models)
}

View(cub)

#parear a base e criar hi1 e hi2 
library(cmrinvflor)
cub=parear_seqmed(
  cub[,c('arv','hi','xvisc','dap','ht','disc','dmte')]);

for(i in 1:nrow(cub)){
  cub$vmte[i]<-fvMTE(hi=cub$hi2[i],dap=cub$dap1[i],ht=cub$ht1[i],models,hi_lower=cub$hi1[i])
}

View(cub)

write.csv2(cub,'..\\dados\\AEC0144.csv',row.names=F)
write.csv2(cub,'..\\dados\\IPB1.csv',row.names=F)
write.csv2(cub,'..\\dados\\VT01.csv',row.names=F)

write.csv2(cub,'..\\dados\\AEC0144_diametro.csv',row.names=F)
write.csv2(cub,'..\\dados\\IPB1_diametro.csv',row.names=F)
write.csv2(cub,'..\\dados\\VT01_diametro.csv',row.names=F)
