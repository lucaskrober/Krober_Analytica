##Aplicação da MTE: Estimativa do diâmetro, área seccional e volume

#Restaurando o objeto models
load(file='..\\dados\\models_AEC0144.rda');
load(file='..\\dados\\models_IPB1.rda');
load(file='..\\dados\\models_VT01.rda');

pmeankoz=mean(models[[1]]$peso$peso);pmeankoz
pmeanscho=mean(models[[2]]$peso$peso);pmeanscho
pmeanmuh=mean(models[[3]]$peso$peso);pmeanmuh
pmeanmax=mean(models[[4]]$peso$peso);pmeanmax
#pmeanlee=mean(models[[5]]$peso$peso);pmeanlee

#Diâmetro
cub=read.csv2('..\\dados\\AEC0144_diametro.csv')
cub=read.csv2('..\\dados\\IPB1_diametro.csv')
cub=read.csv2('..\\dados\\VT01_diametro.csv')

#Volume
cub=read.csv2('..\\dados\\AEC0144.csv')
cub=read.csv2('..\\dados\\IPB1.csv')
cub=read.csv2('..\\dados\\VT01.csv')


fdc<-function(hi,dap,ht,models,nmodel){
  return(models[[nmodel]]$fdest(hi,dap,ht,models[[nmodel]]$parms))
}

fgc<-function(hi,dap,ht,models,nmodel){
  return((pi*fdc(hi,dap,ht,models,nmodel=nmodel)^2)/40000)
}

fvc<-function(hi,dap,ht,models,nmodel,hi_lower=0.2,subdivisions = 100L){
  return(integrate(fgc,lower=hi_lower,upper=hi,dap=dap,ht=ht,models=models,nmodel=nmodel,subdivisions=subdivisions)$value)
}

for(m in 1:length(models)){
  discm<-paste0('discm',m);
  cub[[discm]]<-NA;
  for(i in 1:nrow(cub)){
    cub[[discm]][i]<-fdc(hi=cub$hi[i],dap=cub$dap[i],ht=cub$ht[i],models=models,nmodel=m)
  }
}

for(m in 1:length(models)){
  vm<-paste0('vm',m);
  cub[[vm]]<-NA;
  for(i in 1:nrow(cub)){
    cub[[vm]][i]<-fvc(hi=cub$hi[i],dap=cub$dap[i],ht=cub$ht[i],models=models,nmodel=m,hi_lower=0.2)
  }
}

View(cub)

write.csv2(cub,'..\\dados\\AEC0144_diam_final.csv',row.names=F)
write.csv2(cub,'..\\dados\\IPB1_diam_final.csv',row.names=F)
write.csv2(cub,'..\\dados\\VT01_diam_final.csv',row.names=F)

write.csv2(cub,'..\\dados\\AEC0144_final.csv',row.names=F)
write.csv2(cub,'..\\dados\\IPB1_final.csv',row.names=F)
write.csv2(cub,'..\\dados\\VT01_final.csv',row.names=F)
