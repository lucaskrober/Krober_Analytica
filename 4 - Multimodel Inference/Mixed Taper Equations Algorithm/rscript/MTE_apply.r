# Multimodel Inference Technique
# Applying the Mixed Taper Equations (MTE) proposed by Bernardi (2020)

# Loading the fitted models for each genetic material
load(file='..\\dados\\models_AEC0144.rda');
load(file='..\\dados\\models_IPB1.rda');
load(file='..\\dados\\models_VT01.rda');

# reading reference volume dataframe
cub=read.csv2('..\\dados\\visc_seccao_xilometro.csv')
cub=subset(cub,!arv %in% c(13,21,25,27,34,35,36,74,64,66))#removing outlier

#removing unused columns
cub$hi1=NULL
cub$l1=NULL
cub$l2=NULL

#reading dataframe containing diameter estimative through digital images interpretation 
cub2=read.csv2('..\\dados\\cubagem_fotos.csv')
cub2$arv=cub2$arvore
cub2$arvore=NULL
cub2=subset(cub2,!arv %in% c(13,21,25,27,34,35,36,74,64,66))

cub=merge(cub,cub2)

cub<-cub[order(cub$arv,cub$hi),]

#run once for each genetic material
cub=subset(cub,matgen=="AEC 0144")
cub=subset(cub,matgen=="IPB1")
cub=subset(cub,matgen=="VT01")


#calculating diameter with the MTE algorithm
fdMTE<-function(hi,dap,ht,models){
  ipeso<-models[[1]]$peso$hi>=hi[1];
  ipeso<-(1:length(ipeso))[!duplicated(ipeso) & ipeso==T];
  di<-0;
  for(m in 1:length(models)){
    di<-di+(models[[m]]$fdest(hi,dap,ht,models[[m]]$parms))*models[[m]]$peso$peso[ipeso]
  }
  return(di)
}

#calculating sectional area with the MTE algorithm
fgMTE<-function(hi,dap,ht,models){
  return((pi*fdMTE(hi,dap,ht,models)^2)/40000)
}

#calculating volume with the MTE algorithm
fvMTE<-function(hi,dap,ht,models,hi_lower=0.2,subdivisions = 100L){
  return(integrate(fgMTE,lower=hi_lower,upper=hi,dap=dap,ht=ht,models=models, subdivisions=subdivisions)$value)
}

#fdMTE(hi,dap,ht,models);
#fgMTE(hi,dap,ht,models);
#fvMTE(hi,dap,ht,models)


for(i in 1:nrow(cub)){
  cub$dmte[i]<-fdMTE(hi=cub$hi[i],dap=cub$dap[i],ht=cub$ht[i],models)
}

#modify the dataframe pairing every observation with its next observation creating relative height 1 and 2
#h1 and h2 should be the lower and upper limits for the integrate function  
library(cmrinvflor) #unpublished package
#unpublished function (parear_seqmed) that does the pairing process mentioned above
cub=parear_seqmed(
  cub[,c('arv','hi','xvisc','dap','ht','disc','dmte')]);

#volume calculation 
for(i in 1:nrow(cub)){
  cub$vmte[i]<-fvMTE(hi=cub$hi2[i],dap=cub$dap1[i],ht=cub$ht1[i],models,hi_lower=cub$hi1[i])
}

write.csv2(cub,'..\\dados\\AEC0144.csv',row.names=F)
write.csv2(cub,'..\\dados\\IPB1.csv',row.names=F)
write.csv2(cub,'..\\dados\\VT01.csv',row.names=F)

write.csv2(cub,'..\\dados\\AEC0144_diametro.csv',row.names=F)
write.csv2(cub,'..\\dados\\IPB1_diametro.csv',row.names=F)
write.csv2(cub,'..\\dados\\VT01_diametro.csv',row.names=F)
