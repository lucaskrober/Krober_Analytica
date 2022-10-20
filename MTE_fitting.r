# Multimodel Inference techinque
# Applying the Mixed Taper Equations Algorithm (BERNARDI, 2020)
# using upper-stem diameter measurements from 3 clonal Eucalyptus stands

cub=read.csv2('..\\dados\\cubagem_fotos.csv') #opening dataframe

#Removing outliers (later indentified)
#cub=subset(cub,!arvore %in% c(13,21,25,27,34,36,35,74,64,66))
#cub=subset(cub,!arvore %in% c(34,74,63,64,66))
#cub<-subset(cub,!arv %in% c(21,38,64,73,42));
#renaming some columns
cub$idarvore<-cub$arvore;
cub$di<-cub$disc;

# for later processing, run one genetic material at a time
matgen<-'AEC 0144';
matgen<-'IPB1';
matgen<-'VT01';

#filtering dataframe by genetic material
cub<-cub[cub$matgen==matgen,];

nream<-100; # Resamples number, the minimum suggested is ream<-nrow(mydf)-1
depvar<-'di'; # Dependent variable

# Example of inputting taper equations
# fdest stands for: function for estimate diameter 
# linear should be TRUE if the equation is linear and FALSE if it is non-linear
models<-NULL;
models[[1]]<-list(nom_model='Kozak',
                  model='di~b0*(dap**b1)*(ht**b2)*(((1-((hi/ht)^(1/3)))/(1-((1.3/ht)**(1/3))))**(b3*(hi/ht)**4+b4*(1/exp(dap/ht))+(b5*(((1-((hi/ht)**(1/3)))/(1-((1.3/ht)**(1/3))))**0.1))+(b6*(1/dap))+(b7*(ht**(1-((hi/ht)**(1/3)))))+(b8*((1-((hi/ht)**(1/3)))/(1-((1.3/ht)**(1/3)))))))',
                  fdest=function(hi,dap,ht,parms) {
                    return(with(parms,b0*(dap**b1)*(ht**b2)*(((1-((hi/ht)**(1/3)))/(1-((1.3/ht)**(1/3))))**(b3*(hi/ht)**4+b4*(1/exp(dap/ht))+(b5*(((1-((hi/ht)**(1/3)))/(1-((1.3/ht)**(1/3))))**0.1))+(b6*(1/dap))+(b7*(ht**(1-((hi/ht)**(1/3)))))+(b8*((1-((hi/ht)**(1/3)))/(1-((1.3/ht)**(1/3)))))))))
                  },
                  linear=F,
                  start=list(b0=1.056,b1=0.966,b2=0.018,b3=0.338,b4=-0.374, b5=0.558, b6=0.9709, b7=0.022, b8=-0.23)
             );

models[[2]]<-list(nom_model='5grau',
                  model='di~dap*(b0+b1*(hi/ht)+b2*(hi/ht)**2+b3*(hi/ht)**3+b4*(hi/ht)**4+b5*(hi/ht)**5)',
                  fdest=function(hi,dap,ht,parms) {
                    return(with(parms,dap*(b0+b1*(hi/ht)+b2*(hi/ht)**2+b3*(hi/ht)**3+b4*(hi/ht)**4+b5*(hi/ht)**5)))
                  },
                  linear=F,
                  start=list(b0=1.17,b1=-4,b2=20,b3=-43,b4=42,b5=-15)
             );

models[[3]]<-list(nom_model='Muhairwe',
                  model='di~a0*(dap^a1)*(a2^dap)*(1-sqrt(hi/ht))^((b1*(hi/ht)^2)+b2/(hi/ht)+b3*dap+b4*ht+b5*(dap/ht))',
                  fdest=function(hi,dap,ht,parms) {
                    return(with(parms,a0*(dap^a1)*(a2^dap)*(1-sqrt(hi/ht))^((b1*(hi/ht)^2)+b2/(hi/ht)+b3*dap+b4*ht+b5*(dap/ht))))
                  },
                  linear=F,
                  start=list(a0=1.5836,a1=0.7881,a2=1.0099,b1=0.2743,b2=-0.0092,
                             b3=-0.0164,b4=0.0027,b5=0.7561)
);

models[[4]]<-list(nom_model='Max',
                  model='di~dap*(b1*((hi/ht)-1)+b2*((hi/ht)^2-1)+(b3*(a1-(hi/ht))^2)*((hi/ht)<=a1)+(b4*(a2-(hi/ht))^2)*((hi/ht)<=a2))^0.5',
                  fdest=function(hi,dap,ht,parms) {
                    return(with(parms,dap*(b1*((hi/ht)-1)+b2*((hi/ht)^2-1)+(b3*(a1-(hi/ht))^2)*((hi/ht)<=a1)+(b4*(a2-(hi/ht))^2)*((hi/ht)<=a2))^0.5))
                  },
                  linear=F,
                  start=list(a1=0.7431,a2=0.1125,b1=-3.0257,b2=1.4586,b3=-1.44,b4=39.1081)
);





##Classic regression fitting process
for(m in 1:length(models)){
  if(models[[m]]$linear){
    aj<-lm(models[[m]]$model,cub);
  }else{
    aj<-nls(models[[m]]$model,cub,start=models[[m]]$start); 
  }
  models[[m]]$parms<-as.list(coef(aj));
}

idarvs<-sort(unique(cub$idarvore)); #ordering dataframe by tree ID
na<-ceiling(length(idarvs)/2);  #number of observations for the fitting process

fhidsw<-function(ream,cub,idarvs,na){
  converged<-F;
  while(!converged){
    sidarvs<-sample(idarvs,na,replace=F);
    saa<-subset(cub,idarvore %in% sidarvs);  #Selecting trees for fitting
    saw<-subset(cub,!idarvore %in% sidarvs); #Selecting tress for weight calculation
    
    hisaa<-aggregate(list(nobs=saa$di),list(hi=saa$hi),length);
    hisaa<-subset(hisaa,nobs>=10); # relative height of selected trees with minimum of 10 observations
	
    hisaw<-aggregate(list(nobs=saw$di),list(hi=saw$hi),length);
    hisaw<-subset(hisaw,nobs>=10);
    
    hisaw<-subset(hisaw,hi %in% hisaa$hi);
    
    hidsw<-NULL;
    for(m in 1:length(models)){
      if(models[[m]]$linear){
        aj<-try(lm(models[[m]]$model,saa),silent=T);
      }else{
        aj<-try(nls(models[[m]]$model,saa,start=models[[m]]$parms),silent=T); 
      }
      if(class(aj)=='try-error'){
        converged<-F;
        break;
      }else{
        converged<-T;
        erro1<-cbind(saa$hi,as.vector(saa[depvar])-predict(aj,saa)); names(erro1)<-c('hi','erro'); #calculating errors
        erro2<-cbind(saw$hi,as.vector(saw[depvar])-predict(aj,saw)); names(erro2)<-c('hi','erro');
    
        dsw<-aggregate(list(dsw1=erro2$erro^2),list(hi=erro2$hi),sum);
        dsw<-subset(dsw,hi %in% hisaw$hi);
        
        dsw2<-aggregate(list(calc1=erro1$erro^2),list(hi=erro1$hi),sum);
        calc<-aggregate(list(n=erro1$erro),list(hi=erro1$hi),length);
        dsw2<-merge(dsw2,calc);
        dsw2$dsw2<-with(dsw2,sqrt(calc1/n));
    
        dsw<-merge(dsw,dsw2[,c('hi','dsw2','n')]);
        rm(dsw2,calc);
        
        #dsw[m,1]<-sum(erro2^2); dsw[m,2]<-sqrt(sum(erro1^2)/nrow(saa)); dsw[m,3]<-(dsw[m,2]^(-nrow(saa)))*exp((-1/dsw[m,2]^2)*dsw[m,1]/2);
        dsw$dsw3<-with(dsw,(dsw2^(-n))*exp((-1/dsw2^2)*dsw1/2));
        dsw$n<-NULL; dsw$dsw1<-NULL; dsw$dsw2<-NULL;
        dsw$model<-models[[m]]$nom_model;
        dsw$ream<-ream;
        hidsw<-rbind(hidsw,dsw);
      }
    }
  }
  return(hidsw)
}

print(system.time(
  hidsw<-Reduce('rbind',lapply(1:nream,fhidsw,cub,idarvs,na))
));

swk<-with(hidsw,aggregate(list(sdsw3=dsw3),list(ream=ream,hi=hi),sum));
wk<-merge(hidsw,swk);

wk$peso<-wk$dsw3/wk$sdsw3;

wkf<-aggregate(list(peso=wk$peso),list(hi=wk$hi,model=wk$model),mean);
calc<-aggregate(list(n=wk$peso),list(hi=wk$hi,model=wk$model),length);
wkf<-merge(wkf,calc);
wkf<-wkf[order(wkf$hi,wkf$model),];

for(m in 1:length(models)){
  swkf<-subset(wkf,model==models[[m]]$nom_model, select=c('hi','peso'));
  swkf<-swkf[order(swkf$hi),];
  swkf$hi<-c(swkf$hi[-nrow(swkf)]+diff(swkf$hi)/2,Inf);
  models[[m]]$peso<-swkf;
}  

##Cleaning the object models
for(m in 1:length(models)){
  models[[m]]$model<-NULL;
  models[[m]]$linear<-NULL;
  models[[m]]$start<-NULL;
}


save(models,file=paste0('..\\dados\\models_',gsub(' ','',matgen),'.rda'));

#run again for each genetic material to save a .rda file for each one
