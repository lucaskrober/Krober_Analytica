rm(list=ls(all=T)); graphics.off();

library(cmrinvflor);

cub=read.csv2('..\\dados\\cubagem_eucatex.csv', stringsAsFactors = F);
#names(cub); #View(cubmg)
#cubmg$idarvore<-cubmg$arvore;
scub<-smalian(cub);

scub$cldap<-cut(scub$dap,
                breaks=quantile(scub$dap,probs = c(0,0.33,0.66,1)),
                include.lowest = T);
names(scub);
#tapply(scub$idarvore,scub$cldap,length);
table(scub$cldap);

stratum='cldap';
pkSample<-'idarvore';
varint<-'vtcc';
#cubmg[[varint]]<-cubmg$vtcc;

nResamp<-length(unique(scub[[pkSample]]))-1; #Número de reamostragens.
percResamp = 0.5; #Porcentagem de amostras para ajuste em cada reamostragem
mnsra<-5; #Número mínimo de amostras para ajuste por estrato em cada reamostragem
mnsrv<-5; #Número mínimo de amostras para validação por estrato em cada reamostragem

models<-NULL;
models[[1]]<-list(nameModel='SPURRlog',
                  model='I(log(vtcc))~I(log(dap^2*ht))',
                  fpred=function(dat,ps) {
                    return(with(dat,exp(ps[[1]]+ps[[2]]*log(dap^2*ht))));
                  },
                  linear=T,
                  start=list(list(b0=1.17,b1=-4,b2=20,b3=-43,b4=42,b5=-15))
);
models[[2]]<-list(nameModel='SchHall',
                  model='vtcc~b0*dap^b1*ht^b2',
                  fpred=function(dat,ps) {
                    return(with(dat,ps$b0*dap^ps$b1*ht^ps$b2));
                  },
                  linear=F,
                  start=list(list(b0=pi*0.5/40000,b1=2,b2=1))
);

smodels<-arm(models=models,dfData = scub,varint = varint,
            stratum = stratum,pkSample = pkSample,
            nResamp = nResamp, percResamp = percResamp,
            resampStratum = T,mnsra = mnsra, mnsrv = mnsrv);

save(smodels,file='..\\dados\\smodels_eucatex.rda');

for(i in 1:length(smodels)){
  scub[[paste0('v',smodels[[i]]$nameModel)]]<-smodels[[i]]$fpred(scub,smodels[[i]]$parms)
}
scub$varm<-predict(models = smodels,dfData = scub,stratum = stratum,
                   pkSample = pkSample,sort = T)$pred;

View(scub);

# models<-setRefClass("arm",
#                     fields=list(nameModel="character",
#                                 model="formula",
#                                 linear="logical",
#                                 fpred="function",
#                                 parms="list",
#                                 start="list")
# );

