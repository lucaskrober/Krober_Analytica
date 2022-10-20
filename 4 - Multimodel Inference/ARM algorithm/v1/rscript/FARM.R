dir()
dados=read.csv2('..\\dados\\vol_eucatex.csv');#Eucatex
dados=read.csv2('..\\dados\\vol_eucatex2.csv');#Eucatex2
names(dados)
View(dados)

#rodar por partes
dados=subset(dados,idarvore %in% c(11,31,51,21,1,41,32,52,2,22,12,42,3,13,33,23,43,24,53,4))
dados=subset(dados,idarvore %in% c(34,35,25,54,14,5,44,55,15,26,36,45,6,16,37,7,46,27,28,56))
dados=subset(dados,idarvore %in% c(47,8,38,57,39,29,9,17,18,48,58,30,10,59,19,40,49,60,50,20))

dados=subset(dados,idarvore %in% c(31,41,21,1,11,2,12,42,32,22,33,3,43,44,4,13))
dados=subset(dados,idarvore %in% c(23,34,24,14,45,5,35,25,46,6,15,36,47,16,26,7,37))
dados=subset(dados,idarvore %in% c(48,17,27,8,38,9,49,28,39,18,10,19,40,50,29,20,30))

dados$vcomcc=dados$vcc

#icvint Identificação da coluna contendo a variável de interesse
icvint<-'vcomcc';
dfmodel<-data.frame(
  nom_model=c('Schlog','Spurrlog','Spurr','Schum'),
  model=c('I(log(vcomcc))~I(log(dap))+I(log(ht))',
          'I(log(vcomcc))~I(log(dap^2*ht))',
          'I(vcomcc)~I(dap^2*ht)',
          'vcomcc~b0*dap^b1*ht^b2'
  ),
  linear=c(T,T,T,F),
  type=c('log','log','nlog','nlog'),
  expr=c('b0*dap^b1*ht^b2',
         'b0+b1*dap^2*ht',
         'b0+b1*dap^2*ht',
         'b0*dap^b1*ht^b2'),
  stringsAsFactors = F
);


# dfmodel<-data.frame(
#   nom_model=c('Sch','Spurr'),
#   model=c('vcomcc~b0*dap^b1*ht^b2','vcomcc~I(dap^2*ht)'),
#   linear=c(F,T),
#   expr=c('b0*dap^b1*ht^b2','b0+b1*dap^2*ht'),
#   stringsAsFactors = F
# );


#ream<-nrow(dados)-1;   #Número de reamostragens
ream<-1000;
wk<-matrix(NA,ream,nrow(dfmodel)); # Vetor de pesos por reamostragem
na<-ceiling(nrow(dados)/2);  #Número de amostras para o ajuste
am<-1:nrow(dados);    #Vetor de identificadores dos dados

for(i in 1:ream){
  ii<-sample(am,na,replace=F);
  saa<-dados[ii,];
  saw<-dados[-ii,];
  dsw<-matrix(NA,nrow = nrow(dfmodel),3);
  for(m in 1:nrow(dfmodel)){
    if(dfmodel$linear[m]){
      aj<-lm(dfmodel$model[m],saa);
    }else{
      aj<-nls(dfmodel$model[m],saa,start=list(b0=1.504e-05,b1=2,b2=1)); #Melhorar
    }
    if(dfmodel$type[m]=='log'){
      erro1<-saa$vcomcc-exp(predict(aj,saa));
      erro2<-saw$vcomcc-exp(predict(aj,saw));
    }else{
      erro1<-saa$vcomcc-predict(aj,saa);
      erro2<-saw$vcomcc-predict(aj,saw);
    } 
    dsw[m,1]<-sum(erro2^2);
    dsw[m,2]<-sqrt(sum(erro1^2)/nrow(saa));
    dsw[m,3]<-(dsw[m,2]^(-nrow(saa)))*exp((-1/dsw[m,2]^2)*dsw[m,1]/2);
  }
  for(m in 1:nrow(dfmodel)){
    wk[i,m]<-dsw[m,3]/sum(dsw[,3]);
  } 
}
print(apply(wk,2,mean));

#1
#Schumacher log: 0.1900565  
#Spurr log:      0.3305679
#Spuur:          0.2319225 
#Schum:          0.2474532 

#2
#Schumacher log: 0.2755241  
#Spurr log:      0.2942122 
#Spurr:          0.2690085 
#Schum:          0.1612552

#3
#Schumacher log: 0.3674594 
#Spurr log:      0.1447471
#Spurr:          0.1462542
#Schum:          0.3415394 

