#para ultima medição (é a que nos interessa)
dir()
dados_npar <- read.csv2 ('reclassificaçao_chapp.csv');

ultmed <- dados_npar


b0=3.08153872 
b1=-18.26684802 
b2=-24.69136801   
b3=1.11032992   
b4=2.38830031   
b5=0.06826421

idproj <- 24
x <- idproj
ultmed$vtcc2 <- with(ultmed, exp (b0 + b1/s + b2/x+b3*(id/x)*log(ab)+b4*(1-id/x) + b5*(1-id/x)*s))

idproj <- 36
x <- idproj
ultmed$vtcc3 <- with(ultmed, exp (b0 + b1/s + b2/x+b3*(id/x)*log(ab)+b4*(1-id/x) + b5*(1-id/x)*s))

idproj <- 48
x <- idproj
ultmed$vtcc4 <- with(ultmed, exp (b0 + b1/s + b2/x+b3*(id/x)*log(ab)+b4*(1-id/x) + b5*(1-id/x)*s))

idproj <- 60
x <- idproj
ultmed$vtcc5 <- with(ultmed, exp (b0 + b1/s + b2/x+b3*(id/x)*log(ab)+b4*(1-id/x) + b5*(1-id/x)*s))

idproj <- 72
x <- idproj
ultmed$vtcc6 <- with(ultmed, exp (b0 + b1/s + b2/x+b3*(id/x)*log(ab)+b4*(1-id/x) + b5*(1-id/x)*s))

idproj <- 84
x <- idproj
ultmed$vtcc7 <- with(ultmed, exp (b0 + b1/s + b2/x+b3*(id/x)*log(ab)+b4*(1-id/x) + b5*(1-id/x)*s))

idproj <- 96
x <- idproj
ultmed$vtcc8 <- with(ultmed, exp (b0 + b1/s + b2/x+b3*(id/x)*log(ab)+b4*(1-id/x) + b5*(1-id/x)*s))

idproj <- 108
x <- idproj
ultmed$vtcc9 <- with(ultmed, exp (b0 + b1/s + b2/x+b3*(id/x)*log(ab)+b4*(1-id/x) + b5*(1-id/x)*s))

idproj <- 120
x <- idproj
ultmed$vtcc10 <- with(ultmed, exp (b0 + b1/s + b2/x+b3*(id/x)*log(ab)+b4*(1-id/x) + b5*(1-id/x)*s))

#Calcular os IMAs
dados_ultmed$ima2=dados_ultmed$vtcc2/2;
dados_ultmed$ima3=dados_ultmed$vtcc3/3;
dados_ultmed$ima4=dados_ultmed$vtcc4/4;
dados_ultmed$ima5=dados_ultmed$vtcc5/5;
dados_ultmed$ima6=dados_ultmed$vtcc6/6;
dados_ultmed$ima7=dados_ultmed$vtcc7/7;
dados_ultmed$ima8=dados_ultmed$vtcc8/8;
dados_ultmed$ima9=dados_ultmed$vtcc9/9;
dados_ultmed$ima10=dados_ultmed$vtcc10/10;
View(dados_ultmed);

#Calcular ICAs
dados_ultmed$ica3=with(dados_ultmed,vtcc3-vtcc2);
dados_ultmed$ica4=with(dados_ultmed,vtcc4-vtcc3);
dados_ultmed$ica5=with(dados_ultmed,vtcc5-vtcc4);
dados_ultmed$ica6=with(dados_ultmed,vtcc6-vtcc5);
dados_ultmed$ica7=with(dados_ultmed,vtcc7-vtcc6);
dados_ultmed$ica8=with(dados_ultmed,vtcc8-vtcc7);
dados_ultmed$ica9=with(dados_ultmed,vtcc9-vtcc8);
dados_ultmed$ica10=with(dados_ultmed,vtcc10-vtcc9);

View(ultmed)

write.csv2(ultmed,'vtccfazendas.csv',row.names=F);
shell.exec('vtccfazendas.csv')
