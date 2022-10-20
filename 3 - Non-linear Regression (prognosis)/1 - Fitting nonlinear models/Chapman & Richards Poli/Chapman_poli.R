

#ª Opção: Importação dos dados não pareados
dados_npar=read.csv2('dados_ra.csv')
dados_npar=subset(dados_npar,estrato==494631);
View(dados_npar)

library(cmrinvflor)
dados=parear_seqmed(
  dados_npar[,c('parcela','id','hd','ab','vtcc')]);

x11();
with(dados_npar,plot(hd,vtcc,pch='*',col='green'));

modelo='hd2~b0*(hd1/b0)^(log(1-exp(b1*id2))/log(1-exp(b1*id1)))';  
aj_chap_poli=nls(modelo,data=dados,start=list(b0=35.95,b1=-0.023436));          
sumário_ajchapp=summary(aj_chap_poli);
(sumario=summary(aj_chap_poli));


hdest2=predict(aj_chap_poli);
errounid=residuals(aj_chap_poli);
erroperc=(errounid/dados$hd2)*100;
erropad=errounid/sumário_ajchapp$sigma;
nsyx=round(sumário_ajchapp$sigma,5);nsyx;

print(paste('Erro padrão residual (m):',nsyx=round(sumário_ajchapp$sigma,5)));
print(paste('Erro padrão residual em porcentagem (%):', nsyx=round((sumário_ajchapp$sigma/mean(dados$hd2))*100,5)))

#Gráficos Erro Padronizado

x11()
par(mfrow=c(1,3))
plot(predict(aj_chap_poli),residuals(aj_chap_poli),pch='*',
     col='darkgreen',xlab='hdom estimado (m)',
     ylab='resíduos (m)', ylim=c(-5,5));
abline(h=0);

hist(residuals(aj_chap_poli),xlab='resíduos(m)',ylab='frequência',main='',col='darkgreen');

plot(dados$id2,predict(aj_chap_poli),pch='*',col='darkgreen',
     xlab='idade(meses)', ylab='altura dominante');

points(dados$id2,dados$hd2,pch='*',col='red');


#Download library fBasics para ver se a distribuição dos residuos é normal, se estiver todos dentro do intervalo
library(fBasics); #grafico quantil quantil para análise de Normalidade dos residuos
x11();
qqnormPlot(residuals(aj_chap_poli));

#Gerar Curvas
dados=read.csv2('dados_ra.csv');
dados=subset(dados,estrato==494631);

betas=sumário_ajchapp$coefficients
b0=betas[1];b0;
b1=betas[2];b1;

idref=72; #Idade de referência

#Criar coluna chamada sitio
#Sitio: Altura dominante estimada na idade de referência
dados$sitio=with(dados, b0*(hd/b0)^(log(1-exp(b1*idref))/log(1-exp(b1*id))));
View(dados)

(ls=max(dados$sitio));
(li=min(dados$sitio));

#Ls=32.74 e Li=23.1 sendo 10m a diferença e 4 classes -> Intervalo de classe = 2.5

32.74 
30.33
27.92
25.51
23.10

classe 4 vc = 24.305
classe 3 vc = 26.715
classe 2 vc = 29.125
classe 1 vc = 31.535

nls=33
nli=23
nclasses=4
(interv=(nls-nli)/nclasses);

#altura dominante da idade de referencia
#limite inferior da classe 4
hdiref=nli
dados$licl4=with(dados, b0*(hdiref/b0)^(log(1-exp(b1*id))/log(1-exp(b1*idref)))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl4=with(dados,  b0*(hdiref/b0)^(log(1-exp(b1*id))/log(1-exp(b1*idref)))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl3=with(dados,  b0*(hdiref/b0)^(log(1-exp(b1*id))/log(1-exp(b1*idref)))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl2=with(dados,  b0*(hdiref/b0)^(log(1-exp(b1*id))/log(1-exp(b1*idref)))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl1=with(dados,  b0*(hdiref/b0)^(log(1-exp(b1*id))/log(1-exp(b1*idref)))) 

View(dados)

#dados vai receber valores de dados ordenados pela idade
dados=dados[order(dados$id),];

x11();

with(dados,plot(id,hd,pch='*',col='darkgreen',ylim=c(0,40),
                xlab='idade(meses)',ylab='hdom(m)'))

with(dados,lines(id,licl4,col=1));
with(dados,lines(id,lscl4,col=2));
with(dados,lines(id,lscl3,col=3));
with(dados,lines(id,lscl2,col=4));
with(dados,lines(id,lscl1,col=5));

write.csv2(dados,'curvas_chapp.csv',row.names=F)

shell.exec('curvas_chapp.csv')
