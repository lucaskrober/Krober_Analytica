#Ajuste do modelo de Bailey & Clutter Polimórfico


#ª Opção: Importação dos dados não pareados
dados_npar=read.csv2('dados_ra.csv')
dados_npar=subset(dados_npar,estrato==573116);
View(dados_npar)

library(cmrinvflor)
dados=parear_seqmed(
  dados_npar[,c('parcela','id','hd','ab','vtcc')]);

x11();
with(dados_npar,plot(hd,vtcc,pch='*',col='green'));

bc_poli='hd2~b0*((hd1/b0)^((id1/id2)^b2))';

ajbcp=nls(bc_poli,dados,start=list(b0=50,b2=0.5));
sumário_ajbcp=summary(ajbcp);

(sumario=summary(ajbcp));

hdest2=predict(ajbcp);
errounid=residuals(ajbcp);
erroperc=(errounid/dados$hd2)*100;
erropad=errounid/sumário_ajbcp$sigma;
nsyx=round(sumário_ajbcp$sigma,5);nsyx;

print(paste('Erro padrão residual (m):',nsyx=round(sumário_ajbcp$sigma,5)));
print(paste('Erro padrão residual em porcentagem (%):', nsyx=round((sumário_ajbcp$sigma/mean(dados$hd2))*100,5)))

#Gráficos Erro Padronizado

x11()
par(mfrow=c(1,3))
plot(predict(ajbcp),residuals(ajbcp),pch='*',
     col=3,xlab='hdom estimado (m)',
     ylab='resíduos (m)', ylim=c(-5,5));
abline(h=0);

hist(residuals(ajbcp),xlab='resíduos(m)',ylab='frequência',main='',col='green');

plot(dados$id2,predict(ajbcp),pch='*',col='blue',
     xlab='idade(meses)', ylab='altura dominante');

points(dados$id2,dados$hd2,pch='*',col='green');


#Download library fBasics para ver se a distribuição dos residuos é normal, se estiver todos dentro do intervalo
library(fBasics); #grafico quantil quantil para análise de Normalidade dos residuos
x11();
qqnormPlot(residuals(ajbcp));

#Gerar Curvas
dados=read.csv2('dados_ra.csv');
dados=subset(dados,estrato==573116);

betas=sumário_ajbcp$coefficients
b0=betas[1];b0;
b2=betas[2];b2;

idref=72; #Idade de referência

#Criar coluna chamada sitio
#Sitio: Altura dominante estimada na idade de referência
dados$sitio=with(dados, b0*((hd/b0)^((id/idref)^b2)));
View(dados)

(ls=max(dados$sitio));
(li=min(dados$sitio));

#Ls=33.5 e Li=21.5 sendo 12m a diferença e 4 classes -> Intervalo de classe = 3

nls=33
nli=22
nclasses=4
(interv=(nls-nli)/nclasses);


#altura dominante da idade de referencia
#limite inferior da classe 4
hdiref=nli
dados$licl4=with(dados, b0*((hdiref/b0)^((idref/id)^b2))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl4=with(dados, b0*((hdiref/b0)^((idref/id)^b2))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl3=with(dados, b0*((hdiref/b0)^((idref/id)^b2))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl2=with(dados, b0*((hdiref/b0)^((idref/id)^b2))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl1=with(dados, b0*((hdiref/b0)^((idref/id)^b2))) 

View(dados)

#dados vai receber valores de dados ordenados pela idade
dados=dados[order(dados$id),];

x11();

with(dados,plot(id,hd,pch='*',col='red',ylim=c(0,40),
                xlab='idade(meses)',ylab='hdom(m)'))

with(dados,lines(id,licl4,col=1));
with(dados,lines(id,lscl4,col=2));
with(dados,lines(id,lscl3,col=3));
with(dados,lines(id,lscl2,col=4));
with(dados,lines(id,lscl1,col=5));

write.csv2(dados,'curvas_bc_poli.csv',row.names=F)

shell.exec('curvas_bc_poli.csv')
