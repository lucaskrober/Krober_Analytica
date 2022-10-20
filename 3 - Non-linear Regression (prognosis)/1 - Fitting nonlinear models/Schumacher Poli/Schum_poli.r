# mudou-se na planilha do excel:
# idade para id
# hdom para hd
library(cmrinvflor);
dados_npar=read.csv2('dados_ra.csv');
dados_npar=subset(dados_npar,estrato==494631);
View(dados_npar);

dados=parear_seqmed(subset(dados_npar,,c('parcela','id','hd','ab','vtcc')))  ; # em c(parcela= 1�)coluna do identificador,  2�) coluna que ordena, 3�) colunas que v�o ser pareadas
View(dados);

x11();
with(dados_npar,plot(hd,vtcc,pch='*',col='red'));

#Schumacher anam�rfico - s� digitou o modelo para deixar registrado
schu_poli='hd2~b0*(hd1/b0)^(id1/id2)'

ajsp=nls(schu_poli,data=dados,start=list(b0=30)); #ajuste beatley cutter anamorfico / start=parametros iniciais, se ajustou quer dizer que o parametro inicial foi valido)
sum�rio_ajsp=summary(ajsp);

summary(ajsp); 

hdest2=predict(ajsp);
errounid=residuals(ajsp);
erroperc=(errounid/dados$hd2)*100;
erropad=errounid/sum�rio_ajsp$sigma;
nsyx=round(sum�rio_ajsp$sigma,5);nsyx;

print(paste('Erro padr�o residual (m):',nsyx=round(sum�rio_ajsp$sigma,5)));
print(paste('Erro padr�o residual em porcentagem (%):', nsyx=round((sum�rio_ajsp$sigma/mean(dados$hd2))*100,5)))


########## gr�ficos do erro padronizado#########

x11()
par(mfrow=c(1,3))
plot(predict(ajsp),residuals(ajsp),pch='*',
     col='darkgreen',xlab='hdom estimado (m)',
     ylab='res�duos (m)', ylim=c(-5,5));
abline(h=0);

hist(residuals(ajsp),xlab='res�duos(m)',ylab='frequ�ncia',main='',col='darkgreen');

plot(dados$id2,predict(ajsp),pch='*',col='darkgreen',
     xlab='idade(meses)', ylab='altura dominante');

points(dados$id2,dados$hd2,pch='*',col='red');

#Download library fBasics para ver se a distribui��o dos residuos � normal, se estiver todos dentro do intervalo
library(fBasics); #grafico quantil quantil para an�lise de Normalidade dos residuos
x11();
qqnormPlot(residuals(ajsp));

# Gerar curvas de n�vel

dados=read.csv2('dados_ra.csv');
dados=subset(dados,estrato==494631);

betas=sum�rio_ajsp$coefficients
b0=betas[1];b0;

idref=72; #Idade de refer�ncia

#Criar coluna chamada sitio
#Sitio: Altura dominante estimada na idade de refer�ncia
schu_poli='hd2~b0*(hd1/b0)^(id1/id2))' 
dados$sitio=with(dados, b0*(hd/b0)^(id/idref));
View(dados)

(ls=max(dados$sitio));
(li=min(dados$sitio));

#Ls=32.74 e Li=22.72 sendo 11m a diferen�a e 4 classes -> Intervalo de classe = 2.75

nls=33
nli=22
nclasses=4
(interv=(nls-nli)/nclasses);

#altura dominante da idade de referencia
#limite inferior da classe 4
hdiref=nli
dados$licl4=with(dados, b0*(hdiref/b0)^(idref/id)) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl4=with(dados, b0*(hdiref/b0)^(idref/id)) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl3=with(dados, b0*(hdiref/b0)^(idref/id)) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl2=with(dados, b0*(hdiref/b0)^(idref/id)) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl1=with(dados, b0*(hdiref/b0)^(idref/id)) 

View(dados)

#dados vai receber valores de dados ordenados pela idade
dados=dados[order(dados$id),];

x11();

with(dados,plot(id,hd,pch='*',col='red', ylim=c(0,40),
                xlab='idade(meses)',ylab='hdom(m)'))

with(dados,lines(id,licl4,col=1));
with(dados,lines(id,lscl4,col=2));
with(dados,lines(id,lscl3,col=3));
with(dados,lines(id,lscl2,col=4));
with(dados,lines(id,lscl1,col=5));

write.csv2(dados,'curvas_schuana_poli.csv',row.names=F)

shell.exec('curvas_schuana_poli.csv')
