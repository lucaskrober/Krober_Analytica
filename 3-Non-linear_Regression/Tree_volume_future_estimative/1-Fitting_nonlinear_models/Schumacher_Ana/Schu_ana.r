# mudou-se na planilha do excel:
# idade para id
# hdom para hd
library(cmrinvflor);
dados_npar=read.csv2('dados_ra.csv');
dados_npar=subset(dados_npar,estrato==494631);
View(dados_npar);

dados=parear_seqmed(subset(dados_npar,,c('parcela','id','hd','ab','vtcc')))  ; # em c(parcela= 1º)coluna do identificador,  2º) coluna que ordena, 3º) colunas que vão ser pareadas
View(dados);

x11();
with(dados_npar,plot(hd,vtcc,pch='*',col='red'));

#Schumacher anamórfico - só digitou o modelo para deixar registrado
schu_ana='hd2~hd1*exp(b1*(1/id1-1/id2))' 

ajsa=nls(schu_ana,data=dados,start=list(b1=30)); #ajuste beatley cutter anamorfico / start=parametros iniciais, se ajustou quer dizer que o parametro inicial foi valido)
sumário_ajsa=summary(ajsa);

summary(ajsa); # eroo padrao residual+/-1.603m

hdest2=predict(ajsa);
errounid=residuals(ajsa);
erroperc=(errounid/dados$hd2)*100;
erropad=errounid/sumário_ajsa$sigma;
nsyx=round(sumário_ajsa$sigma,5);nsyx;

print(paste('Erro padrão residual (m):',nsyx=round(sumário_ajsa$sigma,5)));
print(paste('Erro padrão residual em porcentagem (%):', nsyx=round((sumário_ajsa$sigma/mean(dados$hd2))*100,5)))


########## gráficos do erro padronizado#########

x11()
par(mfrow=c(1,3))
plot(predict(ajsa),residuals(ajsa),pch='*',
     col=2,xlab='hdom estimado (m)',
     ylab='resíduos (m)', ylim=c(-5,5));
abline(h=0);

hist(residuals(ajsa),xlab='resíduos(m)',ylab='frequência',main='',col='red');

plot(dados$id2,predict(ajsa),pch='*',col='red',
     xlab='idade(meses)', ylab='altura dominante');

points(dados$id2,dados$hd2,pch='*',col='blue');

#Download library fBasics para ver se a distribuição dos residuos é normal, se estiver todos dentro do intervalo
library(fBasics); #grafico quantil quantil para análise de Normalidade dos residuos
x11();
qqnormPlot(residuals(ajsa));

# Gerar curvas de nível

dados=read.csv2('dados_ra.csv');
dados=subset(dados,estrato==494631);

betas=sumário_ajsa$coefficients
b1=betas[1];b1;
b2=betas[2];b2;   

idref=72; #Idade de referência

#Criar coluna chamada sitio
#Sitio: Altura dominante estimada na idade de referência
schu_ana='hd2~hd1*exp(b1*(1/id1-1/id2))' 
dados$sitio=with(dados, hd*exp(b1*(1/id-1/idref)));
View(dados)

(ls=max(dados$sitio));
(li=min(dados$sitio));

#Ls=37.71 e Li=19.5 sendo 19m a diferença e 4 classes -> Intervalo de classe = 4.75

nls=38
nli=19
nclasses=4
(interv=(nls-nli)/nclasses);

#altura dominante da idade de referencia
#limite inferior da classe 4
hdiref=nli
dados$licl4=with(dados, hdiref*exp(b1*(1/idref-1/id))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl4=with(dados, hdiref*exp(b1*(1/idref-1/id))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl3=with(dados, hdiref*exp(b1*(1/idref-1/id))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl2=with(dados, hdiref*exp(b1*(1/idref-1/id))) 

#limite superior da classe 4
hdiref=hdiref+interv;
dados$lscl1=with(dados, hdiref*exp(b1*(1/idref-1/id))) 

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
