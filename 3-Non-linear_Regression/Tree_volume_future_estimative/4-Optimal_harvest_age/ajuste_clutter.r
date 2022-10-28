dir()
dados=read.csv2('reclassifica�ao_chapp.csv');
View(dados);
names(dados);

library(cmrinvflor);

dados_pareados=parear_seqmed(subset(dados,,c('parcela','id','ab','s','vtcc'))); #pegar objeto dados na ordem parcela,id...e pareia ordenando em ordem crescente de idade

View(dados_pareados)

dados_pareados$s=dados_pareados$s1;
dados_pareados$s1=NULL #remove coluna
dados_pareados$s2=NULL

aj_clutter=lm('log(vtcc2)~I(1/s)+I(1/id2)+I((id1/id2)*log(ab1))+I(1-id1/id2)+I((1-id1/id2)*s)',dados_pareados)

(sumario=summary(aj_clutter));

#Analise variancia, erro padrao residual(corrigir m�) (relat�rio)

#como o erro esta em ln m� temos que corrigir, o primeiro passo � calcular a nova soma de quadrado

nsqe <- sum((dados_pareados$vtcc2 - exp(predict(aj_clutter)))^2)
nsqe

#com esse valor podemos calcular o novo quadrado medio do erro


nqme <- nsqe/aj_clutter$df.residual
nqme

#para achar o erro padrao residual em m� tiramos a raiz do novo quadrado medio

syx_m3 <- sqrt(nqme)
syx_m3

#passando para porcentagem: ero emm m3 divido pela media dos valores observados
syx_perc <- (syx_m3/mean(dados_pareados$vtcc2)*100)
syx_perc

plot(predict(aj_clutter), rstudent(aj_clutter))
#arrumando o grafico
x11()
par(mfrow=c(1,2))
plot(predict(aj_clutter), rstudent(aj_clutter),pch='*',
     col='brown', xlab='ln(volume estimado)', ylab= 'resdiduo estudentizado',
     ylim=c(-5,5), abline(h=0))

hist(residuals(aj_clutter),main='', xlab='Res�duos (ln(m�)', ylab='Frequ�ncia', col='yellow')

coef(aj_clutter)
