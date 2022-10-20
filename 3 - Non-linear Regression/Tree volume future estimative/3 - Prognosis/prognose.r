dir()
dados=read.csv2("reclassificação_chapp2016.csv");
View(dados);


#parear dados
names(dados);
library(cmrinvflor);
dados_pareados=parear_seqmed(subset(dados,,c("parcela","id","vtcc","ab","s")));     #dados = parear dados só as columas; parcela= variavel estatisticadora; id= variavel que coloca em ordem crescente; vtcc,ab,S= demais colunas( não tem necessidade de parear o sitio, pq é igual, só coloca p aparecer na tabela)
dados_pareados$s=dados_pareados$s2
dados_pareados$s1=NULL;             #apagar as colunas
dados_pareados$s2=NULL;             #apagar as colunas
View(dados_pareados);

# Modelo de Clutter
modelo="log(vtcc2)~I(1/s)+I(1/id2)+I((id1/id2)*log(ab1))+I(1-id1/id2)+I((1-id1/id2)*s)";    #1/S=variaveis do model

# Ajuste do modelo
aj_clutter=lm(modelo,dados_pareados);
summary(aj_clutter);    #R²=0,9585 é ótimo! R²aj=0,9582 continua ótimo
anova(aj_clutter);

#_____________________
#Recalcular erro padrão residual  (Como a variavel dependente do modelo é LnV, deve-se recalcular o erro!
#Fazer os gráficos de avaliação do ajuste

################Análise do Ajuste do Modelo
vest=exp(predict(aj_clutter)); vest; #predic=di/dap
sqerro=sum((dados_pareados$vtcc2-vest)^2);
narvore=nrow(dados); narvore;
coefs=as.vector(coef(aj_clutter));coefs;
glreg=length(coefs)-1; glreg;
qme=sqerro/(narvore-glreg-1); qme;
erropadres=sqrt(qme);erropadres;#16.26061m³
erroperc=erropadres/mean(dados_pareados$vtcc2)*100; erroperc;#5.681%

################Análise Gráfica do ajuste
dados_pareados$vtcc2est=exp(predict(aj_clutter)); 
residuos=dados_pareados$vtcc2-dados_pareados$vtcc2est;
dados_pareados$erropad=residuos/erropadres;

#####################
X11()
par(mfrow=c(1,3));

#1º Gráfico
plot(dados_pareados$id2 , dados_pareados$vtcc2 , pch="*", col="blue", xlab="Idade (meses)", ylab="vtcc (m³)", main="Modelo de Clutter") ;
points(dados_pareados$id2, dados_pareados$vtcc2est, pch="*", col="red") ;
legend("bottomright", legend=c("vtcc2_obs", "vtcc2_est"), pch="*", col=c("blue","red"));

#2º Gráfico
plot(dados_pareados$id2 , dados_pareados$erropad, pch="*", col="blue", xlab="Idade (meses)", ylab="Resíduos padronizados", ylim=c(-4,4)) ;
abline(h=0, lty=4, lwd=2);  
t_valor=qt(0.995,nrow(dados)-1) ;
abline(h=t_valor, lty=3, lwd=2);
abline(h=-t_valor, lty=3, lwd=2);

#3º Gráfico 
hist(residuos, xlab="Resíduos (m)", ylab="Frequência", main="",col='blue',bg="red");
#____________________________________

(bs=as.vector(coef(aj_clutter)));       #bs=betas

#pegar a ultima medição de cada parcela
ultmed=aggregate(list(max_id=dados$id),list(parcela=dados$parcela),max)   ;  #ultimed é pra pegar a ultima medição
View(ultmed);
dados=merge(dados,ultmed);     # juntou os dois arquivos
View (dados);
dados_ultmed=subset(dados,id==max_id);   # Pegar os dados da máx id (selecionar a ultima medição)
View(dados_ultmed);

 #Estimar os volumes nas diferentes idades
idadeprog=24;      #(24 meses=2anos)
dados_ultmed$vtcc2=with(dados_ultmed, exp(bs[1]+bs[2]/s+bs[3]/idadeprog+bs[4]*(id/idadeprog)*log(ab)+bs[5]*(1-id/idadeprog)+bs[6]*(1-id/idadeprog)*s)); #bs é valores de betas, guardou-os na posição 1,2,.. pq no R a matriz começa na posição 1! igual no excel, só q não devia.

idadeprog=36;
dados_ultmed$vtcc3=with(dados_ultmed, exp(bs[1]+bs[2]/s+bs[3]/idadeprog+bs[4]*(id/idadeprog)*log(ab)+bs[5]*(1-id/idadeprog)+bs[6]*(1-id/idadeprog)*s));

idadeprog=48;
dados_ultmed$vtcc4=with(dados_ultmed, exp(bs[1]+bs[2]/s+bs[3]/idadeprog+bs[4]*(id/idadeprog)*log(ab)+bs[5]*(1-id/idadeprog)+bs[6]*(1-id/idadeprog)*s));

idadeprog=60;
dados_ultmed$vtcc5=with(dados_ultmed, exp(bs[1]+bs[2]/s+bs[3]/idadeprog+bs[4]*(id/idadeprog)*log(ab)+bs[5]*(1-id/idadeprog)+bs[6]*(1-id/idadeprog)*s));

idadeprog=72;
dados_ultmed$vtcc6=with(dados_ultmed, exp(bs[1]+bs[2]/s+bs[3]/idadeprog+bs[4]*(id/idadeprog)*log(ab)+bs[5]*(1-id/idadeprog)+bs[6]*(1-id/idadeprog)*s));

idadeprog=84;
dados_ultmed$vtcc7=with(dados_ultmed, exp(bs[1]+bs[2]/s+bs[3]/idadeprog+bs[4]*(id/idadeprog)*log(ab)+bs[5]*(1-id/idadeprog)+bs[6]*(1-id/idadeprog)*s));

idadeprog=96;
dados_ultmed$vtcc8=with(dados_ultmed, exp(bs[1]+bs[2]/s+bs[3]/idadeprog+bs[4]*(id/idadeprog)*log(ab)+bs[5]*(1-id/idadeprog)+bs[6]*(1-id/idadeprog)*s));

idadeprog=108;
dados_ultmed$vtcc9=with(dados_ultmed, exp(bs[1]+bs[2]/s+bs[3]/idadeprog+bs[4]*(id/idadeprog)*log(ab)+bs[5]*(1-id/idadeprog)+bs[6]*(1-id/idadeprog)*s));

idadeprog=120;
dados_ultmed$vtcc10=with(dados_ultmed, exp(bs[1]+bs[2]/s+bs[3]/idadeprog+bs[4]*(id/idadeprog)*log(ab)+bs[5]*(1-id/idadeprog)+bs[6]*(1-id/idadeprog)*s));

View(dados_ultmed);

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

View(dados_ultmed);

write.csv2(dados_ultmed,"prognose2016.csv", row.names=F);
