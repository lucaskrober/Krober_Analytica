# Johilton Pavlak Filho
install.packages("xtable")
install.packages("ROCR")
library(readxl)
library(dplyr)
library(xtable)
library(ggplot2)
library(ROCR)

data<-read_xlsx('..\\data\\Base_regressao.xlsx',col_types = c('numeric',
                                                              'numeric',
                                                              'date',
                                                              'date',
                                                              'text',
                                                              'numeric',
                                                              'text',
                                                              'numeric',
                                                              'numeric',
                                                              'numeric',
                                                              'date',
                                                              'text',
                                                              'numeric',
                                                              'numeric',
                                                              'numeric',
                                                              'numeric',
                                                              'text',
                                                              'text',
                                                              'numeric',
                                                              'numeric',
                                                              'numeric',
                                                              'numeric',
                                                              'numeric',
                                                              'numeric',
                                                              'numeric',
                                                              'numeric',
                                                              'text',
                                                              'numeric',
                                                              'numeric',
                                                              'numeric',
                                                              'numeric'
))

#Transforma em fatores as variaveis categoricas e "dummies"
data$RESU_CONS_TITU<-as.factor(data$RESU_CONS_TITU)
data$TIPO_TITU<-as.factor(data$TIPO_TITU)
data$Segmento<-as.factor(data$Segmento)
data$Nicho<-as.factor(data$Nicho)
data$FRAUDADOR<-as.factor(data$FRAUDADOR)
data$CIDA<-as.factor(data$CIDA)
data$`Teve Tranche?`<-as.factor(data$`Teve Tranche?`)
data$FANT<-NULL
data$FILI_ID<-as.factor(data$FILI_ID)

data<-subset(data,!TITU_ID %in% c(196008,196009,124233))

names(data)[23]<-"idade_as_cliente"
names(data)[24]<-"concen_sacados"
names(data)[29]<-"tranche"
names(data)[30]<-"tempo_fraude"
names(data)[25]<-"prorrog_liquid"
names(data)[28]<-"recomp_liquid"

names(data)

data_num<- data %>%
  dplyr::select(TITU_ID,VALO_TITU,FRAUDADOR,RESU_CONS_TITU,Segmento,Nicho,idade_as_cliente,
                concen_sacados,tranche,prorrog_liquid,recomp_liquid,
                PERC_DESC,tbBORD.PERC_DESC,VALO_SCON,LIMI_CRED,VALO_TITU_ORIG,TIPO_TITU)
data_num<-as.data.frame(data_num)

data<-data_num

write.csv2(data,'dados.csv',row.names=F)


data<-read.csv2('dados.csv')
data$RESU_CONS_TITU<-as.factor(data$RESU_CONS_TITU)
data$TIPO_TITU<-as.factor(data$TIPO_TITU)
data$Segmento<-as.factor(data$Segmento)
data$Nicho<-as.factor(data$Nicho)
data$FRAUDADOR<-as.factor(data$FRAUDADOR)
data$tranche<-as.factor(data$tranche)

#Apresenta a estrutura do DataFrame
str(data)

data$Segmento<-NULL
data$Nicho<-NULL
data$TITU_ID<-NULL

# Separar o conjunto de dados em dados p teste e validacao
# indices obtidos apos a aleatorizacao 
ordena = sort(sample(nrow(data), nrow(data)*.6))

#Dados para o treinamento (fica em branco após a virgula para selecionar todas as colunas) 
treinamento<-data[ordena,]

#Dados para a validacao 
validacao<-data[-ordena,]

#Regressao Logistica 
modelo.completo <- glm(treinamento$FRAUDADOR~.,family=binomial,data=treinamento)


#Abordagem Stepwise para selecao de variaveis
stepwise <- step(modelo.completo,direction="both")
stepwise$formula
summary(stepwise)

#Modelo com as variaveis indicadas pelo Stepwise
stepwise <- glm(stepwise$formula, family=binomial,data=treinamento)

# stepwise <- glm(FRAUDADOR ~ RESU_CONS_TITU + Segmento + concen_sacados + idade_as_cliente +
#                   tranche + prorrog_liquid + recomp_liquid + PERC_DESC + VALO_SCON + LIMI_CRED +
#                   VALO_TITU_ORIG + TIPO_TITU, family=binomial,data=treinamento)
# 
# stepwise <- glm(FRAUDADOR ~ RESU_CONS_TITU + Segmento + idade_as_cliente +
#                   prorrog_liquid + recomp_liquid + PERC_DESC + VALO_SCON + LIMI_CRED +
#                   VALO_TITU_ORIG + TIPO_TITU, family=binomial,data=treinamento)

#Resume os resultados do modelo

summary(stepwise)

#Calcula a razÃ£o de chances
razao<-exp(cbind(OR = coef(stepwise), confint(stepwise)))
razao
xtable(razao)
#Faz a previsao para a base de validaco (probabilidade)

# validacao<-subset(validacao,!Segmento=="SETOR PRIMARIO")
# validacao<-subset(validacao,!Nicho=="PRODUTOS DE LIMPEZA E HIGIENE")

predito<-predict(stepwise,validacao,type="response")
predito<-predito[!is.na(predito)]
validacao<-na.omit(validacao)

pred = prediction(predito, validacao$FRAUDADOR)
corte<-as.numeric(performance(pred, "auc")@y.values)

#score validacao data set
validacao$score<-predict(stepwise,type='response',validacao)
pred<-prediction(validacao$score, validacao$FRAUDADOR)
perf <- performance(pred,"tpr","fpr")
plot(perf) 
plot(perf, colorize=TRUE) #adicionar
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline( a =0, b = 1, lwd = 2, lty = 2, col = "gray")
#Escolhe quem vai ser "1"e quem vai ser "0"
predito<-ifelse(predito>=corte,1,0)
#Compara os resultados 
tab<-table(predito,validacao$FRAUDADOR)
tab
xtable(tab)
taxaacerto<-(tab[2,2]+tab[1,1])/sum(tab)
taxaacerto
validacao$score
summary(validacao$score)


aberto<-subset(data,VALO_TITU>0)
aberto2<-na.omit(aberto)
summary(aberto2$FRAUDADOR)

aberto2$score<-predict(stepwise,type='response',aberto2)
summary(aberto2$score)


predito<-predict(stepwise,aberto2,type="response")
predito<-predito[!is.na(predito)]

pred = prediction(predito, aberto2$FRAUDADOR)
corte<-as.numeric(performance(pred, "auc")@y.values)


perf <- performance(pred,"tpr","fpr")
plot(perf) 
plot(perf, colorize=TRUE) #adicionar
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline( a =0, b = 1, lwd = 2, lty = 2, col = "gray")
#Escolhe quem vai ser "1"e quem vai ser "0"
predito<-ifelse(predito>=corte,1,0)
#Compara os resultados 
tab<-table(predito,aberto2$FRAUDADOR)
tab
xtable(tab)
taxaacerto<-(tab[2,2]+tab[1,1])/sum(tab)
taxaacerto
aberto2$score
summary(aberto2$score)

