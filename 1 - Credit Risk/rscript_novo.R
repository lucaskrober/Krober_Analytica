install.packages("xtable")
install.packages("ROCR")
install.packages("janitor")
library(readxl)
library(dplyr)
library(xtable)
library(ggplot2)
library(ROCR)
library(janitor)

names(dados)

dados<-read_xlsx('Base_regressao_20_10.xlsx',sheet = "inativos",col_types = c('numeric',
                                                                              'numeric',
                                                                              'numeric',
                                                                              'numeric',
                                                                              'date',
                                                                              'numeric',
                                                                              'date',
                                                                              'date',
                                                                              "text",
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
                                                                              'numeric',
                                                                              'numeric',
                                                                              'numeric',
                                                                              'numeric',
                                                                              'text'))



data<-janitor::clean_names(dados)


names(data)
data$resu_cons_titu<-as.factor(data$resu_cons_titu)
data$tipo_titu<-as.factor(data$tipo_titu)
data$segmento<-as.factor(data$segmento)
data$nicho<-as.factor(data$nicho)
data$fraudador<-as.factor(data$fraudador)
data$city<-as.factor(data$cida)
data$tranche<-as.factor(data$tranche)
data$fili_id<-as.factor(data$fili_id)


data_num<- data %>%
  dplyr::select(fraudador,resu_cons_titu,idade_clie,
                concen_saca,tranche,prorro_liqui,recomp_liqui,
                perc_desc,tb_bord_perc_desc,valo_scon,limi_cred,valo_titu_orig,tipo_titu)
data_num<-as.data.frame(data_num)
row.names(data_num)<-data$titu_id
data<-data_num

#####
# Separar o conjunto de dados em dados p teste e validacao
# indices obtidos apos a aleatorizacao 
ordena = sort(sample(nrow(data), nrow(data)*.5))

#Dados para o treinamento (fica em branco após a virgula para selecionar todas as colunas) 
treinamento<-data[ordena,]

#Dados para a validacao 
validacao<-data[-ordena,]
#####

#Regressao Logistica 
modelo.completo <- glm(treinamento$fraudador~.,family=binomial,data=treinamento)


#Abordagem Stepwise para selecao de variaveis
stepwise <- step(modelo.completo,direction="both")
stepwise$formula
summary(stepwise)

#Modelo com as variaveis indicadas pelo Stepwise
stepwise <- glm(stepwise$formula, family=binomial,data=treinamento)

#Resume os resultados do modelo

summary(stepwise)

#Calcula a razãoo de chances (demora um pouco pra calcular)
razao<-exp(cbind(OR = coef(stepwise), confint(stepwise)))
razao


# Daqui em diante a predição acontece para uma base que não precisa, necessariamente, 
# ter a informação fraudador. No caso dessa base, temos a informação, por isso conseguimos 
# calcular a precisão/taxa de acerto do modelo. Porém, no caso da predição em uma nova base, 
# onde queremos saber quem pode se tornar um fraudador basta somente aplicar o modelo
# usando as próximas linhas de comando

predito<-predict(stepwise,validacao,type="response")

# Se tem variáveis que aparecem como NA na tabela nova, então precisa rodar essas linhas a seguir
predito<-predito[!is.na(predito)]
validacao<-na.omit(validacao)

# salva os valores estimados
pred = prediction(predito, validacao$fraudador)
# define o valor de corte, ou seja, acima desse valor, tudo vai ser considerado 1
corte<-as.numeric(performance(pred, "auc")@y.values)

corte<-0.3

#cálculo do score no dataset de validacao
validacao$score<-predict(stepwise,type='response',validacao)
summary(validacao$score)

pred<-prediction(validacao$score, validacao$fraudador)
perf <- performance(pred,"tpr","fpr")
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline( a =0, b = 1, lwd = 2, lty = 2, col = "gray")

#Escolhe quem vai ser "1"e quem vai ser "0"
validacao$predito<-ifelse(predito>=corte,1,0)

#Compara os resultados 

tab<-table(validacao$predito,validacao$fraudador)

tab
taxaacerto<-(tab[2,2]+tab[1,1])/sum(tab)
taxaacerto

validacao$score
summary(validacao$score)
validacao$predito<-as.factor(validacao$predito)
summary(validacao$predito)

# Identificando clientes fraudadores
fraudulentos<-subset(validacao,predito==1)

fraudulentos$TITU_ID<-row.names(fraudulentos)
fraude<-merge(fraudulentos,dados,by='TITU_ID')
names(fraude)

id_fraude<-fraude %>%
  dplyr::select(TITU_ID,CLIE_ID)
fraudadores<-unique(id_fraude$CLIE_ID);fraudadores
#434 405 472 518 524  73 535 597 604 537   5 377 227 380 391 408

 
