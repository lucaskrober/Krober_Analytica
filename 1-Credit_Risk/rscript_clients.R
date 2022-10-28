install.packages("DataExplorer")
install.packages("explore")
library(readxl)
library(dplyr)
library(xtable)
library(ggplot2)
library(ROCR)
library(janitor)

dir()

data<-read_xlsx('Base_regressaoCLIE.xlsx')

data<-janitor::clean_names(data)

edit(data)
data$media_taxa[21]<-3.03/2


library(explore)
# if (interactive())  {
#   explore(data)
# }
# DataExplorer::create_report(data)

names(data)
data_num<- data %>%
  dplyr::select(fraudador,idade_clie,resu_a,resu_c,resu_n,resu_r,
                concen_sacados,prorro_liqui,recomp_liqui,media_taxa,mediana_taxa,moda_taxa,
                valo_scon,limi_cred,media_de_valo_face,mediana_de_valo_face,media_de_valo_titu_orig,mediana_de_valo_titu_orig,resu_titu_d,resu_titu_c)
data_num<-as.data.frame(data_num)
row.names(data_num)<-data$clie_id

y<-vector("list",length(2:(ncol(data_num))))

for(i in 2:(ncol(data_num))){
  # y[[i]]<-group_by(data_num,fraudador) %>%
  #   summarise(
  #     count = n(),
  #     mean = mean(data_num[,i], na.rm = TRUE),
  #     desv.pad = sd(data_num[,i], na.rm = TRUE))
  # names(y)[i]<-names(data_num)[i]
  # print(y[i])
  print(names(data_num)[i])
  mann_whitney <- wilcox.test(data_num[,i]~fraudador,
                            data=data_num,
                            exact=FALSE,
                            alternative = "two.sided")
  print(mann_whitney)
}


data_media<-data_num %>% 
  dplyr::select(fraudador,idade_clie,recomp_liqui,
                valo_scon,limi_cred,media_de_valo_face)

data_mediana<-data_num %>% 
  dplyr::select(fraudador,idade_clie,resu_a,resu_c,resu_n,resu_r,
                concen_sacados,prorro_liqui,recomp_liqui,mediana_taxa,
                valo_scon,limi_cred,mediana_de_valo_face,
                mediana_de_valo_titu_orig)
  
data_moda<-data_num %>%
  dplyr::select(fraudador,idade_clie,resu_a,resu_c,resu_n,resu_r,
                concen_sacados,prorro_liqui,recomp_liqui,moda_taxa,
                valo_scon,limi_cred)



#####
# Separar o conjunto de dados em dados p teste e validacao
# indices obtidos apos a aleatorizacao 
data<-data_media
# data<-data_mediana
# data<-data_moda

ordena = sort(sample(nrow(data), nrow(data)*.6))

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
#Modelo com as variaveis indicadas pelo Stepwise
stepwise <- glm(fraudador ~ idade_clie + recomp_liqui + moda_taxa + 
                  valo_scon + limi_cred + media_de_valo_face, family=binomial,data=data_num)

stepwise <- glm(fraudador ~ idade_clie + recomp_liqui + moda_taxa + 
                  valo_scon + limi_cred, family=binomial,data=data_num)

stepwise <- glm(fraudador ~ idade_clie + recomp_liqui + 
                  valo_scon, family=binomial,data=data_num)

step2 <- step(stepwise,direction="backward")
#Resume os resultados do modelo

summary(stepwise)
summary(step2)
#Calcula a razãoo de chances (demora um pouco pra calcular)

razao<-exp(cbind(OR = coef(stepwise), confint(stepwise)))
razao


# Daqui em diante a predição acontece para uma base que não precisa, necessariamente, 
# ter a informação fraudador. No caso dessa base, temos a informação, por isso conseguimos 
# calcular a precisão/taxa de acerto do modelo. Porém, no caso da predição em uma nova base, 
# onde queremos saber quem pode se tornar um fraudador basta somente aplicar o modelo
# usando as próximas linhas de comando

predito<-predict(stepwise,data_num,type="response")

# Se tem variáveis que aparecem como NA na tabela nova, então precisa rodar essas linhas a seguir
predito<-predito[!is.na(predito)]
validacao<-na.omit(validacao)

# salva os valores estimados
pred = prediction(predito, data_num$fraudador)
# define o valor de corte, ou seja, acima desse valor, tudo vai ser considerado 1
corte<-as.numeric(performance(pred, "auc")@y.values)
corte<-0.3

#cálculo do score no dataset de validacao
data_num$score<-predict(stepwise,type='response',data_num);round(data_num$score,4)
summary(data_num$score)

pred<-prediction(validacao$score, validacao$fraudador)
perf <- performance(pred,"tpr","fpr")
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline( a =0, b = 1, lwd = 2, lty = 2, col = "gray")

#Escolhe quem vai ser "1"e quem vai ser "0"
data_num$predito<-ifelse(predito>=corte,1,0)

#Compara os resultados 

tab<-table(data_num$predito,data_num$fraudador)

tab
taxaacerto<-(tab[2,2]+tab[1,1])/sum(tab)
taxaacerto







validacao$score
summary(validacao$score)
validacao$predito<-as.factor(validacao$predito)
summary(validacao$predito)

# Identificando clientes fraudadores

row.names(data_num) %in% !data_num$predito==data$fraudador
erros<-subset(data_num,!predito==fraudador)
fraude<-subset(erros,predito==1);row.names(fraude)
novos<-dplyr::select(fraude,score)
novos$cliente<-row.names(novos)

write.csv2(novos,'possiveis_fraudes.csv',row.names=F)

fraudulentos<-subset(data_num,predito==1)

row.names(fraudulentos)

fraudulentos$TITU_ID<-row.names(fraudulentos)
fraude<-merge(fraudulentos,dados,by='clie_id')
names(fraude)

id_fraude<-fraude %>%
  dplyr::select(TITU_ID,CLIE_ID)
fraudadores<-unique(id_fraude$CLIE_ID);fraudadores
#434 405 472 518 524  73 535 597 604 537   5 377 227 380 391 408