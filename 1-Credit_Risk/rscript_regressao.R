library(readxl)
library(dplyr)

data<-read_xlsx('Base_regressaoCLIE_Inativos.xlsx')
data<-janitor::clean_names(data)
#Regressao Logistica 

data_num<- data %>%
  dplyr::select(-clie_id,-resu_a,-resu_c,-resu_n,-resu_r,-resu_titu_d,-resu_titu_c)

data_num<-as.data.frame(data_num)
row.names(data_num)<-data$clie_id

modelo.completo <- glm(fraudador~.,family=binomial,data=data_num)
summary(modelo.completo)

#Abordagem Stepwise para selecao de variaveis
stepwise <- step(modelo.completo,direction="both")
stepwise$formula #retornar o modelo gerado
summary(stepwise) #resumo da regressão com significâncias de cada variável

#Modelo com as variaveis indicadas pelo Stepwise
stepwise <- glm(stepwise$formula, family=binomial,data=data_num)
summary(stepwise)

predito<-predict(stepwise,data_num,type="response")
# predito<-predito[!is.na(predito)]

# define o valor de corte, ou seja, acima desse valor, tudo vai ser considerado 1
# calculo intermediario para calcular o corte pelo algoritmo de stepwise
#pred = prediction(predito, data_num$fraudador)
#corte<-as.numeric(performance(pred, "auc")@y.values)

# definido como 0.3 para um perfil conservador
corte<-0.3

#cálculo do score no dataset de validacao
data_num$score<-predict(stepwise,type='response',data_num);round(data_num$score,4)
summary(data_num$score)

#razao<-exp(cbind(OR = coef(stepwise), confint(stepwise)))
#razao

#Escolhe quem vai ser "1"e quem vai ser "0"
data_num$predito<-ifelse(predito>=corte,1,0)

#Compara os resultados 

tab<-table(data_num$predito,data_num$fraudador)

tab
taxaacerto<-(tab[2,2]+tab[1,1])/sum(tab)
taxaacerto


prev_fraude<-subset(data_num,predito==0)
prev_errado<-subset(prev_fraude,fraudador==1)

recursao<-tab[2,2]/(tab[2,2]+tab[1,2]);recursao
precisao<-tab[2,2]/(tab[2,2]+tab[2,1]);precisao


# Cálculo do pseudo-R²: significa o quanto (em %) as variáveis do modelo explicaram a variável dependente
library(modEvA)
RsqGLM(stepwise)

saveRDS;