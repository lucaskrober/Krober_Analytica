# Vetor contendo pacotes usados no script
pacotes <- c("dplyr", "readxl", "xlsx", "modEvA", "janitor")

# Função para instalar se não estiverem instalados e carregar logo em seguida
if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
  }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

# Carregando a base de dados de clientes inativos para ajuste
data <- read_xlsx('Base_regressaoCLIE_Inativos.xlsx')
data <- janitor::clean_names(data) # limpando os nomes das colunas

# Removendo essas colunas
data_num <- data %>%
  dplyr::select(-clie_id,
                -resu_a,
                -resu_c,
                -resu_n,
                -resu_r,
                -resu_titu_d,
                -resu_titu_c)

# Convertendo tibble em data frame
data_num <- as.data.frame(data_num)

# Salvando o clie_id
row.names(data_num) <- data$clie_id

# Regressão logística: criando o modelo com todas as colunas
modelo.completo <- glm(fraudador ~ ., family = binomial, data = data_num)
summary(modelo.completo)

#Abordagem Stepwise para selecao de variaveis
stepwise <- step(modelo.completo, direction = "both")
stepwise$formula #retornar o modelo gerado
summary(stepwise) #resumo da regressão com significâncias de cada variável

#Modelo com as variaveis indicadas pelo Stepwise
stepwise <- glm(stepwise$formula, family = binomial, data = data_num)
summary(stepwise)

predito <- predict(stepwise, data_num, type = "response")

# Caso seus dados contenham valores faltantes, remover com a linha a seguir:
# predito<-predito[!is.na(predito)]


# define o valor de corte, ou seja, acima desse valor, tudo vai ser considerado 1
# calculo intermediario para calcular o corte pelo algoritmo de stepwise
# pred <- prediction(predito, data_num$fraudador)
# corte <- as.numeric(performance(pred, "auc")@y.values)

# Corte definido como 0.3 para um perfil conservador
corte <- 0.3

# Cálculo do score no dataset de validacao
data_num$score <-
  predict(stepwise, type = 'response', data_num)
round(data_num$score, 4)
summary(data_num$score)

# Escolhe quem vai ser "1"e quem vai ser "0" para o cálculo da matriz de confusão
data_num$predito <- ifelse(predito >= corte, 1, 0)

# Comparando os resultados, matriz de confusão
tab <- table(data_num$predito, data_num$fraudador);tab

# Taxa de acerto
taxa <- (tab[2, 2] + tab[1, 1]) / sum(tab); taxa

prev_fraude <- subset(data_num, predito == 0)
prev_errado <- subset(prev_fraude, fraudador == 1)

# Verdadeiros positivos para fraudadores / total de fraudadores
recursao <- tab[2, 2] / (tab[2, 2] + tab[1, 2]);recursao

# Verdadeiros positivos para fraudadores / total de caracterizados como fraudadores pelo modelo
precisao <- tab[2, 2] / (tab[2, 2] + tab[2, 1]);precisao


# Cálculo do pseudo-R²: significa o quanto (em %) as variáveis do modelo explicaram a variável dependente
RsqGLM(stepwise,plot=FALSE)

save(stepwise,file='modelo_stepwise.rda')


