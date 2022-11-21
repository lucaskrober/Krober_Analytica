# Vetor contendo pacotes usados no script de aplicação
pacotes <- c("dplyr", "readxl", "xlsx", "janitor")

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

# Carregando a base de dados de clientes ATIVOS:
data <- readxl::read_xlsx('Base_regressaoCLIE.xlsx')

# Limpando os nomes das clunas
data <- janitor::clean_names(data)

# Carregando o modelo já ajustado na base anterior
load(file='modelo_stepwise.rda');

#Calculando o score pelo modelo ajustado, agora para os clientes ativos
data$score <-
  predict(stepwise, type = 'response', data)
round(data$score, 4)

View(data)

# Salvando o resultado em um documento xlsx
write.xlsx(data, 'previsao.xlsx',sheetName = 'Sheet 1', append = TRUE, row.names = FALSE)