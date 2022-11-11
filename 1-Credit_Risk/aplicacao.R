data<-readxl::read_xlsx('Base_regressaoCLIE.xlsx')

data<-janitor::clean_names(data)


data$score<-predict(stepwise,type='response',data);round(data$score,4)
summary(data$score)


write.xlsx(data,'previsao.xlsx')