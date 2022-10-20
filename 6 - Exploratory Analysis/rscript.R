# Sara Toassa


# Análise das respostas dos residentes
library(ggstatsplot)
library(Hmisc)
library(vcdExtra)


# for(i in 3:ncol(resid)){
#   for(j in 1:nrow(resid)){
#     if(resid[j,i]==1){
#       resid[j,i]<-"Acertos"
#     }else{
#       resid[j,i]<-"Erros"
#     }
#   }
# }

#write.csv2(resid,'residentes.csv',row.names=F)
# resid<-read.csv2('residentes.csv',stringsAsFactors = T)


###############################################################################################
# Análise todas as perguntas juntas
# Análise todas as perguntas juntas
# Análise todas as perguntas juntas
# Análise todas as perguntas juntas
# Análise todas as perguntas juntas
# Análise todas as perguntas juntas
# Análise todas as perguntas juntas
###############################################################################################

resid<-read.csv2('residentes_total.csv')

str(resid)

tab<-table(resid$etapa,resid$r1);tab
fisher<-fisher.test(tab);fisher
gama<-GKgamma(tab);gama

ggbarstats(
  resid, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de fisher", ", p-valor ",
    ifelse(fisher$p.value < 0.05, "< 0.05", round(fisher$p.value, 3)))
)


#Post-hoc
pwft<-rstatix::pairwise_fisher_test(as.matrix(tab), p.adjust.method = "fdr")
pwft
resid1<-subset(resid,!etapa==3)
resid2<-subset(resid,!etapa==2)
resid3<-subset(resid,!etapa==1)

ggbarstats(
  resid1, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[1] < 0.05, "< 0.05", round(pwft$p.adj[1], 3)))
)



ggbarstats(
  resid2, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[2] < 0.05, "< 0.05", round(pwft$p.adj[2], 3)))
)


ggbarstats(
  resid3, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[3] < 0.05, "< 0.05", pwft$p.adj[3]))
)
library(ggplot2)
library(Rmisc)

w=1366
h=768
#x11(width=w,height=h);multiplot(g1, g2, g3, cols = 3)

###############################################################################################
# Análise por ano
# Análise por ano
# Análise por ano
# Análise por ano
# Análise por ano
# Análise por ano
# Análise por ano
###############################################################################################

resid<-read.csv2('residentes_total.csv')
resid$ano<-as.factor(resid$ano)

str(resid)
summary(resid)


resid1<-subset(resid,ano==1)
tab<-table(resid1$etapa,resid1$r1);tab
fisher<-fisher.test(tab);fisher
gama<-GKgamma(tab);gama

ggbarstats(
  resid1, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher$p.value < 0.05, "< 0.05", round(fisher$p.value, 3)))
)


resid2<-subset(resid,ano==2)
tab<-table(resid2$etapa,resid2$r1);tab
fisher<-fisher.test(tab);fisher
gama<-GKgamma(tab);gama

ggbarstats(
  resid2, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher$p.value < 0.05, "< 0.05", round(fisher$p.value, 3)))
)

resid3<-subset(resid,ano==3)
tab<-table(resid3$etapa,resid3$r1);tab
fisher<-fisher.test(tab);fisher
gama<-GKgamma(tab);gama

ggbarstats(
  resid3, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher$p.value < 0.05, "< 0.05", round(fisher$p.value, 3)))
)

# w=1366
# h=768
# x11(width=w,height=h);multiplot(g1, g2, g3, cols = 3)

#Post-hoc ano 1
tab<-table(resid1$etapa,resid1$r1);tab
pwft<-rstatix::pairwise_fisher_test(as.matrix(tab), p.adjust.method = "fdr")
pwft
res1<-subset(resid1,!etapa==3)
res2<-subset(resid1,!etapa==2)
res3<-subset(resid1,!etapa==1)

ggbarstats(
  res1, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[1] < 0.05, "< 0.05", round(pwft$p.adj[1], 3)))
)



ggbarstats(
  res2, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[2] < 0.05, "< 0.05", round(pwft$p.adj[2], 3)))
)


ggbarstats(
  res3, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[3] < 0.05, "< 0.05", pwft$p.adj[3]))
)

#x11(width=w,height=h);multiplot(g1, g2, g3, cols = 3)


#Post-hoc ano 2
tab<-table(resid2$etapa,resid2$r1);tab
pwft<-rstatix::pairwise_fisher_test(as.matrix(tab), p.adjust.method = "fdr")
pwft
res1<-subset(resid2,!etapa==3)
res2<-subset(resid2,!etapa==2)
res3<-subset(resid2,!etapa==1)

ggbarstats(
  res1, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[1] < 0.05, "< 0.05", round(pwft$p.adj[1], 3)))
)



ggbarstats(
  res2, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[2] < 0.05, "< 0.05", round(pwft$p.adj[2], 3)))
)


ggbarstats(
  res3, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[3] < 0.05, "< 0.05", pwft$p.adj[3]))
)


#x11(width=w,height=h);multiplot(g1, g2, g3, cols = 3)


#Post-hoc ano 3
tab<-table(resid3$etapa,resid3$r1);tab
pwft<-rstatix::pairwise_fisher_test(as.matrix(tab), p.adjust.method = "fdr")
pwft
res1<-subset(resid3,!etapa==3)
res2<-subset(resid3,!etapa==2)
res3<-subset(resid3,!etapa==1)

ggbarstats(
  res1, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[1] < 0.05, "< 0.05", round(pwft$p.adj[1], 3)))
)



ggbarstats(
  res2, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[2] < 0.05, "< 0.05", round(pwft$p.adj[2], 3)))
)


ggbarstats(
  res3, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[3] < 0.05, "< 0.05", pwft$p.adj[3]))
)

#x11(width=w,height=h);multiplot(g1, g2, g3, cols = 3)


###############################################################################################
# Análise por pergunta
# Análise por pergunta
# Análise por pergunta
# Análise por pergunta
# Análise por pergunta
# Análise por pergunta
# Análise por pergunta
###############################################################################################
resid<-read.csv2('residentes.csv',stringsAsFactors = T)

str(resid)

ggbarstats(
  resid, r3, etapa,
  results.subtitle = F
)
ggbarstats(
  resid, r6, etapa,
  results.subtitle = F
)

resid$r3<-NULL
resid$r6<-NULL

summary(resid)

fisher<- vector("list",length(3:ncol(resid)))
tab<-vector("list",length(3:ncol(resid)))
gama<-vector("list",length(3:ncol(resid)))

for (j in 1:8){
    tab[[j]]<-table(resid$etapa,resid[,(j+2)])
    print(names(resid)[j+2])
    print(tab[[j]])
    fisher[[j]]<-fisher.test(tab[[j]])
    print(fisher[[j]])
    pwft<-rstatix::pairwise_fisher_test(as.matrix(tab[[j]]), p.adjust.method = "fdr")
    print(pwft)
    print(paste('#------------------------------------------------------------#'))
    
}



ggbarstats(
  resid, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[1]]$p.value < 0.05, "< 0.05", round(fisher[[1]]$p.value, 3)))
)

ggbarstats(
  resid, r2, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[2]]$p.value < 0.05, "< 0.05", round(fisher[[2]]$p.value, 3)))
)


ggbarstats(
  resid, r4, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[3]]$p.value < 0.05, "< 0.05", round(fisher[[3]]$p.value, 3)))
)


ggbarstats(
  resid, r5, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[4]]$p.value < 0.05, "< 0.05", round(fisher[[4]]$p.value, 3)))
)


ggbarstats(
  resid, r7, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[5]]$p.value < 0.05, "< 0.05", round(fisher[[5]]$p.value, 3)))
)

ggbarstats(
  resid, r8, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[6]]$p.value < 0.05, "< 0.05", round(fisher[[6]]$p.value, 3)))
)


ggbarstats(
  resid, r9, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[7]]$p.value < 0.05, "< 0.05", round(fisher[[7]]$p.value, 3)))
)

ggbarstats(
  resid, r10, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[8]]$p.value < 0.05, "< 0.05", round(fisher[[8]]$p.value, 3)))
)

#################################################################################################
# Análise etapa 1 vs etapa 2
# Análise etapa 1 vs etapa 2
# Análise etapa 1 vs etapa 2
# Análise etapa 1 vs etapa 2
# Análise etapa 1 vs etapa 2
resid<-subset(resid,!etapa==3)

resid$r3<-NULL
resid$r6<-NULL

fisher<- vector("list",length(3:ncol(resid)))
tab<-vector("list",length(3:ncol(resid)))
gama<-vector("list",length(3:ncol(resid)))

for (j in 1:8){
  tab[[j]]<-table(resid$etapa,resid[,(j+2)])
  print(names(resid)[j+2])
  print(tab[[j]])
  fisher[[j]]<-fisher.test(tab[[j]])
  print(fisher[[j]])
  gama[[j]]<-GKgamma(tab[[j]])
  print(paste0("A estatística gamma é: ", gama[[j]][1]))
  print(paste('#------------------------------------------------------------#'))
}

ggbarstats(
  resid, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de fisher", ", p-valor ",
    ifelse(fisher[[1]]$p.value < 0.05, "< 0.05", round(fisher[[1]]$p.value, 3)))
)

ggbarstats(
  resid, r2, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[2]]$p.value < 0.05, "< 0.05", round(fisher[[2]]$p.value, 3)))
)


ggbarstats(
  resid, r4, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[3]]$p.value < 0.05, "< 0.05", round(fisher[[3]]$p.value, 3)))
)


ggbarstats(
  resid, r5, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[4]]$p.value < 0.05, "< 0.05", round(fisher[[4]]$p.value, 3)))
)


ggbarstats(
  resid, r7, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[5]]$p.value < 0.05, "< 0.05", round(fisher[[5]]$p.value, 3)))
)

ggbarstats(
  resid, r8, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[6]]$p.value < 0.05, "< 0.05", round(fisher[[6]]$p.value, 3)))
)


ggbarstats(
  resid, r9, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[7]]$p.value < 0.05, "< 0.05", round(fisher[[7]]$p.value, 3)))
)

ggbarstats(
  resid, r10, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[8]]$p.value < 0.05, "< 0.05", round(fisher[[8]]$p.value, 3)))
)



####################################################################################################
# Etapa 2 vs etapa 3
# Etapa 2 vs etapa 3
# Etapa 2 vs etapa 3
# Etapa 2 vs etapa 3
# Etapa 2 vs etapa 3
# Etapa 2 vs etapa 3
####################################################################################################
resid<-read.csv2('residentes.csv',stringsAsFactors = T)
resid<-subset(resid,!etapa==1)

resid$r3<-NULL
resid$r6<-NULL

fisher<- vector("list",length(3:ncol(resid)))
tab<-vector("list",length(3:ncol(resid)))
gama<-vector("list",length(3:ncol(resid)))

for (j in 1:8){
  tab[[j]]<-table(resid$etapa,resid[,(j+2)])
  print(names(resid)[j+2])
  print(tab[[j]])
  fisher[[j]]<-fisher.test(tab[[j]])
  print(fisher[[j]])
  gama[[j]]<-GKgamma(tab[[j]])
  print(paste0("A estatística gamma é: ", gama[[j]][1]))
  print(paste('#------------------------------------------------------------#'))
}

ggbarstats(
  resid, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de fisher", ", p-valor ",
    ifelse(fisher[[1]]$p.value < 0.05, "< 0.05", round(fisher[[1]]$p.value, 3)))
)

ggbarstats(
  resid, r2, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[2]]$p.value < 0.05, "< 0.05", round(fisher[[2]]$p.value, 3)))
)


ggbarstats(
  resid, r4, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[3]]$p.value < 0.05, "< 0.05", round(fisher[[3]]$p.value, 3)))
)


ggbarstats(
  resid, r5, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[4]]$p.value < 0.05, "< 0.05", round(fisher[[4]]$p.value, 3)))
)


ggbarstats(
  resid, r7, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[5]]$p.value < 0.05, "< 0.05", round(fisher[[5]]$p.value, 3)))
)

ggbarstats(
  resid, r8, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[6]]$p.value < 0.05, "< 0.05", round(fisher[[6]]$p.value, 3)))
)


ggbarstats(
  resid, r9, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[7]]$p.value < 0.05, "< 0.05", round(fisher[[7]]$p.value, 3)))
)

ggbarstats(
  resid, r10, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[8]]$p.value < 0.05, "< 0.05", round(fisher[[8]]$p.value, 3)))
)



####################################################################################################
# Etapa 1 vs etapa 3
# Etapa 1 vs etapa 3
# Etapa 1 vs etapa 3
# Etapa 1 vs etapa 3
# Etapa 1 vs etapa 3
# Etapa 1 vs etapa 3
####################################################################################################
resid<-read.csv2('residentes.csv',stringsAsFactors = T)

resid<-subset(resid,!etapa==2)

resid$r3<-NULL
resid$r6<-NULL

fisher<- vector("list",length(3:ncol(resid)))
tab<-vector("list",length(3:ncol(resid)))
gama<-vector("list",length(3:ncol(resid)))

for (j in 1:8){
  tab[[j]]<-table(resid$etapa,resid[,(j+2)])
  print(names(resid)[j+2])
  print(tab[[j]])
  fisher[[j]]<-fisher.test(tab[[j]])
  print(fisher[[j]])
  gama[[j]]<-GKgamma(tab[[j]])
  print(paste0("A estatística gamma é: ", gama[[j]][1]))
  print(paste('#------------------------------------------------------------#'))
}

ggbarstats(
  resid, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de fisher", ", p-valor ",
    ifelse(fisher[[1]]$p.value < 0.05, "< 0.05", round(fisher[[1]]$p.value, 3)))
)

ggbarstats(
  resid, r2, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[2]]$p.value < 0.05, "< 0.05", round(fisher[[2]]$p.value, 3)))
)


ggbarstats(
  resid, r4, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[3]]$p.value < 0.05, "< 0.05", round(fisher[[3]]$p.value, 3)))
)


ggbarstats(
  resid, r5, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[4]]$p.value < 0.05, "< 0.05", round(fisher[[4]]$p.value, 3)))
)


ggbarstats(
  resid, r7, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[5]]$p.value < 0.05, "< 0.05", round(fisher[[5]]$p.value, 3)))
)

ggbarstats(
  resid, r8, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[6]]$p.value < 0.05, "< 0.05", round(fisher[[6]]$p.value, 3)))
)


ggbarstats(
  resid, r9, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[7]]$p.value < 0.05, "< 0.05", round(fisher[[7]]$p.value, 3)))
)

ggbarstats(
  resid, r10, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(fisher[[8]]$p.value < 0.05, "< 0.05", round(fisher[[8]]$p.value, 3)))
)




###############################################################################################
# Análise por ano da residência
# Análise por ano da residência
# Análise por ano da residência
# Análise por ano da residência
# Análise por ano da residência
# Análise por ano da residência
# Análise por ano da residência
###############################################################################################














###############################################################################################
# Análise das respostas das enfermeiras
# Análise das respostas das enfermeiras
# Análise das respostas das enfermeiras
# Análise das respostas das enfermeiras
# Análise das respostas das enfermeiras
# Análise das respostas das enfermeiras
# Análise das respostas das enfermeiras
###############################################################################################

# Juntar as perguntas e analisar todas ao mesmo tempo!

resid<-read.csv2('enfermeiras.csv',stringsAsFactors = T)

str(resid)


tab<-table(resid$etapa,resid$r1)
fisher<-fisher.test(tab)
gama<-GKgamma(tab)
tab
fisher
gama


ggbarstats(
  resid, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de fisher", ", p-valor ",
    ifelse(fisher$p.value < 0.05, "< 0.05", round(fisher$p.value, 3)))
)


#Post-hoc
pwft<-rstatix::pairwise_fisher_test(as.matrix(tab), p.adjust.method = "fdr")
pwft

?pairwise_fisher_test

resid1<-subset(resid,!etapa==3)
resid2<-subset(resid,!etapa==2)
resid3<-subset(resid,!etapa==1)



ggbarstats(
  resid1, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[1] < 0.05, "< 0.05", round(pwft$p.adj[1], 3)))
)



ggbarstats(
  resid2, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[2] < 0.05, "< 0.05", round(pwft$p.adj[2], 3)))
)


ggbarstats(
  resid3, r1, etapa,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Teste exato de Fisher", ", p-valor ",
    ifelse(pwft$p.adj[3] < 0.05, "< 0.05", pwft$p.adj[3]))
)
w=1366
h=768
#x11(width=w,height=h);multiplot(g1, g2, g3, cols = 3)
#knitr::stitch('rscript.r')
