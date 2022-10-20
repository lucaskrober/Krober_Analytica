dir()
dadoss=read.csv2('dados_ra..csv')
dados=subset(dadoss,estrato==573116)
names(dados)

seldados=subset(dados,,c('parcela','id','hd'));
names(seldados)=c('idamostra','idade1','hdom1');

modelo='b0*((hdom1/b0)^((idade1/idade2)^b2))';   
parms=data.frame(b0=56.69987, b1=0.67776)

idref=72 #Idade referência

library(cmrinvflor);

x11();

cls=class_sitio_dif_alg(seldados,modelo,parms,idref,graf_curvas=T)
cls$estabilidade;

cls$classes
View(cls$amostras)

write.csv2(cls$amostras,'reclassificaçãoshump.csv',row.names=F);
shell.exec('reclassificaçãoshump.csv')
