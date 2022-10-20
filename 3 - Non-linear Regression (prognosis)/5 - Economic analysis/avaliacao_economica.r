dir()
fc=read.csv2('fluxo_caixa Fazenda A.csv');
View(fc);

library(cmrinvflor);

#tj=0.08; #Taxa de Juros
vt=4221.43; #Valor da Terra

calc_cmpr= T; # Se � ou n�o pra calcular o custo m�dio de produ��o

x11();

ae=avecon(fc,tj=13.65,vt,calc_cmpr); #analise economica, fluxo de caixa, valor da terra, e falar que � pra calcular custo m�dio de produ��o
View(ae)

#Vplinf projetos idades diferentes , vpl curto prazo o quanto realmente receber�

with(ae,plot(projeto,vplinf,type='l',col=2));
grid()

ae=avecon(fc,tj=0.0979,vt,calc_cmpr); #analise economica, fluxo de caixa, valor da terra, e falar que � pra calcular custo m�dio de produ��o

with(ae,lines(projeto,vplinf,type='l',col=3)); #lines fazer gr�fico junto com anterior

ae=avecon(fc,tj=0.0712,vt,calc_cmpr); #analise economica, fluxo de caixa, valor da terra, e falar que � pra calcular custo m�dio de produ��o

with(ae,lines(projeto,vplinf,type='l',col=4));

with(ae,plot(projeto,cmpr,type='l',col=4)); #Custo m�dio produ��o
with(ae,plot(projeto,tir,type='l',col=4)); #Taxa interna de retorno


with(ae,plot(projeto,vplinf,type='l',col=1,ylim=c(-10000,10000)));
tj=c(0.0712,0.1365,0.0979);
for(i in 2: length(tj)){                                    #looping, la�o, i come�ando posi��o 2,
   ae=avecon(fc,tj=tj[i],vt,calc_cmpr);
   with(ae,lines(projeto,vplinf,type='l',col=i));                      
}  
legend('topright',text.col=1:length(tj),
       legend=paste(tj*100,'%',sep=''),
       bty='n',ncol=length(tj))

