#Aplica��o da equa��o de crescimento e produ��o proposta por Clutter(1973)

b0=3.00237443 
b1=-19.28115118 
b2=-22.85321111   
b3=1.13377302  
b4=2.48336295   
b5=0.06528737

#Dados da fazenda BOA

id1=62.48
ab1=29.97
s=29.125

eqvol=expression(exp(b0+b1/s+b2/x+b3*(id1/x)*log(ab1)+b4*(1-id1/x)+b5*(1-id1/x)*s));

#criar fun��o vtcc

vtcc=function(x){eval(eqvol)};

vtcc(seq(48,120,12))

#c�culo derivada

D(eqvol,'x'); 

#Equa��o que descreve o incremento corrente

der.eqvol=expression(exp(b0 + b1/s + b2/x + b3 * (id1/x) * log(ab1) + b4 * (1 - id1/x) + 
                         b5 * (1 - id1/x) * s) * (b4 * (id1/x^2) - (b3 * (id1/x^2) * 
                         log(ab1) + b2/x^2) + b5 * (id1/x^2) * s))


#OU
(der.eqvol=parse(text=deparse(D(eqvol,'x'))))

icm=function(x){eval(der.eqvol)} 

icm(60); #Incremento corrente mensal
(ica5anos=icm(60)*12) #incremento corrente anual para 5 anos

imm=function(x){eval(vtcc(x))/x} #incremento m�dio mensal

imm(60)
(ima5=imm(60)*12); #incremento m�dio anual para 5 anos

#identificando a idade �tima de corte

#1� op��o

op=optimize(f=imm,interv=c(12,120),maximum=T);
(ioc=op$maximum); #Idade �tima de corte em meses
(ioc_anos=ioc/12) #Idade �tima de corte em anos

#2�op��o

fo=function(x){abs(icm(x)-imm(x))}
op=optimize(f=fo,interv=c(12,120),maximum=F);
(ioc=op$minimum); #Idade �tima de corte em meses
(ioc_anos=ioc/12) #Idade �tima de corte em anos



x11();
par(mfrow=c(2,1));
#express�o matematica curve

curve(vtcc(x),12,120,xlab='idade(meses)',ylab='volume(m�/ha)',col='darkgreen');

curve(icm(x)*12,12,120,xlab='idade(meses)',ylab='incremento(m�/ha.ano)',col='darkgreen');

curve(imm(x)*12,12,120,col='red',add=T);
legend('topright',c('ica','ima'),text.col=c('darkgreen','red'),box.lty=0);
grid();
abline(v=ioc,lty=2);
text(ioc,imm(ioc)*12,paste(round(ioc/12,2),'anos'),cex=0.8,col='black')

#pacote ggplot2 para fazer gr�ficos

#3�op��o
#equa��o icremento m�dio mensal
eqimm=expression(exp(b0+b1/s+b2/x+b3*(id1/x)*log(ab1)+b4*(1-id1/x)+b5*(1-id1/x)*s)/x);

D(eqimm,'x');#Derivada da equa��o do incremento m�dio
der.eqimm=expression(exp(b0 + b1/s + b2/x + b3 * (id1/x) * log(ab1) + b4 * (1 - id1/x) + 
                     b5 * (1 - id1/x) * s) * (b4 * (id1/x^2) - (b3 * (id1/x^2) * 
                    log(ab1) + b2/x^2) + b5 * (id1/x^2) * s)/x - exp(b0 + b1/s + 
                    b2/x + b3 * (id1/x) * log(ab1) + b4 * (1 - id1/x) + b5 * 
                    (1 - id1/x) * s)/x^2)


#No maple: isolate(der.eqimm,x)
(x=b4*id1-b3*id1*log(ab1)-b2+b5*id1*s)
imm(x)*12
icm(x)*12
