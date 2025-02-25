require(epiR)  #### carregamento do pacote epidemiologia básica
require(glmtoolbox) ### calculo goodness fit
library(oddsratio) ### biblioteca para calcular odds  ratio automático
library(hnp) ### fazer o grafico de envelope simulado
library(car)


##############################################################
###########------- Conjunto de dados ----------- #############
##############################################################

### Metadados do conjunto de dados

# Descrição: O objetivo deste estudo é explorar a relação entre alguns fatores associados ao
# risco de doença cardiaca (resposta: presença ou ausência), estudo realizado em um período de 10 anos
# em Framingham (Massachusetts). Os dados foram coletados em 10 anos, porém não se trata de um estudo longitudinal.
## Temos 3656 registros completos e 15 variáveis, sendo:

#Resposta: doença cardíaca coronária (CHD),1= presença ou 0=ausência (resposta binária)

#Demográfico:
#• Sexo: masculino ou feminino (Nominal)
#• Idade: Idade do paciente; (Contínuo - Embora as idades registradas tenham sido truncadas para números inteiros, o conceito de idade é contínuo)

#Comportamental
#• Fumante atual: se o paciente ou não é um fumante atual (Nominal)
#• Cigs Per Day: o número de cigarros que a pessoa fuma em média em um dia. (pode ser considerado contínuo, pois pode-se fumar qualquer número de cigarros, mesmo meio cigarro.) 

# Histórico médico 
#• BP Meds: se o paciente estava ou não sob medicação para pressão arterial (Nominal)
#• Prevalente AVC: se o paciente já teve ou não um AVC (Nominal)
#• Prevalente Hyp: se o paciente era ou não hipertenso (Nominal)
#• Diabetes: se o paciente tinha ou não diabetes (Nominal)

# Histórico médico - clínico
#• Tot Chol: nível de colesterol total (Contínua)
#• PA sistólica: pressão arterial sistólica (Contínua)
#• PA Dia: pressão arterial diastólica (Contínua)
#• IMC: Índice de Massa Corporal (Contínua)
#• Frequência cardíaca: frequência cardíaca (contínua - em pesquisas médicas, variáveis ​​como a frequência cardíaca, embora de fato discretas, são consideradas contínuas devido ao grande número de valores possíveis.) 
#• Glicose: nível de glicose (contínuo)



##############################################################
#######------- Leitura do banco de dados----------- #########
##############################################################

data=read.csv2("D:\Projetos_em _R\AnaliseDaDoencaCardiacaCoronária\dataset - dataset.csv",dec=".", sep=";")
head(data)
attach(data)
names(data)


#Fatores associados ao risco, relativos ao histórico médico-clinico;
# Temos as seguintes informações de histórico médico 
#• Tot Chol: nível de colesterol total (Contínua)
#• PA sistólica: pressão arterial sistólica (Contínua)
#• PA Dia: pressão arterial diastólica (Contínua)
#• IMC: Índice de Massa Corporal (Contínua)
#• Frequência cardíaca: frequência cardíaca (contínua) 
#• Glicose: nível de glicose (contínuo)

###  +

### Variaveis dos individuos importantes (Confundidores)
## idade
## sexo
##############################################################
##############################################################



#####################
###nomes dos modelos 

# md1=age
# md2=male 

# md12=TotChol
# md13=PAsistolica
# md14=PADia
# md15=IMC
# md16=FreqCard
# md17=Glicose
##################### modelo com 8 covariáveis, 3656 linhas


        # Modelo 1: Sexo 

md1<-glm(TenYearCHD~factor(male),family=binomial,data=data) #glm generalized, da familia binomial
summary(md1)


# Calculando o odds ratio
or=exp(coef(md1)) #exponencial dos coeficientes
ci=exp(confint(md1)) #intervalo de confiança

#### mostrar organizado com 3 casas decimais (arrendodam com 3 casas)
oddsmd1=round(cbind(or,ci),3)
oddsmd1


# Sexo (Ref= Feminino, ou seja Masculino/Feminino) , têm-se     OR=  1.66 , IC com 95% (1.39 ; 1.99)
# neste caso, podemos interpretar que a
#chance de desenvolver doença cardíaca  em relação aos homens é 1,66 vezes maior quando comparada com a
#as mulheres

####### Valor do critério de Akaike
md1$aic



        # Modelo 2 : Idade  


md2<-glm(TenYearCHD~age,family=binomial,data=data) #glm generalized, da familia binomial
summary(md2)
md2


# Calculando o odds ratio
or=exp(coef(md2)) #exponencial dos coeficientes
#or
ci=exp(confint(md2)) #intervalo de confiança
#ci

oddsmd2=round(cbind(or,ci),3)
oddsmd2

#para aumento na idade em anos completos, aumenta em 1.081x a chance de ter 
#CHD presente em relação aos que não tem (ausencia de CHD), IC95%=(1.06; 1.09)

####### Valor do critério de Akaike
md2$aic



# Modelo 1.2: TotChol  
# Ajuste -Modelo Logistico

md12<-glm(TenYearCHD~totChol,family=binomial,data=data) #glm generalized, da familia binomial
summary(md12)
md12


# Probabilidade ajustada pelo modelo
ajuste=md12$fit


### Calculando o odds ratio
or=exp(coef(md12)) #exponencial dos coeficientes
ci=exp(confint(md12)) #intervalo de confiança

oddsmd12=round(cbind(or,ci),3)
oddsmd12

# Para cada unidade a mais mg/dl, aumenta em 1.006x a chance de ter 
#CHD presente em relação aos que não tem (ausencia de CHD)

# Valor do critério de Akaike
md12$aic


# Modelo 1.3:PAsistolica

md13<-glm(TenYearCHD~sysBP,family=binomial,data=data) #glm generalized, da familia binomial
summary(md13)
md13


# Probabilidade ajustada pelo modelo
ajuste=md13$fit


### Calculando o odds ratio
or=exp(coef(md13)) #exponencial dos coeficientes
ci=exp(confint(md13)) #intervalo de confiança

oddsmd13=round(cbind(or,ci),3)
oddsmd13


# Têm-se  OR=  1.025 , IC com 95% (1.021 ; 1,029)
# neste caso, podemos interpretar que a
# #para cada unidade a mais de pressão, aumenta em 1.025x a chance de ter 
#CHD presente em relação aos que não tem (ausencia de CHD)

# Valor do critério de Akaike
md13$aic


# Modelo 1.4: PADia
# Ajuste -Modelo Logistico

md14<-glm(TenYearCHD~diaBP,family=binomial,data=data) #glm generalized, da familia binomial
summary(md14)
md14

# Probabilidade ajustada pelo modelo
ajuste=md14$fit


### Calculando o odds ratio
or=exp(coef(md14)) #exponencial dos coeficientes
ci=exp(confint(md14)) #intervalo de confiança

oddsmd14=round(cbind(or,ci),3)
oddsmd14


# Têm-se  OR=  1.033 , IC com 95% (1.026 ; 1.041)
# neste caso, podemos interpretar que a
# #para cada unidade a mai de pressão, aumenta em 1.033x a chance de ter 
#CHD presente em relação aos que não tem (ausencia de CHD)

# Valor do critério de Akaike
md14$aic


# Modelo 1.5. IMC (BMI)
# Ajuste -Modelo Logistico

md15<-glm(TenYearCHD~BMI,family=binomial,data=data) #glm generalized, da familia binomial
summary(md15)
md15
# Probabilidade ajustada pelo modelo
ajuste=md15$fit


# Calculando o odds ratio
or=exp(coef(md15)) #exponencial dos coeficientes
ci=exp(confint(md15)) #intervalo de confiança

oddsmd15=round(cbind(or,ci),3)
oddsmd15

## Têm-se  OR=  1.054 , IC com 95% (1.032 ; 1.076)
# neste caso, podemos interpretar que 
#para cada unidade a mais do IMC, aumenta em 1.054x a chance de ter 
#CHD presente em relação aos que não tem (ausencia de CHD)

# Valor do critério de Akaike
md15$aic



# Modelo 1.6. FreqCard (heartRate) 
# Ajuste -Modelo Logistico

md16<-glm(TenYearCHD~heartRate,family=binomial,data=data) #glm generalized, da familia binomial
summary(md16)
md16

# Probabilidade ajustada pelo modelo
ajuste=md16$fit


### Calculando o odds ratio
or=exp(coef(md16)) #exponencial dos coeficientes
ci=exp(confint(md16)) #intervalo de confiança

oddsmd16=round(cbind(or,ci),3)
oddsmd16

## Têm-se  OR=  1.005 , IC com 95% (0.997 ; 1.012)
# neste caso, podemos interpretar que 
#para cada unidade a mais do hearRate, aumenta em 1.005x a chance de ter 
#CHD presente em relação aos que não tem (ausencia de CHD)
# Valor do critério de Akaike
md16$aic

# Modelo 1.7. Glicose
# Ajuste -Modelo Logistico

md17<-glm(TenYearCHD~glucose,family=binomial,data=data) #glm generalized, da familia binomial
summary(md17)
md17


# Probabilidade ajustada pelo modelo
ajuste=md17$fit


### Calculando o odds ratio
or=exp(coef(md17)) #exponencial dos coeficientes
ci=exp(confint(md17)) #intervalo de confiança

oddsmd17=round(cbind(or,ci),3)
oddsmd17


# Têm-se  OR=  1.010 , IC com 95% (1.007 ; 1.017)
# neste caso, podemos interpretar que 
#para cada unidade a mais de glicose, aumenta em 1.010x a chance de ter 
#CHD presente em relação aos que não tem (ausencia de CHD)

# Valor do critério de Akaike
md17$aic










# Modelo de regressão logística multiplo: histórico médico clínico 

# Histórico médico clínico
#Fatores associados ao risco, relativos ao histórico médico-clinico;
# Temos as seguintes informações de histórico médico 
#• Tot Chol: nível de colesterol total (Contínua)
#• PA sistólica: pressão arterial sistólica (Contínua)
#• PA Dia: pressão arterial diastólica (Contínua)
#• IMC: Índice de Massa Corporal (Contínua)
#• Frequência cardíaca: frequência cardíaca (contínua) 
#• Glicose: nível de glicose (contínuo)

### Variaveis dos individuos importantes (Confundidores)
## idade
## sexo


#####
md22<-glm(TenYearCHD ~ age + male+ totChol+ sysBP + diaBP + BMI + heartRate + glucose, ,family=binomial,data=data) 
#glm generalized, da familia binomial
summary(md22)

# Probabilidade ajustada pelo modelo
ajuste=md22$fit


### Calculando o odds ratio ajustado
or=exp(coef(md22)) #exponencial dos coeficientes
#or
ci=exp(confint(md22)) #intervalo de confiança
#ci

oddsmd22=round(cbind(or,ci),3)
oddsmd22

#### Comparando com odds ratio bruto
oddsmd1
oddsmd2
oddsmd12
oddsmd13
oddsmd14
oddsmd15
oddsmd16
oddsmd17

#obs: o odd do male aumentou para 2.066  e outros perderam o efeito 
#(heartRate, diaBP, BMI). O homem (male) tem o dobro da chance de desenvoler
# a doença cardíaça qdo avaliado o histórico médico clínico


# Adequação do modelo 


# Obtenção dos residuos padronizados pela deviance
dev<-residuals(md22,type='deviance')


# Somatório dos desvios deviance
QL<-sum(dev^2)
QL


# Valor-p para o teste de hipótese de adequação do Desvio Deviance baseado numa distribuição qui-quadrado
p1<-1-pchisq(QL,md22$df.residual)
p1

#Resumo do teste:
#Estatistica 2782.798 P-valor 1 não rejeito H0 - modelo adequado

# Obtenção dos residuos padronizados de Pearson
rpears<-residuals(md22,type='pearson')
#rpears

QP<-sum(rpears^2)
QP


#Valor-p para o teste de adequação do Qui-quadrado 
p2<-1-pchisq(QP,md22$df.residual)
p2

##3 Estatística do teste 3715.6, p-valor =0.21
#Tanto pelo resíduo da deviance qto pelo resíduo de Pearson, não rejeito H0
#então, modelo adequado

###### Teste de Hosmer and Lemeshow para adequação do modelo
hltest(md22)

#p-valor 0.36, tb não rejeito H0 - modelo adequado


# ****** Gráfico de normalidade de envelope simulado **
hnp(md22, mains="Gráfico de envelope Simulado", pch=16, col="blue", ylab="Resíduos Deviance", 
    xlab="Quantis Teóricos N(0,1)") #### Função Half-Normal Plots with Simulation Envelopes

#modelo binomial: pontos dentro da banda de confiança, sem outliers,
#sem pontos de influencia. Modelo adequado aos dados


# Gráfico ilustrativo com duas categorias [resíduos deviance]

plot(dev, main = "Resíduos deviance para o modelo RL", ylab = "Resíduos Deviance", 
     xlab = "Indice", pch = 16, ylim=c(-3.2,3.2))
abline(h = 0, lty = "dotted", col = "grey30")
abline(h = c(-3, 3), lty = "dashed", col = "red")
#residuos para o valor 1 e para o valor 0.

# Valor do critério de Akaike
md22$aic   
# obs:menor valor indica o melhor modelo

# Multicolinearidade 

library(car) 
vif(md22)



