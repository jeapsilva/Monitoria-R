# ----- CALCULO DE DESVIO PADRÃO PASI
# FORMULA: DP = erro_padrao*sqrt(n)

# MEDICAMENTO PLACEBO
dp_pasi_placebo = 0.5*168^(1/2)

# MEDICAMENTO ETANERCEPT
dp_pasi_etanercept = 0.3*358^(1/2)

# MEDICAMENTO Ixenquisumab 
dp_pasi_ixenquisumab = 0.3*347^(1/2)

# -------- CALCULO DE DESVIO PADRÃO DLQI

# MEDICAMENTO PLACEBO 
dp_dlqi_placebo - 0.4*168^(1/2)

# MEDICAMENTO ETANERCEPT 
dp_dlqi_etanercept = 0.3*358^(1/2)

# MEDICAMENTO IXEKISUMAB
dp_dlqi_ixekisumab = 0.3*347^(1/2)

# -------- CALCULO DO EFEITO COHENS-D / PASI
# FORMULA d = (m1-m2)/sqrt((dp_1**2 + dp_2**2)/2)

#PASI versus Placebo versus Ixekisumab 
cohen_pasi_placebo_ixek = (-1.31- (-16.8)) / ((6.48^2+5.58^2)/2)^(1/2)

# PASI Etanercept versus Ixenkisumab 
cohen_pasietan_ixen = (-11.8 - (-16.8)) / ((5.67^2+5.58^2)/2)^(1/2)

# PASI Etanercept Versus placebo 
cohen_pasietan_placebo = (-1.31 - (-11.8)) / ((6.48^2+5.67^2)/2)^(1/2)

# -------- CALCULO DO EFEITO COHENS-D / DLQI

# DLQI Placebo versus Ixekisumab 
dlqi_placebo_ixek = (-2 - (-9.4)) / ((5.18^2+5.58^2)/2)^(1/2)

# DLQI Ixekisumab versus etanercept 
dlqi_ixek_etanet = (-9.4 - (-7.7)) / ((5.58^2+5.67^2)/2)^(1/2)

#DLQI Etanercept versus Placebo 
dlqi_etan_placebo = (-7.7 - (-2)) / ((5.67^2+5.18^2)/2)^(1/2)

# -------- CALCULO DO EFEITO COHENS-D / HAMD17

# HAMD17 Placebo versus Fluoxetina
hamd_placebo_fluoxetina = (15.7-14) / ((7.4^2+7.7^2)/2)^(1/2)

#HAMD17 esperado para fluoxetina com grande efeito 
hamd_fluoxetina_efeito = -((0.8*((7.4^2+7.7^2)/2)^(1/2))-15.7)

# ---------- TESTE T PARA DLQI
# COHENS 
d_teste_t = (76.7-72.4)/sqrt((19.4^2+18.0^2)/2)  
pwr.t.test(d= d_teste_t, sig.level=0.05, power=0.8, type='two.sample')

# ------- QUESTÃO DOIS
# Após ler um artigo sobre a importância fisiopatológica do fator de crescimento endotelial
# para tumores você tem a ideia de iniciar um ensaio clínico # de uma nova medição contra 
# este alvo, o Bevacizumab. Seu objetivo e incluir apenas os pacientes com carcinoma espinocelular 
# de pele avançado e comparar a nova medicação com a terapia padrão. Apesar de seu hospital ser 
# referência para estes casos no país, você tem dúvidas se o número de pacientes que acompanha será 
# suficiente para realizar o estudo. Por não encontrar nenhum trabalho prévio que utilizou esta 
# medicação na neoplasia que deseja estudar você decide utilizar os dados de um estudo sobre câncer 
# de ovário para calcular o número de pacientes necessário. No estudo você encontra as seguintes tabelas(3).

# Parte 1 - Mortalidade- Qui quadrado 
install.packages("pwr")
library("pwr")
power.prop.test(p1=0.69, p2=0.64, power=0.8)
power.prop.test(n = NULL, p1 = 0.69, p2 = 0.64, sig.level = 0.05,  power = 0.8, alternative = c("two.sided"))

# Parte 2 - Razão dos riscos
ssizaCT.default(power=8, k=1, pE=0.64,pC=0.69,RR=0.78,alpha=0.05) 
# k = proporção experimental e controle
# pE = probabilidade de chegar ao desfecho do grupo experimental no período do estudo
# pC: Probabilidade de chegar ao desfecho do grupo experimental no período do estudo

# Parte 3 - Qualidade de vida dos sobreviventes
# etapa 1: calculo do desvio padrao conjunto
# FORMULA: dpc = sqrt( ( (n1-1)*(dp_1**2) + (n2-1)*(dp_2**2) ) / (n1+n2-2) )
dpc = (((26-1)*(19.4)**2 + (44-1)*(18**2))/(26+44-2))^(1/2)
#dpc = 18.525

# etapa 2: 
power.t.test(n=NULL,delta=4.3,sd=18.525,sig.level=0.05,
             power=0.8,type=c("two.sampe","one.sample","paired"),
             alternative=c("two.sided","one.sided"),
             strict = FALSE,tol=.Machine$double.eps^0.25)

# ------- ANÁLISE DE SOBREVIDA
install.packages("pracma")
install.packages("powerSurvEpi")
library("pracma")
library("powerSurvEpi")

ssizeCT.default(power = 0.8, k = 1, pE = 0.64, pC = 0.69, RR = 0.78, alpha = 0.05)

power.t.test(n = NULL, delta = 4.3, sd = 18.7, sig.level = 0.05, power =0.8, type = c("two.sample", "one.sample", "paired"), alternative = c("two.sided", "one.sided"),strict = FALSE, tol = .Machine$double.eps^0.25)


