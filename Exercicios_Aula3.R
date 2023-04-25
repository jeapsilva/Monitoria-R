# Leitura da base de dados CSV
caminho1 = 'D:\\Mestrado - Unb\\Disciplinas\\Metodos Quantitativos\\Semana 3\\Atividade 3\\pos AVE Paretico e nao paretico.csv'
base_dados1 = read.csv(caminho1)

# Leitura da base de dados SAV
library(haven)
base_dados2 = read_sav("D:/Mestrado - Unb/Disciplinas/Metodos Quantitativos/Semana 3/Atividade 3/Datafile Patient activation for self-management.sav")

# Analise exploratoria das colunas na base de dados
colnames(base_dados1)
colnames(base_dados2)

# Consultando o tamanho de linhas da base de dados 
length(base_dados1)
length(base_dados2)

# Visualizando as primeiras linhas de cada base de dados
head(base_dados1,5)
head(base_dados2,5)

# Download das bibliotecas necessárias para análises
install.packages("dplyr")
install.packages("devtools")
install.packages("ggpubr")
install.packages("moments")
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("foreign")

# Bibliotecas necessárias para análise
library("dplyr")
library("devtools")
library("ggpubr")
library("moments")
library("ggpubr")
library("ggplot2")
library("foreign")

# analisando o tipo de variavéis da base de dados
str(base_dados1)
str(base_dados2)

# Principais medidas de tendência central de cada uma das bases de dados
summary(base_dados1)
summary(base_dados2)

# Criando análise de duas variáveis
# Simples
by(base_dados2$activation_score, base_dados2$IPQ_Total_score, summary)

# Análise Completa
selecao1 = base_dados2 %>% 
  select(IPQ_Total_score, activation_score) %>%
  group_by(IPQ_Total_score) %>%
  get_summary_stats(activation_score)

# Criação de histogramas de uma variável
# base de dados 1
colnames(base_dados1)
hist(base_dados1$X20..MVC.PL)
hist(base_dados1$X60..MVC.PL)

# base de dados 2
colnames(base_dados2)
hist(base_dados2$Bodyweight)
hist(base_dados2$Age)

# Criação de gráfico de densidade de uma variável
ggdensity(base_dados1$X20..MVC.PL)

# Teste Shapiro Wilk
shapiro.test(base_dados2$Bodyweight)

# Skewness para variáveis quantitativas
skewness(base_dados1$X20..MVC.PL)

# Kurtosis para variáveis quantitativas
kurtosis(base_dados1$X20..MVC.PL)

# EXERCICIO
#Niveis do PAM
#nível 1 (47,0 pontos)
#nível 2 (47,1–55,1 pontos)
#nível 3 (55,2–67 pontos) e 
#nível 4 (67,1 pontos)

# variavel activation score
activation_score = base_dados2$activation_score

#---------------------- ANÁLISE PAM SCORE ---------------
# análise gráfica
hist(activation_score)
ggdensity(activation_score)

# análise normalidade
shapiro.test(activation_score)         # W = 0.6137 e p < 2.2e-16
skewness(activation_score)
kurtosis(activation_score)

# tipo de variavel 
class(base_dados2$activation_score)               # tipo numérico

# testes
t.test(base_dados2$activation_score,base_dados2$Disease)              # teste t não pareado
t.test(base_dados2$activation_score,base_dados2$Disease,paired=TRUE)  # teste t pareado

wilcox.test(base_dados2$activation_score,base_dados2$Disease)                                          # teste wilcox não pareado
wilcox.test(base_dados2$activation_score,base_dados2$Disease,paired=TRUE, alternative="two.sided")     # teste wilcox pareado

# fazendo uma selecao na base de dados 
# aqui vamos fazer a seleção da base de dados de acordo com a presença da doença (2=COPD e 4=CRD)

#modo 1
g2 = subset(base_dados2,base_dados2$Disease == 2 | base_dados2$Disease == 4)

#modo 2
selecao_doencas = filter(base_dados2,Disease==2 | Disease==4)

# fazendo o teste para esse conjunto de dados filtrados
t.test(selecao_doencas$activation_score,selecao_doencas$Disease)
