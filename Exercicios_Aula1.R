# leitura da base de dados
base_dados = read.csv('D:\\Mestrado - Unb\\Disciplinas\\Metodos Quantitativos\\Semana 1\\Atividade 1\\Aula 1 [Base de dados] -  Muito baixo peso.csv')

# analise exploratoria da base
head(base_dados,5)     # imprimindo as 5 primeiras linhas da base de dados
tail(base_dados,5)     # imprimindo as 5 ultimas linhas da base de dados
colnames(base_dados)   # imprimindo os nomes das colunas da base de dados

# MEDIDAS DE TENDÊNCIA CENTRAL:

#------- Média e mediana -------------
print('A variável Menor.PH possui:')
summary(base_dados$Menor.PH)


#------- Amplitude interquartil -------------
IQR(base_dados$Menor.PH, na.rm=TRUE, type=7)

#------- Desvio Padrão -------------
sd(base_dados$Menor.PH, na.rm=TRUE)


#------- Histograma -------------
hist(base_dados$Menor.PH, xlim=c(6,8),breaks=500)


# ------- Instalando bibliotecas -------------

install.packages("graphics")
install.packages("stats")
install.packages("ggpubr")
install.packages("car")

# ------- Chamando essas bibliotecas para utilizar -------------
library("graphics")
library("stats")
library("ggpubr")
library("car")

# ------- Testes de normalidade ------------
shapiro.test(base_dados$Menor.PH)


# ------- Grafico de densidade e qq plot ------------
ggdensity(base_dados$Menor.PH,xlim=c(6.4,7.8),title='Grafico de densidade da variável Menor.PH',color='blue')
?ggdensity # pesquisando os detalhes dessa funcao


# ------- Skewness e Kurtosis ------------
install.packages("moments")
library("moments")
skewness(base_dados$Menor.PH,na.rm=TRUE)
kurtosis(base_dados$Menor.PH, na.rm= TRUE)










