# Bibliotecas
# ps: confira na sessão "PACKAGES" se todos as bibliotecas listadas abaixos estão selecionadas

install.packages("car")
install.packages("reshape2")
install.packages("rstatix")
install.packages("ggplot2")

library("ggpubr")
library("haven")
library("mice")
library("tidyverse")
library("gmodels")
library("ggplot2")
library("dplyr")

dados <- read_stata("D:/Mestrado - Unb/Disciplinas/Metodos Quantitativos/Semana 6/Exercicios/odin_trial_brief.dta")
View(dados)               # visualizando todos
summary(dados)            # estatisticas descritivas
summary(dados$typeint)    # estatisticas descritivas de uma variavel

# tipos de dados
str(dados)

# codificando base de dados do Stata
dados$typeint = as.factor(dados$typeint)
str(dados$typeint)

# visualizando uma variavel que contem dados faltantes
dados$typeint

# gerando um novo banco de dados retirando essa variavel faltante
new_dados = dados%>% drop_na(typeint)
View(new_dados)

# resumo de uma variavel por grupo
by(new_dados$typeint,new_dados$centre, summary, na.rm=TRUE)    # modo 1
new_dados %>%                                                  # modo 2
  group_by(new_dados$centre)%>%
  get_summary_stats(new_dados$typeint, type='full')          

# criando um subset
subset1 = subset(dados, select=c(1,2))    # select = c(numero_coluna1,numero_coluna2)

# criando uma base de addos sem valores faltantes
subset1_sem_missing = na.exclude(subset1)
dados_sem_missing = na.exclude(dados)

# plotando um grafico
ggplot(dados_sem_missing, aes(x=sessions,y=typeint))+geom_dotplot(binaxis='y',stackdir='center',binwidth=1,stackratio=1.5,dotsize=0.1)

# teste t de uma variável
t.test(dados_sem_missing$bdi0,dados_sem_missing$complian)


