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

# ---------------- EXERCICIOS
library(haven)
ODIN = read_stata("D:/Mestrado - Unb/Disciplinas/Metodos Quantitativos/Semana 6/Exercicios/odin_trial_brief.dta")
View(ODIN)

# limpando valores nulos
NEW_ODIN = ODIN %>% drop_na(typeint)

#1a 
by(NEW_ODIN$bdi6, NEW_ODIN$typeint, summary, na.rm= TRUE)
NEW_ODIN %>% 
  group_by(typeint) %>% 
  get_summary_stats(bdi6, type= "full")

#1b 
ODIN_int_bdi6 <- subset(NEW_ODIN, select=c(5, 11))
ODIN_int_bdi6_comp <- na.exclude(ODIN_int_bdi6)
typeint2 <- as.character(ODIN_int_bdi6_comp$typeint)
ggplot(ODIN_int_bdi6_comp, aes(x=typeint2, y=bdi6)) + 
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 2,
               stackratio=1.5, dotsize=0.5)
#1c
t.test(ODIN_int_bdi6_comp$bdi6~typeint2)

#2a-
ODIN_M <- NEW_ODIN
ODIN_M$bdi6[is.na(ODIN_M$bdi6)] <- mean(ODIN_M$bdi6, na.rm=TRUE)
by(ODIN_M$bdi6, ODIN_M$typeint, summary, na.rm= TRUE)
ODIN_M %>% 
  group_by(typeint) %>% 
  get_summary_stats(bdi6, type= "full")
ODIN %>% 
  group_by(typeint) %>% 
  get_summary_stats(bdi6, type= "full")

#2b
ODIN_M_int_bdi6 <- subset(ODIN_M, select=c(5, 11))
ODIN_M_int_bdi6_comp <- na.exclude(ODIN_M_int_bdi6)
typeint2_M <- as.character(ODIN_M_int_bdi6_comp$typeint)
ggplot(ODIN_M_int_bdi6_comp, aes(x=typeint2_M, y=bdi6)) + 
  geom_dotplot(binaxis='y', stackdir='center',
               stackratio=1.5, dotsize=0.5, binwidth = 1)
#2c 
t.test(ODIN_M_int_bdi6_comp$bdi6~typeint2_M)

#3a 
summary(NEW_ODIN)
#criando um subset de variaveis utilizadas na regressao
ODIN_R <- subset(NEW_ODIN, select=c(2, 3, 5, 11))
#cofidicando variaveis como qualitativas 
centre_ODIN_R <- as.character(ODIN_R$centre)
typeint_ODIN_R <- as.character(ODIN_R$typeint)
#Regredindo a variavel bdi6 a patir de bdi0, centro e tipo de intervens?o 
Reg_ODIN_R <- lm(bdi6~bdi0+centre_ODIN_R+typeint_ODIN_R, data=ODIN_R)
Pred_bdi6 <- predict(Reg_ODIN_R, newdata = ODIN_R)
summary(Pred_bdi6)
ODIN_R$bdi6[is.na(ODIN_R$bdi6)] <- predict(Reg_ODIN_R, newdata = ODIN_R)
ODIN_R %>% 
  group_by(typeint) %>%
  get_summary_stats(bdi6, type="full")
ODIN_RC_bdi6 <- na.exclude(ODIN_R)

#3b 
summary(ODIN_R)
summary(ODIN)
NEW_ODIN %>% 
  get_summary_stats(bdi6, type="full")
ODIN_R %>%
  get_summary_stats(bdi6, type="full")
ODIN_R_typeint <- as.character(ODIN_R$typeint)
ggplot(ODIN_R, aes(x=ODIN_R_typeint, y=bdi6)) + 
  geom_dotplot(binaxis='y', stackdir='center',
               stackratio=1.5, dotsize=0.5, binwidth = 1)

#3c 
t.test(ODIN_RC_bdi6$bdi6~ODIN_RC_bdi6$typeint)

