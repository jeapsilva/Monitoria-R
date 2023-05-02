# Bibliotecas
# ps: confira na sessão "PACKAGES" se todos as bibliotecas listadas abaixos estão selecionadas
library("stats")
library("dplyr")
library("ggpubr")
install.packages("car")
library("car")
install.packages("reshape2")
library("reshape2")
install.packages("rstatix")
library("rstatix")
install.packages("ggplot2")
library("ggplot2")
library("ggpubr")

# ------------------- LEITURA DA BASE DE DADOS

# leitura da base de dados
dados = read.csv('D:/Mestrado - Unb/Disciplinas/Metodos Quantitativos/Semana 4/Atividade 4/camp_teach.csv')

# visualizando a base de dados
View(dados)
colnames(dados)
summary(dados)

# tipos de dados
str(dados)

# PASSO 1 - REALIZAR O SUMARIO DAS VARIÁVEIS
# modo simples
dados_filtrados = by(dados$PREFEV, dados$TG, summary) #by(dados$variavel_dependente, dados$variavel_independente, summary)

# modo completo
selecao_completa = dados %>% select(TG, PREFEV) %>% group_by(TG) %>%  get_summary_stats(PREFEV)

# ------------------- ANOVA DE UMA VIA
# anova de uma via contem somente 1 variavel independente 
# criando um subset de variaveis para 12 meses apos inicio de tto
dados_s12 <- subset(dados,  dados$visitc == 12)

# criando a anova
ANOVA_1 = aov(dados_s12$PREFEV~dados_s12$TG) # aov(dados$variavel_dependente~dados$variavel_independente) 
summary(ANOVA_1)

# PRESUPOSTOS DA ANOVA DE UMA VIA (tem que ser uma distribuição normal)
# normalidade dos resíduos (y_pred - y)
hist(ANOVA_1$residuals)

# qqplot dos resíduos
qqPlot(ANOVA_1$residuals)
boxplot(dados_s12$PREFEV~dados_s12$TG,xlab="Classe",ylab="Resíduos")

# shapiro wilk
shapiro.test(ANOVA_1$residuals)

# ------------------- HOMOCEDASTICIDADE
#igualdade das variancias= homocedasticidade
ggplot(dados_s12, aes(x=dados_s12$TG, y=dados_s12$PREFEV)) +  geom_dotplot(binaxis='y', stackdir='center',Stackratio=1.5, dotsize=0.5)

# Levine nao significante = Homocedasticidade
as.factor(dados_s12$PREFEV)
leveneTest(y= dados_s12$PREFEV, group = dados_s12$TG) # isso trouxe o P(>f) = 0.5028

# ------------------- ANOVA DE AMOSTRAS REPETIDAS
# análise de variação de fatores entre grupos (placebo versus tratamento), logo o grupo A é referente ao grupo de budenosida
dados_tga = subset(dados, dados$TG=="A") # dcast(dados_G_A, id2~visitc3, value.var="variavel_dependente")	seleciona a identidade do paciente por variavel que define a visita em meses
View(dados_tga)

# condensando todos os id em uma só linha para ver os dados faltantes
dados_tga_largo <- dcast(dados_tga, id~visitc,value.var="PREFEV")

# Subset de variaveis até a nona coluna que representa a 40ª semana
dados_tga_40 <- subset(dados_tga_largo, select = c(0 : 10))

# retirando valores nulos (NA) dessa base de dado para as 40 semanas
dados_tga_40B <- na.exclude(dados_tga_40)

# Retornando variaveis para  o formato longo, pois para fazer a ANOVA de medidas repetidas é preciso estar no formato longo
as.factor(dados_tga_40B$id) 
dados_tga_40B_long <- melt(dados_tga_40B,id.vars = c(1))

# fazendo a Anova de medidas repetidas. 
ANOVA_TGA_40B <- aov(value~variable+Error(id/variable), data = dados_tga_40B_long) #como são valores correlacionados, o valor do paciente na semana 16 depende do valor da semana 0,1,2..
summary(ANOVA_TGA_40B)

# testando a normalidade dos residuos entre os sujeitos 
hist(ANOVA_TGA_40B[["Within"]][["residuals"]]) # buscando o resultado dentro de Within e instanciando os residuos
qqPlot(ANOVA_TGA_40B[["Within"]][["residuals"]])
shapiro.test(ANOVA_TGA_40B[["Within"]][["residuals"]])

# PRESUPOSTO DE ANOVA DE MEDIDAS REPETIDAS
#testando esfericidade 
ANOVA_TGA_40C <- anova_test(data= dados_tga_40B_long, dv= value, wid= id, within = variable)  

# ------------------- TESTE DE KRUSTAL WALLIS
# Ordenação das variáveis
as.ordered(dados$TG)

#Kruskal-Wallis  
Dados_KRUSKALL <- kruskal.test(dados$PREFEV~dados$TG) # kruskal.test(dados$variavel_dependente~dados$variavel_independente) 
# outro modo que podemos escrever:
Dados_KRUSKALL2 <- kruskal.test(PREFEV~TG, data= dados)


#Friedman test- equivalente ao ANOVA de Medidas repetidas 
#friedman_test(dados_tga_40B_long, value~variable | id) < --------------------------- ATENÇÃO

#Anova- de Welsh considerando heteroscedicidade 
PREFEV12m_Welch <- oneway.test(dados$PREFEV~dados$TG, var.equal = FALSE)
