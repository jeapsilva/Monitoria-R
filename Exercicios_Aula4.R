# Bibliotecas
# ps: confira na sessão "PACKAGES" se todos as bibliotecas listadas abaixos estão selecionadas
library("stats")
library("dplyr")
library("ggpubr")
install.packages("car")
library("car")
install.packages("reshape2")
library("reshape2")
install.packages("rstatix")/
library("rstatix")
install.packages("ggplot2")
library("ggplot2")
library("ggpubr")



# leitura da base de dados
dados = read.csv('D:/Mestrado - Unb/Disciplinas/Metodos Quantitativos/Semana 4/Atividade 4/camp_teach.csv')

# visualizando a base de dados
View(dados)
colnames(dados)
summary(dados)


# PASSO 1 - REALIZAR O SUMARIO DAS VARIÁVEIS
# modo simples
by(dados$PREFEV, dados$TG, summary)

# modo completo
selecao_completa = dados %>% select(TG, PREFEV) %>% group_by(TG) %>%  get_summary_stats(PREFEV)

# ------------------- ANOVA DE UMA VIA
#criando um subset de variaveis para 12 meses apos inicio de tto
dados_m12 <- subset(dados,  dados$visitc == 12)

# criando a anova
ANOVA_1 = aov(dados_m12$PREFEV~dados_m12$TG) # aov(dados$variavel_dependente~dados$variavel_independente) 
summary(ANOVA_1)

# normalidade dos resíduos
hist(ANOVA_1$residuals)

# qqplot dos resíduos
qqPlot(ANOVA_1$residuals)
boxplot(dados_m12$PREFEV~dados_m12$TG)

# shapiro wilk
shapiro.test(ANOVA_1$residuals)

# ------------------- HOMOCEDASTICIDADE
#igualdade das variancias= homocedasticidade
ggplot(dados_m12, aes(x=dados_m12$TG, y=dados_m12$PREFEV)) +  geom_dotplot(binaxis='y', stackdir='center',Stackratio=1.5, dotsize=0.5)

# Levine nao significante = Homocedasticidade
as.factor(dados_m12$PREFEV)
leveneTest(y= dados_m12$PREFEV, group = dados_m12$TG)

# ANOVA de amostras repetidas
dados_tga = subset(dados, dados$TG=="A") # dcast(dados_G_A, id2~visitc3, value.var="variavel_dependente")	seleciona a identidade do paciente por variavel que define a visita em meses
View(dados_tga)

# Subset de variaveis até a nona coluna que representa a 40ª semana
dados_tga_40 <- subset(dados_tga, select = c(0 : 10))

# retirando valores nulos (NA) dessa base de dado
dados_tga_40B <- na.exclude(dados_tga_40)

# Retornando variavies para  o formato longo, pois para fazer a ANOVA de medidas repetidas é preciso estar no formato longo
as.factor(dados_tga_40B$id) 
dados_tga_40B_long <- melt(dados_tga_40B)

# fazendo a Anova de medidas repetidas. 
ANOVA_TGA_40B <- aov(value~variable+Error(variable), data = dados_tga_40B_long) 
#ANOVA_TGA_40B <- aov(value~variable+Error(id/variable), data = dados_tga_40B_long) < --------------------------- ATENÇÃO
summary(ANOVA_TGA_40B)

# testando a normalidade dos residuos entre os sujeitos 
hist(ANOVA_TGA_40B[["Within"]][["residuals"]]) # buscando o resultado dentro de Within e instanciando os residuos
qqPlot(ANOVA_TGA_40B[["Within"]][["residuals"]])
shapiro.test(ANOVA_TGA_40B[["Within"]][["residuals"]])

#testando esfericidade 
#ANOVA_TGA_40C <- anova_test(data= dados_tga_40B_long, dv= value, wid= id, within = variable)  < --------------------------- ATENÇÃO  

# ------------------- TESTE DE KRUSTAL WALLIS
# Ordenação das variáveis
as.ordered(dados$TG)

#boxplot por grupo 
ggboxplot(dados, x = "TG",  y = "PREFEV")

#Kruskal-Wallis  
Dados_KRUSKALL <- kruskal.test(dados$PREFEV~dados$TG) # kruskal.test(dados$variavel_dependente~dados$variavel_independente) 
# outro modo que podemos escrever:
Dados_KRUSKALL2 <- kruskal.test(PREFEV~TG, data= dados)


#Friedman test- equivalente ao ANOVA de Medidas repetidas 
#friedman_test(dados_tga_40B_long, value~variable | id) < --------------------------- ATENÇÃO

#Anova- de Welsh considerando heteroscedicidade 
PREFEV12m_Welch <- oneway.test(dados$PREFEV~dados$TG, var.equal = FALSE)
