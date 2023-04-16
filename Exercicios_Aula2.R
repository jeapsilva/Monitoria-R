
# leitura da base de dados
base_dados = read.csv('D:\\Mestrado - Unb\\Disciplinas\\Metodos Quantitativos\\Semana 2\\Atividade 2\\Aula 2 Desfecho continuo e preditores primarios.csv')

# análise exploratoria da base de dados
head(base_dados,5)
colnames(base_dados)

# ------- BIBLIOTECAS ----------------
# instalação dos pacotes
install.packages("gmodels")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rstatix")
install.packages("moments")

# importação das bibliotecas
library("gmodels")
library("ggplot2")
library("dplyr")
library("rstatix")
library("moments")

# ------- TIPOS DE VARIÁVEIS ----------------

# analisando o tipo de variavéis da base de dados
str(base_dados)

# vendo qual o tipo de uma variável em especifico 
class(base_dados$History.of.major.abdominal.pelvic.surgery)

# ------- FATORES E NÍVEIS DE VARIÁVEIS ----------------

# descobrindo quais são os fatores dentro dessa variavel
factor(base_dados$History.of.major.abdominal.pelvic.surgery)

# verificando se essa variável possui algum nível de ordenação
levels(base_dados$History.of.major.abdominal.pelvic.surgery)

# ------- CRIANDO FATORES E NÍVEIS ----------------
# aqui vou criar uma nova variavel baseada na History.of.major.abdominal.pelvic.surgery que vou chamar de HMAPS_LEVEL
# essa variável não estará presente na nossa base de dados, mas somente no script R
HMAPS_LEVEL = factor(base_dados$History.of.major.abdominal.pelvic.surgery, levels = c("Yes","No"))

# verificando o factor e levels dessa variavel
factor(HMAPS_LEVEL)
levels(HMAPS_LEVEL)

# aplicando o conceito para uma variavel do banco de dados
base_dados$History.of.major.abdominal.pelvic.surgery = factor(base_dados$History.of.major.abdominal.pelvic.surgery, levels = c("Yes","No"))

# verificando se inserirmos o factor e levels na variavel
factor(base_dados$History.of.major.abdominal.pelvic.surgery)
levels(base_dados$History.of.major.abdominal.pelvic.surgery)

# ------- AGRUPANDO VARIÁVEIS PARA GERAR ESTATÍSTICAS  ----------------
# nossa variavel independente é a "Study.Arm..control.versus.tranexamic.acid.group"
# nossa variavel dependente são todas as demais ou podemos selecionar apenas uma

# gerando o resumo 
resumo_unidade_sangue_por_grupo<- base_dados %>% 
  select(Study.Arm..control.versus.tranexamic.acid.group., Units.of.blood.transfused) %>%
  group_by(Study.Arm..control.versus.tranexamic.acid.group.) %>%
  get_summary_stats(Units.of.blood.transfused)

# visualizando a nova variável contendo o resumo: note que esse resumo irá gerar duas colunas das variáveis de tendência central, 
# uma para o grupo "CONTROL" e outro para o "TRANEXAMIC ACID"
# OBS: Esse resumo também aparece na aba ENVIROMENT(Lado direito do R)
resumo_unidade_sangue_por_grupo    

# ------- DISTRIBUIÇÃO E NORMALIDADE DAS VARIÁVEIS  ----------------
# vendo o tipo de variaveis da base de dados
str(base_dados)

# histograma para variáveis qualitativas
hist(base_dados$BMI)

# grafico de densidade para variáveis qualitativas
ggdensity(base_dados$BMI)

# teste de normalidade de Shapiro Wilk
library("momento")
shapiro.test(base_dados$BMI)

# avaliação de Skewness e Kurtosis
skewness(base_dados$BMI)

# avaliação da kurtosis
kurtosis(base_dados$BMI)

# ------- DICOTOMIZANDO UMA VARIÁVEL  ----------------
# vendo o tipo de variaveis
str(base_dados)

# criando o limiar para tudo até igual 36.0 irá receber SIM e acima dessa valor usaremos NÃO.
# A dictomização cria um valor limite e transforma uma variável numérica em uma variável binária (qualitativa nominal)
# Vale lembrar que essa variável poderá também ser visualizada na aba environment.
BMI_dictomizada <- ifelse(base_dados$BMI >= 36, "Sim", "Não")

# visualizando
BMI_dictomizada

# ------- TESTE EXATO DE FISHER  ----------------
# aplicando o teste a duas variáveis para variáveis independentes
CrossTable(base_dados$Study.Arm..control.versus.tranexamic.acid.group., base_dados$Length.of.hospital.stay..days., fisher=TRUE)

# ------- TESTE CHI QUADRADO  ----------------
# teste de associação/dependencia entre duas variáveis categóricas nominais
str(base_dados)
CrossTable(base_dados$Study.Arm..control.versus.tranexamic.acid.group., base_dados$History.of.major.abdominal.pelvic.surgery   , expected=TRUE, format=c("SPSS"))

# ------- TESTE T NÃO PAREADO  ----------------
t.test(base_dados$Units.of.blood.transfused, base_dados$Estimated.blood.loss..ml.)

# ------- TESTE T PAREADO  ----------------
t.test(base_dados$Units.of.blood.transfused, base_dados$Estimated.blood.loss..ml.,paired=TRUE)

# ------- TESTE WILCOXON  ----------------
wilcox.test(base_dados$Units.of.blood.transfused, base_dados$Length.of.hospital.stay..days.)

