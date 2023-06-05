# Relatorio mundial da Felicidade - World happiness report

# Pacotes 

install.packages('dplyr')
install.packages('ggplot2')

# Carregando pacotes
library(dplyr)
library(ggplot2)

# Data load
df <- read.csv('dataset.csv')
str(df)
View(df)
dim(df)
summary(df)
# Verificar se há dados faltantes NA
is.na(df)
# Verificando a quantidade de dados faltantes em cada coluna
colSums(is.na(df))

# Exploratory data analysis - data cleaning

# Casos completos(Linhas com dados completos)
complete_cases <- sum(complete.cases(df))
complete_cases

# Incomplete Cases
not_complete_cases <- sum(!complete.cases(df))
not_complete_cases

# Percentage of incomplete cases
percentual <- (not_complete_cases / complete_cases) * 100
percentual

# Remover objetos anteriores para liberar memoria RAM
rm(complete_cases)
rm(not_complete_cases)

# Column names
colnames(df)

# Gravar nome das colunas em um vetor
myColumns <- colnames(df)
myColumns

# Alterar nome das colunas
myColumns[1] <- "NomePais"
myColumns[2] <- "Ano"
myColumns[3] <- "IndNivelVida"
myColumns[4] <- "PIB_Per_Capita"
myColumns[5] <- "SuporteSocial"
myColumns[6] <- "ExpectativaVida"
myColumns[7] <- "IndLiberdade"
myColumns[8] <- "IndGenerosidade"
myColumns[9] <- "IndCorrupcao"
myColumns[10] <- "IndEmocoesPositiva"
myColumns[11] <- "IndEmocoesNegativa"

# Verificando colunas
myColumns

# Atribuindo novos nomes das colunas no dataframe
colnames(df) <- myColumns
# Removendo o objeto
rm(myColumns)
# Visualizando dados
View(df)

# Verificando a quantidade de paises que foram incluidos na coleta de dados
length(unique(df$NomePais))

# Listando os paises unicos e gravando o resultado(antes de remover registros com valores NA)
list_countries_with_na <- unique(df$NomePais)
list_countries_with_na

# Removendo as linhas com valores ausentes (NA)
df <- na.omit(df)
dim(df)

# Listando os paises apos remover valores ausentes(NA)
list_countries_without_na <- unique(df$NomePais)
list_countries_without_na

# Verificando se perdemos paises ao remover valores NA
length(list_countries_with_na)
length(list_countries_without_na)

# Verificando a diferença antes e depois de remover valores NA
setdiff(list_countries_with_na, list_countries_without_na)

# Remove objetos
rm(list_countries_with_na)
rm(list_countries_without_na)

# Verificando quais os anos presentes nos dados
anos <- unique(df$Ano)

# Verificando menor e maior ano 
range(anos)

# Numero total de anos
length(unique(df$Ano))

# Removendo objeto
rm(anos)

# Qual o numero de registro por ano
table(df$Ano)

# Removendo os anos com menor contribuição (menor volume de dados)
dados_por_anos <- df[df$Ano!=2005 & df$Ano!=2006 & df$Ano!=2007 & df$Ano!=2020,]

# Numero de registros por ano
table(dados_por_anos$Ano)

# Extraindo variaveis numericas
numeric_variable_list <- sapply(df, is.numeric)
numerical_data <- df[numeric_variable_list]

# Criando matriz de correlação
cor(numerical_data)

# Correlation Plot
pairs(numerical_data)
# Filtrando analisando as cinco primeiras
pairs(numerical_data[1:5],labels = colnames(numerical_data)[1:5])
# Filtrando analisando as cinco ultimas
pairs(numerical_data[6:10], labels = colnames(numerical_data)[6:10])

##### Análise Exploratória dos Dados - Resposta às Perguntas de Negócio ##### 

##### Parte 1 - Organização dos Dados ##### 

# Vamos realizar a análise considerando a média de indicadores por país.
# Calculamos as médias fazendo agrupamento por indicador e concatenamos os dataframes resultantes.

# Visualiza os dados
View(df)

# Nomes das colunas
colnames(df)

# Agrupando os dados e calculando média por país
pib_per_capita_pais_media <- df %>%
  group_by(NomePais) %>%
  summarize(PIB_Per_Capita = mean(PIB_Per_Capita))

View(pib_per_capita_pais_media)

# Agrupando os dados e calculando média por país
suporte_social_pais_media <- df %>%
  group_by(NomePais) %>%
  summarize(SuporteSocial = mean(SuporteSocial))

# Merge dos dataframes
df_medias <- merge(pib_per_capita_pais_media, suporte_social_pais_media)
View(df_medias)

# Removendo objetos não estiver usando
rm(pib_per_capita_pais_media)
rm(suporte_social_pais_media)

# Agrupando os dados e calculando media por pais
ind_nivel_vida_pais_media <- df %>%
  group_by(NomePais) %>%
  summarize(IndNivelVida = mean(IndNivelVida))

# Merge
df_medias <- merge(df_medias, ind_nivel_vida_pais_media)
View(df_medias)
rm(ind_nivel_vida_pais_media)

# Agrupando os dados e calculando média por país
expectativa_vida_pais_media <- df %>%
  group_by(NomePais) %>%
  summarize(ExpectativaVida = mean(ExpectativaVida))

# Merge
df_medias <- merge(df_medias, expectativa_vida_pais_media)
View(df_medias)
rm(expectativa_vida_pais_media)

# Agrupando os dados e calculando média por país
ind_liberdade_pais_media <- df %>%
  group_by(NomePais) %>%
  summarize(IndLiberdade = mean(IndLiberdade))

df_medias <- merge(df_medias, ind_liberdade_pais_media)
View(df_medias)
rm(ind_liberdade_pais_media)

# Agrupando os dados e calculando média por país
ind_generosidade_pais_media <- df %>%
  group_by(NomePais) %>%
  summarize(IndGenerosidade = mean(IndGenerosidade))

# Merge
df_medias <- merge(df_medias, ind_generosidade_pais_media)
View(df_medias)
rm(ind_generosidade_pais_media)


# Agrupando os dados e calculando média por país
ind_corrupcao_pais_media <- df %>%
  group_by(NomePais) %>%
  summarize(IndCorrupcao = mean(IndCorrupcao))

# Merge
df_medias <- merge(df_medias, ind_corrupcao_pais_media)
View(df_medias)
rm(ind_corrupcao_pais_media)

# Agrupando os dados e calculando média por país
ind_pos_pais_media <- df %>%
  group_by(NomePais) %>%
  summarize(IndEmocoesPositiva = mean(IndEmocoesPositiva))

# Merge
df_medias <- merge(df_medias, ind_pos_pais_media)
View(df_medias)
rm(ind_pos_pais_media)

# Agrupando os dados e calculando média por país
ind_neg_pais_media <- df %>%
  group_by(NomePais) %>%
  summarize(IndEmocoesNegativa = mean(IndEmocoesNegativa))

# Merge
df_medias <- merge(df_medias, ind_neg_pais_media)
View(df_medias)
rm(ind_neg_pais_media)
dim(df_medias)

##### Parte 2 - Plots e Estatísticas ##### 

# Dados
colnames(df_medias)
View(df_medias)
str(df_medias)

# Pergunta 1
# O aumento do PIB per capita de um país afeta positivamente a expectativa de vida dos cidadãos ao nascer?R Sim
# Qual a correlação entre essas duas variáveis?
plot(df_medias$PIB_Per_Capita, df_medias$ExpectativaVida)
cor.test(df_medias$PIB_Per_Capita, df_medias$ExpectativaVida, method = "pearson")

# Pergunta 2
# Existe uma correlação entre a escala de vida e a conscientização do público em geral sobre a corrupção 
# nos negócios e no governo? R sim há uma relação (há uma relação negativa aumenta uma variavel diminui a outra entao a resposta é sim)
# Qual a correlação entre essas duas variáveis?
plot(df_medias$IndNivelVida, df_medias$IndCorrupcao)
cor.test(df_medias$IndNivelVida, df_medias$IndCorrupcao, method = "pearson")

# Pergunta 3
# O aumento na escala de vida tem algum efeito na média de felicidade entre o público em geral?
# Qual a correlação entre essas duas variáveis?Correlação positiva
plot(df_medias$IndNivelVida, df_medias$IndEmocoesPositiva)
cor.test(df_medias$IndNivelVida, df_medias$IndEmocoesPositiva, method = "pearson")

# Pergunta 4
# O país com o menor índice de suporte social tem maior percepção de corrupção em relação 
# às empresas e ao governo no país?Sim correlação negativa, o pais com menor suporte social tem maior percepção de corrupção

# Indicadores
df_medias[df_medias$SuporteSocial == min(df_medias$SuporteSocial),]
df1 <- df_medias[df_medias$NomePais == "Central African Republic",]
View(df1)
df1$SuporteSocial
df1$IndCorrupcao
max(df_medias$SuporteSocial)
max(df_medias$IndCorrupcao)

# Plot e Estatísticas
df2 <- df[df$NomePais == "Central African Republic",]
View(df2)
plot(df2$SuporteSocial, df2$IndEmocoesPositiva)
cor.test(df2$SuporteSocial, df2$IndEmocoesPositiva, method = "pearson")

# Pergunta 5
# Pessoas generosas são mais felizes? Sim correlação positiva 
plot(df_medias$IndGenerosidade, df_medias$IndEmocoesPositiva)
cor.test(df_medias$IndGenerosidade, df_medias$IndEmocoesPositiva, method = "pearson")



