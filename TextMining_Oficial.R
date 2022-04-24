
#####
#Carregando os Comentários em texto
#Versionado no GITHUB
install.packages("tibble")
install.packages(c("ggplot2","tidyverse","brazilmaps", "library(geobr)"))



library(tibble)
 glimpse(mpg)
library(brazilmaps) # pacote para o mapa do brasil 
library(tidyverse) # pacote para manipulção dos dados
library(ggplot2)
library(lattice)
library(geobr)


##1 ETAPA - RESULTADO PRELIMINARES DADOS etnográficos 
#VIÉS


#lendo csv
dados <- read.csv("https://raw.githubusercontent.com/TatianaFlorentino/trajetoriads/main/Datasets/brazil3.csv", sep=";", dec=",",header=TRUE)
head(dados)
dados$Dem_maritalstatus
head(dados)
tail(dados)
subset(dados,Dem_state == "RJ")
summary(dados)
#resumo das colunas
glimpse(dados)

#describe
describe(dados)

ident <- c("Mulheres", "Homens", "Outros")
barplot(table(dados$Dem_gender), ylab = "Participantes por gênero", names.arg = ident, col = c("pink", "lightblue", "orange"))


## 1. Variável Qualitativa Nominal

#contagem da frequencia de genero

gender.tb <- table(dados$Dem_gender)
cbind("f" = gender.tb)

cbind("f" = addmargins(gender.tb))

cbind("fr" = prop.table(gender.tb))

cbind("fr" = addmargins(prop.table(gender.tb)))

par(mfrow = c(1, 2))
barplot(gender.tb, ylab = "Frequência absoluta")
barplot(prop.table(gender.tb), ylab = "Frequência relativa",
        ylim = c(0, .6))
par(mfrow = c(1,1))

######grau de escolaridade

Dem_edu.tb <- table(dados$Dem_edu)
cbind("f" = addmargins(Dem_edu.tb))

cbind("f" = addmargins(Dem_edu.tb),
      "fr" = addmargins(prop.table(Dem_edu.tb)))

barplot(Dem_edu.tb)

par(mfrow = c(1,2))
## Menor para maior
barplot(sort(Dem_edu.tb))
## Maior para menor
barplot(sort(Dem_edu.tb, decreasing = TRUE))
par(mfrow = c(1,1))



######grau de estado civil


Dem_maritalstatus1 <- table(dados$Dem_maritalstatus)
prop.table(Dem_maritalstatus1)

#graficos estado civil

nomes2 = levels(dados$Dem_maritalstatus)
pielabels<-  c("Divorced/widowed","Married/cohabiting Other","or would rather not say","Single")
porcent2 = round(Dem_maritalstatus1/sum(Dem_maritalstatus1)*100,2)
rotulo2=paste(nomes2," (",pielabels,"%",")",sep="")
pie(table(Dem_maritalstatus1),labels=rotulo2, main="Estado Civil", col=c("#003154","#dd4a37"))

barplot(dados$Dem_dependents)


###PARICIPAÇÃO POR CIDADE

dados_RJ <- subset(dados, Dem_state == 'RJ')

Dem_state1 <- table(dados$Dem_state)
prop.table(Dem_state1)

# media de idade
median(dados$Dem_age)


 
# idades dos participantes        
Dem_age1 <- table(dados$Dem_age)
prop.table(Dem_age1)        
    
cbind("f" = addmargins(Dem_age1),       "fr" = addmargins(prop.table(Dem_age1)))

barplot(Dem_age1)




#renomear cidades
dados$Dem_state[dados$Dem_state == "Rio de Janeiro"] <- "Rio de Janeiro"
dados$Dem_state[dados$Dem_state == "RJ"] <- "Rio de Janeiro"
barplot(dados$Dem_state)






##2 ETAPA


#Separado os comentários do Brasil")


####### análise realizado pela função de obter sentimentos de syuzhet, com o dicionário NRC e os
#resultados obtidos sobre os quais trabalharemos.
#O sistema irá processar nosso texto e transformá-lo em um vetor de caracteres (aqui palavras), para analisá-los 
#individualmente (também é possível fazê-lo por sentenças). 






#############Perguntas################
## É possível observar com os comentários os valores e seus sentimentos?
# quais foram as palavras e termos em comum nessa pandemia ? 
# qual a percepção sentimentos sobre a pandemia 
# Os resultados desses   cálculo automático estão coerente com a realidade vivenciada em comum 
# quais são as palavras mais usadas na descrição das emoções com os comentários?


# Instalacao dos pacotes:
install.packages("syuzhet")
install.packages("RColorBrewer")
install.packages("wordcloud")
#O pacote mais popular para trabalharmos com texto no R se chama tm (“Text Mining”).
#Vamos carregá-lo e passar por algumas funções do pacote para,
#então, trabalharmos com uma nova classe de objeto: Corpus.
install.packages("tm")
install.packages("cluster")

# Carreguando os pacotes
library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(cluster)

#Com os comentários, a primeira coisa que vamos fazer é carregá-lo como um objeto de string
texto <- scan(file = "https://raw.githubusercontent.com/TatianaFlorentino/DeepLearning/master/comentario.txt", fileEncoding = "UTF-8", what = character(), sep = "\n", allowEscapes = T)

#####clean e organização dos textos###
#transformarmos todas as palavras em minúsculas
texto2 <- tolower(texto)
#retirando pontuação
texto2 <- removePunctuation(texto2)
#retirando numeros
texto2 <- removeNumbers(texto2)
stopwords("pt")
#stopwords” da língua portuguesa de nosso conjunto de textos: 
texto2 <- removeWords(texto2, stopwords("pt"))
texto2[1]

# ou usar uma funcao corpus


limpa_corpus <- function(corpus){
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("pt"))
  
  corpus
}


texto_3 <- VCorpus(VectorSource(texto))
texto_3 <- tm_map(texto_3,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
texto_3 <- tm_map(texto_3, content_transformer(tolower))
texto_3 <- tm_map(texto_3, removePunctuation)
texto_3 <- tm_map(texto_3,removeWords, stopwords("pt"))


##dividir o texto (string) em uma lista de palavras (tokens). Isto é muito comum na análise distante de textos.
#Para isso, usamos a função get_tokens() do pacote e geramos um novo objeto, neste caso um vetor de tokens (palavras).
texto_palavras <- get_tokens(texto_3)
head(texto_palavras)

#Agora podemos ver quantas palavras ou tokens estão neste texto com a função length():

length(texto_palavras)

#Se quiser realizar a análise para orações, utilize a função get_sentences() e siga o mesmo processo, com exceção da criação da nuvem de palavras:
# erro
oracoes_vetor <- get_sentences(texto_3)
#Agora podemos ver quantas sentecas ou tokens estão neste texto com a função length():
length(oracoes_vetor)

###NRC Sentiment Lexicon
### para obter os sentimentos dos comentários. Como a função executa por padrão o vocabulário inglês, 
# nós a escrevemos com o argumento “lang” (de language, ou idioma) para usar o vocabulário português (“portuguese”). 
sentimentos_df <- get_nrc_sentiment(texto_palavras, lang="portuguese")

# Mas para evitar “imprimir” milhares de linhas no console, também podemos
#usar a função head() para ver os primeiros seis tokens. 
head(sentimientos_df)


#resumo de cada um dos valores que obtivemos utilizando a função geral summary(). 
#Isto pode ser muito útil ao comparar vários textos, pois permite ver diferentes medidas, 
#tais como a média dos resultados para cada uma das emoções e os dois sentimentos. 
#Por exemplo, podemos ver que o romance Dom Casmurro é, em média (mean), 
#um pouco mais positivo (0,03892) do que negativo (0,03559). Mas se olharmos para as emoções,
#parece que a tristeza (0,02116) aparece em mais momentos do que a alegria (0,01593). 
#Vários dos valores fornecidos pela função de resumo do texto aparecem com um valor igual a 0, 
#incluindo a mediana (median). Isto indica que poucas palavras do romance aparecem no dicionário
#que estamos usando (NRC) ou, #inversamente, que poucas têm uma atribuição de sentimento ou emoção no dicionário.

summary(sentimentos_df)

#Para ver quais as emoções que estão mais presentes no texto, 
#a maneira mais simples é criar um barplot. Para isso, usamos a função barplot() 
#com o resumo das colunas 1 a 8, ou seja, as colunas de raiva (anger),
#antecipação (antecipation), desgosto (disgust), 
#medo (fear), alegria (joy),
#tristeza (sadness), 
#surpresa (surprise) 
#e confiança (trust). Os resultados obtidos vêm do processamento da função prop.table() 
# dos resultados das oito colunas com cada uma das palavras da tabela.

barplot(
  colSums(prop.table(sentimentos_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "'Comentários do Brasil sobre a pesquisa COVIDiSTRESS ",
  sub = "Análise realizada por Tatiana Florentino",
  xlab="emoções", ylab = NULL)



#Estas informações já indicam que as emoções de tristeza e medo e confiança prevalecem mais
#do que as de raiva, desgosto antecipação. Mas quais são as palavras usadas pelos brasileiros  expressão dessa tristeza? 
# Com que frequência cada uma aparece nos comentários como um todo?
barplot(colSums(prop.table(sentimentos_df[, 1:8])))

#A fim de realizar uma análise do texto, é muito interessante saber quais são as palavras usadas com mais frequência no texto 
#em relação à sua identificação com cada emoção.

##############################sadness########################
palavras_tristeza <- texto_palavras[sentimentos_df$sadness > 0]

##O conteúdo de palavras_tristeza nos indica que esta lista não diz muito, pois retorna apenas
##a listagem de palavras sem maiores informações. Para obter a contagem das vezes que cada palavra 
##relacionada à tristeza aparece no romance, geramos uma tabela do primeiro conjunto de caracteres 
##com as funções unlist e table, que depois ordenamos em ordem decrescente (se quisermos uma 
# ordem ascendente mudamos TRUE para FALSE); criamos um novo objeto de tipo tabela e imprimimos as primeiras
##12 palavras da lista com sua frequência:

palavras_tristeza_ordem <- sort(table(unlist(palavras_tristeza)), decreasing = TRUE)
head(palavras_tristeza_ordem, n = 12)
head(palavras_tristeza_ordem, n = 12)
#palavras unicas - Se quisermos saber quantas palavras únicas foram relacionadas à tristeza, basta usar a função length no objeto que agora agrupa as palavras em ordem:
# palavras associado a tristeza 44
length(palavras_tristeza_ordem)



##############################fear########################
#A fim de realizar uma análise do texto, é muito interessante saber quais são as palavras usadas com mais frequência no texto 
#em relação à sua identificação com cada emoção.
palavras_medo <- texto_palavras[sentimentos_df$fear > 0]

##O conteúdo de palavras_tristeza nos indica que esta lista não diz muito, pois retorna apenas
##a listagem de palavras sem maiores informações. Para obter a contagem das vezes que cada palavra 
##relacionada à tristeza aparece no romance, geramos uma tabela do primeiro conjunto de caracteres 
##com as funções unlist e table, que depois ordenamos em ordem decrescente (se quisermos uma 
# ordem ascendente mudamos TRUE para FALSE); criamos um novo objeto de tipo tabela e imprimimos as primeiras
##12 palavras da lista com sua frequência:

palavras_medo_ordem <- sort(table(unlist(palavras_medo)), decreasing = TRUE)
head(palavras_medo_ordem, n = 12)
head(palavras_medo_ordem, n = 12)
#palavras unicas - Se quisermos saber quantas palavras únicas foram relacionadas à tristeza, basta usar a função length no objeto que agora agrupa as palavras em ordem:
# palavras associado a tristeza 44
length(palavras_medo_ordem)


##############################trust ########################
#A fim de realizar uma análise do texto, é muito interessante saber quais são as palavras usadas com mais frequência no texto 
#em relação à sua identificação com cada emoção.
palavras_confianca <- texto_palavras[sentimentos_df$trust > 0]

##O conteúdo de palavras_tristeza nos indica que esta lista não diz muito, pois retorna apenas
##a listagem de palavras sem maiores informações. Para obter a contagem das vezes que cada palavra 
##relacionada à tristeza aparece no romance, geramos uma tabela do primeiro conjunto de caracteres 
##com as funções unlist e table, que depois ordenamos em ordem decrescente (se quisermos uma 
# ordem ascendente mudamos TRUE para FALSE); criamos um novo objeto de tipo tabela e imprimimos as primeiras
##12 palavras da lista com sua frequência:

palavras_confianca_ordem <- sort(table(unlist(palavras_confianca)), decreasing = TRUE)
head(palavras_confianca_ordem, n = 12)
head(palavras_confianca_ordem, n = 12)
#palavras unicas - Se quisermos saber quantas palavras únicas foram relacionadas à tristeza, basta usar a função length no objeto que agora agrupa as palavras em ordem:
# palavras associado a tristeza 44
length(palavras_confianca_ordem)



##############################anger ########################
#A fim de realizar uma análise do texto, é muito interessante saber quais são as palavras usadas com mais frequência no texto 
#em relação à sua identificação com cada emoção.
palavras_raiva <- texto_palavras[sentimentos_df$anger > 0]

##O conteúdo de palavras_tristeza nos indica que esta lista não diz muito, pois retorna apenas
##a listagem de palavras sem maiores informações. Para obter a contagem das vezes que cada palavra 
##relacionada à tristeza aparece no romance, geramos uma tabela do primeiro conjunto de caracteres 
##com as funções unlist e table, que depois ordenamos em ordem decrescente (se quisermos uma 
# ordem ascendente mudamos TRUE para FALSE); criamos um novo objeto de tipo tabela e imprimimos as primeiras
##12 palavras da lista com sua frequência:

palavras_raiva_ordem <- sort(table(unlist(palavras_raiva)), decreasing = TRUE)
head(palavras_raiva_ordem, n = 12)
head(palavras_raiva, n = 12)
#palavras unicas - Se quisermos saber quantas palavras únicas foram relacionadas à tristeza, basta usar a função length no objeto que agora agrupa as palavras em ordem:
# palavras associado a tristeza 44
length(palavras_raiva_orem)

##############################anticipation ########################
#A fim de realizar uma análise do texto, é muito interessante saber quais são as palavras usadas com mais frequência no texto 
#em relação à sua identificação com cada emoção.
palavras_antecipacao <- texto_palavras[sentimentos_df$anticipation > 0]

##O conteúdo de palavras_tristeza nos indica que esta lista não diz muito, pois retorna apenas
##a listagem de palavras sem maiores informações. Para obter a contagem das vezes que cada palavra 
##relacionada à tristeza aparece no romance, geramos uma tabela do primeiro conjunto de caracteres 
##com as funções unlist e table, que depois ordenamos em ordem decrescente (se quisermos uma 
# ordem ascendente mudamos TRUE para FALSE); criamos um novo objeto de tipo tabela e imprimimos as primeiras
##12 palavras da lista com sua frequência:

palavras_antecipacao_ordem <- sort(table(unlist(palavras_antecipacao)), decreasing = TRUE)
head(palavras_antecipacao_ordem, n = 12)
head(palavras_antecipacao, n = 12)
#palavras unicas - Se quisermos saber quantas palavras únicas foram relacionadas à tristeza, basta usar a função length no objeto que agora agrupa as palavras em ordem:
# palavras associado a tristeza 44
length(palavras_antecipacao_ordem)

#Uma vez gerado o vetor, deve convertê-lo em caracteres UTF-8 utilizando a função iconv.
nuvem_emocoes_vetor <- c(
  paste(texto_palavras[sentimentos_df$sadness> 0], collapse = " "),
  paste(texto_palavras[sentimentos_df$joy > 0], collapse = " "),
  paste(texto_palavras[sentimentos_df$anger > 0], collapse = " "),
  paste(texto_palavras[sentimentos_df$fear > 0], collapse = " "))
nuvem_emocoes_vetor <- iconv(nuvem_emocoes_vetor, "latin1", "UTF-8")
#Agora que temos o vetor, criamos um corpus de palavras com quatro “documentos” para a nuvem:
nuvem_corpus <- Corpus(VectorSource(nuvem_emocoes_vetor))

#Em seguida, transformamos este corpus em uma matriz termo-documento com a função 
#TermDocumentMatrix(). Com isto, agora usamos a função as.matrix() para converter o TDM
# em uma matriz que, como podemos ver, lista os termos no texto com um valor maior que zero
#para cada uma das quatro emoções que extraímos aqui. 
#Para ver o início desta informação, use novamente a função head:

nuvem_tdm <- TermDocumentMatrix(nuvem_corpus)
nuvem_tdm <- as.matrix(nuvem_tdm)
head(nuvem_tdm)
#Agora, atribua um nome a cada um dos grupos de palavras ou documentos (Docs) em nossa matriz.
#Aqui vamos usar o termo em português para as colunas que selecionamos para exibir na nuvem. Mais uma vez,
#podemos ver a mudança feita ao executar a função head.
colnames(nuvem_tdm) <- c('tristeza', 'felicidade', 'raiva', 'confiança')
head(nuvem_tdm)
# podemos perceber que a confianca e tristeza andou de maos dados 


set.seed(757) # pode ser qualquer número
comparison.cloud(nuvem_tdm, random.order = FALSE,
                 colors = c("red", "green", "orange", "blue"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)

####O que sugere o resultado desta nuvem? Ficamos impressionados com o aparecimento de 
##palavras como “entre” no conjunto da tristeza e “cavalo” no conjunto da raiva. Este “disparate”
##está relacionado com o aviso já anunciado no início da lição. O vocabulário para análise de 
##sentimentos que estamos usando aqui 
###é traduzido do inglês, um tradutor automático que não é “perfeito”.


#Para complementar a leitura isolada das emoções, estudando a flutuação dos sentimentos positivos e 
# negativos ao longo de um texto, há uma maneira de normalizar e visualizar estas informações. 
# Como a análise da função de extração de sentimento atribui um valor positivo tanto ao 
#sentimento positivo quanto ao negativo, precisamos gerar dados entre um intervalo de -1 
# para o momento mais negativo e 1 para o mais positivo, e onde 0 é neutro. 
#Para isso, calculamos a valência do texto multiplicando os valores na coluna de valores negativos de nosso data frame com os resultados por -1 e 
#adicionamos o valor na coluna de valores positivos.
sentimentos_valencia <- (sentimentos_df$negative * -1) + sentimentos_df$positive
simple_plot(sentimentos_valencia)


##Assim, neste caso, podemos interpretar que os comentários
## varia bastante entre momentos positivos e negativos. 
##Começa de forma mais negativa, fica mais positivo, sendo seguido por 
##um novo momento negativo e um segundo positivo (porém menos do que o primeiro)
##para um desfecho negativo. Qualquer pessoa que tenha vivenciado a pandemia na primeria onda pode confirmar 
##esta variação de sentimentos vivida em 2020.


#Aplicando um pouco de machine learning - Clustering
nuvem_tdm2 <- removeSparseTerms(nuvem_tdm, 0.95)
nuvem_tdm2

#Clustering 1 - Dendograma

distancia <- dist(t(nuvem_tdm), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-1,main = "Dendograma Comentarios",
     xlab = "Distancia",
     ylab = "Altura")  


#Para ler melhor o Dendograma
groups <- cutree(dendograma, k=2)
rect.hclust(dendograma, k=5, border="red")  


#Clustering 2 - K-Means
kmeans_btc <- kmeans(distancia, 2)   
clusplot(as.matrix(distancia), kmeans_btc$cluster, color=T, shade=T, labels=3, lines=0,
         main = "K-Means Comentarios",
         xlab = "PC1",
         ylab = "PC2") 

# Posso afirmar que tenho palavras correlatas aos termos de confianca, tristeza e raiva 

