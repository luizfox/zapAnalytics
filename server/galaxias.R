library(stringr)
require(lubridate)
table <-NA
carregarArquivo <- function (arquivo){
  table <<- readLines(arquivo, encoding="UTF-8")
  #table = iconv(table, from="UTF-8", to="latin1")

  #remover os was added
  table <<- table[!is.na( str_locate(table, ": ")[,1])]
}

datas <- NA
fn_datas <- function (){
  datas <<- mdy_hm(substr(table,1,19))
  table <- table[!is.na(datas)]
  datas <<- mdy_hm(substr(table,1,19))
  table <<- table
  #summary(datas)
}



#localizar o autor
local = final = autor = NA
localizaAutor <- function(){
  local <<- str_locate(table, ":")[,1]
  final <<- str_locate(table, ": ")[,1]
  autor <<- substr (table, local+6, final-1)
#  head(autor,20)
#  unique(autor)
#  table(autor)
}


# limite de 2000 aqui?
# RETIRADO
#hist(sample(datas[autor=="ennio vivo"],1000), breaks=12, col="lightblue", main="Pdg", xlab="Meses", 
#     ylab="Quantidade de mensagens")

dfTable = NA
buildDataFrame <- function (){
  dfTable <<- as.data.frame(table)
  dfTable$autor <<- autor
  dfTable$diaSemana <<- wday(datas, label=T)
  dfTable$hora <<- hour(datas)
  dfTable$month <<- month(datas)
  #head(dfTable)  
}


library(ggplot2)
library(sqldf)

dfTable_hour = dfTable_week = dfTable_month = dfTable_week_hour = NA
buildDataFrameSQL <- function (){
  dfTable_hour <<- sqldf("select count(*) as qtde, autor, hora from dfTable group by autor, hora")
  dfTable_week <<- sqldf("select count(*) as qtde, autor, diaSemana from dfTable group by autor, diaSemana")
  dfTable_month <<- sqldf("select count(*) as qtde, autor, month from dfTable group by autor, month")
  dfTable_week_hour <<- sqldf("select count(*) as qtde, hora, diaSemana from dfTable group by hora, diaSemana")
}
#t = as.data.frame.matrix(t, row.names = rownames(t), col.names=colnames(t))

grafico1 <- function (){
  if (is.na(autor)) {
    buildPreRequisites()
  }
  
  p = ggplot(dfTable_hour, aes(x = hora, y=qtde, group=autor) ) + 
    geom_line(aes(color=(dfTable_hour$autor) ), size=1.1 ) +
    geom_point(aes(color=(dfTable_hour$autor))) +
    scale_color_discrete(name = "User")+
    xlab("Hours of the day") + ylab ("Qty of messages")+
    ggtitle("Messages over hours of the day")
  print(p)
}

grafico2 <- function (){
  if (is.na(autor)) {
    buildPreRequisites()
  }
  
  p = ggplot(dfTable_week, aes(x = diaSemana, y=qtde, group=autor) ) + 
    geom_line(aes(color=(dfTable_week$autor) ), size=1.1 ) +
    geom_point(aes(color=(dfTable_week$autor))) +
    scale_color_discrete(name = "User")+
    xlab("Days of the Week") + ylab ("Qty of messages")+
    ggtitle("Messages over days of the week")
  print(p)
}

grafico3 <- function (){
  if (is.na(autor)) {
    buildPreRequisites()
  }
  
  p = ggplot(dfTable_month, aes(x = month, y=qtde, group=autor) ) + 
    geom_line(aes(color=(dfTable_month$autor) ), size=1.1 ) +
    geom_point(aes(color=(dfTable_month$autor))) +
    scale_color_discrete(name = "User")+
    xlab("Months of the year") + ylab ("Qty of messages")+
    ggtitle("Messages over months of the year")
  print(p)
}

grafico4 <- function (){
  if (is.na(autor)) {
    buildPreRequisites()
  }
  
  p = ggplot(dfTable_week_hour, aes(x = hora, y=diaSemana, fill=qtde) ) + 
    geom_raster(interpolate = F)+
    scale_fill_continuous(high = "#132B43", low = "#56B1F7", name="Density" )+
    xlab("Hours of the day") + ylab ("Day of the week")+
    ggtitle("Overall density messages hours x day")
  print(p)
}

#### checkpoint 1

horas = strHoras = posHoras = NA
additionalHours <- function (){
  horas <- hm(substr(table,14,19))
  strHoras <- paste0(hour(horas),":",minute(horas),":00")
  posHoras <- as.POSIXct(strHoras, format="%H:%M:%S")
# REMOVIDO
#  hist(posHoras, breaks=24, xlab="Horario", ylab = "Numero de msgs", 
#       main="Horario das msgs", col="lightblue")
}


#### checkpoint 2

#install.packages("rJava")
#install.packages("tm")
library(rJava)
library(SnowballC)
library(tm)

table2 = myCorpus = stopWord = tdm = termFrequency = NA

textMining <- function (){
  table2 <<- iconv(table, from="UTF-8", to="latin1")
  table2 <<- tolower(table2)
  table2 <<- str_replace_all(table2,"[^[:graph:]]", " ") 
  table2 <<- substring(table2, final+2)
  table2 <<- sample(table2, ifelse(length(table2) > 10000, 10000, length(table2)))
  myCorpus <<- Corpus(VectorSource(table2))
  myCorpus <<- tm_map(myCorpus, removePunctuation)
  myCorpus <<- tm_map(myCorpus, removeNumbers)
  stopWord <<- stopwords("portuguese")
  myCorpus <<- tm_map(myCorpus, removeWords, stopWord)
  
  tdm <<- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
  findFreqTerms(tdm, lowfreq=150)
  
  termFrequency <<- rowSums(as.matrix(tdm))
  termFrequency <<- subset(termFrequency, termFrequency>=250)
}



dfTermFrequency = NA
textMining2 <- function (){
  library(ggplot2)
  dfTermFrequency <<- data.frame(termFrequency)
  dfTermFrequency$term <<- rownames(dfTermFrequency)
  dfTermFrequency <<- dfTermFrequency[order(dfTermFrequency$termFrequency),]
}


#ggplot(dfTermFrequency, aes(term, termFrequency), xlab="Terms") +
#  geom_bar(stat="identity") +coord_flip()

plotFrequentWords <- function () {
  if (is.na(autor)) {
    buildPreRequisites()
  }
  
  barplot(sort(termFrequency), las = 2, 
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
}


findAssociatedWord <- function (word){
  if (is.na(autor)) {
    buildPreRequisites()
  }
  
  as.data.frame(findAssocs(tdm, word, 0.1))
}


########

## wordcloud skipped - for now
#install.packages("wordcloud")

#library(wordcloud)
#m <- as.matrix(tdm)
# calculate the frequency of words and sort it descendingly by frequency
#wordFreq <- sort(rowSums(m), decreasing=TRUE)
# word cloud
#set.seed(375) # to make it reproducible
#grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
#wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=100, random.order=F,
#          colors=grayLevels)


# cluster terms
#myTdm2 <- removeSparseTerms(tdm, sparse=0.95)
#m2 <- as.matrix(myTdm2)

#distMatrix <- dist(scale(m2))
#fit <- hclust(distMatrix, method="ward.D")
#plot(fit)

############

#m3 <- t(m2)
#k = 8
#library(fpc)
# partitioning around medoids with estimation of number of clusters
#pamResult <- pamk(m3, metric="manhattan")
# number of clusters identified
#(k <- pamResult$nc)
#pamResult <- pamResult$pamobject
# print cluster medoids
#for (i in 1:k) {
#  cat(paste("cluster", i, ":  "))
#  cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
  # print tweets in cluster i
  # print(rdmTweets[pamResult$clustering==i])
#}

#layout(matrix(c(1,2),2,1)) # set to two graphs per page 
#plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1,
#     col.p=pamResult$clustering)
#layout(matrix(1)) # change back to one graph per page 


#m3 <- t(m2)
#k <- 8
#kmeansResult <- kmeans(m3, k)
#round(kmeansResult$centers, digits=3)

#### checkpoint
#t = table ( table2[(autor != "You")], autor[(autor != "manel")])
#barplot(t)

local2 = final2 = autor2 = tamanho = NA
grafico5 <- function (){
  if (is.na(autor)) {
    buildPreRequisites()
  }
  
  table3 = table
  Encoding(table3)  <- "UTF-8"
  #table3 = iconv(table, from="UTF-8", to="ascii", sub=" ")
  #table3 = conversao(table)
  table3 = str_replace_all(table,"[^[:graph:]]", " ") 
  local2 = str_locate(table3, ":")[,1]
  final2 = str_locate(table3, ": ")[,1]
  autor2 = substr (table3, local+6, final-1)
  unique(autor2)
  tamanho = str_length(substr(table3, final2+2,1000))
  
  library(lattice)
  t = table (tamanho, autor2)
  p = barchart(tamanho~autor2, main="Size of the messages", ylab="Size")
  print (p)
  
}
conversao = function(x) {iconv(enc2utf8(x), sub = "byte")}
#xyplot(posHoras ~ datas, col=as.factor(autor), key=list(text=list(unique(autor))))
#plot(posHoras ~ datas, col=as.factor(autor), key=list(text=list(unique(autor)),
#              col=unique(autor)))

#write.csv(cbind(as.numeric(datas), as.numeric(posHoras), autor), 
#          "clusterConversas.csv", quote=F, row.names=F)

lonelyBoy <- function (){
  if (is.na(autor)) {
    buildPreRequisites()
  }
  
  dfArquivo = data.frame(momento = as.numeric(datas) , autor=autor)
  #names(dfArquivo) <- c("momento","autor")
  
  clusterConversa = 0;
  dfArquivo$cluster = NA;
  dfArquivo$cluster[1] = clusterConversa
  for (i in 2:nrow(dfArquivo)){
    if (dfArquivo$momento[i] - dfArquivo$momento[i-1] > 60*60*2 ){
      clusterConversa = clusterConversa + 1;
    }
    dfArquivo$cluster[i] = clusterConversa;
  }

  dfCluster = sort(table(dfArquivo$cluster))
  solitarios = as.numeric(rownames(dfCluster[dfCluster==1]))
  x =sort(table(dfArquivo[dfArquivo$cluster %in% solitarios,2]))
  as.data.frame(x)
  
  
}

### checkpoint

#ignored from here to the end
# hist(wday(datas), breaks=7)
# 
# df = data.frame (autor=autor, data=datas, hora=horas, msgs=gsub("\"", "'",table))
# head(df,20)
# write.csv(df, "galaxiasDF.csv", row.names = F, quote=T, na="")
# 
# table = read.csv ("geral por dia.csv")
# head(table)
# table = table[-37,]
# 
# table = rbind(table[37:268,], table[1:36,])
# head(table)
# tail(table)
# ## checkpoint???
# install.packages("RPostgreSQL")
# library(RPostgreSQL)
# drv <- dbDriver("PostgreSQL")
# con <- dbConnect(drv, user="postgres", dbname="galaxias", host="localhost", password="thebest")
# consulta = "SELECT EXTRACT(doy FROM data) AS diadoano, EXTRACT(dow FROM data) AS diadasemana, count(data) 
# FROM public.msgs m
# where EXTRACT(doy FROM data) <= 40'
# GROUP BY diadasemana, diadoano
# order by diadoano, diadasemana;"
# 
# consulta= 
# 
# montarConsulta <- function (dow, autor=NA){
#   paste (
#   "SELECT EXTRACT(doy FROM data) AS diadoano, EXTRACT(hour FROM data) AS horadodia, count(data)",
#   "FROM public.msgs m",
#   "where EXTRACT(doy FROM data) <= 40 and EXTRACT(dow FROM data) =", dow, 
#   ifelse(!is.na(autor), paste0(" and m.autor ='", autor,"'"), ""),
#   "GROUP BY diadoano, horadodia",
#   "order by diadoano, horadodia"
#   )
# }
# montarConsulta(1, "luiz")
# 
# retornarConsulta <- function (dia, autor){
#     rs <- dbSendQuery(con, montarConsulta(dia, autor))
#   resultado = fetch(rs, n=-1)  
#   resultado
# }
# retornarConsulta(1, "luiz")
# 
# retornarTS <- function (dia, autor){
#   resultado = retornarConsulta(dia, autor)
# #  resultado = retornarConsulta(5, "wesley")
# #  tablets <- ts(resultado$count, frequency=24)
# #  tablets
# #  f <- decompose(tablets)  
#   t = table(resultado$count, resultado$horadodia)
#   t
# }
# 
# emputarHorasDia <- function (resultado, dia) {
#   valoresDoDia = resultado[resultado$diadoano == dia,2] 
#   indicesComplementos = 0:23 %in% resultado[resultado$diadoano == dia,2] 
#   full = rep(0,24)
#   for (i in 1:24){
#     if (indicesComplementos[i] == T){
#       full[i] = resultado[(resultado$diadoano == dia) & (resultado$horadodia == i - 1),3]
#     }else{
#       full[i] = 0
#     }
#   }
#   full
# }
# 
# plotarTS <- function (horariosTS, dia){
#   f = decompose(horariosTS)
#   plot(f$figure, type="b", xaxt="n", xlab="Horario", ylab="Variacao", 
#        main=weekdays(Sys.Date()+3:9)[dia+1], ylim=c(-10,20))
#   plot(f)
#   #horasLabels <- 0:23
#   axis(1, at=1:24, labels=horasLabels, las=2) 
# }
# 
# processarDia <- function (dia, pessoa){
#   resultado = retornarConsulta(dia, pessoa)
#   horarioTodosDias = NA
#   for (i in unique (resultado$diadoano) ){
#     horarioTodosDias = c(horarioTodosDias, emputarHorasDia(resultado, i))
#   }
#   horarioTodosDias = horarioTodosDias[-1]
#   horariosTS <- ts(horarioTodosDias, frequency=24)
#   plotarTS(horariosTS, dia)
# }
# 
# par(mfrow = c(2,4))
# for (i in 0:6) processarDia(i,"jame")
# 
# resultado = retornarConsulta(1, "luiz")
# plot (resultado$horadodia)

buildPreRequisites <- function (folder, file){
  setwd(folder)
  carregarArquivo(file)
  fn_datas()
  localizaAutor()
  buildDataFrame()
  buildDataFrameSQL()
  
  additionalHours()
  textMining()
  textMining2()  

  print("prerequisites ok")  
}



chamaTudo <- function (){
  grafico1()
  grafico2()
  grafico3()
  grafico4()

  plotFrequentWords()
  findAssociatedWord("manel")
  grafico5()
  lonelyBoy()
}

#* Plot a histogram
#* @png
#* @get /plot
function(){
  rand <- rnorm(100)
  hist(rand)
}

#* Plot grafico1()
#* @png
#* @get /grafico1
function(){
  grafico1()
}

#* Plot grafico2()
#* @png
#* @get /grafico2
function(){
  grafico2()
}


#* Plot grafico3()
#* @png
#* @get /grafico3
function(){
  grafico3()
}

#* Plot grafico4()
#* @png
#* @get /grafico4
function(){
  grafico4()
}

#* Plot grafico5()
#* @png
#* @get /grafico5
function(){
  grafico5()
}

#* Plot plotFrequentWords()
#* @png
#* @get /plotFrequentWords
function(){
  plotFrequentWords()
}

#* get the lonely boys
#* @get /lonelyboys
function(){
  lonelyBoy()
}

#* @param term The term to find
#* @get /findassociatedword
function(term=""){
  findAssociatedWord(term)
}

#* @param folder the folder to switch to
#* @param file the file to load
#* @get /load
function(folder="~/datasets", file="galaxias.txt"){
  buildPreRequisites(folder, file)
}


#* @filter cors
cors <- function(req, res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    plumber::forward()
}


# 
#* @get /echo
function(){
  print("recebendo request!")
  # list(msg = paste0("The message is: '", msg, "'"))
  100+1
}

buildPreRequisites(folder="~/datasets", file="galaxias.txt")