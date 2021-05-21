rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\014")
library(BatchGetSymbols)
library(tidyverse)
library(stringr)
#library(quantmod)
#library(GetDFPData)
#library(GetDFPData2)
#
#library(ggthemes)
#library(reshape2)
#library(plyr)
#library(plotly)

#acao = 'BBDC3.sa' #Empresa.sa -> para analisar alguma empresa em espec.
DI = '2014-01-01' #Data de inicio
DF = Sys.Date() #Data de fim(hoje)
benchmark = '^BVSP' #índice da bolsa

tickersIbov = GetIbovStocks() #Retorna as ações negociadas do Brasil, dados completos.
tickersIbov$tickersSA = paste(tickersIbov$tickers,".SA",sep='') #Criar uma coluna e adicionar o .SA nos tickers

IBOVdatabase = BatchGetSymbols(
  tickers = tickersIbov$tickersSA, #Especificando as ações
  first.date = DI,
  last.date= DF,
  bench.ticker = benchmark)

#Pegando o segundo elemento da lista retornada, que e o que contem os dados.
IBOVdatabase = IBOVdatabase$df.tickers
#Selecao de colunas de interesse
IBOVdatabase <- IBOVdatabase %>% 
  select(-c(5,9,10))
#Estrutura do banco de dados
str(IBOVdatabase)
rm(tickersIbov)
#Lista com varios dataframes de acordo com as acoes presentes em IBOVdatabase
IBOVdatabase = dlply(IBOVdatabase,.(ticker),function(x){rownames(x)=x$row;x$row=NULL;x}) 
#Resumir o Banco de Dados

BancoDeDados_Acoes = IBOVdatabase[[1]][,c(6,5)] #Extrair as colunas 7 e 6 do dataframe 1
colnames(BancoDeDados_Acoes) = c("Data",paste(IBOVdatabase[[1]][1,7])) #Renomeando as colunas
#teste <- IBOVdatabase[[1]]
for(i in 2:length(IBOVdatabase)){
  itera_BancoDeDados_Acoes = IBOVdatabase[[i]][,c(6,5)] 
  colnames(itera_BancoDeDados_Acoes) = c("Data",paste(IBOVdatabase[[i]][1,7])) #Renomeando as colunas
  BancoDeDados_Acoes = merge(BancoDeDados_Acoes,itera_BancoDeDados_Acoes, by = "Data") #Juntando os dataframes usando a Data como coluna chave para fazer os joins.
}

#df_info = get_info_companies()
#names(df_info)

##Banco de Dados mais apurado com descrição e os tickers. 
df_emp <-  GetDFPData::gdfpd.get.info.companies()
df_emp <-  df_emp %>% select(c(1,11:14))
#Tirar os duplicados
df_emp <-  df_emp[!duplicated(df_emp), ]
#Tirar as empresas que nao tem um pregao
df_emp <- df_emp %>% filter(tickers!="")
names(df_emp) = c("Nome","Setor","Subsetor","Segmento","Tickers")
#Remover espacos entre as strings
df_emp$Nome    <- str_squish(df_emp$Nome    )
df_emp$Setor   <- str_squish(df_emp$Setor   )
df_emp$Subsetor<- str_squish(df_emp$Subsetor)
df_emp$Segmento<- str_squish(df_emp$Segmento)
df_emp$Tickers <- str_squish(df_emp$Tickers )
df_emp$Nome    <- str_trim(df_emp$Nome    )
df_emp$Setor   <- str_trim(df_emp$Setor   )
df_emp$Subsetor<- str_trim(df_emp$Subsetor)
df_emp$Segmento<- str_trim(df_emp$Segmento)
df_emp$Tickers <- str_trim(df_emp$Tickers )
#Colocar em caixa alta
df_emp$Nome    <- str_to_upper(df_emp$Nome    )
df_emp$Setor   <- str_to_upper(df_emp$Setor   )
df_emp$Subsetor<- str_to_upper(df_emp$Subsetor)
df_emp$Segmento<- str_to_upper(df_emp$Segmento)
df_emp$Tickers <- str_to_upper(df_emp$Tickers )
#Pegando todos os setores
#setores = subset(df_emp, select = c(2))
#setores = setores[!duplicated(setores),]
bimestre = 2
mes1 <- paste("0",as.character((bimestre*2)-1),sep = "")
mes2 <- paste("0",as.character(bimestre*2),sep="")
j = 1
dataIni = (strsplit(DI,"-")[[1]][1])+1
dataF = as.integer(strsplit(as.character(DF),"-")[[1]][1])
qtdeGraf <- dataF - dataIni #Quantidade de gráficos a serem gerados.

dataAtual <- dataIni
ploter <- list()
boxers <- list()
for (i in 1:qtdeGraf){
  final <- 1
  contador <- 0
  inicial <- 0
  for (j in 1:nrow(BancoDeDados_Acoes)){
    #Ano
    if (strsplit(as.character(BancoDeDados_Acoes$Data[j]),"-")[[1]][1] == dataAtual){
      #Bimestre que eu quero
      if(strsplit(as.character(BancoDeDados_Acoes$Data[j]),"-")[[1]][2]==mes1 || strsplit(as.character(BancoDeDados_Acoes$Data[j]),"-")[[1]][2]==mes2){
        final <- j
        print(BancoDeDados_Acoes$Data[j])
        contador <- contador + 1
      }
      inicial <- final-contador+1
    }
  }
  print(final)
  print(contador)
  print(inicial)
  
  #Plotar aqui
  ploter[[i]] <- BancoDeDados_Acoes[inicial:final,] %>%        
    select(Data,ABEV3.SA,BBAS3.SA) %>% 
    melt(id.var = "Data") %>% 
    ggplot(aes(Data,value))+geom_line(aes(colour = variable))
  dataAtual <- dataAtual+1
  boxers[[i]] = melt(BancoDeDados_Acoes[inicial:final,],id.vars = "Data", measure.vars = c("B3SA3.SA"))
  
  
}
ggplotly(ploter[[5]])
ggplot(boxers[[1]],aes(x=Data,y=value))+geom_boxplot()

# 
# #IMPLEMENTANDO FILTRO
# #Montar um dataframe com os dados de um setor específico.
# print(setores[[1]][9])
# setor = setores[[1]][9]    #Saúde
# Acoes_Filtradas = subset(df_emp,df_emp[2]==setor)
# Acoes_Filtradas_lista = Acoes_Filtradas$Tickers
# nlinhas = nrow(Acoes_Filtradas)
# numcol = ncol(BancoDeDados_Acoes)
# DataFrame_Final <- data.frame(Data=c(BancoDeDados_Acoes[1]))
# for(i in 1:nlinhas){
#   tickers = strsplit(Acoes_Filtradas_lista[i],";")
#   for (j in 1:length(tickers[[1]])){
#     for (k in 2:numcol){
#     if (paste(tickers[[1]][j],"SA",sep=".") == names(BancoDeDados_Acoes[k])){
#      DataFrame_Final[paste(tickers[[1]][j],"SA",sep=".")] = subset(BancoDeDados_Acoes,select = c(k))
#       print("Achei")
#     
#       }
#     }
#   }
#   
# }
# 
# df = melt(DataFrame_Final,id.vars = "Data", variable.name = "Ações")
# 
# ggplot(df,aes(Data,value)) + geom_line(aes(colour = Ações))
# ##Pipe: pega o que vem antes e joga na função posterior.
# library(plotly)
#  plot <- BancoDeDados_Acoes %>% 
#   select(Data,ABEV3.SA,AZUL4.SA,B3SA3.SA) %>% 
#   melt(id.var = "Data") %>% 
#   ggplot(aes(Data,value))+geom_line(aes(colour = variable))
# ggplotly(plot)
#  
# 
# 
# df.m <- melt(BancoDeDados_Acoes, id.var = "Data")
# ggplot(df.m,aes(variable,value))+geom_boxplot()
# 
