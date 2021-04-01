library(BatchGetSymbols)
library(quantmod)
library(GetDFPData)
library(GetDFPData2)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(plyr)

#acao = 'BBDC3.sa' #Empresa.sa -> para analisar alguma empresa em espec.
DI = '2016-01-01' #Data de inicio
DF = Sys.Date() #Data de fim(hoje)
benchmark = '^BVSP' #índice da bolsa

ibov = GetIbovStocks() #Retorna as ações mais negociadas do Brasil, dados completos.
ibov$tickersSA = paste(ibov$tickers,".SA",sep='') #Criar uma coluna e adicionar o .SA nos tickers

IBOVdatabase = BatchGetSymbols(
  tickers = ibov$tickersSA,
  first.date = DI,
  last.date= DF,
  bench.ticker = benchmark,
  
)
IBOVdatabase = IBOVdatabase$df.tickers #Pegando o segundo elemento da lista retornada, que é o que contém os dados.
IBOVdatabase = dlply(IBOVdatabase,.(ticker),function(x){rownames(x)=x$row;x$row=NULL;x}) #Lista com vários dataframes
#Resumir o Banco de Dados

databaseResumido = IBOVdatabase[[1]][,c(7,6)] #Extrair as colunas 7 e 6 do dataframe 1
colnames(databaseResumido) = c("Data",paste("Preço",IBOVdatabase[[1]][1,8])) #Renomeando as colunas

for(i in 2:length(IBOVdatabase)){
  itera_databaseResumido = IBOVdatabase[[i]][,c(7,6)] 
  colnames(itera_databaseResumido) = c("Data",paste("Preços",IBOVdatabase[[i]][1,8])) #Renomeando as colunas
  databaseResumido = merge(databaseResumido,itera_databaseResumido, by = "Data") #Juntando os dataframes usando a Data como coluna chave para fazer os joins.
}
##
df_info = get_info_companies()
names(df_info)

df_reduce = subset(df_info,df_info[11]=="ATIVO")
df_reduce = subset(df_reduce, df_reduce[43]=="BOLSA")
comm
#df_reduce = subset(df_reduce, df_reduce[15]=="Categoria A")

#https://www.msperlin.com/shiny/GetDFPData/
