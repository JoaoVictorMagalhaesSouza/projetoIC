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
  tickers = ibov$tickersSA, #Especificando as ações
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

#df_info = get_info_companies()
#names(df_info)

##Banco de Dados mais apurado com descrição e os tickers. 
df = gdfpd.get.info.companies()
df_reduzido = subset(df,select = c(1,11,12,13,14))
df_reduzido = df_reduzido[!duplicated(df_reduzido), ] #Tirar os duplicados
df_reduzido = subset(df_reduzido,df_reduzido[5] != "") #Tirar as empresas que não tem um pregão
names(df_reduzido) = c("Nome da Empresa","Setor","Subsetor","Segmento","Tickers")
ABC
#Uma empresa com mais de um pregão a pesquisa não funciona.
pesquisa = subset(df_reduzido,df_reduzido[5]=="MYPK3")
#df_reduce = subset(df_info,df_info[11]=="ATIVO")
#df_reduce = subset(df_reduce, df_reduce[43]=="BOLSA")
#df_reduce = subset(df_reduce, df_reduce[15]=="Categoria A")
#t1 = gdfpd.get.bovespa.data(1023)
#https://www.msperlin.com/shiny/GetDFPData/
