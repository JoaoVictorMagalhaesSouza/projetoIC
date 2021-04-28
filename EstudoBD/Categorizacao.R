library(BatchGetSymbols)
library(quantmod)
library(GetDFPData)
library(GetDFPData2)
library(tidyverse)
library(ggthemes)
library(reshape2)


#acao = 'BBDC3.sa' #Empresa.sa -> para analisar alguma empresa em espec.
DI = '2016-01-01' #Data de inicio
DF = Sys.Date() #Data de fim(hoje)
benchmark = '^BVSP' #índice da bolsa

tickersIbov = GetIbovStocks() #Retorna as ações mais negociadas do Brasil, dados completos.
tickersIbov$tickersSA = paste(tickersIbov$tickers,".SA",sep='') #Criar uma coluna e adicionar o .SA nos tickers

IBOVdatabase = BatchGetSymbols(
  tickers = tickersIbov$tickersSA, #Especificando as ações
  first.date = DI,
  last.date= DF,
  bench.ticker = benchmark,
  
)
IBOVdatabase = IBOVdatabase$df.tickers #Pegando o segundo elemento da lista retornada, que é o que contém os dados.
IBOVdatabase = dlply(IBOVdatabase,.(ticker),function(x){rownames(x)=x$row;x$row=NULL;x}) #Lista com vários dataframes
#Resumir o Banco de Dados

BancoDeDados_Acoes = IBOVdatabase[[1]][,c(7,6)] #Extrair as colunas 7 e 6 do dataframe 1
colnames(BancoDeDados_Acoes) = c("Data",paste(IBOVdatabase[[1]][1,8])) #Renomeando as colunas

for(i in 2:length(IBOVdatabase)){
  itera_BancoDeDados_Acoes = IBOVdatabase[[i]][,c(7,6)] 
  colnames(itera_BancoDeDados_Acoes) = c("Data",paste(IBOVdatabase[[i]][1,8])) #Renomeando as colunas
  BancoDeDados_Acoes = merge(BancoDeDados_Acoes,itera_BancoDeDados_Acoes, by = "Data") #Juntando os dataframes usando a Data como coluna chave para fazer os joins.
}

#df_info = get_info_companies()
#names(df_info)

##Banco de Dados mais apurado com descrição e os tickers. 
bd = gdfpd.get.info.companies()
DataFrame_Empresas = subset(bd,select = c(1,11,12,13,14))
DataFrame_Empresas = DataFrame_Empresas[!duplicated(DataFrame_Empresas), ] #Tirar os duplicados
DataFrame_Empresas = subset(DataFrame_Empresas,DataFrame_Empresas[5] != "") #Tirar as empresas que não tem um pregão
names(DataFrame_Empresas) = c("Nome da Empresa","Setor","Subsetor","Segmento","Tickers")

#Pegando todos os setores
setores = subset(DataFrame_Empresas, select = c(2))
setores = setores[!duplicated(setores),]
#TESTANDO FILTRO

#tick = DataFrame_Empresas$Tickers
#print(tick[1])
#v = strsplit(tick[1],";")
#print(length(v[[1]]))
#print(v[[1]][3])

#for (i in 1:length(tick)){
#  v = strsplit(tick[i],";")
#  for (j in 1:length(v[[1]])){
#      if (v[[1]][j] == "MAPT3"){
#        posicao = i
 #       numElemento = j
  #    }
   
 # }
  
#}
#v = strsplit(tick[posicao],";")
#print(v[[1]][numElemento])

#IMPLEMENTANDO FILTRO
#Montar um dataframe com os dados de um setor específico.
print(setores[[1]][9])
setor = setores[[1]][9]    #Saúde
Acoes_Filtradas = subset(DataFrame_Empresas,DataFrame_Empresas[2]==setor)
Acoes_Filtradas_lista = Acoes_Filtradas$Tickers
nlinhas = nrow(Acoes_Filtradas)
numcol = ncol(BancoDeDados_Acoes)
DataFrame_Final <- data.frame(Data=c(BancoDeDados_Acoes[1]))
for(i in 1:nlinhas){
  tickers = strsplit(Acoes_Filtradas_lista[i],";")
  for (j in 1:length(tickers[[1]])){
    for (k in 2:numcol){
    if (paste(tickers[[1]][j],"SA",sep=".") == names(BancoDeDados_Acoes[k])){
     DataFrame_Final[paste(tickers[[1]][j],"SA",sep=".")] = subset(BancoDeDados_Acoes,select = c(k))
      print("Achei")
    
      }
    }
  }
  
}

df = melt(DataFrame_Final,id.vars = "Data", variable.name = "Ações")

ggplot(df,aes(Data,value)) + geom_line(aes(colour = Ações))
##Pipe: pega o que vem antes e joga na função posterior.
library(plotly)
 plot <- BancoDeDados_Acoes %>% 
  select(Data,ABEV3.SA,AZUL4.SA,B3SA3.SA) %>% 
  melt(id.var = "Data") %>% 
  ggplot(aes(Data,value))+geom_line(aes(colour = variable))
ggplotly(plot)
 


df.m <- melt(BancoDeDados_Acoes, id.var = "Data")
ggplot(df.m,aes(variable,value))+geom_boxplot()

#Criar uma lista dos setores.
#Tecnologia ??

#pesquisa = subset(DataFrame_Empresas,DataFrame_Empresas[5]=="B3SA3")
#df_reduce = subset(df_info,df_info[11]=="ATIVO")
#df_reduce = subset(df_reduce, df_reduce[43]=="BOLSA")
#df_reduce = subset(df_reduce, df_reduce[15]=="Categoria A")
#t1 = gdfpd.get.bovespa.data(1023)
#https://www.msperlin.com/shiny/GetDFPData/
