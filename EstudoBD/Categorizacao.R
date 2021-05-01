library(BatchGetSymbols)
library(quantmod)
library(GetDFPData)
library(GetDFPData2)
library(tidyverse)
library(ggthemes)
library(reshape2)
library(plyr)
library(ggplot2)
library(plotly)

#acao = 'BBDC3.sa' #Empresa.sa -> para analisar alguma empresa em espec.
DI = '2014-01-01' #Data de inicio
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
##Primeiro semestre faltante.

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

bimestre = 3
mes1 <- paste("0",as.character((bimestre*2)-1),sep = "")
mes2 <- paste("0",as.character(bimestre*2),sep="")
j = 1
dataIni = as.integer(strsplit(DI,"-")[[1]][1])+1
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
ggplotly(ploter[[4]])
ggplot(boxers[[3]],aes(x=Data,y=value))+geom_boxplot()



# for (i in linha:nrow(BancoDeDados_Acoes)){
#      if(strsplit(as.character(BancoDeDados_Acoes$Data[i]),"-")[[1]][2]=="05" || strsplit(as.character(BancoDeDados_Acoes$Data[i]),"-")[[1]][2]=="06"){
#        print(BancoDeDados_Acoes$Data[i])
#        plot[j] <- BancoDeDados_Acoes %>%        
#         select(Data,BBDC3.SA) %>% 
#         melt(id.var = "Data") %>% 
#         ggplot(aes(Data,value))+geom_line(aes(colour = variable))
#         j <- j+1
#      }
# }
# ggplotly(plot)
# 
# #IMPLEMENTANDO FILTRO
# #Montar um dataframe com os dados de um setor específico.
# print(setores[[1]][9])
# setor = setores[[1]][9]    #Saúde
# Acoes_Filtradas = subset(DataFrame_Empresas,DataFrame_Empresas[2]==setor)
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
