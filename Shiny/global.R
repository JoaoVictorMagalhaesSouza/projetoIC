## Load packages
library("shiny")
library("shinyjs")
library("shinyBS")
library("plotly")
library("shinythemes")
library("shinycssloaders")
library("RColorBrewer")
library("data.table")
library("tidyverse")
library("stringr")
library("leaflet")
library("BatchGetSymbols")
library("GetDFPData")
library("plyr")
library("reshape2")
library("rsconnect")
library("pracma")
library("ggthemes")
library("lubridate")
library("GGally")
library("ggplot2")
library("viridis")
library("fPortfolio")
library("timeSeries")
library("dplyr")
library("plotrix")
library("httr")
library("dygraphs")
library("xts")
library("lubridate")
library("plotly")
#Teste
#library("tidyr")
#library("httr")
#library("lubridate")
#library("knitr")
# library("markdown")
# library("flexdashboard")

# library("ggthemes")
# library("treemap")

# library("readxl")
# library("readODS")
# library("htmlTable")

# library("esquisse")
# library("gridExtra")
# library("ggpubr")

options(DT.options = list(scrollY="300px",scrollX="300px", 
                          pageLength = 100, 
                          columnDefs = list(list(className = 'dt-center', targets = "_all"))))
library("shinydashboard")
library("shinyWidgets") # nicer inputs
##############################################################################

## Define font to be used later
f1 = list(family = "Arial", size = 10, color = "rgb(30,30,30)")

## Function to format the dates for better plotting
# printDate = function(date){
#   # paste0(day(date),"/",month(date, lab=T, locale="us"))
#   monthsEn=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#   paste0(day(date),"/",monthsEn[month(date)])
# }


#BancoDeDados_Acoes <- readRDS("BancoDeDados_Acoes.rds")
##############################################################################
horas <- strsplit(as.character(Sys.time())," ")
horas <- horas[[1]][2]
horas <- strsplit(horas,":")
hora <- horas[[1]][1]
minuto <- horas[[1]][2]
#
#dados <- readRDS("LeidenRanking.Rds")
DI = '2015-01-01' #Data de inicio
DF = Sys.Date() #Data de fim(hoje)
benchmark = '^BVSP' #índice da bolsa
tickersIbov = GetIbovStocks() #Retorna as ações negociadas do Brasil, dados completos.
tickersIbov$tickersSA = paste(tickersIbov$tickers,".SA",sep='') #Criar uma coluna e adicionar o .SA nos tickers
#saveRDS(df_emp,"teste.rds")
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

rm(itera_BancoDeDados_Acoes)
#Tratamento para comecar a partir de 2016
BancoDeDados_Acoes = BancoDeDados_Acoes[-(1:184),]

##

##Banco de Dados mais apurado com descrição e os tickers. 
df_emp <-  GetDFPData::gdfpd.get.info.companies()
df_emp <-  df_emp %>% select(c(1,11:14))
#Tirar os duplicados
df_emp <-  df_emp[!duplicated(df_emp), ]
#Tirar as empresas que nao tem um pregao
df_emp <- df_emp %>% dplyr::filter(tickers!="")
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

tema <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
              legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
              legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
              axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
              axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10))) 

#saveRDS(BancoDeDados_Acoes,"BancoDeDados_Acoes.rds")

listaTodosSetores <- function(df_emp){
  setores = subset(df_emp, select = c(2))
  setores = setores[!duplicated(setores),]
  return(setores)
}
listaSetores  <- listaTodosSetores(df_emp)

verificar_coluna <- function(data, coluna){
  retorno <- coluna
  retorno %in% names(data)
}

listaAcoesUmSetor <- function(df_emp,BancoDeDados_Acoes,setorMonitorado){
  setor = setorMonitorado
  #Pegar todas as empresas desse setor
  Acoes_Filtradas = subset(df_emp,df_emp[2]==setor)
  #Pegar todos os tickers dessas empresas desse setor
  Acoes_Filtradas_lista = Acoes_Filtradas$Tickers
  nlinhas <- nrow(Acoes_Filtradas)
  numcol = ncol(BancoDeDados_Acoes)
  #Pegando a coluna "Data" do BancoDeDados_Acoes para fazer um join depois
  df_setor <- data.frame(Data=c(BancoDeDados_Acoes[1]))
  #Vamos conferir quais  os tickers desse BD Auxiliar(no setor escolhido) estao no BD do Yahoo.
  for(i in 1:nlinhas){
    tickers = strsplit(Acoes_Filtradas_lista[i],";")
    for (j in 1:length(tickers[[1]])){
      aux <- paste(tickers[[1]][j],"SA",sep=".")
      if (verificar_coluna(BancoDeDados_Acoes,aux)){
        df_setor[aux] =  select(BancoDeDados_Acoes,aux)
      }
      
    }
    
  }
  acoesDoSetor <- colnames(df_setor)
  return(acoesDoSetor)
  
}

listaSemB3 <- function(){
  acoes <- BancoDeDados_Acoes
  acoes$B3SA3.SA <- NULL
  acoes <- names(acoes)[-1]
  return(acoes)
}
noB3 <- listaSemB3()

#bimestres <- c("Janeiro/Fevereiro","Março/Abril","Maio/Junho","Julho/Agosto","Setembro/Outubro","Novembro/Dezembro")
bimestres <- c(1,2,3,4,5,6)
anoAtual <- strsplit(as.character(Sys.Date()),"-")[[1]][1]
anos <- 2016:anoAtual
secoes <- c("Apresentação","Ativo Único", "Setorial Completo", "Setorial Filtrado", "Sobre os Envolvidos")

