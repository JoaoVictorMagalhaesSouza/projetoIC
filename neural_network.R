library("neuralnet")
library("BatchGetSymbols")
library("dplyr")
library("data.table")
library("caret")
createDataSet <- function(acao){
  DI = '2015-01-01' #Data de inicio
  DF = Sys.Date() #Data de fim(hoje)
  benchmark = '^BVSP' #indice da bolsa
  DataSet = BatchGetSymbols(
    tickers = acao, #Especificando as ações
    first.date = DI,
    last.date= DF,
    bench.ticker = benchmark)
  
  
  #Pegando o segundo elemento da lista retornada, que e o que contem os dados.
  DataSet = DataSet$df.tickers
  
  #Selecao de colunas de interesse
  # DataSet <- DataSet %>% 
  #    select(c(1,2,3,4,7,8))
  
  
  
  DataSet <- createMetrics(DataSet)
  
  return (DataSet)
}

createMetrics <- function (DataSet){
  DataSet$price.highLow <- DataSet$price.high - DataSet$price.low
  DataSet <- data.table(DataSet)
  DataSet[ , `:=`(
    ret.adjusted.prices = fcase(
      is.na(ret.adjusted.prices),0,
      ret.adjusted.prices=!is.na(ret.adjusted.prices),ret.adjusted.prices
    ),
    ret.closing.prices = fcase(
      is.na(ret.closing.prices),0,
      ret.closing.prices != is.na(ret.closing.prices),ret.closing.prices
    )
  )
  
  ]
  DataSet$price.hype <- DataSet$price.high - DataSet$price.open
  return (DataSet)
  
  
}
df = createDataSet("B3SA3.SA")[,-c("ret.closing.prices","ref.date","ticker")]
write.csv(df,"b3unormalized.csv", row.names = FALSE)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(df, normalize))
write.csv(df,"b3normalized.csv", row.names = FALSE)

data_train <- maxmindf[1:1200, ]
data_test <- maxmindf[1201:1695, ]
#partition = createDataPartition(1:dim(df)[1],p=.7)

#data_train <- df[partition$Resample1,]
#data_test <- df[-partition$Resample1,]

modelo = neuralnet(price.close~price.open+price.high+price.low+volume+price.adjusted+ret.adjusted.prices+
               price.highLow+price.hype,data=data_train,hidden=c(8,5),act.fct = "tanh")

plot(modelo,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

teste = neuralnet::compute(modelo,data_test[,-c("price.close")])


resultado = as.data.frame(teste$net.result)



confusao = table(resultado$class,teste$target)

confusao
