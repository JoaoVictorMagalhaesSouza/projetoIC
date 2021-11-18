#install.packages('devtools')
#devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.16.5/catboost-R-Windows-0.16.5.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
library("catboost")
library("BatchGetSymbols")
library("dplyr")
library("data.table")
library("caret")
library("rpart.plot")
library("GGally")
library("ggplot2")
library("dygraphs")
library("xts")
library("lubridate")
library("plotly")
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

df = createDataSet("CIEL3.SA")
datas <- df$ref.date
df <- df[,-c("ret.closing.prices","ticker","ref.date","price.low","price.high")]
mat_corr <- ggcorr(df)
mat_corr
#write.csv(df,"b3unormalized.csv", row.names = FALSE)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(df, normalize))
#write.csv(df,"b3normalized.csv", row.names = FALSE)

data_train <- maxmindf[1:1200, ]
data_test <- maxmindf[1201:1695, ]

train_pool <- catboost.load_pool(data=data_train,label=data_train[,c("price.close")])
test_pool <- catboost.load_pool(data=data_test,label=data_test[,c("price.close")])
model <- catboost.train(train_pool,  test_pool,
                        params = list(
                                      iterations = 100, metric_period=10))

real_pool <- catboost.load_pool(data=maxmindf,label=maxmindf[,c("price.close")])
prediction <- catboost.predict(model, real_pool,verbose = TRUE,thread_count=4)
print(prediction)
prediction <- data.frame(prediction)

# p = ggplot() +
# 
#   geom_line(data = prediction, aes (x= datas,y=prediction), color = "blue") +
#   geom_line(data = maxmindf, aes(x=datas,y=price.close),color="red",) +
#   xlab('Dates') +
#   ylab('percent.change')+
# 
# 
# print(p)
maxmindf$data = datas
prediction$data = datas
maxmindf <- maxmindf[,c("data","price.close")]
names(maxmindf)[2] <- c("Preço Real")
names(prediction)[1] <- c("Preço previsto")
prescription = merge(prediction, maxmindf, by="data")
prescriptionMelted <- reshape2::melt(prescription, id.var='data')
ggplot(prescriptionMelted, aes(x=data, y=value, col=variable)) + geom_line()

bd = data.table()
bd$Data = datas
bd$`Preço Real` = maxmindf$`Preço Real`
bd$`Preço Previsto` = prediction$`Preço previsto`
df <- bd %>% 
  select(Data,`Preço Real`, `Preço Previsto`)   
don <- xts(order.by = bd$Data,x = bd[,-1])
dygraph(don,main="Preço Real x Preço Previsto",xlab = "Data",ylab = "Preço de Fechamento(R$)") %>%
  dyOptions(stackedGraph = TRUE) %>%    
  dyRangeSelector(height = 20)
