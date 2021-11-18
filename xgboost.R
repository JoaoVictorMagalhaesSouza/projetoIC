library("BatchGetSymbols")
library("data.table")

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
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

df = createDataSet("RADL3.SA")
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
data_test <- maxmindf[1201:1695,]

X_train <- data.matrix(data_train[,c(1,3,4,5,6,7)])
y_train <- data.matrix(data_train$price.close)

X_val <- data.matrix(data_test[,c(1,3,4,5,6,7)])
y_val <- data.matrix(data_test$price.close)



xgb_train <- xgb.DMatrix(data = X_train, label = y_train)
xgb_test <- xgb.DMatrix(data = X_val, label = y_val)
xgbc = xgboost(data = xgb_train, max.depth = 8, nrounds = 5000)
print(xgbc)

pred_y = predict(xgbc, xgb_test)
mse = mean((y_val - pred_y)^2)
mae = caret::MAE(y_val, pred_y)
rmse = caret::RMSE(y_val, pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)


x = 1:length(y_val)
plot(x, y_val, col = "red", type = "l")
lines(x, pred_y, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original y_val", "predicted y_test"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))


