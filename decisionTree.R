library("BatchGetSymbols")
library("data.table")
library("rpart")
library("rpart.plot")
library("GGally")
library("ggplot2")
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
  #Price HighLow = Variação do maior preço - o menor preço
  DataSet$price.highLow <- DataSet$price.high - DataSet$price.low
  #Price Hype = variação do maior preço - preço de abertura
  DataSet$price.hype <- DataSet$price.high - DataSet$price.open
  DataSet <- data.table(DataSet)
  DataSet[ , `:=`(
    target = fcase(
      price.open > price.close, "Queda",
      price.open <= price.close, "Alta"
      #,price.open == price.close, "Estavel"
    ),
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
  
  return (DataSet)[,-c("ret.closing.prices")] #Tirando essa coluna inutil.
  
  
}
df = createDataSet("LAME4.SA")

qtdeAlta = nrow(filter(df, target=="Alta"))
qtdeBaixa = nrow(filter(df, target=="Queda"))
qtdeEstavel =  nrow(filter(df, target=="Estavel"))
print(paste("Quantidade de dados classificados em Alta: ",qtdeAlta))
print(paste("Quantidade de dados classificados em Queda: ",qtdeBaixa))
print(paste("Quantidade de dados classificados em Estavel: ",qtdeEstavel))


create_train_test <- function(data, size = 0.9, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(df, 0.8, train = TRUE)
data_test <- create_train_test(df, 0.8, train = FALSE)
dim(data_train)
mat_corr <- ggcorr(df)
mat_corr
graf_disp <- ggplot(df, aes(x=df$ref.date, y=df$price.close)) +
  geom_point() +
  geom_smooth(method="loess", se = F) +
  labs(subtitle = "Comportamento do Ativo",
       y = "Preço de Fechamento (R$)", x = "Data",
       caption = "Fonte: Yahoo Finance")

plot(graf_disp)


fit <- rpart(target~price.open+price.high+price.low+price.close+volume+price.adjusted+ret.adjusted.prices+
                price.highLow+price.hype, data = data_train, method = 'class')
#fit <- rpart(target~price.open+price.close+volume+ret.adjusted.prices
       #, data = data_train, method = 'class')
# fit <- rpart(price.close~price.open+price.high+price.low+volume+price.adjusted+ret.adjusted.prices+
#                price.highLow+price.hype, data = data_train, method = 'class')
#Arvore:
rpart.plot(fit, extra = 106)
predict_unseen <- predict(fit, data_test, type = 'class')
table_mat <- table(data_test$target, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste("A acurácia foi de: ",accuracy_Test*100,"%"))
graf_disp
