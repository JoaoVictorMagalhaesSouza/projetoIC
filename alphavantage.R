library("alphavantager")

av_api_key("T2XHP60PEKMTDC7B")
bd <- av_get(symbol     = "PETR4",
             av_fun = "TIME_SERIES_INTRADAY",
       interval   = "15min",
       outputsize = "full")

