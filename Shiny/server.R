
shinyServer(function(input, output) {
    
    observeEvent(
        input$buttonOk,{
            withProgress(message = 'Gerando Carteira', value = 0, {
            qtdeAtivos <- length(input$inAtivosMark) 
            if (qtdeAtivos > 1){
                print("S")
                n_sim <- 2000
                df <- select(BancoDeDados_Acoes,Data,input$inAtivosMark)
                incProgress(1/5,detail = "Obtendo dados...")
                
                #df <- rename(df,Data=Date)
                names(df)[1] <- c("Date")
                #print(df)
                
                # names(df) <- c("Data", "ABEV3.SA", "B3SA3.SA", "BBAS3.SA",
                #                "BBDC3.SA", "BBXC4.SA")
                # names(df) <- c("Date")
                from_day_to_month <- function(df){
                    df <- df %>%  dplyr::mutate(Date=lubridate::ymd(Date))
                    df <- df %>%  dplyr::mutate(year = lubridate::year(Date), 
                                                month= lubridate::month(Date)) %>%
                        dplyr::group_by(year,month) %>%
                        arrange(Date) %>%
                        dplyr::filter(row_number()==1)
                    df <- df %>% mutate(Date=ymd(paste0(year,"-",month,"-01")))
                    df
                }
                
                upload_stock <- function(x,max_d){
                    df <- select(BancoDeDados_Acoes,Data,input$inAtivosMark)
                    names(df)[1] <- c("Date")
                    
                    df <- from_day_to_month(df)
                    df <- df %>% select(1,x,year,month) #%>% filter(Data<="2021/06/20")
                    df$Date <- as.Date(df$Date)
                    max_date <- as.Date(paste0(year(max_d),"-",month(max_d), "-01"))
                    df <- subset(df, Date <= max_date)
                    
                }
                sel_stocks <-input$inAtivosMark
                #upload_stock("B3SA3.SA", "2021/06/20")
                incProgress(2/5,detail = "Reduzindo os dados...")
                Sys.sleep(0.2)
                series  <- lapply(sel_stocks, upload_stock, max_d = "2021/06/18")
                n_stock <- length(series)
                percentage <- function(number){
                    paste(round(100* number, 2), "%", sep="")
                } 
                sel_min_month <- which.min(sapply(series,function(dat) {length(dat$Date)}))
                dates         <- series[[sel_min_month]]$Date
                first_month   <- length(dates)
                series <- lapply(series,function(dat){
                    if(nrow(dat)>first_month){
                        dat <- dat[(nrow(dat)-first_month+1):nrow(dat),]
                    }
                    dat[,2]
                })
                series <- do.call("cbind",series)
                series <- cbind(dates,series)
                series <- na.omit(series)
                dates         <- series$dates
                
                #Mudei aq
                names_stocks <- names(series)[-1]
                
                
                
                # Compute the monthly returns
                yld     <-  (series[2:nrow(series),-1] - series[1:(nrow(series)-1),-1]) / series[1:(nrow(series)-1),-1]
                
                # Compute the yearly covariance
                cov_yld <-  cov(yld)*12
                
                yld     <-  cbind(dates[-1],yld)
                # Mean monthly returns for each stock 
                mean_yld <- colMeans(yld[,-1])
                # Build the efficient frontier 
                set.seed(20101995)
                
                # Start simulation
                ptf_sim     <- lapply(1:n_sim,function(sim){
                    # Random weights
                    w_i_abs   <- sample(1:1000,n_stock,replace = TRUE)
                    # Rescale weights in (0,1)
                    w_i       <-  w_i_abs/sum(w_i_abs)
                    
                    # Annualized simulated returns
                    return_i  <- (sum(w_i*mean_yld)+1)^12 - 1
                    
                    # Annualized simulated covariances
                    risk_yld  <- t(w_i) %*% cov_yld %*% w_i
                    
                    # Sharpe ratio
                    sharpe_ratio <- return_i/risk_yld
                    
                    list(w_i=w_i, return_i=return_i, risk_yld=risk_yld, sharpe_ratio=sharpe_ratio)
                })
                df_ptf_sim <- data.frame(return       = sapply(ptf_sim,function(col){col$return_i}),
                                         risk_yld     = sapply(ptf_sim,function(col){col$risk_yld}),
                                         sharpe_ratio = sapply(ptf_sim,function(col){col$sharpe_ratio})
                )
                # Insert the weights in the dataframe
                for(i in 1:n_stock){
                    df_ptf_sim <- cbind(df_ptf_sim, sapply(ptf_sim,function(col){col[[1]][i]}))
                    
                }
                # Names the columns containing the weights
                names(df_ptf_sim)[(length(df_ptf_sim)-n_stock+1):length(df_ptf_sim)]   <- names_stocks
                
                # Build a summary column (whose value will appear when hovering with mouse on graph "a")
                tmp <-  apply(df_ptf_sim[,names_stocks], 1, function(stock){
                    paste0(names_stocks,":", percentage(stock), "<br>")
                })
                df_ptf_sim$W <- do.call("paste", c(as.data.frame(t(tmp)),sep="  "))
                rm(tmp)
                
                # Select minimum risk portfolio 
                min_risk         <- df_ptf_sim[which.min(df_ptf_sim$risk_yld),]
                # Select maximum sharp ratio portfolio 
                max_sharpe_ratio <- df_ptf_sim[which.max(df_ptf_sim$sharpe_ratio),]
                
                # Build the minimum risk portfolio and maximum sharp ratio portfolio 
                extract_weight <- function(w){
                    weights <- scan(text = w, what = "") %>% 
                        str_remove_all(., "%<br>")
                    weights <- as.numeric(substr(sub(".*:", "", weights),1,4))/100
                    return(weights)
                }
                incProgress(3/5,detail = "Inferindo informações...")
                extract_weight_min_risk  <- extract_weight(min_risk$W)
                extract_max_sharpe_ratio  <- extract_weight(max_sharpe_ratio$W)
                
                ptf_max_sharpe <- apply(series[-1],1,function(row){extract_max_sharpe_ratio * row }) %>%
                    colSums(.)
                ptf_min_risk         <- apply(series[-1],1,function(row){extract_weight_min_risk * row }) %>%
                    colSums(.) 
                ptf                  <- rbind(ptf_max_sharpe, ptf_min_risk) %>% as.data.frame() %>%
                    t() %>% round(.,2)
                series               <- cbind(series,ptf)
                yld_ptf              <- (ptf[2:nrow(ptf),] - ptf[1:(nrow(ptf)-1),]) / ptf[1:(nrow(ptf)-1),]
                # Summary Portfolio
                summary_ptf <- data.frame(Mean  = colMeans(yld_ptf), 
                                          Sd    = apply(yld_ptf,2,sd),
                                          Worst = apply(yld_ptf,2,min),
                                          Max   = apply(yld_ptf,2,max),
                                          median   = apply(yld_ptf,2, function(col) quantile(col, probs = 0.50 ))
                )
                
                
                
                # Summary
                summary_pos <- data.frame(Mean  = mean_yld, 
                                          Sd    = apply(yld[,-1],2,sd),
                                          Worst = apply(yld[,-1],2,min),
                                          Max   = apply(yld[,-1],2,max),
                                          median   = apply(yld[,-1],2, function(col) quantile(col, probs = 0.50 ))
                )
                summary     <- rbind(summary_pos, summary_ptf)
                
                summary           <- as.data.frame(apply(summary, 2, function(col) round(col,4)))
                rownames(summary) <- rownames(summary)
                incProgress(4/5,detail = "Montando os plots...")
                
                a  <- ggplot(aes(x=risk_yld, y=return, color = sharpe_ratio , text=W), data =df_ptf_sim) +
                    geom_point()+
                    scale_color_viridis()+
                    theme_classic() +
                    scale_y_continuous(labels = scales::percent) +
                    scale_x_continuous(labels = scales::percent) +
                    
                    labs(x = 'Risco',
                         y = 'Retorno Esperado',
                         title = "Fronteira Eficiente de Markowitz",
                         colour = "Índice Sharpe") +
                    
                    geom_point(aes(x = risk_yld, y = return), data =min_risk , color = 'red') +
                    geom_point(aes(x = risk_yld, y = return), data =max_sharpe_ratio, color = 'green') 
                
                max_sharpe_ratio_long  <-  gather(max_sharpe_ratio,"stock","weight",-c("return","risk_yld","sharpe_ratio","W"))
                b <- ggplot(max_sharpe_ratio_long,aes(x="",y=weight, fill=stock)) +
                    geom_bar(stat="identity", width=1, color="white")+
                    labs(x = '',
                         y = 'Asset allocation',
                         title = "Maximum sharpe ratio portfolio") + 
                    theme_classic()
                
                min_risk_long  <-  gather(min_risk,"stock","weight",-c("return","risk_yld","sharpe_ratio","W"))
                c <- ggplot(min_risk_long,aes(x="",y=weight, fill=stock)) +
                    geom_bar(stat="identity", width=1, color="white")+
                    labs(x = '',
                         y = 'Asset allocation',
                         title = "Minimum risk portfolio") + 
                    theme_classic()
                
                # Yld chart
                den <- bind_rows(replicate(nrow(series) - 1, series[1,-1], simplify = FALSE))
                num <- series[2:nrow(series),-1] 
                e   <- cbind(dates[-1] , (num - den) / den) %>% dplyr::rename(dates = 'dates[-1]')
                rm(den,num)
                
                e <-  e %>% gather(key = "Stock", value = "Price", -dates) %>%
                    ggplot(., aes(x = dates , y = Price , color = Stock)) +
                    geom_line() + 
                    theme_bw() +
                    labs(x = 'Date',
                         y = '',
                         title = "Performance") +
                    scale_x_date(date_breaks = "3 month", date_labels = "%b-%y") + 
                    theme(axis.text.x = element_text(angle = 90),
                          plot.title  = element_text(color = "black", size = 25, face = "bold"),
                          panel.border = element_blank()) +
                    scale_y_continuous(labels = scales::percent)  
                
                a  <- ggplotly(a, tooltip = "text") 
                b  <- ggplotly(b)
                c  <- ggplotly(c)
                d  <- ggcorr(yld[,2:(n_stock+1)],label = TRUE) +
                    #theme(plot.title  = element_text(color = "black", size = 25, face = "bold")) +
            
                    labs(title = "Matriz de Correlação dos Ativos")+
                    theme_classic()
                e  <- ggplotly(e)
                
                df_aux <- as.timeSeries(BancoDeDados_Acoes)
                df <- df_aux[,c(input$inAtivosMark)]
        
                Fronteira1 = portfolioFrontier(df, spec = portfolioSpec(), constraints = "LongOnly")
                portfolio.eficiente <- tangencyPortfolio(df, spec = portfolioSpec(), constraints = "LongOnly")
                risco <- cbind(portfolio.eficiente@portfolio@portfolio$covRiskBudgets)
                tab <- as.data.frame(risco)
                tab$Ativos <- rownames(tab)
                names(tab) <- c("Percentual", "Ativos")
                tab$Percentual <- tab$Percentual*100
    
                #A tabela leva em consideração a distribuição ótima (tangente)?
                
                
                incProgress(5/5,detail = "A exibir: ")
                
                
                
                output$outCartMark <- renderPlotly(a)
                output$outMark1 <- renderPlotly(b)
                output$outMark2 <- renderPlotly(c)
                output$outMark3 <- renderPlot(d)
                output$outTabelaMark <- renderTable(tab)
                
            
            }
            })
            
            }
        
    
    )
    

    output$outPlotAtivo <- renderPlotly({
        serieTempAtivo <- function(df_emp,acao){
            #Plotagem do resultado
            plott <- BancoDeDados_Acoes %>% 
                select(Data,acao)  %>% 
                melt(id.var = "Data") %>% 
                ggplot(aes(Data,value))+geom_line(aes(colour = variable)) + ggtitle("Série Temporal: ") + theme_light() +labs(x = "Data (ano)", y = "Valor da Ação (R$)", colour = "Ativo:") + scale_x_date(date_breaks = "9 months", date_labels = "%b/%Y")
            
            ggplotly(plott)
        }
        #Chamando a funcao acima para ver a serie temporal de um setor.
        
       
        serieTempAtivo(df_emp,input$inAtivoSerie)
        
    })
    
    output$outAtivoAnual <- renderPlotly({
        dadosAnual <- function(DF,BancoDeDados_Acoes,ano,acao){
            j = 1
            #dataIni = 2016
            #dataF = as.integer(strsplit(as.character(DF),"-")[[1]][1])
            #qtdeGraf <- dataF - dataIni #Quantidade de gráficos a serem gerados.
            dataAtual <- ano
            # boxers <- list()
            # print(dataAtual)
            final <- 1
            contador <- 0
            inicial <- 0
            for (j in 1:nrow(BancoDeDados_Acoes)){
                #Ano
                if (strsplit(as.character(BancoDeDados_Acoes$Data[j]),"-")[[1]][1] == dataAtual){
                    #Bimestre que eu quero
                    final <- j
                        #print(BancoDeDados_Acoes$Data[j])
                        contador <- contador + 1
                    
                    inicial <- final-contador+1
                }
            }
            
            
            #Plotar aqui
            ploter <- BancoDeDados_Acoes[inicial:final,] %>%        
                select(Data,acao) %>% 
                melt(id.var = "Data") %>% 
                ggplot(aes(Data,value))+geom_line(aes(colour = variable)) #+  #geom_smooth(method = "loess", se = FALSE)
            
            #boxers[[i]] = melt(BancoDeDados_Acoes[inicial:final,],id.vars = "Data", measure.vars = c("B3SA3.SA"))
            
            
            
            ggplotly(ploter)
        }
        
        dadosAnual(DF,BancoDeDados_Acoes,input$inAnoAnual,input$inAtivoAnual)
        
        
        
    })
    
    
    output$outAno <- renderUI({
    
        if (input$inAno == anoAtual){
            mesAtual <- strsplit(as.character(Sys.Date()),"-")[[1]][2]
            print(mesAtual)
            mesAtual <- as.integer(mesAtual)
            if(mesAtual%%2==0){
            numBimestres <- mesAtual%/%2
            }
            else{
                numBimestres <- (mesAtual%/%2)+1 
            }
            print(numBimestres)
            bimestresOf <- 1:numBimestres
    
        }
        else{
            bimestresOf <- c(1,2,3,4,5,6)
        
        }
        
        fluidRow(column(12,
                        selectInput("inBimestre",
                                       strong("Escolha um bimestre: "),
                                       choices = bimestresOf,
                                       
                        ),
                    
                        fixedRow(column(12, offset = 6, align ="center",
                                        plotlyOutput("outBimestral", height = 600)
                        ))
                        
        ))
        
        
        
        
                
        
        
        
    })
    
    
    output$outBimestral <- renderPlotly({
        dadosBimestre <- function(DF,BancoDeDados_Acoes,ano,bimestre,acao){
            intBim = as.integer(bimestre)
            if (intBim < 5){
            mes1 <- paste("0",as.character((intBim*2)-1),sep = "")
            mes2 <- paste("0",as.character(intBim*2),sep="")
            }
            else if (intBim == 5){
            mes1 <- paste("0",as.character((intBim*2)-1),sep = "")
            mes2 <- "10"
            }
            else{
            mes1 <- 11
            mes2 <- 12
            }
            j = 1
            #dataIni = 2016
            #dataF = as.integer(strsplit(as.character(DF),"-")[[1]][1])
            #qtdeGraf <- dataF - dataIni #Quantidade de gráficos a serem gerados.
            
            dataAtual <- ano
            # boxers <- list()
              # print(dataAtual)
             final <- 1
             contador <- 0
             inicial <- 0
                for (j in 1:nrow(BancoDeDados_Acoes)){
                    #Ano
                    if (strsplit(as.character(BancoDeDados_Acoes$Data[j]),"-")[[1]][1] == dataAtual){
                        #Bimestre que eu quero
                        if(strsplit(as.character(BancoDeDados_Acoes$Data[j]),"-")[[1]][2]==mes1 || strsplit(as.character(BancoDeDados_Acoes$Data[j]),"-")[[1]][2]==mes2){
                            final <- j
                            #print(BancoDeDados_Acoes$Data[j])
                            contador <- contador + 1
                        }
                        inicial <- final-contador+1
                    }
                }
               
                
                #Plotar aqui
                ploter <- BancoDeDados_Acoes[inicial:final,] %>%        
                    select(Data,acao) %>% 
                    melt(id.var = "Data") %>% 
                    ggplot(aes(Data,value))+geom_line(aes(colour = variable))
                
                #boxers[[i]] = melt(BancoDeDados_Acoes[inicial:final,],id.vars = "Data", measure.vars = c("B3SA3.SA"))
                
                
            
            ggplotly(ploter)
        }
        
        dadosBimestre(DF,BancoDeDados_Acoes,input$inAno,input$inBimestre,input$inAtivoBimestral)
        
        
        
        
    })
    
    
    
    output$outAtivoCompB3 <- renderPlotly({
        compB3 <- function(df_emp,BancoDeDados_Acoes,acao){
            #Plotagem do resultado
            plott <- BancoDeDados_Acoes %>% 
                select(Data,acao)  %>% 
                melt(id.var = "Data") %>% 
                ggplot(aes(Data,value))+geom_line(aes(colour = variable)) + ggtitle("Comparação do ativo com a B3 ") + tema +labs(x = "Data (ano)", y = "Valor das Ações (R$)", colour = "Ativos:")
            
            ggplotly(plott)
        }
        #Chamando a funcao acima para ver a serie temporal de um setor.
        #acao = "B3SA3.SA"
        
        compB3(df_emp,BancoDeDados_Acoes,c(input$inAtivoCompB3,"B3SA3.SA"))
        
        
    })
    
    
    ##Problema aqui: tive que mudar de plotly para plot
    output$outBoxplotAtivo <- renderPlot({
        boxPlotAtivo <- function(BancoDeDados_Acoes,acao){

                
                BancoDeDados_Acoes %>% 
                select(Data,acao) %>% 
                melt(id.var = "Data") %>% 
                # box <- melt(temp,id.vars = "Data", measure.vars = c("ABEV3.SA"))
                ggplot(aes(Data,value)) + geom_boxplot(fill='#56B4E9',color = "blue",outlier.colour = "red") + ggtitle("Boxplot: ") + labs(x = "Data (ano)", y = "Valor da Ação (R$)") + theme_classic()
            
            
            
        }
        boxPlotAtivo(BancoDeDados_Acoes,input$inAtivoBox)
        
    })
    
    
    output$outBoxAnual <- renderPlot({
        boxAnual <- function(DF,BancoDeDados_Acoes,ano,acao){
            j = 1
            #dataIni = 2016
            #dataF = as.integer(strsplit(as.character(DF),"-")[[1]][1])
            #qtdeGraf <- dataF - dataIni #Quantidade de gráficos a serem gerados.
            dataAtual <- ano
            # boxers <- list()
            # print(dataAtual)
            final <- 1
            contador <- 0
            inicial <- 0
            for (j in 1:nrow(BancoDeDados_Acoes)){
                #Ano
                if (strsplit(as.character(BancoDeDados_Acoes$Data[j]),"-")[[1]][1] == dataAtual){
                    #Bimestre que eu quero
                    final <- j
                    #print(BancoDeDados_Acoes$Data[j])
                    contador <- contador + 1
                    
                    inicial <- final-contador+1
                }
            }
            
            
            #Plotar aqui
                BancoDeDados_Acoes[inicial:final,] %>%        
                select(Data,acao) %>% 
                melt(id.var = "Data") %>% 
                ggplot(aes(Data,value)) + geom_boxplot(fill='#56B4E9',color = "blue",outlier.colour = "red") + theme_classic()
            
            #boxers[[i]] = melt(BancoDeDados_Acoes[inicial:final,],id.vars = "Data", measure.vars = c("B3SA3.SA"))
            
            
            
           # ggplotly(ploter)
        }
        
        boxAnual(DF,BancoDeDados_Acoes,input$inBoxAnualAno,input$inBoxAnualAtivo)
        
        
        
    })
    
    output$outSetorComp <- renderPlotly({
        serieTempSetor <- function(df_emp,BancoDeDados_Acoes,setorMonitorado){
            #aux <- "B3SA3.SA"
            #verificar_coluna(BancoDeDados_Acoes,aux)
            
            #Escolher um setor específico
            setor = setorMonitorado #setores[[1]][9]    #Saúde
            #Pegar todas as empresas desse setor:
            Acoes_Filtradas = subset(df_emp,df_emp[2]==setor)
            #Pegar todos os tickers dessas empresas desse setor:
            Acoes_Filtradas_lista = Acoes_Filtradas$Tickers
            #Obter o numero de linhas do BD das acoes do setor especificado
            nlinhas <- nrow(Acoes_Filtradas)
            #Conferir coluna a coluna 
            numcol = ncol(BancoDeDados_Acoes)
            #Pegando a coluna "Data" do BancoDeDados_Acoes
            
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
            #Plotagem do resultado
            plott <- df_setor %>%         
                melt(id.var = "Data") %>% 
                ggplot(aes(Data,value))+geom_line(aes(colour = variable))
            ggplotly(plott)
        }
        #Chamando a funcao acima para ver a serie temporal de um setor.
        #No shiny criaremos uma listBox para o usuario escolher o setor.
        setorMonitorado = input$inSetorComp
        serieTempSetor(df_emp,BancoDeDados_Acoes,setorMonitorado)
        
        
    })
    
    
    output$outSetorFilt <- renderUI ({
    fluidRow(column(10,
        selectizeInput("inAtivosSetor",
                     strong("Escolha os ativos que deseja monitorar (máx. 5): "),
                     choices = listaAcoesUmSetor(df_emp,BancoDeDados_Acoes,input$inSetorFilt)[-1],
                     multiple = TRUE,
                    options = list(maxItems = 5),
                    
                     
        ),
        fixedRow(column(12, offset = 6, align ="center",
                        plotlyOutput("outAtivosSetor", height = 600)
        ))
        
        ))
        
        
        
        
    })
    
    
    output$outAtivosSetor <- renderPlotly({
        serieTempAlgumasAcoesSetor <- function(df_emp,setorMonitorado,listaAcoes){
            #setores = subset(df_emp, select = c(2))
            #setores = setores[!duplicated(setores),]
            #Escolher um setor específico
            setor = setorMonitorado #setores[[1]][9]    #Saúde
            #Pegar todas as empresas desse setor:
            Acoes_Filtradas = subset(df_emp,df_emp[2]==setor)
            #Pegar todos os tickers dessas empresas desse setor:
            Acoes_Filtradas_lista = Acoes_Filtradas$Tickers
            #Obter o numero de linhas do BD das acoes do setor especificado
            nlinhas <- nrow(Acoes_Filtradas)
            #Conferir coluna a coluna 
            numcol = ncol(BancoDeDados_Acoes)
            #Pegando a coluna "Data" do BancoDeDados_Acoes
            
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
            #Plotagem do resultado
            plott <- df_setor %>%
                select(Data,listaAcoes) %>%
                melt(id.var = "Data") %>% 
                ggplot(aes(Data,value))+geom_line(aes(colour = variable))
            ggplotly(plott)
        }
        #Passaremos a lista com as acoes que o usuario quer monitorar e o setor tambem.
        listaAcoes <- input$inAtivosSetor    #c("FLRY3.SA","RADL3.SA")
        setorMonitorado <- input$inSetorFilt
        if (!is.null(listaAcoes)){
            serieTempAlgumasAcoesSetor(df_emp,setorMonitorado,listaAcoes)
        }
        
    })
    
    
        
        
    
    
   
   
   
    
   
   
    
})
