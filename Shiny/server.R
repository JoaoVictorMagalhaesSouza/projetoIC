
shinyServer(function(input, output) {

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
            numBimestres <- as.integer(mesAtual)%/%2
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
                     strong("Escolha os ativos que deseja monitorar (máx 5): "),
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
   
   
   output$plot3 <- renderPlotly({
       plot2 <- dados %>% filter(Country=="BRAZIL") %>% 
           filter(University=="UNIVERSIDADE FEDERAL DE VICOSA",
                  Period=="2014–2017", 
                  Frac_counting=="1") %>% 
           ggplot(aes(Field, impact_P, fill=Field, label= round(impact_P, digits = 2), 
                      text=paste("Produção:",impact_P, "<br>", 
                                 "Período:", Period))) +
           geom_col(aes(Field, impact_P), show.legend = FALSE) + 
           xlab("Área Ciêntífica (2014-2017)") + ylab("Número de Publicações com Impacto") + 
           geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw() +
           theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
       ggplotly(plot2, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
       
   }) 
    
})
