
shinyServer(function(input, output) {

    output$outPlotAtivo <- renderPlotly({
        serieTempAtivo <- function(df_emp,acao){
            #Plotagem do resultado
            plott <- BancoDeDados_Acoes %>% 
                select(Data,acao)  %>% 
                melt(id.var = "Data") %>% 
                ggplot(aes(Data,value))+geom_line(aes(colour = variable)) + ggtitle("Série Temporal do Ativo Selecionado: ") + tema +labs(x = "Data (ano)", y = "Valor da Ação (R$)", colour = "Ativo:")
            
            ggplotly(plott)
        }
        #Chamando a funcao acima para ver a serie temporal de um setor.
        #acao = "B3SA3.SA"
       
        serieTempAtivo(df_emp,input$inAtivoSerie)
        
    })
    
    
    ##Problema aqui: tive que mudar de plotly para plot
    output$outBoxplotAtivo <- renderPlot({
        boxPlotAtivo <- function(BancoDeDados_Acoes,acao){

                
                BancoDeDados_Acoes %>% 
                select(Data,acao) %>% 
                melt(id.var = "Data") %>% 
                # box <- melt(temp,id.vars = "Data", measure.vars = c("ABEV3.SA"))
                ggplot(aes(Data,value)) + geom_boxplot() + ggtitle("Boxplot do Ativo Selecionado: ") +labs(x = "Data", y = "Valor da Ação")
            
            
            
        }
        boxPlotAtivo(BancoDeDados_Acoes,input$inAtivoBox)
        
    })
    
    output$outAtivoCompB3 <- renderPlotly({
        compB3 <- function(df_emp,acao){
            #Plotagem do resultado
            plott <- BancoDeDados_Acoes %>% 
                select(Data,acao)  %>% 
                melt(id.var = "Data") %>% 
                ggplot(aes(Data,value))+geom_line(aes(colour = variable)) + ggtitle("Comparação entre os dois ativos: ") + tema +labs(x = "Data (ano)", y = "Valor das Ações (R$)", colour = "Ativos:")
            
            ggplotly(plott)
        }
        #Chamando a funcao acima para ver a serie temporal de um setor.
        #acao = "B3SA3.SA"
        
        compB3(df_emp,c(input$inAtivoCompB3,"B3SA3.SA"))
        
        
    })
    
    
    
    output$outPlotVariosAtivos <- renderUI ({
    fluidRow(column(10,
        selectInput("inAtivosSetor",
                     strong("Escolha os ativos que deseja monitorar: "),
                     choices = listaAcoesUmSetor(df_emp,BancoDeDados_Acoes,input$inSetor)[-1],
                     multiple = TRUE,
                    
                     
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
        setorMonitorado <- input$inSetor
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
