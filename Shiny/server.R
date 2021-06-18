#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ##################################################################
    #########################Dados do Participante####################
    ##################################################################
    
    output$PlotAtivo1 <- renderPlotly({
        serieTempAtivo <- function(df_emp,acao){
            #Plotagem do resultado
            plott <- BancoDeDados_Acoes %>% 
                select(Data,acao)  %>% 
                melt(id.var = "Data") %>% 
                ggplot(aes(Data,value))+geom_line(aes(colour = variable)) + ggtitle("Série Temporal do Ativo Selecionado: ") + tema +labs(x = "Data", y = "Valor da Ação", colour = "Ativo:")
            ggplotly(plott)
        }
        #Chamando a funcao acima para ver a serie temporal de um setor.
        #acao = "B3SA3.SA"
        serieTempAtivo(df_emp,input$Ativo1)
        
    })
    
    
    ##Problema aqui: tive que mudar de plotly para plot
    output$PlotAtivo2 <- renderPlot({
        boxPlotAtivo <- function(BancoDeDados_Acoes,acao){

                
                BancoDeDados_Acoes %>% 
                select(Data,acao) %>% 
                melt(id.var = "Data") %>% 
                # box <- melt(temp,id.vars = "Data", measure.vars = c("ABEV3.SA"))
                ggplot(aes(Data,value)) + geom_boxplot() + ggtitle("Boxplot do Ativo Selecionado: ") + tema +labs(x = "Data", y = "Valor da Ação")
            
            
            
        }
        print(typeof(input$Ativo2))
        boxPlotAtivo(BancoDeDados_Acoes,input$Ativo2)
        
    })
    
    output$plot2 <- renderLeaflet({
        dados %>% group_by(Country, latitude, longitude) %>% summarise(NrUniv=n_distinct(University)) %>% 
            leaflet() %>% 
            addTiles() %>% 
            addMarkers(lng = ~longitude, lat = ~latitude, popup = ~NrUniv,
                       clusterOptions = markerClusterOptions(maxClusterRadius = 15))
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
