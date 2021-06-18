#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application
shinyUI(fluidPage(
    navbarPage(title="ANÁLISE DA BOVESPA",
               tabPanel("ATIVO",
                        tabsetPanel(
                            tabPanel("Série Temporal",
                                     fluidRow(column(3,
                                                     selectInput("Ativo1",
                                                                 strong("Escolha um ativo:"),
                                                                 choices=c(names(BancoDeDados_Acoes[-1])))),
                                         column(9,
                                                plotlyOutput("PlotAtivo1", height = 600)))),
                            ##
                            tabPanel("Boxplot",
                                     fluidRow(column(3, 
                                                     selectInput("Ativo2", 
                                                                 strong("Escolha um ativo:"), 
                                                                 choices=c(names(BancoDeDados_Acoes[-1])))),
                                              
                                              ##Problema aqui: tive que mudar de plotly para plot                   
                                              column(9,
                                                plotOutput("PlotAtivo2", height = 600))))
                            
                            ##
                            
                        )# barra de navegacao interna
                    ),# barra de navegacao superior (Dados do Participante)
               
               tabPanel("Menu2",
                        tabsetPanel(
                          ##
                          tabPanel("SubMenu2.1",
                                   fluidRow(column(9,
                                                   leafletOutput("plot2", height = 600)))),
                          tabPanel("SubMenu2.2",
                                   fluidRow(column(9,
                                                   plotlyOutput("plot3", height = 600))))
                          
                          
                        )# barra de navegacao interna
               )# barra de navegacao superior (Dados da Escola)
               
    )#navbarPage
)#fluidPage
)#shinyUI

