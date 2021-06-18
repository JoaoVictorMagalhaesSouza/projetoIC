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
               tabPanel("Ativo Único",
                        tabsetPanel(
                            tabPanel("Série Temporal",icon = icon("chart-line"),
                                     fluidRow(column(3,
                                                     selectInput("inAtivoSerie",
                                                                 strong("Escolha um ativo:"),
                                                                 choices=c(names(BancoDeDados_Acoes[-1])))),
                                         column(9,
                                                plotlyOutput("outPlotAtivo", height = 500)))),
                            ##
                            tabPanel("Boxplot", icon = icon("bold"),
                                     fluidRow(column(3, 
                                                     selectInput("inAtivoBox", 
                                                                 strong("Escolha um ativo:"), 
                                                                 choices=c(names(BancoDeDados_Acoes[-1])))),
                                              
                                              ##Problema aqui: tive que mudar de plotly para plot                   
                                              column(9,
                                                plotOutput("outBoxplotAtivo", height = 600))))
                            
                            ##
                            
                        )# barra de navegacao interna
                    ),# barra de navegacao superior (Dados do Participante)
               
               tabPanel("Vários Ativos",
                        tabsetPanel(
                          ##
                          tabPanel("Série Temporal",
                                   fluidRow(column(9,
                                                   selectInput("inSetor",
                                                               strong("Escolha o setor que deseja monitorar: "),
                                                               choices = listaSetores,
                                                               multiple = FALSE,
                                                               ),
                                                                                                   ),
                                                    
                                      
                                          column(9,
                                                 uiOutput("outPlotVariosAtivos")))),
                                            
                          tabPanel("SubMenu2.2",
                                   fluidRow(column(9,
                                                   plotlyOutput("plot3", height = 600))))
                          
                          
                        )# barra de navegacao interna
               )# barra de navegacao superior (Dados da Escola)
               
)#fluidPage
)#shinyUI
)


