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
      theme = shinytheme("flatly"),lang = "pt", title = "Análise da Bolsa de Valores de São Paulo",
      navbarPage(title="ANÁLISE DA BOVESPA",
                
                 tabPanel("Apresentação",
                          titlePanel("Motivações"),
                          sidebarLayout(
                            sidebarPanel(
                              icon("info"),
                              p(style = "text-align: justify;","Diante das problemáticas nos diversos âmbitos
                                sociais presenciados ultimamente e, partindo da premissa de que a educação básica brasileira não promulga boas
                                práticas de educação financeira, sentimentos a necessidade de promover uma ideia
                                que fometasse, auxiliasse e consolidasse o estudo acerca do Mercado Financeiro,
                                que é de extrema importância e bastante impactante no processo de formação do ser."),
                              br(),
                              p(style = "text-align: justify;","Além disso, também agregamos à essa causa o ideal de somar e impulsionar
                                o campo da Ciência de Dados no Mercado Financeiro Brasileiro, uma vez que há uma carência
                                muito grande dos Bancos de Dados e ",em("dashboards"),"nacionais nesse setor.
                                "),
                              
                            ),
                            
                            mainPanel(
                              
                            )
                            
                          )
                          
                          
                 ),
                
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
                            tabPanel("Comparação com a B3",icon = icon("chart-line"),
                                     fluidRow(column(3,
                                                     selectInput("inAtivoCompB3",
                                                                 strong("Escolha um ativo:"),
                                                                 choices=noB3)),
                                              column(9,
                                                     plotlyOutput("outAtivoCompB3", height = 500)))),
                            
                            tabPanel("Boxplot", icon = icon("bold"),
                                     fluidRow(column(3, 
                                                     selectInput("inAtivoBox", 
                                                                 strong("Escolha um ativo:"), 
                                                                 choices=c(names(BancoDeDados_Acoes[-1])))),
                                              
                                              ##Problema aqui: tive que mudar de plotly para plot                   
                                              column(9,
                                                plotOutput("outBoxplotAtivo", height = 600))))
                            
                            
                            
                        )
                    ),
                tabPanel("Setorial Completo",
                         tabsetPanel(
                           tabPanel("Série Temporal",icon = icon("chart-line"),
                                    fluidRow(column(9,
                                                    selectInput("inSetorComp",
                                                                strong("Escolha o setor que deseja monitorar: "),
                                                                choices = listaSetores,
                                                                selected = NULL,
                                                                multiple = FALSE,
                                                    ),
                                    ),
                                    
                                    
                                    column(9,
                                           plotlyOutput("outSetorComp"))),
                                    
                           )
                         )
                         ),
              
                
                 tabPanel("Setorial Filtrado",
                        tabsetPanel(
                          ##
                          tabPanel("Série Temporal",icon = icon("chart-line"),
                                   fluidRow(column(9,
                                                   selectInput("inSetorFilt",
                                                               strong("Escolha o setor que deseja monitorar: "),
                                                               choices = listaSetores,
                                                                selected = NULL,
                                                               multiple = FALSE,
                                                               ),
                                                                                                   ),
                                                    
                                      
                                          column(9,
                                                 uiOutput("outSetorFilt"))),
              
                                   ),
                                            
                          tabPanel("SubMenu2.2",
                                   fluidRow(column(9,
                                                   plotlyOutput("plot3", height = 600))))
                          
                          
                        )# barra de navegacao interna
               ),
               tabPanel("Sobre os Envolvidos",
                        titlePanel("Informações"),
                        sidebarLayout(
                          sidebarPanel(
                            h4(strong("Desenvolvedor:")),
                            img(src = "joao.png", height = 150, width = 150, align = "center"),
                            em(strong("João Victor M. Souza")),
                            br(),
                            p(style = "text-align: justify;","Aluno do curso de Ciência da Computação da Universidade
                               Federal de Viçosa - Campus Florestal."),
                            br(),
                            icon("at"),
                            em("   joao.souza5@ufv.br"),
                            br(),
                            icon("instagram"),
                            a(href = "https://www.instagram.com/joaovictormagalhaessouza/",em("joaovictormagalhaessouza")),
                            br(),
                            icon("github"),
                            a(href = "https://github.com/JoaoVictorMagalhaesSouza",em("joaovictormagalhaessouza")),
                            
                            
                            ),
                          
                          mainPanel(
                            
                          )
                          
                        )
                        
                        
                        
               )
               
               
)#fluidPage

)#shinyUI

)


