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
                          h2("Motivações"),
                          sidebarLayout(
                            sidebarPanel(
                              icon("info"),
                              p(style = "text-align: justify;","Diante das problemáticas nos diversos âmbitos
                                sociais presenciados ultimamente e, partindo da premissa de que a educação básica brasileira não promulga boas
                                práticas de educação financeira, sentimentos a necessidade de promover uma ideia
                                que fomentasse, auxiliasse e consolidasse o estudo acerca do Mercado Financeiro,
                                que é de extrema importância e bastante impactante no processo de formação do ser."),
                              br(),
                              p(style = "text-align: justify;","Além disso, também agregamos à essa causa o ideal de somar e impulsionar
                                o campo da Ciência de Dados no Mercado Financeiro Brasileiro, uma vez que os dados são muito incipientes e incompletos, formando assim uma carência
                                muito grande de Bancos de Dados e ",em("dashboards"),"nacionais nesse setor.
                                "),
                              
                            ),
                            
                            mainPanel(
                              h2("Nossa Proposta"),
                              icon("check"),
                              br(),
                              p(style = "text-align: justify;","Propomos com este trabalho, então, uma maneira mais prática,
                                intuitiva e em mais alto nível uma forma de visualizar",em("dashboards"),"interativos acerca de pregões
                                da Bolsa de Valores de São Paulo. Nós reunimos um Banco de Dados contendo os valores do 
                                Preço Ajustado das ações mais negocidadas da BOVESPA desde Janeiro de 2016 até o dia atual. O Banco de Dados
                                é atualizado todo dia às 06:00 a.m, no horário de Brasília, de acordo com os dados fornecidos pela API do Yahoo Finance.
                                "),
                              br(),
                              p(style = "text-align: justify;","Sendo assim, nós produzimos
                                séries temporais e",em("boxplots")," de acordo com os filtros que o usuário queira aplicar, de modo a facilitar
                                e guiar sua busca. Dessa forma, buscamos disseminar a ideia de levar essas métricas que embasam a análise de dados
                                de forma que os usuários não precisem saber fazê-las via alguma linguagem de programação, bastando apenas possuir o
                                interesse de analisá-las via ",em("dashboards"),"."),
                              
                              
                            )
                            
                          )
                          
                          
                 ),
                
                tabPanel("Ativo Único",
                        tabsetPanel(
                            tabPanel("Série Temporal Geral",icon = icon("chart-line"),
                                     fluidRow(column(3,
                                                     selectInput("inAtivoSerie",
                                                                 strong("Escolha um ativo:"),
                                                                 choices=c(names(BancoDeDados_Acoes[-1])))),
                                         column(9,
                                                plotlyOutput("outPlotAtivo", height = 500)))),
                            
                            tabPanel("Série Temporal Anual",icon = icon("chart-line"),
                                     fluidRow(column(3,
                                                     selectInput("inAtivoAnual",
                                                                 strong("Escolha um ativo:"),
                                                                 choices=c(names(BancoDeDados_Acoes[-1]))),
                                                     
                                                     selectInput("inAnoAnual",
                                                                 strong("Escolha um ano:"),
                                                                 choices = anos
                                                                 )
                                                     ),
                                              column(9,
                                                     plotlyOutput("outAtivoAnual", height = 500)))),
                            ##
                            tabPanel("Série Temporal Bimestral",icon = icon("chart-line"),
                                     fluidRow(column(9,
                                                     selectInput("inAtivoBimestral",
                                                                 strong("Escolha um ativo:"),
                                                                 choices=c(names(BancoDeDados_Acoes[-1]))),
                              
                                                     
                                                     selectInput("inAno",
                                                                 strong("Escolha um ano:"),
                                                                 choices = anos
                                                     ),
                                                     column(9,
                                                            uiOutput("outAno")    
                                                     ),
                                                     
                                                     ),
                                              )),
                            tabPanel("Comparação com a B3",icon = icon("chart-line"),
                                     fluidRow(column(3,
                                                     selectInput("inAtivoCompB3",
                                                                 strong("Escolha um ativo:"),
                                                                 choices=noB3)),
                                              column(9,
                                                     plotlyOutput("outAtivoCompB3", height = 500)))),
                            
                            tabPanel("Boxplot Geral", icon = icon("bold"),
                                     fluidRow(column(3, 
                                                     selectInput("inAtivoBox", 
                                                                 strong("Escolha um ativo:"), 
                                                                 choices=c(names(BancoDeDados_Acoes[-1])))),
                                              
                                              ##Problema aqui: tive que mudar de plotly para plot                   
                                              column(9,
                                                plotOutput("outBoxplotAtivo", height = 600)))),
                            
                            tabPanel("Boxplot Anual", icon = icon("bold"),
                                     fluidRow(column(3, 
                                                     selectInput("inBoxAnualAtivo", 
                                                                 strong("Escolha um ativo:"), 
                                                                 choices=c(names(BancoDeDados_Acoes[-1]))),
                                                     
                                                     selectInput("inBoxAnualAno",
                                                                 strong("Escolha um ano:"),
                                                                 choices = anos)
                                                     ),
                                              
                                                                
                                              column(9,
                                                     plotOutput("outBoxAnual", height = 600))))
                            
                            
                            
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
                            em("   JoaoVictorMagalhaesSouza@gmail.com"),
                            br(),
                            icon("instagram"),
                            a(href = "https://www.instagram.com/joaovictormagalhaessouza/",em("joaovictormagalhaessouza")),
                            br(),
                            icon("github"),
                            a(href = "https://github.com/JoaoVictorMagalhaesSouza",em("joaovictormagalhaessouza")),
                            br(),
                            icon("briefcase"),
                            a(href="https://joaovictormagalhaessouza.github.io/", em("https://joaovictormagalhaessouza.github.io/")),
                            
                            
                            ),
                          
                          mainPanel(
                            
                          )
                          
                        )
                        
                        
                        
               ),
               tabPanel("Bug Report",
                        tabsetPanel(
                          tabPanel("Reportar um problema",icon = icon("bug"),
                                   fluidRow(column(9,
                                                   textInput("inBugNome","Nome"),
                                                   textInput("inBugEmail","Email"),
                                                   selectInput("inBugSecao",strong("Seção de ocorrência"),choices = secoes),
                                                   dateInput("inBugData",label="Data do ocorrido",format = "dd-mm-yyyy"),
                                                   textAreaInput("inBugDescrip",label="Descrição do problema",width="400px",rows=4,cols = 9),
                                                   fileInput("inBugFile",label="Anexar screenshot (importante)")
                                   ),
                                   
                                   
                                   column(9,
                                          plotlyOutput("outSetorComp"))),
                                   
                          )
                        )
               )
               
               
               
)#fluidPage

)#shinyUI

)


