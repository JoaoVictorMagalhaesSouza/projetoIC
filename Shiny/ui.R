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
                                                                 choices = c("Todos",anos),
                                                     ),
                                                     
                                                     fluidRow(column(9,
                                                            uiOutput("outAno")  )  
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
                tabPanel("Setorial",
                         tabsetPanel(
                           tabPanel("Setorial Completo",icon = icon("chart-line"),
                                    fluidRow(column(9,
                                                    selectInput("inSetorComp",
                                                                strong("Escolha o setor que deseja monitorar: "),
                                                                choices = listaSetores,
                                                                selected = NULL,
                                                                multiple = FALSE,
                                                    ),
                                    ),
                                    
                                    div(
                                     fixedRow( 
                                    column(9,
                                           plotlyOutput("outSetorComp")),
                                  #style = "position:relative; top:-50px ; left: 100px"
                                    ))
                                    
                           )),
                           tabPanel("Setorial Filtrado",icon = icon("chart-line"),
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
                                    
                           )
                           
                         )
                         ),
                
              tabPanel("Fronteira de Markowitz",
                       tabsetPanel(
                         tabPanel("O que é", icon = icon("question"),
                                  
                                  em(h2("Harry Markowitz")),
                                  sidebarLayout(
                                    sidebarPanel(
                                      icon("info"),
                                      p(style = "text-align: justify;",strong("Harry Max Markowitz")," é um importante economista de origem estadunidense. Formado em Economia
                                        pela Universidade de Chicago, Harry trouxe grandes contribuições para a área, ganhando até mesmo o Prêmio Nobel de Economia de 1990.
                                        Dentre seus longos anos de estudo, elaborou a chamada",strong("Teoria Moderna do Portfólio (1952),"), "um dos principais objetos de interesse deste projeto."),
                                      br(),
                                      
                                      
                                    ),
                                    
                                    mainPanel(
                                      h2("Teoria Moderna do Portfólio"),
                                      icon("info"),
                                      br(),
                                      p(style = "text-align: justify;","A Teoria Moderna do Portfólio, também conhecida como Fronteira Eficience de Markowitz ou Portfólio Eficiente de Markowitz,
                                      consiste em uma teoria na qual objetiva-se otimizar a carteira de investimentos de um investidor analisando dois fatores: risco e retorno.
                                      Grosseiramente, é possível analisar combinações de proporções de risco e retorno dos ativos da carteira, de maneira a escolher combinações de investimento que sejam
                                      mais rentáveis e menos arriscadas.
                                "),
                                      br(),
                                      p(style = "text-align: justify;","Essa análise permite que guiar o investidor de forma que, correndo o mesmo risco para diferente distribuições de recursos de investimento,
                                        ele possa escolher a que lhe trará o maior retorno. Paralelamente, a teoria traz, interessantemente, o fato de que a carteira deve ser montada com ações de diferentes setores,
                                        de modo que apresentem uma correlação forte e negativa (inversamente proporcionais), visando sempre otimizar mas, ao mesmo tempo, minimizar as possíveis perdas.
                                        Logo, é possível observar que a Fronteira Eficiente consiste em todos os pontos onde o retorno é máximo e estes pontos, por sua vez, variam de acordo com o risco que o investidor
                                        está disposto a correr."),
                                      
                                      
                                    )
                                    
                                  )
                                  
                                  
                                  ),
                         tabPanel("Simular Portfólio", icon = icon("wallet"),
                                  sidebarLayout(
                                  column(9,
                                                  selectInput("inAtivosMark",
                                                              strong("Escolha os ativos do portfólio (mín. 2):"),
                                                              choices=c(names(BancoDeDados_Acoes[-1])),
                                                              multiple = TRUE
                                                  ),
                                                  actionButton(inputId="buttonOk", label="Gerar", icon("paper-plane"), 
                                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                  
                                 
                                                  ),
                                  mainPanel(
                                  # div(
                                  # h2("Portfólio Eficiente de Markowitz"),
                                  # #style = "; resize:horizontal; overflow: hidden; position:relative; margin-left: 400px; margin-bottom: 60px",
                                  # style = "position:relative; top:-130px ; left:430px;",
                                  # ),
                                  fluidRow(column(offset = 5,width = 12,plotlyOutput("outCartMark"))),
                                  br(),
                                  br(),
                                  br(),
                                 # #  
                                 #    fluidRow(column(plotlyOutput("outMark1"),width = 6),
                                 #             column(plotlyOutput("outMark2"),width = 6),
                                 #             column(plotOutput("outMark3"),width = 6)
                                 #             
                                 #             ),
                                 div(
                                 splitLayout(style = "border 1px solid silver",cellWidths = c("50%","25%","50%"),plotlyOutput("outMark1"),NULL,plotlyOutput("outMark2")),
                                 style = "position:relative; left:150px",
                                 ),
                                 br(),
                                 br(),
                                 br(),
                                 # div(
                                 # h3("Percentual de Distribuição do Melhor Índice Sharpe"),
                                 # 
                                 # ),
                                 br(),
                                 div(
                                 splitLayout(style = "botder 1px solid silver",cellWidths = c("50%","25%","50%"),plotOutput("outMark3"),NULL,tableOutput("outTabelaMark")),
                                 style = "position:relative; left:150px",
                                 ),
                                 
                                 
                                 
                                  #fluidRow(column(offset = 5,width = 10, plotOutput("outMark3"))),
                                 # # fluidRow(column(offset = 5,width = 10, plotlyOutput("outMark4"))),
                                  
                                  
                                  ),
                                 
                                  
                                  
                                  
                                  
                         )
                         
                       )
                       )
                
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
                        
                        
                        
               )
               
               
               
               
)#fluidPage

)#shinyUI

)


