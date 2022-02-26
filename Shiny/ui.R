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
                          #h2("Motivações"),
                          sidebarLayout(
                            sidebarPanel(
                              img(src="b3.png",width="100%"),
                              #icon("info"),
                              #p(style = "text-align: justify;","Diante das problemáticas nos diversos âmbitos
                              #  sociais presenciados ultimamente e, partindo da premissa de que a educação básica brasileira não promulga boas
                              #  práticas de educação financeira, sentimentos a necessidade de promover uma ideia
                              #  que fomentasse, auxiliasse e consolidasse o estudo acerca do Mercado Financeiro,
                              #  que é de extrema importância e bastante impactante no processo de formação do ser."),
                              br(),
                              #p(style = "text-align: justify;","Além disso, também agregamos à essa causa o ideal de somar e impulsionar
                              #  o campo da Ciência de Dados no Mercado Financeiro Brasileiro, uma vez que os dados são muito incipientes e incompletos, formando assim uma carência
                              #  muito grande de Bancos de Dados e ",em("dashboards"),"nacionais nesse setor.
                              #  "),
                              
                            ),
                            
                            mainPanel(
                              h2("Projeto: Análise dos Ativos da B",tags$sup("3")),
                              #icon("check"),
                              br(),
                              p(style = "text-align: justify;", style = "font-size:25px;","Propomos com este trabalho, 
                              uma maneira prática e objetiva para visualizar de forma interativa 
                              e comparativa a evolução dos preço de ativos da Bolsa de Valores de 
                              São Paulo. Este", em("Dashboard"), "foi produzido como resultado do 
                              trabalho de Iniciação Científica voluntária do aluno de graduação", 
                                strong("João Victor M. Souza"), "orientado pelo professor", 
                                strong("Fernando de Souza Bastos"), "(vide aba 'Desenvolvedores')."
                                #Reunimos um Banco de Dados contendo os valores do 
                                #  Preço Ajustado das ações mais negocidadas da BOVESPA desde Janeiro de 2016 até o dia atual. O Banco de Dados
                                #  é atualizado todo dia às 06:00 a.m, no horário de Brasília, de acordo com os dados fornecidos pela API do Yahoo Finance.
                              ),
                              br(),
                              p(style = "text-align: justify;", style = "font-size:25px;",strong("Observação Importante:"), 
                                "Este trabalho não tem o objetivo de sugerir a compra de nenhum ativo da bolsa. ")
                              #p(style = "text-align: justify;","Sendo assim, nós produzimos
                              #  séries temporais e",em("boxplots")," de acordo com os filtros que o usuário queira aplicar, de modo a facilitar
                              #  e guiar sua busca. Dessa forma, buscamos disseminar a ideia de levar essas métricas que embasam a análise de dados
                              #  de forma que os usuários não precisem saber fazê-las via alguma linguagem de programação, bastando apenas possuir o
                              #  interesse de analisá-las via ",em("dashboards"),"."),
                              
                              
                            )
                            
                          )
                          
                          
                 ),
                
                tabPanel("Ativos",
                        tabsetPanel(

                            tabPanel("Série Temporal",icon = icon("chart-line"),
                                     fluidRow(column(3,
                                                     selectInput("inAtivosSerie",
                                                                 strong("Escolha um ou mais ativos:"),
                                                                multiple = TRUE,
                                                                 choices=c(acoesDisponiveis))),
                                                     column(9,
                                                            dygraphOutput("outPlotAtivos", height = 500))
                              
                                                     
                                     )
                                                     ),
                                              
                            tabPanel("Comparação com a B3",icon = icon("chart-line"),
                                     fluidRow(column(3,
                                                     selectInput("inAtivoCompB3",
                                                                 strong("Escolha um ou mais ativos:"),
                                                                 multiple = TRUE,
                                                                 choices=noB3)),
                                              column(9,
                                                     dygraphOutput("outAtivoCompB3", height = 500)))),
                            
                            
                            tabPanel("Boxplot", icon = icon("bold"),
                                     fluidRow(column(3, 
                                                     selectInput("inBoxAnualAtivo", 
                                                                 strong("Escolha um ativo:"), 
                                                                 choices=c(acoesDisponiveis))
                                                     
                                                     # selectInput("inBoxAnualAno",
                                                     #             strong("Escolha um ano:"),
                                                     #             choices = c("Todos",anos))
                                                     ),
                                              
                                                                
                                              column(9,
                                                     plotlyOutput("outBoxplotAtivo", height = 600))))
                            
                            
                            
                        )
                    ),
                 tabPanel("Setorial",
                          tabsetPanel(
                            tabPanel("Setorial Filtrado",icon = icon("chart-line"),
                                     fluidRow(column(9,
                                                     selectInput("inSetorFilt",
                                                                 strong("Escolha o setor que deseja monitorar: "),
                                                                 choices = listaSetores,
                                                                 selected = NULL,
                                                                 multiple = FALSE
                                                     )
                                     ),
                                     
                                     
                                     column(9,
                                            uiOutput("outSetorFilt")))
                                     
                            )
                            
                          )
                          ),
                tabPanel("Candlesticks",
                         tabsetPanel(
                           tabPanel("Candles",icon = icon("chart-line"),
                                    fluidRow(column(3,
                                                    selectizeInput("inCandles",
                                                                strong("Escolha os ativos que deseja monitorar: "),
                                                                choices = c(acoesDisponiveis),
                                                                selected = "B3SA3.SA",
                                                                multiple = FALSE
                                                                
                                                    )
                                    ),
                                    
                                    br(),
                                    column(9,
                                           plotlyOutput("outCandles",height = 500)))),
                                    
                           
                           
                         )
                ),
                
                
                
                 
               tabPanel("Fronteira de Markowitz",
                        tabsetPanel(
                          tabPanel("O que é", icon = icon("question"),
                                   sidebarLayout(
                                     sidebarPanel(
                                   em(h2("Harry Markowitz")),
                                   
                                     
                                       icon("info"),
                                       p(style = "text-align: justify;",strong("Harry Max Markowitz")," é um importante economista de origem estadunidense. Formado em Economia
                                         pela Universidade de Chicago, Harry trouxe grandes contribuições para a área, ganhando até mesmo o Prêmio Nobel de Economia de 1990.
                                         Dentre seus longos anos de estudo, elaborou a chamada",strong("Teoria Moderna do Portfólio (1952),"), "um dos principais objetos de interesse deste projeto."),
                                       br()),
                                       
                                       
                                     
                                     
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
                                       br(),
                                       h4("O que é o ",strong("risco")," ?"),
                                       br(),
                                       p(style = "text-align: justify;","O cálculo do risco, seguindo a Teoria do Portfólio de Markowitz, é calculada utilizando a variância de todos os ativos presentes na carteira acrescida do cálculo
                                         da covariância entre os pares de ativos da carteira. Logo, se tivermos uma carteira com apenas dois ativos A e B, termos o risco dado por: "),
                                       br(),
                                       p(style = "text-align: center;",strong("Risco(A,B) = var(A) + var(B) + 2*cov(A,B)")),
                                       br(),
                                       p(style = "text-align: justify;","Generalizando, o risco para uma carteira com N ativos é expresso da seguinte forma:"),
                                       br(),
                                       p(style = "text-align: center; color: red;",strong("Risco(A,B,C,...,N) = var(A) + var(B) + var(C) + ... + var(N) + 2*cov(A,B) + 2*cov(A,C) + ... + 2*cov(A,N) + ... + 2*cov(N-1,N)")),
                                       br(),
                                       p(style = "text-align: justify;","Estatisticamente, a covariância pode ser escrita também em função da correlação entre os ativos. Dessa forma, se os ativos apresentarem uma correlação forte e negativa,
                                         isso significa que quando um ativo for desvalorizado, o outro não-necessariamente será desvalorizado também, minimizando o risco do investimento."),
                                       br(),
                                       h4("O que é o ",strong("retorno esperado")," ?"),
                                       br(),
                                       p(style = "text-align: justify;","O retorno esperado é medido a partir do cálculo da soma dos valores esperados dos ativos da carteira, considerando seu comportamento temporalmente, ou seja, levando em conta seu desempenho ao longo do tempo.
                                         Sendo assim, se tivermos uma carteira com dois ativos X e Y, nosso retorno esperado será: "),
                                       br(),
                                       p(style = "text-align: center;",strong("Retorno esperado(A,B) = E(A) + E(B)")),
                                       br(),
                                       p(style = "text-align: justify;","De modo geral, o retorno esperado pode ser escrito, para uma carteira com N ativos, como: "),
                                       br(),
                                       p(style = "text-align: center; color: red;",strong("Retorno esperado(A,B,...,N) = E(A) + E(B) + ... + E(N)")),
                                       br(),
                                       p(style = "text-align: justify;","Dessa forma, podemos entender que se um investimento tem 50% de chances de valorizar 20% e 50% de chances de desvalorizar 10%, o retorno esperado é de ",strong("5%")," pois: (20% x 50% + 10% x 50% = 5%)."),
                                       br()
                                       
                                       
                                     
                                       
                                       
                                       
                                     )
                                   
                                     
                                   )
                                   
                                   
                                   ),
                          tabPanel("Simular Portfólio", icon = icon("wallet"),
                                   sidebarLayout(
                                   column(9,
                                                   selectInput("inAtivosMark",
                                                               strong("Escolha os ativos do portfólio (mín. 2):"),
                                                               choices=c(acoesDisponiveis),
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
                                  style = "position:relative; left:150px"
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
                                    splitLayout(style = "botder 1px solid silver",cellWidths = c("50%","25%","50%"),strong(h3("Carteira de Melhor Índice Sharpe")),NULL,strong(h3("Carteira de Menor Risco"))), 
                                  splitLayout(style = "botder 1px solid silver",cellWidths = c("50%","25%","50%"),tableOutput("outTabelaMark"),NULL,tableOutput("outTabelaMark2")),
                                  style = "position:relative; left:150px"
                                  ),
                                br(),
                                  br(),
                                  br(),
                                  plotOutput("outMark3"),
                                  br(),
                                  br(),
                                  br(),
                                  plotlyOutput("outMark4")
                                  
                                  
                                  
                                 #fluidRow(column(offset = 5,width = 10, plotOutput("outMark3"))),
                                  # # fluidRow(column(offset = 5,width = 10, plotlyOutput("outMark4"))),
                                   
                                   
                                   )
                                  
                                   
                                   
                                 
                                   
                          )
                        
                        )
                        )
                 
               ),
               
               
               tabPanel("Predição de Valores",
                        tabsetPanel(
                          
                          tabPanel("Catboost predictor",icon = icon("chart-line"),
                                   fluidRow(column(3,
                                                   selectInput("inAtivoPredict",
                                                               strong("Escolha um ativo:"),
                                                               multiple = FALSE,
                                                               choices=c(acoesDisponiveis)))
                                            
                                            
                                            
                                            
                                   ),
                                   column(9,
                                   fluidRow(
                                          valueBoxOutput("outPredicao"),
                                   
                                          
                                   
                                          )),
                          ))),
              # 
              #   
              #    
               tabPanel("Desenvolvedores",
                        titlePanel("Desenvolvedores e Contatos"),
                        sidebarLayout(
                          
                          sidebarPanel(
                            fluidRow(
                              column(6, 
                                     #h4(strong("Desenvolvedor:")),
                                     img(src = "joao.png", height = 150, width = 150, align = "center",style = "border-radius:50%"),
                                     br(),
                                     em(strong("João Victor M. Souza")),
                                     br(),
                                     p(style = "text-align: justify;","Graduando em Ciência da Computação."),
                                     p(style = "text-align: justify;","Universidade
                                Federal de Viçosa - Campus UFV - Florestal."),
                                     br(),
                                     icon("at"),
                                     #em("   JoaoVictorMagalhaesSouza@gmail.com"),
                                     a(href = "JoaoVictorMagalhaesSouza@gmail.com",em("E-mail")),
                                     br(),
                                     icon("instagram"),
                                     a(href = "https://www.instagram.com/joaovictormagalhaessouza/",em("Instagram")),
                                     br(),
                                     icon("github"),
                                     a(href = "https://github.com/JoaoVictorMagalhaesSouza",em("GitHub")),
                                     br(),
                                     icon("briefcase"),
                                     a(href="https://joaovictormagalhaessouza.github.io/", em("Página Pessoal"))
                              ),
                              column(6, 
                                     img(src = "Fernando.jpg", height = 150, width = 150, align = "center",style = "border-radius:50%"),
                                     br(),
                                     em(strong("Fernando de Souza Bastos")),
                                     br(),
                                     p(style = "text-align: justify;","Doutor em Estatística."),
                                     p(style = "text-align: justify;","Professor da Universidade
                                Federal de Viçosa - Campus UFV - Florestal."),
                                     br(),
                                     icon("at"),
                                     a(href = "fernando.bastos@ufv.br",em("E-mail")),
                                     br(),
                                     icon("instagram"),
                                     a(href = "https://www.instagram.com/fsbmat/",em("Instagram")),
                                     br(),
                                     icon("github"),
                                     a(href = "https://github.com/fsbmat-ufv",em("GitHub")),
                                     br(),
                                     icon("briefcase"),
                                     a(href="https://fsbmat-ufv.github.io/", em("Página Pessoal"))
                              )
                            ),
                            
                            
                            width = 12),
                          
                          mainPanel()
                        )
                        
                        
                        
               )
              
               
               
               
)#fluidPage

)#shinyUI

)


