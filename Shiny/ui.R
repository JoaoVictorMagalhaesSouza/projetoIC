library(shiny)
file("server.R")
ui <- basicPage(
  "Criação Dashboards Estatísticos Através da Análise de Dados da BOVESPA"
)
shinyApp(ui,server)
