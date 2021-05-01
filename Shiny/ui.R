library(shiny)
file("server.R")
ui <- fluidPage(
  "Criação Dashboards Estatísticos Através da Análise de Dados da BOVESPA"
)
shinyApp(ui,server)
