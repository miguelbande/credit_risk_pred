# app.R
library(shiny)

# Cargamos el modelo entrenado previamente 
modelo <- readRDS("modelo_inter_step.rds")  

ui <- fluidPage(
  titlePanel("Predicción de Impago de Crédito"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Home", "Tipo de vivienda", choices = c("owner", "others_h")),
      selectInput("Marital", "Estado civil", choices = c("married", "others_m")),
      selectInput("Records", "Historial negativo", choices = c("no", "yes")),
      selectInput("Job", "Tipo de empleo", choices = c("fixed", "others_j")),
      
      numericInput("Seniority", "Antigüedad laboral (años)", value = 5, min = 0),
      numericInput("Time", "Duración del préstamo (meses)", value = 36),
      numericInput("Age", "Edad", value = 35),
      numericInput("Expenses", "Gastos mensuales (€)", value = 500),
      numericInput("Income", "Ingresos mensuales (€)", value = 1500),
      numericInput("Assets", "Valor de activos (€)", value = 5000),
      numericInput("Amount", "Monto solicitado (€)", value = 10000),
      numericInput("Price", "Precio del bien (€)", value = 15000),
      
      actionButton("predict", "Predecir")
    ),
    
    mainPanel(
      verbatimTextOutput("resultado")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    # Crear nuevo caso con log-transformaciones
    nuevo <- data.frame(
      Home = factor(input$Home, levels = c("owner", "others_h")),
      Marital = factor(input$Marital, levels = c("married", "others_m")),
      Records = factor(input$Records, levels = c("no", "yes")),
      Job = factor(input$Job, levels = c("fixed", "others_j")),
      
      log_Seniority = log(ifelse(input$Seniority > 0, input$Seniority, 1)),
      Time = input$Time,
      #Age = input$Age,
      log_Expenses = log(ifelse(input$Expenses > 0, input$Expenses, 1)),
      log_Income = log(ifelse(input$Income > 0, input$Income, 1)),
      log_Assets = log(ifelse(input$Assets > 0, input$Assets, 1)),
      log_Amount = log(ifelse(input$Amount > 0, input$Amount, 1)),
      log_Price = log(ifelse(input$Price > 0, input$Price, 1))
    )
    
    prob <- predict(modelo, newdata = nuevo, type = "response")
    
    output$resultado <- renderPrint({
      cat("Probabilidad de impago estimada:", round(prob, 4), "\n")
      if (prob > 0.5) {
        cat("❌ Crédito NO recomendado (riesgo elevado de impago)\n")
      } else {
        cat("✅ Crédito aprobado (riesgo bajo de impago)\n")
      }
    })
  })
}

shinyApp(ui = ui, server = server)
