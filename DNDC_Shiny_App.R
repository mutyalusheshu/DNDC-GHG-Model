
library(shiny)

# Define server logic
server <- function(input, output) {
  simulate_dndc <- function(days, C_input, temp, moisture, N_input, k_decomp = 0.01, k_nitr = 0.05, k_denitr = 0.02) {
    SOC <- numeric(days)
    NH4 <- numeric(days)
    NO3 <- numeric(days)
    N2O <- numeric(days)

    SOC[1] <- 10000
    NH4[1] <- N_input
    NO3[1] <- 0

    for (t in 2:days) {
      f_temp <- exp(0.0693 * (temp - 20))
      f_moist <- ifelse(moisture > 0.8, 0.9, 1.0)

      decomposed_C <- k_decomp * SOC[t-1] * f_temp * f_moist
      SOC[t] <- SOC[t-1] - decomposed_C + C_input

      nitrified <- k_nitr * NH4[t-1]
      denitrified <- k_denitr * NO3[t-1] * f_moist

      NH4[t] <- NH4[t-1] - nitrified + (0.01 * decomposed_C)
      NO3[t] <- NO3[t-1] + nitrified - denitrified
      N2O[t] <- N2O[t-1] + 0.6 * denitrified
    }

    return(data.frame(Day = 1:days, SOC, NH4, NO3, N2O))
  }

  output$plot <- renderPlot({
    sim_data <- simulate_dndc(
      days = input$days,
      C_input = input$C_input,
      temp = input$temp,
      moisture = input$moisture,
      N_input = input$N_input
    )

    plot(sim_data$Day, sim_data$N2O, type = "l", col = "blue", 
         main = "Simulated N₂O Emissions", ylab = "mg N₂O-N/kg", xlab = "Day")
  })
}

# Define UI
ui <- fluidPage(
  titlePanel("Simplified DNDC GHG Model"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("days", "Simulation Days:", min = 30, max = 365, value = 180),
      numericInput("C_input", "Daily Carbon Input (mg/kg):", 5),
      numericInput("temp", "Average Temperature (°C):", 22),
      numericInput("moisture", "Soil Moisture (fraction 0-1):", 0.7),
      numericInput("N_input", "Initial NH₄⁺ Input (mg/kg):", 80)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Run the app
shinyApp(ui = ui, server = server)
