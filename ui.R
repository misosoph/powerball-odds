shinyUI(fluidPage(
  titlePanel("Powerball EV Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Calculate Powerball Expected Value based on the values below. The chart represents the expected value of each dollar spent given the previous jackpot and current jackpot."),
      
      sliderInput("lastJackpot", 
                  label = "Last Jackpot (mm):",
      #            min = 50, max = 3000, value = 950, step=50, animate=FALSE),
                  min = 50, max = 3000, value = c(900, 1100), step=50, animate=FALSE),
      
      helpText("Last Jackpot is the value of the previous unwon jackpot rolled into the current jackpot."),
      
      sliderInput("jackpotAdded", 
                  label = "Jackpot Added (mm):",
                  min = 0, max = 1000, value = c(0, 500), step=50, animate=FALSE),
      
      helpText("Jackpot Added is the additional jackpot since the last drawing. This is a function of the revenue and future value of the annuity."),
      sliderInput("powerplayFrac", 
                  label = "Power Play Sales %:",
                  min = 0, max = 20, value = 6, step=.1),
      helpText("The sales percent of revenue attributed to the Powerplay option. Typically from 4-7%"),
      
      sliderInput("yield30Yr", 
                  label = "30 Yr yield:",
                  min = 0.0, max = 10.0, value = 2.98, step= 0.01),
      helpText("The yield used to calculate the advertised jackpot amount. This is the future value of the 30 yr annuity.")
    ),
    mainPanel(plotOutput('evPlot'), plotOutput('splitPlot'))
  )
))