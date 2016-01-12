source("PowerballOdds.R")
shinyServer(function(input, output) {
  # Reactive expression to compose a data frame containing all of the values
  evDollar <- reactive({
    powerplayFrac = input$powerplayFrac
    lastJackpotSeq = rev(seq(input$lastJackpot[1],input$lastJackpot[2], by=50))
    #lastJackpotSeq = c(input$lastJackpot)
    jackpotAddedSeq = seq(input$jackpotAdded[1],input$jackpotAdded[2], by=50)
    evDollar = getEVDollar(lastJackpotSeq, jackpotAddedSeq, input$powerplayFrac/100, input$yield30Yr/100)
    evDollar
  })
  evValues <- reactive({
    print(getEVPlot(evDollar()))
  })
  
  splitValues <- reactive({
    powerplayFrac = input$powerplayFrac
    lastJackpotSeq = rev(seq(input$lastJackpot[1],input$lastJackpot[2], by=50))
    #lastJackpotSeq = c(input$lastJackpot)
    jackpotAddedSeq = seq(input$jackpotAdded[1],input$jackpotAdded[2], by=50)
    plotSplits(evDollar(),lastJackpotSeq, jackpotAddedSeq)
  })
  
  output$evPlot <- renderPlot({
    evValues()
  })
  
  output$splitPlot <- renderPlot({
    splitValues()
  })
})