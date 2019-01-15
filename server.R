server <- function(input, output, session){
  
  army <- reactiveValues(value = integer())
  
  samarianInfoServer(input,output,session,army = "Kukulkani")
  samarianInfoServer(input,output,session,army = "Forsaken")
  samarianInfoServer(input,output,session,army = "C.O.R.E")
  samarianInfoServer(input,output,session,army = "Dragyri")
  samarianInfoServer(input,output,session,army = "Outcasts")
  samarianInfoServer(input,output,session,army = "Brood")
  samarianInfoServer(input,output,session,army = "Skarrd")
  
  observeEvent(input$army_selection,{
    updatePickerInput(session, "subfaction_selection",choices = faction.Data[[input$army_selection]][['sub.Faction']])
    army$value <- 0
  })
  
  output$pointTotal <- renderText({ 
    army$value
  })
  
  observeEvent(input$add1, {
    army$value <- army$value + 1     # if the add button is clicked, increment the value by 1 and update it
  })
  
  observeEvent(input$sub1, {
    army$value <- army$value - 1  # if the sub button is clicked, decrement the value by 1 and update it
  })
  observeEvent(input$reset, {
    army$value <- 0                     # if the reset button is clicked, set the counter value to zero
  })
  
  selectedArmy <- reactive({
    req(input$army_selection)
    req(input$subfaction_selection)
    
    currentArmy <- faction.Df %>%
      dplyr::filter(grepl(input$army_selection, Faction)) %>%
      dplyr::filter(grepl(paste0("Unaligned|",input$subfaction_selection), Subfaction)) %>%
      mutate(Number = "0") %>%
      group_by(Faction) %>%
      arrange(Subfaction,Amount)
    
    return(currentArmy)
  })
  
  output$ArmyTable <- renderDataTable(isolate(selectedArmy()), selection = 'none',
                                      rownames = FALSE,
                                      editable = TRUE)
  
  armyTable_proxy <- dataTableProxy('ArmyTable')
  
  observe({
    replaceData(armyTable_proxy, selectedArmy(), rownames = FALSE)
  })
  
}