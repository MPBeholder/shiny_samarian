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
  
  selectedArmy <- reactive({
    req(input$army_selection)
    req(input$subfaction_selection)
    #req(input$army_value)
    
    validate(
      need(try(input$army_value != ""), "Please select an army point amount!")
    )
    
    session$sendCustomMessage('unbind-DT', 'ArmyTable') #TAG UNBIND
    
    currentArmy <- faction.Df %>%
      dplyr::filter(grepl(input$army_selection, Faction)) %>%
      dplyr::filter(grepl(paste0("Unaligned|",input$subfaction_selection), Subfaction))  %>%
      mutate(Allotment = case_when(
        Amount == "C" ~ 1,
        Amount == "*" ~ 1,
        TRUE ~ (as.numeric(Amount) * if_else(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500)))
      )) %>%
      group_by(Name) %>%
      mutate(Number = paste0('<select id = "',Name,'" class="armyCount">',
                             paste0('<option value="',seq(0,Allotment),'">',seq(0,Allotment),'</option>',collapse = ""),' </select>')) %>%
      ungroup() %>%
      group_by(Faction) %>%
      mutate(Amount = factor(Amount, levels = c("C","1","2","3","4","5","6","*"))) %>%
      arrange(Subfaction, Amount) %>%
      ungroup() %>%
      group_by(Name) %>%
      mutate(nameTooltip = HTML('<button id="',Name,'" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange(&quot;display_card&quot;,  this.id + &quot;_&quot; + Math.random())">Show Card</button>')) %>%
      dplyr::select(Faction,Subfaction,Name,nameTooltip,Cost,Amount,Allotment,Number)
    
    session$sendCustomMessage('reset-tooltip', 'temp') 
    
    return(currentArmy)
  })
  
  output$ArmyTable <- renderDataTable((selectedArmy()), selection = 'none',
                                      rownames = FALSE,
                                      escape = FALSE,
                                      editable = FALSE,
                                      extensions = c("Scroller"),
                                      
                                      options = list(dom = 't',
                                                     paging = FALSE,
                                                     list(targets = {c(6)},
                                                          orderable = FALSE),
                                                     scrollY = '500px',
                                                    #  rowCallback = JS('function(row, data) {
                                                    #      $(row).mouseenter(function(){
                                                    #          var hover_index = $(this)[0]._DT_RowIndex
                                                    #          /* console.log(hover_index); */
                                                    #          Shiny.onInputChange("hoverIndexJS", hover_index);
                                                    #     });
                                                    # }'),
                                                     preDrawCallback = JS('function() {
                                                             Shiny.unbindAll(this.api().table().node());}'),
                                                     drawCallback = JS('function() {
                                                        Shiny.bindAll(this.api().table().node());} '),
                                                     columnDefs = list(list(visible=FALSE, targets=c(NULL))),
                                                    list(
                                                      targets=2,render=DT::JS(
                                                        'function(data,row,type,meta) {
                                                        return "<a class=\'ItemsTooltip\' href=\'www.example.com\' target=\'_blank\'><img class=\'imgTooltip\' src=\'https://i.stack.imgur.com/uSSEu.jpg\'/>" +
                                                        data + "</a>";
}'
    ))
                                      ))
  
  output$currentValue <- renderText({
    req(input$army_value)
    val <- 0

    for (name in selectedArmy()[["Name"]]){
      val <- val + as.numeric(input[[name]]) * as.numeric(selectedArmy()$Cost[which(selectedArmy()[["Name"]] == name)])
    }
    
    if (val <= as.numeric(input$army_value)) {
      textString <- paste0('Current Points: ',val,"/",input$army_value,'</br>Points Remaining: ',as.numeric(input$army_value) - val)
    } else {
      textString <-HTML(paste0('Current Points: <font color=\"#FF0000\">',val,"/",input$army_value,'</font></br>You have too many models!'))
    }
    
    return(textString)
  })
  
  observeEvent(input$display_card,{
    #alert(strsplit(input$display_card, "_")[[1]][1])
    displayedCard <- trimws(strsplit(input$display_card, "_")[[1]][1], which = c("both"))#strsplit(input$display_card, "_")[[1]][1]
    #trimws(strsplit(input$display_card, "_")[[1]][1], which = c("both"))
    showModal(modalDialog(
      size = "l",
      HTML(paste0("<h4>Stat Cards for: ",displayedCard,"</h4>")),
      HTML("<center>"),
      HTML(paste0('<img src = "stat_cards/',displayedCard,'.png">')),
      HTML("</center>")
    ))
  })
  
}