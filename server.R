server <- function(input, output, session){
  # Base Reactive Value(s) ---------------------------------
  army <- reactiveValues(value = integer())  

  # On Launch Modal ---------------------------------
  query_modal <- modalDialog(
    # Opening Modal
    title = "A Note from the Wasteland",
    HTML("This is a fan created army building app for the Cool Mini Or Not (CMON) game Dark Age.</br>All images contained within this app are copyrighted to CMON."),
    easyClose = F
  )

  showModal(query_modal)
  
  # Modular Components ---------------------------------
  samarianInfoServer(input,output,session,faction = "Kukulkani")
  samarianInfoServer(input,output,session,faction = "Forsaken")
  samarianInfoServer(input,output,session,faction = "C.O.R.E")
  samarianInfoServer(input,output,session,faction = "Dragyri")
  samarianInfoServer(input,output,session,faction = "Outcasts")
  samarianInfoServer(input,output,session,faction = "Brood")
  samarianInfoServer(input,output,session,faction = "Skarrd")
  
  # Obersvation Events ---------------------------------
  observeEvent(input$army_selection,{
    # Reset army value when faction is changed.
    updatePickerInput(session, "subfaction_selection",choices = faction.Data[[input$army_selection]][['sub.Faction']])
    army$value <- 0
  })
  
  observeEvent(input$display_card,{
    # Select card
    displayedCard <- trimws(strsplit(input$display_card, "_")[[1]][1], which = c("both"))#strsplit(input$display_card, "_")[[1]][1]
    # Display card
    showModal(modalDialog(
      size = "l",
      HTML(paste0("<h4>Stat Cards for: ",displayedCard,"</h4>")),
      fluidRow(
        column(width = 6,
               HTML(paste0('<img src = "stat_cards/',displayedCard,'_0.png">'))
               ),
        column(width = 6,
               HTML(paste0('<img src = "stat_cards/',displayedCard,'_1.png">'))
        )
      )
    ))
    
  })
  
  # Advanced Reactive Value(s) ---------------------------------
  
  selectedArmy <- reactive({
    # Generate selected army based on input
    req(c(input$army_selection,input$subfaction_selection))
    
    validate(
      need(try(!is.na(input$army_value)), "Please select a valid army point amount!"),
      need(try(as.numeric(input$army_value) / 500 > 0), "Please select a valid army point amount!")
    )
    # Unbind/Bind DT shiny inputs
    session$sendCustomMessage('unbind-DT', 'ArmyTable') #TAG UNBIND
    
    # Generate army data frame
    currentArmy <- faction.Df %>%
      dplyr::filter(grepl(input$army_selection, Faction)) %>%
      dplyr::filter(grepl(paste0("Unaligned|Bounty Hunter|",input$subfaction_selection), Subfaction))  %>%
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
      mutate(Display = HTML('<button id="',Name,'" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange(&quot;display_card&quot;,  this.id + &quot;_&quot; + Math.random())">Show Card</button>')) %>%
      dplyr::select(Faction,Subfaction,Name,Display,Cost,Amount,Allotment,Number)
    
    return(currentArmy)
  })
  
  # Data Outputs ---------------------------------
  
  output$ArmyTable <- renderDataTable((selectedArmy()), selection = 'none',
                                      rownames = FALSE,
                                      escape = FALSE,
                                      editable = FALSE,
                                      extensions = c("Scroller"),
                                      options = list(dom = 't',
                                                     paging = FALSE,
                                                     columnDefs = list(list(targets = {c(6)},
                                                                            orderable = FALSE),
                                                                       list(targets = {c(0,5)},
                                                                            visible = FALSE)
                                                                       ),
                                                     scrollY = '500px',
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
                                                        )
                                                      )
                                                    )
                                      )
  
  output$currentValue <- renderText({
    req(input$army_value)
    
    validate(
      #need(try(length(input$army_value) != 0), "Please select a valid army point amount!"),
      need(try(as.numeric(input$army_value) / 500 > 0), "Please select a valid army point amount!")
      
    )
    
    val <- 0
    # Calculate amount of points selected
    for (name in selectedArmy()[["Name"]]){
      
      val <- val + as.numeric(input[[name]]) * as.numeric(selectedArmy()$Cost[which(selectedArmy()[["Name"]] == name)])
    
    }

    if (length(val) != 0){
      # Generate Current - Max Delta.
      if (val <= as.numeric(input$army_value)) {
        
        textString <- paste0('Current Points: ',val,"/",input$army_value,'</br>Points Remaining: ',as.numeric(input$army_value) - val)
      } else {
        
        textString <-HTML(paste0('Current Points: <font color=\"#FF0000\">',val,"/",input$army_value,'</font></br>You have too many models!'))
      }
      
      # Enable or Disable the download button depending on army value.
      if (val > as.numeric(input$army_value) || 
          val <= 0) {
        
        shinyjs::disable("download_army")
        
      } else {
        
        shinyjs::enable("download_army")
      }
      
      return(textString)
    } else {
      # Error Placeholder
      return("Loading...")
    }
  })
  
  output$ruleReference <- renderText({
    #Render text associated with selected rule.
    req(input$rule_selector)
    
    selectedRule <- which(rules.Reference[["Rules"]] == input$rule_selector)
    
    return(rules.Reference[["Definition"]][[selectedRule]])
    
  })

  output$Downloader <- renderUI({
    #Download only renders when a valid amount of points has been selected
    validate(
      
      need(try(as.numeric(input$army_value) / 500 > 0), "")
      
    )
    downloadButton("download_army","Download Army List")
  }
  )
  
  output$Helper <- renderUI({
    #Helper text for users on how to use the textAreaInput
    validate(
      
      need(try(as.numeric(input$army_value) / 500 > 0), "")
      
    )
    helpText("If needed, place any text you",
             "want displayed on your printout in the",
             "text field to the right. This could include",
             "Subfaction bonuses, Psychogenics, etc...")
  }
  )
  
  output$Notesbox <- renderUI({
    #TextAreaInput only renders when a valid amount of points has been selected.
    validate(
      
      need(try(as.numeric(input$army_value) / 500 > 0), "")
      
    )
    textAreaInput("army_notes",NULL,width = "100%")
  }
  )
  
  # Download Outputs ---------------------------------
  
  output$download_army <- downloadHandler(
    filename = "Armylist.pdf",
    
    content = function(file) {
      # Normalize and Generate files ---------------------------------
      src <- normalizePath('misc/ArmyTemplate.Rmd')
      outputArmy <- tibble(Subfaction = character(),
                           Name = character(),
                           Type = character(),
                           Value = character(),
                           Amount = character())
      
      tempVal <- 0
      step <- 1
      
      normalizedPaths <- rep("",nrow(selectedArmy()))
      stat_name <- rep("",nrow(selectedArmy()))
      
      # Select unit subset ---------------------------------
      
      for (name in selectedArmy()[["Name"]]){
        
        tempVal <- tempVal + as.numeric(input[[name]]) * as.numeric(selectedArmy()$Cost[which(selectedArmy()[["Name"]] == name)])
        if (as.numeric(input[[name]]) >= 1){
          stat_name[step] <- paste(name,"png",sep = ".")
          normalizedPaths[step] <- normalizePath(paste0("www/stat_cards/",paste(name,"png",sep = ".")))
          sel_name <- which(selectedArmy()[["Name"]] == name)
          characterRow <- tibble(Subfaction = as.character(selectedArmy()$Subfaction[sel_name]),
                                 Name = name,
                                 Type = case_when(
                                   as.character(selectedArmy()$Amount[sel_name]) == "C" ~ "Character",
                                   TRUE ~ "Unit"
                                 ),
                                 Value = as.character(selectedArmy()$Cost[sel_name]),
                                 Amount = as.character(input[[name]]))
          outputArmy[step,] <- characterRow
          step <- step + 1
        }
        
      }
      
      # Set temp working directory & copy---------------------------------
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'ArmyTemplate.Rmd', overwrite = TRUE)
      
      # Copy all selected stat cards
      
      for (i in (i = 1:step-1)){
        file.copy(normalizedPaths[i], stat_name[i], overwrite = TRUE)
      }
      
      # Render document
      
      out <- render('ArmyTemplate.Rmd', 
                    pdf_document(),
                    envir = new.env(),
                    params = list(
                      Table = outputArmy,
                      Title = input$army_selection,
                      CurrentPoint =  tempVal,
                      MaxPoint = input$army_value,
                      Notes = input$army_notes
                    ))
      
      #Make document available for download
      
      file.rename(out, file)
    }
  )
  
}