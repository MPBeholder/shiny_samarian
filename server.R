server <- function(input, output, session){
  # Base Reactive Value(s) ---------------------------------
  army <- reactiveValues(value = integer())  

  # On Launch Modal ---------------------------------
  
  sendSweetAlert(
    session = session,
    html = TRUE,
    title = "A Note from the Wasteland",
    text = fluidRow(
          column(width = 12,
                   HTML('This is a fan created army building app for the Cool Mini Or Not (CMON) game Dark Age.</br>All images contained within this app are copyrighted to CMON.</br>If you encounter any issues please post an issue on github. <a href = "https://github.com/MPBeholder/shiny_samarian" target ="_blank"><img width = "15px" height = "15px" src = "github_mark_64px.png"></a>'),
                 hr()
                 ),
          column(width = 6,
                 HTML('<a target="_blank" href="http://dark-age.com"><img class = "custom" style="padding-top:12%" height = "50%" width = "50%" src = "DA_Logo3.png"></a>')),
          column(width = 6,
                 HTML('<a target="_blank" href="https://cmon.com"><img class = "custom" height = "50%" width = "50%" src = "cmon_logo.png"></a>'))
        ),
    type = "info"
  )
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
    sendSweetAlert(
      session = session,
      html = TRUE,
      title = HTML(paste0("Stat Cards for: ",displayedCard)),
      text = fluidRow(
        column(width = 6,
               HTML(paste0('<img class = "custom" src = "stat_cards/',displayedCard,'_0.png">'))
        ),
        column(width = 6,
               HTML(paste0('<img class = "custom" src = "stat_cards/',displayedCard,'_1.png">'))
        )
      ),
      type = "info"
    )
    
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
    suppressWarnings(
    currentArmy <- faction.Df %>%
      dplyr::filter(grepl(input$army_selection, Faction) | grepl(input$subfaction_selection, Faction)) %>%
      dplyr::filter(grepl(paste0("Unaligned|Bounty Hunter|",input$subfaction_selection), Subfaction))  %>%
      mutate(Allotment = case_when(
        Amount == "C" ~ 1,
        Amount == "*" ~ 1,
        TRUE ~ (as.integer(Amount) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500)))
      )) %>%
      group_by(Name,Subfaction) %>%
      mutate(Number = paste0('<select id = "',Name,'" class="armyCount">',
                             paste0('<option value="',seq(0,Allotment),'">',seq(0,Allotment),'</option>',collapse = ""),' </select>')) %>%
      ungroup() %>%
      group_by(Faction) %>%
      mutate(Amount = factor(Amount, levels = c("C","1","2","3","4","5","6","*"))) %>%
      arrange(Subfaction, Amount) %>%
      ungroup() %>%
      group_by(Name) %>%
      mutate(Display = (HTML('<button id="',Name,'" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange(&quot;display_card&quot;,  this.id + &quot;_&quot; + Math.random())">Show Card</button>'))) %>%
      dplyr::select(Faction,Subfaction,Name,Display,Cost,Amount,Allotment,Number)
    )
    return(currentArmy)
  })
  
  # Data Outputs ---------------------------------
  
  output$ArmyTable <- renderDataTable((selectedArmy()), selection = 'none',
                                      rownames = FALSE,
                                      escape = FALSE,
                                      editable = FALSE,
                                      extensions = c("Scroller","FixedHeader"),
                                      options = list(dom = 't',
                                                     autoWidth = FALSE,
                                                     server = TRUE,
                                                     fixedHeader = TRUE,
                                                     scrollX = TRUE,
                                                     paging = FALSE,
                                                     columnDefs = list(list(targets = {c(3,7)},
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
        shinyjs::disable("generateArmy")
      } else {
        shinyjs::enable("generateArmy")
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
    
    # downloadButton("downloadArmy","Download Army List")
    actionButton("generateArmy","Finalize and Generate Army", icon = icon("download"))
    
  }
  )
  
  # output$Helper <- renderUI({
  #   #Helper text for users on how to use the textAreaInput
  #   validate(
  #     
  #     need(try(as.numeric(input$army_value) / 500 > 0), "")
  #     
  #   )
  #   
  #   helpText("If needed, place any text you",
  #            "want displayed on your printout in the",
  #            "text field to the right. This could include",
  #            "Subfaction bonuses, Psychogenics, etc...")
  #   
  # }
  # )
  
  # output$Notesbox <- renderUI({
  #   #TextAreaInput only renders when a valid amount of points has been selected.
  #   validate(
  #     
  #     need(try(as.numeric(input$army_value) / 500 > 0), "")
  #     
  #   )
  #   
  #   textAreaInput("army_notes",NULL,width = "100%")
  # 
  #   }
  # )
  
  # Download Outputs ---------------------------------
  
  output$downloadArmy <- downloadHandler(
    filename = function() {
      paste("armyList-", Sys.Date(), ifelse(input$fileType == "Full",".pdf",".html"), sep="")
    },
    
    #contentType = ifelse(input$fileType == "Full","application/pdf","text/html"),
    
    content = function(file) {
      
      closeSweetAlert(session)
      
      progressSweetAlert(
        session = session, id = "armyProgress",
        title = "Generating Army",
        display_pct = FALSE, value = 0)
      # Make sure it closes when we exit this reactive, even if there's an error
      # on.exit(progress$close())
      n <- nrow(selectedArmy())
      # Normalize and Generate files ---------------------------------
      src <- normalizePath('misc/ArmyTemplate.Rmd')
      src_quick <- normalizePath('misc/quickArmyTemplate.Rmd')
      outputArmy <- tibble(Subfaction = character(),
                           Name = character(),
                           Type = character(),
                           Value = character(),
                           Amount = character())
      
      tempVal <- 0
      step <- 1
      progressIterate <- 1
      
      normalizedPaths <- rep("",nrow(selectedArmy()))
      stat_name <- rep("",nrow(selectedArmy()))
      
      # Select unit subset ---------------------------------
      
      normalizedUpgrades <- gsub(".*\\|","",c(autoReactUpgrades$selected,input$upgradeInput))
      normalizedPsychos <- gsub(".*\\|","",c(autoReactPsychogenics$selected,input$psychogenicInput))
      
      # print(normalizedUpgrades)
      # print(normalizedPsychos)
      
      totalUpgrades <- append(autoReactUpgrades$selected,input$upgradeInput) %>% 
        tibble::enframe(name = NULL) %>% 
        separate(value,c("name","value"),sep = "\\|") %>% 
        group_by(name) %>% 
        dplyr::summarise(value = paste0(value,collapse = ", ")) %>%
        filter(name != "") %>%
        dplyr::rename("Upgrades & Bio-Gens" = "value") %>%
        dplyr::rename("Name" = "name")
      
      totalPsycho <- append(autoReactPsychogenics$selected,input$psychogenicInput) %>% 
        tibble::enframe(name = NULL) %>% 
        separate(value,c("name","value"),sep = "\\|") %>% 
        group_by(name) %>% 
        dplyr::summarise(value = paste0(value,collapse = ", ")) %>%
        filter(name != "")  %>%
        dplyr::rename("Psychogenics & Rituals" = "value") %>%
        dplyr::rename("Name" = "name")
      
      reset("psychogenicInput")
      reset("upgradeInput")
      autoReactUpgrades$selected <- ""
      autoReactPsychogenics$selected <- ""
      
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
        
        updateProgressBar(
          session = session,
          id = "armyProgress",
          value = progressIterate * (100 / n), total = 100,
          title = "Generating Army"
        )
        progressIterate <- progressIterate + 1
        #progress$inc(1/n, detail = paste("Checking: ", name))
        
      }
      
      tmpUpgrades <<- totalUpgrades
      tmpPsycho <<- totalPsycho
      tmpTable <<- outputArmy
      tmpNormalizedUpgrades <<- normalizedUpgrades
      tmpNormalizedPsychos <<- normalizedPsychos
      
      upPsychoArmy <- outputArmy %>% 
        dplyr::select(Name) %>%
        dplyr::left_join(totalUpgrades, by = "Name") %>%
        dplyr::left_join(totalPsycho, by = "Name") %>%
        replace_na(list(`Upgrades & Bio-Gens` = "",`Psychogenics & Rituals` = "")) %>%
        filter(!(`Upgrades & Bio-Gens` == "" & `Psychogenics & Rituals` == ""))
      
      quickrefTable <- outputArmy %>%
        dplyr::left_join(upPsychoArmy, by = "Name")
      
      # Set temp working directory & copy---------------------------------
      
      normalizedAddons <- tibble(destination = character(),current = character())
      
      for (upgrade in normalizedUpgrades) {
        if (upgrade == "") {break}
        name <- gsub(" ","_",tolower(upgrade))
        fullName <- paste0(name,".png")
        upgradePath <- normalizePath(paste0("www/stat_cards/",paste(name,"png",sep = ".")))
        normalizedAddons <- bind_rows(normalizedAddons,tibble(destination = fullName,current = upgradePath))
      }
      
      for (psycho in normalizedPsychos){
        if (psycho == "") {break}
        name <- gsub(" ","_",tolower(psycho))
        fullName <- paste0(name,".png")
        psychoPath <- normalizePath(paste0("www/stat_cards/",paste(name,"png",sep = ".")))
        normalizedAddons <- bind_rows(normalizedAddons,tibble(destination = fullName,current = psychoPath))
      }
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      if (input$fileType == "Reference") {
        file.copy(src_quick, 'quickArmyTemplate.Rmd', overwrite = TRUE)
      } else {
        
      file.copy(src, 'ArmyTemplate.Rmd', overwrite = TRUE)
      
      # Copy all selected stat cards
      
      for (i in (i = 1:step-1)){
        file.copy(normalizedPaths[i], stat_name[i], overwrite = TRUE)
      }
      
      if (nrow(normalizedAddons) != 0){
        for (j in (j = 1:nrow(normalizedAddons))){
          #print(normalizedAddons[j])
          file.copy(normalizedAddons$current[j], normalizedAddons$destination[j], overwrite = TRUE)
        }
      }
      }
      # Render document
      tryCatch({
        
      if (input$fileType == "Reference") {
        out <- render('quickArmyTemplate.Rmd', 
                      html_document(),
                      envir = new.env(),
                      params = list(
                        Table = quickrefTable
                      ))
      } else {
        out <- render('ArmyTemplate.Rmd', 
                      pdf_document(),
                      envir = new.env(),
                      params = list(
                        Table = outputArmy,
                        PsychoTable = upPsychoArmy,
                        Title = input$army_selection,
                        CurrentPoint =  tempVal,
                        MaxPoint = input$army_value,
                        Notes = ''#input$army_notes
                      ))
      }
      #Make document available for download
      closeSweetAlert(session)
      
      sendSweetAlert(
        session = session,
        title = "Army Generated",
        text = "Army successfully downloaded",
        type = "success"
      )
      
      file.rename(out, file)
      
      },
      error = function(c) {
        closeSweetAlert(session)
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Army generation failed",
          type = "error"
        )
      }
      )
    }
  )
  
  # output$addonUI <- renderUI({
  #   #Download only renders when a valid amount of points has been selected
  #   
  #   validate(
  #     
  #     need(try(as.numeric(input$army_value) / 500 > 0), "")
  #     
  #   )
  #   
  #   actionButton("addOns","Upgrades & Psychogenics")
  #   
  # }
  # )
  
  # Add-Ons : Upgrades and Psychogenics ---------------------------------
  
  autoReactUpgrades <- reactiveValues(selected = "")
  autoReactPsychogenics <- reactiveValues(selected = "")
  
  output$upgradeUI <- renderUI({
    
    outputArmy <- tibble(Subfaction = character(),
                         Name = character(),
                         Type = character(),
                         Value = character(),
                         Amount = character())
    
    tempVal <- 0
    step <- 1
    
    # Select unit subset ---------------------------------
    
    for (name in selectedArmy()[["Name"]]){
      
      tempVal <- tempVal + as.numeric(input[[name]]) * as.numeric(selectedArmy()$Cost[which(selectedArmy()[["Name"]] == name)])
      
      if (as.numeric(input[[name]]) >= 1){
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
    
    outputTibble <<- outputArmy
    
    upgradeFrame <- faction.Df %>% 
      filter(Name %in% outputArmy$Name) %>%
      dplyr::select(Name,Upgrade,UpgradeNum) %>%
      filter(!is.na(Upgrade))
    
    validate(
      need(nrow(upgradeFrame) != 0, HTML("No selectable Upgrades or Bio-Gens!"))
    )
    
    if (any(str_detect(upgradeFrame$Upgrade,"/")) && 
        any(str_detect(upgradeFrame$UpgradeNum,"/"))) {
      
      upgradeFrame <- upgradeFrame %>%
        separate_rows(Upgrade,UpgradeNum,sep = "/")
      
    } else if (any(str_detect(upgradeFrame$Upgrade,"/"))) {
      upgradeFrame <- upgradeFrame %>%
        separate_rows(Upgrade,sep = "/")
    } else if (any(str_detect(upgradeFrame$UpgradeNum,"/"))) {
      upgradeFrame <- upgradeFrame %>%
        separate_rows(UpgradeNum,sep = "/")
    }
    
    upgradeFrame <- upgradeFrame %>%
      mutate(UpgradeNum = as.integer(UpgradeNum))
    
    autoUpgrades <- upgradeFrame %>%
      filter(Upgrade %in% upgrades.Df$Name) %>%
      mutate(comboUpgrade = paste(Name,Upgrade,sep = "|")) %>%
      pull(comboUpgrade)
      
    autoReactUpgrades$selected <- autoUpgrades
    
    selectableUpgrades <- upgradeFrame %>%
      filter(!Upgrade %in% unlist(str_split(autoUpgrades,"\\|"))) %>%
      mutate(nameCombo = paste(Name,Upgrade,sep = "|"))  %>%
      filter(!is.na(Upgrade))
    
    validate(
      need(nrow(selectableUpgrades) != 0, HTML("No selectable Upgrades or Bio-Gens!"))
    )
    
    upgradeList <- list()
    
    for (faction in selectableUpgrades$nameCombo) {
      upgradeVector <- upgrades.Df %>% filter(Faction == str_split(faction,"\\|")[[1]][2]) %>% pull(Name)
      
      selUpgradeVector <- paste(str_split(faction,"\\|")[[1]][1],upgradeVector,sep = "|")
      names(selUpgradeVector) <- upgradeVector
      upgradeList[[faction]] <- selUpgradeVector#upgrades.Df %>% filter(Faction == str_split(faction,"\\|")[[1]][2]) %>% pull(Name)
    }
    
    # selectableUpgrades <- upgradeFrame %>%
    #   filter(!Upgrade %in% autoUpgrades) %>%
    #   group_by(Upgrade) %>%
    #   summarize(n = sum(UpgradeNum)) %>%
    #   filter(!is.na(Upgrade))
    
    
    # upgradeList <- list()
    # 
    # for (faction in selectableUpgrades$Upgrade) {
    #   upgradeList[[faction]] <- upgrades.Df %>% filter(Faction == faction) %>% pull(Name)
    # }
    tagList(
    pickerInput(
      inputId = "upgradeInput",
      label = "Select Upgrades and Bio-Gens",
      choices = upgradeList,
      multiple = TRUE,
      #selected = selectedUpgrades$selected,
      options = list("max-options-group" = selectableUpgrades$UpgradeNum,
                     "max-options-text" = "Maximum upgrades or bio-gens for this group selected")
    ))
  })
  
  output$psychoUI <- renderUI({
    
    outputArmy <- tibble(Subfaction = character(),
                         Name = character(),
                         Type = character(),
                         Value = character(),
                         Amount = character())
    
    tempVal <- 0
    step <- 1
    
    # Select unit subset ---------------------------------
    
    for (name in selectedArmy()[["Name"]]){
      
      tempVal <- tempVal + as.numeric(input[[name]]) * as.numeric(selectedArmy()$Cost[which(selectedArmy()[["Name"]] == name)])
      
      if (as.numeric(input[[name]]) >= 1){
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
    
    outputTibble <<- outputArmy
    
    psychogenicFrame <- faction.Df %>% 
      filter(Name %in% outputArmy$Name) %>%
      dplyr::select(Name,Psychogenic,PsychogenicNum) %>%
      filter(!is.na(Psychogenic))
    
    validate(
      need(nrow(psychogenicFrame) != 0, HTML("No selectable Psychogenics or Rituals!"))
    )
    
    if (any(str_detect(psychogenicFrame$Psychogenic,"/")) &&
        any(str_detect(psychogenicFrame$PsychogenicNum,"/"))) {
      
      psychogenicFrame <- psychogenicFrame %>%
        separate_rows(Psychogenic,PsychogenicNum,sep = "/")
    }
    
    psychogenicFrame <- psychogenicFrame %>%
      mutate(PsychogenicNum = as.integer(PsychogenicNum))
    
    autoPsychogenics <- psychogenicFrame %>%
      filter(Psychogenic %in% psychogenic.Df$Name) %>%
      mutate(comboPsycho = paste(Name,Psychogenic,sep = "|")) %>%
      pull(comboPsycho)
    
    autoReactPsychogenics$selected <- autoPsychogenics
    
    selectablePsychogenics <- psychogenicFrame %>%
      filter(!Psychogenic %in% unlist(str_split(autoPsychogenics,"\\|"))) %>%
      mutate(nameCombo = paste(Name,Psychogenic,sep = "|"))  %>%
      filter(!is.na(Psychogenic))
    
    if (any(selectablePsychogenics$PsychogenicNum == 0)) {
      autoIncludes <- psychogenic.Df %>%
        filter(Faction == selectablePsychogenics %>% 
                 filter(PsychogenicNum == 0) %>% 
                 pull(Psychogenic)) %>%
        filter(Subfaction == "Unaligned") %>%
        pull(Name)
      
      massIncluder <- selectablePsychogenics %>% 
        filter(PsychogenicNum == 0) %>% 
        pull(Name)
      
      selectablePsychogenics <- selectablePsychogenics %>%
        filter(PsychogenicNum != 0)
      
      autoPsychogenics <- append(autoPsychogenics,paste(massIncluder,autoIncludes,sep = "|"))

      autoReactPsychogenics$selected <- autoPsychogenics
    }
    
    
    validate(
      need(nrow(selectablePsychogenics) != 0, HTML("No selectable Psychogenics or Rituals!"))
    )
    
    psychogenicList <- list()
    
    for (faction in selectablePsychogenics$nameCombo) {
      splitFaction <- str_split(faction,"\\|")[[1]][2]
      
      if (splitFaction %in% psychogenic.Df$Faction) {
        psychoVector <- psychogenic.Df %>% 
          filter(Faction %in% unlist(str_split(splitFaction,"/"))) %>%
          pull(Name)
      } else {
        psychoVector <- psychogenic.Df %>% 
          filter(Subfaction %in% unlist(str_split(splitFaction,"/"))) %>%
          filter(Faction %in% input$army_selection) %>%
          pull(Name)
      }
      
      selPsychoVector <- paste(str_split(faction,"\\|")[[1]][1],psychoVector,sep = "|")
      names(selPsychoVector) <- psychoVector
      psychogenicList[[faction]] <- selPsychoVector#upgrades.Df %>% filter(Faction == str_split(faction,"\\|")[[1]][2]) %>% pull(Name)
    }
    
    
    
    
    
    
    
    # selectablePsychogenics <- psychogenicFrame %>%
    #   filter(!Psychogenic %in% autoPsychogenics) %>%
    #   group_by(Psychogenic) %>%
    #   summarize(n = sum(PsychogenicNum)) %>%
    #   filter(!is.na(Psychogenic))
    # 
    # if (any(selectablePsychogenics$n == 0)) {
    #   autoIncludes <- psychogenic.Df %>%
    #     filter(Faction == selectablePsychogenics %>%
    #     filter(n == 0) %>%
    #     pull(Psychogenic)) %>%
    #     filter(Subfaction == "Unaligned") %>%
    #     pull(Name) %>%
    #     append(autoPsychogenics)
    #   
    #   autoReactPsychogenics$selected <- autoIncludes
    # }
    # 
    # selectablePsychogenics <- selectablePsychogenics %>%
    #   filter(n != 0)
    
    # validate(
    #   need(nrow(selectablePsychogenics) != 0, HTML("No selectable Psychogenics or Rituals!"))
    # )
    
    # psychogenicList <- list()
    # 
    # for (faction in selectablePsychogenics$Psychogenic) {
    #   psychogenicList[[faction]] <- psychogenic.Df %>% 
    #     filter(Faction == input$army_selection) %>% 
    #     filter(Subfaction == faction) %>%
    #     pull(Name)
    # }
    
    pickerInput(
      inputId = "psychogenicInput",
      label = "Select Psychogenics",
      choices = psychogenicList,
      multiple = TRUE,
      #selected = selectedPsychogenics$selected,selectableUpgrades$UpgradeNum
      options = list("max-options-group" = selectablePsychogenics$PsychogenicNum,
                     "max-options-text" = "Maximum psychogenics for this group selected")
    )
  })
  
  # Add-Ons Modal ---------------------------------
  
  observeEvent(input$generateArmy,{
    
    sendSweetAlert(
      session = session,
      html = TRUE,
      title = 'Finalize Army',
      text = tagList(fluidRow(
        # Upgrades
        column(width = 6,
               HTML("<center>"),
               uiOutput("upgradeUI"),
               HTML("</center>")),
        # Psychogenics
        column(width = 6,
               HTML("<center>"),
               uiOutput("psychoUI"),
               HTML("</center>"))),
        hr(),
        fluidRow(
        column(width = 6,
               HTML("<center>"),
                  pickerInput('fileType','Select Download Type',choices = c("Full","Reference"),selected = "Full"),
               HTML("</center>")),
        column(width = 6,
               div(class = "vertAlign",
                   HTML("<center>"),
                   uiOutput("dlHandler"),#downloadButton('downloadArmy','Download Army'),
                   HTML("</center>")
               )
        ))
      ),
      btn_labels = c('Dismiss'),
      type = "warning"
    )
  })

  output$dlHandler <- renderUI({
    
    #Download only renders when upgrade and psychogenic rules satisfied
    
      currentUpgrades <- gsub(".*\\|","",c(input$upgradeInput))
      currentPsychogenics <- gsub(".*\\|","",c(input$psychogenicInput))

      upgradeVal <- if_else(input$subfaction_selection == "Broodmere Spawn",2,1)
      
      maxUpgrades <- max(table(currentUpgrades),na.rm = T)
      maxPsychos <- max(table(currentPsychogenics),na.rm = T)
        
    validate(

      need(try(maxUpgrades <= upgradeVal), "Too many duplicate Upgrades or Bio-Gens")

    )

    validate(

      need(try(maxPsychos <= 1), "Too many duplicate Psychogenics")

    )
    
    downloadButton('downloadArmy','Download Army')
    
  }
  )
  
}