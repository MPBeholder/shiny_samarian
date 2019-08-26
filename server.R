server <- function(input, output, session){
  # Base Reactive Value(s) ---------------------------------
  army <- reactiveValues(value = integer())  
  
  mongoTrigger <- makeReactiveTrigger()
  # Create session specific connection to mongo
  tryCatch({
    db.SAB <- mongo(collection = "SAB", db = "appFirebase", url = paste0("mongodb://",Sys.getenv('mongoUser'),":",Sys.getenv('mongoPass'),"@",Sys.getenv('mongoURL')))
  },
    error = function(c) {
    sendSweetAlert(
      session,
      title = "Connection Error",
      text = "Cannot connect to database! Try again later",
      type = "error"
    )
      stopApp
  })
  
  # Close session specific connection to mongo
  
  session$onSessionEnded(function() {
    
    
    db.SAB$disconnect()
    
  })
  
  # Modular Components ---------------------------------
  samarianInfoServer(input,output,session,faction = "Kukulkani")
  samarianInfoServer(input,output,session,faction = "Forsaken")
  samarianInfoServer(input,output,session,faction = "C.O.R.E")
  samarianInfoServer(input,output,session,faction = "Dragyri")
  samarianInfoServer(input,output,session,faction = "Outcasts")
  samarianInfoServer(input,output,session,faction = "Brood")
  samarianInfoServer(input,output,session,faction = "Skarrd")
  
  hide_waiter()
  
  # On Launch Modal ---------------------------------
    #shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('', 'txt','png'))
  
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
  # Obersvation Events ---------------------------------
  observeEvent(input$army_selection,{
    # Reset army value when faction is changed.
    updatePickerInput(session, "subfaction_selection",choices = faction.Data[[input$army_selection]][['sub.Faction']])
    army$value <- 0
  })
  
  output$stat1 <- renderImage({
    req(input$display_card)
    displayedCard <- trimws(strsplit(input$display_card, "_")[[1]][1], which = c("both"))
    selCard <- gsub(" ","_",(displayedCard))
    filename <- normalizePath(paste0("./www/stat_cards/",paste(paste0(selCard,"_0"),"png",sep = ".")))
    
    list(src = filename,
         alt = paste("Stat Card: ", gsub(" ","_",(displayedCard))))
    
  }, deleteFile = FALSE)
  
  output$stat2 <- renderImage({
    req(input$display_card)
    displayedCard <- trimws(strsplit(input$display_card, "_")[[1]][1], which = c("both"))
    selCard <- gsub(" ","_",(displayedCard))
    filename <- normalizePath(paste0("./www/stat_cards/",paste(paste0(selCard,"_1"),"png",sep = ".")))
    
    list(src = filename,
         alt = paste("Abilities Card: ", gsub(" ","_",(displayedCard))))
    
  }, deleteFile = FALSE)
  
  observeEvent(input$display_card,{
    # Select card
    displayedCard <- trimws(strsplit(input$display_card, "_")[[1]][1], which = c("both"))#strsplit(input$display_card, "_")[[1]][1]
    # Display card
    sendSweetAlert(
      session = session,
      html = TRUE,
      title = HTML(paste0("Stat Cards for: ",displayedCard)),
      text = div(style = "height:260px;overflow-y:scroll;",
                 fluidRow(
        column(width = 6,
               (imageOutput("stat1",height = '250px')#HTML(paste0('<img class = "custom" src = "stat_cards/',gsub(" ","_",tolower(displayedCard)),'_0.png">'))
        )),
        column(width = 6,
               (imageOutput("stat2",height = '250px')#HTML(paste0('<img class = "custom" src = "stat_cards/',gsub(" ","_",tolower(displayedCard)),'_1.png">'))
        ))
      )),
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
      mutate(Number = paste0('<div id = ',str_replace_all(Name, "[[:punct:]]| ", ""),'><select id = "',gsub(" ","",Name),'" class="armyCount">',
                             paste0('<option value="',seq(0,Allotment),'">',seq(0,Allotment),'</option>',collapse = ""),' </select></div>')) %>%
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
      
      val <- val + as.numeric(input[[gsub(" ","",name)]]) * as.numeric(selectedArmy()$Cost[which(selectedArmy()[["Name"]] == name)])
    
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
    
    if (input$army_selection == "Dragyri") {
      downloadUI <- fluidRow(column(width = 12,
                                    actionButton("generateArmy","Finalize and Generate Army", icon = icon("download"))),
                             column(width = 12,
                                    actionButton("selArbiter","Arbiter Commands", icon = icon("download"))))
    } else {
      downloadUI <- fluidRow(column(width = 12,
                                    actionButton("generateArmy","Finalize and Generate Army", icon = icon("download")))#,
                             # column(width = 12,
                             #        actionButton("test2","Finalize and Generate Army", icon = icon("download")))
                             )
    }
    
    downloadUI
    #actionButton("generateArmy","Finalize and Generate Army", icon = icon("download"))
    
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
      js$pushAnalytic(input$fileType,'Download',paste(input$army_selection,input$subfaction_selection,sep = "-"),as.integer(input$army_value))
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
        dplyr::rename("Name" = "name") %>%
        mutate(Name = gsub(" ","_",Name))
      
      totalPsycho <- append(autoReactPsychogenics$selected,input$psychogenicInput) %>% 
        tibble::enframe(name = NULL) %>% 
        separate(value,c("name","value"),sep = "\\|") %>% 
        group_by(name) %>% 
        dplyr::summarise(value = paste0(value,collapse = ", ")) %>%
        filter(name != "")  %>%
        dplyr::rename("Psychogenics & Rituals" = "value") %>%
        dplyr::rename("Name" = "name") %>%
        mutate(Name = gsub(" ","_",Name))
      
      reset("psychogenicInput")
      reset("upgradeInput")
      autoReactUpgrades$selected <- ""
      autoReactPsychogenics$selected <- ""
      
      for (name in selectedArmy()[["Name"]]){
        
        tempVal <- tempVal + as.numeric(input[[gsub(" ","",name)]]) * as.numeric(selectedArmy()$Cost[which(selectedArmy()[["Name"]] == name)])
        if (as.numeric(input[[gsub(" ","",name)]]) >= 1){
          stat_name[step] <- paste(gsub(" ","_",(name)),"png",sep = ".")
          normalizedPaths[step] <- normalizePath(paste0("www/stat_cards/",paste(gsub(" ","_",(name)),"png",sep = ".")))
          sel_name <- which(selectedArmy()[["Name"]] == name)
          characterRow <- tibble(Subfaction = as.character(selectedArmy()$Subfaction[sel_name]),
                                 Name = gsub(" ","_",(name)),
                                 Type = case_when(
                                   as.character(selectedArmy()$Amount[sel_name]) == "C" ~ "Character",
                                   TRUE ~ "Unit"
                                 ),
                                 Value = as.character(selectedArmy()$Cost[sel_name]),
                                 Amount = as.character(input[[gsub(" ","",name)]]))
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
      tmpNormalized <<- normalizedPaths
      tmpStats <<- stat_name
      
      #outputArmy$Name <- gsub("_"," ",outputArmy$Name)
        
      
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
      
      tempVal <- tempVal + as.numeric(input[[gsub(" ","",name)]]) * as.numeric(selectedArmy()$Cost[which(selectedArmy()[["Name"]] == name)])
      
      if (as.numeric(input[[gsub(" ","",name)]]) >= 1){
        sel_name <- which(selectedArmy()[["Name"]] == name)
        characterRow <- tibble(Subfaction = as.character(selectedArmy()$Subfaction[sel_name]),
                               Name = name,
                               Type = case_when(
                                 as.character(selectedArmy()$Amount[sel_name]) == "C" ~ "Character",
                                 TRUE ~ "Unit"
                               ),
                               Value = as.character(selectedArmy()$Cost[sel_name]),
                               Amount = as.character(input[[gsub(" ","",name)]]))
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
      filter(!is.na(Upgrade))
    
    tempSelectable <<- selectableUpgrades
    
    selectableUpgrades <- selectableUpgrades %>%
      dplyr::left_join(outputArmy %>% 
                         dplyr::select(Name, Amount), 
                       by = "Name") %>%
      mutate(UpgradeNum = UpgradeNum * as.integer(Amount)) %>%
      mutate(nameCombo = paste(Name,Upgrade,UpgradeNum,sep = "|"))  %>%
      dplyr::select(-Amount)
    
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
      
      tempVal <- tempVal + as.numeric(input[[gsub(" ","",name)]]) * as.numeric(selectedArmy()$Cost[which(selectedArmy()[["Name"]] == name)])
      
      if (as.numeric(input[[gsub(" ","",name)]]) >= 1){
        sel_name <- which(selectedArmy()[["Name"]] == name)
        characterRow <- tibble(Subfaction = as.character(selectedArmy()$Subfaction[sel_name]),
                               Name = name,
                               Type = case_when(
                                 as.character(selectedArmy()$Amount[sel_name]) == "C" ~ "Character",
                                 TRUE ~ "Unit"
                               ),
                               Value = as.character(selectedArmy()$Cost[sel_name]),
                               Amount = as.character(input[[gsub(" ","",name)]]))
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
      #mutate(nameCombo = paste(Name,Psychogenic,sep = "|"))  %>%
      filter(!is.na(Psychogenic))
    
    selectablePsychogenics <- selectablePsychogenics %>%
      dplyr::left_join(outputArmy %>% 
                         dplyr::select(Name, Amount), 
                       by = "Name") %>%
      mutate(PsychogenicNum = PsychogenicNum * as.integer(Amount)) %>%
      mutate(nameCombo = paste(Name,Psychogenic,PsychogenicNum,sep = "|"))  %>%
      dplyr::select(-Amount)
    
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
                   column(width = 12,
                          uiOutput("dlHandler")
                          ),
                   hr(),
                   hr(),
                   column(width = 12,
                          uiOutput("saveArmy")),
                   HTML("</center>")
               )
        ))
      ),
      btn_labels = c('Dismiss'),
      type = "warning"
    )
  })

  output$saveArmy <- renderUI({
    
    validate(need(session$userData$current_user(),"Sign in to save armies!"))
    
    currentUpgrades <- gsub(".*\\|","",c(input$upgradeInput))
    currentPsychogenics <- gsub(".*\\|","",c(input$psychogenicInput))
    
    upgradeVal <- if_else(input$subfaction_selection == "Broodmere Spawn",2,1)
    
    suppressWarnings({
      maxUpgrades <- max(table(currentUpgrades),na.rm = T)
      maxPsychos <- max(table(currentPsychogenics),na.rm = T)
    })
    
    validate(
      
      need(try(maxUpgrades <= upgradeVal), "Too many duplicate Upgrades or Bio-Gens")
      
    )
    
    validate(
      
      need(try(maxPsychos <= 1), "Too many duplicate Psychogenics")
      
    )
    
    actionButton('save','Save Army',icon = icon("save"))
    
  })
  
  output$dlHandler <- renderUI({
    
    #Download only renders when upgrade and psychogenic rules satisfied
    
      currentUpgrades <- gsub(".*\\|","",c(input$upgradeInput))
      currentPsychogenics <- gsub(".*\\|","",c(input$psychogenicInput))

      upgradeVal <- if_else(input$subfaction_selection == "Broodmere Spawn",2,1)
      
      suppressWarnings({
        maxUpgrades <- max(table(currentUpgrades),na.rm = T)
        maxPsychos <- max(table(currentPsychogenics),na.rm = T)
      })
        
    validate(

      need(try(maxUpgrades <= upgradeVal), "Too many duplicate Upgrades or Bio-Gens")

    )

    validate(

      need(try(maxPsychos <= 1), "Too many duplicate Psychogenics")

    )
    
    downloadButton('downloadArmy','Download Army')
    
  }
  )

  observeEvent(input$save,{
    closeSweetAlert(session)
    confirmSweetAlert(
      session = session,
      inputId = "confirmSave",
      type = "warning",
      title = "Confirm Save?",
      html = T,
      btn_labels = c("Cancel",
                     "Confirm"),
      text = fluidRow(HTML("<center>"),
                      textInput('saveName','Army Name',value = ""),
                      br(),
                      uiOutput('nameCheck'),
                      HTML("</center>")),
      danger_mode = TRUE
    )
  })
  
  output$nameCheck <- renderUI({
    
    if (input$saveName == "") {
      textOut <- '<span style = "color:lightgray;">Please enter an army name</span>'
    } else if (input$saveName %in% currentArmies()[["SavedName"]]) {
      textOut <- '<span style = "color:red;">Name already in use!</span>'
    } else {
      textOut <- '<span style = "color:lightgray;">Valid army name</span>'
    }
    
    HTML(textOut)
    
  })
  
  observeEvent(input$confirmSave,{
    if (input$confirmSave) {
    outputArmy <- tibble(User = character(),
                         SavedName = character(),
                         Faction = character(),
                         Subfaction = character(),
                         armySubfaction = character(),
                         Name = character(),
                         Type = character(),
                         Value = character(),
                         Amount = character())
    
    tempVal <- 0
    step <- 1
    progressIterate <- 1
    n <- nrow(selectedArmy())
    
    progressSweetAlert(
      session = session, id = "saveProgress",
      title = "Saving Army",
      display_pct = FALSE, value = 0)
    
    totalUpgrades <- append(autoReactUpgrades$selected,input$upgradeInput) %>% 
      tibble::enframe(name = NULL) %>% 
      separate(value,c("name","value"),sep = "\\|") %>% 
      group_by(name) %>% 
      dplyr::summarise(value = paste0(value,collapse = ", ")) %>%
      filter(name != "") %>%
      dplyr::rename("Upgrades & Bio-Gens" = "value") %>%
      dplyr::rename("Name" = "name") %>%
      mutate(Name = gsub(" ","_",Name))
    
    totalPsycho <- append(autoReactPsychogenics$selected,input$psychogenicInput) %>% 
      tibble::enframe(name = NULL) %>% 
      separate(value,c("name","value"),sep = "\\|") %>% 
      group_by(name) %>% 
      dplyr::summarise(value = paste0(value,collapse = ", ")) %>%
      filter(name != "")  %>%
      dplyr::rename("Psychogenics & Rituals" = "value") %>%
      dplyr::rename("Name" = "name") %>%
      mutate(Name = gsub(" ","_",Name))
    
    reset("psychogenicInput")
    reset("upgradeInput")
    autoReactUpgrades$selected <- ""
    autoReactPsychogenics$selected <- ""
    
    for (name in selectedArmy()[["Name"]]){
      sel_name <- which(selectedArmy()[["Name"]] == name)
      tempVal <- tempVal + as.numeric(input[[gsub(" ","",name)]]) * as.numeric(selectedArmy()$Cost[which(selectedArmy()[["Name"]] == name)])
      if (as.numeric(input[[gsub(" ","",name)]]) >= 1){
        characterRow <- tibble(User = as.character(signed_in_user_uid()),
                               SavedName = as.character(input$saveName),
                               Faction = as.character(input$army_selection),
                               armySubfaction = as.character(input$subfaction_selection),
                               Subfaction = as.character(selectedArmy()$Subfaction[sel_name]),
                               Name = gsub(" ","_",(name)),
                               Type = case_when(
                                 as.character(selectedArmy()$Amount[sel_name]) == "C" ~ "Character",
                                 TRUE ~ "Unit"
                               ),
                               Value = as.character(selectedArmy()$Cost[sel_name]),
                               Amount = as.character(input[[gsub(" ","",name)]]))
        outputArmy[step,] <- characterRow
        step <- step + 1
      }

      updateProgressBar(
        session = session,
        id = "saveProgress",
        value = progressIterate * (100 / n), total = 100,
        title = "Saving Army"
      )
      progressIterate <- progressIterate + 1
      # progress$inc(1/n, detail = paste("Checking: ", name))
      
    }
    
    tmpUpgrades <<- totalUpgrades
    tmpPsycho <<- totalPsycho
    tmpTable <<- outputArmy
    
    upPsychoArmy <- outputArmy %>% 
      dplyr::select(Name) %>%
      dplyr::left_join(totalUpgrades, by = "Name") %>%
      dplyr::left_join(totalPsycho, by = "Name") %>%
      replace_na(list(`Upgrades & Bio-Gens` = "",`Psychogenics & Rituals` = "")) %>%
      filter(!(`Upgrades & Bio-Gens` == "" & `Psychogenics & Rituals` == "")) 
    
    quickrefTable <- outputArmy %>%
      dplyr::left_join(upPsychoArmy, by = "Name")
    
    db.SAB$insert(quickrefTable)
    
    mongoTrigger$trigger()
    
    savedFile <<- quickrefTable

    closeSweetAlert(session)
    
    sendSweetAlert(
      session = session,
      title = "Army Saved",
      text = "Army successfully saved",
      type = "success"
    )
    
    } else {
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
                       column(width = 12,
                              uiOutput("dlHandler")
                       ),
                       hr(),
                       hr(),
                       column(width = 12,
                              uiOutput("saveArmy")),
                       HTML("</center>")
                   )
            ))
        ),
        btn_labels = c('Dismiss'),
        type = "warning"
      )
    }
  }, ignoreInit = T)
  
  
  firebaseServer(input,output,session)
  
  observeEvent(input$submitSignIn, {
    updateTabItems(session, "tabs", "army_login")
  })
  
  observeEvent(input$saveName,{
    
    if (input$saveName == "" || input$saveName %in% currentArmies()[["SavedName"]]) {
      
      shinyjs::disable(selector = ".swal-button--confirm")
      
    } else {
      
      shinyjs::enable(selector = ".swal-button--confirm")
      
    }
    
  },ignoreInit = T)
  
  signed_in_user_uid <-reactive({
    req(session$userData$current_user())
    out <- session$userData$current_user()
    out <- unlist(out)
    
    unname(out)[1]
    
  })
  
  signed_in_user_df <- reactive({
    req(session$userData$current_user())
    
    out <- session$userData$current_user()
    out <- unlist(out)
    current_userDat <<- out
    data.frame(
      name = names(out),
      value = unname(out)
    )
  })
  
  currentArmies <- reactive({
    req(session$userData$current_user())
    
    mongoTrigger$depend()
    
    userQuery <- paste0('{"User":"',signed_in_user_uid(),'"}')
    
    print(userQuery)
    db.SAB$find(userQuery)
  })
  
  output$savedArmies <- renderUI({

    validate(need(session$userData$current_user(),"You must have one or more armies saved!"))
    
    armyNames <- currentArmies() %>% 
      pull(SavedName) %>% 
      unique()

    tagList(
      lapply(armyNames, function(x) {
        selectedArmy <- currentArmies() %>%
          filter(SavedName == unlist(x)) %>%
          mutate(actualScore = case_when(
            Value == "C" ~ as.numeric(Value),
            TRUE ~ as.numeric(Value) * as.numeric(Amount))
          )
        
        formattingArmy <- selectedArmy %>%
          dplyr::select(-User,-SavedName,-actualScore,-armySubfaction,-Faction) %>%
          mutate(Name = gsub("_"," ",Name)) %>%
          replace_na(list(`Psychogenics & Rituals` = "None",`Upgrades & Bio-Gens` = "None")) %>%
          mutate(`Psychogenics & Rituals` = case_when(
            `Psychogenics & Rituals` == "" ~ "None",
            TRUE ~ `Psychogenics & Rituals`
          ),
          `Upgrades & Bio-Gens` = case_when(
            `Upgrades & Bio-Gens` == "" ~ "None",
            TRUE ~ `Upgrades & Bio-Gens`
          ))
        
        tempArmy <<- formattingArmy
        
        mainArmy <- selectedArmy %>%
          pull(Faction) %>%
          unique()
        
        armySplash <- switch(mainArmy,
                             "Kukulkani" = "k3_back.jpg","Brood" = "brood_back.jpg",
                             "C.O.R.E" = "core_back.jpg","Forsaken" = "forsaken_back.jpg",
                             "Skarrd" = "skarrd_back.jpg","Outcasts" = "outcast_back.jpg",
                             "Dragryi" = "drag_back.jpg"
        )
        
        subArmy <- selectedArmy %>%
          pull(armySubfaction) %>%
          unique()

        droppedSub <- subArmy[!str_detect(subArmy,"Unaligned|Bounty Hunter|/")]
        
        if (length(droppedSub) == 0) {
          
          armyIcon <- switch(mainArmy,
                               "Kukulkani" = "faction_icons/logo_kukulkani.png","Brood" = "faction_icons/logo_brood_0.png",
                               "C.O.R.E" = "faction_icons/logo_core.png","Forsaken" = "faction_icons/logo_forsaken.png",
                               "Skarrd" = "faction_icons/logo_skaard.png","Outcasts" = "faction_icons/logo_outcasts.png",
                               "Dragryi" = "faction_icons/logo_dragyri.png"
          )
          
        } else {
          armyIcon <- switch(droppedSub,
                             "Air" = "faction_icons/logo_dragyri_air.png","Earth" = "faction_icons/logo_dragyri_earth.png",
                             "Fire" = "faction_icons/logo_dragyri_fire.png","Ice" = "faction_icons/logo_dragyri_ice.png",
                             "Shadow" = "faction_icons/logo_dragyri_shadow.png","Broodmere Spawn" = "faction_icons/broodspawn_front.png",
                             "Progeny" = "faction_icons/progeny_front.png","Blood" = "faction_icons/logo_skaard_blood.png",
                             "Decay" = "faction_icons/logo_skaard_decay.png","Metamorphosis" = "faction_icons/logo_skaard_metamorphosis.png",
                             "Toxic" = "faction_icons/logo_skaard_toxic.png","Mary" = "faction_icons/logo_forsaken_saint_mary.png",
                             "Isaac" = "faction_icons/logo_forsaken_saint_isaac.png",
                             "Luke" = "faction_icons/logo_forsaken_saint_luke.png",
                             "Joan" = "faction_icons/logo_forsaken_saint_joan.png",
                             "Heretic" = "faction_icons/logo_forsaken_saint_johann.png",
                             "John" = "faction_icons/logo_forsaken_saint_john.png",
                             "Mark" = "faction_icons/forsaken_mark.jpg", "Prevailer" = "faction_icons/logo_forsaken_prevailers.png",
                             "Scavengers" = "faction_icons/logo_outcasts.png","Court of Freeton" = "faction_icons/logo_outcasts.png",
                             "Barrow Slavers" = "faction_icons/logo_outcasts_slavers.png","Saltflat Nomads" = "faction_icons/logo_outcasts_salt_flats_nomads.png",
                             "Delta Broodfolk" = "faction_icons/delta_front.png"
          )
        }
        
        widgetUserBox(
          title = unlist(x),
          subtitle = paste0(sum(selectedArmy$actualScore)," point ",mainArmy," army"),
          width = 12,
          type = 2,
          src = armyIcon,
          color = "gray",
          div(style = "width:100%;overflow-y:scroll;",
              DT::datatable(
            cbind(' ' = 'Add-Ons &oplus;', formattingArmy
            ), escape = -2,
            options = list(
              dom = 't',
              columnDefs = list(
                list(visible = FALSE, targets = c(0, 7, 8)),
                list(orderable = FALSE, className = 'details-control', targets = 1)
              )
            ),
            width = "100%",
            height = "200px",
            callback = JS("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
    return '<div style=\"background-color:#eee; padding: .5em;\"> <strong>Upgrades/BioGens:</strong> ' +
            d[7] + '</br><strong>Psychogenics/Rituals:</strong> ' + d[8] + '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('Add-Ons &oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('Add-Ons &CircleMinus;');
    }
  });"
            ))),
          footer = HTML(paste0('<button id="',unlist(x),'" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange(&quot;removeArmy&quot;,  this.id + &quot;_&quot; + Math.random())">Remove Army</button>')),
          background = T,
          backgroundUrl = armySplash
        )
        })
    )
  })
  
  observeEvent(input$removeArmy,{
    armyName <- strsplit(input$removeArmy, "_")[[1]][1]
    
    confirmSweetAlert(
    session = session,
    inputId = "confirmRemove",
    type = "warning",
    title = "Confirm Delete?",
    html = T,
    btn_labels = c("Cancel",
                   "Delete"),
    text = paste0("Do you really want to delete army: ",armyName,"?"),
    danger_mode = TRUE
    )
    
  })
  
  observeEvent(input$confirmRemove,{
    armyName <- strsplit(input$removeArmy, "_")[[1]][1]
    if (input$confirmRemove) {
      
      db.SAB$remove(paste0('{"SavedName" : "',armyName,'"}'))
      
      mongoTrigger$trigger()
    
    
    sendSweetAlert(
      session,
      title = "Successful Deletion",
      text = paste0("Successfully deleted army: ",armyName),
      type = 'success'
    )
    }
  })
  
  observeEvent(input$subfaction_selection,{
    if (input$subfaction_selection %in% c("Blood","Metamorphosis","Toxic","Decay",
                                          "Broodmere Spawn","Delta Broodfolk","Progeny",
                                          "Ice","Fire","Earth","Shadow","Air",
                                          "Isaac","Luke","John","Joan","Mark","Mary","Heretic","Prevailer",
                                          "Court of Freeton","Saltflat Nomads","Scavengers","Barrow Slavers")) {
      
      armyIcon <- switch(input$subfaction_selection,
                         "Air" = "faction_icons/logo_dragyri_air.png","Earth" = "faction_icons/logo_dragyri_earth.png",
                         "Fire" = "faction_icons/logo_dragyri_fire.png","Ice" = "faction_icons/logo_dragyri_ice.png",
                         "Shadow" = "faction_icons/logo_dragyri_shadow.png","Broodmere Spawn" = "faction_icons/broodspawn_front.png",
                         "Progeny" = "faction_icons/progeny_front.png","Blood" = "faction_icons/logo_skaard_blood.png",
                         "Decay" = "faction_icons/logo_skaard_decay.png","Metamorphosis" = "faction_icons/logo_skaard_metamorphosis.png",
                         "Toxic" = "faction_icons/logo_skaard_toxic.png","Mary" = "faction_icons/logo_forsaken_saint_mary.png",
                         "Isaac" = "faction_icons/logo_forsaken_saint_isaac.png",
                         "Luke" = "faction_icons/logo_forsaken_saint_luke.png",
                         "Joan" = "faction_icons/logo_forsaken_saint_joan.png",
                         "Heretic" = "faction_icons/logo_forsaken_saint_johann.png",
                         "John" = "faction_icons/logo_forsaken_saint_john.png",
                         "Mark" = "faction_icons/forsaken_mark.jpg", "Prevailer" = "faction_icons/logo_forsaken_prevailers.png",
                         "Scavengers" = "faction_icons/logo_outcasts.png","Court of Freeton" = "faction_icons/logo_outcasts.png",
                         "Barrow Slavers" = "faction_icons/logo_outcasts_slavers.png",
                         "Saltflat Nomads" = "faction_icons/logo_outcasts_salt_flats_nomads.png",
                         "Delta Broodfolk" = "faction_icons/delta_front.png"
      )
      
      armyText <- switch(input$subfaction_selection,
                         "Air" = "As quick and ephemeral as their namesake, the Air Clan dodges their opponent's attacks while slicing into their foes like the wind from a hurricane.",
                         "Earth" = "As steady as the ground we stand upon, the Earth Clan is adept at controlling the field of battle itself.",
                         "Fire" = "Filled with rage, the Fire Clan can set their foes aflame with their blows.",
                         "Ice" = "The Ice Clan is an inexorable force on the battlefield, relying on their high HP and armor to outlast their opponents.",
                         "Shadow" = "With a slew of debuff and trickery abilities, the Shadow Clan disables their enemies before moving in for the kill.",
                         "Broodmere Spawn" = "Brood controlled by the Broodmere itself, the Spawn are the pinnacle of the Broodmere's evolutionary capabilities.",
                         "Progeny" = "Bio-Organic weaponry from the interior of Captain Jack Flay's starship, the Progeny are an especially destructive subset of the Brood.",
                         "Delta Broodfolk" = "Examples of the Broodmere's ability to warp any living being, the Delta Broodfolk are the first of many Brood Human hybrids.",
                         "Blood" = "Obsessed with sanguine vitae, the Blood Cult are empowered by spilling blood - be it their foe's or theirs.",
                         "Decay" = "Feeding on death, the Decay cult seek to kill everything on Samaria.",
                         "Metamorphosis" = "Extreme examples of the self-mutilation common in the Skarrd, the Metamorphosis cult fuse themselves with machinery to become more than human.",
                         "Toxic" = "Toxins and poisons fill the bodies of the Toxic Cult, and they share their pestilent gift with their foes.",
                         "Mary" = "Mary",
                         "Isaac" = "A capable hacker, Saint Isaac has resurrected a cache of ancient NorTec technology. Forces led by Isaac are able to field robots, drones, and other technological marvels.",
                         "Luke" = "Luke",
                         "Joan" = "Joan is the newest Saint, riding on a wave of inquisition.",
                         "Heretic" = "Forces of the Heretic Saint are composed of an unholy mixture of Forsaken and Skarrd units.",
                         "John" = "John",
                         "Mark" = "Mark",
                         "Prevailer" = "The Prevailer Council is the force behind the city of New Ashkelon - equipped with ancient technology, they are one of the pre-eminent technological forces on Samaria.",
                         "Scavengers" = "Scrounging for survival, Scavengers are able to make the most out of any wreckage on Samaria.",
                         "Court of Freeton" = "A free settlement of Brutes, the Court of Freeton are pound for pound the most dangerous humans on Samaria.",
                         "Barrow Slavers" = "A force of slave taking Outcasts, the Barrow Slavers are capable of inciting panic in their foes - regardless of who or what they are.",
                         "Saltflat Nomads" = "Nomadic and honor-bound, the Saltflat Nomads will take their vengeance on any who slay one of them."
      )
      
      sendSweetAlert(session,
                     title = paste0(input$army_selection,
                                    " : ",
                                    input$subfaction_selection),
                     text = armyText,
                     type = armyIcon,
                     html = T,
                     btn_labels = "Build")
      
    }
  })
  
  output$user_out <- DT::renderDT({
    datatable(
      currentArmies(),
      rownames = FALSE,
      options = list(
        dom = "tp",
        scrollX = TRUE
      )
    )
  })
  
  observe({
    #Observational JS effects
    #Animosity
    #Availability
    #*Tied* models
    
    req(input$army_value != 0)
    
    if (input$army_selection == "Outcasts") {
      if (input$subfaction_selection == "Court of Freeton" && !is.null(input[["JudgeBrooks"]])) {
        if (as.numeric(input[["JudgeBrooks"]]) == 1) {
          bruteAva <- as.integer(6) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          bruteOthAva <- as.integer(2) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "Brute",choices = as.character(seq(0,bruteAva)))
          updateSelectInput(session, inputId = "BruteAnchor",choices = as.character(seq(0,bruteOthAva)))
          updateSelectInput(session, inputId = "BrutePusher",choices = as.character(seq(0,bruteOthAva)))
        } else {
          bruteAva <- as.integer(3) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          bruteOthAva <- as.integer(1) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "Brute",choices = as.character(seq(0,bruteAva)),selected = "0")
          updateSelectInput(session, inputId = "BruteAnchor",choices = as.character(seq(0,bruteOthAva)),selected = "0")
          updateSelectInput(session, inputId = "BrutePusher",choices = as.character(seq(0,bruteOthAva)),selected = "0")
        }
      }
      
      if (input$subfaction_selection == "Saltflat Nomads") {
        if (!is.null(input[["Lynette"]])) {
          if (as.numeric(input[["Lynette"]]) == 1) {
            shinyjs::enable(selector = paste0("#Ideo > select"))
            shinyjs::enable(selector = paste0("#Vox > select"))
          } else {
            updateSelectInput(session, inputId = "Ideo", selected = "0")
            updateSelectInput(session, inputId = "Vox", selected = "0")
            shinyjs::disable(selector = paste0("#Ideo > select"))
            shinyjs::disable(selector = paste0("#Vox > select"))
          }
        }
      }
      
    }
    
    if (input$army_selection == "Dragyri") {
      if (input$subfaction_selection == "Ice" && !is.null(input[["SoulWarden(Ice)"]])) {
        if (as.numeric(input[["SoulWarden(Ice)"]]) > 0 ) {
          elemental <- as.integer(2) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          subElemental <- as.integer(6) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "IceElemental",choices = as.character(seq(0,elemental)))
          updateSelectInput(session, inputId = "HailKin",choices = as.character(seq(0,subElemental)))
        } else {
          elemental <- as.integer(1) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          subElemental <- as.integer(3) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "IceElemental",choices = as.character(seq(0,elemental)),selected = "0")
          updateSelectInput(session, inputId = "HailKin",choices = as.character(seq(0,subElemental)),selected = "0")
        }
      }
      if (input$subfaction_selection == "Earth" && !is.null(input[["SoulWarden(Earth)"]])) {
        if (as.numeric(input[["SoulWarden(Earth)"]])  > 0 ) {
          elemental <- as.integer(2) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          subElemental <- as.integer(4) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "GreaterAvalanceElemental",choices = as.character(seq(0,elemental)))
          updateSelectInput(session, inputId = "BedrockSentry",choices = as.character(seq(0,subElemental)))
        } else {
          elemental <- as.integer(1) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          subElemental <- as.integer(2) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "GreaterAvalanceElemental",choices = as.character(seq(0,elemental)),selected = "0")
          updateSelectInput(session, inputId = "BedrockSentry",choices = as.character(seq(0,subElemental)),selected = "0")
        }
      }
      if (input$subfaction_selection == "Shadow" && !is.null(input[["SoulWarden(Shadow)"]])) {
        if (as.numeric(input[["Huntress"]]) == 1) {
          elemental <- as.integer(4) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          subElemental <- as.integer(6) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "GreaterSpiderling",choices = as.character(seq(0,elemental)))
          updateSelectInput(session, inputId = "LesserSpiderling",choices = as.character(seq(0,subElemental)))
        } else {
          elemental <- as.integer(2) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          subElemental <- as.integer(3) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "GreaterSpiderling",choices = as.character(seq(0,elemental)),selected = "0")
          updateSelectInput(session, inputId = "LesserSpiderling",choices = as.character(seq(0,subElemental)),selected = "0")
        }
        if (as.numeric(input[["SoulWarden(Shadow)"]])  > 0 ) {
          elemental <- as.integer(2) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          #subElemental <- as.integer(4) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "UmbraShadowElemental",choices = as.character(seq(0,elemental)))
          #updateSelectInput(session, inputId = "BedrockSentry",choices = as.character(seq(0,subElemental)))
        } else {
          elemental <- as.integer(1) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          #subElemental <- as.integer(2) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "UmbraShadowElemental",choices = as.character(seq(0,elemental)),selected = "0")
          #updateSelectInput(session, inputId = "BedrockSentry",choices = as.character(seq(0,subElemental)),selected = "0")
        }
      }
      if (input$subfaction_selection == "Fire" && !is.null(input[["SoulWarden(Fire)"]])) {
        if (as.numeric(input[["SoulWarden(Fire)"]])  > 0 ) {
          elemental <- as.integer(2) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          #subElemental <- as.integer(6) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "FireElemental",choices = as.character(seq(0,elemental)))
          #updateSelectInput(session, inputId = "HailKin",choices = as.character(seq(0,subElemental)))
        } else {
          elemental <- as.integer(1) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          #subElemental <- as.integer(3) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "FireElemental",choices = as.character(seq(0,elemental)),selected = "0")
          #updateSelectInput(session, inputId = "HailKin",choices = as.character(seq(0,subElemental)),selected = "0")
        }
      }
      
      if (input$subfaction_selection == "Air" && !is.null(input[["SoulWarden(Air)"]])) {
        if (as.numeric(input[["SoulWarden(Air)"]])  > 0 ) {
          elemental <- as.integer(2) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          subElemental <- as.integer(6) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "StormElemental",choices = as.character(seq(0,elemental)))
          updateSelectInput(session, inputId = "Squall",choices = as.character(seq(0,subElemental)))
          updateSelectInput(session, inputId = "Zephyr",choices = as.character(seq(0,subElemental)))
        } else {
          elemental <- as.integer(1) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          subElemental <- as.integer(3) * ifelse(as.numeric(input$army_value) / 500 < 1, 1, floor(as.numeric(input$army_value) / 500))
          updateSelectInput(session, inputId = "StormElemental",choices = as.character(seq(0,elemental)),selected = "0")
          updateSelectInput(session, inputId = "Squall",choices = as.character(seq(0,subElemental)),selected = "0")
          updateSelectInput(session, inputId = "Zephyr",choices = as.character(seq(0,subElemental)),selected = "0")
        }
      }
    }
    
    if (input$army_selection == "Brood") {
      if (!is.null(input[["PudThrower"]])) {
        if (as.numeric(input[["PudThrower"]]) > 0 ||
            as.numeric(input[["Numbskull"]]) > 0 ||
            as.numeric(input[["Mandible"]]) > 0 ||
            as.numeric(input[["PudSwarm"]]) > 0 ||
            as.numeric(input[["Pod"]]) > 0) {
          shinyjs::enable(selector = paste0("#PudRoamer > select"))
          updateSelectInput(session, inputId = "PudRoamer",selected = "1")
        } else {
          shinyjs::disable(selector = paste0("#PudRoamer > select"))
          updateSelectInput(session, inputId = "PudRoamer",selected = "0")
        }
      }
    }
    
    if (input$army_selection == "Skarrd") {
      
      if (!is.null(input[["SisterofCharity"]])) {
        if (as.numeric(input[["SisterofCharity"]]) > 0) {
          shinyjs::enable(selector = paste0("#CharitysMight > select"))
          shinyjs::enable(selector = paste0("#CharitysZeal > select"))
        } else {
          shinyjs::disable(selector = paste0("#CharitysMight > select"))
          shinyjs::disable(selector = paste0("#CharitysZeal > select"))
        }
      } else {
        shinyjs::disable(selector = paste0("#CharitysMight > select"))
        shinyjs::disable(selector = paste0("#CharitysZeal > select"))
      }
      
      if (input$subfaction_selection == "Decay" && !is.null(input[["SisterofCharity"]])) {
        shinyjs::disable(selector = paste0("#Blazon > select"))
      } else {
        shinyjs::enable(selector = paste0("#Blazon > select"))
      }
      
      if (!is.null(input[["Dominique"]])) {
        if (as.numeric(input[["Dominique"]]) == 1) {
          shinyjs::enable(selector = paste0("#DominiquesChalica > select"))
        } else {
          shinyjs::disable(selector = paste0("#DominiquesChalica > select"))
        }
      } else {
        shinyjs::disable(selector = paste0("#DominiquesChalica > select"))
      }
    }
    
    if (input$army_selection == "Forsaken"){
      if (input$subfaction_selection == "Mark" && !is.null(input[["SaintMark"]])) {
        if (as.numeric(input[["X'Cess"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintMarkDragon > select"))
          shinyjs::toggleState(selector = paste0("#SaintMark > select"))
        } else if (as.numeric(input[["SaintMark"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#XCess > select"))
          shinyjs::toggleState(selector = paste0("#SaintMarkDragon > select"))
        } else if (as.numeric(input[["SaintMark(Dragon)"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#XCess > select"))
          shinyjs::toggleState(selector = paste0("#SaintMark > select"))
        } else {
          shinyjs::enable(selector = paste0("#XCess > select"))
          shinyjs::enable(selector = paste0("#SaintMark > select"))
          shinyjs::enable(selector = paste0("#SaintMarkDragon > select"))
        }
      }
      
      if (input$subfaction_selection == "Isaac" && !is.null(input[["SaintIsaac"]])) {
        if (as.numeric(input[["SaintIsaac"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintIsaacAJAX > select"))
        } else if (as.numeric(input[["SaintIsaac(AJAX)"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintIsaac > select"))
        } else {
          shinyjs::enable(selector = paste0("#SaintIsaacAJAX > select"))
          shinyjs::enable(selector = paste0("#SaintIsaac > select"))
        }
      }
      
      if (input$subfaction_selection == "Mary" && !is.null(input[["SaintMary"]])) {
        if (as.numeric(input[["SaintMary"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintMaryUnicorn > select"))
        } else if (as.numeric(input[["SaintMary(Unicorn)"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintMary > select"))
        } else {
          shinyjs::enable(selector = paste0("#SaintMaryUnicorn > select"))
          shinyjs::enable(selector = paste0("#SaintMary > select"))
        }
      }
      
      if (input$subfaction_selection == "Joan" && !is.null(input[["SaintJoan"]])) {
        if (as.numeric(input[["SaintJoan"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintJoanInquisitor > select"))
        } else if (as.numeric(input[["SaintJoan(Inquisitor)"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintJoan > select"))
        } else {
          shinyjs::enable(selector = paste0("#SaintJoanInquisitor > select"))
          shinyjs::enable(selector = paste0("#SaintJoan > select"))
        }
      }
      
      if (input$subfaction_selection == "John" && !is.null(input[["SaintJohn"]])) {
        if (as.numeric(input[["SaintJohn"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintJohnGriffon > select"))
        } else if (as.numeric(input[["SaintJohn(Griffon)"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintJohn > select"))
        } else {
          shinyjs::enable(selector = paste0("#SaintJohnGriffon > select"))
          shinyjs::enable(selector = paste0("#SaintJohn > select"))
        }
      }
      
      if (input$subfaction_selection == "Prevailer" && !is.null(input[["Esh"]])) {
        if (as.numeric(input[["Esh"]]) == 1) {
          shinyjs::enable(selector = paste0("#Zephon > select"))
        } else if (as.numeric(input[["GrandTemplarMarius"]]) == 1) {
          shinyjs::enable(selector = paste0("#Nabu > select"))
          shinyjs::enable(selector = paste0("#Yael > select"))
        } else {
          shinyjs::disable(selector = paste0("#Nabu > select"))
          shinyjs::disable(selector = paste0("#Yael > select"))
          shinyjs::disable(selector = paste0("#Zephon > select"))
        }
      } else {
        shinyjs::disable(selector = paste0("#Nabu > select"))
        shinyjs::disable(selector = paste0("#Yael > select"))
        shinyjs::disable(selector = paste0("#Zephon > select"))
      }
      
      if (input$subfaction_selection == "Heretic" && !is.null(input[["SaintJohann"]])) {
        if (as.numeric(input[["Dominique(Heretic)"]]) == 1) {
          shinyjs::enable(selector = paste0("#Buzzblade > select"))
        } else if (as.numeric(input[["WormShepherd(Heretic)"]]) > 1) {
          shinyjs::enable(selector = paste0("#Drillhead > select"))
        } else if (as.numeric(input[["Harboya"]]) == 1) {
          shinyjs::enable(selector = paste0("#Harpy > select"))
        }else {
          shinyjs::disable(selector = paste0("#Harpy > select"))
          shinyjs::disable(selector = paste0("#Drillhead > select"))
          shinyjs::disable(selector = paste0("#Buzzblade > select"))
        }
      } else {
        shinyjs::disable(selector = paste0("#Harpy > select"))
        shinyjs::disable(selector = paste0("#Drillhead > select"))
        shinyjs::disable(selector = paste0("#Buzzblade > select"))
      }
      
      if (input$subfaction_selection == "Luke" && !is.null(input[["SaintLuke(Bull)"]])) {
        if (as.numeric(input[["SaintLuke(Bull)"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintLukeSpear > select"))
          shinyjs::toggleState(selector = paste0("#SaintLukeShotgun > select"))
        } else if (as.numeric(input[["SaintLuke(Spear)"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintLukeBull > select"))
          shinyjs::toggleState(selector = paste0("#SaintLukeShotgun > select"))
        } else if (as.numeric(input[["SaintLuke(Shotgun)"]]) == 1) {
          shinyjs::toggleState(selector = paste0("#SaintLukeBull > select"))
          shinyjs::toggleState(selector = paste0("#SaintLukeSpear > select"))
        }else {
          shinyjs::enable(selector = paste0("#SaintLukeBull > select"))
          shinyjs::enable(selector = paste0("#SaintLukeSpear > select"))
          shinyjs::enable(selector = paste0("#SaintLukeShotgun > select"))
        }
      }
    }
    
    if (!is.null(input[["Aren"]])) {
      if (as.numeric(input[["Aren"]]) == 1) {
        shinyjs::enable(selector = paste0("#COREHound > select"))
        updateSelectInput(session,"C.O.R.EHound",choices = c("2"), selected = "2")
      } else {
        shinyjs::disable(selector = paste0("#COREHound > select"))
        updateSelectInput(session,"C.O.R.EHound",choices = c("0","1"), selected = "0")
      }
    } else {
      shinyjs::disable(selector = paste0("#COREHound > select"))
    }
    
    if (!is.null(input[["SuzyBelle"]])) {
      if (as.numeric(input[["SuzyBelle"]]) == 1) {
        shinyjs::enable(selector = paste0("#Wroth > select"))
        shinyjs::disable(selector = paste0("#JohnClankCarter > select"))
        shinyjs::disable(selector = paste0("#CaptainFlay > select"))
      } else {
        updateSelectInput(session, inputId = "Wroth", selected = "0")
        shinyjs::disable(selector = paste0("#Wroth > select"))
        shinyjs::enable(selector = paste0("#JohnClankCarter > select"))
        shinyjs::enable(selector = paste0("#CaptainFlay > select"))
      }
    }
    
    if (!is.null(input[["CaptainFlay"]]) || !is.null(input[["John'Clank'Carter"]])) {
      if (as.numeric(input[["CaptainFlay"]]) == 1 || as.numeric(input[["John'Clank'Carter"]]) == 1) {
        shinyjs::disable(selector = paste0("#SuzyBelle > select"))
      } else {
        shinyjs::enable(selector = paste0("#SuzyBelle > select"))
      }
    }
    
  })
  
}