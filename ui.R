function(request){
  # Sidebar ---------------------------------
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Factions", tabName = "home", icon = icon("vcard")),
      menuItem("Build", tabName = "build_army", icon = icon("cogs"))#,
      #menuItem("Tournament", tabName = "build_tourney", icon = icon("users"))
    )
  )
  
  # Body ---------------------------------
  body <- dashboardBody(
    tags$head(tags$meta(property = "og:title", content = share$title),
              tags$meta(property = "og:type", content = "website"),
              tags$meta(property = "og:url", content = share$url),
              tags$meta(property = "og:description", content = share$description),
              tags$style(".swal-modal {width: 60%;}"),
              tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                               Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());})")),
              tags$style(HTML("
                              @media screen and (min-width: 768px){
                                .rwd-break { display: none; }
                              }

                              img.custom {
                                max-width: 100%;
                                max-height: 100%;
                                display: block;
                                margin-left: auto;
                                vertical-align: middle;
                                margin-right: auto;
                              }

                              .rotate-container {
                                padding-bottom:80px;
                              }
                              
                              .vertAlign {
                                position: relative;
                                top: 50%;
                                transform: translateY(20%);
                                text-align: center;   
                              }
                              "))),
    useShinyjs(),
    introjsUI(),
    useSweetAlert(),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                samarianInfo('K3', box.Id = 1, faction = "Kukulkani"),
                HTML('<br class="rwd-break">'),
                samarianInfo('FS', box.Id = 2, faction = "Forsaken")
              ),
              br(),
              fluidRow(
                samarianInfo('C', box.Id = 3, faction = "C.O.R.E"),
                HTML('<br class="rwd-break">'),
                samarianInfo('Brd', box.Id = 4, faction = "Brood")
              ),
              br(),
              fluidRow(
                samarianInfo('O', box.Id = 5, faction = "Outcasts"),
                HTML('<br class="rwd-break">'),
                samarianInfo('SK', box.Id = 6, faction = "Skarrd")
              ),
              HTML('<br class="rwd-break">'),
              fluidRow(
                samarianInfo('DG', box.Id = 7, faction = "Dragyri")
              )
      ),
      tabItem(tabName = "build_army",
              fluidRow(
                column(width = 2,
                       pickerInput(
                         inputId = "army_selection",
                         label = "Selected Army", 
                         choices = c("Brood","C.O.R.E","Dragyri","Forsaken","Outcasts","Kukulkani","Skarrd")
                       )),
                column(width = 2,
                       pickerInput(
                         inputId = "subfaction_selection",
                         label = "Selected Subfaction", 
                         choices = c("")
                       )),
                column(width = 2,
                       textInput("army_value", label = "Army Size (Points)")
                ),
                column(width = 3,
                       div(class = "vertAlign",
                           htmlOutput("currentValue"))
                ),
                column(width = 3, 
                       div(class = "vertAlign",
                           uiOutput("Downloader")))),
              DT::dataTableOutput("ArmyTable")#,
              # fluidRow(
              #   column(width = 4, uiOutput("Helper")),
              #   column(width = 8, uiOutput("Notesbox")))
      ),
      tabItem(tabName = "build_tourney")
    )
              )
  
  # Header ---------------------------------
  header <- dashboardHeaderPlus(
    title = tagList(
      span(class = "logo-lg", "Samaria Lives"), 
      img(src = "smaller_icon.png", width = 32, height = 25,style="padding-right:10px;")#,
    ),
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "info-circle"
  )
  
  # Right Sidebar ---------------------------------
  rightsidemenu <- rightSidebar(
    background = "dark",
    rightSidebarTabContent(
      id = 1,
      icon = "book",
      title = "Rules Reference",
      active = TRUE,
      pickerInput(inputId = "rule_selector", choices = rules.Reference[["Rules"]]),
      htmlOutput("ruleReference")
    ),
    rightSidebarTabContent(
      id = 2,
      icon = "info-circle",
      title = "General Info",
      active = FALSE,
      HTML("Samarian Army Builder v0.2</br>All logos and cards copyright CMON.</br>Code developed by MPBeholder.")
    ))
  
  # Title ---------------------------------
  
  title = "Samarian Army Builder"
  
  # Generate Page ---------------------------------
  dashboardPagePlus(
    collapse_sidebar = TRUE,
    header,
    sidebar,
    body,
    rightsidemenu,
    title
  )
  }