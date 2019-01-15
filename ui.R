function(request){
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Factions", tabName = "home", icon = icon("vcard")),
      menuItem("Build", tabName = "build_army", icon = icon("cogs")),
      menuItem("Tournament", tabName = "build_tourney", icon = icon("users"))
    )
  )
  
  body <- dashboardBody(
    useShinyjs(),
    introjsUI(),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                samarianInfo('K3', box.Id = 1, faction = "Kukulkani"),
                samarianInfo('FS', box.Id = 2, faction = "Forsaken")
                  ),
              br(),
              fluidRow(
                samarianInfo('C', box.Id = 3, faction = "C.O.R.E"),
                samarianInfo('Brd', box.Id = 4, faction = "Brood")
              ),
              br(),
              fluidRow(
                samarianInfo('O', box.Id = 5, faction = "Outcasts"),
                samarianInfo('SK', box.Id = 6, faction = "Skarrd")
              ),
              br(),
              fluidRow(
                samarianInfo('DG', box.Id = 7, faction = "Dragyri")
              )
  ),
  tabItem(tabName = "build_army",
          pickerInput(
            inputId = "army_selection",
            label = "Selected Army", 
            choices = c("Brood","C.O.R.E","Dragyri","Forsaken","Outcasts","Kukulkani","Skarrd")#,
            # choicesOpt = list(
            #   subtext = HTML('<img class="avatar" src="K3.jpg" width = "20" height = "15" alt="Avatar">'))
          ),
          pickerInput(
            inputId = "subfaction_selection",
            label = "Selected Subfaction", 
            choices = c("")
          ),
          textInput("army_value", label = "Army Size (Points)"),
          textOutput("pointTotal"),
          actionButton("add1", "+ 1"),
          actionButton("sub1", "- 1"),
          actionButton("reset", "set to 0"),
          dataTableOutput("ArmyTable")
          ),
  tabItem(tabName = "build_tourney")
  )
  )
  
  header <- dashboardHeaderPlus(
    title = tagList(
      span(class = "logo-lg", "Samaria Lives"), 
      img(src = "smaller_icon.png", width = 20, height = 20)),
    enable_rightsidebar = FALSE,
    rightSidebarIcon = "gears"
  )
  
  title = "Samarian Army Builder"

  dashboardPagePlus(
    header,
    sidebar,
    body,
    title
  )
}