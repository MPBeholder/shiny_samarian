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
    tags$head(tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                         Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());})")),
              tags$script(HTML("Shiny.addCustomMessageHandler('reset-tooltip', function(val) {
                         $('a[data-toggle=\"tooltip\"]').tooltip({
    animated: 'fade',
    placement: 'bottom',
    html: true
});})"))),
    tags$style(type = "text/css",
               HTML(".imgTooltip {
                    display: none;
                    }
                    
                    .ItemsTooltip:hover .imgTooltip {
                    display: block;
                    position: absolute;
                    z-index: 1;
                    }"
                    )
    ),
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
          fluidRow(
            column(width = 3,
          pickerInput(
            inputId = "army_selection",
            label = "Selected Army", 
            choices = c("Brood","C.O.R.E","Dragyri","Forsaken","Outcasts","Kukulkani","Skarrd")#,
            # choicesOpt = list(
            #   subtext = HTML('<img class="avatar" src="K3.jpg" width = "20" height = "15" alt="Avatar">'))
          )),
          column(width = 3,
          pickerInput(
            inputId = "subfaction_selection",
            label = "Selected Subfaction", 
            choices = c("")
          )),
          column(width = 3,
          textInput("army_value", label = "Army Size (Points)")
          ),
          column(width = 3,
                 htmlOutput("currentValue")
          )),
          DT::dataTableOutput("ArmyTable")
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