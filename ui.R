function(request){
  # Sidebar ---------------------------------
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Factions", tabName = "home", icon = icon("vcard")),
      menuItem("Build", tabName = "build_army", icon = icon("cogs")),
      menuItem("My Armies", tabName = "army_login", icon = icon("users"))
      #shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE)
      #menuItem("Tournament", tabName = "build_tourney", icon = icon("users"))
    )
  )
  
  # Body ---------------------------------
  body <- dashboardBody(
    firebaseHead('SamarianBase',
               Sys.getenv("fbAPI"),
               Sys.getenv("fbDomain"),
               Sys.getenv("fbID")),
    ganalyticsUI('analytics',Sys.getenv("gaAPI")),
    tags$head(tags$link(rel="shortcut icon", href="DA_Logo3.png")),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "snackbar.css"),
      shiny::tags$script(src="snackbar.js"),
      shiny::tags$script(src="https://www.gstatic.com/firebasejs/5.7.0/firebase-app.js"),
      shiny::tags$script(src="https://www.gstatic.com/firebasejs/5.7.0/firebase-auth.js"),
      shiny::tags$script(src="sof-auth.js")
    ),
    shiny::tags$head(shiny::tags$meta(property = "og:title", content = share$title),
                     shiny::tags$meta(property = "og:type", content = "website"),
                     shiny::tags$meta(property = "og:url", content = share$url),
                     shiny::tags$meta(property = "og:description", content = share$description),
                     shiny::tags$style(".swal-modal {width: 100%;}"),
                     shiny::tags$style("@media (min-width:576px) {.swal-modal {width: 60%;}}"),
                     shiny::tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                               Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());})")),
                     shiny::tags$style(HTML("
                              @media screen and (min-width: 768px){
                                .rwd-break { display: none; }
                              }
                              
                              @media (max-width: 767px) {

.skin-blue .main-header .navbar .dropdown-menu li a {
	color: inherit;
}

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
                              
                              .swal-icon--custom {
                               max-width:88px;
                               min-width:88px;
                              }
                              
                              .vertAlign {
                                position: relative;
                                top: 50%;
                                transform: translateY(20%);
                                text-align: center;   
                              }
                              "))),
    useShinyjs(),
    extendShinyjs(text = jsCode),
    extendShinyjs(text = jsCodeToggle1),
    extendShinyjs(text = jsCodeToggle2),
    useSweetAlert(),
    use_waiter(),
    show_waiter_on_load(tagList(#HTML('<span class = "loadingText">Loading Samarian Army Builder...</span>'),
                                spin_fading_circles()),logo = "DA_Logo3.png"),
    tabItems(
      tabItem(tabName = "home",
              div(style = "overflow-y:auto;height:500px;",
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
              ))
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
                       numericInput("army_value", label = "Army Size (Points)",value = 0,min = 0)
                ),
                column(width = 3,
                       div(class = "vertAlign",
                           htmlOutput("currentValue"))
                ),
                column(width = 3, 
                       div(class = "vertAlign",
                           uiOutput("Downloader")))),
              DT::dataTableOutput("ArmyTable")#,
      ),
      tabItem(tabName = 'army_login',
              div(id = "armyBoxes", style = "overflow-y:auto;height:500px;",
                uiOutput("savedArmies")
              )
              #DT::DTOutput("user_out")
              ),
      tabItem(tabName = "build_tourney")
    )
              )
  
  # Left Sidebar ---------------------------------
  
  # leftsideMenu <- tagList(
  #   dropdownBlock(
  #     id = "mydropdown",
  #     title = "Dropdown 1",
  #     icon = icon("sliders"),
  #     sliderInput(
  #       inputId = "n",
  #       label = "Number of observations",
  #       min = 10, max = 100, value = 30
  #     )
  #   )
  # )
  
  # Header ---------------------------------
  header <- dashboardHeaderPlus(
    title = tagList(
      span(class = "logo-lg", "Samarian Army Builder"), 
      img(src = "smaller_icon.png", width = 32, height = 25,style="padding-right:10px;")#,
    ),
    enable_rightsidebar = TRUE,
    tags$li(class = "dropdown",
            style = "padding-top:8px",
            dropdownButton(
              inputId = "loginDropdown",
              label = "",
              icon = icon("user"),
              status = "info",
              right = T,
              circle = FALSE,
              tagList(
                source("modules/firebaseSignIn.R", local = TRUE)$value,
                source("modules/firebaseRegister.R", local = TRUE)$value,
                source("modules/firebaseVerify.R", local = TRUE)$value,
                HTML('<center>'),
                actionButton('submit_sign_out','Sign Out'),
                HTML('</center>')
              )
            )),
    rightSidebarIcon = "info-circle"
  )
  
  # Right Sidebar ---------------------------------
  rightsideMenu <- rightSidebar(
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
      HTML("Samarian Army Builder v0.8</br>All logos and cards copyright CMON.</br>Code developed by MPBeholder.")
    ))
  
  # Title ---------------------------------
  
  title = "Samarian Army Builder"
  
  # Generate Page ---------------------------------
  dashboardPagePlus(
    collapse_sidebar = TRUE,
    header,
    sidebar,
    body,
    rightsideMenu,
    title
  )
  }