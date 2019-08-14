# UI Module(s) ---------------------------------
firebaseHead <- function(id,apiKey = "",authDomain = "",projectId = "") {
  #' Includes requisite JS and CSS elements
  #' 
  #' Generates an infoBox UI element specific to the specific faction.
  #' @param id instrument template to be called - used to generate namespace
  #' @param apiKey Firebase API key
  #' @param authDomain Firebase auth domain
  #' @param projectId Firebase Project ID
  
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # Generate firebase tags
  shiny::tags$head(
    shiny::tags$script(HTML(paste0(
      'const config = {
       apiKey: "',apiKey,'",
       authDomain: "',authDomain,'",
       projectId: "',projectId,'"
       }'))),
      shiny::tags$style(HTML(
        '.auth_panel {
  width: 350px;
  max-width: 100%;
  margin: 0 auto;
  margin-top: 75px;
  border: 2px solid #eee;
  border-radius: 25px;
  padding: 30px;
  background: #f9f9f9;
}'
      ))
  )
}

ganalyticsUI <- function(id,trackingKey = "",userID) {
  #' Includes requisite JS and CSS elements
  #' 
  #' Generates an infoBox UI element specific to the specific faction.
  #' @param id instrument template to be called - used to generate namespace
  #' @param apiKey Firebase API key
  #' @param authDomain Firebase auth domain
  #' @param projectId Firebase Project ID
  
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # Generate firebase tags
  shiny::tags$head(
    shiny::tags$script(src="https://www.googletagmanager.com/gtag/js?id=",trackingKey),
    shiny::tags$script(HTML(paste0(
      "window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', '",trackingKey,"');
  
  $(document).one('shiny:idle', function() {
  gtag('config',
  'GA_MEASUREMENT_ID', {
  'user_id': Shiny.user
});
});
      ")))
  )
}

firebaseServer <- function(input,output,session) {
    ##### Switch Views ------------------
    # if user click link to register, go to register view
    observeEvent(input$go_to_register, {
      shinyjs::show("register_panel", anim = TRUE, animType = "fade")
      shinyjs::hide("sign_in_panel")
    }, ignoreInit = TRUE)
    
    observeEvent(input$go_to_sign_in, {
      shinyjs::hide("register_panel")
      shinyjs::show("sign_in_panel", anim = TRUE, animType = "fade")
    }, ignoreInit = TRUE)
    
    # switch between auth sign in/registration and app for signed in user
    observeEvent(session$userData$current_user(), {
      current_user <- session$userData$current_user()
      
      if (is.null(current_user)) {
        shinyjs::show("sign_in_panel")
        shinyjs::hide("main")
        shinyjs::hide("verify_email_view")
        shinyjs::hide("submit_sign_out")
        shinyjs::show("submitSignIn")
      } else {
        shinyjs::show("submit_sign_out")
        shinyjs::hide("submitSignIn")
        shinyjs::hide("sign_in_panel")
        shinyjs::hide("register_panel")
        
        if (current_user$emailVerified == TRUE) {
          shinyjs::show("main")
        } else {
          shinyjs::show("verify_email_view")
        }
        
      }
      
    }, ignoreNULL = FALSE)
    
    # Signed in user --------------------
    # the `session$userData$current_user()` reactiveVal will hold information about the user
    # that has signed in through Firebase.  A value of NULL will be used if the user is not
    # signed in
    session$userData$current_user <- reactiveVal(NULL)
    
    # input$sof_auth_user comes from front end js in "www/sof-auth.js"
    observeEvent(input$sof_auth_user, {
      
      # set the signed in user
      session$userData$current_user(input$sof_auth_user)
      
    }, ignoreNULL = FALSE)
}