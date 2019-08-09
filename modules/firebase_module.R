# UI Module(s) ---------------------------------
firebaseUI <- function(id,apiKey = "",authDomain = "",projectId = "") {
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