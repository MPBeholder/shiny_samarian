# UI Module(s) ---------------------------------
samarianInfo <- function(id, box.Id = 1, faction = "Kukulkani") {
  #' Generates faction specific UI elements
  #' 
  #' Generates an infoBox UI element specific to the specific faction.
  #' @param id instrument template to be called - used to generate namespace
  #' @param box.Id UI Box ID - needs to be unique
  #' @param faction Specific faction to generate UI element.

  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # Generate UI element
  column(width = 6,
         flipBox(
           id = box.Id,
           width = 6,
           main_img = faction.Data[[faction]][['back.Img']],
           header_img = faction.Data[[faction]][['header.Img']],
           front_title = faction,
           back_title = paste("About",faction,sep = " "),
           faction.Data[[faction]][['front.Blurb']],
           back_content = tagList(
             column(
               width = 12,
               align = "center",
               faction.Data[[faction]][['back.Blurb']],
               # splitLayout(HTML(faction.Data[[faction]][['pros']]),
               #             HTML(faction.Data[[faction]][['cons']]))
               fluidRow(class = "text-left",
                 column(6,HTML("<center>Pros:</center>"),
                        HTML(faction.Data[[faction]][['pros']])),
                 column(6,HTML("<center>Cons:</center>"),
                        HTML(faction.Data[[faction]][['cons']]))
               ),
               actionButton(paste0("army_",faction),"Build")
             )
           )
         ))
}

# Server Module(s) ---------------------------------
samarianInfoServer <- function(input, output, session, faction){
  #' Faction specific Server elements
  #' 
  #' Generates an observeEvent tied to the specific faction. 
  #' @param input Input element of shiny app
  #' @param output Output element of shiny app
  #' @param session Session element of shiny app
  #' @param faction Specific faction to generate Server element.
    observeEvent(input[[paste0("army_",faction)]],{
      updateTabItems(session,"tabs","build_army")
      updatePickerInput(session, "army_selection",selected = faction)
      
    })

}