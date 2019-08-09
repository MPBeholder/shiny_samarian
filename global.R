suppressPackageStartupMessages({
  library(shinyFiles)
  library(V8)
  library(waiter)
  library(shiny)
  library(tidyverse)
  library(lubridate)
  library(shinycssloaders)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyjs)
  library(shinyWidgets)
  library(DT)
  library(rintrojs)
  library(rmarkdown)
})

share <- list(
  title = "Dark Age Army Builder",
  url = "https://radevo.shinyapps.io/Samarian_Army_Builder/",
  description = "Before everything dies, you need an army.",
  twitter_user = "-"
)

source("misc/armyVariables.R")
source("modules/faction_module.R")
source("modules/firebase_module.R")

suppressWarnings(suppressMessages({
  faction.Df <- read_csv("misc/SamarianFrame.csv")
  psychogenic.Df <- read_csv("misc/PsychogenicFrame.csv")
  upgrades.Df <- read_csv("misc/UpgradeFrame.csv")
  faction.Rules <- read_csv("misc/factionRules.csv")
}))


jsCode <- "shinyjs.pushAnalytic = function(params){
var defaultParams = {
    action: 'general',
    category: 'general',
    label: 'general',
    value: 'general',
  };
  params = shinyjs.getParams(params, defaultParams);

  var gtagObject = {
    'event_category': params.category, 
    'event_label': params.label, 
    'value': params.value
  }

  gtag('event', params.action, gtagObject)
}"