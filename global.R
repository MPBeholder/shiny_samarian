suppressMessages({
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

suppressWarnings({
  faction.Df <- read_csv("misc/SamarianFrame.csv")
  psychogenic.Df <- read_csv("misc/PsychogenicFrame.csv")
  upgrades.Df <- read_csv("misc/UpgradeFrame.csv")
  faction.Rules <- read_csv("misc/factionRules.csv")
})