library(shiny)
library(tidyverse)
library(datamods)
library(data.table)
library(DT)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(DT::renderDataTable)
conflicted::conflicts_prefer(shiny::renderDataTable)


`%notin%`<- negate(`%in%`)

deck_parser <- function(deck_path) {
  deck <- read.delim(deck_path, header = FALSE, blank.lines.skip = FALSE) %>%
    filter(!str_detect(.$V1, regex("deck|Sideboard", ignore_case = TRUE))) %>%
    mutate(Side = str_detect(.$V1, regex("^$", ignore_case = TRUE))) %>%
    mutate(
      quantite = as.numeric(str_extract_all(.$V1, "^[:digit:]*\\S*")),
      Card_name = tolower(str_extract(.$V1, "(?<=[:digit:]\\s).*"))
    ) %>%
    select(-V1)
}

data_folder <- "../magic_data_entry/data/"


side_plan_from_entry <- readRDS(file.path(data_folder,"df_Side_table.rds"))



ui <- navbarPage(
  "Dashboard",
  
  # Side entry
  tabPanel(
    "Side_table",
    sidebarPanel(
      # Add side plan
      wellPanel(
        shinyjs::useShinyjs(),
        selectInput(
          "Deck_en_cours_side", "Deck name played",
          c("", unique(read_rds(file.path(outputDir, "modern_deck.rds"))$match_up))
        ),
        uiOutput("Color_deck"),
        
        textInput(
          "Player_side",
          "Player who played",
          value = ""
        ),
        dateInput(
          inputId = "date_side_plan",
          label = "Date du plan de side",
          value =  Sys.Date()
        ),
        textInput(
          "Deck_list_link",
          "Deck liste link if exist",
          value = ""
        ),
        textInput(
          "source_side_link",
          "Source side link if exist",
          value = ""
        ),
        textInput(
          "Note_sources_side",
          "Note on sources if exist",
          value = ""
        ),
        selectInput(
          "Deck_matchup_side", "Deck opponent played",
          c("", unique(read_rds(file.path(outputDir, "modern_deck.rds"))$match_up))
        ),
        
        uiOutput("Color_opponent"),
        
        selectInput(
          "Play_or_draw_side", "Play or Draw",
          c("Both", "Play", "Draw")
        ),
        textInput(
          "Side_plan_number_of_IN",
          "Enter a vector (comma delimited) of IN in side plan"
        ),
        uiOutput("IN_card_of_deck"),
        # verbatimTextOutput("text_in"),
        textInput(
          "Side_plan_number_of_OUT",
          "Enter a vector (comma delimited) of OUT in side plan"
        ),
        uiOutput("OUT_card_of_deck"),
        # verbatimTextOutput("text_out"),
        textInput(
          "Note_on_side_plan",
          "Note on current side plan",
          value = ""
        ),
        sliderInput("Fiabilite",
                    "Choose a fiability ",
                    min = 0L,
                    max = 10L,
                    step = 1L,
                    value = 5L
        ) # ,
      )
    )  ),
  id = "Tab_active_nav_bar"
)




server <- function(input, output, session) {
  
  
}