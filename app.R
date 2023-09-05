library(shiny)
library(tidyverse)
library(datamods)
library(data.table)
library(DT)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(DT::renderDataTable)
conflicted::conflicts_prefer(shiny::renderDataTable)


`%notin%`<- negate(`%in%`)

# operator returning true if var NULL
`%==%` <- function (e1, e2) {
  if (is.null(e2)) {
    return(TRUE)
  } else {
    return(e1 == e2)
  }
}








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



Decklist_parse_add_type_fun <- function(path,Modern_card_DB_fun = Modern_card_DB){
  
  
  
  Modern_card_with_simplified_type <- Modern_card_DB_fun %>% 
    mutate(simplified_type = ifelse(
      str_detect(type_line,"Creature"),
      "Creature",
      ifelse(
        str_detect(type_line,"Land"),
        "Land",
        ifelse(
          str_detect(type_line,"Enchantment"),
          "Enchantment",
          ifelse(
            str_detect(type_line,"Instant|Sorcery"),
            "Spell",
            ifelse(
              str_detect(type_line,"Artifact"),
              "Artifact",
              ifelse(
                str_detect(type_line,"Planeswalker"),
                "Planeswalker", 
                ifelse(
                  str_detect(type_line,"Battle"),
                  "Battle", type_line
                  
                )
              )
              
              
            )
          )
          
        )
      )
    ),
    name = tolower(name)
    
    ) %>%
    drop_na(simplified_type) %>% 
    select(name,simplified_type)
  
  
  
  
  
  list_import_base <- deck_parser(
    
    path
  ) %>% 
    left_join(
      Modern_card_with_simplified_type,
      by = c("Card_name" = "name")
    )
  
  Side_cut_point <- which(list_import_base$Side)[1]
  
  
  list_import_base$Side[Side_cut_point:nrow(list_import_base)] <- TRUE
  
  list_import <- list_import_base[-Side_cut_point,]
  
  # list_import <- list(
  #   main_deck = list_import_base[
  #     1:which(list_import_base$Side)-1,] %>% select(-Side),
  #   sideboard = list_import_base[
  #     (which(list_import_base$Side)+1):
  #       nrow(list_import_base),] %>% select(-Side)
  # )
  
  return(list_import)
  
}












data_folder <- "../magic_data_entry/data/"

Modern_card_DB <- read.csv("data/DBcarte_modern.csv")

# side_plan_from_entry <- readRDS(file.path(data_folder,"df_Side_table.rds"))


liste_of_side_matchup <- list(
  all = c(unique(read_rds(file.path(data_folder, "modern_deck.rds"))$match_up)),
  all_with_color = (read_rds(file.path(data_folder, "modern_deck.rds")) %>%
                      rowwise() %>%
                      mutate(color = str_replace(color,"Any",""),
                             match_up_with_color = ifelse(
                               color == "",match_up,paste(match_up ,color ,sep =  " : ")
                               )
                             )
                    )$match_up_with_color,
  Tier_deck = c("BR midrange" ,
                "Hammer" ,
                "Yawgmoth",
                "Burn",
                "Tron-G",
                "Mill",
                "UR Murktide",
                "Mono B coffer",
                "Death shadow",
                "Jund",
                "Creativity",
                "Rhinos",
                "Topter",
                "UWx Control",
                "Omnath Control"
  )
  
  
  
  #,
  # joute_et_jeux = c("BR midrange" ,
  #                   "Belcher",
  #                   "Hammer" ,
  #                   "Yawgmoth",
  #                   "Burn",
  #                   "Tron-Eldrazi" ,
  #                   "Tron-G",
  #                   "Mill",
  #                   "UR Murktide",
  #                   "UR Prowess",
  #                   "Mono B coffer",
  #                   "Bring to light Nivmizet",
  #                   "Food",
  #                   "Infect",
  #                   "Devoted druid",
  #                   "Enchantress",
  #                   "Spirit",
  #                   "Death shadow",
  #                   "Jund",
  #                   )
                          
  )


options(DT.options = list(
  pageLength = 50,
  lengthMenu = c(5, 50, 100, 1000), 
  language = list(search = 'Filter:')
)
)



ui <- navbarPage(
  "Dashboard",
  # Side entry
  tabPanel(
    "Dash_board",
    sidebarPanel(
      wellPanel(
        selectInput(
          inputId ="Group_by_summary_side_matchup", 
          label = "Column to group by for number of matchup",
          choices =  c("Deck","color_deck","Companion","Wish_board","Player"),
          selected = c("Deck","color_deck","Companion","Wish_board"),
          multiple = TRUE
        )
      ),
      wellPanel(
        selectInput(
          inputId ="Deck_dash_side", 
          label = "Choose a deck",
          choices =  c("",unique(read_rds(file.path(data_folder, "df_Side_table.rds"))$Deck)
                       ),
          multiple = FALSE
        ),
        
        uiOutput("Deck_dash_color"),
        uiOutput("Deck_dash_companion"),
        uiOutput("Deck_dash_wishboard"),

        
        selectInput(
          inputId ="Deck_dash_side_grouping_variable", 
          label = "Choose a deck",
          choices =  c(NULL,"color_opponent","Play_Draw"),
          selected = NULL,
          multiple = TRUE
        )
      ),
      actionButton("browser", label ="browser" )
    ),
    mainPanel(
      wellPanel(
        fluidRow(
          column(width = 6,
                 DTOutput('Dash_number_of_matchup_per_deck')),
          column(width = 6,
                 DTOutput('Matchup_cover_by_deck')
        ))
      ),
      wellPanel(
        
      )
      
    )
  ),
  tabPanel(
    "Empty_Side_from_list_panel",
    sidebarPanel(
      wellPanel(
        shinyjs::useShinyjs(),
        fileInput("deck_list_for_side", NULL,
                  buttonLabel = "Upload...",
                  accept = c("text", ".txt")
        ),
        
        # uiOutput("Deck_dash_color"),
        
        selectInput(
                  "Deck_matchup_side", "Deck you want add in side table",
                  c("",
                    unique(
                      read_rds(
                        file.path(
                          data_folder,
                          "modern_deck.rds")
                        )$match_up)
                    ),
                  multiple = TRUE
                ),
        selectInput(
          "Deck_matchup_side_list", "Deck you want add in side table",
          c("",
            unique(
              names(liste_of_side_matchup)
              )
          ),
          multiple = TRUE
        ),
      actionButton("Create_side_table", "Create side table")
      )
    ),
    
    
    mainPanel(
      
      DTOutput('empty_Side_table_from_list')
      
    )
    ),
  tabPanel(
    "Side_from_data_entry_panel",
    sidebarPanel(
      wellPanel(
        shinyjs::useShinyjs(),
        uiOutput("Deck_from_entry_deck"),
        uiOutput("Deck_from_entry_color_deck"),
        uiOutput("Deck_from_entry_Companion"),
        uiOutput("Deck_from_entry_wishboard"),
        uiOutput("Deck_from_entry_Player"),
        h2("Choose list"),
        uiOutput("Deck_from_entry_Deck_list")
      )
    ),
    
    
    mainPanel(
      
      # DTOutput('empty_Side_table_from_list')
      
    )
  ),
  
  

  
  id = "Tab_active_nav_bar"
)




server <- function(input, output, session) {
  
  reload_data_switch_tab <- eventReactive(input$Tab_active_nav_bar, {
    data_matchup <- reactive(read_rds(
      file.path(
        data_folder,
        "modern_deck.rds"
      )
    ))
    
    df_Side_table <- reactive({
      read_rds(file.path(data_folder, "df_Side_table.rds"))
    })
    
    return(list(
      data_matchup = data_matchup,
      df_Side_table = df_Side_table
    ))
    
  })
  
  
  dash_board_df_matchup_group_by <- reactive({
    reload_data_switch_tab()$df_Side_table() %>%
      group_by(!!!rlang::syms(input$Group_by_summary_side_matchup)) %>%
      summarise(Number_of_matchup = n_distinct(Matchup),
                Tier_matchup = paste0(
                  round(
                    (sum(
                      unique(
                        Matchup) %in% liste_of_side_matchup$Tier_deck
                    )*100)/length(liste_of_side_matchup$Tier_deck)
                    ,0
                  ),
                  " %")
                ) 
  })
  
  output$Dash_number_of_matchup_per_deck <- renderDT(
    datatable(dash_board_df_matchup_group_by()
              
    )
  )
  
  
observeEvent( c(input$Deck_dash_side,input$Deck_dash_side_grouping_variable),{  

  df_Side_table_choosen_deck <-  reactive({
    reload_data_switch_tab()$df_Side_table() %>% 
    filter(Deck == input$Deck_dash_side)})

  
  output$Deck_dash_color <- renderUI({
    selectInput(
      inputId = "Deck_dash_side_color",
      label = "Deck color",
      choices = c(NULL, unique(df_Side_table_choosen_deck()$color_deck)),
      multiple = TRUE
    )
  })
  
  output$Deck_dash_companion <- renderUI({
    
    selectInput(
      inputId = "Deck_dash_side_companion",
      label = "Deck companion",
      choices = c(NULL, unique(df_Side_table_choosen_deck()$Companion)),
      multiple = TRUE
    )
  })
  
  output$Deck_dash_wishboard <- renderUI({
    
    selectInput(
      inputId = "Deck_dash_side_wishboard",
      label = "Deck wishboard",
      choices = c(NULL, unique(df_Side_table_choosen_deck()$Wish_board)),
      multiple = TRUE
    )
  })

  dash_board_df_matchup_fo_selected_deck <- reactive({
    df_Side_table_choosen_deck() %>% 
    filter(color_deck %==% input$Deck_dash_side_color,
           Companion %==% input$Deck_dash_side_companion,
           Wish_board %==% input$Deck_dash_side_wishboard) %>% 
        group_by(
          Matchup,
          !!!rlang::syms(input$Deck_dash_side_grouping_variable)
          ) %>% 
        summarise(
          last_added = max(Date),
          n = n()
        )})

  output$Matchup_cover_by_deck <- renderDT(
    datatable(dash_board_df_matchup_fo_selected_deck()
              
    )
  
  )
  
}
)
  
  
   
################################################################################  
############################   Empty_side_from_list  ###########################
################################################################################    
Deck_list_empty_side_clean <- reactive({
  req(input$deck_list_for_side)    

df_choosen_deck_side_plan <-  reactive({
      reload_data_switch_tab()$df_Side_table() %>% 
        filter(Deck == input$Deck_dash_side)})
  

Deck_list_empty_side_parse <- Decklist_parse_add_type_fun(
  input$deck_list_for_side$datapath
  # "../magic_data_entry/data/FB_joute_23_08.txt"
  )


Deck_list_empty_side_clean <- Deck_list_empty_side_parse %>%
  mutate(
    Side = 
      ifelse(Side,
             "Side",
             " ")
    ) %>% 
  add_column(!!!set_names(rep(0, length(liste_of_side_matchup$Tier_deck )), liste_of_side_matchup$Tier_deck ))  %>% 
  arrange(Side,
          simplified_type)

})

output$empty_Side_table_from_list <- renderDT({
  datatable(Deck_list_empty_side_clean(),
             rownames = FALSE,
            extensions = c("Buttons",'RowGroup'),
            options = list(
              rowGroup =
                list(
                  dataSrc = c(0,3) # disp
                  ),
              columnDefs = list(
                list(
                  visible=FALSE, 
                  targets=c(0,3)
                  )
                ),
              ordering=FALSE,
              dom = 'tB',
              buttons = c('copy', 'csv', 'excel')
              ),
            
            editable = list(target = "cell", disable = list(columns = c(0:3)))
            
  )},
  
)

################################################################################  
############################   side_from_data_entry  ###########################
################################################################################   

# a <- read_rds(file.path(data_folder, "df_Side_table.rds"))
# 
# colnames(a)



Side_from_entry_filter <- reactive({
  reload_data_switch_tab()$df_Side_table() %>%
    filter(
      Deck %==% input$Deck_from_entry_deck,
      color_deck %==% input$Deck_from_entry_color_deck,
      Companion %==% input$Deck_from_entry_Companion,
      Wish_board %==% input$Deck_from_entry_wishboard,
      Player %==% input$Deck_from_entry_Player
           )
  }
)



output$Deck_from_entry_deck <- renderUI({
  
  selectInput(
    inputId = "Deck_from_entry_Deck",
    label = "Deck",
    choices = c(NULL, unique(Side_from_entry_filter()$Deck)),
    multiple = TRUE
  )
})


output$Deck_from_entry_color_deck <- renderUI({
  
  selectInput(
    inputId = "Deck_from_entry_color_deck",
    label = "Color deck",
    choices = c(NULL, unique(Side_from_entry_filter()$color_deck)),
    multiple = TRUE
  )
})


output$Deck_from_entry_Companion <- renderUI({
  
  selectInput(
    inputId = "Deck_from_entry_Companion",
    label = "Companion",
    choices = c(NULL, unique(Side_from_entry_filter()$Companion)),
    multiple = TRUE
  )
})


output$Deck_from_entry_wishboard <- renderUI({
  selectInput(
    inputId = "Deck_from_entry_wishboard",
    label = "Wishboard",
    choices = c(NULL, unique(Side_from_entry_filter()$Wish_board)),
    multiple = TRUE
  )
})



output$Deck_from_entry_Player <- renderUI({
  selectInput(
    inputId = "Deck_from_entry_Player",
    label = "Player",
    choices = c(NULL, unique(Side_from_entry_filter()$Player)),
    multiple = TRUE
  )
})



# Need to add date

output$Deck_from_entry_Deck_list <- renderUI({
  selectInput(
    inputId = "Deck_from_entry_link_deck_list",
    label = "Wishboard",
    choices = c(NULL, unique(Side_from_entry_filter()$link_deck_list)),
    multiple = TRUE
  )
})




  
}








shinyApp(ui, server)






# Side entry
# tabPanel(
#   "Side_table",
#   sidebarPanel(
#     # Add side plan
#     wellPanel(
#       shinyjs::useShinyjs(),
#       selectInput(
#         "Deck_en_cours_side", "Deck name played",
#         c("", unique(read_rds(file.path(outputDir, "modern_deck.rds"))$match_up))
#       ),
#       uiOutput("Color_deck"),
#       
#       textInput(
#         "Player_side",
#         "Player who played",
#         value = ""
#       ),
#       dateInput(
#         inputId = "date_side_plan",
#         label = "Date du plan de side",
#         value =  Sys.Date()
#       ),
#       textInput(
#         "Deck_list_link",
#         "Deck liste link if exist",
#         value = ""
#       ),
#       textInput(
#         "source_side_link",
#         "Source side link if exist",
#         value = ""
#       ),
#       textInput(
#         "Note_sources_side",
#         "Note on sources if exist",
#         value = ""
#       ),
#       selectInput(
#         "Deck_matchup_side", "Deck opponent played",
#         c("", unique(read_rds(file.path(outputDir, "modern_deck.rds"))$match_up))
#       ),
#       
#       uiOutput("Color_opponent"),
#       
#       selectInput(
#         "Play_or_draw_side", "Play or Draw",
#         c("Both", "Play", "Draw")
#       ),
#       textInput(
#         "Side_plan_number_of_IN",
#         "Enter a vector (comma delimited) of IN in side plan"
#       ),
#       uiOutput("IN_card_of_deck"),
#       # verbatimTextOutput("text_in"),
#       textInput(
#         "Side_plan_number_of_OUT",
#         "Enter a vector (comma delimited) of OUT in side plan"
#       ),
#       uiOutput("OUT_card_of_deck"),
#       # verbatimTextOutput("text_out"),
#       textInput(
#         "Note_on_side_plan",
#         "Note on current side plan",
#         value = ""
#       ),
#       sliderInput("Fiabilite",
#                   "Choose a fiability ",
#                   min = 0L,
#                   max = 10L,
#                   step = 1L,
#                   value = 5L
#       ) # ,
#     )
#   )  
#   ),