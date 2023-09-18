library(shiny)
library(tidyverse)
library(datamods)
library(data.table)
library(DT)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(DT::renderDataTable)
conflicted::conflicts_prefer(shiny::renderDataTable)

source("sources.R")


# TO DO
# fetch optimizer debug real optimum
# check if deck is opti



Data_from_other_repo <- "../magic_data_entry/"


data_folder <- paste0(Data_from_other_repo,"data/")



Modern_card_DB <- read.csv("data/DBcarte_modern.csv")



side_plan_from_entry_base <- readRDS(file.path(data_folder,"df_Side_table.rds"))


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
      )
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
          selectInput(
            inputId = "Deck_from_entry_deck_test",
            label = "Deck",
            choices = c("", unique(side_plan_from_entry_base$Deck)),
            multiple = FALSE
      ),
        
        # uiOutput("Deck_from_entry_deck_test"),
        uiOutput("Deck_from_entry_color_deck"),
        uiOutput("Deck_from_entry_Companion"),
        uiOutput("Deck_from_entry_wishboard"),
        uiOutput("Deck_from_entry_Player"),
        h2("Choose list"),
      actionButton("browser", label ="browser" ),
        uiOutput("Deck_from_entry_Deck_list")
      )
    ),
    
    
    mainPanel(
      
      # DTOutput('empty_Side_table_from_list')
      
    )
  ),
  tabPanel(
    "Fetch_optimizer",
    sidebarPanel(
      wellPanel(
        shinyjs::useShinyjs(),
        selectInput(
          inputId = "Deck_type_fetch",
          label = "Deck",
          choices = c("", unique(side_plan_from_entry_base$Deck)),
          multiple = FALSE
        ),
        uiOutput("Deck_type_fetch_color_deck"),
        uiOutput("Deck_type_fetch_Companion"),
        uiOutput("Deck_type_fetch_wishboard"),
        uiOutput("Deck_type_fetch_Player"),
        h2("Choose list"),
        uiOutput("Deck_type_fetch_Deck_list")
      ),
      wellPanel(
        h2("Upload list"),
        fileInput("deck_list_fetch_opti", 
                  NULL,
                  buttonLabel = "Upload...",
                  accept = c(".csv", ".txt")
        ),
      )
    ),
    
    
    mainPanel(
     h2( textOutput("res_text_opti_fetch")),
      DTOutput('efetch_opti_res_base', width = "50%"),
      DTOutput('efetch_opti_res_number_of_fetchable', width = "50%")
      
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
observeEvent(input$browser, {
  browser()
})


observeEvent( input$Deck_from_entry_deck_test,{  
  
  Side_from_entry_filter_with_deck <- reactive({
    reload_data_switch_tab()$df_Side_table() %>%
      filter(
        Deck == input$Deck_from_entry_deck_test
      )
  }
  )
 
  output$Deck_from_entry_color_deck <- renderUI({
    auto_select_input_if_one_choice(
      inputId = "Deck_from_entry_color_deck",
      label = "Color deck",
       choices = unique(Side_from_entry_filter_with_deck()$color_deck), #c(NULL, unique(Side_from_entry_filter_with_deck()$color_deck)),
      multiple = TRUE
    )
  })
  
  output$Deck_from_entry_Companion <- renderUI({
    auto_select_input_if_one_choice(
      inputId = "Deck_from_entry_Companion",
      label = "Companion",
      choices = unique(Side_from_entry_filter_with_deck()$Companion),#c(NULL, unique(Side_from_entry_filter_with_deck()$Companion)),
      multiple = TRUE
    )
  })
  
  output$Deck_from_entry_wishboard <- renderUI({
    auto_select_input_if_one_choice(
      inputId = "Deck_from_entry_wishboard",
      label = "Wishboard",
      choices = unique(Side_from_entry_filter_with_deck()$Wish_board),#c(NULL, unique(Side_from_entry_filter_with_deck()$Wish_board)),
      multiple = TRUE
    )
  })
  output$Deck_from_entry_Player <- renderUI({
    auto_select_input_if_one_choice(
      inputId = "Deck_from_entry_Player",
      label = "Player",
      choices = c(NULL, unique(Side_from_entry_filter_with_deck()$Player)),
      multiple = TRUE
    )

  })
  
  
})

# # Need to add date
Side_from_entry_filter <- reactive({
  reload_data_switch_tab()$df_Side_table() %>%
    filter(
      Deck %==% input$Deck_from_entry_deck_test,
      color_deck %==% input$Deck_from_entry_color_deck,
      Companion %==% input$Deck_from_entry_Companion,
      Wish_board %==% input$Deck_from_entry_wishboard,
      Player %==% input$Deck_from_entry_Player
    )
}
)

output$Deck_from_entry_Deck_list <- renderUI({
  selectInput(
    inputId = "Deck_from_entry_link_deck_list",
    label = "list",
    choices = c("","All", unique(Side_from_entry_filter()$link_deck_list)),
    multiple = TRUE
  )
})



################################################################################  
############################   Fetch optimizer  ###########################
################################################################################   

observeEvent( input$Deck_type_fetch,{  
  
  fetch_opti_filter_with_deck <- reactive({
    reload_data_switch_tab()$df_Side_table() %>%
      filter(
        Deck == input$Deck_type_fetch
      )
  }
  )
  
  
  output$Deck_type_fetch_color_deck <- renderUI({
    auto_select_input_if_one_choice(
      inputId = "Deck_type_fetch_color_deck",
      label = "Color deck",
      choices = unique(fetch_opti_filter_with_deck()$color_deck), 
      multiple = TRUE
    )
  })
  
  output$Deck_type_fetch_Companion <- renderUI({
    auto_select_input_if_one_choice(
      inputId = "Deck_type_fetch_Companion",
      label = "Companion",
      choices = unique(fetch_opti_filter_with_deck()$Companion),
      multiple = TRUE
    )
  })
  
  output$Deck_type_fetch_wishboard <- renderUI({
    auto_select_input_if_one_choice(
      inputId = "Deck_type_fetch_wishboard",
      label = "Wishboard",
      choices = unique(fetch_opti_filter_with_deck()$Wish_board),
      multiple = TRUE
    )
  })
  output$Deck_type_fetch_Player <- renderUI({
    auto_select_input_if_one_choice(
      inputId = "Deck_type_fetch_Player",
      label = "Player",
      choices = c(NULL, unique(fetch_opti_filter_with_deck()$Player)),
      multiple = TRUE
    )
    
  })
  
  
})

# # Need to add date
fetch_from_entry_filter <- reactive({
  reload_data_switch_tab()$df_Side_table() %>%
    filter(
      Deck %==% input$Deck_type_fetch,
      color_deck %==% input$Deck_type_fetch_color_deck,
      Companion %==% input$Deck_type_fetch_Companion,
      Wish_board %==% input$Deck_type_fetch_wishboard,
      Player %==% input$Deck_type_fetch_Player
    )
}
)

output$Deck_type_fetch_Deck_list <- renderUI({
  selectInput(
    inputId = "Deck_type_fetch_Deck_list",
    label = "list",
    choices = c("","All", unique(fetch_from_entry_filter()$link_deck_list)),
    multiple = FALSE
  )
})

fileInput("deck_list_fetch_opti", 
          NULL,
          buttonLabel = "Upload...",
          accept = c(".csv", ".txt")
)


result_opti <- eventReactive({
  input$deck_list_fetch_opti
  input$Deck_type_fetch_Deck_list
  },{
    # browser()
    if (input$Deck_type_fetch_Deck_list != ""){
  result_opti <- Fetch_land_optimizer(
    file.path(Data_from_other_repo,input$Deck_type_fetch_Deck_list),
                            Modern_card_DB,
                            list_format ="csv"
    )
    } else if(!is.null(input$deck_list_fetch_opti)){

      if (!(tools::file_ext(input$deck_list_fetch_opti$datapath) %in% c('csv',"txt"))){
        validate("file must be .txt or .csv")
      }
      
      
      
      result_opti <- Fetch_land_optimizer(
        input$deck_list_fetch_opti$datapath,
        Modern_card_DB,
        list_format =tools::file_ext(input$deck_list_fetch_opti$datapath))
      
      
      
    }
    

    
    
    

  }
  
)







observeEvent(req(result_opti()),{

  output$res_text_opti_fetch <- renderText({ result_opti()$text })
  
  output$efetch_opti_res_base <- renderDT({
    req(result_opti())
    datatable(result_opti()$result_base,
              rownames = FALSE,
              # filter = "top",
              options = list(
                dom = 'tB'
              )
              
    )}
  )

    output$efetch_opti_res_number_of_fetchable <- renderDT({
      req(result_opti())
      datatable(result_opti()$result,
                rownames = FALSE,
                # filter = "top",
                options = list(
                  dom = 'tB'
                )

      )}

    )
})

}








shinyApp(ui, server)




# a <- side_plan_from_entry_base %>%
#   filter(link_deck_list %in% c(
#     "data/deck_list/Omnath Control/Any_Kaheera, the Orphanguard_No wish board_RespectTheCat_2023-09-08_Mtgo leagu_.csv",
#     "data/deck_list/Omnath Control/Any_Kaheera, the Orphanguard_No wish board_Andrea Mengucci_2023-09-08_VEEDEO_.csv"
#                                )
#     )




d <- a %>%
  mutate(
    hash_tot = paste0(hash_deck,hash_side),
    IN = str_split(IN," ; "),
    OUT = str_split(OUT," ; ")
  ) %>%
  rownames_to_column() %>% 
  pivot_longer(cols = c(IN,OUT)

               ) %>%
  unnest_longer(col = c(value)
  )




e <- d %>% 
  mutate(quantite = str_extract(value,"^\\d{1}"),
         value = str_remove(value,"^\\d{1}"))

f <- e %>% 
  group_by(rowname,name) %>% 
  summarise(b = paste0(sort(value),collapse = " / "),.groups = "drop") 


  

  
f %>% janitor::get_dupes(b) %>%
  inner_join( a %>% rownames_to_column(),by = c("rowname")) %>% 
  view()












b <-  lapply(unique(a$link_deck_list), function(x){
  list_loaded_x <- read.csv(
    paste0(Data_from_other_repo,
           x
    )
  )
  main_deck_en_cours_x <- list_loaded_x %>%
    filter(!Side) %>%
    select(-Side) %>%
    arrange(Card_name)
  side_en_cours_x <- list_loaded_x %>%
    filter(Side) %>%
    select(-Side) %>%
    arrange(Card_name)
  return(
    list(
     path = x,
     main = list(main_deck_en_cours_x),
     side = list(side_en_cours_x),
     hash_deck = digest(main_deck_en_cours_x),
     hash_side = digest(side_en_cours_x)
              )
    )
}
) 





x <- unique(a$link_deck_list)[1]
names(b) <- unique(a$link_deck_list)


d <- b %>% unique()


a <- side_plan_from_entry_base %>%
  filter(Player == "mistakenn69"
  ) #%>% 
  # select(Matchup,Play_Draw,IN,OUT)



e <- lapply(b, function(x) digest(x)) %>% unlist()

f <- data.frame(a = names(e),b = e)













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











################################################################################


# old dynamic multiple input selection

################################################################################


# Side_from_entry_filter <- reactive({
#   reload_data_switch_tab()$df_Side_table() %>%
#     filter(
#       Deck %==% input$Deck_from_entry_deck_test,
#       color_deck %==% input$Deck_from_entry_color_deck,
#       Companion %==% input$Deck_from_entry_Companion,
#       Wish_board %==% input$Deck_from_entry_wishboard,
#       Player %==% input$Deck_from_entry_Player
#     )
# }
# )
# 
# 
# 
# 
# 
# 
# # output$Deck_from_entry_deck_test <- renderUI({
# #   selectInput(
# #     inputId = "Deck_from_entry_deck_test",
# #     label = "Deck",
# #     choices = c(NULL, unique(Side_from_entry_filter()$Deck)),
# #     multiple = TRUE
# #   )
# # })
# 
# output$Deck_from_entry_deck_test <- renderUI({
#   
#   choice_Deck <- reactive({
#     reload_data_switch_tab()$df_Side_table() %>%
#       filter(
#         color_deck %==% input$Deck_from_entry_color_deck,
#         Companion %==% input$Deck_from_entry_Companion,
#         Wish_board %==% input$Deck_from_entry_wishboard,
#         Player %==% input$Deck_from_entry_Player
#       ) %>%
#       pull(Deck)
#   }
#   )
#   
#   
#   selectInput(
#     inputId = "Deck_from_entry_deck_test",
#     label = "Deck",
#     choices = c(NULL, unique(choice_Deck())),
#     multiple = TRUE
#   )
# })
# 
# 
# 
# # output$Deck_from_entry_color_deck <- renderUI({
# #   selectInput(
# #     inputId = "Deck_from_entry_color_deck",
# #     label = "Color deck",
# #     choices = c(NULL, unique(Side_from_entry_filter()$color_deck)),
# #     multiple = TRUE
# #   )
# # })
# output$Deck_from_entry_color_deck <- renderUI({
#   
#   choice_color_deck <- reactive({
#     reload_data_switch_tab()$df_Side_table() %>%
#       filter(
#         Deck %==% input$Deck_from_entry_deck_test,
#         Companion %==% input$Deck_from_entry_Companion,
#         Wish_board %==% input$Deck_from_entry_wishboard,
#         Player %==% input$Deck_from_entry_Player
#       ) %>%
#       pull(color_deck)
#   }
#   )
#   
#   selectInput(
#     inputId = "Deck_from_entry_color_deck",
#     label = "Color deck",
#     choices = c(NULL, unique(choice_color_deck())),
#     multiple = TRUE
#   )
# })
# 
# 
# # output$Deck_from_entry_Companion <- renderUI({
# #   selectInput(
# #     inputId = "Deck_from_entry_Companion",
# #     label = "Companion",
# #     choices = c(NULL, unique(Side_from_entry_filter()$Companion)),
# #     multiple = TRUE
# #   )
# # })
# output$Deck_from_entry_Companion <- renderUI({
#   choice_Companion <- reactive({
#     reload_data_switch_tab()$df_Side_table() %>%
#       filter(
#         Deck %==% input$Deck_from_entry_deck_test,
#         color_deck %==% input$Deck_from_entry_color_deck,
#         Wish_board %==% input$Deck_from_entry_wishboard,
#         Player %==% input$Deck_from_entry_Player
#       ) %>%
#       pull(Companion)
#   }
#   )
#   selectInput(
#     inputId = "Deck_from_entry_Companion",
#     label = "Companion",
#     choices = c(NULL, unique(choice_Companion())),
#     multiple = TRUE
#   )
# })
# 
# # output$Deck_from_entry_wishboard <- renderUI({
# #   selectInput(
# #     inputId = "Deck_from_entry_wishboard",
# #     label = "Wishboard",
# #     choices = c(NULL, unique(Side_from_entry_filter()$Wish_board)),
# #     multiple = TRUE
# #   )
# # })
# 
# output$Deck_from_entry_wishboard <- renderUI({
#   choice_Wish_board <- reactive({
#     reload_data_switch_tab()$df_Side_table() %>%
#       # Side_from_entry_filter() %>% 
#       filter(
#         Deck %==% input$Deck_from_entry_deck_test,
#         color_deck %==% input$Deck_from_entry_color_deck,
#         Companion %==% input$Deck_from_entry_Companion,
#         Player %==% input$Deck_from_entry_Player
#       ) %>%
#       pull(Wish_board)
#   }
#   )
#   
#   
#   
#   selectInput(
#     inputId = "Deck_from_entry_wishboard",
#     label = "Wishboard",
#     choices = c(NULL, unique(choice_Wish_board())),
#     multiple = TRUE
#   )
# })
# 
# 
# 
# # output$Deck_from_entry_Player <- renderUI({
# #   selectInput(
# #     inputId = "Deck_from_entry_Player",
# #     label = "Player",
# #     choices = c(NULL, unique(Side_from_entry_filter()$Player)),
# #     multiple = TRUE
# #   )
# #   
# # })
# 
# output$Deck_from_entry_Player <- renderUI({
#   choice_player <- reactive({
#     reload_data_switch_tab()$df_Side_table() %>%
#       filter(
#         Deck %==% input$Deck_from_entry_deck_test,
#         color_deck %==% input$Deck_from_entry_color_deck,
#         Companion %==% input$Deck_from_entry_Companion,
#         Wish_board %==% input$Deck_from_entry_wishboard
#       ) %>%
#       pull(Player)
#   }
#   )
#   
#   
#   selectInput(
#     inputId = "Deck_from_entry_Player",
#     label = "Player",
#     choices = c(NULL, unique(choice_player())),
#     multiple = TRUE
#   )
# })
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # # Need to add date
# # 
# # 
# # observeEvent(Side_from_entry_filter(),{
# #     updateSelectInput(
# #       session,
# #       inputId = "Deck_from_entry_deck_test",
# #       choices = c(NULL, unique(Side_from_entry_filter()$Deck)),
# #       selected = input$Deck_from_entry_deck_test
# #     )
# # 
# #     updateSelectInput(
# #       session,
# #       inputId = "Deck_from_entry_color_deck",
# #       choices = c(NULL, unique(Side_from_entry_filter()$color_deck)),
# #       selected = input$Deck_from_entry_color_deck
# #     )
# # 
# #     updateSelectInput(
# #       session,
# #       inputId = "Deck_from_entry_Companion",
# #       choices = c(NULL, unique(Side_from_entry_filter()$Companion)),
# #       selected = input$Deck_from_entry_Companion
# #     )
# # 
# #     updateSelectInput(
# #       session,
# #       inputId = "Deck_from_entry_wishboard",
# #       choices = c(NULL, unique(Side_from_entry_filter()$Wish_board)),
# #       selected = input$Deck_from_entry_wishboard
# #     )
# # 
# #     updateSelectInput(
# #       session,
# #       inputId = "Deck_from_entry_Player",
# #       choices = c(NULL, unique(Side_from_entry_filter()$Player)),
# #       selected = input$Deck_from_entry_Player
# #     )
# #   
# # })
# 


