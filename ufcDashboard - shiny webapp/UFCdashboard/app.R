## This shiny webapp is for the UFC dash board


library(shiny)
library(tidyverse)
library(elo)
library(shinydashboard)

## **** DF INFO *****
# df is raw version
df <-  read_csv("https://raw.githubusercontent.com/andrew-couch/Tidy-Tuesday/master/TidyTuesdayUFCDashboard/elo.csv")
# elo_df is preprocessed version of the data
elo_df <-  read_csv("https://raw.githubusercontent.com/andrew-couch/Tidy-Tuesday/master/TidyTuesdayUFCDashboard/elo_df.csv")
## *********************

create_elo_data <- function(k) {

temp_df <- elo.run(winner ~ fighter + opponent, k = k, 
                   data = elo_df %>% arrange(fighter, date) ) %>% 
            as_tibble() %>% 
            cbind(elo_df %>% arrange(fighter, date) %>% 
                      select( match_id)) %>% 
    select(team.A, elo.A, team.B, elo.B, match_id)

rbind( temp_df %>% 
        select_at(vars(contains(".A"), contains("id"))) %>% 
        rename_all(.fun = function(x) str_replace(x, ".A", "")), 
    temp_df %>%  
        select_at(vars(contains(".B"), contains("id"))) %>% 
        rename_all(.fun = function(x) str_replace(x, ".B", "")))  %>% 
    rename("fighter" ="team") %>%  # rename left df column "team" to "fighter" so that merge works on given variable name
    left_join(df %>% select(fighter, date, weight_class, match_id), 
              by = c("fighter", "match_id")) 
}


ui <- dashboardPage(
    dashboardHeader(title = "UFC Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Weight Class", 
                     tabName = "weight_class_tab", # tab identifier
                     icon = icon("dashboard")),
            menuItem("Head to Head",
                     tabName = "head_tab",
                     icon = icon("dashboard"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "weight_class_tab",  # connection to the prev weight tab
                    box(plotOutput("elo_timeseries")),
                    box(plotOutput("elo_dist")), 
                    box(tableOutput(outputId = "top_5_table")),
                    box(uiOutput("weight_class_selector_1")),
                    box(sliderInput(inputId = "slider_k_1",
                                    label = "K for ELO calculation",
                                    min = 1, 
                                    max = 100, 
                                    value = 20)) # this id name of the selector object
                    ), 
            tabItem(tabName = "head_tab",         # connection to the prev head to head tab
                    fluidRow(box(uiOutput("Fighter_selector")), box(uiOutput("Opponent_selector"))), 
                    fluidRow(box(valueBoxOutput("Fighter_card")), box(valueBoxOutput("Opponent_card"))), 
                    box(uiOutput("weight_class_selector_2")),
                    box(sliderInput("slider_k_2", 
                                    label  = "K for ELO", 
                                    min = 1,
                                    max = 100, 
                                    value = 20)))
        )
        
    )
)

server <- function(input, output) { 
    
    # some function content
    
    output$weight_class_selector_1 <- renderUI({
        
        weight_class_1_df <- create_elo_data(input$slider_k_1)
        
        selectInput(input = "v_weight_class_1", 
                    label = "Weight Class", 
                    choice =  weight_class_1_df %>% 
                        select(weight_class) %>% 
                        distinct() %>% 
                        arrange(weight_class) )  # v_weight_class_1 is the actually input name
    })
    
    output$weight_class_selector_2 <- renderUI({
        
        weight_class_2_df <- create_elo_data(input$slider_k_2)
        
        selectInput(input = "v_weight_class_2", 
                    label = "Weight Class", 
                    choice =  weight_class_2_df %>% 
                        select(weight_class) %>% 
                        distinct() %>% 
                        arrange(weight_class) )  # v_weight_class_2 is the actually input name
    })
    
    output$Fighter_selector <- renderUI({
        
        fighter_selector_df <-  create_elo_data(input$slider_k_2) %>% 
            filter(weight_class == input$v_weight_class_2) %>% 
            select(fighter) %>% 
            distinct() %>% 
            arrange(fighter)
        
        selectInput(inputId = "v_fighter", 
                    label = "Fighter", 
                    choices = fighter_selector_df)
    })
    
    output$Opponent_selector <- renderUI({
        
        opponent_selector_df <-  create_elo_data(input$slider_k_2) %>% 
            filter(weight_class == input$v_weight_class_2) %>% 
            filter( fighter != input$v_fighter) %>% 
            select(fighter) %>% 
            distinct() %>% 
            arrange(fighter)
        selectInput(inputId = "v_opponent", 
                    label = "Opponent", 
                    choices = opponent_selector_df)
    })
    
    output$Fighter_card <- renderValueBox({
        
        elo_fighter <- elo.run(winner ~ fighter + opponent , 
                k = input$slider_k_2, 
                data = elo_df)  
        
        fighter_prob = predict(elo_fighter, 
                               data.frame(fighter = input$v_fighter, opponent = input$v_opponent))
        
        valueBox(value = paste(round(fighter_prob * 100, 0), "%", ""), 
                 subtitle = paste(input$v_fighter, "Probability", sep = " "), 
                 color = "blue", 
                 icon = icon("hand-rock"))
    })
    
    output$Opponent_card <- renderValueBox({
        elo_opponent <- elo.run(winner ~ fighter + opponent , 
                               k = input$slider_k_2, 
                               data = elo_df)  
        opponent_prob = predict(elo_opponent, 
                               data.frame(fighter = input$v_opponent, opponent = input$v_fighter))
        
        valueBox(value = paste(round(opponent_prob * 100, 0), "%", ""), 
                 subtitle = paste(input$v_opponent, "Probability", sep = " "), 
                 color = "red", 
                 icon = icon("hand-rock"))
    })
    
    output$top_5_table <- renderTable({
        table_df <- create_elo_data(input$slider_k_1) %>% 
            filter(weight_class == input$v_weight_class_1)

        table_df %>% 
            group_by(fighter) %>% 
            arrange(desc(elo)) %>%
            slice(1) %>%
            ungroup() %>%
            top_n(elo, n = 5) %>%
            arrange(desc(elo)) %>%
            select(fighter, elo) %>%
            mutate(rank = row_number())
    })

    output$elo_timeseries <- renderPlot({
        
        elo_timeseries_df <- create_elo_data(input$slider_k_1) %>% 
            filter(weight_class == input$v_weight_class_1)
        
        top_5_fighters <-  elo_timeseries_df %>% 
            group_by(fighter) %>%
            arrange(desc(elo)) %>%
            slice(1) %>%
            ungroup() %>%
            top_n(elo, n = 5) %>% 
            select(fighter)
        
        elo_timeseries_df %>% 
            ggplot(mapping = aes(x = date, y = elo)) + 
            geom_point() +    
            geom_point(data = elo_timeseries_df %>% 
                           filter(fighter %in% top_5_fighters$fighter), 
                       aes(x = date, y = elo, color = fighter)) + 
            theme(legend.position = "top")
        
    })
    
    output$elo_dist <- renderPlot({
        elo_dist <- create_elo_data(input$slider_k_1) %>% 
            filter(weight_class == input$v_weight_class_1)
        
        elo_dist %>% ggplot(aes(x = elo)) + geom_histogram()
    })
    
}


shinyApp(ui = ui, server = server)


