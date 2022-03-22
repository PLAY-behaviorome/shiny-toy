# Load dependencies and helper functions
source("../R/helpers.R")

# Define UI for application that draws a histogram

ui <- navbarPage(
    "PLAY Data",
    tabPanel(
        "select",
        "Select Age Group & QA Status",
        fluidRow(
            column(
                6,
                checkboxGroupInput(
                    "age_groups",
                    "Age Groups",
                    choices = list(
                        "12mo" = 1,
                        "18mo" = 2,
                        "24mo" = 3
                    ),
                    selected = 1:3
                )
            ),
            column(
                6,
                checkboxGroupInput(
                    "gold_silver",
                    "QA Status",
                    choices = list(
                        "PLAY_Gold" = 1,
                        "PLAY_Silver" = 2,
                        "NA" = 3
                    ),
                    selected = 1:2
                )
            )
        )
    ),
    tabPanel(
        "demographics",
        "Participants by Category",
        fluidRow(
            column(4, tableOutput("age_sex")),
            column(4, tableOutput("QA_status")),
            column(4, tableOutput("race_by_ethnicity"))
        ),
    ),
    tabPanel("locomotion", "Locomotion data",
             fluidRow(
                 column(4, plotOutput("walk_onset")),
                 column(4, plotOutput("crawl_onset")),
                 column(4, plotOutput("walk_by_crawl"))
             )),
    tabPanel("feeding", "Feeding data",
             fluidRow(
                 column(4, tableOutput("breastfed_bottle")),
                 column(4, plotOutput("solid_foot_mos")),
                 column(4, plotOutput("solid_by_crawl"))
             ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    selected_data <- reactive({
        play_data %>%
            dplyr::filter(.,
                          age_group %in% age_groups_list[as.numeric(input$age_groups)],
                          group.name %in% selected_qa_list[as.numeric(input$gold_silver)])
    })
    
    # Select locomotor variables
    loco_data <- reactive({
        selected_data() %>%
            dplyr::select(
                .,
                age_group,
                child_sex,
                locomotor_milestones.who_walk.who_walk_onset_mo,
                locomotor_milestones.k_walk.k_walk_onset_mo,
                locomotor_milestones.crawl_onset.crawl_onset_mo
            ) %>%
            dplyr::rename(
                .,
                walk_mos_who = locomotor_milestones.who_walk.who_walk_onset_mo,
                walk_mos_kea = locomotor_milestones.k_walk.k_walk_onset_mo,
                crawl_mos = locomotor_milestones.crawl_onset.crawl_onset_mo
            )
    })
    
    # Select feeding data
    feeding_data <- reactive({
        selected_data() %>%
            dplyr::select(
                .,
                age_group,
                child_sex,
                language_child,
                group.name,
                databrary_url,
                health.feeding_nutrition.breastfeed,
                health.feeding_nutrition.solidfood_age
            ) %>%
            dplyr::rename(., breastfeed = health.feeding_nutrition.breastfeed,
                          solid_food_mos = health.feeding_nutrition.solidfood_age)
    })
    
    # Select loco and feeding data (there's a better way, this is just for fun)
    loco_feeding_data <- reactive({
        selected_data() %>%
            dplyr::select(
                .,
                child_sex,
                locomotor_milestones.crawl_onset.crawl_onset_mo,
                health.feeding_nutrition.solidfood_age
            ) %>%
            dplyr::rename(
                .,
                crawl_mos = locomotor_milestones.crawl_onset.crawl_onset_mo,
                solid_food_mos = health.feeding_nutrition.solidfood_age
            )
    })
    
    # Demographic summary tables
    output$age_sex <-
        renderTable(selected_data() %>% count(age_group, child_sex))
    output$QA_status <-
        renderTable(selected_data() %>% count(age_group, group.name))
    output$race_by_ethnicity <-
        renderTable(selected_data() %>% count(participant.ethnicity, participant.race))
    
    # Walk onset scatterplot
    output$walk_onset <- renderPlot({
        walk_p <- loco_data() %>%
            ggplot(.) +
            aes(walk_mos_who, walk_mos_kea, color = child_sex) +
            geom_point() +
            geom_smooth(method = "lm", aes(group = 1)) +
            xlim(8, 18) +
            ylim(8, 18) +
            theme(legend.position = "bottom") +
            coord_fixed()
        
        ggExtra::ggMarginal(
            walk_p,
            loco_data(),
            walk_mos_who,
            walk_mos_kea,
            type = "density",
            margins = "both",
            groupColour = TRUE,
            groupFill = TRUE
        )
    })
    
    # Crawl onset histogram
    output$crawl_onset <- renderPlot({
        loco_data() %>%
            ggplot(.) +
            aes(crawl_mos, fill = child_sex) +
            geom_histogram(bins = 15) +
            theme(legend.position = "bottom") +
            coord_fixed()
    })
    
    # Crawl onset by walk onset scatterplot
    output$walk_by_crawl <- renderPlot({
        crawl_walk_p <- loco_data() %>%
            ggplot(.) +
            aes(walk_mos_kea, crawl_mos, color = child_sex) +
            geom_smooth(method = "lm", aes(group = 1)) +
            geom_point() +
            theme(legend.position = "bottom") +
            coord_fixed()
        
        ggExtra::ggMarginal(
            crawl_walk_p,
            loco_data(),
            walk_mos_kea,
            crawl_mos,
            type = "density",
            margins = "both",
            groupColour = TRUE,
            groupFill = TRUE
        )
    })
    
    # Breastfed/bottlefed
    output$breastfed_bottle <-
        renderTable(feeding_data() %>% count(child_sex, breastfeed))
    
    # Feeding data histogram
    output$solid_foot_mos <- renderPlot({
        feeding_data() %>%
            ggplot(.) +
            aes(x = solid_food_mos,
                color = child_sex,
                fill = child_sex) +
            geom_histogram(bins = 5)
    })
    
    # Solid food by crawling
    output$solid_by_crawl <- renderPlot({
        crawl_solid_p <- loco_feeding_data() %>%
            ggplot(.) +
            aes(crawl_mos, solid_food_mos, color = child_sex) +
            geom_point() +
            geom_smooth(method = "lm", aes(group = 1)) +
            theme(legend.position = "bottom")
        
        ggExtra::ggMarginal(
            crawl_solid_p,
            loco_feeding_data(),
            crawl_mos,
            solid_food_mos,
            type = "density",
            margins = "both",
            groupColour = TRUE,
            groupFill = TRUE
        )
    })    
}

# Run the application
shinyApp(ui = ui, server = server)
