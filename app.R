#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggpubr)
library(scales)
library(glue)
library(plotly)
library(ggplot2)
library(scales)


full_covid <- read_csv("full_grouped.csv")

full_covid <- full_covid %>% 
    mutate_at(c("Country/Region", "WHO Region"), as.factor)

plot_1_data <- full_covid %>% 
    group_by(`Country/Region`, `WHO Region`) %>% 
    summarise(across(starts_with("New"), sum)) %>% #here you can summarise multiple columns/variables as long as they are grouped by the same columns.
    ungroup() %>% 
    mutate(`Percent Recovered` =  round(`New recovered`/ `New cases`, 3))





# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        sidebarMenu(
            menuItem("RMD Because HTML breaks Shiny!", tabName = "overview", icon = icon("dashboard")),
            menuItem("The Interactive Plotting", tabName = "plot", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview",
                    fluidRow(
                        
                        
                    
                        box(width = 12,
                            includeMarkdown("LBB - IP - Covid19 Case.Rmd")
                            
                        )
                        
                        
                    )
                    ,
                    
                    
            ),
            tabItem(tabName = "plot",
                    fluidRow(
                        box(width = 3, #max width is 12
                          selectInput(inputId = "region", 
                                      label = "WHO Region",
                                      choices = unique(plot_1_data$`WHO Region`),
                                      selected = "South-East Asia")
                            
                            
                        ),
                        box(width = 12 ,
                                plotlyOutput("pp11")
                            
                            
                            
                            
                        ),
                        box(width = 12 ,
                            plotlyOutput("pp111")
                            
                            
                            
                            
                        )
                        
                        
                    )
            )
        ) 
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$pp11 <- renderPlotly({
        plot_1_fail <-  plot_1_data %>% 
            filter(`WHO Region` == input$region) %>% 
            ggplot(aes(x = reorder(`Country/Region`, -`Percent Recovered`), y = `Percent Recovered`, fill = `Country/Region`,
                       text = paste("Country:", `Country/Region`, "<br>", "Percent Recovered:", `Percent Recovered`))) +
            geom_col() + 
            labs(x = "Country", title = "Total Number of Recovered") +
            theme_minimal() +
            theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
        ggplotly(plot_1_fail, tooltip = c("text"))   
    })



    output$pp111 <- renderPlotly({
        plot_2 <-  full_covid %>% 
            filter(`WHO Region` == input$region) %>% 
            ggplot(aes(x = Date, y = `Active`, color = `Country/Region`, group = `Country/Region`,
                       text = paste("Country:", `Country/Region`, "<br>", "Active:", `Active`))) +
            geom_line()+
            theme_minimal() +
            labs(title = "Number of Active Cases", x = NULL) +
            scale_y_continuous(name="Confirmed Cases", labels = comma)+
            scale_x_date(date_breaks = "1 month",  date_labels = "%B") 
        ggplotly(plot_2, tooltip = c("text"))   
    })
}





# Run the application 
shinyApp(ui = ui, server = server)


#includeHTML("C:/Users/windows 10/Documents/Algoritma/LBB/IP/LBB---IP---Covid19-Case.html")


#box(width = 3, #max width is 12
#  selectInput(inputId = "major111", 
#              label = "Major Category Hehe",
#              choices = unique(plot_1_data$`WHO Region`),
#              selected = "Europe")


# )

#box(width = 12,
#plotlyOutput("pp11")
