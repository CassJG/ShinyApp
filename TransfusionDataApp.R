#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# This section is Cassie's original code

library(shiny)
library(here)
library(ggplot2)
library(tidyr)
library(stringr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Blood Transfusion Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "dropdown", label = "Select an option:", 
                      choices = c("Recency", "Frequency"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(outputId = "scatterplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterplot <- renderPlot({
      transfusion <- read.csv(here::here("transfusion.data"), header = TRUE)
      colnames(transfusion) <- c("Recency", "Frequency", "Monetary", "Time", "Donated_March_2007")
      transfusion$Donated_March_2007_label <- ifelse(transfusion$Donated_March_2007 == 1, "Yes", "No")
      ggplot(data = transfusion, aes_string(x = "Time", y = input$dropdown, color = "Donated_March_2007_label")) +
        geom_point() +
        labs(x = "Time (Months Since First Donation)", y = input$dropdown, title = "Plot", legend = "Whether They Donated in March 2007") +
        theme_classic() +
        theme(axis.text = element_text(size = 14, family = "Times New Roman"),
              axis.title = element_text(size = 18, family = "Times New Roman"),
              plot.title = element_text(size = 18, family = "Times New Roman"),
              plot.subtitle = element_text(size = 13, face = "italic", family = "Times New Roman"),
              legend.text = element_text(size = 13, family = "Times New Roman"),
              legend.title = element_text(size = 15, family = "Times New Roman"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
