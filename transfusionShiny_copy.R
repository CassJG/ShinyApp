library(shiny)
library(here)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Blood Transfusion Data"),

  # Sidebar with a slider input for number of bins 
      selectInput(inputId = "dropdown", label = "Select an option:", 
                  choices = c("Recency", "Frequency", "Monetary", "Time")),
      plotOutput(outputId = "scatterplot"),
      br(), br(),
      plotlyOutput(outputId = "boxplot"),
      plotOutput(outputId = "density")

)

# Define server logic required to draw a histogram
# Define server logic required to generate the plots
server <- function(input, output) {
  
  output$scatterplot <- renderPlot({
    transfusion <- read.csv(here::here("transfusion.data"), header = TRUE, sep = ",")
    colnames(transfusion) <- c("Recency", "Frequency", "Monetary", "Time", "Donated_March_2007")
    transfusion$Donated_March_2007_label <- ifelse(transfusion$Donated_March_2007 == 1, "Yes", "No")
    ggplot(data = transfusion, aes_string(x = "Time", y = input$dropdown, color = "Donated_March_2007_label")) +
      geom_point() +
      scale_color_manual(values = c("red", "lightpink")) +
      labs(x = "Time (Months Since First Donation)", y = input$dropdown, title = "Scatterplot of Whether Someone Donated in March 2007", color = "Donated March 2007") +
      theme_classic() +
      theme(axis.text = element_text(size = 12, family = "Times New Roman"),
            axis.title = element_text(size = 18, family = "Times New Roman"),
            plot.title = element_text(size = 20, family = "Times New Roman"),
            plot.subtitle = element_text(size = 13, face = "italic", family = "Times New Roman"),
            legend.text = element_text(size = 13, family = "Times New Roman"),
            legend.title = element_text(size = 15, family = "Times New Roman"))
  })
  
  output$boxplot <- renderPlotly({
    transfusion <- read.csv(here::here("transfusion.data"), header = TRUE, sep = ",")
    colnames(transfusion) <- c("Recency", "Frequency", "Monetary", "Time", "Donated_March_2007")
    transfusion$Donated_March_2007_label <- ifelse(transfusion$Donated_March_2007 == 1, "Yes", "No")
    
    plot_ly(data = transfusion, y = ~get(input$dropdown), 
            color = ~Donated_March_2007_label, 
            colors = c("pink", "red"),
            type = "box") %>%
      layout(title = list(text = "Boxplot of Whether Someone Donated in March 2007", font = list(size = 18), xanchor = "right", yanchor = "top"),
             yaxis = list(title="Number of People", font = list(size = 20), titlefont = list(family = "Times New Roman")), 
             xaxis = list(title = " "),
             legend = list(font = list(size = 12), 
                      title = list(text = "Donated March 2007"), 
                      titlefont = list(family = "Times New Roman"),
                      font = list(family = "Times New Roman")),
            font = list(family = "Times New Roman"))
  })

    output$density <- renderPlot({
      transfusion <- read.csv(here::here("transfusion.data"), header = TRUE)
      colnames(transfusion) <- c("Recency", "Frequency", "Monetary", "Time", "Donated_March_2007")
      transfusion$Donated_March_2007 <- ifelse(transfusion$Donated_March_2007 == 1, "Yes", "No")
      transfusion$Donated_March_2007 <- factor(transfusion$Donated_March_2007, levels = c("Yes","No"))
      
      
      title_of_plot <- switch(input$dropdown,
                              "Recency" = "Months Since Last Donation Based on March 2007 Donation",
                              "Frequency" = "Frequency of Donations Based on March 2007 Donation",
                              "Monetary" = "Monetary Frequency Based on March 2007 Donation",
                              "Time" = "Months Since First Donation Based on March 2007 Donation")
      
      
      
      
      ggplot(data = transfusion, aes_string(x = input$dropdown, group = "Donated_March_2007", fill= "Donated_March_2007")) +
        geom_density(adjust=1.5, alpha=.5) +
        scale_fill_manual(values = c("red", "pink")) +
        labs(x = input$dropdown, y = "Frequency", title = title_of_plot, fill = "Donated March 2007") +
        theme_classic() +
        theme(axis.text = element_text(size = 14, family = "Times New Roman"),
              axis.title = element_text(size = 18, family = "Times New Roman"),
              plot.title = element_text(size = 20, family = "Times New Roman"),
              plot.subtitle = element_text(size = 13, face = "italic", family = "Times New Roman"),
              legend.text = element_text(size = 13, family = "Times New Roman"),
              legend.title = element_text(size = 15, family = "Times New Roman"))
      
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
