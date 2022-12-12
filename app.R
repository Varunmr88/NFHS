#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shiny)
library(tidyverse)
library(ggpubr)
library(readxl)
library(wesanderson)
library(hablar)

nfhs<- read_csv("data/NFHS_5_India_Districts_Factsheet_Data.csv")

# Define UI for application that draws a histogram


# Define UI for miles per gallon app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("India:NFHS-5 District Dashboard "),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            h2("Select State and Indicator"),
            
            # Input: Selector for variable to plot against mpg ----
            selectInput("State", "State:",choices = unique(nfhs$`State/UT`)),
            selectInput("Indicator","Indicator:", choices=names(nfhs[-c(1:2)])),
            p("The dataset for this project is accessed from  Kaggle.",
              a("Download Link & Source Page", 
                href = "https://www.kaggle.com/datasets/kmldas/india-national-family-health-survey-nfhs"))
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Formatted text for caption ---
            h1("Documentation"),
            p("The National Family Health Survey (NFHS) is a large-scale, 
            multi-round survey conducted in a representative sample of households throughout India. 
            Three rounds of the survey have been conducted since the first survey in 1992-93.
            This dashboard uses district factsheet data of the latest NFHS-5 data held in 2019-2020. 
            The dashboard gives state wise filtering and display bar chart of health indicators
            district ise. This will help in easy searching of the Data to 
            find district level indicators."),
            # Output: Plot of the requested variable against mpg ----
            plotOutput("Plot")
            
        )
    )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
    # Compute the formula text ----
    # This is in a reactive expression since it is shared by the
    # output$caption and output$mpgPlot functions
    formulaText <- reactive({
        paste(input$Indicator)
    })
    
    # Return the formula text for printing as a caption ----
    output$caption <- renderText({
        formulaText()
    })
    
    # Generate a plot of the requested variable against mpg ----
    # and only exclude outliers if requested
    output$Plot<-renderPlot({
       nfhs1<-nfhs%>%
           filter(`State/UT`==input$State)%>%select(!! rlang::sym(input$Indicator),1:2)%>%
           arrange((!! rlang::sym(input$Indicator)))
        ggplot(data=nfhs1,aes(x=reorder(`District Names`,(!! rlang::sym(input$Indicator))),y=paste(!! rlang::sym(input$Indicator)),fill=`District Names`))+
            geom_bar(stat = "identity",show.legend = FALSE)+
            geom_text(nfhs1,mapping=aes(label=paste(!! rlang::sym(input$Indicator))),position = position_stack(vjust = 0.5))+
            coord_flip()+
            labs(x="District Names",y=input$Indicator)+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                             panel.background = element_blank(),axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),axis.text = element_text(face="bold"))
            
          
    },height = 700, width = 500)
}

# Create Shiny app ----
shinyApp(ui, server)