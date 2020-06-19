#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(datasets)
library(ggthemes)
library(plotly)
library(shinythemes)
ui <- shinyUI(fluidPage(theme =shinytheme('cyborg'),
    titlePanel("Analysis of dataset-My First Shiny App"),
    tabsetPanel(
        tabPanel("Upload File",
                 titlePanel("Uploading Files"),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         
                         # added interface for uploading data from
                         # http://shiny.rstudio.com/gallery/file-upload.html
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"')
                         
                     ),
                     mainPanel(
                         tableOutput('contents'),
                         verbatimTextOutput('summary')
                     )
                 )
        ),
        tabPanel("First Type",
                 pageWithSidebar(
                     headerPanel('My First Plot'),
                     sidebarPanel(
                         
                         # "Empty inputs" - they will be updated after the data is uploaded
                         selectInput('xcol', 'X Variable', ""),
                         selectInput('ycol', 'Y Variable', "", selected = "")
                         
                     ),
                     mainPanel(
                         plotlyOutput('MyPlot')
                     )
                 )
        )
        
    )
)
)

server <- shinyServer(function(input, output, session) {
    # added "session" because updateSelectInput requires it
    
    
    data <- reactive({ 
        req(input$file1) ## ?req #  require that the input is available
        
        inFile <- input$file1 
        
        # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
        # and                              write.csv(iris, "iris.csv")
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        
        # Update inputs (you could create an observer with both updateSel...)
        # You can also constraint your choices. If you wanted select only numeric
        # variables you could set "choices = sapply(df, is.numeric)"
        # It depends on what do you want to do later on.
        
        updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                          choices = names(df), selected = names(df)[2])
        
        return(df)
    })
    
    output$contents <- renderTable({
        head(data())
    })
    output$summary<-renderPrint(
        summary(data())
    )
    
    output$MyPlot <- renderPlotly({
        # for a histogram: remove the second variable (it has to be numeric as well):
        # x    <- data()[, c(input$xcol, input$ycol)]
        # bins <- nrow(data())
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        # Correct way:
        # x    <- data()[, input$xcol]
        # bins <- nrow(data())
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        
        # I Since you have two inputs I decided to make a scatterplot
        ggplot(data(), aes_string(input$xcol,input$ycol)) +geom_point(color="blue")+
            geom_smooth(method='lm',se=FALSE, colour = "green")
        
        
    })
})

shinyApp(ui, server)