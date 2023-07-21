#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Linear Modeling with Shiny Interface"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
        ),
        
        # Horizontal line ----
            tags$hr(),
        
        # Action button for displaying Linear Modeling
        actionButton("lmBut", label = "View Linear Model")
        
        # Linear Modeling Calculation
        library(ggplot2)

            ggplot()+
                geom_point(aes(x = dataset$x, y = dataset$y), colour = 'red') +
                geom_line(aes(x = dataset$x, y = predict(model, newdata = dataset)), colour = 'blue')+
                ggtitle('Y over X') +
                xlab('X') +
                ylab('Y')
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
last_import_timestamp <- reactiveVal("")

server <- function(input,output,session){
  current_timestamp <- file.info(rdata_path)$mtime 

  if(last_import_timestamp() != current_timestamp){
    # use parent.frame(2) to make data available in other sessions
    load(rdata_path, envir = parent.fame(2))
    # update last_importet_timestamp
    last_import_timestamp(current_timestamp) 
  }
    
        dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
        

#server <- function(input, output, session) {
 #   load("app.R")

  #  dataInput <- reactive({
   #     req(input$file1)
        
    #    df <- read.csv(input$file1$datapath,
     #                  header = input$header,
      #                 sep = input$sep,
       #                quote = input$quote)
        #return(df)
    #})



function(input,output,session){
  current_timestamp <- file.info(rdata_path)$mtime 

  if(last_importet_timestamp() != current_timestamp){
    # use parent.frame(2) to make data available in other sessions
    load(rdata_path, envir = parent.fame(2))
    # update last_importet_timestamp
    last_importet_timestamp(current_timestamp) 
  }
    
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    
    output$lmtPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
        plot
    })
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
options(shiny.autoreload = TRUE, shiny.port = 8008)
print(getOption("shiny.autoreload")) # prints TRUE
print(getOption("shiny.port"))       # prints 8008
shinyApp(ui = ui, server = server)
