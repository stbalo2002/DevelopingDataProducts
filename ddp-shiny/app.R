library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)


## dashboard `ui.R` can be splitted into it's various components (header, sidebar,
## and body), and the three of them concatnated as `ui.R <- c(header, sidebar, body)`

header <- dashboardHeader(title = "DDP project 1")  ## labels my header


## next i add two menu to my dashboard sidebar  ##

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
)


## I divide the body into two tabs (static and dynamic tabs) and boxes in rows and 
## columns for input/output contents

body <- dashboardBody(
  tags$head(
    ## adding a css file ##
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            box(width = 12, status = "primary",
              h2("Introduction"),
              p("This is the", strong("Shiny application"), "for the", strong("Developing Data Products"), 
              "class. The dashboard is divided into 2 menus (the", strong("Dashboard menu"),
"and the", strong("widgets menu"),"). The", strong("Dashboard menu"), "describes the dashboard
 while the interactive graphs are contained in the", strong("widgets menu"),"."),

              h2("About the Widgets menu"),
              p("The widgets menu is divided into 2 tabs. The first tab contains static graphs
                plotted with", code("ggplot2"), "and the second tab contains a dynamic graph
                plotted with", code("plotly"), "."),
              
              p("The", code("ggplot2"), "plot generates the histogram of 'X' random normal data.
            The number of data to be used for the plot is imputed using the slider, the title of the
                plot can be changed using the", em("write a title"), "input.", span(class="span1", 
              "It is neccessary to press the"), em("update"), span(class="span1","button to generate the plot"),
              ". The", span(id="red", "red line"), "on the plot indicates the", code("mean"), "of the generated 
              data which always approximates the theoretical mean (i.e. zero). This approximation
                improves as the number of randomly generated data increases (law of asymptotics)."),
              hr(),
              
              p("The second tab contains a dynamic scatterplot of the", strong("diamonds dataset"), "that comes 
                with the", code("ggplot2"), "package. The slider can be used to enter the number of observations
                (i.e. rows), the default is set at 3000 observations. For a detailed explanation of the dataset, 
                enter", code("?diamonds"), "in the", strong("R"), "console after loading the", code("ggplot2"), "package."),
              
              p("You can click on each of the scatterplot points and can also select/deselect the facetting property (i.e. the 
                cut of the diamonds)."
            )
)
),
    
    # Second tab content
    tabItem(tabName = "widgets",
            fluidRow(
              tabBox(id = "tabset1", width = 12,
                     tabPanel("static graph",
                              fluidRow(
                                box(title = "inputs", width = 12, status = "primary", collapsible = TRUE,
                                  column(width = 5,
                                       box(height = 80, width = 10, solidHeader = TRUE,
                                           sliderInput(inputId = "num", 
                                                       label = "choose a number", 
                                                       min = 0, 
                                                       max = 1000, 
                                                       value = 200, 
                                                       step = 5))
                                       ),
                              column(width = 5,
                                       box(height = 80, width = 12, solidHeader = TRUE,
                                           textInput(
                                             inputId = "title", label = "write a title", value = "Histogram of Random Normal Data"
                                )
                                )
                                ),
                              column(width = 2, 
                                box(height = 50, width = 10, solidHeader = TRUE, background = "blue",
                                actionButton(
                                  inputId = "click", label = "update"
                                )
                              )
                              )
                              )
                              ),
                                     fluidRow( 
                                              column(width = 9, offset = 2,
                                                     box(solidHeader = TRUE, status = "primary", width = 12, plotOutput("plot1"))
                                       )
                                     )
                              ),
                     tabPanel("dynamic graph",
                                fluidRow(
                                box(height = 120, status = "primary", width = 6, collapsible = TRUE,
                                    sliderInput(inputId = "number", 
                                                label = "choose number of rows", 
                                                min = 0, 
                                                max = 10000, 
                                                value = 3000, 
                                                step = 50))),
                              fluidRow(
                                column(width = 10, offset = 1,
                                box(width = 12, solidHeader = TRUE, status = "primary",
                                  plotlyOutput("plot2"))
                                )
                                )
                              )
                     )
              )
    )
  )
)



ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  ## i create a reactive event where the output is dependent on pressing the 
  ## `update` button
  
  data <- eventReactive(input$click, {
    data.frame(x = rnorm(input$num))
  })
  
  ## next I link the slider to the number of rows used to plot the chart ##
  
  data2 <- reactive({
    diamonds[sample(nrow(diamonds), input$number), ]
    })
  
  ## plotting my static graph ##
  
  output$plot1 <- renderPlot({
    ggplot(data = data(), aes(x = x)) + 
      geom_histogram(fill = "lightblue", colour = "grey20", bins = 75) + 
      geom_vline(xintercept = mean(data()$x), size = 1, colour = "red") + 
      theme_minimal() + labs(title = isolate({input$title}), x = "random normal values")
  })
  
  
  ## plotting my dynamic graph ##
  
  output$plot2 <- renderPlotly({
    
    plot_ly(data2(), x = ~carat, y = ~price, color = ~cut,
            size = ~carat, text = ~paste("Clarity: ", clarity), type = "scatter", mode = "markers") %>% 
      layout(title = "Interactive scatterplot of diamonds")
  })
}


shinyApp(ui = ui, server = server)