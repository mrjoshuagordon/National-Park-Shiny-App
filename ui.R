

shinyUI(bootstrapPage(
  
  
  
  sidebarPanel(
    
    #http://withr.me/blog/2014/01/03/authentication-of-shiny-server-application-using-a-simple-method/
    
    
    sliderInput(inputId = "population",
                label = "Population",
                min=100000, max=1000000, value = 300000),
    
    
    
    sliderInput(inputId = "distance",
                label = "Distance",
                min=50, max=500, value = 150),
    
    sliderInput(inputId = "integer",
                label = "Rank",
                min=1, max=9, value = 1),
    
    h1('List of Cities, Ranked'),
    div(align="right", tabPanel('iris',
                                dataTableOutput("mytable2")))
    #     
    #     selectInput(inputId = "n_breaks",
    #                 label = "State name:",
    #                 choices = state.names,
    #                 selected = "new jersey")
    #     
  ),
  
  mainPanel(div(align = "center",
                plotOutput(outputId = "main_plot")),
            tabPanel('iris',
                     dataTableOutput("mytable3"))
            
            
  )
  
  
  
  
  
  
  
))