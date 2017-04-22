shinyUI(pageWithSidebar(
  headerPanel(title="Movie details"),

  sidebarPanel(
    fileInput('file1', "Please choose a csv file.",
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    tags$hr(),
    uiOutput('Year'),
    uiOutput('PlotVariable'),
    #br(),
    radioButtons("dist", "Data Scope:",
                 c("All Movies" = "allm",
                   "Nominated Movies" = "nomi")),
    #br(),
    uiOutput('Checkbox'),
    fluidRow()
  ),
    
  mainPanel(
    
    uiOutput("tab")
    
    )

))

