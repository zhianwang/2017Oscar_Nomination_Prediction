shinyUI(pageWithSidebar(
  headerPanel(title="Prediction of 2017 Oscar Nomination"),

  sidebarPanel(
    fileInput('file1', 'File Input',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    tags$hr(),
    uiOutput('Training'),
    uiOutput('Validation'),


    hr(),
    fluidRow()
  ),
    
  mainPanel(
    h4("Summary"),
    uiOutput("tab")

  )
))

