library(ggplot2)
require(grDevices)

shinyServer(function(input, output) {
  
  output$tab = renderUI({
    if(is.null(input$file1))
      h5("Input the movie data and you can get the detailed infornation for these movies.")
    else
      tabsetPanel(tabPanel("Data Table", DT::dataTableOutput("datatable")),
                  tabPanel("Bar Chart",plotOutput("BarPlot")))
  })
  
  
  output$BarPlot <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df = read.csv(inFile$datapath,header = TRUE)
    df2 <- switch(input$dist,
                  allm = df,
                  nomi = subset(df, nominated == 1))
    #print(df2)
    #df2 <- subset(df, nominated == 1)
    
    chooseyear <- input$year
    #print(class(chooseyear))
    if (is.null(chooseyear))
      return(NULL)
    #mydf <- subset(df2, year == 2012)
    #df2 <- input$df2
    mydf <- subset(df2, year == input$year)
    attach(mydf)
    
    plotvar <- input$plotv
    #if (is.null(plotvar))
    #  return(NULL)
    #print(plotvar)
    plotvariable = mydf[,c(plotvar)]
    #print(plotvariable)

    moviename = as.vector(movie_name)
    #print(moviename)
    
    a = max(plotvariable) + 20
    
    #print(a)
    
    par(pin=c(150,100))
    par(mar=c(2,15,0.1,0.1))
    par(las=1)
    barplot(plotvariable,xlim = c(0, a),
            cex.names = 1,
            names.arg = c(moviename),
            horiz = TRUE,
            col = rainbow(10))
  })
  
  
  output$datatable <- DT::renderDataTable({DT::datatable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df = read.csv(inFile$datapath, header = TRUE)
    df2 <- switch(input$dist,
                  allm = df,
                  nomi = subset(df, nominated == 1))
    chooseyear <- input$year
    if (is.null(chooseyear))
      return(NULL)
    
    rawdata <- subset(df2, year == input$year)
    
    #data = rawdata[nrow(rawdata), ]
    
    #myvar <- paste(movie_name,input$show_vars)
    
    #print(myvar)
    #print(class(myvar))
    
    data1 = rawdata[, input$show_vars, drop = FALSE]
    data2 = rawdata[,1]
    data = data.frame(data2,data1)
    names(data)[1] = names(df)[1]
    #print(names(df)[1]) #"movie_name"
    #print(names(data)[1])
    data
    
    }
    
    ,options = list(lengthMenu = c(10, 20, 30, 50), pageLength = 10)
  

  )
  })

  
  output$Year = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(selectInput('year', 'Choose Released Year', ' '))
    
    mydata = read.csv(inFile$datapath)
    selectInput('year', 'Choose Released Year', unique(mydata[2]),
                selected=names(mydata)[[2]])
  })
  
  
  output$PlotVariable = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(selectInput('plotv', 'Choose Plot Variable', ' '))
    
    mydata = read.csv(inFile$datapath)
    selectInput('plotv', 'Choose Plot Variable', names(mydata)[3:8],
                selected=names(mydata)[[8]])
  })
  
  
  output$Checkbox = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(selectInput('plotv', 'Choose Plot Variable', ' '))
    
    mydata = read.csv(inFile$datapath)
    checkboxGroupInput('show_vars', 'Columns in data table to show:',
                       names(mydata)[3:9], selected = names(mydata))
    
  })
  
})

