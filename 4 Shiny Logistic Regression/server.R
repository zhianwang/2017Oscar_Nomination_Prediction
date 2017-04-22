library(ggplot2)

shinyServer(function(input, output) {

    
  output$Training = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(sliderInput('trainingyear', 'Training Data', 2000,2016,value=c(2000,2010),step = 1))
    
    else
      mydata = read.csv(inFile$datapath)
      return(sliderInput('trainingyear', 'Training Data',min = min(mydata['year']),max = max(mydata['year']),value=c(2012,2014), step=1))
    
  })

  output$Validation = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(sliderInput('validationyear', 'Validation Data', 2000,2016,value=c(2010,2012),step = 1))
    mydata = read.csv(inFile$datapath)
    sliderInput('validationyear', 'Validation Data',min = min(mydata['year']),max = max(mydata['year']),value=c(2014,2014), step=1)
  })
  
  output$tab = renderUI({
    if(is.null(input$file1))
    {
      h5("Input the history movie data and you can get the nomination prediction of 2017")

    }
    else
      tabsetPanel(tabPanel("DataSummary", verbatimTextOutput("datasummary")),tabPanel("Stepwise Model Summary", verbatimTextOutput("stepsummary")),tabPanel("Logistic Model Summary", verbatimTextOutput("logistic")),
                  tabPanel("Confusion Matrix",tableOutput("confusion2")),tabPanel("Misclassfication Rate",tableOutput("misclass")),tabPanel("ROC Curve",plotOutput("ROC")),tabPanel("Lift Curve",plotOutput("lift")),tabPanel("Nomination Movies for 2017 Oscar",tableOutput("Nomination")))
  })
  
  
  output$datasummary <- renderPrint({
      h4("Datasummary")
      inFile <- input$file1
      if (is.null(inFile))
        return("You need to input file")
      data = read.csv(inFile$datapath)
      mydata = subset(data,year>=input$trainingyear[1] & year<=input$trainingyear[2])
      summary(mydata)
})


  output$stepsummary <- renderPrint({

      inFile <-input$file1
      if (is.null(inFile))
        return("You need to input file")
      data = read.csv(inFile$datapath)
      mydata = subset(data,year>=input$trainingyear[1] & year<=input$trainingyear[2])
      library(MASS)
      fit<-glm(nominated~duration+gross+budget+facebook_likes+average_rating+sentiment_score,data=mydata)
      summary(fit)
      step <- stepAIC(fit,direction="both")
      step$anova


  })
  
  output$logistic <- renderPrint({

    inFile <-input$file1
    if (is.null(inFile))
      return("You need to input file")
    data = read.csv(inFile$datapath)
    training = subset(data,year>=input$trainingyear[1] & year<=input$trainingyear[2])
    validation = subset(data,year>=input$validationyear[1] & year<=input$validationyear[2])
    model<- glm(nominated~duration+gross+budget+facebook_likes+average_rating,family=binomial(link='logit'),data=training)
    summary(model)
    })
  
  output$misclass <- renderTable({
    inFile <-input$file1
    if (is.null(inFile))
      return("You need to input file")
    data = read.csv(inFile$datapath)
    training = subset(data,year>=input$trainingyear[1] & year<=input$trainingyear[2])
    validation = subset(data,year>=input$validationyear[1] & year<=input$validationyear[2])
    model<- glm(nominated~duration+gross+budget+facebook_likes+average_rating,family=binomial(link='logit'),data=training)
    library(ROCR)
    new_nominations<-predict(model,newdata = validation,type = "response")
    ord = order(new_nominations,decreasing = TRUE)
    fixed_nominations<-ifelse(new_nominations>1,1,0)
    for (i in ord[1:8]){
      fixed_nominations[i]=1
    }
    validation$Predictions<-fixed_nominations
    confision_matrix<-table(validation$Predictions,validation$nominated)
    misclass_rate<-mean(validation$Predictions!=validation$nominated)
    
  })
  output$confusion2 <- renderTable({
    inFile <-input$file1
    if (is.null(inFile))
      return("You need to input file")
    data = read.csv(inFile$datapath)
    training = subset(data,year>=input$trainingyear[1] & year<=input$trainingyear[2])
    validation = subset(data,year>=input$validationyear[1] & year<=input$validationyear[2])
    model<- glm(nominated~duration+gross+budget+facebook_likes+average_rating,family=binomial(link='logit'),data=training)
    library(ROCR)
    new_nominations<-predict(model,newdata = validation,type = "response")
    ord = order(new_nominations,decreasing = TRUE)
    fixed_nominations<-ifelse(new_nominations>1,1,0)
    for (i in ord[1:8]){
      fixed_nominations[i]=1
    }
    validation$Predictions<-fixed_nominations
    confision_matrix<-table(validation$Predictions,validation$nominated)
  
  })
  output$ROC <- renderPlot({

    inFile <-input$file1
    if (is.null(inFile))
      return("You need to input file")
    data = read.csv(inFile$datapath)
    training = subset(data,year>=input$trainingyear[1] & year<=input$trainingyear[2])
    validation = subset(data,year>=input$validationyear[1] & year<=input$validationyear[2])
    model<- glm(nominated~duration+gross+budget+facebook_likes+average_rating,family=binomial(link='logit'),data=training)
    library(ROCR)
    new_nominations<-predict(model,newdata = validation,type = "response")
    ord = order(new_nominations,decreasing = TRUE)
    fixed_nominations<-ifelse(new_nominations>1,1,0)
    for (i in ord[1:8]){
      fixed_nominations[i]=1
    }
    validation$Predictions<-fixed_nominations
    confision_matrix<-table(validation$Predictions,validation$nominated)
    pred<-prediction(fixed_nominations,validation$nominated)
    roc<-performance(pred,measure="tpr",x.measure="fpr")
    plot(roc,main="ROC Curve")

  })
  output$lift <- renderPlot({
    
    inFile <-input$file1
    if (is.null(inFile))
      return("You need to input file")
    data = read.csv(inFile$datapath)
    training = subset(data,year>=input$trainingyear[1] & year<=input$trainingyear[2])
    validation = subset(data,year>=input$validationyear[1] & year<=input$validationyear[2])
    model<- glm(nominated~duration+gross+budget+facebook_likes+average_rating,family=binomial(link='logit'),data=training)
    library(ROCR)
    new_nominations<-predict(model,newdata = validation,type = "response")
    ord = order(new_nominations,decreasing = TRUE)
    fixed_nominations<-ifelse(new_nominations>1,1,0)
    for (i in ord[1:8]){
      fixed_nominations[i]=1
    }
    validation$Predictions<-fixed_nominations
    confision_matrix<-table(validation$Predictions,validation$nominated)
    pred<-prediction(fixed_nominations,validation$nominated)
    lift_curve<-performance(pred,"lift","rpp")
    plot(lift_curve,main="Lift Curve",colorize=TRUE)
    
  })
  output$Nomination <- renderTable({

    inFile <-input$file1
    if (is.null(inFile))
      return("You need to input file")
    data = read.csv(inFile$datapath)
    training = subset(data,year>=input$trainingyear[1] & year<=input$trainingyear[2])
    validation = subset(data,year>=input$validationyear[1] & year<=input$validationyear[2])
    model<- glm(nominated~duration+gross+budget+facebook_likes+average_rating,family=binomial(link='logit'),data=training)
    movie_test <- read.csv("movie_test.csv")
    movie_test$budget[is.na(movie_test$budget)]<- movie_test$gross[is.na(movie_test$budget)]*0.6
    new_nominations_test<-predict(model,newdata = movie_test,type = "response")
    
    #ordering the nomination predictions in descending order
    ordered_nomination_test<-order(-new_nominations_test)
    movie_test$nomination_prob<- new_nominations_test
    
    #Assigning 0(Not Nominated) to all the movies at first 
    fixed_nominations_test<-ifelse(new_nominations_test>=0,0,1)
    
    #Now assigning 1(Nominated) to the Top 8 movies with highest probability of being nominated
    for (i in ordered_nomination_test[1:8]){
      fixed_nominations_test[i]=1
    }
    table(fixed_nominations_test)
    
    #Storing the predicted nominations in our actual test dataset
    movie_test$Predictions<-fixed_nominations_test
    str(movie_test)
    
    #now listing the movies which our model thinks are going to be nominated
    nomination_predicted<-movie_test[movie_test$Predictions==1,]
    
    #Printing the list of movies which are predicted to be nominated for the best picture award
    head(nomination_predicted$movie_name[order(-nomination_predicted$nomination_prob)],8)
    

  })
  
  
    
    
  })




