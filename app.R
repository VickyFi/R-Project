## app.R ##
library(shinydashboard)
library(caret)
library(dashboardthemes)

ui <- dashboardPage(
  dashboardHeader(title = "R Project"),
  dashboardSidebar(
    fileInput("file1", "Upload File heart.dat", multiple=TRUE)
  ),
  ## Body content
  dashboardBody(
    ### changing theme
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tabsetPanel(
      tabPanel("Data Exploration",
               fluidRow(
                 box(verbatimTextOutput("input_file")),
                 box(plotOutput("boxplot"))
               ),
               fluidRow(
                 box(plotOutput("barplot")),
                 box(plotOutput("myplot"))
               )
      ),
      tabPanel("Project Results",
               fluidRow(
                 h3("Feature Selection"),
                 box(verbatimTextOutput("dataFeatPrint")),
                 box(plotOutput("dataFeatPlot"))
               ),
               fluidRow(
                 h3("Prediction Models"),
                 box(
                   h5("Random Forest"),
                   verbatimTextOutput("randomForestResults")
                   ),
                 box(
                   h5("Logistic Regression"),
                   verbatimTextOutput("LMResults")
                   )
               )
      )      
    )
  )
)

server <- function(input, output) {
  
  output$input_file <- renderPrint({
    file_to_read = input$file1
    if(is.null(file_to_read)){
      return()
    }
    
   df<-read.table(file_to_read$datapath, header = F)
   names(df) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
   str(df)
  })
  
  output$barplot <- renderPlot({
    file_to_read = input$file1
    if(is.null(file_to_read)){
      return()
    }
    
    df<- read.table(file_to_read$datapath, header = F)
    names(df) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
    
    df$num[df$num == 1] <- "Absence"
    df$num[df$num == 2] <- "Presence"
    col <- c("#7FFFD4", "#EE2C2C")
    barplot(table(df$num), main="Heart disease Status", col=col)
  })
  
  output$myplot <- renderPlot({
    file_to_read = input$file1
    if(is.null(file_to_read)){
      return()
    }
    
    df<- read.table(file_to_read$datapath, header = F)
    names(df) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
    
    heart = df #add labels only for plot
    levels(heart$num) = c("No disease","Disease")
    levels(heart$sex) = c("female","male")
    mosaicplot(heart$sex ~ heart$num,
               main="Heart disease by Gender", shade=TRUE,color=TRUE,
               xlab="Gender", ylab="Heart disease")
  })
  
  output$boxplot <- renderPlot({
    file_to_read = input$file1
    if(is.null(file_to_read)){
      return()
    }
    
    df<- read.table(file_to_read$datapath, header = F)
    names(df) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
    colorsb <- c("lightcyan1", "lightcoral")
    
    heart = df 
    boxplot(heart$age ~ heart$num,
            main="Heart disease by Age",
            col=colorsb,
            ylab="Age",xlab="Heart disease")
  })
  
  #Feature Selection
  output$dataFeatPrint <- renderPrint({
    file_to_read = input$file1
    if(is.null(file_to_read)){
      return()
    }
    
    df<-read.table(file_to_read$datapath, header = F)
    names(df) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
    
    convert.types <- function(obj,types){
      for (i in 1:length(obj)){
        FUN <- switch(types[i],character = as.character, 
                      numeric = as.numeric, 
                      factor = as.factor)
        obj[,i] <- FUN(obj[,i])
      }
      obj
    }
    
    classes <-c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor","factor","factor","factor")
    
    df <- convert.types(df,classes)
   
    set.seed(10)
    TrainRows <- createDataPartition(df$num,p=0.70,list=TRUE)
    trainData <- df[TrainRows$Resample1,]
    testData <-  df[-TrainRows$Resample1,]
    
    cv10 = trainControl(method = "cv", number = 10)
    
    #Feature Importance
    # train the model
    hd_feat <- caret::train(num ~ ., data=trainData, method="lvq", preProcess="scale", trControl=cv10)
    # estimate variable importance
    importance <- varImp(hd_feat, scale=FALSE)
    # summarize importance
    print(importance)
    
  })
  
  #Feature Selection
  output$dataFeatPlot <- renderPlot({
    file_to_read = input$file1
    if(is.null(file_to_read)){
      return()
    }
    
    df<-read.table(file_to_read$datapath, header = F)
    names(df) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
    
    convert.types <- function(obj,types){
      for (i in 1:length(obj)){
        FUN <- switch(types[i],character = as.character, 
                      numeric = as.numeric, 
                      factor = as.factor)
        obj[,i] <- FUN(obj[,i])
      }
      obj
    }
    
    classes <-c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor","factor","factor","factor")
    
    df <- convert.types(df,classes)
    
    set.seed(10)
    TrainRows <- createDataPartition(df$num,p=0.70,list=TRUE)
    trainData <- df[TrainRows$Resample1,]
    testData <-  df[-TrainRows$Resample1,]
    
    cv10 = trainControl(method = "cv", number = 10)
    
    #Feature Importance
    # train the model
    hd_feat <- caret::train(num ~ ., data=trainData, method="lvq", preProcess="scale", trControl=cv10)
    # estimate variable importance
    importance <- varImp(hd_feat, scale=FALSE)
    # summarize importance
    # plot importance
    plot(importance)
    
  })
  
  output$randomForestResults <- renderPrint({
    file_to_read = input$file1
    if(is.null(file_to_read)){
      return()
    }
    
    df<-read.table(file_to_read$datapath, header = F)
    names(df) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
    
    convert.types <- function(obj,types){
      for (i in 1:length(obj)){
        FUN <- switch(types[i],character = as.character, 
                      numeric = as.numeric, 
                      factor = as.factor)
        obj[,i] <- FUN(obj[,i])
      }
      obj
    }
    
    classes <-c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor","factor","factor","factor")
    
    df <- convert.types(df,classes)
    
    set.seed(10)
    TrainRows <- createDataPartition(df$num,p=0.70,list=TRUE)
    trainData <- df[TrainRows$Resample1,]
    testData <-  df[-TrainRows$Resample1,]
    
    cv10 = trainControl(method = "cv", number = 10)
    
    hd_rf =  caret::train(
      form = num ~ + thal + ca + cp + oldpeak + thalach + slope + exang, 
      data = trainData,
      method = "rf",
      trControl = cv10
    )
    
    ### predict on test dataset
    hd_rf_pred<-predict(hd_rf,testData)

    ##results
    confusionMatrix(hd_rf_pred,testData$num)

  })
  
  # Logistic Regression
  
  output$LMResults <- renderPrint({
    file_to_read = input$file1
    if(is.null(file_to_read)){
      return()
    }
    
    df<-read.table(file_to_read$datapath, header = F)
    names(df) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
    
    convert.types <- function(obj,types){
      for (i in 1:length(obj)){
        FUN <- switch(types[i],character = as.character, 
                      numeric = as.numeric, 
                      factor = as.factor)
        obj[,i] <- FUN(obj[,i])
      }
      obj
    }
    
    classes <-c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor","factor","factor","factor")
    
    df <- convert.types(df,classes)
    
    set.seed(10)
    TrainRows <- createDataPartition(df$num,p=0.70,list=TRUE)
    trainData <- df[TrainRows$Resample1,]
    testData <-  df[-TrainRows$Resample1,]
    
    cv10 = trainControl(method = "cv", number = 10)
    
    hd_lm =  caret::train(
      form = num ~ + thal + ca + cp + oldpeak + thalach + slope + exang, 
      data = trainData,
      method = "glm",
      trControl = cv10
    )
    
    ### predict on test dataset
    hd_lm_pred<-predict(hd_lm,testData)
    
    ##results
    confusionMatrix(hd_lm_pred,testData$num)
    
  })
  
  

}

shinyApp(ui, server)