
library(httr) #this package will help use use the URL we built to get information from the OMDb API
library(jsonlite) #this package will help us convert the data we get from the OMDb API to a more usable format
library(tidyverse) #this package will help us work with our nicely formatted data.
library(lubridate) #this package will help us create dates 
library(ggplot2) #this package will help us make graphs
library(readr)
library(class)
library(caret)
library(DT)
library(tree)
library(readxl)
library(readr)
library(rpart.plot)
library(randomForest)

data <- read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//movie_data_7_30_2022.csv")
nemo_data <- read_xlsx("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//10_33_7_23_2022//nemo_test.xlsx")
nemo_data$Rated <- as.factor(nemo_data$Rated)
nemo_data$first_Genre <- as.factor(nemo_data$first_Genre)
nemo_data$first_Country <- as.factor(nemo_data$first_Country)
nemo_data$first_Country <- as.factor(nemo_data$first_Country)
nemo_data$Summary_Awards <- as.factor(nemo_data$Summary_Awards)
nemo_data_with_title <- nemo_data
nemo_data <- nemo_data %>% select(-c(Title))
#data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!

#this will help us convert the Ratings.Value column to numeric
parse_number <- function(S){
  if(grepl("/", S)){
    A<-str_split(S, "/")
    A<-as.numeric(unlist(A))
    A<-A[[1]]/A[[2]]
    A<-A*100
  } else {
    A<-as.numeric(gsub("%","",S))
  }
  return(A)
}

#this will help us make the Summary_Awards column:
award <- function(S){
  if(is.na(S)){
    A<-"none"
    return(A)
  }
  S=tolower(S)
  #won and nominated:
  if((grepl("won", S) | grepl("win", S)) && (grepl("nomina", S))){
    A<-"won and nominated"
  }
  #only won:
  else if (grepl("won", S) | grepl("win", S)){
    A<-"won"
  }
  #only nominated:
  else if (grepl("nomina", S)){
    A<-"nomination"
  }
  #no awards or nominations:
  else {(A<-"none")
    return(A)
  }
}

mat1=NULL
format_data <- function(data){
  data$nchar_Title <- nchar(data$Title)
  data$Year <- as.numeric(data$Year)
  data$Rated <- as.factor(data$Rated)
  data$Released <- NULL
  data$Runtime <- as.numeric(gsub(" min","",data$Runtime))
  
  df <- data.frame(x = data$Genre)
  A<-df %>% separate(x, c('Genre_1'))
  data$first_Genre <- as.factor(A$Genre_1)
  data$Genre = NULL
  
  df <- data.frame(x = data$Country)
  A<-df %>% separate(x, c('Country_1'),sep=",")
  data$first_Country <- A$Country_1
  data$first_Country <- gsub("USA","United States",data$first_Country)
  data$first_Country <- as.factor(data$first_Country)
  data$Country = NULL
  
  data$Ratings.Value <- sapply(data$Ratings.Value, FUN=parse_number)
  data$Summary_Awards <- as.factor(sapply(data$Awards, FUN=award))
  data$Metascore <- as.numeric(data$Metascore)
  data$imdbRating <- as.numeric(data$imdbRating)*10
  data$imdbVotes <- as.numeric(gsub(",","",data$imdbVotes))
  data$DVD <- NULL
  data$BoxOffice <- gsub("\\$","",data$BoxOffice)
  data$BoxOffice <- as.numeric(gsub(",","",data$BoxOffice))
  
  data$Plot <- NULL
  data$Language <- NULL
  data$Awards<-NULL
  data$Poster<-NULL
  data$imdbID<-NULL
  data$Type<-NULL
  data$Production<-NULL
  data$Website<-NULL
  data$Response<-NULL
  
  movie_list<-unique(data$Title)
  
  for (i in movie_list){
    temp=data[is.element(data$Title,i),]
    Ratings.Value_mean<-mean(temp$Ratings.Value)
    Metascore<-unique(temp$Metascore)
    imdbRating<-unique(temp$imdbRating)
    
    #note: the average_rating column is the average of ratings from  
    #Internet Movie Database (when applicable)
    #Metacritic (when applicable)
    #Rotten Tomatoes (when applicable)
    #Metascore (when applicable)
    #imdbRating
    
    if(is.na(Metascore)==TRUE){
      temp$average_rating=(Ratings.Value_mean+imdbRating)/2
    }
    if(is.na(Metascore)==FALSE){
      temp$average_rating=(Ratings.Value_mean+Metascore+imdbRating)/3
    }
    mat1=rbind(mat1,temp)
  }
  mat1$Ratings.Value <- NULL
  mat1$Ratings.Source <- NULL
  mat1 <- unique(mat1)
  return(mat1)
}

data <- format_data(data)
data$Rated[data$Rated == "N/A"] <- NA
data <- data %>% select(-c(Metascore,BoxOffice))
data$Rated <- droplevels(data$Rated)
data$first_Genre <- droplevels(data$first_Genre)
data$first_Country <- droplevels(data$first_Country)
data$Summary_Awards <- droplevels(data$Summary_Awards)
data_with_titles <- data[complete.cases(data), ]

data <- data %>% select(-c(Title,Director,Writer,Actors ))

data <- data[complete.cases(data), ]
data$Rated <- droplevels(data$Rated)
data$first_Genre <- droplevels(data$first_Genre)
data$first_Country <- droplevels(data$first_Country)
data$Summary_Awards <- droplevels(data$Summary_Awards)
data_with_titles <- data[complete.cases(data), ]
set.seed(dim(data)[1])
# train <- sample(1:nrow(A), size = nrow(A)*0.8)
# test <- dplyr::setdiff(1:nrow(A), train)
# ATrain <- A[train, ]
# ATest <- A[test, ]


# trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# dtree_fit <- train(average_rating ~ ., data = ATrain, method = "rpart",
#                    trControl = trctrl,
#                    tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
# pred <- predict(dtree_fit, newdata = dplyr::select(ATest, -average_rating))
#sqrt(mean((pred-ATest$average_rating)^2))


library(shiny)
library(caret)
library(tidyverse)
library(DT)
#data("GermanCredit")
library(shiny)
library(ggplot2)

#trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
shinyServer(function(input,output,session)({
  
  ## Get the value of the dataset that is selected by user from the list of datasets
  ## to output the dataset
  
  output$dat <- DT::renderDataTable({
    data_with_titles<-datatable(data_with_titles)
    data_with_titles
  })
  
  output$nemo_dat <- DT::renderDataTable({
    nemo_data_with_title<-datatable(nemo_data_with_title)
    nemo_data_with_title
  })
  
  # output$dat <- renderPrint({
  #   data_with_titles
  # })
  
  output$struct <- renderPrint({
    str(data_with_titles)
  })
  
  output$cont_table <- renderPrint({
    
    tv <- data %>% select(input$table_variable,input$table_variable2)
    table(tv)
  })
  
  output$summary_table <- renderPrint({
    if(input$covariance=="None"){
      sv <- data %>% select(input$summary_variable)
      summary(sv)
    } else if(input$covariance!="None"){
      A<-data %>% select(input$summary_variable)
      B<-data %>% select(input$covariance)
      cov(A,B)
      
    }
    
  })
  
  #if (x%%2 == 0 || x%%5 == 0) {
  
  # if(input$table_variable=="Rated"||input$table_variable=="first_Genre"||input$table_variable=="first_Country"||input$table_variable=="Summary_Awards"){
  #   tv <- ATrain %>% select(input$table_variable)
  #   table(tv)
  # } else if(input$table_variable2=="Rated"||input$table_variable2=="first_Genre"||input$table_variable2=="first_Country"||input$table_variable2=="Summary_Awards"){
  #   tv <- ATrain %>% select(input$table_variable,input$table)
  #   table(tv)
  #   
  # }
  
  
  
  
  # output$radio_button <- renderPlot({
  #   
  #   if(input$radio_button=="Just Classification") {
  #     g <- ggplot(GermanCredit, aes(x = Class))
  #     g + geom_histogram(stat="count")
  #   } else if(input$radio_button=="Classification and Unemployed"){
  #     g <- ggplot(GermanCredit, aes(x = Class))
  #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
  #       scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
  #   } else if(input$radio_button=="Classification and Foreign"){
  #     g <- ggplot(GermanCredit, aes(x = Class))
  #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
  #       scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
  #   }
  # })
  # 
  
  
  output$bar_plot <- renderPlot({
    ggplot(data,aes_string(x=input$bar_plot_variable)) +
      geom_bar(fill="lightblue") +
      ggtitle(paste0("Bar Plot: ",input$bar_plot_variable ))
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(data,aes_string(x=input$scatter_plot_x_variable,y=input$scatter_plot_y_variable,color=input$color_variable)) +
      geom_point() +
      ggtitle(paste0("Scatter Plot: ",input$scatter_plot_x_variable, " vs.", input$scatter_plot_y_variable)) +
      scale_color_discrete(name = input$color_variable)
  })
  # output$cont_table <- renderPrint({
  #   if(input$table=="One Way") {
  #     tv1 <- ATrain %>% select(Rated)
  #     table(tv1)
  #     
  #     tv2 <- ATrain %>% select(first_Genre)
  #     table(tv2)
  #     
  #     tv3 <- ATrain %>% select(first_Country)
  #     table(tv3)
  #     
  #     tv4 <- ATrain %>% select(Summary_Awards)
  #     table(tv4)
  #   } #else if(input$radio_button=="Classification and Unemployed"){
  #   g <- ggplot(GermanCredit, aes(x = Class))
  #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
  #     scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
  # } else if(input$radio_button=="Classification and Foreign"){
  #   g <- ggplot(GermanCredit, aes(x = Class))
  #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
  #     scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
  # }
  #})
  
  
  # output$summary <- renderPlot({
  #   ggplot(ATrain, aes(x = average_rating)) +
  #   geom_histogram(stat="count")
  #   #summary(get(input$dataset))
  # })
  
  output$summary <- renderPlot({
    ggplot(data,aes_string(x=input$hist_graph_variable)) +
      geom_histogram(color = "blue", fill = "red", size = 2) +
      ggtitle(paste0("Histogram: Distribution of ",input$hist_graph_variable ))
  })
  
  # output$radio_button <- renderPlot({
  #        if(input$select_box_1=="Bar Plot") {
  #          g <- ggplot(data(), aes_string(x = input$sel_bar_plot))
  #          g + geom_histogram(stat="count")
  #        }
  
  
  # Pulling the list of variable for choice of variable x
  output$varx <- renderUI({
    selectInput("variablex", "select the X variable", choices=names(data()))
  })
  
  # Pulling the list of variable for choice of variable y
  output$vary <- renderUI({
    selectInput("variabley", "select the Y variable", choices=names(data()))
  })
  
  #Creates a new reactive values
  # observe({
  #   vals <- reactiveValues(intrain = createDataPartition(data$average_rating, p= input$split, list = FALSE),
  #                          training = data[intrain,],
  #                          testing = data[-intrain,])
  # })
  
  # observe({
  #   updated_predictors <- 
  #   updateCheckboxGroupInput(session,"linear_predictors", choices = -input$linear_response)
  #   
  # })
  
  vals <- reactiveValues(intrain = NULL,
                         training = NULL,
                         testing = NULL)
  observeEvent(input$split,{
    vals$intrain = createDataPartition(data[[input$random_forest_response]], p=input$split, list = FALSE)
    vals$training = data[vals$intrain,]
    vals$testing = data[-vals$intrain,]
  })
  
  output$table <- DT::renderDataTable({
    view_training_data<-datatable(vals$training)
    view_training_data
  })
  # output$table2 <- DT::renderDataTable({
  #   data2<-datatable(vals$testing)
  #   data2
  # })
  
  ######################################################
  # f <- reactive({
  #   as.formula(paste(input$random_forest_response, "~."))
  # })
  # e <- reactive({
  #   as.formula(paste("-", input$random_forest_response))
  # })
  # Linear_Model <- reactive({
  #   lm(f(), data = trainingData())
  # })
  
  # output$tree_RMSE <- renderText({
  #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
  #                     trControl = trctrl,
  #                     tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
  #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -e()))
  #   paste0("The RMSE is ", sqrt(mean((pred-vals$testing$e())^2)))
  # })
  # output$tree_plot <- renderPlot({
  #   rpart.plot(dtree_fit$finalModel)
  # })
  # output$tree_plot <- renderPlot({
  #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
  #                      trControl = trctrl,
  #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
  #   rpart.plot(dtree_fit$finalModel)
  # })
  #
  # output$tree_RMSE <- renderText({
  #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing))
  #   paste0("The RMSE is ", sqrt(mean((pred-vals$testing$Year)^2)))
  # })
  #
  
  obs <- reactive({
    vals$testing %>% select(input$random_forest_response)
  })
  
  f <- reactive({
    if(input$random_forest_predictor!="Use All Variables"){
      as.formula(paste(input$random_forest_response, "~",paste(input$random_forest_predictor,collapse="+")))
    } else if(input$random_forest_predictor=="Use All Variables"){
      as.formula(paste(input$random_forest_response, "~."))
    }
  })
  
  #calculate RMSE on testing data
  
  rf <- reactiveValues(random.forest = NULL)
  
  observeEvent(input$execute, {
    output$rf_heading <- renderText({
      "Random Forest Training Results"
    })
    rf$random.forest = train(f(), data=vals$training, method="rf",
                             trControl = trctrl,
                             preProcess = c("center", "scale"),
                             tuneGrid = expand.grid(mtry = seq(from=1,to=15,by=1)))
    output$rf_RMSE_test <- renderPrint({
      rf$random.forest$finalModel
    })
    
    output$rf_pred_heading <- renderText({
      paste0("Prediction for ", input$random_forest_response, " using Random Forest")
    })
    
    output$rf_prediction <- renderText({
      rf_pred1 <- predict(rf$random.forest, newdata = dplyr::select(nemo_data, -input$random_forest_response))
      #rt_pred1 <- predict(rt$dtree_fit, newdata = dplyr::select(nums$rt_testing, -input$predict_value))
      
    })
    
    output$rf_RMSE_results <- renderPrint({
      rf$random.forest$results
    })
    output$varimp <- renderPlot({
      rfImp<-varImp(rf$random.forest)
      plot(rfImp, top = 20)
    })
  })
  ########################################
  
  
  
  
  
  # h4("Here are the results for the final random forest model:")
  # output$rf_RMSE_test <- renderPrint({
  #     random.forest <- train(f(), data=vals$training, method="rf",
  #                           trControl = trctrl,
  #                           preProcess = c("center", "scale"),
  #                           tuneGrid = expand.grid(mtry = seq(from=1,to=15,by=1)))
  #     random.forest$finalModel
  # })
  # output$rf_RMSE_results <- renderPrint({
  #   random.forest$results
  # })
  
  # output$rf_RMSE_test <- renderText({
  #   random.forest = train(f(), data=vals$training, method="rf",
  #                         trControl = trctrl,
  #                         preProcess = c("center", "scale"),
  #                         tuneGrid = expand.grid(mtry = seq(from=1,to=15,by=1)))
  #   #random.forest$finalModel
  #   pred <- predict(random.forest, newdata = dplyr::select(vals$testing, -input$random_forest_response))
  #   paste0("For the random forest tree, the RMSE on the testing data is ",
  #          RMSE(
  #            pred = pred,
  #            obs = vals$testing[[input$random_forest_response]]
  #          )
  #   )
  #   
  # })
  
  
  
  
  
  
  # dtree_fit <- train(f(), data = vals$training, method = "rpart",
  #                    trControl = trctrl,
  #                    tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
  # pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -input$tree_response))
  # paste0("For the regression tree, the RMSE on the testing data is ",
  #        RMSE(
  #          pred = pred,
  #          obs = vals$testing[[input$tree_response]]
  #        )
  # )
  
  #paste0("The RMSE is ", sqrt(mean((pred-(vals$testing[[input$tree_response]]))^2)))
  # })
  
  
  g <- reactive({
    if(input$tree_predictor!="Use All Variables"){
      as.formula(paste(input$tree_response, "~",paste(input$tree_predictor,collapse="+")))
    } else if(input$tree_predictor=="Use All Variables"){
      as.formula(paste(input$tree_response, "~."))
    }
  })
  
  nums <- reactiveValues(intrain = NULL,
                         training = NULL,
                         testing = NULL)
  
  observeEvent(input$split,{
    nums$rt_intrain = createDataPartition(data[[input$tree_response]], p=input$split, list = FALSE)
    nums$rt_training = data[nums$rt_intrain,]
    nums$rt_testing = data[-nums$rt_intrain,]
  })
  
  # g <- reactive({
  #   as.formula(paste(input$tree_response, "~."))
  # })
  
  
  
  
  rt <- reactiveValues(dtree_fit = NULL)
  
  # observe({
  #   rt$dtree_fit = train(g(), data = nums$rt_training, method = "rpart",
  #                        trControl = trctrl,
  #                        tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
  #  
  # 
  # 
  # })
  
  
  
  observeEvent(input$execute, {
    rt$dtree_fit = train(g(), data = nums$rt_training, method = "rpart",
                         trControl = trctrl,
                         tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
    
    output$rt_heading <- renderText({
      "Regression Tree Training Results"
    })
    output$rt_train_rmse <- renderText({
      rt_pred <- predict(rt$dtree_fit, newdata = dplyr::select(nums$rt_training, -input$tree_response))
      paste0("For the regression tree, the RMSE on the training data is ",
             RMSE(
               pred = rt_pred,
               obs = nums$rt_training[[input$tree_response]]
             )
      )
    })
    
    output$rt_pred_heading <- renderText({
      paste0("Prediction for ", input$tree_response, " using Regression Tree")
    })
    
    output$rt_prediction <- renderText({
      rt_pred1 <- predict(rt$dtree_fit, newdata = dplyr::select(nemo_data, -input$tree_response))
      #rt_pred1 <- predict(rt$dtree_fit, newdata = dplyr::select(nums$rt_testing, -input$predict_value))
      
    })
    
    output$rt_RMSE_results <- renderPrint({
      rt$dtree_fit$results
    })
    
    output$rt_plot <- renderPlot({
      rpart.plot(dtree_fit$finalModel)
    })
    
    
    
    
    # output$tree_RMSE_train <- renderText({
    #   dtree_fit <- train(g(), data = nums$rt_training, method = "rpart",
    #                      trControl = trctrl,
    #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
    #   pred <- predict(dtree_fit, newdata = dplyr::select(nums$rt_training, -input$tree_response))
    #   paste0("For the regression tree, the RMSE on the training data is ",
    #          RMSE(
    #            pred = pred,
    #            obs = nums$rt_training[[input$tree_response]]
    #          )
    #   )
    #   
    #   
    #   #paste0("The RMSE is ", sqrt(mean((pred-(nums$rt_training[[input$tree_response]]))^2)))
    # })
    # 
  })
  
  quant <- reactiveValues(intrain = NULL,
                          training = NULL,
                          testing = NULL)
  
  observeEvent(input$split,{
    quant$lm_intrain = createDataPartition(data[[input$linear_response]], p=input$split, list = FALSE)
    quant$lm_training = data[quant$lm_intrain,]
    quant$lm_testing = data[-quant$lm_intrain,]
  })
  
  # if(input$linear_predictors == "Runtime"){
  #   h <- reactive({
  #     as.formula(paste(input$linear_response, "~ Runtime"))
  #   })
  # }
  # "Year",
  # "Runtime",
  # "imdbRating",
  # "imdbVotes",
  # "nchar_Title",
  # "average_rating"
  
  # observe({
  #   output$linear_predictors <- renderUI({
  #     G <- c("Year","Runtime","imdbRating","imdbVotes","nchar_Title","average_rating")
  #     checkboxGroupInput("linear_predictors", "Choose the Variables You Would like to use to Make Your Prediction", G[! G %in% input$linear_response], G[! G %in% input$linear_response], G[! G %in% input$linear_response], G[! G %in% input$linear_response], G[! G %in% input$linear_response])
  #   })
  # })
  
  
  
  h <- reactive({
    if(input$linear_predictor!="Use All Variables"){
      as.formula(paste(input$linear_response, "~",paste(input$linear_predictor,collapse="+")))
    } else if(input$linear_predictor=="Use All Variables"){
      as.formula(paste(input$linear_response, "~."))
    }
  })
  
  
  
  linear_model <- reactiveValues(lm.model = NULL)
  
  # observe({
  #   
  #   if(input$linear_predictor=="Use All Variables"){
  #     h <- reactive({
  #       as.formula(paste(input$linear_response, "~."))
  #     })
  #   } else if(input$linear_predictor!="Use All Variables"){
  #     h <- reactive({
  #       as.formula(paste(input$linear_response, "~",paste(input$linear_predictor,collapse="+")))
  #     })
  #   }
  #   observeEvent(input$execute, {
  #     linear_model$lm.model <- train(h(), data = quant$lm_training, method = "lm",
  #                                    preProcess = c("center", "scale"),
  #                                    trControl = trctrl)
  # })
  # })
  
  observeEvent(input$execute, {
    linear_model$lm.model <- train(h(), data = quant$lm_training, method = "lm",
                                   preProcess = c("center", "scale"),
                                   trControl = trctrl)
    output$lm_heading <- renderText({
      "Linear Model Training Results"
    })
    
    output$lm_pred_heading <- renderText({
      paste0("Prediction for ", input$linear_response, " using Linear Regression")
    })
    
    output$lm_prediction <- renderText({
      lm_pred1 <- predict(linear_model$lm.model, newdata = dplyr::select(nemo_data, -input$linear_response))
      #rt_pred1 <- predict(rt$dtree_fit, newdata = dplyr::select(nums$rt_testing, -input$predict_value))
      
    })
    
    output$lm_results <- renderPrint({
      linear_model$lm.model$results
    })
    output$lm_sum_results <- renderPrint({
      summary(linear_model$lm.model)
    })
  })
  
  
  
  # test_pred <- reactiveValues(pred = NULL)
  
  # observe({
  #   column_names<-c("Year","Rated", "Runtime","imdbRating","imdbVotes","nchar_Title","first_Genre","first_Country","Summary_Awards","average_rating")
  #   values<-c(input$pick_Year,input$pick_rating,input$pick_runtime,input$pick_imdbRating,input$pick_imdbVotes,input$pick_nchar_Title,input$pick_first_Genre,input$pick_first_Country,input$pick_Summary_Awards,input$pick_average_rating)
  #   df <- data.frame(column_names, values)
  #   test_pred$pred <- setNames(data.frame(t(df[,-1])), df[,1])
  #   test_pred$pred$Year <- as.numeric(test_pred$pred$Year)
  #   test_pred$pred$Rated <- as.factor(test_pred$pred$Rated)
  #   test_pred$pred$Runtime <- as.numeric(test_pred$pred$Runtime)
  #   test_pred$pred$imdbRating <- as.numeric(test_pred$pred$imdbRating)
  #   test_pred$pred$imdbVotes <- as.numeric(test_pred$pred$imdbVotes)
  #   test_pred$pred$nchar_Title <- as.numeric(test_pred$pred$nchar_Title)
  #   test_pred$pred$first_Genre <- as.factor(test_pred$pred$first_Genre)
  #   test_pred$pred$first_Country <- as.factor(test_pred$pred$first_Country)
  #   test_pred$pred$Summary_Awards <- as.factor(test_pred$pred$Summary_Awards)
  #   test_pred$pred$average_rating <- as.numeric(test_pred$pred$average_rating)
  # })
  
  #observeEvent(input$predict, {
  
  
  #    observeEvent(input$predict, {
  #     output$rt_prediction <- renderText({
  #       rt_pred1 <- predict(rt$dtree_fit, newdata = dplyr::select(nums$rt_testing, -input$predict_value))
  #       rt_pred1
  # })
  #     })
  
  #})
  
  
  
  # nemo <- reactiveValues(nemo_test = NULL)
  
  
}))





























# 
# library(httr) #this package will help use use the URL we built to get information from the OMDb API
# library(jsonlite) #this package will help us convert the data we get from the OMDb API to a more usable format
# library(tidyverse) #this package will help us work with our nicely formatted data.
# library(lubridate) #this package will help us create dates 
# library(ggplot2) #this package will help us make graphs
# library(readr)
# library(class)
# library(caret)
# library(DT)
# library(tree)
# library(readxl)
# library(readr)
# library(rpart.plot)
# library(randomForest)
# 
# data <- read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//movie_data_7_30_2022.csv")
# #data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!
# 
# #this will help us convert the Ratings.Value column to numeric
# parse_number <- function(S){
#   if(grepl("/", S)){
#     A<-str_split(S, "/")
#     A<-as.numeric(unlist(A))
#     A<-A[[1]]/A[[2]]
#     A<-A*100
#   } else {
#     A<-as.numeric(gsub("%","",S))
#   }
#   return(A)
# }
# 
# #this will help us make the Summary_Awards column:
# award <- function(S){
#   if(is.na(S)){
#     A<-"none"
#     return(A)
#   }
#   S=tolower(S)
#   #won and nominated:
#   if((grepl("won", S) | grepl("win", S)) && (grepl("nomina", S))){
#     A<-"won and nominated"
#   }
#   #only won:
#   else if (grepl("won", S) | grepl("win", S)){
#     A<-"won"
#   }
#   #only nominated:
#   else if (grepl("nomina", S)){
#     A<-"nomination"
#   }
#   #no awards or nominations:
#   else {(A<-"none")
#     return(A)
#   }
# }
# 
# mat1=NULL
# format_data <- function(data){
#   data$nchar_Title <- nchar(data$Title)
#   data$Year <- as.numeric(data$Year)
#   data$Rated <- as.factor(data$Rated)
#   data$Released <- NULL
#   data$Runtime <- as.numeric(gsub(" min","",data$Runtime))
#   
#   df <- data.frame(x = data$Genre)
#   A<-df %>% separate(x, c('Genre_1'))
#   data$first_Genre <- as.factor(A$Genre_1)
#   data$Genre = NULL
#   
#   df <- data.frame(x = data$Country)
#   A<-df %>% separate(x, c('Country_1'),sep=",")
#   data$first_Country <- A$Country_1
#   data$first_Country <- gsub("USA","United States",data$first_Country)
#   data$first_Country <- as.factor(data$first_Country)
#   data$Country = NULL
#   
#   data$Ratings.Value <- sapply(data$Ratings.Value, FUN=parse_number)
#   data$Summary_Awards <- as.factor(sapply(data$Awards, FUN=award))
#   data$Metascore <- as.numeric(data$Metascore)
#   data$imdbRating <- as.numeric(data$imdbRating)*10
#   data$imdbVotes <- as.numeric(gsub(",","",data$imdbVotes))
#   data$DVD <- NULL
#   data$BoxOffice <- gsub("\\$","",data$BoxOffice)
#   data$BoxOffice <- as.numeric(gsub(",","",data$BoxOffice))
#   
#   data$Plot <- NULL
#   data$Language <- NULL
#   data$Awards<-NULL
#   data$Poster<-NULL
#   data$imdbID<-NULL
#   data$Type<-NULL
#   data$Production<-NULL
#   data$Website<-NULL
#   data$Response<-NULL
#   
#   movie_list<-unique(data$Title)
#   
#   for (i in movie_list){
#     temp=data[is.element(data$Title,i),]
#     Ratings.Value_mean<-mean(temp$Ratings.Value)
#     Metascore<-unique(temp$Metascore)
#     imdbRating<-unique(temp$imdbRating)
#     
#     #note: the average_rating column is the average of ratings from  
#     #Internet Movie Database (when applicable)
#     #Metacritic (when applicable)
#     #Rotten Tomatoes (when applicable)
#     #Metascore (when applicable)
#     #imdbRating
#     
#     if(is.na(Metascore)==TRUE){
#       temp$average_rating=(Ratings.Value_mean+imdbRating)/2
#     }
#     if(is.na(Metascore)==FALSE){
#       temp$average_rating=(Ratings.Value_mean+Metascore+imdbRating)/3
#     }
#     mat1=rbind(mat1,temp)
#   }
#   mat1$Ratings.Value <- NULL
#   mat1$Ratings.Source <- NULL
#   mat1 <- unique(mat1)
#   return(mat1)
# }
# 
# data <- format_data(data)
# data$Rated[data$Rated == "N/A"] <- NA
# data <- data %>% select(-c(Metascore,BoxOffice))
# data$Rated <- droplevels(data$Rated)
# data$first_Genre <- droplevels(data$first_Genre)
# data$first_Country <- droplevels(data$first_Country)
# data$Summary_Awards <- droplevels(data$Summary_Awards)
# data_with_titles <- data[complete.cases(data), ]
# 
# data <- data %>% select(-c(Title,Director,Writer,Actors ))
# 
# data <- data[complete.cases(data), ]
# data$Rated <- droplevels(data$Rated)
# data$first_Genre <- droplevels(data$first_Genre)
# data$first_Country <- droplevels(data$first_Country)
# data$Summary_Awards <- droplevels(data$Summary_Awards)
# data_with_titles <- data[complete.cases(data), ]
# set.seed(dim(data)[1])
# # train <- sample(1:nrow(A), size = nrow(A)*0.8)
# # test <- dplyr::setdiff(1:nrow(A), train)
# # ATrain <- A[train, ]
# # ATest <- A[test, ]
# 
# 
# # trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# # dtree_fit <- train(average_rating ~ ., data = ATrain, method = "rpart",
# #                    trControl = trctrl,
# #                    tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
# # pred <- predict(dtree_fit, newdata = dplyr::select(ATest, -average_rating))
# #sqrt(mean((pred-ATest$average_rating)^2))
# 
# 
# library(shiny)
# library(caret)
# library(tidyverse)
# library(DT)
# #data("GermanCredit")
# library(shiny)
# library(ggplot2)
# 
# trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# shinyServer(function(input,output,session)({
#   
#   ## Get the value of the dataset that is selected by user from the list of datasets
#   ## to output the dataset
#   
#   output$dat <- DT::renderDataTable({
#     data_with_titles<-datatable(data_with_titles)
#     data_with_titles
#   })
#   
#   # output$dat <- renderPrint({
#   #   data_with_titles
#   # })
#   
#   output$struct <- renderPrint({
#     str(data_with_titles)
#   })
#   
#   output$cont_table <- renderPrint({
#     
#     tv <- data %>% select(input$table_variable,input$table_variable2)
#     table(tv)
#   })
#   
#   output$summary_table <- renderPrint({
#     if(input$covariance=="None"){
#       sv <- data %>% select(input$summary_variable)
#       summary(sv)
#     } else if(input$covariance!="None"){
#       A<-data %>% select(input$summary_variable)
#       B<-data %>% select(input$covariance)
#       cov(A,B)
#       
#     }
#     
#   })
#   
#   #if (x%%2 == 0 || x%%5 == 0) {
#   
#   # if(input$table_variable=="Rated"||input$table_variable=="first_Genre"||input$table_variable=="first_Country"||input$table_variable=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable)
#   #   table(tv)
#   # } else if(input$table_variable2=="Rated"||input$table_variable2=="first_Genre"||input$table_variable2=="first_Country"||input$table_variable2=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable,input$table)
#   #   table(tv)
#   #   
#   # }
#   
#   
#   
#   
#   # output$radio_button <- renderPlot({
#   #   
#   #   if(input$radio_button=="Just Classification") {
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count")
#   #   } else if(input$radio_button=="Classification and Unemployed"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #       scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   #   } else if(input$radio_button=="Classification and Foreign"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #       scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   #   }
#   # })
#   # 
#   
#   
#   output$bar_plot <- renderPlot({
#     ggplot(data,aes_string(x=input$bar_plot_variable)) +
#       geom_bar(fill="lightblue") +
#       ggtitle(paste0("Bar Plot: ",input$bar_plot_variable ))
#   })
#   
#   output$scatter_plot <- renderPlot({
#     ggplot(data,aes_string(x=input$scatter_plot_x_variable,y=input$scatter_plot_y_variable,color=input$color_variable)) +
#       geom_point() +
#       ggtitle(paste0("Scatter Plot: ",input$scatter_plot_x_variable, " vs.", input$scatter_plot_y_variable)) +
#       scale_color_discrete(name = input$color_variable)
#   })
#   # output$cont_table <- renderPrint({
#   #   if(input$table=="One Way") {
#   #     tv1 <- ATrain %>% select(Rated)
#   #     table(tv1)
#   #     
#   #     tv2 <- ATrain %>% select(first_Genre)
#   #     table(tv2)
#   #     
#   #     tv3 <- ATrain %>% select(first_Country)
#   #     table(tv3)
#   #     
#   #     tv4 <- ATrain %>% select(Summary_Awards)
#   #     table(tv4)
#   #   } #else if(input$radio_button=="Classification and Unemployed"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #     scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   # } else if(input$radio_button=="Classification and Foreign"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #     scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   # }
#   #})
#   
#   
#   # output$summary <- renderPlot({
#   #   ggplot(ATrain, aes(x = average_rating)) +
#   #   geom_histogram(stat="count")
#   #   #summary(get(input$dataset))
#   # })
#   
#   output$summary <- renderPlot({
#     ggplot(data,aes_string(x=input$hist_graph_variable)) +
#       geom_histogram(color = "blue", fill = "red", size = 2) +
#       ggtitle(paste0("Histogram: Distribution of ",input$hist_graph_variable ))
#   })
#   
#   # output$radio_button <- renderPlot({
#   #        if(input$select_box_1=="Bar Plot") {
#   #          g <- ggplot(data(), aes_string(x = input$sel_bar_plot))
#   #          g + geom_histogram(stat="count")
#   #        }
#   
#   
#   # Pulling the list of variable for choice of variable x
#   output$varx <- renderUI({
#     selectInput("variablex", "select the X variable", choices=names(data()))
#   })
#   
#   # Pulling the list of variable for choice of variable y
#   output$vary <- renderUI({
#     selectInput("variabley", "select the Y variable", choices=names(data()))
#   })
#   
#   #Creates a new reactive values
#   # observe({
#   #   vals <- reactiveValues(intrain = createDataPartition(data$average_rating, p= input$split, list = FALSE),
#   #                          training = data[intrain,],
#   #                          testing = data[-intrain,])
#   # })
#   
#   vals <- reactiveValues(intrain = NULL,
#                          training = NULL,
#                          testing = NULL)
#   observeEvent(input$split,{
#     vals$intrain = createDataPartition(data[[input$tree_response]], p=input$split, list = FALSE)
#     vals$training = data[vals$intrain,]
#     vals$testing = data[-vals$intrain,]
#   })
#   
#   output$table <- DT::renderDataTable({
#     view_training_data<-datatable(vals$training)
#     view_training_data
#   })
#   # output$table2 <- DT::renderDataTable({
#   #   data2<-datatable(vals$testing)
#   #   data2
#   # })
#   
#   f <- reactive({
#     as.formula(paste(input$tree_response, "~."))
#   })
#   e <- reactive({
#     as.formula(paste("-", input$tree_response))
#   })
#   # Linear_Model <- reactive({
#   #   lm(f(), data = trainingData())
#   # })
#   
#   # output$tree_RMSE <- renderText({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                     trControl = trctrl,
#   #                     tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -e()))
#   #   paste0("The RMSE is ", sqrt(mean((pred-vals$testing$e())^2)))
#   # })
#   # output$tree_plot <- renderPlot({
#   #   rpart.plot(dtree_fit$finalModel)
#   # })
#   # output$tree_plot <- renderPlot({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                      trControl = trctrl,
#   #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   rpart.plot(dtree_fit$finalModel)
#   # })
#   # 
#   # output$tree_RMSE <- renderText({
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing))
#   #   paste0("The RMSE is ", sqrt(mean((pred-vals$testing$Year)^2)))
#   # })
#   # 
#   
#   obs <- reactive({
#     vals$testing %>% select(input$tree_response)
#   })
#   
#   #calculate RMSE on testing data
#   observeEvent(input$execute, {
#     output$tree_RMSE <- renderText({
#       
#       dtree_fit <- train(f(), data = vals$training, method = "rpart",
#                          trControl = trctrl,
#                          tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#       pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -input$tree_response))
#       paste0("For the regression tree, the RMSE on the testing data is ",
#              RMSE(
#                pred = pred,
#                obs = vals$testing[[input$tree_response]]
#              )
#       )
#       
#       #paste0("The RMSE is ", sqrt(mean((pred-(vals$testing[[input$tree_response]]))^2)))
#     })
#     
#     
#   })
#   
#   
#   #calculate training RMSE
#   observeEvent(input$execute,{
#     output$tree_RMSE_train <- renderText({
#       dtree_fit <- train(f(), data = vals$training, method = "rpart",
#                          trControl = trctrl,
#                          tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#       pred <- predict(dtree_fit, newdata = dplyr::select(vals$training, -input$tree_response))
#       paste0("For the regression tree, the RMSE on the training data is ",
#              RMSE(
#                pred = pred,
#                obs = vals$training[[input$tree_response]]
#              )
#       )
#       
#       
#       #paste0("The RMSE is ", sqrt(mean((pred-(vals$training[[input$tree_response]]))^2)))
#     })
#     
#     
#   })
#   
#   
#   # output$tree_RMSE <- renderText({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                      trControl = trctrl,
#   #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -input$tree_response))
#   #   paste0("The RMSE is ", sqrt(mean((pred-dplyr::select(vals$testing, input$tree_response))^2)))
#   # })
#   
#   
#   # output$tree_RMSE <- renderText({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                      trControl = trctrl,
#   #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   
#   #   
#   #   
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -input$tree_response),type = "vector")
#   #   paste0("The RMSE is ", sqrt(mean((pred-vals$testing$Year)^2)))
#   # })
#   
#   
#   #paste0("The RMSE is ", tree_RMSE_value)
#   
#   #paste0("The RMSE is ", sqrt(mean((pred-obs())^2)))
#   
#   # tree_RMSE_value <- RMSE(
#   #   pred = pred,
#   #   obs = cs_test$Sales
#   # )
#   
#   observeEvent(input$execute,{
#     output$tree_plot <- renderPlot({
#       withProgress(message = 'Popping Popcorn (Running Model)', value = 0, {
#         dtree_fit <- train(f(), data = vals$training, method = "rpart",
#                            trControl = trctrl,
#                            tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#         rpart.plot(dtree_fit$finalModel)
#       })
#     })
#     
#   })
#   
#   
#   nums <- reactiveValues(intrain = NULL,
#                          training = NULL,
#                          testing = NULL)
#   
#   observeEvent(input$split,{
#     nums$intrain = createDataPartition(data[[input$random_forest_response]], p=input$split, list = FALSE)
#     nums$training = data[nums$intrain,]
#     nums$testing = data[-nums$intrain,]
#   })
#   
#   g <- reactive({
#     as.formula(paste(input$random_forest_response, "~."))
#   })
#   
#   #calculate random forest model: stats using training set
#   observeEvent(input$execute,{
#     output$rf_RMSE_train <- renderPrint({
#       rfFit <- randomForest(Year ~ ., data = nums$training, mtry = ncol(nums$training)/3,ntree = 200, importance = TRUE)
#       rfPred <- predict(rfFit, newdata = dplyr::select(nums$testing, -Year))
#       rfRMSE <- sqrt(mean((rfPred-nums$testing$Year)^2))
#       rfRMSE
#       
#       
#       #   withProgress(message = 'Popping Popcorn (Running Model)', value = 10, {
#       #   random.forest = train(g(), data=nums$training, method="rf", 
#       #                         trControl = trctrl,
#       #                         preProcess = c("center", "scale"),
#       #                         tuneGrid = expand.grid(mtry = seq(from=1,to=15,by=1)))
#       #   # random.forest$modelType
#       #   # random.forest$preProcess
#       #   # random.forest$results
#       #   # random.forest$bestTune
#       #   random.forest$finalModel
#       #   })
#       #   #paste0("The RMSE is ", sqrt(mean((pred-(vals$training[[input$tree_response]]))^2)))
#       # })
#       # output$rf_RMSE_table <- renderPrint({
#       #   random.forest$results
#       # })
#       # 
#       # output$varimp <- renderPlot({
#       #   rfImp<-varImp(random.forest)
#       #   plot(rfImp, top = 20)
#       #   
#       # })
#       #     output$rf_train_stats <- renderPrint({
#       #       rf_pred <- predict(random.forest, newdata = dplyr::select(nums$testing, input$random_forest_response))
#       #       #paste0("For the random forest, the RMSE on the training data is ",
#       #              #rf_test_pred <- predict(random.forest, newdata = dplyr::select(vals$testing, -input$random_forest_response))
#       #              confusionMatrix(rf_test_pred, nums$testing[[input$random_forest_response]])
#       #              # RMSE(
#       #              #   pred = rf_pred,
#       #              #   obs = vals$testing[[input$random_forest_response]]
#       #              # )
#       #       #)
#       #       
#       #     })
#       
#       #paste0("The RMSE is ", sqrt(mean((pred-(vals$testing[[input$tree_response]]))^2)))
#     })
#     
#     
#     
#     
#   })
#   
#   # output$text <- renderPlot({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                      trControl = trctrl,
#   #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   rpart.plot(dtree_fit$finalModel)
#   # })
#   
#   
#   
#   
#   
#   
#   #   set.seed = set.seed(dim(data)[1]))
#   # 
#   # output$split_data <- renderPrint({
#   #   vals$set.seed
#   #   data <- data %>% select(input$response)
#   #   intrain <- createDataPartition(data, p= input$split, list = FALSE)
#   #   training <- data[intrain,]
#   #   testing <- data[-intrain,]
#   # })
#   
#   # output$split_data <- renderPrint({
#   # set.seed(dim(data)[1])
#   # train <- sample(1:nrow(data), size = nrow(data)*0.8)
#   # test <- dplyr::setdiff(1:nrow(data), train)
#   # Train <- data[train, ]
#   # Test <- data[test, ]
#   # 
#   # })
#   
#   
#   # to output the structure of the dataset
#   
#   
#   # for summary
#   
#   
#   # For plot
#   output$plot <- renderPlot({
#     ggplot(data(),aes_string(x=input$variablex, y=input$variabley)) +
#       geom_point()
#   })
#   
# }))























# 
# library(httr) #this package will help use use the URL we built to get information from the OMDb API
# library(jsonlite) #this package will help us convert the data we get from the OMDb API to a more usable format
# library(tidyverse) #this package will help us work with our nicely formatted data.
# library(lubridate) #this package will help us create dates 
# library(ggplot2) #this package will help us make graphs
# library(readr)
# library(class)
# library(caret)
# library(DT)
# library(tree)
# library(readxl)
# library(readr)
# library(rpart.plot)
# 
# data <- read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//movie_data_7_30_2022.csv")
# #data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!
# 
# #this will help us convert the Ratings.Value column to numeric
# parse_number <- function(S){
#   if(grepl("/", S)){
#     A<-str_split(S, "/")
#     A<-as.numeric(unlist(A))
#     A<-A[[1]]/A[[2]]
#     A<-A*100
#   } else {
#     A<-as.numeric(gsub("%","",S))
#   }
#   return(A)
# }
# 
# #this will help us make the Summary_Awards column:
# award <- function(S){
#   if(is.na(S)){
#     A<-"none"
#     return(A)
#   }
#   S=tolower(S)
#   #won and nominated:
#   if((grepl("won", S) | grepl("win", S)) && (grepl("nomina", S))){
#     A<-"won and nominated"
#   }
#   #only won:
#   else if (grepl("won", S) | grepl("win", S)){
#     A<-"won"
#   }
#   #only nominated:
#   else if (grepl("nomina", S)){
#     A<-"nomination"
#   }
#   #no awards or nominations:
#   else {(A<-"none")
#     return(A)
#   }
# }
# 
# mat1=NULL
# format_data <- function(data){
#   data$nchar_Title <- nchar(data$Title)
#   data$Year <- as.numeric(data$Year)
#   data$Rated <- as.factor(data$Rated)
#   data$Released <- NULL
#   data$Runtime <- as.numeric(gsub(" min","",data$Runtime))
#   
#   df <- data.frame(x = data$Genre)
#   A<-df %>% separate(x, c('Genre_1'))
#   data$first_Genre <- as.factor(A$Genre_1)
#   data$Genre = NULL
#   
#   df <- data.frame(x = data$Country)
#   A<-df %>% separate(x, c('Country_1'),sep=",")
#   data$first_Country <- A$Country_1
#   data$first_Country <- gsub("USA","United States",data$first_Country)
#   data$first_Country <- as.factor(data$first_Country)
#   data$Country = NULL
#   
#   data$Ratings.Value <- sapply(data$Ratings.Value, FUN=parse_number)
#   data$Summary_Awards <- as.factor(sapply(data$Awards, FUN=award))
#   data$Metascore <- as.numeric(data$Metascore)
#   data$imdbRating <- as.numeric(data$imdbRating)*10
#   data$imdbVotes <- as.numeric(gsub(",","",data$imdbVotes))
#   data$DVD <- NULL
#   data$BoxOffice <- gsub("\\$","",data$BoxOffice)
#   data$BoxOffice <- as.numeric(gsub(",","",data$BoxOffice))
#   
#   data$Plot <- NULL
#   data$Language <- NULL
#   data$Awards<-NULL
#   data$Poster<-NULL
#   data$imdbID<-NULL
#   data$Type<-NULL
#   data$Production<-NULL
#   data$Website<-NULL
#   data$Response<-NULL
#   
#   movie_list<-unique(data$Title)
#   
#   for (i in movie_list){
#     temp=data[is.element(data$Title,i),]
#     Ratings.Value_mean<-mean(temp$Ratings.Value)
#     Metascore<-unique(temp$Metascore)
#     imdbRating<-unique(temp$imdbRating)
#     
#     #note: the average_rating column is the average of ratings from  
#     #Internet Movie Database (when applicable)
#     #Metacritic (when applicable)
#     #Rotten Tomatoes (when applicable)
#     #Metascore (when applicable)
#     #imdbRating
#     
#     if(is.na(Metascore)==TRUE){
#       temp$average_rating=(Ratings.Value_mean+imdbRating)/2
#     }
#     if(is.na(Metascore)==FALSE){
#       temp$average_rating=(Ratings.Value_mean+Metascore+imdbRating)/3
#     }
#     mat1=rbind(mat1,temp)
#   }
#   mat1$Ratings.Value <- NULL
#   mat1$Ratings.Source <- NULL
#   mat1 <- unique(mat1)
#   return(mat1)
# }
# 
# data <- format_data(data)
# data$Rated[data$Rated == "N/A"] <- NA
# data <- data %>% select(-c(Metascore,BoxOffice))
# data_with_titles <- data[complete.cases(data), ]
# 
# data <- data %>% select(-c(Title,Director,Writer,Actors ))
# 
# data <- data[complete.cases(data), ]
# set.seed(dim(data)[1])
# # train <- sample(1:nrow(A), size = nrow(A)*0.8)
# # test <- dplyr::setdiff(1:nrow(A), train)
# # ATrain <- A[train, ]
# # ATest <- A[test, ]
# 
# 
# # trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# # dtree_fit <- train(average_rating ~ ., data = ATrain, method = "rpart",
# #                    trControl = trctrl,
# #                    tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
# # pred <- predict(dtree_fit, newdata = dplyr::select(ATest, -average_rating))
# #sqrt(mean((pred-ATest$average_rating)^2))
# 
# 
# library(shiny)
# library(caret)
# library(tidyverse)
# library(DT)
# #data("GermanCredit")
# library(shiny)
# library(ggplot2)
# 
# trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# shinyServer(function(input,output,session)({
#   
#   ## Get the value of the dataset that is selected by user from the list of datasets
#   ## to output the dataset
#   
#   output$dat <- DT::renderDataTable({
#     data_with_titles<-datatable(data_with_titles)
#     data_with_titles
#   })
#   
#   # output$dat <- renderPrint({
#   #   data_with_titles
#   # })
#   
#   output$struct <- renderPrint({
#     str(data_with_titles)
#   })
#   
#   output$cont_table <- renderPrint({
#     
#     tv <- data %>% select(input$table_variable,input$table_variable2)
#     table(tv)
#   })
#   
#   output$summary_table <- renderPrint({
#     if(input$covariance=="None"){
#       sv <- data %>% select(input$summary_variable)
#       summary(sv)
#     } else if(input$covariance!="None"){
#       A<-data %>% select(input$summary_variable)
#       B<-data %>% select(input$covariance)
#       cov(A,B)
#       
#     }
#     
#   })
#   
#   #if (x%%2 == 0 || x%%5 == 0) {
#   
#   # if(input$table_variable=="Rated"||input$table_variable=="first_Genre"||input$table_variable=="first_Country"||input$table_variable=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable)
#   #   table(tv)
#   # } else if(input$table_variable2=="Rated"||input$table_variable2=="first_Genre"||input$table_variable2=="first_Country"||input$table_variable2=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable,input$table)
#   #   table(tv)
#   #   
#   # }
#   
#   
#   
#   
#   # output$radio_button <- renderPlot({
#   #   
#   #   if(input$radio_button=="Just Classification") {
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count")
#   #   } else if(input$radio_button=="Classification and Unemployed"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #       scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   #   } else if(input$radio_button=="Classification and Foreign"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #       scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   #   }
#   # })
#   # 
#   
#   
#   output$bar_plot <- renderPlot({
#     ggplot(data,aes_string(x=input$bar_plot_variable)) +
#       geom_bar(fill="lightblue") +
#       ggtitle(paste0("Bar Plot: ",input$bar_plot_variable ))
#   })
#   
#   output$scatter_plot <- renderPlot({
#     ggplot(data,aes_string(x=input$scatter_plot_x_variable,y=input$scatter_plot_y_variable,color=input$color_variable)) +
#       geom_point() +
#       ggtitle(paste0("Scatter Plot: ",input$scatter_plot_x_variable, " vs.", input$scatter_plot_y_variable)) +
#       scale_color_discrete(name = input$color_variable)
#   })
#   # output$cont_table <- renderPrint({
#   #   if(input$table=="One Way") {
#   #     tv1 <- ATrain %>% select(Rated)
#   #     table(tv1)
#   #     
#   #     tv2 <- ATrain %>% select(first_Genre)
#   #     table(tv2)
#   #     
#   #     tv3 <- ATrain %>% select(first_Country)
#   #     table(tv3)
#   #     
#   #     tv4 <- ATrain %>% select(Summary_Awards)
#   #     table(tv4)
#   #   } #else if(input$radio_button=="Classification and Unemployed"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #     scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   # } else if(input$radio_button=="Classification and Foreign"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #     scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   # }
#   #})
#   
#   
#   # output$summary <- renderPlot({
#   #   ggplot(ATrain, aes(x = average_rating)) +
#   #   geom_histogram(stat="count")
#   #   #summary(get(input$dataset))
#   # })
#   
#   output$summary <- renderPlot({
#     ggplot(data,aes_string(x=input$hist_graph_variable)) +
#       geom_histogram(color = "blue", fill = "red", size = 2) +
#       ggtitle(paste0("Histogram: Distribution of ",input$hist_graph_variable ))
#   })
#   
#   # output$radio_button <- renderPlot({
#   #        if(input$select_box_1=="Bar Plot") {
#   #          g <- ggplot(data(), aes_string(x = input$sel_bar_plot))
#   #          g + geom_histogram(stat="count")
#   #        }
#   
#   
#   # Pulling the list of variable for choice of variable x
#   output$varx <- renderUI({
#     selectInput("variablex", "select the X variable", choices=names(data()))
#   })
#   
#   # Pulling the list of variable for choice of variable y
#   output$vary <- renderUI({
#     selectInput("variabley", "select the Y variable", choices=names(data()))
#   })
#   
#   #Creates a new reactive values
#   # observe({
#   #   vals <- reactiveValues(intrain = createDataPartition(data$average_rating, p= input$split, list = FALSE),
#   #                          training = data[intrain,],
#   #                          testing = data[-intrain,])
#   # })
#   
#   vals <- reactiveValues(intrain = NULL,
#                          training = NULL,
#                          testing = NULL)
#   observeEvent(input$split,{
#     vals$intrain = createDataPartition(data[[input$tree_response]], p=input$split, list = FALSE)
#     vals$training = data[vals$intrain,]
#     vals$testing = data[-vals$intrain,]
#   })
#   
#   output$table <- DT::renderDataTable({
#     view_training_data<-datatable(vals$training)
#     view_training_data
#   })
#   # output$table2 <- DT::renderDataTable({
#   #   data2<-datatable(vals$testing)
#   #   data2
#   # })
#   
#   f <- reactive({
#     as.formula(paste(input$tree_response, "~."))
#   })
#   e <- reactive({
#     as.formula(paste("-", input$tree_response))
#   })
#   # Linear_Model <- reactive({
#   #   lm(f(), data = trainingData())
#   # })
#   
#   # output$tree_RMSE <- renderText({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                     trControl = trctrl,
#   #                     tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -e()))
#   #   paste0("The RMSE is ", sqrt(mean((pred-vals$testing$e())^2)))
#   # })
#   # output$tree_plot <- renderPlot({
#   #   rpart.plot(dtree_fit$finalModel)
#   # })
#   # output$tree_plot <- renderPlot({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                      trControl = trctrl,
#   #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   rpart.plot(dtree_fit$finalModel)
#   # })
#   # 
#   # output$tree_RMSE <- renderText({
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing))
#   #   paste0("The RMSE is ", sqrt(mean((pred-vals$testing$Year)^2)))
#   # })
#   # 
#   
#   obs <- reactive({
#     vals$testing %>% select(input$tree_response)
#   })
#   
#   #calculate RMSE on testing data
#   observeEvent(input$execute, {
#     output$tree_RMSE <- renderText({
#       dtree_fit <- train(f(), data = vals$training, method = "rpart",
#                          trControl = trctrl,
#                          tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#       pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -input$tree_response))
#       paste0("For the regression tree, the RMSE on the testing data is ",
#              RMSE(
#                pred = pred,
#                obs = vals$testing[[input$tree_response]]
#              )
#       )
#       
#       
#       #paste0("The RMSE is ", sqrt(mean((pred-(vals$testing[[input$tree_response]]))^2)))
#     })
#     
#     
#   })
#   
#   
#   #calculate training RMSE
#   observeEvent(input$execute,{
#     output$tree_RMSE_train <- renderText({
#       dtree_fit <- train(f(), data = vals$training, method = "rpart",
#                          trControl = trctrl,
#                          tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#       pred <- predict(dtree_fit, newdata = dplyr::select(vals$training, -input$tree_response))
#       paste0("For the regression tree, the RMSE on the training data is ",
#              RMSE(
#                pred = pred,
#                obs = vals$training[[input$tree_response]]
#              )
#       )
#       
#       
#       #paste0("The RMSE is ", sqrt(mean((pred-(vals$training[[input$tree_response]]))^2)))
#     })
#     
#     
#   })
#   
#   
#   # output$tree_RMSE <- renderText({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                      trControl = trctrl,
#   #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -input$tree_response))
#   #   paste0("The RMSE is ", sqrt(mean((pred-dplyr::select(vals$testing, input$tree_response))^2)))
#   # })
#   
#   
#   # output$tree_RMSE <- renderText({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                      trControl = trctrl,
#   #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   
#   #   
#   #   
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -input$tree_response),type = "vector")
#   #   paste0("The RMSE is ", sqrt(mean((pred-vals$testing$Year)^2)))
#   # })
#   
#   
#   #paste0("The RMSE is ", tree_RMSE_value)
#   
#   #paste0("The RMSE is ", sqrt(mean((pred-obs())^2)))
#   
#   # tree_RMSE_value <- RMSE(
#   #   pred = pred,
#   #   obs = cs_test$Sales
#   # )
#   
#   observeEvent(input$execute,{
#     output$tree_plot <- renderPlot({
#       dtree_fit <- train(f(), data = vals$training, method = "rpart",
#                          trControl = trctrl,
#                          tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#       rpart.plot(dtree_fit$finalModel)
#     })
#     
#   })
#   
#   output$text <- renderPlot({
#     dtree_fit <- train(f(), data = vals$training, method = "rpart",
#                        trControl = trctrl,
#                        tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#     rpart.plot(dtree_fit$finalModel)
#   })
#   
#   
#   
#   
#   
#   
#   #   set.seed = set.seed(dim(data)[1]))
#   # 
#   # output$split_data <- renderPrint({
#   #   vals$set.seed
#   #   data <- data %>% select(input$response)
#   #   intrain <- createDataPartition(data, p= input$split, list = FALSE)
#   #   training <- data[intrain,]
#   #   testing <- data[-intrain,]
#   # })
#   
#   # output$split_data <- renderPrint({
#   # set.seed(dim(data)[1])
#   # train <- sample(1:nrow(data), size = nrow(data)*0.8)
#   # test <- dplyr::setdiff(1:nrow(data), train)
#   # Train <- data[train, ]
#   # Test <- data[test, ]
#   # 
#   # })
#   
#   
#   # to output the structure of the dataset
#   
#   
#   # for summary
#   
#   
#   # For plot
#   output$plot <- renderPlot({
#     ggplot(data(),aes_string(x=input$variablex, y=input$variabley)) +
#       geom_point()
#   })
#   
# }))





























# 
# library(httr) #this package will help use use the URL we built to get information from the OMDb API
# library(jsonlite) #this package will help us convert the data we get from the OMDb API to a more usable format
# library(tidyverse) #this package will help us work with our nicely formatted data.
# library(lubridate) #this package will help us create dates 
# library(ggplot2) #this package will help us make graphs
# library(readr)
# library(class)
# library(caret)
# library(DT)
# library(tree)
# library(readxl)
# library(readr)
# library(rpart.plot)
# 
# data <- read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//movie_data_7_30_2022.csv")
# #data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!
# 
# #this will help us convert the Ratings.Value column to numeric
# parse_number <- function(S){
#   if(grepl("/", S)){
#     A<-str_split(S, "/")
#     A<-as.numeric(unlist(A))
#     A<-A[[1]]/A[[2]]
#     A<-A*100
#   } else {
#     A<-as.numeric(gsub("%","",S))
#   }
#   return(A)
# }
# 
# #this will help us make the Summary_Awards column:
# award <- function(S){
#   if(is.na(S)){
#     A<-"none"
#     return(A)
#   }
#   S=tolower(S)
#   #won and nominated:
#   if((grepl("won", S) | grepl("win", S)) && (grepl("nomina", S))){
#     A<-"won and nominated"
#   }
#   #only won:
#   else if (grepl("won", S) | grepl("win", S)){
#     A<-"won"
#   }
#   #only nominated:
#   else if (grepl("nomina", S)){
#     A<-"nomination"
#   }
#   #no awards or nominations:
#   else {(A<-"none")
#     return(A)
#   }
# }
# 
# mat1=NULL
# format_data <- function(data){
#   data$nchar_Title <- nchar(data$Title)
#   data$Year <- as.numeric(data$Year)
#   data$Rated <- as.factor(data$Rated)
#   data$Released <- NULL
#   data$Runtime <- as.numeric(gsub(" min","",data$Runtime))
#   
#   df <- data.frame(x = data$Genre)
#   A<-df %>% separate(x, c('Genre_1'))
#   data$first_Genre <- as.factor(A$Genre_1)
#   data$Genre = NULL
#   
#   df <- data.frame(x = data$Country)
#   A<-df %>% separate(x, c('Country_1'),sep=",")
#   data$first_Country <- A$Country_1
#   data$first_Country <- gsub("USA","United States",data$first_Country)
#   data$first_Country <- as.factor(data$first_Country)
#   data$Country = NULL
#   
#   data$Ratings.Value <- sapply(data$Ratings.Value, FUN=parse_number)
#   data$Summary_Awards <- as.factor(sapply(data$Awards, FUN=award))
#   data$Metascore <- as.numeric(data$Metascore)
#   data$imdbRating <- as.numeric(data$imdbRating)*10
#   data$imdbVotes <- as.numeric(gsub(",","",data$imdbVotes))
#   data$DVD <- NULL
#   data$BoxOffice <- gsub("\\$","",data$BoxOffice)
#   data$BoxOffice <- as.numeric(gsub(",","",data$BoxOffice))
#   
#   data$Plot <- NULL
#   data$Language <- NULL
#   data$Awards<-NULL
#   data$Poster<-NULL
#   data$imdbID<-NULL
#   data$Type<-NULL
#   data$Production<-NULL
#   data$Website<-NULL
#   data$Response<-NULL
#   
#   movie_list<-unique(data$Title)
#   
#   for (i in movie_list){
#     temp=data[is.element(data$Title,i),]
#     Ratings.Value_mean<-mean(temp$Ratings.Value)
#     Metascore<-unique(temp$Metascore)
#     imdbRating<-unique(temp$imdbRating)
#     
#     #note: the average_rating column is the average of ratings from  
#     #Internet Movie Database (when applicable)
#     #Metacritic (when applicable)
#     #Rotten Tomatoes (when applicable)
#     #Metascore (when applicable)
#     #imdbRating
#     
#     if(is.na(Metascore)==TRUE){
#       temp$average_rating=(Ratings.Value_mean+imdbRating)/2
#     }
#     if(is.na(Metascore)==FALSE){
#       temp$average_rating=(Ratings.Value_mean+Metascore+imdbRating)/3
#     }
#     mat1=rbind(mat1,temp)
#   }
#   mat1$Ratings.Value <- NULL
#   mat1$Ratings.Source <- NULL
#   mat1 <- unique(mat1)
#   return(mat1)
# }
# 
# data <- format_data(data)
# 
# data$Rated[data$Rated == "N/A"] <- NA
# data <- data %>% select(-c(Metascore,BoxOffice))
# 
# data <- data[complete.cases(data), ]
# 
# A <- data %>% select(-c(Title,Director,Writer,Actors ))
# 
# A <- A[complete.cases(A), ]
# # 
# # set.seed(dim(A)[1])
# # train <- sample(1:nrow(A), size = nrow(A)*0.8)
# # test <- dplyr::setdiff(1:nrow(A), train)
# # ATrain <- A[train, ]
# # ATest <- A[test, ]
# 
# 
# # trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# # dtree_fit <- train(average_rating ~ ., data = ATrain, method = "rpart",
# #                    trControl = trctrl,
# #                    tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
# # pred <- predict(dtree_fit, newdata = dplyr::select(ATest, -average_rating))
# #sqrt(mean((pred-ATest$average_rating)^2))
# 
# 
# library(shiny)
# library(caret)
# library(tidyverse)
# library(DT)
# data("GermanCredit")
# library(shiny)
# library(ggplot2)
# data<-mtcars
# trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# shinyServer(function(input,output)({
#   
#   ## Get the value of the dataset that is selected by user from the list of datasets
#   ## to output the dataset
#   output$dat <- renderPrint({
#     ATrain
#   })
#   
#   output$struct <- renderPrint({
#     str(ATrain)
#   })
#   
#   output$cont_table <- renderPrint({
#     
#     tv <- ATrain %>% select(input$table_variable,input$table_variable2)
#     table(tv)
#   })
#   
#   output$summary_table <- renderPrint({
#     if(input$covariance=="None"){
#       sv <- ATrain %>% select(input$summary_variable)
#       summary(sv)
#     } else if(input$covariance!="None"){
#       A<-ATrain %>% select(input$summary_variable)
#       B<-ATrain %>% select(input$covariance)
#       cov(A,B)
#       
#     }
#     
#   })
#   
#   #if (x%%2 == 0 || x%%5 == 0) {
#   
#   # if(input$table_variable=="Rated"||input$table_variable=="first_Genre"||input$table_variable=="first_Country"||input$table_variable=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable)
#   #   table(tv)
#   # } else if(input$table_variable2=="Rated"||input$table_variable2=="first_Genre"||input$table_variable2=="first_Country"||input$table_variable2=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable,input$table)
#   #   table(tv)
#   #   
#   # }
#   
#   
#   
#   
#   # output$radio_button <- renderPlot({
#   #   
#   #   if(input$radio_button=="Just Classification") {
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count")
#   #   } else if(input$radio_button=="Classification and Unemployed"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #       scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   #   } else if(input$radio_button=="Classification and Foreign"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #       scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   #   }
#   # })
#   # 
#   
#   
#   output$bar_plot <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$bar_plot_variable)) +
#       geom_bar(fill="lightblue") +
#       ggtitle(paste0("Bar Plot: ",input$bar_plot_variable ))
#   })
#   
#   output$scatter_plot <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$scatter_plot_x_variable,y=input$scatter_plot_y_variable,color=input$color_variable)) +
#       geom_point() +
#       ggtitle(paste0("Scatter Plot: ",input$scatter_plot_x_variable, " vs.", input$scatter_plot_y_variable)) +
#       scale_color_discrete(name = input$color_variable)
#   })
#   # output$cont_table <- renderPrint({
#   #   if(input$table=="One Way") {
#   #     tv1 <- ATrain %>% select(Rated)
#   #     table(tv1)
#   #     
#   #     tv2 <- ATrain %>% select(first_Genre)
#   #     table(tv2)
#   #     
#   #     tv3 <- ATrain %>% select(first_Country)
#   #     table(tv3)
#   #     
#   #     tv4 <- ATrain %>% select(Summary_Awards)
#   #     table(tv4)
#   #   } #else if(input$radio_button=="Classification and Unemployed"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #     scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   # } else if(input$radio_button=="Classification and Foreign"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #     scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   # }
#   #})
#   
#   
#   # output$summary <- renderPlot({
#   #   ggplot(ATrain, aes(x = average_rating)) +
#   #   geom_histogram(stat="count")
#   #   #summary(get(input$dataset))
#   # })
#   
#   output$summary <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$hist_graph_variable)) +
#       geom_histogram(color = "blue", fill = "red", size = 2) +
#       ggtitle(paste0("Histogram: Distribution of ",input$hist_graph_variable ))
#   })
#   
#   # output$radio_button <- renderPlot({
#   #        if(input$select_box_1=="Bar Plot") {
#   #          g <- ggplot(data(), aes_string(x = input$sel_bar_plot))
#   #          g + geom_histogram(stat="count")
#   #        }
#   
#   
#   # Pulling the list of variable for choice of variable x
#   output$varx <- renderUI({
#     selectInput("variablex", "select the X variable", choices=names(data()))
#   })
#   
#   # Pulling the list of variable for choice of variable y
#   output$vary <- renderUI({
#     selectInput("variabley", "select the Y variable", choices=names(data()))
#   })
#   
#   #Creates a new reactive values
#   # observe({
#   #   vals <- reactiveValues(intrain = createDataPartition(data$average_rating, p= input$split, list = FALSE),
#   #                          training = data[intrain,],
#   #                          testing = data[-intrain,])
#   # })
#   
#   vals <- reactiveValues(intrain = NULL,
#                          training = NULL,
#                          testing = NULL)
#   observeEvent(input$split,{
#     vals$intrain = createDataPartition(A$average_rating, p=input$split, list = FALSE)
#     vals$training = A[vals$intrain,]
#     vals$testing = A[-vals$intrain,]
#   })
#   
#   output$table <- DT::renderDataTable({
#     data<-datatable(vals$training)
#     data
#   })
#   # output$table2 <- DT::renderDataTable({
#   #   data2<-datatable(vals$testing)
#   #   data2
#   # })
#   
#   f <- reactive({
#     as.formula(paste(input$tree_response, "~."))
#   })
#   e <- reactive({
#     as.formula(paste("-", input$tree_response))
#   })
#   # Linear_Model <- reactive({
#   #   lm(f(), data = trainingData())
#   # })
#   
#   # output$tree_RMSE <- renderText({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                     trControl = trctrl,
#   #                     tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -e()))
#   #   paste0("The RMSE is ", sqrt(mean((pred-vals$testing$e())^2)))
#   # })
#   # output$tree_plot <- renderPlot({
#   #   rpart.plot(dtree_fit$finalModel)
#   # })
#   # output$tree_plot <- renderPlot({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                      trControl = trctrl,
#   #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   rpart.plot(dtree_fit$finalModel)
#   # })
#   # 
#   # output$tree_RMSE <- renderText({
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing))
#   #   paste0("The RMSE is ", sqrt(mean((pred-vals$testing$Year)^2)))
#   # })
#   # 
#   
#   obs <- reactive({
#     vals$testing %>% select(input$tree_response)
#   })
#   
#   output$tree_RMSE <- renderText({
#     dtree_fit <- train(f(), data = vals$training, method = "rpart",
#                        trControl = trctrl,
#                        tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#     pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -input$tree_response))
#     paste0("The RMSE is ", sqrt(mean((pred-(vals$testing$average_rating))^2)))
#   })
#   
#   # output$tree_RMSE <- renderText({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                      trControl = trctrl,
#   #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -input$tree_response))
#   #   paste0("The RMSE is ", sqrt(mean((pred-dplyr::select(vals$testing, input$tree_response))^2)))
#   # })
#   
#   
#   # output$tree_RMSE <- renderText({
#   #   dtree_fit <- train(f(), data = vals$training, method = "rpart",
#   #                      trControl = trctrl,
#   #                      tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#   #   
#   #   
#   #   
#   #   pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -input$tree_response),type = "vector")
#   #   paste0("The RMSE is ", sqrt(mean((pred-vals$testing$Year)^2)))
#   # })
#   
#   
#   #paste0("The RMSE is ", tree_RMSE_value)
#   
#   #paste0("The RMSE is ", sqrt(mean((pred-obs())^2)))
#   
#   # tree_RMSE_value <- RMSE(
#   #   pred = pred,
#   #   obs = cs_test$Sales
#   # )
#   
#   
#   output$tree_plot <- renderPlot({
#     dtree_fit <- train(f(), data = vals$training, method = "rpart",
#                        trControl = trctrl,
#                        tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#     rpart.plot(dtree_fit$finalModel)
#   })
#   
#   
#   
#   
#   
#   #   set.seed = set.seed(dim(data)[1]))
#   # 
#   # output$split_data <- renderPrint({
#   #   vals$set.seed
#   #   data <- data %>% select(input$response)
#   #   intrain <- createDataPartition(data, p= input$split, list = FALSE)
#   #   training <- data[intrain,]
#   #   testing <- data[-intrain,]
#   # })
#   
#   # output$split_data <- renderPrint({
#   # set.seed(dim(data)[1])
#   # train <- sample(1:nrow(data), size = nrow(data)*0.8)
#   # test <- dplyr::setdiff(1:nrow(data), train)
#   # Train <- data[train, ]
#   # Test <- data[test, ]
#   # 
#   # })
#   
#   
#   # to output the structure of the dataset
#   
#   
#   # for summary
#   
#   
#   # For plot
#   output$plot <- renderPlot({
#     ggplot(data(),aes_string(x=input$variablex, y=input$variabley)) +
#       geom_point()
#   })
#   
# }))























# 
# library(httr) #this package will help use use the URL we built to get information from the OMDb API
# library(jsonlite) #this package will help us convert the data we get from the OMDb API to a more usable format
# library(tidyverse) #this package will help us work with our nicely formatted data.
# library(lubridate) #this package will help us create dates 
# library(ggplot2) #this package will help us make graphs
# library(readr)
# library(class)
# library(caret)
# library(DT)
# library(tree)
# library(readxl)
# library(readr)
# library(rpart.plot)
# 
# data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!
# 
# #this will help us convert the Ratings.Value column to numeric
# parse_number <- function(S){
#   if(grepl("/", S)){
#     A<-str_split(S, "/")
#     A<-as.numeric(unlist(A))
#     A<-A[[1]]/A[[2]]
#     A<-A*100
#   } else {
#     A<-as.numeric(gsub("%","",S))
#   }
#   return(A)
# }
# 
# #this will help us make the Summary_Awards column:
# award <- function(S){
#   if(is.na(S)){
#     A<-"none"
#     return(A)
#   }
#   S=tolower(S)
#   #won and nominated:
#   if((grepl("won", S) | grepl("win", S)) && (grepl("nomina", S))){
#     A<-"won and nominated"
#   }
#   #only won:
#   else if (grepl("won", S) | grepl("win", S)){
#     A<-"won"
#   }
#   #only nominated:
#   else if (grepl("nomina", S)){
#     A<-"nomination"
#   }
#   #no awards or nominations:
#   else {(A<-"none")
#     return(A)
#   }
# }
# 
# mat1=NULL
# format_data <- function(data){
#   data$nchar_Title <- nchar(data$Title)
#   data$Year <- as.numeric(data$Year)
#   data$Rated <- as.factor(data$Rated)
#   data$Released <- NULL
#   data$Runtime <- as.numeric(gsub(" min","",data$Runtime))
#   
#   df <- data.frame(x = data$Genre)
#   A<-df %>% separate(x, c('Genre_1'))
#   data$first_Genre <- as.factor(A$Genre_1)
#   data$Genre = NULL
#   
#   df <- data.frame(x = data$Country)
#   A<-df %>% separate(x, c('Country_1'),sep=",")
#   data$first_Country <- A$Country_1
#   data$first_Country <- gsub("USA","United States",data$first_Country)
#   data$first_Country <- as.factor(data$first_Country)
#   data$Country = NULL
#   
#   data$Ratings.Value <- sapply(data$Ratings.Value, FUN=parse_number)
#   data$Summary_Awards <- as.factor(sapply(data$Awards, FUN=award))
#   data$Metascore <- as.numeric(data$Metascore)
#   data$imdbRating <- as.numeric(data$imdbRating)*10
#   data$imdbVotes <- as.numeric(gsub(",","",data$imdbVotes))
#   data$DVD <- NULL
#   data$BoxOffice <- gsub("\\$","",data$BoxOffice)
#   data$BoxOffice <- as.numeric(gsub(",","",data$BoxOffice))
#   
#   data$Plot <- NULL
#   data$Language <- NULL
#   data$Awards<-NULL
#   data$Poster<-NULL
#   data$imdbID<-NULL
#   data$Type<-NULL
#   data$Production<-NULL
#   data$Website<-NULL
#   data$Response<-NULL
#   
#   movie_list<-unique(data$Title)
#   
#   for (i in movie_list){
#     temp=data[is.element(data$Title,i),]
#     Ratings.Value_mean<-mean(temp$Ratings.Value)
#     Metascore<-unique(temp$Metascore)
#     imdbRating<-unique(temp$imdbRating)
#     
#     #note: the average_rating column is the average of ratings from  
#     #Internet Movie Database (when applicable)
#     #Metacritic (when applicable)
#     #Rotten Tomatoes (when applicable)
#     #Metascore (when applicable)
#     #imdbRating
#     
#     if(is.na(Metascore)==TRUE){
#       temp$average_rating=(Ratings.Value_mean+imdbRating)/2
#     }
#     if(is.na(Metascore)==FALSE){
#       temp$average_rating=(Ratings.Value_mean+Metascore+imdbRating)/3
#     }
#     mat1=rbind(mat1,temp)
#   }
#   mat1$Ratings.Value <- NULL
#   mat1$Ratings.Source <- NULL
#   mat1 <- unique(mat1)
#   return(mat1)
# }
# 
# data <- format_data(data)
# 
# data <- data[complete.cases(data), ]
# 
# A <- data %>% select(-c(Title,Director,Writer,Actors ))
# 
# A <- A[complete.cases(A), ]
# # 
# # set.seed(dim(A)[1])
# # train <- sample(1:nrow(A), size = nrow(A)*0.8)
# # test <- dplyr::setdiff(1:nrow(A), train)
# # ATrain <- A[train, ]
# # ATest <- A[test, ]
# 
# 
# # trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# # dtree_fit <- train(average_rating ~ ., data = ATrain, method = "rpart",
# #                    trControl = trctrl,
# #                    tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
# # pred <- predict(dtree_fit, newdata = dplyr::select(ATest, -average_rating))
# #sqrt(mean((pred-ATest$average_rating)^2))
# 
# 
# library(shiny)
# library(caret)
# library(tidyverse)
# library(DT)
# data("GermanCredit")
# library(shiny)
# library(ggplot2)
# data<-mtcars
# trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# shinyServer(function(input,output)({
#   
#   ## Get the value of the dataset that is selected by user from the list of datasets
#   ## to output the dataset
#   output$dat <- renderPrint({
#     ATrain
#   })
#   
#   output$struct <- renderPrint({
#     str(ATrain)
#   })
#   
#   output$cont_table <- renderPrint({
#     
#     tv <- ATrain %>% select(input$table_variable,input$table_variable2)
#     table(tv)
#   })
#   
#   output$summary_table <- renderPrint({
#     if(input$covariance=="None"){
#       sv <- ATrain %>% select(input$summary_variable)
#       summary(sv)
#     } else if(input$covariance!="None"){
#       A<-ATrain %>% select(input$summary_variable)
#       B<-ATrain %>% select(input$covariance)
#       cov(A,B)
#       
#     }
#     
#   })
#   
#   #if (x%%2 == 0 || x%%5 == 0) {
#   
#   # if(input$table_variable=="Rated"||input$table_variable=="first_Genre"||input$table_variable=="first_Country"||input$table_variable=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable)
#   #   table(tv)
#   # } else if(input$table_variable2=="Rated"||input$table_variable2=="first_Genre"||input$table_variable2=="first_Country"||input$table_variable2=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable,input$table)
#   #   table(tv)
#   #   
#   # }
#   
#   
#   
#   
#   # output$radio_button <- renderPlot({
#   #   
#   #   if(input$radio_button=="Just Classification") {
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count")
#   #   } else if(input$radio_button=="Classification and Unemployed"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #       scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   #   } else if(input$radio_button=="Classification and Foreign"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #       scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   #   }
#   # })
#   # 
#   
#   
#   output$bar_plot <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$bar_plot_variable)) +
#       geom_bar(fill="lightblue") +
#       ggtitle(paste0("Bar Plot: ",input$bar_plot_variable ))
#   })
#   
#   output$scatter_plot <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$scatter_plot_x_variable,y=input$scatter_plot_y_variable,color=input$color_variable)) +
#       geom_point() +
#       ggtitle(paste0("Scatter Plot: ",input$scatter_plot_x_variable, " vs.", input$scatter_plot_y_variable)) +
#       scale_color_discrete(name = input$color_variable)
#   })
#   # output$cont_table <- renderPrint({
#   #   if(input$table=="One Way") {
#   #     tv1 <- ATrain %>% select(Rated)
#   #     table(tv1)
#   #     
#   #     tv2 <- ATrain %>% select(first_Genre)
#   #     table(tv2)
#   #     
#   #     tv3 <- ATrain %>% select(first_Country)
#   #     table(tv3)
#   #     
#   #     tv4 <- ATrain %>% select(Summary_Awards)
#   #     table(tv4)
#   #   } #else if(input$radio_button=="Classification and Unemployed"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #     scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   # } else if(input$radio_button=="Classification and Foreign"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #     scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   # }
#   #})
#   
#   
#   # output$summary <- renderPlot({
#   #   ggplot(ATrain, aes(x = average_rating)) +
#   #   geom_histogram(stat="count")
#   #   #summary(get(input$dataset))
#   # })
#   
#   output$summary <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$hist_graph_variable)) +
#       geom_histogram(color = "blue", fill = "red", size = 2) +
#       ggtitle(paste0("Histogram: Distribution of ",input$hist_graph_variable ))
#   })
#   
#   # output$radio_button <- renderPlot({
#   #        if(input$select_box_1=="Bar Plot") {
#   #          g <- ggplot(data(), aes_string(x = input$sel_bar_plot))
#   #          g + geom_histogram(stat="count")
#   #        }
#   
#   
#   # Pulling the list of variable for choice of variable x
#   output$varx <- renderUI({
#     selectInput("variablex", "select the X variable", choices=names(data()))
#   })
#   
#   # Pulling the list of variable for choice of variable y
#   output$vary <- renderUI({
#     selectInput("variabley", "select the Y variable", choices=names(data()))
#   })
#   
#   #Creates a new reactive values
#   # observe({
#   #   vals <- reactiveValues(intrain = createDataPartition(data$average_rating, p= input$split, list = FALSE),
#   #                          training = data[intrain,],
#   #                          testing = data[-intrain,])
#   # })
#   
#   vals <- reactiveValues(intrain = NULL,
#                          training = NULL,
#                          testing = NULL)
#   observeEvent(input$split,{
#     vals$intrain = createDataPartition(A$average_rating, p=input$split, list = FALSE)
#     vals$training = A[vals$intrain,]
#     vals$testing = A[-vals$intrain,]
#   })
#   
#   output$table <- DT::renderDataTable({
#     data<-datatable(vals$training)
#     data
#   })
#   # output$table2 <- DT::renderDataTable({
#   #   data2<-datatable(vals$testing)
#   #   data2
#   # })
#   
#   output$tree_RMSE <- renderText({
#     dtree_fit <- train(average_rating ~ ., data = vals$training, method = "rpart",
#                        trControl = trctrl,
#                        tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
#     pred <- predict(dtree_fit, newdata = dplyr::select(vals$testing, -average_rating))
#     paste0("The RMSE is ", sqrt(mean((pred-vals$testing$average_rating)^2)))
#   })
#   output$tree_plot <- renderPlot({
#     rpart.plot(dtree_fit$finalModel)
#   })
#   
#   
#   
#   
#   
#   
#   #   set.seed = set.seed(dim(data)[1]))
#   # 
#   # output$split_data <- renderPrint({
#   #   vals$set.seed
#   #   data <- data %>% select(input$response)
#   #   intrain <- createDataPartition(data, p= input$split, list = FALSE)
#   #   training <- data[intrain,]
#   #   testing <- data[-intrain,]
#   # })
#   
#   # output$split_data <- renderPrint({
#   # set.seed(dim(data)[1])
#   # train <- sample(1:nrow(data), size = nrow(data)*0.8)
#   # test <- dplyr::setdiff(1:nrow(data), train)
#   # Train <- data[train, ]
#   # Test <- data[test, ]
#   # 
#   # })
#   
#   
#   # to output the structure of the dataset
#   
#   
#   # for summary
#   
#   
#   # For plot
#   output$plot <- renderPlot({
#     ggplot(data(),aes_string(x=input$variablex, y=input$variabley)) +
#       geom_point()
#   })
#   
# }))























# library(httr) #this package will help use use the URL we built to get information from the OMDb API
# library(jsonlite) #this package will help us convert the data we get from the OMDb API to a more usable format
# library(tidyverse) #this package will help us work with our nicely formatted data.
# library(lubridate) #this package will help us create dates 
# library(ggplot2) #this package will help us make graphs
# library(readr)
# library(class)
# library(caret)
# library(DT)
# library(tree)
# library(readxl)
# library(readr)
# library(rpart.plot)
# 
# data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!
# 
# #this will help us convert the Ratings.Value column to numeric
# parse_number <- function(S){
#   if(grepl("/", S)){
#     A<-str_split(S, "/")
#     A<-as.numeric(unlist(A))
#     A<-A[[1]]/A[[2]]
#     A<-A*100
#   } else {
#     A<-as.numeric(gsub("%","",S))
#   }
#   return(A)
# }
# 
# #this will help us make the Summary_Awards column:
# award <- function(S){
#   if(is.na(S)){
#     A<-"none"
#     return(A)
#   }
#   S=tolower(S)
#   #won and nominated:
#   if((grepl("won", S) | grepl("win", S)) && (grepl("nomina", S))){
#     A<-"won and nominated"
#   }
#   #only won:
#   else if (grepl("won", S) | grepl("win", S)){
#     A<-"won"
#   }
#   #only nominated:
#   else if (grepl("nomina", S)){
#     A<-"nomination"
#   }
#   #no awards or nominations:
#   else {(A<-"none")
#     return(A)
#   }
# }
# 
# mat1=NULL
# format_data <- function(data){
#   data$nchar_Title <- nchar(data$Title)
#   data$Year <- as.numeric(data$Year)
#   data$Rated <- as.factor(data$Rated)
#   data$Released <- NULL
#   data$Runtime <- as.numeric(gsub(" min","",data$Runtime))
#   
#   df <- data.frame(x = data$Genre)
#   A<-df %>% separate(x, c('Genre_1'))
#   data$first_Genre <- as.factor(A$Genre_1)
#   data$Genre = NULL
#   
#   df <- data.frame(x = data$Country)
#   A<-df %>% separate(x, c('Country_1'),sep=",")
#   data$first_Country <- A$Country_1
#   data$first_Country <- gsub("USA","United States",data$first_Country)
#   data$first_Country <- as.factor(data$first_Country)
#   data$Country = NULL
#   
#   data$Ratings.Value <- sapply(data$Ratings.Value, FUN=parse_number)
#   data$Summary_Awards <- as.factor(sapply(data$Awards, FUN=award))
#   data$Metascore <- as.numeric(data$Metascore)
#   data$imdbRating <- as.numeric(data$imdbRating)*10
#   data$imdbVotes <- as.numeric(gsub(",","",data$imdbVotes))
#   data$DVD <- NULL
#   data$BoxOffice <- gsub("\\$","",data$BoxOffice)
#   data$BoxOffice <- as.numeric(gsub(",","",data$BoxOffice))
#   
#   data$Plot <- NULL
#   data$Language <- NULL
#   data$Awards<-NULL
#   data$Poster<-NULL
#   data$imdbID<-NULL
#   data$Type<-NULL
#   data$Production<-NULL
#   data$Website<-NULL
#   data$Response<-NULL
#   
#   movie_list<-unique(data$Title)
#   
#   for (i in movie_list){
#     temp=data[is.element(data$Title,i),]
#     Ratings.Value_mean<-mean(temp$Ratings.Value)
#     Metascore<-unique(temp$Metascore)
#     imdbRating<-unique(temp$imdbRating)
#     
#     #note: the average_rating column is the average of ratings from  
#     #Internet Movie Database (when applicable)
#     #Metacritic (when applicable)
#     #Rotten Tomatoes (when applicable)
#     #Metascore (when applicable)
#     #imdbRating
#     
#     if(is.na(Metascore)==TRUE){
#       temp$average_rating=(Ratings.Value_mean+imdbRating)/2
#     }
#     if(is.na(Metascore)==FALSE){
#       temp$average_rating=(Ratings.Value_mean+Metascore+imdbRating)/3
#     }
#     mat1=rbind(mat1,temp)
#   }
#   mat1$Ratings.Value <- NULL
#   mat1$Ratings.Source <- NULL
#   mat1 <- unique(mat1)
#   return(mat1)
# }
# 
# data <- format_data(data)
# 
# data <- data[complete.cases(data), ]
# 
# A <- data %>% select(-c(Title,Director,Writer,Actors ))
# 
# A <- A[complete.cases(A), ]
# 
# set.seed(dim(A)[1])
# train <- sample(1:nrow(A), size = nrow(A)*0.8)
# test <- dplyr::setdiff(1:nrow(A), train)
# ATrain <- A[train, ]
# ATest <- A[test, ]
# 
# 
# # trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# # dtree_fit <- train(average_rating ~ ., data = ATrain, method = "rpart",
# #                    trControl = trctrl,
# #                    tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
# # pred <- predict(dtree_fit, newdata = dplyr::select(ATest, -average_rating))
# #sqrt(mean((pred-ATest$average_rating)^2))
# 
# 
# library(shiny)
# library(caret)
# library(tidyverse)
# library(DT)
# data("GermanCredit")
# library(shiny)
# library(ggplot2)
# data<-mtcars
# shinyServer(function(input,output)({
#   
#   ## Get the value of the dataset that is selected by user from the list of datasets
#   ## to output the dataset
#   output$dat <- renderPrint({
#     ATrain
#   })
#   
#   output$struct <- renderPrint({
#     str(ATrain)
#   })
#   
#   output$cont_table <- renderPrint({
#     
#     tv <- ATrain %>% select(input$table_variable,input$table_variable2)
#     table(tv)
#   })
#   
#   output$summary_table <- renderPrint({
#     if(input$covariance=="None"){
#       sv <- ATrain %>% select(input$summary_variable)
#       summary(sv)
#     } else if(input$covariance!="None"){
#       A<-ATrain %>% select(input$summary_variable)
#       B<-ATrain %>% select(input$covariance)
#       cov(A,B)
#       
#     }
#     
#   })
#   
#   #if (x%%2 == 0 || x%%5 == 0) {
#   
#   # if(input$table_variable=="Rated"||input$table_variable=="first_Genre"||input$table_variable=="first_Country"||input$table_variable=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable)
#   #   table(tv)
#   # } else if(input$table_variable2=="Rated"||input$table_variable2=="first_Genre"||input$table_variable2=="first_Country"||input$table_variable2=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable,input$table)
#   #   table(tv)
#   #   
#   # }
#   
#   
#   
#   
#   # output$radio_button <- renderPlot({
#   #   
#   #   if(input$radio_button=="Just Classification") {
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count")
#   #   } else if(input$radio_button=="Classification and Unemployed"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #       scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   #   } else if(input$radio_button=="Classification and Foreign"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #       scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   #   }
#   # })
#   # 
#   
#   
#   output$bar_plot <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$bar_plot_variable)) +
#       geom_bar(fill="lightblue") +
#       ggtitle(paste0("Bar Plot: ",input$bar_plot_variable ))
#   })
#   
#   output$scatter_plot <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$scatter_plot_x_variable,y=input$scatter_plot_y_variable,color=input$color_variable)) +
#       geom_point() +
#       ggtitle(paste0("Scatter Plot: ",input$scatter_plot_x_variable, " vs.", input$scatter_plot_y_variable)) +
#       scale_color_discrete(name = input$color_variable)
#   })
#   # output$cont_table <- renderPrint({
#   #   if(input$table=="One Way") {
#   #     tv1 <- ATrain %>% select(Rated)
#   #     table(tv1)
#   #     
#   #     tv2 <- ATrain %>% select(first_Genre)
#   #     table(tv2)
#   #     
#   #     tv3 <- ATrain %>% select(first_Country)
#   #     table(tv3)
#   #     
#   #     tv4 <- ATrain %>% select(Summary_Awards)
#   #     table(tv4)
#   #   } #else if(input$radio_button=="Classification and Unemployed"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #     scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   # } else if(input$radio_button=="Classification and Foreign"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #     scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   # }
#   #})
#   
#   
#   # output$summary <- renderPlot({
#   #   ggplot(ATrain, aes(x = average_rating)) +
#   #   geom_histogram(stat="count")
#   #   #summary(get(input$dataset))
#   # })
#   
#   output$summary <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$hist_graph_variable)) +
#       geom_histogram(color = "blue", fill = "red", size = 2) +
#       ggtitle(paste0("Histogram: Distribution of ",input$hist_graph_variable ))
#   })
#   
#   # output$radio_button <- renderPlot({
#   #        if(input$select_box_1=="Bar Plot") {
#   #          g <- ggplot(data(), aes_string(x = input$sel_bar_plot))
#   #          g + geom_histogram(stat="count")
#   #        }
#   
#   
#   # Pulling the list of variable for choice of variable x
#   output$varx <- renderUI({
#     selectInput("variablex", "select the X variable", choices=names(data()))
#   })
#   
#   # Pulling the list of variable for choice of variable y
#   output$vary <- renderUI({
#     selectInput("variabley", "select the Y variable", choices=names(data()))
#   })
#   
#   #Creates a new reactive values
#   # observe({
#   #   vals <- reactiveValues(intrain = createDataPartition(data$average_rating, p= input$split, list = FALSE),
#   #                          training = data[intrain,],
#   #                          testing = data[-intrain,])
#   # })
#   
#   
#   
#   
#   #   set.seed = set.seed(dim(data)[1]))
#   # 
#   # output$split_data <- renderPrint({
#   #   vals$set.seed
#   #   data <- data %>% select(input$response)
#   #   intrain <- createDataPartition(data, p= input$split, list = FALSE)
#   #   training <- data[intrain,]
#   #   testing <- data[-intrain,]
#   # })
#   
#   # output$split_data <- renderPrint({
#   # set.seed(dim(data)[1])
#   # train <- sample(1:nrow(data), size = nrow(data)*0.8)
#   # test <- dplyr::setdiff(1:nrow(data), train)
#   # Train <- data[train, ]
#   # Test <- data[test, ]
#   # 
#   # })
#   
#   
#   # to output the structure of the dataset
#   
#   
#   # for summary
#   
#   
#   # For plot
#   output$plot <- renderPlot({
#     ggplot(data(),aes_string(x=input$variablex, y=input$variabley)) +
#       geom_point()
#   })
#   
# }))






























# library(httr) #this package will help use use the URL we built to get information from the OMDb API
# library(jsonlite) #this package will help us convert the data we get from the OMDb API to a more usable format
# library(tidyverse) #this package will help us work with our nicely formatted data.
# library(lubridate) #this package will help us create dates 
# library(ggplot2) #this package will help us make graphs
# library(readr)
# library(class)
# library(caret)
# library(DT)
# library(tree)
# library(readxl)
# library(readr)
# library(rpart.plot)
# 
# data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!
# 
# #this will help us convert the Ratings.Value column to numeric
# parse_number <- function(S){
#   if(grepl("/", S)){
#     A<-str_split(S, "/")
#     A<-as.numeric(unlist(A))
#     A<-A[[1]]/A[[2]]
#     A<-A*100
#   } else {
#     A<-as.numeric(gsub("%","",S))
#   }
#   return(A)
# }
# 
# #this will help us make the Summary_Awards column:
# award <- function(S){
#   if(is.na(S)){
#     A<-"none"
#     return(A)
#   }
#   S=tolower(S)
#   #won and nominated:
#   if((grepl("won", S) | grepl("win", S)) && (grepl("nomina", S))){
#     A<-"won and nominated"
#   }
#   #only won:
#   else if (grepl("won", S) | grepl("win", S)){
#     A<-"won"
#   }
#   #only nominated:
#   else if (grepl("nomina", S)){
#     A<-"nomination"
#   }
#   #no awards or nominations:
#   else {(A<-"none")
#     return(A)
#   }
# }
# 
# mat1=NULL
# format_data <- function(data){
#   data$nchar_Title <- nchar(data$Title)
#   data$Year <- as.numeric(data$Year)
#   data$Rated <- as.factor(data$Rated)
#   data$Released <- NULL
#   data$Runtime <- as.numeric(gsub(" min","",data$Runtime))
#   
#   df <- data.frame(x = data$Genre)
#   A<-df %>% separate(x, c('Genre_1'))
#   data$first_Genre <- as.factor(A$Genre_1)
#   data$Genre = NULL
#   
#   df <- data.frame(x = data$Country)
#   A<-df %>% separate(x, c('Country_1'),sep=",")
#   data$first_Country <- A$Country_1
#   data$first_Country <- gsub("USA","United States",data$first_Country)
#   data$first_Country <- as.factor(data$first_Country)
#   data$Country = NULL
#   
#   data$Ratings.Value <- sapply(data$Ratings.Value, FUN=parse_number)
#   data$Summary_Awards <- as.factor(sapply(data$Awards, FUN=award))
#   data$Metascore <- as.numeric(data$Metascore)
#   data$imdbRating <- as.numeric(data$imdbRating)*10
#   data$imdbVotes <- as.numeric(gsub(",","",data$imdbVotes))
#   data$DVD <- NULL
#   data$BoxOffice <- gsub("\\$","",data$BoxOffice)
#   data$BoxOffice <- as.numeric(gsub(",","",data$BoxOffice))
#   
#   data$Plot <- NULL
#   data$Language <- NULL
#   data$Awards<-NULL
#   data$Poster<-NULL
#   data$imdbID<-NULL
#   data$Type<-NULL
#   data$Production<-NULL
#   data$Website<-NULL
#   data$Response<-NULL
#   
#   movie_list<-unique(data$Title)
#   
#   for (i in movie_list){
#     temp=data[is.element(data$Title,i),]
#     Ratings.Value_mean<-mean(temp$Ratings.Value)
#     Metascore<-unique(temp$Metascore)
#     imdbRating<-unique(temp$imdbRating)
#     
#     #note: the average_rating column is the average of ratings from  
#     #Internet Movie Database (when applicable)
#     #Metacritic (when applicable)
#     #Rotten Tomatoes (when applicable)
#     #Metascore (when applicable)
#     #imdbRating
#     
#     if(is.na(Metascore)==TRUE){
#       temp$average_rating=(Ratings.Value_mean+imdbRating)/2
#     }
#     if(is.na(Metascore)==FALSE){
#       temp$average_rating=(Ratings.Value_mean+Metascore+imdbRating)/3
#     }
#     mat1=rbind(mat1,temp)
#   }
#   mat1$Ratings.Value <- NULL
#   mat1$Ratings.Source <- NULL
#   mat1 <- unique(mat1)
#   return(mat1)
# }
# 
# data <- format_data(data)
# 
# A <- data %>% select(-c(Title,Director,Writer,Actors ))
# 
# A <- A[complete.cases(A), ]
# 
# set.seed(dim(A)[1])
# train <- sample(1:nrow(A), size = nrow(A)*0.8)
# test <- dplyr::setdiff(1:nrow(A), train)
# ATrain <- A[train, ]
# ATest <- A[test, ]
# 
# 
# # trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# # dtree_fit <- train(average_rating ~ ., data = ATrain, method = "rpart",
# #                    trControl = trctrl,
# #                    tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
# # pred <- predict(dtree_fit, newdata = dplyr::select(ATest, -average_rating))
# #sqrt(mean((pred-ATest$average_rating)^2))
# 
# 
# library(shiny)
# library(caret)
# library(tidyverse)
# library(DT)
# data("GermanCredit")
# library(shiny)
# library(ggplot2)
# data<-mtcars
# shinyServer(function(input,output)({
#   
#   ## Get the value of the dataset that is selected by user from the list of datasets
#   ## to output the dataset
#   output$dat <- renderPrint({
#     ATrain
#   })
#   
#   output$struct <- renderPrint({
#     str(ATrain)
#   })
#   
#   output$cont_table <- renderPrint({
#     
#     tv <- ATrain %>% select(input$table_variable,input$table_variable2)
#     table(tv)
#   })
#   
#   output$summary_table <- renderPrint({
#     if(input$covariance=="None"){
#       sv <- ATrain %>% select(input$summary_variable)
#       summary(sv)
#     } else if(input$covariance!="None"){
#       A<-ATrain %>% select(input$summary_variable)
#       B<-ATrain %>% select(input$covariance)
#       cov(A,B)
#       
#     }
#     
#   })
#   
#   #if (x%%2 == 0 || x%%5 == 0) {
#   
#   # if(input$table_variable=="Rated"||input$table_variable=="first_Genre"||input$table_variable=="first_Country"||input$table_variable=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable)
#   #   table(tv)
#   # } else if(input$table_variable2=="Rated"||input$table_variable2=="first_Genre"||input$table_variable2=="first_Country"||input$table_variable2=="Summary_Awards"){
#   #   tv <- ATrain %>% select(input$table_variable,input$table)
#   #   table(tv)
#   #   
#   # }
#   
#   
#   
#   
#   # output$radio_button <- renderPlot({
#   #   
#   #   if(input$radio_button=="Just Classification") {
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count")
#   #   } else if(input$radio_button=="Classification and Unemployed"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #       scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   #   } else if(input$radio_button=="Classification and Foreign"){
#   #     g <- ggplot(GermanCredit, aes(x = Class))
#   #     g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #       scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   #   }
#   # })
#   # 
#   
#   
#   output$bar_plot <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$bar_plot_variable)) +
#       geom_bar(fill="lightblue") +
#       ggtitle(paste0("Bar Plot: ",input$bar_plot_variable ))
#   })
#   
#   output$scatter_plot <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$scatter_plot_x_variable,y=input$scatter_plot_y_variable,color=input$color_variable)) +
#       geom_point() +
#       ggtitle(paste0("Scatter Plot: ",input$scatter_plot_x_variable, " vs.", input$scatter_plot_y_variable)) +
#       scale_color_discrete(name = input$color_variable)
#   })
#   # output$cont_table <- renderPrint({
#   #   if(input$table=="One Way") {
#   #     tv1 <- ATrain %>% select(Rated)
#   #     table(tv1)
#   #     
#   #     tv2 <- ATrain %>% select(first_Genre)
#   #     table(tv2)
#   #     
#   #     tv3 <- ATrain %>% select(first_Country)
#   #     table(tv3)
#   #     
#   #     tv4 <- ATrain %>% select(Summary_Awards)
#   #     table(tv4)
#   #   } #else if(input$radio_button=="Classification and Unemployed"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #     scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   # } else if(input$radio_button=="Classification and Foreign"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #     scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   # }
#   #})
#   
#   
#   # output$summary <- renderPlot({
#   #   ggplot(ATrain, aes(x = average_rating)) +
#   #   geom_histogram(stat="count")
#   #   #summary(get(input$dataset))
#   # })
#   
#   output$summary <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$hist_graph_variable)) +
#       geom_histogram(color = "blue", fill = "red", size = 2) +
#       ggtitle(paste0("Histogram: Distribution of ",input$hist_graph_variable ))
#   })
#   
#   # output$radio_button <- renderPlot({
#   #        if(input$select_box_1=="Bar Plot") {
#   #          g <- ggplot(data(), aes_string(x = input$sel_bar_plot))
#   #          g + geom_histogram(stat="count")
#   #        }
#   
#   
#   # Pulling the list of variable for choice of variable x
#   output$varx <- renderUI({
#     selectInput("variablex", "select the X variable", choices=names(data()))
#   })
#   
#   # Pulling the list of variable for choice of variable y
#   output$vary <- renderUI({
#     selectInput("variabley", "select the Y variable", choices=names(data()))
#   })
#   
#   # to output the structure of the dataset
#   
#   
#   # for summary
#   
#   
#   # For plot
#   # output$plot <- renderPlot({
#   #   ggplot(data(),aes_string(x=input$variablex, y=input$variabley)) +
#   #     geom_point() 
#   # }) 
#   
# }))























# library(httr) #this package will help use use the URL we built to get information from the OMDb API
# library(jsonlite) #this package will help us convert the data we get from the OMDb API to a more usable format
# library(tidyverse) #this package will help us work with our nicely formatted data.
# library(lubridate) #this package will help us create dates 
# library(ggplot2) #this package will help us make graphs
# library(readr)
# library(class)
# library(caret)
# library(DT)
# library(tree)
# library(readxl)
# library(readr)
# library(rpart.plot)
# 
# data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!
# 
# #this will help us convert the Ratings.Value column to numeric
# parse_number <- function(S){
#   if(grepl("/", S)){
#     A<-str_split(S, "/")
#     A<-as.numeric(unlist(A))
#     A<-A[[1]]/A[[2]]
#     A<-A*100
#   } else {
#     A<-as.numeric(gsub("%","",S))
#   }
#   return(A)
# }
# 
# #this will help us make the Summary_Awards column:
# award <- function(S){
#   if(is.na(S)){
#     A<-"none"
#     return(A)
#   }
#   S=tolower(S)
#   #won and nominated:
#   if((grepl("won", S) | grepl("win", S)) && (grepl("nomina", S))){
#     A<-"won and nominated"
#   }
#   #only won:
#   else if (grepl("won", S) | grepl("win", S)){
#     A<-"won"
#   }
#   #only nominated:
#   else if (grepl("nomina", S)){
#     A<-"nomination"
#   }
#   #no awards or nominations:
#   else {(A<-"none")
#     return(A)
#   }
# }
# 
# mat1=NULL
# format_data <- function(data){
#   data$nchar_Title <- nchar(data$Title)
#   data$Year <- as.numeric(data$Year)
#   data$Rated <- as.factor(data$Rated)
#   data$Released <- NULL
#   data$Runtime <- as.numeric(gsub(" min","",data$Runtime))
#   
#   df <- data.frame(x = data$Genre)
#   A<-df %>% separate(x, c('Genre_1'))
#   data$first_Genre <- as.factor(A$Genre_1)
#   data$Genre = NULL
#   
#   df <- data.frame(x = data$Country)
#   A<-df %>% separate(x, c('Country_1'),sep=",")
#   data$first_Country <- A$Country_1
#   data$first_Country <- gsub("USA","United States",data$first_Country)
#   data$first_Country <- as.factor(data$first_Country)
#   data$Country = NULL
#   
#   data$Ratings.Value <- sapply(data$Ratings.Value, FUN=parse_number)
#   data$Summary_Awards <- as.factor(sapply(data$Awards, FUN=award))
#   data$Metascore <- as.numeric(data$Metascore)
#   data$imdbRating <- as.numeric(data$imdbRating)*10
#   data$imdbVotes <- as.numeric(gsub(",","",data$imdbVotes))
#   data$DVD <- NULL
#   data$BoxOffice <- gsub("\\$","",data$BoxOffice)
#   data$BoxOffice <- as.numeric(gsub(",","",data$BoxOffice))
#   
#   data$Plot <- NULL
#   data$Language <- NULL
#   data$Awards<-NULL
#   data$Poster<-NULL
#   data$imdbID<-NULL
#   data$Type<-NULL
#   data$Production<-NULL
#   data$Website<-NULL
#   data$Response<-NULL
#   
#   movie_list<-unique(data$Title)
#   
#   for (i in movie_list){
#     temp=data[is.element(data$Title,i),]
#     Ratings.Value_mean<-mean(temp$Ratings.Value)
#     Metascore<-unique(temp$Metascore)
#     imdbRating<-unique(temp$imdbRating)
#     
#     #note: the average_rating column is the average of ratings from  
#     #Internet Movie Database (when applicable)
#     #Metacritic (when applicable)
#     #Rotten Tomatoes (when applicable)
#     #Metascore (when applicable)
#     #imdbRating
#     
#     if(is.na(Metascore)==TRUE){
#       temp$average_rating=(Ratings.Value_mean+imdbRating)/2
#     }
#     if(is.na(Metascore)==FALSE){
#       temp$average_rating=(Ratings.Value_mean+Metascore+imdbRating)/3
#     }
#     mat1=rbind(mat1,temp)
#   }
#   mat1$Ratings.Value <- NULL
#   mat1$Ratings.Source <- NULL
#   mat1 <- unique(mat1)
#   return(mat1)
# }
# 
# data <- format_data(data)
# 
# A <- data %>% select(-c(Title,Director,Writer,Actors ))
# 
# A <- A[complete.cases(A), ]
# 
# set.seed(dim(A)[1])
# train <- sample(1:nrow(A), size = nrow(A)*0.8)
# test <- dplyr::setdiff(1:nrow(A), train)
# ATrain <- A[train, ]
# ATest <- A[test, ]
# 
# 
# # trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# # dtree_fit <- train(average_rating ~ ., data = ATrain, method = "rpart",
# #                    trControl = trctrl,
# #                    tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
# # pred <- predict(dtree_fit, newdata = dplyr::select(ATest, -average_rating))
# #sqrt(mean((pred-ATest$average_rating)^2))
# 
# 
# library(shiny)
# library(caret)
# library(tidyverse)
# library(DT)
# data("GermanCredit")
# library(shiny)
# library(ggplot2)
# data<-mtcars
# shinyServer(function(input,output)({
#   
#   ## Get the value of the dataset that is selected by user from the list of datasets
#   ## to output the dataset
#   output$dat <- renderPrint({
#     ATrain
#   })
#   
#   output$struct <- renderPrint({
#     str(ATrain)
#   })
#   
#   output$cont_table <- renderPrint({
#     tv <- ATrain %>% select(input$table_variable)
#     table(tv)
#   })
#   
#   output$bar_plot <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$bar_plot_variable)) +
#       geom_bar(fill="lightblue") +
#       ggtitle(paste0("Bar Plot: ",input$bar_plot_variable ))
#   })
#   
#   output$scatter_plot <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$scatter_plot_x_variable,y=input$scatter_plot_y_variable,color=input$color_variable)) +
#       geom_point() +
#       ggtitle(paste0("Scatter Plot: ",input$scatter_plot_x_variable, " vs.", input$scatter_plot_y_variable)) +
#       scale_color_discrete(name = input$color_variable)
#   })
#   # output$cont_table <- renderPrint({
#   #   if(input$table=="One Way") {
#   #     tv1 <- ATrain %>% select(Rated)
#   #     table(tv1)
#   #     
#   #     tv2 <- ATrain %>% select(first_Genre)
#   #     table(tv2)
#   #     
#   #     tv3 <- ATrain %>% select(first_Country)
#   #     table(tv3)
#   #     
#   #     tv4 <- ATrain %>% select(Summary_Awards)
#   #     table(tv4)
#   #   } #else if(input$radio_button=="Classification and Unemployed"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #     scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   # } else if(input$radio_button=="Classification and Foreign"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #     scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   # }
#   #})
#   
#   
#   # output$summary <- renderPlot({
#   #   ggplot(ATrain, aes(x = average_rating)) +
#   #   geom_histogram(stat="count")
#   #   #summary(get(input$dataset))
#   # })
#   
#   output$summary <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$hist_graph_variable)) +
#       geom_histogram(color = "blue", fill = "red", size = 2) +
#       ggtitle(paste0("Histogram: Distribution of ",input$hist_graph_variable ))
#   })
#   
#   # output$radio_button <- renderPlot({
#   #        if(input$select_box_1=="Bar Plot") {
#   #          g <- ggplot(data(), aes_string(x = input$sel_bar_plot))
#   #          g + geom_histogram(stat="count")
#   #        }
#   
#   
#   # Pulling the list of variable for choice of variable x
#   output$varx <- renderUI({
#     selectInput("variablex", "select the X variable", choices=names(data()))
#   })
#   
#   # Pulling the list of variable for choice of variable y
#   output$vary <- renderUI({
#     selectInput("variabley", "select the Y variable", choices=names(data()))
#   })
#   
#   # to output the structure of the dataset
#   
#   
#   # for summary
#   
#   
#   # For plot
#   # output$plot <- renderPlot({
#   #   ggplot(data(),aes_string(x=input$variablex, y=input$variabley)) +
#   #     geom_point() 
#   # }) 
#   
# }))














# library(httr) #this package will help use use the URL we built to get information from the OMDb API
# library(jsonlite) #this package will help us convert the data we get from the OMDb API to a more usable format
# library(tidyverse) #this package will help us work with our nicely formatted data.
# library(lubridate) #this package will help us create dates 
# library(ggplot2) #this package will help us make graphs
# library(readr)
# library(class)
# library(caret)
# library(DT)
# library(tree)
# library(readxl)
# library(readr)
# library(rpart.plot)
# 
# data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!
# 
# #this will help us convert the Ratings.Value column to numeric
# parse_number <- function(S){
#   if(grepl("/", S)){
#     A<-str_split(S, "/")
#     A<-as.numeric(unlist(A))
#     A<-A[[1]]/A[[2]]
#     A<-A*100
#   } else {
#     A<-as.numeric(gsub("%","",S))
#   }
#   return(A)
# }
# 
# #this will help us make the Summary_Awards column:
# award <- function(S){
#   if(is.na(S)){
#     A<-"none"
#     return(A)
#   }
#   S=tolower(S)
#   #won and nominated:
#   if((grepl("won", S) | grepl("win", S)) && (grepl("nomina", S))){
#     A<-"won and nominated"
#   }
#   #only won:
#   else if (grepl("won", S) | grepl("win", S)){
#     A<-"won"
#   }
#   #only nominated:
#   else if (grepl("nomina", S)){
#     A<-"nomination"
#   }
#   #no awards or nominations:
#   else {(A<-"none")
#     return(A)
#   }
# }
# 
# mat1=NULL
# format_data <- function(data){
#   data$nchar_Title <- nchar(data$Title)
#   data$Year <- as.numeric(data$Year)
#   data$Rated <- as.factor(data$Rated)
#   data$Released <- NULL
#   data$Runtime <- as.numeric(gsub(" min","",data$Runtime))
#   
#   df <- data.frame(x = data$Genre)
#   A<-df %>% separate(x, c('Genre_1'))
#   data$first_Genre <- as.factor(A$Genre_1)
#   data$Genre = NULL
#   
#   df <- data.frame(x = data$Country)
#   A<-df %>% separate(x, c('Country_1'),sep=",")
#   data$first_Country <- A$Country_1
#   data$first_Country <- gsub("USA","United States",data$first_Country)
#   data$first_Country <- as.factor(data$first_Country)
#   data$Country = NULL
#   
#   data$Ratings.Value <- sapply(data$Ratings.Value, FUN=parse_number)
#   data$Summary_Awards <- as.factor(sapply(data$Awards, FUN=award))
#   data$Metascore <- as.numeric(data$Metascore)
#   data$imdbRating <- as.numeric(data$imdbRating)*10
#   data$imdbVotes <- as.numeric(gsub(",","",data$imdbVotes))
#   data$DVD <- NULL
#   data$BoxOffice <- gsub("\\$","",data$BoxOffice)
#   data$BoxOffice <- as.numeric(gsub(",","",data$BoxOffice))
#   
#   data$Plot <- NULL
#   data$Language <- NULL
#   data$Awards<-NULL
#   data$Poster<-NULL
#   data$imdbID<-NULL
#   data$Type<-NULL
#   data$Production<-NULL
#   data$Website<-NULL
#   data$Response<-NULL
#   
#   movie_list<-unique(data$Title)
#   
#   for (i in movie_list){
#     temp=data[is.element(data$Title,i),]
#     Ratings.Value_mean<-mean(temp$Ratings.Value)
#     Metascore<-unique(temp$Metascore)
#     imdbRating<-unique(temp$imdbRating)
#     
#     #note: the average_rating column is the average of ratings from  
#     #Internet Movie Database (when applicable)
#     #Metacritic (when applicable)
#     #Rotten Tomatoes (when applicable)
#     #Metascore (when applicable)
#     #imdbRating
#     
#     if(is.na(Metascore)==TRUE){
#       temp$average_rating=(Ratings.Value_mean+imdbRating)/2
#     }
#     if(is.na(Metascore)==FALSE){
#       temp$average_rating=(Ratings.Value_mean+Metascore+imdbRating)/3
#     }
#     mat1=rbind(mat1,temp)
#   }
#   mat1$Ratings.Value <- NULL
#   mat1$Ratings.Source <- NULL
#   mat1 <- unique(mat1)
#   return(mat1)
# }
# 
# data <- format_data(data)
# 
# A <- data %>% select(-c(Title,Director,Writer,Actors ))
# 
# A <- A[complete.cases(A), ]
# 
# set.seed(dim(A)[1])
# train <- sample(1:nrow(A), size = nrow(A)*0.8)
# test <- dplyr::setdiff(1:nrow(A), train)
# ATrain <- A[train, ]
# ATest <- A[test, ]
# 
# 
# # trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# # dtree_fit <- train(average_rating ~ ., data = ATrain, method = "rpart",
# #                    trControl = trctrl,
# #                    tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
# # pred <- predict(dtree_fit, newdata = dplyr::select(ATest, -average_rating))
# #sqrt(mean((pred-ATest$average_rating)^2))
# 
# 
# library(shiny)
# library(caret)
# library(tidyverse)
# library(DT)
# data("GermanCredit")
# library(shiny)
# library(ggplot2)
# data<-mtcars
# shinyServer(function(input,output)({
#   
#   ## Get the value of the dataset that is selected by user from the list of datasets
#   ## to output the dataset
#   output$dat <- renderPrint({
#     ATrain
#   })
#   
#   output$struct <- renderPrint({
#     str(ATrain)
#   })
#   
#   output$cont_table <- renderPrint({
#     tv <- ATrain %>% select(input$table_variable)
#     table(tv)
#   })
#   
#   output$bar_plot <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$bar_plot_variable)) +
#       geom_bar(fill="lightblue") +
#       ggtitle(paste0("Bar Plot: ",input$bar_plot_variable ))
#   })
#   # output$cont_table <- renderPrint({
#   #   if(input$table=="One Way") {
#   #     tv1 <- ATrain %>% select(Rated)
#   #     table(tv1)
#   #     
#   #     tv2 <- ATrain %>% select(first_Genre)
#   #     table(tv2)
#   #     
#   #     tv3 <- ATrain %>% select(first_Country)
#   #     table(tv3)
#   #     
#   #     tv4 <- ATrain %>% select(Summary_Awards)
#   #     table(tv4)
#   #   } #else if(input$radio_button=="Classification and Unemployed"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(EmploymentDuration.Unemployed))) +
#   #     scale_fill_discrete(name="Unemployment status",labels=c("Employed", "Unemployed"))
#   # } else if(input$radio_button=="Classification and Foreign"){
#   #   g <- ggplot(GermanCredit, aes(x = Class))
#   #   g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#   #     scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#   # }
#   #})
#   
#   
#   # output$summary <- renderPlot({
#   #   ggplot(ATrain, aes(x = average_rating)) +
#   #   geom_histogram(stat="count")
#   #   #summary(get(input$dataset))
#   # })
#   
#   output$summary <- renderPlot({
#     ggplot(ATrain,aes_string(x=input$hist_graph_variable)) +
#       geom_histogram(color = "blue", fill = "red", size = 2) +
#       ggtitle(paste0("Histogram: Distribution of ",input$hist_graph_variable ))
#   })
#   
#   # output$radio_button <- renderPlot({
#   #        if(input$select_box_1=="Bar Plot") {
#   #          g <- ggplot(data(), aes_string(x = input$sel_bar_plot))
#   #          g + geom_histogram(stat="count")
#   #        }
#   
#   
#   # Pulling the list of variable for choice of variable x
#   output$varx <- renderUI({
#     selectInput("variablex", "select the X variable", choices=names(data()))
#   })
#   
#   # Pulling the list of variable for choice of variable y
#   output$vary <- renderUI({
#     selectInput("variabley", "select the Y variable", choices=names(data()))
#   })
#   
#   # to output the structure of the dataset
#   
#   
#   # for summary
#   
#   
#   # For plot
#   # output$plot <- renderPlot({
#   #   ggplot(data(),aes_string(x=input$variablex, y=input$variabley)) +
#   #     geom_point() 
#   # }) 
#   
# }))
#
#
#
# shinyServer(function(input, output) {
#   data <- reactive({
#     req(input$sel_bar_plot)
#     df <- ATrain %>% select(input$sel_bar_plot)
#   })
#   
#   output$radio_button <- renderPlot({
#     if(input$select_box_1=="Bar Plot") {
#       g <- ggplot(data(), aes_string(x = input$sel_bar_plot))
#       g + geom_histogram(stat="count")
#     } else if(input$select_box_1=="Classification and Unemployed"){
#       A<-ATrain %>%
#         select(first_Genre,average_rating)
#       g <- ggplot(A, aes(x = first_Genre, y = average_rating, fill = first_Genre))
#       g + geom_boxplot() +
#         coord_flip()
#     } else if(input$select_box_1=="Classification and Foreign"){
#       g <- ggplot(GermanCredit, aes(x = Class))
#       g + geom_histogram(stat="count",position="dodge",aes(fill=factor(ForeignWorker))) +
#         scale_fill_discrete(name="Status",labels=c("German", "Foreign"))
#     }
#   })
#  
#   
#   output$var <- DT::renderDataTable({
#     var <- input$var
#     GermanCreditSub <- GermanCredit[, c("Class", "InstallmentRatePercentage", var),
#                                     drop = FALSE]
#     tab <- aggregate(GermanCreditSub[[var]] ~ Class + InstallmentRatePercentage,
#                      data = GermanCreditSub, FUN = mean)
#     
#     tab <- datatable(tab, colnames=c("Class", "InstallmentRatePercentage", paste0("Average ",var)))
#     formatRound(table=tab,columns = 3, digits = input$numeric_input)
#   })
# })
# 
#
#
# library(shiny)
# 
# # Define server logic required to draw a histogram
# 
# shinyServer(function(input, output) {
#   output$radio_button <- renderPlot({
#     
#     if(input$radio_button=="Histogram: Response Variable") {
#       
#       g <- ggplot(GermanCredit, aes(x = Class))
#       g + geom_histogram(stat="count")
#       
#       # g <- ggplot(ATrain,aes(x = average_rating))
#       # g + geom_histogram(stat="count")
#       
#       
#       
#       # ATrain %>% 
#       #   ggplot(aes(x = average_rating)) + 
#       #   geom_histogram() +
#       #   labs(
#       #     title = paste0("Histogram: Distribution of Average Rating"),
#       #     y="Frequency",
#       #     x="Average Rating")
#       } else if (input$radio_button=="Box Plot: Response Variable"){
#         A<-ATrain %>%
#                  select(first_Genre,average_rating)
#              
#              A %>%
#              ggplot(aes(x = first_Genre, y = average_rating, fill = first_Genre)) +
#                geom_boxplot() +
#                theme(legend.position = "none",axis.text.x = element_text(angle = 90)) +
#                labs(x = "Genre", title = "Average Rating for Each Genre", y="Average Rating") +
#                coord_flip()
#         
#         
#         
#       }
#     
#   })
# })
# 
#     
#   #   } else {(input$radio_button=="Box Plot: Response Variable"){
#   #     
#   #     A<-ATrain %>%
#   #       select(first_Genre,average_rating)
#   #     
#   #     A %>%
#   #       ggplot(aes(x = first_Genre, y = average_rating, fill = first_Genre)) +
#   #       geom_boxplot() +
#   #       theme(legend.position = "none",axis.text.x = element_text(angle = 90)) +
#   #       labs(x = "Genre", title = "Average Rating for Each Genre", y="Average Rating") +
#   #       coord_flip()
#   #   }
#   # })
#   # 
#   
#   
#   # output$histPlot <- renderPlot({
#   # ATrain %>% 
#   #   ggplot(aes(x = average_rating)) + 
#   #   geom_histogram() +
#   #   labs(
#   #     title = paste0("Histogram: Distribution of Average Rating"),
#   #     y="Frequency",
#   #     x="Average Rating") 
#   # })
#   # observe({
#   #   split_data <- reactiveValues(
#   #     seed = set.seed(dim(A)[1]),
#   #     percent = input$split,
#   #     train = sample(1:nrow(A), size = nrow(A)*percent),
#   #     browser(),
#   #     test = dplyr::setdiff(1:nrow(A), train),
#   #     ATrain = A[train, ],
#   #     ATest = A[test, ],
#   #     nrow_ATrain = nrow(ATrain))
#   # })
#   
#      # text = paste0("The number of rows in the training set is ", nrow(split_data$ATrain),"."),
#       
#     # output$info <- renderText({
#     #   paste0("The number of rows in the training set is ", nrow_ATrain,".")
#     # })
#     
#     
#   
#     
#         
#       
#   
#   
#   # output$distPlot <- renderPlot({
#   #   
#   #   # generate bins based on input$bins from ui.R
#   #   #x    <- faithful[, 2]
#   #   output$info <- renderText({
#   #     paste0("The number of rows in the data is ", nrow(ATrain))
#   #     
#   #   })
#   #   
#   #   
#   #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
#   #   
#   #   # draw the histogram with the specified number of bins
#   #   hist(x, breaks = bins, col = 'darkgray', border = 'white',
#   #        xlab = 'Waiting time to next eruption (in mins)',
#   #        main = 'Histogram of waiting times')
#   #   
#   # })
#   
#   
# 
# 
# #shinyServer(function(input, output) {
#   
#   
#   # output$info <- renderText({
#   #       split_data <- reactiveValues(
#   #       seed = set.seed(dim(A)[1]),
#   #       train = sample(1:nrow(A), size = nrow(A)*input$split),
#   #       test = dplyr::setdiff(1:nrow(A), train),
#   #       ATrain = A[train, ],
#   #       ATest = A[test, ],
#   #       nrow_ATrain=nrow(ATrain))
#   #       paste0("The number of rows in the training set is ", split_data$nrow_ATrain)
#   #     )
#         
#           
#           #paste0("The number of bins is ", input$split)
#       
#     
#     
#     
#     # generate bins based on input$bins from ui.R
#     # x    <- faithful[, 2]
#     # bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     # paste0("The number of bins is ", input$split)
#     
#     # draw the histogram with the specified number of bins
#     # hist(x, breaks = bins, col = 'darkgray', border = 'white',
#     #      xlab = 'Waiting time to next eruption (in mins)',
#     #      main = 'Histogram of waiting times')
#     
#  # })
#   
# #})
#   
#   # observe({
#   #   split_data <- reactiveValues(
#   #     seed = set.seed(dim(A)[1]),
#   #     train = sample(1:nrow(A), size = nrow(A)*input$split),
#   #     test = dplyr::setdiff(1:nrow(A), train),
#   #     ATrain = A[train, ],
#   #     ATest = A[test, ],
#   #     nrow_ATrain=nrow(ATrain)
#   #   )
#   #     output$info <- renderText({
#   #       paste0("The number of rows in the training set is ", split_data$nrow_ATrain)
#   #       #paste0("The number of bins is ", input$split)
#   #     })
#   # })
#   
#   # observe({
#   #   vals <- reactiveValues(initial = input$split)
#   #   output$info <- renderText({
#   #     paste0("The number of bins is ", vals$initial)
#   #     #paste0("The number of bins is ", input$split)
#   #   })
#   # })
#   
#   # output$textString <- renderText({
#   #   paste0("The value of initial is ", vals$initial)
#   # })
# 
#     
#     
# 
# 


























#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
# library(httr) #this package will help use use the URL we built to get information from the OMDb API
# library(jsonlite) #this package will help us convert the data we get from the OMDb API to a more usable format
# library(tidyverse) #this package will help us work with our nicely formatted data.
# library(lubridate) #this package will help us create dates 
# library(ggplot2) #this package will help us make graphs
# library(readr)
# library(class)
# library(caret)
# library(DT)
# library(tree)
# library(readxl)
# library(readr)
# library(rpart.plot)
#
# data<-read_xlsx("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//movie_data.xlsx")
# data<-as_tibble(data)
# 
# #this will help us convert the Ratings.Value column to numeric
# parse_number <- function(S){
#   if(grepl("/", S)){
#     A<-str_split(S, "/")
#     A<-as.numeric(unlist(A))
#     A<-A[[1]]/A[[2]]
#     A<-A*100
#   } else {
#     A<-as.numeric(gsub("%","",S))
#   }
#   return(A)
# }
# 
# #this will help us make the Summary_Awards column:
# award <- function(S){
#   if(is.na(S)){
#     A<-"none"
#     return(A)
#   }
#   S=tolower(S)
#   #won and nominated:
#   if((grepl("won", S) | grepl("win", S)) && (grepl("nomina", S))){
#     A<-"won and nominated"
#   }
#   #only won:
#   else if (grepl("won", S) | grepl("win", S)){
#     A<-"won"
#   }
#   #only nominated:
#   else if (grepl("nomina", S)){
#     A<-"nomination"
#   }
#   #no awards or nominations:
#   else {(A<-"none")
#     return(A)
#   }
# }
# 
# mat1=NULL
# format_data <- function(data){
#   data$Year <- as.numeric(data$Year)
#   data$Released <- dmy(data$Released)
#   data$Runtime <- as.numeric(gsub(" min","",data$Runtime))
#   data$Ratings.Value <- sapply(data$Ratings.Value, FUN=parse_number)
#   data$Summary_Awards <- as.factor(sapply(data$Awards, FUN=award))
#   data$Metascore <- as.numeric(data$Metascore)
#   data$imdbRating <- as.numeric(data$imdbRating)*10
#   data$imdbVotes <- as.numeric(gsub(",","",data$imdbVotes))
#   data$DVD <- dmy(data$DVD)
#   data$BoxOffice <- gsub("\\$","",data$BoxOffice)
#   data$BoxOffice <- as.numeric(gsub(",","",data$BoxOffice))
#   movie_list<-unique(data$Title)
#   
#   for (i in movie_list){
#     temp=data[is.element(data$Title,i),]
#     Ratings.Value_mean<-mean(temp$Ratings.Value)
#     Metascore<-unique(temp$Metascore)
#     imdbRating<-unique(temp$imdbRating)
#     
#     #some of the values in the Metascore column have NAs, so these if statements accommodate for this:
#     if(is.na(Metascore)==TRUE){
#       temp$average_rating=(Ratings.Value_mean+imdbRating)/2
#     }
#     if(is.na(Metascore)==FALSE){
#       temp$average_rating=(Ratings.Value_mean+Metascore+imdbRating)/3
#     }
#     mat1=rbind(mat1,temp)
#   }
#   return(mat1)
# }
# #check average ratings for harry potter (make sure it's calculated correctly)
# data<-format_data(data)
# 
# data <- unique(subset(formatted_data, select = -c(Ratings.Source,Ratings.Value,Metascore,imdbRating) ))
# data <- subset(data, select = -c(Title,Released,Plot,Language,Awards,Poster,imdbID,Type,DVD,Production,Website,Response,Director,Writer,Actors) )
# 
# #separate genre
# df <- data.frame(x = data$Genre)
# A<-df %>% separate(x, c('Genre_1', 'Genre_2', 'Genre_3', 'Genre_4'))
# data$first_Genre <- as.factor(A$Genre_1)
# data$Genre = NULL
# 
# #separate country
# df <- data.frame(x = data$Country)
# A<-df %>% separate(x, c('Country_1', 'Country_2', 'Country_3', 'Country_4'),sep=",")
# data$first_Country <- A$Country_1
# data$first_Country <- gsub("USA","United States",data$first_Country)
# data$first_Country <- as.factor(data$first_Country)
# data$Country = NULL
# 
# #separate director 
# 
# data$Rated <- as.factor(data$Rated)
#
#data<-read_csv("movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!
#data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv")
#
#trying full function again:
#
#this will help us convert the Ratings.Value column to numeric
# parse_number <- function(S){
#   if(grepl("/", S)){
#     A<-str_split(S, "/")
#     A<-as.numeric(unlist(A))
#     A<-A[[1]]/A[[2]]
#     A<-A*100
#   } else {
#     A<-as.numeric(gsub("%","",S))
#   }
#   return(A)
# }
#
#this will help us make the Summary_Awards column:
# award <- function(S){
#   if(is.na(S)){
#     A<-"none"
#     return(A)
#   }
#   S=tolower(S)
#   #won and nominated:
#   if((grepl("won", S) | grepl("win", S)) && (grepl("nomina", S))){
#     A<-"won and nominated"
#   }
#   #only won:
#   else if (grepl("won", S) | grepl("win", S)){
#     A<-"won"
#   }
#   #only nominated:
#   else if (grepl("nomina", S)){
#     A<-"nomination"
#   }
#   #no awards or nominations:
#   else {(A<-"none")
#     return(A)
#   }
# }
# 
# mat1=NULL
# format_data <- function(data){
#   data$nchar_Title <- nchar(data$Title)
#   data$Year <- as.numeric(data$Year)
#   data$Rated <- as.factor(data$Rated)
#   data$Released <- NULL
#   #data$Released <- dmy(data$Released)
#   data$Runtime <- as.numeric(gsub(" min","",data$Runtime))
#   
#   df <- data.frame(x = data$Genre)
#   A<-df %>% separate(x, c('Genre_1'))
#   data$first_Genre <- as.factor(A$Genre_1)
#   data$Genre = NULL
#   
#   df <- data.frame(x = data$Country)
#   A<-df %>% separate(x, c('Country_1'),sep=",")
#   data$first_Country <- A$Country_1
#   data$first_Country <- gsub("USA","United States",data$first_Country)
#   data$first_Country <- as.factor(data$first_Country)
#   data$Country = NULL
#   
#   data$Ratings.Value <- sapply(data$Ratings.Value, FUN=parse_number)
#   data$Summary_Awards <- as.factor(sapply(data$Awards, FUN=award))
#   data$Metascore <- as.numeric(data$Metascore)
#   data$imdbRating <- as.numeric(data$imdbRating)*10
#   data$imdbVotes <- as.numeric(gsub(",","",data$imdbVotes))
#   data$DVD <- NULL
#   data$BoxOffice <- gsub("\\$","",data$BoxOffice)
#   data$BoxOffice <- as.numeric(gsub(",","",data$BoxOffice))
#   
#   data$Plot <- NULL
#   data$Language <- NULL
#   data$Awards<-NULL
#   data$Poster<-NULL
#   data$imdbID<-NULL
#   data$Type<-NULL
#   data$Production<-NULL
#   data$Website<-NULL
#   data$Response<-NULL
#   
#   movie_list<-unique(data$Title)
#   
#   for (i in movie_list){
#     temp=data[is.element(data$Title,i),]
#     Ratings.Value_mean<-mean(temp$Ratings.Value)
#     Metascore<-unique(temp$Metascore)
#     imdbRating<-unique(temp$imdbRating)
#     
#     #note: the average_rating column is the average of ratings from  
#     #Internet Movie Database (when applicable)
#     #Metacritic (when applicable)
#     #Rotten Tomatoes (when applicable)
#     #Metascore (when applicable)
#     #imdbRating
#     
#     if(is.na(Metascore)==TRUE){
#       temp$average_rating=(Ratings.Value_mean+imdbRating)/2
#     }
#     if(is.na(Metascore)==FALSE){
#       temp$average_rating=(Ratings.Value_mean+Metascore+imdbRating)/3
#     }
#     mat1=rbind(mat1,temp)
#   }
#   mat1$Ratings.Value <- NULL
#   mat1$Ratings.Source <- NULL
#   mat1 <- unique(mat1)
#   return(mat1)
# }
# 
# data <- format_data(data)
# 
# A <- data %>% select(-c(Title,Director,Writer,Actors ))
# 
# A <- A[complete.cases(A), ]
# 
# set.seed(dim(A)[1])
# train <- sample(1:nrow(A), size = nrow(A)*0.8)
# test <- dplyr::setdiff(1:nrow(A), train)
# ATrain <- A[train, ]
# ATest <- A[test, ]
# 
# 
# trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# dtree_fit <- train(average_rating ~ ., data = ATrain, method = "rpart",
#                    trControl = trctrl,
#                    tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
# pred <- predict(dtree_fit, newdata = dplyr::select(ATest, -average_rating))
# #sqrt(mean((pred-ATest$average_rating)^2))
# 
# 
# library(shiny)
# 
# # Define server logic required to draw a histogram
# shinyServer(function(input, output, session) {
#  
  # getData <- reactive({
  #   newData <- data
  # })
  # 
  # observe({
  #   split_data <- reactiveValues(
  #     train = sample(1:nrow(A), size = nrow(A)*input$split),
  #     test = dplyr::setdiff(1:nrow(A), train),
  #     ATrain = A[train, ],
  #     ATest = A[test, ]
  #   )
  # })
  # 
  # # output$textString <- renderText({
  # #   paste0("The value of initial is ", vals$initial)
  # # })
  # 
  # 
  # output$splited_data <- DT::renderDataTable({
  #   vals$ATrain %>%
  #     datatable() 
  # })
  # 
  # 
  # output$test_table <- DT::renderDataTable({
  #   getData() %>%
  #     datatable() %>%
  #     formatRound(columns=c('average_rating'), digits=2)
  # })
  # 
  # #Creates a new reactive values
  # vals <- reactiveValues(data = rnorm(150), initial = 0)
  # output$textString <- renderText({
  #   paste0("The value of initial is ", vals$initial)
  # })
  # output$hist <- renderPlot({
  #   rpart.plot(dtree_fit$finalModel)
  #   #hist(vals$data)
  # })
  # 
  # 
  # #vals <- reactiveValues(
  #   # A <- getData() %>% select(-c(Title,Director,Writer,Actors )),
  #   # A <- A[complete.cases(A), ],
  #   # set.seed(dim(A)[1]),
  #   # train <- sample(1:nrow(A), size = nrow(A)*0.8),
  #   # test <- dplyr::setdiff(1:nrow(A), train),
  #   # ATrain <- A[train, ],
  #   # ATest <- A[test, ],
  #   # trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3),
  #   # dtree_fit <- train(average_rating ~ ., data = ATrain, method = "rpart",
  #   #                    trControl = trctrl,
  #   #                    tuneGrid = data.frame(cp=seq(0,0.1,0.01))),
  #   #iris_data<-iris
  #   # pred <- predict(dtree_fit, newdata = dplyr::select(ATest, -average_rating))
  #   # sqrt(mean((pred-ATest$average_rating)^2))
  #   # rpart.plot(dtree_fit$finalModel)
  #   
  # #)
  # 
  # # output$treePlot <- renderPlot({
  # #   plot(vals$iris_data)
  # #   #rpart.plot((vals$dtree_fit)[finalModel])
  # # })
  # 
  # # vals <- reactiveValues(data = rnorm(150), initial = 0)
  # # output$textString <- renderText({
  # #   paste0("The value of initial is ", vals$initial)
  # # })
  # 
  # 
  # # use_this_one_data <- reactive({
  # #   movie_data <- data
  # # })
  # # 
  # # output$model_table <- DT::renderDataTable({
  # #   use_this_one_data() %>%
  # #     datatable()
  # # })
  # 
  # 
  # 
  # 
  # # vals <- reactiveValues(
  # #   set.seed(dim(use_this_one_data())[1]),
  # #   train <- sample(1:nrow(use_this_one_data()), size = nrow(use_this_one_data())*0.8),
  # #   test <- dplyr::setdiff(1:nrow(use_this_one_data()), train),
  # #   CTrain <- C[train, ],
  # #   CTest <- C[test, ],
  # #   treeFit <- tree(average_rating ~ ., data = CTrain)
  # # )
  # 
  # # output$treegraph <- renderPlot({
  # #   plot(vals$treeFit)
  # # })
  # 
  # # tree_model <- reactive({
  # #   set.seed(dim(C)[1])
  # #   train <- sample(1:nrow(C), size = nrow(C)*0.8)
  # #   test <- dplyr::setdiff(1:nrow(C), train)
  # #   CTrain <- C[train, ]
  # #   CTest <- C[test, ]
  # #   treeFit <- tree(average_rating ~ ., data = CTrain)
  # #   plot(treeFit); text(treeFit)
  # #   cvTree <- cv.tree(treeFit); cvTree
  # #   
  # #   
  # # })
  # 
  # 
  # # output$test <- renderTable({
  # #   use_this_one_data()
  # # })
  # 
  # #tree model"
  # 
  # 
  # 
  #   output$irisPlot <- renderPlot({
  #     plot(iris)
  #   })

#})
