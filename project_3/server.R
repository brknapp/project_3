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

data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!

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

A <- data %>% select(-c(Title,Director,Writer,Actors ))

A <- A[complete.cases(A), ]

set.seed(dim(A)[1])
train <- sample(1:nrow(A), size = nrow(A)*0.8)
test <- dplyr::setdiff(1:nrow(A), train)
ATrain <- A[train, ]
ATest <- A[test, ]


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
data("GermanCredit")
library(shiny)
library(ggplot2)
data<-mtcars
shinyServer(function(input,output)({
  
  ## Get the value of the dataset that is selected by user from the list of datasets
  ## to output the dataset
  output$dat <- renderPrint({
    ATrain
  })
  
  output$struct <- renderPrint({
    str(ATrain)
  })
  
  output$cont_table <- renderPrint({
    
    tv <- ATrain %>% select(input$table_variable,input$table_variable2)
    table(tv)
  })
  
  output$summary_table <- renderPrint({
    if(input$covariance=="None"){
      sv <- ATrain %>% select(input$summary_variable)
      summary(sv)
    } else if(input$covariance!="None"){
      A<-ATrain %>% select(input$summary_variable)
      B<-ATrain %>% select(input$covariance)
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
    ggplot(ATrain,aes_string(x=input$bar_plot_variable)) +
      geom_bar(fill="lightblue") +
      ggtitle(paste0("Bar Plot: ",input$bar_plot_variable ))
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(ATrain,aes_string(x=input$scatter_plot_x_variable,y=input$scatter_plot_y_variable,color=input$color_variable)) +
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
    ggplot(ATrain,aes_string(x=input$hist_graph_variable)) +
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
  
  # to output the structure of the dataset
  
  
  # for summary
  
  
  # For plot
  # output$plot <- renderPlot({
  #   ggplot(data(),aes_string(x=input$variablex, y=input$variabley)) +
  #     geom_point() 
  # }) 
  
}))























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
