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

library(caret)
data("GermanCredit")
library(shiny)
library(DT)

library(shiny)
library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Use case - Change the side bar panel elements based on the selected tab."),
  sidebarPanel(
    
    ## conditionalPanel() functions for selected tab
    #conditionalPanel(condition="input.tabselected==1",h4("Demo conditionalPanel()")),
    conditionalPanel(condition="input.tabselected==2",
                     # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'), 
                     #             selected = "mtcars"),
                     radioButtons("choice","Choose an option", choices=c("Dataset" = 1,
                                                                         "Structure" = 2,
                                                                         "Graphical Summary of Quantitative Data - Histogram" = 3,
                                                                         "Numerical Summary of Categorical Data - Contingency Tables" = 4,
                                                                         "Graphical Summary of Categorical Data - Barplots" = 5,
                                                                         "Graphical Summary of Quantitative Data - Scatter Plot" = 6,
                                                                         "Numerical Summaries of Quantitative Data" = 7))
                     
    ),
    
    conditionalPanel(condition="input.choice==3",
                     selectInput(inputId="hist_graph_variable",
                                 label="Choose Which Variable You Want to Graph",
                                 choices=c("Year",
                                           "Runtime",
                                           "Metascore",
                                           "imdbRating",
                                           "imdbVotes",
                                           "BoxOffice",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "Year")),
    # 
    # conditionalPanel(condition="input.choice==4",
    #                  radioButtons(inputId="table",
    #                              label="Choose Contingency Table Type",
    #                              choices=c("One Way",
    #                                        "Two Way"),
    #                              selected = "One Way")),
    
    conditionalPanel(condition="input.choice==4",
                     selectInput(inputId="table_variable",
                                 label="Choose First Variable",
                                 choices=c("Rated",
                                           "first_Genre",
                                           "first_Country",
                                           "Summary_Awards"),
                                 selected = "Rated"),
                     selectInput(inputId="table_variable2",
                                 label="Choose Second Variable (if same as first, will make a one-way table)",
                                 choices=c("Rated",
                                           "first_Genre",
                                           "first_Country",
                                           "Summary_Awards"),
                                 selected = "first_Genre")),
    
    conditionalPanel(condition="input.choice==5",
                     selectInput(inputId="bar_plot_variable",
                                 label="Choose a Variable",
                                 choices=c("Rated",
                                           "first_Genre",
                                           "first_Country",
                                           "Summary_Awards"))),
    
    conditionalPanel(condition="input.choice==7",
                     selectInput(inputId="summary_variable",
                                 label="Choose a Variable",
                                 choices=c("Year",
                                           "Runtime",
                                           "Metascore",
                                           "imdbRating",
                                           "imdbVotes",
                                           "BoxOffice",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected="Year"),
                     selectInput(inputId="covariance",
                                 label="Choose a Variable (If 'None' is selected, the 5-number summary will be shown. If 'None' is selected, the covariance will be shown)",
                                 choices=c("None",
                                           "Year",
                                           "Runtime",
                                           "Metascore",
                                           "imdbRating",
                                           "imdbVotes",
                                           "BoxOffice",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected="None")),
    
    conditionalPanel(condition="input.choice==6",
                     selectInput(inputId="scatter_plot_x_variable",
                                 label="Choose X Variable",
                                 choices=c("Year",
                                           "Runtime",
                                           "Metascore",
                                           "imdbRating",
                                           "imdbVotes",
                                           "BoxOffice",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "Year"),
                     selectInput(inputId="scatter_plot_y_variable",
                                 label="Choose Y Variable",
                                 choices=c("Year",
                                           "Runtime",
                                           "Metascore",
                                           "imdbRating",
                                           "imdbVotes",
                                           "BoxOffice",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "Runtime"),
                     selectInput(inputId="color_variable",
                                 label="Choose Color Variable",
                                 choices=c("Rated",
                                           "first_Genre",
                                           "first_Country",
                                           "Summary_Awards"),
                                 selected = "Rated")),
    
    
    conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary"))
    
  ),
  mainPanel(
    # recommend review the syntax for tabsetPanel() & tabPanel() for better understanding
    # id argument is important in the tabsetPanel()
    # value argument is important in the tabPanle()
    tabsetPanel(
      #       tabPanel("About", value=1, helpText("conditionalPanel(condition, ...) creates a panel that is visible or hidden, 
      # depending on the condition given. The condition is evaluated once at 
      #               startup and whenever Shiny detects a relevant change in input/output.
      #                                          ")),
      tabPanel("Data", value=2, conditionalPanel(condition="input.choice==1", verbatimTextOutput("dat")),
               conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
               conditionalPanel(condition="input.choice==3", plotOutput("summary")),
               conditionalPanel(condition="input.choice==4", verbatimTextOutput("cont_table")),
               conditionalPanel(condition="input.choice==5", plotOutput("bar_plot")),
               conditionalPanel(condition="input.choice==6", plotOutput("scatter_plot")),
               conditionalPanel(condition="input.choice==7", verbatimTextOutput("summary_table"))),
      #conditionalPanel(condition="input.table==")),
      tabPanel("Plot", value=3, plotOutput("plot")), 
      id = "tabselected"
    )
  )
))




















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
# library(caret)
# data("GermanCredit")
# library(shiny)
# library(DT)
# 
# library(shiny)
# library(shiny)
# shinyUI(pageWithSidebar(
#   headerPanel("Use case - Change the side bar panel elements based on the selected tab."),
#   sidebarPanel(
#     
#     ## conditionalPanel() functions for selected tab
#     #conditionalPanel(condition="input.tabselected==1",h4("Demo conditionalPanel()")),
#     conditionalPanel(condition="input.tabselected==2",
#                      # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'), 
#                      #             selected = "mtcars"),
#                      radioButtons("choice","Choose an option", choices=c("Dataset" = 1,
#                                                                          "Structure" = 2,
#                                                                          "Graphical Summary of Quantitative Data - Histogram" = 3,
#                                                                          "Numerical Summary of Categorical Data - Contingency Table" = 4,
#                                                                          "Graphical Summary of Categorical Data - Barplots" = 5,
#                                                                          "Graphical Summary of Quantitative Data - Scatter Plot" = 6))
#                      
#     ),
#     
#     conditionalPanel(condition="input.choice==3",
#                      selectInput(inputId="hist_graph_variable",
#                                  label="Choose Which Variable You Want to Graph",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "Year")),
#     # 
#     # conditionalPanel(condition="input.choice==4",
#     #                  radioButtons(inputId="table",
#     #                              label="Choose Contingency Table Type",
#     #                              choices=c("One Way",
#     #                                        "Two Way"),
#     #                              selected = "One Way")),
#     
#     conditionalPanel(condition="input.choice==4",
#                      selectInput(inputId="table_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"))),
#     
#     conditionalPanel(condition="input.choice==5",
#                      selectInput(inputId="bar_plot_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"))),
#     
#     conditionalPanel(condition="input.choice==6",
#                      selectInput(inputId="scatter_plot_x_variable",
#                                  label="Choose X Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "Year"),
#                      selectInput(inputId="scatter_plot_y_variable",
#                                  label="Choose Y Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "Runtime"),
#                      selectInput(inputId="color_variable",
#                                  label="Choose Color Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "Rated")),
#     
#     
#     conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary"))
#     
#   ),
#   mainPanel(
#     # recommend review the syntax for tabsetPanel() & tabPanel() for better understanding
#     # id argument is important in the tabsetPanel()
#     # value argument is important in the tabPanle()
#     tabsetPanel(
#       #       tabPanel("About", value=1, helpText("conditionalPanel(condition, ...) creates a panel that is visible or hidden, 
#       # depending on the condition given. The condition is evaluated once at 
#       #               startup and whenever Shiny detects a relevant change in input/output.
#       #                                          ")),
#       tabPanel("Data", value=2, conditionalPanel(condition="input.choice==1", verbatimTextOutput("dat")),
#                conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
#                conditionalPanel(condition="input.choice==3", plotOutput("summary")),
#                conditionalPanel(condition="input.choice==4", verbatimTextOutput("cont_table")),
#                conditionalPanel(condition="input.choice==5", plotOutput("bar_plot")),
#                conditionalPanel(condition="input.choice==6", plotOutput("scatter_plot"))),
#       #conditionalPanel(condition="input.table==")),
#       tabPanel("Plot", value=3, plotOutput("plot")), 
#       id = "tabselected"
#     )
#   )
# ))


















# #
# # This is the user-interface definition of a Shiny web application. You can
# # run the application by clicking 'Run App' above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
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
# library(caret)
# data("GermanCredit")
# library(shiny)
# library(DT)
# 
# library(shiny)
# library(shiny)
# shinyUI(pageWithSidebar(
#   headerPanel("Use case - Change the side bar panel elements based on the selected tab."),
#   sidebarPanel(
#     
#     ## conditionalPanel() functions for selected tab
#     #conditionalPanel(condition="input.tabselected==1",h4("Demo conditionalPanel()")),
#     conditionalPanel(condition="input.tabselected==2",
#                      # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'), 
#                      #             selected = "mtcars"),
#                      radioButtons("choice","Choose an option", choices=c("Dataset" = 1,
#                                                                          "Structure" = 2,
#                                                                          "Graphical Summary of Quantitative Data - Histogram" = 3,
#                                                                          "Numerical Summary of Categorical Data - Contingency Table" = 4,
#                                                                          "Graphical Summary of Categorical Data - Barplots" = 5))
#                      
#     ),
#     
#     conditionalPanel(condition="input.choice==3",
#                      selectInput(inputId="hist_graph_variable",
#                                  label="Choose Which Variable You Want to Graph",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "Year")),
#     # 
#     # conditionalPanel(condition="input.choice==4",
#     #                  radioButtons(inputId="table",
#     #                              label="Choose Contingency Table Type",
#     #                              choices=c("One Way",
#     #                                        "Two Way"),
#     #                              selected = "One Way")),
#     
#     conditionalPanel(condition="input.choice==4",
#                      selectInput(inputId="table_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"))),
#     
#     conditionalPanel(condition="input.choice==5",
#                      selectInput(inputId="bar_plot_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"))),
#     
#     
#     conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary"))
#     
#   ),
#   mainPanel(
#     # recommend review the syntax for tabsetPanel() & tabPanel() for better understanding
#     # id argument is important in the tabsetPanel()
#     # value argument is important in the tabPanle()
#     tabsetPanel(
#       #       tabPanel("About", value=1, helpText("conditionalPanel(condition, ...) creates a panel that is visible or hidden, 
#       # depending on the condition given. The condition is evaluated once at 
#       #               startup and whenever Shiny detects a relevant change in input/output.
#       #                                          ")),
#       tabPanel("Data", value=2, conditionalPanel(condition="input.choice==1", verbatimTextOutput("dat")),
#                conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
#                conditionalPanel(condition="input.choice==3", plotOutput("summary")),
#                conditionalPanel(condition="input.choice==4", verbatimTextOutput("cont_table")),
#                conditionalPanel(condition="input.choice==5", plotOutput("bar_plot"))),
#       #conditionalPanel(condition="input.table==")),
#       tabPanel("Plot", value=3, plotOutput("plot")), 
#       id = "tabselected"
#     )
#   )
# ))








#shinyUI(pageWithSidebar(
# shinyUI(fluidPage(
#   titlePanel("Summaries for German Credit Data"),
#   sidebarLayout(
#     sidebarPanel(
#       conditionalPanel(condition="input.tabselected==1",h4("Demo conditionalPanel()")),
#       h4("This data set comes from the", a("caret package", href = "https://topepo.github.io/caret/"), " - originally from the UCI machiene learning repository"),
#       br(),
#       p("You can create a few bar plots using the radio buttons below."),
#         selectInput(inputId="select_box_1",
#                     label="Select the Plot Type",
#                     choices = c("Bar Plot",
#                                 "Classification and Unemployed",
#                                 "Classification and Foreign"),
#                     selected = "Classification and Unemployed"),
#       conditionalPanel(
#         condition = "input.select_box_1 == 'Bar Plot'",
#          selectInput(inputId="sel_bar_plot",
#                      label = "Choose Predictor Variable For Bar Plot",
#                      list("Rated",
#                           "first_Genre",
#                           "first_Country",
#                           "Summary_Awards"))),
#       
#       
#       # selectInput(inputId="sel_predictor",
#       #             label = "Choose Predictor Variable",
#       #             list("Rated",
#       #                  "first_Genre",
#       #                  "first_Country",
#       #                  "Summary_Awards")),
#       
#       
#       
#       
#       #checkboxInput("conservation", h4("Color Code Conservation Status", style = "color:red;")),
#       # conditionalPanel(condition = "input$select_box_1 == Just Classification",
#       #                  checkboxInput("sleep", "Also change symbol based on REM sleep?")),
#       p("You can find the", strong("sample mean"), "for a few variations below:"),
#       selectInput(inputId="var", 
#                   label="Variables to Summarize",
#                   choices=c("Duration","Amount","Age"),
#                   selected = "Age"),
#       numericInput(inputId="numeric_input",
#                    label="Select the number of digits for rounding",
#                    value=2),
#     ),
#     mainPanel(
#       conditionalPanel(condition="input.tabselected==1",plotOutput("radio_button")),
#        tabsetPanel(
#          tabPanel("About", value=1, helpText("conditionalPanel(condition, ...) creates a panel that is visible or hidden, 
#        depending on the condition given. The condition is evaluated once at 
#                      startup and whenever Shiny detects a relevant change in input/output.
#                                                 ")),
#       #   tabPanel("Data", value=2, conditionalPanel(condition="input.choice==1", verbatimTextOutput("dat")),
#       #            conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
#       #            conditionalPanel(condition="input.choice==3", verbatimTextOutput("summary"))),
#       #   tabPanel("Plot", value=3, plotOutput("plot")), 
#          id = "tabselected"
#        )
#       
#       
#       # tabsetPanel(
#       #   tabPanel("EDA",  plotOutput("radio_button"),
#       #   id = "tabselected"
#       # )
#       # )
#       
#       
#       
#       # plotOutput("radio_button"),
#       # dataTableOutput("var")
#       #plotOutput("Classification_and_Unemployed")
#     
#   )
# ))
# )
# 





# library(shiny)
# 
# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("Old Faithful Geyser Data"),
#   
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
#       h4("EDA"),
#       br(),
#       p("Learn About the Movie Data"),
#       radioButtons(inputId="radio_button",
#                    label="Select the Plot Type", 
#                    choices = c("Histogram: Response Variable",
#                                "Box Plot: Response Variable",
#                                "Y"),
#                    selected = "Histogram: Response Variable")
#     ),
#     # sidebarPanel(
#     #   sliderInput("bins",
#     #               "Number of bins:",
#     #               min = 1,
#     #               max = 50,
#     #               value = 30)
#     # ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("histPlot")
#     )
#   )
# )
# )
# 
# 
# # library(shiny)
# # shinyUI(fluidPage(
# #   
# #   # Application title
# #   titlePanel("Old Faithful Geyser Data"),
# #   
# #   # Sidebar with a slider input for number of bins
# #   sidebarLayout(
# #              sidebarPanel(
# #                numericInput(inputId="split",
# #                             label="Split Data into Training and Test Sets (min is 50% and max is 95%)",
# #                             value=0.90,
# #                             min=0.50,
# #                             max=0.95,
# #                             step = 0.01)
# #              ),
# #     
# #     # Show a plot of the generated distribution
# #     mainPanel(
# #       #plotOutput("distPlot"),
# #       textOutput("info")
# #     )
# #   )
# # ))
# # 
# # 
# # # Define UI for application that draws a histogram
# # # shinyUI(fluidPage(
# # # 
# # #     # Application title
# # #     titlePanel("Old Faithful Geyser Data"),
# # # 
# # #     # Sidebar with a slider input for number of bins
# # #     sidebarLayout(
# # #         sidebarPanel(
# # #           numericInput(inputId="split",
# # #                        label="Split Data into Training and Test Sets (min is 50% and max is 95%)",
# # #                        value=0.80,
# # #                        min=0.50,
# # #                        max=0.95,
# # #                        step = 0.01)
# # #         ),
# # # 
# # #         # Show a plot of the generated distribution
# # #         mainPanel(
# # #           textOutput("info")
# # #         )
# # #     )
# # # ))
















#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)

#make a data set of about 500 movies from movie API and use it for my app for Project 3

# Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   headerPanel("Use case - Change the side bar panel elements based on the selected tab."),
#   sidebarPanel(
#     
#     ## conditionalPanel() functions for selected tab
#     conditionalPanel(condition="input.tabselected==1",h4("Demo conditionalPanel()")),
#     conditionalPanel(condition="input.tabselected==2",
#                      selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'), 
#                                  selected = "mtcars"),
#                      radioButtons("choice","Choose an option", choices=c("Dataset" = 1, "Structure" = 2,
#                                                                          "Summary" = 3 ))
#                      
#     ),
#     
#     conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary"))
#     
#   ),
#   mainPanel(
#     # recommend review the syntax for tabsetPanel() & tabPanel() for better understanding
#     # id argument is important in the tabsetPanel()
#     # value argument is important in the tabPanle()
#     tabsetPanel(
#       tabPanel("About", value=1, helpText("conditionalPanel(condition, ...) creates a panel that is visible or hidden, 
# depending on the condition given. The condition is evaluated once at 
#               startup and whenever Shiny detects a relevant change in input/output.
#                                          ")),
#       tabPanel("Data", value=2, conditionalPanel(condition="input.choice==1", verbatimTextOutput("dat")),
#                conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
#                conditionalPanel(condition="input.choice==3", verbatimTextOutput("summary"))),
#       tabPanel("Plot", value=3, plotOutput("plot")), 
#       id = "tabselected"
#     )
#   )
#     # Application title
#     titlePanel("Project 3"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             # sliderInput("bins",
#             #             "Number of bins:",
#             #             min = 1,
#             #             max = 50,
#             #             value = 30),
#           conditionalPanel(condition="input.tabselected==2",
#                            numericInput(inputId="split",label="Split Data into Training and Test Sets (min is 50% and max is 95%)",value=0.80,min=0.50,max=0.95,step = 0.01))
# 
#                            
#           ),
#             #conditionalPanel(condition="input.tabselected==1",h4("Demo conditionalPanel()"))
#             # conditionalPanel(condition="input.tabselected==2",
#             #                  selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'), 
#             #                              selected = "mtcars"),
#             #                  radioButtons("choice","Choose an option", choices=c("Dataset" = 1, "Structure" = 2,
#             #                                                                      "Summary" = 3 ))
#         
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#           tabsetPanel(
#             #tabPanel("Movie_Data",DT::dataTableOutput(outputId = "splited_data"))
#             
#           #tabPanel("Data", value=2, conditionalPanel(condition="input.choice==1", verbatimTextOutput("dat")),
#              tabPanel("Data", value=2, conditionalPanel(condition="input.choice==1", verbatimTextOutput("dat")),
#                       conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
#                       conditionalPanel(condition="input.choice==3", verbatimTextOutput("summary")))
#           #   tabPanel("Plot", value=3, DT::dataTableOutput(outputId = "splited_data")), 
#           #   id = "tabselected"
#           # )
# #           tabsetPanel(type="tab",
# #                       #tabPanel("Data",plotOutput("distPlot")),
# #                       tabPanel("About", value=1, helpText("conditionalPanel(condition, ...) creates a panel that is visible or hidden, 
# # depending on the condition given. The condition is evaluated once at 
# #               startup and whenever Shiny detects a relevant change in input/output. ")),
# #                       # tabPanel("Data", value=2, conditionalPanel(condition="input.choice==1", verbatimTextOutput("dat")),
# #                       #          conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
# #                       tabPanel("iris",plotOutput("irisPlot")),
# #                       tabPanel("Movie_Data",DT::dataTableOutput(outputId = "test_table")),
# #                       tabPanel("Tree_Model",plotOutput("treePlot")),
# #                       tabPanel("Text",textOutput("textString")),
# #                       tabPanel("Hist",plotOutput("hist")),
# #                       id = "tabselected"
# #                       #tabPanel("test",DT::dataTableOutput(outputId = "model_table"))
# #                       #tabPanel("Tree_model",plotOutput("treegraph"))
# #                       #tabPanel("test",tableOutput("test")),
# #                       )
#           
#           
#           
#         )
#         
#         
#     )
 #))
