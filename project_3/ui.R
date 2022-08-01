
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

data <- read_csv("../project_3/movie_data_7_30_2022.csv")
#data <- read_csv("movie_data_7_30_2022.csv")
#data <- read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//movie_data_7_30_2022.csv")

nemo_data <- read_xlsx("../project_3/nemo_test.xlsx")

#nemo_data <- read_xlsx("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//nemo_test.xlsx")

#data <- read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//movie_data_7_30_2022.csv")
#nemo_data <- read_xlsx("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//nemo_test.xlsx")
#data<-read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//movie_data.csv") #put the excel sheet in the same file as the server.R and ui.R files!

nemo_data$Rated <- as.factor(nemo_data$Rated)
nemo_data$first_Genre <- as.factor(nemo_data$first_Genre)
nemo_data$first_Country <- as.factor(nemo_data$first_Country)
nemo_data$first_Country <- as.factor(nemo_data$first_Country)
nemo_data$Summary_Awards <- as.factor(nemo_data$Summary_Awards)
nemo_data_with_title <- nemo_data
nemo_data <- nemo_data %>% select(-c(Title))

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
#data_with_titles <- data[complete.cases(data), ]
data$Rated[data$Rated == "N/A"] <- NA
data <- data %>% select(-c(Metascore,BoxOffice))
data_with_titles <- data[complete.cases(data), ]
#data <- data %>% select(-c(Title,Director,Writer,Actors ))

data$Rated <- droplevels(data$Rated)
data$first_Genre <- droplevels(data$first_Genre)
data$first_Country <- droplevels(data$first_Country)
data$Summary_Awards <- droplevels(data$Summary_Awards)
#data_with_titles <- data[complete.cases(data), ]



# data <- format_data(data)
# data$Rated[data$Rated == "N/A"] <- NA
# data <- data %>% select(-c(Metascore,BoxOffice))
# data_with_titles <- data[complete.cases(data), ]
# 
# data <- data %>% select(-c(Title,Director,Writer,Actors ))


data <- data %>% select(-c(Title,Director,Writer,Actors ))

data <- data[complete.cases(data), ]
data$Rated <- droplevels(data$Rated)
data$first_Genre <- droplevels(data$first_Genre)
data$first_Country <- droplevels(data$first_Country)
data$Summary_Awards <- droplevels(data$Summary_Awards)
#data_with_titles <- data[complete.cases(data), ]
set.seed(500)
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
    
    conditionalPanel(condition="input.tabselected==2 && input.choice==3",
                     selectInput(inputId="hist_graph_variable",
                                 label="Choose Which Variable You Want to Graph",
                                 choices=c("Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
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
    
    conditionalPanel(condition="input.tabselected==2 && input.choice==4",
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
    
    conditionalPanel(condition="input.tabselected==2 && input.choice==5",
                     selectInput(inputId="bar_plot_variable",
                                 label="Choose a Variable",
                                 choices=c("Rated",
                                           "first_Genre",
                                           "first_Country",
                                           "Summary_Awards"))),
    
    conditionalPanel(condition="input.tabselected==2 && input.choice==7",
                     selectInput(inputId="summary_variable",
                                 label="Choose a Variable",
                                 choices=c("Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected="Year"),
                     selectInput(inputId="covariance",
                                 label="Choose a Variable (If 'None' is selected, the 5-number summary will be shown. If 'None' is selected, the covariance will be shown)",
                                 choices=c("None",
                                           "Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected="None")),
    
    conditionalPanel(condition="input.tabselected==2 && input.choice==6",
                     selectInput(inputId="scatter_plot_x_variable",
                                 label="Choose X Variable",
                                 choices=c("Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "Year"),
                     selectInput(inputId="scatter_plot_y_variable",
                                 label="Choose Y Variable",
                                 choices=c("Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
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
    
    
    #conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary")),
    
    
    conditionalPanel(condition="input.tabselected==5 && input.tab==7",
                     # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
                     #             selected = "mtcars"),
                     sliderInput("split",
                                 "Choose Percent in Training Set",
                                 min = 0.50,
                                 max = 0.95,
                                 value = 0.80)),
    #radioButtons("choice2","Choose a Model", choices=c("Regression Tree" = 1),selected = character(0))),
    
    conditionalPanel(condition="input.tabselected==5 && input.tab==7",
                     h4("Regression Tree"),
                     selectInput(inputId="tree_response",
                                 label="Choose the Variable You Would Like to Predict in Your Regression Tree",
                                 choices=c("Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "Year"),
                     selectInput(inputId="tree_predictor",
                                 label="First Regression Tree Predictor Variable (if 'Use All Variables' is selected, the box below will be ignored)",
                                 choices=c("Use All Variables",
                                           "Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "Runtime"),
                     selectInput(inputId="tree_predictor_2",
                                 label="Second Regression Tree Predictor Variable",
                                 choices=c("Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "imdbRating"),
                     h4("Random Forest"),
                     selectInput(inputId="random_forest_response",
                                 label="Choose the Variable You Would Like to Predict in Your Random Forest",
                                 choices=c("Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "Year"),
                     selectInput(inputId="random_forest_predictor",
                                 label="First Random Forest Predictor Variable (if 'Use All Variables' is selected, the box below will be ignored)",
                                 choices=c("Use All Variables",
                                           "Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "Runtime"),
                     selectInput(inputId="random_forest_predictor_2",
                                 label="Second Random Forest Predictor Variable:",
                                 choices=c("Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "imdbRating"),
                     
                     h4("Linear Model"),
                     selectInput(inputId="linear_response",
                                 label="Choose the Variable You Would Like to Predict in Your Linear Model",
                                 choices=c("Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "Year"),
                     selectInput(inputId="linear_predictor",
                                 label="First Linear Model Predictor Variable (if 'Use All Variables' is selected, the box below will be ignored)",
                                 choices=c("Use All Variables",
                                           "Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "Runtime"),
                     selectInput(inputId="linear_predictor_2",
                                 label="Second Linear Model Predictor Variable",
                                 choices=c("Year",
                                           "Runtime",
                                           "imdbRating",
                                           "imdbVotes",
                                           "nchar_Title",
                                           "average_rating"),
                                 selected = "imdbRating"),
                     
                     
                     
                     
                     actionButton(inputId="execute",
                                  label="Go!")),
    
    
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
    #                  selectInput(inputId="predict_value",
    #                              label="I want to predict:",
    #                              choices=c("Year",
    #                                        "Rated",
    #                                        "Runtime",
    #                                        "imdbRating",
    #                                        "imdbVotes",
    #                                        "nchar_Title",
    #                                        "first_Genre",
    #                                        "first_Country",
    #                                        "Summary_Awards",
    #                                        "average_rating"),
    #                              selected="average_rating"),
    #                  
    #                  h4("Pick Your Favorite Model and Click 'Predict!' to Make Predictions"),
    #                  radioButtons(inputId="fav_model",
    #                               label="Select Model",
    #                               choices = c("Random Forest" = 1,
    #                                          "Regression Tree" = 2,
    #                                          "Linear Regression" = 3),
    #                    selected = "Regression Tree"),
    #                  radioButtons(inputId = "choose_to_predict_year",
    #                               label="Year",
    #                               choices=c("Do Not Predict Year" = 1,
    #                                         "Predict Year" = 2))),
    #                 conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_year==1",
    #                                   numericInput(inputId="pick_Year",
    #                                                label="Input Year",
    #                                                value="2003",
    #                                                step = 1)),
    # 
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
    #                  radioButtons(inputId = "choose_to_predict_rating",
    #                               label="Rated",
    #                               choices=c("Do Not Predict Rating" = 1,
    #                                         "Predict Rating" = 2))),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_rating==1",
    #                  selectInput(inputId="pick_rating",
    #                              label="Input Rating",
    #                              choices=c("Approved",
    #                                        "G",
    #                                        "GP",
    #                                        "Not Rated",
    #                                        "Passed",
    #                                        "PG",
    #                                        "PG-13",
    #                                        "R",
    #                                        "TV-14",
    #                                        "TV-PG",
    #                                        "Unrated"),
    #                              selected="G")),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
    #                  radioButtons(inputId = "choose_to_predict_runtime",
    #                               label="Runtime",
    #                               choices=c("Do Not Predict Runtime" = 1,
    #                                         "Predict Runtime" = 2))),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_runtime==1",
    #                  numericInput(inputId="pick_runtime",
    #                               label="Input Runtime",
    #                               value="100",
    #                               step = 1)),
    # 
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
    #                  radioButtons(inputId = "choose_to_predict_imdbRating",
    #                               label="imdbRating",
    #                               choices=c("Do Not Predict imdbRating" = 1,
    #                                         "Predict imdbRating" = 2))),
    # 
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_imdbRating==1",
    #                  numericInput(inputId="pick_imdbRating",
    #                               label="Input imdbRating",
    #                               value="82",
    #                               step = 1)),
    # 
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
    #                  radioButtons(inputId = "choose_to_predict_imdbvotes",
    #                               label="imdbVotes",
    #                               choices=c("Do Not Predict imdbVotes" = 1,
    #                                         "Predict imdbVotes" = 2))),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_imdbvotes==1",
    #                  numericInput(inputId="pick_imdbVotes",
    #                              label="imdbVotes",
    #                               value="1027134",
    #                                step = 1)),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
    #                  radioButtons(inputId = "choose_to_predict_nchar_Title",
    #                               label="nchar_Title",
    #                               choices=c("Do Not Predict nchar_Title" = 1,
    #                                         "Predict nchar_Title" = 2))),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_nchar_Title==1",
    #                   numericInput(inputId="pick_nchar_Title",
    #                                label="Input nchar_Title",
    #                                value="12",
    #                                step = 1)),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
    #                  radioButtons(inputId = "choose_to_predict_first_Genre",
    #                               label="first_Genre",
    #                               choices=c("Do Not Predict first_Genre" = 1,
    #                                         "Predict first_Genre" = 2))),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_first_Genre==1",
    #                   selectInput(inputId="pick_first_Genre",
    #                               label="Input first_Genre",
    #                               choices=c("Action",
    #                                         "Adventure",
    #                                         "Animation",
    #                                         "Biography",
    #                                         "Comedy",
    #                                         "Crime",
    #                                         "Documentary",
    #                                         "Drama",
    #                                         "Family",
    #                                         "Horror",
    #                                         "Mystery"),
    #                               selected = "Animation")),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
    #                  radioButtons(inputId = "choose_to_predict_first_Country",
    #                               label="first_Country",
    #                               choices=c("Do Not Predict first_Country" = 1,
    #                                         "Predict first_Country" = 2))),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_first_Country==1",
    #                  selectInput(inputId="pick_first_Country",
    #                               label="Input first_Country",
    #                               choices=c("Canada",
    #                                         "France",
    #                                         "Germany",
    #                                         "Hong Kong",
    #                                         "Mexico",
    #                                         "New Zealand",
    #                                         "South Korea",
    #                                         "Spain",
    #                                         "Taiwan",
    #                                         "Thailand",
    #                                         "United Kingdom",
    #                                         "United States"),
    #                               selected = "United States")),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
    #                  radioButtons(inputId = "choose_to_predict_Summary_Awards",
    #                               label="Summary_Awards",
    #                               choices=c("Do Not Predict Summary_Awards" = 1,
    #                                         "Predict Summary_Awards" = 2))),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_Summary_Awards==1",
    #                   selectInput(inputId="pick_Summary_Awards",
    #                               label="Input Summary_Awards",
    #                               choices=c("nomination",
    #                                         "none",
    #                                         "won",
    #                                         "won and nominated"),
    #                               selected = "won and nominated")),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
    #                  radioButtons(inputId = "choose_to_predict_average_rating",
    #                               label="average_rating",
    #                               choices=c("Do Not Predict average_rating" = 1,
    #                                         "Predict average_rating" = 2))),
    # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_average_rating==1",
    #                   numericInput(inputId="pick_average_rating",
    #                                label="Input average_rating",
    #                               value="88.2",
    #                                step = 0.1),
    # actionButton(inputId="predict",
    #              label="Predict!")),
    
    conditionalPanel(condition="input.tabselected==5 && input.tab==8",
                     # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'), 
                     #             selected = "mtcars"),
                     radioButtons("pred_choice","Choose an option", choices=c("Regresssion Tree" = 1,
                                                                              "Random Forest" = 2,
                                                                              "Linear Regression" = 3))
    ),
    conditionalPanel(condition="input.tabselected==9",
                     selectInput(inputId="select_filter_genre",
                                 label="Select Filter: Genre",
                                 choices=c("Action",
                                           "Adventure",
                                           "Animation",
                                           "Biography",
                                           "Comedy",
                                           "Crime",
                                           "Documentary",
                                           "Drama",
                                           "Family",
                                           "Horror",
                                           "Mystery"),
                                 selected = "Animation")
    ),
    conditionalPanel(condition="input.tabselected==9",
                     selectInput(inputId="select_filter_rating",
                                 label="Select Filter: Rating",
                                 choices=c("Approved",
                                           "G",
                                           "GP",
                                           "Not Rated",
                                           "Passed",
                                           "PG",
                                           "PG-13",
                                           "R",
                                           "TV-14",
                                           "TV-PG",
                                           "Unrated"),
                                 selected = "G"),
    ),
    
    conditionalPanel(condition="input.tabselected==9",
                     downloadButton("downloadData", "Download Filtered: Genre"),
                     downloadButton("downloadData_rating", "Download Filtered: Rating"),
                     downloadButton("downloadData_entire", "Download Entire Data Set")
                     
                     
    ),
    
    
    
    
    
    # conditionalPanel(condition="input.tabselected==4",
    # radioButtons(inputId="tree_predictors",
    #              label="Decide if you want R to find the best predictors or specify them yourself",
    #              choices=c("Let R decide best predictors" = 1, "I want to choose my predictors" = 2),
    #              selected = "Let R decide best predictors")),
    
    
    
    
    #),
    
    # conditionalPanel(condition="input.tabselected==4",
    #                  # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
    #                  #             selected = "mtcars"),
    #                  radioButtons("choice2","Choose an option", choices=c("Regression Tree" = 1))
    # 
    # ),
    
    # conditionalPanel(condition="input.tabselected==4 && input.choice2==1",
    #                  sliderInput("split",
    #                              "Choose Percent in Training Set",
    #                              min = 0.50,
    #                              max = 0.95,
    #                              value = 0.80),
    # numericInput(inputId="split",
    #              label="Split Data into Training and Test Sets (min is 0.50 and max is 0.95)",
    #              value=0.80,
    #              min=0.50,
    #              max=0.95,
    #              step = 0.01),
    # selectInput(inputId="response",
    #             label="Choose Your Response Variable",
    #             choices=c("Year",
    #                       "Runtime",
    #                       "Metascore",
    #                       "imdbRating",
    #                       "imdbVotes",
    #                       "BoxOffice",
    #                       "nchar_Title",
    #                       "average_rating"),
    #             selected = "Year")),
    
    
    
    # 
    # conditionalPanel(condition="input.choice2==1",
    #                  numericInput(inputId="split",
    #                               label="Split Data into Training and Test Sets (min is 50% and max is 95%)",
    #                               value=0.90,
    #                               min=0.50,
    #                               max=0.95,
    #                               step = 0.01))
    
    
    
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
      #tabPanel("About",value=1,conditionalPanel(condition="input.tabselected==1", DT::dataTableOutput("table"))),
      tabPanel("About",value=1,h4("The purpose of this app is to explore and train models for a movie data set created using the Open Movie Database", a("(OMDb API).", href = "http://www.omdbapi.com/"),
                                  "In order to access the OMDb API, you need to get a free api key. For every movie requested, the OMDb API gives the following information: Title, Year, Rated, Released,
                                  Runtime, Genre, Director, Writer, Actors, Plot, Language, Country, Awards, Poster, Ratings.Source, Ratings.Value, Metascore, imdbRating, imdbVotes, imdbID, Type, DVD, 
                                  BoxOffice, Production, Website, and Response. The code above the shiny app in the server.R and ui.R files removes some of these columns, converts others to numeric, 
                                  and creates two columns: the average_rating column showed the average of the ratings values provided for each movie and Summary_Awards shows whether a movie won and 
                                  was nominated for an award, won an award, was nominated for an award, or did not win and was not nominated for an award."),
               br(),
               h4("This app will explore the data by displaying dynamic summaries (both graphical and numerical summaries of categorical and quantitative data) using histograms, contingency tables, 
                  bar plots, and scatter plots (see Data Exploration tab)."),
               br(),
               h4("This app will model the data (see Modeling tab) dynamically using a multiple linear regression model, regression tree, and a random forest model. A description of each model can
                  be found under the Modeling Info subtab. The models will be fit using training data and cross validation (see Model Fitting subtab) and used to make predictions on the test set
                  (see Prediction subtab)."),
               br(),
               h4("This app will also allow you to look at the dataset and save it as a .csv file (see Data tab)."),
               br(),
               tags$img(src='Lobby.png',height="200px", width="300px",alt="something went wrong",deleteFile=FALSE),
               #img(src="C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//Let's_All_Go_to_the_Lobby.png", align = "center"),
      ),
      
      tabPanel("Data Exploration", value=2, conditionalPanel(condition="input.choice==1", DT::dataTableOutput("dat")),
               conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
               conditionalPanel(condition="input.choice==3", plotOutput("summary")),
               conditionalPanel(condition="input.choice==4", verbatimTextOutput("cont_table")),
               conditionalPanel(condition="input.choice==5", plotOutput("bar_plot")),
               conditionalPanel(condition="input.choice==6", plotOutput("scatter_plot")),
               conditionalPanel(condition="input.choice==7", verbatimTextOutput("summary_table"))),
      #conditionalPanel(condition="input.table==")),
      #tabPanel("Plot", value=3, plotOutput("plot")), 
      #tabPanel("Modeling",value=4),
      #conditionalPanel(condition="input.tabselected==4", DT::dataTableOutput("table")),
      #conditionalPanel(condition="input.tabselected==4",plotOutput("tree_plot"),textOutput("tree_RMSE"),textOutput("tree_RMSE_train"))),
      #conditionalPanel(condition="input.tabselected==4", DT::dataTableOutput("table"), DT::dataTableOutput("table2"), plotOutput("tree_plot"), textOutput("tree_RMSE"))),
      #conditionalPanel(condition="input.tabselected==4 && input.choice2==1", plotOutput("tree_plot"), textOutput("tree_RMSE"))),
      # tabPanel("Modeling",value=5,
      #          tabPanel("subTab11",value=6)),
      
      tabPanel("Modeling",value=5,
               tabsetPanel(
                 tabPanel("Modeling Info",value=6),
                 #tabPanel("Model Fitting",value=7, textOutput("lm_heading"), verbatimTextOutput("lm_results"), verbatimTextOutput("lm_sum_results")),
                 tabPanel("Model Fitting",value=7, DT::dataTableOutput("table"),textOutput("rf_heading"), verbatimTextOutput("rf_RMSE_test"),verbatimTextOutput("rf_RMSE_results"),plotOutput("varimp"),textOutput("rf_test_rmse"),textOutput("rt_heading"),textOutput("rt_train_rmse"),textOutput("rt_test_rmse"),verbatimTextOutput("rt_RMSE_results"),plotOutput("rt_plot"), textOutput("lm_heading"), verbatimTextOutput("lm_results"), verbatimTextOutput("lm_sum_results"),textOutput("lm_test_rmse")),
                 #tabPanel("Model Fitting",value=7,DT::dataTableOutput("table"),plotOutput("tree_plot"),textOutput("tree_RMSE_train"), textOutput("rf_RMSE_train"), verbatimTextOutput("rf_RMSE_table"), plotOutput("varimp"), verbatimTextOutput("rf_train_stats")),
                 #tabPanel("Prediction",value=8, tableOutput("predictionTable"),textOutput("rt_pred_heading"),textOutput("rt_prediction"),textOutput("rf_pred_heading"),textOutput("rf_prediction"),textOutput("lm_pred_heading"),textOutput("lm_prediction")),
                 
                 
                 tabPanel("Prediction",value=8, DT::dataTableOutput("nemo_dat"),
                          conditionalPanel(condition="input.pred_choice==1",textOutput("rt_pred_heading"),textOutput("rt_prediction")),
                          conditionalPanel(condition="input.pred_choice==2",textOutput("rf_pred_heading"),textOutput("rf_prediction")),
                          conditionalPanel(condition="input.pred_choice==3",textOutput("lm_pred_heading"),textOutput("lm_prediction"))),
                 id="tab"
               )),
      tabPanel("Data", value=9,
               h4("Entire Data Set"),
               DT::dataTableOutput("entire_dataset"),
               h4("Filtered Data by Genre"),
               tableOutput("filtered_dataset_genre"),
               h4("Filtered Data by Rating"),
               tableOutput("filtered_dataset_rating")), 
      # conditionalPanel(condition="input.pred_choice==1",textOutput("rt_pred_heading"),textOutput("rt_prediction")),
      # 
      # DT::dataTableOutput("dat")),
      
      
      
      # tabsetPanel(id = "subTabPanel1", 
      #             tabPanel("subTab11"),
      #             tabPanel("subTab12")
      # )
      # tabPanel("Model",value=4,
      #          conditionalPanel(condition="input.choice2==1", verbatimTextOutput("split_data"))),
      id = "tabselected"
    )
  )
  
))



























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
# 
# data <- read_csv("../project_3/movie_data_7_30_2022.csv")
# #data <- read_csv("movie_data_7_30_2022.csv")
# #data <- read_csv("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//movie_data_7_30_2022.csv")
# 
# nemo_data <- read_xlsx("../project_3/nemo_test.xlsx")
# 
# #nemo_data <- read_xlsx("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//nemo_test.xlsx")
# 
# 
# #nemo_data <- read_xlsx("C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//10_33_7_23_2022//nemo_test.xlsx")
# nemo_data$Rated <- as.factor(nemo_data$Rated)
# nemo_data$first_Genre <- as.factor(nemo_data$first_Genre)
# nemo_data$first_Country <- as.factor(nemo_data$first_Country)
# nemo_data$first_Country <- as.factor(nemo_data$first_Country)
# nemo_data$Summary_Awards <- as.factor(nemo_data$Summary_Awards)
# nemo_data_with_title <- nemo_data
# nemo_data <- nemo_data %>% select(-c(Title))
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
# #trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
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
#                                                                          "Numerical Summary of Categorical Data - Contingency Tables" = 4,
#                                                                          "Graphical Summary of Categorical Data - Barplots" = 5,
#                                                                          "Graphical Summary of Quantitative Data - Scatter Plot" = 6,
#                                                                          "Numerical Summaries of Quantitative Data" = 7))
#                      
#     ),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==3",
#                      selectInput(inputId="hist_graph_variable",
#                                  label="Choose Which Variable You Want to Graph",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
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
#     conditionalPanel(condition="input.tabselected==2 && input.choice==4",
#                      selectInput(inputId="table_variable",
#                                  label="Choose First Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "Rated"),
#                      selectInput(inputId="table_variable2",
#                                  label="Choose Second Variable (if same as first, will make a one-way table)",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "first_Genre")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==5",
#                      selectInput(inputId="bar_plot_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"))),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==7",
#                      selectInput(inputId="summary_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="Year"),
#                      selectInput(inputId="covariance",
#                                  label="Choose a Variable (If 'None' is selected, the 5-number summary will be shown. If 'None' is selected, the covariance will be shown)",
#                                  choices=c("None",
#                                            "Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="None")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==6",
#                      selectInput(inputId="scatter_plot_x_variable",
#                                  label="Choose X Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "Year"),
#                      selectInput(inputId="scatter_plot_y_variable",
#                                  label="Choose Y Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
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
#     #conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary")),
#     
#     
#     conditionalPanel(condition="input.tabselected==5 && input.tab==7",
#                      # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
#                      #             selected = "mtcars"),
#                      sliderInput("split",
#                                  "Choose Percent in Training Set",
#                                  min = 0.50,
#                                  max = 0.95,
#                                  value = 0.80)),
#     #radioButtons("choice2","Choose a Model", choices=c("Regression Tree" = 1),selected = character(0))),
#     
#     conditionalPanel(condition="input.tabselected==5 && input.tab==7",
#                      h4("Regression Tree"),
#                      selectInput(inputId="tree_response",
#                                  label="Choose the Variable You Would Like to Predict in Your Regression Tree",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "average_rating"),
#                      selectInput(inputId="tree_predictor",
#                                  label="Regression Tree Predictor Variable (optional):",
#                                  choices=c("Use All Variables",
#                                            "Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "imdbVotes"),
#                      h4("Random Forest"),
#                      selectInput(inputId="random_forest_response",
#                                  label="Choose the Variable You Would Like to Predict in Your Random Forest",
#                                  choices=c("Year",
#                                            "Rated",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards",
#                                            "average_rating"),
#                                  selected = "Year"),
#                      selectInput(inputId="random_forest_predictor",
#                                  label="Random Forest Predictor Variable (optional):",
#                                  choices=c("Use All Variables",
#                                            "Year",
#                                            "Rated",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards",
#                                            "average_rating"),
#                                  selected = "Use All Variables"),
#                      
#                      h4("Linear Model"),
#                      selectInput(inputId="linear_response",
#                                  label="Choose the Variable You Would Like to Predict in Your Linear Model",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "Year"),
#                      selectInput(inputId="linear_predictor",
#                                  label="Linear Model Predictor Variable (optional):",
#                                  choices=c("Use All Variables",
#                                            "Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "Runtime"),
#                      
#                      
#                      
#                      actionButton(inputId="execute",
#                                   label="Go!")),
#     
#     
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
#     #                  selectInput(inputId="predict_value",
#     #                              label="I want to predict:",
#     #                              choices=c("Year",
#     #                                        "Rated",
#     #                                        "Runtime",
#     #                                        "imdbRating",
#     #                                        "imdbVotes",
#     #                                        "nchar_Title",
#     #                                        "first_Genre",
#     #                                        "first_Country",
#     #                                        "Summary_Awards",
#     #                                        "average_rating"),
#     #                              selected="average_rating"),
#     #                  
#     #                  h4("Pick Your Favorite Model and Click 'Predict!' to Make Predictions"),
#     #                  radioButtons(inputId="fav_model",
#     #                               label="Select Model",
#     #                               choices = c("Random Forest" = 1,
#     #                                          "Regression Tree" = 2,
#     #                                          "Linear Regression" = 3),
#     #                    selected = "Regression Tree"),
#     #                  radioButtons(inputId = "choose_to_predict_year",
#     #                               label="Year",
#     #                               choices=c("Do Not Predict Year" = 1,
#     #                                         "Predict Year" = 2))),
#     #                 conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_year==1",
#     #                                   numericInput(inputId="pick_Year",
#     #                                                label="Input Year",
#     #                                                value="2003",
#     #                                                step = 1)),
#     # 
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
#     #                  radioButtons(inputId = "choose_to_predict_rating",
#     #                               label="Rated",
#     #                               choices=c("Do Not Predict Rating" = 1,
#     #                                         "Predict Rating" = 2))),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_rating==1",
#     #                  selectInput(inputId="pick_rating",
#     #                              label="Input Rating",
#     #                              choices=c("Approved",
#     #                                        "G",
#     #                                        "GP",
#     #                                        "Not Rated",
#     #                                        "Passed",
#     #                                        "PG",
#     #                                        "PG-13",
#     #                                        "R",
#     #                                        "TV-14",
#     #                                        "TV-PG",
#     #                                        "Unrated"),
#     #                              selected="G")),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
#     #                  radioButtons(inputId = "choose_to_predict_runtime",
#     #                               label="Runtime",
#     #                               choices=c("Do Not Predict Runtime" = 1,
#     #                                         "Predict Runtime" = 2))),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_runtime==1",
#     #                  numericInput(inputId="pick_runtime",
#     #                               label="Input Runtime",
#     #                               value="100",
#     #                               step = 1)),
#     # 
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
#     #                  radioButtons(inputId = "choose_to_predict_imdbRating",
#     #                               label="imdbRating",
#     #                               choices=c("Do Not Predict imdbRating" = 1,
#     #                                         "Predict imdbRating" = 2))),
#     # 
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_imdbRating==1",
#     #                  numericInput(inputId="pick_imdbRating",
#     #                               label="Input imdbRating",
#     #                               value="82",
#     #                               step = 1)),
#     # 
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
#     #                  radioButtons(inputId = "choose_to_predict_imdbvotes",
#     #                               label="imdbVotes",
#     #                               choices=c("Do Not Predict imdbVotes" = 1,
#     #                                         "Predict imdbVotes" = 2))),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_imdbvotes==1",
#     #                  numericInput(inputId="pick_imdbVotes",
#     #                              label="imdbVotes",
#     #                               value="1027134",
#     #                                step = 1)),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
#     #                  radioButtons(inputId = "choose_to_predict_nchar_Title",
#     #                               label="nchar_Title",
#     #                               choices=c("Do Not Predict nchar_Title" = 1,
#     #                                         "Predict nchar_Title" = 2))),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_nchar_Title==1",
#     #                   numericInput(inputId="pick_nchar_Title",
#     #                                label="Input nchar_Title",
#     #                                value="12",
#     #                                step = 1)),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
#     #                  radioButtons(inputId = "choose_to_predict_first_Genre",
#     #                               label="first_Genre",
#     #                               choices=c("Do Not Predict first_Genre" = 1,
#     #                                         "Predict first_Genre" = 2))),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_first_Genre==1",
#     #                   selectInput(inputId="pick_first_Genre",
#     #                               label="Input first_Genre",
#     #                               choices=c("Action",
#     #                                         "Adventure",
#     #                                         "Animation",
#     #                                         "Biography",
#     #                                         "Comedy",
#     #                                         "Crime",
#     #                                         "Documentary",
#     #                                         "Drama",
#     #                                         "Family",
#     #                                         "Horror",
#     #                                         "Mystery"),
#     #                               selected = "Animation")),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
#     #                  radioButtons(inputId = "choose_to_predict_first_Country",
#     #                               label="first_Country",
#     #                               choices=c("Do Not Predict first_Country" = 1,
#     #                                         "Predict first_Country" = 2))),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_first_Country==1",
#     #                  selectInput(inputId="pick_first_Country",
#     #                               label="Input first_Country",
#     #                               choices=c("Canada",
#     #                                         "France",
#     #                                         "Germany",
#     #                                         "Hong Kong",
#     #                                         "Mexico",
#     #                                         "New Zealand",
#     #                                         "South Korea",
#     #                                         "Spain",
#     #                                         "Taiwan",
#     #                                         "Thailand",
#     #                                         "United Kingdom",
#     #                                         "United States"),
#     #                               selected = "United States")),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
#     #                  radioButtons(inputId = "choose_to_predict_Summary_Awards",
#     #                               label="Summary_Awards",
#     #                               choices=c("Do Not Predict Summary_Awards" = 1,
#     #                                         "Predict Summary_Awards" = 2))),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_Summary_Awards==1",
#     #                   selectInput(inputId="pick_Summary_Awards",
#     #                               label="Input Summary_Awards",
#     #                               choices=c("nomination",
#     #                                         "none",
#     #                                         "won",
#     #                                         "won and nominated"),
#     #                               selected = "won and nominated")),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8",
#     #                  radioButtons(inputId = "choose_to_predict_average_rating",
#     #                               label="average_rating",
#     #                               choices=c("Do Not Predict average_rating" = 1,
#     #                                         "Predict average_rating" = 2))),
#     # conditionalPanel(condition="input.tabselected==5 && input.tab==8 && input.choose_to_predict_average_rating==1",
#     #                   numericInput(inputId="pick_average_rating",
#     #                                label="Input average_rating",
#     #                               value="88.2",
#     #                                step = 0.1),
#     # actionButton(inputId="predict",
#     #              label="Predict!")),
#     
#     conditionalPanel(condition="input.tabselected==5 && input.tab==8",
#                      # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'), 
#                      #             selected = "mtcars"),
#                      radioButtons("pred_choice","Choose an option", choices=c("Regresssion Tree" = 1,
#                                                                               "Random Forest" = 2,
#                                                                               "Linear Regression" = 3))
#     ),
#     
#     
#     
#     
#     
#     # conditionalPanel(condition="input.tabselected==4",
#     # radioButtons(inputId="tree_predictors",
#     #              label="Decide if you want R to find the best predictors or specify them yourself",
#     #              choices=c("Let R decide best predictors" = 1, "I want to choose my predictors" = 2),
#     #              selected = "Let R decide best predictors")),
#     
#     
#     
#     
#     #),
#     
#     # conditionalPanel(condition="input.tabselected==4",
#     #                  # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
#     #                  #             selected = "mtcars"),
#     #                  radioButtons("choice2","Choose an option", choices=c("Regression Tree" = 1))
#     # 
#     # ),
#     
#     # conditionalPanel(condition="input.tabselected==4 && input.choice2==1",
#     #                  sliderInput("split",
#     #                              "Choose Percent in Training Set",
#     #                              min = 0.50,
#     #                              max = 0.95,
#     #                              value = 0.80),
#     # numericInput(inputId="split",
#     #              label="Split Data into Training and Test Sets (min is 0.50 and max is 0.95)",
#     #              value=0.80,
#     #              min=0.50,
#     #              max=0.95,
#     #              step = 0.01),
#     # selectInput(inputId="response",
#     #             label="Choose Your Response Variable",
#     #             choices=c("Year",
#     #                       "Runtime",
#     #                       "Metascore",
#     #                       "imdbRating",
#     #                       "imdbVotes",
#     #                       "BoxOffice",
#     #                       "nchar_Title",
#     #                       "average_rating"),
#     #             selected = "Year")),
#     
#     
#     
#     # 
#     # conditionalPanel(condition="input.choice2==1",
#     #                  numericInput(inputId="split",
#     #                               label="Split Data into Training and Test Sets (min is 50% and max is 95%)",
#     #                               value=0.90,
#     #                               min=0.50,
#     #                               max=0.95,
#     #                               step = 0.01))
#     
#     
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
#       #tabPanel("About",value=1,conditionalPanel(condition="input.tabselected==1", DT::dataTableOutput("table"))),
#       tabPanel("About",value=1,h4("The purpose of this app is to explore and train models for a movie data set created using the Open Movie Database", a("(OMDb API).", href = "http://www.omdbapi.com/"),
#                                   "In order to access the OMDb API, you need to get a free api key. For every movie requested, the OMDb API gives the following information: Title, Year, Rated, Released,
#                                   Runtime, Genre, Director, Writer, Actors, Plot, Language, Country, Awards, Poster, Ratings.Source, Ratings.Value, Metascore, imdbRating, imdbVotes, imdbID, Type, DVD, 
#                                   BoxOffice, Production, Website, and Response. The code above the shiny app in the server.R and ui.R files removes some of these columns, converts others to numeric, 
#                                   and creates two columns: the average_rating column showed the average of the ratings values provided for each movie and Summary_Awards shows whether a movie won and 
#                                   was nominated for an award, won an award, was nominated for an award, or did not win and was not nominated for an award."),
#                br(),
#                h4("This app will explore the data by displaying dynamic summaries (both graphical and numerical summaries of categorical and quantitative data) using histograms, contingency tables, 
#                   bar plots, and scatter plots (see Data Exploration tab)."),
#                br(),
#                h4("This app will model the data (see Modeling tab) dynamically using a multiple linear regression model, regression tree, and a random forest model. A description of each model can
#                   be found under the Modeling Info subtab. The models will be fit using training data and cross validation (see Model Fitting subtab) and used to make predictions on the test set
#                   (see Prediction subtab)."),
#                br(),
#                h4("This app will also allow you to look at the dataset and save it as a .csv file (see Data tab)."),
#                br(),
#                tags$img(src='Lobby.png',height="200px", width="300px",alt="something went wrong",deleteFile=FALSE),
#                #img(src="C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//Let's_All_Go_to_the_Lobby.png", align = "center"),
#       ),
#       
#       tabPanel("Data Exploration", value=2, conditionalPanel(condition="input.choice==1", DT::dataTableOutput("dat")),
#                conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
#                conditionalPanel(condition="input.choice==3", plotOutput("summary")),
#                conditionalPanel(condition="input.choice==4", verbatimTextOutput("cont_table")),
#                conditionalPanel(condition="input.choice==5", plotOutput("bar_plot")),
#                conditionalPanel(condition="input.choice==6", plotOutput("scatter_plot")),
#                conditionalPanel(condition="input.choice==7", verbatimTextOutput("summary_table"))),
#       
#       
#       #conditionalPanel(condition="input.table==")),
#       #tabPanel("Plot", value=3, plotOutput("plot")), 
#       #tabPanel("Modeling",value=4),
#       #conditionalPanel(condition="input.tabselected==4", DT::dataTableOutput("table")),
#       #conditionalPanel(condition="input.tabselected==4",plotOutput("tree_plot"),textOutput("tree_RMSE"),textOutput("tree_RMSE_train"))),
#       #conditionalPanel(condition="input.tabselected==4", DT::dataTableOutput("table"), DT::dataTableOutput("table2"), plotOutput("tree_plot"), textOutput("tree_RMSE"))),
#       #conditionalPanel(condition="input.tabselected==4 && input.choice2==1", plotOutput("tree_plot"), textOutput("tree_RMSE"))),
#       # tabPanel("Modeling",value=5,
#       #          tabPanel("subTab11",value=6)),
#       
#       tabPanel("Modeling",value=5,
#                tabsetPanel(
#                  tabPanel("Modeling Info",value=6),
#                  #tabPanel("Model Fitting",value=7, textOutput("lm_heading"), verbatimTextOutput("lm_results"), verbatimTextOutput("lm_sum_results")),
#                  tabPanel("Model Fitting",value=7, textOutput("rf_heading"), verbatimTextOutput("rf_RMSE_test"),verbatimTextOutput("rf_RMSE_results"),plotOutput("varimp"),textOutput("rt_heading"),textOutput("rt_train_rmse"),verbatimTextOutput("rt_RMSE_results"),plotOutput("rt_plot"), textOutput("lm_heading"), verbatimTextOutput("lm_results"), verbatimTextOutput("lm_sum_results")),
#                  #tabPanel("Model Fitting",value=7,DT::dataTableOutput("table"),plotOutput("tree_plot"),textOutput("tree_RMSE_train"), textOutput("rf_RMSE_train"), verbatimTextOutput("rf_RMSE_table"), plotOutput("varimp"), verbatimTextOutput("rf_train_stats")),
#                  #tabPanel("Prediction",value=8, tableOutput("predictionTable"),textOutput("rt_pred_heading"),textOutput("rt_prediction"),textOutput("rf_pred_heading"),textOutput("rf_prediction"),textOutput("lm_pred_heading"),textOutput("lm_prediction")),
#                  
#                  
#                  tabPanel("Prediction",value=8, DT::dataTableOutput("nemo_dat"),
#                           conditionalPanel(condition="input.pred_choice==1",textOutput("rt_pred_heading"),textOutput("rt_prediction")),
#                           conditionalPanel(condition="input.pred_choice==2",textOutput("rf_pred_heading"),textOutput("rf_prediction")),
#                           conditionalPanel(condition="input.pred_choice==3",textOutput("lm_pred_heading"),textOutput("lm_prediction"))),
#                  id="tab"
#                )),
#       tabPanel("Data", value=9),
#       
#       
#       
#       
#       # tabsetPanel(id = "subTabPanel1", 
#       #             tabPanel("subTab11"),
#       #             tabPanel("subTab12")
#       # )
#       # tabPanel("Model",value=4,
#       #          conditionalPanel(condition="input.choice2==1", verbatimTextOutput("split_data"))),
#       id = "tabselected"
#     )
#   )
#   
# ))




























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
#                                                                          "Numerical Summary of Categorical Data - Contingency Tables" = 4,
#                                                                          "Graphical Summary of Categorical Data - Barplots" = 5,
#                                                                          "Graphical Summary of Quantitative Data - Scatter Plot" = 6,
#                                                                          "Numerical Summaries of Quantitative Data" = 7))
#                      
#     ),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==3",
#                      selectInput(inputId="hist_graph_variable",
#                                  label="Choose Which Variable You Want to Graph",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
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
#     conditionalPanel(condition="input.tabselected==2 && input.choice==4",
#                      selectInput(inputId="table_variable",
#                                  label="Choose First Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "Rated"),
#                      selectInput(inputId="table_variable2",
#                                  label="Choose Second Variable (if same as first, will make a one-way table)",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "first_Genre")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==5",
#                      selectInput(inputId="bar_plot_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"))),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==7",
#                      selectInput(inputId="summary_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="Year"),
#                      selectInput(inputId="covariance",
#                                  label="Choose a Variable (If 'None' is selected, the 5-number summary will be shown. If 'None' is selected, the covariance will be shown)",
#                                  choices=c("None",
#                                            "Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="None")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==6",
#                      selectInput(inputId="scatter_plot_x_variable",
#                                  label="Choose X Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "Year"),
#                      selectInput(inputId="scatter_plot_y_variable",
#                                  label="Choose Y Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
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
#     #conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary")),
#     
#     
#     conditionalPanel(condition="input.tabselected==5 && input.tab==7",
#                      # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
#                      #             selected = "mtcars"),
#                      sliderInput("split",
#                                  "Choose Percent in Training Set",
#                                  min = 0.50,
#                                  max = 0.95,
#                                  value = 0.80)),
#     #radioButtons("choice2","Choose a Model", choices=c("Regression Tree" = 1),selected = character(0))),
#     
#     conditionalPanel(condition="input.tabselected==5 && input.tab==7",
#                      h4("Regression Tree"),
#                      selectInput(inputId="tree_response",
#                                  label="Choose the Variable You Would Like to Predict in Your Regression Tree",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "average_rating"),
#                      h4("Random Forest"),
#                      selectInput(inputId="random_forest_response",
#                                  label="Choose the Variable You Would Like to Predict in Your Random Forest",
#                                  choices=c("Year",
#                                            "Rated",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards",
#                                            "average_rating"),
#                                  selected = "Year"),
#                      
#                      actionButton(inputId="execute",
#                                   label="Go!")),
#     
#     
#     # conditionalPanel(condition="input.tabselected==4",
#     # radioButtons(inputId="tree_predictors",
#     #              label="Decide if you want R to find the best predictors or specify them yourself",
#     #              choices=c("Let R decide best predictors" = 1, "I want to choose my predictors" = 2),
#     #              selected = "Let R decide best predictors")),
#     
#     
#     
#     
#     #),
#     
#     # conditionalPanel(condition="input.tabselected==4",
#     #                  # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
#     #                  #             selected = "mtcars"),
#     #                  radioButtons("choice2","Choose an option", choices=c("Regression Tree" = 1))
#     # 
#     # ),
#     
#     # conditionalPanel(condition="input.tabselected==4 && input.choice2==1",
#     #                  sliderInput("split",
#     #                              "Choose Percent in Training Set",
#     #                              min = 0.50,
#     #                              max = 0.95,
#     #                              value = 0.80),
#     # numericInput(inputId="split",
#     #              label="Split Data into Training and Test Sets (min is 0.50 and max is 0.95)",
#     #              value=0.80,
#     #              min=0.50,
#     #              max=0.95,
#     #              step = 0.01),
#     # selectInput(inputId="response",
#     #             label="Choose Your Response Variable",
#     #             choices=c("Year",
#     #                       "Runtime",
#     #                       "Metascore",
#     #                       "imdbRating",
#     #                       "imdbVotes",
#     #                       "BoxOffice",
#     #                       "nchar_Title",
#     #                       "average_rating"),
#     #             selected = "Year")),
#     
#     
#     
#     # 
#     # conditionalPanel(condition="input.choice2==1",
#     #                  numericInput(inputId="split",
#     #                               label="Split Data into Training and Test Sets (min is 50% and max is 95%)",
#     #                               value=0.90,
#     #                               min=0.50,
#     #                               max=0.95,
#     #                               step = 0.01))
#     
#     
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
#       #tabPanel("About",value=1,conditionalPanel(condition="input.tabselected==1", DT::dataTableOutput("table"))),
#       tabPanel("About",value=1,h4("The purpose of this app is to explore and train models for a movie data set created using the Open Movie Database", a("(OMDb API).", href = "http://www.omdbapi.com/"),
#                                   "In order to access the OMDb API, you need to get a free api key. For every movie requested, the OMDb API gives the following information: Title, Year, Rated, Released,
#                                   Runtime, Genre, Director, Writer, Actors, Plot, Language, Country, Awards, Poster, Ratings.Source, Ratings.Value, Metascore, imdbRating, imdbVotes, imdbID, Type, DVD, 
#                                   BoxOffice, Production, Website, and Response. The code above the shiny app in the server.R and ui.R files removes some of these columns, converts others to numeric, 
#                                   and creates two columns: the average_rating column showed the average of the ratings values provided for each movie and Summary_Awards shows whether a movie won and 
#                                   was nominated for an award, won an award, was nominated for an award, or did not win and was not nominated for an award."),
#                br(),
#                h4("This app will explore the data by displaying dynamic summaries (both graphical and numerical summaries of categorical and quantitative data) using histograms, contingency tables, 
#                   bar plots, and scatter plots (see Data Exploration tab)."),
#                br(),
#                h4("This app will model the data (see Modeling tab) dynamically using a multiple linear regression model, regression tree, and a random forest model. A description of each model can
#                   be found under the Modeling Info subtab. The models will be fit using training data and cross validation (see Model Fitting subtab) and used to make predictions on the test set
#                   (see Prediction subtab)."),
#                br(),
#                h4("This app will also allow you to look at the dataset and save it as a .csv file (see Data tab)."),
#                br(),
#                tags$img(src='Lobby.png',height="200px", width="300px",alt="something went wrong",deleteFile=FALSE),
#                #img(src="C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//Let's_All_Go_to_the_Lobby.png", align = "center"),
#       ),
#       
#       tabPanel("Data Exploration", value=2, conditionalPanel(condition="input.choice==1", DT::dataTableOutput("dat")),
#                conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
#                conditionalPanel(condition="input.choice==3", plotOutput("summary")),
#                conditionalPanel(condition="input.choice==4", verbatimTextOutput("cont_table")),
#                conditionalPanel(condition="input.choice==5", plotOutput("bar_plot")),
#                conditionalPanel(condition="input.choice==6", plotOutput("scatter_plot")),
#                conditionalPanel(condition="input.choice==7", verbatimTextOutput("summary_table"))),
#       #conditionalPanel(condition="input.table==")),
#       #tabPanel("Plot", value=3, plotOutput("plot")), 
#       #tabPanel("Modeling",value=4),
#       #conditionalPanel(condition="input.tabselected==4", DT::dataTableOutput("table")),
#       #conditionalPanel(condition="input.tabselected==4",plotOutput("tree_plot"),textOutput("tree_RMSE"),textOutput("tree_RMSE_train"))),
#       #conditionalPanel(condition="input.tabselected==4", DT::dataTableOutput("table"), DT::dataTableOutput("table2"), plotOutput("tree_plot"), textOutput("tree_RMSE"))),
#       #conditionalPanel(condition="input.tabselected==4 && input.choice2==1", plotOutput("tree_plot"), textOutput("tree_RMSE"))),
#       # tabPanel("Modeling",value=5,
#       #          tabPanel("subTab11",value=6)),
#       
#       tabPanel("Modeling",value=5,
#                tabsetPanel(
#                  tabPanel("Modeling Info",value=6),
#                  tabPanel("Model Fitting",value=7,DT::dataTableOutput("table"),plotOutput("tree_plot"),textOutput("tree_RMSE_train"), verbatimTextOutput("rf_RMSE_train"), verbatimTextOutput("rf_RMSE_table"), plotOutput("varimp"), verbatimTextOutput("rf_train_stats")),
#                  tabPanel("Prediction",value=8,textOutput("tree_RMSE")),
#                  id="tab"
#                )),
#       
#       
#       
#       # tabsetPanel(id = "subTabPanel1", 
#       #             tabPanel("subTab11"),
#       #             tabPanel("subTab12")
#       # )
#       # tabPanel("Model",value=4,
#       #          conditionalPanel(condition="input.choice2==1", verbatimTextOutput("split_data"))),
#       id = "tabselected"
#     )
#   )
# ))

























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
#                                                                          "Numerical Summary of Categorical Data - Contingency Tables" = 4,
#                                                                          "Graphical Summary of Categorical Data - Barplots" = 5,
#                                                                          "Graphical Summary of Quantitative Data - Scatter Plot" = 6,
#                                                                          "Numerical Summaries of Quantitative Data" = 7))
#                      
#     ),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==3",
#                      selectInput(inputId="hist_graph_variable",
#                                  label="Choose Which Variable You Want to Graph",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
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
#     conditionalPanel(condition="input.tabselected==2 && input.choice==4",
#                      selectInput(inputId="table_variable",
#                                  label="Choose First Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "Rated"),
#                      selectInput(inputId="table_variable2",
#                                  label="Choose Second Variable (if same as first, will make a one-way table)",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "first_Genre")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==5",
#                      selectInput(inputId="bar_plot_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"))),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==7",
#                      selectInput(inputId="summary_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="Year"),
#                      selectInput(inputId="covariance",
#                                  label="Choose a Variable (If 'None' is selected, the 5-number summary will be shown. If 'None' is selected, the covariance will be shown)",
#                                  choices=c("None",
#                                            "Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="None")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==6",
#                      selectInput(inputId="scatter_plot_x_variable",
#                                  label="Choose X Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "Year"),
#                      selectInput(inputId="scatter_plot_y_variable",
#                                  label="Choose Y Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
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
#     #conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary")),
#     
#     
#     conditionalPanel(condition="input.tabselected==5 && input.tab==7",
#                      # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
#                      #             selected = "mtcars"),
#                      sliderInput("split",
#                                  "Choose Percent in Training Set",
#                                  min = 0.50,
#                                  max = 0.95,
#                                  value = 0.80)),
#     #radioButtons("choice2","Choose a Model", choices=c("Regression Tree" = 1),selected = character(0))),
#     
#     conditionalPanel(condition="input.tabselected==5 && input.tab==7",
#                      h4("Regression Tree"),
#                      selectInput(inputId="tree_response",
#                                  label="Choose the Variable You Would Like to Predict in Your Regression Tree",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "average_rating"),
#                      actionButton(inputId="execute",
#                                   label="Go!")),
#     
#     
#     # conditionalPanel(condition="input.tabselected==4",
#     # radioButtons(inputId="tree_predictors",
#     #              label="Decide if you want R to find the best predictors or specify them yourself",
#     #              choices=c("Let R decide best predictors" = 1, "I want to choose my predictors" = 2),
#     #              selected = "Let R decide best predictors")),
#     
#     
#     
#     
#     #),
#     
#     # conditionalPanel(condition="input.tabselected==4",
#     #                  # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
#     #                  #             selected = "mtcars"),
#     #                  radioButtons("choice2","Choose an option", choices=c("Regression Tree" = 1))
#     # 
#     # ),
#     
#     # conditionalPanel(condition="input.tabselected==4 && input.choice2==1",
#     #                  sliderInput("split",
#     #                              "Choose Percent in Training Set",
#     #                              min = 0.50,
#     #                              max = 0.95,
#     #                              value = 0.80),
#     # numericInput(inputId="split",
#     #              label="Split Data into Training and Test Sets (min is 0.50 and max is 0.95)",
#     #              value=0.80,
#     #              min=0.50,
#     #              max=0.95,
#     #              step = 0.01),
#     # selectInput(inputId="response",
#     #             label="Choose Your Response Variable",
#     #             choices=c("Year",
#     #                       "Runtime",
#     #                       "Metascore",
#     #                       "imdbRating",
#     #                       "imdbVotes",
#     #                       "BoxOffice",
#     #                       "nchar_Title",
#     #                       "average_rating"),
#     #             selected = "Year")),
#     
#     
#     
#     # 
#     # conditionalPanel(condition="input.choice2==1",
#     #                  numericInput(inputId="split",
#     #                               label="Split Data into Training and Test Sets (min is 50% and max is 95%)",
#     #                               value=0.90,
#     #                               min=0.50,
#     #                               max=0.95,
#     #                               step = 0.01))
#     
#     
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
#       #tabPanel("About",value=1,conditionalPanel(condition="input.tabselected==1", DT::dataTableOutput("table"))),
#       tabPanel("About",value=1,h4("The purpose of this app is to explore and train models for a movie data set created using the Open Movie Database", a("(OMDb API).", href = "http://www.omdbapi.com/"),
#                                   "In order to access the OMDb API, you need to get a free api key. For every movie requested, the OMDb API gives the following information: Title, Year, Rated, Released,
#                                   Runtime, Genre, Director, Writer, Actors, Plot, Language, Country, Awards, Poster, Ratings.Source, Ratings.Value, Metascore, imdbRating, imdbVotes, imdbID, Type, DVD, 
#                                   BoxOffice, Production, Website, and Response. The code above the shiny app in the server.R and ui.R files removes some of these columns, converts others to numeric, 
#                                   and creates two columns: the average_rating column showed the average of the ratings values provided for each movie and Summary_Awards shows whether a movie won and 
#                                   was nominated for an award, won an award, was nominated for an award, or did not win and was not nominated for an award."),
#                br(),
#                h4("This app will explore the data by displaying dynamic summaries (both graphical and numerical summaries of categorical and quantitative data) using histograms, contingency tables, 
#                   bar plots, and scatter plots (see Data Exploration tab)."),
#                br(),
#                h4("This app will model the data (see Modeling tab) dynamically using a multiple linear regression model, regression tree, and a random forest model. A description of each model can
#                   be found under the Modeling Info subtab. The models will be fit using training data and cross validation (see Model Fitting subtab) and used to make predictions on the test set
#                   (see Prediction subtab)."),
#                br(),
#                h4("This app will also allow you to look at the dataset and save it as a .csv file (see Data tab)."),
#                br(),
#                tags$img(src='Lobby.png',height="200px", width="300px",alt="something went wrong",deleteFile=FALSE),
#                #img(src="C://Users//Bridget//OneDrive//R_Scripts//repos//project_3//project_3//Let's_All_Go_to_the_Lobby.png", align = "center"),
#       ),
#       
#       tabPanel("Data Exploration", value=2, conditionalPanel(condition="input.choice==1", DT::dataTableOutput("dat")),
#                conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
#                conditionalPanel(condition="input.choice==3", plotOutput("summary")),
#                conditionalPanel(condition="input.choice==4", verbatimTextOutput("cont_table")),
#                conditionalPanel(condition="input.choice==5", plotOutput("bar_plot")),
#                conditionalPanel(condition="input.choice==6", plotOutput("scatter_plot")),
#                conditionalPanel(condition="input.choice==7", verbatimTextOutput("summary_table"))),
#       #conditionalPanel(condition="input.table==")),
#       #tabPanel("Plot", value=3, plotOutput("plot")), 
#       #tabPanel("Modeling",value=4),
#       #conditionalPanel(condition="input.tabselected==4", DT::dataTableOutput("table")),
#       #conditionalPanel(condition="input.tabselected==4",plotOutput("tree_plot"),textOutput("tree_RMSE"),textOutput("tree_RMSE_train"))),
#       #conditionalPanel(condition="input.tabselected==4", DT::dataTableOutput("table"), DT::dataTableOutput("table2"), plotOutput("tree_plot"), textOutput("tree_RMSE"))),
#       #conditionalPanel(condition="input.tabselected==4 && input.choice2==1", plotOutput("tree_plot"), textOutput("tree_RMSE"))),
#       # tabPanel("Modeling",value=5,
#       #          tabPanel("subTab11",value=6)),
#       
#       tabPanel("Modeling",value=5,
#                tabsetPanel(
#                  tabPanel("Modeling Info",value=6),
#                  tabPanel("Model Fitting",value=7,DT::dataTableOutput("table"),plotOutput("tree_plot"),textOutput("tree_RMSE_train")),
#                  tabPanel("Prediction",value=8,textOutput("tree_RMSE")),
#                  id="tab"
#                )),
#       
#       
#       
#       # tabsetPanel(id = "subTabPanel1", 
#       #             tabPanel("subTab11"),
#       #             tabPanel("subTab12")
#       # )
#       # tabPanel("Model",value=4,
#       #          conditionalPanel(condition="input.choice2==1", verbatimTextOutput("split_data"))),
#       id = "tabselected"
#     )
#   )
# ))



























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
#                                                                          "Numerical Summary of Categorical Data - Contingency Tables" = 4,
#                                                                          "Graphical Summary of Categorical Data - Barplots" = 5,
#                                                                          "Graphical Summary of Quantitative Data - Scatter Plot" = 6,
#                                                                          "Numerical Summaries of Quantitative Data" = 7))
#                      
#     ),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==3",
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
#     conditionalPanel(condition="input.tabselected==2 && input.choice==4",
#                      selectInput(inputId="table_variable",
#                                  label="Choose First Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "Rated"),
#                      selectInput(inputId="table_variable2",
#                                  label="Choose Second Variable (if same as first, will make a one-way table)",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "first_Genre")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==5",
#                      selectInput(inputId="bar_plot_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"))),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==7",
#                      selectInput(inputId="summary_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="Year"),
#                      selectInput(inputId="covariance",
#                                  label="Choose a Variable (If 'None' is selected, the 5-number summary will be shown. If 'None' is selected, the covariance will be shown)",
#                                  choices=c("None",
#                                            "Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="None")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==6",
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
#     #conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary")),
#     
#     
#     conditionalPanel(condition="input.tabselected==4",
#                      # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
#                      #             selected = "mtcars"),
#                      sliderInput("split",
#                                  "Choose Percent in Training Set",
#                                  min = 0.50,
#                                  max = 0.95,
#                                  value = 0.80)),
#     #radioButtons("choice2","Choose a Model", choices=c("Regression Tree" = 1),selected = character(0))),
#     
#     conditionalPanel(condition="input.tabselected==4",
#                      h4("Regression Tree"),
#                      selectInput(inputId="tree_response",
#                                  label="Choose the Variable You Would Like to Predict in Your Regression Tree",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "average_rating")),
#     
#     # conditionalPanel(condition="input.tabselected==4",
#     # radioButtons(inputId="tree_predictors",
#     #              label="Decide if you want R to find the best predictors or specify them yourself",
#     #              choices=c("Let R decide best predictors" = 1, "I want to choose my predictors" = 2),
#     #              selected = "Let R decide best predictors")),
#     
#     
#     
#     
#     #),
#     
#     # conditionalPanel(condition="input.tabselected==4",
#     #                  # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
#     #                  #             selected = "mtcars"),
#     #                  radioButtons("choice2","Choose an option", choices=c("Regression Tree" = 1))
#     # 
#     # ),
#     
#     # conditionalPanel(condition="input.tabselected==4 && input.choice2==1",
#     #                  sliderInput("split",
#     #                              "Choose Percent in Training Set",
#     #                              min = 0.50,
#     #                              max = 0.95,
#     #                              value = 0.80),
#     # numericInput(inputId="split",
#     #              label="Split Data into Training and Test Sets (min is 0.50 and max is 0.95)",
#     #              value=0.80,
#     #              min=0.50,
#     #              max=0.95,
#     #              step = 0.01),
#     # selectInput(inputId="response",
#     #             label="Choose Your Response Variable",
#     #             choices=c("Year",
#     #                       "Runtime",
#     #                       "Metascore",
#     #                       "imdbRating",
#     #                       "imdbVotes",
#     #                       "BoxOffice",
#     #                       "nchar_Title",
#     #                       "average_rating"),
#     #             selected = "Year")),
#     
#     
#     
#     # 
#     # conditionalPanel(condition="input.choice2==1",
#     #                  numericInput(inputId="split",
#     #                               label="Split Data into Training and Test Sets (min is 50% and max is 95%)",
#     #                               value=0.90,
#     #                               min=0.50,
#     #                               max=0.95,
#     #                               step = 0.01))
#     
#     
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
#                conditionalPanel(condition="input.choice==6", plotOutput("scatter_plot")),
#                conditionalPanel(condition="input.choice==7", verbatimTextOutput("summary_table"))),
#       #conditionalPanel(condition="input.table==")),
#       #tabPanel("Plot", value=3, plotOutput("plot")), 
#       tabPanel("Model",value=4,
#                conditionalPanel(condition="input.tabselected==4", DT::dataTableOutput("table")),
#                conditionalPanel(condition="input.tabselected==4",plotOutput("tree_plot"),textOutput("tree_RMSE"))),
#       #conditionalPanel(condition="input.tabselected==4", DT::dataTableOutput("table"), DT::dataTableOutput("table2"), plotOutput("tree_plot"), textOutput("tree_RMSE"))),
#       #conditionalPanel(condition="input.tabselected==4 && input.choice2==1", plotOutput("tree_plot"), textOutput("tree_RMSE"))),
#       # tabPanel("Modeling",value=5,
#       #          tabPanel("subTab11",value=6)),
#       
#       tabPanel("Modeling",value=5,
#                tabsetPanel(
#                  tabPanel("Modeling Info"),
#                  tabPanel("Model Fitting"),
#                  tabPanel("Prediction")
#                )),
#       
#       
#       
#       # tabsetPanel(id = "subTabPanel1", 
#       #             tabPanel("subTab11"),
#       #             tabPanel("subTab12")
#       # )
#       # tabPanel("Model",value=4,
#       #          conditionalPanel(condition="input.choice2==1", verbatimTextOutput("split_data"))),
#       id = "tabselected"
#     )
#   )
# ))




























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
#                                                                          "Numerical Summary of Categorical Data - Contingency Tables" = 4,
#                                                                          "Graphical Summary of Categorical Data - Barplots" = 5,
#                                                                          "Graphical Summary of Quantitative Data - Scatter Plot" = 6,
#                                                                          "Numerical Summaries of Quantitative Data" = 7))
#                      
#     ),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==3",
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
#     conditionalPanel(condition="input.tabselected==2 && input.choice==4",
#                      selectInput(inputId="table_variable",
#                                  label="Choose First Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "Rated"),
#                      selectInput(inputId="table_variable2",
#                                  label="Choose Second Variable (if same as first, will make a one-way table)",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "first_Genre")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==5",
#                      selectInput(inputId="bar_plot_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"))),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==7",
#                      selectInput(inputId="summary_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="Year"),
#                      selectInput(inputId="covariance",
#                                  label="Choose a Variable (If 'None' is selected, the 5-number summary will be shown. If 'None' is selected, the covariance will be shown)",
#                                  choices=c("None",
#                                            "Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="None")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==6",
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
#     #conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary")),
#     
#     
#     conditionalPanel(condition="input.tabselected==4",
#                      # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
#                      #             selected = "mtcars"),
#                      sliderInput("split",
#                                  "Choose Percent in Training Set",
#                                  min = 0.50,
#                                  max = 0.95,
#                                  value = 0.80),
#                      radioButtons("choice2","Choose a Model", choices=c("Regression Tree" = 1),selected = character(0))),
#     
#     #),
#     
#     # conditionalPanel(condition="input.tabselected==4",
#     #                  # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
#     #                  #             selected = "mtcars"),
#     #                  radioButtons("choice2","Choose an option", choices=c("Regression Tree" = 1))
#     # 
#     # ),
#     
#     # conditionalPanel(condition="input.tabselected==4 && input.choice2==1",
#     #                  sliderInput("split",
#     #                              "Choose Percent in Training Set",
#     #                              min = 0.50,
#     #                              max = 0.95,
#     #                              value = 0.80),
#     # numericInput(inputId="split",
#     #              label="Split Data into Training and Test Sets (min is 0.50 and max is 0.95)",
#     #              value=0.80,
#     #              min=0.50,
#     #              max=0.95,
#     #              step = 0.01),
#     # selectInput(inputId="response",
#     #             label="Choose Your Response Variable",
#     #             choices=c("Year",
#     #                       "Runtime",
#     #                       "Metascore",
#     #                       "imdbRating",
#     #                       "imdbVotes",
#     #                       "BoxOffice",
#     #                       "nchar_Title",
#     #                       "average_rating"),
#     #             selected = "Year")),
#     
#     
#     
#     # 
#     # conditionalPanel(condition="input.choice2==1",
#     #                  numericInput(inputId="split",
#     #                               label="Split Data into Training and Test Sets (min is 50% and max is 95%)",
#     #                               value=0.90,
#     #                               min=0.50,
#     #                               max=0.95,
#     #                               step = 0.01))
#     
#     
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
#                conditionalPanel(condition="input.choice==6", plotOutput("scatter_plot")),
#                conditionalPanel(condition="input.choice==7", verbatimTextOutput("summary_table"))),
#       #conditionalPanel(condition="input.table==")),
#       #tabPanel("Plot", value=3, plotOutput("plot")), 
#       tabPanel("Model",value=4,
#                conditionalPanel(condition="input.tabselected==4", DT::dataTableOutput("table"), DT::dataTableOutput("table2")),
#                conditionalPanel(condition="input.tabselected==4 && input.choice2==1", plotOutput("tree_plot"), textOutput("tree_RMSE"))),
#       # tabPanel("Model",value=4,
#       #          conditionalPanel(condition="input.choice2==1", verbatimTextOutput("split_data"))),
#       id = "tabselected"
#     )
#   )
# ))



























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
#                                                                          "Numerical Summary of Categorical Data - Contingency Tables" = 4,
#                                                                          "Graphical Summary of Categorical Data - Barplots" = 5,
#                                                                          "Graphical Summary of Quantitative Data - Scatter Plot" = 6,
#                                                                          "Numerical Summaries of Quantitative Data" = 7))
#                      
#     ),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==3",
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
#     conditionalPanel(condition="input.tabselected==2 && input.choice==4",
#                      selectInput(inputId="table_variable",
#                                  label="Choose First Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "Rated"),
#                      selectInput(inputId="table_variable2",
#                                  label="Choose Second Variable (if same as first, will make a one-way table)",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "first_Genre")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==5",
#                      selectInput(inputId="bar_plot_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"))),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==7",
#                      selectInput(inputId="summary_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="Year"),
#                      selectInput(inputId="covariance",
#                                  label="Choose a Variable (If 'None' is selected, the 5-number summary will be shown. If 'None' is selected, the covariance will be shown)",
#                                  choices=c("None",
#                                            "Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="None")),
#     
#     conditionalPanel(condition="input.tabselected==2 && input.choice==6",
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
#     #conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary")),
#     
#     
#     
#     
#     conditionalPanel(condition="input.tabselected==4",
#                      # selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'),
#                      #             selected = "mtcars"),
#                      radioButtons("choice2","Choose an option", choices=c("Regression Tree" = 1))
#                      
#     ),
#     
#     conditionalPanel(condition="input.tabselected==4 && input.choice2==1",
#                      numericInput(inputId="split",
#                                   label="Split Data into Training and Test Sets (min is 0.50 and max is 0.95)",
#                                   value=0.80,
#                                   min=0.50,
#                                   max=0.95,
#                                   step = 0.01),
#                      selectInput(inputId="response",
#                                  label="Choose Your Response Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected = "Year")),
#     
#     
#     
#     # 
#     # conditionalPanel(condition="input.choice2==1",
#     #                  numericInput(inputId="split",
#     #                               label="Split Data into Training and Test Sets (min is 50% and max is 95%)",
#     #                               value=0.90,
#     #                               min=0.50,
#     #                               max=0.95,
#     #                               step = 0.01))
#     
#     
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
#                conditionalPanel(condition="input.choice==6", plotOutput("scatter_plot")),
#                conditionalPanel(condition="input.choice==7", verbatimTextOutput("summary_table"))),
#       #conditionalPanel(condition="input.table==")),
#       #tabPanel("Plot", value=3, plotOutput("plot")), 
#       tabPanel("Model",value=4,
#                conditionalPanel(condition="input.choice2==1", verbatimTextOutput("split_data"))),
#       id = "tabselected"
#     )
#   )
# ))

































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
#                                                                          "Numerical Summary of Categorical Data - Contingency Tables" = 4,
#                                                                          "Graphical Summary of Categorical Data - Barplots" = 5,
#                                                                          "Graphical Summary of Quantitative Data - Scatter Plot" = 6,
#                                                                          "Numerical Summaries of Quantitative Data" = 7))
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
#                                  label="Choose First Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "Rated"),
#                      selectInput(inputId="table_variable2",
#                                  label="Choose Second Variable (if same as first, will make a one-way table)",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"),
#                                  selected = "first_Genre")),
#     
#     conditionalPanel(condition="input.choice==5",
#                      selectInput(inputId="bar_plot_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Rated",
#                                            "first_Genre",
#                                            "first_Country",
#                                            "Summary_Awards"))),
#     
#     conditionalPanel(condition="input.choice==7",
#                      selectInput(inputId="summary_variable",
#                                  label="Choose a Variable",
#                                  choices=c("Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="Year"),
#                      selectInput(inputId="covariance",
#                                  label="Choose a Variable (If 'None' is selected, the 5-number summary will be shown. If 'None' is selected, the covariance will be shown)",
#                                  choices=c("None",
#                                            "Year",
#                                            "Runtime",
#                                            "Metascore",
#                                            "imdbRating",
#                                            "imdbVotes",
#                                            "BoxOffice",
#                                            "nchar_Title",
#                                            "average_rating"),
#                                  selected="None")),
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
#                conditionalPanel(condition="input.choice==6", plotOutput("scatter_plot")),
#                conditionalPanel(condition="input.choice==7", verbatimTextOutput("summary_table"))),
#       #conditionalPanel(condition="input.table==")),
#       tabPanel("Plot", value=3, plotOutput("plot")), 
#       id = "tabselected"
#     )
#   )
# ))




















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
