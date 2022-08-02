# project_3

This app displays, explores, models, and formulates predictions using data about movies pulled from the OMDb API. The movie data csv file contains the data I pulled from the OMDb API. The nemo data is an example of data someone could provide to formulate predictions using the models in my app. The "www" folder contains the image used in my app.

tidyverse helps format and select the data. ggplot2, rpart.plot makes plots. readr and readxl help read in the data. caret, DT,tree, randomForest, and shiny helped run models and interactively display findings. 

Run this to install all of the packages you will need:
install.packages(c("tidyverse","ggplot2", "readr","class","caret", "DT","tree","readxl","rpart.plot","randomForest","shiny"))

shiny::runGitHub(repo="project_3",username = "brknapp",ref = "main",subdir = "project_3")
