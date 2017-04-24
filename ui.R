#
# Ars Magna - ui.R
#
# (c) 2015 R. Garcia Leiva
# http://www.mathematicsunknown.com
#
# Shiny user interface
#

library(shiny)

CategoriesL1 <- read.csv("../data/CategoriesLevel1.csv", quote="\"", header=TRUE, stringsAsFactors=FALSE)
CategoriesL2 <- read.csv("../data/CategoriesLevel2.csv", quote="\"", header=TRUE, stringsAsFactors=FALSE)
CategoriesL3 <- read.csv("../data/CategoriesLevel3.csv", quote="\"", header=TRUE, stringsAsFactors=FALSE)
CategoriesL4 <- read.csv("../data/CategoriesLevel4.csv", quote="\"", header=TRUE, stringsAsFactors=FALSE)

# Colums of the dataset
namesQuestion    <- list("Topic_1", "Metric_1a", "Metric_1b", "Topic_2", "Metric_2a", "Metric_2b", "Interestingness")
namesSerendipity <- list("Question_1", "Category_1", "Relevance_1", "Nescience_1", "Question_2", "Category_2", "Relevance_2", "Nescience_2", "Interestingness")

# Define UI for dataset viewer application
shinyUI(
    
    navbarPage("Ars Magna",
               
               #
               # Metrics Tab
               #
               
               tabPanel("Metrics",
                        
                        fluidPage(
                            
                            # Application title
                            titlePanel("Classification of Research Topics"),
                            
                            fluidRow(
                                
                                column(6,
                                       sliderInput("sliderLevel", "Category Level", min = 1, max = 4, value = 1, step=1),
                                       selectInput("category1", "Select Level 1", choices = c(CategoriesL1$Level1, "ALL OF THEM")),
                                       conditionalPanel(
                                           condition = "input.sliderLevel >= 2",
                                           uiOutput("category2Controls")
                                       ),
                                       conditionalPanel(
                                           condition = "input.sliderLevel >= 3",
                                           uiOutput("category3Controls")
                                       ),
                                       conditionalPanel(
                                           condition = "input.sliderLevel >= 4",
                                           uiOutput("category4Controls")
                                       )
                                ),
                                
                                column(6,
                                       selectInput("topicRole", "Role of Topic", choices = c("As Tool", "As Question")),
                                       conditionalPanel(
                                           condition = "input.topicRole == 'As Tool'",
                                           radioButtons("toolRadioButton", "Metric", c("Applicability", "Maturity", "Interestingness"))
                                       ),
                                       conditionalPanel(
                                           condition = "input.topicRole == 'As Question'",
                                           radioButtons("questionRadioButton", "Metric", c("Relevance", "Nescience", "Interestingness"))
                                       )
                                )),
                                
                            hr(),
                            
                            fluidRow(
                                dataTableOutput(outputId="view")
                            )
                                
                            ) ),
               
               #
               # Questions Tab
               #
               
               tabPanel("Questions",
                        
                        fluidPage(
                            
                            # Application title
                            titlePanel("Interesting Research Questions"),
                            
                            fluidRow(
                                
                                column(4,
                                       sliderInput("sliderLevelQ", "Category Level", min = 1, max = 4, value = 1, step=1)
                                ),

                                column(4,
                                       selectInput("topicRoleQ1", "Role of Topic", choices = c("As Tool", "As Question"), selected = "As Question"),
                                       selectInput("category1Q1", "Select Category Level 1", choices = c(CategoriesL1$Level1, "ALL OF THEM")),
                                       conditionalPanel(
                                           condition = "input.sliderLevelQ >= 2",
                                           uiOutput("category2ControlsQ1")
                                       ),
                                       conditionalPanel(
                                           condition = "input.sliderLevelQ >= 3",
                                           uiOutput("category3ControlsQ1")
                                       ),
                                       conditionalPanel(
                                           condition = "input.sliderLevelQ >= 4",
                                           uiOutput("category4ControlsQ1")
                                       )
                                ),
                                
                                column(4,
                                       selectInput("topicRoleQ2", "Role of Topic", choices = c("As Tool", "As Question"), selected = "As Question"),
                                       selectInput("category1Q2", "Select Category Level 1", choices = c(CategoriesL1$Level1, "ALL OF THEM")),
                                       conditionalPanel(
                                           condition = "input.sliderLevelQ >= 2",
                                           uiOutput("category2ControlsQ2")
                                       ),
                                       conditionalPanel(
                                           condition = "input.sliderLevelQ >= 3",
                                           uiOutput("category3ControlsQ2")
                                       ),
                                       conditionalPanel(
                                           condition = "input.sliderLevelQ >= 4",
                                           uiOutput("category4ControlsQ2")
                                       )
                                )
                                                                
                            ),
                            
                            hr(),
                            
                            checkboxGroupInput('show_vars_questions', 'Select columns to show:',
                                               namesQuestion, selected = namesQuestion, inline = TRUE),
                            
                            fluidRow(
                                dataTableOutput(outputId="viewQuestions")
                            )
                            
                        ) ),
               
               #
               # Serendipity Tab
               #
               
               tabPanel("Serendipity",
                        
                        fluidPage(
                            
                            # Application title
                            titlePanel("Serendipity Topics"),

                            fluidRow(
                                
                                column(4,
                                       sliderInput("sliderLevelS", "Category Level", min = 1, max = 4, value = 2, step=1)
                                )
                                
                            ),
                            
                            hr(),
                            
                            checkboxGroupInput('show_vars_serendipity', 'Select columns to show:',
                                               namesSerendipity, selected = namesSerendipity, inline = TRUE),
                            
                            fluidRow(
                                dataTableOutput(outputId="viewSerendipity")
                            )
                            
                        )
                )
               
    ))