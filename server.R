#
# Ars Magna - server.R
#
# (c) 2015 R. Garcia Leiva
# http://www.mathematicsunknown.com
#
# Shiny server side
#

library(shiny)
library(AID)

CategoriesL1 <- read.csv("../data/CategoriesLevel1.csv", quote="", sep=" ", header=TRUE, stringsAsFactors=FALSE)
CategoriesL2 <- read.csv("../data/CategoriesLevel2.csv", quote="", sep=" ", header=TRUE, stringsAsFactors=FALSE)
CategoriesL3 <- read.csv("../data/CategoriesLevel3.csv", quote="", sep=" ", header=TRUE, stringsAsFactors=FALSE)
CategoriesL4 <- read.csv("../data/CategoriesLevel4.csv", quote="", sep=" ", header=TRUE, stringsAsFactors=FALSE)

PagesL1 <- read.csv("../data/PagesLevel1.csv", quote="", sep=" ", header=TRUE, stringsAsFactors=FALSE)
PagesL2 <- read.csv("../data/PagesLevel2.csv", quote="", sep=" ", header=TRUE, stringsAsFactors=FALSE)
PagesL3 <- read.csv("../data/PagesLevel3.csv", quote="", sep=" ", header=TRUE, stringsAsFactors=FALSE)
PagesL4 <- read.csv("../data/PagesLevel4.csv", quote="", sep=" ", header=TRUE, stringsAsFactors=FALSE)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

    #
    # Dynamic generated UI controls
    #
    
    # Metrics Panel
    
    output$category2Controls <- renderUI({
        Categories <- CategoriesL2[CategoriesL2$Level1 == input$category1,]$Level2
        selectInput("category2", "Select Level 2", choices = c(Categories, "ALL OF THEM"))
    })
    
    output$category3Controls <- renderUI({
        Categories <- CategoriesL3[CategoriesL3$Level2 == input$category2,]$Level3
        selectInput("category3", "Select Level 3", choices = c(Categories, "ALL OF THEM"))
    })
    
    output$category4Controls <- renderUI({
        Categories <- CategoriesL4[CategoriesL4$Level3 == input$category3,]$Level4
        selectInput("category4", "Select Level 4", choices = c(Categories, "ALL OF THEM"))
    })
    
    # Questions Panel
    
    output$category2ControlsQ1 <- renderUI({
        Categories <- CategoriesL2[CategoriesL2$Level1 == input$category1Q1,]$Level2
        selectInput("category2Q1", "Select Level 2", choices = c(Categories, "ALL OF THEM"))
    })
    
    output$category3ControlsQ1 <- renderUI({
        Categories <- CategoriesL3[CategoriesL3$Level2 == input$category2Q1,]$Level3
        selectInput("category3Q1", "Select Level 3", choices = c(Categories, "ALL OF THEM"))
    })
    
    output$category4ControlsQ1 <- renderUI({
        Categories <- CategoriesL4[CategoriesL4$Level3 == input$category3Q1,]$Level4
        selectInput("category4Q1", "Select Level 4", choices = c(Categories, "ALL OF THEM"))
    })
    
    output$category2ControlsQ2 <- renderUI({
        Categories <- CategoriesL2[CategoriesL2$Level1 == input$category1Q2,]$Level2
        selectInput("category2Q2", "Select Level 2", choices = c(Categories, "ALL OF THEM"))
    })
    
    output$category3ControlsQ2 <- renderUI({
        Categories <- CategoriesL3[CategoriesL3$Level2 == input$category2Q2,]$Level3
        selectInput("category3Q2", "Select Level 3", choices = c(Categories, "ALL OF THEM"))
    })
    
    output$category4ControlsQ2 <- renderUI({
        Categories <- CategoriesL4[CategoriesL4$Level3 == input$category3Q2,]$Level4
        selectInput("category4Q2", "Select Level 4", choices = c(Categories, "ALL OF THEM"))
    })
    
    #
    # Metrics
    #
    
    datasetInput <- reactive({

        switch(input$sliderLevel, {
            if (input$category1 == "ALL OF THEM") {
                Pages <- PagesL1
                Pages <- Pages[!duplicated(Pages$Page),]
            } else {
                Pages <- PagesL1[PagesL1$Category == input$category1,]
            }
        }, {
            if (input$category2 == "ALL OF THEM") {
                categoryList <- CategoriesL2[CategoriesL2$Level1 == input$category1,]$Level2
                Pages <- PagesL2[PagesL2$Category %in% categoryList,]
                Pages <- Pages[!duplicated(Pages$Page),]
            } else {
                Pages <- PagesL2[PagesL2$Category == input$category2,]
            }
        }, {
            if (input$category3 == "ALL OF THEM") {
                categoryList <- CategoriesL3[CategoriesL3$Level2 == input$category2,]$Level3
                Pages <- PagesL3[PagesL3$Category %in% categoryList,]
                Pages <- Pages[!duplicated(Pages$Page),]
            } else {
                Pages <- PagesL3[PagesL3$Category == input$category3,]
            }
        }, {
            if (input$category4 == "ALL OF THEM") {
                categoryList <- CategoriesL4[CategoriesL4$Level3 == input$category3,]$Level4
                Pages <- PagesL4[PagesL4$Category %in% categoryList,]
                Pages <- Pages[!duplicated(Pages$Page),]
            } else {
                Pages <- PagesL4[PagesL4$Category == input$category4,]
            }
        })

        # Clean-up data
        
        cat("Datos: ", nrow(Pages), "\n")
        
        Pages <- Pages[complete.cases(Pages),]
        Pages <- Pages[Pages$Description > Pages$Complexity,]
        
        cat("Datos Limpios: ", nrow(Pages), "\n")
        
        # Compute the metrics
        
        Pages$Relevance <- Pages$ExternalLinks + 1    # boxcox requires values greater than 0
        Pages$Nescience <- (Pages$Description - Pages$Complexity) / Pages$Complexity
        
        Pages$Applicability <- Pages$InternalLinks + 1    # boxcox requires values greater than 0
        Pages$Maturity      <- Pages$Complexity / (Pages$Description - Pages$Complexity)
        
        # Normalize the values
        
        L1 <- boxcoxnc(Pages$Relevance, method="sw", lam=seq(-4,4,0.01), plotit=FALSE)$result[1]
        L2 <- boxcoxnc(Pages$Nescience, method="sw", lam=seq(-4,4,0.01), plotit=FALSE)$result[1]
        L3 <- boxcoxnc(Pages$Applicability, method="sw", lam=seq(-4,4,0.01), plotit=FALSE)$result[1]
        L4 <- boxcoxnc(Pages$Maturity, method="sw", lam=seq(-4,4,0.01), plotit=FALSE)$result[1]
        
        Pages$NormRelevance <- (Pages$Relevance^L1 - 1) / L1
        Pages$NormRelevance <- (Pages$NormRelevance - min(Pages$NormRelevance) ) / (max(Pages$NormRelevance) - min(Pages$NormRelevance))
        
        Pages$NormNescience <- (Pages$Nescience^L2 - 1) / L2
        Pages$NormNescience <- (Pages$NormNescience - min(Pages$NormNescience) ) / (max(Pages$NormNescience) - min(Pages$NormNescience))

        Pages$NormApplicability <- (Pages$Applicability^L3 - 1) / L3
        Pages$NormApplicability <- (Pages$NormApplicability - min(Pages$NormApplicability) ) / (max(Pages$NormApplicability) - min(Pages$NormApplicability))
        
        Pages$NormMaturity <- (Pages$Maturity^L4 - 1) / L4
        Pages$NormMaturity <- (Pages$NormMaturity - min(Pages$NormMaturity) ) / (max(Pages$NormMaturity) - min(Pages$NormMaturity))
        
        # Interestingness
        
        Pages$Interestingness      <- Pages$NormRelevance     * Pages$NormNescience
        Pages$InterestingnessTools <- Pages$NormApplicability * Pages$NormMaturity
        
        Pages$Page <- paste('<a href="http://en.wikipedia.org/wiki/', Pages$Page, '" target="_blank">', Pages$Page, '</a>', sep='')
        
        switch(input$topicRole,
               
               "As Tool" = {
                   switch(input$toolRadioButton, 
                          "Applicability" = {                              
                              Pages[with(Pages, order(Applicability, decreasing = TRUE)), c("Page", "Applicability", "NormApplicability")]
                          },
                          "Maturity" = {
                              Pages[with(Pages, order(Maturity, decreasing = TRUE)), c("Page","Maturity", "NormMaturity")]
                          },
                          "Interestingness" = {
                              Pages[with(Pages, order(InterestingnessTools, decreasing = TRUE)), c("Page","Applicability", "Maturity", "InterestingnessTools")]
                          }
                   )
               },
               
               "As Question" = {
                   switch(input$questionRadioButton, 
                          "Relevance" = {
                              Pages[with(Pages, order(Relevance, decreasing = TRUE)), c("Page", "Relevance", "NormRelevance")]
                          },
                          "Nescience" = {
                              Pages[with(Pages, order(Nescience, decreasing = TRUE)), c("Page", "Nescience", "NormNescience")]
                          },
                          "Interestingness" = {
                              Pages[with(Pages, order(Interestingness, decreasing = TRUE)), c("Page","Relevance", "Nescience", "Interestingness")]
                          }
                   )
                   
               }
               
        )
                
    })
        
    #
    # Interesting Questions
    #
    
    datasetInputQuestions <- reactive({

        switch(input$sliderLevelQ, {
            if (input$category1Q1 == "ALL OF THEM") {
                Pages1 <- PagesL1
            } else {
                Pages1 <- PagesL1[PagesL1$Category == input$category1Q1,]
            }
            if (input$category1Q2 == "ALL OF THEM") {
                Pages2 <- PagesL1
            } else {
                Pages2 <- PagesL1[PagesL1$Category == input$category1Q2,]
            }
        }, {
            if (input$category2Q1 == "ALL OF THEM") {
                categoryList <- CategoriesL2[CategoriesL2$Level1 == input$category1Q1,]$Level2
                Pages1 <- PagesL2[PagesL2$Category %in% categoryList,]
            } else {
                Pages1 <- PagesL2[PagesL2$Category == input$category2Q1,]
            }
            if (input$category2Q2 == "ALL OF THEM") {
                categoryList <- CategoriesL2[CategoriesL2$Level1 == input$category1Q2,]$Level2
                Pages2 <- PagesL2[PagesL2$Category %in% categoryList,]
            } else {
                Pages2 <- PagesL2[PagesL2$Category == input$category2Q2,]
            }
        }, {
            if (input$category3Q1 == "ALL OF THEM") {
                categoryList <- CategoriesL3[CategoriesL3$Level2 == input$category2Q1,]$Level3
                Pages1 <- PagesL3[PagesL3$Category %in% categoryList,]
            } else {
                Pages1 <- PagesL3[PagesL3$Category == input$category3Q1,]
            }
            if (input$category3Q2 == "ALL OF THEM") {
                categoryList <- CategoriesL3[CategoriesL3$Level2 == input$category2Q2,]$Level3
                Pages2 <- PagesL3[PagesL3$Category %in% categoryList,]
            } else {
                Pages2 <- PagesL3[PagesL3$Category == input$category3Q2,]
            }
        }, {
            if (input$category4Q1 == "ALL OF THEM") {
                categoryList <- CategoriesL4[CategoriesL4$Level3 == input$category3Q1,]$Level4
                Pages1 <- PagesL4[PagesL4$Category %in% categoryList,]
            } else {
                Pages1 <- PagesL4[PagesL4$Category == input$category4Q1,]
            }
            if (input$category4Q2 == "ALL OF THEM") {
                categoryList <- CategoriesL4[CategoriesL4$Level3 == input$category3Q2,]$Level4
                Pages2 <- PagesL4[PagesL4$Category %in% categoryList,]
            } else {
                Pages2 <- PagesL4[PagesL4$Category == input$category4Q2,]
            }
        })
        
        # Clean-up data
        
        cat("Datos: ", nrow(Pages1), nrow(Pages2), "\n")
        
        Pages1 <- Pages1[complete.cases(Pages1),]
        Pages2 <- Pages2[complete.cases(Pages2),]
        
        Pages1 <- Pages1[Pages1$Description > Pages1$Complexity,]
        Pages2 <- Pages2[Pages2$Description > Pages2$Complexity,]
        
        cat("Datos Limpios: ", nrow(Pages1), nrow(Pages2), "\n")
        
        # Select the required metrics
        
        switch(input$topicRoleQ1,
               "As Tool" = {
                   Pages1$Metric1 <- Pages1$InternalLinks + 1     # boxcox requires values greater than 0
                   Pages1$Metric2 <- (Pages1$Description - Pages1$Complexity) / Pages1$Complexity
                   Pages1$Metric2 <- max(Pages1$Metric2) - (Pages1$Description - Pages1$Complexity) / Pages1$Complexity
                   Pages1$Metric2 <- Pages1$Metric2 + 0.01        # boxcox requires values greater than 0
               },
               "As Question" = {
                   Pages1$Metric1 <- Pages1$ExternalLinks + 1     # boxcox requires values greater than 0
                   Pages1$Metric2 <- (Pages1$Description - Pages1$Complexity) / Pages1$Complexity    
               }
        )

        switch(input$topicRoleQ2,
               "As Tool" = {
                   Pages2$Metric1 <- Pages2$InternalLinks + 1     # boxcox requires values greater than 0
                   Pages2$Metric2 <- (Pages2$Description - Pages2$Complexity) / Pages2$Complexity
                   Pages2$Metric2 <- max(Pages2$Metric2) - (Pages2$Description - Pages2$Complexity) / Pages2$Complexity
                   Pages2$Metric2 <- Pages1$Metric2 + 0.01        # boxcox requires values greater than 0
               },
               "As Question" = {
                   Pages2$Metric1 <- Pages2$ExternalLinks + 1     # boxcox requires values greater than 0
                   Pages2$Metric2 <- (Pages2$Description - Pages2$Complexity) / Pages2$Complexity    
               }
        )
        
        # Normalize the values
                
        L1 <- boxcoxnc(Pages1$Metric1, method="sw", lam=seq(-4,4,0.01), plotit=FALSE)$result[1]
        L2 <- boxcoxnc(Pages2$Metric1, method="sw", lam=seq(-4,4,0.01), plotit=FALSE)$result[1]
        
        Pages1$Metric1 <- (Pages1$Metric1^L1 - 1) / L1
        Pages1$Metric1 <- (Pages1$Metric1 - min(Pages1$Metric1) ) / (max(Pages1$Metric1) - min(Pages1$Metric1))
        
        Pages2$Metric1 <- (Pages2$Metric1^L2 - 1) / L2
        Pages2$Metric1 <- (Pages2$Metric1 - min(Pages2$Metric1) ) / (max(Pages2$Metric1) - min(Pages2$Metric1))
        
        L1 <- boxcoxnc(Pages1$Metric2, method="sw", lam=seq(-4,4,0.01), plotit=FALSE)$result[1]
        L2 <- boxcoxnc(Pages2$Metric2, method="sw", lam=seq(-4,4,0.01), plotit=FALSE)$result[1]
        
        Pages1$Metric2 <- (Pages1$Metric2^L1 - 1) / L1
        Pages1$Metric2 <- (Pages1$Metric2 - min(Pages1$Metric2) ) / (max(Pages1$Metric2) - min(Pages1$Metric2))
        
        Pages2$Metric2 <- (Pages2$Metric2^L2 - 1) / L2
        Pages2$Metric2 <- (Pages2$Metric2 - min(Pages2$Metric2) ) / (max(Pages2$Metric2) - min(Pages2$Metric2))
                
        # Copute interesting questions
        
        IQ <- c()
        
        for(i in 1:(nrow(Pages1)-1))  {
                        
            tt <- sapply((i+1):nrow(Pages2), function(j) {
                
                interest <- Pages1$Metric1[i] * Pages2$Metric1[j] + Pages1$Metric2[i] * Pages2$Metric2[j]
                
                c(Pages1$Page[i], Pages1$Metric1[i], Pages1$Metric2[i], Pages2$Page[j], Pages2$Metric1[j], Pages2$Metric2[j], interest)
                
            })
            
            IQ <- rbind(IQ, t(tt))
            
        }
        
        IQ <- data.frame(IQ, stringsAsFactors=FALSE)
        colnames(IQ) <- c("Topic_1", "Metric_1a", "Metric_1b", "Topic_2", "Metric_2a", "Metric_2b", "Interestingness")
        IQ[, 7] <- sapply(IQ[, 7], as.double)
        IQ[with(IQ, order(Interestingness, decreasing=TRUE)),]
        
        # Add HTML tags
        
        IQ$Topic_1 <- paste('<a href="http://en.wikipedia.org/wiki/', IQ$Topic_1, '" target="_blank">', IQ$Topic_1, '</a>', sep='')
        IQ$Topic_2 <- paste('<a href="http://en.wikipedia.org/wiki/', IQ$Topic_2, '" target="_blank">', IQ$Topic_2, '</a>', sep='')
        
        return(IQ)
        
    })
    
    #
    # Serendipity Questions
    #
    
    datasetInputSerendipity <- reactive({
        
        switch(input$sliderLevelS, {
            Pages <- PagesL1
        }, {
            Pages <- PagesL2
        }, {
            Pages <- PagesL3
        }, {
            Pages <- PagesL4
        })
        
        # Clean-up data
        
        cat("Datos: ", nrow(Pages), "\n")
        
        Pages <- Pages[!duplicated(Pages$Page),]
        Pages <- Pages[complete.cases(Pages),]
        Pages <- Pages[Pages$ExternalLinks != 0,]
        Pages <- Pages[Pages$Description > Pages$Complexity,]
        
        cat("Datos Limpios: ", nrow(Pages), "\n")
        
        # Compute the metrics
        
        Pages$Relevance <- Pages$ExternalLinks
        Pages$Nescience <- (Pages$Description - Pages$Complexity) / Pages$Complexity

        Pages <- Pages[Pages$Relevance > 100,]
        Pages <- Pages[Pages$Nescience > 1,]
        
        cat("Datos Super Filtrados: ", nrow(Pages), "\n")
        
        Pages$Interestingness <- sqrt( Pages$Relevance^2 + Pages$Nescience^2)
        
        # Normalize values
        
        Pages$NormRelevance <- log(Pages$Relevance)
        Pages$NormRelevance <- (Pages$NormRelevance - min(Pages$NormRelevance) ) / (max(Pages$NormRelevance) - min(Pages$NormRelevance))
         
        Pages$NormNescience <- log(Pages$Nescience)
        Pages$NormNescience <- (Pages$NormNescience - min(Pages$NormNescience) ) / (max(Pages$NormNescience) - min(Pages$NormNescience))

        Pages$Interestingness <- sqrt( Pages$NormRelevance^2 + Pages$NormNescience^2)
        
        Pages$Page <- paste('<a href="http://en.wikipedia.org/wiki/', Pages$Page, '" target="_blank">', Pages$Page, '</a>', sep='')
        
        # Copute interesting questions
        
        NPages <- nrow(Pages)

        IQ <- c()
        
        for(i in 1:(NPages-1))  {

            cat(c(i, " ", NPages, "\n"))
            
            tt <- sapply((i+1):NPages, function(j) {
                
                Interest  <- Pages$NormRelevance[i] * Pages$NormRelevance[j] + Pages$NormNescience[i] + Pages$NormNescience[j]
                
                c(Pages$Page[i], Pages$Category[i], Pages$Relevance[i], Pages$Nescience[i], Pages$Page[j], Pages$Category[j], Pages$Relevance[j], Pages$Nescience[j], Interest)
                
            })
            
            IQ <- rbind(IQ, t(tt))
            IQ <- IQ[tail(order(IQ[,9]), 100),]
            
        }
        
        IQ <- data.frame(IQ, stringsAsFactors=FALSE)
        colnames(IQ) <- c("Question_1", "Category_1", "Relevance_1", "Nescience_1", "Question_2", "Category_2", "Relevance_2", "Nescience_2", "Interestingness")
        IQ[, 3] <- sapply(IQ[, 3], as.double)
        IQ[, 4] <- sapply(IQ[, 4], as.double)
        IQ[, 7] <- sapply(IQ[, 7], as.double)
        IQ[, 8] <- sapply(IQ[, 8], as.double)
        IQ[, 9] <- sapply(IQ[, 9], as.double)
        
        IQ[with(IQ, order(Interestingness, decreasing=TRUE)),]
                
        return(IQ)
        
    })

    # Show the first "n" observations
    
    output$view <- renderDataTable({
        datasetInput()
    }, escape = FALSE)
    
    output$viewQuestions <- renderDataTable({
        dataset <- datasetInputQuestions()
        dataset[, input$show_vars_questions, drop = FALSE]
    }, escape = FALSE)
    
    output$viewSerendipity <- renderDataTable({
        dataset <- datasetInputSerendipity()
        dataset[, input$show_vars_serendipity, drop = FALSE]
    }, escape = FALSE)
    
})