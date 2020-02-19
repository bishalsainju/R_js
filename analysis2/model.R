library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs
library(dplyr)
library(LDAvis)
library(tidyverse)
library(tidytext)

### Data Reading
proconFilePath = "~/Desktop/R_js/data/combined_sampled.csv"
proconData <- read.csv(proconFilePath) 

### Data Preprocessing
proconProcessed <- textProcessor(proconData$Px_Texts, metadata = proconData, lowercase = FALSE,
                           removestopwords = FALSE, removenumbers = FALSE, removepunctuation = FALSE,
                           stem = FALSE, wordLengths = c(3, Inf), sparselevel = 1,
                           language = "en", verbose = TRUE, onlycharacter = FALSE,
                           striphtml = TRUE, customstopwords = NULL, v1 = FALSE)


proconPx <- prepDocuments(proconProcessed$documents, proconProcessed$vocab, proconProcessed$meta)


# proPx["docs.removed"]
# conPx["docs.removed"]

#### Training STM model

##Pros
num_topic = 12
model12 <- stm(proconPx$documents, proconPx$vocab, K=num_topic, prevalence=~ Pro_Con+Job_Status, 
                       data=proconPx$meta, init.type="Spectral", 
                       seed=42)

model12_cov <- estimateEffect(formula = 1:num_topic ~Pro_Con+Job_Status, 
                                 stmobj = model12, metadata = proconPx$meta, 
                                 uncertainty = "Global")



# Beta - Gamma
pro_beta <- tidy(pro_model) #prob that each word is generated from the topic
pro_beta
write_csv(pro_beta, path="~/Desktop/R_js/data/beta/procon12_beta.csv")

pro_gamma <- tidy(pro_model, matrix='gamma')
pro_gamma
write_csv(pro_gamma, path="~/Desktop/R_js/data/gamma/procon12_gamma.csv")


