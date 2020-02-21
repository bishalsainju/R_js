library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs
library(dplyr)
library(LDAvis)
library(tidyverse)
library(tidytext)

# install.packages('stm')
# install.packages('igraph')
# install.packages('stmCorrViz')
# install.packages('dplyr')
# install.packages('LDAvis')
# install.packages('tidyverse')
# install.packages('tidytext')

### Data Reading
proFilePath = "~/Desktop/R_js/data/pro_doc_sampled.csv"
conFilePath = "~/Desktop/R_js/data/con_doc_sampled.csv"
proData <- read.csv(proFilePath) 
conData <- read.csv(conFilePath)


### Data Preprocessing
proProcessed <- textProcessor(proData$Px_Texts, metadata = proData, lowercase = FALSE,
                           removestopwords = FALSE, removenumbers = FALSE, removepunctuation = FALSE,
                           stem = FALSE, wordLengths = c(3, Inf), sparselevel = 1,
                           language = "en", verbose = TRUE, onlycharacter = FALSE,
                           striphtml = TRUE, customstopwords = NULL, v1 = FALSE)
conProcessed <- textProcessor(conData$Px_Texts, metadata = conData, lowercase = FALSE,
                              removestopwords = FALSE, removenumbers = FALSE, removepunctuation = FALSE,
                              stem = FALSE, wordLengths = c(3, Inf), sparselevel = 1,
                              language = "en", verbose = TRUE, onlycharacter = FALSE,
                              striphtml = TRUE, customstopwords = NULL, v1 = FALSE)

proPx <- prepDocuments(proProcessed$documents, proProcessed$vocab, proProcessed$meta)
conPx <- prepDocuments(conProcessed$documents, conProcessed$vocab, conProcessed$meta)

# proPx["docs.removed"]
# conPx["docs.removed"]

#### Training STM model

##Pros
num_topic = 26
modelPro26 <- stm(proPx$documents, proPx$vocab, K=num_topic, prevalence=~Ratings+Job_Status+Reviewed_Year, 
                       data=proPx$meta, init.type="Spectral", 
                       seed=42)

modelPro26_cov <- estimateEffect(formula = 1:num_topic ~ Ratings + Job_Status+Reviewed_Year, 
                                 stmobj = modelPro26, metadata = proPx$meta, 
                                 uncertainty = "Global")


num_topic = 8
modelPro8 <- stm(proPx$documents, proPx$vocab, K=num_topic, prevalence=~Ratings+Job_Status+Reviewed_Year, 
                  data=proPx$meta, init.type="Spectral", 
                  seed=42)

modelPro8_cov <- estimateEffect(formula = 1:num_topic ~ Ratings + Job_Status+Reviewed_Year, 
                                 stmobj = modelPro8, metadata = proPx$meta, 
                                 uncertainty = "Global")


num_topic = 16
modelPro16 <- stm(proPx$documents, proPx$vocab, K=num_topic, prevalence=~Ratings+Job_Status+Reviewed_Year, 
                 data=proPx$meta, init.type="Spectral", 
                 seed=42)

modelPro16_cov <- estimateEffect(formula = 1:num_topic ~ Ratings + Job_Status+Reviewed_Year, 
                                stmobj = modelPro16, metadata = proPx$meta, 
                                uncertainty = "Global")


num_topic = 38
modelPro38 <- stm(proPx$documents, proPx$vocab, K=num_topic, prevalence=~Ratings+Job_Status+Reviewed_Year, 
                  data=proPx$meta, init.type="Spectral", 
                  seed=42)

modelPro38_cov <- estimateEffect(formula = 1:num_topic ~ Ratings + Job_Status+Reviewed_Year, 
                                 stmobj = modelPro38, metadata = proPx$meta, 
                                 uncertainty = "Global")


#Cons
num_topic = 8
modelCon8 <- stm(conPx$documents, conPx$vocab, K=num_topic, prevalence=~Job_Status, 
                  data=conPx$meta, init.type="Spectral", 
                  seed=42)

modelCon8_cov <- estimateEffect(formula = 1:num_topic ~ Job_Status, 
                                 stmobj = modelCon10, metadata = conPx$meta, 
                                 uncertainty = "Global")


num_topic = 12
modelCon12 <- stm(conPx$documents, conPx$vocab, K=num_topic, prevalence=~Job_Status, 
                  data=conPx$meta, init.type="Spectral", 
                  seed=42)

modelCon12_cov <- estimateEffect(formula = 1:num_topic ~Job_Status, 
                                 stmobj = modelCon12, metadata = conPx$meta, 
                                 uncertainty = "Global")



num_topic = 16
modelCon16 <- stm(conPx$documents, conPx$vocab, K=num_topic, prevalence=~Job_Status, 
                  data=conPx$meta, init.type="Spectral", 
                  seed=42)

modelCon16_cov <- estimateEffect(formula = 1:num_topic ~ Job_Status, 
                                 stmobj = modelCon16, metadata = conPx$meta, 
                                 uncertainty = "Global")


num_topic = 20
modelCon20 <- stm(conPx$documents, conPx$vocab, K=num_topic, prevalence=~Job_Status, 
                  data=conPx$meta, init.type="Spectral", 
                  seed=42)

modelCon20_cov <- estimateEffect(formula = 1:num_topic ~ Job_Status, 
                                 stmobj = modelCon20, metadata = conPx$meta, 
                                 uncertainty = "Global")


### Saving the model
save.image('~/Desktop/R_js/data/stm-con.RData')


### Load the model
load('~/Desktop/R_js/data/stm.RData') 


# Pro Model Analysis
model = modelCon16

beta <- tidy(model) #prob that each word is generated from the topic
write_csv(pro_beta, path="~/Desktop/R_js/data/beta/con16_beta.csv")

gamma <- tidy(model, matrix='gamma')
write_csv(gamma, path="~/Desktop/R_js/data/gamma/con16_gamma.csv")



