library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs
library(dplyr)
library(LDAvis)
library(tidyverse)
library(tidytext)

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
num_topic = 10
modelCon10 <- stm(conPx$documents, conPx$vocab, K=num_topic, prevalence=~Ratings+Job_Status+Reviewed_Year, 
                  data=conPx$meta, init.type="Spectral", 
                  seed=42)

modelCon10_cov <- estimateEffect(formula = 1:num_topic ~ Ratings + Job_Status+Reviewed_Year, 
                                 stmobj = modelCon10, metadata = conPx$meta, 
                                 uncertainty = "Global")


num_topic = 16
modelCon16 <- stm(conPx$documents, conPx$vocab, K=num_topic, prevalence=~Ratings+Job_Status+Reviewed_Year, 
                  data=conPx$meta, init.type="Spectral", 
                  seed=42)

modelCon16_cov <- estimateEffect(formula = 1:num_topic ~ Ratings + Job_Status+Reviewed_Year, 
                                 stmobj = modelCon16, metadata = conPx$meta, 
                                 uncertainty = "Global")


num_topic = 24
modelCon24 <- stm(conPx$documents, conPx$vocab, K=num_topic, prevalence=~Ratings+Job_Status+Reviewed_Year, 
                  data=conPx$meta, init.type="Spectral", 
                  seed=42)

modelCon24_cov <- estimateEffect(formula = 1:num_topic ~ Ratings + Job_Status+Reviewed_Year, 
                                 stmobj = modelCon24, metadata = conPx$meta, 
                                 uncertainty = "Global")


num_topic = 38
modelCon38 <- stm(conPx$documents, conPx$vocab, K=num_topic, prevalence=~Ratings+Job_Status+Reviewed_Year, 
                  data=conPx$meta, init.type="Spectral", 
                  seed=42)

modelCon38_cov <- estimateEffect(formula = 1:num_topic ~ Ratings + Job_Status+Reviewed_Year, 
                                 stmobj = modelCon38, metadata = conPx$meta, 
                                 uncertainty = "Global")


### Saving the model
save.image('~/Desktop/R_js/data/stm.RData')

# A Shiny Application for STM 
library(stminsights)
run_stminsights()


### Load the model
# models = load('~/Desktop/R_js/data/stm.RData') 
# models
load('~/Desktop/R_js/data/stm.RData') 

# Pro Model Analysis
pro_model = modelPro26
pro_model

pro_beta <- tidy(pro_model) #prob that each word is generated from the topic
pro_beta
write_csv(pro_beta, path="~/Desktop/R_js/data/beta/pro26_beta.csv")

pro_gamma <- tidy(pro_model, matrix='gamma')
pro_gamma
write_csv(pro_gamma, path="~/Desktop/R_js/data/gamma/pro26_gamma.csv")


# Con Model Analysis
con_model = modelCon24

con_beta <- tidy(con_model) #prob that each word is generated from the topic
con_beta
write_csv(con_beta, path="~/Desktop/R_js/data/beta/con24_beta.csv")

con_gamma <- tidy(con_model, matrix='gamma')
con_gamma
write_csv(con_beta, path="~/Desktop/R_js/data/beta/con24_gamma.csv")





