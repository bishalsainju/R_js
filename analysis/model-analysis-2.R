library(stm)  # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs
library(dplyr)
library(LDAvis)
library(tidyverse)
library(tidytext)

# install.packages("broom")

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

proPx <- prepDocuments(proProcessed$documents, proProcessed$vocab, proProcessed$meta, lower.thresh = 1)
conPx <- prepDocuments(conProcessed$documents, conProcessed$vocab, conProcessed$meta)

### Evaluating STM model



#### Training STM model

##Pros
num_topic = 12
modelPro12 <- stm(proPx$documents, proPx$vocab, K=num_topic, prevalence=~Job_Status, 
                  data=proPx$meta, init.type="Spectral", 
                  seed=42)

modelPro12_cov <- estimateEffect(formula = 1:num_topic ~ Job_Status, 
                                 stmobj = modelPro12, metadata = proPx$meta, 
                                 uncertainty = "Global")

num_topic = 8
modelPro8 <- stm(proPx$documents, proPx$vocab, K=num_topic, prevalence=~Job_Status, 
                  data=proPx$meta, init.type="Spectral", 
                  seed=42)

modelPro8_cov <- estimateEffect(formula = 1:num_topic ~ Job_Status, 
                                 stmobj = modelPro8, metadata = proPx$meta, 
                                 uncertainty = "Global")

num_topic = 20
modelPro20 <- stm(proPx$documents, proPx$vocab, K=num_topic, prevalence=~Job_Status, 
                 data=proPx$meta, init.type="Spectral", 
                 seed=42)

modelPro20_cov <- estimateEffect(formula = 1:num_topic ~ Job_Status, 
                                stmobj = modelPro20, metadata = proPx$meta, 
                                uncertainty = "Global")

summary(modelPro12)


####### Load Pretrained model

load('~/Desktop/R_js/data/stm2.RData')


####### Covariance effect

# Pro Model Analysis
num_topic_pro = 12
pro_model = modelPro12
pro_model_cov = modelPro12_cov
proPx = proPx

model = pro_model

#Summary
summary(model)
plot(model, type="labels")
labelTopics(model)
plot(model, type="perspectives", topics=c(3,5))

#Covariance Effect
plot(pro_model_cov, covariate = "Job_Status", topics = pro_model_cov$topics,
     model = pro_model, method="difference", xlab="Former ............ Current",
     main="Effect of Former vs Current(Positive Feedback)",
     cov.value1 = 1, cov.value2 = 0, labeltype = "custom", custom.labels = 1:num_topic)
summary(pro_model_cov)

#Saving the model
save.image('~/Desktop/R_js/data/stm1.RData')
