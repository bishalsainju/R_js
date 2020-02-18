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
                  content=proPx$meta$Job_Status, data=proPx$meta, init.type="Spectral", 
                  seed=42)

modelPro12_cov <- estimateEffect(formula = 1:num_topic ~ Job_Status, 
                                 stmobj = modelPro12, metadata = proPx$meta, 
                                 uncertainty = "Global")


summary(modelPro12)


####### Load Pretrained model

load('~/Desktop/R_js/data/stm1.RData')


####### Covariance effect

# Pro Model Analysis
num_topic_pro = 16
pro_model = modelPro16
pro_model_cov = modelPro16_cov
dataPx = proPx

num_topic = num_topic_pro
model = pro_model

#Summary
summary(model)
plot(model, type="labels")
labelTopics(model)
plot(model, type="perspectives", topics=c(3,5))


#Topic Evaluation
# Topic Quality
topicQuality(model, dataPx$documents, xlab="Semantic Coherence", ylab="Exclusivity", labels=1:num_topic_pro)

# Topic Correlations
corr <- topicCorr(model, method="simple", cutoff=.2)
plot(corr, vlabels = c(1:num_topic))

# Topic Proportions
plot(model, type="summary")


#Covariance Effect
plot(pro_model_cov, covariate = "Job_Status", topics = pro_model_cov$topics,
     model = pro_model, method="difference", xlab="Former ............ Current",
     main="Effect of Former vs Current(Positive Feedback)",
     cov.value1 = 1, cov.value2 = 0, labeltype = "custom", custom.labels = 1:num_topic)
summary(pro_model_cov)


# Pro Model Analysis
pro_beta <- tidy(pro_model) #prob that each word is generated from the topic
pro_beta
write_csv(pro_beta, path="~/Desktop/R_js/data1/beta/pro16_beta.csv")

pro_gamma <- tidy(pro_model, matrix='gamma')
pro_gamma
write_csv(pro_gamma, path="~/Desktop/R_js/data1/gamma/pro16_gamma.csv")
