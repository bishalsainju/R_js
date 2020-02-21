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


proconPx <- prepDocuments(proconProcessed$documents, proconProcessed$vocab, proconProcessed$meta, lower.thresh = 1)


# proPx["docs.removed"]
# conPx["docs.removed"]

#### Training STM model

# Diagnosis
sk <- searchK(proconPx$documents, proconPx$vocab, K=c(8, 12, 16, 20), 
              prevalence =~ Pro_Con+Job_Status, data=proconPx$meta, 
              max.em.its = 75)
plot(sk)

##Pro-Con
### Num Topic = 16
num_topic = 16
model16 <- stm(proconPx$documents, proconPx$vocab, K=num_topic, prevalence=~ Pro_Con+Job_Status, 
                       data=proconPx$meta, init.type="Spectral", 
                       seed=42)

model16_cov <- estimateEffect(formula = 1:num_topic ~Pro_Con+Job_Status, 
                                 stmobj = model16, metadata = proconPx$meta, 
                                 uncertainty = "Global")

plot(model16_cov, covariate = "Pro_Con", topics = model16_cov$topics,
     model = model16, method="difference", xlab="Pro ............ Con",
     main="Effect of Pro vs Con",
     cov.value1 = "con", cov.value2 = "pro", labeltype = "custom", custom.labels = 1:num_topic)
summary(model16_cov)
summary(model16)


### Num Topic = 20
num_topic = 20
model20 <- stm(proconPx$documents, proconPx$vocab, K=num_topic, prevalence=~ Pro_Con+Job_Status, 
                       data=proconPx$meta, init.type="Spectral", 
                       seed=42)

model20_cov <- estimateEffect(formula = 1:num_topic ~Pro_Con+Job_Status, 
                                 stmobj = model20, metadata = proconPx$meta, 
                                 uncertainty = "Global")

plot(model20_cov, covariate = "Pro_Con", topics = model20_cov$topics,
     model = model20, method="difference", xlab="Pro ............ Con",
     main="Effect of Pro vs Con",
     cov.value1 = "con", cov.value2 = "pro", labeltype = "custom", custom.labels = 1:num_topic)
summary(model20_cov)
summary(model20)

### Saving the model
save.image('~/Desktop/R_js/data/stm-combo.RData')

### Loading the model
load('~/Desktop/R_js/data/stm-combo.RData')


### Converting estimates to df and save to csv
num_topic = 16
a <- summary(model16_cov)
vals <- t(sapply(1:num_topic, function(i) c(i, 
                                            a[["tables"]][[i]]["Pro_Conpro", "Estimate"],
                                            a[["tables"]][[i]]["Pro_Conpro", "Pr(>|t|)"],
                                            a[["tables"]][[i]]["Job_Status", "Estimate"],
                                            a[["tables"]][[i]]["Job_Status", "Pr(>|t|)"]
)))
vals

colnames(vals)=c('Topic_Num','Pro_Con', 'Pro_Con_p', 'Job_Status', 'Job_Status_p')
df = as.data.frame(vals)
df

write.csv(df,"~/Desktop/R_js/data1/estimates/combo-16.csv")


# Beta - Gamma
beta <- tidy(model20) #prob that each word is generated from the topic
write_csv(beta, path="~/Desktop/R_js/data1/beta/procon20_beta.csv")

gamma <- tidy(model20, matrix='gamma')
write_csv(gamma, path="~/Desktop/R_js/data1/gamma/procon20_gamma.csv")


