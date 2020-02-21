library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs
library(dplyr)
library(LDAvis)

# Loading Data
load('~/Desktop/R_js/data/stm-combo.RData')

# Pro-Con Model Analysis
num_topic = 20
model = model20
model_cov = model20_cov
proconPx = proconPx


# LDA viz
toLDAvis(model, dataPx$documents, R=30 )


#Summary
summary(model)
# plot(model, type="labels")
# labelTopics(model)
plot(model, type="perspectives", topics=c(3,5))


# Topic Quality
topicQuality(mdoel, docs, xlab="Semantic Coherence", 
            ylab="Exclusivity", labels=1:num_topic)


# Topic Correlations
corr <- topicCorr(mdoel, method="simple", cutoff=.2)
plot(corr, vlabels = c(1:num_topic))


# Topic Proportions
plot(model, type="summary")


# Covariate Effect
plot(model_cov, covariate = "Job_Status", topics = model_cov$topics,
     model = model, method="difference", xlab="Former ............ Current",
     main="Effect of Former vs Current",
     cov.value1 = 1, cov.value2 = 0, labeltype = "custom", custom.labels = 1:num_topic)

plot(model_cov, covariate = "Pro_Con", topics = model_cov$topics,
     model = model, method="difference", xlab="Former ............ Current",
     main="Pro vs Con",
     cov.value1 = "con", cov.value2 = "pro", labeltype = "custom", custom.labels = 1:num_topic)

summary(model_cov)



