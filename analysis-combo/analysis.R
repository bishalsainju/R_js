library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs
library(dplyr)
library(LDAvis)

# Loading Data
load('~/Desktop/R_js/data/stm.RData')
# load('~/Desktop/R_js/data/stm_dataviz.RData')

# Pro Model Analysis
num_topic_pro = 10
pro_model = modelPro10
pro_model_cov = modelPro10_cov
proPx = proPx

# Con Model Analysis
num_topic_con = 12
con_model = modelCon12
con_model_cov = modelCon12_cov
conPx = conPx

#Model for further analysis
num_topic = num_topic_pro
model = pro_model
dataPx = proPx 

# LDA viz
toLDAvis(model, dataPx$documents, R=30 )


#Summary
summary(model)
plot(model, type="labels")
labelTopics(model)
plot(model, type="perspectives", topics=c(3,5))


# Topic Quality
topicQuality(poliblogPrevFit, docs, xlab="Semantic Coherence", 
            ylab="Exclusivity", labels=1:num_topic)


# Topic Correlations
corr <- topicCorr(poliblogPrevFit, method="simple", cutoff=.2)
plot(corr, vlabels = c(1:num_topic))


# Topic Proportions
plot(model, type="summary")


# Covariate Effect
plot(pro_model_cov, covariate = "Job_Status", topics = pro_model_cov$topics,
     model = model, method="difference", xlab="Former ............ Current",
     main="Effect of Former vs Current(Positive Feedback)",
     cov.value1 = 1, cov.value2 = 0, labeltype = "custom", custom.labels = 1:num_topic)
summary(pro_model_cov)



