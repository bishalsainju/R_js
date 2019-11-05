install.packages("LDAvis") 
install.packages("servr")

library(LDAvis)
library(servr)
library(RJSONIO)

help(createJSON, package="LDAvis")

data(TwentyNewsgroups, package="LDAvis")

json <- with(TwentyNewsgroups, 
             createJSON(phi, theta, doc.length, vocab, term.frequency))


jsonPath = "~/Desktop/R_js/data/data.json"
exportJSON <- toJSON(json)
write(exportJSON, jsonPath)

library("rjson")