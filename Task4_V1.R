# Market Basket Analysis #
# Floriana Trama #
# Data analysis department #
# G1: Understand Electronidex customers looking at possible association in its transactions #
# G2: Understand if Electronidex would be an optimal acquisition #
# 22 February 2019 #




# Libraries ---------------------------------------------------------------

library(arules)

library(arulesViz)


# Upload Data set ---------------------------------------------------------

BasketData <- read.transactions("C:/Users/T450S/Desktop/Floriana/Ubiqum/Data Analytics II/Task 4/ElectronidexTransactions2017.csv",
                          format = "basket", sep=";", 
                          rm.duplicates=TRUE)


# Get to know the dataset -------------------------------------------------

inspect(BasketData)

length(BasketData)

size(BasketData)

LIST(BasketData)

itemLabels(BasketData)

summary(BasketData)

str(BasketData)


# Visualization -----------------------------------------------------------

itemFrequencyPlot(BasketData,topN=20,type="absolute")

itemFrequencyPlot(BasketData,topN=20,type="relative")

image(BasketData[1:15])

image(BasketData[1:50])

image(sample(BasketData, 25))

image(BasketData[1:200])


# Apriori algorithm -------------------------------------------------------

rules <- apriori(BasketData, parameter = list(supp = 0.1, conf = 0.8))

rules <- apriori(BasketData, parameter = list(supp = 0.01, conf = 0.50))

rules <- apriori(BasketData, parameter = list(supp = 0.005, conf = 0.65))
inspect(rules)
summary(rules)
inspect(sort(rules, decreasing = TRUE, by = "support"))
inspect(sort(rules, decreasing = TRUE, by = "confidence"))

plot(rules, method = "two-key plot")
