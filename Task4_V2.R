# Market Basket Analysis #
# Floriana Trama #
# Data analysis department #
# G1: Understand Electronidex customers looking at possible association in its transactions #
# G2: Understand if Electronidex would be an optimal acquisition #
# 22 February 2019 #




# Libraries ---------------------------------------------------------------

library(arules)

library(arulesViz)

library(caret)

library(shiny)


# Upload Data set ---------------------------------------------------------

BasketData <- read.transactions("C:/Users/T450S/Desktop/Floriana/Ubiqum/Data Analytics II/Task 4/ElectronidexTransactions2017.csv",
                          format = "basket", sep=";" , 
                          cols = NULL, rm.duplicates = F)

PdtCategoryList <- read.csv("C:/Users/T450S/Desktop/Product Category List.csv", 
                            ";", header = TRUE)

BasketDataList <- BasketData

BasketDataList@itemInfo$labels <- PdtCategoryList$ProductCategory

BasketDataList <- aggregate(BasketDataList, by = BasketDataList@itemInfo$labels)


# Get to know the dataset -------------------------------------------------

itemLabels(BasketDataList)

summary(BasketDataList)


# Transactions with 1 category --------------------------------------------

categories_1cat <- BasketDataList[which(size(BasketDataList) == 1), ]

crosstable <- crossTable(categories_1cat)

itemFrequencyPlot(categories_1cat, topN = 10, type = "absolute")


# Categories frequency ----------------------------------------------------

itemFrequencyPlot(BasketDataList,topN=10,type="absolute")

itemFrequencyPlot(BasketDataList,topN=15,type="relative")


# How many different items, customers buy the most ------------------------

items_freq = data.frame(Distinct_Items = c(1:15), 
                        Frequency = c(2325, 1822, 1491, 1185, 975, 
                                      766, 525, 315, 205, 123,
                                      69, 23, 8, 1, 0))

ggplot(items_freq, aes(x = Distinct_Items, y = Frequency)) +
  theme_bw() + 
  geom_bar(stat="identity", color = "black", fill = "orange") + 
  labs(y = "Frequency",
       x = "Distinct items",
       title = "Composition of baskets by distinct items")


# Mode --------------------------------------------------------------------

sizes <- size(BasketDataList)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

result <- Mode(sizes)

print(result)


# Apriori algorithm - Rules per product category --------------------------

ruleExplorer(BasketDataList)

# Support 1 #usa questa regola
rules1Sup <- apriori(BasketDataList, parameter = list(supp = 0.01, conf = 0.50, minlen = 2, maxlen = 2))
summary(rules1Sup)
inspect(sort(rules1Sup, decreasing = TRUE, by = "lift")[1:20])
inspect(sort(rules1Sup, decreasing = TRUE, by = "support")[1:10])
is.redundant(rules1Sup)


# Support 2 # usa questa regola
rules2Sup <- apriori(BasketDataList, parameter = list(supp = 0.02, conf = 0.50, minlen = 2))
summary(rules2Sup)
inspect(sort(rules2Sup, decreasing = TRUE, by = "lift")[1:20])
is.redundant(rules2Sup)

# Support 3
rules3Sup <- apriori(BasketDataList, parameter = list(supp = 0.03, conf = 0.001, minlen = 2, maxlen = 2))
summary(rules3Sup)
inspect(sort(rules3Sup, decreasing = TRUE, by = "lift")[1:20])
is.redundant(rules3Sup)


# Rules Visualization -----------------------------------------------------

plot(rules1Sup[1:5], method="paracoord", control=list(reorder=TRUE))
plot(rules1Sup, measure=c("support", "confidence"), shading="lift", engine = "interactive")
plot(rules1Sup[1:5], method="grouped", measure="support")
plot(rules1Sup, measure = c("support", "lift"), shading = "confidence")
plot(rules3Sup, measure=c("support", "confidence"), shading="lift", engine = "interactive")

