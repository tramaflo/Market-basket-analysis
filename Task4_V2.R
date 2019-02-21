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
# Summary is the most useful #

itemLabels(BasketDataList)

summary(BasketDataList)


# Visualization -----------------------------------------------------------
# Different ways to plot the same result #

itemFrequencyPlot(BasketDataList,topN=15,type="absolute")

itemFrequencyPlot(BasketDataList,topN=15,type="relative")


# How many different items, customers buy the most ------------------------

items_freq = data.frame(Distinct_Items = c(1:15), 
                        Frequency = c(2325, 1822, 1491, 1185, 975, 
                                      766, 525, 315, 205, 123,
                                      69, 23, 8, 1, 0))

ggplot(items_freq, aes(x = Distinct_Items, y = Frequency)) +
  theme_bw() + 
  geom_bar(stat="identity", color = "black", fill = "green") + 
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

# Support 1
rules1Sup <- apriori(BasketDataList, parameter = list(supp = 0.01, conf = 0.80, minlen = 2))

inspect(rules1Sup)

summary(rules1Sup)

inspect(sort(rules1Sup, decreasing = TRUE, by = "lift")[1:20])

inspect(sort(rules1Sup, decreasing = TRUE, by = "support")[1:10])

is.redundant(rules1Sup)

plot(rules1Sup[1:5], method="paracoord", control=list(reorder=TRUE)) #non capisco
plot(rules1Sup, measure=c("support", "confidence"), shading="lift", engine = "interactive") #si
plot(rules1Sup, method="graph",interactive=FALSE,shading="lift") #no
plot(rules1Sup[1:5], method = "graph", control =list(type(Laptops))) #non funziona
plot(rules1Sup[1:5], method="grouped", measure="support")
plot(rules1Sup, measure = c("support", "lift"), shading = "confidence") #si
plot(rules1Sup, method = "two-key plot") #no

# Support 2
rules2Sup <- apriori(BasketDataList, parameter = list(supp = 0.02, conf = 0.75, minlen = 2))

inspect(rules2Sup)
summary(rules2Sup)
inspect(sort(rules2Sup, decreasing = TRUE, by = "lift")[1:28])
is.redundant(rules2Sup)

# Support 3
rules3Sup <- apriori(BasketDataList, parameter = list(supp = 0.03, conf = 0.70, minlen = 2))

inspect(rules3Sup)
summary(rules3Sup)
inspect(sort(rules3Sup, decreasing = TRUE, by = "lift")[1:20])
is.redundant(rules3Sup)

plot(rules3Sup, measure=c("support", "confidence"), shading="lift", engine = "interactive")

# Support 4
rules4Sup <- apriori(BasketDataList, parameter = list(supp = 800/9832, conf = 0.001, minlen = 2))

inspect(rules4Sup)
inspect(sort(rules4Sup, decreasing = TRUE, by = "lift")[1:10])
summary(rules4Sup)
is.redundant(rules4Sup)


# Rules per product -------------------------------------------------------

rulesAppleEarpods <- apriori(BasketData, 
                  parameter = list(support = 0.004
                                   ,confidence = 0.4))

inspect(rulesAppleEarpods)

Earpod <- subset(rulesAppleEarpods, items %in% "Apple Earpods")
inspect(sort(Earpod, by="lift"))

