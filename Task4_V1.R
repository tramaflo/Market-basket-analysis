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


# Upload Data set ---------------------------------------------------------

BasketData <- read.transactions("C:/Users/T450S/Desktop/Floriana/Ubiqum/Data Analytics II/Task 4/ElectronidexTransactions2017.csv",
                          format = "basket", sep=";", 
                          skip = 1)


# Get to know the dataset -------------------------------------------------
# Summary is the most useful #

inspect(BasketData)

length(BasketData)

size(BasketData)

LIST(BasketData)

itemLabels(BasketData)

summary(BasketData)

str(BasketData)


# Visualization -----------------------------------------------------------
# Different ways to plot the same result #

itemFrequencyPlot(BasketData,topN=20,type="absolute")

itemFrequencyPlot(BasketData,topN=20,type="relative")

image(BasketData[1:100])

image(BasketData[1:50])

image(sample(BasketData, 25))

image(BasketData[1:200])


# How many different items, customers buy the most ------------------------

items_freq = data.frame(Distinct_Items = c(1:30), 
                        Frequency = c(2163, 1647, 1294, 1021, 856, 
                                      646, 540, 439, 353, 247, 171,
                                      119, 77, 72, 56, 41, 26, 20, 
                                      10, 10, 10, 5, 3, 0, 1, 1, 3, 
                                      0, 1, 1))

ggplot(items_freq, aes(x = Distinct_Items, y = Frequency)) +
  theme_bw() + 
  geom_bar(stat="identity", color = "black", fill = "red") + 
  labs(y = "Frequency",
       x = "Distinct items bought in transaction",
       title = "Frequency of transactions with number of distinct items")


# Mode --------------------------------------------------------------------

sizes <- size(BasketData)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

result <- Mode(sizes)
print(result)


# Apriori algorithm -------------------------------------------------------

rules <- apriori(BasketData, parameter = list(supp = 0.1, conf = 0.8, minlen = 2))

rules <- apriori(BasketData, parameter = list(supp = 0.001, conf = 0.80))

rules <- apriori(BasketData, parameter = list(supp = 0.005, conf = 0.7, minlen = 2))
inspect(rules)
summary(rules)
inspect(sort(rules, decreasing = TRUE, by = "support"))
inspect(sort(rules, decreasing = TRUE, by = "confidence"))

is.redundant(rules)


# Rules per product -------------------------------------------------------

rules3 <- apriori(BasketData, 
                  parameter = list(support = 0.004
                                   ,confidence = 0.4)) 

inspect(rules3)

Earpod <- subset(rules3, items %in% "Apple Earpods")
inspect(sort(Earpod, by="lift"))

plot(Earpod, method="paracoord", control=list(reorder=TRUE))
