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
                          rm.duplicates = TRUE)


# Get to know the dataset -------------------------------------------------
# Summary is the most useful #

inspect(BasketData) #no

length(BasketData)

size(BasketData) #no

LIST(BasketData) #no

itemLabels(BasketData)

summary(BasketData)

str(BasketData) #no


# Transactions with 1 product ---------------------------------------------

products_1item <- BasketData[which(size(BasketData) == 1), ]
crosstable <- crossTable(products_1item)
crosstable[10:14, 10:14]  #no
itemFrequencyPlot(products_1item, topN = 10, type = "absolute")


# Visualization -----------------------------------------------------------
# Different ways to plot the same result #

itemFrequencyPlot(BasketData,topN=20,type="absolute")

itemFrequencyPlot(BasketData,topN=20,type="relative")

image(BasketData[1:100]) #no

image(BasketData[1:50]) #no

image(sample(BasketData, 25)) #no

image(BasketData[1:200]) #no


# How many different items, customers buy the most ------------------------

items_freq = data.frame(Distinct_Items = c(1:30), 
                        Frequency = c(2163, 1647, 1294, 1021, 856, 
                                      646, 540, 439, 353, 247, 171,
                                      119, 77, 72, 56, 41, 26, 20, 
                                      10, 10, 10, 5, 3, 0, 1, 1, 3, 
                                      0, 1, 1))

ggplot(items_freq, aes(x = Distinct_Items, y = Frequency)) +
  theme_bw() + 
  geom_bar(stat="identity", color = "black", fill = "orange") + 
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

#1
rules1 <- apriori(BasketData, parameter = list(supp = 0.01, conf = 0.5, minlen = 2))
inspect(sort(rules1, decreasing = TRUE, by = "lift") [1:19])
inspect(sort(rules1, decreasing = TRUE, by = "confidence") [1:19])
summary(rules1)
is.redundant(rules1)

#2
rules2 <- apriori(BasketData, parameter = list(supp = 0.02, conf = 0.3, minlen = 2))
inspect(sort(rules2, decreasing = TRUE, by = "lift") [1:20])
summary(rules2)
is.redundant(rules2)

#3
rules3 <- apriori(BasketData, parameter = list(supp = 0.03, conf = 0.3, minlen = 2))
inspect(sort(rules3, decreasing = TRUE, by = "confidence"))
summary(rules3)
is.redundant(rules3)

#4
rules4 <- apriori(BasketData, parameter = list(supp = 0.01, conf = 0.001, minlen = 3))
inspect(sort(rules4, decreasing = TRUE, by = "lift") [1:20])
summary(rules4)
is.redundant(rules4)

