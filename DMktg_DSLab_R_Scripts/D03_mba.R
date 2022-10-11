library('arules')
#install.packages('arulesViz')
library('arulesViz')

#Load dataset
#best seller
count_tickets <- df_7_tic_clean_final %>%
  group_by(ID_ARTICOLO) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_max(count, n = 100)

count_tickets %>%
  slice_max(count, n = 10) %>%
  ggplot(aes(x = reorder(ID_ARTICOLO, count), y = count)) +
  geom_bar(stat= "identity", fill = "red") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + # Centering Title
  theme(axis.text.x=element_text(angle = 60, vjust = 0.5)) +
  labs(x = "Article",
       y = "Total Purchase",
       title = "Top 10 Best Sellers")
count_tickets

tickets_ordered <- df_7_tic_clean_final[order(df_7_tic_clean_final$ID_CLI),]

itemList <- plyr::ddply(df_7_tic_clean_final, c("ID_CLI", "TIC_DATE"),
                        function(df1)paste(df1$ID_ARTICOLO, 
                                           collapse = ","))

itemList$ID_ARTICOLO <- NULL
itemList$TIC_DATE <- NULL
colnames(itemList) <- c("items")

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr, topN = 20, type = 'absolute',col='red')
rules <- apriori(tr, parameter = list(supp = 0.001, conf = 0.8))
rules <- sort(rules, by = 'confidence', decreasing = TRUE)
summary(rules)
inspect(rules)

topRules <- rules[1:10]

plot(topRules)
plot(topRules, method = "graph")