##### DATA PREPARATION #####

# RECENCY

#Recency means understanding how recently the customer purchase for the last time

# We select a reference date
reference_rfm <- max(df_7_tic_clean_final$TIC_DATE) 
#we take the last date to assure us that the 80%-90% of 
#the customers repurchase within this time interval from last purchase 
#[1] "2019-04-30" max date
#[1] "2018-05-01" min date

# We create a column that contain the difference between the reference and the other dates

df_7_diffdays <- df_7_tic_clean_final %>% 
  filter(DIREZIONE == 1) %>%
  select(c("ID_CLI", "TIC_DATE"))  %>%
  unique() %>%
  mutate(DIFF_DAYS = as.numeric(difftime(reference_rfm,TIC_DATE, units = "days")))


#We compute the most recent purchase for every costumers

df_7_diffdays_min <- df_7_diffdays %>% 
  group_by(ID_CLI) %>%
  summarise(DIFF_DAYS_MIN = min(DIFF_DAYS))

#We create the column RECENCY

df_7_diffdays_min <- df_7_diffdays_min %>%
  mutate(RECENCY = case_when((DIFF_DAYS_MIN >= 0) & (DIFF_DAYS_MIN < 37) ~ "Last month",
                             (DIFF_DAYS_MIN >= 37) & (DIFF_DAYS_MIN < 208) ~ "Between second and seventh month",
                             (DIFF_DAYS_MIN >= 208) & (DIFF_DAYS_MIN < 365) ~ "More than seven months",
                             DIFF_DAYS_MIN >= 365 ~ "Inactive"))
# FREQUENCY

# Frequency indicate how often the customers purchase

df_7_purchase <- df_7_tic_clean_final %>% 
  filter(DIREZIONE == 1) %>%
  select(c("ID_CLI", "ID_ARTICOLO")) %>%
  unique() %>%
  group_by(ID_CLI) %>%
  summarise(num = n())

# We create the column of interest 

df_7_purchase <- df_7_purchase %>%
  mutate(FREQUENCY = case_when(num == 0 ~ "Inactive",
                               (num >= 1) & (num < 4) ~ "At least 1",
                               (num >= 4) & (num < 20) ~ "At least 4",
                               num >= 20 ~ "At least 20"))


# MONETARY

# Monetary indicate how much a customer spend

df7_monetary <- df_7_tic_clean_final %>%
  filter(IMPORTO_LORDO >= 0) %>%
  select(c("ID_CLI", "IMPORTO_LORDO", "SCONTO")) %>%
  group_by(ID_CLI) %>%
  summarise(TOT_NETTO = sum(IMPORTO_LORDO) - sum(SCONTO))

# We create the column of interest 

df7_monetary <- df7_monetary %>%
  mutate(MONETARY = case_when((TOT_NETTO >= 0) & (TOT_NETTO < 77) ~ "Cheap",
                              (TOT_NETTO >= 77) & (TOT_NETTO < 632) ~ "Medium",
                              TOT_NETTO >= 632 ~ "Expensive"))

# Now we join all the information computed before

library(plyr)
dt_rfm <- join(df_7_diffdays_min, df_7_purchase, by = "ID_CLI", type='inner') %>% left_join(df7_monetary, by = "ID_CLI",type='inner')%>% select(-num)
detach(package:plyr)

######### DATA EXPLORATION #######
# Explore the variable RECENCY

plot_recency <- dt_rfm %>% group_by(RECENCY) %>%
  summarise(NUM_CLI = n()) %>% 
  ggplot(aes(x = factor(RECENCY, levels = c("Last month", "Between second and seventh month", "More than seven months")), y = NUM_CLI)) +
  geom_bar(stat = "identity", fill = c('blue','red','black')) +
  xlab("RECENCY") +
  ylab("NUMBER OF CLIENTS") +
  ggtitle("RECENCY")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(legend.title = element_blank()) 

plot_recency

# Explore the variable FREQUENCY
plot_frequency <- dt_rfm %>% group_by(FREQUENCY) %>% 
  summarise(NUM_CLI = n_distinct(ID_CLI)) %>% 
  ggplot(aes(x = factor(FREQUENCY, levels = c("At least 1", "At least 4", "At least 20")), y = NUM_CLI)) +
  geom_bar(stat = "identity", fill = c('blue','red','black')) +
  xlab("FREQUENCY") +
  ylab("NUMBER OF CLIENTS") +
  ggtitle("FREQUENCY")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(legend.title = element_blank()) 

plot_frequency

# Explore the variable MONETARY
plot_monetary <- dt_rfm %>% group_by(MONETARY) %>% 
  summarise(NUM_CLI = n_distinct(ID_CLI)) %>% 
  ggplot(aes(x = factor(MONETARY, levels = c("Cheap", "Medium", "Expensive")), y = NUM_CLI)) +
  geom_bar(stat = "identity", fill = c('blue','red','black')) +
  xlab("MONETARY") +
  ylab("NUMBER OF CLIENTS") +
  ggtitle("MONETARY")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(legend.title = element_blank()) 

plot_monetary

#### RFM MODEL #######

#  We divide the customers base in five categories

dt_rfm <- dt_rfm %>%
  filter(RECENCY != "Inactive") %>%
  filter(FREQUENCY != "Inactive") %>%
  mutate(LOYALTY = case_when((FREQUENCY == "At least 1") &  (RECENCY == "Last month") ~ "Occasional",
                             (FREQUENCY == "At least 1") &  (RECENCY == "Between second and seventh month") ~ "Occasional",
                             (FREQUENCY == "At least 1") &  (RECENCY == "More than seven months") ~ "Leaving",
                             (FREQUENCY == "At least 4") &  (RECENCY == "Last month") ~ "Engaged",
                             (FREQUENCY == "At least 4") &  (RECENCY == "Between second and seventh month") ~ "Engaged",
                             (FREQUENCY == "At least 4") &  (RECENCY == "More than seven months") ~ "Leaving",
                             (FREQUENCY == "At least 20") &  (RECENCY == "Last month") ~ "Top",
                             (FREQUENCY == "At least 20") &  (RECENCY == "Between second and seventh month") ~ "Top",
                             (FREQUENCY == "At least 20") &  (RECENCY == "More than seven months") ~ "Leaving Top"))

# We plot now the distribution

plot_loyalty <- dt_rfm %>% group_by(LOYALTY) %>%
  summarise(NUM_CLI = n_distinct(ID_CLI)) %>% 
  ggplot(aes(x = factor(LOYALTY, levels = c("Occasional", "Leaving", "Engaged", "Top", "Leaving Top")), y = NUM_CLI)) +
  geom_bar(stat = "identity", fill = c("green", "red", "lightgreen", "yellow", "blue")) +
  xlab("LOYALTY") +
  ylab("NUMBER OF CLIENTS") +
  ggtitle("LOYALTY") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(legend.title = element_blank()) 
plot_loyalty

# Now we classify the customers using monetary values and defining RFM_classes

dt_rfm <- dt_rfm %>%
  mutate(RFM = case_when((LOYALTY == "Occasional") &  (MONETARY == "Cheap") ~ "Cheap",
                         (LOYALTY == "Occasional") &  (MONETARY == "Medium") ~ "Tin",
                         (LOYALTY == "Occasional") &  (MONETARY == "Expensive") ~ "Copper",
                         (LOYALTY == "Leaving") &  (MONETARY == "Cheap") ~ "Tin",
                         (LOYALTY == "Leaving") &  (MONETARY == "Medium") ~ "Copper",
                         (LOYALTY == "Leaving") &  (MONETARY == "Expensive") ~ "Bronze",
                         (LOYALTY == "Engaged") &  (MONETARY == "Cheap") ~ "Copper",
                         (LOYALTY == "Engaged") &  (MONETARY == "Medium") ~ "Bronze",
                         (LOYALTY == "Engaged") &  (MONETARY == "Expensive") ~ "Silver",
                         (LOYALTY == "Leaving Top") &  (MONETARY == "Cheap") ~ "Bronze",
                         (LOYALTY == "Leaving Top") &  (MONETARY == "Medium") ~ "Silver",
                         (LOYALTY == "Leaving Top") &  (MONETARY == "Expensive") ~ "Gold",
                         (LOYALTY == "Top") &  (MONETARY == "Cheap") ~ "Silver",
                         (LOYALTY == "Top") &  (MONETARY == "Medium") ~ "Gold",
                         (LOYALTY == "Top") &  (MONETARY == "Expensive") ~ "Diamond"))%>%
  mutate(RFM = as.factor(RFM))

class <-  c("Diamond", "Gold", "Silver", "Bronze", "Copper", "Tin", "Cheap")

# Plotting the classification

plot_rfm <- dt_rfm %>% group_by(RFM) %>%
  summarise(NUM_CLI = n_distinct(ID_CLI)) %>%
  ggplot(aes(x = RFM,
             y = NUM_CLI/sum(NUM_CLI))) +
  geom_bar(stat = "identity", fill = c("blue", "lightblue", "lightgreen", "green", "gray", "black", "red"))  +
  xlab("RFM CLASSES") +
  ylab("PERCENTAGE OF CLIENTS") +
  ggtitle("RFM MODEL")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(legend.title = element_blank()) 

plot_rfm

