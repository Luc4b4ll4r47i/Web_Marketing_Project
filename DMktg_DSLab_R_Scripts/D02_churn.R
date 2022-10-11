# We notice from the tickets dataset the presence of purchases with gross negative amount ad reimbursements with postive gross amount
# These lines are definitely wrong
#
# Let's filter purchases on the basis of the structure it should have 
# If it's a purchase (DIREZIONE==1) then gross amount and discount must be positive
# If it's a refund (DIREZIONE==-1) the gross amount and discount must be negative
df_7_1 <-df_7_tic_clean_final  %>% filter(DIREZIONE == 1,
                          IMPORTO_LORDO > 0,
                          SCONTO >= 0)
df_7_2 <- df_7_tic_clean_final %>% filter(DIREZIONE == -1,
                         IMPORTO_LORDO < 0,
                         SCONTO <= 0)

# let's put it back together
df_7_tickets <- merge(df_7_1, df_7_2, all = T)


# setting seed
set.seed(12345)

str(df_7_tickets)

min(df_7_tickets$TIC_DATE)
max(df_7_tickets$TIC_DATE)
# we observed that the first receipt was issued on 2018-05-01 and the last receipt was issued on 2019-04-30

df <- df_7_tickets
df <- df[order(df$ID_CLI,rev(df$TIC_DATETIME)),]
dft2 <- df %>%
  group_by(ID_SCONTRINO) %>%
  summarise(ID_CLI = max(ID_CLI),TIC_DATE=max(TIC_DATE))
dft2 <- dft2[order(dft2$ID_CLI,rev(dft2$TIC_DATE)),]
dft3 <- dft2 %>% group_by(ID_CLI) %>% summarise(tot = n()) %>% filter(tot>1) #number of purchases for each customer
dft3 <- left_join(dft3,dft2,by="ID_CLI") #Let's add id_scontrino & datetime
dft4 <- dft3 %>%
  arrange(desc(TIC_DATE)) %>%
  group_by(ID_CLI) %>%
  summarise(last=nth(TIC_DATE,1),secondl=nth(TIC_DATE,2))

dft4 <- dft4 %>%
  mutate(AVG_PURCHASE_DATE=as.numeric(last - secondl))

dft5 <- dft4 %>%
  group_by(AVG_PURCHASE_DATE) %>%
  summarize(NUM_CLI = sum(n_distinct(ID_CLI)))

dft5$CUM_SUM_CLI <- cumsum(dft5$NUM_CLI)

dft5$PERC_CUM_CLI <- (dft5$CUM_SUM_CLI / max(dft5$CUM_SUM_CLI)) * 100

p <- ggplot(dft4, aes(x= as.numeric(last - secondl))) +
  geom_histogram(color="black", fill="lightblue") +
  geom_vline(aes(xintercept = 60), color="blue", linetype="dashed", size=1) +
  labs(title = "Ultimo acquisto - penultimo acquisto", x = "days", y = "frequency") +
  scale_x_continuous(breaks=seq(0,300,30)) +
  theme_minimal()
p

q <- ggplot(dft4, aes(as.numeric(last-secondl), cumsum(stat(count)/nrow(dft4)))) +
  geom_freqpoly(binwidth = 8,alpha=0.8,col="#F6D935", size = 1.5) +
  labs(title = "Cumulative Repurchase Percentage") +
  geom_line(data = data.frame(days=1:365,const=0.80),aes(days,const),linetype="solid",col="#404040", size = 0.8) +
  geom_line(data = data.frame(y=seq(0,1,0.1),x=70),aes(x,y),linetype="solid",col="#404040", size = 0.8) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        panel.border = element_blank(),
        panel.grid = element_line(colour = "#E0E0E0"),
        plot.background = element_rect(fill = "#FFFFFF"),
        text = element_text(size = 13, colour = "#404040")) +
  xlab("AVERAGE DAYS NEXT PURCHASE") +
  ylab("PERCENTAGE CUSTOMERS %")
q

# let's create the churner column
# On average, 80% of customers made their last purchase 73 days after the previous one
# All customers who made purchases before 2019-02-16 are considered churners, 73 days less from the last recorded date between transactions
df_churn <- df_7_tickets %>%
  group_by(ID_CLI) %>%
  summarize(LAST_PURCHASE_DATE = max(TIC_DATE),
            TOTAL_PURCHASE = sum(IMPORTO_LORDO),
            NUMBER_OF_PURCHASE=n()) %>%
  mutate(CHURN = as.numeric(LAST_PURCHASE_DATE < as.Date("2019-02-16"))) %>%
  select(CHURN,ID_CLI,LAST_PURCHASE_DATE,TOTAL_PURCHASE,NUMBER_OF_PURCHASE)

# df master
df_master <- df_1_cli_fid_clean %>%
  select(ID_CLI, FIRST_ID_NEG, LAST_TYP_CLI_FID, LAST_COD_FID, LAST_STATUS_FID) %>%
  left_join(df_2_cli_account_clean %>%
              select(ID_CLI, ID_ADDRESS, TYP_CLI_ACCOUNT, W_PHONE)
            , by = "ID_CLI") %>%
  left_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS, REGION)
            , by = "ID_ADDRESS") %>%
  left_join(df_4_cli_privacy_clean
            , by = "ID_CLI") %>%
  left_join(open_rate_per_cli %>%
              select(ID_CLI, OPEN_RATE)
            , by = "ID_CLI") %>%
  left_join(ctr_per_cli %>%
              select(ID_CLI, CTR)
            , by = "ID_CLI") %>%
  select(-ID_ADDRESS)

# final dataset containing the churners
df_master_churn <- df_churn %>%
  left_join(df_master, by="ID_CLI")%>%
  mutate(REGION = fct_explicit_na(REGION))

df_master_churn[is.na(df_master_churn)]<-0

str(df_master_churn)
df_master_churn$OPEN_RATE=as.numeric(df_master_churn$OPEN_RATE)
df_master_churn$CTR=as.numeric(df_master_churn$CTR)

# model training
library(e1071)
library(caret)
#install.packages('pander')
library(pander)

df1 <- df_master_churn 
table(df1$CHURN)
#86820 NON CHURNER & 125304 CHURNER on 212124 clients
table(df1$CHURN)/dim(df1)[1]
#41% of customers are non-churners while 59% are churners

df1 <- df1[,-c(2,3)] #Let's remove client id and purchase date. They're not significant for this analysis
str(df1)

# conversion into factor
df1$CHURN <- as.factor(df1$CHURN)
df1$TYP_CLI_ACCOUNT <- as.factor(df1$TYP_CLI_ACCOUNT)
summary(df1)
str(df1)

df_mod <- df1

#Train e Test set
train_index <- createDataPartition(df_mod$CHURN,
                                   p = .70,
                                   list = FALSE,
                                   times = 1)
train <- df_mod[train_index,]
test <- df_mod[-train_index,]

# train set is unbalanced
churn0 <- train %>% filter(CHURN == 0) # 60774 rows
churn1 <- train %>% filter(CHURN == 1) # 87713 rows

balance1 <- churn1[sample(nrow(churn1), nrow(churn0)),] # now churn1 has 60774 rows, like churn0
train_balanced <- rbind(balance1, churn0)
train <- train_balanced #now we can train the models correctly

library(rpart)
library(rpart.plot)
#install.packages('MLmetrics')
library(MLmetrics)
library(randomForest)
#install.packages('Rcmdr')
library(Rcmdr)
#install.packages('glmnet')
library(glmnet)


#Recursive Partitioning And Regression Trees
tree <- rpart(CHURN ~ ., data= train)
rpart.plot(tree, extra = "auto")
summary(tree) #number of purchases is the most important variable
printcp(tree) #complexity parameter

#prediction rpart
pred <- rpart.predict(tree, test[,-1],type = "class")
p1 <- unlist(pred)
confusionMatrix( p1, test$CHURN, positive='1')
recall(p1,test$CHURN,relevant = '1') #0,76
precision(p1,test$CHURN,relevant = '1') #0,68
F1_Score(p1,test$CHURN,positive = '1') #0,72
acc_rpart <- Accuracy(pred,test$CHURN) #0,64
acc_rpart
memory.limit(100000)
tree_rf <- randomForest(CHURN ~ ., data= train, ntree = 100)
print(tree_rf)

#prediction rf
pred_rf <- rpart.predict(tree_rf, test[,-1], type = "class")
confusionMatrix(pred_rf, test$CHURN, positive='1')

##Random Forest

recall(pred_rf, test$CHURN,relevant = '1') #0.69
precision(pred_rf ,test$CHURN,relevant = '1') # 0.71
F1_Score(pred_rf ,test$CHURN,positive = '1') # 0.70
acc_rf <- Accuracy(pred_rf, test$CHURN) #0.65
acc_rf


train1 <- train
test1 <- test
train1$CHURN <- as.factor(train1$CHURN)
str(train1)

##Naive Bayes
nb <- naiveBayes(CHURN ~ ., train1 )
print(nb)
pred_nb <- predict(nb, test1[,-1])
confusionMatrix(pred_nb, test1$CHURN, positive='1')
recall(pred_nb, test1$CHURN,relevant = '1') #0,87
precision(pred_nb ,test1$CHURN,relevant = '1') #0,64
F1_Score(pred_nb ,test1$CHURN,positive = '1') #0,74
acc_nb <- Accuracy(pred_nb, test1$CHURN) #0,63
acc_nb


##Generalized Linear Models
gl <- glm(CHURN ~ ., train1, family = "binomial")
p1 = predict(gl, test)
pred1 = if_else(p1>0.5,1,0)
table_gl = table(pred1, test$CHURN)
pred1 <- as.factor(pred1)
confusionMatrix(table_gl, positive='1')
#evaluate
recall(pred1, test$CHURN, relevant = "1") #0.23
precision(pred1, test$CHURN, relevant = "1") # 0.77
F1_Score(pred1 ,test$CHURN,positive = '1') # 0.36
acc_glm <- Accuracy(pred1, test$CHURN) #0.51
acc_glm


#install.packages('ipred')
library(ipred)

##Bagging Classification And Regression Trees
bag <- bagging(CHURN ~ ., train1, nbagg = 25)
pred_bag <- predict(bag, test1[,-1])
confusionMatrix(pred_bag, test1$CHURN, positive='1')
recall(pred_bag, test1$CHURN,relevant = '1') #0.64
precision(pred_bag ,test1$CHURN,relevant = '1') #0,69
F1_Score(pred_bag ,test1$CHURN,positive = '1') #0,66
acc_bag <- Accuracy(pred_bag, test1$CHURN) #0,62
acc_bag


## plotting models accuracy
modello <- c("rpart", "rf", "naiveBayes", "glm", "bagging")
value <- c(0.64,0.65,0.63,0.51,0.62)
df_accuracy <- data.frame(modello, value )
summary(df_accuracy)
#png("accuracy_churn.png")
#barplot(value, main = "Accuracy dei modelli", names.arg = modello, xlab="modello", ylab = "accuracy", col.main = "blue", font.main = 4)
#dev.off()
colnames(df_accuracy) <- c("Models", "Accuracy")

ggplot(df_accuracy, aes(x = Models, y = Accuracy)) + 
  geom_bar(stat="identity", 
           fill = c("aquamarine","cyan", "blueviolet","mediumpurple", 
                    "blue"), color = "black") +
  labs(x="Models", y="Accuracy", 
       title="Accuracy of models") +
  scale_y_continuous(breaks=seq(0,0.66,.15))

                                  
## plotting models F1_score                                 
modello_f <- c("rpart", "rf", "naiveBayes", "glm", "bagging")
value_f <- c(0.71,0.68,0.75,0.28,0.67 )
df_f1score <- data.frame(modello_f, value_f )
#png("f1_churn.png")
#barplot(value_f, main = "F1 score", names.arg = modello_f, xlab = "modello", ylab = "F1 score", col.main = "blue", font.main = 4)
#dev.off()


ggplot(df_f1score, aes(x = modello_f, y = value_f)) + 
  geom_bar(stat="identity", 
           fill = c("aquamarine","cyan", "blueviolet","mediumpurple", 
                    "blue"), color = "black") +
  labs(x="Models", y="F1_score", 
       title="F1 measure of models") +
  scale_y_continuous(breaks=seq(0,0.76,.15))

## ROC curves
#install.packages('pROC')
library('pROC')
#install.packages('ROCR')
library(ROCR)

# ROC glm
roc_glm <- ROCR::prediction(predictions = pred1, 
                             labels = test$CHURN)
perf.roc_glm <- performance(roc_glm, measure = "tpr", x.measure = "fpr")
perf.auc_glm <- performance(roc_glm, measure = "auc")
ROC_df_glm <- data.frame(unlist(perf.roc_glm@x.values),
                          unlist(perf.roc_glm@y.values))
colnames(ROC_df_glm) <- c("fpr","tpr")

print(paste("AUC (glm) -->",format((perf.auc_glm@y.values)[[1]]*100,digits = 4),"%"))

# ROC random forest
roc_rf <- ROCR::prediction(predictions = pred_rf, 
                             labels = test$CHURN)
perf.roc_rf <- performance(roc_rf, measure = "tpr", x.measure = "fpr")
perf.auc_rf <- performance(roc_rf, measure = "auc")
ROC_df_rf <- data.frame(unlist(perf.roc_rf@x.values),
                          unlist(perf.roc_rf@y.values))
colnames(ROC_df_rf) <- c("fpr","tpr")

print(paste("AUC (rf) -->",format((perf.auc_rf@y.values)[[1]]*100,digits = 4),"%"))

# ROC Naive Bayes
roc_nb <- ROCR::prediction(predictions = pred_nb, 
                             labels = test$CHURN)
perf.roc_nb <- performance(roc_nb, measure = "tpr", x.measure = "fpr")
perf.auc_nb <- performance(roc_nb, measure = "auc")
ROC_df_nb <- data.frame(unlist(perf.roc_nb@x.values),
                          unlist(perf.roc_nb@y.values))
colnames(ROC_df_nb) <- c("fpr","tpr")

print(paste("AUC (nb) -->",format((perf.auc_nb@y.values)[[1]]*100,digits = 4),"%"))

# ROC bagging
roc_bag <- ROCR::prediction(predictions = pred_bag, 
                             labels = test$CHURN)
perf.roc_bag <- performance(roc_bag, measure = "tpr", x.measure = "fpr")
perf.auc_bag <- performance(roc_bag, measure = "auc")
ROC_df_bag <- data.frame(unlist(perf.roc_bag@x.values),
                          unlist(perf.roc_bag@y.values))
colnames(ROC_df_bag) <- c("fpr","tpr")

print(paste("AUC (bag) -->",format((perf.auc_bag@y.values)[[1]]*100,digits = 4),"%"))

# ROC rpart
roc_rpart <- ROCR::prediction(predictions = pred, 
                             labels = test$CHURN)
perf.roc_rpart <- performance(roc_rpart, measure = "tpr", x.measure = "fpr")
perf.auc_rpart <- performance(roc_rpart, measure = "auc")
ROC_df_rpart <- data.frame(unlist(perf.roc_rpart@x.values),
                          unlist(perf.roc_rpart@y.values))
colnames(ROC_df_rpart) <- c("fpr","tpr")

print(paste("AUC (rpart) -->",format((perf.auc_rpart@y.values)[[1]]*100,digits = 4),"%"))


ggplot() + 
  geom_line(data=ROC_df_glm, aes(x=fpr, y=tpr, color="GLM")) +
  geom_line(data=ROC_df_rf, aes(x=fpr, y=tpr, color="RF")) +
  geom_line(data=ROC_df_nb, aes(x=fpr, y=tpr, color="NB")) +
  geom_line(data=ROC_df_bag, aes(x=fpr, y=tpr, color="Bagging")) +
  geom_line(data=ROC_df_rpart, aes(x=fpr, y=tpr, color="Rpart")) +
  geom_line(data=xyline, aes(x=xline, y=yline), color='black',linetype = "dashed") +
  xlab("FPR") + ylab("TPR") +
  scale_colour_manual("Models",
                      values=c("GLM"="mediumpurple","RF"="cyan","NB"="blueviolet", "Bagging"="blue", "Rpart"="aquamarine")) +
  ggtitle("ROC")


