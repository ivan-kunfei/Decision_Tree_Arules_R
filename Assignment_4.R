library(dplyr)

# 1
data <- read.csv("d:/bostonuniversity/ad699_data_mining/assignment_4/telecom_users.csv")
View(data)
str(data)
# 2
data$Churn = as.factor(data$Churn)
str(data)


# 3
data$SeniorCitizen <- as.character(data$SeniorCitizen)
data_character <- select(data, where(is.character))

col_names <- colnames(data_character)
chisq_df <- data.frame(col_names = col_names, chisq_p_value=0)
chisq_df <- data.frame(chisq_df, row.names = 1)

for (each in  col_names){
  table <- table(data$Churn, data_character[,each])
  p_value <- chisq.test(table)$p.value
  chisq_df[each,1] = p_value
  }

chisq_df <- arrange(chisq_df, desc(chisq_df$chisq_p_value))
chisq_df


library(ggplot2)
ggplot(data, aes(PhoneService, fill = Churn)) + geom_bar(position = "fill")
ggplot(data, aes(gender, fill = Churn)) + geom_bar(position = "fill")
ggplot(data, aes(customerID, fill = Churn)) + geom_bar(position = "fill")

data_number <- select(data,  -where(is.character))
data_positive <- filter(data, Churn=="Yes")
data_negative <- filter(data, Churn=="No")
col_names <- colnames(select(data_number,-c("Churn")))
t_test_df <- data.frame(col_names=col_names, t_test_p_value=0)
t_test_df <- data.frame(t_test_df, row.names = 1)
for (each in col_names){
  a <- data_positive[,each]
  b <- data_negative[,each]
  p_value <- t.test(a,b)$p.value
  print(p_value)
  t_test_df[each,1] = p_value
}
arrange(t_test_df, desc(t_test_p_value))
ggplot(data, aes(x=Churn, y=X)) + geom_boxplot()

data <- select(data,-c("X","customerID"))
str(data)

# 4
set.seed(30)
length = dim(data)[1]
shuffle <- sample_n(data, length)
index = round(length*0.6)
train <-slice(shuffle, 1:index)
valid <- slice(shuffle, index+1:length)

# 5
library(rpart)
library(rpart.plot)
View(train)
model_1 <- rpart(Churn ~ ., data = train, method = "class", minsplit = 2,minbucket=1,maxdepth=30, cp=0.001)
prp(model_1, branch=0, type=0, extra=0, varlen = -1, split.font =0.1, cex = 0.1)
library(caret)
pred_train <- predict(model_1,train,type = "class")
confusionMatrix(pred_train, train$Churn)

pred_valid <- predict(model_1,valid,type = "class")
confusionMatrix(pred_valid, valid$Churn)

# 6
model_2 <- rpart(Churn ~ ., data = train, method = "class",cp=0.01, minsplit=1, maxdepth=2)
prp(model_2, branch=0, type=2, extra=1, varlen = -30, split.font =0.9, cex = 0.9)

pred_train <- predict(model_2,train,type = "class")
confusionMatrix(pred_train, train$Churn)

pred_valid <- predict(model_2,valid,type = "class")
confusionMatrix(pred_valid, valid$Churn)


# 7
model_temp <- rpart(Churn ~., data=train, method="class",minsplit=1)
printcp(model_temp)
# 0.01
model_3 <- rpart(Churn ~ ., data = train, method = "class", minsplit = 2,minbucket=1,maxdepth=30,cp=0.01)
prp(model_3, branch=0, type=2, extra=1, varlen = -30, split.font =0.9, cex = 0.9)

pred_train <- predict(model_3,train,type = "class")
confusionMatrix(pred_train, train$Churn)

pred_valid <- predict(model_3,valid,type = "class")
confusionMatrix(pred_valid, valid$Churn)

# 8


# 9
prp(model_3, branch=0, type=2, extra=4, varlen = -30, split.font =0.9, cex = 0.9)

#10

# 11
gini_inpurity <- 1- ((0.93)^2 + (0.07)^2)
gini_inpurity

# 12
# a
data_2 <- data
View(data_2)
five_num <- fivenum(data_2$MonthlyCharges)
five_num
bk <- c(-Inf,five_num[2:5])
data_2$MonthlyCharges <- cut(data_2$MonthlyCharges, breaks=bk,
                                    labels = c("lowest","slightly low","slightly high","highest"))
table(data_2$MonthlyCharges)
shuffle_2 <- sample_n(data_2, length)
train_2 <-slice(shuffle_2, 1:index)
valid_2 <- slice(shuffle_2, index:length)

# b
model_4 <- rpart(MonthlyCharges~., data=data_2)
prp(model_4, type=2, extra=1)
# c
pred_train_2 <- predict(model_4, train_2,type = "class")
confusionMatrix(pred_train_2, train_2$MonthlyCharges)

pred_valid_2 <- predict(model_4, valid_2,type = "class")
confusionMatrix(pred_valid_2, valid_2$MonthlyCharges)

# d
data_3 <- data
bk_2 <- c(-Inf,19,22,115,120)
data_3$MonthlyCharges <- cut(data_3$MonthlyCharges, breaks=bk_2, labels = c("lowest","slightly low","slightly high","highest"))
table(data_3$MonthlyCharges)
  
# e
shuffle_3 <- sample_n(data_3, length)
train_3 <-slice(shuffle_3, 1:index)
valid_3 <- slice(shuffle_3, index:length)

model_5 <- rpart(MonthlyCharges~., data=data_3)
prp(model_5, type=2, extra=1)
# f
pred_train_3 <- predict(model_5, train_3,type = "class")
confusionMatrix(pred_train_3, train_3$MonthlyCharges)

pred_valid_3 <- predict(model_5, valid_3,type = "class")
confusionMatrix(pred_valid_3, valid_3$MonthlyCharges)


#### Task 2 ####
library(arules)
data(Groceries)
# 1
str(Groceries)
# 9835 rows, 169 columns

# 2
itemFrequencyPlot(Groceries, support = 0.05)

# 3
lhs_rules <- apriori(Groceries, 
                     parameter = list(support = 0.006, confidence=0.1,minlen=2),
                     appearance = list (default="rhs",lhs= "rolls/buns"),
                     control = list (verbose=F))
rhs_rules <- apriori(Groceries, 
                     parameter = list(support = 0.006, confidence=0.1,minlen=2),
                     appearance = list (default="lhs",rhs= "rolls/buns"),
                     control = list (verbose=F))

# 4
inspect(lhs_rules)
# 5
library(arulesViz)

plot(rhs_rules[1:3])

# 6
plot(lhs_rules[1:3], method="graph",
     engine="htmlwidget")
