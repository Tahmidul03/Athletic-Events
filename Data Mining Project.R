df <- read.csv(file="C:\\Users\\tahmi\\OneDrive\\Desktop\\Data Mining CUS610\\Project\\athlete_events.csv",header=TRUE, sep=",")
"C:\\Users\\tahmi\\OneDrive\\Desktop\\Data Mining CUS610\\Project\\athlete_events.csv"
print(df)
library(readr) 
library(dplyr)
library(stringr)
library(caret) 
library(rpart) 
install.packages("rpart.plot")
library(rpart.plot) 
library(class) 
library(e1071) 
library(ggplot2)
library(randomForest)
install.packages("gbm")
library(gbm)

# Check for any missing data
sum(is.na(df))

#Replaces all NA from Height and Weight Columns to Random Integer
set.seed(42)
df <- df %>%
  mutate(
    Height = ifelse(is.na(Height), sample(150:200, sum(is.na(Height)), replace = TRUE), Height), 
    Weight = ifelse(is.na(Weight), sample(50:100, sum(is.na(Weight)), replace = TRUE), Weight)
  )
print(df)

# Remove Outliers from Height and Weight Coloumns
remove_outliers <- function(x, na.rm = TRUE) {
  quantile <- quantile(x, probs = c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  x[x < (quantile[1] - H) | x > (quantile[2] + H)] <- NA
  return(x)
}

df$Height <- remove_outliers(df$Height)
df$Weight <- remove_outliers(df$Weight)

print(df$Height)
print(df$Weight)


# Remove rows with NA values introduced by outlier removal
df <- na.omit(df)
print(df)

#Remove any Negative Values if any

df[df < 0] <- NA
print(df)

#If any duplicate rows are occuring, Remove them
df <- distinct(df)
print(df)

#Create a new feauture/attribute for Atheletes who earned a medal
df <- df %>% mutate(Has_Medal = ifelse(!is.na(Medal), "Yes", "No")) 
head(df)

# Discretize the Age column into age groups
df <- df %>%
  mutate(Age_Group = cut(Age, breaks = c(0, 20, 25, 30, 35, 40, 50, Inf), 
                  labels = c("0-20", "21-25", "26-30", "31-35", "36-40", "41-50", "50+"), right = FALSE))

# Check the discretized data
table(df$Age_Group)

#Optional Filter the dataset based on All Summer and Winter Games Seperately
#summer_olympics <- df %>% filter(Season == "Summer")
#winter_olympics <- df %>% filter(Season == "Summer")
#head(summer_olympics)
#head(winter_olympics)

#Lets Filter all the Columns right before we perform algorithms
df <- df %>% filter(!is.na(Medal), !is.na(Height), !is.na(Weight), !is.na(Age), 
                    !is.na(Sport), !is.na(Sex)) %>% 
          mutate(Medal = factor(Medal, levels = c("Gold", "Silver", "Bronze")),
                  Sport = factor(Sport), Sex = factor(Sex))

print(df)

#Present a bar chart displaying amount of medals won based on Ages throughout Olympic History
ggplot(df, aes(x = Age)) + 
  geom_bar(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Age Distribution of Medal Winners", 
       x = "Age",
       y = "Number of Medal Winners") 
+ theme_minimal()

#Present Bar Chart displaying Medal count for all Sports through Olympic History
ggplot(df, aes(x = Sport, fill = Medal)) +
  geom_bar(position = "stack")+
  coord_flip() +
  labs(title = "Medals Across Different Sports in Olympic History", 
       x = "Sport", 
       y = "Medals") + 
  theme_minimal() 

#Question 1: Can we predict what sport an athelete can compete based on weight,height,age?
#We gonna use Decision Trees 

set.seed(42)
indices <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_set <- df[indices, ]
test_set<- df[-indices, ]

decision_tree_model <- rpart(Sport ~ Height + Weight + Age, data = train_set, method = "class")

rpart.plot(decision_tree_model)

predictions <- predict(decision_tree_model, test_set, type = "class")

confusion_matrix <- table(Predicted = predictions, Actual = test_set$Sport)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy)) 


#Question #2: Using Logistical Regression Techqnique, Can we see if their are factors
# that lead to a specific athelete to win a medal? Such as certain age, sex, weight, etc?

set.seed(42)
train_indices <- sample(1:nrow(df), size = 0.7 * nrow(df)) 
train_set <- df[train_indices, ] 
test_set <- df[-train_indices, ]
logit_model <- glm(Medal ~ Age + Height + Weight + Sex, data = train_set)
predictions <- predict(logit_model, newdata = test_set, type = "response") 
test_set$Predicted <- ifelse(predictions > 0.5, 1, 0)
accuracy <- mean(test_set$Predicted == test_set$Medal)
print(paste("Accuracy:", accuracy))

#Question 3: How can we use K-Means Clustering to Group Atheletes based on Height, Weight, Age?
set.seed(42)
kmeans_model <- kmeans(df[, c('Height', 'Weight', 'Age')], centers = 3)
df$Cluster <- kmeans_model$cluster
ggplot(df, aes(x = Height, y = Weight, color = factor(Cluster))) +
  geom_point() +
  labs(color = "Cluster")

#4: Finding Accuracy based on Brand New Algorithm Called "Gradient Boosting"
#To predict how many atheletes can win either Gold, Silver, Bronze, or even combination?
set.seed(42)
indices <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_set <- df[indices, ]
test_set <- df[-indices, ]

gbm_model <- gbm(Medal ~ Height + Weight + Age + Sport + Sex, 
                 data = train_set, distribution = "multinomial", 
                 n.trees = 25, interaction.depth = 3, 
                 shrinkage = 0.1, cv.folds = 3) 

predictions <- predict(gbm_model, test_set, n.trees = gbm_model$n.trees, type = "response")
predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]
predicted_classes <- factor(predicted_classes, levels = c("Gold", "Silver", "Bronze"))
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_set$Medal) 
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy)) 



