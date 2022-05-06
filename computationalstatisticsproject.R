## Computational Statistics Project

# Loading Libraries: ----
library(tidyverse)
library(ggplot2)
library(bestglm)
library(MASS)
library(broom)
library(rpart)


# Import data : ----
# heartdata <- read.table("/Users/juliettegudknecht/Downloads/heart_failure_clinical_records_datase.csv", 
#                         header = TRUE,
#                         sep = ",")

heartdata <- read.table("/Users/tarunagrawal/Documents/GitHub/datascienceproject/heart_failure_clinical_records_dataset.csv", 
                        header = TRUE,
                        sep = ",")
                        
# Clean data:----
head(heartdata)
dim(heartdata)

# check for NA values
sum(is.na(heartdata))

#describe data 
summary(heartdata)
str(heartdata)

colnames(heartdata)
# Making correction in Data Type:----'
categoricalcolumns <- c("anaemia","smoking","sex","diabetes","DEATH_EVENT","high_blood_pressure")
for( i in categoricalcolumns){
heartdata[,i] <- as.factor(heartdata[,i])
}
str(heartdata)

# Visualisation : ----

ggplot(heartdata, aes(x=age)) +   
  geom_histogram(color="white", fill="red")

ggplot(heartdata, aes(x=anaemia)) +   
  geom_bar(color="white", fill="red")

ggplot(heartdata, aes(x=creatinine_phosphokinase)) +   
  geom_histogram(color="white", fill="red")

ggplot(heartdata, aes(x=diabetes)) +   
  geom_bar(color="white", fill="red")

ggplot(heartdata, aes(x=ejection_fraction)) +   
  geom_histogram(color="white", fill="red")

ggplot(heartdata, aes(x=high_blood_pressure)) +   
  geom_bar(color="white", fill="red")

ggplot(heartdata, aes(x=platelets)) +   
  geom_histogram(color="white", fill="red")

ggplot(heartdata, aes(x=serum_creatinine)) +   
  geom_histogram(color="white", fill="red")

ggplot(heartdata, aes(x=serum_sodium)) +   
  geom_histogram(color="white", fill="red")

ggplot(heartdata, aes(x=sex)) +   
  geom_bar(color="white", fill="red")

ggplot(heartdata, aes(x=smoking)) +   
  geom_bar(color="white", fill="red")

ggplot(heartdata, aes(x=time)) +   
  geom_histogram(color="white", fill="red")

ggplot(heartdata, aes(x=DEATH_EVENT)) +   
  geom_bar(color="white", fill="red")

# Assumptions Checking(Logistic): ----
## Assumption 1: ----
# Binary Outcome (DEATH_EVENT)

table(heartdata$DEATH_EVENT)

## Assumption 2: ---- 
# Linear Relationship between logit function and each continuous predictor: 

# Applying Model:
model_test <- glm(DEATH_EVENT ~ ., family = binomial, 
              data = heartdata)
probabilities <- predict(model_test, type = "response")

data_check <- heartdata %>% select_if(is.numeric) 
predictors <- colnames(data_check)

# Bind the logit and tidying the data for plot
mydata <- data_check %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

# Plots for checking assumptions:
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

## Assumption 3: ----
# Influential Value in predictors:
plot(model_test, which = 4, id.n = 3) # Gives Influential Values based on observations.

model.data <- augment(model_test) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = DEATH_EVENT), alpha = .5) +
  theme_bw()

model.data %>% 
  filter(abs(.std.resid) > 3)

# No Influential Measures found.

## Assumption 4: ----
# Multicollinearity:
car::vif(model_test)

# Logistic Regression ----
set.seed(123)
n = nrow(heartdata)
split <- sample(1:n, 0.8*n, replace = FALSE)
training <- heartdata[split,]
test <- heartdata[-split,]

bs1 <- bestglm(Xy = training, family = binomial, IC = "BIC")
summary(bs1)
bs1$BestModels
bs1$Subsets

model1 <- glm(DEATH_EVENT ~ ejection_fraction + serum_creatinine + time, family = binomial, 
              data = training)
summary(model1)

bs2 <- bestglm(Xy = training, family = binomial, IC = "AIC")
summary(bs2)
bs2$BestModels
bs2$Subsets

model2 <- glm(DEATH_EVENT ~ ejection_fraction + diabetes + age  + serum_creatinine + time, family = binomial, 
              data = training)
summary(model2)

model3 <- glm(DEATH_EVENT ~ ., family = binomial, 
              data = training)
summary(model3)


predicted<-predict(model2, test, type="response")
for (i in 1:length(predicted)) {
  if(predicted[i]>=0.05){
    predicted[i]=1
  } else
  {predicted[i]=0
  }
}

(accuracy <- (length(which(predicted==test$DEATH_EVENT)) / 59.8) * 100)

# Linear Discriminant Analysis:----

# a) Use the lda function with default priors to separate the Species using all other variables
(ldamod = lda(DEATH_EVENT ~ ., data= heartdata))

# b) Obtain the prediction vs. actual matrix
predict(ldamod)
table(Predicted=predict(ldamod)$class, Actual = heartdata$DEATH_EVENT)

# Quadratic Discriminant Analysis:
qda.fit <- qda(DEATH_EVENT ~ ., data= heartdata)
qda.fit
qda.class <- predict(qda.fit)$class
table(qda.class,heartdata$DEATH_EVENT)
# Performing Worse than LDA.

# Tree Based Methods: ----

# grow tree
fit_tree <- rpart(DEATH_EVENT ~ ., method="class", data=heartdata)

fit_tree # display the results 
plot(fit, uniform=TRUE, main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex= 0.7)

# Mis-classification error
(conf.matrix = table(heartdata$DEATH_EVENT, predict(fit_tree,type="class")))
sum(diag(conf.matrix))/nrow(heartdata)

## Pruning:----
fitp = prune(fit_tree, cp = 0.019608)
printcp(fitp)
plot(fitp) #plot smaller rpart object
text(fitp, use.n=TRUE, all=TRUE, cex=.8)

(conf.matrix.prune = table(heartdata$DEATH_EVENT, predict(fitp,type="class")))
sum(diag(conf.matrix.prune))/nrow(heartdata)
