install.packages("psycho")
library(psycho)

diabetes <- read.csv('diabetes.csv')

str(diabetes)
head(diabetes)

# Perform PCA
pca_result <- prcomp(diabetes, scale = TRUE)
pca_result


# Proportion of variance explained by each component

explained_variance <- pca_result$sdev^2
total_variance <- sum(explained_variance)
proportion_of_variance <- explained_variance / total_variance

# Display the proportion of variance explained by each component
print(proportion_of_variance)


#FACTOR ANALYSIS 

fa_result <- factanal(diabetes[, c(1, 3, 4, 6, 7, 8, 9)], factors = 2)
print(fa_result)

#Generative capacity of FACTOR analysis 

num_samples <- nrow(diabetes)
latent_factors <- matrix(rnorm(2 * num_samples), nrow = num_samples)
latent_factors

simulated_data <- latent_factors %*% t(fa_result$loadings)
simulated_data

#Compare Simulated Data to Original Data

#For example, calculate correlation coefficients between original and simulated data(-1/1 high correlation with negative/positive effect )

correlation_matrix <- cor(diabetes[, c(1, 3, 4, 6, 7, 8, 9)], simulated_data)
correlation_matrix

#Correlation matric shows the correlation between real and simulated data, here i had 
#to poit out that I expected to see more correlation.


#PLOT PCA

pca_df <- as.data.frame(pca_result$x[, 1:2]) #Select the frist 2 PCA (most information)
pca_df

pca_df$outcome <- diabetes$Outcome #Add outcome variables to perform clustering 
pca_df

set.seed(123)  # for reproducibility
kmeans_clusters <- kmeans(pca_df[, -3], centers = 2) #PCA1, PCA2, Outcome clustering k-means method 

kmeans_clusters

logistic_model <- glm(outcome ~ ., data = pca_df, family = binomial) #generalized binomial model for outcome ~ PCA1 and PCA2
summary(logistic_model)$coef

library(ggplot2)

#K-means clusteringf plot 

plot_clusters <- ggplot(pca_df, aes(x = PC1, y = PC2, color = factor(kmeans_clusters$cluster))) +
  geom_point() +
  labs(title = "Clustering Results", color = "Cluster") +
  theme_minimal()

#Separation trendline plot between the groups (Outcome 1 or 0)

plot_classification <- ggplot(pca_df, aes(x = PC1, y = PC2, color = outcome)) +
  geom_point() +
  geom_abline(intercept = -logistic_model$coefficients[1] / logistic_model$coefficients[3],
              slope = -logistic_model$coefficients[2] / logistic_model$coefficients[3],
              linetype = "dashed", color = "red") +
  labs(title = "Classification Results", color = "Outcome") +
  theme_minimal()


installed.packages("gridExtra")
library(gridExtra)

grid.arrange(plot_clusters, plot_classification, nrow = 1) #Plot plot_classification and plot_clusters together
plot_classification


#Perform a biplot to understand the contribution to each variable to the principal components 

install.packages("FactoMineR")
library(FactoMineR)


diabetes <- read.csv('diabetes.csv')
diabetes
# Perform PCA
pca_result <- PCA(diabetes[, -9], scale.unit = TRUE, graph = FALSE)
pca_result

print(pca_result$eig)

eigenvectors <- as.data.frame(pca_result$var$coord)
eigenvectors

#regnancies              0.1858666  0.7812764 -0.01327939  0.07550248  0.41526247
#Glucose                  0.5688680  0.2289796  0.47480446 -0.37832919 -0.40716190
#BloodPressure            0.5209948  0.2419568 -0.54336983  0.05238640 -0.28634347
#SkinThickness            0.6365125 -0.4367849 -0.24116921  0.03553411  0.42596376
#Insulin                  0.6295687 -0.3299663  0.34166084 -0.32744136  0.30291688
#BMI                      0.6540483 -0.1328383 -0.36718649  0.05019636 -0.22107811
#DiabetesPedigreeFunction 0.3916281 -0.1606129  0.43955987  0.78007205 -0.10460934
#Age                      0.2865842  0.8165422  0.07635420  0.06662220  0.09542361



biplot <- function(pca_result) {
  eigenvectors <- as.data.frame(pca_result$var$coord)
  observations <- as.data.frame(pca_result$ind$coord)
  
  ggplot() +
    geom_segment(data = eigenvectors, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2), 
                 arrow = arrow(length = unit(0.2, "cm")), color = "red") +
    geom_text(data = eigenvectors, aes(x = Dim.1, y = Dim.2, label = rownames(eigenvectors)), 
              vjust = -0.5, size = 2, color = "red") +
    geom_point(data = observations, aes(x = Dim.1, y = Dim.2), size = 0.3, alpha = 0.5) +
    labs(x = paste0("PC1 (", round(pca_result$eig[1, "percentage of variance"], 2), "%)"),
         y = paste0("PC2 (", round(pca_result$eig[2, "percentage of variance"], 2), "%)"),
         title = "PCA Biplot") +
    theme_minimal()
}

biplot(pca_result)


#From Eigenvectors values and biplot we can observe that variables like Glucose level, Blood pressure, INsulin and BMI have a large influx on the Components 

#Now comparing the data with factor analysis--> Loadings:
#Factor1 Factor2
#Pregnancies               0.626         
#BloodPressure             0.253   0.344 
#SkinThickness            -0.131   0.528 
#BMI                               0.772 
#DiabetesPedigreeFunction          0.228 
#Age                       0.874         
#Outcome                   0.281   0.317 

#Pregnances is influnced only one latent factor (factor 1)and a value of 0.626 can be considered meaningful, Blood pressure shows 
#Crossloading mening that they are potnentially influenced by multiple latent factors. 
#Age, BMI showed high values and are unique for a latent factor making them also good candidate and meningful like pregnance. 


#In overall comparing FA and PCA analysis, I can say that BMI and AGE and pregnances contribute in a significant way (not tnough at the moment to say) to PCA and diabetes and are only influnced by 1 factor (FA analysis)


#Try to make a model that predict outcome of diabetes patients with the input variable. the first importntant step is to 
#split a train and test dataset: 

install.packages("caTools")
install.packages("glmnet")

library(caTools)
library(glmnet)


set.seed(123) # for reproducibility
split <- sample.split(diabetes$Outcome, SplitRatio = 0.7) #70% to train 30% to test
train_data <- subset(diabetes, split == TRUE)
test_data <- subset(diabetes, split == FALSE)


diabetes

x_train <- data.matrix(train_data[, 1:8])#select the x of train dataset
x_train


y_train <- as.numeric(train_data[, 9]) #select y for train
y_train


x_test <- data.matrix(test_data[, 1:8]) #select x for the test dataset
y_test <- as.numeric(test_data[, 9]) #select y for the test dataset

x_test
y_test


glm.model <- cv.glmnet(x_train, y_train, alpha=1, nfolds=10) #lasso regularization. 
glm.model

lambda.min = glm.model$lambda.min #save the lambda that minimize the error of the model 
lambda.min


glm_coef = round(coef(glm.model,s= lambda.min),2)
glm_coef #weight of the coeficent

plot(glm.model)

plot(glmnet(x_train,y_train, family="gaussian", alpha=1),"lambda",label=T, main="")
abline(v = log(lambda.min), lty = 3)

log(lambda.min)


install.packages("e1071") #SVM machine leraning approach
library(e1071)


svm.model <- svm(x_train, y_train, cost =1, gamma = c(1/(ncol(x_train)-1)), kernel = "radial", cross = 10)
svm.model


install.packages("nnet") #Neuronal network approach

library(nnet)
nne.model <- nnet(x_train, y_train, size = 5) #with size we set the size the number of hidden layers


#If one multiply teh input of a "neuron" we found what is reached in teh other "neurons" 

glm.predict <- round(predict(glm.model, x_test, type="response"),0) 
glm.predict

svm.predict <- round(predict(svm.model, x_test, type = "response"), 0)
svm.predict

nne.predict <- round(predict(nne.model, x_test, type = "raw"), 0)
nne.predict

#Sensitivity =true positives/actual positives (2) Actual positive cases correctly detected
#Specificity =true negatives/actual negatives (3) Actual negatve cases correctly detected
#Accuracy =(true positives + true negatives)/total predictions --> 

install.packages("tidyverse")
library(tidyverse)

predictions <- tibble(glm = c(glm.predict), svm = c(svm.predict), nne = c(nne.predict))
predictions #make a tibble wwith predictions 

algorithm_n <- 3 #number fo algirithm here tested svm, glm, and nne

predictions <- as.data.frame(predictions)
predictions

install.packages("caret")

library(caret)

confusionMatrix(as.factor(glm.predict),as.factor(y_test)) #compare prediction outcome with test 
confusionMatrix(as.factor(svm.predict),as.factor(y_test)) #""
confusionMatrix(as.factor(nne.predict), as.factor(y_test))


#glm and svm seems have the highest accuracy 74 and 72% respectively, whereas nne only 65%.

#In all cases specificity (negative cases correctly detected) are bad


#The code below sets the values for the features to be evaluated by the trained and validated model

str(diabetes)

Pregnancies = 0             
Glucose = 75                 
BloodPressure = 75           
SkinThickness  = 25          
Insulin = 6                 
BMI  = 20                  
DiabetesPedigreeFunction = 0.333
Age  = 20    

new_data = c(Pregnancies,Glucose,BloodPressure,SkinThickness, Insulin,BMI,DiabetesPedigreeFunction,Age)

new_data

new_pred_glm = predict(glm.model ,data.matrix(t(new_data))
                       ,type="response")
new_pred_glm

#As expected low probabilty to have diabetes based on the example data


#With this last example my goal was to show how today data can be use in a powerful manner to predict outcomes for patients, 
#beside this the intent was also tpo show to demonstrate how autoencoders in contrast to factor analysis are non-linear process, which 
#allow more flexibility for data analysis and interpretation.



