Alzheimer_data<-read.csv("project data.csv",head=TRUE)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(plotly)
attach(Alzheimer_data)
library(RColorBrewer)
library(boot)
library(caret)

#Remove all NAs

Alzheimer_data<-Alzheimer_data %>%  na.omit()

#remove rows with Group "Converted"

Alzheimer_data<-Alzheimer_data %>%  filter(Group=='Demented' | Group=='Nondemented')

#Transform M/F into numeric values

Alzheimer_data$sex<- factor(Alzheimer_data$M.F, 
                 levels=c("F","M"), 
                 labels=c(0,1))
#Transform socioeconomic status into a factor
Alzheimer_data$SES<- factor(Alzheimer_data$SES,levels = c("1","2","3","4","5"))

#Transform Group into a factor
Alzheimer_data$Group<-as.factor(Alzheimer_data$Group)

#Obtain the names of the columns and a summary of the data                            
names(Alzheimer_data)
summary(Alzheimer_data)

###################################################################################
#########################1.DESCRIPTIVE STATISTICS##################################
###################################################################################
attach(Alzheimer_data)

#bar chart and table of Group by sex

tab_gen<-table(M.F,Group)
prop_group<-prop.table(tab_gen,1)
perc_group<-round((100*prop_group),2)
perc_group

ggplot(Alzheimer_data,aes(x=M.F,fill=Group))+
  geom_bar()+scale_fill_brewer(palette="Paired",direction=-1)+ theme_minimal()+
  theme(panel.grid.major.x = element_blank())+
  labs(title = "Group proportion by sex",x = "sex")



#Plot of Group by sex and Socioeconomic status

tab_gen<-table(SES,Group)
prop_group<-prop.table(tab_gen,1)
perc_group<-round((100*prop_group),2)
perc_group

ggplot(Alzheimer_data,aes(x=M.F,fill=Group))+
  geom_bar()+scale_fill_brewer(palette="Paired",direction=-1)+ theme_minimal()+
  theme(panel.grid.major.x = element_blank())+
  labs(title = "Group proportion by sex and SES",x = "sex")+
  facet_wrap(~SES)


#Plot of Group by SES
ggplot(Alzheimer_data,aes(x=SES,fill=Group))+
  geom_bar()+scale_fill_brewer(palette="Paired",direction=-1)+ theme_minimal()+
  theme(panel.grid.major.x = element_blank())+
  labs(title = "Group proportion by SES",x = "Socioeconomic Status")

ggplot(Alzheimer_data, aes(x = reorder(SES,MMSE, .fun='median'), y = MMSE)) +
  geom_boxplot(color = "#6baed6") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Mini mental state Examination score by SES",x = "Socioeconomic status", y = "MMSE score")+
  ylim(0, 35)

#Plot of group by AGE

summary(Alzheimer_data$Age)

ggplot(Alzheimer_data,aes(x=Age))+geom_histogram(binwidth=4,fill="#6baed6", color="white")+
  theme_minimal()+labs(title = "Age Distribution",x = "Age")

#Plot of group by Education

ggplot(Alzheimer_data, aes(x = reorder(Group,EDUC, .fun='median'), y = EDUC)) +
  geom_boxplot(color = "#6baed6") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Years of Education by Group",x = "Group", y = "Education")+
  ylim(0, 20)


###
#Plot of MMSE by group
ggplot(Alzheimer_data, aes(MMSE))+
  geom_histogram(binwidth=2,fill="#6baed6", color="white")+facet_grid(rows=vars(Group))+
  theme_minimal()+
  labs(title = "MMSE by Group")
#Plot of nwBv by group

ggplot(Alzheimer_data, aes(nWBV))+
  geom_histogram(binwidth=0.02,fill="#6baed6", color="white")+facet_grid(rows=vars(Group))+
  theme_minimal()+
  labs(title = "nWBV by Group")


#CORRPLOT

Alzheimer_data$dgroup<- factor(Alzheimer_data$Group, 
                            levels=c("Nondemented","Demented"), 
                            labels=c(0,1))

Alzcorr.df<-as.data.frame(Alzheimer_data[3:12])
Alzcorr.df <- as.data.frame(apply(Alzcorr.df, 2, as.numeric))


corr.mat <- round(cor(Alzcorr.df), 2)
pval.cor <- cor_pmat(Alzcorr.df)
corr_plot = ggcorrplot(corr.mat,hc.order = TRUE,type = "lower", lab = TRUE,outline.color = "white",lab_size = 3,colors = c("#6D9EC1", "white", "#E46726"))+
  labs(title = "Correlation Matrix \n on Alzheimer data \n", 
       x = "", y = "")+theme(axis.text.x = element_text(angle = 90)) + theme_minimal()
corr_plot



###################################################################################
#########################2.CLUSTERING ALGORITHMS###################################
###################################################################################
#Creates a dataframe from the dataset with only numeric variables
Alz.df<-as.data.frame(Alzheimer_data[3:11])
Alz.df <- as.data.frame(apply(Alz.df, 2, as.numeric))
# Convert variables to numeric

# Set seed
set.seed(1)

# Creates a vector that sets demented as 1 and non demented as 0
diagnosis <- as.numeric(Alzheimer_data$Group == "Demented")

# Means and sd are too different from each other, they need to be scaled
colMeans(Alz.df)
apply(Alz.df,2,sd)

# Scalede data
Alz.scaled<-scale(Alz.df)

# hierarchical clustering model
hclust.Alz<-hclust(dist(Alz.scaled), method="complete")
rect.hclust(hclust.Alz, k=2, border="red")

# Cut tree
Alz.hclust.clusters<-cutree(hclust.Alz,k=2)

# Compare hierarchichal cluster to actual diagnoses
table(Alz.hclust.clusters,diagnosis)

# k-means model 
Alz.km<-kmeans(scale(as.matrix(Alz.scaled)),centers=2,nstart=20)
Alz.km
#Visualize kmeans
fviz_cluster(Alz.km, data = Alz.scaled,ggtheme = theme_minimal(),geom = "point")
#determine optimal number of clusters
fviz_nbclust(Alz.scaled, kmeans, method = "wss")+
  geom_vline(xintercept = 2, linetype = 2)


Alzheimer_data$cluster <- as.factor(Alz.km$cluster)


# Compare k-means to actual diagnoses
table(Alz.km$cluster,diagnosis)

# Compare k-means to hierarchical clustering
table(Alz.km$cluster,Alz.hclust.clusters)



Alz.pr.hclust <- hclust(dist(Alz.pr$x[, 1:7]), method = "complete")

#cut model
Alz.pr.hclust.clusters<-cutree(Alz.pr.hclust,k=2)

# Compare to actual diagnoses
table(diagnosis, Alz.pr.hclust.clusters)
table(diagnosis, Alz.hclust.clusters)

# Compare to k-means and hierarchical
table(diagnosis,Alz.km$cluster)


###################################################################################
#########################3.LOGISTIC REGRESSION#####################################
###################################################################################
set.seed(1)

# training and testing sets
trainIndex <- createDataPartition(Alzheimer_data$dgroup, p = 0.7, list = FALSE)
trainData <- Alzheimer_data[trainIndex, ]
testData <- Alzheimer_data[-trainIndex, ]

glm.fit<-glm(dgroup~sex+EDUC+nWBV,data=Alzheimer_data,family=binomial)
summary(glm.fit)

# predicted probabilities
glm.probs <- predict(glm.fit, newdata = testData, type = "response")
threshold <- 0.5
glm.predicted <- ifelse(glm.probs > threshold, 1, 0)

# Table of predicted and real diagnosis
table(glm.predicted, testData$dgroup)

# Accuracy of the predictions
accuracy <- mean(glm.predicted == testData$dgroup)
accuracy


###################################################################################
#########################4.FEATURE SELECTION#######################################
###################################################################################
###################################################################
# PCA
Alz.pr<-prcomp(Alz.df,scale=TRUE)
summary(Alz.pr)
#PCA with correlation matrix
Alz.pca <- princomp(corr.mat)
Alz.pca$loadings[, 1:2]


# Visualize the biplot
biplot(Alz.pr)

# Scatter plot of PC1 and PC2
plot(Alz.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")

# Scatter plot of PC1 and PC2
plot(Alz.pr$x[, c(1, 3)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC3")


par(mfrow = c(1, 2))

# Variability of each component
pr.var<-Alz.pr$sdev^2

# Variance explained by each principal component
pve<-pr.var/sum(pr.var)

# Variance plot
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Cumulative plot
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

