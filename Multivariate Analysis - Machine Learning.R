#install.packages("robustbase")
#install.packages("tigerstats")
#install.packages("AID")
#install.packages("ggpubr")

library(AID)
library(robustbase)
library(tidyverse)
library(ggplot2)
library(MASS)
library(tigerstats)
library(e1071)


Patient_Survival_data =read.csv("Patient_Survival_data.csv",header=T,sep=",")
str(Patient_Survival_data)
head(Patient_Survival_data)


#MANOVA :
# To conduct monova test, we take simple random sample from our data (100sample.)

#I’ll test whether the response variables (heart_rate_apache and resprate_apache) varies with respect to icu_admit_source type.

#icu_admit_source : The location of the patient prior to being admitted to the unit
#heart_rate_apache   : The heart rate measured during the first 24 hours which results in the highest APACHE III score
#resprate_apache : The respiratory rate measured during the first 24 hours which results in the highest APACHE III score


set.seed(123)
md=popsamp(100,Patient_Survival_data)
str(md)


table(md$icu_admit_source)


# exploratory data analysis:

library(AID)
library(robustbase)
library(tidyverse)
library(ggplot2)
library(MASS)
library(tigerstats)
library(e1071)


data =read.csv("Patient_Survival_data.csv",header=T,sep=",")
str(data)
head(data)

summary(Patient_Survival_data)



Patient_Survival_data %>%
  filter(., heart_rate_apache != "" & intubated_apache != "", age != "") %>% 
  mutate(intubated_apache = recode(intubated_apache, "0"="No", "1"="Yes")) %>% ggplot(., aes(x=age, y=heart_rate_apache)) + geom_point(aes(color=factor(intubated_apache))) +
  labs(title="Age vs. Heart Rate Scatter Plot by Intubated Patient", x="Age", y="Heart Rate", color="Intubated Patient") +
  scale_color_manual(values = c("purple", "orange"))



Patient_Survival_data %>%   
  filter(., apache_3j_bodysystem != "" & gender != "", age != "") %>% 
  mutate(gender = recode(gender, "F"="Female", "M"="Male")) %>% 
  ggplot(. ,aes(x=apache_3j_bodysystem, y=bmi)) + geom_boxplot(fill=10) +
  facet_wrap(~ gender, nrow=1) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Diagnosis") + ylab("BMI") + ggtitle("Boxplot for Diagnosis & BMI by Gender") + stat_summary(fun=mean, geom="point", shape=20, size=2, color="white", fill="red")



Patient_Survival_data %>%   
  filter(., pre_icu_los_days != "" & intubated_apache != "", apache_4a_hospital_death_prob != "") %>% 
  mutate(intubated_apache = recode(intubated_apache, "0"="No ", "1"="Yes ")) %>%
  ggplot(., aes(x=pre_icu_los_days, y=apache_4a_hospital_death_prob)) +
  geom_point(aes(color=factor(intubated_apache))) + ylim(c(0,1)) +xlim(c(0,5)) + labs(title="Time & Probability of Patient Death by Intubated Patient", x="Time between hospital admission and admission to the unit", y="Probability of Patient Death", color="Intubated Patient")




Patient_Survival_data %>%   
  filter(., bmi != "" & intubated_apache != "") %>% 
  mutate(intubated_apache = recode(intubated_apache, "0"="No ", "1"="Yes ")) %>%
  ggplot(., aes(x=bmi, color=factor(intubated_apache))) + geom_density(linetype=6, size=1.2) +
  labs(title= "BMI Density Curves by Intubated Patient", x="BMI",y= "Density", colour="Intubated Patient")

library(dplyr)

Patient_Survival_data %>%   
  filter(., apache_3j_bodysystem != "" & apache_4a_hospital_death_prob != "" & immunosuppression != "") %>% 
  mutate(immunosuppression = recode(immunosuppression, "0"="Not suppressed ", "1"="Suppressed ")) %>%
  ggplot(., aes(x = apache_3j_bodysystem, y=apache_4a_hospital_death_prob, fill=factor(immunosuppression))) + scale_fill_brewer(palette = 7) + geom_col(position = position_dodge(0.7), stat = "identity", width = 0.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Diagnosis & Probability of Patient Death by Immunosuppression", y="Probability of Patient Death", x="Diagnosis for apache III", fill="Immunosuppression") + ylim(c(0,1))


library(dplyr)

Patient_Survival_data %>% 
  filter(., ethnicity != "" & gender != "" & age != "") %>% 
  ggplot(. ,aes(x=gender, y=age, fill=gender)) + geom_boxplot() +
  facet_wrap(~ ethnicity, nrow=2) +
  labs(title = "Boxplot for Age & Gender", x="Gender", y="Age", fill="Gender") + scale_x_discrete(labels = c("Female", "Male")) + scale_fill_discrete(labels = c("Female", "Male")) + stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="red")



# manova data :
manova_data= md[,c(7,14,16)]
head(manova_data)

# summary statistics for dependent variable height 
manova_data %>% group_by(manova_data$icu_admit_source) %>%  summarise(n = n(), mean = mean(manova_data$heart_rate_apache), sd = sd(manova_data$heart_rate_apache))

manova_data %>% group_by(manova_data$icu_admit_source) %>%  summarise(n = n(), mean = mean(manova_data$resprate_apache), sd = sd(manova_data$resprate_apache))
# the assumption of adequate sample size is satisfied,we have large data set for manova test.

library(gridExtra)
p1 <- ggplot(manova_data, aes(x = icu_admit_source, y = heart_rate_apache, fill = icu_admit_source)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
p2 <- ggplot(manova_data, aes(x = icu_admit_source, y = resprate_apache, fill = icu_admit_source)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
grid.arrange(p1, p2, ncol=2)


#Test MANOVA assumptions
#install.packages("rstatix")
#install.packages("ICSNP")
library("ICSNP")

#For normality :

#install.packages("mvnormalTest")
library(mvnormalTest)
mardia(manova_data[, c("heart_rate_apache", "resprate_apache")])$mv.test

#As the p value is non-significant (p > 0.05) for Mardia’s Skewness and Kurtosis test, we fail to reject the null hypothesis and conclude that data follows multivariate normality.
#Also, our data is big enough to assume normality by MCLT.


# Homogeneity of the variance-covariance matrices

# We will use Box’s M test to assess the homogeneity of the variance-covariance matrices. 
#Null hypothesis: variance-covariance matrices are equal for each combination formed by each group in the independent variable
#install.packages("heplots")
library(heplots)
boxM(Y = manova_data[, c("heart_rate_apache", "resprate_apache")], group = manova_data$icu_admit_source)

#As the p value is  (p < 2.2e-16) for Box’s M test, we can reject the null hypothesis.



manova_data %>% 
  gather(key = "variable", value = "value", heart_rate_apache, resprate_apache) %>%
  group_by(variable) %>%
  levene_test(value ~ icu_admit_source)


# p values 0.776,and 0.0664 resp. ,so there was homogeneity of variances.


# QQ plot of heart_rate_apache

#install.packages("ggpubr")
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(ggqqplot)
library(ggplot2)
#install.packages("lattice")

library(lattice)
data("manova_data", package = "lattice")



ggplot(manova_data, aes(sample = manova_data$heart_rate_apache, colour = factor(manova_data$icu_admit_source))) +
  stat_qq() +
  stat_qq_line()


ggplot(manova_data, aes(sample = manova_data$resprate_apache, colour = factor(manova_data$icu_admit_source))) +
  stat_qq() +
  stat_qq_line()

#Univariate outliers
#Univariate outliers can be easily identified using box plot methods, implemented in the R function
library(rstatix)
#attach(manova_data)
manova_data %>%
  group_by(icu_admit_source) %>%
  identify_outliers(heart_rate_apache)

#There are some univariate outliers.
# Multivariate outliers


library(rstatix)
# get distance
mahalanobis_distance(data = manova_data[, c("heart_rate_apache", "resprate_apache")])
#it means there are multivariate outliers in the data set.



#Check linearity assumption
# Create a scatterplot matrix by group
#install.packages("GGally")
library(GGally)
results <- manova_data %>%
  select(heart_rate_apache,resprate_apache,icu_admit_source) %>%
  group_by(manova_data$icu_admit_source) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results

results$plots

#As the correlation coefficient between the dependent variable is > 0.9, there is multicollinearity.

table(icu_admit_source)

#Linearity assumption can be checked by visualizing the pairwise scatterplot for the dependent variable for each group. The data points should lie on the straight line to meet the linearity assumption.
# The violation of the linearity assumption reduces the statistical power

library(gridExtra)
p1 <- manova_data  %>% group_by(icu_admit_source) %>% filter(icu_admit_source == "Accident & Emergency") %>% ggplot(aes(x = heart_rate_apache, y = resprate_apache)) + geom_point() 
p2 <- manova_data  %>% group_by(icu_admit_source) %>% filter(icu_admit_source == " Floor Operating Room ") %>% ggplot(aes(x = heart_rate_apache, y = resprate_apache))  
p3 <- manova_data  %>% group_by(icu_admit_source) %>% filter(icu_admit_source == "Other Hospital") %>% ggplot(aes(x = heart_rate_apache, y = resprate_apache)) + geom_point() 
p4 <- manova_data  %>% group_by(icu_admit_source) %>% filter(icu_admit_source == "Recovery") %>% ggplot(aes(x = heart_rate_apache, y = resprate_apache)) + geom_point()  
grid.arrange(p1, p2, p3, p4, ncol=2)

table(manova_data$icu_admit_source)

#the scatterplot indicates that dependent variables do not have a linear relationship for each group in the independent variable
#The reoson of the linearity problem ,can be outliers.



#Multicollinearity assumption

cor.test(x = manova_data$heart_rate_apache, y = manova_data$resprate_apache, method = "pearson")$estimate

#As the correlation coefficient between the dependent variable is < 0.9, there is no multicollinearity.


#All the assumption did not  satisfied.



#We can apply our MANOVA test :



dep_vars <- cbind(manova_data$heart_rate_apache, manova_data$resprate_apache)
fit <- manova(dep_vars ~ icu_admit_source, data = manova_data)
summary(fit)


# get effect size
#install.packages("effectsize")
library(effectsize)
effectsize::eta_squared(fit)


# [Pillai’s Trace = 0.10264, p > 0.001] 
#ýt indicates that icu_admit_source doesnt have  significance association with both combined heart_rate_apache and resprate_apache.
#The measure of effect size (Partial Eta Squared) is 0.05 
# suggests that there is no effect of icu_admit_source on both both heart_rate_apache and resprate_apache..


###########################################


# INFERENCES ABOUT A MEAN VECTOR (Hotelling's T2)


data2 =data[,c(30,50,8)]


#New colum from original data set is added, so again we check the new colum na values.


# In selected data ,there are 91713 obs. of  50 variables.500 obs. taken from the selected dataset randomly.
set.seed(123)
Hotelling_data=popsamp(100,data2)
str(Hotelling_data)
head(Hotelling_data)
which(is.na(Hotelling_data$h1_diasbp_max))
mean(Hotelling_data$h1_diasbp_max,na.rm=T)
Hotelling_data[c(13,40,63,70),2]=75.52083

which(is.na(Hotelling_data$h1_diasbp_max))

# now , we will consider this research question :

y<-Hotelling_data[,1:2]
head(y)
head(Hotelling_data)
#Then create µ0 vector.


# Print out correlation (cor) of dependent variables
# Print out means (mean) and standard deviations (sd) of dependent variables

cor(y)
mean(Hotelling_data$d1_diasbp_max)
mean(Hotelling_data$h1_diasbp_max)


sd(Hotelling_data$d1_diasbp_max)
sd(Hotelling_data$h1_diasbp_max)
# Install and load R psych package
# Graph dependent variable means using error.bars() function


library(psych)
error.bars(y,ylab="Group Means",xlab="Dependent Variables")

# Conduct a Hotelling T2 test of null hypothesis that dependent means are different than zero
# muH0 assigns population means in matrix vector to equal zero

library("ICSNP")

muH0 <- c(0, 0)
HotellingsT2(y, mu=muH0)


#The results for the one-sample multivariate t test indicated that the two dependent variable means together are statistically significantly different from zero. 
#The correlation matrix indicated that the two dependent variables were correlated,0.56 The Hotelling T 2 value was statistically significant. 
#Therefore, the null hypothesis of no joint mean difference is rejected.



# The results indicated that T2 = 1702.9, with 2 and 98 df and p = 2.2e-16 . 
#The null hypothesis of no group mean difference is rejected.
#The alternative hypothesis is accepted—true location difference is not equal to c(0,0).




HotellingsT2(cbind(Hotelling_data$d1_diasbp_max,Hotelling_data$h1_diasbp_max) ~ Hotelling_data$gender)


##################################################################

# PRINCIPAL COMPONENTS ANALAYSIS and PRINCIPAL COMPONENTS REGRESSION

#Principal Components Analysis
library(tidyverse)


PCA_data=Patient_Survival_data[,c(18,20,22,24,26,28,30,32,34)]

dim(PCA_data)

head(PCA_data)



#calculate principal components
results <- prcomp(PCA_data, scale = TRUE)

#reverse the signs
results$rotation <- -1*results$rotation

#display principal components
results$rotation


#reverse the signs of the scores
results$x <- -1*results$x

#display the first six scores
head(results$x)
summary(results)



#Visualize the Results with a Biplot

biplot(results, scale = 0)
#ind Variance Explained by Each Principal Component

#We can use the following code to calculate the total variance in the original dataset explained by each principal component:

#calculate total variance explained by each principal component
results$sdev^2 / sum(results$sdev^2)


#We can also create a scree plot – a plot that displays the total variance explained by each principal component – to visualize the results of PCA:

#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)
library(ggplot2)
#create scree plot
qplot(c(1:9), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


#Principal Components Regression

install.packages("pls")
library(pls)

pca1=results$x[,1:5]
head(pca1)
res1 <- cor(pca1, method="pearson")
corrplot::corrplot(res1, method= "color", order = "hclust")

#make this example reproducible
set.seed(1)
head(PCA_data)
#fit PCR model
model <- pcr(apache_4a_hospital_death_prob~d1_diasbp_max+d1_heartrate_max+d1_resprate_max+d1_spo2_max+d1_sysbp_max +d1_temp_max+d1_glucose_max +d1_potassium_max , data=PCA_data, scale=TRUE, validation="CV")
summary(model)


lmodel <- lm(apache_4a_hospital_death_prob ~ ., data = pca1)
summary(lmodel)


####################################BARIÞ#########3


#Factor Analysis

#numeric variables
num_sampling <- Patient_Survival_data
nums <- unlist(lapply(num_sampling, is.numeric))
num_var <- num_sampling[ , nums]
str(num_var)

subset_num_var <- subset(num_var,select = -c(elective_surgery,apache_post_operative,arf_apache,intubated_apache,diabetes_mellitus,immunosuppression,hospital_death))
str(subset_num_var)

important_numerical_indicators <- subset(subset_num_var, select = c(age,bmi,pre_icu_los_days,apache_2_diagnosis,apache_3j_diagnosis,heart_rate_apache,d1_diasbp_max,d1_diasbp_min,d1_spo2_max,d1_spo2_min,d1_glucose_max,d1_glucose_min,apache_4a_hospital_death_prob,apache_4a_icu_death_prob))
str(important_numerical_indicators)

library(mlbench)
library(caret)
library(ggplot2)
library(ggcorrplot)


# Assumptions:
# • The variables used in factor analysis should be linearly related to each other. This can
# be checked by looking at scatterplots of pairs of variables.
# • Sample size: The sample size should be large enough to yield reliable estimates of
# correlations among the variables.
# • Normality: Statistical inference is improved if variables are multivariate normal.


correlationMatrix <- round(cor(subset_num_var),digits=2)
correlationMatrix
ggcorrplot(correlationMatrix)

#first we must check overall Kaiser-Meyer-Olkin (KMO)

library(psych)
KMO(subset_num_var)

#KMO is greater than 0.5. We can run the factor analysis if bartlett's test also satisfies normality.

library(mvnormalTest)
mardia(important_numerical_indicators)$mv.test
library(rstatix)
# get distance
mahalanobis_distance(data = important_numerical_indicators)
#it means there are multivariate outliers in the data set.


print(cortest.bartlett(correlationMatrix,nrow(subset_num_var)))

#We must look at the parallel analysis to see how many factors and components are needed to use in factor analysis.

parallel <- fa.parallel(subset_num_var,fm="ml",fa="both", show.legend = F)
parallel

#Parallel analysis suggests that the number of factors =  11  and the number of components =  10 

#We will try to use factanal function according to rotations to see whether we can fit a factorial analysis with 11 factors.

#Using factanal command:
nfactors <- 11
fit <- factanal(x = subset_num_var, nfactors, scores = c("regression"),rotation = "none",control = list(trace = T),lower = 0.015)
fit

#Varimax Rotation
fit1 <-factanal(subset_num_var,nfactors,scores = c("regression"),rotation = "varimax",control = list(trace = T),lower = 0.015)
fit1

#Promax rotation
fit2 <-factanal(subset_num_var,nfactors,scores = c("regression"),rotation = "promax",control = list(trace = T),lower = 0.015)
fit2

#Since all of the fits result to reject the null hypothesis, we cannot conduct a factorial analysis.


#Canonical Correlation Analysis (CCA)

library(ggplot2)
library(GGally)
library(CCA)

str(subset_num_var)
set1 <- Patient_Survival_data[,c(18,20,22,24)]
str(set1)
set2 <- Patient_Survival_data[,c(26,28,30,32)]
str(set2)

x = as.data.frame(c(set1,set2))

ggpairs(x,aes(alpha = 0.5))
#In this graph,we look at the corr off each pairs
# Next, we’ll look at the correlations within and between the two sets of variables using the matcor function from the CCA package.

# correlations
matcor(set1,set2)

cc1=cc(set1,set2)
cc1$cor # display the canonical correlations

cc1$xcoef
cc1$ycoef
#For the variable d1_sysbp_max , a one unit increase in d1_sysbp_max leads to a 0.03830950 decrease in the first canonical variate of set 2 when all of the other variables are held constant.

CC1_X <- as.matrix(set1) %*% cc1$xcoef[, 1]
CC1_Y <- as.matrix(set2) %*% cc1$ycoef[, 1]


CC2_X <- as.matrix(set1) %*% cc1$xcoef[, 2]
CC2_Y <- as.matrix(set2) %*% cc1$ycoef[, 2]


cor(CC1_X,CC1_Y)
cor(CC2_X,CC2_Y)

#for the variable sodium, a one unit increase in sodium level leads to a 0.28 decreases in the first canonical variate of set 2 when all of the other variables are held constant.

# Next, we’ll use "comput" to compute the loadings of the variables on the canonical dimensions (variates). 
# These loadings are correlations between variables and the canonical variates.


cc2=comput(set1,set2,cc1) #compute canonical loadings
cc2

# The above correlations are between observed variables and canonical variables which are known as the canonical loadings.
# These canonical variates are actually a type of latent variable.

ev=(1 - cc1$cor^2) # tests of canonical dimensions
ev

###############################Classification##################
#Discriminant Analysis

library(MASS)
library(tidyverse)
library(caret)
library(klaR)
theme_set(theme_classic())
#LDA
set.seed(1000)
subset_num_var <- subset(num_var,select = -c(elective_surgery,apache_post_operative,arf_apache,intubated_apache,diabetes_mellitus,immunosuppression,hospital_death))

lda_data <- subset_num_var
lda_data$fever <-ifelse(subset_num_var$d1_temp_max < 38, lda_data$fever<-"0", lda_data$fever<-"1")

training.individuals <- lda_data$fever %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data <- lda_data[training.individuals, ]
test.data <- lda_data[-training.individuals, ]

# Estimate preprocessing parameters
preproc.parameter <- train.data %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transform <- preproc.parameter %>% predict(train.data)
test.transform <- preproc.parameter %>% predict(test.data)


model.lda<-lda(fever~age+bmi+d1_heartrate_max+d1_heartrate_min,data = train.transform, na.action="na.omit")
model.lda

table(predict(model.lda, type="class")$class,train.transform$fever)
accuracy_of_lda <- (354+5)/(354+40+2+5)
accuracy_of_lda

# Make predictions
predictions <- starbucks.lda %>% predict(test.transform)

# Model accuracy
table(predictions$class==test.transform$fever)
mean(predictions$class==test.transform$fever)

############# LOGISTIC REGRESSION ############


index_train = sample(nrow(Patient_Survival_data), round(0.8*nrow(Patient_Survival_data))) #produce index number for train data

train.data  <- Patient_Survival_data[index_train, ]
test.data <- Patient_Survival_data[-index_train, ]


fit<-glm(hospital_death~age+gender+ethnicity+icu_admit_source+pre_icu_los_days+apache_2_diagnosis+apache_3j_diagnosis+heart_rate_apache+d1_diasbp_max+d1_diasbp_min+d1_spo2_max+d1_spo2_min+d1_glucose_max+d1_glucose_min+apache_4a_hospital_death_prob+apache_4a_icu_death_prob, data =train.data, family = "binomial")
summary(fit)
install.packages("glmulti")
library(glmulti)
glmulti.logistic.out <-
  glmulti(hospital_death~bmi+heart_rate_apache+d1_diasbp_min+apache_4a_hospital_death_prob, data = train.data,
          level = 1,               # No interaction considered
          method = "h",            # Exhaustive approach
          crit = "aic",            # AIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "glm",     # glm function
          family = binomial)       # binomial family for logistic regression

## Show 5 best models (Use @ instead of $ for an S4 object)
glmulti.logistic.out@formulas

summary(glmulti.logistic.out@objects[[1]])
fit2 <- glmulti.logistic.out@objects[[1]]

#Assumption check
fit2_subset <- subset(Patient_Survival_data,select = c(hospital_death,bmi,heart_rate_apache,d1_diasbp_min,apache_4a_hospital_death_prob))

#check influential points
plot(fit2, which = 4, id.n = 3) #no problem


# Ho: The regression model fits the data well.
# 
# H1: The logistic regression model does not fit the data well.
library(ResourceSelection)
ht <- hoslem.test(train.data$hospital_death,fitted(fit2),g=10) # g is the number of bins used to calculate quantiles.
ht

library(car)
car::vif(fit2)
#All VIFs < 5. No multicollinearity problem.


# Odds ratios
exp(coef(fit2))

# Odds ratio and 95% CI
exp(cbind(OR = coef(fit2), confint(fit2)))

pred.probabilities <- fit2 %>% predict(test.data, type = "response")
head(pred.probabilities)

predicted.classes_0.2 <- ifelse(pred.probabilities > 0.2, "1", "0")
#Creating confusion matrix
measeures_02 <- confusionMatrix(data=factor(predicted.classes_0.2), reference = factor(test.data$hospital_death),positive = "1")

#Display results 
measeures_02

#rapora model yaz
#dependent variable to be binary
# observations to be independent of each other
# no multicollinearity among the independent variables.
# it requires that the independent variables are linearly related to the log odds.
# requires a large sample size.


#Clustering

library("factoextra")
library(devtools)
library(FactoMineR)


important_numerical_indicators <- subset(subset_num_var, select = c(age,bmi,pre_icu_los_days,apache_2_diagnosis,apache_3j_diagnosis,heart_rate_apache,d1_diasbp_max,d1_diasbp_min,d1_spo2_max,d1_spo2_min,d1_glucose_max,d1_glucose_min,apache_4a_hospital_death_prob,apache_4a_icu_death_prob))

# Visualize
km.res <- kmeans(x = important_numerical_indicators, centers = 2, nstart = 25)

fviz_cluster(km.res, data = important_numerical_indicators,ellipse.type = "convex",palette = "jco",repel = TRUE, ggtheme = theme_minimal())

library("factoextra")
library(FactoMineR)
library(dplyr)
library(devtools)
library(qgraph)

categoricals <- Patient_Survival_data

#Kmeans
#We can compute k-means in R with the kmeans function. Here we will group the data into two clusters (centers = 2)


input <- important_numerical_indicators
input <- scale(input)

library(cluster)
library(factoextra)
fviz_nbclust(input, kmeans, method = "silhouette")
#The results show that 2 clusters maximize the average silhouette values.

sil <- silhouette(clustering$cluster, dist(input))
fviz_silhouette(sil)

clustering <- kmeans(input, centers = 2, nstart = 20)
clustering #K-means clustering with 2 clusters of sizes 379, 121
fviz_nbclust(input, kmeans, method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow
Method")


library(GGally)
library(plotly)
important_numerical_indicators$cluster <- as.factor(clustering$cluster)
p <- ggparcoord(data = important_numerical_indicators, columns = c(1:4), groupColumn = "cluster", scale = "std") +
  labs(x = ".................", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(p)
fviz_cluster(clustering, data = input)
#It can be said that first dimension explains the 15.3% of the variability while the second dimension explains 11.7% of variability


# Hierarchical Cluster Analysis
important_numerical_indicators <- subset(subset_num_var, select = c(age,bmi,pre_icu_los_days,apache_2_diagnosis,apache_3j_diagnosis,heart_rate_apache,d1_diasbp_max,d1_diasbp_min,d1_spo2_max,d1_spo2_min,d1_glucose_max,d1_glucose_min,apache_4a_hospital_death_prob,apache_4a_icu_death_prob))

res = PCA(important_numerical_indicators, scale.unit=TRUE, ncp=7, graph=T)
res.hcpc <- HCPC(res, nb.clust = -1, min=4, max = 4,graph = F)

fviz_dend(res.hcpc, cex = 0.80,
          palette = "jco",
          rect = TRUE, rect_fill = TRUE,
          rect_border = "jco",
          labels_track_height = 0.50,scale="none"
)

fviz_cluster(res.hcpc,
             repel = TRUE,
             show.clust.cent = TRUE,
             palette = "jco",
             main = "Factor map"
)
plot(res.hcpc, choice = "2D.map")


# install.packages("webshot")
# webshot::install_phantomjs()

#Decision Tree
library(rsample) # data splitting
library(dplyr) # data wrangling
library(rpart) # performing regression trees
library(rpart.plot) # plotting regression trees
library(ipred) # bagging
library(caret) # bagging


Patient_Survival_data_split <- initial_split(Patient_Survival_data, prop = .7)
Patient_Survival_data_split_train <- training(Patient_Survival_data_split)
Patient_Survival_data_split_test <- testing(Patient_Survival_data_split)
m1 <- rpart(formula = heart_rate_apache ~ ., data = Patient_Survival_data_split_train, method = "anova")
rpart.plot(m1)
plotcp(m1)


m2 <- rpart(
  formula = heart_rate_apache ~ .,
  data = Patient_Survival_data_split_train,
  method = "anova",
  control = list(cp = 0, xval = 10)
)
plotcp(m2)
abline(v = 4, lty = "dashed")
m3 <- rpart(
  formula = heart_rate_apache ~ .,
  data = Patient_Survival_data_split_train,
  method = "anova",
  control = list(minsplit = 10, maxdepth = 12, xval = 10)
)
hyper_grid <- expand.grid(
  minsplit = seq(1, 10, 1),
  maxdepth = seq(1, 100, 1)
)
head(hyper_grid)
models <- list()
for (i in 1:nrow(hyper_grid)) {
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = heart_rate_apache ~ .,
    data = Patient_Survival_data_split_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}
get_cp <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"]
}
# function to get minimum error
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}
hyper_grid %>%
  mutate(
    cp = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)
penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)
m4 <- rpart(
  formula = heart_rate_apache ~ .,
  data = Patient_Survival_data_split_train,
  method = "anova",
  parms = list(loss = penalty.matrix),
  control = list(minsplit = 1, maxdepth = 49, cp=0.02)
)
rpart.plot(m4)
plotcp(m4)
cp.optim <- m4$cptable[which.min(m4$cptable[,"xerror"]),"CP"]
tree <- prune(m4, cp=cp.optim)
rpart.plot(tree)
pred <- predict(tree,Patient_Survival_data_split_test)
t <- table(pred,Patient_Survival_data_split_test$heart_rate_apache)
rsq <- function (x, y) cor(x, y) ^ 2
R2 <- rsq(pred,Patient_Survival_data_split_test$heart_rate_apache)
R2

#Random Forest
#install.packages("randomForest")
library(randomForest);library(rpart);library(Ecdat)
xs=quantile(Patient_Survival_data$heart_rate_apache,c(0,1/4,1/2,3/4,1))
xs[1]=xs[1]-.00005
Patient_Survival_data_2 <- Patient_Survival_data %>% mutate(category=cut(heart_rate_apache, breaks=xs, labels=c("low","middle","high","vhigh")))
boxplot(Patient_Survival_data$heart_rate_apache~Patient_Survival_data_2$category,col=3:5)
ind=sample(2,nrow(Patient_Survival_data_2),replace=T,prob=c(.7,.3))
train<-Patient_Survival_data_2[ind==1,]
test<-Patient_Survival_data_2[ind==2,]

#Regression Tree
str(subset_num_var)
library(rpart.plot)
library(rpart)
ind=sample(2,nrow(subset_num_var),replace=T,prob=c(.7,.3))
train<-subset_num_var[ind==1,]
test<-subset_num_var[ind==2,]
formula=heart_rate_apache~.
dtree=rpart(formula,train,method="anova",control=rpart.control(minsplit=30,cp=0.001))
rpart.plot(dtree)
summary(dtree)
printcp(dtree)
plotcp(dtree)
printcp(dtree)
pdtree<- prune(dtree, cp=dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"])
plot(pdtree, uniform=TRUE,
     main="Pruned Classification Tree For Heart Rate")
text(pdtree, use.n=TRUE, all=TRUE, cex=.8)
rpart.plot(pdtree, uniform=TRUE,
           main="Pruned Classification Tree For Heart Rate")
# cross-validation results
#Make factor to the response
xs=quantile(subset_num_var$heart_rate_apache,c(0,1/4,1/2,3/4,1))
xs[1]=xs[1]-.00005
library(tidyverse)
library(dplyr)
Patient_Survival_data_2 <- subset_num_var %>% mutate(category=cut(heart_rate_apache, breaks=xs, labels=c("low","middle","high","vhigh")))
boxplot(subset_num_var$heart_rate_apache~Patient_Survival_data_2$category,col=3:5)

for (i in 1:ncol(Patient_Survival_data_2)) { # For every column...
  if (typeof(Patient_Survival_data_2[[i]]) == 'character') { # if the column type is character...
    Patient_Survival_data_2[[i]] <- as.factor(Patient_Survival_data_2[[i]]) # Convert it to factor.
  }
}
ind=sample(2,nrow(Patient_Survival_data_2),replace=T,prob=c(.7,.3))
train<-Patient_Survival_data_2[ind==1,]
test<-Patient_Survival_data_2[ind==2,]
library(randomForest)
str(train)
rf.pros<-randomForest(category~.,data = train)
rf.pros
#Random Forest
library(dplyr)
model1 <- randomForest(category ~ ., data = train, importance = TRUE)
model1
# Fine tuning parameters of Random Forest model
model2 <- randomForest(category ~ ., data = train, ntree = 500, mtry = 6, importance = TRUE)
model2
plot(model2)
# Predicting on train set
predTrain <- predict(model2, train, type = "class")
# Checking classification accuracy
table(predTrain, train$category)
predValid <- predict(model2, test, type = "class")
# Checking classification accuracy
mean(predValid == test$category)
table(predValid,test$category)
# To check important variables
importance(model2)
varImpPlot(model2)
accuracy_Test1 <- sum(diag(t)) / sum(t)
accuracy_Test1
xs=quantile(subset_num_var$heart_rate_apache,c(0,1/4,1/2,3/4,1))
xs[1]=xs[1]-.00005
Patient_Survival_data_2 <- subset_num_var %>% mutate(category=cut(heart_rate_apache, breaks=xs, labels=c("low","middle","high","vhigh")))
boxplot(subset_num_var$heart_rate_apache~Patient_Survival_data_2$category,col=3:5)


ind=sample(2,nrow(Patient_Survival_data_2),replace=T,prob=c(.7,.3))
train<-Patient_Survival_data_2[ind==1,]
test<-Patient_Survival_data_2[ind==2,]
print(table(Patient_Survival_data_2$category))
library(e1071)
NBclassfier=naiveBayes(category~., data=train)
print(NBclassfier)
printALL=
  function(model){
    trainPred=predict(model, train, type = "class")
    trainTable=table(train$category, trainPred)
    testPred=predict(NBclassfier,test, type="class")
    testTable=table(test$category, testPred)
    trainAcc=(trainTable[1,1]+trainTable[2,2]+trainTable[3,3]+trainTable[4,4])/sum(trainTable)
    testAcc=(testTable[1,1]+testTable[2,2]+testTable[3,3]+testTable[4,4])/sum(testTable)
    message("Contingency Table for Training Data")
    print(trainTable)
    message("Contingency Table for Test Data")
    print(testTable)
    message("Accuracy")
    print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
  }
printALL(NBclassfier)

library(naivebayes)
newNBclassifier=naive_bayes(category~.,usekernel=T, data=train)
printALL(newNBclassifier)

#xgboost
library(tidyverse)
library(ISLR)
ml_data <- Patient_Survival_data_2
ml_data %>%
  glimpse()
ind=sample(2,nrow(ml_data),replace=T,prob=c(.7,.3))
train<-Patient_Survival_data_2[ind==1,]
test<-Patient_Survival_data_2[ind==2,]
# Train model with preprocessing & repeated cv
library(caret)
library(gbm)
model_gbm <- caret::train(category ~ .,
                          data = train,
                          method = "gbm",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv",
                                                   number = 5,
                                                   repeats = 5,
                                                   verboseIter = FALSE),
                          verbose = 0)
model_gbm
caret::confusionMatrix(
  data = predict(model_gbm, test),
  reference = test$category
)
#with numeric y xgboost
head(subset_num_var)
ind=sample(2,nrow(subset_num_var),replace=T,prob=c(.7,.3))
train<-subset_num_var[ind==1,]
test<-subset_num_var[ind==2,]


library(xgboost)

train_df_model <- train
y_train <- train_df_model$heart_rate_apache
train_df_model$y <- NULL
#Row binding train & test set for feature engineering
train_test <- bind_rows(train_df_model, test)
ntrain <- nrow(train_df_model)
features <- names(train)
#convert character into integer
for (f in features) {
  if (is.character(train_test[[f]])) {
    levels = sort(unique(train_test[[f]]))
    train_test[[f]] = as.integer(factor(train_test[[f]],levels = levels))
  }
}
#splitting whole data back again
train_x <- train_test %>% .[1:ntrain,]
test_x <- train_test %>% .[(ntrain + 1):nrow(train_test),]
#convert into numeric for XGBoost implementation
train_x[] <- map(train_x, as.numeric)
test_x[] <- map(test_x, as.numeric)
dtrain <- xgb.DMatrix(as.matrix(train_x),label = y_train)
dtest <- xgb.DMatrix(as.matrix(test_x))
xgb_params <- list(colsample_bytree = 0.7, #how many variables to consider for each tree
                   subsample = 0.7, #how much of the data to use for each tree
                   booster = "gbtree",
                   max_depth = 5, #how many levels in the tree
                   eta = 0.1, #shrinkage rate to control overfitting through conservative approach
                   eval_metric = "rmse",
                   objective = "reg:linear",
                   gamma = 0)
xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 100, nfold = 4, nrounds=500)
xgb_cv$evaluation_log %>%
  gather(type,value, -iter) %>%
  ggplot(aes(iter,value)) + geom_line(aes(colour = type))
gb_dt <- xgb.train(xgb_params,dtrain,nrounds = 54)
gb_dt$params
test_preds <- predict(gb_dt,dtest)
summary(gb_dt)
rmse <- sqrt(mean((test_preds-test$heart_rate_apache)^2))
rmse
R2 <- rsq(test_preds,test$heart_rate_apache)
all.test <- data.frame(test$heart_rate_apache ,test_preds)
# Predicted vs. actual for each model
ggplot(data = all.test , aes(x = test$heart_rate_apache , y = test_preds)) +
  geom_point(colour = "blue")+geom_abline(intercept = 0, slope = 1, colour = "red") +
  ggtitle("Predicted vs. Actual, by model")











