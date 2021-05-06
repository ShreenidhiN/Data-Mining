library(mltools)
library(data.table)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(magrittr)

# importing the dataset and selecting the desired values

data = read.csv("xaa.csv", header = FALSE, sep = " ")
d = data[c("V1","V8","V11")]


# 1.Compute the covariance matrix for the three numerical attributes you are analyzing;
# also compute the correlation for each of the three pairs of attributes. Interpret the
# statistical findings.

covariance = cov(d)
print("Covariance of the dataset:")
print(cov(d))

corelation = cor(d)
print("Corelation of the dataset:")
print(cor(d))

# Interpretation: From the covariance and corelation matrix it can be that COMPACTNESS and ELONGATEDNESS are 
# negatively corelated while COMPACTNESS and SCALED VARIANCE ALONG MAJOR AXIS are postively corelated. 
# On the other hand ELONGATEDNESS and SCALED VARIANCE ALONG MAJOR AXIS are highly negatively corelated.
##################################################################################################################





# 2. Create a scatter plot for the last two numerical attributes of your dataset. Interpret the
# scatter plot.

scatter_plot = ggplot(data, aes(x = V8, y = V11)) +
  geom_point(color = "#00AFBB")

scatter_plot +
  labs( x = "ELONGATEDNES",
        y = "SCALED VARIANCE",
        title = "ELONGATEDNES VS SCALED VARIANCE ALONG MAJOR AXIS" )
# Interpretation: From the scatter plot we can see a downward trend for the plotted data.
##################################################################################################################






# 3.Create histograms for the first 2 numerical attributes each for the whole dataset and
# the instances of each of the 4 classes. That is, you create 10 histograms. Interpret the
# obtained displays.

# Entire Dataset
# Compactness
ggplot(d, aes(x=V1)) + 
  geom_histogram(color="darkblue", fill="lightblue") +
  labs( x = "COMPACTNESS",
        title = "COMPACTNESS" )


# Elongatedness
ggplot(d, aes(x=V8)) + 
  geom_histogram(fill="#69b3a2", color="#e9ecef") +
  labs( x = "ELONGATEDNESS",
        title = "ELONGATEDNESS" )

# Interpretation: The COMPACTNESS attribute follows a normal distribution 
# while the ELONGATEDNESS attribte has more than one peak in its distribution


# Opel
# Compactness
data %>%
  filter( V19 == "opel" ) %>%
  ggplot( aes(x=V1)) +
  geom_histogram(bins= 30, color="darkblue", fill="lightblue", alpha=0.9) +
  ggtitle("COMPACTNESS of OPEL") 

# Elongatedness
data %>%
  filter( V19 == "opel" ) %>%
  ggplot( aes(x=V8)) +
  geom_histogram(bins= 30, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("ELONGATEDNESS of OPEL") 


# SAAB
# Compactness
data %>%
  filter( V19 == "saab" ) %>%
  ggplot( aes(x=V1)) +
  geom_histogram(bins= 30, color="darkblue", fill="lightblue", alpha=0.9) +
  ggtitle("COMPACTNESS of SAAB") 

# Elongatedness
data %>%
  filter( V19 == "saab" ) %>%
  ggplot( aes(x=V8)) +
  geom_histogram(bins= 30, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("ELONGATEDNESS of SAAB") 



# BUS
# Compactness
data %>%
  filter( V19 == "bus" ) %>%
  ggplot( aes(x=V1)) +
  geom_histogram(bins= 30, color="darkblue", fill="lightblue", alpha=0.9) +
  ggtitle("COMPACTNESS of BUS") 

# Elongatedness
data %>%
  filter( V19 == "bus" ) %>%
  ggplot( aes(x=V8)) +
  geom_histogram(bins= 30, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("ELONGATEDNESS of BUS") 



# Van
# Compactness
data %>%
  filter( V19 == "van" ) %>%
  ggplot( aes(x=V1)) +
  geom_histogram(bins= 30, color="darkblue", fill="lightblue", alpha=0.9) +
  ggtitle("COMPACTNESS of Van") 

# Elongatedness
data %>%
  filter( V19 == "van" ) %>%
  ggplot( aes(x=V8)) +
  geom_histogram(bins= 30, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("ELONGATEDNESS of Van") 

# Interpretation: OPEL and SAAB have most of its datapoints concentrated to a particular value while
# BUS has more wide spread values. VAN has a normal distribution of its data.

##################################################################################################################





# 4.Create box plots for the COMPACTNESS attribute for the instances of each class and
# a fifth box plot for all instances in the dataset. Interpret and compare the 5 box plots.

# Entire dataset
boxplot(d$V1, 
        main = "COMPACTNESS (for the entire dataset)",
        xlab = "COMPACTNESS",
        col = "#FFDB6D",
        outcol = "#C4961A")

# For each Silhouettes
box_plot = ggplot(data, aes(V19, V1)) +
  geom_boxplot(fill = "#FFDB6D", color = "#C4961A")
box_plot +
  labs( x = "Vehicle Silhouettes",
        y = "COMPACTNESS",
        title = "COMPACTNESS" )


# Interpretation: The COMPACTNESS of entire dataset is distributed between 70 and 110 with one outlier.
#  The COMPACTNESS of BUS is distributed between 82 and 110.
#  The COMPACTNESS of VAN is distributed between 85 and 100.
#  The COMPACTNESS of OPEL is distributed between 78 and 105 with one outlier.
#  The COMPACTNESS of SAAB is distributed between 80 and 120.
##################################################################################################################









# 5.Create 3 supervised scatter plots using 2 of the 3 attributes and the class variable; use
# different colors for the class variable. Interpret the scatter plots.

# Scatter plot for all the classes
scatter_plot = ggplot(data, aes(x = V8, y = V11)) +
  geom_point(aes(color = factor(V19)))

scatter_plot +
  labs( x = "ELONGATEDNESS",
    y = "SCALED VARIANCE",
    color = "Vehicle Silhouettes",
    title = "ELONGATEDNESS VS SCALED VARIANCE ALONG MAJOR AXIS" )


# Scatter plot for van
van = data %>% filter( V19 == "van" )
ggplot(van, aes(x = V8, y = V11)) +
  geom_point(color = 'violetred1') +
  labs( x = "ELONGATEDNESS",
        y = "SCALED VARIANCE",
        title = "ELONGATEDNESS VS SCALED VARIANCE ALONG MAJOR AXIS FOR VAN" )

# Scatter plot for opel
opel = data %>% filter( V19 == "opel" )
ggplot(opel, aes(x = V8, y = V11)) +
  geom_point(color = 'violetred2') +
  labs( x = "ELONGATEDNESS",
        y = "SCALED VARIANCE",
        title = "ELONGATEDNESS VS SCALED VARIANCE ALONG MAJOR AXIS FOR OPEL" )

# Scatter plot for saab
saab = data %>% filter( V19 == "saab" )
ggplot(saab, aes(x = V8, y = V11)) +
  geom_point(color = 'violetred3') +
  labs( x = "ELONGATEDNESS",
        y = "SCALED VARIANCE",
        title = "ELONGATEDNESS VS SCALED VARIANCE ALONG MAJOR AXIS FOR SAAB" )

# Scatter plot for bus
bus = data %>% filter( V19 == "bus" )
ggplot(bus, aes(x = V8, y = V11)) +
  geom_point(color = 'violetred') +
  labs( x = "ELONGATEDNESS",
        y = "SCALED VARIANCE",
        title = "ELONGATEDNESS VS SCALED VARIANCE ALONG MAJOR AXIS FOR BUS" )

# Interpretation: Like the dataset and all the classes have a downward trend.
# But the class VAN has a much lower trend when compared to all other classes.

##################################################################################################################





# 6.Fit a linear model that predicts the dependent variable B and H using all the 18 numerical
# attributes as independent variables

sub_data = subset(data, select = -c(V19,V20) )
data_z <- sapply(sub_data, function(sub_data) (sub_data-mean(sub_data))/sd(sub_data))
newdata <- one_hot(as.data.table(data))
B = newdata$V19_bus
V = newdata$V19_van
data_z = cbind(data_z,B)
data_z = cbind(data_z,V)
data_z = as.data.frame(data_z)


# Model fitting for BUS
lm_b = lm(B ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + 
                V13 + V14 + V15 + V16 + V17 + V18, data = data_z, family = binomial)
summary(lm_b)
# Coefficients
print(lm_b$coefficients)
# Negative and positive coefficients
coef = c(lm_b$coefficients)
print(coef[order(coef)])



# Model fitting for VAN
lm_v = lm(V ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + 
            V13 + V14 + V15 + V16 + V17 + V18, data = data_z, family = binomial)
summary(lm_v)
# Coefficients
print(lm_v$coefficients)
# Negative and positive coefficients
coef = c(lm_v$coefficients)
print(coef[order(coef)])

# Interpretation: 
# R square value for BUS linear model: 0.7598
# R square value for VAN linear model: 0.8047
#
# Comparing both the linear models it can be seen that the attributes V4, V8, V17 and V5 have more affect on  
# BUS model while attributes V7, V2, V12, V10 and V3 have more say in the VAN model.  
# Attributes like V13, V16, V11 and V1 has very low effect in both the models
##################################################################################################################
# 8. Write a conclusion summarizing the most important findings of task1-7-what did we learn
# about the dataset? In particular, address the findings obtained related to predicting buses,
# vans, and all 4 classes using the attributes in the dataset. Also assess the difficulty of your
# classification task.

COMPACTNESS and ELONGATEDNESS are negatively correlated and COMPACTNESS and SCALED VARIANCE ALONG MAJOR AXIS are positively correlated. ELONGATEDNESS and SCALED VARIANCE ALONG MAJOR AXIS are highly negativey correlated. We generally observe the downward trend in the data set. That is the relation between the different attributes follow this trend and it is also observed for each classes. While the COMPACTNESS attribute follow normal distribution, we can see that the ELONGATEDNESS attribute has more than one peak. Some attributes have more effect on classifying the given dtapoint as BUS and some others have more effect on classifying it as VAN. Some attributes doesn't amount to much during the classification. This is one of the main difficulties faced during the classification task.

##################################################################################################################

# 9. Are there any other interesting observations about your dataset?


It is observed that the ELONGATEDNESS attribute has multiple peaks makes it an interesting attribute when applying the classification model. Also while fitting the model, we can observe that different attributes affect different model of vehicles. And also we saw that while OPEL and SAAB have more of its datapoints concetrated on a particular value, VAN has a normal distribution, and BUS has a more wide spread values.


##################################################################################################################
