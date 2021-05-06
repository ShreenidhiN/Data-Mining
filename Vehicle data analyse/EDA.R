#Importing required libraries
library(ggplot2)
library(dplyr)

#Read the data
data <- read.delim(file.choose(), sep=' ', header=FALSE)

my_data <- data.frame(data[c(1,8,11,19)])
colnames(my_data) <- c("X95", "X42", "X176", "y")

features <- data[c(1,8,11)]
colnames(features) <- c("X95", "X42", "X176")
print(features)


########## EXPLORATORY DATA ANALYSIS ##########

"1 . Compute the covariance matrix for the three numerical attributes you are analyzing;
also compute the correlation for each of the three pairs of attributes. 
Interpret the statistical findings."

print(cov(features, y = features, use = "all.obs",method = c("pearson", "kendall", "spearman")))

print("Correlation between feature 1 and 2 ")
print(cor(features[1], features[2], method = c("pearson", "kendall", "spearman")))

print("Correlation between feature 2 and 3 ")
print(cor(features[2], features[3], method = c("pearson", "kendall", "spearman")))

print("Correlation between feature 1 and 3 ")
print(cor(features[1], features[3], method = c("pearson", "kendall", "spearman")))

"2. Create a scatter plot for the last two numerical attributes of your dataset. Interpret the scatter plot."

features$X95 <- unlist(features$X42)
features$X42 <- unlist(features$X176)
p1 <- ggplot(my_data, aes(x = X42,y = X176)) + geom_point()
print(p1)



"3. Create histograms for the first 2 numerical attributes each for the whole dataset and the instances of each of the 4 classes. That is, you create 10 histograms. Interpret the obtained displays."



p2 <- ggplot(features, aes(x=features$X95))+geom_histogram(color="darkblue", fill="lightblue")
print(p2)

p3 <- ggplot(features, aes(x=features$X42))+geom_histogram(color="darkblue", fill="lightblue")
print(p3)


class1 <- my_data[with(my_data, my_data[4] == "opel"), ]
class2 <- my_data[with(my_data, my_data[4] == "saab"), ]
class3 <- my_data[with(my_data, my_data[4] == "bus"), ]
class4 <- my_data[with(my_data, my_data[4] == "van"), ]

p4 <- ggplot(class1, aes(x= X95))+geom_histogram(color="darkblue", fill="lightblue")
print(p4 + ggtitle("X95 for opel"))

p5 <- ggplot(class2, aes(x= X95))+geom_histogram(color="darkblue", fill="lightblue")
print(p5 + ggtitle("X95 with saab"))

p6 <- ggplot(class3, aes(x= X95))+geom_histogram(color="darkblue", fill="lightblue")
print(p6 + ggtitle("X95 with bus"))

p7 <- ggplot(class4, aes(x= X95))+geom_histogram(color="darkblue", fill="lightblue")
print(p7 + ggtitle("X95 with van"))

p8 <- ggplot(class1, aes(x= X42))+geom_histogram(color="darkblue", fill="lightblue")
print(p8 + ggtitle("X42 with opel"))

p9 <- ggplot(class2, aes(x= X42))+geom_histogram(color="darkblue", fill="lightblue")
print(p9 + ggtitle("X42 with saab"))

p10 <- ggplot(class3, aes(x= X42))+geom_histogram(color="darkblue", fill="lightblue")
print(p10 + ggtitle("X42 with bus"))

p11 <- ggplot(class4, aes(x= X42))+geom_histogram(color="darkblue", fill="lightblue")
print(p11 + ggtitle("X42 with van"))


"4. Create box plots for the COMPACTNESS attribute for the instances of each class and a fifth box plot for all instances in the dataset. Interpret and compare the 5 box plots."

p12 <- boxplot(class1$X95, class2$X95, class3$X95, class4$X95, main='X95 for each class')
print(p12)

p13<- box_plot(my_data$X95, main='X95 for the whole dataset')
print(p13)