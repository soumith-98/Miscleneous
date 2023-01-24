cereal_data=read.csv(file = 'cereal.csv')
head(cereal_data)

quartz()
plot(cereal_data$calories,cereal_data$rating,  ylab="rating", xlab="clories")
boxplot(cereal_data$protein)
boxplot(cereal_data$calories,cereal_data$rating,  ylab="rating", xlab="clories")
plot(cereal_data)
boxplot(cereal_data$calories,cereal_data$rating,  ylab="calories", xlab="rating")
barplot(cereal_data$calories,cereal_data$rating,  ylab="calories", xlab="rating")
plot(cereal_data$calories,cereal_data$rating,  ylab="rating", xlab="calories")
plot(cereal_data)
boxplot(cereal_data)
(data = -1)

plot(cereal_data$sodium,cereal_data$rating,  ylab="rating", xlab="sodium")
plot(cereal_data$protein,cereal_data$rating,  ylab="rating", xlab="protein") #Protein is constituting for the majority of the rating but dosen't follow a specific pattern#
plot(cereal_data$fat,cereal_data$rating,  ylab="rating", xlab="fat") #With increase in fat content the rating of cereal is dropped#
plot(cereal_data$fiber,cereal_data$rating,  ylab="rating", xlab="fiber")  #With increase in fiber content the rating of cereal is dropped except for 2 cereals#
plot(cereal_data$carbo,cereal_data$rating,  ylab="rating", xlab="carbo")# The rating is high for low carb cereals and vice versa#
plot(cereal_data$sugars,cereal_data$rating,  ylab="rating", xlab="sugars") # There is a linear drop in rating with increasing sugars in cereals#
plot(cereal_data$potass,cereal_data$rating,  ylab="rating", xlab="potass") #potass dosen't contribue much for rating of cereals#
plot(cereal_data$vitamins,cereal_data$rating,  ylab="rating", xlab="vitamins") #vitamins dosen't contribute for rating#
plot(cereal_data$mfr,cereal_data$rating,  ylab="rating", xlab="mfr")
cereal-dara.heatmap()
df$shelf <- NULL
drops <- c("shelf","cups")
cereal <- cereal_data
cereal <- cereal[-c(shelf),]
drops <- c("shelf")
df = subset(cereal, select = -c(shelf,cups) )# Tried dropping a couple of columns which dosen't contribute for rating#
new_cereal = subset(df,select = -c(type))# Tried dropping a couple of columns which dosen't contribute for rating#
save(new_cereal,cereal,file="My_cereal.Rdata")# Save data into new data set after dropping#
save(new_cereal,cereal,file="My_cereal.csv")
save(new_cereal,file="My_cereal1.csv")
#Multiple Regeression#




plot(new_cereal)
multiple.regression <- lm(rating ~ calories+protein+fat+sodium+fiber+carbo+sugars+potass+vitamins, data = new_cereal)
summary(multiple.regression)#  Here comes the summary of multiple Regression
install.packages("MASS") # installing package mass which has many data sets of different cities of different countries
library(MASS)
ls("package:MASS")
boston # Calling thw dataset Boston
plot(Boston) # Plotting all the graphs in the dataset
plot(Boston$crim,Boston$ptratio,  ylab="ptratio", xlab="crim") # Crim is not contributing for ptratio
plot(Boston$zn,Boston$ptratio,  ylab="ptratio", xlab="zn")# zn and ptratio are spread all along the graph and not concentrated at a single point
plot(Boston$indus,Boston$ptratio,  ylab="ptratio", xlab="indus")# All the suburbs are concentrated at lowest indus value when pratio is taken on y.
plot(Boston$chas,Boston$ptratio,  ylab="ptratio", xlab="chas")# Chas values are all constant with only two values 0 and 1.
plot(Boston$nox,Boston$ptratio,  ylab="ptratio", xlab="nox")# pratio is distributed along the nox values
plot(Boston$rm,Boston$ptratio,  ylab="ptratio", xlab="rm")# when pratios plotted aganist rm all the suburbs are concentreted at the center
plot(Boston$age,Boston$ptratio,  ylab="ptratio", xlab="age")# The age data aganist ptratio is wide spread and not coagulated
plot(Boston$dis,Boston$ptratio,  ylab="ptratio", xlab="dis")# the frequency is distributed at the center
plot(Boston$rad,Boston$ptratio,  ylab="ptratio", xlab="rad")# The frequency is all distributed amoung lower rad 
plot(Boston$tax,Boston$ptratio,  ylab="ptratio", xlab="tax")# All the suburbs are scattered
plot(Boston$ptratio,Boston$ptratio,  ylab="ptratio", xlab="ptratio")
plot(Boston$black,Boston$ptratio,  ylab="ptratio", xlab="black")# All the suburbs are concentrated at higher black value compared to ptratio
plot(Boston$lstat,Boston$ptratio,  ylab="ptratio", xlab="lstat")# all the values are wide spread
plot(Boston$medv,Boston$ptratio,  ylab="ptratio", xlab="medv")
plot(crimtab)
boxplot(crim)
boxplot(Boston$crim)# The suburd with highest crime per cap is above 80
barplot(Boston$crim)
plot(Boston$crim)