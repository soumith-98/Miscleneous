cereal_data=read.csv(file = 'cereal.csv')
head(cereal_data)


plot(cereal_data$calories,cereal_data$rating,  ylab="rating", xlab="clories")
boxplot(cereal_data$protein)
boxplot(cereal_data$calories,cereal_data$rating,  ylab="rating", xlab="clories")
plot(cereal_data)
boxplot(cereal_data$calories,cereal_data$rating,  ylab="calories", xlab="rating")
barplot(cereal_data$calories,cereal_data$rating,  ylab="calories", xlab="rating")
plot(cereal_data$calories,cereal_data$rating,  ylab="rating", xlab="calories")
plot(cereal_data)


plot(cereal_data$sodium,cereal_data$rating,  ylab="rating", xlab="sodium")
plot(cereal_data$protein,cereal_data$rating,  ylab="rating", xlab="protein") #Protein is constituting for the majority of the rating but dosen't follow a specific pattern#
plot(cereal_data$fat,cereal_data$rating,  ylab="rating", xlab="fat") #With increase in fat content the rating of cereal is dropped#
plot(cereal_data$fiber,cereal_data$rating,  ylab="rating", xlab="fiber")  #With increase in fiber content the rating of cereal is dropped except for 2 cereals#
plot(cereal_data$carbo,cereal_data$rating,  ylab="rating", xlab="carbo")# The rating is high for low carb cereals and vice versa#
plot(cereal_data$sugars,cereal_data$rating,  ylab="rating", xlab="sugars") # There is a linear drop in rating with increasing sugars in cereals#
plot(cereal_data$potass,cereal_data$rating,  ylab="rating", xlab="potass") #potass dosen't contribue much for rating of cereals#
plot(cereal_data$vitamins,cereal_data$rating,  ylab="rating", xlab="vitamins") #vitamins dosen't contribute for rating#
drops <- c("shelf","cups")
cereal <- cereal_data
drops <- c("shelf")
df = subset(cereal, select = -c(shelf,cups) )# Tried dropping a couple of columns which dosen't contribute for rating#
new_cereal = subset(df,select = -c(type))# Tried dropping a couple of columns which dosen't contribute for rating#
save(new_cereal,cereal,file="My_cereal.Rdata")# Save data into new data set after dropping#
save(new_cereal,cereal,file="My_cereal.csv")
save(new_cereal,file="My_cereal1.csv")
write.table(new_cereal, sep=",", file="cereal3.csv",row.names=FALSE, col.names=colnames(new_cereal))


#Multiple Regeression#


#2.

plot(new_cereal)
multiple.regression <- lm(rating ~ calories+protein+fat+sodium+fiber+carbo+sugars+potass+vitamins, data = new_cereal)
summary(multiple.regression)#  Here comes the summary of multiple Regression
#2.a Here in the cereal data set columns calories, proteins, fat,sodium,fiber,carbo,sugars,potass,vitamins  all hold significant relationship to the response
#2.b.The coefficient varible for sugar in multiple regression suggets that it holds a significant role in the rating of different cereals in the dataset
summary(lm(rating~sodium*carbo ,data=cereal))# 2.C Interaction



#3.A
#install.packages("MASS") # installing package mass which has many data sets of different cities of different countries
library(MASS)
ls("package:MASS")
Boston # Calling thw dataset Boston
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

#3.B
plot(Boston$zn,Boston$crim,  ylab="crim", xlab="zn")
plot(Boston$indus,Boston$crim,  ylab="crim", xlab="indus")
plot(Boston$chas,Boston$crim,  ylab="crim", xlab="chas")
plot(Boston$nox,Boston$crim,  ylab="crim", xlab="nox")#increase
plot(Boston$rm,Boston$crim,  ylab="crim", xlab="rm")# drop
plot(Boston$age,Boston$crim,  ylab="crim", xlab="age")# increase
plot(Boston$dis,Boston$crim,  ylab="crim", xlab="dis")# drop
plot(Boston$rad,Boston$crim,  ylab="crim", xlab="rad")
plot(Boston$tax,Boston$crim,  ylab="crim", xlab="tax")
plot(Boston$ptratio,Boston$crim,  ylab="crim", xlab="ptratio")
plot(Boston$black,Boston$crim,  ylab="crim", xlab="black")
plot(Boston$lstat,Boston$crim,  ylab="crim", xlab="lstat")# distribute
#3.B factors such as nox,rm,age,dis,black,lstat are contributing for percapita crime change



#3.C

boxplot(Boston$crim)# The suburd with highest crime per cap is above 80
barplot(Boston$crim)
plot(Boston$crim)
barplot(Boston$tax)
barplot(Boston$ptratio)# Crime rate and tax showed peaks at some suburbs around Boston but ptratio has maintained a similiar treand throughout thedata set




#3.d
length(which(Boston$rm>7))
length(which(Boston$rm>8))
#suburbs that average more than eight rooms per dwelling are significantlly very low co,pared to that with more than 7 that prove that there are more suburbs with average no. of rooms are 7

