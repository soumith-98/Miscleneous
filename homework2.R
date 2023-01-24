### Loading the data
library(recommenderlab)
data("MovieLense")
MovieLense
###?MovieLense
head(as(MovieLense[5,], "list")[[1]]) # first user ratings
#graphics.off()
## visual part of the matrix ##################### 

image(MovieLense[1:100,1:100]) 

## total number of user ratings

hist(rowCounts(MovieLense)) 

## total number of user movie ratings 

hist(colCounts(MovieLense)) 

## mean rating

mean(rowMeans(MovieLense)) 

## user meta data that is available

head(MovieLenseUser)


### rating matrix
dim(getRatingMatrix(MovieLense))
getRatingMatrix(MovieLense)[1:20, 1:20]

### Plotting the raw rating
image(MovieLense[1:20, 1:20], main = "Raw Ratings")

hist(getRatings(MovieLense), breaks = 100, main = "Histogram ratings")

#####Normalized matrix###### 

Normalized_data <- normalize(MovieLense) 

getRatingMatrix(Normalized_data)[1:20, 1:20] 

#####Normalized data visualization


image(Normalized_data[1:100,1:100], main = "Ratings that have been normalized") 


######Putting together a binary matrix###### 

binarized_data <- binarize(MovieLense, minRating = 4) 

getRatingMatrix(binarized_data)[1:10, 1:10] 

image(binarized_data[1:100,1:100], main = "Binarized evaluations")



######Data is divided into two groups: training and testing.######## 

set.seed(1) 

MovieLense_split <- sample(MovieLense,4000,replace=TRUE) 

dim(MovieLense_split) 

eval<- evaluationScheme(MovieLense_split, method = "cross", k = 5, given = 15, goodRating = 4) 

eval 

algorithms <- list("random items" = list(name= "RANDOM", param=NULL),
                   "popular items"= list(name = "POPULAR", param =NULL),
                   "user-based CF"= list(name = "UBCF", param = list(nn=25)),
                   "item-based CF"= list(name = "IBCF", param = list(nn=25))
)


######Creating a recommender system###### 

model_1 <- Recommender(getData(eval,"train"), "UBCF",
                       param=list(method="Cosine", nn=10)) 

names(getModel(model_1)) 

#######Predict the ratings that are missing.########

P1<- predict(model_1, getData(eval, "known"), type="ratings")

ERROR_1<- rbind(UBCF = calcPredictionAccuracy(P1, getData(eval,"unknown")))
ERROR_1

result_1<- evaluate(eval, algorithms, type="topNList", n=c(1,3,5,10,15,20))


plot(result_1,annotate=TRUE,main="ROC curve") 

graphics.off() 


plot(result_1, "prec/rec", annotate=TRUE, main="Prec/Rec curve") 

graphics.off() 

##########Create a list of the top ten recommendations for three users.##########
top10 <- predict(model_1, MovieLense[5:7], n=10)



#problem 2
data(state)
d=state.x77
d=scale(d)
head(d)
clust = hclust(dist(d),method="ave")
plot(clust,hang = -1)



library(cluster)
tree1 = cutree(clust, k = 2)
sih = silhouette(tree1,dist = dist(d))

plot(sih)

for(i in 2:10){
  tree1 = cutree(clust, k = i)
  sih = silhouette(tree1,dist = dist(d))
  sm = summary(sih)$avg.width
  print(sm)
}

############ 
# SOM 
############

library(kohonen)
set.seed(123)
smg = somgrid(xdim = 4, ydim=4, topo = "hexagonal")
s.som = som(d,grid = smg,rlen = 10000)

plot(s.som)

codes = s.som$codes[[1]]

plot(s.som,type = "changes")

plot(s.som,type = "count")

plot(s.som,type = "mapping")


plot(s.som,type = "dist.neighbours")

dis = dist(codes)
hct = hclust(dis)


plot(hct)

for(i in 2:10){
  clust = cutree(hct, k = i)
  sih = silhouette(clust,dist = dis)
  sm= summary(sih)$avg.width
  print(sm)
}

s = cutree(hct,k = 4)


my_pal = c("orange","cyan","green","purple")
my_bhcol = my_pal[s]

plot(s.som,type="mapping",col = "red",bgcol = my_bhcol)
add.cluster.boundaries(s.som,s)

#2(d)

#Clustering should always be done using scaled data since larger magnitude columns will skew cluster formation because the euclidean distance will be blown up, resulting in useless clusters with points that are not homogeneous except for the high magnitude columns. The normalised WSS for those clusters will also be high.
#When the scaling was done in the preceding scenario, 31 states had less crime in all dimensions, clusters were easily identifiable, and they were heterogeneous as well.However, when clustering without scaling, the clusters were near in terms of criminality, and the standard deviation of points within each cluster was considerable when compared to clustering with scaled data, implying that clustering without scaling is erroneous.

#problem 3
data(iris)
head(iris)
library(cluster)
library(fossil)
dats0 = iris[,1:4]
YY = iris[,5]

dats = scale(dats0)

#calculating the main components
pc_ex <- prcomp(dats, center = FALSE, scale = FALSE)


co=as.numeric(unique(YY))

pc_ex$rotation

PC_Dat =c(pc_ex$x[,1],pc_ex$x[,2])

plot(PC_Dat,col=co,main="Principal Components Plot")

##### (3b) ######
reduced_data = data.frame(pc_ex$x[,1:2])

km2 = kmeans(reduced_data, centers = 3, nstart = 10)g
plot(reduced_data,col=km2$cluster,pch=km2$cluster,cex=1.5,main="Clustered Points Plot")
###### (3c) ######

rand.index(km2$cluster, as.numeric(iris$Species))
adj.rand.index(km2$cluster, as.numeric(iris$Species))

###### (3d) ######

# kmeans - gap statistics
gap_kmeans <- clusGap(dats0, kmeans, nstart = 20, K.max = 10, B = 100)


plot(gap_kmeans, main = "kmeans - gap statistics")
#We can observe from the gap statistic plot that the ideal 
#k would be between 3 and 4

km = kmeans(dats0, centers = 3, nstart = 10)
d = dist(dats0)
s = silhouette(km$cluster,dist=d)


plot(s)

###### (3E) ######
#rand index = 0.8322148
# adjusted rand index is 0.6201352 which justifies that k=3
#from gap statistics plot the optimal k lies at 2 
#the silhouette plot with k=3 has an average silhouette width is 0.53 which suggests a reasonable structure has been formed 
