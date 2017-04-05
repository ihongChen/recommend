# recommenderlab ----------------------------------------------------------
install.packages("recommenderlab")
library(recommenderlab)
m <- matrix(sample(c(as.numeric(0:5),NA),50,
                   replace=T,prob = c(rep(.4/6,6),.6)),ncol=10,
                   dimnames = list(user=paste("u",1:5,sep=''),
                                   item=paste("i",1:10,sep='')) )

similarity(r,method="jaccard")

r <- as(m,"realRatingMatrix")
getRatingMatrix(r)
identical(as(r,"matrix"),m) # check 
as(r,"list")

head(as(r,"data.frame"))

# normalize ---------------------------------------------------------------
r_m <- normalize(r)
getRatingMatrix(r_m)
denormalize(r_m)
image(r,main="Raw Ratings")
image(r_m,main = 'Normalize Rating')


# binary ------------------------------------------------------------------

r_b <- binarize(r,minRating=4)
as(r_b,"matrix")

image(r_b,main = "binary with minRating=4")


# datasets : jester5k -----------------------------------------------------

data(Jester5k)
Jester5k
set.seed(1234)
r <- sample(Jester5k,1000)
rowCounts(r[1,],"list")
as(r[1,],"list")
rowMeans(r[1,])

hist(getRatings(r),breaks = 100)
hist(getRatings(normalize(r)),breaks=100)
hist(getRatings(normalize(r,method="z-score")),breaks = 100)

hist(rowCounts(r),breaks = 50)
hist(colMeans(r),breaks=20)


# create recommender ------------------------------------------------------

recommenderRegistry$get_entries(dataType="realRatingMatrix")

r<-Recommender(Jester5k[1:1000],method="popular")
r
names(getModel(r))
getModel(r)$topN

recom <- predict(r,Jester5k[1001:1002],n=5)
recom
as(recom,"list")
recom3 <- bestN(recom,n=3)
as(recom3,"list")

## rating #
recom_rating <- predict(r,Jester5k[1001:1002],type="ratings")
as(recom_rating,"matrix")[,1:10]

## ratingMatrix ##
recom_rating_mx <- predict(r,Jester5k[1001:1002],type="ratingMatrix")
recom_rating_mx
as(recom_rating_mx,"matrix")[,1:10]

# evaluation predict rating -----------------------------------------------

e <- evaluationScheme(Jester5k[1:1000],method="split",train=0.9,
                      given=15,goodRating=5)
r1 <- Recommender(getData(e,"train"),"UBCF")
r2 <- Recommender(getData(e,"train"),"IBCF")

p1 <-predict(r1,getData(e,"known"),type="ratings")
p1
p2 <-predict(r2,getData(e,"known"),type="ratings")
p2
error <-rbind(
  UBCF=calcPredictionAccuracy(p1,getData(e,"unknown")),
  IBCF=calcPredictionAccuracy(p2,getData(e,"unknown"))
)

error

# evaluation top N recommender --------------------------------------------


scheme <- evaluationScheme(Jester5k[1:1000],method="cross",k=4,given=3,
                           goodRating=5)
scheme
results <- evaluate(scheme,method="POPULAR",type="topNList",
                   n=c(1,3,5,10,15,20))
results
getConfusionMatrix(results)
avg(results)

plot(results,annotate=T)
plot(results, "prec/rec", annotate=TRUE)


# compare algo ------------------------------------------------------------

set.seed(2016)
scheme <- evaluationScheme(Jester5k[1:1000],method="split",train=.9,
                           k=1,given=-5,goodRating=5)
scheme

algorithms <- list(
  "random items" = list(name="RANDOM",param=NULL),
  "popular items" = list(name="POPULAR",param=NULL),
  "user-based CF" = list(name="UBCF",param=list(nn=50)),
  "item-based CF" = list(name="IBCF",param=list(k=50)),
  "SVD Approximation" = list(name="SVD",param=list(k=50))
)

## run algo
results <- evaluate(scheme,algorithms,type="topNList",n=c(1,3,5,10,15,20))
results
names(results)

plot(results,annotate=c(1,3),legend = "bottomright")
plot(results,"prec/rec",annotate=c(1,3),legend="topleft")


