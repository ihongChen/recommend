
# 基金推薦--協同過濾法-- -----------------------------------------------------------

## 套件引用
library(RODBC)
library(tidyverse)
library(recommenderlab)
library(reshape2)

## 連結資料庫
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=project2017;Uid=sa;Pwd=01060728;")
conn2 <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=test;Uid=sa;Pwd=01060728;")
# 取資料-> 171,914
sql_fund <- "SELECT * FROM [基金推薦_庫存明細]"
fund=sqlQuery(conn,sql_fund)

dim(fund) # 171,914 *3 
save(fund,file="fund.RData")
colnames(fund)
fund1 <- 
  fund %>%
  group_by(身分證字號,基金中文名稱) %>%
  count() %>% 
  ungroup()

## 用戶持有一檔基金數量
fund1 <- 
  fund1 %>%
  arrange(desc(n))

fund1 %>% distinct(基金中文名稱) # 1377 Item

fund1 %>% 
  distinct(身分證字號) # 5481 user 


## id歸戶-每個用戶擁有的基金庫存
fund2 <-fund1 %>%
  dcast(身分證字號~基金中文名稱)

rownames(fund2) <-fund2$身分證字號
fund2$身分證字號 <-NULL

sum(fund2[1,],na.rm=T)
dim(fund2) # 55,666*2,301 (user-item)


######### use recommenderLab #########################
## 參考 vignette("recommenderlab") 


# 物品相似度 -------------------------------------------------------------------
ui_trans_m <- data.matrix(fund2)
dim(ui_trans_m) # 55666*2301 - UI

ui_trans <- as(ui_trans_m,"realRatingMatrix")
image(ui_trans,main = "U-I table")
colCounts(ui_trans)[1:5]

r_b <- binarize(ui_trans,minRating=1)
r_b <- as(r_b,"binaryRatingMatrix")

image(r_b,main="User-Item binary table")

simItem_table <- similarity(r_b,method="cosine",which="items") ## 基於cosine相似度
simItem_table <- similarity(r_b,method="jaccard",which="items") ## 基於jaccard相似度
simItem_table_M <-as(simItem_table,"matrix")

simItem_table_exclude <- ifelse(simItem_table_M<0.01,NA,simItem_table_M) #排除<0.01相似度
# simItem_table_exclude %>% head() %>% View()
simItem_sparse <- as(simItem_table_exclude,"realRatingMatrix")

image(simItem_sparse,xlab="Item-1",ylab="Item-2") ## 物品相似度

rowCounts(simItem_sparse[1,]) ## 23 筆資料
rowCounts(simItem_sparse[2,]) ## 6

simItem_df <- as(simItem_sparse,"data.frame")
simItem_df %>% head()
simItem_df <- simItem_df %>% 
  `colnames<-` (c('基金1','基金2','相似度'))

dim(simItem_df)
simItem_df %>% head() %>% rownames()

vartypes = c(`基金1` = "varchar(99)",`基金2` = "varchar(99)",`相似度` = "numeric(4,3)")
# vartypes
## test 
# sqlSave(conn2,simItem_df[sample(nrow(simItem_df),5),],
#         tablename = "test2",rownames = FALSE,varTypes=vartypes)
  
sqlSave(conn,simItem_df,tablename = "基金推薦_基金相似度_J",rownames = FALSE,varTypes=vartypes)


# library(data.table)

# simItem_df %>% 
#   filter(`基金1` %like% '103' ) ## %like% : from data.table



# 熱門基金 --------------------------------------------------------------------

hot100Fund <- 
colCounts(r_b) %>% sort(decreasing= TRUE) %>% head(100)

hot100Fund_df <- tibble(`基金`=names(hot100Fund),`持有人數`=hot100Fund)

names(hot100Fund)
## write data to csv
write.table(hot100Fund_df, file = "hot100Fund.csv", sep = ",")
hot100Fund

image(simItem_sparse[names(hot100Fund),names(hot100Fund)],
      xlab='Item1',ylab='Item2', main='熱門基金相似度')

temp <- simItem_sparse[names(hot100Fund),names(hot100Fund)]
# as(simItem_sparse[names(hot100Fund),names(hot100Fund)],"matrix") %>% View()

image(simItem_sparse[1:100,1:100])


# 資料探索 --------------------------------------------------------------------
dim(r_b) # 55666 users * 2301 items
## 檢查資料
rowCounts(r_b[1,])
rowCounts(r_b[2,])
rowCounts(r_b[10,])
hist(rowCounts(r_b), breaks=100)
# 用戶持有
sort(rowCounts(r_b),decreasing = T)[10000:15000] # 前10000名用戶,持有數至少4檔基金
table(rowCounts(r_b)) #大部分用戶持有僅持有一檔(種)基金


# 推薦模型 --------------------------------------------------------------
recommenderRegistry$get_entries(dataType="binaryRatingMatrix")
####### popular ####### 
r_popular <- Recommender(r_b[1:45000],method="POPULAR")
names(getModel(r_popular))
p_popular <- predict(r_popular, r_b[50000:50010], type="topNList",n=5)
as(p_popular,"list")

##### User based ######
r_user <- Recommender(r_b[1:45000,],method="UBCF")
p_user <- predict(r_user, r_b[50000:50010], type="topNList",n=5)
l <- as(p_user,"list")

as(bestN(p_user,n=5),"list")

names(getModel(r_user))

getModel(r_user)

###### Item based ######
r_item <- Recommender(r_b[1:45000,],method="IBCF")
p_item <- predict(r_item,r_b[50000:50010],type = "topNList",n=5)
as(bestN(p_item,n=5),"list")
names(getModel(r_item))

image(getModel(r_item)$sim)

# 模型測試區 ---------------------------------------------------------------------

scheme<-evaluationScheme(r_b[1:50000],method="cross",k=4,given=-1)
results_popular <- evaluate(scheme,method="POPULAR",type="topNList",
                            n=c(1,3,5,10))

results_
##############################
m <- data.matrix(fund2)
r <- as(m,"realRatingMatrix")
as(r,"list")
image(r,main="USER ITEM TABLE")
image(normalize(r),main = "UI table(norm)")

# table(m) # 88.3% : 12877 /14586
t1 <- table(m) # 大部分用戶只用有一隻庫存基金...
fundStoreTable <- as.data.frame(t1)
colnames(fundStoreTable) <- c('庫存基金數','人數')
fundStoreTable

# tibble(t1)

# head(fund1)

r_b <- binarize(r,minRating=1)
as(r_b,"matrix")

image(r_b,main = "binary with minTrans=1")
class(r_b)

## 檢查資料
rowCounts(r_b[1,])
rowCounts(r_b[2,])
rowCounts(r_b[10,])
hist(rowCounts(r_b), breaks=100)

## model ##

r1 <- Recommender(r_b[1:500,],method="POPULAR")
names(getModel(r1))
predictions <- predict(r1, r_b[501:510], type="topNList",n=5)
as(predictions,"list")


r2 <- Recommender(r_b[1:500,],method="IBCF")
p2 <- predict(r2, r_b[501:510], type="topNList",n=5)
l <- as(p2,"list")

as(bestN(p2,n=5),"list")

names(getModel(r1))
names(getModel(r2))




