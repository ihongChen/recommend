
# 基金推薦--協同過濾法-- -----------------------------------------------------------

## 套件引用
library(RODBC)
library(tidyverse)
library(recommenderlab)
library(reshape2)

## 連結資料庫
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=DB_WM;Uid=xxxxxx;Pwd=xxxxx;")
# 取10% 資料-> 17192
sql_fund <- "select top 1 percent 身分證字號,[交易日(開始)],基金中文名稱	
            from DB_WM.dbo.v_庫存基金存量分析
            where DB分類 = 'a.一般身分'
	          and 投資型態 = 'b.單筆申購'
	          and YYYYMM = '201703'"
fund=sqlQuery(conn,sql_fund)

dim(fund) # 1720 *3 

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

fund1 %>% distinct(基金中文名稱) # 456 Item

fund1 %>% 
  distinct(身分證字號) # 515 user 


## id歸戶-每個用戶擁有的基金庫存
fund2 <-fund1 %>%
  dcast(身分證字號~基金中文名稱)

rownames(fund2) <-fund2$身分證字號
fund2$身分證字號 <-NULL

sum(fund2[1,],na.rm=T)
dim(fund2) # 526*466
### use recommenderLab 
## 參考 vignette("recommenderlab")

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
hist(rowCounts(r_b), breaks=10)

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

l

