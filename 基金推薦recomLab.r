
# 基金推薦--協同過濾法-- -----------------------------------------------------------

## 套件引用
library(RODBC)
library(tidyverse)
library(recommenderlab)
library(reshape2)

## 連結資料庫
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=DB_WM;Uid=xxxxx;Pwd=xxxxxx;")
# 取10% 資料-> 17192
sql_fund <- "select top 10 percent 身分證字號,[交易日(開始)],基金中文名稱	
            from DB_WM.dbo.v_庫存基金存量分析
            where DB分類 = 'a.一般身分'
	          and 投資型態 = 'b.單筆申購'
	          and YYYYMM = '201703'"
fund=sqlQuery(conn,sql_fund)

dim(fund) # 17192 *3 

colnames(fund)
fund1 <- 
  fund %>%
  group_by(身分證字號,基金中文名稱) %>%
  count()

## 用戶持有一檔基金數量
fund1 <- 
  fund1 %>%
  arrange(desc(n))

## 
fund2 <-fund1 %>%
  dcast(身分證字號~基金中文名稱)


rownames(fund2) <-fund2$身分證字號
fund2$身分證字號 <-NULL


### use recommenderLab 
m <- data.matrix(fund2)
r <- as(m,"realRatingMatrix")
as(r,"list")
image(r,main="USER ITEM TABLE")
image(normalize(r),main = "UI table(norm)")

table(m) # 88.3% : 12877 /14586

r_b <- binarize(r,minRating=4)
as(r_b,"matrix")

image(r_b,main = "binary with minTrans=4")
r_b

