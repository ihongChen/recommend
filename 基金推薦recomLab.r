
# 基金推薦--協同過濾法-- -----------------------------------------------------------

## 套件引用
library(RODBC)
library(tidyverse)
library(recommenderlab)
library(reshape2)

## 連結資料庫
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=project2017;Uid=sa;Pwd=01060728;")

# 取資料-> 171,914
sql_fund <- "SELECT * FROM [基金推薦_庫存明細]"
fund=sqlQuery(conn,sql_fund)

dim(fund) # 171,914 *3 
save(fund,file="fund.RData")
load('fund.Rdata')

## 
fund1 <- 
  fund %>%
  group_by(身分證字號,基金中文名稱) %>%
  count() %>% 
  ungroup()

## 用戶持有一檔基金數量
fund1 <- 
  fund1 %>%
  arrange(desc(n))

# fund1 %>% distinct(基金中文名稱) # 2301 Item

# fund1 %>% 
#   distinct(身分證字號) # 55666 user 


## id歸戶-每個用戶擁有的基金庫存
fund2 <-fund1 %>%
  dcast(身分證字號~基金中文名稱)

rownames(fund2) <-fund2$身分證字號
fund2$身分證字號 <-NULL

# sum(fund2[1,],na.rm=T)
# dim(fund2) # 55,666*2,301 (user-item)


######### use recommenderLab #########################
## 參考 vignette("recommenderlab") 



# user-item  -------------------------------------------------------------------
ui_trans_m <- data.matrix(fund2)
# dim(ui_trans_m) # 55666*2301 - UI

ui_trans <- as(ui_trans_m,"realRatingMatrix")
# image(ui_trans,main = "U-I table")
# colCounts(ui_trans)[1:5]

r_b <- binarize(ui_trans,minRating=1)
r_b <- as(r_b,"binaryRatingMatrix")
r_bex <-r_b[rowCounts(r_b)>4] # 排除庫存基金數<5
# image(r_b) # 7,125 人

# image(r_b,main="User-Item binary table")
# image(r_bex,main="U-I (排除持有數<5)")

# 資料探索 --------------------------------------------------------------------
# dim(r_b) # 55,666 users * 2,301 items
## 檢查資料
# rowCounts(r_b[1,])
# rowCounts(r_b[2,])
# rowCounts(r_b[10,])
# hist(rowCounts(r_b), breaks=100)
# # 用戶持有
# sort(rowCounts(r_b),decreasing = T)[10000:15000] # 前10000名用戶,持有數至少4檔基金
# table(rowCounts(r_b)) #大部分用戶持有僅持有一檔(種)基金
# table(rowCounts(r_bex))

# 物品相似度 -------------------------------------------------------------------

# 
# # simItem_table <- similarity(r_b,method="cosine",which="items") ## 基於cosine相似度
# simItem_table <- similarity(r_b,method="jaccard",which="items") ## 基於jaccard相似度
# simItem_table_M <-as(simItem_table,"matrix")
# ## 僅涵蓋購買基金數>4, 共計有7125人
# simItem_table_ex <- similarity(r_bex,method="jaccard",which="items")
# simItem_table_exM <- as(simItem_table_ex,"matrix")
# simItem_table_ex_exclude <- ifelse(simItem_table_exM<0.01,NA,simItem_table_exM)
# 
# simItem_sparse_ex <-as(simItem_table_ex_exclude,"realRatingMatrix")
# image(simItem_sparse_ex,xlab="Item1",ylab="Item2") # 
# 
# 
# #### 考慮全部購買55,666人
# simItem_table_exclude <- ifelse(simItem_table_M<0.01,NA,simItem_table_M) #排除<0.01相似度
# # simItem_table_exclude %>% head() %>% View()
# simItem_sparse <- as(simItem_table_exclude,"realRatingMatrix")
# 
# image(simItem_sparse,xlab="Item-1",ylab="Item-2") ## 物品相似度
# 
# rowCounts(simItem_sparse[1,]) ## 23 筆資料
# rowCounts(simItem_sparse[2,]) ## 6
# 
# simItem_df <- as(simItem_sparse,"data.frame")
# simItem_df %>% head()
# simItem_df <- simItem_df %>% 
#   `colnames<-` (c('基金1','基金2','相似度'))
# 
# dim(simItem_df)
# simItem_df %>% head() %>% rownames()
# 
# vartypes = c(`基金1` = "varchar(99)",`基金2` = "varchar(99)",`相似度` = "numeric(4,3)")
# # vartypes
# ## test 
# # sqlSave(conn2,simItem_df[sample(nrow(simItem_df),5),],
# #         tablename = "test2",rownames = FALSE,varTypes=vartypes)
#   
# sqlSave(conn,simItem_df,tablename = "基金推薦_基金相似度_J",rownames = FALSE,varTypes=vartypes)


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


# 推薦模型 --------------------------------------------------------------
# recommenderRegistry$get_entries(dataType="binaryRatingMatrix")
# ####### popular ####### 
# r_popular <- Recommender(r_b[1:45000],method="POPULAR")
# names(getModel(r_popular))
# p_popular <- predict(r_popular, r_b[50000:50010], type="topNList",n=5)
# as(p_popular,"list")
# 
# ##### User based ######
# r_user <- Recommender(r_b[1:45000,],method="UBCF")
# p_user <- predict(r_user, r_b[50000:50010], type="topNList",n=5)
# l <- as(p_user,"list")
# 
# as(bestN(p_user,n=5),"list")
# 
# names(getModel(r_user))
# 
# getModel(r_user)
# 
# ###### Item based ######
# r_item <- Recommender(r_b[1:45000,],method="IBCF")
# p_item <- predict(r_item,r_b[50000:50010],type = "topNList",n=5)
# as(bestN(p_item,n=5),"list")
# names(getModel(r_item))
# 
# image(getModel(r_item)$sim)

# 模型測試區 ---------------------------------------------------------------------


####################################################################
##   算法評估
####################################################################

algorithms <- list(
  "random items" = list(name="RANDOM"),
  "popular items" = list(name="POPULAR"),
  "user-based CF" = list(name="UBCF",param=list(nn=50)),
  "item-based CF" = list(name="IBCF",param=list(k=50))
  # "SVD approx" = list(name="SVD",param=list(k=50))
)
## 排除持有數<4 === 共7,125 users 2,301 items #
# r_bex
scheme_rbex_split <- evaluationScheme(r_bex,method="split",train=0.9,k=1,given=-1) # split

scheme_rbex_cv <- evaluationScheme(r_bex,method="cross",k=4,given=-1) # cross
ev_resultEx_split <- evaluate(scheme_rbex_split,algorithms,type="topNList",
                          n=c(1,3,5,10,20))
ev_resultEx_cross <- evaluate(scheme_rbex_cv,algorithms,type="topNList",
                          n=c(1,3,5,10,20))


plot(ev_resultEx_split,annotate=c(1,3))
plot(ev_resultEx_cross,annotate=c(1,3))
avg(ev_resultEx_cross)

save(ev_resultEx_split,ev_resultEx_cross,file="ev_result.RData")
load('ev_result.RData')

## 找出最佳模型  -- check recall 
ev_dataList <- avg(ev_resultEx_cross)
ev_dataList$`popular items`[5,'recall']
ev_dataList$`user-based CF`[5,'recall']

recall_compare <- sapply(ev_dataList,`[[`,5,'recall') 
best_model <- names(which.max(recall_compare))
if (best_model=='popular items') {
  best_model <- 'popular'
} else if (best_model=='user-based CF'){
  best_model <- 'UBCF'
} else if (best_model=='item-based CF'){
  best_model <- 'IBCF'
}



# 預測結果 --------------------------------------------------------------------

## predict

# rec_popular <- Recommender(r_bex,method="popular")
# rec_ubcf <- Recommender(r_bex,method = 'UBCF')
# rec_ibcf <- Recommender(r_bex,method = "IBCF")

# pred_popular <- predict(rec_popular, r_bex[1:10], type="topNList",n=5)
# pred_ubcf <- predict(rec_ubcf,r_bex[1:10],type="topNList",n=5)
# pred_ibcf <- predict(rec_ibcf,r_bex[1:10],type="topNList",n=5)
# 
# as(pred_popular,"list")
# as(pred_ubcf,"list")
# as(pred_ibcf,"list")
# 
# rowCounts(r_bex[1,])
# rowCounts(r_bex[2,])


recommender_model <- Recommender(r_bex,method = best_model)
pred_result <- predict(recommender_model,r_bex,type="topNList",n=20)

pred_result_list <- as(pred_result,"list")

# temp <- pred_result_list %>% head(20)

df_t <- t(as.data.frame(pred_result_list,stringsAsFactors = F))

itemNames <- sapply('item',paste0,c(1:20))[,1]

df_exclude <- data.frame(df_t); colnames(df_exclude) <- itemNames;



sqlSave(conn,hot100Fund_df,
        tablename = "基金推薦_熱門100基金",
        rownames = F)

sqlSave(conn,df_exclude,
        tablename = "基金推薦_個人基金Top20",
        rownames = "uid")




# test --------------------------------------------------------------------

### 大部分UBCF推薦清單是熱門商品!!! ####
names <- rownames(df_exclude)
rownames(df_exclude) <- NULL
df <- cbind(names,df_exclude)

df %>% 
  group_by(item1) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
  

# ## 不排除 ===共55,666 user, 2,301 items === 記憶體超過4.3G(UBCF) ===
# r_b
# scheme_cross <-evaluationScheme(r_b,method="cross",k=4,given=-1)
# result_cross <- evaluate(scheme_cross,algorithms,type="topNList",
#                          n=c(1,3,5,10,20))
# plot(result_cross)



