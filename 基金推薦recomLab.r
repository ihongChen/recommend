
# �������--��P�L�o�k-- -----------------------------------------------------------

## �M��ޥ�
library(RODBC)
library(tidyverse)
library(recommenderlab)
library(reshape2)

## �s����Ʈw
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=DB_WM;Uid=xxxxxx;Pwd=xxxxx;")
# ��10% ���-> 17192
sql_fund <- "select top 1 percent �����Ҧr��,[�����(�}�l)],�������W��	
            from DB_WM.dbo.v_�w�s����s�q���R
            where DB���� = 'a.�@�먭��'
	          and ��ꫬ�A = 'b.�浧����'
	          and YYYYMM = '201703'"
fund=sqlQuery(conn,sql_fund)

dim(fund) # 1720 *3 

colnames(fund)
fund1 <- 
  fund %>%
  group_by(�����Ҧr��,�������W��) %>%
  count() %>% 
  ungroup()

## �Τ�����@�ɰ���ƶq
fund1 <- 
  fund1 %>%
  arrange(desc(n))

fund1 %>% distinct(�������W��) # 456 Item

fund1 %>% 
  distinct(�����Ҧr��) # 515 user 


## id�k��-�C�ӥΤ�֦�������w�s
fund2 <-fund1 %>%
  dcast(�����Ҧr��~�������W��)

rownames(fund2) <-fund2$�����Ҧr��
fund2$�����Ҧr�� <-NULL

sum(fund2[1,],na.rm=T)
dim(fund2) # 526*466
### use recommenderLab 
## �Ѧ� vignette("recommenderlab")

m <- data.matrix(fund2)
r <- as(m,"realRatingMatrix")
as(r,"list")
image(r,main="USER ITEM TABLE")
image(normalize(r),main = "UI table(norm)")

# table(m) # 88.3% : 12877 /14586
t1 <- table(m) # �j�����Τ�u�Φ��@���w�s���...
fundStoreTable <- as.data.frame(t1)
colnames(fundStoreTable) <- c('�w�s�����','�H��')
fundStoreTable

# tibble(t1)

# head(fund1)

r_b <- binarize(r,minRating=1)
as(r_b,"matrix")

image(r_b,main = "binary with minTrans=1")
class(r_b)

## �ˬd���
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

