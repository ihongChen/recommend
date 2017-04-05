
# �������--��P�L�o�k-- -----------------------------------------------------------

## �M��ޥ�
library(RODBC)
library(tidyverse)
library(recommenderlab)
library(reshape2)

## �s����Ʈw
conn <- odbcDriverConnect("Driver=SQL Server;Server=dbm_public;Database=DB_WM;Uid=xxxxx;Pwd=xxxxxx;")
# ��10% ���-> 17192
sql_fund <- "select top 10 percent �����Ҧr��,[�����(�}�l)],�������W��	
            from DB_WM.dbo.v_�w�s����s�q���R
            where DB���� = 'a.�@�먭��'
	          and ��ꫬ�A = 'b.�浧����'
	          and YYYYMM = '201703'"
fund=sqlQuery(conn,sql_fund)

dim(fund) # 17192 *3 

colnames(fund)
fund1 <- 
  fund %>%
  group_by(�����Ҧr��,�������W��) %>%
  count()

## �Τ�����@�ɰ���ƶq
fund1 <- 
  fund1 %>%
  arrange(desc(n))

## 
fund2 <-fund1 %>%
  dcast(�����Ҧr��~�������W��)


rownames(fund2) <-fund2$�����Ҧr��
fund2$�����Ҧr�� <-NULL


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

