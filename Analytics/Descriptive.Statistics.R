# Descriptive Statistics

library(RMySQL)
library(RODBC)
library(dbConnect)

con <- dbConnect(MySQL(),host="10.120.26.12", dbname = "urcosme", username="root", password="iii" )
dbSendQuery(con,"SET NAMES big5")
data = dbGetQuery(con, "SELECT * FROM prodinfo p INNER JOIN finaleff f ON p.proid = f.proid where count>25 and skin!='na' and score>0 and DATE NOT LIKE '%停產%' and price>10;")
#load("C:/Users/BigData/R/project/Rdata/data.Rda")   #或從Rda檔匯入data (事前從MySQL中篩選出來另存的data frame)
data_scoregt5 = subset(data,subset=(score>=5))
brand = as.data.frame(sort(table(data_scoregt5$brand),decreasing = TRUE))
top5_brand = subset(data_scoregt5,subset=(brand=='DHC'|brand=='kuanyuanlian 廣源良'|brand=='KIEHL`S 契爾氏'|brand=='Biore 蜜妮'|brand=='L`OREAL PARiS 巴黎萊雅'|brand=='SHISEIDO 資生堂-專櫃'))
price_deg = nrow(top5_brand)            
price_deg[top5_brand$price<500]='low'
price_deg[top5_brand$price>=500 & top5_brand$price<1000]='median'
price_deg[top5_brand$price>=1000]='high'
top5_brand = cbind(top5_brand,price_deg)
table(top5_brand$brand,top5_brand$price_deg)
brand_table = as.data.frame(table(top5_brand$proid))

#output DHC,廣源良,KIEHL'S各自最多討論度的產品類型
#1:臉部卸妝 2:眼唇卸妝 3:洗顏 4:化妝水 5:凝膠凍 6:乳液 7:乳霜 8:防曬 9:前導保養 10:精華液 11:保養面膜 12:清潔面膜 13:去角質 14:眼部保養 15:唇部保養
pDHC = subset(top5_brand,subset=(brand=='DHC'))
sort(table(pDHC$protype),decreasing = TRUE)
kuanyuanlian = subset(top5_brand,subset=(brand=='kuanyuanlian 廣源良'))
sort(table(kuanyuanlian$protype),decreasing = TRUE)

KIEHL = subset(top5_brand,subset=(brand=='KIEHL`S 契爾氏'))
sort(table(KIEHL$protype),decreasing = TRUE)


#網頁直方圖用(各產品類型在各年齡層的討論度)
age_deg=nrow(data)
age_deg[data$age<=19]='19以下'
age_deg[data$age>=20 & data$age<=29]='20代'
age_deg[data$age>=30]='30以上'
data=cbind(data,age_deg)
data['n']=1
rowcount = as.data.frame(with(data,aggregate(n, by = list(protype,age_deg),FUN=sum)))  #算各年齡層各產品類型的評論數
colnames(rowcount) = c('產品類形','年齡','評論數')

#敘述統計
load("C:/Users/BigData/R/project/Rdata/data_all.Rda")  #已事先從MySQL中撈出來的資料
summary(data_all$price)
summary(data_all$age)
boxplot(subset(data_all,subset=(price<=2500))$price,col='cadetblue1',ylab='price',main="boxplot of price")
#boxplot(data_all$age,col='palegoldenrod',ylab='age',main="boxplot of age")

age_deg=nrow(data_all)
age_deg[data_all$age<=19]='19以下'
age_deg[data_all$age>=20 & data_all$age<=29]='20~29'
age_deg[data_all$age>=30 & data_all$age<=39]='30~39'
age_deg[data_all$age>=40]='40以上'
data2=cbind(data_all,age_deg)
summary(data2$age_deg)
hist(subset(data_all,subset=(age<=90))$age,xlab='age',main="Histogram of age",col='palegoldenrod',xlim=range(0:60))


table_protype = as.data.frame(table(data_all$protype))
prod_type = c('臉部卸妝','眼唇卸妝','洗顏','化妝水','凝膠凍','乳液','乳霜','防曬','前導保養','精華','保養面膜','清潔面膜','臉部去角質','眼部保養','唇部保養')
table_protype=cbind(table_protype,prod_type)
pie(table_protype$Freq,prod_type)
