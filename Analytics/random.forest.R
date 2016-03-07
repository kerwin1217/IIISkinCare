#安裝函式庫
#install.packages("RODBC")
#install.packages("RMySQL")
#install.packages("dbConnect")

#引用函式庫
library(RMySQL)
library(RODBC)
library(dbConnect)

#連線 host=主機IP,daname=資料庫名稱,username=使用者帳號,password=使用者密碼
con <- dbConnect(MySQL(),host="10.120.26.12", dbname = "urcosme", username="root", password="iii" )

#設定連線時資料庫Query為big5
dbSendQuery(con,"SET NAMES big5")

#篩選資料條件:心得數>25, score >0, skin不等於na, 非停產, 價格>10)
data = dbGetQuery(con, "SELECT * FROM prodinfo p INNER JOIN finaleff f ON p.proid = f.proid where count>25 and skin!='na' and score>0 and DATE NOT LIKE '%停產%' and price>10;")

#算各類產品資料數
review.counts = matrix(0, nrow=15 , ncol=1 )
rownames(review.counts)=c('1臉部卸妝','2眼唇卸妝','3洗顏','4化妝水','5凝膠凍','6乳液','7乳霜','8防曬','9前導保養','10精華','11保養面膜','12清潔面膜','13臉部去角質','14眼部保養','15唇部保養')
for(i in c(1:15)){
  review.counts[i,1]=nrow(subset(data,subset=(protype==i)))
}
review.counts = as.data.frame(review.counts)


#將年齡分成2個levels Y:age<=24 O:age>=25
age.deg = nrow(data)              #建立空陣列age_degree
age = data$age
age.deg[age<=24] = 'Y'            #age<=24 為young類別 若x=c(1,2) y=c(0,0) x>1的結果為[false,true] y[x>1]='yes'表示在y在x>1為true的位置塞yes,結果為0,yes
age.deg[age>=25] = 'O'

#將膚質分成五類  A:油性肌膚 B:乾性肌膚 C:混合性肌膚 D:普通性肌膚 E:others(先天過敏性肌膚+敏感性肌膚)
skintype = nrow(data)     
skin = data$skin
skintype[skin=='油性肌膚'] = 'A'
skintype[skin=='乾性肌膚'] = 'B'
skintype[skin=='混合性肌膚'] = 'C'
skintype[skin=='普通性肌膚'] = 'D'
skintype[skin=='先天過敏性肌膚'|skin=='敏感性肌膚'] = 'E'

data = cbind(data,age.deg,skintype)

#以random forest做變數選取 (59種使用效果, 哪幾種效果對評分最有影響?)

#install.packages("randomForest")
library(randomForest)

effset = subset(data,select=c(proid,protype,price,skintype,age.deg,score,A:AD,AE:BG))

rebuy = nrow(data)              
rebuy[data$score<=4] = 0              #將分數<=4歸類為評價低
rebuy[data$score>=5] = 1              #將分數>=5歸類為評價高

effsum = rowSums(effset[,c(7:65)])    #將每一筆評論的效果做加總(加總為0代表沒選任何效果)
effset = cbind(effset,rebuy,effsum)   #將effsum塞在effset data frame後
effset = subset(effset,subset=!(C==1 & score<5 ) & (effsum>0))  #篩掉評分<5但選擇會回購 及所有效果都沒勾選的

product = subset(effset,subset=((protype==4) & skintype=='C'))   #篩選自己要的條件

# 將效果及rebuy的資料形態轉成factor(原本是int)
for(i in c(7:66)){
  product[,i] = as.factor(product[,i])
}

# 將資料隨機分成訓練集與測試集
ind = sample(2, nrow(product), replace = TRUE, prob=c(0.8, 0.2))
product.tr = product[ind == 1,]     #training set 訓練集
product.te = product[ind == 2,]     #testing  set 測試集

#tune參數(mtry和ntree,ntree從100 try到1000)
mtry100 = as.data.frame(tuneRF(product.tr[,7:65], product.tr[,'rebuy'], mtryStart = 1, stepFactor = 2, ntreeTry = 100, improve = 0.001, plot = FALSE));
n = rep(100,times=nrow(mtry100))      #將n設為元素都是100的vector
mtry100 = cbind(mtry100,n)            #在結果後加一行100代表這些結果是屬於ntree=100

mtry200 = as.data.frame(tuneRF(product.tr[,7:65], product.tr[,'rebuy'], mtryStart = 1, stepFactor = 2, ntreeTry = 200, improve = 0.001, plot = FALSE));
n = rep(200,times=nrow(mtry200))
mtry200 = cbind(mtry200,n)

mtry300 = as.data.frame(tuneRF(product.tr[,7:65], product.tr[,'rebuy'], mtryStart = 1, stepFactor = 2, ntreeTry = 300, improve = 0.001, plot = FALSE));
n = rep(300,times=nrow(mtry300))
mtry300 = cbind(mtry300,n)

mtry400 = as.data.frame(tuneRF(product.tr[,7:65], product.tr[,'rebuy'], mtryStart = 1, stepFactor = 2, ntreeTry = 400, improve = 0.001, plot = FALSE));
n = rep(400,times=nrow(mtry400))
mtry400 = cbind(mtry400,n)

mtry500 = as.data.frame(tuneRF(product.tr[,7:65], product.tr[,'rebuy'], mtryStart = 1, stepFactor = 2, ntreeTry = 500, improve = 0.001, plot = FALSE));
n = rep(500,times=nrow(mtry500))
mtry500 = cbind(mtry500,n)

mtry600 = as.data.frame(tuneRF(product.tr[,7:65], product.tr[,'rebuy'], mtryStart = 1, stepFactor = 2, ntreeTry = 600, improve = 0.001, plot = FALSE));
n = rep(600,times=nrow(mtry600))
mtry600 = cbind(mtry600,n)

mtry700 = as.data.frame(tuneRF(product.tr[,7:65], product.tr[,'rebuy'], mtryStart = 1, stepFactor = 2, ntreeTry = 700, improve = 0.001, plot = FALSE));
n = rep(700,times=nrow(mtry700))
mtry700 = cbind(mtry700,n)

mtry800 = as.data.frame(tuneRF(product.tr[,7:65], product.tr[,'rebuy'], mtryStart = 1, stepFactor = 2, ntreeTry = 800, improve = 0.001, plot = FALSE));
n = rep(800,times=nrow(mtry800))
mtry800 = cbind(mtry800,n)

mtry900 = as.data.frame(tuneRF(product.tr[,7:65], product.tr[,'rebuy'], mtryStart = 1, stepFactor = 2, ntreeTry = 900, improve = 0.001, plot = FALSE));
n = rep(900,times=nrow(mtry900))
mtry900 = cbind(mtry900,n)

mtry1000 = as.data.frame(tuneRF(product.tr[,7:65], product.tr[,'rebuy'], mtryStart = 1, stepFactor = 2, ntreeTry = 1000, improve = 0.001, plot = FALSE));
n = rep(1000,times=nrow(mtry1000))
mtry1000 = cbind(mtry1000,n)

mtry = rbind(mtry100,mtry200,mtry300,mtry400,mtry500,mtry600,mtry700,mtry800,mtry900,mtry1000)


best.mtry = mtry[mtry[, 2] == min(mtry[, 2]), 1]           #選使錯誤率最小的mtry
if (length(best.mtry) >1){best.mtry = tail(best.mtry,1)}
best.ntree = mtry[mtry[, 2] == min(mtry[, 2]), 3]          #選使錯誤率最小的mtry
if (length(best.ntree)>1){best.ntree =tail(best.ntree,1)}

xaxis = max(mtry$mtry)

plot(mtry500$mtry,mtry500$OOBError,type ='o',xlab="mtry", ylab="OOBError",col=5,lty =5,xaxt="n")
axis(1,c(1:xaxis))

lines(mtry100$mtry,mtry100$OOBError,type ='o',col=1,lty =1)
lines(mtry200$mtry,mtry200$OOBError,type ='o',col=2,lty =2)
lines(mtry300$mtry,mtry300$OOBError,type ='o',col=3,lty =3)
lines(mtry400$mtry,mtry400$OOBError,type ='o',col=4,lty =4)
lines(mtry600$mtry,mtry600$OOBError,type ='o',col=6,lty =6)
lines(mtry700$mtry,mtry700$OOBError,type ='o',col=7,lty =7)
lines(mtry800$mtry,mtry800$OOBError,type ='o',col=8,lty =8)
lines(mtry900$mtry,mtry900$OOBError,type ='o',col=9,lty =9)
lines(mtry1000$mtry,mtry1000$OOBError,type ='o',col=10,lty =10)
legend("topright", c('ntree=100','ntree=200','ntree=300','ntree=400','ntree=500','ntree=600','ntree=700','ntree=800','ntree=900','ntree=1000'), ncol = 1, cex = 0.8, col=c(1,2,3,4,5,6,7,8,9,10), lty = c(1,2,3,4,5,6,7,8,9,10))


#rfcv:做變數選取, ex:共59個效果來預測評分水準,看選幾個變數就能對評分水準有影響
varn.product.tr = rfcv(product.tr[,c(7:65)],product.tr[,'rebuy'],cv.fold=10,step=0.70)
varn.product.tr$error.cv
with(varn.product.tr, plot(n.var, error.cv, type="o", lwd=2))  #with(data,function) can use variable name directly


#將最加參數代入用training set做random forest
best.mtry=8
best.ntree=500
rf.tr = randomForest(product.tr[,c(7:65)],product.tr[,'rebuy'], ntree = best.ntree, mtry = best.mtry,importance=TRUE)
table.rf.tr = rf.tr$confusion[,1:2]   
sum(diag(table.rf.tr))/sum(table.rf.tr)   #計算預測正確率
rf.tr$importance
varImpPlot(rf.tr,cex=0.7)   #最有影響的效果,越上方對評分越有影響
print(rf.tr)

#用training出來的model套進testing set看結果
pre.test = predict(rf.tr, product.te)
table.test = table(product.te$rebuy,pre.test)
sum(diag(table.test))/sum(table.test)    #testing set預測正確率
rf.te = randomForest(product.te[,c(7:65)],product.te[,'rebuy'], ntree = 500, mtry = 8)
te = as.data.frame((1-rf.te$err.rate[,1])*100)
colnames(te)='rate'
rowcol=c(1:500)
test = cbind(rowcol,te)

plot((1-rf.tr$err.rate[,1])*100,type='l',xlab='no. of trees',ylab='%correct',ylim = c(73,82),col=4)
lines(test$rowcol,test$rate,type='l',col=2)
legend("bottomright", c('Training set','Testing set'), ncol = 1, col=c(4,2), lty = c(1,1))

#計算各個效果對於評分的重要性(MeanDecreaseGini 越大越該效果越會影響評分)
varn = 20 
imp.tr = as.data.frame(round(importance(rf.tr),2))
imp.tr = imp.tr[with(imp.tr, order(imp.tr$MeanDecreaseGini,decreasing = TRUE)), ]
weight = head(imp.tr$MeanDecreaseGini,varn)/sum(head(imp.tr$MeanDecreaseGini,varn))+1  #利用前varn個重要效果的MeanDecreaseGini算權重
top.w = cbind(head(imp.tr,varn),weight)[5]
topeffect = head(row.names(imp.tr),varn)

#write.table(top.w,"C:/Users/BigData/R/project/result/weight/5E_imp.txt",sep=',')

tmp = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,-1,-1)  #代表效果A~BG的正負向,1為正向效果, -1為負向效果
tmp = as.matrix(tmp)              
rownames(tmp) = c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','AA','AB','AC','AD','AE','AF','AG','AH','AI','AJ','AK','AL','AM','AN','AO','AP','AQ','AR','AS','AT','AU','AV','AW','AX','AY','AZ','BA','BB','BC','BD','BE','BF','BG')
for(i in c(1:nrow(top.w))){
  tmp[row.names(top.w)[i],1]= tmp[row.names(top.w)[i],1]*top.w[row.names(top.w)[i],1]         #計算A~BG效果的評分權重 前20重要的效果給予較重的權重
}
product[,7:65] = data.matrix(product[,7:65])-1                          #將效果的value(0或1)由factor轉換成數值形態

w = nrow(product)
for(j in c(1:nrow(product))){
  w[j] = as.matrix(product[j,7:65])%*%tmp                          #依各效果重要性計算每筆評論分數
}
w = w/product$effsum                                               
score.new = product$score+w                                        
product = cbind(product,w,score.new)

selCondition = subset(product,subset=(age.deg=='O' & price<=500))  #篩哪個年齡層及什麼價位的資料
prodCount = as.data.frame(table(selCondition$proid))
colnames(prodCount) = c('proid','counts') 
selCondition = merge(selCondition, prodCount, all.x = TRUE)
sort(table(selCondition$proid),decreasing = TRUE)

selCondition = subset(selCondition,subset=(counts>=30))             #篩選評論數, 自行調整

#write.table(selCondition,"C:/Users/BigData/R/project/result/finaldata/5EO_result.txt",sep=',')

selCondition$proid=as.factor(selCondition$proid)                          #將商品的資料型態由int轉為factor
library("psych")
des.mat = describeBy(selCondition$score.new,selCondition$proid,mat=TRUE)  #計算各商品的平均分數
top.product = des.mat[with(des.mat, order(-des.mat$mean)), ]  
recom.pro = head(top.product[,2],3)                                       #output平均分數前3高的商品
recom.pro


