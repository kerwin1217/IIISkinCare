#參考DatamMing課本165
install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(MASS)
library(rpart)
library(CHAID)
data = read.csv("4_join.csv",header=FALSE,col.names=c("reviewid","proid","age","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","AA","AB","AC","AD","score","skin","spring","summer","fall","winter","proname"))
data = subset(data,subset=(age<90 & age>10 & skin!='na' & skin!=''))

C_degree = nrow(data)   	#建立空的欄位
C = data$C             	  #抓取age資料
C_degree[C==0] = 'NO'     #會回購  no=0、yes=1 
C_degree[C==1] = 'YES'
skin_degree=nrow(data)
skin=data$skin
skin_degree[skin=='普通性肌膚'] = '1' 
skin_degree[skin=='乾性肌膚'] = '2'
skin_degree[skin=='油性肌膚'] = '3'
skin_degree[skin=='混合性肌膚'] = '4'
skin_degree[skin=='敏感性肌膚'] = '5'
skin_degree[skin=='先天過敏性肌膚'] = '6'
data = cbind(data,C_degree)             #建立(egree資料)在原本data後面
data = cbind(data,skin_degree) 

ind = sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))   #分兩組:實驗及對照組
trainset=data[ind==1,]                      
testset=data[ind==2,]

testset = subset(trainset,select=c(A,B,D:H,C_degree,skin_degree))
testset$skin_degree = as.factor(testset$skin_degree)
testset = subset(testset, subset=(skin_degree== 3)) 
#testset$C_degree = as.factor(testset$C_degree)
summary(testset)
set.seed(1111)

#---------------------決策樹tree---------------------------------
cart=rpart(C_degree~.,testset,control = rpart.control(cp=0))
summary(cart)
par(xpd=TRUE);plot(cart);text(cart)


#---------------------------------------------------------------
#C50 (TREE)

C50_tree=C5.0(C_degree~.,testset,control = C5.0Control(noGlobalPruning = F))
summary(C50_tree)
par(xpd=TRUE);plot(C50_tree)

---------------------計算切點<無效>------------------------------
  min(cart$cptable[,"xerror"])
which.min(cart$cptable[,"xerror"])
cart.cp = cart$cptable[33,"CP"]
prune.tree = prune(cart, cp= cart.cp)
par(xpd=TRUE);plot(prune.tree);text(cart)