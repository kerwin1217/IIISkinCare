library(MASS)
library(RSNNS)
data = read.csv("11_JOIN.csv",header=FALSE,col.names=c("reviewid","proid","age","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","AA","AB","AC","AD","score","skin","spring","summer","fall","winter","proname"))
data = subset(data,subset=(age<90 & age>10 & skin!='na' & skin!=''))

ind = sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))   #分兩組:實驗及對照組
trainset=data[ind==1,]                      
#trainset
testset=data[ind==2,]
#testset
#Pima=rbind(trainset,testset)

#book 

testset = subset(testset,select=c(A:AD))

testset$A = as.factor(testset$A)
testset$B = as.factor(testset$B)
testset$C = as.factor(testset$C)
testset$D = as.factor(testset$D)
testset$E = as.factor(testset$E)
testset$F = as.factor(testset$F)
testset$G = as.factor(testset$G)
testset$H = as.factor(testset$H)
testset$I = as.factor(testset$I)
testset$J = as.factor(testset$J)
testset$K = as.factor(testset$K)
testset$L = as.factor(testset$L)
testset$M = as.factor(testset$M)
testset$N = as.factor(testset$N)
testset$O = as.factor(testset$O)
testset$P = as.factor(testset$P)
testset$Q = as.factor(testset$Q)
testset$R = as.factor(testset$R)
testset$S = as.factor(testset$S)
testset$T = as.factor(testset$T)
testset$U = as.factor(testset$U)
testset$V = as.factor(testset$V)
testset$W = as.factor(testset$W)
testset$X = as.factor(testset$X)
testset$Y = as.factor(testset$Y)
testset$Z = as.factor(testset$Z)
testset$AA = as.factor(testset$AA)
testset$AB = as.factor(testset$AB)
testset$AC = as.factor(testset$AC)
testset$AD = as.factor(testset$AD)

trainset = subset(trainset,select=c(A:AD))

trainset$A = as.factor(trainset$A)
trainset$B = as.factor(trainset$B)
trainset$C = as.factor(trainset$C)
trainset$D = as.factor(trainset$D)
trainset$E = as.factor(trainset$E)
trainset$F = as.factor(trainset$F)
trainset$G = as.factor(trainset$G)
trainset$H = as.factor(trainset$H)
trainset$I = as.factor(trainset$I)
trainset$J = as.factor(trainset$J)
trainset$K = as.factor(trainset$K)
trainset$L = as.factor(trainset$L)
trainset$M = as.factor(trainset$M)
trainset$N = as.factor(trainset$N)
trainset$O = as.factor(trainset$O)
trainset$P = as.factor(trainset$P)
trainset$Q = as.factor(trainset$Q)
trainset$R = as.factor(trainset$R)
trainset$S = as.factor(trainset$S)
trainset$T = as.factor(trainset$T)
trainset$U = as.factor(trainset$U)
trainset$V = as.factor(trainset$V)
trainset$W = as.factor(trainset$W)
trainset$X = as.factor(trainset$X)
trainset$Y = as.factor(trainset$Y)
trainset$Z = as.factor(trainset$Z)
trainset$AA = as.factor(trainset$AA)
trainset$AB = as.factor(trainset$AB)
trainset$AC = as.factor(trainset$AC)
trainset$AD = as.factor(trainset$AD)

library(bnlearn)
?bnlearn
bn=naive.bayes(testset,"C");plot(bn);bn
fitted=bn.fit(bn,testset)
pred=predict(fitted,trainset)
tab=table(pred,trainset[,"C"]);tab

acc=sum(diag(tab))/sum(tab);acc

#----------------------------------------------

tan=tree.bayes(testset,"C");plot(tan);tan
fitted=bn.fit(bn,testset,method = "bayes")
pred=predict(fitted,trainset)
tab=table(pred,trainset[,"C"]);tab
acc=sum(diag(tab))/sum(tab);acc
