#1
#Import data
#install.packages("data.table")
library(data.table)
ac_bundles_dt<-fread("piccollage_accounts_bundles.csv")
ac_bundles_matrix<-as.matrix(ac_bundles_dt[,-1,with=FALSE])
#1-b-i
#create a matrix for top 5 recommendations for all bundles
recom<-matrix(0,nrow=5,ncol =length(ac_bundles_matrix[1,]) ,dimnames = list(c("rec1","rec2","rec3","rec4","rec5"),colnames(ac_bundles_matrix)))
recom[,1:5]
#Write a function to calculate cosine similarity
cossim<-function(matrix){
  return<-matrix(0,nrow=6,ncol =length(ac_bundles_matrix[1,]) ,dimnames = list(c("rec1","rec2","rec3","rec4","rec5","rec6"),colnames(ac_bundles_matrix)))
  for (i in 1:length(ac_bundles_matrix[1,])){
    a<-matrix[,i]
    cos<-c()#create cosine similarity matrix
    for(j in 1:length(ac_bundles_matrix[1,])){
      b<-matrix[,j]
      cos<-c(cos,sum(a*b)/(sqrt(sum(a^2))*sqrt(sum(b^2))))#compute cosine similarity 
    }
    #print(order(cos,decreasing=TRUE)[1:5])#第一個是自己
    #print(i)
    #print(cos[order(cos,decreasing=TRUE)[1:5]])
    #print(colnames(ac_bundles_matrix)[order(cos,decreasing=TRUE)[1:6]])
    return[,i]=colnames(ac_bundles_matrix)[order(cos,decreasing=TRUE)[1:6]]#put the top 1-6 bundles into recom matirx(top1 is itself)
  }
  return[]#return recommendation matrix
}
top6<-cossim(ac_bundles_matrix)
top6[,'xmassketches']
recom<-top6[1:5,]#"xmassketches" itself doesn't appear in top 6 
recom[,"xmassketches"]
#1-b-ii
#create a matrix for top 5 recommendations for all bundles
recom<-matrix(0,nrow=5,ncol =length(ac_bundles_matrix[1,]) ,dimnames = list(c("rec1","rec2","rec3","rec4","rec5"),colnames(ac_bundles_matrix)))
#Some adujstment on correlation
col_means <- apply(ac_bundles_matrix, 2, mean)
col_means_matrix <- t(replicate(nrow(ac_bundles_matrix),col_means))
ac_bundles_matrix_cor <- ac_bundles_matrix - col_means_matrix
top6 <- cossim(ac_bundles_matrix_cor)
top6[,'xmassketches']
recom<-top6[1:5,]#"xmassketches" itself doesn't appear in top 6 
recom[,"xmassketches"]

#1-b-iii
#create a matrix for top 5 recommendations for all bundles
recom<-matrix(0,nrow=5,ncol =length(ac_bundles_matrix[1,]) ,dimnames = list(c("rec1","rec2","rec3","rec4","rec5"),colnames(ac_bundles_matrix)))
#Some adujstment for adjusted-cosine similarity
account_means <- apply(ac_bundles_matrix, 1, mean)
account_means_matrix <- replicate(ncol(ac_bundles_matrix),account_means)
ac_bundles_matrix_adjcos <- ac_bundles_matrix - account_means_matrix
top6 <- cossim(ac_bundles_matrix_adjcos)
top6[,'xmassketches']
recom[1,]<-top6[1,]#"xmassketches" itself appears in top 6 
recom[2:5,]<-top6[3:6,]
recom[,"xmassketches"]





