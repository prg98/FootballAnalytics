setwd('F:/Google Drive Synced/MTech - Coursework/Seminars/Sports Analytics- CoLLearn')
df1<- read.csv("Big5CollatedPlayerStats_6thJune2021.csv")
a<-df1[df1$Min>1000,]
filtereddata<-df1[df1$Min>500,]
filtereddata2<-filtereddata[,c(1:8,20:148)]
filtereddata3<-filtereddata2[grepl("MF",filtereddata$Pos),]

squaredsum<- function(abc){
  sumanswer<-sum(abc[,-1]*abc[,-1])
  sumanswer
}
sumproduct<- function(abc,def){
  sumprod<-sum(abc[,-1]*def[,-1])
  sumprod
}
alldata_scaled <- function(player_name,datfr){
  
  datfr[,c(9:137)]<-scale(datfr[,c(9:137)])
  vector_input <-datfr[datfr$Player==player_name,]
  vector_input
  
  sqr_sum_player<-squaredsum(vector_input[,(9:137)])
  df<-datfr[datfr$Player!=player_name,]
  similars <-setNames(data.frame(matrix(ncol=7,nrow=0)),c("name","position","club","age","cosine","manhattan","euclidean"))
  j<-1
  for (i in 1:nrow(df)){
    vector2<-df[i,]
    sqr_sum_player2<-squaredsum(vector2[,c(9:137)])
    prod<-sumproduct(vector_input[,c(9:137)],vector2[,c(9:137)])
    cos_dist <-prod/(sqr_sum_player*sqr_sum_player2)
    man_dist <- sum(abs(vector_input[,c(9:137)]-vector2[,c(9:137)]))
    euclid<-dist(rbind(vector_input[,c(9:137)],vector2[,c(9:137)]))
    rm
    similars[j,"name"]<-paste0(vector2[1,2])
    similars[j,"position"]<-paste0(vector2[1,4])
    similars[j,"club"]<-paste0(vector2[1,5])
    similars[j,"age"]<-paste0(vector2[1,7])
    similars[j,"cosine"]<-cos_dist
    similars[j,"manhattan"]<-man_dist
    similars[j,"euclidean"]<-euclid 
    j<-j+1
  }
  similars
    
}
sclaed_results<-alldata_scaled("Sergio-Aguero",filtereddata3)


# ##########################################
filtereddata4<-as.data.frame(df1)
filtereddata4[,c(20:148)]<-apply(filtereddata4[,c(20:148)],2,rank)/nrow(filtereddata4)

filtereddata4<-filtereddata4[df1$Min>500,]
filtereddata4<-filtereddata4[,c(1:8,20:148)]
filtereddata5<-filtereddata4[grepl("MF",filtereddata$Pos),]

squaredsum<- function(abc){
  sumanswer<-sum(abc[,-1]*abc[,-1])
  sumanswer
}
sumproduct<- function(abc,def){
  sumprod<-sum(abc[,-1]*def[,-1])
  sumprod
}
alldata_percentile <- function(player_name,datfr){
  
  datfr[,c(9:137)]<-scale(datfr[,c(9:137)])
  vector_input <-datfr[datfr$Player==player_name,]
  vector_input
  
  sqr_sum_player<-squaredsum(vector_input[,(9:137)])
  df<-datfr[datfr$Player!=player_name,]
  similars <-setNames(data.frame(matrix(ncol=7,nrow=0)),c("name","position","club","age","cosine","manhattan","euclidean"))
  j<-1
  for (i in 1:nrow(df)){
    vector2<-df[i,]
    sqr_sum_player2<-squaredsum(vector2[,c(9:137)])
    prod<-sumproduct(vector_input[,c(9:137)],vector2[,c(9:137)])
    cos_dist <-prod/(sqr_sum_player*sqr_sum_player2)
    man_dist <- sum(abs(vector_input[,c(9:137)]-vector2[,c(9:137)]))
    euclid<-dist(rbind(vector_input[,c(9:137)],vector2[,c(9:137)]))
    rm
    similars[j,"name"]<-paste0(vector2[1,2])
    similars[j,"position"]<-paste0(vector2[1,4])
    similars[j,"club"]<-paste0(vector2[1,5])
    similars[j,"age"]<-paste0(vector2[1,7])
    similars[j,"cosine"]<-cos_dist
    similars[j,"manhattan"]<-man_dist
    similars[j,"euclidean"]<-euclid 
    j<-j+1
  }
  similars
  
}
percentile_results<-alldata_percentile("Sergio-Aguero",filtereddata5)

