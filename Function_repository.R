# Scoring function for the output of the main function
scoring <- function(predicted_, test_){
  score=test_ %>%
    select(row_id,place_id)%>%
    left_join(predicted_, by=c("row_id"))%>%
    with((sum(place_id==P1,na.rm=T)+
            sum(place_id==P2,na.rm=T)/2+
            sum(place_id==P3,na.rm=T)/3)/nrow(test_))
  return(score)
}


# ### Main function with C++ distance function
# main_cpp <- function(test_df, train_df, data_df, K=10){
#   Predict=matrix(nrow=nrow(test_df),ncol=4)
#   p=1
#   nb_insuff_nbr=0
#   for(xi in min(test_df$xbin):max(test_df$xbin)){
#     for(yi in min(test_df$ybin):max(test_df$ybin)){
#       #checking that the test area is not empty
#       if(nrow(test_df[xbin==xi & ybin==yi])>0){
#         
#         area=test_df[xbin==xi & ybin==yi]
#         
#         # Selecting the points that are in the bins contiguous to the one chosen
#         sq_nbrhd=subset(train_df, 
#                         xbin>=xi-1 & xbin<=xi+1 & ybin>=yi-1 & ybin<=yi+1)
#         if(nrow(sq_nbrhd)<10){nb_insuff_nbr=nb_insuff_nbr+1}
#         
#         for(i in 1:nrow(area)){
#           # Calculating the euclidean distance between the test point and the 
#           # neighboor points
#           v=c(area$x[i], area$y[i])
#           sq_nbrhd$dist=vdist_cpp2(as.matrix(sq_nbrhd[,.(x,y)]),
#                                    v, lambda)
#           # Ordering the neighboors by distance
#           setorder(sq_nbrhd,dist)
#           #The K closest neighboors "vote"
#           Outcomes <- sq_nbrhd[1:K,]%>%
#             group_by(place_id)%>%
#             tally()
#           setorder(Outcomes,-n)
#           Predict[p,]=c(area$row_id[i],
#                         Outcomes$place_id[1],
#                         Outcomes$place_id[2],
#                         Outcomes$place_id[3]
#           )
#           p=p+1
#         }
#       }
#     }
#   }
#   #Converting to a data.frame
#   Predict2=as.data.frame(Predict)
#   #Converting the level indexes to the level values
#   Predict2[,1]=levels(test_df$row_id)[Predict[,1]]
#   Predict2[,2:4]=levels(data_df$place_id)[Predict[,2:4]]
#   colnames(Predict2)=c("row_id","P1","P2","P3")
#   Predict2$row_id=factor(Predict2$row_id, levels=levels(data_df$row_id))
#   Predict2$P1=factor(Predict2$P1, levels=levels(data_df$place_id))
#   Predict2$P2=factor(Predict2$P2, levels=levels(data_df$place_id))
#   Predict2$P3=factor(Predict2$P3, levels=levels(data_df$place_id))
#   Predict_dt=as.data.table(Predict2)
#   remove(Predict2)
#   return(Predict_dt)}

### Main function, with number of neighbors, 
### and variable number of predicted places

# main_cpp2 <- function(test_, train_, data_, K_=10, NbPredict_=3){
#   Predict=matrix(nrow=nrow(test_),ncol=1+NbPredict_*2)
#   p=1
#   for(xi in min(test_$xbin):max(test_$xbin)){
#     for(yi in min(test_$ybin):max(test_$ybin)){
#         area=test_[xbin==xi & ybin==yi]
#         if(nrow(area)==0){next}
#         
#         # Selecting the points that are in the bins contiguous to the one chosen
#         sq_nbrhd=subset(train_, 
#                         xbin>=xi-1 & xbin<=xi+1 & ybin>=yi-1 & ybin<=yi+1)
#         for(i in 1:nrow(area)){
#           # Calculating the euclidean distance between the test point and the 
#           # neighboor points
#           v=c(area$x[i], area$y[i])
#           sq_nbrhd$dist=vdist_cpp(as.matrix(sq_nbrhd[,.(x,y)]),v)
#           # Ordering the neighboors by distance
#           setorder(sq_nbrhd,dist)
#           #The K closest neighboors "vote"
#           Outcomes <- sq_nbrhd[1:K_,]%>%
#             group_by(place_id)%>%
#             tally()
#           setorder(Outcomes,-n)
#           Predict[p,1]=area$row_id[i]
#           Predict[p,2:(1+NbPredict_)]=Outcomes$place_id[1:NbPredict_]
#           Predict[p,(2+NbPredict_):(2*NbPredict_+1)]=Outcomes$n[1:NbPredict_]
#           p=p+1
#         }
#       }
#     }
#   #Converting to a data.frame
#   Predict2=as.data.frame(Predict)
#   #Converting the level indexes to the level values
#   Predict2[,1]=levels(test_$row_id)[Predict[,1]]
#   Predict2[,2:(1+NbPredict_)]=levels(data_$place_id)[Predict[,2:(1+NbPredict_)]]
# #   Predict2$row_id=factor(Predict2$row_id, levels=levels(data_$row_id))
# #   Predict2$P1=factor(Predict2$P1, levels=levels(data_df$place_id))
# #   Predict2$P2=factor(Predict2$P2, levels=levels(data_df$place_id))
# #   Predict2$P3=factor(Predict2$P3, levels=levels(data_df$place_id))
#   Predict_dt=as.data.table(Predict2)
#   remove(Predict2)
#   return(Predict_dt)}


########################################################################


# ### Main function with C++ distance function using places as train set
# main_cpp_places <- function(test_df, train_df, data_df, K=3,lambda_){
#   Predict=matrix(nrow=nrow(test_df),ncol=4)
#   p=1
#   nb_insuff_nbr=0
#   for(xi in min(test_df$xbin):max(test_df$xbin)){
#     for(yi in min(test_df$ybin):max(test_df$ybin)){
#       #checking that the test area is not empty
#       if(nrow(test_df[xbin==xi & ybin==yi])>0){
#         
#         area=test_df[xbin==xi & ybin==yi]
#         
#         # Selecting the places that are in the bins contiguous to the one chosen
#         sq_nbrhd=subset(train_df, 
#                         xbin>=xi-1 & xbin<=xi+1 & ybin>=yi-1 & ybin<=yi+1)
#         if(nrow(sq_nbrhd)<10){nb_insuff_nbr=nb_insuff_nbr+1}
#         
#         for(i in 1:nrow(area)){
#           # Calculating the euclidean distance between the test point and the 
#           # places
#           v=c(area$x[i], area$y[i])
#           sq_nbrhd$dist=vdist_cpp2(as.matrix(sq_nbrhd[,.(x,y)]),
#                                    v, lambda_)
#           # Ordering the places by distance
#           setorder(sq_nbrhd,dist)
#           
#           Predict[p,]=c(area$row_id[i],
#                         sq_nbrhd$place_id[1],
#                         sq_nbrhd$place_id[2],
#                         sq_nbrhd$place_id[3]
#           )
#           p=p+1
#         }
#       }
#     }
#   }
#   #Converting to a data.frame
#   Predict2=as.data.frame(Predict)
#   #Converting the level indexes to the level values
#   Predict2[,1]=levels(test_df$row_id)[Predict[,1]]
#   Predict2[,2:4]=levels(data_df$place_id)[Predict[,2:4]]
#   colnames(Predict2)=c("row_id","P1","P2","P3")
#   Predict2$row_id=factor(Predict2$row_id, levels=levels(data_df$row_id))
#   Predict2$P1=factor(Predict2$P1, levels=levels(data_df$place_id))
#   Predict2$P2=factor(Predict2$P2, levels=levels(data_df$place_id))
#   Predict2$P3=factor(Predict2$P3, levels=levels(data_df$place_id))
#   Predict_dt=as.data.table(Predict2)
#   remove(Predict2)
#   return(Predict_dt)}
# 
# 
# ###########################################################################


#Distance function matrix-vector written in C++
# lambda=1
# 
# cppFunction('NumericVector vdist_cpp(NumericMatrix A, NumericVector B){
#             int nrow = A.nrow();
#             NumericVector out(nrow);
#             for (int i = 0; i < nrow; ++i){
#             out[i]=sqrt(pow(A(i,0)-B[0], 2.0)+pow(A(i,1)-B[1], 2.0));
#             }
#             return out;
#             }')
# 
# cppFunction('NumericVector vdist_cpp2(NumericMatrix A, NumericVector B, double lambda){
#             int nrow = A.nrow();
#             NumericVector out(nrow);
#             for (int i = 0; i < nrow; ++i){
#             out[i]=sqrt(pow(A(i,0)-B[0], 2.0)+lambda*pow(A(i,1)-B[1], 2.0));
#             }
#             return out;
#             }')
