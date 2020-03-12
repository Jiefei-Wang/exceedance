#############################
## These functions are replaced by
## C++ functions
#############################



## get the range index L_i,H_i
## such that x[L_i]>=l_i and x[H_i] <= h_i 
# get_range_by_bound<-function(sx,bound){
#     nx <- length(sx)
#     l <- bound$l
#     h <- bound$h
#     n <- length(l)
#     L <- rep(0, n)
#     H <- rep(0, n)
#     index_x_l <-1L
#     index_x_h <- nx
#     for(i in seq_len(n)){
#         j<- n-i+1L
#         repeat{
#             if(sx[index_x_l]>=l[i]&&index_x_l>=i){
#                 L[i] <- index_x_l
#                 break
#             }else{
#                 index_x_l <- index_x_l +1L
#                 if(index_x_l == nx+1L){
#                     return(NULL)
#                 }
#             }
#         }
#         repeat{
#             if(sx[index_x_h] <= h[j]&&nx-index_x_h+1L>=i){
#                 H[j] <- index_x_h
#                 break
#             }else{
#                 index_x_h <- index_x_h -1L
#                 if(index_x_h == 0L){
#                     return(NULL)
#                 }
#             }
#         }
#         index_x_h = index_x_h-1L
#     }
#     if(any(L>H)){
#         return(NULL)
#     }
#     list(L=L,H=H)
# }






