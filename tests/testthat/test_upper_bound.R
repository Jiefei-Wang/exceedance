context("Test upper bound function")
set.seed(123)
m <- 10

k_test <-function(x,k){
    n <- length(x)
    if(length(x)>=k)
        pbeta(x[k],k,n-k+1)
    else 
        1
}
test_that("General vs JW vs k order",{
    for(i in 1:100){
        k<-5
        alpha <- 0.05
        x<-rbeta(m,1,3)
        
        ## General: General
        params1 <- param_general(function(x)k_test(x,k),algorithm = "general")
        profile1 <- profile_pvalue(x,params1)
        gammabar1 = c()
        for(j in seq_along(x)){
            gammabar1[j] = exceedance_bound(profile1,alpha,sri = 1:j)
        }
        gammabar1
        
        ## General: JW
        params2 <- param_general(function(x)k_test2(x,k),
                                 algorithm = "JW")
        profile2 <- profile_pvalue(x,params2)
        gammabar2 = c()
        for(j in seq_along(x)){
            gammabar2[j] = exceedance_bound(profile2,alpha,sri = 1:j)
        }
        gammabar2
        
        
        ## GW:k order
        params3 <- param_GW(statistic = "k_order",param1 = k)
        profile3 <- profile_pvalue(x,params3)
        gammabar3 = c()
        for(j in seq_along(x)){
            gammabar3[j] = exceedance_bound(profile3,alpha,sri = 1:j)
        }
        gammabar3
        
        
        expect_equal(gammabar1,gammabar2)
        expect_equal(gammabar1,gammabar3)
        # message(i)
    }
})

###################################################
k_test2 <-function(x,k){
    n <- length(x)
    k_max <- k[length(k)]
    if(length(x)>=k_max)
        GKSStat(x,index=k,statName = "KS")$pvalue
    else 
        1
}
test_that("General KS vs KS",{
    for(i in 1:40){
        k<-c(2,4,5)
        alpha <- 0.05
        x<-rbeta(m,1,10)
        
        params1 <- param_general(function(x)k_test2(x,k),
                                 algorithm = "general")
        profile1 <- profile_pvalue(x,params1)
        gammabar1 = c()
        for(j in seq_along(x)){
            gammabar1[j] = exceedance_bound(profile1,alpha,sri = 1:j)
        }
        gammabar1
        
        params2 <- param_GW(statistic = "KS",param1 = k,param2 = k)
        profile2 <- profile_pvalue(x,params2)
        gammabar2 = c()
        for(j in seq_along(x)){
            gammabar2[j] = exceedance_bound(profile2,alpha,sri = 1:j)
        }
        gammabar2
        
        expect_equal(gammabar1,gammabar2)
        # message(i)
    }
})

k_test3 <-function(x,k){
    n <- length(x)
    k_max <- k[length(k)]
    if(length(x)>=k_max)
        GKSStat(x,index=k,statName = "KS+")$pvalue
    else 
        1
}
test_that("General KS+ vs KS+",{
    for(i in 1:40){
        k<-c(2,4,5)
        alpha <- 0.05
        x<-rbeta(m,1,10)
        
        params1 <- param_general(function(x)k_test3(x,k),
                                 algorithm = "general")
        profile1 <- profile_pvalue(x,params1)
        gammabar1 = c()
        for(j in seq_along(x)){
            gammabar1[j] = exceedance_bound(profile1,alpha,sri = 1:j)
        }
        gammabar1
        
        params2 <- param_GW(statistic = "KS",param1 = k)
        profile2 <- profile_pvalue(x,params2)
        gammabar2 = c()
        for(j in seq_along(x)){
            gammabar2[j] = exceedance_bound(profile2,alpha,sri = 1:j)
        }
        gammabar2
        
        expect_equal(gammabar1,gammabar2)
        # message(i)
    }
})

###################################################