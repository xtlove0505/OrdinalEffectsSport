library(parallel)
cl <- makeCluster(detectCores()-1)
load("./MySportinjury/OrdinalDataT.RData")
set.seed(1234)
list<-list()
for (i in 1:500){
  N<-42766
  list[[i]] <- OrdinalData[sample(N,N,replace = TRUE),]
}
save(list, file = "Boot_List_sport500.RData")
# 

clusterEvalQ(cl,{
  library(BiDAG)
  load("./MyOrdinalEffects/Application/OrdinalDataT.RData")
  #load("./Application/Boot_List.RData")
  #source("ordinalEffects.R")
  source("./OSEMSource/R/OrdinalScore.R")
  insertSource("./OSEMSource/R/spacefns.R",package = "BiDAG")
  insertSource("./OSEMSource/R/usrscorefns.R",package = "BiDAG")
  insertSource("./OSEMSource/R/initpar.R",package = "BiDAG")
  insertSource("./OSEMSource/R/scoreagainstdag.R",package = "BiDAG")
})

clusterEvalQ(cl, {
  sim_once <- function(x) {
    n <- ncol(OrdinalData)
    N <- nrow(OrdinalData)
    OSEMfit <- ordinalStructEM(n, x,
                               usrpar = list(penType = "other",
                                             L = 5,
                                             lambda = 6))
    # cuts <-OSEMfit[["param"]][["cuts"]]
    # S<- OSEMfit[["param"]][["Sigma_hat"]]
    # DAG <- as.matrix(OSEMfit$DAG)
    # Chol <- getCov(S,DAG)
    # B <- Chol[[1]]
    # Vchol <- Chol[[2]]
    # res <-getallEffects(mu=rep(0,n),B,V=Vchol,cuts,intType = "OWEN")
    res<-OSEMfit
    return(res)
  }
})


system.time(results <- parLapply(cl,list,function(x) sim_once(x)))
stopCluster(cl)
save(results, file = "Results_Boot_sport_500.RData")
