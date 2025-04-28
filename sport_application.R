
# Required packages
library(BiDAG)
library(pcalg)
library(bnlearn)
library(corrplot)
library(igraph)
library(abind)
source("ordinalEffects.R")
source("./OSEMSource/R/OrdinalScore.R")
insertSource("./OSEMSource/R/spacefns.R",package = "BiDAG")
insertSource("./OSEMSource/R/usrscorefns.R",package = "BiDAG")
insertSource("./OSEMSource/R/initpar.R",package = "BiDAG")
insertSource("./OSEMSource/R/scoreagainstdag.R",package = "BiDAG")

# Data processing
load("./MyOrdinalEffects/Application/OrdinalData.RData")
n <- ncol(OrdinalData)
N <- nrow(OrdinalData)
OrdinalData[] <- lapply(OrdinalData, as.integer)
load("./MyOrdinalEffects/Application/Results_Boot_sportinjury_100.RData")

###########
# Bootstrapping
# NOT RUN
#source("psych_boot.R")

# Compute Effects
# NOT RUN

#OWEN Methods
Effects<-list()
system.time(for (i in 1:100){
  OSEMfit<-results[[i]]
  cuts <-OSEMfit[["param"]][["cuts"]]
  S<- OSEMfit[["param"]][["Sigma_hat"]]
  DAG <- as.matrix(OSEMfit$DAG)
  Chol <- getCov(S,DAG)
  B <- Chol[[1]]
  Vchol <- Chol[[2]]
  Effects[[i]] <-getallEffects(mu=rep(0,n),B,V=Vchol,cuts,intType = "OWEN")
})

## Distributional method
# Effectsdis<-list()
# system.time(for (i in 1:500){
#   OSEMfit<-results[[i]]
#   cuts <-OSEMfit[["param"]][["cuts"]]
#   S<- OSEMfit[["param"]][["Sigma_hat"]]
#   DAG <- as.matrix(OSEMfit$DAG)
#   Chol <- getCov(S,DAG)
#   B <- Chol[[1]]
#   Vchol <- Chol[[2]]
#   Effectsdis[[i]] <-getallEffects(mu=rep(0,n),B,V=Vchol,cuts,intType = "DIS")
#   print(i)
# })

#save Effects to a file 
save(Effects, file = file.path("./MyOrdinalEffects/Application","Sportinjury_Effects_100.RData"))
# save(Effectsdis, file="Psych_EffectsDIS.RData")
load("./MyOrdinalEffects/Application/Sportinjury_Effects_100.RData")
#load("./Application/Psych_EffectsDIS.RData")

# Check computations
# for (i in 1:500){
#   if (!(identical(round(unlist(Effects[[i]]),2),round(unlist(Effectsdis[[i]]),2)))){
#   print(i)
#   }
# }

# i = 338 not equal, check the position of the mismatch (code below): int 5 on out 8. 
# for (k in 1:24){
#   for (j in 1:24){
#   if (!(identical(round(unlist(Effects[[i]][[k]][[j]]),2),round(unlist(Effectsdis[[i]][[k]][[j]]),2)))){
#     print(paste0(k,j))
#   }
#   }
# }



#Generate list of effects for each int-out couple
effects4couple<-vector(mode="list", 11)
for (i in 1:11){
  effects4couple[[i]]<-vector(mode="list",11)
  for (j in 1:11){
    effects4couple[[i]][[j]]<-vector(mode="list",100)
    for (k in c(1:100)){
      effects4couple[[i]][[j]][[k]]<-Effects[[k]][[i]][[j]]
    }
  }
}


# Point estimates这里好像跟上面数据量的大小无关必须加载BiDAG
OSEMfit_point <- ordinalStructEM(n, OrdinalData,
                                 usrpar = list(penType = "other",
                                               L = 5,
                                               lambda = 6))
save(OSEMfit_point,file = file.path("./MyOrdinalEffects/Application","OSEMfit_point.RData"))
g <- as_graphnel(graph_from_adjacency_matrix(OSEMfit_point$DAG))
cpdag_OSEM <- dag2cpdag(g)
png(paste0("OSEM_CDDAG_Pysch_100",i,".png"), width = 465, height = 225, units='mm', res = 300)
plot(as(cpdag_OSEM, "graphNEL"),main = "Cpdag estimated with OSEM")
dev.off()



# Get strengths arrows 
#Credits: OSEM code source application 
res<-list()
for (i in 1:100){
  res[[i]]<- results[[i]]$maxtrace[[1]]$DAG
}
newarray<-array(NA, dim=c(n,n,100))
for (i in 1:n){
  for (j in 1:n){
    for (k in 1:100){
      newarray[i,j,k]<-res[[k]][i,j]
    }
  }
}
res_cpdag<-newarray
for (j in c(1:100)) {
  res_cpdag[,,j] <- dag2cpdag(newarray[,,j])
}
res_OSEM<-res_cpdag
save(res_OSEM, file=file.path("./MyOrdinalEffects/Application","Res_OSEM_sportinjury_100.RData"))
load("./MyOrdinalEffects/Application/Res_OSEM_sportinjury_100.RData")

get_strength <- function (c, cboot) {
  n <- nrow(c)
  for (i in c(1:n)) {
    for (j in c(1:n)) {
      if (c[i,j] & i != j) {
        c[i,j] <- mean(apply(cboot,3,function (A) if ((A[i,j] + A[j,i]) == 1) {A[i,j]} else {A[i,j] / 2}))
      }
    }
  }
  return(c)
}

pdf("cpdag_OSEM_plot.pdf", width = 8, height = 6)

# 绘制图形时候考虑到了采样的数据res_OSEM，这个数据来源于results
cpdag_OSEM <- as(OSEMfit_point$DAG,"matrix")
cpdag_OSEM <- get_strength(cpdag_OSEM, res_OSEM)
corrplot(cpdag_OSEM, method = "shade", is.corr = FALSE,
         tl.col = "grey", col.lim = c(0,1),
         mar = c(1,0,0,0)+0.5, addgrid.col = "lightgrey", diag=FALSE)
#title(sub = "OSEM")

# 关闭 PDF 输出
dev.off()




######### Plots Rain PLot New 
# Preallocate the data frame for plotting Effects 9 on 10 
Effects511 <- effects4couple[[5]][[11]]
total_rows <- 100 * 2 * 3  # Total iterations
data <- data.frame(Level = integer(total_rows),
                   Change = integer(total_rows),
                   OCE = numeric(total_rows))

row_index <- 1
for (i in 1:100) {
  for (l in 1:2) {
    for (k in 1:3) {
      if (k %in% c(1, 2)) {
        data[row_index, ] <- c(l, k, Effects511[[i]][,,l][1, k + 1 ])
      } else {
        data[row_index, ] <- c(l, k, Effects511[[i]][,,l][2, 3])
      }
      row_index <- row_index + 1
    }
  }
}
data$Change <- as.factor(data$Change)
data$Level <- as.factor(data$Level)
save(data, file = "MyOrdinalEffects/Application/DataInt511_For_Plots_100.RData")







