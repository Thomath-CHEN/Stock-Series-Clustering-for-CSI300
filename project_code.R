library(dtwclust)

# =================================================
# Experiment 0: data loading and preprocessing
# =================================================

# load sector information (each stock belongs to which sector)
sector = read.csv("E:\\硕士学习\\第二年第一学期\\Forecasting & Preductive analystics\\Project\\sector.csv")
sector = sector[,-1]
sector = factor(c(sector[,-1]))

# load stock price data (daily adjusted closed price from 2019/01/01 to 2019/06/28)
csi300 = read.csv("E:\\硕士学习\\第二年第一学期\\Forecasting & Preductive analystics\\Project\\CSI300_train.csv")
csi300 = csi300[,-1]   # delete datetime column
code = names(csi300)  # get the code for each stock
csi300 = t(data.matrix(csi300))
csi300 = zscore(csi300, multivariate = FALSE)

# first experiment
pc.csi300 <- tsclust(csi300, k = 8, type = "partitional",
                 distance = "dtw2", centroid = "pam"
                 )

# Cluster validity indices
cvi(pc.csi300, b = sector)   # single model

plot(pc.csi300)

## experiments in the paper

# =================================================
# Experiment 1: number of clusters
# =================================================

number_k = list()

for( i in 11:20 ){
  
  number_k[i-10] <- tsclust(csi300, k = i,
                              distance = "dtw2", centroid = "pam",
                              trace = TRUE)

}

cvi_result1 <- sapply(number_k, cvi, b = sector)   # multiple model
write.csv(cvi_result1,"E:\\硕士学习\\第二年第一学期\\Forecasting & Preductive analystics\\Project\\cvi_result1.csv")
# indicators to be minimize. Take reverse number to maximize them
cvi_result1[5,] = -cvi_result1[5,]
cvi_result1[12,] = -cvi_result1[12,]
cvi_result1[9,] = -cvi_result1[9,]
cvi_result1[10,] = -cvi_result1[10,]

# Calculate the relative rank under each index
relative_order1 = matrix(,nrow=12,ncol=10)

for (i in 1:12) {
  
  relative_order1[i,] = rank(cvi_result1[i,])
  
}

# sum of ranks for External CVI
EXCVI1 = colSums(relative_order1[1:5,])
# sum of ranks for Internal CVI
INCVI1 = colSums(relative_order1[6:12,])

# we take k = 16

# =================================================
# Experiment 2: choice of distance
# =================================================

dist = list()

dist_name = c("dtw2","sbd","sdtw","gak","lbk")
j = 1

for( i in dist_name ){

  
  if (i=="lbk") {
    
    dist[j] <- tsclust(csi300, k = 16,
                       distance = i, centroid = "pam",
                       trace = TRUE, seed = 8319,
                       args = tsclust_args(dist = list(window.size = 10L, norm="L2")))
    
  } else {
    dist[j] <- tsclust(csi300, k = 16, seed = 8319,
                       distance = i, centroid = "pam", trace = TRUE)
    
    
  }
  
  j = j+1
  
}

cvi_result2 <- sapply(dist, cvi, b = sector)   # multiple model
cvi_result2[5,] = -cvi_result2[5,]
cvi_result2[12,] = -cvi_result2[12,]
cvi_result2[9,] = -cvi_result2[9,]
cvi_result2[10,] = -cvi_result2[10,]

# Calculate the relative rank under each index
relative_order2 = matrix(,nrow=12,ncol=5)

for (i in 1:12) {
  
  relative_order2[i,] = rank(cvi_result2[i,])
  
}

# sum of ranks for External CVI
EXCVI2 = colSums(relative_order2[1:5,])
# sum of ranks for Internal CVI
INCVI2 = colSums(relative_order2[6:12,])

# =================================================
# Experiment 3: Choice of prototype
# =================================================

proto = list()
proto_name = c("pam","dba","shape")
i = 1

for (item in proto_name) {
  
  if (item == "pam"){
    
    proto[i] <- tsclust(csi300, k = 16, seed = 8319,
                        distance = "sbd", centroid = "pam",
                        trace = TRUE)
    
  } else {
    
    proto[i] <- tsclust(csi300, k = 16,seed = 8319,
                        entroid = item,
                        trace = TRUE)
    
  }
  i = i+1

}

cvi_result3 <- sapply(proto, cvi,b = sector)   # multiple model
cvi_result3[5,] = -cvi_result3[5,]
cvi_result3[12,] = -cvi_result3[12,]
cvi_result3[9,] = -cvi_result3[9,]
cvi_result3[10,] = -cvi_result3[10,]

# Calculate the relative rank under each index
relative_order3 = matrix(,nrow=12,ncol=3)

for (i in 1:12) {
  
  relative_order3[i,] = rank(cvi_result3[i,])
  
}

# sum of ranks for External CVI
EXCVI3 = colSums(relative_order3[1:5,])
# sum of ranks for Internal CVI
INCVI3 = colSums(relative_order3[6:12,])

# =================================================
# Experiment 4: Plot the clusters
# =================================================

k = 16

final <- tsclust(csi300, k = k, seed = 8319,
                    distance = "sbd", centroid = "pam",
                    trace = TRUE)

plot(final)   # plot stocks in each cluster

final@cluster  # the indices of each stock belonging to which cluster

res_clu = list()

for (i in 1:k){
  
  ser = c()
  
  for (j in 1:length(final@cluster)) {
    
    if (i==final@cluster[j]) {
      
      ser = c(ser,j)
      
    }
    
  }
  
  res_clu[[i]] = ser
  
}


# =================================================================
# Experiment 5: Calculate the return and volatility in each cluster
# =================================================================

# 1. load test data (from 2019/07/01 to 2019/08/31)

test_org = read.csv("E:\\硕士学习\\第二年第一学期\\Forecasting & Preductive analystics\\Project\\CSI_test.csv")
test = data.matrix(test_org)

# 2. Calculate the average return of clusters
return_stock = colSums(test)

return_matrix = matrix(,nrow = k, ncol = 2)

return_matrix[,1] = 1:k
return_matrix[,2] = 0

for (i in 1:k) {
  
  for (j in 1:length(code)){
    
    if (final@cluster[j]==i){
    
    return_matrix[i,2] = return_matrix[i,2]+return_stock[j]
    
    }
    
  }
  
}

# (1) count the frequency of clusters
counts = c(table(final@cluster))

# (2) calculate the average (equally weighted) return
return_matrix[,2] = return_matrix[,2]/counts

# (3) order the return by descending
return_matrix = return_matrix[order(return_matrix[,2],decreasing=TRUE),]


# 3. Calculate the volatility (standard deviation) of each cluster

std_vector = sapply(test_org, sd)
iter = 200 # set iteration times

# (1) inter-cluster portfolio

std_inter = c()

for (i in 1:iter) {
  
clu_sel = sample(1:k,10, replace=FALSE)
temp = 0

for (j in 1:10) {
  
  temp = temp + std_vector[sample(res_clu[[clu_sel[j]]], 1)]
  
}

std_inter[i] = temp/10
}

print(mean(std_inter))

# (2) random portfolio

std_random = c()

for (i in 1:iter) {

stocks = sample(1:length(std_vector),10, replace=FALSE) 
# randomly select 10 stocks from the whole set, and calculate their average std

std_random[i] = mean(std_vector[stocks]) 

}

print(mean(std_random))

# (3) within-cluster portfolio

std_inner = c()

for (i in 1:iter){
  
  temp = sample(1:k,1)  # randomly select a cluster
  
  while (length(res_clu[[temp]])<10) {
    
    temp = sample(1:k,1)
    
  }
  
  std_inner[i] = std_vector[sample(res_clu[[temp]],10,replace=FALSE)]  
  # randomly select 10 stocks from that cluster and calculate average std
  
}

print(mean(std_inner))

