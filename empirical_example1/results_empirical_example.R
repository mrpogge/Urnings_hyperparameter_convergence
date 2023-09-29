library(ComplexHeatmap)
library(tidyverse)
library(circlize)

################################################################################
# Cold case
################################################################################

df_g6 = readRDS("empirical_example_G6.rds")
df_g8 = readRDS("empirical_example_G8.rds")
df_g10 = readRDS("empirical_example_G10.rds")

################################################################################
# Correlation based on the urn sizes
################################################################################

#calculating the correlations
irt_person_param_6 = df_g6[1,]
irt_person_param_6 = exp(irt_person_param_6) / (1 + exp(irt_person_param_6))
irt_person_param_8 = df_g8[1,]
irt_person_param_8 = exp(irt_person_param_8) / (1 + exp(irt_person_param_8))
irt_person_param_10 = df_g10[1,]
irt_person_param_10 = exp(irt_person_param_10) / (1 + exp(irt_person_param_10))

cor6 = numeric(nrow(df_g6)-2)
cor8 = numeric(nrow(df_g8)-2)
cor10 = numeric(nrow(df_g10)-2)

for(i in 2:(nrow(df_g6)-1)){
  cor6[i-1] = cor(irt_person_param_6, df_g6[i, ]) 
  cor8[i-1] = cor(irt_person_param_8, df_g8[i, ])  
  cor10[i-1] = cor(irt_person_param_10, df_g10[i, ])  
}

cor_results = matrix(0, nrow = 4, ncol = 4)
player_urn_sizes = c(8,16,32,64)
item_urn_sizes = c(16,32,64,128)
counter = 1 
for(pus in 1:4){
  for(ius in 1:4){
    cor_results[pus,ius] = mean(cor6[counter], cor8[counter], cor10[counter])
    counter = counter + 1 
  }
}

rownames(cor_results) = player_urn_sizes
colnames(cor_results) = item_urn_sizes

col_fun = colorRamp2(c(min(cor_results), mean(cor_results), max(cor_results)), c("red", "white",  "green"))
col_fun(seq(0,100))

Heatmap(cor_results, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Correlation"),
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.3f", cor_results[i,j]), x, y, gp = gpar(fontsize = 10))
        },
        column_title = "Item urn sizes",
        row_title = "Student urn sizes")

################################################################################
# Correlation plots
################################################################################
layout(matrix(c(1:16), 4, 4, byrow = TRUE))

plot(irt_person_param_6, df_g6[2,])
abline(0,1, col = "red")

for(i in 3:(nrow(df_g6)-1)){
  plot(irt_person_param_6, df_g6[i,])
  abline(0,1, col = "red")
}

################################################################################
# Different number of students 
################################################################################
df_g6_list = readRDS("empirical_example_G6_cold_N.rds")
df_g8_list = readRDS("empirical_example_G8_cold_N.rds")
df_g10_list = readRDS("empirical_example_G10_cold_N.rds")

#calculating the correlations
for(n in 1:3){
  df_g6 = df_g6_list[[n]]
  df_g8 = df_g6_list[[n]]
  df_g10 = df_g6_list[[n]]
  
  irt_person_param_6 = df_g6[1,]
  irt_person_param_6 = exp(irt_person_param_6) / (1 + exp(irt_person_param_6))
  irt_person_param_8 = df_g8[1,]
  irt_person_param_8 = exp(irt_person_param_8) / (1 + exp(irt_person_param_8))
  irt_person_param_10 = df_g10[1,]
  irt_person_param_10 = exp(irt_person_param_10) / (1 + exp(irt_person_param_10))
  
  cor6 = numeric(nrow(df_g6)-1)
  cor8 = numeric(nrow(df_g8)-1)
  cor10 = numeric(nrow(df_g10)-1)
  
  for(i in 2:(nrow(df_g6))){
    cor6[i-1] = cor(irt_person_param_6, df_g6[i, ]) 
    cor8[i-1] = cor(irt_person_param_8, df_g8[i, ])  
    cor10[i-1] = cor(irt_person_param_10, df_g10[i, ])  
  }
  
  cor_results = matrix(0, nrow = 4, ncol = 4)
  player_urn_sizes = c(8,16,32,64)
  item_urn_sizes = c(16,32,64,128)
  counter = 1 
  for(pus in 1:4){
    for(ius in 1:4){
      cor_results[pus,ius] = mean(cor6[counter], cor8[counter], cor10[counter])
      counter = counter + 1 
    }
  }
  
  rownames(cor_results) = player_urn_sizes
  colnames(cor_results) = item_urn_sizes
  
  col_fun = colorRamp2(c(min(cor_results), mean(cor_results), max(cor_results)), c("red", "white",  "green"))
  col_fun(seq(0,100))
  
  Heatmap(cor_results, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Correlation"),
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.3f", cor_results[i,j]), x, y, gp = gpar(fontsize = 10))
          },
          column_title = "Item urn sizes",
          row_title = "Student urn sizes")
}

################################################################################
# Cold case stop
################################################################################
df_g6 = readRDS("empirical_example_G6_stop.rds")
df_g8 = readRDS("empirical_example_G8_stop.rds")
df_g10 = readRDS("empirical_example_G10_stop.rds")

stop_results_g6 = matrix(0, 4,4)
stop_results_g8 = matrix(0, 4,4)
stop_results_g10 = matrix(0, 4,4)

player_urn_sizes = c(8,16,32,64)
item_urn_sizes = c(16,32,64,128)
counter = 2
for(pus in 1:4){
  for(ius in 1:4){
    stop_results_g6[pus,ius] = rowMeans(df_g6)[counter]
    stop_results_g8[pus,ius] = rowMeans(df_g8)[counter]
    stop_results_g10[pus,ius] = rowMeans(df_g10)[counter]
    counter = counter + 1 
  }
}

rownames(stop_results_g6) = player_urn_sizes
colnames(stop_results_g6) = item_urn_sizes

col_fun = colorRamp2(c(max(stop_results_g6), mean(stop_results_g6), min(stop_results_g6)), c("red", "white",  "green"))
col_fun(seq(0,100))

Heatmap(stop_results_g6, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Stop time"),
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.3f", stop_results_g6[i,j]), x, y, gp = gpar(fontsize = 10))
        },
        column_title = "Item urn sizes",
        row_title = "Student urn sizes")
################################################################################
# Different number of students stop
################################################################################
df_g6_list = readRDS("empirical_example_G6_cold_N_stop.rds")
df_g8_list = readRDS("empirical_example_G8_cold_N_stop.rds")
df_g10_list = readRDS("empirical_example_G10_cold_N_stop.rds")

for(n in 1:3){
  df_g6 = df_g6_list[[n]]
  df_g8 = df_g6_list[[n]]
  df_g10 = df_g6_list[[n]]
  
  stop_results_g6 = matrix(0, 4,4)
  stop_results_g8 = matrix(0, 4,4)
  stop_results_g10 = matrix(0, 4,4)
  
  player_urn_sizes = c(8,16,32,64)
  item_urn_sizes = c(16,32,64,128)
  counter = 2
  for(pus in 1:4){
    for(ius in 1:4){
      stop_results_g6[pus,ius] = rowMeans(df_g6)[counter]
      stop_results_g8[pus,ius] = rowMeans(df_g8)[counter]
      stop_results_g10[pus,ius] = rowMeans(df_g10)[counter]
      counter = counter + 1 
    }
  }
  
  rownames(stop_results_g6) = player_urn_sizes
  colnames(stop_results_g6) = item_urn_sizes
  
  col_fun = colorRamp2(c(max(stop_results_g6), mean(stop_results_g6), min(stop_results_g6)), c("red", "white",  "green"))
  col_fun(seq(0,100))
  
  Heatmap(stop_results_g6, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Stop time"),
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.3f", stop_results_g6[i,j]), x, y, gp = gpar(fontsize = 10))
          },
          column_title = "Item urn sizes",
          row_title = "Student urn sizes")
  
}
  
  


################################################################################
# Warm case 
################################################################################
df_g6 = readRDS("empirical_example_G6_warm.rds")
df_g8 = readRDS("empirical_example_G8_warm.rds")
df_g10 = readRDS("empirical_example_G10_warm.rds")

#calculating the correlations
irt_person_param_6 = df_g6[1,]
irt_person_param_6 = exp(irt_person_param_6) / (1 + exp(irt_person_param_6))
irt_person_param_8 = df_g8[1,]
irt_person_param_8 = exp(irt_person_param_8) / (1 + exp(irt_person_param_8))
irt_person_param_10 = df_g10[1,]
irt_person_param_10 = exp(irt_person_param_10) / (1 + exp(irt_person_param_10))

cor6 = numeric(nrow(df_g6)-1)
cor8 = numeric(nrow(df_g8)-1)
cor10 = numeric(nrow(df_g10)-1)

for(i in 2:(nrow(df_g6))){
  cor6[i-1] = cor(irt_person_param_6, df_g6[i, ]) 
  cor8[i-1] = cor(irt_person_param_8, df_g8[i, ])  
  cor10[i-1] = cor(irt_person_param_10, df_g10[i, ])  
}

cor_results = matrix(0, nrow = 4, ncol = 4)
player_urn_sizes = c(8,16,32,64)
item_urn_sizes = c(16,32,64,128)
counter = 1 
for(pus in 1:4){
  for(ius in 1:4){
    cor_results[pus,ius] = mean(cor6[counter], cor8[counter], cor10[counter])
    counter = counter + 1 
  }
}

rownames(cor_results) = player_urn_sizes
colnames(cor_results) = item_urn_sizes

col_fun = colorRamp2(c(min(cor_results), mean(cor_results), max(cor_results)), c("red", "white",  "green"))
col_fun(seq(0,100))

Heatmap(cor_results, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Correlation"),
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.3f", cor_results[i,j]), x, y, gp = gpar(fontsize = 10))
        },
        column_title = "Item urn sizes",
        row_title = "Student urn sizes")

################################################################################
# Correlation plots
################################################################################
layout(matrix(c(1:16), 4, 4, byrow = TRUE))

plot(irt_person_param_6, df_g6[2,])
abline(0,1, col = "red")

for(i in 3:(nrow(df_g6)-1)){
  plot(irt_person_param_6, df_g6[i,])
  abline(0,1, col = "red")
}
################################################################################
# Different number of students 
################################################################################
df_g6_list = readRDS("empirical_example_G6_warm_N.rds")
df_g8_list = readRDS("empirical_example_G8_warm_N.rds")
df_g10_list = readRDS("empirical_example_G10_warm_N.rds")

#calculating the correlations
for(n in 1:3){
  df_g6 = df_g6_list[[n]]
  df_g8 = df_g6_list[[n]]
  df_g10 = df_g6_list[[n]]
  
  irt_person_param_6 = df_g6[1,]
  irt_person_param_6 = exp(irt_person_param_6) / (1 + exp(irt_person_param_6))
  irt_person_param_8 = df_g8[1,]
  irt_person_param_8 = exp(irt_person_param_8) / (1 + exp(irt_person_param_8))
  irt_person_param_10 = df_g10[1,]
  irt_person_param_10 = exp(irt_person_param_10) / (1 + exp(irt_person_param_10))
  
  cor6 = numeric(nrow(df_g6)-1)
  cor8 = numeric(nrow(df_g8)-1)
  cor10 = numeric(nrow(df_g10)-1)
  
  for(i in 2:(nrow(df_g6))){
    cor6[i-1] = cor(irt_person_param_6, df_g6[i, ]) 
    cor8[i-1] = cor(irt_person_param_8, df_g8[i, ])  
    cor10[i-1] = cor(irt_person_param_10, df_g10[i, ])  
  }
  
  cor_results = matrix(0, nrow = 4, ncol = 4)
  player_urn_sizes = c(8,16,32,64)
  item_urn_sizes = c(16,32,64,128)
  counter = 1 
  for(pus in 1:4){
    for(ius in 1:4){
      cor_results[pus,ius] = mean(cor6[counter], cor8[counter], cor10[counter])
      counter = counter + 1 
    }
  }
  
  rownames(cor_results) = player_urn_sizes
  colnames(cor_results) = item_urn_sizes
  
  col_fun = colorRamp2(c(min(cor_results), mean(cor_results), max(cor_results)), c("red", "white",  "green"))
  col_fun(seq(0,100))
  
  Heatmap(cor_results, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Correlation"),
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.3f", cor_results[i,j]), x, y, gp = gpar(fontsize = 10))
          },
          column_title = "Item urn sizes",
          row_title = "Student urn sizes")
}

################################################################################
# warm case stop
################################################################################
df_g6 = readRDS("empirical_example_G6_warm_stop.rds")
df_g8 = readRDS("empirical_example_G8_warm_stop.rds")
df_g10 = readRDS("empirical_example_G10_warm_stop.rds")

stop_results_g6 = matrix(0, 4,4)
stop_results_g8 = matrix(0, 4,4)
stop_results_g10 = matrix(0, 4,4)

player_urn_sizes = c(8,16,32,64)
item_urn_sizes = c(16,32,64,128)
counter = 2
for(pus in 1:4){
  for(ius in 1:4){
    stop_results_g6[pus,ius] = rowMeans(df_g6)[counter]
    stop_results_g8[pus,ius] = rowMeans(df_g8)[counter]
    stop_results_g10[pus,ius] = rowMeans(df_g10)[counter]
    counter = counter + 1 
  }
}

rownames(stop_results_g6) = player_urn_sizes
colnames(stop_results_g6) = item_urn_sizes

col_fun = colorRamp2(c(max(stop_results_g6), mean(stop_results_g6), min(stop_results_g6)), c("red", "white",  "green"))
col_fun(seq(0,100))

Heatmap(stop_results_g6, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Stop time"),
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.3f", stop_results_g6[i,j]), x, y, gp = gpar(fontsize = 10))
        },
        column_title = "Item urn sizes",
        row_title = "Student urn sizes")

################################################################################
# Different number of students stop
################################################################################
df_g6_list = readRDS("empirical_example_G6_warm_N_stop.rds")
df_g8_list = readRDS("empirical_example_G8_warm_N.rds")
df_g10_list = readRDS("empirical_example_G10_warm_N_stop.rds")

for(n in 1:3){
  df_g6 = df_g6_list[[n]]
  df_g8 = df_g6_list[[n]]
  df_g10 = df_g6_list[[n]]
  
  stop_results_g6 = matrix(0, 4,4)
  stop_results_g8 = matrix(0, 4,4)
  stop_results_g10 = matrix(0, 4,4)
  
  player_urn_sizes = c(8,16,32,64)
  item_urn_sizes = c(16,32,64,128)
  counter = 2
  for(pus in 1:4){
    for(ius in 1:4){
      stop_results_g6[pus,ius] = rowMeans(df_g6)[counter]
      stop_results_g8[pus,ius] = rowMeans(df_g8)[counter]
      stop_results_g10[pus,ius] = rowMeans(df_g10)[counter]
      counter = counter + 1 
    }
  }
  
  rownames(stop_results_g6) = player_urn_sizes
  colnames(stop_results_g6) = item_urn_sizes
  
  col_fun = colorRamp2(c(max(stop_results_g6), mean(stop_results_g6), min(stop_results_g6)), c("red", "white",  "green"))
  col_fun(seq(0,100))
  
  Heatmap(stop_results_g6, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, heatmap_legend_param = list(title = "Stop time"),
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.3f", stop_results_g6[i,j]), x, y, gp = gpar(fontsize = 10))
          },
          column_title = "Item urn sizes",
          row_title = "Student urn sizes")
  
}
