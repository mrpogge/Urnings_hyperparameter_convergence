library(tidyverse)

sim2_linear = readRDS("sim2_linear.rds")
sim2_discrete_1jump = readRDS("sim2_discrete_1jump.rds")
colnames(sim2_linear)[3] = "amount_of_change"


#recreating change
change_matrix_1jump_first = matrix(rep(sim2_discrete_1jump[,"true_value_first"], 249), nrow = nrow(sim2_discrete_1jump), ncol = 249)
change_matrix_1jump_second = log(change_matrix_1jump_first / (1-change_matrix_1jump_first)) + sim2_discrete_1jump[,"amount_of_change"]
change_matrix_1jump_second = exp(change_matrix_1jump_second) / (1 + exp(change_matrix_1jump_second))
change_matrix_1jump = cbind(change_matrix_1jump_first, change_matrix_1jump_second)
rm(change_matrix_1jump_first, change_matrix_1jump_second)

change_matrix_linear = matrix(0, nrow = nrow(sim2_linear), ncol = 500)
change_matrix_linear[,1] = log(sim2_linear[,"true_value_first"] / (1-sim2_linear[,"true_value_first"]))
for(cl in 2:ncol(change_matrix_linear)){
  change_matrix_linear[, cl] = change_matrix_linear[, cl-1] + sim2_linear[, "amount_of_change"]
}
change_matrix_linear = exp(change_matrix_linear) / (1 + exp(change_matrix_linear))


#mse
sqe_output_1jump_helper = (change_matrix_1jump - (sim2_discrete_1jump %>% select(starts_with("iter"))))^2
sqe_output_1jump = sim2_discrete_1jump
sqe_output_1jump[,6:505] = sqe_output_1jump_helper

sqe_output_linear_helper = (change_matrix_linear - (sim2_linear %>% select(starts_with("iter"))))^2
sqe_output_linear = sim2_linear
sqe_output_linear[,6:505] = sqe_output_linear_helper

#0.1 bounds
bounds_linear_lower = change_matrix_linear - 0.05 < sim2_linear %>% select(starts_with("iter"))
bounds_linear_upper = change_matrix_linear + 0.05 >  sim2_linear %>% select(starts_with("iter"))
bounds_linear_helper = bounds_linear_lower & bounds_linear_upper
bounds_linear = sim2_linear[,1:5] 
bounds_linear_helper = rowMeans(bounds_linear_helper)
bounds_linear = cbind(bounds_linear, bounds_linear_helper)
#bias discrete
bias_urnXstartingXchange = sim2_discrete_1jump %>%
  filter(true_value_first == 0.5) %>%
  group_by(amount_of_change) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(amount_of_change, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(amount_of_change)

true_value_plotting = change_matrix_1jump[change_matrix_1jump[,1] == 0.5,]
plot(true_value_plotting[1, ], type = "l", lty = "dotted", ylim = c(0,1))
for(i in 2:nrow(true_value_plotting)){
  lines(true_value_plotting[i, ], lty = "dotted")
}

for(i in 1:nrow(bias_urnXstartingXchange)){
  lines(as.vector(unlist(bias_urnXstartingXchange[i,-c(1,2)])), col = (i %% 4) + 1)
}

#bias 
bias_urnXstartingXchange = sim2_discrete_1jump %>%
  filter(true_value_first == '0.1', adapt == "n_adaptive") %>%
  group_by(amount_of_change, player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, amount_of_change, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(amount_of_change, player_urn_size)

true_value_plotting = change_matrix_1jump[change_matrix_1jump[,1] == '0.1',]
lines(true_value_plotting[1, ], lty = "dotted")
for(i in 2:nrow(true_value_plotting)){
  lines(true_value_plotting[i, ], lty = "dotted")
}

for(i in 1:nrow(bias_urnXstartingXchange)){
  lines(as.vector(unlist(bias_urnXstartingXchange[i,-c(1,2)])), col = (i %% 4) + 1)
}

#bias 
bias_urnXstartingXchange = sim2_discrete_1jump %>%
  filter(true_value_first == 0.9, adapt == "n_adaptive") %>%
  group_by(amount_of_change, player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, amount_of_change, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(amount_of_change, player_urn_size)

true_value_plotting = change_matrix_1jump[change_matrix_1jump[,1] == 0.9,]
lines(true_value_plotting[1, ], lty = "dotted")
for(i in 2:nrow(true_value_plotting)){
  lines(true_value_plotting[i, ], lty = "dotted")
}

for(i in 1:nrow(bias_urnXstartingXchange)){
  lines(as.vector(unlist(bias_urnXstartingXchange[i,-c(1,2)])), col = (i %% 4) + 1)
}

#bias linear
colnames(sim2_linear)[3] = "amount_of_change"

bias_urnXstartingXchange = sim2_linear %>%
  filter(true_value_first == 0.5) %>%
  group_by(amount_of_change, player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, amount_of_change, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(amount_of_change, player_urn_size)

true_value_plotting = change_matrix_linear[change_matrix_linear[,1] == 0.5,]
plot(true_value_plotting[1, ], type = "l", lty = "dotted", ylim = c(0,1))
for(i in 2:nrow(true_value_plotting)){
  lines(true_value_plotting[i, ], lty = "dotted")
}

for(i in 1:nrow(bias_urnXstartingXchange)){
  lines(as.vector(unlist(bias_urnXstartingXchange[i,-c(1,2)])), col = (i %% 4) + 1)
}

bias_urnXstartingXchange = sim2_linear %>%
  filter(true_value_first == 0.9) %>%
  group_by(amount_of_change, player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, amount_of_change, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(amount_of_change, player_urn_size)

true_value_plotting = change_matrix_linear[change_matrix_linear[,1] == 0.9,]
lines(true_value_plotting[1, ], lty = "dotted")
for(i in 2:nrow(true_value_plotting)){
  lines(true_value_plotting[i, ], lty = "dotted")
}

for(i in 1:nrow(bias_urnXstartingXchange)){
  lines(as.vector(unlist(bias_urnXstartingXchange[i,-c(1,2)])), col = (i %% 4) + 1)
}

bias_urnXstartingXchange = sim2_linear %>%
  filter(true_value_first == '0.1') %>%
  group_by(amount_of_change, player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, amount_of_change, starts_with('iter')) %>%
  mutate_at(1, as.integer) %>%
  arrange(amount_of_change, player_urn_size)

true_value_plotting = change_matrix_linear[change_matrix_linear[,1] == '0.1',]
lines(true_value_plotting[1, ], lty = "dotted")
for(i in 2:nrow(true_value_plotting)){
  lines(true_value_plotting[i, ], lty = "dotted")
}

for(i in 1:nrow(bias_urnXstartingXchange)){
  lines(as.vector(unlist(bias_urnXstartingXchange[i,-c(1,2)])), col = (i %% 4) + 1)
}

#heatmaps
library(ComplexHeatmap)
library(circlize)
library(png)
library(grid)

colnames(sqe_output_linear)[3] = "amount_of_change"

changeXurnsizeXadapt_sqe = sqe_output_linear[,1:5] 
mean_sqe = rowMeans(sqe_output_linear[,6:106])
changeXurnsizeXadapt_sqe = cbind(changeXurnsizeXadapt_sqe, mean_sqe)



mat_list = list()
counter = 1
for(i in c("n_adaptive", "adaptive50", "adaptive70", "adaptive_sigma")){
  for(j in c('8','16','32','64')){
    bounds_test = bounds_linear %>%
      filter(adapt == i, player_urn_size == j) %>%
      select(-when, -adapt) %>%
      group_by(amount_of_change, true_value_first) %>%
      summarise(across(starts_with("bounds"), ~ mean(.))) %>%
      pivot_wider(names_from = true_value_first, values_from = bounds_linear_helper) %>%
      column_to_rownames("amount_of_change") %>%
      arrange_all()
    
      mat_data <- as.matrix(bounds_test[order(as.numeric(rownames(bounds_test))),
                                                        order(as.numeric(colnames(bounds_test)))])
      mat_list[[counter]] = mat_data
      print(c(i,j))
      counter = counter + 1 
  }
}
col_fun = colorRamp2(c(min(unlist(mat_list)), mean(unlist(mat_list)), max(unlist(mat_list))), c("red", "white",  "green"))
col_fun(seq(0,100))

cell_fun = function(data, j, i, x, y, width, height, fill) {
  grid.text(sprintf("%.1f", data[i,j]), x, y, gp = gpar(fontsize = 10))}

#creating plot
file_path = file.path("figures/coverage_map1.png")
png(file_path, width = 10, height = 5, units = "in", res = 300)
#n_adaptive
h1 = Heatmap(mat_list[[1]], cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, column_title = "8", heatmap_legend_param = list(title = "Coverage"),
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[1]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h2 = Heatmap(mat_list[[2]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "16", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[2]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h3 = Heatmap(mat_list[[3]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "32", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[3]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h4 = Heatmap(mat_list[[4]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "64", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[4]][i,j]), x, y, gp = gpar(fontsize = 10))
             })

ht_list = h1 + h2 + h3 +h4
draw(ht_list, row_title = "U(0,1)")
dev.off()

#adaptive50
file_path = file.path("figures/coverage_map2.png")
png(file_path, width = 10, height = 5, units = "in", res = 300)
h1 = Heatmap(mat_list[[5]], cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, column_title = "8", heatmap_legend_param = list(title = "Coverage"),
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[5]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h2 = Heatmap(mat_list[[6]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "16", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[6]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h3 = Heatmap(mat_list[[7]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "32", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[7]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h4 = Heatmap(mat_list[[8]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "64", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[8]][i,j]), x, y, gp = gpar(fontsize = 10))
             })

ht_list = h1 + h2 + h3 +h4
draw(ht_list, row_title = "N(0,0.5)")
dev.off()
#adaptive70

file_path = file.path("figures/coverage_map3.png")
png(file_path, width = 10, height = 5, units = "in", res = 300)
h1 = Heatmap(mat_list[[9]], cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, column_title = "8", heatmap_legend_param = list(title = "Coverage"),
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[9]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h2 = Heatmap(mat_list[[10]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "16", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[10]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h3 = Heatmap(mat_list[[11]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "32", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[11]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h4 = Heatmap(mat_list[[12]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "64", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[12]][i,j]), x, y, gp = gpar(fontsize = 10))
             })

ht_list = h1 + h2 + h3 +h4
draw(ht_list, row_title = "N(0.83,0.5)")
dev.off()
#adaptive_sigma

file_path = file.path("figures/coverage_map4.png")
png(file_path, width = 10, height = 5, units = "in", res = 300)
h1 = Heatmap(mat_list[[13]], cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun, column_title = "8", heatmap_legend_param = list(title = "Coverage"),
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[13]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h2 = Heatmap(mat_list[[14]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "16", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[14]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h3 = Heatmap(mat_list[[15]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "32", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[15]][i,j]), x, y, gp = gpar(fontsize = 10))
             })
h4 = Heatmap(mat_list[[16]], cluster_rows = FALSE, cluster_columns = FALSE,  col = col_fun, column_title = "64", show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.1f", mat_list[[16]][i,j]), x, y, gp = gpar(fontsize = 10))
             })

ht_list = h1 + h2 + h3 +h4
draw(ht_list, row_title = "N(0,0.25)")
dev.off()

################################################################################
#MSE based on urn size and adaptivity
################################################################################
usXadaptive = sqe_output_1jump %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, adapt, starts_with('iter'))

usXadapt_totalMSE = usXadaptive[,1:3]
usXadapt_totalMSE[,3] = rowMeans(usXadaptive[, 253:ncol(usXadaptive)])

for(i in 1:nrow(usXadapt_totalMSE)) {
  usXadapt_totalMSE[i,3] = sd(usXadaptive[i,253:ncol(usXadaptive)])
}

usXadaptive = sqe_output_linear %>%
  group_by(player_urn_size, adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, adapt, starts_with('iter'))

usXadapt_totalMSE = usXadaptive[,1:3]
usXadapt_totalMSE[,3] = rowMeans(usXadaptive[, 3:ncol(usXadaptive)])

for(i in 1:nrow(usXadapt_totalMSE)) {
  usXadapt_totalMSE[i,4] = sd(usXadaptive[i,3:ncol(usXadaptive)])
}

usXadaptive = sqe_output_1jump %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, starts_with('iter'))

usXadapt_totalMSE = usXadaptive[,1:2]
usXadapt_totalMSE[,2] = rowMeans(usXadaptive[, 253:ncol(usXadaptive)])

for(i in 1:nrow(usXadapt_totalMSE)) {
  usXadapt_totalMSE[i,3] = sd(usXadaptive[i,3:ncol(usXadaptive)])
}

usXadaptive = sqe_output_1jump %>%
  group_by(adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt, starts_with('iter'))

usXadapt_totalMSE = usXadaptive[,1:2]
usXadapt_totalMSE[,2] = rowMeans(usXadaptive[, 253:ncol(usXadaptive)])

for(i in 1:nrow(usXadapt_totalMSE)) {
  usXadapt_totalMSE[i,3] = sd(usXadaptive[i,3:ncol(usXadaptive)])
}

usXadaptive = sqe_output_linear %>%
  group_by(player_urn_size) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(player_urn_size, starts_with('iter'))

usXadapt_totalMSE = usXadaptive[,1:2]
usXadapt_totalMSE[,2] = rowMeans(usXadaptive[, 3:ncol(usXadaptive)])

for(i in 1:nrow(usXadapt_totalMSE)) {
  usXadapt_totalMSE[i,3] = sd(usXadaptive[i,3:ncol(usXadaptive)])
}

usXadaptive = sqe_output_linear %>%
  group_by(adapt) %>%
  summarise(across(starts_with("iter"), ~ mean(.))) %>%
  select(adapt, starts_with('iter'))

usXadapt_totalMSE = usXadaptive[,1:2]
usXadapt_totalMSE[,2] = rowMeans(usXadaptive[, 3:ncol(usXadaptive)])

for(i in 1:nrow(usXadapt_totalMSE)) {
  usXadapt_totalMSE[i,3] = sd(usXadaptive[i,3:ncol(usXadaptive)])
}
