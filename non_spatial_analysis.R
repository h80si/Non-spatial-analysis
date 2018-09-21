#install.packages("devtools")
library("devtools")
#install_github("kassambara/factoextra") 
library("factoextra")
#install.packages("corrplot")
library("corrplot")
#install.packages(ggplot2)
library(ggplot2)
#install.packages("NMF")
library(NMF)
#install.packages("NMI")
library(NMI)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("caret")
library(caret)
#install.packages("rattle")
library(rattle)
#install.packages("rpart")
library(rpart.plot)
library(rpart)
library(e1071)
library(gdata)

#-------------------------------------------------------------------------------------------------------------------------------#
# SET UP DIRECTORIES AND DATA

# Output directory
outputDir <- c("C:\\Users\\Heidi\\Documents\\Information architecture project\\data analysis\\zur_360_upd_subj\\")


# read data for both experiments (Zürich and Weimar)
inputFile <- "C:\\Users\\Heidi\\Documents\\Information architecture project\\data analysis\\zur_360_updated.csv"
otherFile <- "C:\\Users\\Heidi\\Documents\\Information architecture project\\data analysis\\Wei_360_updated.csv" 
mydata <- read.csv(inputFile, header = TRUE)
other_dataset <- read.csv(otherFile, header= TRUE)

curTitle <- c("Zurich_360_updated") # This title will be used in the names of figures


# SELECT CURRENT Y COLUMN (x-columns are always the same)

cur_y <- "subjective"

xList <- c('Area', 'Perimeter', 'Compactness', 'Occlusivity', 'Min_radial','Volume','RayPortion_sky','RayPortion_obstacles', 'Betweenness_car', 'Betweenness_ped')
curCols <- append(xList, cur_y)

curData <- subset(mydata, select = curCols)
xdata_scaled = data.frame(scale(curData[xList]))
curData_scaled <- cbind(xdata_scaled, curData[cur_y])
xdata_unscaled <- subset(mydata, select = xList)
curScores <- factor(curData[,cur_y]) # a factor containing objective or subjective scores
#-------------------------------------------------------------------------------------------------------------------------------#

# FUNCTIONS

#PAIRS PLOT, raw data. 
# all raw data variables plotted against each other with data points colored according to the current y variable values
draw_raw_pairs = function(outDir, fileName, dataName, scoresFactored, scoreType) {
  
  filePath <- paste(c(outDir, fileName), collapse='')
  png(filePath, width = 3000, height = 3000, pointsize = 60)
  plotTitle <- paste(c("Matrix plot of raw data variables, ", dataName), collapse="")
  
  pairplot_raw = pairs(x = curData, 
                       main = plotTitle,
                       col = scoresFactored, # Color data points according to objective / subjective score
                       oma=c(4,4,5,14), 
                       upper.panel = NULL, pch = 16, cex = 0.6, 
                       cex.labels=0.6)
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  
  if(scoreType == "objective_discrete") {
    legend("topright", legend = c("0","1","2", "3"), xpd = TRUE, inset = c(0,0), bty = "n", pch = 16, col = 1:4, cex = 1, title = scoreType)
  } else if (scoreType == "subjective") {
    legend("topright", legend = unique(scoresFactored), xpd = TRUE, inset = c(0,0), bty = "n", pch = 16, col = 1:4, cex = 1, title = scoreType)
  }
  
  print(pairplot_raw)
  dev.off()  
  
}

# REGRESSION PLOT
draw_regression_plot = function(data, x_axis_var, y_axis_var, outDir) {
  # data - dataset for drawing the plot
  # x_axis_var / y_axis_var - x-axis variable or y-axis variable
  # outDir - directory where plot will be saved
  
  filePath <- paste(c(outputDir, "regression_" , x_axis_var, "_", y_axis_var, ".png"), collapse='')
  png(filePath, width = 900, height = 600, pointsize = 60)
  regressionplot = ggscatter(data, x = x_axis_var, y = y_axis_var,
                             add = "reg.line", conf.int = TRUE, 
                             cor.coef = TRUE, cor.method = "pearson",
                             cor.coef.size = 7,
                             xlab = x_axis_var, ylab = y_axis_var) +
    font("xlab", size = 18) +
    font("ylab", size = 18) +
    font("xy.text", size = 18) +
    font("subtitle", size = 18)
  
  print(regressionplot)
  dev.off()  
  
}

# ELBOW CURVE

draw_elbow = function(pca, outDir, dataName) {
  # This function draws an elbow curve (used in a pca analysis)
  # pca - Principal component analysis object
  # outDir - directory where the plot will be saved
  # dataName (string) - name of the current dataset ("Zurich_360" or "Weimar_360" ) 
  
  filePath <- paste(c(outDir, "elbow.png"), collapse='')
  png(filePath, width = 2000, height = 1500)
  
  plotTitle <- paste(c("Elbow curve,", curTitle), collapse=" ")
  elbow <- fviz_eig(pca, addLabels=TRUE, labelsize = 6, main = plotTitle) +
    theme(text = element_text(size = 40),
          axis.title.x = element_text(size = 30, margin = unit(c(10, 0, 0, 0), "mm"), angle = 0),
          axis.title.y = element_text(size = 30, margin = unit(c(0, 10, 0, 0), "mm"), angle = 90),
          axis.text = element_text(size = 30))
  print(elbow)
  dev.off()
  }



# PC PAIRS PLOT
# n principal components plotted against each other with data points colored according to the current y variable values
draw_pc_pairs = function(pca, n_components, scores_factored, score_type, outDir, dataName){
  # pca  - PCA object
  # n_components - number of PCs to include in the plot
  # scores_factored - a factor object of objective / subjective scores 
  # score_type - string object to be used in the legend title, e.g. "objective" or "subjective"
  # outDir (string) - directory where the plot will be saved
  # dataName (string) - name of the current dataset ("Zurich_360" or "Weimar_360" ) 
  
  filePath <- paste(c(outDir, "pc_pairs.png"), collapse='')
  plotTitle <- paste(c("Matrix plot of principal components, ", dataName), collapse="")
  png(filePath, width = 3000, height = 3000, pointsize = 60)
  

  plot = pairs(x = pca$x[,1:n_components],
           main = plotTitle,
           col = scores_factored, oma=c(4,4,5,14), # color each data point according to the objective/subjective score
           upper.panel = NULL, pch = 16, cex = 0.6, 
           cex.labels=1.2)
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  
  legend("topright", legend = c("0","1","2", "3"), xpd = TRUE, inset = c(0,0), bty = "n", pch = 16, col = 1:4, cex = 1, title = score_type)
  
  print(plot)
  dev.off()
}



# CONTRIBUTIONS PLOT & TABLE
draw_contributions = function(pca, outDir, dataName) {
  # pca  - PCA object
  # outDir (string) - directory where the plot will be saved
  # dataName (string) - name of the current dataset ("Zurich_360" or "Weimar_360" ) 
  
  filePath <- paste(c(outDir, "contributions_plot.png"), collapse='')
  png(filePath, width = 1000, height = 1000, pointsize = 30)
  contribPlot = fviz_pca_var(pca,labelsize = 8,
                             col.var = "contrib", # Color by contributions to the PC
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                             repel = TRUE)+     # Avoid text overlapping) +
    theme(text = element_text(size = 30),
          axis.title.x = element_text(size = 25, margin = unit(c(10, 0, 0, 0), "mm"), angle = 0),
          axis.title.y = element_text(size = 25, margin = unit(c(0, 10, 0, 0), "mm"), angle = 90),
          axis.text = element_text(size = 25))
  print(contribPlot)
  dev.off() 
}


create_contribs_table = function(pca) {
  loadings <- pca$rotation
  contribs <- sweep(x = abs(loadings), MARGIN = 2, 
                    STATS = colSums(abs(loadings)), FUN = "/")*100
  contribs
  
}


# CORRELATION PLOT
draw_corr_plot = function(pca, outDir, dataName){
  # pca  - PCA object
  # outDir (string) - directory where the plot will be saved
  # dataName (string) - name of the current dataset ("Zurich_360" or "Weimar_360" ) 

  filePath <- paste(c(outDir, "correlation_plot.png"), collapse='')
  png(filePath, width = 1000, height = 1000, pointsize = 20)
  
  var <- get_pca_var(pca)
  corrplot2 = corrplot(var$cos2, 
                       title = paste(c("Correlation plot", dataName), collapse=", "), 
                       tl.cex=1, tl.col = "black", cl.cex = 1, is.corr = FALSE,
                       mar=c(0,0,5,0))
  print(corrplot2)
  dev.off()  
  
}

# K-MEANS PAIRS PLOT
draw_kmeans_pairs = function(data, clust_factored, data_description, n_clusters, outDir, dataName) {
  # data (dataframe)  - data to be used
  # clust_factored (factor) - factor containing the cluster which each data point belongs to
  # data_description (str) - brief description of which variables are used in the clustering (used in the plot title)
  # n_clusters (int) - number of clusters
  # outDir (string) - directory where the plot will be saved
  # dataName (string) - name of the current dataset ("Zurich_360" or "Weimar_360" ) 
  
  filePath <- paste(c(outputDir, "kmeans_", data_description, ".png"), collapse='')
  png(filePath, width = 2000, height = 1500, pointsize = 30)
  
  
  plotTitle = paste(c("K-means clustering on ", data_description,  ", k = ", n_clusters, ", ", dataName), collapse="")
  
  kmeans_pairs = plot(x= data, 
                      main = plotTitle,
                      col=clust_factored, #color data points according to clusters
                      oma = c(3,3,5,15),
                      pch=16)
  par(xpd = TRUE)
  legend("topright", legend=levels(clust_factored),col = 1:4, pch=16)
  
  print(kmeans_pairs)
  dev.off()
  
}


# SINGLE K-MEANS PLOT WITH DATA POINTS' COLOR DETERMINED BY CLUSTERS AND SHAPE DETERMINED BY SCORES
draw_single_kmeans = function(x, y, data, clust_factored, scores_factored, data_description, n_clusters, outDir, dataName){
  # x, y (str) - x and y-axis variables
  # data (dataframe)  - data to be used
  # clust_factored (factor) - factor containing the cluster which each data point belongs to
  # data_description (str) - brief description of which variables are used in the clustering (used in the plot title)
  # n_clusters (int) - number of clusters
  # outDir (string) - directory where the plot will be saved
  # dataName (string) - name of the current dataset ("Zurich_360" or "Weimar_360" ) 
  
  filePath <- paste(c(outputDir, "kmeans_single_", data_description, ".png"), collapse='')
  plotTitle = paste(c(y, " vs ", x, ". K-means clustering on ", data_description, ", k=", n_clusters, ", ", dataName), collapse="")
  xdata <- data[, x]
  ydata <- data[, y]
  png(filePath, width = 2000, height = 1500, pointsize = 60)
  kmeans_plot <- ggplot(data, aes(x = xdata, y = ydata, shape = scores_factored, color = clust_factored)) + 
    labs(x=x, y =  y) +
    ggtitle(plotTitle) + 
    geom_point(size=6) +
    theme(text = element_text(size = 30, margin = unit(c(0, 0, 0, 20), "mm")),
          axis.title.x = element_text(size = 30, margin = unit(c(10, 0, 0, 0), "mm"), angle = 0),
          axis.title.y = element_text(size = 30, margin = unit(c(0, 10, 0, 0), "mm"), angle = 90),
          axis.text = element_text(size = 25))
  
  
  
  print(kmeans_plot)
  dev.off()
  
}


# Calculate frequency of objective / subjective scores in each cluster
count_scores_in_clusters = function(clust_factored, scores_factored){
  # clust_factored (factor) - factor containing the cluster which each data point belongs to
  # scores_factored (factor) - factor containing the objective / subjective score which each data point has
  
  df_cl_sc <- data.frame(clust_factored, scores_factored) # data frame with clusters and subj./obj. scores as columns
  
  if(length(levels(clust_factored)) == 4){
    cluster1 <- data.frame(dplyr::filter(df_cl_sc, clust_factored == 1))
    cluster2 <- data.frame(dplyr::filter(df_cl_sc, clust_factored == 2))
    cluster3 <- data.frame(dplyr::filter(df_cl_sc, clust_factored == 3))
    cluster4 <- data.frame(dplyr::filter(df_cl_sc, clust_factored == 4))
    
    cluster1_count <- plyr::count(cluster1, 'scores_factored')
    cluster1_mean <- mean(as.numeric(paste(cluster1$scores_factored)))
    
    cluster2_count <- plyr::count(cluster2, 'scores_factored')
    cluster2_mean <- mean(as.numeric(paste(cluster2$scores_factored)))
    
    cluster3_count <- plyr::count(cluster3, 'scores_factored')
    cluster3_mean <- mean(as.numeric(paste(cluster3$scores_factored)))
    
    cluster4_count <- plyr::count(cluster4, 'scores_factored')
    cluster4_mean <- mean(as.numeric(paste(cluster4$scores_factored)))
    
    
    print("cluster 1:")
    print(cluster1_count)
    print("cluster 2:")
    print(cluster2_count)
    print("cluster 3:")
    print(cluster3_count)
    print("cluster 4:")
    print(cluster4_count)
    print(paste(c("cluster 1 mean: ", cluster1_mean), collapse=""))
    print(paste(c("cluster 2 mean: ", cluster2_mean), collapse=""))
    print(paste(c("cluster 3 mean: ", cluster3_mean), collapse=""))
    print(paste(c("cluster 4 mean: ", cluster4_mean), collapse=""))
    
  } else if(length(levels(clust_factored)) == 4) {
    cluster1 <- data.frame(dplyr::filter(df_cl_sc, clust_factored == 1))
    cluster2 <- data.frame(dplyr::filter(df_cl_sc, clust_factored == 2))
    
    cluster1_count <- plyr::count(cluster1, 'scores_factored')
    cluster1_mean <- mean(as.numeric(paste(cluster1$scores_factored)))
    
    cluster2_count <- plyr::count(cluster2, 'scores_factored')
    cluster2_mean <- mean(as.numeric(paste(cluster2$scores_factored)))
    
    print("cluster 1:")
    print(cluster1_count)
    print("cluster 2:")
    print(cluster2_count)
    print(paste(c("cluster 1 mean: ", cluster1_mean), collapse=""))
    print(paste(c("cluster 2 mean: ", cluster2_mean), collapse=""))
  }
}


# Create a test and train dataset
create_test_train_data = function(data, prop_train){
  #data (dataframe) - data to use
  # prop_train (float) - proportion of data which should be used for training
  
  ## 75% of the sample size
  smp_size <- floor(prop_train * nrow(data))
  test_smp_size <- nrow(data) - smp_size
  
  ## set the seed to make your partition reproducible
  set.seed(123) #123
  
  ## Split the data into test and train data
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  
  train_data<- data[train_ind, ]
  print(train_data)
  test_data <- data[-train_ind, ]
  print(test_data)
  return(list(train_data, test_data))
}

# Build decision tree prediction model
build_decision_tree = function(train_data, var_to_predict, prune){
  #train_data (dataframe) - data for training
  # var_to_predict (str)- variable which the decision tree is trained to predict
  # prune (boolean) - whether the decision tree is pruned (TRUE) or not (FALSE)
  print(train_data)
  
  ## Build the tree. https://www.r-bloggers.com/a-quick-introduction-to-machine-learning-in-r-with-caret/
  if(var_to_predict == "objective_discrete"){
    fit1 <-rpart(objective_discrete ~ .,
                 method = "class",
                 data =train_data)
  } else if(var_to_predict == "subjective") {
    fit1 <-rpart(subjective ~ .,
                 method = "class",
                 data =train_data)
  }
  
  if(prune == TRUE){
    pfit<- prune(fit1, cp= fit1$cptable[which.min(fit1$cptable[,"xerror"]),"CP"])  # Prune the tree
    printcp(pfit) # display the results
    return(pfit)
  
  } else {
    printcp(pfit) # display the results
    return(fit1)
  }
}

# print two confusion matrices for the tree (one when tree is applied to test data from same dataset, one when applied to another dataset)
print_conf_matrix = function(fit, test_data_same, test_data_other, var_to_predict){
  # fit (rpart decision tree model object) - decision tree
  # test_data_same (dataframe) - test data from the same dataset which the tree was trained one
  # test_data_other (dataframe) - test data from the other dataset
  # var_to_predict (str) - variable which tree tries to predict, ie "objective_discrete" or "subjective"
  
    # Confusion matrix of the tree when applied to the same data set (e.g. when a tree trained on Zürich data is applied to the Zürich test data)
  pred_same <- predict(fit, test_data_same, type="class")
  confMat_same <- confusionMatrix(pred_same, factor(test_data_same[, var_to_predict]))
  print("Confusion matrix same:")
  print(confMat_same)
  
  # Confusion matrix of the tree when applied to the other data set (e.g. when a tree trained on Zürich data is applied to the Weimar test data)
  pred_other <- predict(fit, test_data_other, type = "class")
  confMat_other <- confusionMatrix(factor(pred_other), factor(test_data_other[,var_to_predict]))
  print("Confusion matrix other:")
  print(confMat_other)
}


# Plot the decision tree
plot_tree = function(fit, var_to_predict, outDir, data_description, dataName){
  # fit (rpart decision tree object)
  # var_to_predict (str) - variable which tree tries to predict, ie "objective_discrete" or "subjective"
  # outDir (str) - directory where the plot will be saved
  # data_description (str) - brief description of contents of data, to be used in plot title
  # dataName (str) - name of original dataset, e.g. Weimar_360 or Zurich_360, to be used in plot title
  
  # Create an image file for displaying the tree
  fileName <- paste(c(outDir, "00decision_tree_", data_description, ".png"), collapse='')
  png(fileName, width = 900, height = 600)
  
  
  plotName <- paste(c("Decision tree with ", data_description, ", ", var_to_predict, ", ", dataName), collapse = "")
  
  # Plot the tree
  treeplot <- rpart.plot(fit,
                         main= plotName, 
                         cex.main=1.2,
                         type = 1,
                         extra=101,
                         box.palette = "GnBu",
                         branch.lty =3,
                         shadow.col = "gray",
                         nn = TRUE)
  print(treeplot)
  print(summary(fit)) # detailed summary of splits
  dev.off()
}

#----------------------------------------------------------------------------------------
# CALL FUNCTIONS

# Pairs plot and regression
draw_raw_pairs(outputDir, "pairs_plot_raw_data.png", curTitle, curScores, cur_y)
draw_regression_plot(curData, cur_y, "RayPortion_obstacles", outputDir)

# PCA
myPCA <- prcomp(curData[,1:10], center = T, scale = T)
summary(myPCA)
get_eigenvalue(X=myPCA) #http://www.sthda.com/english/wiki/eigenvalues-quick-data-visualization-with-factoextra-r-software-and-data-mining
draw_elbow(myPCA, outputDir, curTitle)
draw_contributions(myPCA, outputDir, curTitle)
create_contribs_table(myPCA)
draw_corr_plot(myPCA, outputDir, curTitle)
draw_pc_pairs(myPCA, 4, curScores, cur_y, outputDir, curTitle)


set.seed(20)
# The following k-means clustering code for different data sets creates, for each set,:
###### a pairs plot where data poitns are colored according to clusters
###### a single plot with 2 variables where points' colors represent clusters and shapes represent objective scores
###### entropy and purity calculation (printed in the console)
###### a count of score frequencies and means in each cluster (printed in the console

# k-means clustering for all scaled variables. 
raw_data_scaled <- xdata_scaled[,xList] 
raw_clusters <- kmeans(raw_data_scaled, centers=4, iter.max = 100, nstart = 25)
raw_clusters_factored <- as.factor(raw_clusters$cluster)

draw_kmeans_pairs(raw_data_scaled, raw_clusters_factored, "all scaled raw data", 4, outputDir, curTitle)
draw_single_kmeans(x= "Betweenness_car", y="Betweenness_ped", raw_data_scaled, raw_clusters_factored, curScores, "all scaled raw data", 4, outputDir, curTitle)
print(paste(c("entropy: ", entropy(x = raw_clusters_factored, y = curScores, method = "best")), collapse ="")) # y = true class labels
print(paste(c("purity: ", purity(x = raw_clusters_factored, y = curScores)), collapse = ""))
count_scores_in_clusters(raw_clusters_factored, curScores)

# k-means clustering for selected variables
selected_var <- c("Betweenness_ped", "Min_radial","Occlusivity", "Volume")
raw_data_selected <- curData_scaled[,selected_var]
sel_clusters <- kmeans(raw_data_selected, centers=4, iter.max = 100, nstart = 25)
sel_clusters_factored <- as.factor(sel_clusters$cluster)
draw_kmeans_pairs(raw_data_selected, sel_clusters_factored, "4 select variables", 4, outputDir, curTitle)
draw_single_kmeans(x= "Min_radial", y= "Betweenness_ped", raw_data_selected, sel_clusters_factored, curScores, "4 select variables", 4, outputDir, curTitle)
print(paste(c("entropy: ", entropy(x = sel_clusters_factored, y = curScores, method = "best")), collapse ="")) # y = true class labels
print(paste(c("purity: ", purity(x = sel_clusters_factored, y = curScores)), collapse = ""))
count_scores_in_clusters(sel_clusters_factored, curScores)

# k-means clustering for selected principal components
pc_list <- c(1,2)
cur_pc_data <- data.frame(myPCA$x[,pc_list])
pc_clusters <- kmeans(cur_pc_data, centers=4, iter.max = 100, nstart = 25)
pc_clusters_factored <- as.factor(pc_clusters$cluster)
draw_kmeans_pairs(cur_pc_data, pc_clusters_factored, "PC1-2", 4, outputDir, curTitle)
draw_single_kmeans(x= "PC1", y= "PC2", cur_pc_data, pc_clusters_factored, curScores, "PC1-2", 4, outputDir, curTitle)
print(paste(c("entropy: ", entropy(x = pc_clusters_factored, y = curScores, method = "best")), collapse ="")) # y = true class labels
print(paste(c("purity: ", purity(x = pc_clusters_factored, y = curScores)), collapse = ""))
count_scores_in_clusters(pc_clusters_factored, curScores)

tree_cols <- c("Betweenness_ped", "Min_radial","Occlusivity", "Volume", "RayPortion_sky", "Area", cur_y)
tree_data <- curData[, tree_cols]
train_test_data <- create_test_train_data(tree_data, 0.75)
train <- data.frame(train_test_data[1])
test <- data.frame(train_test_data[2])

other_data <- other_dataset[, tree_cols]

fit = build_decision_tree(train, cur_y, prune = TRUE)
print_conf_matrix(fit, test, other_data, cur_y)

plot_tree(fit, cur_y, outputDir, "six select variables", curTitle)

#---------------------------------------------------------



