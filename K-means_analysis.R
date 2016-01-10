
data <- cbind(london_Cmi@data$C_mi, london_Cmi@data$areas)
data <- scale(data)
wssplot <- function(data, nc=15, seed=1234){
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc){
        set.seed(seed)
        wss[i] <- sum(kmeans(data, centers=i)$withinss)}
    plot(1:nc, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")}

test <- kmeans(data, centers = 10)
london_Cmi@data$Cluster <- test$cluster
plot_spdf_byClassInt(london_Cmi, plot_variable = "Cluster", n_breaks = 5, style_intervals = "equal")


