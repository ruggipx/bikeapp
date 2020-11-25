gen_km_plot <- function(centers, by_n){
  df = data.frame(time = character(), prob = double(), cluster = character())
  k = ncol(centers)-1
  cluster_names = colnames(centers)
  for (i in 1:k){
    temp = data.frame(time = 1:(288/by_n), prob = centers[,i], cluster = cluster_names[i])
    df = rbind(df, temp)
  }
  ggplot(df, aes(x = time,y = prob, color = cluster, group = cluster)) +
    geom_line() +
    geom_point(size = 1)+
    scale_x_continuous(breaks = seq(1,(288/by_n), 12/by_n), labels = c(0:23)) +
    labs(x = "Hour",
         y = "Probability",
         title = "Probability of Getting a Bike per Hour by Station Group") +
    scale_color_manual(name="Station Group", labels = c("1", "2", "3", "4", "5"), values = brewer.pal(5, "Dark2")) +
    theme(text = element_text(size = 20),
          plot.title = element_text(hjust = 0.5), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
}