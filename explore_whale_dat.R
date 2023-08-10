library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(wesanderson)
library(factoextra) # fviz_cluster

pal = wes_palette("Darjeeling1")
df <- read.csv('data/whales_rings_chl_tmp.csv')
df<- df %>% 
  mutate(date = lubridate::mdy(date),
         year = lubridate::year(date), 
         week = lubridate::week(date))

df$prop.streamering <- (df$num_ring_with_streamer)/(df$num_ring)

#df.21.23 <- df %>% filter(year %in% c(2021:2023))
#df.21.23$prop.streamering <- (df.21.23$num_ring_with_streamer)/(df.21.23$num_ring)

df_tally <- df %>%
  group_by(year) %>%
  summarise(ttl_right = sum(na.omit(right_whale_sight)), 
            ttl_other = sum(na.omit(other_whale_sight)), 
            ttl_unidentified = sum(na.omit(unidentified_whale_sight)),
            ttl_streamer_ring = sum(na.omit(num_ring_with_streamer)), 
            ttl_ring = sum(na.omit(num_ring)))
df_tally$prop_stream_ring <- df_tally$ttl_streamer_ring/df_tally$ttl_ring
df_tally$ttl_whale <- rowSums(df_tally[,c(2:4)])

tally.long <- df_tally %>%
  pivot_longer(ttl_right:ttl_whale, names_to = 'whales', values_to = 'count')

g = ggplot() +
  geom_col(data = tally.long %>% 
             filter(whales %in% c('ttl_right','ttl_other','ttl_unidentified')),
           aes(x = factor(year), y = count, fill = whales), 
           size = 1, color = 'black',
           stat='identity', position='dodge') +
  scale_fill_manual(values = pal,
                    name = 'Species',
                    labels = c('Right Whale',
                               'Other Large Whale',
                               'Unidentified Large Whale')) +
  geom_line(data = tally.long %>% filter(whales =='ttl_whale'), 
                                  aes(x = factor(year), y = count), size = 1.5, 
            color = 'black', group = 1) +
  xlab('Year') +
  ylab('Number of Whale Sightings (n)') +
  #labs(title = 'Offshore Flight Data Summary')+
  theme_classic()  
g +  theme(text = element_text(size = 14), 
         axis.text = element_text(size = 13), 
         axis.title = element_text(size = 15),
         legend.position = c(0.25, 0.85),
         legend.background = element_rect(fill = "white", color = "black"), 
         legend.title = element_text(size = 11),
         legend.text = element_text(size = 11))
### Plot number of rings with streamer 
g2 = ggplot() +
  # geom_rect(aes(xmin = '2014', xmax = '2014', ymin = 0, ymax = 100),
  #           fill = 'lightgrey',color = 'grey95', alpha = 0.8) +
  # geom_rect(aes(xmin = '2016', xmax = '2017', ymin = 0, ymax = 100),
  #           fill = 'lightgrey', color = 'grey95', alpha = 0.8) +
  # geom_rect(aes(xmin = '2019', xmax = '2023', ymin = 0, ymax = 100),
  #           fill = 'lightgrey', color = 'grey95', alpha = 0.8) +
  geom_col(data = tally.long %>%
             filter(whales %in% c('ttl_right','ttl_other','ttl_unidentified')),
           aes(x = factor(year), y = count, fill = whales),
           size = 1, color = 'black',
           stat='identity', position='dodge') +
  scale_fill_manual(values = pal,
                    name = 'Species',
                    labels = c('Right Whale',
                               'Other Large Whale',
                               'Unidentified Large Whale')) +
  geom_line(data = tally.long %>% filter(whales =='ttl_streamer_ring'), 
            aes(x = factor(year), y = count), size = 1.5, 
            color = 'black', group = 1) +
  geom_point(data = tally.long %>% filter(whales =='ttl_streamer_ring' & year %in% c(2014, 2016, 2017, 2019, 2020, 2021, 2022,2023 )), 
             aes(x = factor(year), y = count), bg = 'yellow', size = 3, pch = 21,
             color = 'black', group = 1) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = 'Catch (lbs)')) + 
  xlab('Year') +
  ylab('Number of Whale Sightings / Number of Rings w/Streamer') +
  #labs(title = 'Offshore Flight Data Summary')+
  theme_classic()  
g2 +  theme(text = element_text(size = 14), 
           axis.text = element_text(size = 13), 
           axis.title = element_text(size = 15),
           legend.position = c(0.25, 0.85),
           legend.background = element_rect(fill = "white", color = "black"), 
           legend.title = element_text(size = 11),
           legend.text = element_text(size = 11))



ggplot() +
  geom_col(data = tally.long %>% 
             filter(whales %in% c('ttl_right','ttl_other','ttl_unidentified')),
           aes(x = factor(year), y = count, fill = whales), 
           size = 1, color = 'black',
           stat='identity', position='dodge') +
  scale_fill_manual(values = pal,
                    name = 'Species',
                    labels = c('Right Whale',
                               'Other Large Whale',
                               'Unidentified Large Whale')) +
  geom_line(data = tally.long %>% filter(whales =='prop_stream_ring'), 
            aes(x = factor(year), y = count*100), size = 1.5, 
            color = 'black', group = 1) +
  geom_point(data = tally.long %>% filter(whales =='prop_stream_ring' & year %in% c(2014, 2016, 2017, 2019, 2020, 2021, 2022,2023 )), 
             aes(x = factor(year), y = count*100), bg = 'yellow', size = 3, pch = 21,
             color = 'black', group = 1) +
  scale_y_continuous(sec.axis = sec_axis(~., name = 'Proportion of rings w/streamer')) + 
  xlab('Year') +
  ylab('Number of Whale Sightings / Number of Rings w/Streamer') +
  #labs(title = 'Offshore Flight Data Summary')+
  theme_classic()  
### K Means

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

# Combine the selected variables into a new data frame

# testing code
# df.21.23[,5] <- sample(1:7, 37, replace=TRUE)



#selected.vars <- df.21.23[, c('right_whale_sight', 'prop.streamering')]
selected.vars <- df.21.23[, c('right_whale_sight', 'num_ring_with_streamer')]
selected.vars <- na.omit(selected.vars)
n.clusters <- 3
clusters <- kmeans(selected.vars, n.clusters, nstart = 25)

fviz_cluster(clusters, data = selected.vars)

par(mar = c(5.1, 4.1, 0, 1))
plot(selected.vars,
     col = 1:n.clusters,
     pch = 20, cex = 3, 
     xlab = 'Right whale sightings', 
     ylab = 'Number of rings w/streamer')
points(clusters$centers, pch = 4, cex = 4, lwd = 4)
k2 <- kmeans(selected.vars, 2, nstart = 25)
k3 <- kmeans(selected.vars, 3, nstart = 25)
k4 <- kmeans(selected.vars, 4, nstart = 25)
k5 <- kmeans(selected.vars, 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = selected.vars) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = selected.vars) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = selected.vars) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = selected.vars) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# optimal number of clusters:   
fviz_nbclust(selected.vars, kmeans, method = "silhouette")
library(cluster)
set.seed(123)
gap_stat <- clusGap(selected.vars, FUN = kmeans, nstart = 25,
                    K.max = 5, B = 50)
fviz_gap_stat(gap_stat)
print(k2)
fviz_cluster(final, data = df)



selected.vars3 <- df.21.23[, c('right_whale_sight', 
                              'num_ring_with_streamer',
                              'num_ring', 'prop.streamering')]

selected.vars3 <- na.omit(selected.vars3)
k3 <- kmeans(selected.vars3, 3, nstart = 25)
fviz_cluster(k3, geom = "point",  data = selected.vars) + ggtitle("k = 3")

k3['Cluster']
# High med low
selected.vars3$prop <- case_when(
  selected.vars3$prop.streamering >= .6 ~ 'high',
  selected.vars3$prop.streamering >= .3 & selected.vars3$prop.streamering < .6 ~ 'mod',
  selected.vars3$prop.streamering <= .3 ~ 'low'
)

# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(selected.vars3[, -c(3,4,5)]), 3, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster
# Dimension reduction using PCA
res.pca <- prcomp(selected.vars3[,-c(4,5)],  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Species groups from the original data sett
ind.coord$prop <- selected.vars3$prop
# Data inspection
head(ind.coord)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)
library(ggpubr)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = 'prop', size = 3,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)

### Just a two cluster : Right whales 
# optimal number of clusters:   
fviz_nbclust(selected.vars3[,-c(3,4,5)], kmeans, method = "silhouette")

# High low
selected.vars3$prop <- case_when(
  selected.vars3$prop.streamering >= .5 ~ 'high',
  selected.vars3$prop.streamering < .5 ~ 'low'
)
# High med low
selected.vars3$prop <- case_when(
  selected.vars3$prop.streamering >= .6 ~ 'high',
  selected.vars3$prop.streamering >= .3 & selected.vars3$prop.streamering < .6 ~ 'mod',
  selected.vars3$prop.streamering <= .3 ~ 'low'
)
# Compute k-means with k = 2
set.seed(123)
res.km <- kmeans(scale(selected.vars3[, -c(3,4,5)]), 2, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster
# Dimension reduction using PCA
res.pca <- prcomp(selected.vars3[,-c(4,5)],  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Species groups from the original data sett
ind.coord$prop <- selected.vars3$prop
# Data inspection
head(ind.coord)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = 'prop', size = 3,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)



### Cluster : Other whales 
selected.vars.ow <- df.21.23[, c('other_whale_sight', 
                               'num_ring_with_streamer',
                               'num_ring', 'prop.streamering')]
selected.vars.ow <- na.omit(selected.vars.ow)
# optimal number of clusters:   
fviz_nbclust(selected.vars.ow[,-c(3,4,5)], kmeans, method = "silhouette")

# High low
selected.vars.ow$prop <- case_when(
  selected.vars.ow$prop.streamering >= .5 ~ 'high',
  selected.vars.ow$prop.streamering < .5 ~ 'low'
)
# High med low
selected.vars.ow$prop <- case_when(
  selected.vars.ow$prop.streamering >= .6 ~ 'high',
  selected.vars.ow$prop.streamering >= .3 & selected.vars.ow$prop.streamering < .6 ~ 'mod',
  selected.vars.ow$prop.streamering <= .3 ~ 'low'
)
# Compute k-means with k = 2
set.seed(123)
res.km <- kmeans(scale(selected.vars.ow[, -c(3,4,5)]), 2, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster
# Dimension reduction using PCA
res.pca <- prcomp(selected.vars.ow[,-c(4,5)],  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Species groups from the original data sett
ind.coord$prop <- selected.vars.ow$prop
# Data inspection
head(ind.coord)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = 'prop', size = 3,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)






