library(stats)

### Code for RQ2

# Turn off scientific notation so it does not appear on graphs
options(scipen = 999)

# Create table 3.5: industry summary - size and growth for each
table_5 <- emp_long %>%
  filter(Area == 'Yorks and Humber', Year == 2022, Industry != 'Area Total') 


### Principal Component Analysis (PCA)
# uses ind_09 (database two)

# Scaled industry heatmap
heatmap(as.matrix(scale(ind_09)), margins = c(7,7), # margins widened so column names can be read
        main = ' Figure 7: Heatmap of Industry Size by Area') 

## Run PCA
pca_result <- prcomp(ind_09, scale. = TRUE) # run PCA
summary(pca_result) # View results

# Make data frame for scree plot
scree_data <- data.frame(
  Component = 1:length(pca_result$sdev), # column of component numbers
  Variance = pca_result$sdev^2 / sum(pca_result$sdev^2)) # calculate variance explained

# Make Scree plot
ggplot(scree_data, aes(x = Component, y = Variance)) +
  geom_bar(stat = "identity") +
  geom_line() +
  geom_point() +
  labs(title = 'Figure 3.6: Scree Plot for Industry PCA', x = 'Principal Component', 
       y = 'Proportion of Variance Explained')

# Create df of PCA vectors
pca_vectors <- as.data.frame(pca_result$x)
pca_vectors <- pca_vectors %>% 
  mutate(Area = rownames(ind_09)) %>% # add Area column
  arrange(Area) # order alphabetically

# Create Df of PCA loadings
pca_loadings <- as.data.frame((pca_result$rotation))
pca_loadings <- pca_loadings %>% # add industry column for analysis
  mutate(Industry = rownames(pca_loadings))

# PC1 loadings Bar Chart
pca_loadings %>%
  ggplot(aes(x = reorder(rownames(pca_loadings), PC1), y = PC1)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = 'Figure 3.7: Loadings for Principal Component 1', x = 'Industry',
       y = 'Principal Component 1')



