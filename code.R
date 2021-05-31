library(jpeg) # to read jpeg RGB array
library(glcm) # to compute the "GLCM" matrices
library(raster) # to use raster format
library(narray) # manipulate arrays in R 

# part A. Read and format the old and new images. 



#1. convert the jpegs to a 287 by 512 pixels by 3 bands (RGB) array
new_array_RGB <- readJPEG("~/Documents/job_applications/data_scientist_gpl/data_task/Fig2_2019.jpeg") 
old_array_RGB <- readJPEG("~/Documents/job_applications/data_scientist_gpl/data_task/Fig1_1939.jpeg")
dims <- c(287, 512) # number of pixels for later use



#2. convert to panchromatic using 'Luma' linear combination (.3*Red +.59*Green +.11*Blue) for comparability
#converting the new photo
new_raster_pan <- raster((.3*new_array_RGB[,,1] +.59*new_array_RGB[,,2] +.11*new_array_RGB[,,3])*255)
#the old picture is already nearly panchromatic, but not entirely
old_raster_pan <- raster((.3*old_array_RGB[,,1] +.59*old_array_RGB[,,2] +.11*old_array_RGB[,,3])*255)




# part B. Texture analysis


#using contrast as it's most relevant in our case
#set the parameters 
metrics <- c("contrast")
#using all directions co-occurrences (the algorithm eventually takes the mean across the directions)
directions <- list(c(0,1), c(1,1), c(1,0), c(1,-1))
#replacing the NA values (edge of photo) by zero to be able to handle the matrix later
replace_edge <- 0
# using 7 by 7 windows 
wdo <- c(7,7)
# number of different tones 
greys <- 255 
new_rglcm <- glcm(new_raster_pan, 
              n_grey=greys,
              window = wdo, 
              shift = directions, 
              statistics = metrics,
              na_val=replace_edge)
old_rglcm <- glcm(old_raster_pan, 
                  n_grey=greys,
                  window = wdo, 
                  shift = directions, 
                  statistics = metrics,
                  na_val=replace_edge)
# plot the rasters to look at them
# plot(old_rglcm)
# plot(new_rglcm)




# part C. Use the contrast texture maps and implement simple k-mean clustering unsupervised classification



#1. format the data
n <- raster:::as.array(new_rglcm)[,,1]
o <- raster:::as.array(old_rglcm)[,,1]
n <- data.frame(
  x = base::rep(1:dims[2], each = dims[1]),
  y = base::rep(dims[1]:1, dims[2]),
  tone = as.vector(n)
)
o <- data.frame(
  x = base::rep(1:dims[2], each = dims[1]),
  y = base::rep(dims[1]:1, dims[2]),
  tone = as.vector(o)
)
contrasts <- list()
contrasts[['old']] <- o
contrasts[['new']] <- n



#2.Implement k-mean clustering with k=2 for classification into agricultural or urban category
k <- 2
#new
kMeans <- kmeans(contrasts[['new']], centers=k)
colors <- kMeans$centers
contrasts[['new']]$color <- factor(colors[kMeans$cluster,][,3])
#old
kMeans <- kmeans(contrasts[['old']], centers=k)
colors <- kMeans$centers
contrasts[['old']]$color <- factor(colors[kMeans$cluster,][,3])



#3.Plot classplotting the clustered colours :
ggplot(data = contrasts[['old']]) +
  geom_point(aes(x = x, y = y, color=color)) +
  labs(title = paste("classified 1939 photograph")) +
  xlab("x") +
  ylab("y") +
  plotTheme() +
  scale_color_discrete(name='classification', labels=c('agricultural','urban residential and roads'))
ggplot(data = contrasts[['new']]) +
  geom_point(aes(x = x, y = y, color=color)) +
  labs(title = paste("classified 2019 photograph")) +
  xlab("x") +
  ylab("y") +
  plotTheme() +
  scale_color_discrete(name='classification', labels=c('agricultural','urban residential and roads'))



