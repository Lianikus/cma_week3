library(tidyverse)
library(ggplot2)

caro60 <- read_delim("caro60.csv")
str(caro60)
caro60 <- as_tibble(caro60)
caro60
#temporal window of 6 min, sampling interval at 1 minute --> window size = 6 positions
#--> 6 fixes

#Task 1: Segmentation
#1.) pos[n-3] to pos[n]
#2.) pos[n-2] to pos[n]
#3.) pos[n-1] to pos[n]
#1.) pos[n] to pos[n+1]
#2.) pos[n] to pos[n+2]
#3.) pos[n] to pos[n+3]

#part b)

caro60 <- caro60 %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),   # distance to pos -3 minutes
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -2 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -1 minute
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +1 minute
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2),  # distance to pos +2 minutes
    nPlus3  = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2)  # distance to pos +3 minutes
)

caro60 <- caro60 %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3,nMinus2, nMinus1,nPlus1,nPlus2, nPlus3))
  ) %>%
  ungroup()

#Step c) or Task 2: apply threshold d

summary(caro60)
plot(caro60$stepMean)
boxplot(caro60$stepMean)

caro60 <- caro60 %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))
#mean seems to be a reasonable cutoff with this distribution - everything below might
#be non-movement / lying around

caro60_filter <- caro60 %>%
  filter(!static)

#Task 3: visualize trajectories
#first only movement:
caro60_filter %>%
  ggplot(aes(E, N, colour=static))  +
  geom_path() +
  geom_point() +
  coord_equal() +
  theme(legend.position = "bottom")

#now also with movement below threshold
caro60 %>%
  ggplot(aes(E, N, colour=static))  +
  geom_path() +
  geom_point() +
  coord_equal() +
  theme(legend.position = "bottom")

#function
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}
#segments
caro60 <- caro60 %>%
  mutate(segment_id = rle_id(static))

#visualize all segments
caro60_clean <- caro60 %>%
  group_by(segment_id) %>%
  rowwise() %>%
  filter(stepMean>5) %>%
  ungroup()

par(mfrow=c(2,1))
caro60 %>%
  ggplot(aes(E, N, colour=segment_id))  +
  geom_path() +
  geom_point() +
  coord_equal() +
  theme(legend.position = "bottom") + labs(title="All segments")

caro60_clean %>%
  ggplot(aes(E, N, colour=segment_id))  +
  geom_path() +
  geom_point() +
  coord_equal() +
  theme(legend.position = "bottom") + labs(title = "Segments > 5 Minutes")


#Task 5
pedestrian <- read_delim(as.tibble("pedestrian.csv"))

pedestrian %>%
  ggplot(aes(E,N, colour=TrajID)) +
  geom_path() + 
  geom_point() +
  coord_equal() +
  facet_wrap(vars(TrajID), labeller=label_both) +
  labs(title="Visual comparison of the 6 trajectories")
  
## Colors don't work with all kinds of: scale_fill_distiller(palette="Spectral")

#Task 6
#install.packages("SimilarityMeasures")
library(SimilarityMeasures)

# Create matrix for every trajectory without time
pedestrian.1.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 1))
pedestrian.2.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 2))
pedestrian.3.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 3))
pedestrian.4.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 4))
pedestrian.5.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 5))
pedestrian.6.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 6))

# Calculate the Measures and store the results in a DF
Sililarity <- data.frame(ID = seq(1:6),
                         DTW = c(DTW(pedestrian.1.m, pedestrian.1.m),
                                 DTW(pedestrian.1.m, pedestrian.2.m),
                                 DTW(pedestrian.1.m, pedestrian.3.m),
                                 DTW(pedestrian.1.m, pedestrian.4.m),
                                 DTW(pedestrian.1.m, pedestrian.5.m),
                                 DTW(pedestrian.1.m, pedestrian.6.m)),
                         EditDist = c(EditDist(pedestrian.1.m, pedestrian.1.m),
                                      EditDist(pedestrian.1.m, pedestrian.2.m),
                                      EditDist(pedestrian.1.m, pedestrian.3.m),
                                      EditDist(pedestrian.1.m, pedestrian.4.m),
                                      EditDist(pedestrian.1.m, pedestrian.5.m),
                                      EditDist(pedestrian.1.m, pedestrian.6.m)),
                         Frechet = c(Frechet(pedestrian.1.m, pedestrian.1.m),
                                     Frechet(pedestrian.1.m, pedestrian.2.m),
                                     Frechet(pedestrian.1.m, pedestrian.3.m),
                                     Frechet(pedestrian.1.m, pedestrian.4.m),
                                     Frechet(pedestrian.1.m, pedestrian.5.m),
                                     Frechet(pedestrian.1.m, pedestrian.6.m)),
                         LCSS = c(LCSS(pedestrian.1.m, pedestrian.1.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1),
                                  LCSS(pedestrian.1.m, pedestrian.2.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1),
                                  LCSS(pedestrian.1.m, pedestrian.3.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1),
                                  LCSS(pedestrian.1.m, pedestrian.4.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1),
                                  LCSS(pedestrian.1.m, pedestrian.5.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1),
                                  LCSS(pedestrian.1.m, pedestrian.6.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1)))


# Long-Format
Sililarity_long <- gather(Sililarity, Measure, Value, DTW : LCSS)

# Plot results
ggplot(Sililarity_long, aes(as.factor(ID), Value, fill = as.factor(ID))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Computed similarities using different measures \nbetween trajectory 1 and all the others",
       y = "Value\n", x = "\nComparison trajectory")