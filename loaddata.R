install.packages("devtools")
devtools::install_github("mikaelpoul/readgdf")
library(readgdf)

data <- read_gdf("channelnet_seeds1_nodes35_2019_04_29-09_11_49.gdf")
head(data)
data
