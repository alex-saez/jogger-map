
# Code for adding a tree score to Central Park segments.

library(data.table)
library(dplyr)
library(mgcv)

blocks = fread('../output/blocks_Manhattan.csv')

# Central Park corners:
SW = c(-73.98150 , 40.76832)
SE = c(-73.97315 , 40.76470)
NW = c(-73.95804 , 40.80034)
NE = c(-73.94965 , 40.79681)

# Central Park outline:
CP_outline = rbind(SW, NW, NE, SE)

# indices for segments starting or ending within CP
seg_starts =  as.matrix(select(blocks, start_lon, start_lat))
seg_ends = as.matrix(select(blocks, end_lon, end_lat))
start_in_CP = in.out(CP_outline, seg_starts) # seg starts in CP 
end_in_CP = in.out(CP_outline, seg_ends) # seg ends in CP

# define tree density for CP as 3 times the max seen on streets:
tree_dens_CP = 3*max(blocks$tree_dens)
blocks$tree_dens[start_in_CP | end_in_CP]  = tree_dens_CP


write.csv(blocks, file = '../output/blocks_Manhattan.csv', row.names = FALSE)


# for checking that it works: CP segments in blue
library(leaflet)
segs = which(start_in_CP | end_in_CP)
m <- leaflet() %>% addTiles()
for(i in segs){
  m = addPolylines(m, lng = c(blocks$start_lon[i], blocks$end_lon[i]),
                   lat = c(blocks$start_lat[i], blocks$end_lat[i]),
                   opacity = 1
  )
}
m
