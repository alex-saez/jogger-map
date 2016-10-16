
# Code for assigning water fountains to Manhattan street segments.

library(data.table)
library(dplyr)
library(mgcv)

fountains = fread('../data/drink_location.csv')
blocks = fread('../output/blocks_Manhattan.csv')
node_list = fread('../output/node_list.csv')
manhattan_outline = fread('../data/Boundary.csv')
source('../lib/assign_to_segment.R')

fountains = fountains %>%
              select(lon, lat) %>%
              filter(!is.na(lon) & !is.na(lat)) %>%
              as.matrix()
manhattan_outline = as.matrix(manhattan_outline)
fountains = fountains[in.out(manhattan_outline, fountains),] # select manhattan fountains only
node_list = filter(node_list, segment_id %in% blocks$PHYSICALID) # manhattan nodes only


# assign a segment ID to each tree:
segments_fountains = apply(fountains, 1, FUN=assign_to_segment, node_list)

# count fountains per segment:
fountains_per_seg = data.frame(table(segments_fountains))

fountains_per_seg = fountains_per_seg %>%
                    select(seg_ID = segments_fountains, n_fountains = Freq) %>%
                    mutate(seg_ID = as.integer(as.character(seg_ID)))

blocks = left_join(blocks, fountains_per_seg, by=c('PHYSICALID'='seg_ID'))

blocks$n_fountains[is.na(blocks$n_fountains)] = 0 # replace NAs with 0s

blocks$n_fountains[blocks$PHYSICALID %in% c(132284, 3763)] = 2 # correct numbers that don't make sense based on map


write.csv(blocks, file = '../output/blocks_Manhattan.csv', row.names = FALSE)


# for checking that it works: fountains in red, assigned segments in blue
library(leaflet)
segs = which(blocks$n_fountains!=0)
m <- leaflet() %>% addTiles()
for(i in segs){
  m = addPolylines(m, lng = c(blocks$start_lon[i], blocks$end_lon[i]),
                   lat = c(blocks$start_lat[i], blocks$end_lat[i]),
                   opacity = 1
  )
}
for(i in 1:nrow(fountains)){
  m = addCircles(m, lng = c(fountains[i,1]), lat = c(fountains[i,2]), color = 'red', opacity = 1)
}
m

