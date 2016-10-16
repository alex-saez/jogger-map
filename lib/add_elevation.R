
# Code for adding a "slope" to each Manhattan street segment.
# The slope is the abs of the difference in elevation between the two ends of the segment
# divided by the length of the segment

library(data.table)
library(dplyr)

blocks = fread('../output/blocks_Manhattan.csv')
elevation = fread('../data/Manhattan_Elevation.csv')

elevation = elevation %>%
            filter(FEAT_CODE==3000) %>% # ground elevation only
            distinct(LONG, ALT, .keep_all = TRUE)

find_closest_elev = function(point, samples){
  # Input:  point: in the form c(lon, lat)
  #         samples: matrix with 3 columns lon, lat, elevation
  # Ouput:  elevation of the sample closest to 'point'
  dists = (samples[,1]-point[1])^2 + (samples[,2]-point[2])^2
  return(samples[which.min(dists),3])
}

topo = as.matrix(select(elevation, LONG, ALT, ELEVATION))

start_mat = as.matrix(select(blocks, start_lon, start_lat))
start_elev = apply(start_mat, 1, FUN=find_closest_elev, topo)

end_mat = as.matrix(select(blocks, end_lon, end_lat))
end_elev = apply(end_mat, 1, FUN=find_closest_elev, topo)

blocks = mutate(blocks, slope = abs(start_elev - end_elev)/SHAPE_Leng)

write.csv(blocks, file = '../output/blocks_Manhattan.csv', row.names = FALSE)


# for checking that it works:
# library(leaflet)
# 
# pal = colorRampPalette(c("black", "red"))
# colors = pal(nrow(blocks))
# 
# segs = sample(nrow(blocks))
# 
# m <- leaflet() %>% addTiles()
# for(i in segs){
#   m = addPolylines(m, lng = c(blocks$start_lon[i], blocks$end_lon[i]),
#                       lat = c(blocks$start_lat[i], blocks$end_lat[i]),
#                       color = colors[rank(blocks$slope)[i]],
#                       opacity = 1
#                    )
# }
# m

