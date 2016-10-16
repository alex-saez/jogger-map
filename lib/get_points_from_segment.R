

get_points_from_segment = function(the_geom){
  # Extract points necessary for plotting segment on map
  # Input: 'the_geom' field of a segment
  
  library(dplyr)
  
  the_geom = gsub("LINESTRING \\(", "", the_geom)
  the_geom = gsub("\\)", "", the_geom)
  
  points = the_geom %>% 
              strsplit(', ') %>%
              unlist() %>%
              strsplit(' ') %>%
              unlist() %>%
              as.numeric()
    
  point_coords = data.frame(lon = points[seq(1, length(points), 2)], 
                            lat = points[seq(2, length(points), 2)])

  return(point_coords)

}



# # Usage:
# library(leaflet)
# blocks = fread('../output/blocks_Manhattan.csv')
# 
# ind = sample(nrow(blocks), 1000)
# 
# m <- leaflet() %>% addTiles()
# for(i in ind){
#   seg_points = get_points_from_segment(blocks$the_geom[i])
#   m = addPolylines(m, lng = seg_points$lon, lat = seg_points$lat,   opacity = 1)
# }
# m





