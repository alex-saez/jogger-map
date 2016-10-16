
# code for adding end point information to each Manhattan street segment.

library(data.table)
library(dplyr)

blocks = fread('../data/street blocks.csv')

blocks = blocks %>%
          filter(BOROCODE==1) %>% # Manhattan only
          select(PHYSICALID, the_geom, ST_WIDTH, SHAPE_Leng) # only useful variables

get_end_nodes = function(seg, type){ 
  # input:  seg = 'the_geom' field of a row in 'blocks'
  #         type = one of the following: start_lon, start_lat, end_lon, end_lat
  # output: the lon or lat of one of the 2 end points of the street block according to the value of 'type'
  
  seg = gsub("LINESTRING \\(", "", seg)
  seg = gsub("\\)", "", seg)
  nodes = strsplit(seg, ', ')
  
  node_coords = data.frame(lon=NA, lat=NA)
  for(i in 1:length(nodes[[1]])){
    node_coords[i,] = as.double(strsplit(nodes[[1]][i], ' ')[[1]])
  }
  
  switch(type,
         start_lon = return(node_coords[1,1]),
         start_lat = return(node_coords[1,2]),
         end_lon = return(node_coords[nrow(node_coords),1]),
         end_lat = return(node_coords[nrow(node_coords),2])
  ) 
}


# add lon and lat of segment end points as columns:
blocks = mutate(blocks, 
                start_lon = sapply(the_geom, get_end_nodes,'start_lon'),
                start_lat = sapply(the_geom, get_end_nodes,'start_lat'),
                end_lon = sapply(the_geom, get_end_nodes,'end_lon'),
                end_lat = sapply(the_geom, get_end_nodes,'end_lat') )



# add unique IDs to segment end points based on their coords rounded to the 5th decimal:
blocks = blocks %>% 
          mutate(start_ID = paste(as.character(round(start_lon, 5)), as.character(round(start_lat, 5)))) %>% 
          mutate(end_ID = paste(as.character(round(end_lon, 5)), as.character(round(end_lat, 5))))


write.csv(blocks, file = '../output/blocks_Manhattan.csv', row.names = FALSE)


# # for checking that it works:
# library(leaflet)
# block = filter(blocks, PHYSICALID==3188) # enter any block ID here
# 
# m <- leaflet() %>%
#       addTiles() %>%
#       addMarkers(lng=block$start_lon , lat=block$start_lat) %>%
#       addMarkers(lng=block$end_lon , lat=block$end_lat)
# m



