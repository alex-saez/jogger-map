

assign_to_segment = function(point, node_list){
  # function to assign a feature with (lon, lat) coords to the street segment it belongs to on the map
  # point in the form c(lon, lat)
  # need to preload the list of all nodes and give it as second argument: node_list = fread('../output/node_list.csv')
  # returns the ID of the assigned segment. IDs are defined by table 'street blocks.csv'
  
  library(data.table)
  library(dplyr)
  
  dist_to_segment = function(point, line){
    # auxiliary function to calculate distance between a point and a bounded line segment,
    # where point in the form c(x0,y0) and line in the form c(x1,y1,x2,y2)
    x0 = point[1]; y0 = point[2]
    x1 = line[1]; y1 = line[2]
    x2 = line[3]; y2 = line[4]
    px = x2-x1
    py = y2-y1
    if(px^2 + py^2 == 0)  u = 1
    else u = ((x0 - x1)*px + (y0 - y1)*py) / (px^2 + py^2)
    if(u>1) u = 1
    else if(u<0) u = 0
    x = x1 + u*px
    y = y1 + u*py
    dx = x - x0
    dy = y - y0
    dist = dx^2 + dy^2
    return(dist)
  }
  
  
  node_coords = as.matrix(select(node_list, lon, lat))
  
  dist_mat = (node_coords[,1]-point[1])^2 + (node_coords[,2]-point[2])^2
  
  closest_nodes = node_list[order(dist_mat),][1:100,] # look only at 100 closest nodes
  closest_dupl_nodes_a = filter(closest_nodes, duplicated(segment_id, fromLast = FALSE))
  closest_dupl_nodes_b = filter(closest_nodes, duplicated(segment_id, fromLast = TRUE))
  possible_segments = left_join(closest_dupl_nodes_a, closest_dupl_nodes_b, by = c("segment_id" = "segment_id"))
  possible_segments = possible_segments %>% filter(node_id.x != node_id.y)
  
  segment_assigned = possible_segments %>% 
                      group_by(node_id.x, node_id.y, segment_id) %>% 
                      summarise(dist = dist_to_segment(point, c(lon.x, lat.x, lon.y, lat.y))) %>%
                      arrange(dist) %>%
                      head(1) %>% 
                      as.data.frame() %>%
                      select(segment_id) %>%
                      as.integer()
 
  return(segment_assigned)
  
}



# # for checking: pick a tree and plot its location along as the 2 end points of the segment it was assigned to:
# 
# library(leaflet)
# blocks = fread('../data/street blocks.csv')
# node_list = fread('../output/node_list.csv')
# trees = fread('../data/TreesCount2015Trees.csv')
# trees = as.matrix(select(trees, longitude, latitude))
# n_nodes = nrow(node_list)
# 
# point = trees[154454,] # enter any tree index here
# seg = select(filter(blocks, PHYSICALID==assign_to_segment(point, node_list)), the_geom)
# seg_coords = nodes_from_seg(seg) # find this function within /lib/build_node_list.R
# 
# m <- leaflet() %>%
#   addTiles() %>%
#   addMarkers(lng=point[1], lat=point[2]) %>%
#   addMarkers(lng=seg_coords$lon[1] , lat=seg_coords$lat[1]) %>%
#   addMarkers(lng=seg_coords$lon[2] , lat=seg_coords$lat[2])
# m





