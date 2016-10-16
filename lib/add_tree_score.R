
# code for adding a "tree score" to each Manhattan street segment.

library(data.table)
library(dplyr)
library(mgcv)

trees = fread('../data/TreesCount2015Trees.csv')
blocks = fread('../output/blocks_Manhattan.csv')
node_list = fread('../output/node_list.csv')
manhattan_outline = fread('../data/Boundary.csv')
source('../lib/assign_to_segment.R')

trees = select(trees, lon=longitude, lat=latitude) # remove useless columns
trees = as.matrix(trees)
manhattan_outline = as.matrix(manhattan_outline)
trees = trees[in.out(manhattan_outline, trees),] # select manhattan trees only
node_list = filter(node_list, segment_id %in% blocks$PHYSICALID) # manhattan nodes only


# assign a segment ID to each tree:
segments_trees = apply(trees, 1, FUN=assign_to_segment, node_list)

# count trees per segment:
trees_per_seg = data.frame(table(segments_trees))

trees_per_seg = trees_per_seg %>%
                  select(seg_ID = segments_trees, tree_dens = Freq) %>%
                  mutate(seg_ID = as.integer(as.character(seg_ID)))

blocks = blocks %>%
          left_join(trees_per_seg, by=c('PHYSICALID'='seg_ID')) %>%
          mutate(tree_dens = tree_dens/SHAPE_Leng)

blocks$tree_dens[is.na(blocks$tree_dens)] = 0 # replace NAs with 0s

write.csv(blocks, file = '../output/blocks_Manhattan.csv', row.names = FALSE)




