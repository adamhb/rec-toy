#determining light


#mapping the 50 ha plot
bci_plot <- raster::shapefile("BCI_50Ha")
bci_plot_utm <- spTransform(bci_plot, CRSobj = CRS("+init=epsg:32617"))

plot(bci_plot_utm)

ha_quads <- SpatialPoints(coords = BCI.env[,c(1,2)], proj4string = CRS("+init=epsg:32617"))
plot(ha_quads, add = T)



BCI.env
