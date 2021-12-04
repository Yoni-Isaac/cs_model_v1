aa=sf::st_read("G:/Geohydrology/Apps/CS_Model_V02/data/DEMs/library_dms_V2.shp")
bb=sf::st_read("C:/Users/ADSL/Downloads/ugrid_2D.shp") %>% 
  sf::st_union (., by_feature = FALSE, is_coverage = FALSE) %>%  st_transform(4326) %>% 
  st_cast(.,"POLYGON")

st_write(bb,"C:/Users/ADSL/Downloads/north_eastren.shp")

cc=st_read("C:/Users/ADSL/Downloads/north_eastren.shp", quiet = T) %>% 
  mutate(basin="north_eastren") %>% 
  subset(.,,c("basin","geometry"))


dd=bind_rows(aa,cc)

st_write(dd,"G:/Geohydrology/Apps/CS_Model_V02/data/DEMs/library_dms_V2.shp",delete_layer =T)
