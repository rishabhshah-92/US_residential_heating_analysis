include_states <- c(fips::lower48 %>% pull(fips),"02","15")

county_borders <- read_sf("cb_2018_us_county_5m/cb_2018_us_county_5m.shp") %>%
  filter(STATEFP %in% include_states) %>%
  rename(GEO_ID=AFFGEOID)
 
# county_borders %>%
#   ggplot()+
#   geom_sf()+
#   theme_void()