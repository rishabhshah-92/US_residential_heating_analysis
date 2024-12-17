rm(list=ls())
source('preamble.R')
source('load_county_borders.R')

years <- c(2010:2023)

df_all_years <- data.frame()

for (y in years) {
  filename <- sprintf("productDownload_2024-12-16T171417/ACSDT5Y%s.B25040-Data.csv",y)
  df_y <- read.csv(file = filename,header=T)[-1,] %>% 
    select(-c(ends_with("M"),"X")) %>%
    mutate_at(vars(starts_with("B25")), as.numeric) %>%
    mutate(year=y) %>%
    select(-ends_with("010E"))
  
  df_all_years <- rbind(df_all_years,df_y)
}


fuel_dict <- c("B25040_001E"="All"
               , "B25040_002E" = "Piped natural gas"
               , "B25040_004E" = "Electric"
               , "B25040_003E" = "Bottled/LP gas"
               , "B25040_005E" = "Oil"
               , "B25040_006E" = "Coal"
               , "B25040_007E" = "Wood"
               , "B25040_008E" = "Solar"
               , "B25040_009E" = "Other"
               , "B25040_010E" = "No heating")

county_totals <- df_all_years %>% 
  pivot_longer(cols=-c("GEO_ID","NAME","year"), names_to="col_name") %>%
  mutate(fuel=fuel_dict[col_name]) %>%
  filter(fuel!='All') %>%
  group_by(GEO_ID,year) %>%
  summarise(total_houses=sum(value))

county_fuel_specific <- df_all_years %>% 
  pivot_longer(cols=-c("GEO_ID","NAME","year"), names_to="col_name", values_to = "num_houses") %>%
  mutate(fuel=fuel_dict[col_name]) %>%
  filter(fuel!='All') %>%
  merge(county_totals %>% select(GEO_ID,year,total_houses), by=c("GEO_ID","year")) %>%
  mutate(perc_share=num_houses/total_houses)

county_largest_share <- county_fuel_specific %>%
  group_by(GEO_ID,year) %>%
  arrange(-perc_share) %>%
  slice(1)

county_year_grid <- expand.grid(GEO_ID=unique(df_all_years$GEO_ID)
                                , year=unique(df_all_years$year))

county_largest_share <- county_year_grid %>%
  merge(county_largest_share, by=c("GEO_ID","year"), all.x=T)

flagged_geo_ids <- unique(county_largest_share %>% filter(is.na(NAME)) %>% pull(GEO_ID))

county_largest_share <- county_largest_share %>%
  filter(!GEO_ID %in% flagged_geo_ids)

sf <- county_borders %>%
  merge(county_largest_share, by="GEO_ID", all.y=F) %>%
  mutate(fuel = factor(fuel, levels=unname(fuel_dict)))




######################################
df_for_ts <- county_fuel_specific %>%
  group_by(year, fuel) %>%
  summarise(total_houses_all_fuel = sum(total_houses)
            , total_houses_fuel = sum(num_houses)
            , fuel_share = total_houses_fuel/total_houses_all_fuel) %>%
  ungroup() %>%
  mutate(fuel = factor(fuel, levels=unname(fuel_dict))) %>%
  filter(fuel %in% unique(sf$fuel)) 

for (y in years){  
    single_fuel_map <- sf %>%
      filter(year==y) %>%
      ggplot(aes(fill=fuel, alpha=perc_share))+
      geom_sf(col=NA)+
      scale_fill_brewer(palette = "Set1")+
      theme_void()+
      labs(title="How homes in the US are heated since 2010"
           , subtitle = "*analysis at the County-level"
           , fill="Note: each County's color indicates the majority heat source; brighter shade means larger majority"
           , caption="Prepared by Dr. Rishabh Shah (linkedin.com/in/rishabh-shah-phd-36196246/)\nData Source: US 2023 Census Data, Table B25040") +
      theme(plot.title = element_text(face = "bold", hjust=0.5, vjust=0.5, color="white",size=14)
            , plot.subtitle = element_text(hjust=0.5, vjust=0.5, color="white",size=9)
            , plot.caption = element_text(hjust=0.95, vjust=2, color="lightgray",size=6)
            , plot.background=element_rect(fill="black")
            , legend.position = "inside"
            , legend.direction = "horizontal"
            , legend.position.inside = c(0.5,0.92)
            , legend.title.position = "bottom"
            , legend.title = element_text(color="lightgray", hjust=0.5, face = "italic", size=7)
            , legend.text = element_text(color="lightgray", size=7)
            , legend.key.size = unit(0.7, 'cm')
            , legend.key.height = unit(0.5, "cm"))+
      guides(fill = guide_legend(nrow = 1),
             alpha = "none")+
      annotate("text",label=y, fontface="bold",
               , x = -90, y=25,
               col="white", size=8, vjust=0)+
      lims(x=c(-125, -67), y=c(24.5-7,49.3+5))
    
    ak_fuel_map <- sf %>%
      filter(year==y) %>%
      ggplot(aes(fill=fuel, alpha=perc_share))+
      geom_sf(col=NA)+
      scale_fill_brewer(palette = "Set1")+
      theme_void()+
      labs(title="Alaska")+
      theme(legend.position="none"
            , plot.background=element_rect(fill="black")
            , plot.title = element_text(color="lightgray", hjust=0.35, size=7)
      )+
      lims(x=c(-170, -129), y=c(53,71))
    
    hi_fuel_map <- sf %>%
      filter(year==y) %>%
      ggplot(aes(fill=fuel, alpha=perc_share))+
      geom_sf(col=NA)+
      scale_fill_brewer(palette = "Set1")+
      theme_void()+
      labs(title="Hawaii")+
      theme(legend.position="none"
            , plot.background=element_rect(fill="black")
            , plot.title = element_text(color="lightgray", hjust=0.35, size=7)
      )+
      lims(x=c(-160, -154), y=c(18,22))
    
    
    ts_y <- df_for_ts %>%
      filter(year<=y) %>%
      ggplot(aes(year,fuel_share,col=fuel))+
      geom_point(data = . %>% filter(year==min(years)), size=0.12)+
      geom_line(lwd=0.7)+
      geom_text(aes(label=ifelse(year==y,paste0(round(fuel_share*100),"%"),NA)), vjust=0, hjust=-0.1, size=2)+
      xlim(c(min(years),max(years)+1))+
      ylim(c(0,0.57))+
      scale_color_brewer(palette = "Set1")+
      theme_classic()+
      labs(title="Nation-wide trend over time")+
      theme(legend.position="none"
            , plot.background=element_rect(fill=NA, color=NA)
            , panel.background = element_rect(fill=NA)
            , axis.text.y = element_blank()
            , axis.text.x = element_text(color="lightgray",size=5)
            , axis.ticks.y = element_blank()
            , axis.title.y = element_blank()
            , axis.title.x = element_blank()
            , plot.margin=unit(c(0,0,0,0), "mm")
            , plot.title = element_text(color="lightgray", hjust=0.1, size=6)
            )
    
    composite_map <- single_fuel_map + 
      annotation_custom(ggplotGrob(ts_y), xmin = -127, xmax = -101, ymin = 16, ymax = 30)+
      annotation_custom(ggplotGrob(ak_fuel_map), xmin = -75, xmax = -65, ymin = 25, ymax = 35)+
      annotation_custom(ggplotGrob(hi_fuel_map), xmin = -75, xmax = -65, ymin = 20, ymax = 25)
                        
    
    ggsave(plot=composite_map, filename = sprintf("plots/fuel_map_%s.png",y)
           , height = 5, width = 6, units = "in", dpi = 500)
}

filenames <- paste0("plots/fuel_map_", years, ".png")

m <- magick::image_read(filenames[1])

for (i in c(2:length(filenames),rep(length(filenames),3))) {
  m <- c(m, magick::image_read(filenames[i]))
}
  
m <- magick::image_animate(m, fps = 2, dispose = "previous", optimize=T)
magick::image_write(m, "fuel_map.gif")


