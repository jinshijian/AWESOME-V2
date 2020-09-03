
country_lat_long_check <- function(sdata){
  ggplot(data = counties) + 
    geom_polygon(aes(x = long, y = lat, group = group),
                 color = "gray", fill = 'white', alpha = 0.5, size = 0.25) + 
    guides(fill=FALSE) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_rect(data = sdata,
              mapping=aes(xmin=Long_min, xmax=Long_max, ymin=Lat_min, ymax=Lat_max),
              color="red", alpha = 0.5) +
    labs(title=sdata$Country,
         x ="Latitude", y = "Longitude") ->
    p_check
  print(p_check)}


qc_background <- function (sdata) { 
  ggplot(sub_data, aes(x=Value)) + 
    geom_histogram(color="black", fill="gray", bins = 30) + 
    theme_bw() +
    xlab (var_num_col[i]) ->
    p_hist
  print(p_hist)
}