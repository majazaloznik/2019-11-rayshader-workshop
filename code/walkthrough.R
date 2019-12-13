###############################################################################
## walk through the code for the workshop
##
## NB: this repo has renv initialised. 
## for some reason there was an issue with stringi. an error on loading
#' the rayshader library indicated there was sth wrong with stringi although 
#' it was installed. the solution was to remove the package and reinstall it
#' (stringi) and that made it work again..
###############################################################################


## preliminaires ##############################################################
  
  options(rgl.useNULL = FALSE)
  library(ggplot2)
  library(whitebox)
  library(rayshader)
  library(rayrender)
library(raster)
library(spatstat)
library(spatstat.utils)
library(suncalc)
library(sp)
library(lubridate)
library(rgdal)

## download data ##############################################################
loadzip = tempfile() 
download.file("https://dl.dropboxusercontent.com/s/8ltz4j599z4njay/dem_01.tif.zip", 
              loadzip)
hobart_tif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)

hobart_mat = raster_to_matrix(hobart_tif)
unlink("dem_01.tif")

hobart_mat[1:10,1:10]

#' a good 3d map starts with a good 2d map
#' 
###############################################################################
## play 
###############################################################################

## have a look at regular visualisation
hobart_mat %>%
  height_shade() %>%
  plot_map()

## use sphere shade instead
hobart_mat %>%
  sphere_shade() %>%
  plot_map()

## add detect water
hobart_mat %>%
  sphere_shade() %>%
  add_water(detect_water(hobart_mat)) %>%
  plot_map()

# try different textures
hobart_mat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(hobart_mat), color = "desert") %>%
  plot_map()

hobart_mat %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(detect_water(hobart_mat), color = "imhof4") %>%
  plot_map()

hobart_mat %>%
  sphere_shade(texture = "bw") %>%
  add_water(detect_water(hobart_mat), color = "unicorn") %>%
  plot_map()


hobart_mat %>%
  lamb_shade(zscale = 33) %>%
  plot_map()

hobart_mat %>%
  lamb_shade(zscale = 33, sunangle = 135) %>%
  plot_map()

hobart_mat_inverted = -hobart_mat

hobart_mat %>%
  lamb_shade(zscale = 33) %>%
  plot_map()

hobart_mat_inverted %>%
  lamb_shade(zscale = 33, sunangle = 135) %>%
  plot_map()

#' zscale, sontehing to do with the resolution/sampling of points?
# add the ray shades to our map
hobart_mat %>%
  lamb_shade(zscale = 33) %>%
  add_shadow(ray_shade(hobart_mat, zscale = 33, 
                       sunaltitude = 10, lambert = FALSE), 0.1) %>%
  plot_map()

hobart_mat_inverted %>%
  lamb_shade(zscale = 33, sunangle = 135) %>%
  add_shadow(ray_shade(hobart_mat_inverted, zscale = 33, sunangle = 135,
                       sunaltitude = 6, lambert = FALSE), 0.3) %>%
  plot_map()

hobart_mat %>%
  sphere_shade() %>%
  add_water(detect_water(hobart_mat), color = "lightblue") %>%
  add_shadow(ray_shade(hobart_mat,zscale = 33, sunaltitude = 3,lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(hobart_mat,zscale = 33,sunaltitude = 3), max_darken = 0.5) %>%
  plot_map()

hobart_mat %>%
  sphere_shade() %>%
  add_water(detect_water(hobart_mat), color = "lightblue") %>%
  add_shadow(ray_shade(hobart_mat, zscale = 33, sunaltitude = 5,lambert = FALSE), 
             max_darken = 0.5) %>%
  add_shadow(lamb_shade(hobart_mat,zscale = 33,sunaltitude = 5), max_darken = 0.8) %>%
  plot_map()


hobart_mat %>%
  sphere_shade(sunangle = 225) %>%
  add_water(detect_water(hobart_mat), color = "lightblue") %>%
  add_shadow(ray_shade(hobart_mat,sunangle = 225, zscale = 33, sunaltitude = 5,lambert = FALSE), 
             max_darken = 0.5) %>%
  add_shadow(lamb_shade(hobart_mat,zscale = 33,sunaltitude = 5), max_darken = 0.8) %>%
  plot_map()


hobart_mat %>%
  ambient_shade() %>%
  plot_map()

hobart_mat %>%
  sphere_shade() %>%
  add_water(detect_water(hobart_mat), color = "lightblue") %>%
  add_shadow(ray_shade(hobart_mat, zscale = 33, sunaltitude = 5,lambert = FALSE), 
             max_darken = 0.5) %>%
  add_shadow(lamb_shade(hobart_mat,zscale = 33, sunaltitude = 5), max_darken = 0.7) %>%
  plot_map()

hobart_mat %>%
  sphere_shade() %>%
  add_water(detect_water(hobart_mat), color = "lightblue") %>%
  add_shadow(ray_shade(hobart_mat, zscale = 33, sunaltitude = 5,lambert = FALSE), 
             max_darken = 0.5) %>%
  add_shadow(lamb_shade(hobart_mat,zscale = 33, sunaltitude = 5), max_darken = 0.7) %>%
  add_shadow(ambient_shade(hobart_mat), max_darken = 0.1) %>%
  plot_map()


# 3D Mapping with Rayshader ###################################################




ambientshadows = ambient_shade(hobart_mat)

hobart_mat %>%
  sphere_shade() %>%
  add_water(detect_water(hobart_mat), color = "lightblue") %>%
  add_shadow(ray_shade(hobart_mat, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(hobart_mat, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambientshadows, max_darken = 0.1) %>%
  plot_3d(hobart_mat, zscale = 10,windowsize = c(1000,1000))

render_snapshot()

hobart_mat %>%
  sphere_shade() %>%
  add_water(detect_water(hobart_mat), color = "lightblue") %>%
  add_shadow(ray_shade(hobart_mat, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(hobart_mat, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambientshadows, max_darken = 0) %>%
  plot_3d(hobart_mat, zscale = 10,windowsize = c(1000,1000), 
          phi = 40, theta = 135, zoom = 0.9, 
          background = "grey30", shadowcolor = "grey5", 
          soliddepth = -50, shadowdepth = -100)

render_snapshot(title_text = "River Derwent, Tasmania", 
                title_font = "Helvetica", 
                title_size = 50,
                title_color = "grey90")


render_camera(theta = 90, phi = 30, zoom = 0.7, fov = 0)
render_snapshot()

render_camera(theta = 90, phi = 30, zoom = 0.7, fov = 90)
render_snapshot()

render_camera(theta = 120, phi = 20, zoom = 0.3, fov = 90)
render_snapshot()

if(.Platform$OS.type == "windows") {
  freetype = FALSE
} else {
  freetype = TRUE
}

render_label(hobart_mat, "River Derwent", textcolor ="white", linecolor = "white", freetype = freetype,
             x = 450, y = 260, z = 1400, textsize = 2.5, linewidth = 4, zscale = 10)
render_snapshot(title_text = "render_label() demo, part 1", 
                title_bar_alpha = 0.8,
                title_bar_color = "white")


render_label(hobart_mat, "Jordan River (not that one)", textcolor ="white", linecolor = "white", freetype = freetype,
             x = 450, y = 140, z = 1400, textsize = 2.5, linewidth = 4, zscale = 10, dashed = TRUE)
render_snapshot(title_text = "render_label() demo, part 2", 
                title_bar_alpha = 0.8,
                title_bar_color = "white")


render_camera(zoom = 0.9, phi = 50, theta = -45,fov = 0)
render_snapshot()

render_label(hobart_mat, "Mount Faulkner", textcolor ="white", linecolor = "white", freetype = freetype,
             x = 135, y = 130, z = 2500, textsize = 2, linewidth = 3, zscale = 10, clear_previous = TRUE)
render_snapshot()

render_label(hobart_mat, "Mount Dromedary", textcolor ="white", linecolor = "white", freetype = freetype, 
             x = 320, y = 390, z = 1000, textsize = 2, linewidth = 3, zscale = 10)
render_snapshot()

render_label(clear_previous = TRUE, freetype = freetype)
render_snapshot()

rgl::rgl.close()


hobart_mat %>%
  sphere_shade(sunangle = 60) %>%
  add_water(detect_water(hobart_mat), color = "lightblue") %>%
  add_shadow(ray_shade(hobart_mat, sunangle = 60, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(hobart_mat, sunangle = 60, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambientshadows, max_darken = 0.1) %>%
  plot_3d(hobart_mat, zscale = 10,windowsize = c(1000,1000), 
          background = "#edfffc", shadowcolor = "#273633")

render_camera(theta = 120, phi = 20, zoom = 0.3, fov = 90)
render_depth(focus = 0.81, preview_focus = TRUE)
render_depth(focus = 0.9, preview_focus = TRUE)
render_depth(focus = 0.81)


render_depth(focus = 0.81, focallength = 200, title_bar_color = "black", vignette = TRUE,
             title_text = "The River Derwent, Tasmania", title_color = "white", title_size = 50)

montereybay %>%
  sphere_shade() %>%
  plot_3d(montereybay, water = TRUE, waterlinecolor = "white",
          theta = -45, zoom = 0.9, windowsize = c(1000,1000),zscale = 50)
render_snapshot(title_text = "Monterey Bay, California", 
                title_color = "white", title_bar_color = "black")

render_water(montereybay, zscale = 50, waterdepth = -100, 
             waterlinecolor = "white", wateralpha = 0.7)
render_snapshot(title_text = "Monterey Bay, California (water level: -100 meters)", 
                title_color = "white", title_bar_color = "black")

render_water(montereybay, zscale = 50, waterdepth = 30, 
             waterlinecolor = "white", wateralpha = 0.7)
render_snapshot(title_text = "Monterey Bay, California (water level: 30 meters)", 
                title_color = "white", title_bar_color = "black")


mont_bathy = montereybay
mont_bathy[mont_bathy >= 0] = NA

montereybay %>%
  sphere_shade() %>%
  add_shadow(ray_shade(mont_bathy,zscale = 50, sunaltitude = 15, lambert = FALSE),0.5) %>%
  plot_3d(mont_bathy, water = TRUE, waterlinecolor = "white",
          theta = -45, zoom = 0.9, windowsize = c(1000,1000))

render_snapshot(title_text = "Monterey Bay Canyon", 
                title_color = "white", 
                title_bar_color = "black")


mont_topo = montereybay
mont_topo[mont_topo < 0] = NA

montereybay %>%
  sphere_shade() %>%
  add_shadow(ray_shade(mont_topo, zscale = 50, sunaltitude = 15, lambert = FALSE),0.5) %>%
  plot_3d(mont_topo, shadowdepth = -50, 
          theta = 135, zoom = 0.9, windowsize = c(1000,1000))

render_snapshot(title_text = "Monterey Bay (sans water)", 
                title_color = "white", 
                title_bar_color = "black")


montereybay %>%
  sphere_shade() %>%
  add_shadow(ray_shade(montereybay,zscale = 50,sunaltitude = 15,lambert = FALSE),0.5) %>%
  plot_3d(montereybay, water = TRUE, waterlinecolor = "white", baseshape = "hex",
          theta = -45, zoom = 0.7, windowsize = c(1000,1000), 
          shadowcolor = "#4e3b54", background = "#f7e8fc")

montereybay %>%
  sphere_shade() %>%
  add_shadow(ray_shade(montereybay,zscale = 50,sunaltitude = 15,lambert = FALSE),0.5) %>%
  plot_3d(montereybay, water = TRUE, waterlinecolor = "white", baseshape = "circle",
          theta = -45, zoom = 0.7, windowsize = c(1000,1000),
          shadowcolor = "#4f3f3a", background = "#ffeae3")


montereybay %>%
  sphere_shade() %>%
  plot_3d(montereybay, water = TRUE, waterlinecolor = "white",
          theta = -45, zoom = 0.9, windowsize = c(600,600))


render_movie(filename = "montbay.mp4", title_text = 'render_movie(type = "orbit")', 
             phi = 30 , theta = -45)

render_movie(filename = "montbayosc.mp4", phi = 30 , theta = -90, type = "oscillate",
             title_text = 'render_movie(type = "oscillate")', title_color = "black")


unlink("montbay.mp4")
unlink("montbayosc.mp4")


ease_function = function(beginning, end, steepness = 1, length.out = 180) {
  single = (end) + (beginning - end) * 1/(1 + exp(seq(-10, 10, length.out = length.out)/(1/steepness)))
  single
}

zoom_values = c(ease_function(1,0.3), ease_function(0.3,1))

#This gives us a zoom that looks like this:
ggplot(data.frame(x = 1:360,y = zoom_values),) + 
  geom_line(aes(x = x,y = y),color = "red",size = 2) +
  ggtitle("Zoom value by frame")

render_movie(filename = "montbaycustom.mp4", type = "custom",
             phi = 30 + 15 * sin(1:360 * pi /180), 
             theta = -45 - 1:360, 
             zoom = zoom_values)

rgl::rgl.close()

unlink("montbaycustom.mp4")


montereybay %>%
  sphere_shade(texture = "desert") %>%
  plot_3d(montereybay, windowsize = c(600,600),
          shadowcolor = "#222244", background = "lightblue")

render_camera(theta = -90, fov = 70,phi = 30,zoom = 0.8)

for(i in 1:60) {
  render_water(montereybay, zscale = 50, waterdepth = -60 - 60 * cos(i*pi*6/180),
               watercolor = "#3333bb",waterlinecolor = "white", waterlinealpha = 0.5)
  render_snapshot(filename = glue::glue("iceage{i}.png"), title_size = 30, instant_capture = TRUE,
                  title_text = glue::glue("Sea level: {round(-60 - 60 *cos(i*pi*6/180),1)} meters"))
}

av::av_encode_video(glue::glue("iceage{1:60}.png"), output = "custom_movie.mp4",
                    framerate = 30)

rgl::rgl.close()

unlink(glue::glue("iceage{1:60}.png"))
unlink("custom_movie.mp4")


hobart_mat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(hobart_mat), color = "desert") %>%
  plot_3d(hobart_mat, zscale = 10)
render_highquality()


render_highquality(light = FALSE, scene_elements = sphere(y = 150, radius = 30,material = diffuse(lightintensity = 40, implicit_sample = TRUE)))


montereybay %>%
  sphere_shade() %>%
  plot_3d(montereybay, zscale = 50, water = TRUE)
render_camera(theta = -45, zoom = 0.7, phi = 30,fov = 70)
render_highquality(lightdirection = 100, lightaltitude = 45, lightintensity = 800,
                   clamp_value = 10, title_text = "Monterey Bay, CA", 
                   title_color = "white", title_bar_color = "black")




download.file("https://dl.dropboxusercontent.com/s/hkdxt1zbsjp68jl/LID2007_118755_e.zip",destfile = "LID2007_118755_e.zip")
download.file("https://dl.dropboxusercontent.com/s/omvb63urfby6ddb/LID2007_118754_e.zip",destfile = "LID2007_118754_e.zip")
unzip("LID2007_118755_e.zip")
unzip("LID2007_118754_e.zip")

whitebox::wbt_lidar_tin_gridding(here::here("LID2007_118755_e.las"),
                                 output = here::here("miamibeach.tif"),
                                 resolution = 1, verbose_mode = TRUE,
                                 exclude_cls = '3,4,5,7,13,14,15,16,18')

whitebox::wbt_lidar_tin_gridding(here::here("LID2007_118754_e.las"),
                                 output = here::here("miamibeach2.tif"),
                                 resolution = 1, verbose_mode = TRUE,
                                 exclude_cls = '3,4,5,7,13,14,15,16,18')


download.file("https://dl.dropboxusercontent.com/s/rwajxbdwtkcw50c/miamibeach.tif", destfile = "miamibeach.tif")
download.file("https://dl.dropboxusercontent.com/s/39abkh87h05n7rm/miamibeach2.tif", destfile = "miamibeach2.tif")


miami1 = raster::raster("miamibeach.tif")
miami2 = raster::raster("miamibeach2.tif")

miami_combined = raster::merge(miami1, miami2)

miami_beach = raster_to_matrix(miami_combined)

dim(miami_beach)

# 1/4th the size, so 0.25 for the second argument
miami_beach_small = reduce_matrix_size(miami_beach, 0.25) 

## Backup:
#download.file("https://www.tylermw.com/data/miami_beach_small.Rds",destfile = "miami_beach_small.Rds")
#miami_beach_small = readRDS("miami_beach_small.Rds")
dim(miami_beach_small)

miami_beach_small %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(miami_beach_small,zscale = 4)) %>%
  add_shadow(ray_shade(miami_beach_small, zscale = 4, multicore = TRUE, 
                       sunaltitude = 10, sunangle = -110),0.3) %>%
  plot_map()

miami_beach_small %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(miami_beach_small, cutoff=0.2, zscale=4,
                         min_area = length(miami_beach_small)/150,
                         max_height = 3)) %>%
  add_shadow(ray_shade(miami_beach_small, zscale = 4, multicore = TRUE, 
                       sunaltitude = 10, sunangle = -110),0.3) %>%
  plot_map()

lat_long_to_other = function(lat,long, epsg_code) {
  data = data.frame(long=long, lat=lat)
  coordinates(data) <- ~ long+lat
  #Input--lat/long
  proj4string(data) <- CRS("+init=epsg:4326")
  #Convert to coordinate system specified by EPSG code
  xy = data.frame(spTransform(data, CRS(paste0("+init=epsg:", epsg_code))))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}

# Florida East State Plane EPSG code is 2236
# I grabbed these latitudes and longitudes from double clicking on google maps
bottomleft = lat_long_to_other(25.763675, -80.142499, 2236)
topright = lat_long_to_other(25.775562, -80.127569, 2236)


e = extent(c(bottomleft[1], topright[1], bottomleft[2], topright[2]))

# Use that extent object to crop the original grid to our desired area
crop(miami_combined, e) %>%
  raster_to_matrix() %>%
  reduce_matrix_size(0.25) -> 
  miami_cropped 


miami_cropped %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(miami_cropped, cutoff = 0.3,
                         min_area = length(miami_cropped)/20,
                         max_height = 2)) %>%
  add_shadow(ray_shade(miami_cropped, zscale = 4, multicore = TRUE, 
                       sunaltitude = 30, sunangle = -110),0.3) %>%
  plot_map()


miami_cropped %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(miami_cropped, zscale = 4, multicore = TRUE),0.3) %>% 
  plot_3d(miami_cropped, zscale = 4, water = TRUE, waterdepth = 0,
          zoom=0.85, windowsize = 1000, 
          background = "grey50", shadowcolor = "grey20")

render_camera(phi = 45,fov = 70,zoom = 0.45,theta = 25)
render_snapshot(title_text = "Modern Day Miami Beach, Sea Level: 0 feet",
                title_bar_color = "black",
                title_color = "white", vignette = 0.2)


render_camera(phi = 45,fov = 70,zoom = 0.44,theta = 20)
render_water(miami_cropped,zscale = 4,waterdepth = 3)
render_snapshot(title_text = "Miami Beach, Sea Level: 3 feet",
                title_bar_color = "black",
                title_color = "white", vignette = 0.2)


render_camera(phi = 45,fov = 70,zoom = 0.42,theta = 10)
render_water(miami_cropped,zscale = 4,waterdepth = 7)
render_snapshot(title_text = "Miami Beach, Sea Level: 7 feet (Max pred. 2100 sea level rise)",
                title_bar_color = "black",
                title_color = "white", vignette = 0.2)

render_camera(phi = 45,fov = 70,zoom = 0.41,theta = 5)
render_water(miami_cropped,zscale = 4,waterdepth = 16)
render_snapshot(title_text = "Miami Beach, Sea Level: 16 feet (Max sea level rise + 9 ft storm surge)",
                title_size = 30,
                title_bar_color = "black",
                title_color = "white", vignette = 0.2)


rgl::rgl.close()




