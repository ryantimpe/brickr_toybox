####
# brick Animated solar system
####

library(brickr)
library(dplyr)
library(tidyr)

set.seed(1223334)

# Define shape functions ----

# Each planet/object is a blank sphere to begin with
create_sphere <- function(sphere_radius, ring_radius = 0){
  if(is.null(ring_radius) | ring_radius == 0){ring_radius <- sphere_radius}
  crossing(x = 1:(ring_radius*2), 
           y = x,
           z = 1:round((5/6)*2*ring_radius)) %>%
    mutate(dist = ((x-median(x))^2 + (y-median(y))^2 + (6/5)*(z-median(z))^2)^(1/2)) %>% 
    #Hollow it for faster calcs
    filter(between(dist, sphere_radius-3, sphere_radius))
}

# Some of the planets have rings... gotta add that in
create_rings <- function(sphere_radius, ring_radius, ring_thickness, z_tilt = 0){
  #Build x and y data frame... add z later
  crossing(x = 1:(2*ring_radius), 
           y = x) %>% 
    mutate(dist = ((x-median(x))^2 + (y-median(y))^2)^(1/2)) %>%
    #Only want the outter thickness
    filter(dist < ring_radius,
           dist >= ring_radius - ring_thickness) %>% 
    #Add Z with tilt
    mutate(dist_x = x-median(x)) %>% 
    mutate(z = round((5/6)*(ring_radius + (z_tilt * (dist_x/ring_radius))))) %>% 
    select(-dist_x)
}

# Test out some planets -----
# display_colors()
# list(create_sphere(5, 10) %>% 
#   mutate(Color = "Cool yellow"),
#   create_rings(5, 10, 3, 2) %>% 
#     mutate(Color = "Bright green")) %>% 
#   bind_rows() %>% 
#   bricks_from_coords() %>% 
#   display_bricks(background = "#111133")
# 
# create_sphere(22) %>% 
#   mutate(Color = "Dark red") %>% 
#   bricks_from_coords() %>% 
#   display_bricks(background = "#111133")

# Data about the actual solar system in miles ----

#The dense main rings extend from (4,300 mi) to (50,000 mi) away from Saturn's 
# equator, whose radius is (37,500 mi) (see Major subdivisions).

#The saturn rings data is edited to make the LEGO model look better

solar_system <- tibble::tribble(
    ~Object, ~Radius,     ~Distance, ~Rings_start, ~Rings_thick, ~Pattern, ~col1, ~col2, ~col3,
      "Sun",  432163,             0, 0, 0,         "solid", "Bright yelow", NA, NA,
  "Mercury",    1516,      35983610, 0, 0,         "solid", "Medium stone grey", NA, NA,
    "Venus",    3761,      67232360, 0, 0,         "solid", "Cool yellow", NA, NA,
    "Earth",    3963,      92957100, 0, 0,         "splot", "Bright blue", "Bright green", NA,
     "Mars",    2111,     141635300, 0, 0,         "solid", "Dark red", NA, NA,
  "Jupiter",   44423,     483632000, 0, 0,         "strip", "Nougat", "Light nougat", "Reddish brown",
   "Saturn",   37449,     888188000, 41749, 30000, "strip", "Light nougat", "White", "Sand yellow",
   "Uranus",   15882,    1783950000, 0, 0,         "solid", "Light royal blue", NA, NA,
  "Neptune",   15389,    2798842000, 0, 0,         "splot", "Dark azur", "Medium blue", NA
  ) %>% 
  #Each stud is 2000 miles... at least for the radius
  mutate_if(is.numeric, list(~round(. / (2*1000)))) %>% 
  mutate(effective_radius = case_when(
    Object == "Sun" ~ 0,
    Rings_start == 0 ~ Radius, 
    Rings_start >  0 ~ Rings_start + Rings_thick),
    distance_x = round(cumsum(effective_radius*1.8)),
    distance_y = round(max(effective_radius))) %>% 
  filter(Object != "Sun") %>% 
  mutate(distance_z = round(max(Radius)))

#Function to create a planet based off detail above
create_planet <- function(planet, radius, rings_start, rings_thick, 
                          x_offset, y_offset, z_offset,
                          pattern, col1, col2, col3) {
  if(rings_start == 0){
    ring_radius = 0
  } else {
    ring_radius <- rings_start + rings_thick
  }
  
  body_cols <- c(col1, col2, col3)
  body_cols <- body_cols[!is.na(body_cols)]
  
  body <- create_sphere(radius, ring_radius) %>% 
    do(if(pattern == "solid"){
      mutate(., Color = body_cols[1])
    } else if(pattern == "strip"){
      group_by(., z) %>% 
        mutate(color_index = runif(1, 1, 20),
               color_index = floor(mean(color_index) %% 2),
               Color = body_cols[color_index+1]) %>% 
        select(-color_index) %>% 
        ungroup()
    } else if(pattern == "splot"){
      mutate(., Color = sample(body_cols[1:2], nrow(.), prob = c(6, 4), replace = TRUE))
    })
  
  ring_cols <- rep(c(col3, col1, col3, NA, col3, col2, NA), 10)
  
  if(rings_start > 0){
    rings <- create_rings(radius, ring_radius, rings_thick, round(radius * 0.23)) %>% 
      mutate(Color = ring_cols[ring_radius - floor(dist)])
   
    body <- bind_rows(body, rings) %>% 
      group_by(x, y, z) %>% 
      filter(row_number() == 1) %>% 
      ungroup() %>% 
      drop_na(Color)
  }
  
  planet_dat <- body %>% 
    mutate(Object = planet,
           x = x + x_offset,
           y = (y-round(median(y)) + y_offset),
           z = (z-round(median(z)) + z_offset))
  return(planet_dat)
  
}

#Create a LEGO version of each planet... stack them ----
solar_sys_bricks <- solar_system %>% 
  filter(Object != "Sun") %>% 
  split(.$Object) %>% 
  purrr::map_df(~create_planet(planet = .$Object, 
                               x_offset = .$distance_x, y_offset = .$distance_y, z_offset = .$distance_z,
                               radius = .$Radius, rings_start = .$Rings_start, rings_thick = .$Rings_thick,
                               pattern = .$Pattern, col1 = .$col1, col2 = .$col2, col3 = .$col3))

#My calculations to offset the x values above is wrong... redo here
adjust_x <- solar_sys_bricks %>% group_by(Object) %>% 
  summarize_at(vars(x, y, z), list(min = min, max = max)) %>% 
  arrange(x_min) %>% 
  mutate(width = x_max - x_min + 1) %>% 
  mutate(x_start = lag(cumsum(width + 4)),
         x_adjust = ifelse(is.na(x_start), 0, x_start-x_min))

#Both  bricks_from_coords() and  display_bricks() take a VERY long time to run... you've been warned! ----
model_bricks <- solar_sys_bricks %>% 
  left_join(adjust_x %>% select(Object, x_adjust)) %>% 
  mutate(x = x + x_adjust) %>% 
  # filter(Object == "Earth") %>% 
  bricks_from_coords() 

model_bricks %>% 
  display_bricks(background = "#111133")

#Animate! ----

num_frames = 360

phivec = c( rep(20, 60), 
            seq(20, 1, length.out = 60),
            rep(1, 60),
            seq(1, 20, length.out = 60),
            seq(20, 90, length.out = 30), rep(90, 30),
            seq(90, 20, length.out = 60)
)


thetavec = c(rep(0, 60), #60 frames frozen 
          270 + 10 + 80 * 1/(1 + exp(seq(-5, 10, length.out = num_frames/6))), #60 to turn
          rep(270+10, 60), #60 frozen + zoom
          270 + 10 - rev(100 * 1/(1 + exp(seq(-5, 10, length.out = num_frames/6)))), #60 to turn
          seq(181, 360, length.out = 60),
          rep(0, 60)
          )

zoomvec <- c(rep(1, 60),
             rep(1, 60),
             rep(1, 10), .2 + .8*(1/(1 + exp(seq(-5, 10, length.out = 50)))),
             .2 + .8*rev(1/(1 + exp(seq(-5, 10, length.out = num_frames/6)))),
             rep(1, 60),
             rep(1, 60))


for(ii in 1:num_frames){
  rayshader::render_camera(theta = thetavec[ii], phi = phivec[ii],
                           zoom = zoomvec[ii])
  rayshader::render_snapshot(paste0("frame", ii, ".png"))
}




# From here, I use ffmpeg to create a video... see the bottom of ?rayshader::render_camera()
# You could also convert the pngs into a GIF

