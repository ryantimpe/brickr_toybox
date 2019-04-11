####
# brick Animated black hole
####

library(brickr)
library(dplyr)
library(tidyr)

set.seed(1223334)

#Size of blackhole in LEGO studs
event_horizon_radius <- 16
black_hole_radius <- round(event_horizon_radius / 4)


#Black hole is just a black sphere with 1/4 radius
black_hole <- crossing(x = -black_hole_radius:black_hole_radius, 
                       y = x,
                       z = -round((5/6)*black_hole_radius):round((5/6)*black_hole_radius)) %>%
  mutate_all(list(~.+event_horizon_radius)) %>% 
  mutate(dist = ((x-median(x))^2 + (y-median(y))^2 + (z-median(z))^2)^(1/2),
         Color = ifelse(dist <= black_hole_radius, "Black", NA))

# Probably an over-complicated function to turn coordinates into spiral legods
create_spirals <- function(spiral_radius, num_coils, color,
                             z_base = event_horizon_radius, z_tilt=0, rotation=0){
  
  chord_length=1
  
  dat <- crossing(x = -spiral_radius:spiral_radius, 
                  y = x)%>%
    mutate_all(list(~.+spiral_radius))
  
  #Derive additional spiral specifications
  centerX <- median(dat$x)
  centerY <- median(dat$y)
  
  thetaMax <- num_coils * 2 * pi
  awayStep <- spiral_radius / thetaMax
  
  #While loop to keep drawing spiral until we hit thetaMax
  spiral <- tibble()
  theta <- chord_length/awayStep
  
  while(theta <= thetaMax){
    #How far away from center
    away = awayStep * theta
    
    #How far around the center
    around = theta + rotation
    
    #Convert 'around' and 'away' to X and Y.
    x = centerX + cos(around) * away
    y = centerY + sin(around) * away
    
    spiral <- spiral %>% 
      bind_rows(tibble(x=x, y=y))
    
    theta = theta + chord_length/away
  }
  
  spiral2 <- spiral %>%
    mutate_all(list(~round(.))) %>% 
    distinct()
  
  #Add Z with tilt
  spiral3 <- spiral2 %>% 
    mutate(dist_x = x-median(x)) %>% 
    mutate(z = z_base + round(z_tilt * (dist_x/spiral_radius))) %>% 
    mutate(Color = color)
  
  return(spiral3)
}

#We want 7 spirals in the Event Horizon, only can use a few colors
num_spirals <- 7
event_horizon_colors <- c("Bright yellow", "Flame yel. orange", "Bright red", 
                          "Bright orange", "Flame yel. orange", "Bright red", 
                          "Bright reddish violet", "Bright orange")

event_horizon <- list(
  spiral_radius = rep(event_horizon_radius, num_spirals),
  num_coils = sample(3:5, num_spirals, replace = TRUE),
  color = rep(event_horizon_colors, 10)[1:num_spirals],
  z_base =  rep(event_horizon_radius, num_spirals),
  z_tilt = sample(-5:5, num_spirals, replace = T, prob = (1:11)^(1/2)),
  rotation = seq(1, 360, length.out = num_spirals)
) 

#Rotate spiral colors - we will swap between 3 color sets in the animation
event_horizon_list <- list(
  event_horizon, event_horizon, event_horizon
)
event_horizon_list[[2]]$color <- rep(event_horizon_colors, 20)[(1:(num_spirals))+1]
event_horizon_list[[3]]$color <- rep(event_horizon_colors, 20)[(1:(num_spirals))+2]

event_horizon_all <- event_horizon_list %>% 
  purrr::map(function(dd){
    dd %>% purrr::pmap_df(create_spirals)
  })

#Animate! ----

num_frames = 360

phivec = c(rep(90, num_frames/4), 90 * 1/(1 + exp(seq(-5, 10, length.out = num_frames/4))))
phivecfull = c(phivec, rev(phivec))

thetavec = seq(0, 719, length.out = num_frames)

zoomvec = c(rep(1, num_frames/3), 0.5 + 0.5 * 1/(1 + exp(seq(-5, 10, length.out = num_frames/6))))
zoomvecfull = c(zoomvec, rev(zoomvec))

frames_to_capture <- rep(rep(seq_along(event_horizon_all), each = 15),100)[1:num_frames]

for(ll in seq_along(event_horizon_all)){
  
  bind_rows(list(black_hole, event_horizon_all[[ll]])) %>%
    drop_na(Color) %>% 
    group_by(x, y, z) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    bricks_from_coords() %>% 
    display_bricks(background = "#111122")
  
  for(ii in 1:num_frames){
    if(frames_to_capture[ii] == ll){
      rayshader::render_camera(
        theta = thetavec[ii], phi = phivecfull[ii], zoom = zoomvecfull[[ii]]
      )
      rayshader::render_snapshot(paste0("frame", ii, ".png"))
    }
  }
  rgl::rgl.clear()
}

# From here, I use ffmpeg to create a video... see the bottom of ?rayshader::render_camera()
# You could also convert the pngs into a GIF

