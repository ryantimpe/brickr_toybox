library(tidyverse)
library(brickr)

#Alderaan ----
alderaan_radius <- 25
alderaan_nudge <- -40

alderaan <- crossing(x = 1:(alderaan_radius*2), 
         y = x,
         z = 1:round((5/6)*2*alderaan_radius)) %>%
  mutate(dist = ((x-median(x))^2 + (y-median(y))^2 + (6/5)*(z-median(z))^2)^(1/2)) %>% 
  #Hollow it for faster calcs
  filter(between(dist, alderaan_radius-10, alderaan_radius)) %>% 
  #Big planet, so only show some of it in image
  mutate(x = x + alderaan_nudge) %>% 
  filter(x > 0) %>% 
  #Blue planet with some green and clouds
  mutate(Color = sample(c("Bright blue", "Dark green", "White"), nrow(.), prob = c(4, 1, 2), replace = T))
  
alderaan %>% 
  bricks_from_coords() %>% 
  display_bricks(background = "#111133")

# Death Star ----
ds_radius <- 10

death_star <- crossing(x = 1:(ds_radius*2), 
                      y = x,
                      z = 1:round((5/6)*2*ds_radius)) %>%
  mutate(dist = ((x-median(x))^2 + (y-median(y))^2 + (6/5)*(z-median(z))^2)^(1/2),
         cannon_dist = ((x-quantile(x)[2])^2 + (y-quantile(y)[4])^2 + (6/5)*(z-quantile(z)[4]+1)^2)^(1/2)) %>% 
  #Hollow it for faster calcs
  filter(between(dist, ds_radius-7, ds_radius), cannon_dist > 2.5) %>% 
  #Blue planet with some green and clouds
  mutate(panel = (z %/% 4 + y %/% 3 + x %/% 3) %% 2) %>% 
  mutate(Color = case_when(
    cannon_dist <= 4.5 ~ "Sand blue",
    z == median(z) & (y*x)%%2 == 0 ~ "Black",
    z == median(z) ~ "White",
    z %in% quantile(z) ~ "Medium stone grey",
    panel == 0 ~ "Medium stone grey",
    panel == 1 ~ "Dark stone grey",
    TRUE ~ "White"
  ))

death_star %>% 
  bricks_from_coords() %>% 
  display_bricks(background = "#111133")

# Death Star cannon beams ----
cannon_end <- c(0, 0, 10)

ds_cannons <- death_star %>% 
  filter(Color == "Sand blue") %>% 
  top_n(16, cannon_dist) %>% 
  mutate(cannon_point = purrr::pmap(list(x, y, z), function(xa, ya, za){
    n_bricks <- max(abs(x - cannon_end[1]), abs(y - cannon_end[2]), abs(z - cannon_end[3]))
    tibble(x = seq(xa, cannon_end[1], length.out = n_bricks),
           y = seq(ya, cannon_end[2], length.out = n_bricks),
           z = seq(za, cannon_end[3], length.out = n_bricks))
  })) %>% 
  select(cannon_point) %>% 
  unnest() %>% 
  mutate_all(round) %>% 
  bind_rows(tibble(x=cannon_end[1]:(cannon_end[1]+5),
                   y=cannon_end[2]:(cannon_end[2]+5),
                   z = 10)) %>% 
  mutate(Color = "Bright yel. green")

bind_rows(death_star, ds_cannons) %>% 
  bricks_from_coords() %>% 
  display_bricks(background = "#111133")

#Explode function ----

explode_object <- function(brickr_obj, exp_radius = 1, dissipate = 1,
                           heat = 0){
  expl <- brickr_obj %>% 
    mutate_at(vars(x, y, z), 
              list(~. + sign(.-median(.))*sample(0:exp_radius, length(.), 
               prob = c(1:(exp_radius+1))^(1/2), replace = TRUE))) %>% 
    sample_frac(dissipate) 
  
  expl$heat <- sample(c(T,F), nrow(expl), prob = c(heat, 1-heat), replace = TRUE)
  
  expl[expl$heat, "Color"] <- sample(c("White", "Bright yellow", "Flame yel. orange", "Bright red"), 
                                     nrow(expl[expl$heat,]),
                                prob = c(3, 2, 1.5, 1), replace = TRUE)
  
  return(expl)
}

death_star %>% 
  bricks_from_coords() %>% 
  display_bricks(background = "#111133") 


#Animate ----

death_star %>% 
  mutate(x=x+30, y=y+30, z=z+30)%>% 
  explode_object(10, 0.3, 0.5)  %>% 
  bricks_from_coords() %>% 
  display_bricks(background = "#111133") 

expl_rate <- 20

ds_models <- list(
  radius = c(0:expl_rate),
  dissipation = c(1, seq(1, 0.1, length.out = expl_rate)^(2)),
  heat = c(0, seq(0, 1, length.out = expl_rate)^(1/2))
) %>% 
  purrr::pmap(function(radius, dissipation, heat){
    death_star %>% 
      mutate(x=x+20, y=y+20, z=z+20)%>% 
      explode_object(radius, dissipation, heat)  %>% 
      bricks_from_coords()
  })

nf = 720

models_to_show <- c(rep(1, 240),
                    rep(2:21, each = (nf-240)/expl_rate))

phivec = 15
phivecfull = rep(phivec, nf)

thetavec = c(seq(1, 360, length.out = 120),
             seq(1, 360, length.out = 120),
             seq(1, 360*4, length.out = 720-240))


zoomvec = c(seq(50, 100, length.out = 120)/100,
            rep(1, 120),
            seq(100, 200, length.out = 120)/100)
zoomvecfull = c(zoomvec, seq(200, 50, length.out = nf-length(zoomvec))/100)
            

rayshader::render_camera(theta = 245, phi = 15, zoom = 2)

for(mm in 2:max(models_to_show)){
  ds_models[[mm]] %>% 
    build_bricks(background = "#050510", theta = 245, phi = 15)
  print(paste("Model iteration:", mm))
  
  for(ii in 1:nf) {
    if(models_to_show[ii] == mm){
      rayshader::render_camera(theta = thetavec[ii],phi = phivecfull[ii], zoom = zoomvecfull[ii])
      rayshader::render_snapshot(paste0("frame", ii, ".png"))
    }
  }
  print(paste("End model:", mm))
  rgl::rgl.clear()
}

for(ii in 721:780){
  rayshader::render_snapshot(paste0("frame", ii, ".png"))
}
