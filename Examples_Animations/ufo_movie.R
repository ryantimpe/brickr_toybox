library(tidyverse)
library(brickr)
library(rgl)

this_file = "Examples_Animations/brickr_ufo3.xlsx"

#This script builds a very complicated UFO cow animation

U = structure(c(1, -0.2, 1, 0, 1, 0.1, -1, 0, 0, 1, 0.2, 0, 0, 0, 
                0, 1), .Dim = c(4L, 4L))

bg_cols = (0:7/10) %>% purrr::map_chr(~ colorspace::darken("#87ceeb", amount=.x))

tibble::tibble(
  x=0:7,
  col= (x/10) %>% purrr::map_chr(~ colorspace::darken("#87ceeb", amount=.x))
) %>% 
  ggplot(aes(x=x, y=1, fill=col)) +
  geom_tile()+
  scale_fill_identity()

#Kinda cool, you can use the same piece table for all since they dont render bricks

#Background
readxl::read_xlsx(this_file, sheet = "bg_colors") %>% 
  bricks_from_excel(
    piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
  ) %>% 
  build_bricks(
    trans_alpha = 0.3,
    background_color =  "#87ceeb" #"#003152"
  ) 

#Cloud
readxl::read_xlsx(this_file, sheet = "cloud_colors") %>% 
  bricks_from_excel(
    piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
  ) %>% 
  build_bricks(
    trans_alpha = 0.7, background_color =  "#87ceeb" #"#003152"
  ) 

#Sun
readxl::read_xlsx(this_file, sheet = "sun_colors") %>% 
  bricks_from_excel(
    piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
  ) %>% 
  build_bricks(
    trans_alpha = 0.7,
    background_color =  "#87ceeb" #"#003152"
  ) 

#Cow
readxl::read_xlsx(this_file, sheet = "cow_colors") %>% 
  bricks_from_excel(
    piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
    increment_level = 0
  ) %>% 
  build_bricks(
    #rgl_lit = F, outline_bricks = T,
    trans_alpha = 0.3,
    background_color =  "#87ceeb" #"#003152"
  ) 



#Animation

par3d(windowRect = c(20, 30, 900, 800), zoom = 0.9)

# U <- par3d("userMatrix")
# U <- round(U, 1)

par3d(userMatrix = U)




#PART 1 - DAYTIME ----

for(ii in 1:90){
  #Full light
  if(ii <= 30){
    rgl::rgl.snapshot(paste0("rrgl/ufo", stringr::str_pad(ii, 4, pad="0"), ".png"))
  } else {
    sunset_index = (ii-31) %/% 10

    if(ii%%10 == 1){
      rgl.clear()
      
      #Background
      readxl::read_xlsx(this_file, sheet = "bg_colors") %>% 
        bricks_from_excel(
          piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
        ) %>% 
        build_bricks(
          trans_alpha = 0.3,
          background_color =  "#87ceeb" 
        ) 
      
      
      #Cloud
      readxl::read_xlsx(this_file, sheet = "cloud_colors") %>% 
        bricks_from_excel(
          piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
        ) %>% 
        build_bricks(
          trans_alpha = 0.7, background_color =  "#87ceeb" #"#003152"
        ) 
      
      #Sun
      readxl::read_xlsx(this_file, sheet = "sun_colors") %>% 
        bricks_from_excel(
          piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
          increment_level = -(sunset_index+1)*4
        ) %>% 
        build_bricks(
          trans_alpha = 0.7,
          background_color =  "#87ceeb" 
        ) 
      
      #Cow
      readxl::read_xlsx(this_file, sheet = "cow_colors") %>% 
        bricks_from_excel(
          piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
          increment_level = 0
        ) %>% 
        build_bricks(
          trans_alpha = 0.3,
          background_color =  "#87ceeb" 
        )
      
      par3d(windowRect = c(20, 30, 900, 800), zoom = 0.9)
      par3d(userMatrix = U)
    }
    
    rgl::bg3d(color = bg_cols[sunset_index + 2])
    rgl::rgl.snapshot(paste0("rrgl/ufo", stringr::str_pad(ii, 4, pad="0"), ".png"))
      
   }
  
}


#PART 2 - NIGHT ----
#91-120

#Background
readxl::read_xlsx(this_file, sheet = "bg_colors") %>% 
  bricks_from_excel(
    piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
  ) %>% 
  build_bricks(
    trans_alpha = 0.3,
    background_color =  "#87ceeb" #"#003152"
  ) 

#Cloud
readxl::read_xlsx(this_file, sheet = "cloud_colors") %>% 
  bricks_from_excel(
    piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
  ) %>% 
  build_bricks(
    trans_alpha = 0.7, background_color =  "#87ceeb" #"#003152"
  ) 

#Moon
readxl::read_xlsx(this_file, sheet = "moon_colors") %>% 
  bricks_from_excel(
    piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
  ) %>% 
  build_bricks(
    trans_alpha = 0.3,
    background_color =  "#87ceeb" #"#003152"
  ) 

#Cow
readxl::read_xlsx(this_file, sheet = "cow_colors") %>% 
  bricks_from_excel(
    piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
    increment_level = 0
  ) %>% 
  build_bricks(
    #rgl_lit = F, outline_bricks = T,
    trans_alpha = 0.3,
    background_color = bg_cols[8]
  ) 

par3d(windowRect = c(20, 30, 900, 800), zoom = 0.9)
par3d(userMatrix = U)

for(ii in 91:120){
  rgl::rgl.snapshot(paste0("rrgl/ufo", stringr::str_pad(ii, 4, pad="0"), ".png"))
}


#PART 3 - DESCENT ----

save_ufo_level = 99
save_ufo_light = 99

for(ii in 121:210){
  ufo_level = pmin((210-ii) %/% ((210-121)/14), 13)
  
  ufo_lights = ((ii - 1) %/% 6) %% 3
  
  if(ufo_level != save_ufo_level | ufo_lights != save_ufo_light){
    rgl.clear()
    
    #Background
    readxl::read_xlsx(this_file, sheet = "bg_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Cloud
    readxl::read_xlsx(this_file, sheet = "cloud_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.7, background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Moon
    readxl::read_xlsx(this_file, sheet = "moon_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Cow
    readxl::read_xlsx(this_file, sheet = "cow_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
        increment_level = 0
      ) %>% 
      build_bricks(
        #rgl_lit = F, outline_bricks = T,
        trans_alpha = 0.3,
        background_color = bg_cols[8]
      ) 
    
    #UFO
    readxl::read_xlsx(this_file, sheet = paste0("ufo_colors", ufo_lights+1)) %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
        increment_level = ufo_level,
        max_level = 26 + 8
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color = bg_cols[8]
      ) 
    par3d(windowRect = c(20, 30, 900, 800), zoom = 0.9)
    par3d(userMatrix = U)
  }
  save_ufo_level <- ufo_level
  save_ufo_light <- ufo_lights
  
  rgl::rgl.snapshot(paste0("rrgl/ufo", stringr::str_pad(ii, 4, pad="0"), ".png"))
  
}
  

#PART 4 - BEAM DROPPING ----

save_beam_min = 99
save_ufo_light = 99

p4_frames = 90

#Levels 12-21 are beams

for(ii in 211:(211+p4_frames-1)){
  (beam_min = pmax(211:(211+p4_frames-1) %/% ((p4_frames)/10) - 12, 12))
  
  beam_min = rev(beam_min)[ii-210]
  
  ufo_lights = ((ii - 1) %/% 6) %% 3
  
  if(beam_min != save_beam_min | ufo_lights != save_ufo_light){
    rgl.clear()
    
    #Background
    readxl::read_xlsx(this_file, sheet = "bg_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Cloud
    readxl::read_xlsx(this_file, sheet = "cloud_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.7, background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Moon
    readxl::read_xlsx(this_file, sheet = "moon_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Cow
    readxl::read_xlsx(this_file, sheet = "cow_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
        increment_level = 0
      ) %>% 
      build_bricks(
        #rgl_lit = F, outline_bricks = T,
        trans_alpha = 0.3,
        background_color = bg_cols[8]
      ) 
    
    #UFO
    readxl::read_xlsx(this_file, sheet = paste0("ufo_colors", ufo_lights+1)) %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
        increment_level = 0,
        max_level = Inf,
        min_level = beam_min,
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color = bg_cols[8]
      ) 
    
    #Beams
    readxl::read_xlsx(this_file, sheet = "beams_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
        increment_level = 0,
        min_level = beam_min
      ) %>% 
      build_bricks(
        #rgl_lit = F, outline_bricks = T,
        trans_alpha = 0.3,
        background_color = bg_cols[8]
      ) 
    
    par3d(windowRect = c(20, 30, 900, 800), zoom = 0.9)
    par3d(userMatrix = U)
  }
  save_beam_min  <- beam_min
  save_ufo_light <- ufo_lights
  
  rgl::rgl.snapshot(paste0("rrgl/ufo", stringr::str_pad(ii, 4, pad="0"), ".png"))
  
}


#PART 5 - BEAM ME UP ----
save_cow_level = 99
save_ufo_light = 99
save_beam_min = 99

p5_frames = 150

thetavec = seq(0, 2*pi, length.out = p5_frames)
thetavec = scales::rescale(sin(seq(0, 2*pi, length.out = p5_frames) * pi/180), c(0, 2*pi))
thetavec[length(thetavec)] = 0

zoomvechalf = 0.9 + 0.1 * 1/(1 + exp(seq(-5, 10, length.out = p5_frames/2)))
zoomvec = c(rev(zoomvechalf), zoomvechalf)

#Max cow level 21
301
for(ii in 432:(301+p5_frames-1)){

  cow_level = (ii-1) %/% (p5_frames/23) - 46
  
  beam_min = pmax((ii-1) %/% ((p5_frames)/10) - 8, 8)
  
  ufo_lights = ((ii - 1) %/% 12) %% 3
  
  if(beam_min != save_beam_min | ufo_lights != save_ufo_light | cow_level != save_cow_level){
    rgl.clear()
    
    #Background
    readxl::read_xlsx(this_file, sheet = "bg_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Cloud
    readxl::read_xlsx(this_file, sheet = "cloud_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.7, background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Moon
    readxl::read_xlsx(this_file, sheet = "moon_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Cow
    if(ii < 430){
      readxl::read_xlsx(this_file, sheet = "cow_colors") %>% 
        bricks_from_excel(
          piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
          increment_level = cow_level,
          max_level = 21
        ) %>% 
        build_bricks(
          #rgl_lit = F, outline_bricks = T,
          trans_alpha = 0.3,
          background_color = bg_cols[8]
        ) 
    }
    
    #UFO
    readxl::read_xlsx(this_file, sheet = paste0("ufo_colors", ufo_lights+1)) %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
        increment_level = 0,
        max_level = Inf,
        min_level = 0,
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color = bg_cols[8]
      ) 
    
    #Beams
    readxl::read_xlsx(this_file, sheet = "beams_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
        increment_level = 0,
        min_level = beam_min
      ) %>% 
      build_bricks(
        #rgl_lit = F, outline_bricks = T,
        trans_alpha = 0.3,
        background_color = bg_cols[8]
      ) 
    
    par3d(windowRect = c(20, 30, 900, 800))
    
  }
  
  par3d(userMatrix = rotate3d(U, thetavec[ii-300], 0, 0 , 1), zoom = zoomvec[ii-300])
  
  save_cow_level  <- cow_level
  save_ufo_light <- ufo_lights
  save_beam_min <- beam_min
  
  rgl::rgl.snapshot(paste0("rrgl/ufo", stringr::str_pad(ii, 4, pad="0"), ".png"))
  
}

#PART 6 - BYE BYE UFO ----

save_ufo_level = 99
save_ufo_light = 99

p6_frames = 90

phivec = rev(scales::rescale(1/(1 + exp(seq(-2, 4, length.out = p6_frames))), c(0, pi/4)))

for(ii in 451:(451+p6_frames-1)){
  ufo_level = pmin((ii-1) %/% ((210-121)/14)-70, 13)
  
  ufo_lights = ((ii - 1) %/% 6) %% 3
  
  if(ufo_level != save_ufo_level | ufo_lights != save_ufo_light){
    rgl.clear()
    
    #Background
    readxl::read_xlsx(this_file, sheet = "bg_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Cloud
    readxl::read_xlsx(this_file, sheet = "cloud_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.7, background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Moon
    readxl::read_xlsx(this_file, sheet = "moon_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  "#87ceeb" #"#003152"
      ) 
    
    #UFO
    readxl::read_xlsx(this_file, sheet = paste0("ufo_colors", ufo_lights+1)) %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
        increment_level = ufo_level,
        max_level = 26 + 8
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color = bg_cols[8]
      ) 
  }
  par3d(windowRect = c(20, 30, 900, 800), zoom = 0.9)
  par3d(userMatrix =  rotationMatrix(phivec[ii-450], 1,0,0) %*% U)
  
  save_ufo_level <- ufo_level
  save_ufo_light <- ufo_lights
  
  rgl::rgl.snapshot(paste0("rrgl/ufo", stringr::str_pad(ii, 4, pad="0"), ".png"))
  
}

#PART 7 - SUNRISE ----

save_sun_level = 99
save_bg_col = "#000000"

p7_frames = 90

p7_bg = rev(rep(bg_cols[-length(bg_cols)], each = p7_frames/(length(bg_cols)-1)))

p7_bg = c(p7_bg, rep(bg_cols[1], p7_frames-length(p7_bg)))

#Opposite of Part 6
phivec = scales::rescale(1/(1 + exp(seq(-2, 4, length.out = p7_frames))), c(0, pi/4))

for(ii in 541:(541+p7_frames-1)){
  sun_level = (p7_frames- (ii-541)) %/% 4
  
  bg_col = p7_bg[ii-540]
  
  if(sun_level != save_sun_level){
    rgl.clear()
    
    #Background
    readxl::read_xlsx(this_file, sheet = "bg_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Cloud
    readxl::read_xlsx(this_file, sheet = "cloud_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.7, background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Sun
    readxl::read_xlsx(this_file, sheet = "sun_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
        increment_level = -sun_level
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  bg_col
      ) 
    
  }
  par3d(windowRect = c(20, 30, 900, 800), zoom = 0.9)
  par3d(userMatrix =  rotationMatrix(phivec[ii-540], 1,0,0) %*% U)
  
  save_sun_level <- sun_level
  save_bg_col <- bg_col
  
  rgl::rgl.snapshot(paste0("rrgl/ufo", stringr::str_pad(ii, 4, pad="0"), ".png"))
  
}

#PART 8 - DROP COW ----
save_cow_level = 99

p8_frames = 60

for(ii in 631:(631+p8_frames-1)){
  cow_level = (p8_frames- (ii-631)) %/% 2
  
  if(cow_level != save_cow_level){
    rgl.clear()
    
    #Background
    readxl::read_xlsx(this_file, sheet = "bg_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Cloud
    readxl::read_xlsx(this_file, sheet = "cloud_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks")
      ) %>% 
      build_bricks(
        trans_alpha = 0.7, background_color =  "#87ceeb" #"#003152"
      ) 
    
    #Sun
    readxl::read_xlsx(this_file, sheet = "sun_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
        increment_level = 0
      ) %>% 
      build_bricks(
        trans_alpha = 0.3,
        background_color =  bg_cols[1]
      ) 
    
    readxl::read_xlsx(this_file, sheet = "cow_colors") %>% 
      bricks_from_excel(
        piece_table = readxl::read_xlsx(this_file, sheet = "master_bricks"),
        increment_level = cow_level
      ) %>% 
      build_bricks(
        #rgl_lit = F, outline_bricks = T,
        trans_alpha = 0.3,
        background_color = bg_cols[1]
      ) 
    
  }
  par3d(windowRect = c(20, 30, 900, 800), zoom = 0.9)
  par3d(userMatrix =  U)
  
  save_cow_level <- cow_level
  
  rgl::rgl.snapshot(paste0("rrgl/ufo", stringr::str_pad(ii, 4, pad="0"), ".png"))
  
}

for(ii in 691:750){
  rgl::rgl.snapshot(paste0("rrgl/ufo", stringr::str_pad(ii, 4, pad="0"), ".png"))
}

av::av_encode_video(paste0("rrgl/", list.files("rrgl/")), 
                    "brickr_cow1.mp4", framerate = 30)


