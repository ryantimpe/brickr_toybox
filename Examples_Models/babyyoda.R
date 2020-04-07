library(dplyr)
library(brickr)
library(rgl)
library(httr)

#Render a static  model
GET("http://www.ryantimpe.com/files/babyyoda.xlsx", write_disk(tf <- tempfile(fileext = ".xlsx")))
babyyoda <- readxl::read_xlsx(tf, sheet = "BabyYoda") %>% 
  bricks_from_excel() 

babyyoda %>% 
  build_bricks(rgl_lit = F, outline_bricks = T, background_color = "#8a496b")

#Set up window parameters
U <- par3d("userMatrix")
U <- round(U)

par3d(windowRect = c(20, 30, 800, 600), zoom = .8)

#Set up animation
num_frames <- 480

max_phi <- pi/4
phivec = c(rep(pi/16, 240), 
           seq(pi/16, max_phi, length.out = 60), 
           rep(max_phi, 60), 
           seq(max_phi, pi/16, length.out = 60), 
           rep(pi/16, 60))

thetavec = c(rep(0, 120), 
             rev(seq(0, 2*pi, length.out = 300)), 
             rep(0, 60))

zoomvec = c(rep(0.8, 240), 
            seq(0.8, 1, length.out = 60),
            rev(seq(0.8, 1, length.out = 60)),
            rep(0.8, 120))

rglvec = rep(c(rep(101, 20), rep(102, 20), rep(103, 20)), 8)


#Animation ----
#Loop over 3 different force waves
for(rr in 101:103){
  readxl::read_xlsx(tf, sheet = "BabyYoda") %>% 
    mutate_at(vars(user_color), list(~ifelse(.==101, rr, .))) %>% 
    bricks_from_excel() %>% 
    build_bricks(rgl_lit = F, outline_bricks = T, background_color = "#8a496b")
  
  par3d(windowRect = c(20, 30, 800, 600), zoom = .8)
  
  #Loop over window parameters
  for(ii in 1:num_frames){

    if(rglvec[ii] == rr){
      # par3d(userMatrix = rotate3d(U, thetas[ii], 0, 0 ,1)) # Rotate about model's z axis
      
      par3d(userMatrix = rotationMatrix(phivec[ii], 1,0,0) %*% rotate3d(U, thetavec[ii], 0, 0 ,1),
            zoom = zoomvec[ii]) # Rotate aboutviewer's z axis
      
      Sys.sleep(0.1)
      rayshader::render_snapshot(paste0("rrgl/yoda", stringr::str_pad(ii, width=3, pad="0"), ".png"))
    } else {next}
    
  }
  rgl::rgl.clear()
  
}


av::av_encode_video(paste0("rrgl/", list.files("rrgl/")), 
                    "babyyoda.mp4", framerate = 30)

#Instructions
p <- babyyoda %>% 
  build_instructions(num_steps = 20)

p +
  ggplot2::theme(panel.grid = element_blank())

ggplot2::ggsave("babyyoda_instructions.png", device="png", width = 6, height = 6)

#Bricks
babyyoda %>% 
  build_pieces()

ggplot2::ggsave("babyyoda_pieces.png", device="png", width = 8, height = 4)
