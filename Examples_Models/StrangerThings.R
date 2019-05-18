# Build brickr models using the Excel "brickr_starterkit" template

# MAKE SURE TO INSTALL THE LATEST {brickr} VERSION
#install.packages("devtools")
#devtools::install_github("ryantimpe/brickr")

# Also install the latest readxl
# install.packages("readxl")

library(brickr)

#The Excel file has 4 example sets, all created (mostly) in the same way. 

#Upside up 
up_set <- readxl::read_xlsx("Examples_Models/StrangerThings.xlsx", sheet = "UpsideUp")
dn_set <- readxl::read_xlsx("Examples_Models/StrangerThings.xlsx", sheet = "UpsideDown")

bricks_up <- up_set %>% 
  bricks_from_excel()

# bricks_up %>% display_bricks()

bricks_dn <- dn_set %>% 
  bricks_from_excel() 

# bricks_dn %>% display_bricks()

full_set_up <- list(
  Img_bricks = bind_rows(list(
    bricks_dn$Img_bricks %>%
      mutate(Level = max(Level) - Level + 1)),
    bricks_up$Img_bricks %>% 
      mutate(Level = Level + max(bricks_dn$Img_bricks$Level))),
  ID_bricks = bind_rows(list(
    bricks_dn$ID_bricks %>%
      mutate(Level = max(Level) - Level + 1)),
    bricks_up$ID_bricks %>% 
      mutate(Level = Level + max(bricks_dn$Img_bricks$Level))),
  Img_lego = bind_rows(list(
    bricks_dn$Img_lego %>%
      mutate(Level = max(Level) - Level + 1)),
    bricks_up$Img_lego %>% 
      mutate(Level = Level + max(bricks_dn$Img_bricks$Level)))
)

full_set_down <- full_set_up %>% 
  purrr::map(~mutate(.x, Level = max(Level) - Level + 1))

full_set_up %>% 
  display_bricks()

#Animate!

nframes = 480

orientation <- c(rep("up", 200), rep("down", 220), rep("up", 60))
thetavec <- c(rep(10, 60), seq(10, 190, length.out = 90),
              rep(190, 120), seq(190, 370, length.out = 90),
              rep(370, 120))
zoomvec <- c(seq(50, 100, length.out = 60)/100,
             rep(1, 240),
             seq(100, 50, length.out = 120)/100,
             rep(.5, 60)
             )

for(oo in c("down")){
  if(oo == "up"){
    full_set_up %>% 
      display_bricks(background = "#92B7FE")
  } else {
    full_set_down %>% 
      display_bricks(background = "#222266")
  }
  
  for(ii in 1:nframes){
    if(orientation[ii] == oo){
      rayshader::render_camera(phi = 5, theta = thetavec[ii], zoom = zoomvec[ii])
      rayshader::render_snapshot(paste0("frame", ii, ".png"))
    }
  }
  rgl::rgl.clear()
}
