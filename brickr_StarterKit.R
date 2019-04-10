# Build brickr models using the Excel "brickr_starterkit" template

# MAKE SURE TO INSTALL THE LATEST {brickr} VERSION
#install.packages("devtools")
#devtools::install_github("ryantimpe/brickr")

# Also install the latest readxl
# install.packages("readxl")

library(brickr)

#The Excel file has 4 example sets, all created (mostly) in the same way. 

#Penguin 
example_penguin <- readxl::read_xlsx("brickr_starterkit.xlsx", sheet = "Set_Penguin")

example_penguin %>% 
  bricks_from_excel(exclude_level = 1) %>% #If you don't want the base plate, exclude_level = 1
  display_bricks(background = "#7EC0EE")


#T.rex  
readxl::read_xlsx("brickr_starterkit.xlsx", sheet = "Set_Trex") %>% 
  bricks_from_excel() %>% 
  display_bricks(background = "#7EC0EE")

#Rocket 
readxl::read_xlsx("brickr_starterkit.xlsx", sheet = "Set_Rocket") %>% 
  bricks_from_excel() %>% 
  display_bricks(background = "#7EC0EE")

#House 
readxl::read_xlsx("brickr_starterkit.xlsx", sheet = "Set_House") %>% 
  #Here, the example only draws every other brick layer, we want to repeat each one
  bricks_from_excel(repeat_levels = 2) %>% 
  display_bricks(background = "#7EC0EE")
