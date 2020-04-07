# install.packages("brickr")

library(brickr)
library(rgl)
library(httr)

#Render static model ----

GET("http://www.ryantimpe.com/files/brickr_release3.xlsx", 
    write_disk(tf <- tempfile(fileext = ".xlsx")))

readxl::read_xlsx(tf, sheet = "brickr3") %>% 
  bricks_from_excel(
    piece_table = readxl::read_xlsx(tf, sheet = "brickr3_p")
  ) %>% 
  build_bricks(#rgl_lit = FALSE, outline_bricks = TRUE,
               background_color =  "#99e7ff")

par3d(windowRect = c(20, 30, 900, 800))

#Manually move the model so that the brickr logo is the only thing visible
# I used dput() to save the proper view below
# U <- par3d("userMatrix")
# U <- round(U)

U <- structure(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), 
               .Dim = c(4L, 4L))

par3d(userMatrix = U,
      zoom = 0.8)

# Animation ----
# There are 6 sections to the animation
# Values here are the # of frames
a1 = 90   # frames logo
a2 = 60   # frames transition to pi/2 - 0.1
a3 = 60   # frames zoom
a4 = 180  # frames rotate 360deg (120 no zoom)
a5 = 60   # frames transition back + zoom out
a6 = 30   # frames logo

nframes = (a1+a2+a3+a4+a5+a6)

#Rotations about the Z axis (model flips toward the viewer)
max_phi = -(pi/2 - 0.05)
phivec = c(rep(0, a1),
           seq(0, max_phi, length.out = a2),
           rep(max_phi, a3+a4),
           rev(seq(0, max_phi, length.out = a5)),
           rep(0, a6)
           )
length(phivec) == nframes

#Rotations around the Y axis (model spins)
thetavec = c(rep(0, a1+a2+a3),
             seq(0, -2*pi, length.out = a4),
             rep(0, a5+a6))
length(thetavec) == nframes

# Zoom (smaller value = closer model)
max_zoom = 0.4
zoomvec = c(rep(0.8, a1+a2),
            seq(0.8, max_zoom, length.out = a3),
            rep(max_zoom, a4),
            rev(seq(0.8, max_zoom, length.out = a5)),
            rep(0.8, a6)
            )

#Loop over all the frame, changing the view to the phi, theta, zoom
for(ii in 1:nframes){
  
  #I barely understand this matrix algebra but it works
  par3d(userMatrix = rotationMatrix(phivec[ii], 1,0,0) %*% rotate3d(U, thetavec[ii], 0, 0 ,1),
        zoom = zoomvec[ii]) # Rotate aboutviewer's z axis
  
  rgl::snapshot3d(paste0("rrgl/brickr", stringr::str_pad(ii, width=3, pad="0"), ".png"))
  
}

av::av_encode_video(paste0("rrgl/", list.files("rrgl/")), 
                    "brickr3.mp4", framerate = 20)
