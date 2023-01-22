############################################################
#
# Author: DeAndre Thomas
# Purpose: Week 9 Lab
#   RGL 3D Visualizatiom
#   use ist719NetworkObject.rda
#
#############################################################
library(igraph)
library(rgl)

my.dir<- "C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 9\\"
load(paste0(my.dir, "ist719NetworkObject.rda"))

g

coords<- layout_with_kk(g, dim = 3)
rglplot(g, layout = coords)

l<- layout_with_kk(g)
V(g)$x<- l[,1]
V(g)$y<- l[,2]
V(g)$z<- V(g)$bet
rglplot(g)

V(g)$label<-""

V(g)$x<- coords[,1]
V(g)$y<- coords[,2]
V(g)$z<- coords[,3]

rglplot(g)
par3d(windowRect = c(100, 100, 640, 640))
rgl.bringtotop()
rgl.bg(color = "black") #sets color
rgl.viewpoint(0,20) #sets to uniform space

E(g)$color = "yellow"
E(g)$width<- .5
V(g)$label<- ""
rglplot(g)
####################################################################
#
#   RGL & Network Animation
#   Use ist719NetworkObject.rda
#
###################################################################
out.dir<- "C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 9\\"
library(stringr)
install.packages("animation")
library(animation)
library(rgl)
library(igraph)
library(plotrix)
g

rglplot(g)

my.rgl.out<- paste0(out.dir, "Network3DVisualization.png")
rgl.snapshot(filename = my.rgl.out)

rglplot(g)
par3d(windowRect = c(100,100,500,500))
rgl.bringtotop()
rgl.bg(color = "black")
rgl.viewpoint(0,0, zoom = 0.7)

max.loops<- 360
my.angle<- rescale(1:max.loops, c(-90, 90))
for(i in 1:max.loops) {
  rgl.viewpoint(theta = -my.angle[i]
                , phi = my.angle[i] * 0.7
                , zoom = 0.75 - i/(max.loops * 1.7))
  # Theta is spinning around y, phi is spinning aroung x
}

install.packages("ffmpeg")
images.out<- paste0(out.dir, "out\\")
rglplot(g)
par3d(windowRect = c(100,100,500,500))
rgl.bringtotop()
rgl.bg(color = "black")
rgl.viewpoint(0,0, zoom = 0.7)

max.loops<- 40

my.angle<- rescale(1:max.loops, c(-180, 180))
for(i in 1:max.loops) {
  # i <-1 + i
  rgl.viewpoint(theta = -my.angle[i]
                , phi = 0
                , zoom = 0.75 - i/(max.loops * 1.7))
  # Theta is spinning around y, phi is spinning aroung x
  snapshot.fname<- paste(0, images.out, "network", str_pad(i, width = 4
                                                           , side = "left"
                                                           , pad = "O")
                                                           , ".png")
  rgl.snapshot(filename = my.rgl.out)
}

ani.options(interval = .1)
imgs<- list.files(images.out, pattern = "*.png")
saveGIF({
  for(img in imgs){
    im<- magick::image_read(paste0(images.out, img))
    print(im)
  }
}, movie.name = paste0(my.rgl.out, "ClassNetwork.gif"))

ani.options(ffmpeg = "C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 9\\video\\ffmpeg\\bin\\ffmpeg.exe")
savevideo({
  for(img in imgs){
    im<- magick::image_read(paste0(images.out, img))
    plot(im)
    #plot(as.raste(im))
  }
}, video.name = paste0(my.rgl.out, "ClassNetwork.mp4")
, other.opts = "-vf format=yuv420p")
##########################################################################
#
#   RGL & Network Animation
#   Use ist719NetworkObject.rda
#
#########################################################################

library(rgl)
library(scatterplot3d)

n<- 1000
x<- rnorm(n)
y<- ((2*x)^2)/10+ rnorm(n, mean = 0, sd = .2)
z<- sqrt(abs(x))+ rnorm(n, mean = 0, sd = .2)

scatterplot3d(x,y,z, pch = 16, type = "h")
plot3d(x,y,z, col = "red", size = 3) #rgl version of above plot
plot3d(x,y,z, col = "gold", size = 1, type = "s")
rgl.bg(color = "black")
rgl.viewpoint(0,0,, zoom = .7)

rgl.clear(type = "shapes") #clears the plot space

spheres3d(4*x, 4*y, 4*z, radius = .1
          , color = "gold")

rgl.light(theta = 0, phi = 0
          , viewpoint.rel = TRUE
          , ambient = "#FFFFFF"
          , diffuse = "#FFFFFF" #what light is being reflected back
          , specular = "#FFFFFF") #the sharpness of the light that is relfected
rgl.clear(type = "lights") #takes all light out of plot

light3d(diffuse = "gray75"
        , specular = "gray 75"
        , viewpoint.rel = FALSE)

rgl.light(ambient = "#444444"
          , diffuse = "#0000FF" 
          , specular = "#FF0000")

######################################################################
#
#   RGL & Shapes, the environment and materials
#
######################################################################
library(rgl)
open3d()
par3d()$windowRect

lines3d(x = c(-2, 2), y = c(0,0), z = c(0,0)
        , col = "red", lwd = 1)
text3d(2.2, 0,0,"x", col = "red", cex = 1, adj = 0)

lines3d(x = c(0, 0), y = c(-2,2), z = c(0,0)
        , col = "green", lwd = 1)
text3d(0,2.2,0,"y", col = "green", cex = 1, adj = 0)

lines3d(x = c(0,0), y = c(0,0), z = c(-2,2)
        , col = "blue", lwd = 1)
text3d(0, 0,2.2,"z", col = "red", cex = 1, adj = 0)

wire3d(cube3d())

wire3d(scale3d(cube3d(), 2,2,2), col = "red")

rgl.ids()
rgl.pop(id = 2921) #gets rid of elements in frame

rgl.clear(type = "shapes")

shapelist3d(tetrahedron3d(), 2,0,0, size = .5, color = "tan")

material3d(alpha = .2, shininess = 75, emision = "blue")
shapelist3d(cube3d(), 0,2,0, size = .2, color = "cadetblue")

material3d(alpha = 1, shininess = 5, emision = "black")
shapelist3d(octahedron3d(), 0,0,2.5, size = .9, color = "red")

material3d(alpha = .75, shininess = 50, emision = "black")
shapelist3d(icosahedron3d(), 2,0,2.5, size = 2, color = "orange")

#######################################################################
#
#   RGL: A Simple Scene
#
#######################################################################

library(rgl)
web.dir<- "C:\\Users\\deand\\OneDrive\\Documents\\SyrU ADS\\IST719\\Lab 9\\web\\"

open3d()
material3d(alpha = 1, shininess = 50, emission = "black")
rgl.bg(color = "black")

rgl.clear(type = "shapes")

n<- 10
x<- 0
y<- 0
z.start<- 0
z.end<- 0

M<- matrix(c(rep(x,n), rep(y,n)
             , seq(from = z.start, to = z.end
             , length.out = n)), nrow = n, byrow = FALSE)
M2<- cylinder3d(center = M, radius = .2, sides = 50, closed = -2)
shade3d(M2, alpha = 1, color = "gray45")

z.end<- .25

M2<- cylinder3d(center = M, radius = 1.5,  sides = 50, closed = -2)
shade3d(M2, alpha = 1, color = "gray45")

material3d(alpha = 1, shininess = 50, emission = "tan")
spheres3d(x, y, 10.5, radius = .25, col = "yellow")

material3d(alpha = .5, shininess = 50, emission = "black")
shapelist3d(cuboctahedron3d(), x, y, 10.5, size = .575, color = "gold")

material3d(alpha = 1, shininess = 50, emission = "black")

M<- matrix(c(rep(x,n), rep(y,n)
             , seq(from = 11, to = 11.5
                   , length.out = n)), nrow = n, byrow = FALSE)

m4<- cylinder3d(center = M, radius = .75, sides = 50, closed = -2)
shade3d(m4, alpha = 1, color = "gray45")

ns<- 100
xs<- rnorm(ns, 0, 3)
ys<- rnorm(ns, 0, 3)
zs<- rnorm(ns, 9, 2)
rs<- rnorm(ns, 1, .3)
material3d((alpha =1, shininess = 100, emission = "black")
particles3d(xs, ys, zs, radius = rs, color = "white")

writeWebGL(dir = web.dir, filename = index.html)
filename<- writeWebGL(dir = web.dir, width = 500, reuse = 500)

######################################################################
#
#   Package Management
#
######################################################################

writeOBJ()
readOBJ()

readSTL()
writeSTL()

tmp<- installed.packages()
ip<- as.data.frame(tmp, stringAsFactor = FALSE)

View(ip)
table(ip$LibPath)

.libPaths()
ip$Package[grep(patter = "^gg", ip$Package)]

sessionInfo()

tmp<- sessionInfo()
tmp$

#library(igraph) to load
#detach(igraph) to unload

memory.size()
gc()
