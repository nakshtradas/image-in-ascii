library(tidyverse)
library(imager)
library(Cairo)
library(forcats)
library(gtools)
library(imager)
library(nabor)
library(cowplot) 
im <- load.image("http://clipart-library.com/images/pi7rLno4T.jpg") 
plot(im)
asc <- gtools::chr(38:126) #We use a subset of ASCII, R doesn't render the rest
head(asc,10)
txt <- imfill(50,50,val=1) %>% implot(text(20,20,"Blah")) 
txt
plot(txt,interp=FALSE)
g.chr <- function(chr) implot(imfill(50,50,val=1),text(25,25,chr,cex=5)) %>% grayscale %>% mean
g <- map_dbl(asc,g.chr)
n <- length(g)
plot(1:n,sort(g),type="n",xlab="Order",ylab="Lightness")
text(1:n,sort(g),asc[order(g)])
char <- asc[order(g)]
d <- grayscale(im) %>% imresize(.1)  %>% as.data.frame
d <- mutate(d,qv=cut_number(value,n) %>% as.integer)
d <- mutate(d,char=char[qv])
ggplot(d,aes(x,y))+geom_text(aes(label=char),size=1)+scale_y_reverse()
psize <- 5
cen <- ceiling(psize/2)
gr <- grayscale(im) %>% pixel.grid %>%
  filter((x %% psize)==cen,(y %% psize) == cen)
plot(im,xlim=c(200,250),ylim=c(200,250))
with(gr,points(x,y,cex=1,col="red"))
ptch <- extract_patches(im,gr$x,gr$y,psize,psize)
ptch
syms <- 1:25
render <- function(sym) implot(imfill(psize,psize,val=1),points(cen,cen,pch=sym,cex=1)) %>% grayscale
ptch.c <- map(syms,render)
plot(ptch.c[[4]],interp=FALSE)
Pim <- map(ptch,as.vector) %>% do.call(rbind,.) 
Pc <- map(ptch.c,as.vector) %>% do.call(rbind,.)
nn <- nabor::knn(Pc,Pim,1)$nn.idx
mutate(gr,sym=syms[nn]) %$% plot(x,y,pch=sym,cex=.2,ylim=c(height(im),1))
extract.all <- function(im,psize=5)
{
  cen <- ceiling(psize/2)
  gr <- pixel.grid(R(im)) %>% filter((x %% psize)==cen,(y %% psize) == cen) 
  extract_patches(im,gr$x,gr$y,psize,psize)
}
match.patches <- function(dict,im)
{
  psize <- height(dict[[1]]) 
  im <- resize(im,(width(im) %/% psize)*psize,(height(im) %/% psize)*psize)
  pim <- extract.all(im,psize)
  Pa <- map(dict,as.vector) %>% do.call(rbind,.)
  Pb <-  map(pim,as.vector) %>% do.call(rbind,.)
  nn <- nabor::knn(Pa,Pb,1)$nn.idx %>% c
  imappend(dict[nn],"x") %>% imsplit("x",-width(im)) %>% imappend("y") 
}

