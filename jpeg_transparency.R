# file: jpeg_transparency.R
# purpose: add transparency to jpeg
# author: kay cichini
# arguments: path_to_jpeg, outpath, alpha (transparency 0-1)
# outpath defaults to default home directory
# alpha defaults to 0.5
# packages used: jpeg, png
# input: a jpeg image
# output: a png image

jpeg_transp <- function(myjpeg, alpha = 0.5, outpath = NA){

   require(jpeg)
   require(png)

   if(is.na(outpath)) {outpath <- path.expand("~")}

# create new array with 4 dimensions, the new dimension
# representing alpha:
   newimg <- array(data = c(as.vector(myjpeg),
                   rep(alpha, dim(myjpeg)[1]*dim(myjpeg)[2])),
                   dim = c(dim(myjpeg)[1], dim(myjpeg)[2], 4))

# assign output file name and make png:
   mynewimg <- paste(tempfile(),".png", sep = "")
   png(mynewimg, 
       width = dim(myjpeg)[2]*5,
       height = dim(myjpeg)[1]*5)

# print newimg to plot region:
   par(mar = rep(0, 4), oma = rep(0, 4), new = F)
   plot(0, xlim = c(0, 100), ylim = c(0, 100),
        xlab = "", ylab = "", axes = F,
        yaxs ="i", xaxs = "i")

rasterImage(newimg, 0, 0, 100, 100)
dev.off()

shell.exec(mynewimg)
}

# example:
myjpeg <- readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))
jpeg_transp(myjpeg)