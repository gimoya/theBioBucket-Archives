# file: jpeg_transparency.R
# purpose: add transparency to jpeg
# author: kay cichini
# arguments: path_to_jpeg, path_to_outfile, alpha (transparency 0-1)
# path_to_outfile defaults to default home directory
# alpha defaults to 0.5
# packages used: jpeg, png
# input: a jpeg image
# output: a png image

jpeg_transp <- function(path_to_jpeg, alpha = 0.5,
                        path_to_outfile = path.expand("~/mynewimg.png")){

   require(jpeg)
   require(png)

   myjpeg <- readJPEG(path_to_jpeg)

# create new array with 4 dimensions, the new dimension
# representing alpha:
   newimg <- array(data = c(as.vector(myjpeg),
                   rep(alpha, dim(myjpeg)[1]*dim(myjpeg)[2])),
                   dim = c(dim(myjpeg)[1], dim(myjpeg)[2], 4))

   png(path_to_outfile,
       width = dim(myjpeg)[2],
       height = dim(myjpeg)[1])

# print newimg to plot region:
   par(mar = rep(0, 4), oma = rep(0, 4), new = F)
   plot(NA, xlim = c(0, 100), ylim = c(0, 100),
        xlab = "", ylab = "", axes = F, type = "n",
        yaxs ="i", xaxs = "i")

   rasterImage(newimg, 0, 0, 100, 100)
   graphics.off()
}

# Example:
# path_to_jpeg <- system.file("img", "Rlogo.jpg", package="jpeg")
# jpeg_transp(path_to_jpeg)

# open png:
# shell.exec(path.expand("~/mynewimg.png"))
