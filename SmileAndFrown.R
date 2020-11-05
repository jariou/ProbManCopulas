circle <-
function (n,r=1,x0=0,y0=0, from=0, to=1) 
{
	theta = 2*(from + runif(n) *(to - from)) * pi
	X     = r * cos(theta)
	Y     = r * sin(theta)
	list(x = X + x0, y = Y + y0)
}
frown <-
function (n) 
{
	main     = circle(n,1)
	leftEye  = circle(n,.25,-.4,.4)
	rightEye = circle(n,.25,.4,.4)
	mouth    = circle(n,.7, 0, -.8, .05, .45)
	retVal   = list(
		          x = c(main$x, leftEye$x, rightEye$x, mouth$x),
		          y = c(main$y, leftEye$y, rightEye$y, mouth$y)
		          )
	class(retVal) = "jointSample"
	retVal
}
smile <-
function (n) 
{
	main     = circle(n,1)
	leftEye  = circle(n,.25,-.4,.4)
	rightEye = circle(n,.25,.4,.4)
	mouth    = circle(n,.7, 0, 0, .55, .95)
	list(
		x = c(main$x, leftEye$x, rightEye$x, mouth$x),
		y = c(main$y, leftEye$y, rightEye$y, mouth$y)
		)

}
print.jointSample <-
function (x, ...) 
{
    cat("Joint Sample\n")
    print(unclass(x), ...)
    invisible(x)
}
