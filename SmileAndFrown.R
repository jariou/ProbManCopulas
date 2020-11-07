ball <-
function (n, r = 1, hole = 0, x0 = 0, y0 = 0, from = 0, to = 1) 
{
	rPos  = sqrt(
			  runif(n) * 
			  (r^2 - hole^2) + 
			  hole^2
			)
	theta = 2 * (from + runif(n) *(to - from)) * pi
	X     = rPos * cos(theta)
	Y     = rPos * sin(theta)

	list(x = X + x0, y = Y + y0)
}
circle <-
function (n,r=1,x0=0,y0=0, from=0, to=1) 
{
	theta = 2*(from + runif(n) *(to - from)) * pi
	X     = r * cos(theta)
	Y     = r * sin(theta)
	list(x = X + x0, y = Y + y0)
}
copula <-
function (js) 
{
	size = length(js$x) 	
	lapply(
		lapply(
			lapply(js, rank), 
			"[", 
			lapply(js,order)$x 
			),
		function(t){t/(size+1)}
		)
}
dump.functions <-
function (file) 
{
	dump(functions(),file)
}
frown <-
function ( n,
           mainRadius  = 1,
           eyeRadius   = 0.25,
           mouthRadius = 0.7,
           mouthStart  = 0.05,
           mouthEnd    = 0.45
           ) 
{
        totalLength = mainRadius +
                      2 * eyeRadius +
                      mouthRadius * (mouthEnd - mouthStart)
  
        mouthCount = ceiling(n * mouthRadius * (mouthEnd - mouthStart) / totalLength)
        eyeCount   = ceiling(n * eyeRadius / totalLength)
        mainCount  = n - mouthCount - 2 * eyeCount

        main       = circle(mainCount, mainRadius)
        leftEye    = circle(eyeCount, eyeRadius, -0.4, 0.4)
        rightEye   = circle(eyeCount, eyeRadius,  0.4, 0.4)

        mouth      = circle( mouthCount, 
                             mouthRadius, 
                             0, 
                             -0.8, 
                             mouthStart , 
                             mouthEnd
                             )

        retVal     = list(
                          x = c(main$x, leftEye$x, rightEye$x, mouth$x),
                          y = c(main$y, leftEye$y, rightEye$y, mouth$y)
                          )

        class(retVal) = "jointSample"
        invisible(retVal)
}
functions <-
function()
{
	objs = objects(envir = globalenv() )

	objs[unlist(lapply(lapply(lapply(objs,function(t)parse(text=t)),eval),typeof))=="closure"]
}
normal <-
function (n, r = 1, hole = 0, x0 = 0, y0 = 0, from = 0, to = 1) 
{
	rPos  = sqrt(
			  runif(n) * 
			  (r^2 - hole^2) + 
			  hole^2
			)
	theta = 2 * (from + runif(n) *(to - from)) * pi
	X     = rPos * cos(theta)
	Y     = rPos * sin(theta)

	list(x = X + x0, y = Y + y0)
}
print.jointSample <-
function (x, ...) 
{
    cat("Joint Sample\n")
    print(unclass(x), ...)
    invisible(x)
}
randCopula <-
function (cop, sample) 
{
	size = length(cop$x)
	ss = lapply(sample, sort)
	list(
		x = ss$x[cop$x *(size + 1)], 
		y = ss$y[cop$y *(size + 1)]
		)
}
smile <-
function ( n,
	     mainRadius  = 1,
	     eyeRadius   = 0.25,
	     mouthRadius = 0.7,
     	     mouthStart  = 0.55,
	     mouthEnd    = 0.95
 	     ) 
{
	totalLength = mainRadius +
	     		  2 * eyeRadius +
		        mouthRadius * (mouthEnd - mouthStart)
  
	mouthCount = ceiling(n * mouthRadius * (mouthEnd - mouthStart) / totalLength)
	eyeCount   = ceiling(n * eyeRadius / totalLength)
	mainCount  = n - mouthCount - 2 * eyeCount

	main       = circle(mainCount, mainRadius)
	leftEye    = circle(eyeCount, eyeRadius, -0.4, 0.4)
	rightEye   = circle(eyeCount, eyeRadius,  0.4, 0.4)

	mouth      = circle(mouthCount, 
				  mouthRadius, 
                          0, 
                          0, 
                          mouthStart , 
                          mouthEnd
                          )

	tmp       =   list(
             		 x = c(main$x, leftEye$x, rightEye$x, mouth$x),
       		       y = c(main$y, leftEye$y, rightEye$y, mouth$y)
	            	 )

	class(tmp) = "jointSample"
	invisible(tmp)
}
square <-
function (n) 
{
	list(
		x = runif(2*n),
		y = runif(3*n)
		)
}
