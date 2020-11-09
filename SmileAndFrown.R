ball <-
function (n, r = 1, hole = 0, x0 = 0, y0 = 0, from = 0, to = 1) 
{
	rPos  = sqrt(
			     runif(n) * 
			     (r^2 - hole^2) + 
			     hole^2
			     )

	theta = 2 * (from + runif(n) * (to - from)) * pi
	X     = rPos * cos(theta)
	Y     = rPos * sin(theta)

	me = list(
			  x = X + x0, 
			  y = Y + y0
			  )

	class(me) <- append(class(me), "jointSample")
	return(me)
}
circle <-
function (n, r = 1, x0 = 0, y0 = 0, from = 0, to = 1) 
{
	theta = 2*(from + runif(n) *(to - from)) * pi
	X     = r * cos(theta)
	Y     = r * sin(theta)

	me    = list(
				 x = X + x0, 
				 y = Y + y0
				 )

	class(me) <- append(class(me), "jointSample")
	return(me)
}
copula <-
function (js) 
{
	size = length(js$x) 	

	me = lapply(
		    	lapply(
			    		lapply(js, rank), 
				    	"[", 
					    lapply(js,order)$x 
    					),
	    		function(t){t/(size+1)}
		    	)

	class(me) <- append(class(me), "jointSample")
	class(me) <- append(class(me), "copula")
	return(me) 
}
dump.functions <-
function (file) 
{
	dump(functions(),file)
}
et <-
structure(function (v) 
.approxfun(x, y, v, method, yleft, yright, f, na.rm), class = c("ecdf", 
"stepfun", "function"), call = quote(ecdf(triangle(100)$y)))
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

        mouth      = circle( 
							mouthCount, 
                            mouthRadius, 
                            0, 
                            -0.8, 
                            mouthStart , 
                            mouthEnd
                            )

        me     = list(
                      x = c(main$x, leftEye$x, rightEye$x, mouth$x),
                      y = c(main$y, leftEye$y, rightEye$y, mouth$y)
                      )

    	class(me) <- append(class(me), "jointSample")
		return(me)
}
functions <-
function()
{
	objs = objects(envir = globalenv() )

	objs[
		 unlist(
			    lapply(
					   lapply(
						      lapply(
								     objs,
									 function(t)parse(text=t)
									 ),
							   eval),
						typeof
						)
				)
				==
				"closure"
		]
}
jointSamples <-
function()
{
	objs = objects(envir = globalenv() )

	objs[
		 unlist(
			    lapply(
					   lapply(
						      lapply(
								     objs,
									 function(t)parse(text=t)
									 ),
							   eval),
						class
						)
				)
				==
				"jointSample"
		]
}
myCdf <-
function (x) 
{
	sx = sort(x)
	rl = rle(sx)
	l  = rl$lengths
	ll = length(l)
 	myX = rep(rl$values, l)
	myY = cumsum(rep(rep(1,ll),l))
	list( x = myX, y = myY )
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

	me = list(
			  x = X + x0, 
			  y = Y + y0
			  )

	class(me) <- append(class(me), "jointSample")
	return(me)
}
plot.jointSample <-
function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL, 
    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
    ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL, 
    panel.last = NULL, asp = NA, xgap.axis = NA, ygap.axis = NA, 
    pch=20, cex=0.1, all=F, ...) 
{
	if(all)
	{
		plot.default(
				 x           = x, 
				 y           = y, 
				 type        = type, 
				 xlim        = xlim, 
				 ylim        = ylim, 
    				 log         = log, 
				 main        = main, 
				 sub         = sub, 
				 xlab        = xlab, 
				 ylab        = ylab, 
    				 ann         = ann, 
				 axes        = axes, 
				 frame.plot  = frame.plot, 
				 panel.first = panel.first, 
    				 panel.last  = panel.last, 
				 asp         = asp, 
				 xgap.axis   = xgap.axis, 
				 ygap.axis   = ygap.axis, 
		      	 pch         = 20, 
				 cex         = 0.1
				)
		tmpY = myCdf(x$y)
		plot(list(x = tmpY$y, y = tmpY$x),pch=20,cex=.1)
		plot(myCdf(x$x),pch=20,cex=.1)
		plot(copula(x))
	}
	else
	{
		plot.default(
				 x           = x, 
				 y           = y, 
				 type        = type, 
				 xlim        = xlim, 
				 ylim        = ylim, 
    				 log         = log, 
				 main        = main, 
				 sub         = sub, 
				 xlab        = xlab, 
				 ylab        = ylab, 
    				 ann         = ann, 
				 axes        = axes, 
				 frame.plot  = frame.plot, 
				 panel.first = panel.first, 
    				 panel.last  = panel.last, 
				 asp         = asp, 
				 xgap.axis   = xgap.axis, 
				 ygap.axis   = ygap.axis, 
		      	 pch         = 20, 
				 cex         = 0.1
				)
	}
}
print.jointSample <-
function (x, ...) 
{
    cat("Joint Sample\n")
    NextMethod("print", x)
    invisible(x)
}
randCopula <-
function (cop, sample) 
{
	size = length(cop$x)
	ss = lapply(sample, sort)
	sCopX = ss$x[cop$x * (size + 1)]
	sCopY = ss$x[cop$y * (size + 1)]

	me = list(
		      x   = sCopX, 
		      y   = sCopY,
			lx  = length(sCopX),
			ly  = length(sCopY),
			ss  = ss,
			cop = cop
		      )

	class(me) <- append(class(me), "jointSample")
	return(me)
}
smile <-
function ( 
	      n,
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

	mouth      = circle(
		                mouthCount, 
				        mouthRadius, 
                        0, 
                        0, 
                        mouthStart , 
                        mouthEnd
                        )

	me =   list(
        		 x = c(main$x, leftEye$x, rightEye$x, mouth$x),
       	         y = c(main$y, leftEye$y, rightEye$y, mouth$y)
	          	 )

	class(me) <- append(class(me), "jointSample")
	return(me)
}
square <-
function (n) 
{
	me = list(
		    x = runif( n),
	          y = runif( n)
		    )

      class(me) <- append(class(me), "jointSample")
      return(me)
}
triangle <-
function (n, rev = F) 
{
	y = runif( n)

	me = list(
		    x = y/2 + runif( n) * (1 - y),
	          y = y
		    )

      class(me) <- append(class(me), "jointSample")
      return(me)
}
tt <-
structure(function (v) 
.approxfun(x, y, v, method, yleft, yright, f, na.rm), class = c("ecdf", 
"stepfun", "function"), call = quote(ecdf(sort(x$y))))
