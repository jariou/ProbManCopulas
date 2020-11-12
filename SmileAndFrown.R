##################################################
# Simple simulation models                       #
##################################################
ball <-
function(n, r = 1, hole = 0, x0 = 0, y0 = 0, from = 0, to = 1) {
   radius_pos  <- sqrt(
                        runif(n) *
                        (r^2 - hole^2) +
                        hole^2
                        )

   theta <- 2 * (from + runif(n) * (to - from)) * pi
   x_pos     <- radius_pos * cos(theta)
   y_pos     <- radius_pos * sin(theta)

   me <- list(
           x = x_pos + x0,
           y = y_pos + y0
           )

   class(me) <- append(class(me), "jointSample")
   return(me)
}

##################################################
circle <-
function(n, r = 1, x0 = 0, y0 = 0, from = 0, to = 1) {
   theta <- 2 * (from + runif(n) * (to - from)) * pi
   x_pos <- r * cos(theta)
   y_pos <- r * sin(theta)

   me    <- list(
                 x = x_pos + x0,
                 y = y_pos + y0
                 )

   class(me) <- append(class(me), "jointSample")
   return(me)
}

##################################################
square <-
function(n) {
   me <- list(
              x = runif(n),
              y = runif(n)
              )

      class(me) <- append(class(me), "jointSample")
      return(me)
}

##################################################
triangle <-
function(n, rev = F) {
   y  <- runif(n)

   me  <- list(
               x = y / 2 + runif(n) * (1 - y),
               y = y
               )

      class(me) <- append(class(me), "jointSample")
      return(me)
}

##################################################
smile <-
function(
         n,
         main_radius  = 1,
         eye_radius   = 0.25,
         mouth_radius = 0.7,
         mouth_start  = 0.55,
         mouth_end    = 0.95
         ) {
   total_length  <- main_radius +
                    2 * eye_radius +
                    mouth_radius * (mouth_end - mouth_start)

   mouth_count  <- ceiling(
                           n * mouth_radius * (mouth_end - mouth_start)
                           /
                           total_length
                           )
   eye_count    <- ceiling(n * eye_radius / total_length)
   main_count   <- n - mouth_count - 2 * eye_count

   main        <- circle(main_count, main_radius)
   left_eye     <- circle(eye_count, eye_radius, -0.4, 0.4)
   right_eye    <- circle(eye_count, eye_radius,  0.4, 0.4)

   mouth       <- circle(
                         mouth_count,
                         mouth_radius,
                         0,
                         0,
                         mouth_start,
                         mouth_end
                         )

   me  <-   list(
                 x = c(main$x, left_eye$x, right_eye$x, mouth$x),
                 y = c(main$y, left_eye$y, right_eye$y, mouth$y)
                 )

   class(me) <- append(class(me), "jointSample")
   return(me)
}

##################################################
frown <-
function(n,
         main_radius  = 1,
         eye_radius   = 0.25,
         mouth_radius = 0.7,
         mouth_start  = 0.05,
         mouth_end    = 0.45
         ) {
   total_length <- main_radius +
                  2 * eye_radius +
                  mouth_radius * (mouth_end - mouth_start)
   mouth_count  <- ceiling(
                           n * mouth_radius *
                           (mouth_end - mouth_start)
                           /
                           total_length
                           )
   eye_count    <- ceiling(n * eye_radius / total_length)
   main_count   <- n - mouth_count - 2 * eye_count

   main         <- circle(main_count, main_radius)
   left_eye     <- circle(eye_count, eye_radius, -0.4, 0.4)
   right_eye    <- circle(eye_count, eye_radius,  0.4, 0.4)

   mouth        <- circle(
                          mouth_count,
                          mouth_radius,
                          0,
                          -0.8,
                          mouthStart,
                          mouthEnd
                          )
   me     <- list(
                  x = c(main$x, left_eye$x, right_eye$x, mouth$x),
                  y = c(main$y, left_eye$y, right_eye$y, mouth$y)
                  )

   class(me) <- append(class(me), "jointSample")
   return(me)
}


##################################################
copula <-
function(joint_sample) {
   size <- length(joint_sample$x)
   print(paste("----> size is ",size))

   str(joint_sample)
   tmp_0 <- lapply(joint_sample, rank)
   tmp_1 <- lapply(
                   tmp_0,
                   "[",
                   lapply(joint_sample, order)$x
                   )
   me <- lapply(
                tmp_1,
                function(t) {
                             t / (size + 1)
                             }
                )

   class(me) <- append(class(me), "jointSample")
   class(me) <- append(class(me), "copula")
   return(me)
}


##################################################
dump.functions <-
function(file) {
   dump(functions(), file)
}


##################################################
# Return a list of functions
functions <-
function() {
   objs  <- objects(envir = globalenv())

   objs[
       unlist(
             lapply(
                  lapply(
                        lapply(
                             objs,
                            function(t)parse(text = t)
                            ),
                        eval),
                  typeof
                  )
            )
            ==
            "closure"
      ]
}

##################################################
# List all jointSample objects
joint_samples <-
function() {
   objs  <- objects(envir = globalenv())

   objs[
        unlist(
               lapply(
                      lapply(
                             lapply(
                                    objs,
                                    function(t)parse(text = t)
                                    ),
                             eval
                             ),
                      class
                     )
              )
              ==
              "jointSample"
        ]
}

##################################################
# Simpleimplementation of empirical cdf
my_cdf <-
function(x) {
   sx  <- sort(x)
   rl  <- rle(sx)

   l   <- rl$lengths
   ll  <- length(l)

   my_x  <- rep(rl$values, l)
   my_y  <- cumsum(rep(rep(1, ll), l))

   list(x = my_x, y = my_y)
}



##################################################
# Simulate a joint normal distribution
normal <-
function(n, r = 1, hole = 0, x0 = 0, y0 = 0, from = 0, to = 1) {
   r_pos   <- sqrt(
                  runif(n) *
                  (r^2 - hole^2) +
                  hole^2
                  )
   theta  <- 2 * (from + runif(n) * (to - from)) * pi
   x      <- r_pos * cos(theta)
   y      <- r_pos * sin(theta)

   me  <- list(
           x = x + x0,
           y = y + y0
           )

   class(me) <- append(class(me), "jointSample")
   return(me)
}

##################################################
# Plot a joint sample
plot.jointSample <-
function(
         x,
         pch         = 20,
         cex         = 0.1,
         all         = F,
         ...
         ) {
   if (all) {
      # Setup the 4 plot canvases
      par(mfrow = c(2, 2), mar = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))

      # Plain X-Y plot of the sample
      plot.default(
                   x           = x,
                   pch         = 20,
                   cex         = 0.1,
                   ...
                  )

      # Plot flipped CDF of Y marginal
      tmp_y <- my_cdf(x$y)
      plot(
           list(x = tmp_y$y, y = tmp_y$x),
           pch = 20,
           cex = 0.1,
           ...
           )

      # Plot CDF of X marginal
      plot(
           my_cdf(x$x),
           pch = 20,
           cex = 0.1,
           ...
           )

      print("Done with the 3 easy ones")

      # Plot of the copula
      plot(copula(x))
   }
   else {
      plot.default(
                   x           = x,
                   pch         = 20,
                   cex         = 0.1,
                   ...
                   )
   }
}

##################################################
# Print method for the jointSample type
print.jointSample <-
function(x, ...) {
    cat("Joint Sample\n")
    NextMethod("print", x)
    invisible(x)
}

##################################################
# Simulate using a copula
rand_copula <-
function(cop, sample) {
   size    <- length(cop$x)
   ss      <- lapply(sample, sort)
   s_cop_x <- ss$x[cop$x * (size + 1)]
   s_cop_y <- ss$x[cop$y * (size + 1)]

   me  <- list(
               sample = list(
                              x   = s_cop_x,
                              y   = s_cop_y
                              ),
               lx  = length(s_cop_x),
               ly  = length(s_cop_y),
               ss  = ss,
               cop = cop
               )

   class(me) <- append(class(me), "jointSample")
   return(me)
}