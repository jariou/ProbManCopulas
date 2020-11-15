######################################################
# See Metalog in R                                   #
# https://cran.r-project.org/web/packages            #
# /rmetalog/vignettes/rmetalog-vignette.html         #
######################################################

# Making Your First R Package
# https://tinyheero.github.io/jekyll/update
# /2015/07/26/making-your-first-R-package.html

# R Packages: A Beginner's Guide
# https://www.datacamp.com/community/tutorials/r-packages-guide

# R package primer
# Connecting to other packages
# https://kbroman.org/pkg_primer/pages/depends.html

#------------------------------
# Marginal distributionn Models
#------------------------------


#-----------------------------------
# Perfect positive Dependency Copula
One <-
function(n) {
  me <- list(
             x = (1:n) / (n + 1),
             y = (1:n) / (n + 1)
            )
  class(me) <- append(class(me), "jointSample")
  class(me) <- append(class(me), "copula")
  return(me)
}

#-----------------------------------
# Perfect negative Dependency Copula
MinusOne <-
function(n) {
  me <- list(
             x = (1:n) / (n + 1),
             y = (n:1) / (n + 1)
            )
  class(me) <- append(class(me), "jointSample")
  class(me) <- append(class(me), "copula")
  return(me)
}

#-------------------------------------------------
# Exponential Distribution with Scale parameter
Exponential <-
function(params) {
  pdf <- function(x) {
                      dexp(x, rate = 1 / params)
                      }
  cdf <- function(x) {
                      pexp(x, rate = 1 / params)
                      }
  quantile <- function(p) {
                           qexp(p, rate = 1 / params)
                           }
  rand <- function(n) {
                       rexp(n, rate= 1 / params)
                       }
  param_names <- c("Theta")
  param_desc  <- c("Scale parameter")

  me <- list(
             Pdf        = pdf,
             Cdf        = cdf,
             Quantile   = quantile,
             Roll       = rand,
             ParamNames = param_names,
             ParamDesc  = param_desc
             )
  class(me) <- "RV-Model"
  return(me)
}

#----------------------------------------
# Degenerate Distribution with parameter
Degenerate <-
function(params) {
  pdf <- function(x) {
                      dexp(x, rate = 1 / params)
                      }
  cdf <- function(x) {
                      pexp(x, rate = 1 / params)
                      }
  quantile <- function(p) {
                           qexp(p, rate = 1 / params)
                           }
  rand <- function(n) {
                       rep(params, n)
                       }
  param_names <- c("X0")
  param_desc  <- c("The single possible realized value")

  me <- list(
             Pdf        = pdf,
             Cdf        = cdf,
             Quantile   = quantile,
             Roll       = rand,
             ParamNames = param_names,
             ParamDesc  = param_desc
             )
  class(me) <- "RV-Model"
  return(me)
}

#----------------------------------------
# Uniform Distribution defaults to [0, 1]
Uniform <-
function(params) {
  pdf <- function(x) {
                      dunif(x, params[1], params[2])
                      }
  cdf <- function(x) {
                      punif(x, params[1], params[2])
                      }
  quantile <- function(p) {
                           qunif(p, params[1], params[2])
                           }
  rand <- function(n) {
                       runif(n, params[1], params[2])
                       }

  param_names <- c("Min", "Max")
  param_desc  <- c("Minimum possible value", "Maximum possible value")

  me <- list(
             Pdf        = pdf,
             Cdf        = cdf,
             Quantile   = quantile,
             Roll       = rand,
             ParamNames = param_names,
             ParamDesc  = param_desc
             )
  class(me) <- "RV-Model"
  return(me)
}

#--------------------
# Pareto Distribution
Pareto <-
function(params) {
  alpha <- params[2]
  theta <- params[1]

  pdf <- function(x) {
                      alpha / theta / (1 + x / theta)^ (alpha + 1)
                      }
  cdf <- function(x) {
                      1 - (1 / (x / theta + 1))^alpha
                      }
  quantile <- function(p) {
                           theta * (
                                    1 / (
                                         (1 - p)^ (1 / alpha)
                                         ) 
                                    - 1
                                   )
                           }
  rand <- function(n) {
                       theta * (
                                1 / (
                                     (1 - runif(n))^ (1 / alpha)
                                     )
                                - 1
                                )
                       }

  param_names <- c("Theta", "Alpha")
  param_desc  <- c("Scale parameter", "Shape parameter")

  me <- list(
             Pdf        = pdf,
             Cdf        = cdf,
             Quantile   = quantile,
             Roll       = rand,
             ParamNames = param_names,
             ParamDesc  = param_desc
             )
  class(me) <- "RV-Model"
  return(me)
}

#----------------------------
# Inverse Pareto Distribution
InversePareto <-
function(params) {
  tau <- params[2]
  theta <- params[1]

  pdf <- function(x) {
                      tau / theta / (1 + x / theta)^ (alpha + 1)
                      }
  cdf <- function(x) {
                      1 - (1 / (x / theta + 1))^alpha
                      }
  quantile <- function(p) {
                           theta * (
                                    1 / (
                                         (1 - p)^ (1 / alpha)
                                         )
                                    - 1
                                   )
                           }
  rand <- function(n) {
                       theta * (
                                1 / (
                                     (1 - runif(n))^ (1 / alpha)
                                     )
                                - 1
                                )
                       }

  param_names <- c("Theta", "Alpha")
  param_desc  <- c("Scale parameter", "Shape parameter")

  me <- list(
             Pdf        = pdf,
             Cdf        = cdf,
             Quantile   = quantile,
             Roll       = rand,
             ParamNames = param_names,
             ParamDesc  = param_desc
             )
  class(me) <- "RV-Model"
  return(me)
}

#-------------------------
# Simple simulation models
ball <-
function(n, r = 1, hole = 0, x0 = 0, y0 = 0, from = 0, to = 1) {
  radius_pos  <- sqrt(
                      runif(n) *
                      (r^2 - hole^2) +
                      hole^2
                      )

  theta <- 2 * (from + runif(n) * (to - from)) * pi
  x_pos <- radius_pos * cos(theta)
  y_pos <- radius_pos * sin(theta)

  me <- list(
             x = x_pos + x0,
             y = y_pos + y0
             )
  class(me) <- append(class(me), "jointSample")
  return(me)
}

#------------------------------------
# Common Scale Factor Dependent Model
common_scale <-
function(
         n,
         scale_model   = Exponential(100),
         margin_models = list(
                              x = Exponential(100),
                              y = Exponential(100)
                              )
         ) {
  scales <- scale_model$Roll(n)
  tmp <- lapply(margin_models, "[[", "Roll")
  me <- list(
             x = tmp$x(n) * scales,
             y = tmp$y(n) * scales
             )
  class(me) <- append(class(me), "jointSample")
  return(me)
}

#---------------------------------
# Uniform distribution on a circle
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

#-------------------
# Uniform  distribution on a square
square <-
function(n) {
  me <- list(
             x = runif(n),
             y = runif(n)
             )
  class(me) <- append(class(me), "jointSample")
  return(me)
}

#---------------------------------
# Uniform distribution on a triangle
triangle <-
function(n, rev = F) {
  y  <- 1 - sqrt(runif(n))

  me <- list(
             x = y / 2 + runif(n) * (1 - y),
             y = y
             )
  class(me) <- append(class(me), "jointSample")
  return(me)
}

#--------------------------------
# Distribution uniform on a smile
smile <-
function(
        n,
        main_radius  = 1,
        eye_radius   = 0.25,
        mouth_radius = 0.7,
        mouth_start  = 0.55,
        mouth_end    = 0.95
        ) {
  total_length <- main_radius +
                  2 * eye_radius +
                  mouth_radius * (mouth_end - mouth_start)

  mouth_count  <- ceiling(
                          n * mouth_radius * (mouth_end - mouth_start)
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

#--------------------------------
# Uniform distribution on a frown
frown <-
function(
         n,
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
                         mouth_start,
                         mouth_end
                         )
  me     <- list(
                 x = c(main$x, left_eye$x, right_eye$x, mouth$x),
                 y = c(main$y, left_eye$y, right_eye$y, mouth$y)
                 )
  class(me) <- append(class(me), "jointSample")
  return(me)
}

#----------------------------------------------
# Generate empirical copula from a joint sample
copula <-
function(joint_sample) {
  size  <- length(joint_sample$x)
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

#----------------------------------------------
# Dump all the functions from a search location
dump.functions <-
function(file, search_loc = 1) {
  dump(functions(search_loc), file)
}

#----------------------------------------------
# Return a list of functions
functions <-
function(search_loc = 1) {
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
                     typeof
                     )
              )
              ==
              "closure"
       ]
}

#-----------------------------
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

#---------------------------------------
# Simple implementation of empirical cdf
my_cdf <-
function(x) {
  sx    <- sort(x)
  rl    <- rle(sx)

  l     <- rl$lengths
  ll    <- length(l)

  my_x  <- rep(rl$values, l)
  my_y  <- cumsum(rep(rep(1, ll), l))

  list(
       x = my_x, 
       y = my_y
       )
}

#-------------------------------------
# Simulate a joint normal distribution
normal <-
function(n, r = 1, hole = 0, x0 = 0, y0 = 0, from = 0, to = 1) {
  r_pos   <- sqrt(
                  runif(n) *
                  (r^2 - hole^2) +
                  hole^2
                  )
  theta   <- 2 * (from + runif(n) * (to - from)) * pi
  x       <- r_pos * cos(theta)
  y       <- r_pos * sin(theta)

  me  <- list(
              x = x + x0,
              y = y + y0
              )
  class(me) <- append(class(me), "jointSample")
  return(me)
}

#--------------------
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
    par(
        mfrow = c(2, 2), 
        mar   = c(0, 0, 0, 0), 
        mai   = c(0, 0, 0, 0)
        )

    # Plain X-Y plot of the sample
    plot.default(
                 x    = x,
                 pch  = 20,
                 cex  = 0.1,
                 tcl  = 0,
                 xlab = "",
                 ylab = "",
                 xaxt = "n",
                 yaxt = 'n',
                 ...
                 )

    # Plot flipped CDF of Y marginal
    tmp_y <- my_cdf(x$y)
    plot(
         list(x = tmp_y$y, y = tmp_y$x),
         pch  = 20,
         cex  = 0.1,
         tcl  = 0,
         xlab = "",
         ylab = "",
         xaxt = "n",
         yaxt = 'n',
         ...
         )

    # Plot CDF of X marginal
    plot(
         my_cdf(x$x),
         pch  = 20,
         cex  = 0.1,
         tcl  = 0,
         xlab = "",
         ylab = "",
         xaxt = "n",
         yaxt = 'n',
         ...
         )

    # Plot of the copula
    plot(
         copula(x),
         xaxt = "n",
         xlab = "",
         ylab = "",
         xaxt = "n",
         yaxt = 'n',
         ...
         )
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

#--------------------------------------
# Print method for the jointSample type
print.jointSample <-
function(x, ...) {
  cat("Joint Sample\n")
  NextMethod("print", x)
  invisible(x)
}

#------------------------
# Simulate using a copula
rand_copula <-
function(cop, sample) {
  size    <- length(cop$x)
  ss      <- lapply(sample, sort)
  s_cop_x <- ss$x[cop$x * (size + 1)]
  s_cop_y <- ss$x[cop$y * (size + 1)]

  me  <- list(
              x = s_cop_x,
              y = s_cop_y
              )
  class(me) <- append(class(me), "jointSample")
  return(me)
}
