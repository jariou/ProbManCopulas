# GGPLOT2 examples

#---------------------
# A simple scatterplot
ggplot() +
layer(
      data     = diamonds, # dtaset being used 
      mapping  = aes(
                    x = carat, 
                    y = price
                    ),
      geom     = "point",
      stat     = "identity",
      position = "identity"
      ) +
scale_y_continuous() +
scale_x_continuous() +
coord_cartesian()

# This can be reduced to a
# simpler form using defaults
ggplot() +
layer(
      data = diamonds,
      mapping = aes(
                    x = carat,
                    y = price
                    ),
      geom = "point"
      )

# Simpler yet
ggplot(diamonds, aes(carat, price)) +
geom_point()


# Now, with a log scale
ggplot(diamonds, aes(carat, price)) +
  geom_point() +
  stat_smooth(method = lm) +
  scale_x_log10() +
  scale_y_log10()

# Which is shorthand for
ggplot() +
  layer(
        data     = diamonds,
        mapping  = aes(
                      x = carat,
                      y = price
                      ),
        geom     = "point", 
        stat     = "identity", 
        position = "identity"
        ) +
  layer(
        data     = diamonds, 
        mapping  = aes(
                        x = carat, 
                        y = price
                        ),
        geom     = "smooth", 
        position = "identity",
        stat     = "smooth", 
        method = lm
        ) +
  scale_y_log10() +
  scale_x_log10() +
  coord_cartesian()


# Much shorter versions with qplot
qplot(carat, price, data = diamonds)

qplot(
      carat,
      price,
      data   = diamonds,
      geom   = c("point", "smooth"),
      method = "lm",
      log    = "xy"
      )

# Histogram
ggplot(data = diamonds, mapping = aes(price)) +
layer(
      geom    = "bar", 
      stat    = "bin",
      mapping = aes(y = ..count..)
      )

# in simpler ways 
ggplot(diamonds, aes(x = price)) + 
geom_histogram()

# and
qplot(
      price, 
      data = diamonds, 
      geom = "histogram"
      )