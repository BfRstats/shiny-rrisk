plot_pdf <- function(dist_name, params, xlab = "x") 
{
  df_plot <- get_pdf_data_for_plotting(dist_name, params)
  if (is.null(df_plot$x)) {
    plot(x    = NULL, 
         y    = NULL, 
         xlim = c(0,1), 
         ylim = c(0,1),
         xlab = xlab, 
         ylab = "pdf"
    )
    text(x = 0.5, y = 0.5, labels = "can not plot pdf")
  } else {
    plot(x    = df_plot$x, 
         y    = df_plot$y, 
         type = df_plot$type, 
         xlab = xlab, 
         ylab = "pdf",
         ylim = c(0, max(df_plot$y))
    )
  }
}

get_pdf_data_for_plotting <- function(dist_name, params)
{
  x <- NULL
  y <- NULL
  type <- "l"
  
  switch(
    EXPR = dist_name,
    "unif" = {
      min_value <- params[["min"]]
      max_value <- params[["max"]]
      
      if (is.numeric(max_value) &&
          is.numeric(min_value) &&
          max_value > min_value) {
        
        ext_value <- 0.1 * (max_value - min_value)
        x <- seq(from       = min_value - ext_value, 
                 to         = max_value + ext_value,
                 length.out = 101)
        y <- dunif(x = x, min = min_value, max = max_value)
        
      }
    },
    "binom" = {
      size_value <- params[["size"]]
      prob_value <- params[["prob"]]
      
      if (is.numeric(size_value) &&
          is.numeric(prob_value) &&
          size_value >= 0 &&
          size_value %% 1 == 0 && # size_value is a whole number
          prob_value >= 0 && 
          prob_value <= 1) {
        
        x <- seq(from = 0, to = size_value)
        y <- dbinom(x, size = size_value, prob = prob_value)
        type <- "h"
        
      }
    },
    "poisson" = {
      lambda_value <- params[["lambda"]]
      
      if (is.numeric(lambda_value) &&
          lambda_value >= 0) {
        
        x <- seq(from = 0, to = ceiling(1.5 * lambda_value) + 5)
        y <- dpois(x, lambda = lambda_value)
        type <- "h"
        
      }
    },
    "nbinom" = {
      size_value <- params[["size"]]
      prob_value <- params[["prob"]]
      
      if (is.numeric(size_value) &&
          is.numeric(prob_value) &&
          size_value > 0 &&
          size_value %% 1 == 0 && # size must be a whole number
          prob_value > 0 && 
          prob_value <= 1) {
        
        x <- seq(from = 0, 
                 to   = 3*round(size_value * (1 - prob_value)/prob_value))
        y <- dnbinom(x, size = size_value, prob = prob_value)
        type <- "h"
        
      }
    },
    "geom" = {
      prob_value <- params[["prob"]]
      
      if (is.numeric(prob_value) &&
          prob_value > 0 &&
          prob_value < 1) {
        
        m <- ceiling(-1/log2(1 - prob_value))
        max_x <- if (m == 0) 2 else if (is.infinite(m)) 1000 else m * 3
        x <- seq(from = 0, to = max_x)
        y <- dgeom(x, prob = prob_value)
        type <- "h"
        
      }
      
    },
    "hypergeom" = {
      m_value <- params[["M"]]
      n_value <- params[["N"]]
      k_value <- params[["k"]]
      
      if (is.numeric(m_value) &&
          is.numeric(n_value) &&
          is.numeric(k_value) &&
          m_value >= 0 && m_value %% 1 == 0 &&
          n_value >= 0 && n_value %% 1 == 0 &&
          k_value >= 0 && k_value <= m_value + n_value) {
        
        x <- seq(from = 0, to   = m_value + 1)
        y <- dhyper(x, m = m_value, n = n_value, k = k_value)
        type <- "h"
        
      }
    },
    "discrete" = {
      x_value <- params[["x"]]
      prob_value <- params[["prob"]]
      
      if (is.numeric(x_value) &&
          is.numeric(prob_value) &&
          (length(x_value) >= 2) &&
          (length(x_value) == length(prob_value)) &&
          (sum(prob_value) == 1) &&
          all(prob_value >= 0) &&
          all(prob_value <= 1)) {
        
        x <- x_value
        y <- prob_value
        type <- "h"
        
      }
    },
    "gaussian" = {
      mu_value <- params[["mean"]]
      sigma_value <- params[["sd"]]
      lower <- params[["lower"]]
      upper <- params[["upper"]]
      
      if (is.numeric(mu_value) &&
          is.numeric(sigma_value) &&
          is.numeric(lower) &&
          is.numeric(upper) &&
          sigma_value > 0 &&
          lower < upper) {
        
        if (is.infinite(lower)) {
          min_value <- mu_value - 4 * sigma_value
        } else {
          min_value <- lower - 0.1 * abs(lower)
        }
        
        if (is.infinite(upper)) {
          max_value <- mu_value + 4 * sigma_value
        } else {
          max_value <- upper + 0.1 * abs(upper)
        }
      
        x <- seq(from = min_value, to = max_value, length.out = 101)
        y <- rrisk_dnorm(x     = x, 
                         mean  = mu_value, 
                         sd    = sigma_value, 
                         lower = lower, 
                         upper = upper)
      }
    
    },
    "lognormal" = {
      meanlog <- params[["meanlog"]]
      sdlog   <- params[["sdlog"]]
      lower   <- params[["lower"]]
      upper   <- params[["upper"]]
      
      if (is.numeric(meanlog) &&
          is.numeric(sdlog) &&
          is.numeric(lower) &&
          is.numeric(upper) &&
          sdlog >= 0 &&
          lower >= 0 &&
          lower < upper) {
        
        if (is.infinite(upper)) {
          max_value <- qlnorm(0.99, meanlog, sdlog)
        } else {
          max_value <- 1.05 * upper
        }
        
        min_value <- lower
        if (min_value > 0) {
          min_value <- 0.9 * min_value
        }

        x <- seq(from = min_value, to = max_value, length.out = 101)
        y <- rrisk_dlnorm(x       = x, 
                          meanlog = meanlog, 
                          sdlog   = sdlog,
                          lower   = lower, 
                          upper   = upper)
      }
    },
    "loglogistic" = {
      location <- params[["location"]]
      alpha <- params[["alpha"]]
      beta <- params[["beta"]]
      lower <- params[["lower"]]
      upper <- params[["upper"]]
      
      if (is.character(lower) &&
          lower == "location") {
        lower <- location
      }
      
      if (is.numeric(location) &&
          is.numeric(alpha) &&
          is.numeric(beta) &&
          is.numeric(lower) &&
          is.numeric(upper) &&
          alpha > 0 &&
          beta > 0 &&
          lower >= location &&
          lower < upper) {
        
        if (lower == location) {
          min_value <- location - 1
        } else {
          min_value <- lower - 1
        }
        
        if (is.infinite(upper)) {
          max_value <- rrisk_qll3(0.99, location, alpha, beta)
        } else {
          max_value <- upper + 1
        }
        
        x <- seq(from = min_value, to = max_value, length.out = 101)
        y <- rrisk_dll3(x        = x, 
                        location = location, 
                        alpha    = alpha, 
                        beta     = beta, 
                        lower    = lower, 
                        upper    = upper)
      }
    },
    "beta" = {
      shape1 <- params[["shape1"]]
      shape2 <- params[["shape2"]]
      
      if (is.numeric(shape1) &&
          is.numeric(shape2) &&
          shape1 > 0 &&
          shape2 > 0) {
        
        x <- seq(from = 0, to = 1, length.out = 101)
        y <- dbeta(x, shape1, shape2)
        
      }
    },
    "pert" = {
      min_value <- params[["min"]]
      mode_value <- params[["mode"]]
      max_value <- params[["max"]]
      shape <- params[["shape"]]
      
      if (is.numeric(min_value) &&
          is.numeric(mode_value) &&
          is.numeric(max_value) &&
          is.numeric(shape) &&
          min_value < mode_value &&
          mode_value < max_value &&
          shape > 0) {
        
        x <- seq(from = min_value, to = max_value, length.out = 101)
        y <- rrisk_dmodpert(x     = x, 
                            min   = min_value, 
                            mode  = mode_value, 
                            max   = max_value, 
                            shape = shape)
      }
    },
    "exponential" = {
      rate <- params[["rate"]]
      lower <- params[["lower"]]
      upper <- params[["upper"]]
      
      if (is.numeric(rate) &&
          is.numeric(lower) &&
          is.numeric(upper) &&
          rate > 0 &&
          lower >= 0 &&
          lower < upper) {
        
        if (is.infinite(upper)) {
          p_min <- pexp(q = lower, rate)
          max_value <- -log(1 - p_min - 0.95*(1 - p_min)) / rate
        } else {
          max_value <- 1.05 * upper 
        }

        x <- seq(from = 0, to = max_value, length.out = 201)
        y <- rrisk_dexp(x     = x, 
                        rate  = rate, 
                        lower = lower, 
                        upper = upper)
      }
    },
    "weibull" = {
      shape <- params[["shape"]]
      scale <- params[["scale"]]
      lower <- params[["lower"]]
      upper <- params[["upper"]]
      
      if (is.numeric(shape) &&
          is.numeric(scale) &&
          is.numeric(lower) &&
          is.numeric(upper) &&
          shape > 0 &&
          scale > 0 &&
          lower >= 0 &&
          lower < upper) {
        
        if (is.infinite(upper)) {
          max_value <- stats::qweibull(p = 0.9999, shape, scale)
        } else {
          max_value <- upper + 0.1 * upper
        }
        
        x <- seq(from = 0, to = max_value, length.out = 201)
        y <- rrisk_dweibull(x     = x, 
                            shape = shape, 
                            scale = scale, 
                            lower = lower, 
                            upper = upper)
      }
    },
    "triang" = {
      min_value <- params[["min"]]
      mode_value <- params[["mode"]]
      max_value <- params[["max"]]
      lower <- params[["lower"]]
      upper <- params[["upper"]]
    
      if (is.numeric(min_value) &&
          is.numeric(mode_value) &&
          is.numeric(max_value) &&
          min_value < mode_value &&
          mode_value < max_value) {
        
        if (is.character(lower) &&
            lower == "min") {
          lower <- min_value
        } else if (is.numeric(lower) &&
                   lower >= min_value &&
                   lower < mode_value) {
        } else {
          return()
        }
        
        if (is.character(upper) &&
            upper == "max") {
          upper <- max_value
        } else if (is.numeric(upper) &&
                   upper <= max_value &&
                   upper > mode_value) {
        } else {
          return()
        }
        
        x <- seq(from = min_value, to = max_value, length.out = 101)
        y <- rrisk_dtriang(x     = x, 
                           min   = min_value, 
                           mode  = mode_value, 
                           max   = max_value,
                           lower = lower, 
                           upper = upper)
      }
    },
    "gamma" = {
      shape <- params[["shape"]]
      rate <- params[["rate"]]
      lower <- params[["lower"]]
      upper <- params[["upper"]]
    
      if (is.numeric(shape) &&
          is.numeric(rate) &&
          is.numeric(lower) &&
          is.numeric(upper) &&
          shape >= 0 &&
          rate >= 0 &&
          lower >= 0 &&
          lower < upper) {
        
        if (is.infinite(upper)) {
          max_value <- stats::qgamma(p = 0.999, shape = shape, rate = rate)
        } else {
          max_value <- 1.05 * upper
        }
        
        x <- seq(from = 0, to = max_value, length.out = 101)
        y <- rrisk_dgamma(x     = x, 
                          shape = shape, 
                          rate  = rate, 
                          lower = lower, 
                          upper = upper)
      }
    },
    "invgamma" = {
      shape <- params[["shape"]]
      rate <- params[["rate"]]
      lower <- params[["lower"]]
      upper <- params[["upper"]]
      
      if (is.numeric(shape) &&
          is.numeric(rate) &&
          is.numeric(lower) &&
          is.numeric(upper) &&
          shape > 0 &&
          rate > 0 &&
          lower >= 0 &&
          lower < upper) {
        
        if (is.infinite(upper)) {
          max_value <- 5*rate / (shape + 1) 
        } else {
          max_value <- 1.05 * upper
        }
        
        x <- seq(from = 0, to = max_value, length.out = 101)
        y <- rrisk_dinvgamma(x     = x, 
                             shape = shape, 
                             rate  = rate, 
                             lower = lower, 
                             upper = upper)
      }
    },
    "cumulative" = {
      full_x <- params[["full_x"]]
      full_p <- params[["full_p"]]
      smooth <- params[["smooth"]]
      
      if (is.numeric(full_x) &&
          is.numeric(full_p) &&
          length(full_x) >= 2 &&
          length(full_x) == length(full_p) &&
          !is.unsorted(full_x) &&
          !is.unsorted(full_p) &&
          min(full_p) == 0 &&
          max(full_p) == 1) {
        
        x <- seq(from = min(full_x), to = max(full_x), length.out = 101)
        y <- rrisk_dcumulative(x      = x, 
                               full_p = full_p, 
                               full_x = full_x,
                               smooth = smooth == 1)
        
      }
    },
    "general" = {
      full_x <- params[["full_x"]]
      full_p <- params[["full_p"]]
      
      if (is.numeric(full_x) &&
          is.numeric(full_p) &&
          length(full_x) >= 3 &&
          length(full_x) == length(full_p) &&
          !is.unsorted(full_x) &&
          min(full_p) == 0) {
        
        x <- full_x
        y <- rrisk_dgeneral(full_x, full_p)
        
      }
    },
    "sigma_dist" = {
      sd_value <- params[["sd"]]
      nn <- params[["nn"]]
      
      if (is.numeric(sd_value) &&
          is.numeric(nn) &&
          sd_value >= 0 &&
          sd_value <= 100 &&
          nn >= 2 &&
          nn <= 310) {
        
        x <- seq(from = 0, to = 2 * sd_value + 1/nn, length.out = 201)
        y <- rrisk_dsigma_dist(x = x, sd = sd_value, nn = nn)
        
      }
    },
    "yule_furry_process" = {
      N0 <- params[["N0"]]
      rate <- params[["rate"]]
      time <- params[["time"]]
      
      if (is.numeric(N0) &&
          is.numeric(rate) &&
          is.numeric(time) &&
          N0 > 0 &&
          N0 %% 1 == 0 &&
          rate > 0 &&
          time > 0) {
        
        x <- seq(from = 0, to = N0 + 3*ceiling(N0*exp(rate*time)))
        y <- rrisk_dyule_furry_process(x, N0, rate, time)
        type <- "h"
        
      }
    }
  )

  is_finite <- is.finite(y)
  x <- x[is_finite]
  y <- y[is_finite]
  
  list(x = x, y = y, type = type)
}