get_param_dist_def <- function() {
  list(
    #---BEGIN: discrete distributions-------------------------------------------
    # binomial
    binom    = list(
      display_name = "binomial",
      type = "discrete",
      def = list(
        size   = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("size >= 0",
                          "size %% 1 == 0"),
          optional    = FALSE
        ),
        prob   = list(
          init        = 0.5,
          range       = list(min = "0",
                             max = "1"),
          check_rules = c("0 < prob",
                          "prob < 1"),
          optional    = FALSE
        )
      ),
      output_range = list(min = "0",
                          max = "size"),
      function_call = "rrisk_rbinom"
    ),
    # negative binomial
    nbinom = list(
      display_name = "negative binomial",
      type         = "discrete",
      def = list(
        size = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("size >= 0",
                          "size %% 1 == 0"),
          optional    = FALSE
        ),
        prob = list(
          init        = 0.1,
          range       = list(min = "0",
                             max = "1"),
          check_rules = c("0 < prob",
                          "prob < 1"),
          optional    = FALSE
        )
      ),
      output_range  = list(min = "0",
                           max = "Inf"),
      function_call = "rrisk_rnbinom"
    ),
    # geometric distribution
    geom = list(
      display_name  = "geometric",
      type          = "discrete",
      def           = list(
        prob = list(
          init        = 0.1,
          range       = list(min = "0",
                             max = "1"),
          check_rules = c("prob >= 0",
                          "prob <= 1"),
          optional    = FALSE
        )
      ),
      output_range  = list(min = "0",
                           max = "Inf"),
      function_call = "rrisk_rgeom"
    ),
    # hypergeometric distribution
    hypergeom = list(
      display_name = "hypergeometric",
      type = "discrete",
      def = list(
        # number of white balls in urn
        M = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("M >= 0",
                          "M %% 1 == 0"),
          optional    = FALSE
        ),
        # number of black balls in urn
        N = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("N >= 0",
                          "N %% 1 == 0"),
          optional    = FALSE
        ),
        # number of balls drawn from urn
        k = list(
          init        = 1,
          range       = list(min = "0",
                             max = "M+N"),
          check_rules = c("k >= 0",
                          "k <= M+N"),
          optional    = FALSE
        )
      ),
      output_range  = list(min = "0",
                           max = "Inf"),
      function_call = "rrisk_rhyper"
    ),
    # poisson
    poisson  = list(
      display_name = "poisson",
      type = "discrete",
      def = list(lambda = list(
        init        = 1,
        range       = list(min = "0",
                           max = "Inf"),
        check_rules = c("lambda > 0"),
        optional    = FALSE
      )),
      output_range  = list(min = "0",
                           max = "Inf"),
      function_call = "rrisk_rpois"
    ),
    # discrete
    discrete = list(
      display_name = "discrete",
      type = "discrete",
      def = list(
        x    = list(
          init        = c(0, 1),
          range       = list(min = "-Inf",
                             max = "Inf"),
          check_rules = c("length(x) >= 2"),
          is_vector   = TRUE,
          optional    = FALSE
        ),
        prob = list(
          init        = c(0.5, 0.5),
          range       = list(min = "0",
                             max = "1"),
          check_rules = c(
            "length(prob) == length(x)",
            "sum(prob) == 1",
            "all(0 <= prob)",
            "all(prob <= 1)"
          ),
          is_vector   = TRUE,
          optional    = FALSE
        )
      ),
      output_range  = list(min = "min(x)",
                           max = "max(x)"),
      function_call = "rrisk_rdiscrete"
    ),
    # # multinomial
    # multinom = list(
    #   display_name = "multinomial",
    #   type = "discrete",
    #   def = list(
    #     prob   = list(
    #       init        = c(0, 1),
    #       range       = list(min = "0",
    #                          max = "1"),
    #       check_rules = c(
    #         "length(prob) >= 2",
    #         "sum(prob) == 1",
    #         "all(0 <= prob)",
    #         "all(prob <= 1)",
    #         "is.unsorted(prob) == FALSE"
    #       ),
    #       is_vector   = TRUE,
    #       optional    = FALSE
    #     )
    #   ),
    #   output_range  = list(min = "0",
    #                        max = "Inf"),
    #   function_call = "rrisk_rmultinom"
    # ),
    #---END: discrete distributions---------------------------------------------
    #---BEGIN: continuous distributions-----------------------------------------
    # uniform
    unif     = list(
      display_name = "uniform",
      type = "continuous",
      def = list(
        min    = list(
          init        = 0,
          range       = list(min = "-Inf",
                             max = "max"),
          check_rules = c("min < max"),
          optional    = FALSE
        ),
        max    = list(
          init        = 1,
          range       = list(min = "min",
                             max = "Inf"),
          check_rules = c("min < max"),
          optional    = FALSE
        )
      ),
      output_range  = list(min = "min",
                           max = "max"),
      function_call = "rrisk_runif"
    ),
    # beta
    beta     = list(
      display_name = "beta",
      type = "continuous",
      def = list(
        shape1 = list(
          init        = 2,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("shape1 > 0"),
          optional    = FALSE
        ),
        shape2 = list(
          init        = 2,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("shape2 > 0"),
          optional    = FALSE
        )
      ),
      output_range  = list(min = "0",
                           max = "1"),
      function_call = "rrisk_rbeta"
    ),
    # modified PERT
    pert     = list(
      display_name = "modified PERT",
      type = "continuous",
      def = list(
        min    = list(
          init        = 0,
          range       = list(min = "-Inf",
                             max = "mode"),
          check_rules = c("min < mode"),
          optional    = FALSE
        ),
        mode   = list(
          init        = 1,
          range       = list(min = "min",
                             max = "max"),
          check_rules = c("min < mode",
                          "mode < max"),
          optional    = FALSE
        ),
        max    = list(
          init        = 2,
          range       = list(min = "mode",
                             max = "Inf"),
          check_rules = c("mode < max"),
          optional    = FALSE
        ),
        shape  = list(
          init        = 4,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("shape > 0"),
          optional    = TRUE
        )
      ),
      output_range  = list(min = "min",
                           max = "max"),
      function_call = "rrisk_rmodpert"
    ),
    # exponential
    exponential = list(
      display_name = "exponential",
      type = "continuous",
      def = list(
        rate  = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("rate > 0"),
          optional    = FALSE
        ),
        lower = list(
          init        = 0,
          range       = list(min = "0",
                             max = "upper"),
          check_rules = c("0 <= lower",
                          "lower <= upper"),
          optional    = TRUE
        ),
        upper = list(
          init        = Inf,
          range       = list(min = "lower",
                             max = "Inf"),
          check_rules = c("lower <= upper"),
          optional    = TRUE
        )
      ),
      output_range  = list(min = "lower",
                           max = "upper"),
      function_call = "rrisk_rexp"
    ),
    # gaussian
    gaussian = list(
      display_name = "gaussian (normal)",
      type = "continuous",
      def = list(
        mean   = list(
          init     = 0,
          range    = list(min = "-Inf",
                          max = "Inf"),
          optional = FALSE
        ),
        sd     = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("sd > 0"),
          optional    = FALSE
        ),
        lower  = list(
          init        = -Inf,
          range       = list(min = "-Inf",
                             max = "upper"),
          check_rules = c("lower < upper"),
          optional    = TRUE
        ),
        upper  = list(
          init        = Inf,
          range       = list(min = "lower",
                             max = "Inf"),
          check_rules = c("lower < upper"),
          optional    = TRUE
        )
      ),
      output_range  = list(min = "lower",
                           max = "upper"),
      function_call = "rrisk_rnorm"
    ),
    # lognormal
    lognormal = list(
      display_name = "lognormal",
      type = "continuous",
      def = list(
        meanlog = list(
          init        = 0,
          range       = list(min = "-Inf",
                             max = "Inf"),
          optional    = FALSE
        ),
        sdlog   = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("sdlog > 0"),
          optional    = FALSE
        ),
        lower   = list(
          init        = 0,
          range       = list(min = "0",
                             max = "upper"),
          check_rules = c("0 <= lower",
                          "lower < upper"),
          optional    = TRUE
        ),
        upper   = list(
          init        = Inf,
          range       = list(min = "lower",
                             max = "Inf"),
          check_rules = c("lower < upper"),
          optional = TRUE
        )
      ),
      output_range = list(min = "lower",
                          max = "upper"),
      function_call = "rrisk_rlnorm"
    ),
    # weibull
    weibull  = list(
      display_name = "weibull",
      type = "continuous",
      def = list(
        shape  = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("shape > 0"),
          optional    = FALSE
        ),
        scale  = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("scale > 0"),
          optional    = FALSE
        ),
        lower  = list(
          init        = 0,
          range       = list(min = "0",
                             max = "upper"),
          check_rules = c("0 <= lower",
                          "lower < upper"),
          optional    = TRUE
        ),
        upper  = list(
          init        = Inf,
          range       = list(min = "lower",
                             max = "Inf"),
          check_rules = c("lower < upper"),
          optional    = TRUE
        )
      ),
      output_range  = list(min = "lower",
                           max = "upper"),
      function_call = "rrisk_rweibull"
    ),
    # gumbel
    gumbel = list(
      display_name = "gumbel",
      type         = "continuous",
      def          = list(
        mu   = list(
          init     = 0,
          range    = list(min = "-Inf",
                          max = "Inf"),
          optional = FALSE
        ),
        beta = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("beta > 0"),
          optional    = FALSE
        )
      ),
      output_range = list(min = "-Inf",
                          max = "Inf"),
      function_call = "rrisk_rgumbel"
    ),
    # gompertz
    gompertz = list(
      display_name  = "gompertz",
      type          = "continuous",
      def           = list(
        location = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("location > 0"),
          optional    = FALSE
        ),
        shape = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("shape > 0"),
          optional    = FALSE
        )
      ),
      output_range  = list(min = "0",
                           max = "Inf"),
      function_call = "rrisk_rgompertz"
    ),
    # gamma
    gamma    = list(
      display_name = "gamma",
      type         = "continuous",
      def = list(
        shape = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("shape > 0"),
          optional    = FALSE
        ),
        rate = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("rate > 0"),
          optional    = FALSE
        ),
        lower = list(
          init        = 0,
          range       = list(min = "0",
                             max = "upper"),
          check_rules = c("0 <= lower",
                          "lower < upper"),
          optional    = TRUE
        ),
        upper = list(
          init        = Inf,
          range       = list(min = "upper",
                             max = "Inf"),
          check_rules = c("lower < upper"),
          optional    = TRUE
        )
      ),
      output_range  = list(min = "lower",
                           max = "upper"),
      function_call = "rrisk_rgamma"
    ),
    # inv. gamma
    invgamma = list(
      display_name = "inverted gamma",
      type = "continuous",
      def = list(
        shape  = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("shape > 0"),
          optional    = FALSE
        ),
        rate   = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("rate > 0"),
          optional    = FALSE
        ),
        lower  = list(
          init        = 0,
          range       = list(min = "0",
                             max = "upper"),
          check_rules = c("0 <= lower",
                          "lower < upper"),
          optional    = TRUE
        ),
        upper  = list(
          init        = Inf,
          range       = list(min = "lower",
                             max = "Inf"),
          check_rules = c("lower < upper"),
          optional    = TRUE
        )
      ),
      output_range  = list(min = "lower",
                           max = "upper"),
      function_call = "rrisk_rinvgamma"
    ),
    # log logistic
    loglogistic = list(
      display_name = "shifted log-logistic",
      type = "continuous",
      def = list(
        location = list(
          init     = 0,
          range    = list(min = "-Inf",
                          max = "Inf"),
          optional = TRUE
        ),
        alpha    = list(
          init        = 2,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("alpha > 0"),
          optional    = FALSE
        ),
        beta    = list(
          init        = 2,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("beta > 0"),
          optional    = FALSE
        ),
        lower    = list(
          init        = "location",
          range       = list(min = "location",
                             max = "Inf"),
          check_rules = c("location <= lower"),
          optional    = TRUE
        ),
        upper    = list(
          init        = Inf,
          range       = list(min = "lower",
                             max = "Inf"),
          check_rules = c("lower < upper"),
          optional    = TRUE
        )
      ),
      output_range = list(min = "lower",
                          max = "upper"),
      function_call = "rrisk_rll3"
    ),
    # triangular
    triang   = list(
      display_name = "triangular",
      type = "continuous",
      def = list(
        min    = list(
          init        = 0,
          range       = list(min = "-Inf",
                             max = "mode"),
          check_rules = c("min <= mode"),
          optional    = FALSE
        ),
        mode   = list(
          init        = 1,
          range       = list(min = "min",
                             max = "max"),
          check_rules = c("min <= mode",
                          "mode <= max"),
          optional    = FALSE
        ),
        max    = list(
          init        = 2,
          range       = list(min = "mode",
                             max = "Inf"),
          check_rules = c("mode <= max"),
          optional    = FALSE
        ),
        lower  = list(
          init        = "min",
          range       = list(min = "min",
                             max = "mode"),
          check_rules = c("min <= lower",
                          "lower <= mode"),
          optional    = TRUE
        ),
        upper  = list(
          init        = "max",
          range       = list(min = "mode",
                             max = "max"),
          check_rules = c("mode <= upper",
                          "upper <= max"),
          optional    = TRUE
        )
      ),
      output_range = list(min = "lower",
                          max = "upper"),
      function_call = "rrisk_rtriang"
    ),
    # general
    general  = list(
      display_name = "general",
      type = "continuous",
      def = list(
        full_x  = list(
          init        = c(0, 1, 2),
          range       = list(min = "-Inf",
                             max = "Inf"),
          check_rules = c("length(full_x) >= 3",
                          "is.unsorted(full_x) == FALSE"),
          is_vector   = TRUE,
          optional    = FALSE
        ),
        full_p  = list(
          init        = c(0, 1, 0),
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c(
            "length(full_p) == length(full_x)",
            "min(full_p) == 0"
          ),
          is_vector   = TRUE,
          optional    = FALSE
        )
      ),
      output_range  = list(min = "min(full_x)",
                           max = "max(full_x)"),
      function_call = "rrisk_rgeneral"
    ),
    # cumulative
    cumulative = list(
      display_name = "cumulative",
      type = "continuous",
      def = list(
        full_x = list(
          init        = c(0, 1),
          range       = list(min = "-Inf",
                             max = "Inf"),
          check_rules = c("length(full_x) >= 2",
                          "is.unsorted(full_x) == FALSE"),
          is_vector   = TRUE,
          optional    = FALSE
        ),
        full_p = list(
          init        = c(0, 1),
          range       = list(min = "0",
                             max = "1"),
          check_rules = c(
            "length(full_p) >= 2",
            "length(full_p) == length(full_x)",
            "min(full_p) == 0",
            "max(full_p) == 1",
            "is.unsorted(full_p) == FALSE"
          ),
          is_vector   = TRUE,
          optional    = FALSE
        ),
        smooth = list(
          init        = FALSE,
          range       = list(min = FALSE,
                             max = TRUE),
          check_rules = c("is.logical(smooth)"),
          optional    = TRUE
        )
      ),
      output_range  = list(min = "min(full_x)",
                           max = "max(full_x)"),
      function_call = "rrisk_rcumulative"
    ),
    #---END: continuous distributions-------------------------------------------
    #---BEGIN: special distributions--------------------------------------------
    # sigma distribution
    sigma_dist = list(
      display_name = "sigma distribution",
      type = "special",
      def = list(
        sd     = list(
          init        = 1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("sd > 0"),
          optional    = FALSE
        ),
        nn     = list(
          init        = 2,
          range       = list(min = "2",
                            max = "Inf"),
          check_rules = c("nn >= 2"),
          optional = FALSE
        )
      ),
      output_range  = list(min = "0",
                           max = "Inf"),
      function_call = "rrisk_rsigma_dist"
    ),
    # yules-furres process
    yule_furry_process = list(
      display_name = "Yule-Furry Process (Only birth)",
      type = "special",
      def = list(
        N0   = list(
          init        = 1,
          range       = list(min = "1",
                          max = "Inf"),
          check_rules = c("N0 %% 1 == 0",
                          "N0 > 0"),
          optional    = FALSE
        ),
        rate = list(
          init        = 0.1,
          range       = list(min = "0",
                          max = "Inf"),
          check_rules = c("rate > 0"),
          optional    = FALSE
        ),
        time = list(
          init        = 0.1,
          range       = list(min = "0",
                             max = "Inf"),
          check_rules = c("time > 0"),
          optional    = FALSE
        )
      ),
      output_range = list(min = "0",
                          max = "Inf"),
      function_call = "rrisk_yule_furry_process"
    )
    #---END: special distributions----------------------------------------------
  )
}