## Theory

$$
\begin{aligned}
  \mathcal{R}\_\text{emp} & = \frac{1}{n}\\\mathbf{R}^T\\\mathbf{V}\\\mathbf{R} \\
  \mathbf{R} & = \mathbf{Y} - \hat{\mathbf{Y}} \\
  \hat{\mathbf{Y}} & = h(\mathbf{X})
\end{aligned}
$$

The matrix **V** is a quadratic *n* × *n* matrix. The elements of the
**V** can be defined as:

$$
v\_{ij}(x\_i, x\_j) = \sum\_{s=1}^n G(x\_s - x\_i)\\G(x\_s - x\_j)
$$

The function *G* is a square-integrable function.

$$
\begin{aligned}
  \mathbf{\sigma} = \text{diag}(\mathcal{R}\_\text{emp})\\
  R\_\text{emp} = \sum\_{i=1}^{c} \sigma\_i^2
\end{aligned}
$$

If the covariances are also to be included, the eigenvalues of the
variance-covariance matrix could be used:

$$
\begin{aligned}
  \mathcal{R}\_\text{emp}\\\mathbf{e} = \lambda\\\mathbf{e} \\
  R\_\text{emp} = \sum\_{i=1}^c \lambda\_i
\end{aligned}
$$

The empirical risk will have in both cases the same value, but the terms
(*σ* or *λ*) have a different value.

## Multinomiale regression model

multinomial response

    mnr <- function(input)
    {
      result <- matrix(nrow = nrow(input), ncol = ncol(input))
      for (i in seq_len(nrow(input))) {
        tmp <- exp(input[i,])
        result[i,] <- tmp/(sum(tmp))
      }
      result
    }

    model <- function(X, W)
    {
      result <- cbind(1, X) %*% W
      result <- mnr(result)
      colnames(result) <- colnames(W)
      result
    }

## Risk function

    get_risk <- function(Y, pred_Y, std_method = TRUE)
    {
      R <- Y - pred_Y
      n <- nrow(Y)
      Rmat <- (t(R) %*% R)/n
      risk <- if (std_method)
        sum(diag(Rmat))
      else {
        tryCatch(sum(eigen(Rmat, 
                           symmetric = TRUE,
                           only.values = TRUE)$values),
                 error = function(e) e)
      }
      risk
    }

## Example data

    df <- iris[, c("Species", "Sepal.Length", "Petal.Length")]
    head(df)

    ##   Species Sepal.Length Petal.Length
    ## 1  setosa          5.1          1.4
    ## 2  setosa          4.9          1.4
    ## 3  setosa          4.7          1.3
    ## 4  setosa          4.6          1.5
    ## 5  setosa          5.0          1.4
    ## 6  setosa          5.4          1.7

    one_hot_coding <- function(y)
    {
      # do one hot coding
      Y <- model.matrix(~ y - 1)
      # clean up result
      colnames(Y) <- unique(y)
      rownames(Y) <- NULL
      # return result
      Y
    }

    Y <- one_hot_coding(df$Species)

    head(Y)

    ##      setosa versicolor virginica
    ## [1,]      1          0         0
    ## [2,]      1          0         0
    ## [3,]      1          0         0
    ## [4,]      1          0         0
    ## [5,]      1          0         0
    ## [6,]      1          0         0

    attributes(Y)

    ## $dim
    ## [1] 150   3
    ## 
    ## $dimnames
    ## $dimnames[[1]]
    ## NULL
    ## 
    ## $dimnames[[2]]
    ## [1] "setosa"     "versicolor" "virginica" 
    ## 
    ## 
    ## $assign
    ## [1] 1 1 1
    ## 
    ## $contrasts
    ## $contrasts$y
    ## [1] "contr.treatment"

    X <- as.matrix(df[, c("Sepal.Length", "Petal.Length")])
    X <- scale(X)
    head(X)

    ##      Sepal.Length Petal.Length
    ## [1,]   -0.8976739    -1.335752
    ## [2,]   -1.1392005    -1.335752
    ## [3,]   -1.3807271    -1.392399
    ## [4,]   -1.5014904    -1.279104
    ## [5,]   -1.0184372    -1.335752
    ## [6,]   -0.5353840    -1.165809

## Test everything

    get_named_weights <- function(par, X, Y)
    {
      W <- matrix(par, nrow = ncol(X) + 1, ncol = ncol(Y))
      colnames(W) <- colnames(Y)
      rownames(W) <- c("bias", colnames(X))
      W
    }
    W <- get_named_weights(rnorm((ncol(X)+1)*ncol(Y)), X, Y)
    W

    ##                  setosa versicolor   virginica
    ## bias         -2.0995755 -1.5424966  0.01772813
    ## Sepal.Length -1.0942434  0.8887776 -0.81926137
    ## Petal.Length  0.3063862 -0.5371397 -0.66514959

    pred_Y <- model(X, W)
    head(pred_Y)

    ##          setosa versicolor virginica
    ## [1,] 0.03895198 0.03537635 0.9256717
    ## [2,] 0.04201690 0.02363748 0.9343456
    ## [3,] 0.04282422 0.01565353 0.9415222
    ## [4,] 0.04923102 0.01287224 0.9378967
    ## [5,] 0.04048088 0.02893545 0.9305837
    ## [6,] 0.04020544 0.06489648 0.8948981

    get_risk(Y, pred_Y)

    ## [1] 1.180338

## Combine everything to the object functions

    obj_func <- function(par, Y, X, std_method, lambda)
    {
      W <- matrix(par, nrow = ncol(X) + 1, ncol = ncol(Y))
      pred_Y <- model(X, W)
      # return L2 regularized risk
      get_risk(Y, pred_Y, std_method) + lambda * mean(par^2)
    }

    fit <- optim(
      par     = runif(n = (ncol(X) + 1) * ncol(Y),
                      min = -1, max = 1),
      fn      = obj_func,
      gr      = NULL,
      Y, X, FALSE, 0.001,
      method  = "BFGS",
      control = list(maxit = 1e3)
    )
    fit

    ## $par
    ## [1] -0.4848719  0.3162406 -7.3188222  3.3326124  0.6154061 -1.9096668 -2.9294037
    ## [8] -0.9783038  9.3080157
    ## 
    ## $value
    ## [1] 0.07577997
    ## 
    ## $counts
    ## function gradient 
    ##      221      216 
    ## 
    ## $convergence
    ## [1] 0
    ## 
    ## $message
    ## NULL

    W_fit <- get_named_weights(fit$par, X, Y)
    W_fit

    ##                  setosa versicolor  virginica
    ## bias         -0.4848719  3.3326124 -2.9294037
    ## Sepal.Length  0.3162406  0.6154061 -0.9783038
    ## Petal.Length -7.3188222 -1.9096668  9.3080157

    pred_Y <- model(X, W_fit)
    R <- Y - pred_Y
    n <- nrow(Y)
    (t(R) %*% R) / n

    ##                   setosa   versicolor     virginica
    ## setosa      1.072798e-03 -0.001116332  4.353314e-05
    ## versicolor -1.116332e-03  0.028755199 -2.763887e-02
    ## virginica   4.353314e-05 -0.027638867  2.759533e-02

    labels <- colnames(Y)
    pred_label <- apply(
      X      = model(X, W_fit), 
      MARGIN = 1,
      FUN    = function(x) labels[which.max(x)]
    )
    cat("Acc =", round(mean(df$Species == pred_label), 3), "\n")

    ## Acc = 0.973

    caret::confusionMatrix(data      = factor(pred_label),
                           reference = df$Species)

    ## Confusion Matrix and Statistics
    ## 
    ##             Reference
    ## Prediction   setosa versicolor virginica
    ##   setosa         50          0         0
    ##   versicolor      0         47         1
    ##   virginica       0          3        49
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9733          
    ##                  95% CI : (0.9331, 0.9927)
    ##     No Information Rate : 0.3333          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.96            
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: setosa Class: versicolor Class: virginica
    ## Sensitivity                 1.0000            0.9400           0.9800
    ## Specificity                 1.0000            0.9900           0.9700
    ## Pos Pred Value              1.0000            0.9792           0.9423
    ## Neg Pred Value              1.0000            0.9706           0.9898
    ## Prevalence                  0.3333            0.3333           0.3333
    ## Detection Rate              0.3333            0.3133           0.3267
    ## Detection Prevalence        0.3333            0.3200           0.3467
    ## Balanced Accuracy           1.0000            0.9650           0.9750

## Plot results

![](Multiple_Regression_mit_V-Matrix_2024-11-13_files/figure-markdown_strict/unnamed-chunk-19-1.png)
