library(shiny)
shinyServer(function(input, output,session) {
  
  
  plot.htest <- function(test, col='royalblue', cex=0.8, shade.col='royalblue', ...)
  {
    # Z test
    if (test$method %in% c('One Sample z-test',
                           'Z test for mean')) {
      if (test$alternative == 'less')
        asbio::shade.norm(x=test$statistic, tail='lower',
                          las=1, shade.col=shade.col, ...)
      if (test$alternative == 'greater')
        asbio::shade.norm(x=test$statistic, tail='upper', 
                          las=1, shade.col=shade.col, ...)
      if (test$alternative == 'two.sided')
        asbio::shade.norm(x=test$statistic, tail="two", 
                          las=1, shade.col=shade.col, ...)
      title(main='Area sombreada correspondiente al Valor-P')
      mtext(text=round(test$statistic, digits=4),
            side=1, at=test$statistic, 
            col=col, cex=cex, adj=0.5)
    }
    
    # t test
    if (test$method %in% c('One Sample t-test',
                           'Welch Two Sample t-test',
                           ' Two Sample t-test',
                           'Two Sample t-test',
                           'Paired t-test')) {
      if (test$alternative == 'less')
        asbio::shade.t(x=test$statistic, tail='lower',
                       nu=test$parameter,
                       las=1, shade.col=shade.col, ...)
      if (test$alternative == 'greater')
        asbio::shade.t(x=test$statistic, tail='upper',
                       nu=test$parameter,
                       las=1, shade.col=shade.col, ...)
      if (test$alternative == 'two.sided')
        asbio::shade.t(x=test$statistic, tail="two",
                       nu=test$parameter,
                       las=1, shade.col=shade.col, ...)
      title(main='Area sombreada correspondiente al Valor-P')
      mtext(text=round(test$statistic, digits=4),
            side=1, at=test$statistic, 
            col=col, cex=cex, adj=0.5)
    }
    
    # Chi squared test
    if (test$method %in% c('X-squared test for variance')) {
      if (test$alternative == 'less')
        asbio::shade.chi(x=test$statistic, nu=test$parameter, 
                         tail='lower', las=1, shade.col=shade.col, ...)
      if (test$alternative == 'greater')
        asbio::shade.chi(x=test$statistic, nu=test$parameter, 
                         tail='upper', las=1, shade.col=shade.col, ...)
      if (test$alternative == 'two.sided')
        asbio::shade.chi(nu=test$parameter, tail="two",
                         las=1, shade.col=shade.col,
                         prob.to.each.tail=test$p.value/2, ...)
      title(main='Area sombreada correspondiente al Valor-P')
      mtext(text=round(test$statistic, digits=4),
            side=1, at=test$statistic, 
            col=col, cex=cex, adj=0.5)
    }
    
    # F test
    if (test$method %in% c('F test to compare two variances')) {
      if (test$alternative == 'less')
        asbio::shade.F(x=test$statistic,
                       nu1=test$parameter[1],
                       nu2=test$parameter[2], 
                       tail='lower', las=1, shade.col=shade.col, ...)
      if (test$alternative == 'greater')
        asbio::shade.F(x=test$statistic,
                       nu1=test$parameter[1],
                       nu2=test$parameter[2], 
                       tail='upper', las=1, shade.col=shade.col, ...)
      if (test$alternative == 'two.sided')
        asbio::shade.F(nu1=test$parameter[1],
                       nu2=test$parameter[2], 
                       tail="two", shade.col=shade.col,
                       prob.to.each.tail=test$p.value/2, las=1, ...)
      title(main='Area sombreada correspondiente al Valor-P')
      mtext(text=round(test$statistic, digits=4),
            side=1, at=test$statistic, 
            col=col, cex=cex, adj=0.5)
    }
  }
  
  
  ##################z_test
  
  z_test <- function(meanx, nx, sigma2=NULL,
                     alternative='two.sided',
                     mu=0, conf.level=0.95) {
    
    # Checking if the information is correct
    if (nx <= 0) 
      stop(paste("The sample size nx must be positive", "\n", ""))
    if (nx %% 1 != 0) 
      stop(paste("The sample size nx must be integer", "\n", ""))
    
    # Checking the sigma2 value
    if (is.null(sigma2) | !is.numeric(sigma2))
      stop(paste("Check the sigma2 value", "\n", ""))
    if (sigma2 <= 0)
      stop("The variance sigma2 must be positive")
    
    # Argument Verification Using Partial Matching
    alternative <- match.arg(arg=alternative,
                             choices=c("two.sided","greater","less"))
    
    alpha <- 1 - conf.level
    
    # Alternative two.sided
    if (alternative == 'two.sided') {
      statistic <- (meanx - mu) / sqrt(sigma2 / nx)
      p.value <- 2 * pnorm(q=abs(statistic), lower.tail=FALSE)
      quantiles <- c(-qnorm(p=alpha/2, lower.tail=FALSE),
                     qnorm(p=alpha/2, lower.tail=FALSE))
      conf.int <- meanx + quantiles * sqrt(sigma2 / nx)
    }
    # Alternative less
    if (alternative == 'less') {
      statistic <- (meanx - mu) / sqrt(sigma2 / nx)
      p.value <- pnorm(q=statistic, lower.tail=TRUE)
      conf.int <- c(-Inf,
                    meanx + qnorm(p=1-alpha) * sqrt(sigma2 / nx))
    }
    # Alternative greater
    if (alternative == 'greater') {
      statistic <- (meanx - mu) / sqrt(sigma2 / nx)
      p.value <- pnorm(q=statistic, lower.tail=FALSE)
      conf.int <- c(meanx + qnorm(p=alpha) * sqrt(sigma2 / nx),
                    Inf)
    }
    
    # To ensure that the output values are in the correct form
    names(statistic) <- 'Z'
    attr(conf.int, 'conf.level') <- conf.level
    estimate <- meanx
    names(estimate) <- 'mean of x'
    method <- 'Z test for mean'
    data.name <- deparse(substitute(x))
    
    res <- list(statistic=statistic,
                p.value=p.value,
                conf.int=conf.int,
                estimate=estimate,
                null.value=mu,
                alternative=alternative,
                method=method,
                data.name=data.name)
    
    class(res) <- "htest"
    res
    
    
  }
  
  
  
#####################t_test
  
  
  t_test <- function(meanx, varx, nx, 
                     meany=NULL, vary=NULL, ny=NULL, 
                     alternative='two.sided', mu=0, 
                     conf.level=0.95, var.equal=FALSE){
    
    # Checking if the information is correct
    
    # To check if the information about x sample is correct
    if (varx <= 0) 
      stop(paste("The variance x must be positive", "\n", ""))
    if (nx <= 0) 
      stop(paste("The sample size nx must be positive", "\n", ""))
    if (nx %% 1 != 0) 
      stop(paste("The sample size nx must be integer", "\n", ""))
    
    # To check if the user provided information about y sample
    if (xor(is.null(vary), is.null(ny))) 
      stop("Some information about sample y is missing", "\n", "")
    
    # To check if the information about y sample is corrected
    if (! is.null(vary) & ! is.null(ny) & ! is.null(meany)) {
      if (vary <= 0) 
        stop(paste("The variance y must be positive", "\n", ""))
      if (ny <= 0) 
        stop(paste("The sample size ny must be positive", "\n", ""))
      if (ny %% 1 != 0) 
        stop(paste("The sample size ny must be integer", "\n", ""))
      if (is.null(meany) == TRUE)
        stop(paste("Some information about y sample is missing", "\n",""))
    }
    
    # To check if the conf.level it's a number between 1 and 0
    if(conf.level <= 0 || conf.level >= 1)
      stop("The conf.level argument must be > 0 and < 1", "\n", "" )
    if(conf.level < 0.5)
      warning("Confidence levels are often close to 1, eg. 0.95")
    
    # Argument Verification Using Partial Matching
    alternative <- match.arg(arg=alternative,
                             choices=c("two.sided","greater","less"))
    
    # To perform the test
    if (is.null(meany) & is.null(vary) & is.null(ny))
      res <- t_test_one(meanx, varx, nx, alternative, mu, conf.level)
    
    else {
      if (var.equal == TRUE)
        res <- t_test_two_equal(meanx, varx, nx, meany, vary, ny, 
                                alternative, mu, conf.level, var.equal)
      
      if (var.equal == FALSE)
        res <- t_test_two_difer(meanx, varx, nx, meany, vary, ny, 
                                alternative, mu, conf.level, var.equal)
    }
    
    class(res) <- "htest"
    res   
    
  }
  #' @rdname t_test
  #' @importFrom stats pt qt
  #' @export
  t_test_one <- function(meanx, varx, nx, alternative, mu,
                         conf.level) {
    
    alpha <- 1 - conf.level
    
    if (alternative == 'two.sided') {
      statistic <- (meanx - mu) / sqrt(varx / nx)
      p.value <- 2 * pt(q=abs(statistic), df=nx-1, lower.tail=FALSE)
      quantiles <- c(-qt(p=alpha/2, df=nx-1, lower.tail=FALSE),
                     qt(p=alpha/2, df=nx-1, lower.tail=FALSE))
      conf.int <- meanx + quantiles * sqrt(varx / nx)
    }
    
    if (alternative == 'less') {
      statistic <- (meanx - mu) / sqrt(varx / nx)
      p.value <- pt(q=statistic, df=nx-1, lower.tail=TRUE)
      conf.int <- c(-Inf,
                    meanx + qt(p=1-alpha, df=nx-1) * sqrt(varx / nx))
    }
    
    if (alternative == 'greater') {
      statistic <- (meanx - mu) / sqrt(varx / nx)
      p.value <- pt(q=statistic, df=nx-1, lower.tail=FALSE)
      conf.int <- c(meanx + qt(p=alpha, df=nx-1) * sqrt(varx / nx),
                    Inf)
    }
    
    # To ensure that the output values are in the correct form
    names(statistic) <- 't'
    parameter <- nx - 1
    names(parameter) <- 'df'
    attr(conf.int, 'conf.level') <- conf.level
    estimate <- meanx
    names(estimate) <- 'mean of x'
    null.value <- mu
    names(null.value) <- 'mean'
    method <- 'One Sample t-test'
    data.name <- paste('meanx = ', meanx, ', var = ', varx, ' and nx = ', nx, sep='')
    
    res <- list(statistic=statistic,
                parameter=parameter,
                p.value=p.value,
                conf.int=conf.int,
                estimate=estimate,
                null.value=null.value,
                alternative=alternative,
                method=method,
                data.name=data.name)
    return(res)
  }
  #' @rdname t_test
  #' @importFrom stats pt qt
  #' @export
  t_test_two_difer <- function(meanx, varx, nx,
                               meany, vary, ny,
                               alternative, mu,
                               conf.level, var.equal) {
    
    
    alpha <- 1 - conf.level
    
    df <- (varx/nx + vary/ny)^2 / ((varx/nx)^2 / (nx-1) + (vary/ny)^2 / (ny-1))
    se <- sqrt(varx/nx + vary/ny)
    statistic <- (meanx - meany - mu) / se
    
    if (alternative == 'two.sided') {
      p.value <- 2 * pt(q=abs(statistic), df=df, lower.tail=FALSE)
      quantiles <- c(-qt(p=alpha/2, df=df, lower.tail=FALSE),
                     qt(p=alpha/2, df=df, lower.tail=FALSE))
      conf.int <- (meanx-meany) + quantiles * se
    }
    
    if (alternative == 'less') {
      p.value <- pt(q=statistic, df=df, lower.tail=TRUE)
      conf.int <- c(-Inf,
                    (meanx-meany) + qt(p=alpha, df=df, lower.tail=F) * se)
    }
    
    if (alternative == 'greater') {
      p.value <- pt(q=statistic, df=df, lower.tail=FALSE)
      conf.int <- c((meanx-meany) - qt(p=alpha, df=df, lower.tail=F) * se,
                    Inf)
    }
    # To ensure that the output values are in the correct form
    names(statistic) <- 't'
    parameter <- df
    names(parameter) <- 'df'
    attr(conf.int, 'conf.level') <- conf.level
    estimate <- c(meanx, meany)
    names(estimate) <- c('mean of x', 'mean of y')
    null.value <- mu
    names(null.value) <- 'difference in means'
    method <- 'Welch Two Sample t-test'
    data.name <- paste('meanx = ', meanx, ', nx = ', nx,
                       ', meany = ', meany, ' and ny = ', ny, sep='')
    
    res <- list(statistic=statistic,
                parameter=parameter,
                p.value=p.value,
                conf.int=conf.int,
                estimate=estimate,
                null.value=null.value,
                alternative=alternative,
                method=method,
                data.name=data.name)
    return(res)
  }
  #' @rdname t_test
  #' @importFrom stats pt qt
  #' @export
  t_test_two_equal <- function(meanx, varx, nx,
                               meany, vary, ny,
                               alternative, mu,
                               conf.level, var.equal) {
    
    alpha <- 1 - conf.level
    
    df <- nx + ny - 2
    sp2 <- ((nx-1) * varx + (ny-1) * vary) / df
    se <- sqrt(sp2/nx + sp2/ny)
    statistic <- (meanx - meany - mu) / se
    
    if (alternative == 'two.sided') {
      mu <- as.numeric(mu)
      alt <- paste("true difference in means is not equal to", mu)
      p.value <- 2 * pt(q=abs(statistic), df=df, lower.tail=FALSE)
      quantiles <- c(-qt(p=alpha/2, df=df, lower.tail=FALSE),
                     qt(p=alpha/2, df=df, lower.tail=FALSE))
      conf.int <- (meanx-meany) + quantiles * se
    }
    
    if (alternative == 'less') {
      mu <- as.numeric(mu)
      alt <- paste("true difference in means is less than", mu)
      p.value <- pt(q=statistic, df=df, lower.tail=TRUE)
      conf.int <- c(-Inf,
                    (meanx - meany) + qt(p=alpha, df=df, lower.tail=F) * se)
    }
    
    if (alternative == 'greater') {
      mu <- as.numeric(mu)
      alt<- paste("true difference in means is greater than", mu)
      p.value <- pt(q=statistic, df=df, lower.tail=FALSE)
      conf.int <- c((meanx-meany) - qt(p=alpha, df=df, lower.tail=F) * se,
                    Inf)
    }
    
    # To ensure that the output values are in the correct form
    names(statistic) <- 't'
    parameter <- df
    names(parameter) <- 'df'
    attr(conf.int, 'conf.level') <- conf.level
    estimate <- c(meanx, meany)
    names(estimate) <- c('mean of x', 'mean of y')
    null.value <- mu
    names(null.value) <- 'difference in means'
    method <- 'Two Sample t-test'
    data.name <- paste('meanx =', meanx, ', nx =', nx,
                       ', meany =', meany, 'and ny =', ny)
    
    res <- list(statistic = statistic,
                parameter = parameter,
                p.value = p.value,
                conf.int = conf.int,
                estimate = estimate,
                null.value = null.value,
                alternative = alternative,
                method = method,
                data.name = data.name)
    return(res)
  }
 
  
  ###############var_test
  
  var_test <- function(varx, nx, vary=NULL, ny=NULL,
                       alternative='two.sided',
                       null.value=1, conf.level=0.95) {
    
    # Checking if the information is correct
    
    # To check if the information about x sample is correct
    if (varx <= 0) 
      stop(paste("The variance x must be positive", "\n", ""))
    if (nx <= 0) 
      stop(paste("The sample size nx must be positive", "\n", ""))
    if (nx %% 1 != 0) 
      stop(paste("The sample size nx must be integer", "\n", ""))
    
    # To check if the user provided information about y sample
    if (xor(is.null(vary), is.null(ny))) 
      stop("Some information about sample y is missing", "\n", "")
    
    # To check if the information about x sample is correct
    if (! is.null(vary) & ! is.null(ny)) {
      if (vary <= 0) 
        stop(paste("The variance y must be positive", "\n", ""))
      if (ny <= 0) 
        stop(paste("The sample size ny must be positive", "\n", ""))
      if (ny %% 1 != 0) 
        stop(paste("The sample size ny must be integer", "\n", ""))
    }
    
    # To check if the conf.level is a number between 1 and 0
    if(conf.level <= 0 || conf.level >= 1)
      stop("The conf.level argument must be > 0 and < 1", "\n", "" )
    if(conf.level < 0.5)
      warning("Confidence levels are often close to 1")
    
    # To check if the null.value is positive
    if(null.value <= 0)
      stop(paste("The null value must be positive", "\n", ""))
    
    # Argument Verification Using Partial Matching
    alternative <- match.arg(arg=alternative,
                             choices=c("two.sided","greater","less"))
    
    if (is.null(vary))
      res <- var_test_one(varx, nx, alternative, 
                          conf.level, null.value)
    else
      res <- var_test_two(varx, nx, vary, ny, 
                          alternative, conf.level, null.value)
    
    class(res) <- "htest"
    res
  }
  #' @rdname var_test
  #' @importFrom stats pchisq qchisq
  #' @export
  var_test_one <- function(varx, nx, alternative, conf.level, null.value) {
    alpha <- 1 - conf.level
    # Alternative two.sided
    if (alternative == 'two.sided') { 
      quantiles <- c(qchisq(p=alpha/2, df=nx-1, lower.tail=F),
                     qchisq(p=1-alpha/2, df=nx-1, lower.tail=F))
      conf.int <- (nx-1) * varx / quantiles
      statistic <- (nx-1) * varx / null.value
      p.value <- 2 * min(c(pchisq(statistic, nx-1, lower.tail=F),
                           pchisq(statistic, nx-1, lower.tail=T)))
    }
    # Alternative less
    if (alternative == 'less') {
      quantiles <- c(qchisq(p=conf.level, df=nx-1, lower.tail=T),
                     0)
      conf.int <- (nx-1) * varx / quantiles
      statistic <- (nx-1) * varx / null.value
      p.value <- pchisq(statistic, nx-1)
    }
    # Alternative greater
    if (alternative == 'greater') {
      quantiles <- c(Inf,
                     qchisq(p=conf.level, df=nx-1, lower.tail=F))
      conf.int <- (nx-1) * varx / quantiles
      statistic <- (nx-1) * varx / null.value
      p.value <- pchisq(statistic, nx-1, lower.tail=F)
    }
    
    # To ensure that the output values are in the correct form
    names(statistic) <- 'X-squared'
    parameter <- nx - 1
    names(parameter) <- 'df'
    attr(conf.int, 'conf.level') <- conf.level
    estimate <- varx
    names(estimate) <- 'variance of x'
    method <- 'X-squared test for variance'
    data.name <- paste('varx =', varx, 'and nx =', nx)
    
    res <- list(statistic=statistic,
                parameter=parameter,
                p.value=p.value,
                conf.int=conf.int,
                estimate=estimate,
                null.value=null.value,
                alternative=alternative,
                method=method,
                data.name=data.name)
    return(res)
  }
  #' @rdname var_test
  #' @importFrom stats pf qf
  #' @export
  var_test_two <- function(varx, nx, vary, ny, 
                           alternative, conf.level, null.value) {
    alpha <- 1 - conf.level
    # Alternative two.sided
    if (alternative == 'two.sided') { 
      quantiles <- c(qf(p=alpha/2,   df1=nx-1, df2=ny-1, lower.tail=F),
                     qf(p=1-alpha/2, df1=nx-1, df2=ny-1, lower.tail=F))
      conf.int <- (varx / vary) / quantiles
      statistic <- (varx / vary) / null.value
      p.value <- 2 * min(c(pf(statistic, nx-1, ny-1, lower.tail=F),
                           pf(statistic, nx-1, ny-1, lower.tail=T)))
    }
    # Alternative less
    if (alternative == 'less') {
      quantiles <- c(Inf,
                     qf(p=conf.level, df1=nx-1, df2=ny-1, lower.tail=F))
      conf.int <- (varx / vary) / quantiles
      statistic <- (varx / vary) / null.value
      p.value <- pf(q=statistic, df1=nx-1, df2=ny-1, lower.tail=T)
    }
    # Alternative greater
    if (alternative == 'greater') {
      quantiles <- c(qf(p=conf.level, df1=nx-1, df2=ny-1, lower.tail=T),
                     0)
      conf.int <- (varx / vary) / quantiles
      statistic <- (varx / vary) / null.value
      p.value <- pf(q=statistic, df1=nx-1, df2=ny-1, lower.tail=F)
    }
    
    # To ensure that the output values are in the correct form
    names(statistic) <- 'F'
    parameter <- c(nx-1, ny-1)
    names(parameter) <- c('num df', 'denom df')
    attr(conf.int, 'conf.level') <- conf.level
    estimate <- varx / vary
    names(estimate) <- 'ratio of variances'
    method <- 'F test to compare two variances'
    data.name <- paste('varx =', varx, ', nx =', nx,
                       ', vary =', vary, 'and ny =', ny)
    
    res <- list(statistic=statistic,
                parameter=parameter,
                p.value=p.value,
                conf.int=conf.int,
                estimate=estimate,
                null.value=null.value,
                alternative=alternative,
                method=method,
                data.name=data.name)
    return(res)
  }
  
  
  
  
  
  
  output$miplot <- renderPlot({
    
  
  if(input$Prueba == "PH. para la media"){
    if(input$header == "Si"){
      
      meanx <- input$media
      sigma2 <- input$varT
      nx <- input$nx
      mu <- input$mu0m
      alternative <- input$h0m
      conf.level <- input$alfa
      
      
      ph1 <- z_test(meanx=input$media, nx=input$nx, sigma2=input$varT, 
                    alternative=input$h0m,mu=input$mu0m,conf.level=input$alfa)
      plot.htest(ph1)
      output$resul1 <-renderText({
        phmedia <- z_test(meanx=input$media, nx=input$nx, sigma2=input$varT, 
                          alternative=input$h0m,mu=input$mu0m,conf.level=input$alfa)
        conclusión <- ifelse(phmedia$p.value < 0.05, 'es rechazada',
                             'no es rechazada')
        paste0('El estadístico de prueba es zo=', round(phmedia$statistic, 4),
               ' con un valor-P de ', round(phmedia$p.value, 4), ', por esta razón
               se puede concluir que, dada la información de la muestra, 
               la hipótesis nula ', conclusión, 
               ' (a un nivel de significancia del 5%).')
      })
      
      output$resul2 <- renderText({
        phmedia <- z_test(meanx=input$media, nx=input$nx, sigma2=input$varT, 
                          alternative=input$h0m,mu=input$mu0m,conf.level=input$alfa)
        intervalo <- paste("(", round(phmedia$conf.int[1], digits=4), ", ",
                           round(phmedia$conf.int[2], digits=4), ").", sep='')
        paste0('El intervalo de confianza del ', 100*input$alfa,
               '% para la media poblacional es ', intervalo)
      })
    }
  
      else{
      
      meanx <- input$media
      varx <- input$varZ
      nx <- input$nx
      mu <- input$mu0m
      alternative <- input$h0m
      conf.level <- input$alfa
      
      
      ph2 <- t_test(meanx=input$media, varx=input$varZ, nx=input$nx, 
                    alternative=input$h0m,mu=input$mu0m,conf.level=input$alfa)
      plot.htest(ph2)
      
      output$resul1 <-renderText({
        phmedian <- t_test(meanx=input$media, varx=input$varZ, nx=input$nx, 
                           alternative=input$h0m,mu=input$mu0m,conf.level=input$alfa)
        conclusión <- ifelse(phmedian$p.value < 0.05, 'es rechazada',
                             'no es rechazada')
        paste0('El estadístico de prueba es to=', round(phmedian$statistic, 4),
               ' con un valor-P de ', round(phmedian$p.value, 4), ', por esta razón
               se puede concluir que, dada la información de la muestra, 
               la hipótesis nula ', conclusión, 
               ' (a un nivel de significancia del 5%).')
      })
      
      
      output$resul2 <- renderText({
        phmedian <- t_test(meanx=input$media, varx=input$varZ, nx=input$nx, 
                           alternative=input$h0m,mu=input$mu0m,conf.level=input$alfa)
        intervalo <- paste("(", round(phmedian$conf.int[1], digits=4), ", ",
                           round(phmedian$conf.int[2], digits=4), ").", sep='')
        paste0('El intervalo de confianza del ', 100*input$alfa,
               '% para la media poblacional es ', intervalo)
      })

      
      }
    
  }
    
    if(input$Prueba == "PH. para la varianza"){
      
      varx <- input$varianza
      nx <- input$nxv
      null.value <- input$sigma0
      conf.level <- input$alfa2
      alternative<- input$h0v
      
      
      ph3 <- var_test(varx =input$varianza , nx=input$nxv, 
                    alternative=input$h0v, null.value = input$sigma0,conf.level=input$alfa2)
      
      plot.htest(ph3)
      output$resul1 <-renderText({
        
        phvari <- var_test(varx =input$varianza , nx=input$nxv, 
                           alternative=input$h0v, null.value = input$sigma0,conf.level=input$alfa2)
        conclusión <- ifelse(phvari$p.value < 0.05, 'es rechazada',
                             'no es rechazada')
        paste0('El estadístico de prueba es xo=', round(phvari$statistic, 4),
               ' con un valor-P de ', round(phvari$p.value, 4), ', por esta razón
               se puede concluir que, dada la información de la muestra, 
               la hipótesis nula ', conclusión, 
               ' (a un nivel de significancia del 5%).')
      })
      
      output$resul2 <- renderText({
        phvari <- var_test(varx =input$varianza , nx=input$nxv, 
                           alternative=input$h0v, null.value = input$sigma0,conf.level=input$alfa2)
        intervalo <- paste("(", round(phvari$conf.int[1], digits=4), ", ",
                           round(phvari$conf.int[2], digits=4), ").", sep='')
        paste0('El intervalo de confianza del ', 100*input$alfa2,
               '% para la varianza poblacional es ', intervalo)
      })
      
    }
    
    if(input$Prueba == "PH. para la diferencia de medias"){
        
        meanx <- input$mediap1
        meany <- input$mediap2
        varx <- input$varp22
        vary <- input$varp32
        nx <- input$nxv1
        ny <- input$nxv2
        alternative <- input$h0v
        mu <- input$delta0
        conf.level <- input$alfa3
        var.equal <- input$iguales
        
        
        ph4 <- t_test(meanx=input$mediap1, meany=input$mediap2,varx=input$varp22,vary=input$varp32, nx=input$nxv1, 
                      ny=input$nxv2,alternative=input$h0v,mu=input$delta0,conf.level=input$alfa3,var.equal = input$iguales)
       
         plot.htest(ph4)
        
        output$resul1 <-renderText({
          phdife <- t_test(meanx=input$mediap1, meany=input$mediap2,varx=input$varp22,vary=input$varp32, nx=input$nxv1, 
                           ny=input$nxv2,alternative=input$h0v,mu=input$delta0,conf.level=input$alfa3, var.equal = input$iguales)
          conclusión <- ifelse(phdife$p.value < 0.05, 'es rechazada',
                               'no es rechazada')
          paste0('El estadístico de prueba es to=', round(phdife$statistic, 4),
                 ' con un valor-P de ', round(phdife$p.value, 4), ', por esta razón
               se puede concluir que, dada la información de la muestra, 
               la hipótesis nula ', conclusión, 
                 ' (a un nivel de significancia del 5%).')
        })
        
        
        output$resul2 <- renderText({
          phdife <- t_test(meanx=input$mediap1, meany=input$mediap2,varx=input$varp22,vary=input$varp32, nx=input$nxv1, 
                             ny=input$nxv2,alternative=input$h0v,mu=input$delta0,conf.level=input$alfa3)
          intervalo <- paste("(", round(phdife$conf.int[1], digits=4), ", ",
                             round(phdife$conf.int[2], digits=4), ").", sep='')
          paste0('El intervalo de confianza del ', 100*input$alfa3,
                 '% para la diferencia de medias poblacionales es ', intervalo)
        })
      
    }
    
    
    if(input$Prueba == "PH. para el cociente de varianzas"){
      
      
      varx <- input$varc1
      vary <- input$varc2
      nx <- input$nxvar
      ny <- input$nxvar2
      alternative <- input$pi
      conf.level <- input$alfa4
      
      
      ph5 <- var_test(varx=input$varc1,vary=input$varc2, nx=input$nxvar, 
                    ny=input$nxvar2,alternative=input$pi,conf.level=input$alfa4)
      
      plot.htest(ph5)
      
      output$resul1 <-renderText({
        phcoci <- var_test(varx=input$varc1,vary=input$varc2, nx=input$nxvar, 
                           ny=input$nxvar2,alternative=input$pi,conf.level=input$alfa4)
        conclusión <- ifelse(phcoci$p.value < 0.05, 'es rechazada',
                             'no es rechazada')
        paste0('El estadístico de prueba es Fo=', round(phcoci$statistic, 4),
               ' con un valor-P de ', round(phcoci$p.value, 4), ', por esta razón
               se puede concluir que, dada la información de la muestra, 
               la hipótesis nula ', conclusión, 
               ' (a un nivel de significancia del 5%).')
      })
      
      
      output$resul2 <- renderText({
        phcoci <- var_test(varx=input$varc1,vary=input$varc2, nx=input$nxvar, 
                           ny=input$nxvar2,alternative=input$pi,conf.level=input$alfa4)
        intervalo <- paste("(", round(phcoci$conf.int[1], digits=4), ", ",
                           round(phcoci$conf.int[2], digits=4), ").", sep='')
        paste0('El intervalo de confianza del ', 100*input$alfa4,
               '% para el cociente de varianzas poblacionales es ', intervalo)
      })
      
    }
    
    if(input$Prueba == "PH. para la diferencia de proporciones"){
      
      
      x <- c(input$primpobla, input$segpobla)
      n <- c(input$primnpobla, input$segnpobla)
      p <- c(input$pp, input$ps)
      alternative <- input$P0
      conf.level <- input$alfa5
      correct <- input$headerpp
      
      ph6 <- prop.test(x=c(input$primpobla, input$segpobla),n=c(input$primnpobla, input$segnpobla), p=c(input$pp, input$ps), 
                       alternative=input$P0,conf.level=input$alfa5, correct=input$headerpp)
      
      plot.htest(ph6)
      
      output$resul1 <-renderText({
        phprop <- prop.test(x=c(input$primpobla, input$segpobla),n=c(input$primnpobla, input$segnpobla), p=c(input$pp, input$ps), 
                           alternative=input$P0,conf.level=input$alfa5, correct=input$headerpp)
        conclusión <- ifelse(phprop$p.value < 0.05, 'es rechazada',
                              'no es rechazada')
        paste0('El estadístico de prueba es Xo=', round(phprop$statistic, 4),
               ' con un valor-P de ', round(phprop$p.value, 4), ', por esta razón
               se puede concluir que, dada la información de la muestra, 
               la hipótesis nula ', conclusión, 
               ' (a un nivel de significancia del 5%).')
      })
      
      output$resul2 <- renderText({})
    }
    
    if(input$Prueba == "PH. para proporciones"){
      
      
      x <- input$propor
      n <- input$proporn
      p <- input$ppropor
      alternative <- input$proporP0
      conf.level <- input$alfa6
      correct <- input$headerpropor
      
      ph6 <- prop.test(x=input$propor, input$segpobla,n=input$proporn, p=input$ppropor, 
                       alternative=input$proporP0,conf.level=input$alfa6, correct=input$headerpropor)
      
      plot.htest(ph6)
      
      output$resul1 <-renderText({
        phprop <- prop.test(x=input$propor, input$segpobla,n=input$proporn, p=input$ppropor, 
                            alternative=input$proporP0,conf.level=input$alfa6, correct=input$headerpropor)
        conclusión <- ifelse(phprop$p.value < 0.05, 'es rechazada',
                              'no es rechazada')
        paste0('El estadístico de prueba es Xo=', round(phprop$statistic, 4),
               ' con un valor-P de ', round(phprop$p.value, 4), ', por esta razón
               se puede concluir que, dada la información de la muestra, 
               la hipótesis nula ', conclusión, 
               ' (a un nivel de significancia del 5%).')
      })
      
      output$resul2 <- renderText({})
    }
    
    })#miplot
  })
  