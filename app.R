library(shiny)
library(ggplot2)
library(extraDistr)

ui <- fluidPage(
  selectInput("distribution", "Select a Distribution:", c("Binomial", "Normal","Poisson", "Gamma", "Exponential", "Weibull", "Hypergeometric", "Log-normal", "Negative binomial", "Gamma", "Beta", "Fisher", "Cauchy", "Chy-Squared", "Laplace", "Pareto")),
  conditionalPanel(condition = "input.distribution == 'Binomial'",
                   textInput("size", "Size:", value = 10),
                   textInput("prob", "Probability:", value = 0.5)),
  conditionalPanel(condition = "input.distribution == 'Normal'",
                   textInput("mean", "Mean:", value = 0),
                   textInput("sd", "Standard Deviation:", value = 1)),
  conditionalPanel(condition = "input.distribution == 'Poisson'",
                   textInput("lambda", "Lambda:", value = 1)),
  conditionalPanel(condition = "input.distribution == 'Gamma'",
                   textInput("shape", "Shape:", value = 1),
                   textInput("scale", "Scale:", value = 1)),
  conditionalPanel(condition = "input.distribution == 'Exponential'",
                   textInput("rate", "Rate:", value = 1)),
  conditionalPanel(condition = "input.distribution == 'Weibull'",
                   textInput("shapeW", "Shape:", value = 1),
                   textInput("scaleW", "Scale:", value = 1)),
  conditionalPanel(condition = "input.distribution == 'Hypergeometric'",
                   textInput("m", "M:", value = 1),
                   textInput("n", "N:", value = 1),
                   textInput("k", "K:", value = 1)),
  conditionalPanel(condition = "input.distribution == 'Log-normal'",
                   textInput("ml", "MeanLog:", value = 1),
                   textInput("sl", "SdLog:", value = 1)),
  conditionalPanel(condition = "input.distribution == 'Negative binomial'",
                   textInput("s", "Size:", value = 1),
                   textInput("p", "Prob:", value = 1)),
  conditionalPanel(condition = "input.distribution == 'Beta'",
                   textInput("s1", "Shape1:", value = 1),
                   textInput("s2", "Shape2:", value = 1),
                   textInput("ncp", "Ncp:", value = 1)),
  
  conditionalPanel(condition = "input.distribution == 'Fisher'",
                   textInput("df1", "Df1:", value = 1),
                   textInput("df2", "Df2:", value = 1),
                   textInput("ncp", "Ncp:", value = 1)),
  
  conditionalPanel(condition = "input.distribution == 'Cauchy'",
                   textInput("l", "Location:", value = 1),
                   textInput("s", "Scale:", value = 1)),
  
  conditionalPanel(condition = "input.distribution == 'Chy-Squared'",
                   textInput("df", "Df:", value = 1),
                   textInput("ncp", "Ncp:", value = 1)),
  
  conditionalPanel(condition = "input.distribution == 'Laplace'",
                   textInput("mu", "Mu:", value = 1),
                   textInput("sigma", "Sigma:", value = 1)),
  
  conditionalPanel(condition = "input.distribution == 'Pareto'",
                   textInput("aP", "A:", value = 1),
                   textInput("bP", "B:", value = 1)),
  
  textInput("a","a:"),
  textInput("b","b:"),
  tabsetPanel(type = "tabs",
              tabPanel("Density Function", plotOutput("densityPlot")),
              tabPanel("Mass Function", plotOutput("massPlot")),
              tabPanel("Repartition Function", plotOutput("cdfPlot"))
  )
)

server <- function(input, output) {
  
  output$densityPlot <- renderPlot({
    dist <- input$distribution
    a <- as.numeric(input$a)
    b <- as.numeric(input$b)
    if (dist == "Binomial") {
      size <- as.numeric(input$size)
      prob <- as.numeric(input$prob)
      x <- 0:size
      y <- dbinom(x, size = size, prob = prob)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(size =", size, ", prob =", prob, ")"))
    } else if (dist == "Normal") {
      mean <- as.numeric(input$mean)
      sd <- as.numeric(input$sd)
      x <- seq(mean-4*sd, mean+4*sd, length.out = 100)
      y <- dnorm(x, mean = mean, sd = sd)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(mean =", mean, ", sd =", sd, ")"))
    } else if (dist == "Poisson") {
      lambda <- as.numeric(input$lambda)
      x <- 0:30
      y <- dpois(x, lambda = lambda)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(lambda =", lambda, ")"))
    } else if (dist == "Gamma") {
      shape <- as.numeric(input$shape)
      scale <- as.numeric(input$scale)
      x <- seq(0, shape*scale*2, length.out = 100)
      y <- dgamma(x, shape = shape, scale = scale)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(shape =", shape, ", scale =", scale, ")"))
    }else if (dist == "Exponential") {
      rate <- as.numeric(input$rate)
      x <- seq(0, 2/rate, length.out = 100)
      y <- dexp(x, rate = rate)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(rate =", rate, ")"))
    }
    else if (dist == "Weibull") {
      shape <- as.numeric(input$shapeW)
      scale <- as.numeric(input$scaleW)
      x <- seq(0, shape*scale*2, length.out = 100)
      y <- dweibull(x, shape = shape, scale = scale)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(shape =", shape, ", scale =", scale, ")"))
    }
    else if(dist == "Hypergeometric"){
      m <- as.numeric(input$m)
      n <- as.numeric(input$n)
      k <- as.numeric(input$k)
      x <- 0:25
      y <- dhyper(x, m = m, n = n, k = k)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(M =", m, ", N =", n, ", K =", k ,")"))
    }
    else if(dist == "Log-normal"){
      ml <- as.numeric(input$ml)
      sl <- as.numeric(input$sl)
      x <- 0:25
      y <- dlnorm(x, meanlog = ml, sdlog = sl)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(MeanLog =", ml, ", SdLog =", sl, ")"))
    }
    else if(dist == "Negative binomial"){
      s <- as.numeric(input$s)
      p <- as.numeric(input$p)
      x <- 0:25
      y <- dnbinom(x, size = s, prob = p)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Size =", s, ", Prob =", p,")"))
    }
    else if(dist == "Beta"){
      s1 <- as.numeric(input$s1)
      s2 <- as.numeric(input$s2)
      ncp <- as.numeric(input$ncp)
      x <- 0:25
      y <- dbeta(x, shape1 = s1, shape2 = s2, ncp = ncp)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Shape1 =", s1, ", Shape2 =", s2, " , Ncp = ", ncp ,")"))
    }
    else if(dist == "Fisher"){
      df1 <- as.numeric(input$df1)
      df2 <- as.numeric(input$df2)
      ncp <- as.numeric(input$ncp)
      x <- 0:25
      y <- df(x, df1 = df1,df2 = df2, ncp = ncp)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Df1 =", df1, ", Df2 =", df2, " , Ncp = ", ncp ,")"))
    }
    else if(dist == "Cauchy"){
      l <- as.numeric(input$l)
      s <- as.numeric(input$s)
      x <- 0:25
      y <- dcauchy(x, location = l, scale = s)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Location =", l, ", Scale =", s,")"))
    }
    else if(dist == "Chy-Squared"){
      df <- as.numeric(input$df)
      ncp <- as.numeric(input$ncp)
      x <- 0:25
      y <- dchisq(x, df = df, ncp = ncp)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Df =", df," , Ncp = ", ncp ,")"))
    }
    else if(dist == "Laplace"){
      m <- as.numeric(input$mu)
      s <- as.numeric(input$sigma)
      x <- 0:25
      y <- dlaplace(x, mu = m, sigma = s)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Mu =", m," , Sigma = ", s ,")"))
    }
    else if(dist == "Pareto"){
      aP <- as.numeric(input$aP)
      bP <- as.numeric(input$bP)
      x <- 0:25
      y <- dpareto(x, a = aP, b = bP)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(A =", aP," , B = ", bP ,")"))
    }
  })
  
  output$massPlot <- renderPlot({
    dist <- input$distribution
    a <- as.numeric(input$a)
    b <- as.numeric(input$b)
    if (dist == "Binomial") {
      size <- as.numeric(input$size)
      prob <- as.numeric(input$prob)
      x <- 0:size
      y <- dbinom(x, size = size, prob = prob, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(size =", size, ", prob =", prob, ")"))
    } else if (dist == "Normal") {
      mean <- as.numeric(input$mean)
      sd <- as.numeric(input$sd)
      x <- seq(mean-4*sd, mean+4*sd, length.out = 100)
      y <- dnorm(x, mean = mean, sd = sd, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(mean =", mean, ", sd =", sd, ")"))
    } else if (dist == "Poisson") {
      lambda <- as.numeric(input$lambda)
      x <- 0:30
      y <- dpois(x, lambda = lambda, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(lambda =", lambda, ")"))
    } else if (dist == "Gamma") {
      shape <- as.numeric(input$shape)
      scale <- as.numeric(input$scale)
      x <- seq(0, shape*scale*2, length.out = 100)
      y <- dgamma(x, shape = shape, scale = scale, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(shape =", shape, ", scale =", scale, ")"))
    } else if (dist == "Exponential") {
      rate <- as.numeric(input$rate)
      x <- seq(0, 2/rate, length.out = 100)
      y <- dexp(x, rate = rate, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(rate =", rate, ")"))
    }
    else if (dist == "Weibull") {
      shape <- as.numeric(input$shapeW)
      scale <- as.numeric(input$scaleW)
      x <- seq(0, shape*scale*2, length.out = 100)
      y <- dweibull(x, shape = shape, scale = scale, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(shape =", shape, ", scale =", scale, ")"))
    }else if(dist == "Hypergeometric"){
      m <- as.numeric(input$m)
      n <- as.numeric(input$n)
      k <- as.numeric(input$k)
      x <- 0:25
      y <- dhyper(x, m = m, n = n, k = k, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(M =", m, ", N =", n, ", K =", k ,")"))
    }else if(dist == "Log-normal"){
      ml <- as.numeric(input$ml)
      sl <- as.numeric(input$sl)
      x <- 0:25
      y <- dlnorm(x, meanlog = ml, sdlog = sl, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(MeanLog =", ml, ", SdLog =", sl, ")"))
    }else if(dist == "Negative binomial"){
      s <- as.numeric(input$s)
      p <- as.numeric(input$p)
      x <- 0:25
      y <- dnbinom(x, size = s, prob = p, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Size =", s, ", Prob =", p,")"))
    }else if(dist == "Beta"){
      s1 <- as.numeric(input$s1)
      s2 <- as.numeric(input$s2)
      ncp <- as.numeric(input$ncp)
      x <- 0:25
      y <- dbeta(x, shape1 = s1, shape2 = s2, ncp = ncp, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Shape1 =", s1, ", Shape2 =", s2, " , Ncp = ", ncp ,")"))
    }else if(dist == "Fisher"){
      df1 <- as.numeric(input$df1)
      df2 <- as.numeric(input$df2)
      ncp <- as.numeric(input$ncp)
      x <- 0:25
      y <- df(x, df1 = df1,df2 = df2, ncp = ncp, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Df1 =", df1, ", Df2 =", df2, " , Ncp = ", ncp ,")"))
    }else if(dist == "Cauchy"){
      l <- as.numeric(input$l)
      s <- as.numeric(input$s)
      x <- 0:25
      y <- dcauchy(x, location = l, scale = s, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Location =", l, ", Scale =", s,")"))
    }
    else if(dist == "Chy-Squared"){
      df <- as.numeric(input$df)
      ncp <- as.numeric(input$ncp)
      x <- 0:25
      y <- dchisq(x, df = df, ncp = ncp, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Df =", df," , Ncp = ", ncp ,")"))
    }
    else if(dist == "Laplace"){
      m <- as.numeric(input$mu)
      s <- as.numeric(input$sigma)
      x <- 0:25
      y <- dlaplace(x, mu = m, sigma = s, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Mu =", m," , Sigma = ", s ,")"))
    }else if(dist == "Pareto"){
      aP <- as.numeric(input$aP)
      bP <- as.numeric(input$bP)
      x <- 0:25
      y <- dpareto(x, a = aP, b = bP, log = TRUE)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(A =", aP," , B = ", bP ,")"))
    }
  })
  
  output$cdfPlot <- renderPlot({
    dist <- input$distribution
    a <- as.numeric(input$a)
    b <- as.numeric(input$b)
    if (dist == "Binomial") {
      size <- as.numeric(input$size)
      prob <- as.numeric(input$prob)
      x <- 0:size
      y <- pbinom(x, size = size, prob = prob)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(size =", size, ", prob =", prob, ")"))
    } else if (dist == "Normal") {
      mean <- as.numeric(input$mean)
      sd <- as.numeric(input$sd)
      x <- seq(mean-4*sd, mean+4*sd, length.out = 100)
      y <- pnorm(x, mean = mean, sd = sd)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(mean =", mean, ", sd =", sd, ")"))
    } else if (dist == "Poisson") {
      lambda <- as.numeric(input$lambda)
      x <- 0:30
      y <- ppois(x, lambda = lambda)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(lambda =", lambda, ")"))
    } else if (dist == "Gamma") {
      shape <- as.numeric(input$shape)
      scale <- as.numeric(input$scale)
      x <- seq(0, shape*scale*2, length.out = 100)
      y <- pgamma(x, shape = shape, scale = scale)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(shape =", shape, ", scale =", scale, ")"))
    }
    else if (dist == "Exponential") {
      rate <- as.numeric(input$rate)
      x <- seq(0, 2/rate, length.out = 100)
      y <- pexp(x, rate = rate)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(rate =", rate, ")"))
    }
    else if (dist == "Weibull") {
      shape <- as.numeric(input$shapeW)
      scale <- as.numeric(input$scaleW)
      x <- seq(0, shape*scale*2, length.out = 100)
      y <- pweibull(x, shape = shape, scale = scale)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(shape =", shape, ", scale =", scale, ")"))
    }else if(dist == "Hypergeometric"){
      m <- as.numeric(input$m)
      n <- as.numeric(input$n)
      k <- as.numeric(input$k)
      x <- 0:25
      y <- phyper(x, m = m, n = n, k = k)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(M =", m, ", N =", n, ", K =", k ,")"))
    }else if(dist == "Log-normal"){
      ml <- as.numeric(input$ml)
      sl <- as.numeric(input$sl)
      x <- 0:25
      y <- plnorm(x, meanlog = ml, sdlog = sl)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(MeanLog =", ml, ", SdLog =", sl, ")"))
    }else if(dist == "Negative binomial"){
      s <- as.numeric(input$s)
      p <- as.numeric(input$p)
      x <- 0:25
      y <- pnbinom(x, size = s, prob = p)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Size =", s, ", Prob =", p,")"))
    }else if(dist == "Beta"){
      s1 <- as.numeric(input$s1)
      s2 <- as.numeric(input$s2)
      ncp <- as.numeric(input$ncp)
      x <- 0:25
      y <- pbeta(x, shape1 = s1, shape2 = s2, ncp = ncp)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Shape1 =", s1, ", Shape2 =", s2, " , Ncp = ", ncp ,")"))
    }else if(dist == "Fisher"){
      df1 <- as.numeric(input$df1)
      df2 <- as.numeric(input$df2)
      ncp <- as.numeric(input$ncp)
      x <- 0:25
      y <- pf(x, df1 = df1,df2 = df2, ncp = ncp)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Df1 =", df1, ", Df2 =", df2, " , Ncp = ", ncp ,")"))
    }else if(dist == "Cauchy"){
      l <- as.numeric(input$l)
      s <- as.numeric(input$s)
      x <- 0:25
      y <- pcauchy(x, location = l, scale = s)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Location =", l, ", Scale =", s,")"))
    }else if(dist == "Chy-Squared"){
      df <- as.numeric(input$df)
      ncp <- as.numeric(input$ncp)
      x <- 0:25
      y <- pchisq(x, df = df, ncp = ncp)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Df =", df," , Ncp = ", ncp ,")"))
    }else if(dist == "Laplace"){
      m <- as.numeric(input$mu)
      s <- as.numeric(input$sigma)
      x <- 0:25
      y <- plaplace(x, mu = m, sigma = s)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(Mu =", m," , Sigma = ", s ,")"))
    }else if(dist == "Pareto"){
      aP <- as.numeric(input$aP)
      bP <- as.numeric(input$bP)
      x <- 0:25
      y <- ppareto(x, a = aP, b = bP)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_line() +
        geom_area(data = data.frame(x = x[x<=a], y = y[x<=a]), aes(x, y), fill = "red", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=b], y = y[x>=b]), aes(x, y), fill = "green", alpha = 0.2) +
        geom_area(data = data.frame(x = x[x>=a & x<=b], y = y[x>=a & x<=b]), aes(x, y), fill = "blue", alpha = 0.2) +
        ggtitle(paste(dist, "(A =", aP," , B = ", bP ,")"))
    }
  })
}

shinyApp(ui, server)
      