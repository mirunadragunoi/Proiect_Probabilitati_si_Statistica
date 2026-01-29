# PROIECT EXERCITIUL 2 APLICATIE SHINY PT FUNCTII DE REPARTITIE
library(shiny)
library(ggplot2)

# interfata
ui <- fluidPage(
  titlePanel("Exercitiul 2: Functii de Repartitie pentru Variabile aleatoare"),
  
  sidebarLayout(
    sidebarPanel(
      # selectare distributie
      selectInput("distribution", "Selecteaza distributia:",
                  choices = list(
                    "1. Normal Standard N(0,1)" = "normal_standard",
                    "2. Normal N(μ,σ²)" = "normal_general",
                    "3. Exponentiala Exp(λ)" = "exponential",
                    "4. Poisson Pois(λ)" = "poisson",
                    "5. Binomiala Binom(r,p)" = "binomial"
                  )),
      
      # parametrii comuni
      numericInput("n", "n (numar variabile):", value = 10, min = 1, max = 100, step = 1),
      numericInput("n_samples", "Numar simulari:", value = 1000, min = 100, max = 10000, step = 100),
      
      # parametrii specifici (conditionati)
      conditionalPanel(
        condition = "input.distribution == 'normal_general'",
        numericInput("mu", "μ (media):", value = 0, step = 0.1),
        numericInput("sigma", "σ (deviere standard):", value = 1, min = 0.1, step = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'exponential'",
        numericInput("lambda_exp", "λ (rată):", value = 1, min = 0.1, step = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'poisson'",
        numericInput("lambda_pois", "λ (rată):", value = 3, min = 0.1, step = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'binomial'",
        numericInput("r", "r (numar incercari):", value = 10, min = 1, step = 1),
        numericInput("p", "p (probabilitate succes):", value = 0.5, min = 0.01, max = 0.99, step = 0.01)
      ),
      
      hr(),
      actionButton("generate", "Genereaza Grafice", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Grafice CDF",
                 plotOutput("cdf_plots", height = "800px")
        ),
        tabPanel("Informatii",
                 h3("Variabilele afisate:"),
                 verbatimTextOutput("info")
        )
      )
    )
  )
)

# logica serverului
server <- function(input, output) {
  
  # generare de date reactive
  data <- eventReactive(input$generate, {
    n <- input$n
    n_samples <- input$n_samples
    
    # generare esantioane
    if (input$distribution == "normal_standard") {
      samples <- matrix(rnorm(n * n_samples, 0, 1), nrow = n_samples, ncol = n)
    } else if (input$distribution == "normal_general") {
      samples <- matrix(rnorm(n * n_samples, input$mu, input$sigma), nrow = n_samples, ncol = n)
    } else if (input$distribution == "exponential") {
      samples <- matrix(rexp(n * n_samples, input$lambda_exp), nrow = n_samples, ncol = n)
    } else if (input$distribution == "poisson") {
      samples <- matrix(rpois(n * n_samples, input$lambda_pois), nrow = n_samples, ncol = n)
    } else if (input$distribution == "binomial") {
      samples <- matrix(rbinom(n * n_samples, input$r, input$p), nrow = n_samples, ncol = n)
    }
    
    # calcul transformari
    X <- samples[, 1]  # prima variabila
    
    if (input$distribution == "normal_standard" || input$distribution == "normal_general") {
      transform1 <- 3 + 2*X
      transform2 <- X^2
      sum_X <- rowSums(samples)
      sum_X2 <- rowSums(samples^2)
      
      list(
        X = X,
        transform1 = transform1,
        transform2 = transform2,
        sum_X = sum_X,
        sum_X2 = sum_X2,
        names = c("X", "3+2X", "X²", "Σ Xᵢ", "Σ Xᵢ²")
      )
    } else if (input$distribution == "exponential") {
      transform1 <- 2 - 5*X
      transform2 <- X^2
      sum_X <- rowSums(samples)
      
      list(
        X = X,
        transform1 = transform1,
        transform2 = transform2,
        sum_X = sum_X,
        names = c("X", "2-5X", "X²", "Σ Xᵢ")
      )
    } else if (input$distribution == "poisson") {
      transform1 <- 3*X + 2
      transform2 <- X^2
      sum_X <- rowSums(samples)
      
      list(
        X = X,
        transform1 = transform1,
        transform2 = transform2,
        sum_X = sum_X,
        names = c("X", "3X+2", "X²", "Σ Xᵢ")
      )
    } else if (input$distribution == "binomial") {
      transform1 <- 5*X + 4
      transform2 <- X^3
      sum_X <- rowSums(samples)
      
      list(
        X = X,
        transform1 = transform1,
        transform2 = transform2,
        sum_X = sum_X,
        names = c("X", "5X+4", "X³", "Σ Xᵢ")
      )
    }
  })
  
  # graficele
  output$cdf_plots <- renderPlot({
    req(data())
    d <- data()
    
    # numar de variabile
    n_vars <- length(d$names)
    
    # lista efectiva de grafice
    plots <- list()
    
    for (i in 1:n_vars) {
      var_data <- switch(i,
                         d$X,
                         d$transform1,
                         d$transform2,
                         d$sum_X,
                         if(!is.null(d$sum_X2)) d$sum_X2 else NULL)
      
      if (!is.null(var_data)) {
        # creez dataframe pt ggplot
        df <- data.frame(value = var_data)
        
        # plot ECDF
        p <- ggplot(df, aes(x = value)) +
          stat_ecdf(geom = "step", color = "blue", size = 1) +
          labs(title = paste("CDF pentru", d$names[i]),
               x = d$names[i],
               y = "F(x)") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10))
        
        plots[[i]] <- p
      }
    }
    
    # aranjare grafice
    if (n_vars == 4) {
      gridExtra::grid.arrange(grobs = plots, ncol = 2)
    } else {
      gridExtra::grid.arrange(grobs = plots, ncol = 2)
    }
  })
  
  # info despre variabile
  output$info <- renderText({
    req(data())
    d <- data()
    
    dist_name <- switch(input$distribution,
                        "normal_standard" = "N(0,1)",
                        "normal_general" = sprintf("N(%.2f, %.2f²)", input$mu, input$sigma),
                        "exponential" = sprintf("Exp(%.2f)", input$lambda_exp),
                        "poisson" = sprintf("Pois(%.2f)", input$lambda_pois),
                        "binomial" = sprintf("Binom(%d, %.2f)", input$r, input$p))
    
    paste0(
      "Distributie: ", dist_name, "\n",
      "Numar variabile (n): ", input$n, "\n",
      "Numar simulari: ", input$n_samples, "\n\n",
      "Variabile afisate:\n",
      paste(d$names, collapse = "\n")
    )
  })
}

# rulare aplicatie efectiv
shinyApp(ui = ui, server = server)