
fit_distributions <- function(year, bootstrap = NULL) {
    
    # Libraries
    library(dplyr)
    library(poweRlaw)
    
    ## INITIALISE DATA ##########################
    # Load data
    load("degree_distribution.RData")
    # Filter year
    ddyear <- degree_distribution_all %>% filter(Year == year) %>% group_by() %>% select(Order, count)
    # Produce tall vector (as poweRlaw interacts with)
    vec <- NULL
    for(row in seq_len(nrow(ddyear))) {
        vec <- c(vec, rep(as.numeric(ddyear[row,"Order"]), ddyear[row,"count"]))
    }
    # Remove zeros because log scale can't handle it. 
    vec <- vec[vec != 0] 
    
    ## Fit distributions #######################
    
    # Power law
    m_pl <- displ$new(vec)
    est = estimate_pars(m_pl)
    est_pl <- estimate_xmin(m_pl)
    m_pl$setXmin(est_pl)

    # Log normal
    m_ln <- dislnorm$new(vec)
    est = estimate_pars(m_ln)
    est_ln <- estimate_xmin(m_ln)
    m_ln$setXmin(est_ln)

    # Exponential
    m_exp <- disexp$new(vec)
    est = estimate_pars(m_exp)
    est_exp <- estimate_xmin(m_exp)
    m_exp$setXmin(est_exp)

    # Poisson
    m_pois <- dispois$new(vec)
    est = estimate_pars(m_pois)
    est_pois <- estimate_xmin(m_pois)
    m_pois$setXmin(est_pois)

    # Plot distributions
    par(mfrow = c(2,2))
    par(cex = .5)
    par(lwd = 2)
    plot(m_pl, main = "power-law"); lines(m_pl, col="green") 
    plot(m_ln, main = "log-normal"); lines(m_ln, col = "red") 
    plot(m_exp, main = "exponential"); lines(m_exp, col="blue") 
    plot(m_pois, main = "poisson"); lines(m_pois, col="orange") 
    distFit.plot = recordPlot()
    
    ## Finding p values via bootstrapping #######################
    if ("pl" %in% bootstrap) {
        bs_pl = bootstrap_p(m_pl, no_of_sims=2500, threads=8)
        save(bs_pl, file = paste0("bs_pl", year, ".RData"))
        bs_pl$p
    } else bs_pl = NULL
    
    if ("ln" %in% bootstrap) {
        bs_ln = bootstrap_p(m_ln, no_of_sims=2500, threads=8)
        save(bs_ln, file = paste0("bs_ln", year, ".RData"))
             bs_ln$p
    } else bs_ln = NULL
    
    # The xmin must be the same for both distributions so set it to the larger of the two
    x1 <- m_pl$getXmin()
    x2 <- m_ln$getXmin()
    xgreater <- ifelse(x1 > x2, x1, x2)

    if (x1 > x2) {
        m_ln$setXmin(xgreater)
        est_ln = estimate_pars(m_ln)
        m_ln$setPars(est_ln)
    } else {
        m_pl$setXmin(xgreater)
        est_pl = estimate_pars(m_pl)
        m_pl$setPars(est_pl)
    }
    
    # Replot
    # Plot distributions
    par(mfrow = c(2,2))
    par(cex = .5)
    par(lwd = 2)
    plot(m_pl, main = "power-law"); lines(m_pl, col="green") 
    plot(m_ln, main = "log-normal"); lines(m_ln, col = "red") 
    plot(m_exp, main = "exponential"); lines(m_exp, col="blue") 
    plot(m_pois, main = "poisson"); lines(m_pois, col="orange") 
    distFit2.plot = recordPlot()
    
    comp_plln  <- compare_distributions(m_pl, m_ln)
    comp_lnpl  <- compare_distributions(m_ln, m_pl)

    par(mfrow = c(1,1))
    plot(comp_plln)
    loglik.plot = recordPlot()
    
    # Save results to file
    ret <- list(distributions = list(m_exp, m_ln, m_pl, m_pois), 
                par_estimates = list(est_exp, est_ln, est_pl, est_pois),
                boostraps = list(bs_pl, bs_ln),
                comparisons = list(powerlaw = comp_plln, lognormal = comp_lnpl),
                plots = list(distFit.plot, loglik.plot, distFit2.plot))

    saveRDS(ret, file = paste0("order_fit_distributions", year, ".rds"))
    
    return(ret)
}