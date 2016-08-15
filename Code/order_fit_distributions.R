
fit_distributions <- function(year, out_lab = "", bootstrap = NULL, degree_distribution_all = NULL, var = "count") {
    
    # Libraries
    library(dplyr)
    library(poweRlaw)
    
    ## INITIALISE DATA ##########################
        # Load data
    if (is.null(degree_distribution_all)) {
        load("Dat/degree_distribution.RData")
    }
    # Filter year
    ddyear <- degree_distribution_all %>% filter(Year == year) %>% group_by() %>% select(Order, get(var))
    # Produce tall vector (as poweRlaw interacts with)
    vec <- NULL
    for(row in seq_len(nrow(ddyear))) {
        vec <- c(vec, rep(as.numeric(ddyear[row,"Order"]), ddyear[row, var]))
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
    distFit.plot <- function(this) {
        par(mfrow = c(2,2))
        par(cex = .5)
        par(lwd = 2)
        plot(m_pl, main = "power-law"); lines(this$distributions$pl, col="green") 
        plot(m_ln, main = "log-normal"); lines(this$distributions$ln, col = "red") 
        plot(m_exp, main = "exponential"); lines(this$distributions$exp, col="blue") 
        plot(m_pois, main = "poisson"); lines(this$distributions$pois, col="orange")  
    }
    

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
    # So create new estimate with equal xmins (equal to the largest of the previous xmins)
    m_ln2 = m_ln
    m_pl2 = m_pl
    
    x1 <- m_pl$getXmin()
    x2 <- m_ln$getXmin()
    xgreater <- ifelse(x1 > x2, x1, x2)
    
    if (x1 > x2) {
        m_ln2$setXmin(xgreater)
        est_ln2 = estimate_pars(m_ln2)
        m_ln2$setPars(est_ln2)
        est_pl2 = est_pl
    } else {
        m_pl2$setXmin(xgreater)
        est_pl2 = estimate_pars(m_pl2)
        m_pl2$setPars(est_pl2)
        est_ln2 = est_ln
    }
    
    # Replot
    # Plot distributions
    distFit_equalXmin.plot <- function(this) {
        par(mfrow = c(2,1))
        par(cex = .5)
        par(lwd = 2)
        plot(m_pl, main = "power-law"); lines(this$distributions$pl_equalXmin, col="green") 
        plot(m_ln, main = "log-normal"); lines(this$distributions$ln_equalXmin, col = "red") 
    }
    comp_plln  <- compare_distributions(m_pl2, m_ln2)
    comp_lnpl  <- compare_distributions(m_ln2, m_pl2)

    # Save results to file
    ret <- list(distributions = list(exp = m_exp, ln = m_ln, pl = m_pl, pois = m_pois, ln_equalXmin = m_ln2, pl_equalXmin = m_pl2), 
                par_estimates = list(exp = est_exp, ln = est_ln, pl = est_pl, pois = est_pois, ln_equalXmin = est_ln2, pl_equalXmin = est_pl2),
                boostraps = list(bs_pl, bs_ln),
                comparisons = list(powerlaw = comp_plln, lognormal = comp_lnpl),
                plots = list(distFit = distFit.plot, distFit_Xmin = distFit_equalXmin.plot))

    saveRDS(ret, file = paste0("Dat/order_fit_distributions", out_lab, year, ".rds"))
    
    return(ret)
}

# ¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬
# LOOP THROUGH EACH YEAR ---------------------------------------------------------------------------------------
# ¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬
library(poweRlaw)
datNew <- readRDS("Dat/orderFrequencies.rds")
years <- 1976:2015
pl <- list()
pl_Other <- list()
pl_Examiner <- list()
for(i in seq_along(years)) {
    yr <- years[i]
    print(yr)
    pl[[i]] <- fit_distributions(yr)
    if (yr %in% 2001:2015) {
        pl_Other[[i]] <- fit_distributions(yr, degree_distribution = datNew, var = "Other")
        pl_Examiner[[i]] <- fit_distributions(yr, degree_distribution = datNew, var = "Examiner")
    } else {
        pl_Other[[i]] <- NULL
        pl_Examiner[[i]] <- NULL
    }
}

save(pl, pl_Other, pl_Examiner, file = "powerLawFits.rdata")