library(scales)

# Label axes in proper notation
fancy_scientific <- function(label, simplify = FALSE) {
    label <- format(label, scientific = TRUE)
    label <- gsub("^(.*)e", "'\\1'e", label)
    label <- gsub("e\\+","e",label)
    label <- gsub("e", "%*%10^", label)
    if (simplify == TRUE) label <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", label)
    parse(text = label)
}

# General plot theme
theme_pub <- function(base_size = 12, base_family = "arial") {
    #extrafont::font_import(pattern = "arial", prompt = FALSE)
    theme_grey(base_size = base_size) %+replace%  # , base_family = base_family
        
        theme(

            # Set text size
            plot.title = element_text(size = 18),
            axis.title.x = element_text(size = 16,
                                        margin = margin(t = 10)),
            axis.title.y = element_text(size = 16, 
                                        angle = 90, 
                                        margin = margin(r = 10)),
            
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            
            strip.text.x = element_text(size = 15),
            strip.text.y = element_text(size = 15,
                                        angle = -90),
            
            # Legend text
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 15),
            
            # Configure lines and axes
            axis.ticks.x = element_line(colour = "black"), 
            axis.ticks.y = element_line(colour = "black"), 
            
            # Plot background
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "grey83", 
                                            size = 0.2), 
            panel.grid.minor = element_line(colour = "grey88", 
                                            size = 0.5), 
            
            # Facet labels        
            legend.key = element_rect(colour = "grey80"), 
            strip.background = element_rect(fill = "grey80", 
                                            colour = "grey50", 
                                            size = 0.2),
            legend.position = c(0.8, 0.8),
            legend.title = element_blank()
        )
}

# Colour schemes
entries <- c("1984", "1992", "2002", "2012",
                         "2006", "2010", "2014",
                         "Examiner", "Other", "Total")
names(entries) <- entries
Scheme <- data.frame(colour = rep(NA, length(entries)), 
                     gray = rep(NA, length(entries)),
                     shape = rep(NA, length(entries)),
                     linetype =rep(NA, length(entries)),
                     row.names = entries
                     )

# Years
years1 <- entries[c("1984","1992", "2002", "2012")]
Scheme[years1, "gray"] <- gray.colors(n = 4, 0.3, 0.7)
Scheme[years1, "colour"] <- brewer_pal(palette = "Set2")(4)

years2 <- entries[c("2006", "2010", "2014")]
Scheme[years2, "gray"] <- gray.colors(n = 3, 0.2 ,0.7)
Scheme[years2, "colour"] <- brewer_pal(palette = "Dark2")(3)
Scheme[years2, "linetype"] <- linetype_pal()(3)
    
# Citedy By
citedBy <- entries[c("Examiner", "Other", "Total")]
Scheme[citedBy, "gray"] <- gray.colors(n = 3, 0.2 ,0.7)
Scheme[citedBy, "colour"] <- brewer_pal(palette = "Set1")(3)
Scheme[citedBy, "shape"] <- shape_pal()(3)
Scheme[citedBy, "linetype"] <- linetype_pal()(3)

 

