require(corrplot, papaja, apaTable)

rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)


rwthcolors <- list(blue = rgb2hex(0,84,159),
                   lightblue = rgb2hex(142,186,229),
                   black= rgb2hex(0,0,0),
                   magenta = rgb2hex(227,0,102),
                   yellow = rgb2hex(255,237,0),
                   petrol = rgb2hex(0,97,101),
                   turquois = rgb2hex(0,152,161),
                   green = rgb2hex(87,171,39),
                   maygreen = rgb2hex(189,205,0),
                   orange = rgb2hex(246,168,0),
                   red = rgb2hex(204,7,30),
                   bordeaux = rgb2hex(161,16,53),
                   violet = rgb2hex(97,33,88),
                   purple = rgb2hex(122,111,172)
)


cplot <- function(data) {
  p <- cor.mtest(data, conf.level = .95)
  col <- colorRampPalette(c(rwthcolors$red, "#FFFFFF", rwthcolors$blue))
  cor(data, use = "pairwise.complete.obs") %>% corrplot( method = "color", col = col(200),
                                                         type = "upper", order = "hclust", number.cex = .7,
                                                         addCoef.col = "black", # Add coefficient of correlation
                                                         tl.col = "black", tl.srt = 90, # Text label color and rotation
                                                         # Combine with significance
                                                         p.mat = p$p, sig.level = c(.001, .01, .05), insig = "n", 
                                                         # hide correlation coefficient on the principal diagonal
                                                         diag = TRUE, tl.pos = "lt")
  cor(data, use = "pairwise.complete.obs") %>% corrplot( method = "color", col = col(200),
                                                         type = "lower", order = "hclust", number.cex = .7,
                                                         #addCoef.col = "black", # Add coefficient of correlation
                                                         #tl.col = "black", tl.srt = 90, # Text label color and rotation
                                                         # Combine with significance
                                                         p.mat = p$p, sig.level = c(.001, .01, .05), insig = "label_sig", 
                                                         pch.cex = 0.8,
                                                         # hide correlation coefficient on the principal diagonal
                                                         diag = TRUE, add=TRUE, tl.pos = "n")
  
}


my_factors <- function(data, vars) {
  jmv::pca(data, vars, 
           nFactorMethod = "eigen",
           rotation = "varimax",
           eigen = T, 
           kmo = TRUE, 
           bartlett = TRUE, 
           factorSummary = TRUE)
}



a_table <- function(table_data){
  apa_table(table_data$table.body, caption = table_data$table.title, note=table_data$table.note)
}
