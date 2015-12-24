library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(maptools)


### Show all the colour schemes available
display.brewer.all()



plot_C_mi <- function(spdf_Cmi, plot_variable, n_breaks = 5, col_pal = "Set3", style_intervals = "jenks"){
    require(sp, rgeos, maptools, RColorBrewer)
    col_palette <- brewer.pal(n_breaks, col_pal)
    breaks <- classIntervals(spdf_Cmi@data[,plot_variable], n = n_breaks, style = style_intervals)
    breaks <- breaks$brks
    plot(spdf_Cmi,
         col=col_palette[findInterval(spdf_Cmi@data[,plot_variable],
                                  breaks, 
                                  all.inside = TRUE)], axes=F, asp=T, lwd = 0.05, border = "cornsilk3")
    legend(x=679902.9, y=5728119, legend=leglabs(breaks), 
           fill=col_palette, bty="n", cex = .4)
}

plot_C_mi(london_pol40, plot_variable = "C_mi", n = 8, col_pal = "YlGnBu", style_intervals = "quantile")
