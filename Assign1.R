data <- data.frame(murder = USArrests$Murder, state = tolower(rownames(USArrests)))
map <- map_data("state")
seals$z <- with(seals, sqrt(delta_long^2 + delta_lat^2))

plots <- list(
    ggplot(ToothGrowth, aes(x = len)) + 
      geom_area(stat = "bin", binwidth = 1) +
      ggtitle("geom_area()"),
    ggplot(ToothGrowth, aes(x = len)) + 
      geom_density(kernel = "gaussian") +
      ggtitle("geom_density()"),
    ggplot(ToothGrowth, aes(x = len)) + 
      geom_dotplot(binwidth = 1) +
      ggtitle("geom_dotplot()"),
    ggplot(ToothGrowth, aes(x = len)) + 
      geom_freqpoly(binwidth = 1) +
      ggtitle("geom_freqpoly()"),
    ggplot(ToothGrowth, aes(x = len)) + 
      geom_histogram(binwidth = 5) +
      ggtitle("geom_histogram()"),
    ggplot(mpg, aes(x = displ)) + 
      geom_bar() +
      ggtitle("geom_bar()"),
    ggplot(ToothGrowth, aes(x = len, y = dose)) +
      geom_blank() +
      ggtitle("geom_blank()"),
    ggplot(ToothGrowth, aes(x = len, y = dose)) +
      geom_jitter() +
      ggtitle("geom_jitter()"),
    ggplot(ToothGrowth, aes(x = len, y = dose)) +
      geom_point() +
      ggtitle("geom_point()"),
    ggplot(ToothGrowth, aes(x = len, y = dose)) +
      geom_quantile() +
      ggtitle("geom_quantile()"),
    ggplot(ToothGrowth, aes(x = len, y = dose)) +
      geom_rug(sides = "bl") +
      ggtitle("geom_rug()"),
    ggplot(ToothGrowth, aes(x = len, y = dose)) +
      geom_smooth(method = "auto", formula = y ~ x) + 
      ggtitle("geom_smooth()"),
    ggplot(ToothGrowth, aes(x = len, y = dose)) +
      geom_text(aes(label = len)) +
      ggtitle("geom_text()"),
    ggplot(mpg, aes(class, y = cty)) +
      geom_bar(stat = "identity") + 
      ggtitle("geom_bar()"),
    ggplot(mpg, aes(class, y = cty)) +
      geom_boxplot() +
      ggtitle("geom_boxplot()"),
    ggplot(mpg, aes(class, y = cty)) +
      geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 1) +
      ggtitle("geom_dotplot()"),
    ggplot(mpg, aes(class, y = cty)) +
      geom_violin(scale = "area") +
      ggtitle("geom_violin()"),
    ggplot(diamonds, aes(cut, color)) +
      geom_jitter() +
      ggtitle("geom_jitter()"),
    ggplot(movies, aes(year, rating)) +
      geom_bin2d(binwidth = c(5, 0.5)) +
      ggtitle("geom_bin2d()"),
    ggplot(movies, aes(year, rating)) +
      geom_density2d() +
      ggtitle("geom_density2d()"),
    ggplot(movies, aes(year, rating)) +
      geom_hex() +
      ggtitle("geom_hex()"),
    ggplot(economics, aes(date, unemploy)) +
      geom_area() +
      ggtitle("geom_area()"),
    ggplot(economics, aes(date, unemploy)) +
      geom_line() +
      ggtitle("geom_line()"),
    ggplot(economics, aes(date, unemploy)) +
      geom_step(direction = "hv") +
      ggtitle("geom_step()"),
    ggplot(data.frame(grp=c("A", "B"), fit=4:5, se=1:2), aes(grp, fit, ymin = fit - se, ymax = fit + se)) +
      geom_crossbar(fatten = 2) +
      ggtitle("geom_crossbar()"),
    ggplot(data.frame(grp=c("A", "B"), fit=4:5, se=1:2), aes(grp, fit, ymin = fit - se, ymax = fit + se)) +
      geom_errorbar() +
      ggtitle("geom_errorbar()"),
    ggplot(data.frame(grp=c("A", "B"), fit=4:5, se=1:2), aes(grp, fit, ymin = fit - se, ymax = fit + se)) +
      geom_linerange() +
      ggtitle("geom_linerange()"),
    ggplot(data.frame(grp=c("A", "B"), fit=4:5, se=1:2), aes(grp, fit, ymin = fit - se, ymax = fit + se)) +
      geom_pointrange() +
      ggtitle("geom_pointrange()"),
    ggplot(data, aes(fill = murder)) +
      geom_map(aes(map_id = state), map = map) +
      expand_limits(x = map$long, y = map$lat) +
      ggtitle("geom_map()"),
    ggplot(seals, aes(long, lat)) +
      geom_contour(aes(z=z)) +
      ggtitle("geom_contour()"),
    ggplot(seals, aes(long, lat)) +
      geom_raster(aes(fill=z), hjust = 0.5,vjust = 0.5, interpolate = FALSE) +
      ggtitle("geom_raster()"),
    ggplot(seals, aes(long, lat)) +
      geom_tile(aes(fill=z)) +
      ggtitle("geom_tile()"),
    ggplot(map, aes(long, lat)) +
      geom_polygon(aes(group=group)) +
      ggtitle("geom_polygon()"),
    ggplot(economics, aes(date, unemploy)) +
      geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
      ggtitle("geom_path()"),
    ggplot(economics, aes(date, unemploy)) +
      geom_ribbon(aes(ymin=unemploy-900, ymax=unemploy+900)) +
      ggtitle("geom_ribbon()"),
    ggplot(seals, aes(x=long, y=lat)) +
      geom_segment(aes(xend=long + delta_long, yend=lat + delta_lat)) +
      ggtitle("geom_segment()"),
    ggplot(seals, aes(x=long, y=lat)) +
      geom_rect(aes(xmin=long, ymin=lat, xmax=long + delta_long, ymax=lat + delta_lat)) +
      ggtitle("geom_rect()"),
    ggplot(mpg, aes(fl)) +
      geom_bar() +
      coord_cartesian(xlim=c(0,5)) +
      ggtitle("coord_cartesian()"),
    ggplot(mpg, aes(fl)) +
      geom_bar() +
      coord_fixed(ratio = 1/2) +
      ggtitle("coord_fixed()"),
    ggplot(mpg, aes(fl)) +
      geom_bar() +
      coord_flip() +
      ggtitle("coord_flip()"),
    ggplot(mpg, aes(fl)) +
      geom_bar() +
      coord_polar(theta="x", direction=1) +
      ggtitle("coord_polar()"),
    ggplot(mpg, aes(fl)) +
      geom_bar() +
      coord_trans(y="sqrt") +
      ggtitle("coord_trans()"),
    ggplot(mpg, aes(cty, hwy)) + geom_point() +
      facet_grid(. ~ fl) +
      ggtitle("facet_grid() - fl"),
    ggplot(mpg, aes(cty, hwy)) + geom_point() +
      facet_grid(year ~ .) +
      ggtitle("facet_grid() - year"),
    ggplot(mpg, aes(cty, hwy)) + geom_point() +
      facet_grid(year ~ fl) +
      ggtitle("facet_grid() - year&fl"),
    ggplot(mpg, aes(cty, hwy)) + geom_point() +
      facet_wrap(~ fl) +
      ggtitle("facet_wrap()"),
    ggplot(mpg, aes(fl,fill = drv)) +
      geom_bar(position = "dodge") +
      ggtitle("geom_bar - side by side"),
    ggplot(mpg, aes(fl,fill = drv)) +
      geom_bar(position = "fill") +
      ggtitle("geom_bar - fill"),
    ggplot(mpg, aes(fl,fill = drv)) +
      geom_bar(position = "stack") +
      ggtitle("geom_bar - stack"),
    ggplot(mpg, aes(cty, hwy)) +
      geom_point(position = "jitter") +
      ggtitle("geom_point - jitter"),
    ggplot(mpg, aes(cty, hwy)) + geom_point() +
      labs(title = "Title", x = "X", y = "Y"),
    ggplot(mpg, aes(cty, hwy)) + geom_point() +
      theme(legend.position = "bottom") +
      guides(color="none") +
      scale_fill_discrete(name="Title", labels=c("A","B","C")),
    ggplot(mpg, aes(cty, hwy)) + geom_point() +
      coord_cartesian(xlim = c(0, 100), ylim = c(10, 20)) +
      ggtitle("without clipping"),
    ggplot(mpg, aes(cty, hwy)) + geom_point() +
      xlim(0, 100) + ylim(10, 20) +
      ggtitle("with clipping"),
    ggplot(mpg, aes(fl)) +
      geom_bar() +
      theme_bw(),
    ggplot(mpg, aes(fl)) +
      geom_bar() +
      theme_classic(),
    ggplot(mpg, aes(fl)) +
      geom_bar() +
      theme_grey(),
    ggplot(mpg, aes(fl)) +
      geom_bar() +
      theme_minimal(),
    ggplot(mpg, aes(hwy)) +
      stat_bin(binwidth = 1, origin= 10) +
      ggtitle("stat_bin()"),
    ggplot(mpg, aes(hwy)) +
      stat_density(adjust = 1, kernel = "gaussian") +
      ggtitle("stat_density()"),
    ggplot(mpg, aes(cty,hwy)) +
      stat_bin2d(bins=30, drop=TRUE) +
      ggtitle("stat_bin2d()"),
    ggplot(mpg, aes(cty,hwy)) +
      stat_binhex(bins = 30) +
      ggtitle("stat_binhex()"),
    ggplot(mpg, aes(cty,hwy)) +
      stat_density2d(contour = TRUE, n=100) +
      ggtitle("stat_density2d()"),
    ggplot(seals, aes(long,lat)) +
      stat_contour(aes(z=z)) +
      ggtitle("stat_contour()"),
    ggplot(seals, aes(long,lat)) +
      stat_spoke(aes(radius=z, angle=z)) +
      ggtitle("stat_spoke()"),
    ggplot(seals, aes(long,lat)) +
      stat_summary_hex(aes(z=z), bins = 30, fun = mean) +
      ggtitle("stat_summary_hex()"),
    ggplot(seals, aes(long,lat)) +
      stat_summary2d(aes(z=z), bins=30, fun=mean) +
      ggtitle("stat_summary2d()"),
    ggplot(mpg, aes(class,hwy)) +
      stat_boxplot(coef = 1.5) +
      ggtitle("stat_boxplot()"),
    ggplot(mpg, aes(class,hwy)) +
      stat_ydensity(adjust=1, kernel="gaussian", scale = "area") +
      ggtitle("stat_ydensity()"),
    ggplot(mpg, aes(cty,hwy)) +
      stat_ecdf(n=40) +
      ggtitle("stat_ecdf()"),
    ggplot(mpg, aes(cty,hwy)) +
      stat_quantile(quantiles = c(0.25,0.5,0.75), formula = y~log(x),method = "rq") +
      ggtitle("stat_quantile()"),
    ggplot(mpg, aes(cty,hwy)) +
      stat_smooth(method = "auto", formula = y~x, se=TRUE, n=80, fullrange = FALSE, level=0.95) +
      ggtitle("stat_smooth()"),
    ggplot() + stat_function(aes(x=-3.3), fun = dnorm, n=101,args=list(sd=0.5)) +
      ggtitle("stat_function()"),
    ggplot(mpg, aes(cty,hwy)) +
      stat_identity() +
      ggtitle("stat_identity()"),
    ggplot() + stat_qq(aes(sample = 1:100), distribution = qt, dparams = list(df=5)) +
      ggtitle("stat_qq()"),
    ggplot(mpg, aes(cty,hwy)) +
      stat_sum() +
      ggtitle("stat_sum()"),
    ggplot(mpg, aes(cty,hwy)) +
      stat_summary() +
      ggtitle("stat_summary()"),
    ggplot(mpg, aes(cty,hwy)) +
      stat_unique() +
      ggtitle("stat_unique()"),
    ggplot(mpg, aes(fl)) + geom_bar(aes(fill=fl)) +
      scale_fill_manual(values = c("skyblue", "royalblue", "blue", "navy"), limits = c("d", "e", "p", "r"), breaks =c("d", "e", "p", "r"),name = "fuel", labels = c("D", "E", "P", "R")) +
      ggtitle("scale_fill_manual()"),
    ggplot(mpg, aes(fl)) + geom_bar(aes(fill=fl)) +
      scale_fill_brewer(palette = "Blues") +
      ggtitle("scale_fill_brewer()"),
    ggplot(mpg, aes(fl)) + geom_bar(aes(fill=fl)) +
      scale_fill_grey(start=0.2,end=0.8,na.value = "red") +
      ggtitle("scale_fill_grey()"),
    ggplot(mpg, aes(hwy)) + geom_dotplot(aes(fill=..x..)) +
      scale_fill_gradient(low = "red", high = "yellow") +
      ggtitle("scale_fill_gradient()"),
    ggplot(mpg, aes(hwy)) + geom_dotplot(aes(fill=..x..)) +
      scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 25) +
      ggtitle("scale_fill_gradient()2"),
    ggplot(mpg, aes(hwy)) + geom_dotplot(aes(fill=..x..)) +
      scale_fill_gradientn(colours = terrain.colors(6)) +
      ggtitle("scale_fill_gradientn()"),
    ggplot(mpg, aes(cty, hwy)) + geom_point(aes(shape=fl)) +
      scale_shape(solid=FALSE) + 
      ggtitle("scale_shape()"),
    ggplot(mpg, aes(cty, hwy)) + geom_point(aes(shape=fl)) +
      scale_shape_manual(values = c(3:7)) + 
      ggtitle("scale_shape_manual()"),
    ggplot(mpg, aes(cty, hwy)) + geom_point(aes(size=cyl)) +
      scale_size_area(max_size = 6) +
      ggtitle("scale_size_area()")
)

ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "One Variable",
      fluidRow(
        column(6,plotOutput('plot1')),
        column(6,plotOutput('plot2'))
      ),
      fluidRow(
        column(6,plotOutput('plot3')),
        column(6,plotOutput('plot4'))
      ),
      fluidRow(
        column(6,plotOutput('plot5')),
        column(6,plotOutput('plot6'))
      )
    ),
    tabPanel("Two Variable",
      fluidRow(
        column(6,plotOutput('plot8')),
        column(6,plotOutput('plot9'))
      ),
      fluidRow(
        column(6,plotOutput('plot10')),
        column(6,plotOutput('plot11'))
      ),
      fluidRow(
        column(6,plotOutput('plot12')),
        column(6,plotOutput('plot13'))
      ),
      fluidRow(
        column(6,plotOutput('plot14')),
        column(6,plotOutput('plot15'))
      ),
      fluidRow(
        column(6,plotOutput('plot16')),
        column(6,plotOutput('plot17'))
      ),
      fluidRow(
        column(6,plotOutput('plot18')),
        column(6,plotOutput('plot19'))
      ),
      fluidRow(
        column(6,plotOutput('plot20')),
        column(6,plotOutput('plot21'))
      ),
      fluidRow(
        column(6,plotOutput('plot22')),
        column(6,plotOutput('plot23'))
      ),
      fluidRow(
        column(6,plotOutput('plot24')),
        column(6,plotOutput('plot25'))
      ),
      fluidRow(
        column(6,plotOutput('plot26')),
        column(6,plotOutput('plot27'))
      ),
      fluidRow(
        column(6,plotOutput('plot28')),
        column(6,plotOutput('plot29'))
      )
    ),
    tabPanel("Three Variable",
      fluidRow(
        column(6,plotOutput('plot30')),
        column(6,plotOutput('plot31'))
      ),
      fluidRow(
        column(6,plotOutput('plot32'))
      )
    ),
    tabPanel("Graphical Primitives",
      fluidRow(
        column(6,plotOutput('plot33')),
        column(6,plotOutput('plot34'))
      ),
      fluidRow(
        column(6,plotOutput('plot35')),
        column(6,plotOutput('plot36'))
      ),
      fluidRow(
        column(6,plotOutput('plot37'))
      )
    ),
    tabPanel("Coordinate Systems",
      fluidRow(
        column(6,plotOutput('plot38')),
        column(6,plotOutput('plot39'))
      ),
      fluidRow(
        column(6,plotOutput('plot40')),
        column(6,plotOutput('plot41'))
      ),
      fluidRow(
        column(6,plotOutput('plot42'))
      )
    ),
    tabPanel("Faceting",
      fluidRow(
        column(6,plotOutput('plot43')),
        column(6,plotOutput('plot44'))
      ),
      fluidRow(
        column(6,plotOutput('plot45')),
        column(6,plotOutput('plot46'))
      )
    ),
    tabPanel("Position Adjustments",
      fluidRow(
        column(6,plotOutput('plot47')),
        column(6,plotOutput('plot48'))
      ),
      fluidRow(
        column(6,plotOutput('plot49')),
        column(6,plotOutput('plot50'))
      )
    ),
    tabPanel("Labels & Legends",
      fluidRow(
        column(6,plotOutput('plot51')),
        column(6,plotOutput('plot52'))
      )
    ),
    tabPanel("Zooming",
      fluidRow(
        column(6,plotOutput('plot53')),
        column(6,plotOutput('plot54'))
      )
    ),
    tabPanel("Themes",
      fluidRow(
        column(6,plotOutput('plot55')),
        column(6,plotOutput('plot56'))
      ),
      fluidRow(
        column(6,plotOutput('plot57')),
        column(6,plotOutput('plot58'))
      )
    ),
    tabPanel("Stats",
      fluidRow(
        column(6,plotOutput('plot59')),
        column(6,plotOutput('plot60'))
      ),
      fluidRow(
        column(6,plotOutput('plot61')),
        column(6,plotOutput('plot62'))
      ),
      fluidRow(
        column(6,plotOutput('plot63')),
        column(6,plotOutput('plot64'))
      ),
      fluidRow(
        column(6,plotOutput('plot65')),
        column(6,plotOutput('plot66'))
      ),
      fluidRow(
        column(6,plotOutput('plot67')),
        column(6,plotOutput('plot68'))
      ),
      fluidRow(
        column(6,plotOutput('plot69')),
        column(6,plotOutput('plot70'))
      ),
      fluidRow(
        column(6,plotOutput('plot71')),
        column(6,plotOutput('plot72'))
      ),
      fluidRow(
        column(6,plotOutput('plot73')),
        column(6,plotOutput('plot74'))
      ),
      fluidRow(
        column(6,plotOutput('plot75')),
        column(6,plotOutput('plot76'))
      ),
      fluidRow(
        column(6,plotOutput('plot77')),
        column(6,plotOutput('plot78'))
      )
    ),
    tabPanel("Scales",
      fluidRow(
        column(6,plotOutput('plot79')),
        column(6,plotOutput('plot80'))
      ),
      fluidRow(
        column(6,plotOutput('plot81')),
        column(6,plotOutput('plot82'))
      ),
      fluidRow(
        column(6,plotOutput('plot83')),
        column(6,plotOutput('plot84'))
      ),
      fluidRow(
        column(6,plotOutput('plot85')),
        column(6,plotOutput('plot86'))
      ),
      fluidRow(
        column(6,plotOutput('plot87'))
      )
    )
  )
)

server <- function(input, output)
{
  lapply(seq(length(plots)), function(x){
    output[[paste0('plot', x)]] <- renderPlot(plots[x])
  })
}

shinyApp(ui, server)