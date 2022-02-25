library(dsims)
myshapefilelocation <- "C:/users/erexs/documents/simulation-strata/shapefiles/StrataPrj.shp"#here("shapefiles/StrataPrj.shp")
northsea <- make.region(region.name = "minkes",
            shape = myshapefilelocation,
            strata.name = c("South", "North"),
            units = "km")
areas <- northsea@area
prop.south <- areas[1]/sum(areas)
prop.north <- areas[2]/sum(areas)
total.abundance <- 3000
abund.south <- round(total.abundance * prop.south)
abund.north <- round(total.abundance * prop.north)
constant <- make.density(region = northsea, x.space = 10, constant = 1)
minkepop <- make.population.description(region = northsea,
                                        density = constant,
                                        N = c(abund.south, abund.north))
coverage.grid <- make.coverage(northsea, n.grid.points = 100)
trunc <- 5
equal.cover <- make.design(region = northsea,
                            transect.type = "line",
                            design = "systematic",
                            samplers=40,
                            design.angle = c(50, 40),
                            truncation = trunc,
                            coverage.grid = coverage.grid)
b <- generate.transects(equal.cover)
plot(northsea, b, covered.area=TRUE)
num.sims <- 1
pooled.hn <- make.ds.analysis(dfmodel = list(~1),
                              key = "hn",
                              criteria = "AIC",
                              truncation = trunc)
sigma.strata <- c(50,50)
detect <- make.detectability(key.function = "hn",
                             scale.param = sigma.strata,
                             truncation = trunc)
equalcover.sim <- make.simulation(reps = num.sims,
                                  design = equal.cover,
                                  population.description = minkepop,
                                  detectability = detect,
                                  ds.analysis = pooled.hn)
itran <- run.survey(equalcover.sim)
plot(itran, northsea, covered.area=TRUE)

onetransect <- itran@dist.data$distance[itran@dist.data$Sample.Label<2]
fivetransect <- itran@dist.data$distance[itran@dist.data$Sample.Label<6]
tentransect <- itran@dist.data$distance[itran@dist.data$Sample.Label<11]
twentytransect <- itran@dist.data$distance[itran@dist.data$Sample.Label<21]
all <- itran@dist.data$distance

### qqplot of uniformity (with bounds)
###  see https://stackoverflow.com/questions/19392066/simultaneous-null-band-for-uniform-qq-plot-in-r
library(car)
library(goftest)

unitest <- function(obj, title) {
  qqPlot(obj, distribution="unif", main=title, envelope=FALSE)
  fred <- cvm.test(obj, null="punif")
# text(.4, 1, paste("Omega=", round(fred$statistic,1), 
#                    "P=", round(fred$p.value,3)))
}
par(mfrow=c(3,2))
unitest(onetransect, "Single transect")
unitest(fivetransect, "5 transects")
unitest(tentransect, "10 transects")
unitest(twentytransect, "20 transects")
unitest(all, "39 transects")
### and cvm test
###  see https://search.r-project.org/CRAN/refmans/goftest/html/cvm.test.html


