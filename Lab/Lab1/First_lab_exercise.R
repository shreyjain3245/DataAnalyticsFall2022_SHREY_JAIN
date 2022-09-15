EPI_data <- read.csv(file.choose(), skip = 1, header = TRUE)


View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI
tf <- is.na(EPI)
ntf <- EPI[!tf]
ntf
summary(EPI)
fivenum(EPI, na.rm = TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob = TRUE)
lines(density(EPI, na.rm = TRUE, bw = 1.))
bw <- "SJ"
rug(EPI)
plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)
par(pty = "s")
qqnorm(EPI)
qqline(EPI)
s <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot")
qqline(s)


ENVHEALTH <- EPI_data$ENVHEALTH
ECOSYSTEM <- EPI_data$ECOSYSTEM
DALY <- EPI_data$DALY
AIR_H <- EPI_data$AIR_H
WATER_H <- EPI_data$WATER_H
AIR_E <- EPI_data$AIR_E
WATER_E <- EPI_data$WATER_E
BIODIVERSITY <- EPI_data$BIODIVERSITY

boxplot(EPI, ENVHEALTH)
qqplot(EPI, ENVHEALTH)

boxplot(EPI, ECOSYSTEM)
qqplot(EPI, ECOSYSTEM)

boxplot(EPI, DALY)
qqplot(EPI, DALY)

boxplot(EPI, AIR_H)
qqplot(EPI, AIR_H)

boxplot(EPI, WATER_H)
qqplot(EPI, WATER_H)

boxplot(EPI, AIR_E)
qqplot(EPI, AIR_E)

boxplot(EPI, WATER_E)
qqplot(EPI, WATER_E)

boxplot(EPI, BIODIVERSITY)
qqplot(EPI, BIODIVERSITY)

EPILand <- EPI[!EPI_data$Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob = TRUE)
plot(ecdf(ELand), do.points = FALSE, verticals = TRUE)

SurfaceWater <- EPI[!EPI_data$No_surface_water]
SWater <- SurfaceWater[!is.na(SurfaceWater)]
hist(SWater)
hist(SWater, seq(30., 95., 1.0), prob = TRUE)
plot(ecdf(SWater), do.points = FALSE, verticals = TRUE)

Desert <- EPI[!EPI_data$Desert]
Deserted <- Desert[!is.na(Desert)]
hist(Deserted)
hist(Deserted, seq(30., 95., 1.0), prob = TRUE)
plot(ecdf(Deserted), do.points = FALSE, verticals = TRUE)

HighPopDensity <- EPI[!EPI_data$High_Population_Density]
PopDensity <- HighPopDensity[!is.na(HighPopDensity)]
hist(PopDensity)
hist(PopDensity, seq(30., 95., 1.0), prob = TRUE)
plot(ecdf(PopDensity), do.points = FALSE, verticals = TRUE)

EPI_South_Asia <- EPI[EPI_data$GEO_subregion == "South Asia"]
South_Asia <- EPI_South_Asia[!is.na(EPI_South_Asia)]
hist(South_Asia)
qqnorm(South_Asia)