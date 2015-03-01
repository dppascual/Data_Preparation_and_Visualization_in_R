##### PREPARACIÓN DE LOS DATOS A VISUALIZAR #####
require(data.table)
require(reshape2)
require(ggplot2)
setwd("")

#####################
#
# Obtenemos los datos del tiempo en la Comunidad de Madrid
#
#####################
download.file("http://academic.udayton.edu/kissock/http/Weather/gsod95-current/SPMADRID.txt", destfile = "data/SPMADRID.txt", method = "curl", extra = "-L")
#system("bunzip2 SP1.csv.bz2")
df.weather <- read.table(file = "data/SPMADRID.txt", header = FALSE, stringsAsFactors = FALSE)
dt.weather.origin <- as.data.table(df.weather)

####################
#
# Preparamos los datos del tiempo
#
####################
dt.weather <- copy(dt.weather.origin)
setnames(dt.weather, c("month", "day", "year", "temp"))
dt.weather[temp != -99,':=' (temp = round((temp-32)*5/9, digits = 2))]
dt.tmp <- copy(dt.weather)

# Obtenemos los valores del año a representar (2013)
dt.weather.present <- dt.weather[year > 2009 & year < 2014, .SD[order(day)], 
                                 by = .(year, month)][, newday := seq(1, length(day)), 
                                                      by = year][temp != -99 & year == 2013]

# Obtenemos el valor máximo y mínimo de temperatura para cada día del año
dt.weather.past <- dt.weather[year > 2009 & year < 2014 & year != 2013, .SD[order(day)], 
                              by = .(year, month)][, newday := seq(1, length(day)), 
                                                   by = year][temp != -99,':=' (upper = max(temp),
                                                                                               lower = min(temp)) , by = newday]
####################
#
# Preparamos los datos del censo
#
####################

# Cargamos y limpiamos el dt con los nacimientos por provincia en los últimos años (2010, 2011, 2012 y 2013)
tmp <- lapply(c(10:13), function(x) 
  read.table(paste0(paste0("data/nacimientos_20", x), ".csv"), sep = ",", header = T, encoding = "UTF-8"))

preparation_data_birth <- function(data, year)
{
  nacimientos <- as.data.table(data)
  setnames(nacimientos, "X", "Provincias")
  melt(nacimientos[,':=' (X.1 = NULL, Year = year)][Provincias == "Madrid"], id = c("Provincias", "Year"))
}

dt.nacimientos2010 <- preparation_data_birth(tmp[[1]], "2010")
dt.nacimientos2011 <- preparation_data_birth(tmp[[2]], "2011")
dt.nacimientos2012 <- preparation_data_birth(tmp[[3]], "2012")
dt.nacimientos2013 <- preparation_data_birth(tmp[[4]], "2013")

dt.nacimientos <- rbind(dt.nacimientos2010, dt.nacimientos2011, dt.nacimientos2012, dt.nacimientos2013)
dt.nacimientos.past <- rbind(dt.nacimientos2010, dt.nacimientos2011, dt.nacimientos2012)
setnames(dt.nacimientos.past, c("variable", "value"), c("Month", "Births"))
dt.nacimientos.past[, ':=' (upper = max(Births), lower = min(Births)) , by = Month]

####################
#
# Normalizamos y juntamos los datos
#
####################
normalizacion <- function(data, max, min)
{
  # Para un rango 0 - 100
  return (((data - min) / (max - min)) * 100)
}

dt.weather.past[,':=' (upper_births = normalizacion(dt.nacimientos.past[Month == month.name[month], upper][1], max(dt.nacimientos$value), min(dt.nacimientos$value)), 
                       lower_births = normalizacion(dt.nacimientos.past[Month == month.name[month], lower][1], max(dt.nacimientos$value), min(dt.nacimientos$value))) , by = month]
dt.weather.present[,':=' (births = normalizacion(dt.nacimientos2013$value[month], max(dt.nacimientos$value), min(dt.nacimientos$value))) , by = month]

####################
#
# Representación de los datos
#
####################
# variable para el eje y
axes.y <- seq(-10, 100, by=10)
png("data/graph.png", width = 1256, height = 620)

ggplot(dt.weather.past, aes(newday, temp)) +
        theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
        geom_linerange(dt.weather.past, mapping=aes(x=newday, ymin=lower, ymax=upper), colour = "royalblue2", alpha=.1) +
        geom_linerange(dt.weather.past, mapping=aes(x=newday, ymin=lower_births, ymax = upper_births), colour = "wheat2", alpha=.1) +
        geom_line(dt.weather.present, mapping=aes(x=newday, y=temp, group=1), colour = "royalblue4") +
        geom_line(dt.weather.present, mapping=aes(x=newday, y=births, group=1), colour = "wheat4") +
        geom_vline(xintercept = 0, colour = "wheat3", linetype=1, size=1) + 
        geom_hline(yintercept = -10, colour = "white", linetype=1) +
        geom_hline(yintercept = 0, colour = "white", linetype=1) +
        geom_hline(yintercept = 10, colour = "white", linetype=1) +
        geom_hline(yintercept = 20, colour = "white", linetype=1) +
        geom_hline(yintercept = 30, colour = "white", linetype=1) +
        geom_hline(yintercept = 40, colour = "white", linetype=1) +
        geom_hline(yintercept = 50, colour = "white", linetype=1) +
        geom_hline(yintercept = 60, colour = "white", linetype=1) +
        geom_hline(yintercept = 70, colour = "white", linetype=1) +
        geom_hline(yintercept = 80, colour = "white", linetype=1) +
        geom_hline(yintercept = 90, colour = "white", linetype=1) +
        geom_hline(yintercept = 100, colour = "white", linetype=1) +
        geom_vline(xintercept = 31, colour = "wheat3", linetype=3, size=.5) +
        geom_vline(xintercept = 59, colour = "wheat3", linetype=3, size=.5) +
        geom_vline(xintercept = 90, colour = "wheat3", linetype=3, size=.5) +
        geom_vline(xintercept = 120, colour = "wheat3", linetype=3, size=.5) +
        geom_vline(xintercept = 151, colour = "wheat3", linetype=3, size=.5) +
        geom_vline(xintercept = 181, colour = "wheat3", linetype=3, size=.5) +
        geom_vline(xintercept = 212, colour = "wheat3", linetype=3, size=.5) +
        geom_vline(xintercept = 243, colour = "wheat3", linetype=3, size=.5) +
        geom_vline(xintercept = 273, colour = "wheat3", linetype=3, size=.5) +
        geom_vline(xintercept = 304, colour = "wheat3", linetype=3, size=.5) +
        geom_vline(xintercept = 334, colour = "wheat3", linetype=3, size=.5) +
        geom_vline(xintercept = 365, colour = "wheat3", linetype=3, size=.5) +
        coord_cartesian(ylim = c(-10,100)) +
        scale_y_continuous(breaks = seq(-10,100, by=10), labels = axes.y) +
        scale_x_continuous(expand = c(0, 0),
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = month.name) +
        ggtitle("La temperatura y el número de nacimientos en Madrid a lo largo de 2013") +
        theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
        annotate("text", x = 77, y = 98, label = "Temperatura en grados Celsius (ºC) - Número de nacimientos (Eje y normalizado)", size=4, fontface="bold") +
        annotate("text", x = 56, y = 93,
           label = "Según la distribución de temperaturas que se representa, no se puede obtener", size=3, colour="gray30") +
        annotate("text", x = 57, y = 90,
           label = "relación alguna con el número de nacimientos a lo largo de 2013.", size=3, colour="gray30") +
        annotate("text", x = 56, y = 87,
           label = "Lo que se puede comprobar es que el número de nacimientos en el año 2013", size=3, colour="gray30") +
        annotate("text", x = 57, y = 84, label = "está por debajo del intervalo calculado en los años anteriores", size=3, colour="gray30") +
        annotate("segment", x = 181, xend = 181, y = -5, yend = 10, colour = "wheat2", size=3) +
        annotate("text", x = 158, y = 4.5, label = "Escala normalizada de", size=4, colour="gray30") +
        annotate("text", x = 158, y = 2.5, label = "nacimientos en 2013", size=4, colour="gray30") +
        annotate("text", x = 200, y = 10, label = paste0("RECORD HIGH: ",max(dt.nacimientos$value)), size=4, colour="gray30") +
        annotate("text", x = 200, y = -5, label = paste0("RECORD LOW: ",min(dt.nacimientos$value)), size=4, colour="gray30")

dev.off()






