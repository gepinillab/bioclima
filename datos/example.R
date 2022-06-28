library(terra)
library(here)
library(purrr)
# Min temperature
tmin <- terra::rast(here::here("datos", "rasters", "tmin_1958-01-to-2019-12.tif"))
tmin <- tmin[[733:744]]
# Max temperature
tmax <- terra::rast(here::here("datos", "rasters", "tmax_1958-01-to-2019-12.tif"))
tmax <- tmax[[733:744]]
# Average temperature
tprop <- misqua(tmin = tmin, tmax = tmax)
# Precipitation
prcp <- terra::rast(here::here("datos", "rasters", "prcp_1958-01-to-2019-12.tif"))
prcp <- prcp[[733:744]]

## BIOS
# Bio01
uno <- ata(tavg = tprop)
one <- clima(bios = 1, tmin = tmin, tmax = tmax)
# Bio02
dos <- bosa(tmin = tmin, tmax = tmax)
two <- clima(bios = 2, tmin, tmax)
# Bio04
cuatro <- muihica(tavg = tprop)
four <- clima(bios = 4, tmin, tmax)
# Bio05
cinco <- hisca(tmax = tmax)
five <- clima(bios = 5, tmax = tmax)
# Bio06
seis <- ta(tmin = tmin)
six <- clima(bios = 6, tmin = tmin)
# Bio07
siete <- cuhupcua(bio05 = cinco, bio06 = seis)
seven <- clima(bios = 7, tmin, tmax)
# Bio03
tres <- mica(bio02 = dos, bio07 = siete)
three <- clima(bios = 3, tmin, tmax)
# Bio12
doce <- quihicha_bosa(prcp = prcp)
twelve <- clima(bios = 12, prcp = prcp)
# Bio13
trece <- quihicha_mica(prcp = prcp)
thirteen <- clima(bios = 13, prcp = prcp)
# Bio14
catorce <- quihicha_muihica(prcp = prcp)
fourteen <- clima(bios = 14, prcp = prcp)
# Bio15
quince <- quihicha_hisca(prcp = prcp)
fifteen <- clima(bios = 15, prcp = prcp)
# Ventana prcp
iotuc <- ventana(prcp, period = 3, circular = FALSE)
# Bio16
diezyseis <- quihicha_ta(wet = iotuc)
sixteen <- clima(bios = 16, prcp = prcp)
# Bio17
diezysiete <- quihicha_cuhupcua(wet = iotuc)
seventeen <- clima(bios = 17, prcp = prcp)
# Ventana temperature
chituc <- ventana(tprop, period = 3, circular = FALSE) / 3
# Bio10
diez <- ubchihica(tmp = chituc)
ten <- clima(bios = 10, tmin, tmax)
# Bio11
once <- quihicha_ata(tmp = chituc)
eleven <- clima(bios = 11, tmin, tmax)
# Bio08
ocho <- suhusa(tmp = chituc, wet = iotuc)
eight <- clima(bios = 8, tmin, tmax, prcp)
# Bio09
nueve <- aca(tmp = chituc, wet = iotuc)
nine <- clima(bios = 9, tmin, tmax, prcp)
# Bio18
diezyocho <- quihicha_suhusa(tmp = chituc, wet = iotuc)
eighteen <- clima(bios = 18, tmin, tmax, prcp)
# Bio19
diezynueve <- quihicha_aca(tmp = chituc, wet = iotuc)
nineteen <- clima(bios = 19, tmin, tmax, prcp)

bios <- clima(bios = 1:27, tmin, tmax, prcp, period = 3, circular = FALSE)
bios2 <- clima(bios = 1:27, tmin, tmax, prcp, period = 3, circular = FALSE)
bios3 <- clima(bios = 1:27, tmin, tmax, prcp, period = 8, circular = FALSE)
