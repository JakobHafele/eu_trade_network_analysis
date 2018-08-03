library(plyr)
library(rowr)

Core <- c("eudeu", "eufra", "euita", "eugbr", "euaut", "eudnk", "euswe", "eufin", "eunld", "eubel", "eulux", "euirl")
Periphery <- c("euprt", "euesp", "eugrc", "eumlt", "eucyp")
Semiperiphery <- c("euhun", "eucze", "eusvn", "eusvk", "eupol")
East <- c("eubgr", "eurou", "euhrv")
Baltics <- c("euest", "eulva", "eultu")

cluster <- data.frame(Core)
cluster <- cbind.fill(Core, Periphery, Semiperiphery, East, Baltics, fill = NA)
colnames(cluster) <- c("Core", "Periphery", "Semiperiphery", "East", "Baltics")
setDF(oec_countrycodes)
cluster$Core <- countrycode(cluster$Core, "id", "name", custom_dict = oec_countrycodes)
cluster$Periphery <- countrycode(cluster$Periphery, "id", "name", custom_dict = oec_countrycodes)
cluster$Semiperiphery <- countrycode(cluster$Semiperiphery, "id", "name", custom_dict = oec_countrycodes)
cluster$East <- countrycode(cluster$East, "id", "name", custom_dict = oec_countrycodes)
cluster$Baltics <- countrycode(cluster$Baltics, "id", "name", custom_dict = oec_countrycodes)

print(xtable(Cluster, type = "latex"), file = "output/Clusters.tex", include.rownames=FALSE)

?cbind.fill
