

#' Title
#'
#' @param x
#' @param species
#' @param code
#' @param runs
#' @param slope
#' @param tempjan
#' @param tempjul
#' @param water
#' @param sediment
#' @param geomorph
#' @param method
#' @param area
#' @param floodpsite
#' @param distsource
#' @param labels
#' @param type
#' @param labels2
#' @param type2
#' @param len_up
#' @param len_down
#' @param tolerance
#'
#' @importFrom stats predict model.matrix na.omit
#'
#' @returns
#' @export
#'
#' @examples
fi_index <- function(x, species, code, runs,
  slope = "Actual.river.slope",
  tempjan = "Temp.jan",
  tempjul = "Temp.jul",
  water = "Water.source.type",
  sediment = "Natural.sediment",
  geomorph = "Geomorph.river.type",
  method = "Method",
  area = "Area.ctch",
  floodpsite = "Floodplain.site",
  distsource = "Distance.from.source",
  labels,
  type,
  labels2,
  type2,
  len_up = "Number.length.over.150",
  len_down = "Number.length.below.150",
  tolerance = 1e-7){

  #==========
  spp <-  unlist(x[, species])
  runs <- unlist(x[, runs])
  codv <- unlist(x[, code])
  len_up <- unlist(x[, len_up])
  len_down <- unlist(x[, len_down])

  faun     <- as.data.frame(tapply(runs, list(codv, spp), identity))
  len_b150 <- as.data.frame(tapply(len_down, list(codv, spp), identity))
  len_u150 <- as.data.frame(tapply(len_up, list(codv, spp), identity))


  #observed parameters
  obsm <- fi_observed(xdf = faun, spp = spp, type = type, labels = labels, tolerance=tolerance)
  w2       <- obsm[[1]]
  richness <- obsm[[2]]
  dens     <- obsm[[3]]
  psalmo   <- obsm[[4]]

  len1       <- fi_observed(xdf = len_b150, spp = spp, type2 = type2,  labels2 = labels2, tolerance=tolerance, len = TRUE)[[1]]

  len2       <- fi_observed(xdf =len_u150, spp = spp,   type2 = type2, labels2 = labels2, tolerance=tolerance, len = TRUE)[[1]]

  #get the expected parameters
  df <-  x[!duplicated(codv),]

  nameOrder <- row.names(w2)

  row.names(df) <- df[, code]

  df <- df[nameOrder,]

  slope     <-    unlist(df[,slope])
  water     <-    unlist(df[,water])
  sediment  <-    unlist(df[,sediment])
  geomorph  <-    unlist(df[,geomorph])
  method    <-    unlist(df[,method])
  area      <-    unlist(df[,area])
  distsource <-   unlist(df[,distsource])

  # slope
  lslope <- ifelse(slope < 0.002,log(0.001),log(slope))

  Tjul <- unlist(df[,tempjul])

  Tdif <- Tjul-unlist(df[,tempjan])

  # natsed
  natsed <- as.factor(c("large","medium",rep("small",3))[match(sediment,c("Boulder/Rock","Gravel/Pebble/Cobble","Organic","Sand","Silt"))])

  # fldpl
  fldpl <- unlist(df[,floodpsite])

  # watersource
  watersource <- as.factor(c("Non-Pluvial","Non-Pluvial","Non-Pluvial",
  "Pluvial")[match(water,c("Glacial","Groundwater","Nival","Pluvial"))])

  # geomorph
  geomorph <- as.factor(c("braided","meand","meand","constraint","sinuous")[match(geomorph,
  c("Braided","Meand regular","Meand tortous","Naturally constraint no mob","Sinuous"))])

  # method   "Boat"   "Mixed"  "Wading"
  method <- as.factor(c("boat","noboat","noboat")[match(method,c("Boat","Mixed","Wading"))])

  # dist
  ldist <- ifelse(distsource < 0.5,log(0.05),log(distsource))

  # drainage area
  lDR <- ifelse(area< 0.5,log(0.05),log(area))

  geomodels <- get("geomodel", envir = asNamespace("efiplusr"))

  dout <- data.frame(lslope,lDR,ldist,method,area, geomorph,watersource, fldpl, natsed, Tdif, Tjul)

  syngeom1 <- predict(geomodels[[1]],newdata=dout)
  syngeom2 <- predict(geomodels[[2]],newdata=dout)


  env11 <- data.frame(dout,syngeomorph1=syngeom1,syngeomorph2=syngeom2,richesse=richness,captures=dens,psalmo=psalmo)

  env1 <- data.frame(df,env11)


  fitm   <- fi_expected(env = env1, model = TRUE)

  fitlen <- fi_expected(env = env1, model = FALSE)

  return(list(envdata= env1, observed=w2, expected = fitm, oblengthb150= len1, oblengthu150 = len2, expectedlength = fitlen))
}
