#
# efindex <- function(data, code, species, runs, sediment){
#
#     if(!is(data, "data.frame")) stop("Only dataframe accepted.")
#
#
#
#     df1 <- inputdata[,c("code","species","Total.number.run1")]
#
# names(df1) <- c("code","species","number")
#
# df12 <- xtabs2df(xtabs(Total.number.run1 ~ code + species, data=inputdata))
#
# l1  <- xtabs2df(xtabs(Number.length.below.150 ~ code + species, data=inputdata))
# l2  <- xtabs2df(xtabs(Number.length.over.150 ~ code + species, data=inputdata))
#
#
# labels1 <- c("WQO2.O2INTOL","HTOL.HINTOL","HabSp.RHPAR","Repro.LITH")
# type1 <- c("dens","dens","ric","dens")
#
# obs1 <- MetricCompute(faun,guild,labels=labels1,type=type1)
# }
#
# guildata <- efiplusr::guild
#
#
# # row code preparation
# inputdata$sites <- paste(inputdata$Site.code,inputdata$Day,inputdata$Month,inputdata$Year,sep="_")
#
# # Species names checking
# inputdata$Species <- CheckSpNames(inputdata$Species)
# inputdata$species <- gsub(" ",".",inputdata$Species)
#
# #--------------------------------------------------
# # faunistic table pr?paration
# # give a table [code x species]
# #--------------------------------------------------
# faun <- prepTab(inputdata,param=c("code","species","Total.number.run1"))
#
# prepTab <- function(df,param=c("code","species","number")){
#   if(!inherits(df,'data.frame'))
#     stop("non convenient argument !")
#   df <- df[,param]
# # need to modify the formula for more genericity !
# # 11/26/08 14:57:31
#   names(df) <- c("code","species","number")
#   df <- xtabs2df(xtabs(number ~ code + species, data=df))
#   return(df)
# }
#
# ss <- inputdata[, c("code","Species","Total.number.run1")]
#
# fn <- as.data.frame(xtabs(Total.number.run1 ~ code + Species, data=ss))
#
# fn2 <- as.data.frame(tapply(ss$Total.number.run1, list(ss$code, ss$Species), identity))
#
#
#
#
# labels1 <- c("WQO2.O2INTOL","HTOL.HINTOL","HabSp.RHPAR","Repro.LITH")
# type1 <- c("dens","dens","ric","dens")
# obs1 <- MetricCompute(faun,guild,labels=labels1,type=type1)
#
#
# ##get the guild data
# guilddx <- efiplusr::guild
#
#
# #extract the species names from the guild found in the input data
# x <- inputdata
# spp <- "Species"
# spl <- unlist(x[, spp])
#
# xxx <- which(guilddx$species%in%spl==TRUE)#species in input data but in alo in guild
#
# spguild <- guilddx[xxx,]
#
#
# acm.disjonctif <-function (df){
#     acm.util <- function(i) {
#         cl <- df[, i]
#         cha <- names(df)[i]
#         n <- length(cl)
#         cl <- as.factor(cl)
#         x <- matrix(0, n, length(levels(cl)))
#         x[(1:n) + n * (unclass(cl) - 1)] <- 1
#         dimnames(x) <- list(row.names(df), paste(cha, levels(cl),
#             sep = "."))
#         return(x)
#     }
#     G <- lapply(1:ncol(df), acm.util)
#     G <- data.frame(G, check.names = FALSE)
#     return(G)
# }
# colnames(guild2)
#
# row.names(spguild) <- spguild$species
# spguild$species <- NULL
#
# #R does not dro the levels
# cl1 <- spguild[, 1]
# cha <- names(spguild)[1]
# n <- length(cl1)
# cl2 <- as.factor(cl1)
# x3 <- matrix(0, n, length(levels(cl2)))
#
# x3[(1:n) + n * (unclass(cl2) - 1)] <- 1
# dimnames(x3) <- list(row.names(spguild), paste(cha, levels(cl2), sep = "."))
#
#
#
# xxt <- as.matrix(spguild)
# x01t <- as.numeric(xxt > 0)
# dim(x01t) <- dim(xxt)
# x01tt <- as.data.frame(x01t)
# names(x01tt) <- names(spguild)
# row.names(x01tt) <- row.names(spguild)
#
# #instead of preTab for fauna
#
# fn2 <- as.data.frame(tapply(ss$Total.number.run1, list(ss$code, ss$Species), identity))
# #get density for site 1
# d_s <- apply(fn2, 1, sum)
#
# #get richnes for site 1
# #scale df to 1
# fn3 <- fn2
# fn3[] <- 1
# d_r <- apply(fn2, 1, length)
# d_r <- apply(fn3, 1, sum)
#
# gfaunA1 <- as.data.frame(as.matrix(fn2)%*%as.matrix(x01tt))
# gfaunR1 <- as.data.frame(as.matrix(fn3)%*%as.matrix(x01tt))
# names(gfaunA1) <- paste("dens",names(gfaunA1),sep=".")
# names(gfaunR1) <- paste("ric",names(gfaunR1),sep=".")
# w1 <- cbind(gfaunA1,gfaunR1)
#
# #if(!missing(type))
# w2 <- w1[,paste(type1,labels1,sep=".")]
# w3 <- as.data.frame(apply(w2,2,function(x,y=Tol) f2tolerence(x,tol=1e-7)))
#
#
#   nbsalmo <- sum(names(faun)%in%salmospecies)
#
#
#
#   if(nbsalmo == 1 & nbsalmo >0){
#     psalmo <- faun[,names(faun)%in%salmospecies]
#   }else if(nbsalmo > 1){
#     psalmo <- apply(faun[,names(faun)%in%salmospecies],1,sum)
#   }else psalmo <- rep(0,nrow(faun))
#   psalmo <- ifelse(den==0,NA,psalmo/den)
#   attr(w,"Richness") <- ric
#   attr(w,"Density") <- den
#   attr(w,"Psalmo") <- psalmo
#   attr(w,"Nbmetric") <- ncol(guild)
#   return(w)
#
#
#
#
#
# psm <- ifelse(d_s==0,NA,psalmo/d_s)
# attributes(obs1)
#
# #========
# #compute the density and richness of the sites
# #get sites
# sites <- inputdata$code
#
# #get species based of each sites
#
# sp1 <- ss[ss$code=="xxxxxx_5_8_20",]
# d_sp <- sum(sp1$Total.number.run1)
# rs_sp <- length(sp1$Species)
#
# pp <- sp1$Species=="Salmo trutta fario"
#
# c1 <- split(sp1, seq_len(nrow(sp1)))
# c2 <- as.list(sp1)
# x11 <- names(spguild)[2]
# x12 <- as.character(spguild[,2])
# x13 <- paste0(x11,"_",x12)
#
# nn <- names(spguild)
#
# x14 <- lapply(1:length(nn), function(x)paste0(nn[x],"_",as.character(spguild[,x])))
#
# x15 <- stack(spguild)
# x15 <- data.frame(WQ02 = c("O2INTOL","O2INTOL"),
#                  HTOL = c("HINTOL","HINTOL"))
#
#
# cc <- rownames(spguild)[1]
# v <- sp1$Total.number.run1[which(sp1$Species==cc)]
# c1 <- as.character(spguild[1,4])
#
# unzload(paste(directory,"/internal/internal.zip",sep=""),"guildNew.rda")
#
# faun1 <- rescale.names(faun,guild,"col.row")
# guild1 <- rescale.names(guild,faun,"row.col")
# guild2 <- acm.disjonctif(guild1)
# guild3 <- guild2[,labels1]
#
#
# faun01 <- scale01(faun1)
# den <- apply(faun1,1,sum)
# ric <- apply(faun01,1,sum)
# gfaunA <- as.data.frame(as.matrix(faun1)%*%as.matrix(guild3))
# gfaunR <- as.data.frame(as.matrix(faun01)%*%as.matrix(guild3))
# names(gfaunA) <- paste("dens",names(gfaunA),sep=".")
# names(gfaunR) <- paste("ric",names(gfaunR),sep=".")
# w <- cbind(gfaunA,gfaunR)
#
# w1 <- w[,paste(type1,labels1,sep=".")]
#
# w2 <- as.data.frame(apply(w1,2,function(x,y=Tol) f2tolerence(x,tol=1e-7)))
# nbsalmo <- sum(names(faun1)%in%salmospp)
#
#   if(nbsalmo == 1 & nbsalmo >0){
#     psalmo <- faun[,names(faun)%in%salmospecies]
#   }else if(nbsalmo > 1){
#     psalmo <- apply(faun1[,names(faun1)%in%salmospp],1,sum)
#   }else psalmo <- rep(0,nrow(faun))
#
#
#   psalmo <- ifelse(den==0,NA,psalmo/den)
#   attr(w2,"Richness") <- ric
#   attr(w2,"Density") <- den
#   attr(w2,"Psalmo") <- psalmo
#   attr(w2,"Nbmetric") <- ncol(guild3)
#
# abundance1 <- attributes(w2)$Density
# richness1 <- attributes(w2)$Richness
# psalmo1 <- attributes(w2)$Psalmo
#
# site1 <- faun[rownames(faun)=="xxxxxx_5_8_20",]
# g1 <- as.character(guild1[,3])
# nn <- names(site1)
#
# d1 <- site1[,nn[2]]
#
# #validate length
#
# #expected metric
# unzload(paste(directory,"/internal/internal.zip",sep=""),"models.rda")
# unzload(paste(directory,"/internal/internal.zip",sep=""),"geomodels.rda")
#
# env1 <- prep_env(inputdata,richness=richness1,abundance=abundance1,psalmo=psalmo1,
#   geomodels=geomodels,nameOrder=row.names(w2),
#   slopecol = "Actual.river.slope",
#   tempjan = "Temp.jan", tempjul = "Temp.jul",
#   watersourcetype = "Water.source.type",
#   sedimentnatural = "Natural.sediment",
#   Geomorphology = "Geomorph.river.type",
#   Method = "Method",
#   AreaCatch = "Area.ctch",
#   Floodplainsite = "Floodplain.site",
#   DistancefromSource = "Distance.from.source")
#
# pl <- length(models)
# nl <- nrow(env)
#
#
#
#
#
#
#
#
#
