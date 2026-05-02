


#' Title
#'
#' @param xdf
#' @param spp
#' @param labels
#' @param type
#' @param labels2
#' @param type2
#' @param len
#' @param tolerance
#'
#' @returns
#' @export
#'
#' @examples
fi_observed <- function(xdf, spp, labels = NULL, type = NULL, labels2 = NULL, type2 = NULL, len=FALSE, tolerance){

  salvec <- efiplusr::salmovec

  guilddf <- efiplusr::guild

  #get extract guild data
  gout <- guilddf[which(guilddf$species%in%spp==TRUE),] |>na.omit()

  row.names(gout) <- gout$species
  gout$species <- NULL

  guild2 <- model.matrix(~ . - 1, data = gout)

  lab = if(len==FALSE) labels else labels2

  guild3 <- guild2[,labs]

  #convert to presence absence
  pa <- function(x) as.data.frame((x > 0) * 1)

  tf <- colnames(xdf)%in%rownames(gout)
  df <- xdf[,tf==TRUE]
  df[is.na(df)] <- 0

  faun01 <- pa(df)

  den <- apply(df,1,sum)
  ric <- apply(faun01,1,sum)
  gfaunA <- as.data.frame(as.matrix(df)%*%as.matrix(guild3))
  gfaunR <- as.data.frame(as.matrix(faun01)%*%as.matrix(guild3))
  names(gfaunA) <- paste("dens",names(gfaunA),sep=".")
  names(gfaunR) <- paste("ric",names(gfaunR),sep=".")
  w <- cbind(gfaunA,gfaunR)

  if(len==FALSE){
    w1 <- w[,paste(type,labels,sep=".")]
  }else{
    w1 <- w[,paste(type2,labels2,sep=".")]
  }

  f2 <- function(x,tol) ifelse(abs(x) < tol & !is.na(x),0,x)

  w2 <- as.data.frame(apply(w1,2,function(x,y=Tol) f2(x,tol=tolerance)))

  stf <- names(df)%in%salvec
  nbsalmo <- sum(stf)

  if(nbsalmo == 1 & nbsalmo >0){
      psalmo <- df[,stf]
  }else if(nbsalmo > 1){
      psalmo <- apply(df[,stf],1,sum)
  }else{
    psalmo <- rep(0,nrow(df))
  }

  psalmo <- ifelse(den==0,NA,psalmo/den)

  return(list(w2, ric, den, psalmo))
}



#' Title
#'
#' @param env
#' @param model
#'
#' @returns
#' @export
#'
#' @examples
fi_expected <- function(env, model=TRUE){
  models =if(model==TRUE) {
    models <- get("modelint", envir = asNamespace("efiplusr"))
  }else{
   models <- get("lenmodel", envir = asNamespace("efiplusr"))
  }
  out <- as.data.frame(do.call(cbind, dd <- lapply(1:length(models), function(x){
    mo <- predict(models[[x]],newdata=as.data.frame(env),type="response")
    })))
  names(out) <- names(models)
  row.names(out) <- row.names(env)
  return(out)
}


#' Title
#'
#' @param df
#' @param region
#' @param lat
#' @param medtype
#' @param spp
#'
#' @returns
#' @export
#'
#' @examples
ecoregions <- function(df, spp, region, lat, medtype){

  #get ecoregions on boarded: can changed
  ecodata <- efiplusr::ecodata

  e1 <- merge(x=df, y =ecodata, by.x = region, by.y ="ecoregion")

  #safegaurd for different options
  e1$medizone <- ifelse(e1[, medtype] %in% c(1, "1", "yes", "YES", "Yes", TRUE, "TRUE"),"Yes",
               ifelse(e1[, medtype] %in% c(0, "0", "no", "NO", "No", FALSE, "FALSE"),"No",NA))

  e1$ecoregions <- ifelse(e1$medizone == "Yes" & df[, lat] < 45 &
                            e1$ecoabbr %in% c("Ita", "W.p", "Ibe"),"Med",
                          ifelse(e1$ecoabbr %in% c("Alp","Pyr"), "Alp",
                                 ifelse(e1$ecoabbr %in% c("E.p","Pon","Hun"), "Est",
                                        ifelse(e1$ecoabbr %in% c("Fen","Bor"), "Nor",
                                               e1$ecoabbr))))

  efidata <- efiplusr::efitype
  e2 <-  merge(e1, efidata, by.x = spp, by.y = "species")


  e2$efitype <- ifelse(e2$efizone == "T.NA",NA,
                 ifelse(e2$ecoregions %in% c("Est", "Bal", "Med") |
                          e2$efizone %in% c("T.5", "T.6", "T.14", "T.15") |
                          e2$medizone == "Yes", "Cyprinid", "Salmonid"))

  e2$efitype[e2$ecoregions %in% c("Alp", "Nor")] <- "Salmonid"
  return(e2)
}








