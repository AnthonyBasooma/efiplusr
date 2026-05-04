


#' Title
#'
#' @param xdf
#' @param spp
#' @param labels
#' @param type
#' @param labels2
#' @param type1
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

  #one hot encoding but it drops NAs
  guild2 <- model.matrix(~ . - 1, data = gout)
  colnames(guild2) <- sub(paste0("^(", paste(colnames(gout), collapse="|"), ")"),"\\1.",colnames(guild2))

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

  densval <- as.data.frame(as.matrix(df)%*%as.matrix(guild3))

  ricval <- as.data.frame(as.matrix(faun01)%*%as.matrix(guild3))

  names(densval) <- paste("dens",names(densval),sep=".")

  names(ricval) <- paste("ric",names(ricval),sep=".")

  w <- cbind(densval,ricval)

  if(len==FALSE){
    w1 <- w[,paste(type,labels,sep=".")]
  }else{
    w1 <- w[,paste(type1,labels2,sep=".")]
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

  return(list(w2, ric, den, psalmo, guild2))
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


#' Standardized distance between the observed (sampled) and expected (no alteration) values in the community
#'
#' @param labels
#' @param mode
#' @param lat
#' @param region
#' @param medtype
#' @param env
#' @param observed
#' @param expected
#' @param obslength
#' @param efitype
#'
#' @returns
#' @export
#'
#' @examples
metricscore <- function(env, lat, labels, region, medtype,  mode = "none", observed, expected, obslength, efitype){

    match.arg(mode, choices = c("none", "salmonid", "cpyrinid"))

    #process regions and ecoregions

    #get ecoregions on boarded: can changed
    ecodata <- efiplusr::ecodata

    #column name with ecoregions
    e1 <- merge(x=env, y = ecodata, by.x = region, by.y ="ecoregion")

    #safegaurd for different options
    e1$medizone <- ifelse(e1[, medtype] %in% c(1, "1", "yes", "YES", "Yes", TRUE, "TRUE"),"Yes",
                 ifelse(e1[, medtype] %in% c(0, "0", "no", "NO", "No", FALSE, "FALSE"),"No",NA))

    e1$ecoregions <- ifelse(e1$medizone == "Yes" & env[, lat] < 45 &
                              e1$ecoabbr %in% c("Ita", "W.p", "Ibe"),"Med",
                            ifelse(e1$ecoabbr %in% c("Alp","Pyr"), "Alp",
                                   ifelse(e1$ecoabbr %in% c("E.p","Pon","Hun"), "Est",
                                          ifelse(e1$ecoabbr %in% c("Fen","Bor"), "Nor",
                                                 e1$ecoabbr))))

    #efidata <- efiplusr::efitype
    #e1 <-  merge(e1, efidata, by.x = spp, by.y = "species")


    e1$efitype <- ifelse(e1[,efitype] == "T.NA",NA,
                   ifelse(e1$ecoregions %in% c("Est", "Bal", "Med") |
                            e1[,efitype] %in% c("T.5", "T.6", "T.14", "T.15") |
                            e1$medizone == "Yes", "Cyprinid", "Salmonid"))

    e1$efitype[e1$ecoregions %in% c("Alp", "Nor")] <- "Salmonid"

    #get the observed and expected values
    row.names(e1) <- e1$code

    tf <- rownames(e1)%in%rownames(observed)

    region22 <- e1[which(tf==TRUE),]

    tf2 <- rownames(region22)%in%rownames(obslength)

    regionlength <- region22[which(tf2==TRUE),]


    if(mode=="none"){
        stdata <- efiplusr::stdpacklength
        centrs   <- stdata$center
        scaless  <-  stdata$scale
        limits   <- stdata$limits[labels]
    }else if(mode=="salmonid"){
        stdata    <- efiplusr::stdpackTrout
        centrs    <- stdata$center
        scaless   <-  stdata$scale
        limits    <- stdata$limits[labels]
        qclass    <- stdata$qclass
    }else{
        stdata    <- efiplusr::stdpackNoTrout
        centrs    <- stdata$center
        scaless   <- stdata$scale
        limits    <- stdata$limits[labels]
        qclass    <- stdata$qclass
    }

    eregions <-  regionlength$ecoregions

    #select variables
    scalev      <-  scaless[labels]
    centerv     <-  centrs[,labels]
    obsv1       <-  observed[, labels]
    fitted1     <-  expected[,labels]

    #log transform the data
    obsvv       <- log(obsv1+1)
    fittedvv    <- log(fitted1+1)

    #Median value of the residuals in the ecoregion
    #median is chosen because it is less sensitive to extreme values than the mean.
    indrowv <- match(eregions,row.names(centerv))
    indcolv <- match(names(obsvv),names(centerv))
    meancenter <- centerv[indrowv,indcolv]

    #model residual, i.e. difference between observed and expected metric value for the given site
    residual <- obsvv-fittedvv

    #centering the data
    resv <- as.matrix(residual-meancenter)

    #Standard deviation of the residuals in the whole undisturbed dataset for a given river zone(Salmonid or Cyprinid)
    zscore <- sweep(resv, MARGIN =  2, STATS = scalev, "/", check.margin = FALSE)

    #standardized zscore
    out <- as.data.frame(zscore)

    if(mode !="none"){
        object_nt <- out[,labels]
        res1 <- data.frame(sapply(1:ncol(object_nt),function(x) punif(object_nt[,x],limits[1,x],limits[2,x])))
        names(res1) <- labels
        row.names(res1) <- row.names(out)

        #ClassifIDScoring
        ss <- as.data.frame(sapply(labels, function(x) 6-as.numeric(cut(res1[,x],qclass[,x],right=FALSE))))
        row.names(ss) <- row.names(res1)
        names(ss) <- labels
        out <- list(zscore = out, idscore = ss)
    }

   return(out)
}







