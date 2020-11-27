# create environment in which to put cookie_handler
.pkgenv <- new.env(parent=emptyenv())

# alternative url is "http://apis.google.com/Cookies/OTZ"
# function to create cookie_handler, which is necessary to run get_widget()
get_api_cookies <- function(cookie_url) {
    # create new handler
    cookie_handler <- curl::new_handle()

    # set options for the proxy
    proxy_domain <- ifelse(is.null(.pkgenv[["handle_domain"]]), "", paste0(.pkgenv[["handle_domain"]],"\\"))
    proxy_user_pwd <- paste0(proxy_domain, .pkgenv[["handle_user"]], ":", .pkgenv[["handle_password"]])
    curl_opts <- list(ssl_verifypeer=0L,
                      proxyuserpwd=proxy_user_pwd,
                      proxyauth=.pkgenv[["handle_proxyauth"]],
                      proxy=.pkgenv[["handle_proxyhost"]],
                      proxyport=.pkgenv[["handle_proxyport"]])
    ## add extra curl options
    curl_opts <- append(curl_opts, .pkgenv[["handle_extra_curl_opts"]])
    curl::handle_setopt(handle=cookie_handler, .list=curl_opts)

    # fetch API cookies
    cookie_req <- curl::curl_fetch_memory(cookie_url, handle = cookie_handler)
    curl::handle_cookies(cookie_handler)
    # assign handler to .pkgenv environment
    .pkgenv[["cookie_handler"]] <- cookie_handler
    return(NULL)
}

check_time <- function(time_ranges) {
    stopifnot(is.character(time_ranges))

    fixed_format <- c(
        "now 1-H", # last hour
        "now 4-H", # last four hours
        "now 1-d", # last day
        "now 7-d", # last seven days
        "today 1-m", # past 30 days
        "today 3-m", # past 90 days
        "today 12-m", # past 12 months
        "today+5-y", # last 5 years (default)
        "all" # Since begening of Google Trends (2004)
    )

    for (tr in time_ranges){

        ## Return TRUE if one of the basic date formats is used
        if (tr %in% fixed_format) {
            return(TRUE)
        }

        ## The other possible format is by using time range
        time <- unlist(strsplit(tr, " "))

        ## Need to be a vector of two
        if (length(time) != 2) {
            return(FALSE)
        }

        if(!grepl("T",time[1])){
            start_date <- as.POSIXct(format(anytime::anydate(time[1],tz="UTC"),tz="UTC"),tz="UTC")
            end_date <- as.POSIXct(format(anytime::anydate(time[2],tz="UTC"),tz="UTC"),tz="UTC")
        }else{
            start_date <- anytime::anytime(time[1])
            end_date <- anytime::anytime(time[2])
        }

        if (is.na(start_date) | is.na(end_date)) {
            return(FALSE)
        }

        ## Start date can't be after end date
        if (start_date >= end_date) {
            return(FALSE)
        }

        ## Start date can't be before 2004-01-01
        if (start_date < as.POSIXct("2004-01-01",tz="UTC")) {
            return(FALSE)
        }

        ## End date can't be after today
        if (end_date > as.POSIXct(Sys.time())) {
            return(FALSE)
        }
    }

    return(TRUE)
}


get_widget <- function(comparison_item, category, gprop, hl, cookie_url, tz) {
    token_payload <- list()
    token_payload$comparisonItem <- comparison_item
    token_payload$category <- category
    token_payload$property <- gprop

    url <- paste0(URLencode(paste0("https://trends.google.com/trends/api/explore?hl=",hl,"&tz=",tz,"&req=")),
                  encode_payload(paste0(jsonlite::toJSON(token_payload, auto_unbox = TRUE)),reserved = TRUE,repeated=TRUE),
                  URLencode(paste0("&tz=",tz)))

    #url <- encode_keyword(url)

    # if cookie_handler hasn't been set up, get the requisite cookies from Google's API
    if(!exists("cookie_handler", envir = .pkgenv)){ get_api_cookies(cookie_url) }
    # get the tokens etc., using the URL and the cookie_handler
    widget <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

    stopifnot(widget$status_code == 200)

    ## Fix encoding issue for keywords like österreich"
    temp <- rawToChar(widget$content)
    Encoding(temp) <- "UTF-8"

    myjs <- jsonlite::fromJSON(substring(temp, first = 6))

    # The above will silently convert strings containing "NA" to
    # a logical NA (see: https://github.com/jeroen/jsonlite/issues/314)
    # There currently is no way to fix this other than setting simplifyVector = F
    # which likely breaks other things. Thus what follows is a hacky fix to
    # catch the likely case that somebody is looking for searches with
    # geo="NA"
    if (is.logical(myjs$widgets$request$comparisonItem[[1]]$geo$country)) {
        myjs$widgets$request$comparisonItem[[1]]$geo$country <- comparison_item$geo
        myjs$widgets$request$geo$country <- comparison_item$geo
        myjs$widgets$request$restriction$geo$country <- comparison_item$geo
        myjs$widgets$geo <- comparison_item$geo
    }

    widget <- myjs$widgets
}

interest_over_time <- function(widget, comparison_item,tz) {
    payload2 <- list()
    # if there is a mix of search and topic terms requests are all shifted by one
    # for some reason. Maybe there is a better fix for this. I don't understand
    # precisely the structure of the widget.
    # try this example:
    # topicKeys <- c("/m/05s_khw", "Assassins Creed Brotherhood", "/m/0gmg6lv")
    # vs.
    # topicKeys <- c("Assassins Creed", "Assassins Creed Brotherhood", "Assassins Creed Rogue")
    # gtrends(topicKeys, time = "all")

    # the following conditional statments are necessary when no keyword is supplied but
    # a search for a category is called for
    if(is.null(unlist(widget$request$comparisonItem))){
        onlyCategory <- TRUE
    }else if(!any(grepl("keyword",names(unlist(widget$request$comparisonItem))))){
        onlyCategory <- TRUE
    }else{
        onlyCategory <- FALSE
    }

    if(onlyCategory){


        payload2$locale <- widget$request$locale[1]
        payload2$comparisonItem <- widget$request$comparisonItem[[1]]
        payload2$resolution <- widget$request$resolution[1]
        payload2$requestOptions$category <- widget$request$requestOptions$category[1]
        payload2$requestOptions$backend <- widget$request$requestOptions$backend[1]
        payload2$time <- widget$request$time[1]
        payload2$requestOptions$property <- widget$request$requestOptions$property[1]
        token_payload2 <- widget$token[1]
        url <- paste0(URLencode("https://www.google.com/trends/api/widgetdata/multiline/csv?req="),
                      URLencode(jsonlite::toJSON(payload2, auto_unbox = T,null="list"),reserved = TRUE),
                      URLencode(paste0("&token=", token_payload2,"&tz=",tz)))
    }else{
        Test.For.Multiple.Timeframes <- (length(widget$request$comparisonItem[[2]]$time)!=1)&
            (length(unique(widget$request$comparisonItem[[2]]$time))!=1)&
            (!is.null(widget$request$comparisonItem[[2]]) )


        if(Test.For.Multiple.Timeframes){
            payload2$time <- widget$request$time[head(which(!is.na(widget$request$time)),1)]
            payload2$time <- gsub(" ","+",payload2$time)
            payload2$resolution <- widget$request$resolution[head(which(!is.na(widget$request$resolution)),1)]
            payload2$locale <- widget$request$locale[head(which(!is.na(widget$request$locale)),1)]
            payload2$comparisonItem <- widget$request$comparisonItem[[2]]
            payload2$comparisonItem$geo <- widget$request$comparisonItem[[2]]$geo
            payload2$requestOptions$property <- widget$request$requestOptions$property[2]
            payload2$requestOptions$backend <- widget$request$requestOptions$backend[2]
            payload2$requestOptions$category <- widget$request$requestOptions$category[2]
            token_payload2 <- widget$token[which(widget$id == "TIMESERIES")]


            url <- paste0(URLencode("https://trends.google.com/trends/api/widgetdata/multirange/csv?req="),
                          encode_payload(jsonlite::toJSON(payload2, auto_unbox = T,null="list"),reserved = TRUE),
                          URLencode(paste0("&token=", token_payload2,"&tz=",tz)))
            # url <- URLencode(paste0(
            #   "https://www.google.com/trends/api/widgetdata/multirange/csv?req=",
            #   jsonlite::toJSON(payload2, auto_unbox = T,null="list"),
            #   "&token=", token_payload2,
            #   "&tz=",tz
            # ))
        }else{
            if(!is.na(widget$request$locale[1])|(length(unique(unlist(widget$request$comparisonItem[[1]]$geo)))>1)){
                payload2$locale <- widget$request$locale[1]
                payload2$comparisonItem <- widget$request$comparisonItem[[1]]
                payload2$resolution <- widget$request$resolution[1]
                payload2$requestOptions$category <- widget$request$requestOptions$category[1]
                payload2$requestOptions$backend <- widget$request$requestOptions$backend[1]
                payload2$time <- widget$request$time[1]
                payload2$requestOptions$property <- widget$request$requestOptions$property[1]
                token_payload2 <- widget$token[1]
            } else {
                payload2$locale <- widget$request$locale[2]
                payload2$comparisonItem <- widget$request$comparisonItem[[1]]
                payload2$resolution <- widget$request$resolution[2]
                payload2$requestOptions$category <- widget$request$requestOptions$category[2]
                payload2$requestOptions$backend <- widget$request$requestOptions$backend[2]
                payload2$time <- widget$request$time[2]
                payload2$requestOptions$property <- widget$request$requestOptions$property[2]
                token_payload2 <- widget$token[2]
            }
            url <- paste0(URLencode("https://www.google.com/trends/api/widgetdata/multiline/csv?req="),
                          URLencode(jsonlite::toJSON(payload2, auto_unbox = T,null="list"),reserved = TRUE),
                          URLencode(paste0("&token=", token_payload2,"&tz=",tz)))
        }
    }

    # ****************************************************************************
    # Downoad the results
    # ****************************************************************************
    #url <- encode_keyword(url)

    # VY. use the handler with proxy options.
    res <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

    # Something went wrong
    if (res$status_code != 200) {
        stop("Status code was not 200. Returned status code:", res$status_code)
    }

    # ****************************************************************************
    # Format the results in a nice way
    # ****************************************************************************
    con <- textConnection(rawToChar(res$content))
    df <- read.csv(con, skip = 1, stringsAsFactors = FALSE)
    close(con)

    if (nrow(df) < 1) {
        return(NULL) ## No data
    }

    # Redo the substitution of + for a nice data frame
    comparison_item$keyword <- sapply(comparison_item$keyword,function(x) gsub("\\+"," ",x))
    comparison_item$keyword <- sapply(comparison_item$keyword,function(x) gsub("%2B","+",x))

    if(!onlyCategory){
        # This conditional statment is necessary when no keyword is supplied but
        # a search for a category is called for


        if((length(widget$request$comparisonItem[[2]]$time)==1)|
           (length(unique(widget$request$comparisonItem[[2]]$time))==1)|
           (is.null(widget$request$comparisonItem[[2]]) )
        ){
            n <- nrow(df) # used to reshape the data

            df <- reshape(
                df,
                varying = names(df)[2:ncol(df)],
                v.names = "hits",
                direction = "long",
                timevar = "temp",
                times = names(df)[2:ncol(df)]
            )

            df$temp <- NULL

            df <- cbind(
                df,
                comparison_item[rep(seq_len(nrow(comparison_item)), each = n), 1:3],
                row.names = NULL
            )

            df$geo <- ifelse(df$geo == "", "world", df$geo)
            df$gprop <- ifelse(widget$request$requestOptions$property[1] == "", "web", widget$request$requestOptions$property[1])
            df$category <- widget$request$requestOptions$category[1]
            names(df)[1] <- "date"
            df$id <- NULL

            # Format the returned date
            if(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",df$date))){
                df$date <- as.POSIXct(df$date,format="%Y-%m-%d",tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)),asUTC=T)
            }else if(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}$",df$date))){
                df$date <- as.POSIXct(df$date,format="%Y-%m-%dT%H",tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)),asUTC=T)
            }else if(all(grepl("^[0-9]{4}-[0-9]{2}$",df$date))){
                df$date <- df$date <- as.POSIXct(paste0(df$date,"-01"),
                                                 format = "%Y-%m-%d", tz = paste0("GMT", ifelse(tz >= 0, "+", "-"),
                                                                                  (abs(tz)/60)), asUTC = T)
            }else{
                df$date <- gsub("^([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}).*$","\\1",df$date)
                df$date <- as.POSIXct(df$date,format="%Y-%m-%dT%H:%M:%S",tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)),asUTC=T)
            }

        }else{
            n <- nrow(df) # used to reshape the data
            kw <- payload2$comparisonItem$complexKeywordsRestriction[[1]][[1]]$value
            kw <- gsub("[[:blank:]-]",".",kw)
            dates <- df[,which(!grepl(kw,names(df)))]

            if(all(sapply(lapply(dates,function(x) grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",x)),function(y) all(y)))){
                dates <- data.frame(lapply(dates,
                                           function(x) as.POSIXct(x,
                                                                  format="%Y-%m-%d",
                                                                  tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)))))
            }else if(all(sapply(lapply(dates,function(x) grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}$",x)),function(y) all(y)))){
                dates <- data.frame(lapply(dates,
                                           function(x) as.POSIXct(x,
                                                                  format="%Y-%m-%dT%H",
                                                                  tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)))))
            }else{
                dates <- data.frame(lapply(dates,
                                           function(x)
                                               gsub("^([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}).*$","\\1",x)))
                dates <- data.frame(lapply(dates,
                                           function(x) as.POSIXct(x,
                                                                  format="%Y-%m-%dT%H:%M:%S",
                                                                  tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)))))
            }

            # dates <- data.frame(lapply(dates,function(x) anytime::anytime(x,tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)),asUTC=T)))

            hits <- df[,which(grepl(kw,names(df)))]

            for(jj in 1:NCOL(dates)){
                df_tmp <- data.frame(dates[jj],hits[jj])
                df_tmp2 <- comparison_item[rep(jj,n), 1:3]

                df_tmp2[,1] <- ifelse(df_tmp2[,1] == "", "world", df_tmp2[,1])
                df_tmp2[,3] <- ifelse(widget$request$requestOptions$property[1] == "", "web", widget$request$requestOptions$property[1])
                df_tmp2[,4] <- widget$request$requestOptions$category[1]
                if(jj==1){
                    df_res <- cbind(df_tmp,df_tmp2)
                    names(df_res) <- c("date","hits","geo","time","gprop","category")
                }else{
                    df_tmp3 <- cbind(df_tmp,df_tmp2)
                    names(df_tmp3) <- c("date","hits","geo","time","gprop","category")
                    df_res <- rbind(df_res,df_tmp3)
                }
            }
            df <- df_res
        }
    }else{
        n <- nrow(df) # used to reshape the data
        names(df) <- c("date","hits")

        if(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",df$date))){
            df$date <- as.POSIXct(df$date,format="%Y-%m-%d",tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)),asUTC=T)
        }else if(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}$",df$date))){
            df$date <- as.POSIXct(df$date,format="%Y-%m-%dT%H",tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)),asUTC=T)
        }else if(all(grepl("^[0-9]{4}-[0-9]{2}$",df$date))){
            df$date <- df$date <- as.POSIXct(paste0(df$date,"-01"),
                                             format = "%Y-%m-%d", tz = paste0("GMT", ifelse(tz >= 0, "+", "-"),
                                                                              (abs(tz)/60)), asUTC = T)
        }


        comparison_item[,"geo"] <- ifelse(comparison_item[,"geo"] == "", "world", comparison_item[,"geo"])
        comparison_item[,"gprop"] <- ifelse(widget$request$requestOptions$property[1] ==
                                                "", "web", widget$request$requestOptions$property[1])
        comparison_item[,"category"] <- widget$request$requestOptions$category[1]
        df <- cbind(df,comparison_item[rep(1,n), 2:5])

    }

    return(df)
}


interest_by_region <- function(widget, comparison_item, low_search_volume,tz) {
    i <- which(grepl("Interest by", widget$title) == TRUE)

    if (length(i) == 0) {
        return(list(NULL))
    }

    ## Interest by region need to be retreived individually

    # resolution <- sub(".* (\\w+)$", "\\1", widget$title[i])
    # resolution[resolution == "subregion"] <- "region"
    # resolution[resolution == "metro"] <- "dma"

    # resolution <- c(resolution, rep(c("city", "dma"), each = length(resolution)))

    ##
    resolution <-
        expand.grid(i, c(ifelse(
            grepl("world", na.omit(widget$geo)), "country", "region"
        ), "city", "dma"), stringsAsFactors = FALSE)

    resolution <- unique(resolution)

    i <- resolution$Var1
    resolution <- resolution$Var2

    ## If it is not US metro, then also search for "city"
    # if (!all(grepl("dma", resolution))) {
    #   resolution <- c(resolution, rep("city", length(resolution)))
    # }
    #

    ## If no country is specified, resolution should be "COUNTRY"
    # resolution[grepl("world", na.omit(widget$geo))] <- "country"
    resolution <- toupper(resolution)

    res <-
        mapply(
            create_geo_payload,
            i,
            resolution,
            MoreArgs = list(widget = widget, low_search_volume = low_search_volume, tz = tz),
            SIMPLIFY = FALSE
        )

    ## Remove duplicated
    ii <- !duplicated(res)
    res <- res[ii]
    resolution <- resolution[ii]

    ## Remove NA
    ii <- !unlist(lapply(res, is.null))
    res <- res[ii]
    resolution <- resolution[ii]


    res <- setNames(res, tolower(resolution))

    return(res)
}


create_geo_payload <- function(i, widget, resolution, low_search_volume,tz) {
    payload2 <- list()
    payload2$locale <- unique(na.omit(widget$request$locale))
    payload2$comparisonItem <- widget$request$comparisonItem[[i]]
    payload2$resolution <- resolution
    payload2$requestOptions$backend <- widget$request$requestOptions$backend[i]
    payload2$requestOptions$property <- widget$request$requestOptions$property[i]
    payload2$requestOptions$category <- widget$request$requestOptions$category[i]
    payload2$geo <- as.list((widget$request$geo[i, , drop = FALSE]))
    payload2$includeLowSearchVolumeGeos <- low_search_volume


    url <- paste0(URLencode("https://www.google.com/trends/api/widgetdata/comparedgeo/csv?req="),
                  URLencode(jsonlite::toJSON(payload2, auto_unbox = T,null="list"),reserved = TRUE),
                  URLencode(paste0("&token=",widget$token[i],"&tz=",tz,"&hl=en-US")))

    # url <- URLencode(paste0(
    #   "https://www.google.com/trends/api/widgetdata/comparedgeo/csv?req=",
    #   jsonlite::toJSON(payload2, auto_unbox = T,null="list"),
    #   "&token=", widget$token[i],
    #   "&tz=",tz,"&hl=en-US"
    # ))

    # url <- encode_keyword(url)
    # VY. use the handler with proxy options.
    res <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

    if (res$status_code != 200) {
        return(NULL)
    }

    con <- textConnection(rawToChar(res$content))
    df <- read.csv(con, skip = 1, stringsAsFactors = FALSE)
    close(con)

    if (nrow(df) == 0) {
        return(NULL)
    }

    n <- nrow(df) # used to reshape the data

    df <- reshape(
        df,
        varying = names(df)[2:ncol(df)],
        v.names = "hits",
        direction = "long",
        timevar = "temp",
        times = names(df)[2:ncol(df)]
    )
    if(length(widget$request$comparisonItem[[i]]$complexKeywordsRestriction$operator)==0){
        kw <- do.call(rbind, widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword)
    }else{
        value <- paste(widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword[[1]]$value,collapse=" + ")
        type <- unique(widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword[[1]]$type)
        kw <- data.frame(type=type,value=value)
    }

    #kw <- do.call(rbind, widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword)

    df <- cbind(
        df,
        kw[rep(seq_len(nrow(kw)), each = n), 2],
        row.names = NULL,
        stringsAsFactors = FALSE
    )

    df$temp <- NULL
    # df$geo <- widget$geo[i]
    df$geo <- suppressWarnings(na.omit(unlist(widget$request$geo[i, ])))

    df$geo <- ifelse(is.null(df$geo), "world", df$geo)
    df$gprop <- ifelse(widget$request$requestOptions$property[i] == "", "web", widget$request$requestOptions$property[i])

    df$id <- NULL
    rownames(df) <- NULL

    names(df) <- c("location", "hits", "keyword", "geo", "gprop")

    return(df)
}

## Remove NA from list
na.omit.list <- function(y) {
    return(y[!sapply(y, function(x)
        all(is.na(x)))])
}

## Replace special characters in keywords like P&500 -> P%26500
encode_keyword <- function(url) {
    url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
    url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
    url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
    url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
    gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
}


map_tz2min <- function(timezone){
    round((unclass(as.POSIXct(format(Sys.time(),"%Y-%m-%d %H:%M:%S",tz="UTC")))-unclass(as.POSIXct(format(Sys.time(),"%Y-%m-%d %H:%M:%S",tz=timezone))))/60)
}

encode_payload <- function (URL, reserved = FALSE, repeated = FALSE){
    # This is a adjusted URLencode function.
    # The , and + are removed from the reserved characters list
    if (!repeated && grepl("%[[:xdigit:]]{2}", URL, useBytes = TRUE))
        return(URL)
    OK <- paste0("[^", if (!reserved)
        "][!'()*;=/?@#", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz0123456789._~,:%+-",
        "]")
    x <- strsplit(URL, "")[[1L]]
    z <- grep(OK, x)
    if (length(z)) {
        y <- vapply(x[z], function(x) paste0("%", toupper(as.character(charToRaw(x))),
                                             collapse = ""), "")
        x[z] <- y
    }
    paste(x, collapse = "")
}

related_topics <- function(widget, comparison_item, hl,tz) {
    i <- which(grepl("topics", widget$title) == TRUE)

    res <- lapply(i, create_related_topics_payload, widget = widget, hl = hl,tz=tz)
    res <- do.call(rbind, res)

    return(res)
}


create_related_topics_payload <- function(i, widget, hl, tz) {
    payload2 <- list()
    payload2$restriction$geo <-
        as.list(widget$request$restriction$geo[i, , drop = FALSE])
    payload2$restriction$time <- widget$request$restriction$time[[i]]
    payload2$restriction$originalTimeRangeForExploreUrl <-
        widget$request$restriction$originalTimeRangeForExploreUrl[[i]]
    payload2$restriction$complexKeywordsRestriction$keyword <-
        widget$request$restriction$complexKeywordsRestriction$keyword[[i]]
    payload2$restriction$complexKeywordsRestriction$operator <-
        widget$request$restriction$complexKeywordsRestriction$operator[[i]]
    payload2$keywordType <- widget$request$keywordType[[i]]
    payload2$metric <- widget$request$metric[[i]]
    payload2$trendinessSettings$compareTime <-
        widget$request$trendinessSettings$compareTime[[i]]
    payload2$requestOptions$property <-
        widget$request$requestOptions$property[[i]]
    payload2$requestOptions$backend <-
        widget$request$requestOptions$backend[[i]]
    payload2$requestOptions$category <-
        widget$request$requestOptions$category[[i]]
    payload2$language <- widget$request$language[[i]]
    payload2$userCountryCode <- widget$request$userCountryCode[[i]]

    url <- paste0(
        URLencode(
            "https://www.google.com/trends/api/widgetdata/relatedsearches/csv?req="
        ),
        URLencode(paste0(
            jsonlite::toJSON(payload2, auto_unbox = TRUE)
        ), reserved = TRUE),
        URLencode(paste0("&token=", widget$token[i])),
        URLencode(paste0("&tz=", tz, "&hl=", hl))
    )

    res <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

    # Something went wrong
    if (res$status_code != 200) {
        stop(
            "Status code was not 200. Returned status code:",
            res$status_code
        )
    }

    res <- readLines(textConnection(rawToChar(res$content)))

    start_top <- which(grepl("TOP", res))
    start_rising <- which(grepl("RISING", res))

    if (length(start_top) == 0 & length(start_rising) == 0) {
        return(NULL) ## No data returned
    }

    new_res <- NULL

    if (length(start_top) > 0) {
        end_top <- ifelse(length(start_rising) == 0, length(res), start_rising - 2)
        top <- read.csv(textConnection(res[start_top:end_top]), row.names = NULL)
        top$subject <- rownames(top)
        rownames(top) <- NULL
        top <- top[, c(2, 1)]
        names(top) <- c("subject", "top")

        top <- reshape(
            top,
            varying = "top",
            v.names = "value",
            direction = "long",
            timevar = "related_topics",
            times = "top"
        )

        new_res <- rbind(new_res, top)
    }

    if (length(start_rising) > 0) {
        rising <- read.csv(textConnection(res[start_rising:length(res)]), row.names = NULL)
        rising$subject <- rownames(rising)
        rownames(rising) <- NULL
        rising <- rising[, c(2, 1)]
        names(rising) <- c("subject", "rising")

        rising <- reshape(
            rising,
            varying = "rising",
            v.names = "value",
            direction = "long",
            timevar = "related_topics",
            times = "rising"
        )

        new_res <- rbind(new_res, rising)
    }

    res <- new_res
    res$id <- NULL
    res$geo <- unlist(payload2$restriction$geo, use.names = FALSE)

    if (length(widget$request$restriction$complexKeywordsRestriction$operator) !=
        0) {
        if (is.na(widget$request$restriction$complexKeywordsRestriction$operator[[i]])) {
            res$keyword <-
                widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value
        } else {
            res$keyword <-
                paste(
                    widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value,
                    collapse = "+"
                )
        }
    } else {
        res$keyword <-
            widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value
    }
    res$category <- payload2$requestOptions$category

    return(res)
}

extract_related_topics <- function(i, raw_data) {

    n <- length(raw_data)
    end <- i + min(which(raw_data[i:n] == "")) - 1

    df <- read.csv(textConnection(raw_data[i:end]), row.names = NULL)
    df$subject <- rownames(df)
    rownames(df) <- NULL
    df <- df[, c(2, 1)]
    names(df) <- c("subject", tolower(colnames(df)[1]))

    df <- reshape(
        df,
        varying = tolower(colnames(df)[2]),
        v.names = "value",
        direction = "long",
        timevar = "related_topics",
        times = tolower(colnames(df)[2])
    )

}

related_queries <- function(widget, comparison_item,tz,hl) {
    i <- which(grepl("queries", widget$title) == TRUE)

    res <- lapply(i, create_related_queries_payload, widget = widget,tz=tz, hl = hl)
    res <- do.call(rbind, res)

    return(res)
}


create_related_queries_payload <- function(i, widget,tz,hl) {
    payload2 <- list()
    payload2$restriction$geo <- as.list(widget$request$restriction$geo[i, , drop = FALSE])
    payload2$restriction$time <- widget$request$restriction$time[[i]]
    payload2$restriction$originalTimeRangeForExploreUrl <- widget$request$restriction$originalTimeRangeForExploreUrl[[i]]
    payload2$restriction$complexKeywordsRestriction$keyword <- widget$request$restriction$complexKeywordsRestriction$keyword[[i]]
    payload2$restriction$complexKeywordsRestriction$operator <- widget$request$restriction$complexKeywordsRestriction$operator[[i]]
    payload2$keywordType <- widget$request$keywordType[[i]]
    payload2$metric <- widget$request$metric[[i]]
    payload2$trendinessSettings$compareTime <- widget$request$trendinessSettings$compareTime[[i]]
    payload2$requestOptions$property <- widget$request$requestOptions$property[[i]]
    payload2$requestOptions$backend <- widget$request$requestOptions$backend[[i]]
    payload2$requestOptions$category <- widget$request$requestOptions$category[[i]]
    payload2$language <- widget$request$language[[i]]
    payload2$userCountryCode <- widget$request$userCountryCode[[i]]

    url <- paste0(URLencode("https://www.google.com/trends/api/widgetdata/relatedsearches/csv?req="),
                  URLencode(paste0(jsonlite::toJSON(payload2, auto_unbox = TRUE)),reserved=TRUE),
                  URLencode(paste0("&token=", widget$token[i])),
                  URLencode(paste0("&tz=",tz,"&hl=",hl))
    )

    #url <- encode_keyword(url)
    # VY. use the handler with proxy options.
    res <- curl::curl_fetch_memory(URLencode(url), handle = .pkgenv[["cookie_handler"]])

    # Something went wrong
    if (res$status_code != 200) {
        stop("Status code was not 200. Returned status code:", res$status_code)
    }

    res <- readLines(textConnection(rawToChar(res$content)))

    ## Not enough data
    ## https://trends.google.ca/trends/explore?cat=20&date=today%205-y,today%205-y&geo=CA,US&q=NHL,NFL
    if (length(res) <= 4) {
        return(NULL)
    }

    start_top <- which(grepl("TOP", res))
    start_rising <- which(grepl("RISING", res))

    if (length(start_top) == 0 | length(start_rising) == 0) {
        return(NULL) ## No data returned
    }

    top <- read.csv(textConnection(res[start_top:(start_rising - 2)]), row.names = NULL)
    top$subject <- rownames(top)
    rownames(top) <- NULL
    top <- top[, c(2, 1)]
    names(top) <- c("subject", "top")

    top <- reshape(
        top,
        varying = "top",
        v.names = "value",
        direction = "long",
        timevar = "related_queries",
        times = "top"
    )

    rising <- read.csv(textConnection(res[start_rising:length(res)]), row.names = NULL)
    rising$subject <- rownames(rising)
    rownames(rising) <- NULL
    rising <- rising[, c(2, 1)]
    names(rising) <- c("subject", "rising")

    rising <- reshape(
        rising,
        varying = "rising",
        v.names = "value",
        direction = "long",
        timevar = "related_queries",
        times = "rising"
    )

    res <- rbind(top, rising)
    res$id <- NULL
    res$geo <- unlist(payload2$restriction$geo, use.names = FALSE)
    if(length(widget$request$restriction$complexKeywordsRestriction$operator)!=0){
        if(is.na(widget$request$restriction$complexKeywordsRestriction$operator[[i]])){
            res$keyword <- widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value
        }else{
            res$keyword <- paste(widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value,collapse="+")
        }
    }else{
        res$keyword <- widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value
    }


    res$category <- payload2$requestOptions$category

    return(res)
}

#' Google Trends Query
#'
#' The \code{gtrends} default method performs a Google Trends query for the
#' \sQuote{query} argument and session \sQuote{session}. Optional arguments for
#' geolocation and category can also be supplied.
#'
#' @param keyword A character vector with the actual Google Trends query
#'   keywords. Multiple keywords are possible using \code{gtrends(c("NHL",
#'   "NBA", "MLB", "MLS"))}.
#'
#' @param geo A character vector denoting geographic regions for the query,
#'   default to \dQuote{all} for global queries. Multiple regions are possible
#'   using \code{gtrends("NHL", c("CA", "US"))}.
#'
#' @param time A string specifying the time span of the query. Possible values
#'   are:
#'
#'   \describe{ \item{"now 1-H"}{Last hour} \item{"now 4-H"}{Last four hours}
#'   \item{"now 1-d"}{Last day} \item{"now 7-d"}{Last seven days} \item{"today
#'   1-m"}{Past 30 days} \item{"today 3-m"}{Past 90 days} \item{"today
#'   12-m"}{Past 12 months} \item{"today+5-y"}{Last five years (default)}
#'   \item{"all"}{Since the beginning of Google Trends (2004)} \item{"Y-m-d
#'   Y-m-d"}{Time span between two dates (ex.: "2010-01-01 2010-04-03")} }
#'
#' @param category A character denoting the category, defaults to \dQuote{0}.
#'
#' @param gprop A character string defining the Google product for which the
#'   trend query if preformed. Valid options are:
#'
#'   \itemize{ \item "web" (default) \item "news" \item "images" \item "froogle"
#'   \item "youtube" }
#'
#' @param hl A string specifying the ISO language code (ex.: \dQuote{en-US} or
#'   \dQuote{fr}). Default is \dQuote{en-US}. Note that this is only influencing
#'   the data returned by related topics.
#'
#' @param tz A number specifying the minutes the returned dates should be offset to UTC.
#' Note the parameter 'time' above is specified in UTC.
#' E.g. choosing "time=2018-01-01T01 2018-01-01T03" and "tz=-120" will yield data between 2018-01-01T03 and 2018-01-01T05,
#' i.e. data specified to be in UTC+2.
#'
#' @param low_search_volume Logical. Should include low search volume regions?
#'
#' @param cookie_url A string specifying the URL from which to obtain cookies.
#'   Default should work in general; should only be changed by advanced users.
#'
#' @param onlyInterest If you only want the interest over time set it to TRUE.
#'
#' @section Categories: The package includes a complete list of categories that
#'   can be used to narrow requests. These can be accessed using
#'   \code{data("categories")}.
#'
#' @section Related topics: Note that *related topics* are not retrieved when
#'   more than one keyword is provided due to Google restriction.
#'
#' @importFrom stats na.omit reshape setNames
#' @importFrom utils URLencode read.csv head
#'
#' @return An object of class \sQuote{gtrends} (basically a list of data
#'   frames).
#'
#' @examples
#'
#' \dontrun{
#'
#' head(gtrends("NHL")$interest_over_time)
#' head(gtrends("NHL")$related_topics)
#' head(gtrends("NHL")$related_queries)
#'
#' head(gtrends(c("NHL", "NFL"))$interest_over_time)
#'
#' head(gtrends(c("NHL", "NFL"), geo = c("CA", "US"))$interest_over_time)
#'
#' ## Interest by city
#'
#' gtrends(keyword="obama",geo="US-AL-630")
#'
#' ## Sport category (20)
#' data(categories)
#' categories[grepl("^Sport", categories$name), ]
#' gtrends(c("NHL", "NFL"), geo = c("CA", "US"), category = 20)
#' gtrends(geo = c("CA"), category = 20)
#'
#' ## Playing with time format
#'
#' gtrends(c("NHL", "NFL"), time = "now 1-H") # last hour
#' gtrends(c("NHL", "NFL"), time = "now 4-H") # last four hours
#' gtrends(c("NHL", "NFL"), time = "now 1-d") # last day
#' gtrends(c("NHL", "NFL"), time = "today 1-m") # last 30 days
#' gtrends(c("NHL", "NFL"), time = "today 3-m") # last 90 days
#' gtrends(c("NHL", "NFL"), time = "today 12-m") # last 12 months
#' gtrends(c("NHL", "NFL"), time = "today+5-y") # last five years (default)
#' gtrends(c("NHL", "NFL"), time = "all") # since 2004
#'
#'
#' ## Custom date format
#'
#' gtrends(c("NHL", "NFL"), time = "2010-01-01 2010-04-03")
#'
#' ## Search from various Google's services
#'
#' head(gtrends(c("NHL", "NFL"), gprop = "news")$interest_over_time)
#' head(gtrends(c("NHL", "NFL"), gprop = "youtube")$interest_over_time)
#'
#' ## Language settings
#'
#' head(gtrends("NHL", hl = "en")$related_topics)
#' head(gtrends("NHL", hl = "fr")$related_topics)
#' }
#' @export
gtrends <- function(
    keyword = NA,
    geo = "",
    time = "today+5-y",
    gprop = c("web", "news", "images", "froogle", "youtube"),
    category = 0,
    hl = "en-US",
    low_search_volume = FALSE,
    cookie_url = "http://trends.google.com/Cookies/NID",
    tz=0, # This equals UTC
    onlyInterest=FALSE
) {
    stopifnot(
        # One  vector should be a multiple of the other
        (length(keyword) %% length(geo) == 0) || (length(geo) %% length(keyword) == 0) || (length(time) %% length(keyword) == 0),
        is.vector(keyword),
        length(keyword) <= 5,
        length(geo) <= 5,
        length(time) <= 5,
        length(hl) == 1,
        is.character(hl),
        length(cookie_url) == 1,
        is.character(cookie_url)
    )

    ## Check if valide category
    if (!all(category %in% categories[, "id"])) {
        stop(
            "Category code not valid. Please use 'data(categories)' to retreive valid codes.",
            call. = FALSE
        )
    }

    ## Check if time format is ok
    if (!check_time(time)) {
        stop("Cannot parse the supplied time format.", call. = FALSE)
    }

    if(!(is.numeric(tz))){
        if (tz %in% OlsonNames()){
            tz <- map_tz2min(tz)
        }else{
            stop("Given timezone not known. Check function OlsonNames().", call. = FALSE)
        }
    }

    # time <- "today+5-y"
    # time <- "2017-02-09 2017-02-18"
    # time <- "now 7-d"
    # time <- "all_2006"
    # time <- "all"
    # time <- "now 4-H"
    # geo <- c("CA", "FR", "US")
    # geo <- c("CA", "DK", "FR", "US", "CA")
    # geo <- "US"

    gprop <- match.arg(gprop, several.ok = FALSE)
    gprop <- ifelse(gprop == "web", "", gprop)

    # ****************************************************************************
    # Request a token from Google
    # ****************************************************************************
    keyword <- sapply(keyword,function(x){
        y <- gsub("[+]","%2B",x)
        z <- gsub(" ","+",y)
        return(z)
    })
    names(keyword) <- NULL
    comparison_item <- data.frame(keyword,geo,time, stringsAsFactors = FALSE)

    widget <- get_widget(comparison_item, category, gprop, hl, cookie_url,tz)

    # ****************************************************************************
    # Now that we have tokens, we can process the queries
    # ****************************************************************************

    interest_over_time <- interest_over_time(widget, comparison_item,tz)

    if(!onlyInterest){
        interest_by_region <- interest_by_region(widget, comparison_item, low_search_volume,tz)
        related_topics <- related_topics(widget, comparison_item, hl,tz)
        related_queries <- related_queries(widget, comparison_item,tz,hl)
        res <- list(
            interest_over_time = interest_over_time,
            interest_by_country = do.call(rbind, interest_by_region[names(interest_by_region) == "country"]),
            interest_by_region = do.call(rbind, interest_by_region[names(interest_by_region) == "region"]),
            interest_by_dma = do.call(rbind, interest_by_region[names(interest_by_region) == "dma"]),
            interest_by_city = do.call(rbind, interest_by_region[names(interest_by_region) == "city"]),
            related_topics = related_topics,
            related_queries = related_queries
        )
    }else{
        res <- list(interest_over_time = interest_over_time)
    }

    ## Remove row.names
    res <- lapply(res, function(x) {
        row.names(x) <- NULL
        x
    })

    class(res) <- c("gtrends", "list")

    return(res)
}

#' Plot Google Trends interest over time
#'
#' @param x A \code{\link{gtrends}} object.
#' @param ... Additional parameters passed on in method dispatch. Currently not
#'   used.
#'
#' @import ggplot2
#'
#' @return A ggplot2 object is returned silently.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- gtrends("nhl", geo = c("CA", "US"))
#' plot(res)
#' }
plot.gtrends <- function(x, ...) {
    df <- x$interest_over_time
    df$hits <- if(typeof(df$hits) == 'character'){
        as.numeric(gsub('<','',df$hits))
    } else {
        df$hits
    }

    df$legend <- paste(df$keyword, " (", df$geo, ")", sep = "")

    p <- ggplot(df, aes_string(x = "date", y = "hits", color = "legend")) +
        geom_line() +
        xlab("Date") +
        ylab("Search hits") +
        ggtitle("Interest over time") +
        theme_bw() +
        theme(legend.title = element_blank())

    print(p)
    invisible(p)
}