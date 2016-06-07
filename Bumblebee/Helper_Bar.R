is.installed <- function(mypkg) is.element(mypkg, installed.packages()[, 1])
reference <- function(name) {
    print(name)
    if (!is.installed(name)) {

        install.packages(name, repos = "http://R-Forge.R-project.org")

    }
    require(name, character.only = TRUE)
    print(name)
}

options(scipen = 999)
options('max.print' = 30)
reference('doParallel')
reference('zoo')
reference('ggplot2')
reference('quantstrat')
reference("plyr")
reference("jsonlite")
reference("data.table")
getDate <- function(dateInt = NA, year = NA, month = NA, day = NA) {
    if (is.integer(dateInt) && dateInt > 19700000) {
        year <- as.integer(dateInt / 10000)
        month <- as.integer((dateInt - year * 10000) / 100)
        day <- dateInt - month * 100 - year * 10000
    }
    return(as.Date(paste(year, month, day, sep = "-")))
}
loadProducts <- function(filePath = 'D:\\nautilusdata\\InstrumentInfo.csv') {
    tryCatch({
        products <- read.csv(filePath, header = T)
        colnames(products)[2] <- 'ProductID'
        colnames(products)[3] <- 'Exchange'

        products$VolumeMultiper <- products$TickValue / products$TickStep
        products$Exchange
        products.Globle <<- subset(products, select = -(1))
    }, error = function(e) {
        print(paste(filePath, "load failed", e))
    })
}
getContractByID <- function(Contract, filePath = 'D:\\nautilusdata\\InstrumentInfo.csv') {

    if (!exists('products.Globle') || is.null(products.Globle)) {
        print('Load products')
        loadProducts(filePath);
    }

    product <- getProductID(Contract)
    c <- products.Globle[products.Globle$ProductID == product,]
    return(cbind(Contract, c))

}

getProductID <- function(contractID) {

    cs <- strsplit(contractID, "")[[1]]
    x <- suppressWarnings(as.numeric(cs))
    return(trimws(paste(cs[is.na(x)], collapse = '')))

}

repeat.before = function(x) {
    # repeats the last non NA value. Keeps leading NA
    ind = which(!is.na(x)) # get positions of nonmissing values
    if (is.na(x[1]))
        # if it begins with a missing, add the 
            ind = c(1, ind) # first position to the indices
    rep(x[ind], times = diff(# repeat the values at these indices
    c(ind, length(x) + 1))) # diffing the indices + length yields how often 
}

genSpread <- function(merged, hedgeRatio = 1) {
    merged$bb1.spread <- merged$bb1.sub - hedgeRatio * merged$bb1.main
    merged$bbz1.spread <- min(merged$bbz1.sub, merged$bbz1.main / hedgeRatio)

    merged$bb2.spread <- merged$bb1.sub - hedgeRatio * merged$ba1.main
    merged$bbz2.spread <- min(merged$bbz1.sub, merged$baz1.main / hedgeRatio)


    merged$ba1.spread <- merged$ba1.sub - hedgeRatio * merged$ba1.main
    merged$baz1.spread <- min(merged$baz1.sub, merged$baz1.main / hedgeRatio)

    merged$ba2.spread <- merged$ba1.sub - hedgeRatio * merged$bb1.main
    merged$baz2.spread <- min(merged$baz1.sub, merged$bbz1.main / hedgeRatio)

    merged$mid.spread <- (merged$bb1.spread + merged$ba1.spread) * 0.5
    return(merged)
}

mergeMainSub <- function(mainTicks, subTicks) {
    subTicks$FeedTime <- subTicks$utcQuoteTime
    mainTicks$FeedTime <- mainTicks$utcQuoteTime
    merged <- merge(subTicks, mainTicks, by = c('date', 'utcQuoteTime'), suffixes = c(".sub", ".main"), all = T)
    #  merged$utcReceiveTime.sub <- repeat.before(merged$utcReceiveTime.sub)
    merged$bb1.sub <- repeat.before(merged$bb1.sub)
    merged$ba1.sub <- repeat.before(merged$ba1.sub)
    # merged$bb2.sub <- repeat.before(merged$bb2.sub)
    # merged$ba2.sub <- repeat.before(merged$ba2.sub)
    merged$bbz1.sub <- repeat.before(merged$bbz1.sub)
    #merged$bbz2.sub <- repeat.before(merged$bbz2.sub)
    #merged$baz2.sub <- repeat.before(merged$baz2.sub)
    merged$baz1.sub <- repeat.before(merged$baz1.sub)
    merged$mid.sub <- (merged$bb1.sub + merged$ba1.sub) * 0.5
    merged$FeedTime.sub <- repeat.before(merged$FeedTime.sub)

    # merged$utcReceiveTime.main <- repeat.before(merged$utcReceiveTime.main)
    merged$bb1.main <- repeat.before(merged$bb1.main)
    merged$ba1.main <- repeat.before(merged$ba1.main)
    #merged$bb2.main <- repeat.before(merged$bb2.main)
    #merged$ba2.main <- repeat.before(merged$ba2.main)
    merged$bbz1.main <- repeat.before(merged$bbz1.main)
    #merged$bbz2.main <- repeat.before(merged$bbz2.main)
    #merged$baz2.main <- repeat.before(merged$baz2.main)
    merged$baz1.main <- repeat.before(merged$baz1.main)
    merged$mid.main <- (merged$bb1.main + merged$ba1.main) * 0.5
    merged$FeedTime.main <- repeat.before(merged$FeedTime.main)

    return(merged)

}

createFolder <- function(folder) {
    if (!dir.exists(folder)) {
        print(paste('Create', folder))
        dir.create(folder)
    }
}


get1LevelTicks <- function(contractId = 'AU1606', exchangeID = 'SHFE', date = 20160310, session = 'D', tickSource = '\\\\172.30.50.120\\nautilusdata\\') {
    sessionMap <- fromJSON(paste(tickSource, "utcTradingTime\\", exchangeID, '.json', sep = ''))
    productID <- getProductID(contractId)
    sessions <- sessionMap[productID]
    tickSource <- paste(tickSource, exchangeID, sep = '')
    tickSource <- paste(tickSource, contractId, contractId, sep = '\\')
    tickSource <- paste(tickSource, date, session, sep = '-')
    file <- paste(tickSource, 'tick', sep = '.')
    tryCatch({
        a <- read.csv(file, header = T)

        a$mid <- (a$bb1 + a$ba1) / 2
        if (date < 20160215) {
            print(paste("date <20160215 ", date))
            return(NULL)
        }
        a <- a[a$utcQuoteTime > 0,]
        #a<-a[(a$utcQuoteTime %% 100 = 0),]
        a <- a[a$utcQuoteTime %% 100 == 0,]

        x1 <- a$utcQuoteTime >= sessions[[productID]]$Sessions[1, 1]
        x2 <- a$utcQuoteTime <= sessions[[productID]]$Sessions[1, 2]
        x <- x1 & x2
        for (i in 2:sessions[[productID]]$SessionCount) {
            y1 <- a$utcQuoteTime >= sessions[[productID]]$Sessions[i, 1]
            y2 <- a$utcQuoteTime <= sessions[[productID]]$Sessions[i, 2]
            y <- y1 & y2
            x <- x | y
            }
            a <- a[x,]
            a$date <- date
            return(subset(a, select = c('date', 'utcQuoteTime', 'mid', 'bb1', 'ba1', 'bbz1', 'baz1')))
    }, error = function(e) {
        print(paste(file, "load failed", e))
        return(NULL)
    })

}

get10LevelTicks <- function(contractId = 'NI1606', exchangeID = 'SHFE_10', date = 20160328, session = 'D', tickSource = '\\\\172.30.50.120\\nautilusdata\\') {

    tickSource <- paste(tickSource, exchangeID, sep = '')
    tickSource <- paste(tickSource, contractId, contractId, sep = '\\')
    tickSource <- paste(tickSource, date, session, sep = '-')
    file <- paste(tickSource, 'tick', sep = '.')
    tryCatch({
        a <- read.csv(file, header = T)

        a$mid <- (a$bb1 + a$ba1) / 2
        if (date < 20160315) {
            a <- a[a$utcTime > 0,]
            #a<-a[(a$utcQuoteTime %% 100 = 0),]
            a <- a[a$utcTime %% 100 == 0,]
            #utcTime,lastPrice,volume, volumeAcc,openInterest,bbz1,bb1,ba1,baz1,bbz10,bbz9,bbz8,bbz7,bbz6,bbz5,bbz4,bbz3,bbz2,bbz1,bb10,bb9,bb8,bb7,bb6,bb5,bb4,bb3,bb2,bb1,ba1,ba2,ba3,ba4,ba5,ba6,ba7,ba8,ba9,ba10,baz1,baz2,baz3,baz4,baz5,baz6,baz7,baz8,baz9,baz10,HM
            re <- subset(a, select = c('utcTime', 'mid', 'bb1', 'ba1', 'bb2', 'ba2', 'bbz1', 'bbz2', 'baz1', 'baz2'))
            re$utcQuoteTime <- re$utcTime
            return(re)
        }
        a <- a[a$utcQuoteTime > 0,]
        #a<-a[(a$utcQuoteTime %% 100 = 0),]
        a <- a[a$utcQuoteTime %% 100 == 0,]
        return(subset(a, select = c('utcQuoteTime', 'mid', 'bb1', 'ba1', 'bb2', 'ba2', 'bbz1', 'bbz2', 'baz1', 'baz2')))
    }, error = function(e) {
        print(paste(file, "load failed", e))
        return(NULL)
    })
}


ticksToBars <- function(ticks, samplePeriod = 60) {
    startTime <- roundDown(ticks$utcQuoteTime[1], 1000)
    endTime <- roundDown(tail(ticks$utcQuoteTime, 1), 1000)
    breaks <- c(seq.int(from = startTime, to = endTime, by = samplePeriod * 1000))
    ticks <- data.table(ticks)
    bars <- ticks[, list(Date = head(date, 1), Start = head(utcQuoteTime, 1), End = last(utcQuoteTime), Open = head(mid.spread, 1), High = max(mid.spread), Low = min(mid.spread), Close = last(mid.spread)), by = list(Range = cut(utcQuoteTime, breaks = breaks, dig.lab = 10))]
    return(bars)
}


createSig <- function(merged, maPeriod, sdAmplifier, maType = 'SMA') {
    bars <- ticksToBars(merged, samplePeriod = 60)
    bars <- cbind(bars, BBands(bars$Close, n = maPeriod, maType = maType, sd = sdAmplifier))
    merged['dn'] <- NA
    merged['mavg'] <- NA
    merged['up'] <- NA
    merged['pctB'] <- NA
    j = 1
    for (i in 1:nrow(merged)) {
        if (j == 1 & merged$utcQuoteTime[i] <= bars$End[j]) {
            merged$dn[i] <- NA
            merged$mavg[i] <- NA
            merged$up[i] <- NA
            merged$pctB[i] <- NA
        } else if (j != 1 & merged$utcQuoteTime[i] <= bars$End[j]) {
            merged$dn[i] <- bars$dn[j - 1]
            merged$mavg[i] <- bars$mavg[j - 1]
            merged$up[i] <- bars$up[j - 1]
            merged$pctB[i] <- bars$pctB[j - 1]
        } else if (merged$utcQuoteTime[i] > bars$End[j]) {
            j = j + 1
            merged$dn[i] <- bars$dn[j - 1]
            merged$mavg[i] <- bars$mavg[j - 1]
            merged$up[i] <- bars$up[j - 1]
            merged$pctB[i] <- bars$pctB[j - 1]
        }
    }
    #merged <- cbind(merged, BBands((merged$ba2.spread + merged$bb2.spread) / 2, n = maPeriod, maType = maType, sd = sdAmplifier))

    merged$LongSpreadSig <- ifelse(merged$ba2.spread <= merged$dn, merged$ba2.spread, 0)
    merged$ShortSpreadSig <- ifelse(merged$bb2.spread >= merged$up, merged$bb2.spread, 0)
    merged$LongSpreadSig <- ifelse(is.na(merged$LongSpreadSig), 0, merged$LongSpreadSig)
    merged$ShortSpreadSig <- ifelse(is.na(merged$ShortSpreadSig), 0, merged$ShortSpreadSig)
    return(merged)
}

tradeSpread <- function(merged, main, sub, orderSize = 1, hedgeRatio = 1, positionLimit = 10, distortTk = 1, distortPower = 1, tradeCoolDown = 30000, timeDiff = 20000, fee = 6, slippage = 1.5, subTrigger = T) {
    slippage <- slippage * as.double(sub$TickStep)
    Act <- rep.int(0, nrow(merged))
    lastTradeTime <- 0
    BuyTrigger <- rep.int(0, nrow(merged))
    SellTrigger <- rep.int(0, nrow(merged))
    DistortValue <- rep.int(0, nrow(merged))
    distortStep <- distortTk * as.double(sub$TickStep)
   # print(sub$TickStep)
    position <- 0
    #merged$Act <- ifelse(merged$ba2.spread <= merged$dn, 1, ifelse(merged$bb2.spread >= merged$up, -1),0))
    for (i in 1:nrow(merged)) {
        r <- merged[i,]
        if (is.na(r$mavg)) {
            next
        }
        if (r$utcQuoteTime - lastTradeTime < tradeCoolDown) {
            next
        }
        if (abs(r$FeedTime.main - r$FeedTime.sub) > timeDiff) {
            next
        }
        if (subTrigger && r$utcQuoteTime != r$FeedTime.sub) {
            next
        }
        if (!subTrigger && r$utcQuoteTime != r$FeedTime.main) {
            next
        }
        #if (i > nrow(merged) - 360) {
            #if (position != 0) {
                #print('End')
                #if (position > 0) {
                    #Act[i] <- -position
                    #position <- 0
                #} else {

                    #Act[i] <- -position
                    #position <- 0
                #}
            #}
            #next

        #}
        DistortValue[i] <- abs(position) * distortStep / orderSize
        if (position <= positionLimit) {
            if (position < 0) {

                BuyTrigger[i] <- ifelse(position > 0, r$dn - DistortValue[i], r$dn)
                #BuyTrigger[i]<-r$mavg
            } else {
                BuyTrigger[i] <- ifelse(position > 0, r$dn - DistortValue[i], r$dn)
            }

            if (r$ba2.spread <= BuyTrigger[i]) {
                Act[i] <- orderSize
                lastTradeTime <- r$utcQuoteTime
                position <- position + orderSize
            }
        }

        if (position >= -positionLimit) {
            if (position > 0) {
                SellTrigger[i] <- ifelse(position < 0, r$up + DistortValue[i], r$up)
                #  SellTrigger[i] <- r$mavg
            } else {
                SellTrigger[i] <- ifelse(position < 0, r$up + DistortValue[i], r$up)
            }

            if (r$bb2.spread >= SellTrigger[i]) {
                Act[i] <- -orderSize
                lastTradeTime <- r$utcQuoteTime
                position <- position - orderSize
            }
        }

    }
    merged$Act <- Act
    merged$Cashflow <- ifelse(merged$Act > 0,
                          ( - (merged$ba1.sub + slippage) * as.double(sub$VolumeMultiper) + hedgeRatio * merged$bb1.main * as.double(main$VolumeMultiper)) * merged$Act,
                          ifelse(merged$Act < 0,
                                 ((merged$bb1.sub - slippage) * as.double(sub$VolumeMultiper) - hedgeRatio * merged$ba1.main * as.double(main$VolumeMultiper)) * -merged$Act, 0))


    merged$NetPosi <- cumsum(merged$Act)

    merged$FloatingPL <- ifelse(merged$NetPosi < 0,
                             - merged$NetPosi * ( - merged$ba1.sub * as.double(sub$VolumeMultiper) + hedgeRatio * merged$bb1.main * as.double(main$VolumeMultiper)),
                             ifelse(merged$NetPosi > 0,
                                    merged$NetPosi * (merged$bb1.sub * as.double(sub$VolumeMultiper) - hedgeRatio * merged$ba1.main * as.double(main$VolumeMultiper)), 0))

    merged$Fee <- 2 * abs(fee * merged$Act)

    merged$Net <- cumsum(merged$Cashflow) + merged$FloatingPL - cumsum(merged$Fee)
    return(merged)

}



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like ,
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                             ncol = cols, nrow = ceiling(numPlots / cols))
    }

    if (numPlots == 1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                               layout.pos.col = matchidx$col))
        }
    }
}

plotReturn <- function(bars, path, lblCol = 'utcQuoteTime', width = 1920, height = 1080, save = F) {
    names(bars)[names(bars) == lblCol] <- "ID"
    step = as.integer(nrow(bars) / 100)
    breaks <- index(bars)[index(bars$ID) %% step == 0]
    lbl <- bars$ID[index(bars$ID) %% step == 0]

    p <- ggplot() + geom_line(data = bars, aes(x = seq_along(along.with = ID),
            y = Net), colour = "green") +
                scale_x_discrete('ID', breaks = breaks, labels = lbl) +
                theme(axis.text.x = element_text(angle = -90),
                      panel.background = element_rect(fill = "black"),
                      panel.grid.major = element_line(colour = "grey40"),
                      panel.grid.minor = element_blank())

    q <- ggplot() + geom_line(data = bars, aes(x = seq_along(along.with = ID), y = NetPosi), colour = "green") +
                 scale_x_discrete('ID', breaks = breaks, labels = lbl) +
                 theme(axis.text.x = element_text(angle = -90),
                       panel.background = element_rect(fill = "black"),
                       panel.grid.major = element_line(colour = "grey40"),
                       panel.grid.minor = element_blank())

    if (save) {
        png(filename = path, width = width, height = height, units = "px", pointsize = 12, bg = "transparent")
        multiplot(p, q, cols = 1)
        dev.off()
    }

    return(list(p, q))
}
plotLines <- function(data, lblCol, valueCols,
        isSave = F, path = "C:\\Users\\victor\\Documents\\Tmp\\", fileName = 'pic', width = 1024, height = 800) {

    names(srcData)[names(srcData) == lblCol] <- "Lbl"
    srcData$ID <- seq_along(along.with = srcData$Lbl)
    plotData <- srcData[, c("ID", valueCols)]

    plotData <- melt(plotData, id = "ID")

    step = as.integer(nrow(srcData) / 100)
    breaks <- index(srcData)[index(srcData$ID) %% step == 0]
    lbl <- srcData$Lbl[index(srcData$Lbl) %% step == 0]
    p <- ggplot(data = plotData, aes(x = ID, color = variable, y = value, group = variable)) +
        geom_line() + scale_x_continuous('ID', breaks = breaks, labels = lbl) +
        theme(text = element_text(size = 20), axis.text.x = element_text(angle = -90),
        # panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(colour = "grey40"),
         panel.grid.minor = element_blank())
    file <- paste(path, fileName, ".png", sep = "")
    print(file)
    if (isSave) {
        tryCatch({
            png(filename = file,
                    width = width, height = height, units = "px", pointsize = 5,
                     bg = "transparent")
            p
            dev.off()

        }, error = function(e) {
            dev.off()
            print(paste(filePath, "save png failed", e))
        })
    }

    return(p)
}


plotSpread <- function(bars, path, lblCol = 'utcQuoteTime', closeCol = 'mid.spread', plotSignal = T, width = 1920, height = 1080, save = F) {

    names(bars)[names(bars) == lblCol] <- "ID"
    names(bars)[names(bars) == closeCol] <- "price"

    step = as.integer(nrow(bars) / 100)
    breaks <- index(bars)[index(bars$ID) %% step == 0]
    lbl <- bars$ID[index(bars$ID) %% step == 0]

    bars$ShortSpreadSig <- ifelse(bars$ShortSpreadSig == 0, NA, bars$ShortSpreadSig)
    bars$LongSpreadSig <- ifelse(bars$LongSpreadSig == 0, NA, bars$LongSpreadSig)

    p <- ggplot(data = bars, aes(x = seq_along(along.with = ID))) +
                geom_line(data = bars, aes(y = price), colour = "blue") +
                geom_line(data = bars, aes(y = dn), colour = "green") +
                geom_line(data = bars, aes(y = up), colour = "red") +
                geom_line(data = bars, aes(y = mavg), colour = "black") +
                geom_point(data = bars, aes(y = ShortSpreadSig), colour = "gold", size = 2) +
                geom_point(data = bars, aes(y = LongSpreadSig), colour = "red", size = 2) +
                scale_x_discrete('ID', breaks = breaks, labels = lbl) +
                theme(text = element_text(size = 20), axis.text.x = element_text(angle = -90),
                      panel.background = element_rect(fill = "black"),
                      panel.grid.major = element_line(colour = "grey40"),
                      panel.grid.minor = element_blank())




    bars$ShortSpreadSig <- ifelse(bars$Act < 0, 1, 0) * bars$ShortSpreadSig

    bars$LongSpreadSig <- ifelse(bars$Act > 0, 1, 0) * bars$LongSpreadSig
    bars$ShortSpreadSig <- ifelse(bars$ShortSpreadSig == 0, NA, bars$ShortSpreadSig)
    bars$LongSpreadSig <- ifelse(bars$LongSpreadSig == 0, NA, bars$LongSpreadSig)

    q <- ggplot(data = bars, aes(x = seq_along(along.with = ID))) +
                geom_line(data = bars, aes(y = price), colour = "blue") +
                geom_line(data = bars, aes(y = dn), colour = "green") +
                geom_line(data = bars, aes(y = up), colour = "red") +
                geom_line(data = bars, aes(y = mavg), colour = "black") +
                geom_point(data = bars, aes(y = ShortSpreadSig), colour = "gold", size = 2) +
                geom_point(data = bars, aes(y = LongSpreadSig), colour = "red", size = 2) +
                scale_x_discrete('ID', breaks = breaks, labels = lbl) +
                theme(text = element_text(size = 20), axis.text.x = element_text(angle = -90),
                      panel.background = element_rect(fill = "black"),
                      panel.grid.major = element_line(colour = "grey40"),
                      panel.grid.minor = element_blank())

    if (save) {
        png(filename = path, width = width, height = height, units = "px", pointsize = 12, bg = "transparent")
        multiplot(p, q, cols = 1)
        dev.off()
    }

    return(list(p, q))

}

plotLines <- function(data, lblCol, valueCols,
            isSave = F, path = "C:\\Users\\victor\\Documents\\Tmp\\", fileName = 'pic', width = 1024, height = 800) {

    names(srcData)[names(srcData) == lblCol] <- "Lbl"
    srcData$ID <- seq_along(along.with = srcData$Lbl)
    plotData <- srcData[, c("ID", valueCols)]

    plotData <- melt(plotData, id = "ID")

    step = as.integer(nrow(srcData) / 100)
    breaks <- index(srcData)[index(srcData$ID) %% step == 0]
    lbl <- srcData$Lbl[index(srcData$Lbl) %% step == 0]
    p <- ggplot(data = plotData, aes(x = ID, color = variable, y = value, group = variable)) +
            geom_line() + scale_x_continuous('ID', breaks = breaks, labels = lbl) +
            theme(text = element_text(size = 20), axis.text.x = element_text(angle = -90),
            # panel.background = element_rect(fill = "black"),
            panel.grid.major = element_line(colour = "grey40"),
             panel.grid.minor = element_blank())

    if (isSave) {
        file <- paste(path, fileName, ".png", sep = "")
        print(file)
        tryCatch({
            png(filename = file,
                            width = width, height = height, units = "px", pointsize = 5,
                             bg = "transparent")
            print(p)
            dev.off()

        }, error = function(e) {
            dev.off()
            print(paste(filePath, "save png failed", e))
        })
    }

    return(p)
}

roundUp <- function(value, tickUnit) {
    return(ceiling(value / tickUnit) * tickUnit)

}
roundDown <- function(value, tickUnit) {
    return(floor(value / tickUnit) * tickUnit)

}