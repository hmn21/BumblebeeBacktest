source('Helper_interday.R')
coreNum <- 4
allBars <<- NULL
allData <<- NULL
runBackTest <- function(mainContract, subContract, param, tickSource, outputFolder, maType = 'SMA', isDraw = F) {
    subContracID <- as.character(subContract$Contract)
    subExchange <- as.character(subContract$Exchange)
    mainContracID <- as.character(mainContract$Contract)
    mainExchange <- as.character(mainContract$Exchange)
    orderSize = as.numeric(param['orderSize'])
    positionLimit = as.numeric(param['positionLimit'])
    distortTk = as.numeric(param['distortTk'])
    tradeCoolDown = as.numeric(param['tradeCoolDown'])
    fee = as.numeric(param['fee'])
    slippageTick = as.numeric(param['slippageTick'])
    subTrigger = as.logical(param['subTrigger'])
    sdMax <- as.numeric(param['sdMax'])
    sdMin <- as.numeric(param['sdMin'])
    sdStep <- as.numeric(param['sdStep'])
    maMax <- as.numeric(param['maMax'])
    maMin <- as.numeric(param['maMin'])
    maStep <- as.numeric(param['maStep'])

    startDate <- getDate(as.integer(param['startDate']))

    endDate <- getDate(as.integer(param['endDate']))
    summary <- NULL
    for (date in startDate:endDate) {


        date <- as.Date(date, origin = '1970-01-01')
        date <- strftime(date, format = "%Y%m%d", tz = "GMT")
     #   print(date)
        ticks <- NULL
        subTicks <- get1LevelTicks(subContracID, exchangeID = subExchange,
                     date = date, session = 'D', tickSource = tickSource)
        mainTicks <- get1LevelTicks(mainContracID, exchangeID = mainExchange,
                    date = date, session = 'D', tickSource = tickSource)
        if (!is.null(subTicks) && !is.null(mainTicks)) {
            ticks <- rbind(ticks, mergeMainSub(mainTicks, subTicks))
        }


        subTicks <- get1LevelTicks(subContracID, exchangeID = subExchange, date = date, session = 'N', tickSource = tickSource)
        mainTicks <- get1LevelTicks(mainContracID, exchangeID = mainExchange, date = date, session = 'N', tickSource = tickSource)
        if (!is.null(subTicks) && !is.null(mainTicks)) {
            ticks <- rbind(ticks, mergeMainSub(mainTicks, subTicks))
        }

        if (is.null(ticks)) {
            next
        }

        ticks <- ticks[order(ticks$utcQuoteTime),]
        print(paste('load', date))
        data <- genSpread(ticks, hedgeRatio)
        bars <- ticksToBars(data)
        allData <<- rbind(allData, data)
        allBars <<- rbind(allBars, bars)

        #data ready





    }
print('data ready')
    data <- allData
    bars <- allBars
    cl <- makeCluster(coreNum)
    tryCatch({
        registerDoParallel(cl)


        #   t <- system.time({
        # parallel
        dateGroup <- foreach(sdi = 0:as.numeric((sdMax - sdMin) / sdStep), .combine = 'rbind',
                                        .export = c('createSig', 'tradeSpread', 'plotSpread', 'plotReturn')) %do% {
            #.export = c('createSig', 'tradeSpread', 'plotSpread', 'plotReturn', 'ticksToBars', 'roundDown')) %dopar% {

                                                source('Helper_interday.R')
                                                sdAmplifier <- sdMin + sdi * sdStep
                                                print(paste('SD', sdAmplifier))

                                                sdGroup <- NULL
                                                for (mai in 0:as.numeric((maMax - maMin) / maStep)) {

                                                    maPeriod <- maMin + mai * maStep
                                                    print(paste('maPeriod', maPeriod))
                                                    fileName <- paste("ma", maPeriod, "sd", sdAmplifier, sep = '')

                                                    merged <- data
                                                    t <- system.time(merged <- createSig(merged, bars, maPeriod, sdAmplifier, maType = maType))
                                                    print(paste("createSig", t[3]))
                                                    t <- system.time(merged <- tradeSpread(merged, main = mainContract, sub = subContract,
                                                                                                    orderSize = orderSize,
                                                                                                    positionLimit = positionLimit,
                                                                                                    distortTk = distortTk,
                                                                                                    tradeCoolDown = tradeCoolDown,
                                                                                                    fee = fee,
                                                                                                    slippage = slippageTick,
                                                                                                    subTrigger = subTrigger))
                                                    print(paste("tradeSpread", t[3]))
                                                    tryCatch(pics <- plotSpread(bars = merged, lblCol = 'utcQuoteTime', closeCol = 'mid.spread', path = paste(outputFolder, date, fileName, "_Spread.png", sep = ""), save = isDraw),
                             error = function(e) {
                                 e
                             })
                                                    #multiplot(pics, cols = 1)
                                                    tryCatch(pics <- plotReturn(merged, lblCol = 'utcQuoteTime', path = paste(outputFolder, date, fileName, "_PNL.png", sep = ""), save = isDraw), error = function(e) {
                                                        e
                                                    })
                                                    #multiplot(pics, cols = 1)
                                                    write.csv(x = merged, file = paste(outputFolder, date, fileName, ".csv", sep = ""), append = F)

                                                    max <- max(merged$Net)
                                                    min <- min(merged$Net)
                                                    drawDown <- min(merged$Net - cummax(merged$Net))
                                                    end <- last(merged$Net)
                                                    re <- c(maPeriod, sdAmplifier, distortTk)
                                                    re <- c(date, re, as.numeric(end), as.numeric(max), as.numeric(min), as.numeric(drawDown))

                                                    sdGroup <- rbind(sdGroup, re)
                                                }

                                                return(sdGroup)

                                            }
                                            summary <- rbind(summary, dateGroup)

        #  })
        print(paste('done', date))
      #  print(t)
    }, finally = {
        stopCluster(cl)
    })
    rownames(summary) <- NULL
    # summary <- data.frame(summary,stringsAsFactors = F)
    # colnames(summary) <- c('Date', 'maPeriod', 'sdAmplifier', 'distortStep', 'rtn', 'max', 'min', 'drop')
    # summary$RtnDrop <- summary$rtn / -summary$drop
    #  write.csv(summary, file = paste(folder, 'summary.csv', sep = ''))
    return(summary)
}
