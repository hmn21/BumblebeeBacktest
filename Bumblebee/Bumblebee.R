source('Helper.R')
coreNum <- 7
runBackTest <- function(mainContract, subContract, param, tickSource, outputFolder, maType = 'EMA', isDraw = F) {
    subContracID <- as.character( subContract$Contract)
    subExchange <- as.character(subContract$Exchange)
    mainContracID <- as.character(mainContract$Contract)
    mainExchange <- as.character(mainContract$Exchange)
         orderSize = as.numeric(param['orderSize'])
    positionLimit = as.numeric(param['positionLimit'])
    distortStep = as.numeric(param['distortStep'])
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
        print(date)
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
       # print(ticks)
        data <- genSpread(ticks, hedgeRatio)
     #   allData<<-rbind(allData,data)
        #data ready

       # next;
        cl <- makeCluster(coreNum)
        tryCatch({
            registerDoParallel(cl)


            t <- system.time({
                # parallel
                dateGroup <- foreach(sdi = 0:as.numeric((sdMax - sdMin) / sdStep), .combine = 'rbind',
                                  #                      .export = c('createSig', 'tradeSpread', 'plotSpread', 'plotReturn')) %do% { 
                .export = c('createSig', 'tradeSpread', 'plotSpread', 'plotReturn')) %dopar% {
              
               
                    source('Helper.R')
                    sdAmplifier <- sdMin + sdi * sdStep
                    print(paste('SD', sdAmplifier))

                    sdGroup <- NULL
                    for (mai in 0:as.numeric((maMax - maMin) / maStep)) {

                        maPeriod <- maMin + mai * maStep
                        print(paste('maPeriod', maPeriod))
                        fileName <- paste("ma", maPeriod, "sd", sdAmplifier, sep = '')

                        merged <- data
                        merged <- createSig(merged, maPeriod, sdAmplifier, maType = maType)
                        merged <- tradeSpread(merged, main = mainContract, sub = subContract,
                                                                                                    orderSize = orderSize,
                                                                                                    positionLimit = positionLimit,
                                                                                                    distortStep = distortStep,
                                                                                                    tradeCoolDown = tradeCoolDown,
                                                                                                    fee = fee,
                                                                                                    slippage = slippageTick,
                                                                                                    subTrigger = subTrigger)
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
                        re <- c(maPeriod, sdAmplifier, distortStep)
                        re <- c(date, re, as.numeric(end), as.numeric(max), as.numeric(min), as.numeric(drawDown))

                        sdGroup <- rbind(sdGroup, re)
                    }

                    return(sdGroup)

                }
                summary <- rbind(summary, dateGroup)

            })
            print(paste('done', date))
            print(t)
        }, finally = {
            stopCluster(cl)
        })

    }

    rownames(summary) <- NULL
   # summary <- data.frame(summary,stringsAsFactors = F)
   # colnames(summary) <- c('Date', 'maPeriod', 'sdAmplifier', 'distortStep', 'rtn', 'max', 'min', 'drop')
   # summary$RtnDrop <- summary$rtn / -summary$drop
  #  write.csv(summary, file = paste(folder, 'summary.csv', sep = ''))
    return(summary)
}
