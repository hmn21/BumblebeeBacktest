source('Bumblebee_interday.R')

maType <- 'SMA'
startDate <- 20160401
endDate <- 20160429
tickSource <- 'C:\\Users\\victor\\Documents\\nautilusdata\\'
productSource <- 'C:\\Users\\victor\\Documents\\nautilusdata\\InstrumentInfo.csv'
#tickSource <- 'D:\\nautilusdata\\'
mainInstrumentID <- 'NI1609'
subInstrumentID <- 'NI1701'

sdMax <- 3
sdMin <- 1.8
sdStep <- 0.2
maMax <- 300
maMin <- 100
maStep <- 50
positionLimit = 10
distortTk = 0.5
orderSize <- 1
hedgeRatio <- 1
subTrigger = T
tradeCoolDown <- 30000
fee <- 4
slippageTick <- 1
isDraw <- T
allData <<- NULL
allBars <<- NULL
paramHeader <- c('sdMax', 'sdMin', 'sdStep', 'maMax', 'maMin', 'maStep',
  'positionLimit', 'distortTk', 'orderSize',
  'hedgeRatio', 'subTrigger', 'tradeCoolDown', 'fee', 'slippageTick', 'startDate', 'endDate')
param <- c(sdMax, sdMin, sdStep, maMax, maMin, maStep, positionLimit, distortTk, orderSize,
     hedgeRatio, subTrigger, tradeCoolDown, fee, slippageTick, startDate, endDate)
names(param) <- paramHeader

#subContract <- c('BU1609', 'SHFE', 2, 10)
#mainContract <- c('RU1609', 'SHFE', 5, 10)
mainContract <- getContractByID(mainInstrumentID, productSource)
subContract <- getContractByID(subInstrumentID)

folder <- paste('C:\\Users\\victor\\Documents\\Backtest\\Bumblebee\\', subContract$Contract, mainContract$Contract, "_", startDate, '_', endDate, '\\', sep = '')
createFolder(folder)

summary <- runBackTest(mainContract, subContract, param, tickSource, folder, maType, isDraw)
summary <- as.data.frame(summary)
colnames(summary) <- c('Date', 'maPeriod', 'sdAmplifier', 'distortTk', 'rtn', 'max', 'min', 'drop')
summary$rtn <- as.numeric(as.character(summary$rtn))
summary$RtnDrop <- as.numeric(as.character(summary$rtn)) / -as.numeric(as.character(summary$drop))
write.csv(summary, file = paste(folder, mainInstrumentID, '_summary.csv', sep = ''))
#NULL
groupColumns = c("sdAmplifier", "maPeriod")
dataColumns = c("RtnDrop", "rtn")
res = ddply(summary, groupColumns, function(x) colMeans(x[dataColumns]))

x <- matrix(res$rtn, nrow = length(unique(res$maPeriod)), ncol = length(unique(res$sdAmplifier)))
rownames(x) <- unique(res$maPeriod)
colnames(x) <- unique(res$sdAmplifier)
write.csv(x, file = paste(folder, mainInstrumentID, '_ma_sd_rtn.csv', sep = ''))

x <- matrix(res$RtnDrop, nrow = length(unique(res$maPeriod)), ncol = length(unique(res$sdAmplifier)))
rownames(x) <- unique(res$maPeriod)
colnames(x) <- unique(res$sdAmplifier)
write.csv(x, file = paste(folder, mainInstrumentID, '_ma_sd_SR.csv', sep = ''))

res = ddply(summary, groupColumns, function(x) colSums(x[dataColumns]))
x <- matrix(res$rtn, nrow = length(unique(res$maPeriod)), ncol = length(unique(res$sdAmplifier)))
rownames(x) <- unique(res$maPeriod)
colnames(x) <- unique(res$sdAmplifier)
write.csv(x, file = paste(folder, mainInstrumentID, '_ma_sd_rtn_acu.csv', sep = ''))