#source('Helper.R')
#allData <<-NULL
#20160401-20160415
source('Bumblebee.R') 
startDate <- 20160502
endDate <- 20160519
tickSource <- 'C:\\Users\\victor\\Documents\\nautilusdata\\'
productSource<-'C:\\Users\\victor\\Documents\\nautilusdata\\InstrumentInfo.csv'
#tickSource <- 'D:\\nautilusdata\\'
mainInstrumentID <- 'RU1609'
subInstrumentID <- 'RU1701'
sdMax <- 2.6
sdMin <- 2.0
sdStep <- 0.2
maMax <- 250
maMin <- 180
maStep <- 30
positionLimit = 10
distortStep = 0
orderSize <- 1
hedgeRatio <- 1
subTrigger = T
tradeCoolDown <- 30000
fee <- 6
slippageTick <- 1
isDraw <- T
allData<<-NULL
paramHeader <- c('sdMax', 'sdMin', 'sdStep', 'maMax', 'maMin', 'maStep',
  'positionLimit', 'distortStep', 'orderSize',
  'hedgeRatio', 'subTrigger', 'tradeCoolDown', 'fee', 'slippageTick', 'startDate', 'endDate')
param <- c(sdMax, sdMin, sdStep, maMax, maMin, maStep, positionLimit, distortStep, orderSize,
     hedgeRatio, subTrigger, tradeCoolDown, fee, slippageTick, startDate, endDate)
names(param) <- paramHeader

#subContract <- c('BU1609', 'SHFE', 2, 10)
#mainContract <- c('RU1609', 'SHFE', 5, 10)
mainContract <- getContractByID(mainInstrumentID, productSource)
subContract <- getContractByID(subInstrumentID)
maType <- 'EMA'
folder <- paste('C:\\Users\\victor\\Documents\\Backtest\\Bumblebee\\', subContract$Contract, mainContract$Contract, "_", maType,"_", startDate, '_', endDate, '\\', sep = '')
createFolder(folder)

summary <- runBackTest(mainContract, subContract, param, tickSource, folder, maType, isDraw)
summary <- as.data.frame(summary)
colnames(summary) <- c('Date', 'maPeriod', 'sdAmplifier', 'distortStep', 'rtn', 'max', 'min', 'drop') 
summary$rtn <- as.numeric(as.character(summary$rtn))
summary$RtnDrop <- as.numeric(as.character(summary$rtn)) / -as.numeric(as.character(summary$drop))
write.csv(summary,  file = paste(folder, mainInstrumentID, '_summary.csv', sep = ''))
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
#write.csv(allData, file = paste(folder, mainInstrumentID, '_allData.csv', sep = ''))