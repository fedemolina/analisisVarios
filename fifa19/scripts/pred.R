# Predicción
set.seed(123)
paquetes <- c("magrittr", "data.table", "caret")
sapply(paquetes, require, character.only = TRUE)
dt <- readRDS(here::here("fifa19", "data", "fifa.rds"))
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = FALSE,
                     savePredictions = TRUE,
                     verboseIter = FALSE,
                     allowParallel = TRUE)

clean_dt <- copy(dt[complete.cases(dt), ])
filter_unique <- function(DT, n = 1) {
    cols = sapply(DT, function(x) {length(unique(x)) == n})
    DT[, names(.SD), .SDcols = cols]
}
filter_unique(clean_dt, n = 1)
clean_dt[, (filter_unique(.SD, n = 1)) := NULL]
names(dt)[clean_dt[, sapply(.SD, is.numeric)]]
names(dt)[clean_dt[, sapply(.SD, is.character)]]
clean_dt[, position_su]

rfit <- train(wage_million ~ .,
              data = clean_dt,
              method = "ranger",
              importance = "permutation",
              trControl = ctrl,
              # tuneGrid = expand.grid(mtry = c(4,5, 6)),
              verbose = F)


