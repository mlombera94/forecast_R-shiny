my.spread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    dcast(Date~temp, fun=sum, fill = 99999.99) # NOTE: DATE is hard encoded, therfore, time column needs to be named DATE
}


xts.to.ts <- function(X, freq = 12L) {
  ts(as.numeric(X), 
     start = c(.indexyear(X)[1] + 1900, .indexmon(X)[1] + 1),
     freq = freq)
}
