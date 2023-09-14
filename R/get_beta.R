#' get_beta
#' 
#' @import dplyr
#' @import zoo
#' @import xts
#' @import quantmod
#' @import lubridate
#' 
#' @description Estimates the beta for a stock using data from yahoo finance. By default it uses 5 years of monthly data. The same as yahoo finance
#'
#' @param ticker stock ticker from yahoo as a string
#' @param index index ticker from yahoo as a string
#' @param start_date start of estimation period
#' @param end_date end of estimation period
#'
#' @return Returns the estimated beta
#'
#' @examples
#' get_beta('TSLA', '^GSPC')
#' get_beta('EQNR.OL', 'OSEBX.OL', '2015-01-01', '2017-01-01')
#' 
#' @export
#' @name get_beta

get_beta <- function(ticker, index, 
			   start_date = Sys.Date() %>% floor_date('month') %m-% months(61), 
			   end_date = Sys.Date() %>% floor_date('month')) {
	getSymbols(c(index, ticker), from = start_date, to = end_date, src = 'yahoo') %>% suppressWarnings()
	index <- gsub('\\^', '', index)
	xts_beta <- merge(get(index), get(ticker)) %>% na.omit()
	df_beta <- xts_beta %>%
		as_tibble() %>%
		mutate(date = index(xts_beta), 
			 index = get(paste0(index, '.Close')), 
			 ticker = get(paste0(ticker, '.Close'))) %>% 
		select(date, index, ticker) %>% 
		mutate(year = year(date), 
			 month = month(date)) %>%
		group_by(year, month) %>% 
		filter(date == max(date)) %>% 
		ungroup() %>% 
		mutate(index_return = (index - dplyr::lag(index)) / dplyr::lag(index), 
			 ticker_return = (ticker - dplyr::lag(ticker)) / dplyr::lag(ticker)) %>% 
		na.omit()
	
	return(lm(ticker_return ~ index_return, data = df_beta)$coefficients[2] %>% unname() %>% round(2))
}