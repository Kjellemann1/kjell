#' get_beta
#' 
#' @import dplyr
#' @import zoo
#' @import xts
#' @import quantmod
#' @import lubridate
#' 
#' @description Estimate the beta for a stock using data from yahoo finance
#'
#' @param ticker stock ticker from yahoo as a string
#' @param index index ticker from yahoo as a string
#' @param start_date start of estimation period
#' @param end_date end of estimation period
#'
#' @return Returns the estimated beta
#'
#' @examples
#' get_beta('EQNR.OL', 'OSEBX.OL')
#' get_beta('EQNR.OL', 'OSEBX.OL', '2015-01-01', '2017-01-01')
#' 
#' @export
#' @name get_beta

get_beta <- function(ticker, index, start_date = Sys.Date() - 365 * 5, end_date = Sys.Date()) {
	suppressWarnings(getSymbols(c(index, ticker), from = start_date, to = end_date))
	
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
		mutate(index_m_ar = index[date == max(date)] / index[date == min(date)] - 1, 
			 ticker_m_ar = ticker[date == max(date)] / ticker[date == min(date)] - 1) %>% 
		na.fill0(0) %>% 
		ungroup() %>% 
		select(year, month, index_m_ar, ticker_m_ar) %>% 
		unique()
	
	return(lm(ticker_m_ar ~ index_m_ar, data = df_beta)$coefficients[2] %>% unname())
}