#' get_beta
#' 
#' @import dplyr
#' @import xts
#' @import quantmod
#' @import lubridate
#' 
#' @description estimate beta for a stock using data from yahoo finance
#'
#' @param ticker stock ticker from yahoo as a string. Example: 'EQNR.OL'
#' @param index index ticker from yahoo as a string. Example: 'OSEBX.OL'
#' @param start_date starting date. Default is 5 years from today
#' @param end_date end date. Default is today
#'
#' @return the estimated beta
#'
#' @examples
#' get_beta('EQNR.OL', 'OSEBX.OL')
#' get_beta('EQNR.OL', 'OSEBX.OL', '2021-01-01', '2023-01-01')
#' 
#' @export
#' @name get_beta

get_beta <- function(ticker, index, star_date = Sys.Date() - 365 * 5, end_date = Sys.Date()) {
	suppressWarnings(getSymbols(c(index, ticker), from = '2021-01-01', to = Sys.Date()))
	
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