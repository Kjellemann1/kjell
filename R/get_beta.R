#' get_beta
#' 
#' @import dplyr
#' @import zoo
#' @import xts
#' @import quantmod
#' @import lubridate
#' 
#' @description Estimates the beta for a stock using data from yahoo finance. 
#' By default it uses 5 years of monthly data aggregated from daily adjusted close returns.
#' It also has a second method which uses the closing price of the last day of the month,
#' which should be equivalent to the method used by yahoo finance.
#' 
#' @param ticker stock ticker from yahoo as a string
#' @param index index ticker from yahoo as a string
#' @param method 'A' uses daily returns aggregated into monthly. 'B' uses last closing price of the month
#' @param start_date start of estimation period
#' @param end_date end of estimation period
#'
#' @return Returns the estimated beta
#'
#' @examples
#' get_beta(ticker = 'TSLA', index = '^GSPC')
#' get_beta(ticker = 'MSFT', index = '^GSPC', method = 'B')
#' get_beta(ticker = 'MOWI.OL', index = 'OSEBX.OL', start_date = '2015-01-01', end_date = '2017-01-01')
#' 
#' @export
#' @name get_beta
get_beta <- function(ticker, index, method = 'A',
			   start_date = Sys.Date() %>% floor_date('month') %m-% months(61), 
			   end_date = Sys.Date() %>% floor_date('month')) {
	names <- getSymbols(c(index, ticker), from = start_date, to = end_date, src = 'yahoo')
	xts_beta <- merge(get(names[1]), get(names[2])) %>% na.omit()
	names <- gsub('[^a-zA-Z0-9]', '.', names)
	df_beta <- switch(method, 
				A = xts_beta %>% # Aggregates daily returns into monthly returns
					as_tibble() %>%
					mutate(date = zoo::index(xts_beta), 
						 ticker = get(paste0(names[1], '.Adjusted')), 
						 index = get(paste0(names[2], '.Adjusted'))) %>% 
					select(date, index, ticker) %>% 
					mutate(year = year(date), 
						 month = month(date)) %>%
					mutate(ticker_dlr = log(ticker/dplyr::lag(ticker)), 
						 index_dlr = log(index/dplyr::lag(index))) %>% 
					na.omit() %>% 
					group_by(year, month) %>% 
					mutate(ticker_mlr = sum(ticker_dlr), 
						 index_mlr = sum(index_dlr), 
						 ticker_return = exp(ticker_mlr) - 1, 
						 index_return = exp(index_mlr) - 1) %>% 
					filter(date == max(date)) %>% 
					ungroup(),
				B = xts_beta %>% # Uses last day of the month
					as_tibble() %>%
					mutate(date = zoo::index(xts_beta), 
						 ticker = get(paste0(names[1], '.Close')), 
						 index = get(paste0(names[2], '.Close'))) %>% 
					select(date, index, ticker) %>% 
					mutate(year = year(date), 
						 month = month(date)) %>%
					group_by(year, month) %>% 
					filter(date == max(date)) %>% 
					ungroup() %>% 
					mutate(index_return = (index-dplyr::lag(index))/dplyr::lag(index), 
						 ticker_return = (ticker-dplyr::lag(ticker))/dplyr::lag(ticker)) %>% 
					na.omit())
	return(lm(ticker_return ~ index_return, data = df_beta)$coefficients[2] %>% unname() %>% round(2))
}

