clear all

**************************************************************
**** STEP 1: CREATE G10 EQUAL WEIGHTED EXCHANGE RATES
**************************************************************

local periodicity = `" "m" "q" "'
local base_currs = `" "g10_inc_usd" "g10_ex_usd" "non_g10" "'

foreach period in `periodicity' {
	foreach base_curr_set in `base_currs' {
		use "$user_dir/data/raw/ER_data/market_fx_prices_`period'.dta", clear
		keep iso date lcu_per_usd_SPOT_eop
		if "`period'" == "q" {
			keep if date_q >= tq(1974q1)
		} 
		else {
			keep if date_m >= tm(1974m1)
		}
		encode iso_currency_code , gen (cid)
		tsset cid date
		gen log_e = log(lcu_per_usd_SPOT_eop)
		gen d_e = log_e - L.log_e
		save "$user_dir/data/output/ER_data/log_exrate_bilateral_`period'.dta", replace emptyok
		keep iso date lcu_per_usd_SPOT_eop
		
		keep if inlist(iso, $curr_1to5) | inlist(iso, $curr_6to10) | inlist(iso, $curr_11to15) | inlist(iso, $curr_16to20) | inlist(iso, $curr_21to25) | inlist(iso, $curr_26to30)
		if "`base_curr_set'" == "g10_inc_usd" {
			local broad_curr_list "AUD CAD CHF EUR GBP JPY NOK NZD SEK USD"
		}
		else if "`base_curr_set'" == "g10_ex_usd" {
			local broad_curr_list "AUD CAD CHF EUR GBP JPY NOK NZD SEK"
		}
		else if "`base_curr_set'" == "non_g10" {
			local broad_curr_list "MYR MXN KRW ZAR COP RUB IDR ARS SGD PLN BRL ILS INR TRY CZK CLP THB HUF PHP TWD USD"
		}
		levels iso_currency_code, local(foreign_currencies)

		reshape wide lcu_per_usd_SPOT_eop, i(date) j(iso_currency_code) string

		foreach cur of local foreign_currencies {
			foreach basecur of local broad_curr_list {
				if "`cur'" != "`basecur'" {
					if "`basecur'"=="USD" {
						gen lcu_`cur'_per_`basecur' = lcu_per_usd_SPOT_eop`cur'
						local check = strpos("`broad_curr_list'", "`cur'")
						if `check' > 0 {
							gen lcu_`basecur'_per_`cur' = 1/lcu_`cur'_per_`basecur'
						}
					} 
					else {
						gen lcu_`cur'_per_`basecur' = lcu_per_usd_SPOT_eop`cur' / lcu_per_usd_SPOT_eop`basecur'
					}
				}
			}
		}
		
		* USD is only in non_g10 list to create a non-G10 equal weight for USD as base currency. remove otherwise.
		if "`base_curr_set'" == "non_g10" {
			foreach exrate of varlist lcu_*_per_USD {
				drop `exrate'
			}
		}

		drop lcu_per_usd_SPOT_eop*

		foreach var of varlist lcu* {
			gen log_`var' = log(`var')	
			drop `var'
		}

		reshape long log_, i(date) j(iso_currency_code) string

		* Update levels of iso_currency_code to include USD
		gen new_codes = substr(iso_currency_code, 5, 3)
		levels new_codes, local(foreign_currencies)
		drop new_codes
		encode iso_currency_code, gen(cid)
		tsset cid date
		gen d_log = log_ - L.log_
		drop cid log_

		reshape wide d_log, i(date) j(iso_currency_code) string

		foreach cur of local foreign_currencies {
			egen d_log_`cur'_eq_wgt = rowmean(d_loglcu_`cur'*)
		}

		keep date *_eq_wgt

		reshape long d_log_, i(date) j(iso_currency_code) string
		ren d_log_ d_log_s_eq_wgt
		replace iso_currency_code = substr(iso_currency_code, 1, 3)
		replace d_log_s_eq_wgt = . if iso_currency_code=="RUB" & d_log_s_eq_wgt==0

		if "`period'" == "q" {
			keep if date_q >= tq(1974q1)
		} 
		else {
			keep if date_m >= tm(1974m1)
		}
		if "`base_curr_set'" == "g10_inc_usd" {
			save "$user_dir/data/output/ER_data/log_exrate_eqwgt_`period'.dta", replace emptyok
		} 
		else if "`base_curr_set'" == "g10_ex_usd" {
			save "$user_dir/data/output/ER_data/log_exrate_eqwgt_exusd_`period'.dta", replace emptyok
		}
		else if "`base_curr_set'" == "non_g10" {
			save "$user_dir/data/output/ER_data/log_exrate_eqwgt_nong10_`period'.dta", replace emptyok
		}
	}
}

**************************************************************
**** STEP 2: CREATE MATRIX OF ALL G10 BILATERALS
**************************************************************

local periodicity = `" "m" "q" "'
foreach period in `periodicity' {

	use "$user_dir/data/raw/ER_data/market_fx_prices_`period'.dta", clear
	gen keep=0
	foreach x of global g10_currency {
		replace keep=1 if iso_curr=="`x'"
	}
	keep if keep==1	
	keep iso_currency_code date lcu_per_usd_SPOT_eop
	rename lcu_per_usd_SPOT_eop E_usd
	encode iso_curr, gen(cid)
	tsset cid date
	gen d_e_=log(E_usd)-log(l.E_usd)
	drop cid
	drop E_usd
	reshape wide d_e_, i(date) j(iso_curr) str
	rename d_e* d_e*_USD
	gen d_e_USD_USD=0

	foreach x of global g10_currency {
		foreach y of global g10_currency {
			cap gen d_e_`x'_`y'=d_e_`x'_USD-d_e_`y'_USD
		}
	}	
	save "$user_dir/data/output/ER_data/bilateral_matrix_`period'.dta", replace

}
