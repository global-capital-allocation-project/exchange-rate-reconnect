*************************
*** 0: DATA: LOAD AND MERGE DATA
*************************

*************************
*** 0a: DATA: Fundamentals IFS
*************************

import excel "$user_dir/data/raw/ifs/fundamentals_int.xlsx", sheet("International Financial Statis") firstrow clear
save "$user_dir/data/temp/ifs_interest.dta", replace emptyok

import excel "$user_dir/data/raw/ifs/fundamentals.xlsx", sheet("International Financial Statis") firstrow clear
keep if variable=="National Accounts, Expenditure, Gross Domestic Product, Real, Percent Change, Previous Period, Seasonally Adjusted, Percent" | ///
	variable=="National Accounts, Expenditure, Gross Domestic Product, Nominal, Spliced Historical Series, Seasonally Adjusted, Domestic Currency" | /// 
	variable=="National Accounts, Expenditure, Gross Domestic Product, Nominal, Seasonally Adjusted, Domestic Currency" | ///
	var=="Prices, Consumer Price Index, All items, Index" | ///
	regexm(variable, "International Investment Position, Assets (with Fund Record) [BPM6], US Dollar") & ///
	regexm(variable, "International Investment Position, Liabilities (with Fund Record) [BPM6], US Dollar") | ///
	regexm(variable, "National Accounts, Expenditure, Gross Domestic Product, Final Consumption Expenditure, Nominal")
	
append using "$user_dir/data/temp/ifs_interest.dta"

gen varsym="Y_REAL_CHGQ" if regexm(variable,"Expenditure, Gross Domestic Product, Real")==1
replace varsym="Y_NOM" if regexm(variable,"Gross Domestic Product, Nominal, Seasonally Adjusted, Domestic Currency")==1
replace varsym="Y_NOM" if regexm(variable, "National Accounts, Expenditure, Gross Domestic Product, Nominal, Spliced Historical Series, Seasonally Adjusted, Domestic Currency")==1
replace varsym="P" if regexm(variable,"Price")==1
replace varsym="i" if regexm(variable,"Interest Rates")==1
replace varsym="b_a" if regexm(variable,"International")==1 & regexm(variable,"Assets")==1
replace varsym="b_l" if regexm(variable,"International")==1 & regexm(variable,"Liabilities")==1
replace varsym="C_NOM" if regexm(variable, "Consumption")
gen varprior = 1 if regexm(variable, "Gross Domestic Product, Real, Percent Change")
replace varprior = 1 if regexm(variable, "National Accounts, Expenditure, Gross Domestic Product, Nominal, Spliced")
replace varprior = 2 if regexm(variable, "Gross Domestic Product, Nominal, Seasonally Adjusted, Domestic Currency")
replace varprior = 1 if regexm(variable, "Consumption") & regexm(variable, "Seasonally Adjusted")
replace varprior = 2 if regexm(variable, "Consumption") & !regexm(variable, "Seasonally Adjusted")
replace varprior = 1 if regexm(variable, "Interest") & regexm(variable, "Treasury Bills, 3 Months")
replace varprior = 2 if regexm(variable, "Interest") & regexm(variable, "Treasury Bills, Percent")
replace varprior = 3 if regexm(variable, "Interest") & regexm(variable, "Government Bond Yields, Short to Medium Term")
replace varprior = 4 if regexm(variable, "Interest") & regexm(variable, "Government Bonds, Percent")
replace varprior = 1 if regexm(variable, "Prices")

order varsym varprior, after(quarter)
drop variable
sort quarter varsym varprior

gen qnum=substr(quarter,2,1)
gen year=substr(quarter,-4,4)
order qnum year
gen monthstr="3" if qnum=="1"
replace monthstr="6" if qnum=="2"
replace monthstr="9" if qnum=="3"
replace monthstr="12" if qnum=="4"
gen datestr=monthstr+"/28/"+year
gen date=date(datestr,"MDY")
format date %td
gen date_q=qofd(date)
drop date qnum year datestr quarter monthstr
order date_q
format date_q %tq
reshape wide Australia-UnitedStates, i(date_q varsym) j(varprior)
foreach x of varlist _all {
	rename `x' xx`x'
}
rename xxdate_q date_q
rename xxvarsym var
reshape long xx, i(date_q var) j(country) str
reshape wide xx, i(date_q country) j(var) str
renpfix xx
encode country, gen(cid)
order cid
tsset cid date_q

gen p=log(P)
gen pi_q=p-l.p
gen pi_y=p-l4.p

gen y_nom=log(Y_NOM)
gen dy_nom_q=y_nom-l.y_nom
gen dy_nom_y=y_nom-l4.y_nom

gen c_nom=log(C_NOM)
gen dc_nom_q=c_nom-l.c_nom
gen dc_nom_y=c_nom-l4.c_nom

gen c_real=c_nom-p
gen y_real=y_nom-p
gen dc_real_q=c_real-l.c_real
gen dc_real_y=c_real-l4.c_real
gen dy_real_def_q=y_real-l.y_real
gen dy_real_def_y=y_real-l4.y_real

gen dy_q = Y_REAL_CHGQ/100
gen dy_y = dy_q + l1.dy_q + l2.dy_q + l3.dy_q
replace dy_q = dy_real_def_q if missing(dy_q)
replace dy_y = dy_real_def_y if missing(dy_y)

replace i=i/100

save "$user_dir/data/output/ifs/before_filter.dta", replace emptyok
tsset cid date_q, delta(1)
replace y_real = l.y_real*(1+dy_q) if missing(y_real)
drop if missing(y_real)
tsset cid date_q, delta(1)
tsfilter hp y_gap = y_real, smooth(1600)
keep date cid y_gap 
save "$user_dir/data/output/ifs/after_filter.dta", replace emptyok

use "$user_dir/data/output/ifs/before_filter.dta", clear
merge 1:1 date cid using "$user_dir/data/output/ifs/after_filter.dta", nogen
gen priority = real(substr(country,length(country),1))
replace country = substr(country,1,length(country)-1)
drop C_NOM	P	Y_REAL
order cid date country priority
* Replace priority 2 with priority 3 if missing; Replace priority 1 with priority 2 if missing
foreach x of varlist i-y_gap {
	di "Doing `x'"
	bysort date country (priority): replace `x' = `x'[_n+1] if missing(`x') & _n==2
	bysort date country (priority): replace `x' = `x'[_n+1] if missing(`x') & _n==1
}

cap rm "$user_dir/data/output/ifs/before_filter.dta"
cap rm "$user_dir/data/output/ifs/after_filter.dta"

drop if priority > 1
drop priority cid
encode country, gen(cid)
sort date_q country
foreach x of varlist i-y_gap {
	replace `x' = `x'[_n+1] if missing(`x') & country=="EuroArea" & country[_n+1]=="Germany"
}
drop if country=="Germany"

gen usa="usa"
replace usa="g10" if country!="UnitedStates"
collapse (mean) i pi_q pi_y dc_nom_q dc_nom_y dc_real_q dc_real_y dy_q dy_y y_gap, by(date_q usa) 
gen usa_str="_usa"
replace usa_str="_g10" if usa=="g10"
drop usa dc_nom*
rename dc_real_* dc_* 
reshape wide i pi* dy* dc_* y_gap, i(date_q) j(usa_str) str

foreach x in i pi_q pi_y dy_q dy_y dc_q dc_y y_gap {
	gen `x'_diff=`x'_usa-`x'_g10 
}	

keep if date_q <= tq(2019q2)
save "$user_dir/data/output/ifs/fundamentals.dta", replace

*************************
*** 0b: DATA: Individual cleans
*************************

* Aggripino-Rey Global Factor
insheet using "$user_dir/data/raw/aggripinorey/globalfactor.csv", names clear
gen date=date(v1,"YMD")
gen date_q = qofd(date)
format date_q %tq
keep date_q gf
tsset date_q
gen d_gf = gf-L.gf
save "$user_dir/data/temp/intermediate/globalfactor.dta", replace emptyok

insheet using "$user_dir/data/raw/aggripinorey/globalfactor_m.csv", names clear
gen date=date(v1,"YMD")
gen date_m = mofd(date)
format date_m %tm
keep date_m gf
tsset date_m
gen d_gf = gf-L.gf
save "$user_dir/data/temp/intermediate/globalfactor_m.dta", replace emptyok

*VXO from FRED
clear
set fredkey d94c240d2b0edfbd964ff47545a4c8ab	
import fred vxocls
drop if missing(VXOCLS)
drop datestr
gen ln_vxo = log(VXOCLS)
gen date_m = mofd(daten)
gen date_q = qofd(daten)
bysort date_m (daten): keep if _n==_N // keeps only last day of month
format date_m %tm
format date_q %tq
drop daten
preserve
	tsset date_m
	gen d_ln_vxo = ln_vxo-L.ln_vxo
	drop date_q
	save "$user_dir/data/temp/intermediate/vxo_m.dta", replace emptyok
restore
	bysort date_q (date_m): keep if _n==_N // keeps only last month of quarter
	tsset date_q
	drop date_m
	gen d_ln_vxo = ln_vxo-L.ln_vxo
	save "$user_dir/data/temp/intermediate/vxo_q.dta", replace emptyok

* SP500
insheet using "$user_dir/data/raw/spx/spxdaily.csv", clear
rename gspcadjusted spx
gen date = date(v1,"YMD")
gen date_m = mofd(date)
gen date_q = qofd(date)
bysort date_m (date): keep if _n==_N // keeps only last day of month
format date_m %tm
format date_q %tq
keep date_m date_q spx
gen ln_spx = log(spx)
preserve
	tsset date_m
	gen d_log_spx = ln_spx-L.ln_spx
	drop date_q
	save "$user_dir/data/temp/intermediate/spx_m.dta", replace emptyok
restore
	bysort date_q (date_m): keep if _n==_N // keeps only last month of quarter
	tsset date_q
	drop date_m
	gen d_log_spx = ln_spx-L.ln_spx
	save "$user_dir/data/temp/intermediate/spx_q.dta", replace emptyok

* Gilchrist Zakrajšek Spread
insheet using "$user_dir/data/raw/gz/ebp_csv.csv", clear
rename date datestr
replace datestr="01"+datestr
gen date=date(datestr,"DMY")
format date %td
gen date_m=mofd(date)
format date_m %tm
gen date_q=qofd(date)
format date_q %tq
bysort date_m (date): keep if _n==_N // keeps only last day of month
drop date datestr
preserve
	tsset date_m
	gen d_gz_spread=gz-l.gz
	gen d_ebp=ebp-l.ebp
	drop date_q
	save "$user_dir/data/temp/intermediate/gz_clean_m.dta", replace emptyok
restore
	bysort date_q (date_m): keep if _n==_N // keeps only last month of quarter
	tsset date_q
	drop date_m
	gen d_gz_spread=gz-l.gz
	gen d_ebp=ebp-l.ebp
	save "$user_dir/data/temp/intermediate/gz_clean_q.dta", replace emptyok
	
* Treasury Premium DIS
use "$user_dir/data/raw/treasury_basis/CIP_data.dta", clear
replace group="g11" if currency=="DKK"
keep if group=="g10" & tenor=="1y" 
drop if missing(cip_govt)
gen date_m=mofd(date)
format date_m %tm
gen date_q=qofd(date)
format date_q %tq
bysort date_m currency (date): keep if _n==_N // keeps only last nonmissing day within month.
collapse (mean) cip_govt, by(date_m date_q)
preserve
	order date_m cip_govt
	tsset date_m
	gen d_treasbasis = cip_govt-L.cip_govt
	keep date_m d_treasbasis
	save "$user_dir/data/temp/intermediate/dis_m.dta", replace emptyok
restore
	bysort date_q (date_m): keep if _n==_N // keeps only last day of quarter
	order date_q cip_govt
	tsset date_q
	gen d_treasbasis = cip_govt-L.cip_govt
	keep date_q d_treasbasis
	save "$user_dir/data/temp/intermediate/dis_q.dta", replace emptyok
	
* He Kelly Manela Value Weighted Returns
insheet using "$user_dir/data/raw/hkm/He_Kelly_Manela_Factors_monthly.csv", clear
keep yyyymm intermediary_value_weighted_inve
tostring yyyymm, replace
gen yyyy=substr(yyyymm,1,4)
gen mm=substr(yyyymm,5,6)
gen datestr=yyyy + "/" + mm + "/01"
gen date=date(datestr,"YMD")
format date %td
gen date_m=mofd(date)
format date_m %tm
gen date_q=qofd(date)
format date_q %tq
preserve
	keep date_m intermediary_value_weighted_inve
	rename intermediary_value_weighted_inve int_value_weighted_inve
	order date_m int_value_weighted_inve
	save "$user_dir/data/temp/intermediate/hkm_m.dta", replace emptyok
restore
	bysort date_q (date_m): keep if _n==_N // keeps only last month of quarter
	keep date_q date_m intermediary_value_weighted_inve
	tsset date_q
	rename intermediary_value_weighted_inve int_value_weighted_inve
	order date_q int_value_weighted_inve
	save "$user_dir/data/temp/intermediate/hkm_q.dta", replace emptyok
	
*************************
*** 1: DATA: QUARTERLY: Aggregate fundamentals, flows, and risk factors 
*************************

use "$user_dir/data/output/imf_bop/imf_bop_lmns_regression_q_ipolate.dta", clear

*TREASURY PREMIUM DIS
mmerge date_q using "$user_dir/data/temp/intermediate/dis_q.dta", unmatched(master)

*GLOBAL FACTOR
mmerge date_q using "$user_dir/data/temp/intermediate/globalfactor.dta", unmatched(master)

*LOG VXO
mmerge date_q using "$user_dir/data/temp/intermediate/vxo_q.dta", ukeep(d_ln_vxo) unmatched(master)

*LOG SPX
mmerge date_q using "$user_dir/data/temp/intermediate/spx_q.dta", ukeep(d_log_spx) unmatched(master)

*GZ
mmerge date_q using "$user_dir/data/temp/intermediate/gz_clean_q.dta", ukeep(d_gz_spread) unmatched(master)

*HKM
mmerge date_q using "$user_dir/data/temp/intermediate/hkm_q.dta", ukeep(int_value_weighted_inve) unmatched(master)

*IFS FUNDAMENTALS
mmerge date_q using "$user_dir/data/output/ifs/fundamentals.dta", ukeep(i_diff pi_q_diff pi_y_diff dy_q_diff dy_y_diff dc_q_diff dc_y_diff y_gap_diff) unmatched(master)

keep if date_q>=tq(1973q1)
duplicates drop
tsset cid date_q 
drop *merge*
save "$user_dir/data/output/allmerged.dta", replace

*************************
*** 2: DATA: MONTHLY: Aggregate risk factors 
*************************

use "$user_dir/data/output/ER_data/log_exrate_eqwgt_m.dta", clear
keep if iso_currency_code=="USD"
merge 1:1 date_m using "$user_dir/data/temp/intermediate/globalfactor_m.dta"
rename _merge _merge_d_gf
merge 1:1 date_m using "$user_dir/data/temp/intermediate/spx_m.dta"
rename _merge _merge_d_log_spx
merge 1:1 date_m using "$user_dir/data/temp/intermediate/vxo_m.dta"
rename _merge _merge_d_ln_vxo
merge 1:1 date_m using "$user_dir/data/temp/intermediate/gz_clean_m.dta"
rename _merge _merge_d_gz_spread
gen _merge_d_ebp = _merge_d_gz_spread
merge 1:1 date_m using "$user_dir/data/temp/intermediate/hkm_m.dta"
rename _merge _merge_hkm
merge 1:1 date_m using "$user_dir/data/temp/intermediate/dis_m.dta"
rename _merge _merge_dis

tsset date_m
keep if date_m >= tm(1977m1) & date_m <= $mrendm
drop *merge*
drop gf_aggripinorey spx ln_spx VXOCLS ln_vxo gz_spread ebp est_prob 
save "$user_dir/data/output/monthlymerged.dta", replace
