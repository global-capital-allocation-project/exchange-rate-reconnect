
global user_dir "/Users/yourussername/Downloads/Replication_Package"

*************************
*** DIRECTORY GLOBALS
*************************

global output "$user_dir/Data"
global regs "$user_dir/regs"
cap mkdir "$regs"
global regs_to_pres "$regs/to_pres"
cap mkdir "$regs_to_pres"
global regs_quantiles "$regs/quantiles"
cap mkdir "$regs_quantiles"
global graphs "$user_dir/graphs"
cap mkdir "$graphs"
global fcast "$user_dir/data/temp/fcast"
cap mkdir "$fcast"

*************************
*** CURRENCY LISTS
*************************

* Countries and currencies in output
global currlist "AUD BRL CAD CHF CLP CZK EUR GBP HUF IDR INR JPY KRW MXN MYR NOK NZD PHP PLN RUB SEK SGD THB TRY USD ZAR"
global counlist "AUS BRA CAN CHE CHL CZE EMU GBR HUN IDN IND JPN KOR MEX MYS NOR NZL PHL POL RUS SWE SGP THA TUR USA ZAF"

* List of EU Countries
global eu1 `""ITA","DEU","FRA","ESP","GRC","NLD","AUT" "'
global eu2 `""BEL","FIN","PRT","CYP","EST","LAT","LTU","SVK","SVN" "'
global eu3 `""MLT","EMU","LUX","IRL" "'

global g10_country "AUS CAN CHE EMU GBR JPN NOR NZL SWE USA" // one list
global g10_currency "AUD CAD CHF EUR GBP JPY NOK NZD SEK USD" // one list
global em_currency "BRL CLP CZK HUF IDR INR KRW MXN MYR PHP PLN RUB SGD THB TRY ZAR" // Narrow list
global emcurrlist "ARS BRL CLP COP CZK DKK HUF IDR ILS INR KRW MXN MYR PHP PLN RUB SEK SGD THB TRY ZAR" // Broad list

* List of developed and emerging, split within by alphabetical
global devcolist1 " "AUS","CAN","CHE","EMU","GBR" "
global devcolist2 " "JPN","NOR","NZL","SWE","USA" "
global emcolist1 " "BRA","CHL","CZE","HUN","IDN","KOR","MEX","RUS","ZAF" "
global emcolist2 " "PHL","POL","RUS","SGP","THA","TUR","ZAF" "

* list of top 30 currencies by marketvalue in Morningstar Dataset (2005-2017, end of year, simple average)
* for use in inlist, can only be 7 long
global curr_0 `" "USD" "'
global curr_1to5 `" "EUR", "JPY", "GBP", "AUD", "CAD" "'
global curr_6to10 `" "SEK", "DKK", "NZD", "NOK", "BRL" "'
global curr_11to15 `" "MYR", "MXN", "KRW", "ZAR", "COP" "'
global curr_16to20 `" "RUB", "IDR", "ARS", "SGD", "PLN" "'
global curr_21to25 `" "CHF", "ILS", "INR", "TRY", "CZK" "'
global curr_26to30 `" "CLP", "THB", "HUF", "PHP", "TWD" "'

*************************
*** REGRESSION START DATES
*************************

*Start of regressions
gl startq=tq(2007q1)
gl endq=tq(2019q2)
gl startq_bop=tq(1975q1)
gl startm=tm(2007m1)

*************************
*** MEESE ROGOFF TEST GLOBALS
*************************

global burnin=8
global trailing=20

gl mrstart_macro=tq(1980q1)
gl mrend=tq(2019q2)
 
gl mrevalstart=$mrstart +$burnin
gl NQ=$mrend-$mrevalstart
gl crisisstart=tq(2007q1)
gl crisisend=tq(2009q2)
gl mr_rollsize=40

gl mrstart_macro_m=tm(1980m1)
gl mrendm=tm(2018m12)
gl endm=$mrendm 
gl mrevalstartm=$mrstart +$burnin
gl NQm=$mrend-$mrevalstart
gl crisisstartm=tm(2007m1)
gl crisisendm=tm(2009m6)
gl mr_rollsizem=120

