******************************************************************************
* This program creates the key datasets and then produces Table V and Figure I in 
* Greenwood, Hanson, Stein, and Sunderam, "A Quantity-Driven Theory of Term Premia
* and Exchange Rates". The corresponding author is Samuel Hanson, shanson@hbs.edu.
******************************************************************************

set more off
program drop _all
* set trace off
set type double

******************************************************************************
**************** Assemble data 
******************************************************************************

		
		***********************************************************************************
		*** Define program that assembles a panel of foreign bond yields and currencies ***
		***********************************************************************************
		
		global start = 2007
		global end = 2021
		
		program define assemble_daily_panel

			***********************************
			*** Canadian zero-coupon yields ***
			*********************************** 

			*** Source: https://www.bankofcanada.ca/rates/interest-rates/bond-yield-curves/

				import excel using canada_yield_curves.xlsx, clear sheet(upload) firstrow
				drop if sveny10 == .
				keep if year(date) >= $start & year(date) <= $end
				keep date sven*
				gen currency = "CAD"
				order date currency
				save fx_daily_panel_USD, replace
				
			********************************
			*** Swiss zero-coupon yields ***
			********************************
			
			*** Source: https://www.snb.ch/en/srv/id/contact

				import excel using swiss_yield_curves.xlsx, clear sheet(daily_upload) firstrow
				drop if sveny10 == .
				keep if year(date) >= $start & year(date) <= $end
				keep date sven*
				
				* Yields are quoted on an annual bond equivalent basis: take log(1+Y) to get continuously compounded equivalent		
				unab varlist: sven*
				foreach var of local varlist {
					replace `var' = 100*ln(1+`var'/100)	
				}
				
				gen currency = "CHF"
				order date currency
				append using fx_daily_panel_USD
				sort currency date
				save fx_daily_panel_USD, replace
						
			******************************************
			*** Japan Constant Maturity Par yields ***
			******************************************
		
				*** https://www.mof.go.jp/english/jgbs/reference/interest_rate/index.htm
				import excel using jgbcme_all.xlsx, clear sheet(upload) firstrow 
				keep if year(date) >= $start & year(date) <= $end
			
					**********************************************************************
					***Bootstrap the Japanese zero coupon curve from Japanese CMT yields
					*** CMT yields are semi annual and we treat these as par yields
					*** Procedure:
					*** 1)  Take 6-month discount bill yield from Bloomberg: Use this to compute the price of an 6-month zero-coupon
					*** 2)  Linearly interpolate 1.5 par yield as the average of 1 and 2 year par yields
					*** 3)  Recurisviely solve for zero-coupon prices (the discount function)
					*** 4)  Compute continuously compunded annual yields
					**********************************************		
				
					drop if cmtpy1_0 == .
					
					* Interpolate n.5 par yields as (Y(n) + Y(n+1))/2
					forvalues n = 9(-1)1 {
						local n1 = `n' + 1
						gen cmtpy`n'_5 = (cmtpy`n'_0 + cmtpy`n1'_0)/2
						order cmtpy`n'_*
					}
					order date cmtpy0_5 cmtpy*
					unab varlist: cmtpy*
					foreach var of local varlist {
						replace `var' = `var'/100
					}	
					
					* We only have 6-month yields from Bloomberg beginning in 1999
					* Prior to that assume 6-month and 12-month yields are the same
					replace cmtpy0_5 = cmtpy1_0 if cmtpy0_5 == .
					
					* Initialize bootstrap using 6-month yield
					gen z0_5 = 1/(1+0.5*cmtpy0_5)
					gen sveny0_5 = -100*ln(z0_5)/0.5
					
					* Recurisviely solve for zero-coupon prices and yields
					gen z_sum = z0_5	
					forvalues n = 1/9 {
						gen z`n'_0  = (1 - 0.5*cmtpy`n'_0*z_sum)/(1 + 0.5*cmtpy`n'_0)
						gen sveny`n'_0 = -100*ln(z`n'_0)/`n'		
						replace z_sum = z_sum + z`n'_0
						
						gen z`n'_5  = (1 - 0.5*cmtpy`n'_5*z_sum)/(1 + 0.5*cmtpy`n'_5)
						gen sveny`n'_5 = -100*ln(z`n'_5)/(`n'+0.5)				
						replace z_sum = z_sum + z`n'_5		
					}
					local n = 10
					gen z`n'_0  = (1 - 0.5*cmtpy`n'_0*z_sum)/(1 + 0.5*cmtpy`n'_0)
					gen sveny`n'_0 = -100*ln(z`n'_0)/`n'		
				
					unab varlist: cmtpy*
					foreach var of local varlist {
						replace `var' = `var'*100
					}
					
					/*
					* Plot data to make sure that bootstrapping looks reasonable				
					twoway line cmtpy5_0 sveny5_0 date
					twoway line cmtpy7_0 sveny7_0 date
					twoway line cmtpy10_0 sveny10_0 date
					gen svenf_7to10 = (10*sveny10_0 - 7*sveny7_0)/3
					gen cmtpy_7to10 = (10*cmtpy10_0 - 7*cmtpy7_0)/3
					twoway line cmtpy_7to10 svenf_7to10 date
					* Conclusion: Bootstrapped zero-coupon yields are extremely correlated with par yields
					* and levels are similar since the level of rates in Japan has been so low in recent decades
					* A zero coupon-yield is an equal weighted average of forward short-term rates
					* A par yield is a VW average (involving ZC prices) of future forwards with more distant forwards receiving less weight (assuming positive rates)
					* However, when the level of rates is low the VW average and the EW average are quite similar: Bond math works!
					*/
					
					keep date sveny*
					drop  sveny*_5
					
					forvalues n = 1/10 {
						rename sveny`n'_0 sveny`n'
					}								
		
				gen currency = "JPY"
				drop if sveny10 == .

				order date currency
				append using fx_daily_panel_USD
				sort currency date
				save fx_daily_panel_USD, replace

			*******************************
			***German zero-coupon yields **
			*******************************

			*** Source: https://www.bundesbank.de/en/statistics/money-and-capital-markets/interest-rates-and-yields/term-structure-of-interest-rates-622440

				import excel using bbk_paket1_daily.xlsx, clear sheet(upload) firstrow
				drop if sveny10 == .
				keep if year(date) >= $start & year(date) <= $end
				keep date sven*
				gen currency = "EUR"
				order date currency
				append using fx_daily_panel_USD
				sort currency date
				save fx_daily_panel_USD, replace
		
			*************************************
			*** Australian zero-coupon yields ***
			*************************************
			
			* Source: Noted in the spreadsheet
			* Sheet 1, from https://www.rba.gov.au/statistics/tables/xls-hist/zcr-analytical-series-hist.xls
			* Sheet 2, from https://www.rba.gov.au/statistics/tables/xls/f17hist.xls

				import excel using aud_yield_curves, clear sheet(output) firstrow
				drop if sveny10 == . 
				keep if year(date) >= $start & year(date) <= $end
				keep date sven*
				gen currency = "AUD"
				order date currency
				append using fx_daily_panel_USD
				order date currency
				sort currency date
				save fx_daily_panel_USD, replace
			
			*****************************
			*** UK zero-coupon yields ***
			*****************************
			
			*** Source: https://www.bankofengland.co.uk/statistics/yield-curves
				import excel using GLC_Nominal_daily_data_2005_to_2015.xlsx, clear firstrow
				drop if sveny10 == .
				keep if year(date) >= $start & year(date) <= $end
				keep date sven*
				gen currency = "GBP"
				order date currency
				append using fx_daily_panel_USD
				sort currency date
				save fx_daily_panel_USD, replace
				
				import excel using GLC_Nominal_daily_data_2016_to_Present.xlsx, clear firstrow				
				drop if sveny10 == .
				keep if year(date) >= $start & year(date) <= $end
				keep date sven*
				gen currency = "GBP"
				order date currency
				append using fx_daily_panel_USD
				sort currency date
				save fx_daily_panel_USD, replace

			*** Rename foreign zeros as "for" ***
				
				unab svenlist: sven*
				foreach sven of local svenlist {
						rename `sven' for_`sven'
						format for_`sven' %9.4fc
						label var for_`sven' for_`sven'				
				}
				sort currency date
				save fx_daily_panel_USD, replace
		
			****************************************
			*** Merge on FX rates from Bloomberg ***
			****************************************		

			** Bond yields will be missing on different weekdays due to national holidays
			** However, FX markets are open 24-hours a day on weekdays, so we have end-of-day (5pm EST) quotes for all weekdays
			** Ths, we base the master set of dates on the FX dataset
			
			import excel using FX_Bloomberg_daily.xlsx, clear sheet(PX_LAST_Upload) firstrow
			
			sort date
			gen t = _n
			
			*** Express all FX rates expressed as USD/FCU (indirect quotes), so higher FX means stronger FCU versus USD	
			
			*** Spot FX rates
			gen FX_AUD = AUD
			gen FX_CAD = 1/CAD
			gen FX_CHF = 1/CHF
			gen FX_EUR = EUR
			gen FX_GBP = GBP
			gen FX_JPY = 1/JPY
			
			keep date t FX_*
						
			reshape long FX_, i(date t) j(currency) string
			rename FX_ FX
			
			* Take 100* log(FX) so log returns will be in percentage points
			gen fx = 100*log(FX)		
			preserve
			drop if currency == "USD"
			merge 1:1 date currency using fx_daily_panel_USD
			tab _merge
			
			gen year = year(date)
			tab year currency if _merge == 3
			tab year currency if _merge == 1
			keep if _merge == 3 | _merge == 1
			drop _merge

			sort currency date			
			egen currency_num = group(currency)
			xtset currency_num t			
			save fx_daily_panel_USD, replace
			
			*****************************
			*** US zero-coupon yields ***
			*****************************
			
			*** Source: https://www.federalreserve.gov/pubs/feds/2006/200628/200628abs.html

				import excel using feds200628.xlsx, clear sheet(upload) firstrow
				drop if sveny10 == .
				keep if year(date) >= $start & year(date) <= $end
				keep date sven*
				unab svenlist: sven*
				foreach sven of local svenlist {
						rename `sven' dom_`sven'
						format dom_`sven' %9.4fc
						label var dom_`sven' dom_`sven'				
				}
				merge 1:m date using fx_daily_panel_USD				
				order currency_num date currency FX fx for_sven* dom_sven*				
				tab _merge
				drop _merge
				sort currency date				
				order date currency for_sven* dom_sven*
				save fx_daily_panel_USD, replace						
			
			
			** Fillin missing yield data
			xtset currency_num t
			assert r(balanced) == "strongly balanced"
			gen old_for_sveny1 = for_sveny1
			replace for_sveny1 = l.for_sveny1 if for_sveny1 == . & l.for_sveny1 != .
			gen fillin_for_sveny = (for_sveny1 !=. & old_for_sveny1 ==.)
			gen old_dom_sveny1 = dom_sveny1
			replace dom_sveny1 = l.dom_sveny1 if dom_sveny1 == . & l.dom_sveny1 != .
			gen fillin_dom_sveny = (dom_sveny1 !=. & old_dom_sveny1 == .)
			drop old*
			unab svenlist: *sven*
			foreach sven of local svenlist {
				replace `sven' = l.`sven' if `sven' == . & l.`sven' != .
			}
			xtset currency_num t
			save fx_daily_panel_USD, replace		


  			* Generate forwards and rate spreads
			use fx_daily_panel_USD, clear			
			gen for_svenf_7to10 = (10*for_sveny10 - 7*for_sveny7)/3			
			gen dom_svenf_7to10 = (10*dom_sveny10 - 7*dom_sveny7)/3
			
  			* Foreign minus USD rates
			gen for_dom_sveny1 = for_sveny1 - dom_sveny1
			gen for_dom_sveny10 = for_sveny10 - dom_sveny10
			gen for_dom_svenf_7to10 = for_svenf_7to10 - dom_svenf_7to10
			
			* Compute changes (for programs that don't permit s.x)
			xtset currency_num t
			local varlist = "fx for_sveny1 dom_sveny1 for_sveny10 dom_sveny10 for_svenf_7to10 dom_svenf_7to10 for_dom_sveny1 for_dom_sveny10 for_dom_svenf_7to10"
			foreach var of local varlist {
					gen s_`var' = s.`var'
					label variable s_`var' "s_`var'"
					gen s5_`var' = s5.`var'
					label variable s5_`var' "s_`var'"
			}					
			
			order date t currency currency_num FX fx for_sveny* dom_sveny*		
	
			xtset currency_num t
			format s* for* dom* %9.2f			
			save fx_daily_panel_USD, replace
			
			* Index of USD versus Euro, GBP, and JPY *
			gen inv_FX = 1/FX if inlist(currency,"EUR","GBP","JPY")
			local currencylist = "EUR GBP JPY"
			foreach currency of local currencylist { 
				sum date
				sum inv_FX if date == r(min) & currency == "`currency'"
				replace inv_FX = inv_FX/r(mean) if currency == "`currency'"
			}
			bysort date: egen USD = mean(inv_FX)
			gen usd = ln(USD)
			drop inv_FX

			* Index of each currency versus USD, EUR, GBP, and JPY
			gen FX_av = .
			gen fx_av = .
			foreach currency of local currencylist {
				gen inv = FX if (currency == "`currency'")
				bysort date: egen inv_FX = mean(inv)
				replace inv_FX = . if inlist(currency,"EUR","GBP","JPY") ==0
				replace inv_FX = inv_FX/FX if inlist(currency,"EUR","GBP","JPY") & currency != "`currency'"
				replace inv_FX = inv if (currency == "`currency'")
				drop inv
				foreach c of local currencylist { 
					sum date
					sum inv_FX if date == r(min) & currency == "`c'"
					replace inv_FX = inv_FX/r(mean) if currency == "`c'"
				}
				bysort date: egen `currency' = mean(inv_FX)
				drop inv_FX
				local fx = lower("`currency'")
				gen `fx' = ln(`currency')
				replace FX_av = `currency' if currency == "`currency'"
				replace fx_av = `fx' if currency == "`currency'"			
			}
			xtset
			save fx_daily_panel_USD, replace
			
			** Merge on QE event dates
			use QE_dates_2022, clear
			keep if currency == "USD"
			drop currency
			rename duration usd_duration 
			rename des_short usd_des_short
			rename des_medium usd_des_medium
			merge 1:m date using fx_daily_panel_USD
			gen usd_event = (_merge == 3)
			gen usd_QE = (_merge == 3 & usd_duration == "YES")
			drop _merge
			xtset
			save fx_daily_panel_USD, replace

			use QE_dates_2022, clear
			drop if currency == "USD"
			rename duration for_duration 
			rename des_short for_des_short
			rename des_medium for_des_medium
			merge 1:1 date currency using fx_daily_panel_USD
			gen for_event = (_merge == 3)
			gen for_QE = (_merge == 3 & for_duration == "YES")
			drop _merge
			xtset
			order currency_num currency date usd_QE usd_event usd_des_short usd_des_medium for_QE for_event for_des_short for_des_medium
			save fx_daily_panel_USD, replace		
			
end

assemble_daily_panel

program define fastload
			use fx_daily_panel_USD, clear
end


**************
** Figure 1 **
**************
		
		** Regressions for all QE dates on changes in FX on changes in local and foreign yields **
		fastload 
		local varlist = "fx for_dom_svenf_7to10"
		foreach var of local varlist {
			gen f2s4_`var' = -f2s4.`var'			
		}
		keep if usd_QE == 1
		keep if currency=="EUR" | currency=="JPY" | currency=="GBP"
		collapse (mean) f2s4_*, by(date)
		gen currency = "USD"
		save figure1_data, replace
		
		fastload 
		local varlist = "fx for_dom_svenf_7to10"
		foreach var of local varlist {
			gen f2s4_`var' = f2s4.`var'			
		}
		keep if for_QE == 1
		keep date currency f2s4*
		append using figure1_data

		tostring date, generate(datestring) format(%tdnn/dd/YY) force
		gen currency_date_string = currency + " " + datestring

		label var f2s4_for_dom_svenf_7to10 "Foreign minus other change in 3-year, 7-years forward (%)"
		label var f2s4_fx "Currency appreciation (%)"
		
		save figure1_data, replace
		
		* All events
		twoway scatter f2s4_fx f2s4_for_dom_svenf_7to10, ytitle("Currency appreciation (%)") ytitle("Foreign minus Other change in 10-year Yield (%)") legend(off) mlabel(currency_date_string) mcolor(blue) msymbol(O) msize(vsmall) mlabsize(vsmall) mlabgap(1.5) scheme(david3) || lfit f2s4_fx f2s4_for_dom_svenf_7to10, lcolor(red) lwidth(medthick) xscale(range(-0.8 0.8)) xlabel(-0.75(0.25)0.75, grid glcolor(black)) yscale(range(-7 7)) ylabel(-6(2)6, grid glcolor(black)) saving(figure1_color, replace)
		graph export figure1_color.jpg, replace
		graph export figure1_color.eps, replace
		
		twoway scatter f2s4_fx f2s4_for_dom_svenf_7to10, ytitle("Currency appreciation (%)") ytitle("Foreign minus Other change in 10-year Yield (%)") legend(off) mlabel(currency_date_string) mcolor(gs8) msymbol(O) msize(vsmall) mlabsize(vsmall) mlabgap(1.5) scheme(david3) || lfit f2s4_fx f2s4_for_dom_svenf_7to10, lcolor(black) lwidth(medthick) xscale(range(-0.8 0.8)) xlabel(-0.75(0.25)0.75, grid glcolor(black)) yscale(range(-7 7)) ylabel(-6(2)6, grid glcolor(black)) saving(figure1_bw, replace)
		graph export figure1_bw.jpg, replace
		graph export figure1_bw.eps, replace		
		

*************
** Table V **
*************
		
		fastload
		
		** Regressions for all QE dates on changes in FX on changes in local and foreign yields **
 
		local varlist = "fx for_dom_sveny1 for_sveny1 dom_sveny1 for_dom_svenf_7to10 for_svenf_7to10 dom_svenf_7to10"
		foreach var of local varlist {
			gen f2s4_`var' = f2s4.`var'
		}
		
		* All events
		reg f2s4_fx f2s4_for_dom_sveny1 if (for_QE==1 | usd_QE==1) & inlist(currency,"JPY", "EUR","GBP"), cluster(date)		
			outreg using "Table_V.txt", bdec(2) tdec(2) se 3aster replace
		reg f2s4_fx f2s4_for_dom_sveny1 f2s4_for_dom_svenf_7to10 if (for_QE==1 | usd_QE==1) & inlist(currency,"JPY", "EUR","GBP"), cluster(date)		
			outreg using "Table_V.txt", bdec(2) tdec(2) se 3aster append
		reg f2s4_fx f2s4_for_sveny1 f2s4_dom_sveny1 if (for_QE==1 | usd_QE==1) & inlist(currency,"JPY", "EUR","GBP"), cluster(date)		
			outreg using "Table_V.txt", bdec(2) tdec(2) se 3aster append
		reg f2s4_fx f2s4_for_sveny1 f2s4_dom_sveny1 f2s4_for_svenf_7to10 f2s4_dom_svenf_7to10 if (for_QE==1 | usd_QE==1) & inlist(currency,"JPY", "EUR","GBP"), cluster(date)		
			outreg using "Table_V.txt", bdec(2) tdec(2) se 3aster append
