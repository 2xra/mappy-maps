*! outreg2 1.3 Jun2007
*! by roywada@hotmail.com
*! based on outreg 3.0.6 and 4.0.0 by john_gallup@alum.swarthmore.edu
*
* version history
* 0.9   Oct2005	beta
* 1.0   Oct2005	fixed the glitched with column (":") in the filename
*			the error in column title location due to converting to append default (which requires replace like behavior)
* 1.1   Nov2005	took seeout and seeing back to 8.2 by eliminating egen=rowmiss( ) option
*			alpha( ) option with user-defined levels of significance
*			took out 10pct option
*			fixed the error with label option (something with putting cap on _est_*)
* 1.2   Dec2005	handle very long outreg2 command (more than 244 characters)
*			handle if label was absent with label option
* 1.2.1 Aug2006	fixed the label thing when appending with only one column
* 1.3   Jun2007   inserted version control 8.2 for master program, changed strpos to index
*                 fixed the anova thing: local cons "nocons"
*                 restoring the original active estimates per Richard

program define outreg2Main
version 8.2

* write formatted regression output to file
syntax [anything] using [,			 	///
	APpend 						///
	REPLACE						///	
	SEEout						///
	LABel 						///
	TItle(passthru) 					///
	CTitle(passthru) 					///
	Onecol 						///
	TEX							///
	TEX1(passthru)					///
	WORD							///
	EXCEL							///
	LONG							///
	COMma 						///
	Quote							///
	noNOTes 						///
	ADDNote(passthru) 				///
	BDec(numlist int >=0 <=11) 			///
	BFmt(passthru) 					///
	AUTO(integer 3)					///
	LESS(integer 1)					///
	TSTAT							///
	Pvalue 						///
	CI 							///
	BEtaco 						///
	Level(integer $S_level)				///
	Tdec(numlist int >=0 <=11 max=1) 		///
	noPAren 						///
	BRacket 						///
	noASter 						///
	2aster 						///
	ALPHA(passthru)					///
	SYMbol(passthru)	 				///
	noCONs 						///
	noNI 							///
	noR2 							///
	ADJr2 						///
	E(string)						///
	RDec(numlist int >=0 <=11 max=1) 		///
	ADDStat(passthru) 				///
	ADEc(numlist int >=0 <=11) 			///
	EForm 						///
	Margin1 						///
	Margin2(string) 					///
	Xstats]


***  a partial list of original macro names
* eqlist is the equation names within multi-equation models
* neq is the number of equation
* varlist is the variables requested by the user
* tdec specifies the decimal points for pvalue
* numi is e(N_g), the xt number of groups
* noNI is user request to not to report xt number of groups
* ivar is the e(ivar), the id for xt

*** default warnings
if "`replace'"=="replace" & "`append'"=="append" {
	di in green "replaced when both {opt replace} and {opt append} chosen"
	local replace "replace"
	local append ""
}

*** set default options
if "`replace'"=="" & "`append'"=="" {
	local append "append"
}

* always coefastr
if "`aster'"~="noaster" {
	local coefastr "coefastr"
}

* se instead of tstat
if "`tstat'"~="tstat" & "`pvalue'"~="pvalue" & "`ci'"~="ci" & "`betaco'"~="betaco" {
	local se "se"
}
else {
	local se ""
}


*** clean up file name, attach .txt if no file type is specified
local rest "`using'"
* strip off "using"
gettoken part rest: rest, parse(" ")
* strip off quotes
gettoken first second: rest, parse(" ")
local rest: list clean local(rest)

local rabbit `"""'
if index(`"`using'"', ".")==0 {
	local file = `"`rabbit'`first'.txt`rabbit'"'
	local using = `"using `file'"'
}
else {
	local file = `"`rabbit'`first'`rabbit'"'
	local using = `"using `file'"'
}


*** confirm the output file existance, to be adjusted later
cap confirm file `file'
if !_rc {
	* it exists
	local fileExist 1
}
else {
	local fileExist 0
}


*** mainfile
* cleaning the user provided inputs

if "`long'"=="long" & "`onecol'"~="onecol" {
	di in green "{opt long} option is meaningful only with {opt o:necol} option"
}


if ("`tstat'"!="")+("`pvalue'"!="")+("`ci'"!="")+("`betaco'"!="")>1 {	
	di in red "choose only one of tstat, pvalue, ci, or beta"
	exit 198
}


if `level'<10 | `level'>99 {	
	di in red "level() invalid"
	exit 198
}


if "`aster'"=="noaster" & ( "`coefastr'"=="coefastr" | "`symbol'"!="") {
	if "`coefastr'"=="coefastr" {
		di in red "cannot choose both noaster and coefastr options"
	}
	else {
		di in red "cannot choose both noaster and symbol options"
	}
	exit 198
}


if (`"`addnote'"'!="" & "`append'"=="append" & `fileExist'==1) {
	di in red "warning: addnote ignored in appended regressions"
}

		
*** LaTeX options
local tex = ("`tex'"!="")
if "`tex1'"!="" {
	if `tex' {
		di in red "may not specify both tex and tex() options"
		exit 198
	}
	local tex 1
	gettoken part rest: tex1, parse(" (")
	gettoken texopts zilch: rest, parse(" (") match(parns) /* strip off "tex1()" */
}

		
*** setting the decimals
if `"`bfmt'"'~="" {
	* disabled
	*local bdec = 3
	local bdec = 3
}


if "`tdec'"=="" {
	if "`ci'"=="ci" {
		*local tdec = `bdec'
		local tdec = 3
	}
	else if "`pvalue'"=="pvalue" {
		local tdec = 3
	}
	else {
		local tdec = 2
	}
}


if "`rdec'"=="" {
	local rdec = `tdec'
}
if (`"`addstat'"'=="" & "`adec'"!="" & "`e'"=="" ) {
	di in red "cannot choose adec option without addstat option"
	exit 198
}
if "`adec'"=="" {
	* disabled
	* local adec = `tdec'
}


if "`quote'"!="quote" {
	local quote "noquote"
}

	
*** portion here snipped away to outside the bracket below
		
if "`margin1'"~="" | "`margin2'"~="" {
	local margin = "margin"
	if "`margin2'"~="" {
		local margucp "margucp(_`margin2')"
		scalar `df_r' = .
		if "`margin1'"~="" {
			di in red "may not specify both margin and margin()"
			exit 198
		}
	}
	else {
		if "`cmd'"=="tobit" {
			di in red "dtobit requires margin({u|c|p}) after dtobit command"
			exit 198
		}
	}
}


* refreshing the tempnames (otherwise have to perform separate parsings to control for each consequtive run)
foreach var in `regN' `df_r' `rsq' `numi' `r2mat' `varname' `coefcol' `b' `vc' `b_alone' `convert' `b_eq' `vc_eq'	`beta' `st_err' `tstat' `astrix' `mrgrow' {
	cap drop `var'
}



*** separate the varist from the eqlist
local open=index("`anything'","[")
local close=index("`anything'","]")

if `open'~=0 & `close'~=0 {
	local estimates=trim(substr("`anything'",`open'+1,`close'-`open'-1))
	local temp1=trim(substr("`anything'",1,`open'-1))
	local temp2=trim(substr("`anything'",`close'+1,length("`anything'")))
	local varlist=trim("`temp1' `temp2'")
}
else {
	local varlist "`anything'"
}
* warning
if "`varlist'"~="" & ("`onecol'"=="onecol" | "`estimates'"~="" ) {
	di in text "Varlist specified: {yellow}`varlist' {green}must be present in each equation"
}

* unambiguate the names of stored estimates (wildcards)
if "`estimates'"~="" {
	local collect ""
	foreach var in `estimates' {
		local temp "_est_`var'"
		local collect "`collect' `temp'"
	}
	unab estimates : `collect'
	local collect ""
	foreach var in `estimates' {
		local temp=substr("`var'",6,length("`var'")-4)
		local collect "`collect'`temp' "
	}
	local estimates=trim("`collect'")
}

* or use est_expand


tempname estnameUnique
* a place holding name to the current estimates that has no name entered into the outreg

if "`estimates'"=="" {
	local estimates="`estnameUnique'"
}
else {
	local estimates: list uniq local(estimates)
}


*** titlefile needs set out here
tempfile titlefile

*** per Richard W., store the currently active estimates to be restored later
tempname coefActive
_estimates hold `coefActive', restore copy nullok

*** run each estimates consecutively
local estmax: word count `estimates'
forval estnum=1/`estmax' {
	local estname: word `estnum' of `estimates'
	if "`estimates'"~="`estnameUnique'" {
		qui estimates restore `estname'
	}
	* to avoid overwriting after the first time, append from the second time around
	if `estnum'==2 & "`replace'"=="replace" {
		local append "append"
		local replace ""
	}
	
	
	* the names of the available stats in e( )
	local result "scalars"
		* took out macros from the local result
	local elist=""
	foreach var in `result' {
		local var: e(`var')
		tokenize `var'
		local i=1
		while "``i''"~="" {
			*** di "e(``i'')" _col(25) "`e(``i'')'"
			local elist="`elist'``i'' "
			local i=`i'+1
		}
	}
	local elist: list uniq local(elist)
	
	* take out N (because it is always reported)
	local subtract "N"
	local elist : list elist - subtract		
	
	
	* r2 option
	* save the original for the first run and restore prior to each subsequent run
	if `estnum'==1 {
		local r2Save `"`r2'"'
	}
	else {
		local r2 `"`r2Save'"'
	}
	
	*** e(all) option
	* save the original for the first run and restore prior to each subsequent run
	if `estnum'==1 {
		local addstatSave `"`addstat'"'
	}
	else {
		local addstat `"`addstatSave'"'
	}
	
	
	*** dealing with e( ) option: put it throught addstat( )
	* local= expression restricts the length;					///
		requires a work-around to avoid subinstr/substr	functions
	
	* looking for "all" anywhere
	local position: list posof "all" in e
	
	if `"`addstat'"'~="" {
		if "`e'"~="" {
			local e: subinstr local e "," " ",all
			local e: list uniq local(e)
			
			if `position'~=0 {
				local count: word count `elist'
				local addstat=substr("`addstat'",1,length("`addstat'")-1)
				forval num=1/`count' {
					local wordtemp: word `num' of `elist'
					local addstat "`addstat',`wordtemp',e(`wordtemp')"
				}
			}
			else { /* other than all */
				local count: word count `e'
				local addstat=substr("`addstat'",1,length("`addstat'")-1)
				forval num=1/`count' {
					local wordtemp: word `num' of `e'
					local addstat "`addstat',`wordtemp',e(`wordtemp')"
				}
			}
			local addstat "`addstat')"
		}
	}
	
	* if addstat was previously empty
	else if "`addstat'"=="" {
		if "`e'"~="" {
			local e: subinstr local e "," " ",all
			local e: list uniq local(e)
			
			if `position'~=0 {
				local count: word count `elist'
				local addstat "addstat("
				forval num=1/`count' {
					local wordtemp: word `num' of `elist'
					local addstat "`addstat'`wordtemp',e(`wordtemp')"
					if `num'<`count' {
							local addstat "`addstat',"
					}
				}
			}
			else {
				local count: word count `e'
				local addstat "addstat("
				forval num=1/`count' {
					local wordtemp: word `num' of `e'
					local addstat "`addstat'`wordtemp',e(`wordtemp')"
					if `num'<`count' {
						local addstat "`addstat',"
					}
				}
			}
			local addstat "`addstat')"
		}
	}
	
	
	*** dealing with single/multiple equations
	* getting equation names
	local eqlist: coleq e(b)
	local eqlist: list clean local(eqlist)
	local eqlist: list uniq local(eqlist)
	
	
	* counting the number of equation
	local eqnum: word count `eqlist'
	* local eqnum : list sizeof eqlist
	
	
	* 0 if it is multiple equations; 1 if it is a single
	if 1<`eqnum' & `eqnum'<. {
		local univar=0
	}
	else {
		local univar=1
	}		
	
	
	tempname regN df_r rsq numi r2mat
	tempname varname coefcol b vc b_alone convert
	
	**** snipped portion moved here from above
	* for svy commands with subpop(), N_sub is # of obs used for estimation
	local cmd = e(cmd)
	
	local svy = substr("`cmd'",1,3)
	if "`svy'"=="svy" & e(N_sub) != . {
		scalar `regN' = e(N_sub)
	}  
	else {
		scalar `regN' = e(N)
	}
	
	scalar `df_r' = e(df_r)
	local	depvar  = e(depvar)

	mat `b'=e(b)
	mat `vc'=e(V)
	
	local bcols=colsof(`b')	/* cols of b */
	local bocols=`bcols'		/* cols of b only, w/o other stats */
	
	* the work around for xtmixed
	if "`e(N_g)'"=="matrix" {
		mat `convert'=e(N_g)
		scalar `numi'=`convert'[1,1]
	}
	else {
		scalar `numi'	= e(N_g)
	}
	
	local	robust  = e(vcetype)
	if "`robust'"=="." {
		local robust "none"
	}
	local	ivar	 = e(ivar)
	* equals one if true	
	capture local fracpol = (e(fp_cmd)=="fracpoly")
	**** snipped portion end
	
	
	*** parse addstat to convert possible r(), e(), and s() macros to numbers
	* clear newadd every time
	local newadd=""
	* (to avoid conflicts with r-class commands used in this program)
	if `"`addstat'"'!="" {
		gettoken part rest: addstat, parse(" (")
		gettoken part rest: rest, parse(" (") /* strip off "addstat(" */
		local i = 1
		while `"`rest'"' != "" {
			gettoken name rest : rest, parse(",") quote
			if `"`name'"'=="" {
				di in red "empty strings not allowed in addstat() option"
				exit 6
			}
			gettoken acomma rest : rest, parse(",")
			gettoken valstr rest : rest, parse(",")
			if `"`rest'"' == "" { /* strip off trailing parenthesis */
				local valstr = substr(`"`valstr'"',1,length(`"`valstr'"')-1)
				local comma2 ""
			}
			else {
				gettoken comma2 rest: rest, parse(",")
			}
			
			local value = `valstr'
			capture confirm number `value'
			if _rc!=0 {
			 	* di in red `"`valstr' found where number expected in addstat() option"'
			 	* exit 7
			}
			
			local count: word count `adec'
			local aadec : word `i' of `adec'
			
			* coding check: di "adec `adec' i `i' count `count' name `name' value `value'"
				* runs only if the user defined adec is absent for that number
			if `i'>`count' & `i'<. {
				* auto-digits: auto( )
				autodigits2 `value' `auto'
					* needs to be less than 11
				local valstr = string(`value',"%12.`r(valstr)'")
				if "`valstr'"=="" {
					local valstr .
				}
					local newadd `"`newadd'`name'`acomma'`valstr'`comma2'"'
			}
			else {
				* using previous ones if no other option
				if "`aadec'"=="" {
					local aadec `prvadec'
				}
				local valstr = string(`value',"%12.`aadec'f")
				local newadd `"`newadd'`name'`acomma'`valstr'`comma2'"'
				local prvadec = `aadec'
			}
			local i = `i'+1
		}
		local addstat `"`newadd'"'
	}
	
	
	*** logistic reports coeffients in exponentiated form (odds ratios)
	if "`cmd'"=="logistic" {
		local eform "eform"
	}
	
	* report the constant anyway
	*if "`eform'"=="eform" {
	*	local cons "nocons"
	*}
	
	*** multi-equation models
	if "`cmd'"=="mvreg" | "`cmd'"=="sureg" | "`cmd'"=="reg3" {
		local univar = 0 /* multivariate regression (multiple equations) */
		if "`onecol'" != "onecol" {
			mat `r2mat' = e(b) /* get column labels */
			local neq = e(k_eq)
		***D	local eqlist = e(eqnames)
			local depvar = "`eqlist'"
			if "`cmd'"=="mvreg" {
				local r2list = e(r2)
			}
			local eq = 1
			while `eq' <= `neq' {
				if "`cmd'"=="mvreg" {
					local r2str: word `eq' of `r2list'
					scalar `rsq' = real("`r2str'")
				}
				else {
					scalar `rsq' = e(r2_`eq')
				}
				mat `r2mat'[1,`eq'] = `rsq'
				local eq = `eq' + 1
			}
		}
		else {
			/* if onecol */
			local r2 = "nor2"	
			scalar `rsq' = .
		}
	} /* `rsq' after `r2list' to avoid type mismatch */
	
	else if "`adjr2'"=="adjr2" {
		scalar `rsq' = e(r2_a)
		if `rsq' == . {
			di in red "Adjusted R-squared (e(r2_a)) not defined; cannot use adjr2 option"
			exit 198
		}
	}
	else {
		scalar `rsq' = e(r2)
	}
	
	if ("`cmd'"=="intreg" | "`cmd'"=="svyintrg" | "`cmd'"=="xtintreg") {
		local depvar : word 1 of `depvar' /* 2 depvars listed */
	}
	
	* nolabels for anova and fracpoly
	if ("`cmd'"=="anova" | `fracpol' | "`cmd'"=="nl") {
		/* e(fp_cmd)!=. means fracpoly */
		local cons "nocons"
	}
	
	*** margin or dprobit: substitute marginal effects into b and vc
	else if ("`cmd'"=="dprobit" | "`margin'"=="margin") {
		if "`cmd'"=="dlogit2" | "`cmd'"=="dprobit2" | "`cmd'"=="dmlogit2" {
			di in red "warning: margin option not needed"
		}
		else {
			marginal2, b(`b') vc(`vc') `se' `margucp'
			local bcols = colsof(`b') /* cols of b */
			local bocols = `bcols' /* cols of b only, w/o other stats */
			if "`cmd'"=="dprobit" {
				local cons "nocons"
			}
		}
	}
	
	
	*** to handle single or multiple equations
	local neq = `eqnum'
	local eqlist "`eqlist'"
	if "`onecol'"=="onecol" | `univar'==1 {
		if "`depvar'"=="" {
			local depvar: rowname e(b)
			local depvar: word 1 of `depvar'
		}
	}
	
	*** the column title:
	* ctitle1: from ctitle, estimates name; or depvar
	* ctitle2: sometimes depvar
		* save the original ctitle for the first run and restore prior to each subsequent run
	if `estnum'==1 {
		local ctitleSave `"`ctitle'"'
	}
	else {
		local ctitle `"`ctitleSave'"'
	}

	* set ctitle1 with ctitle	
	if (`"`ctitle'"'=="" | `"`ctitle'"'==`"ctitle("")"') & (`univar'==1|"`onecol'"=="onecol") {
		if "`estname'"== "`estnameUnique'" {
			* singles here
			local ctitle=`"ctitle(`depvar')"'
			local ctitle2=""
			if `univar'~=1 & "`onecol'"=="onecol" {
				local temp=proper("`e(cmd)'")
				local ctitle=`"ctitle(`temp')"'
				local ctitle2=""
			}
		}
		else {
			local ctitle=`"ctitle(`estname')"'
			local ctitle2=""
			if `univar'==1 & "`onecol'"~="onecol" {
				local ctitle=`"ctitle(`estname')"'
				local ctitle2="`depvar'"
			}
		}
	}
	* ctitle2 is set inside coeftxt2 program

	*** lots of codes removed from here
	* tested: dlogit2 and dprobit2
	* leaves the equation names: bivariate probit, heckman, heckprob
	* leaves the choice names: mlogit
	* not sure: dmlogit2
	* not tested: svymlog
	
	
	*** when e(b) includes extra statistics (which don't have variable labels)
	capture mat `b_alone' = `b'[1,"`depvar':"]
	
	if _rc==0 {
		local bocols = colsof(`b_alone')
	}
	else if ("`cmd'"=="ologit" | "`cmd'"=="oprobit") {
		local bocols = e(df_m)
		mat `b_alone' = `b'[1,1..`bocols']
	}
	else if ("`cmd'"=="cnreg" | ("`cmd'"=="tobit" & "`margin'"~="margin")) {
		local bocols = `bocols'-1 /* last element of e(b) is not est coef */
		mat `b_alone' = `b'[1,1..`bocols']
	}
	else if ("`cmd'"=="intreg" | "`cmd'"=="svyintrg") {
		mat `b_alone' = `b'[1,"model:"]
		local bocols = colsof(`b_alone')
	}
	else if ("`cmd'"=="truncreg") {
		mat `b_alone' = `b'[1,"eq1:"]
		local bocols = colsof(`b_alone')
	}
	if `univar' & `bcols'>`bocols' & "`xstats'"!="xstats" {
		mat `b' = `b_alone'
		mat `vc' = `vc'[1..`bocols',1..`bocols']
	}
	if "`xstats'"=="xstats"& `bcols'==`bocols' {
		di in red "warning: no extra statistics - xstats ignored"
	}
	
	
	*** create table with coeftxt2 and append to existing table
	* NOTE: coeftxt2 command is rclass
	qui {
		cap preserve
		
		*** make univariate regression table (single equation)
		if `univar'==1 | "`onecol'"=="onecol" {
			
			* changing the equation name of univariate case for housekeeping purposes
			if `univar'==1 & "`onecol'"=="onecol" {
				* attach equation marker for onecol output; it sorts better
				mat colnames `b'= "`depvar':"
			}

			coeftxt2 `varlist', `se' `pvalue' `ci' `betaco' level(`level') bdec(`bdec') `bfmt' tdec(`tdec') `paren' `bracket' `aster' `symbol' `coefastr' `cons' `eform' `nobs' `ni' `r2' `adjr2' rdec(`rdec') `ctitle' ctitle2(`ctitle2') addstat(`addstat') adec(`adec') `notes' `addnote' `append' regN(`regN') df_r(`df_r') rsq(`rsq') numi(`numi') ivar(`ivar') depvar(`depvar') robust(`robust') borows(`bocols') b(`b') vc(`vc') varname(`varname') coefcol(`coefcol') univar(`univar') `onecol' estname(`estname') auto(`auto') estnameUnique(`estnameUnique') fileExist(`fileExist') less(`less') alpha(`alpha') `2aster'
				if "`append'"~="append" {
				* replace
				noi outsheet `varname' `coefcol' `using', nonames `quote' `comma' replace
				local fileExist 1
			}
			
			else {
				*** appending
				* confirm the existence of the output file
				local rest "`using'"
				* strip off "using"
				gettoken part rest: rest, parse(" ")
				if `fileExist'==1 {
					appfile2 `using', varname(`varname') coefcol(`coefcol') titlefile(`titlefile')
					noi outsheet v* `coefcol' `using', nonames `quote' `comma' replace
					drop v*
				}
				else {
					* does not exist and therefore needs to be created
					noi outsheet `varname' `coefcol' `using', nonames `quote' `comma' replace
					local fileExist 1
				}
			}
			restore, preserve
		}
		
		
		*** make multiple equation regression table
		else {
			if `"`ctitle'"'!="" & `"`ctitle'"'!=`"ctitle("")"' {		/* parse ctitle */
				partxtl2 `"`ctitle'"'
				local nct = r(numtxt)	/* number of ctitles */
				local n = 1
				while `n'<=`nct' {
					local ctitl`n' `r(txt`n')'
					local n = `n'+1
				}
			}
			else {
				local nct=0
			}
			tempname b_eq vc_eq
			
			* getting the depvar list from eqlist
			local eq = 1
			while `eq' <= `neq' {
				if `eq' <= `nct' {
					local ctitle `"ctitle(`ctitl`eq'')"'
					local ctitle2 ""
				}
				local eqname: word `eq' of `eqlist'
				local depvar: word `eq' of `eqlist'
				
				if `nct'==0 {
					if "`estname'"~="`estnameUnique'" & "`estname'"~="" {
						local ctitle ""
						if `eq'==1 {
							local ctitle "ctitle(`estname')"
						}
						local ctitle2 "`depvar'"
					}
					else {
						local ctitle "ctitle(`depvar')"
						local ctitle2 ""
					}
				}
				
				
				*** r2mat doesn't exist for mlogit ="capture" 
				
				capture scalar `rsq' = `r2mat'[1,`eq']	
				mat `b_eq' = `b'[.,"`eqname':"]
				matrix colnames `b_eq' = _:				/* remove roweq from b_eq for explicit varlist */
				mat `vc_eq' = `vc'["`eqname':","`eqname':"]
				local bocols = colsof(`b_eq')
				
				if `eq'>1 {
					local addstat ""
				}
				
				if `eq' == 1 & "`append'"!="append" {
					local apptmp ""
				}
				else {
					local apptmp "append"
				}
				
				coeftxt2 `varlist', `se' `pvalue' `ci' `betaco' level(`level') bdec(`bdec') `bfmt' tdec(`tdec') `paren' `bracket' `aster' `symbol' `coefastr' `cons' `eform' `nobs' `ni' `r2' `adjr2' rdec(`rdec') `ctitle'  ctitle2(`ctitle2') addstat(`addstat') adec(`adec') `notes' `addnote' `apptmp' regN(`regN') df_r(`df_r') rsq(`rsq') numi(`numi') ivar(`ivar') depvar(`depvar') robust(`robust') borows(`bocols') b(`b_eq') vc(`vc_eq') varname(`varname') coefcol(`coefcol') univar(`univar') `onecol' estname(`estname') auto(`auto') estnameUnique(`estnameUnique') fileExist(`fileExist') less(`less') alpha(`alpha') `2aster'
				
				* create new file: replace and the first equation		
				if `eq' == 1 & "`append'"!="append" {
					noi outsheet `varname' `coefcol' `using', nonames `quote' `comma' `replace'
					local fileExist 1
				}
				* appending here: another estimates or another equation
				else {
					* confirm the existence of the output file
					local rest "`using'"
				 	* strip off "using"
					gettoken part rest: rest, parse(" ")
					if `fileExist'==1 {
						* it exists: keep on appending even if its the first equation
						appfile2 `using', varname(`varname') coefcol(`coefcol') titlefile(`titlefile')
						noi outsheet v* `coefcol' `using', nonames `quote' `comma' replace
						drop v*
					}
					else {
						* does not exist and specified append: need to be created for the first equation only
						if `eq' == 1 & "`append'"=="append" {
							noi outsheet `varname' `coefcol' `using', nonames `quote' `comma' `replace'
							local fileExist 1
						}
					}
				}
				local eq = `eq' + 1
				
				restore, preserve /* to access var labels after first equation */
			}
		}	
	}		/* for quietly */
}		/* running consecutively */


* restoring the currently active estimate here
_estimates unhold `coefActive'

quietly {
	
	*** get the label names
	if "`label'"=="label" {
		tempfile labelfile
		
		gen str7 var1=""
		gen str7 labels=""
		unab varlist_all : *
		cap unab subtract: _est_*
		local varlist_only : list varlist_all - subtract
		local count=1
		foreach var in `varlist_only' {
			local lab: var label `var'
			local lab=trim("`lab'")
			if "`lab'"~="" {
				replace var1="`var'" in `count'
				replace labels="`lab'" in `count'
				local count=`count'+1
				}
			}
		keep var1 labels
		drop if var1==""

		* indicate no label contained
		if `=_N'==0 {
			local emptyLabel=1
		}
		else {
			local emptyLabel=0
		}

		save `labelfile'
	}
	
	
	tempvar id1 id2 id3 id4
	insheet `using', nonames clear
	
	*** clean up equation names, title, label
	gen id1=_n
	gen str7 equation=""
	gen str7 variable=""
	forval num=1/`=_N' {
		local name=trim(v1[`num'])
		local column=index("`name'",":")
		if `column'~=0 {
			local equation=trim(substr("`name'",1,`column'-1))
			local variable=trim(substr("`name'",`column'+1,length("`name'")))
			replace equation="`equation'" in `num'
			replace variable="`variable'" in `num'
		}
	}
	gen lagged=equation[_n-1]
	replace equation=lagged if equation=="" & lagged~=""
	gen top="1" if equation[_n]~=equation[_n-1] & equation[_n]~=""
	
	count if equation~=""
	if `r(N)'~=0 {
		* move equation names, instead of inserting them
		if v1[1]~="EQUATION" & v1[2]~="EQUATION" {
			gen v0=equation
			if v1[3]~="" {
				replace v0="EQUATION" in 1
			}
			else {
				replace v0="EQUATION" in 2
			}
			order v0
			replace v1=variable if variable~=""
		}
		else {
			replace v1=equation
			if v2[3]~="" {
				replace v1="EQUATION" in 1
			}
			else {
				replace v1="EQUATION" in 2
			}
		}
	}

	* strips the redundant equation names
	* must be undone at the insheet that recall this file in appfile
	count if equation~=""
	if `r(N)'~=0 {
		*** for one column option
		replace v0="" if top=="" & _n>2
	}
	
	drop id1 equation variable lagged top
	outsheet `using', nonames `quote' `comma' replace
	
	
	
	*** clean up labels
	if "`label'"=="label" {
		/*
		local dotloc = index("`bname'", ".") 
		if `dotloc'!=0 {
			/* deal w/time series prefixes */
			local tspref = substr("`bname'",1,`dotloc')
			local bname = substr("`bname'",`dotloc'+1,.)
			local blabel : var label `bname'
			local blabel = `"`tspref' `blabel'"'
		}
		else {
			local blabel : var label `bname'
		}
		*/
		
		ren v1 var1
		gen `id2'=_n
		
		* skip merging process if no label was contained
		if `emptyLabel'==1 {
			gen str7 labels=""
		}
		else {
			joinby var1 using `labelfile', unmatched(master)
			drop _merge
		}
		
		sort `id2'
		drop `id2'
		order var1 labels
		cap order v0 var1 labels
		if var1[3]~="" {
			replace labels="LABELS" in 1
		}
		else {
			replace labels="LABELS" in 2
		}
	ren var1 v1
	}
	
	
	if `"`title'"'=="" {
		* NOTE: v0- saved here
		tempfile appending
		tempvar tomato potato
		gen `tomato' =_n+10000
		save `appending',replace
		
		*** Clean up titles
		* just coef, no label, no equation
		cap confirm file `titlefile'
		if !_rc {
			use `titlefile',clear
			gen `id3'=1 if v1=="COEFFICIENT"
			replace `id3'=1 if `id3'[_n-1]==1
			drop if `id3'==1
			keep if v1~=""
			if `=_N'~=0 {
				keep v1
				gen `potato'=_n
				local titlrow=_N
				joinby v1 using `appending', unmatched(both)
				sort `potato' `tomato'
				drop _merge `potato' `tomato'
				aorder
			}
			else {
				use `appending',replace
				drop `tomato'
			}
		}
		cap drop `tomato'
	}
	
	else {
		* parse title
		partxtl2 `"`title'"'
		local titlrow = `r(numtxt)'
		local t = 1
		while `t'<=`titlrow' {
			local titl`t' `r(txt`t')'
			local t = `t'+1
		}
		
		local oldN=_N
		set obs `=`r(numtxt)'+_N'
		gen `id4'=_n+10000
		forval num=1/`r(numtxt)' {
			replace v1="`r(txt`num')'" in `=`oldN'+`num''
			replace `id4'=`num' in `=`oldN'+`num''
		}
		sort `id4'
		drop `id4'
	}
	
	if "`titlrow'"=="" {
		local titlrow=0
	}
	
	
	
	outsheet `using', nonames `quote' `comma' replace
	
	*** preparing for outputs and seeout
	ren v1 coef
	cap ren v0 eq
	
	unab vlist : v*
	local count: word count `vlist'
	forval num=1/`count' {
		local vname: word `num' of `vlist'
		ren `vname' v`num'
	}
	
	
	* number of columns
	local numcol = c(k)
	tempvar blanks rowmiss
	gen int `blanks' = (trim(v1)=="")
	
	foreach var of varlist v* {
		replace `blanks' = `blanks' & (trim(`var')=="")
	}
	
	* in case ctitle is missing: 1 of 2
	* colheadN
	replace `blanks'=0 if coef=="COEFFICIENT" | coef[_n-1]=="COEFFICIENT"
	if `count'==1 {
		local colheadN=2+`titlrow' 
	}
	else {
		local colheadN=3+`titlrow'
	}
	
	* statistics rows
	count if `blanks'==0
	local strowN `=`titlrow'+`r(N)''

	*** making alternative output files
	if "`long'"=="long" | `tex'==1 | "`word'"=="word" | "`excel'"=="excel" {
		quietly {
			
			*** convert the data into long format (insert the equation names if they exist)
			if "`long'"=="long" & "`onecol'"=="onecol" {		
				* a routine to insert equation names into coefficient column
				count if `blanks'==0 & eq~="" & eq~="EQUATION"
				tempvar id5
				gen float `id5'=_n
				local _firstN=_N
				set obs `=_N+`r(N)''
				
				local times 1
				forval num=2/`_firstN' {
					if eq[`num']~="" & eq[`num']~="EQUATION" {
						replace `id5'=`num'-.5 in `=`_firstN'+`times''
						replace coef=eq[`num'] in `=`_firstN'+`times'' 
						local times=`times'+1
					}
				}
				
				sort `id5'
				drop eq `id5' `blanks'
				
				* change `strowN' by the number of equations inserted
				local strowN=`strowN'+`r(N)'
				
				local dot=index(`"`using'"',".")
				if `dot'~=0 {
					local before=substr(`"`using'"',1,`dot'-1)
					local after=substr(`"`using'"',`dot'+1,length("`using'"))
					local usingLong=`"`before'_long.`after'"'
				}
				
				* v names
				unab vlist : *
				local count: word count `vlist'
				forval num=1/`count' {
					local vname: word `num' of `vlist'
					ren `vname' c`num'
				}
				forval num=1/`count' {
					local vname: word `num' of `vlist'
					ren c`num' v`num'
				}
				
				noi outsheet v* `usingLong', nonames `quote' `comma' replace
					
			} /* long format */
			
			else {
				drop `blanks'

				* v names
				unab vlist : *
				local count: word count `vlist'
				forval num=1/`count' {
					local vname: word `num' of `vlist'
					ren `vname' c`num'
				}
				forval num=1/`count' {
					local vname: word `num' of `vlist'
					ren c`num' v`num'
				}
			}
			
			tempfile outing
			save `outing'
			
			
			* local file defined earlier, strip off quotes and extension
			gettoken first second: file, parse(" ")
			local beg_dot = index(`"`first'"',".")
			local strippedname = substr(`"`first'"',1,`=`beg_dot'-1')
			
			
			*** LaTeX thing
			if `tex' {
				* make certain `1' is not `using' (another context)
				out2tex2 v* `using', titlrow(`titlrow') colheadN(`colheadN') strowN(`strowN') `texopts' replace
				local usingTerm `"`strippedname'.tex"'
				local cl `"{stata "shellout using `usingTerm'":"`usingTerm'"}"'
				noi di as txt `"`cl'"'
			}
			
			*** Word rtf file thing
			if "`word'"=="word" {
				use `outing',clear
				* there must be varlist to avoid error

			      out2rtf2 v* `using',  titlrow(`titlrow') colheadN(`colheadN') strowN(`strowN') replace nopretty
				local temp `r(documentname)'
				
				* strip off "using" and quotes
				gettoken part rest: temp, parse(" ")
				gettoken usingTerm second: rest, parse(" ")
				
				* from school
				*local cl `"{stata shell winexec cmd /c tommy.rtf & exit `usingTerm' & EXIT :`usingTerm' }"'
				* these work but leaves the window open
				*local cl `"{stata winexec cmd /c "`usingTerm'" & EXIT :`usingTerm'}"'
				*local cl `"{stata shell "`usingTerm'" & EXIT :`usingTerm'}"'
				*local cl `"{stata shell cmd /c "`usingTerm'" & EXIT :`usingTerm'}"'
				local cl `"{stata "shellout using `usingTerm'":"`usingTerm'"}"'
				noi di as txt `"`cl'"'
			}
			
			
			*** Excel xml file thing
			if "`excel'"=="excel" {
				use `outing',clear
				if c(stata_version)<9 & c(stata_version)~=. {
					noi di in green "{opt excel} requires Stata 9 or higher"
				}
				else {
					xmlsave `"`strippedname'"',doctype(excel) replace
					
					local usingTerm `"`strippedname'.xml"'
					local cl `"{stata "shellout using `usingTerm'":"`usingTerm'"}"'
					noi di as txt `"`cl'"'
				}
			}
			
		} /* quietly */
	} /* output files */
}
	


*** see the output
if "`seeout'"=="seeout" {
	seeing `using'
}
local cl `"{stata seeout:seeout}"'
di as txt `"`cl'"'


*** saving the current preferences
gettoken first second : 0, parse(",:. ")

while "`first'"~="," & "`first'"~="" {
	gettoken first second : second, parse(",:. ")
}

* strip seeout, replace
local second: subinstr local second "replace" "",all
local second: subinstr local second "see" "",all
local second: subinstr local second "seeo" "",all
local second: subinstr local second "seeou" "",all
local second: subinstr local second "seeout" "",all
*local second: list uniq local(second)
*local second: list retokenize second

* retokenize is dangerous for quotes; do it manually for double space
local secondClean ""
while `"`second'"'~="" {
	gettoken second1 second : second, parse(" ") quotes
	local secondClean `"`secondClean'`second1'"'
	if `"`second'"'~="" {
		local secondClean `"`secondClean' "'
	}
}

local pref `"`using'"'
local options `"`secondClean'"'

* NOTE: `0' is now overwritten
quietly findfile outreg2_prf.ado
tempname myfile
file open `myfile' using `"`r(fn)'"', write text replace
file write `myfile'  `"`c(current_date)'"' _n
file write `myfile'  `"`pref'"' _n
file write `myfile'  `"`options'"'
file close `myfile'

end		/* end of outreg2Main */




***********************


program define appfile2
version 8.2
* append regression results to pre-existing file

syntax using/, varname(string) coefcol(string) titlefile(string)

*** take out COEFFICIENT as the column heading and restore later
replace `varname' = "" in 1

* r2 issue: convert dot into empty, 2 of 2
if "`rsq'"=="." {
	local rsq==""
}

tempname vord1 vord2 vartmp varsml v2plus vorder merge2 
tempfile tmpf1
gen str80 `vartmp' = substr(`varname',1,79) /* room for "!" at end */
replace `vartmp' = "0" if _n==1
gen `varsml' = trim(`vartmp')
replace `vartmp' = `vartmp'[_n-1]+"!" if `varsml'==""

* add "!" to variable name to make it sort after previous variable name
* will cause bug if both "varname" and "varname!" already exist
count if (`varsml'=="" | (`varsml'[_n+1]=="" & _n!=_N))
local ncoeff2 = r(N)				/* number of estimated coefficients in file 2 */
local N2 = _N					/* number of lines in file 2 */
gen `vord2' = _n					/* ordering variable for file 2 */
sort `vartmp'
keep `varname' `coefcol' `vartmp' `vord2'

save "`tmpf1'", replace

insheet using `"`using'"', nonames clear


*** save equation column if it exists before dropping it
local exists=0
count if v1=="EQUATION"
if `r(N)'~=0 {
	gen v0=v1
	local exists=1
	drop v1
	* count v0 as well
	forval num=2/`=c(k)' {
		ren v`num' v`=`num'-1'
	}
}
*** strip labels columns
/* throw away after a while Aug 2006
count if v2=="LABELS"
if `r(N)'~=0 {
	drop v2
	* count v0 as well
	forval num=2/`=c(k)' {
		ren v`num' v`=`num'-1'
	}
}
*/

count if v2=="LABELS"
if `r(N)'~=0 {
	drop v2
	* count v0 as well
	forval num=2/`=c(k)' {
		ren v`=`num'+1' v`num'
	}
}

*** save title first one only, before stripping coef columns
cap save `titlefile'
* drop titles
while v1[1]~="COEFFICIENT" & v1[1]~="" {
	drop in 1
}


*** finish cleaning the equation columns
* NOTE: assuming Observation exists
if "`exists'"=="1" {
	order v0
	*** Strip the equation names and slap it back onto the variable column
	forval num=5/`=_N' {
		replace v0=v0[`num'-2] in `num' if v0[`num']=="" & v1[`num']~="" & v1[`num']~="Observations"
	}
	forval num=3/`=_N' {
		if v0[`num']~="" {
			local temp1=v0[`num']
			local temp2=v1[`num']
			replace v1="`temp1':`temp2'" in `num'
		}
	}
	drop v0
}

*** take out COEFFICIENT as the column heading and restore later
local coeftitle1 v1[1]
local coeftitle2 v1[2]
replace v1 = "" in 1/2

/*
* the 0 makes it match
if "`coeftitle1'"~="" & "`coeftitle2'"~="" {
	replace v1="0" in 1
}
*/


* getting the characteristics
describe, short
local numcol = r(k)				/* number of columns already in file 1 */
gen str80 `vartmp' = substr(v1,1,79)	/* room for "!" at end */
local titlrow = (v1[1]!="")

* `titlrow'	is assumed to be zero
local frstrow = 1 + `titlrow'			/* first non-title row */

replace `vartmp' = "0" if _n==`frstrow' & v2=="(1)"
replace `vartmp' = "0!" if _n==`frstrow' & v2!="(1)"
replace `vartmp' = `vartmp'[_n-1]+"!" if `vartmp'==""
gen long `vord1' = _n
gen str80 `v2plus' = trim(v2)
local col = 3
if `col'<=`numcol' {
	replace `v2plus' = `v2plus' + trim(v`col')
	local col = `col'+1
}
*count if ((v1=="" & `v2plus'!="") | (v1[_n+1]=="" & (`v2plus'[_n+1]!=""|_n==1) & _n!=_N))
	* i.e. a t stat or column heading
	* i.e. a coefficient (next row is a t stat)
* make it count empty ctitle2 with this code:
tempvar topoff
gen `topoff'=1 if v1~=""
replace `topoff'=1 if `topoff'[_n-1]==1
replace `topoff'=sum(`topoff')
count if (`topoff'==0 | (v1=="" & `v2plus'!="") | (v1[_n+1]=="" & (`v2plus'[_n+1]!=""|_n==1) & _n!=_N))
drop `topoff'

local ncoeff1 = r(N)

gen `varsml' = `vartmp'
summ `vord1' if `vord1'>`ncoeff1' & `v2plus'!=""	/* v2plus for addstat */
local endsta1 = r(max)						/* calc last row of statistics before notes */

if `endsta1'==. {
	local endsta1 = `ncoeff1'
}
drop `varsml'
sort `vartmp'

*** merging the two files
merge `vartmp' using `tmpf1'
gen `varsml' = `vartmp'
gen `vorder' = 1 if (`vord1'<=`ncoeff1' | `vord2'<=`ncoeff2')			/* coefficients */
replace `vorder' = 0 if ((`vord1'<=`titlrow') | (`vartmp'=="0" & _merge==2))	/* has title or has no column numbers */
replace `vorder' = 2 if (`varsml'=="Constant" | `varsml'=="Constant!")		/* constant */
replace `vorder' = 3 if `vorder'==. & (`vord1'<=`endsta1' | `vord2'<=`N2')	/* statistics */
replace `vorder' = 4 if `vorder'==.								/* notes below statistics */

gen byte `merge2' = _merge==2
sort `vorder' `vord1' `vord2' `merge2'

replace v1 = `varname' if v1=="" & `varname'!=""
drop `varname' `vartmp' `varsml' `vorder' `vord1' `vord2' `merge2' _merge
if (`numcol'==2) {
	replace v2 = "(1)" if _n==`frstrow'
	replace `coefcol' = "(2)" if _n==`frstrow'
}
else {
	replace `coefcol' = "(" + string(`numcol') + ")" if _n==`frstrow'
}

*** restore COEFFICIENT and 0 head
replace v1 = "" in 1
replace v1 = "COEFFICIENT" in 2

end
/* appfile2 */


***********************


program define marginal2
version 8.2
* put marginal effects (dfdx) into b and vc matrices 

syntax , b(string) vc(string) [se margucp(string)]

tempname dfdx se_dfdx new_vc dfdx_b2		
capture mat `dfdx' = e(dfdx`margucp')
if _rc==0 {
	local cnam_b : colnames `dfdx'
	local cnam_1 : word 1 of `cnam_b'
}
if _rc!=0 {
	if "`cnam_1'"=="c1" {
		di in red `"Update dprobit ado file: type "help update" in Stata"'
	}
		else {
		di in red "{opt margin} option invalid: no marginal effects matrix e(dfdx`margucp') exists"
	}
	exit
}


/* create matrix of diagonals for vc */
if "`se'"=="se" {
	if e(cmd)=="dprobit" | e(cmd)=="dtobit" {
		if e(cmd)=="dprobit" {
			local margucp "_dfdx"
		}
		mat `se_dfdx' = e(se`margucp')
		mat `vc' = diag(`se_dfdx')
		mat `vc' = `vc' * `vc'
	}
	else {
		mat `vc' = e(V_dfdx)
	}
	mat colnames `vc' = `cnam_b'
}
else {
	/* if t or p stats reported then trick `cv' into giving the right t stat */
	local coldfdx = colsof(`dfdx')
	mat `new_vc' = J(`coldfdx',`coldfdx',0)
	local i = 1
	while `i' <= `coldfdx' {
		scalar `dfdx_b2' = (el(`dfdx',1,`i')/el(`b',1,`i'))^2
		mat `new_vc'[`i',`i'] = `dfdx_b2'*`vc'[`i',`i']
		local i = `i'+1
	}
	mat colnames `new_vc' = `cnam_b'
	mat `vc' = `new_vc'
}  
mat `b' = `dfdx'
end


***********************


program define partxtl2, rclass
version 8.2
*** parse text list to find number of text elements and return them
	local ntxt = 0
	gettoken part rest: 1, parse(" (") 
	gettoken part rest: rest, parse(" (")		/* strip off "option(" */
	while `"`rest'"' != "" {
		local ntxt = `ntxt'+1
		gettoken part rest: rest, parse(",)") 
		return local txt`ntxt' `"`part'"'
		gettoken part rest: rest, parse(",)")	/* strip off "," or "(" */
	}
	return local numtxt `ntxt'
end


***********************


program define coeftxt2
version 8.2
* getting the coefficient name, values, and t statistics

syntax [varlist(default=none ts)] [, SE Pvalue CI BEtaco Level(integer $S_level) BDEC(numlist) BFmt(passthru) TDEC(numlist) noPAren BRacket noASter SYMbol(passthru) COEfastr noCONs EForm noNOBs noNI noR2 ADJr2 RDec(numlist) CTitle(string) CTITLE2(string) ADDStat(passthru) ADEC(numlist) noNOTes ADDNote(passthru) APpend regN(string) df_r(string) rsq(string) numi(string) ivar(string) depvar(string) robust(string) BOROWS(string) b(string) vc(string) varname(string) coefcol(string) univar(string) Onecol estname(string) AUTO(integer 3) estnameUnique(string) fileExist(integer 1) less(integer 1) ALPHA(string)  2aster]

* r2 issue: convert dot into empty, 1 of 2
if "`rsq'"=="." {
	local rsq==""
}

tempvar beta st_err tstat astrix mrgrow
if "`betaco'"=="betaco" {
	tempname betcoef
}

tempfile bcopy 
tempname b_alone vc_alon b_xtra vc_xtra t_alpha

mat `b' = `b''
mat `vc' = vecdiag(`vc')
mat `vc' = `vc''

local brows = rowsof(`b')

*** setting ctitle1
local coltitl `"`ctitle'"'

*** setting ctitle2
local coltit2=`"`ctitle2'"'

*** xt options
if (`numi'!=. & "`ni'"!="noni") {
	if `"`iname'"'=="" {
		local iname "`ivar'"
	}
	if `"`iname'"'=="." {
		local iname "groups"
	}
}

* fill in "beta" "st. err." & "tstat" variables from regression output
* in case varlist is specified:

mat `b_alone' = `b'[1..`borows',1] /* use to avoid _cons in xtra stats */
if `brows'>`borows' {
	/* allow for xtra stats */
	local borows1 = `borows'+1
	mat `b_xtra' = `b'[`borows1'...,1...]
	mat `vc_xtra' = `vc'[`borows1'...,1...]
}
if "`varlist'"!="" {
	tempname arow testnew newb newvc
	/* add the constant unless "nocons" is chosen */
	if "`cons'"!="nocons" {
		local varlist "`varlist' _cons"
	}
	local vname : word 1 of `varlist'
	local i=1
	while "`vname'"!="" {
		local j = rownumb(`b_alone',"`vname'")
		if `j'!=. {
			matrix `arow' = `b'[`j',1...]			/* "..." needed to get rownames */
			matrix `newb' = nullmat(`newb')\ `arow'
			matrix `arow' = `vc'[`j',1...]
			matrix `newvc' = nullmat(`newvc')\ `arow'
		}
		else if (`univar' & "`vname'"!="_cons") {
			di in red "`vname' not found in regression coefficients"
			exit 111
		}
		local i = `i'+1
		local vname : word `i' of `varlist'
	}
	mat `b_alone' = `newb'
	if `brows'>`borows' {
		* allow for xtra stats
		mat `newb' = `newb'\ `b_xtra'
		mat `newvc' = `newvc'\ `vc_xtra'
	}
	mat `b' = `newb'
	mat `vc' = `newvc'
}
else if "`cons'"=="nocons" {
	* delete the constant if "nocons" is chosen
	local j_1 = rownumb(`b_alone',"_cons")-1
	if `j_1'!=. {
		/* in case there is no constant in b */
		mat `b_alone' = `b_alone'[1..`j_1',1...]
		mat `vc_alon' = `vc'[1..`j_1',1...]
		if `brows'==`borows' { 
			mat `b' = `b_alone'
			mat `vc' = `vc_alon'
		}
		else {
			* allow for xtra stats
			mat `b' = `b_alone' \ `b_xtra'
			mat `vc' = `vc_alon' \ `vc_xtra'
		}
	}
}

local borows = rowsof(`b_alone')
local brows = rowsof(`b') /* reset brows */

gen double `beta' = matrix(`b'[_n, 1]) in 1/`brows'
gen double `st_err' = matrix(`vc'[_n, 1]) in 1/`brows'
replace `st_err' = sqrt(`st_err')
gen double `tstat' = (`beta'/`st_err')
if "`pvalue'"=="pvalue" {
	if `df_r'==. {
		replace `tstat' = 2*(1-normprob(abs(`tstat')))
	}
	else {
		replace `tstat' = tprob(`df_r', abs(`tstat'))
	}
}
if "`eform'"=="eform" {
	* exponentiate beta and st_err
	replace `beta' = exp(`beta')
	replace `st_err' = `beta'*`st_err'
}
if "`betaco'"=="betaco" {
	* create "beta" coefficients
	sum `depvar' if e(sample)
	gen `betcoef' = `beta'/r(sd) in 1/`brows'
}

* fill in variables names column
gen str31 `varname' = ""
local bnames : rowfullnames(`b_alone')

local bname : word 1 of `bnames'
local i 1

*** _cons is replaced by Constant
* to make the replacement in presence of equation name: take the equation name off for constant only
while "`bname'"!="" {
	tokenize "`bname'", parse(":")
	local temp="`3'"

	if "`bname'"!="_cons" {
		* codes cut out: labels with time-series put into blabel if nolabel not specified
		local blabel ""
		if "`betaco'"=="betaco" {
			/* create "beta" coefficients */
			sum `bname' if e(sample)
			replace `betcoef' = r(sd)*`betcoef' if `i'==_n
		}
	}
	else {
		local blabel "Constant"
	}

	if "`temp'"=="_cons" {
		local blabel "`1':Constant"
	}

	if `"`blabel'"'=="" { 
		local blabel "`bname'" 
	}
	replace `varname' = trim(`"`blabel'"') if `i'==_n
	local i = `i'+1
	local bname : word `i' of `bnames'
}


if `brows'>`borows' {
	* allow for xtra stats
	local borows1 = `borows'+1
	mat `b_xtra' = `b'[`borows1'...,1...]
	local bnames : rownames(`b_xtra')
	local beqs : roweq(`b_xtra')
	local bname : word 1 of `bnames'
	local beq : word 1 of `beqs'
	local i 1
	while "`bname'"!="" {
		if "`bname'"!="_cons" {
			if "`beq'"=="_" {
				local blabel "`bname'"
			}
			else {
				local blabel "`beq':`bname'"
			}
		}
		else {
			local blabel "`beq':Constant"
		}
		replace `varname' = `"`blabel'"' if `i'+`borows'==_n
		local i = `i'+1
		local bname : word `i' of `bnames'
		local beq : word `i' of `beqs'
	}
}

* keep data until after 'betaco'
keep if `beta'!=.

* get rid of original data since labels already accessed
keep `varname' `beta' `tstat' `st_err' `betcoef' 

* calculate asterisks for t stats (or standard errors)
local titlrow=0
if "`append'"=="append" & `fileExist'==1 {
	local appottl = 1
}
else {
	local appottl = `titlrow'
}

* either an appended column (not the first regression) or has a title
* i.e. need an extra line above the coefficients
* added a second extra line above the coefficients: place 1 of 2
gen `mrgrow' = 2*_n + 1 + `appottl' + 1


*** dealing with the asterisks
if "`aster'"!="noaster" {

	if "`alpha'"~="" {
		* parse ALPHA
		partxtl2 `"`alpha'"'
		local alphaCount = r(numtxt)
		local num=1
		while `num'<=`alphaCount' {
			local alpha`num' `r(txt`num')'
			capture confirm number `alpha`num''
			if _rc!=0 {
				noi di in red `"`alpha`num'' found where number expected in {opt alpha()} option"'
				exit 7
			}
		local num = `num'+1
		}
	}
	else {
		if "`2aster'"=="2aster" {
			local alpha1=.01
			local alpha2=.05
			local alphaCount=2
		}
		else {
			local alpha1=.01
			local alpha2=.05
			local alpha3=.10
			local alphaCount=3
		}
	}


	if `"`symbol'"'!="" {
		* parse SYMBOL
		partxtl2 `"`symbol'"'
		local symbolCount = r(numtxt)
		local num=1
		while `num'<=`symbolCount' {
			local symbol`num' `r(txt`num')'
			capture confirm number `symbol`num''
			if _rc==0{
				noi di in red `"`symbol`num'' found where non-number expected in {opt sym:bol()}"'
				exit 7
			}
		local num = `num'+1
		}
	}
	else {
		*** assume 2aster when only two alpha was given
		if "`2aster'"=="2aster" | `alphaCount'==2 {
			* 1 and 5 %
			local symbol1 "**"
			local symbol2 "*"
			local symbolCount=2
		}
		else {
			* 1, 5, and 10%
			local symbol1 "***"
			local symbol2 "**"
			local symbol3 "*"
			local symbolCount=3
		}
		* when only SYMBOL was given
		if "`alpha'"=="" {
			
			
		}
	}
	
	if "`alpha'"~="" & `"`symbol'"'~="" {
		if `symbolCount'~=`alphaCount' {
			di in red "{opt alpha()} and {opt sym:bol()} must have the same number of elements"
			exit 198
		}
	}

	if "`alpha'"=="" & `"`symbol'"'~="" {
		if `symbolCount'>=4 {
			di in red "{opt alpha()} must be specified when more than 3 symbols are specified with {opt sym:bol()}"
			exit 198
		}
	}

	if "`alpha'"~="" & `"`symbol'"'=="" {
		local symbolCount=`alphaCount'
		if `alphaCount'>=4 {
			di in red "{opt sym:bol()} must be specified when more than 3 levels are specified with {opt alpha()}"
			exit 198
		}
	}

	* fix the leading zero
	local num=1
	while `num'<=`alphaCount' {
		if index(trim("`alpha`num''"),".")==1 {
			local alpha`num'="0`alpha`num''"
		}
		local num=`num'+1
	}

	* creating the notes for the alpha significance
	local astrtxt `"`symbol1' p<`alpha1'"'
	local num=2
	while `num'<=`symbolCount' {
		local astrtxt `"`astrtxt', `symbol`num'' p<`alpha`num''"'
		local num=`num'+1
	}

	* assign the SYMBOL
	if "`pvalue'"=="pvalue" {
		gen str4 `astrix' = `"`symbol1'"' if (abs(`tstat')<`alpha1' & abs(`tstat')!=.)
		local num=2
		while `num'<=`symbolCount' {
			replace `astrix' = `"`symbol`num''"' if `astrix'=="" & (abs(`tstat')<`alpha`num'' & abs(`tstat')!=.)
			local num=`num'+1
		}
	}
	else {

/* used scalar earlier
		* taken out from tempname
		if `df_r'==. {
			scalar `tcrit1' = invnorm(0.995)
			scalar `tcrit5' = invnorm(0.975)
			scalar `tcrit10' = invnorm(0.95)
		}
		else {
			* replacement for invt( ) function under version 6
			* note the absolute sign: invttail is flipped from invnorm
			scalar `tcrit1' = abs(invttail(`df_r',0.995))
			scalar `tcrit5' = abs(invttail(`df_r',0.975))
			scalar `tcrit10' = abs(invttail(`df_r',0.950))
		}
*/

		if `df_r'==. {
			gen str4 `astrix' = `"`symbol1'"' if (abs(`tstat')>`=invnorm(1-`alpha1'/2)' & abs(`tstat')!=.)
			local num=2
			while `num'<=`symbolCount' {
				replace `astrix' = `"`symbol`num''"' if `astrix'=="" & (abs(`tstat')>`=invnorm(1-`alpha`num''/2)' & abs(`tstat')!=.)
				local num=`num'+1
			}
		}
		else {
			* replacement for invt( ) function under version 6
			* note the absolute sign: invttail is flipped from invnorm
			gen str4 `astrix' = `"`symbol1'"' if (abs(`tstat')>`=abs(invttail(`df_r',1-`alpha1'/2))' & abs(`tstat')!=.)
			local num=2
			while `num'<=`symbolCount' {
				replace `astrix' = `"`symbol`num''"' if `astrix'=="" & (abs(`tstat')>`=abs(invttail(`df_r',1-`alpha`num''/2))' & abs(`tstat')!=.)
				local num=`num'+1
			}
		}
	}
}
else {
	gen str2 `astrix' = ""
}

if "`paren'"!="noparen" {
	if "`bracket'"=="bracket" {
		local lparen "["
		local rparen "]"
	}
	else {
		local lparen "("
		local rparen ")"
	}
}
else {
	local lparen ""
	local rparen ""
}

if "`ci'"=="ci" {
	if `df_r'==. {
		scalar `t_alpha' = invnorm( 1-(1-`level' /100)/2 )
	}
	else {
		* replacement for invt( ) function under version 6
		* note the absolute sign: invttail is flipped from invnorm
		scalar `t_alpha' = abs(invttail(`df_r', (1-`level' /100)/2))
	}
}

* the fixed or floating specification
if `"`bfmt'"'=="" {
	local bfmt1 "fc"
	local bfmtcnt=1
}
else {
	* parse bfmt
	local fmttxt "e f g fc gc"
	partxtl2 `"`bfmt'"'
	local bfmtcnt = r(numtxt)
	local b = 1
	while `b'<=`bfmtcnt' {
		local bfmt`b' `r(txt`b')'
		if index("`fmttxt'","`bfmt`b''")==0 {
			di in red `"bfmt element "`bfmt`b''" is not a valid number format (f,fc,e,g or gc)"'
			exit 198
		}
	local b = `b'+1
	}
}

local bdeccnt : word count `bdec'

* this condition is always satisfied: `bfmtcnt'>=1
if `bdeccnt'>=1 {
	*** fill in bdec(#) & bfmt(txt)
	local b = 1
	while `b'<=_N {
		local bdec`b' : word `b' of `bdec'
		if "`bdec`b''"=="" {
			local bdec`b' = `prvbdec'
		}
		local prvbdec "`bdec`b''"
		local b = `b'+1
	}
	* bfmt1 is already set above
	local b = `bfmtcnt'+1
	while `b'<=_N {
		local b_1 = `b'-1
		local bfmt`b' "`bfmt`b_1''"
		local b = `b'+1
	}
}

* first put the t statistics (or se | ci| pvalue | beta) in `coefcol'

*** for the (parenthesis) numbers
if "`se'"!="se" & "`ci'"!="ci" & "`betaco'"!="betaco" {
	* pvalue is here
	gen str12 `coefcol' = ""
	forval num=1/`=_N' {
		autodigits2 `tstat'[`num'] `auto' `less'
		replace `coefcol' = string(`tstat',"%12.`r(valstr)'") in `num'
	}
}
else {
	if `bdeccnt'==0 & `bfmtcnt'==1 {
		* use autodigits because bdec was NOT given
		gen str12 `coefcol' = ""
		forval num=1/`=_N' {
			if "`ci'"=="ci" {
				if "`eform'"!="eform" {
					autodigits2 `=`beta'[`num']-`st_err'[`num']*`t_alpha'' `auto' `less'
					replace `coefcol' = string(`beta'-`st_err'*`t_alpha',"%12.`r(valstr)'") + " - " + string(`beta'+`st_err'*`t_alpha',"%12.`r(valstr)'") in `num'
				}
				else {
					autodigits2 `=exp(ln(`beta'[`num'])-`t_alpha'*`st_err'[`num']/ `beta'[`num'])' `auto' `less'
					replace `coefcol' = string(exp(ln(`beta')-`t_alpha'*`st_err'/ `beta'),"%12.`r(valstr)'") + " - " + string(exp(ln(`beta')+`t_alpha'*`st_err'/ `beta'),"%12.`r(valstr)'") in `num'
				}
			}
			else if "`betaco'"=="betaco" {
				autodigits2 `betcoef'[`num'] `auto' `less'
				replace `coefcol' = string(`betcoef',"%12.`r(valstr)'") in `num'
			}
			else {
				autodigits2 `st_err'[`num'] `auto' `less'
				replace `coefcol' = string(`st_err',"%12.`r(valstr)'") in `num'
			}
		}
	}
	else {

	* because bdec was given, use the fixed format
		local i 1
		gen str12 `coefcol' = ""
		while `i'<=_N {
			if "`ci'"=="ci" {
				if "`eform'"!="eform" {
					replace `coefcol' = string(`beta'-`t_alpha'*`st_err',"%12.`bdec`i''`bfmt`i''") + " - " + string(`beta'+`t_alpha'*`st_err',"%12.`bdec`i''`bfmt`i''") if `i'==_n
		  		  		}
				else {
					replace `coefcol' = string(exp(ln(`beta')-`t_alpha'*`st_err'/ `beta'),"%12.`bdec`i''`bfmt`i''") + " - " + string(exp(ln(`beta')+`t_alpha'*`st_err'/ `beta'),"%12.`bdec`i''`bfmt`i''") if `i'==_n
				}
			}
			else if "`betaco'"=="betaco" {
				replace str12 `coefcol' = string(`betcoef',"%12.`bdec`i''`bfmt`i''") if `i'==_n
			}
			else {
				replace `coefcol' = string(`st_err',"%12.`bdec`i''`bfmt`i''") if `i'==_n
			}
			local i = `i'+1
		}
	}
}


if "`coefastr'" != "coefastr" {
	replace `coefcol' = "`lparen'" + `coefcol' + "`rparen'" + `astrix'
}
else {
	replace `coefcol' = "`lparen'" + `coefcol' + "`rparen'"
}
sort `mrgrow'

save "`bcopy'", replace /* double quotes needed for Macintosh */
/* problems with spaces in file names, fixed Sep 2005 */


*** then offset the row number and put the coefficients in coefcol
replace `mrgrow' = `mrgrow'-1 

if `bdeccnt'==0 & `bfmtcnt'==1 {
	forval num=1/`=_N' {
		autodigits2 `beta'[`num'] `auto'
		replace `coefcol' = string(`beta',"%12.`r(valstr)'") in `num'
	}
}

else if `bdeccnt'==1 & `bfmtcnt'==1 {
	replace `coefcol' = string(`beta',"%12.`bdec'`bfmt1'")
}

else if `bdeccnt'>1 | `bfmtcnt'>1 {
	local i 1
	while `i'<=_N {
		replace `coefcol' = string(`beta',"%12.`bdec`i''`bfmt`i''") if `i'==_n
		local i = `i'+1
	}
}

if "`coefastr'" == "coefastr" {
	replace `coefcol' = `coefcol' + `astrix'
}
sort `mrgrow'
* interleave the coefficients with the t statistics
merge `mrgrow' using "`bcopy'"

replace `varname' = " " if _merge==2 /* no variable names next to tstats */
drop `beta' `st_err' `tstat' `astrix' `betcoef' _merge
sort `mrgrow'

* the coefficient and parenthesis combined
save "`bcopy'", replace

drop `varname' `coefcol'

* offset row numbers again to add header and other statistics
* first find number of new rows for addstat()
if `"`addstat'"'!="" {
	partxtl2 `"`addstat'"'
	local naddst = int((real(r(numtxt))+1)/2)
	local n = 1
	while `n'<=`naddst' {
		local t = (`n'-1)*2+1
		local astnam`n' `r(txt`t')'
		local t = `t'+1
		local astval`n' `r(txt`t')' /* pair: stat name & value */
		local n = `n'+1
	}
}
else {
	local naddst=0
}

* find number of new rows for addnote()
if (`"`addnote'"'!="" & "`append'"!="append") | (`"`addnote'"'!="" & `fileExist'==0) {
	partxtl2 `"`addnote'"'
	local naddnt = r(numtxt)
	local n = 1
	while `n'<=`naddnt' {
		local anote`n' `r(txt`n')'
			local n = `n'+1
	}
}
else {
	local naddnt=0
}

* calculate total number of rows in table
* added a second extra line above the coefficients: place 2 of 2
local coefrow = 2*`brows'+1+`appottl' + 1

* for ivreg2 type per Kit B.
*local totrows = `coefrow' + ("`nobs'"!="nonobs") + (`numi'!=.) + ("`r2'"!="nor2"&`rsq'!=.&`df_r'!=.) + `naddst' + ("`notes'"!="nonotes"&"`append'"!="append")*(1+("`aster'"!="noaster")) + `naddnt'
local totrows = `coefrow' + ("`nobs'"!="nonobs") + (`numi'!=.) + ("`r2'"!="nor2") + `naddst' + ("`notes'"!="nonotes"&"`append'"!="append")*(1+("`aster'"!="noaster")) + ("`notes'"!="nonotes"&`fileExist'==0)*(1+("`aster'"!="noaster")) + `naddnt'

local minobs = max(1000,`brows')
set obs `minobs' /* obs must be set to >= 100 (depending on matsize?) */
keep if _n <= `totrows'
replace `mrgrow' = _n
sort `mrgrow'

merge `mrgrow' using "`bcopy'"
sort `mrgrow'
drop _merge


* inserting column titles
if "`append'"=="append" & `fileExist'==1 {
	replace `coefcol' = `"`coltitl'"' if _n==2
	replace `coefcol' = `"`coltit2'"' if _n==3
}
else {
	replace `coefcol' = `"`coltitl'"' if _n==1
	replace `coefcol' = `"`coltit2'"' if _n==2
}

if "`nobs'"!="nonobs" {
	local coefrow = `coefrow'+1
	replace `varname' = "Observations" if _n==`coefrow'
	replace `coefcol' = string(`regN') if _n==`coefrow'
}
if (`numi'!=. & "`ni'"!="noni") {
	local coefrow = `coefrow'+1
	replace `varname' = "Number of " + rtrim(`"`iname'"') if _n==`coefrow'
	replace `coefcol' = string(`numi') if _n==`coefrow'
}


* the r2 is reported for ivreg2, per Kit B.
*if "`r2'"!="nor2" & `rsq'!=. & `df_r'!=. {
if "`r2'"!="nor2" {
	/* if df_r=., not true r2 */
	local coefrow = `coefrow'+1
	replace `coefcol' = string(`rsq',"%12.`rdec'f") if _n==`coefrow'
	replace `varname' = "R-squared" if _n==`coefrow'
	if "`adjr2'"=="adjr2" {
		replace `varname' = "Adjusted " + `varname' if _n==`coefrow'
	}
}

if `"`addstat'"'!="" {
	local i 1
	local adeccnt : word count `adec'
	while `i'<=`naddst' {
		local coefrow = `coefrow'+1
		local aadec : word `i' of `adec'
		if "`aadec'"=="" {
			local aadec `prvadec'
		}
		if `"`astval`i''"'!="" {
			replace `coefcol' = "`astval`i''" if _n==`coefrow'
		}
		replace `varname' = trim(`"`astnam`i''"') if _n==`coefrow'
		local i = `i'+1
		local prvadec `aadec'
	}
}
if ("`notes'"!="nonotes" & "`append'"!="append") | ("`notes'"!="nonotes" & `fileExist'==0) {
	local coefrow = `coefrow'+1
	if "`bracket'"=="bracket" {
		local par_bra "brackets"
	}
	else {
		local par_bra "parentheses"
	}
	if "`pvalue'"=="pvalue" {
		local statxt "p values"
	}
	else if "`se'"=="se" {
		local statxt "Standard errors"
	}
	else if "`ci'"=="ci" {
		local statxt "`level'% confidence intervals"
	}
	else if "`betaco'"=="betaco" {
		local statxt "Normalized beta coefficients"
	}
	else {
		if `df_r'!=. {
			local t_or_z "t"
		}
		else {
			local t_or_z "z"
		}
		local statxt "`t_or_z' statistics"
		if "`robust'"=="none" {
			local statxt "`statxt'"
		}
	}
	if "`robust'"=="Robust" {
		local statxt = "Robust " + lower("`statxt'")
	}
	replace `varname' = "`statxt' in `par_bra'" if _n==`coefrow'
	if "`aster'"!="noaster" {
		local coefrow = `coefrow'+1
		replace `varname' = "`astrtxt'" if _n==`coefrow'
	}
}
if (`"`addnote'"'!="" & "`append'"!="append") | (`"`addnote'"'!="" & `fileExist'==0) {
	local i 1
	while `i'<=`naddnt' {
		local coefrow = `coefrow'+1
		replace `varname' = `"`anote`i''"' if _n==`coefrow'
		local i = `i'+1
	}
}

* attach the column name
replace `varname'="COEFFICIENT" in 1

end		/* coeftxt2 */



***********************



program define seeing, rclass
version 8.2

quietly{
	* syntax using/[, Clear]
	syntax using

	preserve
	
	insheet `using', nonames clear
	describe, short
	
	* number of columns
	local numcol = r(k)	

	tempvar blanks rowmiss	
	count if v1=="EQUATION"
	if `r(N)'~=0 {
		count if v3=="LABELS"
		if `r(N)'~=0 {
			local num=4
		}
		else {
			local num=3
		}
	}
	else {
		count if v2=="LABELS"
		if `r(N)'~=0 {
			local num=3
		}
		else {
			local num=2
		}
	}

	gen int `blanks' = (trim(v`num')=="")
	forvalues col = `num'/`numcol' {
		replace `blanks' = `blanks' & (trim(v`col')=="")
	}
	
	* in case ctitle is missing: 2 of 2
	* colheadN

	replace `blanks'=0 if v1=="COEFFICIENT" | v1[_n-1]=="COEFFICIENT" ///
					| v2=="COEFFICIENT" | v2[_n-1]=="COEFFICIENT"
	
	* title rows
      local titlrow = 0 
      while `blanks'[`titlrow'+1] {
		local titlrow = `titlrow'+1
	}
	
	
	if `numcol'==2 {
		local colheadN=2+`titlrow' 
	}
	else {
		local colheadN=3+`titlrow'
	}
	
	* statistics rows
	count if `blanks'==0
	local strowN = `r(N)'+`titlrow'
	
	
	* move the notes and titles to the top of a new column
	gen str5 Notes_Titles=""
	format Notes_Titles %-20s 
	count if v1=="EQUATION"
	if `r(N)'==0 {
		* EQUATION column does not exist
		if `titlrow'>0 {
			forval num=1/`titlrow' {
				replace Notes_Titles=v1[`num'] in `num'
				replace v1="" in `num'
			}
		}
		
		local one = 1
		local legend = v1[`strowN'+`one']
		while "`legend'"~="" {
			local place=`strowN'+`one'
			local legend = v1[`place']
			replace Notes_Titles="`legend'" in `=`one'+`titlrow'+1'
			if "`legend'"~="" {
				replace v1="" in `place'
			}
			local one = `one'+1
		}
		* change the string length
		gen str5 temp=""
		replace temp=v1
		drop v1
		ren temp v1
		order v1
		* format
		foreach var of varlist v1 {
			local _format= "`: format `var''"
			local _widths=substr("`_format'",2,length(trim("`_format'"))-2)
			format `var' %-`_widths's
		}
	}
	else {
		* equation column exists
		if `titlrow'>0 {
			forval num=1/`titlrow' {
				replace Notes_Titles=v2[`num'] in `num'
				replace v2="" in `num'
			}
		}
		
		local one = 1
		local legend = v2[`strowN'+`one']
		while "`legend'"~="" {
			local place=`strowN'+`one'
			local legend = v2[`place']
			replace Notes_Titles="`legend'" in `=`one'+`titlrow'+1'
			if "`legend'"~="" {
				replace v2="" in `place'
			}
			local one = `one'+1
		}
		* change the string length
		gen str5 temp=""
		replace temp=v2
		drop v2
		ren temp v2
		order v1 v2
		* format
		foreach var of varlist v1 v2 {
			local _format= "`: format `var''"
			local _widths=substr("`_format'",2,length(trim("`_format'"))-2)
			format `var' %-`_widths's
		}
	}
	
	* clean up
	*egen `rowmiss'=rowmiss(_all)
	* rowmiss option not available in 8.2, do it by hand

	gen `rowmiss'=0
	foreach var of varlist _all {
		if "`var'"~="`rowmiss'" & "`var'"~="`blanks'" {
			replace `rowmiss'=1+`rowmiss' if `var'==""
		}
	}
	
	drop if `rowmiss'==`numcol'+1
	drop `blanks' `rowmiss'
	
	browse
	restore, preserve
}

end  /* end of seeing */



***********************


program define autodigits2, rclass
version 8.2
* getting the significant digits
args input auto less

if `input'~=. {
	local times=0
	local left=0

	* integer checked by modified mod function
	if round((`input' - int(`input')),0.0000000001)==0 {
		local whole=1
	}
	else {
		local whole=0
		* non-interger
		 if `input'<. {
			
			* digits that need to be moved if it were only decimals: take the ceiling of log 10 of absolute value of decimals
			local times=abs(int(ln(abs(`input'-int(`input')))/ln(10)-1))	
			
			* the whole number: take the ceiling of log 10 of absolute value
			local left=int(ln(abs(`input'))/ln(10)+1)
		}
	}
	
	
	* assign the fixed decimal values into aadec
	if `whole'==1 {
		local aadec=0
	}
	else if .>`left' & `left'>0 {
		* reduce the left by one if more than zero to accept one extra digit
		if `left'<=`auto' {
			local aadec=`auto'-`left'+1
		}
		else {
			local aadec=0
		}
	}
	else {
		local aadec=`times'+`auto'-1
	}

	if "`less'"=="" {
		* needs to between 0 and 11
		if `aadec'<0 {
			local aadec=0
		}
		if `aadec'<11 {
			* use fixed
			local valstr "`aadec'f"
		}
		else {
			* use exponential
			local valstr "`=`auto'-1'e"
		}
	}
	else {
		* needs to between 0 and 11
		local aadec=`aadec'-`less'
		if `aadec'<0 {
			local aadec=0
		}
		if `aadec'<10 {
			* use fixed
			local valstr "`aadec'f"
		}
		else {
			* use exponential
			local valstr "`=`auto'-1'e"
		}
	}

	return scalar value=`aadec'
	return local valstr="`valstr'"
}
else {
	* it is a missing value
	return scalar value=.
	return local valstr="missing"
}
end


****************


program define outreg2
version 8.2

	* separate the possible regression command from the outreg2 precommand
	
	tokenize `"`0'"', parse(" ,")
	
	* avoid the column in the file name by encasing it in quotes
	local 0 ""
	local count 1
	local countUsing=0

	while `"``count''"'~="" {
		if `"``count''"'=="using" {
			local countUsing=1

			*** clean up file name, attach .txt if no file type is specified
			*local rest "`using'"
			* strip off "using"
			*gettoken part rest: rest, parse(" ")
			
			local rest `"``=`count'+1''"'
			* strip off quotes
			gettoken first second: rest, parse(" ")
			local rest: list clean local(rest)

			* take off comma at the end
			*if index(`"`rest'"',",")==length(`"`rest'"',",") {
			*	local rest=substr(`"`rest'"',1,`=length(`"`rest'"')-1')
			*}
			
			* has no comma at the end
			local rabbit `"""'
			if index(`"`rest'"', ".")==0 {
				local `=`count'+1' `"`rabbit'`rest'.txt`rabbit'"'
			}
			else {
				local `=`count'+1' `"`rabbit'`rest'`rabbit'"'
			}
		}
		local 0 `"`0' ``count''"'
		local count=`count'+1
	}
	
	* check for the column location	
	gettoken first second : 0, parse(": ")
	while `"`first'"'~=":" & `"`first'"'~="" {
		local options "`options' `first'"
		gettoken first second : second, parse(":")
	}
	local command `"`second'"'
	
	* need to handle very long expression, did it the long way above with countUsing
	*if `"`second'"'=="" & index(`"`0'"', "using")~=0 {

	if `"`second'"'=="" & `countUsing'==1 {
		local second: subinstr local 0 "replace" "",all word count (local replace1)
		local second: subinstr local second "seeout" "",all word count (local seeout1)
		*local second: list uniq local(second)
		*local second: list retokenize second
		
		if `replace1' {
			local extraCmd "replace"
		}
		if `seeout1' {
			local extraCmd "seeout"
		}
		if `seeout1' & `replace1' {
			local extraCmd "replace seeout"
		}
		
		outreg2Main `0'
	}
	else {

		* need to handle very long expression, did it the long way above with countUsing
		*if index(`"`0'"', "using")~=0 {
		if `countUsing'==1 {
			di "{red}using not allowed in the shorthand syntax"
			exit 198
		}
		else {
			
			local 0 `"`options'"'
			syntax [anything] [,				 	///
				REPLACE						///
				SEEout]
			
			`command'
			
			*** read the set preference if not out of date
			
			* NOTE: `0' is written over below
			quietly findfile outreg2_prf.ado
			tempname myfile
			file open `myfile' using `"`r(fn)'"', read text
			file read `myfile' date
			file read `myfile' pref
			file read `myfile' options
			file close `myfile'
			
			* fix comma
			local comma ""
			if `"`macval(options)'"'~="" | "`replace'"~="" | "`seeout'"~="" {
				local comma ","
			}
			
			if "`date'"== "`c(current_date)'" {
				local seecommand "outreg2"
				local precommand "outreg2Main"
				foreach var in anything  macval(pref) comma macval(options) replace seeout {
					if `"``var''"'~="" {
						if `"``var''"'=="," {
							local seecommand `"`seecommand'``var''"'
							local precommand `"`precommand'``var''"'
						}
						else {
							local seecommand `"`seecommand' ``var''"'
							local precommand `"`precommand' ``var''"'
						}
					}
				}
				local cl `"{stata `"`seecommand'"':  `seecommand'}"'
				di as txt `"`cl'"'
				`precommand'
			}
			else {
				di in red "must specify the full syntax (the last preference has expired)"
				exit 100
			}
		}
	}
end /* end of outreg2 */



*******************


program define out2tex2, sortpreserve
* based on version 0.9 4oct01 by john_gallup@alum.swarthmore.edu
version 8.2

if "`1'" == "using" {
	syntax using/ [, Landscape Fragment noPRetty			///
		Fontsize(numlist integer max=1 >=10 <=12) noBorder Cellborder ///
		Appendpage noPAgenum a4 a5 b5 LETter LEGal EXecutive replace  ///
		Fast]
		
	if "`fast'" == "" {
		preserve
	}
	
	loadout using "`using'", clear
	local numcol	= `r(numcol)'
	local titlrow  = `r(titlrow)'
	local colheadN = `r(colheadN)'
	local strowN	= `r(strowN)'
	local totrows	= _N
	
	local varname "v1"
	unab statvars : v2-v`numcol'
	}
	
	else {
		syntax varlist using/, TItlrow(int) ColheadN(int) StrowN(int)		///
			[TOtrows(int 0) Landscape Fragment noPRetty				///
			Fontsize(numlist integer max=1 >=10 <=12) noBorder Cellborder	///
			Appendpage noPAgenum a4 a5 b5 LETter LEGal EXecutive replace]
		if `totrows'==0 {
			local totrows = _N
		}
		local numcols : word count `varlist'
		gettoken varname statvars : varlist
		local fast 1
	}
	
	local colhead1 = `titlrow' + 1
	local strow1 = `colheadN' + 1
	
	* insert $<$ to be handled in LaTeX conversion
	forval num=`strowN'/`=_N' {
		local temp=v1[`num']
		tokenize `"`temp'"', parse (" <")
		local count 1
		local newTex ""
		local noSpace 0
		while `"``count''"'~="" {
			if `"``count''"'=="<" {
				local `count' "$<$"
				local newTex `"`newTex'``count''"'
				local noSpace 1
			}
			else {
				if `noSpace'~=1 {
					local newTex `"`newTex' ``count''"'
				}
				else {
					local newTex `"`newTex'``count''"'					
					local noSpace 0
				}
			}
			local count=`count'+1
		}
		replace v1=`"`newTex'"' in `num'
	}
	
	*** replace if equation column present
	count if v1=="EQUATION"
	if `r(N)'~=0 {
		tempvar myvar
		* use v2 instead
		replace v1 = v2 in `=`strowN'+1'/`totrows'
		replace v2 = "" in `=`strowN'+1'/`totrows'
		
		* change the string length
		gen str5 `myvar' =""
		replace `myvar' =v2
		drop v2
		ren `myvar' v2
		order v1 v2
	}
	
	* if file extension specified in `using', replace it with ".tex" for output
	local beg_dot = index(`"`using'"', ".")
	if `beg_dot' {
		local using = substr("`using'",1,`=`beg_dot'-1')
	}
	
	local using `"using "`using'.tex""'
	local fsize = ("`fontsize'" != "")
	if `fsize' {
		local fontsize "`fontsize'pt"
	}
	local lscp = ("`landscape'" != "") 
	if (`lscp' & `fsize') {
		local landscape ",landscape"
	}
	local pretty	= ("`pretty'" == "")
	local cborder  = ("`cellborder'" != "")
	local noborder = ("`border'" != "")
	local nopagen  = ("`pagenum'" != "")
	local nofrag	= ("`fragment'" == "")
	
	if `cborder' & `noborder' {
		di in red "may not specify both cellborder and noborder options"
		exit 198
	}
	
	local nopt : word count `a4' `a5' `b5' `letter' `legal' `executive'
	if `nopt' > 1 {
		di in red "choose only one of a4, a5, b5, letter, legal, executive"
		exit 198 
	}
	local pagesize "`a4'`a5'`b5'`letter'`legal'`executive'"
	if "`pagesize'"=="" | "`letter'"!="" {
		local pwidth  "8.5in"
		local pheight "11in"
	}
	else if "`legal'"!="" {
		local pwidth  "8.5in"
		local pheight "14in"
	}
	else if "`executive'"!="" {
		local pwidth  "7.25in"
		local pheight "10.5in"
	}
	else if "`a4'"!="" {
		local pwidth  "210mm"
		local pheight "297mm"
	}
	else if "`a5'"!="" {
		local pwidth  "148mm"
		local pheight "210mm"
	}
	else if "`b5'"!="" {
		local pwidth  "176mm"
		local pheight "250mm"
	}
	if `lscp' {
		local temp	 "`pwidth'"
		local pwidth  "`pheight'"
		local pheight "`temp'"
	}
	if "`pagesize'"!="" {
		local pagesize "`pagesize'paper"
		if (`lscp' | `fsize') {
			local pagesize ",`pagesize'"
		}
	}
	if `cborder' & `noborder' {
		di in red "may not specify both cellborder and noborder options"
		exit 198
	}
	
	quietly {
		tempvar has_eqn st2_row last_st pad0 pad1 pad2_n padN order
		
		* replace % with \%, and _ with \_ if <2 $'s (i.e. not an inline equation: $...$
		* has_eqn indicates that varname has 2+ $'s
		
		gen byte `has_eqn' = index(`varname',"$")
		
		* make sure there are 2+ "$" in varname
		replace `has_eqn' = index(substr(`varname',`has_eqn'+1,.),"$")>0 if `has_eqn'>0
		replace `varname'= subinstr(`varname',"_", "\_", .) if !`has_eqn'
		replace `varname'= subinstr(`varname',"%", "\%", .)
		
		if `pretty' {
			replace `varname'= subinword(`varname',"R-squared", "\$R^2$", 1) in `strow1'/`strowN'
			replace `varname'= subinstr(`varname'," t stat", " \em t \em stat", 1) in `strowN'/`totrows'
			replace `varname'= subinstr(`varname'," z stat", " \em z \em stat", 1) in `strowN'/`totrows'
		}
		
		foreach svar of local statvars { /* make replacements for column headings rows of statvars */
			replace `has_eqn' = index(`svar',"$") in `colhead1'/`colheadN'
			replace `has_eqn' = index(substr(`svar',`has_eqn'+1,.),"$")>0 in `colhead1'/`colheadN' if `has_eqn'>0
			replace `svar'= subinstr(`svar',"_", "\_", .) in `colhead1'/`colheadN' if !`has_eqn'
			replace `svar'= subinstr(`svar',"%", "\%", .) in `colhead1'/`colheadN'
			
			/* replace <, >, {, }, | with $<$, $>$, \{, \}, and $|$ in stats rows */
			/* which can be used as brackets by outstat */
			replace `svar'= subinstr(`svar',"<", "$<$", .) in `strow1'/`strowN'
			replace `svar'= subinstr(`svar',">", "$>$", .) in `strow1'/`strowN'
			replace `svar'= subinstr(`svar',"{", "\{", .)  in `strow1'/`strowN'
			replace `svar'= subinstr(`svar',"}", "\}", .)  in `strow1'/`strowN'
			replace `svar'= subinstr(`svar',"|", "$|$", .) in `strow1'/`strowN'
		}
			
		if `pretty' {  /* make title fonts large; notes & t stats small */
			local blarge "\begin{large}"
			local elarge "\end{large}"
			local bfnsize "\begin{footnotesize}"
			local efnsize "\end{footnotesize}"
		}
		if `cborder' {
			local vline "|"
		} 
		gen str20 `pad0' = ""
		gen str20 `padN' = ""
		if `titlrow' {
			replace `pad0' = "\multicolumn{`numcols'}{`vline'c`vline'}{`blarge'" in 1 / `titlrow'
			replace `padN' = "`elarge'} \\\" in 1 / `titlrow'
		}
		if `strowN' < `totrows' {
			local noterow1 = `strowN' + 1
			replace `pad0' = "\multicolumn{`numcols'}{`vline'c`vline'}{`bfnsize'" in `noterow1' / l
			replace `padN' = "`efnsize'} \\\" in `noterow1' / l
		}
		
		gen str3 `pad1' = " & " in `colhead1' / `strowN'
		if `numcols' > 2 {
			gen str3 `pad2_n' = `pad1'
		}
		if `pretty' { /* make stats 2-N small font */
			local strow1 = `colheadN' + 1
			gen byte `st2_row' = 0
			replace `st2_row' = (trim(`varname') == "") in `strow1' / `strowN'	 /* only stats 2+ */
			gen byte `last_st' = (`st2_row' & `varname'[_n+1] != "")			 /* last stats row */
			if !`cborder' {
				replace `pad0'	= "\vspace{4pt}" if `last_st'
			}
				replace `pad1'	= `pad1' + "`bfnsize'" if `st2_row'
				if `numcols' > 2 {
					replace `pad2_n' = "`efnsize'" + `pad2_n' + "`bfnsize'" if `st2_row'
				}
				replace `padN'	= "`efnsize'" if `st2_row'
			}
		
			replace `padN' = `padN' + " \\\" in `colhead1' / `strowN'
			if `cborder' {
				replace `padN' = `padN' + " \hline"
			}
			else {
			if !`noborder' {
				if `colheadN' {
					if `titlrow' {
						replace `padN' = `padN' + " \hline" in `titlrow'
					}
					replace `padN' = `padN' + " \hline" in `colheadN'
				}
				replace `padN' = `padN' + " \hline" in `strowN'
			}
		}
		
		local vlist "`pad0' `varname' `pad1'"
		tokenize `statvars'
		local ncols_1 = `numcols' - 1
		local ncols_2 = `ncols_1' - 1
		forvalues v = 1/`ncols_2' {
			local vlist "`vlist' ``v'' `pad2_n'"
		}
		local vlist "`vlist' ``ncols_1'' `padN'"
		
		local texheadfootrows = `nofrag' + `pretty' + 1	/* in both headers and footers */ 
		local texheadrow = 2 * `nofrag' + `nopagen' + `texheadfootrows'
		local texfootrow = `texheadfootrows'
		local newtotrows = `totrows' + `texheadrow' + `texfootrow'
		if `newtotrows' > _N {
			local oldN = _N
			set obs `newtotrows'
		}
		else {
			local oldN = 0
		}
		gen long `order' = _n + `texheadrow' in 1 / `totrows'
		local newtexhrow1 = `totrows' + 1
		local newtexhrowN = `totrows' + `texheadrow'
		replace `order' = _n - `totrows' in `newtexhrow1' / `newtexhrowN'
		sort `order'
		
		
		* insert TeX header lines
		local ccc : display _dup(`ncols_1') "`vline'c"
		if `nofrag' {
			replace `pad0' = "\documentclass[`fontsize'`landscape'`pagesize']{article}" in 1
			replace `pad0' = "\setlength{\pdfpagewidth}{`pwidth'} \setlength{\pdfpageheight}{`pheight'}" in 2
			replace `pad0' = "\begin{document}" in 3
			replace `pad0' = "\end{document}" in `newtotrows'  
		}
		if `nopagen' {
			local row = `texheadrow' - 1 - `pretty'
			replace `pad0' = "\thispagestyle{empty}" in `row'
		}
		if `pretty' {
			local row = `texheadrow' - 1
			replace `pad0' = "\begin{center}" in `row'
			local row = `newtotrows' - `texfootrow' + 2
			replace `pad0' = "\end{center}"	in `row'
		}
		local row = `texheadrow'
		replace `pad0' = "\begin{tabular}{`vline'l`ccc'`vline'}" in `row'
		if (!`titlrow' | `cborder') & !`noborder' {
			replace `pad0' = `pad0' + " \hline" in `row'
		}
		local row = `newtotrows' - `texfootrow' + 1
		replace `pad0' = "\end{tabular}" in `row'

		noi outfile `vlist' `using' in 1/`newtotrows', `replace' runtogether

		* delete new rows created for TeX table, if any
		if `oldN' {
			keep in 1/`totrows'
		}
	} /* quietly */
end  /* end out2tex2 */



*******************


program define out2rtf2, sortpreserve rclass
* based on version 0.9 4oct01 by john_gallup@alum.swarthmore.edu
	if "`1'" == "using" {
		syntax using/ [, Landscape Fragment noPRetty				///
			Fontsize(numlist max=1 >0) noBorder Cellborder			///
			Appendpage PAgesize(string)						///
			Lmargin(numlist max=1 >=0.5) Rmargin(numlist max=1 >=0.5) 	///
			Tmargin(numlist max=1 >=0.5) Bmargin(numlist max=1 >=0.5) 	///
			replace Fast]
		if "`fast'" == "" {preserve}
		loadout using "`using'", clear
		local numcol	= `r(numcol)'
		local titlrow  = `r(titlrow)'
		local colheadN = `r(colheadN)'
		local strowN	= `r(strowN)'
		local totrows	= _N
		local varname "v1"
		unab statvars : v2-v`numcol'
	}
	else {
		syntax varlist using/, TItlrow(int) ColheadN(int) StrowN(int)	///
			[TOtrows(int 0) Landscape Fragment noPRetty			///
			Fontsize(numlist max=1 >0) noBorder Cellborder			///
			Appendpage PAgesize(string)						///
			Lmargin(numlist max=1 >=0.5) Rmargin(numlist max=1 >=0.5)	///
			Tmargin(numlist max=1 >=0.5) Bmargin(numlist max=1 >=0.5)	///
			replace]
		if `totrows'==0 {
			local totrows = _N
		}
		local numcols : word count `varlist'
		gettoken varname statvars : varlist
		local fast 1
	}
	
	local colhead1 = `titlrow' + 1
	local strow1 = `colheadN' + 1



	*** replace if equation column present
	local hack 0
	count if v1=="EQUATION"
	if `r(N)'~=0 {
		* use v2 instead
		replace v1 = v2 in `=`strowN'+1'/`totrows'
		replace v2 = "" in `=`strowN'+1'/`totrows'
		
		* change the string length
		gen str5 myvar =""
		replace myvar =v2
		drop v2
		ren myvar v2
		order v1 v2
		
		local hack 1
	}

	* if file extension specified in `using', replace it with ".rtf" for output
	local beg_dot = index("`using'", ".")
	if `beg_dot' {
		local using = substr("`using'",1,`=`beg_dot'-1')
	}
	local using `"using "`using'.rtf""'
	return local documentname `"`using'"'
	
	if "`fontsize'" == "" {
		local fontsize "11"
	}

	local lscp = ("`landscape'" != "") 
	local pretty	= ("`pretty'" == "")
	local cborder  = ("`cellborder'" != "")
	local noborder = ("`border'" != "")
	local stdborder = (!`noborder' & !`cborder')
	local nopagen  = ("`pagenum'" != "")
	local nofrag	= ("`fragment'" == "")
	
	
	if `cborder' & !`noborder' {
		di in red "may not specify both cellborder and noborder options"
		exit 198
	}
	
	* reformat "R-squared" and italicize "t" or "z"
	if `pretty' {
		quietly {
			replace `varname'= subinword(`varname',"R-squared", "{\i R{\super 2}}", 1) in `strow1'/`strowN'
			replace `varname'= subinstr(`varname'," t stat", " {\i t} stat", 1) in `strowN'/`totrows'
			replace `varname'= subinstr(`varname'," z stat", " {\i z} stat", 1) in `strowN'/`totrows'
		}
	}
	
	* font sizes in points*2
	local font2 = int(`fontsize'*2)
	if `pretty' {
		/* make title fonts large; notes & t stats small */
		local fslarge = "\fs" + string(int(`font2' * 1.2))
		local fsmed	= "\fs" + string(`font2')
		local fssmall = "\fs" + string(int(`font2' * 0.8))
		local sa0 "\sa0"	/* put space after t stats rows */
		local gapsize = int(`fontsize'*0.4*20)  /* 40% of point size converted to twips */
		local sa_gap "\sa`gapsize'"
	}
	else {
		local fs0 = "\fs" + string(`font2')
	}
	
	local onecolhead = (`colheadN' - `titlrow' == 1)
			/* onecolhead = true if only one row of column headings */
	if `stdborder' {
		if !`onecolhead' {
			* runs here
			*local trbrdrt "\clbrdrt\brdrs"	/* table top is overlined */
			*local trbrdrt "\trbrdrt\brdrs"	/* table top is overlined */
			local clbrdr_ul "\clbrdrb\brdrs"	/* cells are underlined */
		}
		else {
			/* cells are over- and underlined */
			local clbrdr_ul "\clbrdrt\brdrs\clbrdrb\brdrs"
		
		}
		local trbrdrb "\trbrdrb\brdrs"
	}
	if `cborder' {
		/* if !cborder then clbrdr is blank */
		local clbrdr "\clbrdrt\brdrs\clbrdrb\brdrs\clbrdrl\brdrs\clbrdrr\brdrs"
	}
	
	* figure out max str widths to make cell boundaries
	* cell width in twips = (max str width) * (pt size) * 12
	* (12 found by trial and error)
	local twipconst = int(`fontsize' * 12 )
	tempvar newvarname
	qui gen str80 `newvarname' = `varname' in `strow1'/`strowN'
	
	local newvarlist "`newvarname' `statvars'"
	qui compress `newvarlist'
	local cellpos = 0
	foreach avar of local newvarlist {
		local strwidth : type `avar'
		local strwidth = subinstr("`strwidth'", "str", "", .)
		local strwidth = `strwidth' + 1  /* add buffer */
		local cellpos = `cellpos' + `strwidth'*`twipconst'

		* hacking
		if `hack'==1 & "`avar'"=="`newvarname'" & `cellpos'<1270 {
			local cellpos=1270
		}
		local clwidths "`clwidths'`clbrdr'\cellx`cellpos'"
		* put in underline at bottom of header in clwidth_ul
		local clwidth_ul "`clwidth_ul'`clbrdr_ul'\cellx`cellpos'"
	}
	
	if `stdborder' {
		if `onecolhead' {
			local clwidth1 "`clwidth_ul'"
		}
		else {
			local clwidth1 "`clwidths'"
			local clwidth2 "`clwidth_ul'"
		}
		local clwidth3 "`clwidths'"
	}
	else{
		local clwidth1 "`clwidths'"
	}
	
	* statistics row formatting
	tempvar prettyfmt
	qui gen str12 `prettyfmt' = ""  /* empty unless `pretty' */
	if `pretty' {
		* make stats 2-N small font
		tempvar st2_row last_st
		quietly {
			gen byte `st2_row' = 0
			replace `st2_row' = (trim(`varname') == "") in `strow1' / `strowN'	 /* only stats 2+ */
			gen byte `last_st' = (`st2_row' & `varname'[_n+1] != "")			 /* last stats row */
			replace `prettyfmt' = "`sa0'" in `strow1' / `strowN'
			replace `prettyfmt' = "`sa_gap'"  if `last_st' in `strow1' / `strowN'
			replace `prettyfmt' = `prettyfmt' + "`fsmed'" if !`st2_row' in `strow1' / `strowN'
			replace `prettyfmt' = `prettyfmt' + "`fssmall'"  if `st2_row' in `strow1' / `strowN'
		}
	}
	
	* create macros with file write contents
	
	forvalues row = `colhead1'/`strowN' { 
		local svarfmt`row' `"(`prettyfmt'[`row']) "\ql " (`varname'[`row']) "\cell""'
		foreach avar of local statvars {
			local svarfmt`row' `"`svarfmt`row''"\qc " (`avar'[`row']) "\cell""' 
		}
		local svarfmt`row' `"`svarfmt`row''"\row" _n"'
	}
	
	* write file
	tempname rtfile
	file open `rtfile' `using', write `replace'
	file write `rtfile' "{\rtf1`fs0'" _n  /* change if not roman: \deff0{\fonttbl{\f0\froman}} */
	
	if `titlrow' {
		file write `rtfile' "\pard\qc`fslarge'" _n
		forvalues row = 1/`titlrow' {
			file write `rtfile' (`varname'[`row']) "\par" _n
		}
	}
	
	
	file write `rtfile' "\trowd\intbl\trqc`fsmed'`trbrdrt'`clwidth1'" _n
	
	if !`onecolhead' {
		* here
		*added:
		*file write `rtfile' "\trowd\trqc`clwidth2'" _n
		
		local colheadN_1 = `colheadN' - 1
		* write header rows 1 to N-1
		forvalues row = `colhead1'/`colheadN_1' {
			file write `rtfile' `svarfmt`row''
		}
		if `stdborder' {
			file write `rtfile' "\trowd\trqc`clwidth2'" _n
		}
	}
	* write last header row
	file write `rtfile' `svarfmt`colheadN''

	* write stat rows 1 to N-1 if stborder, else write all stat rows
	if `stdborder' {
		local strowNN_1 = `strowN' - 1
		/* turn off cell underlining */
		file write `rtfile' "\trowd\trqc`clwidth3'" _n
	}
	else {
		local strowNN_1 = `strowN'
	}
	
	forvalues row = `strow1'/`strowNN_1' {
		file write `rtfile' `svarfmt`row''
	}
	if `stdborder' {
		/* write last row */
		file write `rtfile' "\trowd\trqc`trbrdrb'`clwidths'" _n
		file write `rtfile' `svarfmt`strowN''
	}
	
	/* write notes rows */
	if `strowN' < `totrows' {
		local noterow1 = `strowN' + 1
		file write `rtfile' "\pard\qc`fssmall'" _n
		forvalues row = `noterow1'/`totrows' {
			file write `rtfile' (`varname'[`row']) "\par" _n
		}
	}
	
	* write closing curly bracket
	file write `rtfile' "}"
end  /* end out2rtf2 */






