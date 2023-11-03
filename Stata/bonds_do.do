//Financial markets and products assignment
//Federica Bosio, Nizar Fatihi, Silvia Giacobazzi, Sofia Gioacchini, Vittoria Ricci

clear all
capture log close
log using "Finance_log", text replace
cd "/Users/sofiagioacchini/Library/CloudStorage/OneDrive-Personale/Documenti/LMEC/Finance I.C./Financial products and markets/Problem set"
import excel "Bonds.xlsx", sheet ("Search Results") cellrange (A1:M31371) firstrow clear


//We destring the variables of interest.  
encode Issuer, gen (issuer_n) 
encode GreenBond, gen (Green_Bond) 
encode Seniority, gen (seniority_n) 
encode Sector, gen (sector_n) 
encode CountryofIssue, gen (countryofissue_n)
encode PrincipalCurrency, gen (currency_n)
gen ytm = real(YieldtoMaturity)
gen ramount = real(AmountIssuedUSD)
gen rcoupon = real(Coupon)
gen year=year(IssueDate)
gen matyears=(Maturity-IssueDate)/365 
gen logamt = log(ramount)
//we create a new variable which is the difference between maturity and issue date.
tab issuer_n, miss


//We want to know how many observations are repeated how many times. 
egen issuer_group = group (issuer_n)
sum issuer_group //there are 8764
duplicates report issuer_n //it tells how many observations are repeated how many times.

bysort issuer_n: gen id = _n //it assigns a number to each observation within a given issuer. 

//we drop missing values  
drop if missing(ytm) 

//We eliminate ytm outliers. 
cumul ytm, generate(freq) eq
drop if freq >.99
drop if freq <.01
 
// we create dummy Green_type from the variable GreenBond that has "Yes" for green bonds and "No" for brown bonds.  1 = green 0= brown
generate Green_type = 0
replace Green_type = 1 if GreenBond == "Yes"

//we drop all obs with dummy mean equal to 1 or 0, in order to keep only issuers which have issued both brown and green bonds. 
bysort issuer_n: egen mean_dummy = mean(Green_type) //we compute the mean of the dummy variable within each issuer
drop if mean_dummy == 1 
drop if mean_dummy == 0

graph pie, over(Green_type) plabel(_all name) title (Corporate bonds)
gr_edit plotregion1.pieslices[2].style.editstyle shadestyle(color(dkgreen)) editcopy
gr_edit plotregion1.pieslices[2].style.editstyle linestyle(color(dkgreen)) editcopy
gr_edit plotregion1.pieslices[1].style.editstyle shadestyle(color(khaki)) editcopy
gr_edit plotregion1.pieslices[1].style.editstyle linestyle(color(khaki)) editcopy
gr_edit plotregion1.pielabel[2].text = {}
gr_edit plotregion1.pielabel[2].text.Arrpush Green
gr_edit plotregion1.pielabel[1].text = {}
gr_edit plotregion1.pielabel[1].text.Arrpush Non-green
gr_edit legend.plotregion1.label[1].text = {}
gr_edit legend.plotregion1.label[1].text.Arrpush Non-green bonds
gr_edit legend.plotregion1.label[2].text = {}
gr_edit legend.plotregion1.label[2].text.Arrpush Green bonds
graph export "bonds1.png", replace 

graph pie, over(sector_n) title("Sectors") legend(size(tiny) position(right))
graph export "sectors.png", replace

//-----------FIXED EFFECT REGRESSION-----------//

//interaction issuer green type has as a result p-values higher than 10%
//issuer alone has all ommitted variables 
// if interact country of issue and Green Bond obtain only one missing value. all p values higher than 5%
//interaction between issuer and country not possible 

xtset issuer_n id
xtdescribe 

xtreg ytm Green_type logamt rcoupon i.seniority matyears i.year, robust fe
est store regFE


//--------REGRESSIION WITH GROUPING-----------//

///---YIELD TO MATURITY---///

// divide ytm 
tab ytm
generate ytm_group = 0
replace ytm_group = 1 if ytm < 2.5
replace ytm_group = 2 if ytm >= 2.5 & ytm < 5
replace ytm_group = 3 if ytm >= 5
*mainly ytm in group = 2

xtset issuer_n id

//consider group 1: not significant
preserve
keep if ytm_group == 1
xtreg ytm Green_type logamt rcoupon i.seniority matyears i.year, robust fe
est store regytm1
restore


//consider group 2: not significant
preserve
keep if ytm_group == 2
xtreg ytm Green_type logamt rcoupon i.seniority matyears i.year, robust fe
est store regytm2
restore


//consider group 3: not significant
preserve
keep if ytm_group == 3
xtreg ytm Green_type logamt rcoupon i.seniority matyears i.year, robust fe
est store regytm3
restore


///---YEAR---///

xtset issuer_n id

//Let's consider only year 2019: not significant
preserve
keep if year == 2019
xtreg ytm Green_type logamt rcoupon i.seniority matyears i.year, robust fe
est store regy19
restore


//Let's consider only year 2020: not significant
preserve
keep if year == 2020
xtreg ytm Green_type logamt rcoupon i.seniority matyears i.year, robust fe
est store regy20
restore

//Let's consider only year 2021: not significant
preserve
keep if year == 2021
xtreg ytm Green_type logamt rcoupon i.seniority matyears i.year, robust fe
est store regy21
restore

//Let's consider only year 2022: not significant
preserve
keep if year == 2022
xtreg ytm Green_type logamt rcoupon i.seniority matyears i.year, robust fe
est store regy22
restore

//Let's consider only year 2023: significant at 5% level! but only 112 bonds
preserve
keep if year == 2023
xtreg ytm Green_type logamt rcoupon i.seniority matyears i.year, robust fe
est store regy23
restore

count if year == 2023


//TABLE1: fixed effectsy with ytm
esttab regFE regytm1 regytm2 regytm3 using "Table1.tex" , label title ("Regression with fixed effects grouping by yield to maturity") nonumbers mtitles ("Standard Fixed Effects" "FE with ytm < 2.5%" "FE with ytm >= 2.5% & < 5%" "FE with ytm >= 5%") star(* 0.10 * 0.05 ** 0.01) varlabel (Green_type "Green Bond" logamt "Amount Issued" i.seniority "Bond Seniority" matyears "Maturity Years" i.years "Year" _cons "Constant" )


//TABLE 2: fixed effects over years
esttab regFE regytm1 regytm2 regytm3 using "Table2.tex" , label title ("Regression with fixed effects grouping by year") nonumbers mtitles ("Standard Fixed Effects" "FE 2019" "FE 2020" "FE 2021" "FE 2022" "FE 2023") star(* 0.10 * 0.05 ** 0.01) varlabel (Green_type "Green Bond" logamt "Amount Issued" i.seniority "Bond Seniority" matyears "Maturity Years" i.years "Year" _cons "Constant" )


//-------REGRESSION WITH MATCHING DISTANCE---------//
//ssc install psmatch2 
xtset issuer_n id
xtdescribe 

xtreg ytm Green_type logamt rcoupon i.seniority matyears i.year Green_type#sector_n, robust fe


global X rcoupon currency_n countryofissue_n logamt seniority_n sector_n year Maturity matyears
global Y ytm 
psmatch2 $treatment $X, outcome($Y) logit 
drop if _weight==. 
gen WM = _weight*ytm 

ttest WM, by (Green_type)


//-------CEMS MATCHING METHOD-----------//

// how imbalanced is the treatment group?
imb year matyears rcoupon logamt, treatment(Green_type)
//Multivariate L1 distance: 0.65   imbalanced (from 0 to 1)


ssc install cem
// perform matching, accounting for maturity years, year of issuance, currency of bonds, coupon
cem rcoupon (#0) matyears (#0), treatment(Green_type)

ssc install outreg2
outreg2 using "reg_results.doc", replace

// degree of imbalanced reduced to almost zero. 
//After conducting the cem, we now have three new variables in our dataset. cem_matched is simply an indicator of which cases were matched versus unmatched. The most important variable is cem_weights. Unless you chose to do a k2k match, it will be crucial to use the weights in your analyses. However, you must use these as iweights. So, now let's examine the effect of issuing green bonds both before matching and after matching:

reg ytm Green_type i.seniority_n i.issuer_n i.countryofissue_n 
reg ytm Green_type i.seniority_n i.issuer_n i.countryofissue_n [iw=cem_weights]

reg ytm Green_type 
reg ytm Green_type  [iw=cem_weights]


//The second solution is the one that incorporates the matching, through the implementation of the weights. The weights created by cem are to provide the most efficient use of the data possible by using as many observations as possible. However, if you have a large number of observations and wish to simplify your analysis, you can to k2k matching, which means pairs will be created so that for each firm in the control group there is just one match in the treatment group (and vice versa). To do this, simply add the k2k option after the comma in the cem command. And then, limit your subsequent analyses only to those who are identified as having a match by the cem_matched variable:

cem matyears (#0) year (#0) rcoupon (#0) logamt (#0), treatment(Green_type) k2k
reg ytm Green_type if cem_matched==1

