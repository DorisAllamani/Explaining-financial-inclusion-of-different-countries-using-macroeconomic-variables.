 drop _all
 clear
 
 import excel "C:\Users\Asus\Downloads\fin data.xlsx", sheet("Data")firstrow
 
 
 kdensity finfree, normal name(g17) legend(off)
 kdensity internet, normal name(g5) legend(off)
 kdensity urb, normal name(g6)  legend(off)
 kdensity gdp, normal name(g1) legend(off)
 kdensity fincl, normal name(g2) legend(off)
 kdensity lit, normal name(g3) legend(off)
 kdensity gini, normal name(g4) legend(off)
 kdensity banks, normal name(g7) legend(off)
 kdensity gvr, normal name(g8) legend(off)
 kdensity regu, normal name(g11) legend(off)
 kdensity law, normal name(g12) legend(off)
 kdensity hci, normal  name(g15) legend(off)
 kdensity finlit, normal name(g16) legend(off)
 kdensity busfree, normal name(g18) legend(off)
 kdensity Unem, normal name(g19) legend(off)
 graph combine g1 g2 g3 g4 g7 g8 g11 g12 g18 g19 g16 g15 g17 g5 g6

  

*given that some distribution do not resemble the normal distribution 
*the distributions are GDP, lit, internet, unem, finfree, gini
 gen loggini= log(gini)
 gen loglit= log(lit)
 gen loggdp= log(gdp)
 gen logunem = log(Unem)
 kdensity loggdp, normal name(f1) legend(off)
 kdensity loglit, normal name(f2) legend(off)
 kdensity loggini, normal name(f3) legend(off)
 kdensity logunem, normal name (f4) legend(off)
 graph combine f1 g1 f2 g3 f3 g4 f4 g19, col(4) xsize(20) ysize(8) 

* looking at the attemps of fitting the data to a normal through a log transformation for the varibles such as gini, gdp, unemployment was succeful
 
twoway scatter fincl loggdp || (lfit fincl loggdp), name(m1) legend(off)
twoway scatter fincl lit || (lfit fincl lit), name(m2) legend(off)
twoway scatter fincl loggini || (lfit fincl loggini), name(m3) legend(off)
twoway scatter fincl internet || (lfit fincl internet), name(m4) legend(off)
twoway scatter fincl urb || (lfit fincl urb), name(m5) legend(off)
twoway scatter fincl banks || (lfit fincl banks), name(m6) legend(off)
twoway scatter fincl gvr || (lfit fincl gvr), name(m7) legend(off)
twoway scatter fincl regu || (lfit fincl regu), name(m9) legend(off)
twoway scatter fincl law || (lfit fincl law), name(m11) legend(off)
twoway scatter fincl logunem || (lfit fincl logunem), name(m12) legend(off)
twoway scatter fincl busfree || (lfit fincl busfree), name(m13) legend(off)
twoway scatter fincl hci || (lfit fincl hci), name(m14) legend(off)
twoway scatter fincl finlit || (lfit fincl finlit), name(m15) legend(off)
twoway scatter fincl finfree || (lfit fincl finfree), name(m16) legend(off)

graph combine m1 m2 m3  m4 m5 m6 m7  m9 m11 m12 m13 m14 m15, name(m17)  

**********************************************************************

**Continuous varibles descriptive statistics 

summarize  fincl lit loggdp loggini internet banks gvr regu law hci finlit finfree urb logunem busfree

**Categorical varibles - creating dummies****************************

tabulate income, gen(income)

tabulate incomegr, gen(i)
graph pie i1 i2 i3 i4, plabel(1 percent) plabel(2 percent) plabel(3 percent) plabel(4 percent)
 graph hbar fincl, over(incomegr, label(labsize(small)) sort(1) descending) name(t6) 
 graph hbar busfree, over(incomegr, label(labsize(small)) sort(1) descending) name(t7)
 graph hbar finfree, over(incomegr, label(labsize(small)) sort(1) descending) name(t8)
 graph hbar banks, over(incomegr, label(labsize(small)) sort(1) descending) name(t9)
graph combine t7 t6 t8 t9


**First regression**************************************************

 reg fincl lit loggdp loggini internet banks gvr regu law hci finlit finfree urb logunem busfree income2 income3 income4  

** Outliers step2 ***************************************************
 rvfplot , mlabel(country) name(o1)
 *looking on thr graph we can se that the Kenya, Iran could be outliers as their residual is larger than the most of the the other observation
 predict  predfincl
 predict residfincl, residuals
 list fincl predfincl residfincl in 1/20
 kdensity residfincl, normal
 sktest residfincl
 *from the test of normality we can see that assympotitacally they are normal 
 * Generally, a point with leverage greater than (2k+2)/n should be carefully examined.
display (2*22+2)/144
** 0.319444

lvr2plot , mlabel(country) name(o2)
predict lev, leverage
list country if lev > 0.3914
drop   if lev > 0.3914
 
graph combine o1 o2, xsize(25) ysize(40) col(1)
  
*standartizing the residuals  
egen stdres = std(residfincl)
kdensity stdres, normal
*deleting the outliers larger than 3 standart deviation
list country if abs(stdres) > 3
drop if abs(stdres) > 3
**Multicolinearity / Omitting the varibles***********************
 reg fincl lit loggdp loggini internet banks gvr regu law hci finlit finfree urb logunem busfree income2 income3 income4  


vif

correlate regu gvr law loggdp internet 
* we can se clearly that the pearson correlation is high telling us that these explanatory varibles are positevely highly correlated correlated
* given that the p-values for loggdp loggini internet gvr regu hci logunem is greater than 5 %, we will run a fisher test to se if we can drop
* as by the vif we are going to keep only law variable
 test loggdp loggini internet gvr regu hci logunem urb
 *we fail to reject the null hypothesis, we ommit these variables
 
reg fincl lit banks law finlit finfree busfree income2 income3 income4  

 
**Checking for heteroskedasticity *********************************
* as Breusch–Pagan/Cook–Weisberg test for heteroskedasticity Assumption: Normal error terms/// we will check if our residuals are normal 

 predict  pred
 predict res, residuals
 kdensity res, normal
 sktest res

 estat hettest
* given that p-value is greater than 5% with the outliers  there is homoscadacity in our model 
*whitest works better through large samples but nethertheless we are going to run the test
estat imtest, white 
* we fail to reject the null hypothesis hence our model is hoscedastic
ssc install whitetst
whitetst
**Final model ******************************************************

reg fincl lit banks law finlit finfree busfree income2 income3 income4 







**Another alternative**************************************
clear 

 import excel "C:\Users\Asus\Downloads\fin data.xlsx", sheet("Data")firstrow
 
** without deleting the outliers
 *** we are going to run a stepwise regression backward selection 
 stepwise, pr(.05): regress  fincl lit loggdp loggini internet banks gvr regu law hci finlit finfree urb logunem busfree (i.income)
 **** (i.income) has entered the regression as a block


* testing for homoscedascity
 predict  pred
 predict res, residuals
 kdensity res, normal
 sktest res
 estat hettest
 estat imtest, white 
* we have homoscedascity  

* if it was the case of heteroskedasticity, we would simple run the stata line as below to correct the variances of our estimates
* reg fincl lit banks law finlit finfree busfree income2 income3 income4, robust

