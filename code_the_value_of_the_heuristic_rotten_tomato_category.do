*The Value of the Heuristic: Rotten Tomato Category

clear

cd "/Users/MusicMelody/Documents/Econ641/"

capture log close

log using 641FinalProject, t replace

use "/Users/MusicMelody/Documents/Econ641/Econ641Project.dta"

label values genre genre_label

tab tomatometer_status

rename tomatometer_status Tomatometer_Status

gen byte romance = (genre==11)

gen byte action = (genre==0)

gen byte comedy = (genre==4)

gen byte animation = (genre==1)

gen byte arthouse = (genre==2)

gen byte classics = (genre==3)

gen byte documentary = (genre==5)

gen byte drama = (genre==6)

gen byte horror = (genre==7)

gen byte kidsfam = (genre==8)

gen byte musicpa = (genre==9)

gen byte mystsusp = (genre==10)

gen byte scififant = (genre==12)

gen byte western = (genre==13)

gen byte PG13 = (rating==3)

gen byte R = (rating==4)

gen byte PG = (rating==2)

DCdensity tomatometer_rating if (tomatometer_rating>0 & tomatometer_rating<1), breakpoint(0.6) generate(Xj Yj r0 fhat se_fhat)

graph export "641_rd_density.png", replace

rddensity tomatometer_rating, c(0.6)

ssc install asdoc, replace

asdoc sum, replace label

ssc install rdrobust, replace

ssc install cmogram, replace

cmogram lnopening tomatometer_rating if tomatometer_rating <1, cut(0.6) scatter line(0.6) lowess 

graph export "641_rd_plot.png", replace

*** Lienar RD with different bandwidths ***

gen x_c 	= tomatometer_rating - .6
gen x_c2 	= x_c^2
gen x_c_S 	= x_c*Tomatometer_Status
gen x_c2_S 	= x_c2*Tomatometer_Status

label variable x_c "TomaRate"
label variable x_c2 "TomaRate²"
label variable x_c_S "TomaRate x TomaStat"
label variable x_c2_S "TomaRate² x TomaStat"


reg lnopening Tomatometer_Status x_c x_c_S
	eststo reg1

reg lnopening Tomatometer_Status x_c x_c_S if abs(x_c) < 0.05
	eststo reg2

reg lnopening Tomatometer_Status x_c x_c_S if abs(x_c) < 0.1
	eststo reg3
	
reg lnopening Tomatometer_Status x_c x_c_S if abs(x_c) < 0.2
	eststo reg4

reg lnopening Tomatometer_Status x_c x_c_S if abs(x_c) < 0.25
	eststo reg5

esttab reg1 reg2 reg3 reg4 reg5 using "/Users/MusicMelody/Documents/Econ641/Linear_RD_1", ///
	mtitles("All" "h = 0.05" "h = 0.1" "h = 0.2" "h = 0.25") keep(Tomatometer_Status x_c x_c_S) ///
	se r2 star(* 0.10 ** 0.05 *** 0.01) label replace	

	*** Quadratic RD with different bandwidths
estimates clear

reg lnopening Tomatometer_Status x_c x_c2 x_c_S x_c2_S
	eststo reg1

reg lnopening Tomatometer_Status x_c x_c2 x_c_S x_c2_S if abs(x_c) < 0.05
	eststo reg2

reg lnopening Tomatometer_Status x_c x_c2 x_c_S x_c2_S if abs(x_c) < 0.1
	eststo reg3
	
reg lnopening Tomatometer_Status x_c x_c2 x_c_S x_c2_S if abs(x_c) < 0.2
	eststo reg4	

reg lnopening Tomatometer_Status x_c x_c2 x_c_S x_c2_S if abs(x_c) < 0.25
	eststo reg5

esttab reg1 reg2 reg3 reg4 reg5 using "/Users/MusicMelody/Documents/Econ641/Quadratic_RD_1", ///
	mtitles("All" "h = 0.05" "h = 0.1" "h = 0.2" "h = 0.25") keep(Tomatometer_Status x_c x_c2 x_c_S x_c2_S) ///
	se r2 star(* 0.10 ** 0.05 *** 0.01) label replace	

// gen com=1 if genre==4
 
//reg com tomatometer_status x_c x_c_S franchise, robust
estimates clear

reg lnopening Tomatometer_Status x_c x_c_S comedy action i.year franchise PG PG13 R
	eststo reg1

reg lnopening Tomatometer_Status x_c x_c_S comedy action i.year franchise PG PG13 R if abs(x_c) < 0.05
	eststo reg2

reg lnopening Tomatometer_Status x_c x_c_S comedy action i.year franchise PG PG13 R if abs(x_c) < 0.1
	eststo reg3
	
reg lnopening Tomatometer_Status x_c x_c_S comedy action i.year franchise PG PG13 R if abs(x_c) < 0.2
	eststo reg4

reg lnopening Tomatometer_Status x_c x_c_S comedy action i.year franchise PG PG13 if abs(x_c) < 0.25
	eststo reg5

esttab reg1 reg2 reg3 reg4 reg5 using "/Users/MusicMelody/Documents/Econ641/RD_Controls", ///
	mtitles("All" "h = 0.05" "h = 0.1" "h = 0.2" "h = 0.25") keep(Tomatometer_Status x_c x_c_S) ///
	se r2 star(* 0.10 ** 0.05 *** 0.01) label replace
	
	
	***Kernel density estimator
	
net install rdrobust, from(https://sites.google.com/site/rdpackages/rdrobust/stata) replace

rdrobust lnopening x_c	
local obs = e(h_r) + e(N_h_r) // Collect Obs number
local bw = e(h_r)

outreg2 using RDrobust.doc, adds(Bandwidth, `bw', Obs, `obs') replace

rdrobust lnopening x_c, h(.05 .05)	
local obs = e(h_r) + e(N_h_r) // Collect Obs number
local bw = e(h_r)

outreg2 using RDrobust.doc, adds(Bandwidth, `bw', Obs, `obs') append

rdrobust lnopening x_c, h(.1 .1)	
local obs = e(h_r) + e(N_h_r) // Collect Obs number
local bw = e(h_r)

outreg2 using  RDrobust.doc, adds(Bandwidth, `bw', Obs, `obs') append

rdrobust lnopening x_c, h(.2 .2)	
local obs = e(h_r) + e(N_h_r) // Collect Obs number
local bw = e(h_r)

outreg2 using RDrobust.doc, adds(Bandwidth, `bw', Obs, `obs') append

rdrobust lnopening x_c, h(.25 .25)	
local obs = e(h_r) + e(N_h_r) // Collect Obs number
local bw = e(h_r)

outreg2 using RDrobust.doc, adds(Bandwidth, `bw', Obs, `obs') append

*Bonus questions - Manipulation test

rddensity tomatometer_rating, c(0.6)
*Yields a p-value of 0.7172

save "Econ641Project.dta" , replace

save "641FinalProject log", replace

log close




