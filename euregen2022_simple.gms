* Cause dollar statements to appear in lst file
$ondollar
* Set EOL comment indicator to the default of !!
$oneolcom

* * * Fundamental sets
set
t                                Time period
v                                Vintage (installation period) of generation capacity
oldv(v)                          Existing vintages
newv(v)                          New vintages
tv(t,v)                          Time period in which vintage v is installed
r                                Regions
;

$gdxin database\setpar_%n%.gdx
$load t, v, oldv, newv, tv, r
$gdxin

alias(r,rr);
alias(t,tt);
alias(v,vv);

* * * Timeseries and calibration
set
i                                Generation technology
h                                Hours
s                                Segments (representative hours)
m                                Months
sm(s,m)                          Mapping segments to months
peak(s,r,t)                      Peak segment
;

$gdxin database\setpar_%n%.gdx
$load i, h, s, m, sm
$gdxin

set
superirnw
quantiles
irnw_mapq(i,quantiles)
superirnw_mapq(i,quantiles,superirnw)
;

$gdxin database\setpar_%n%.gdx
$load superirnw, quantiles, superirnw_mapq, irnw_mapq
$gdxin

PARAMETER
hours(s)                                Number of hours per segment
load_st(s,r,t)                          Load time series (GW) (already corrected when using segments instead of hours)
daref(r,t)                              Reference annual demand by region over time (TWh)
peakload(r,t)                           Maximum load (GW)
vrsc(s,i,v,r)                           Capacity factor for variable resources (techboost yes no)
vrsc_s(s,i,v,r)                         Capacity factor for variable resources (uncorrected)
vrsccorr(i,v,r)                         Correction of intermittent renewables to meet full-load hours when using segments instead of hours
vrscincrease(i,v,r)                     Reflects technological progress of solar PV
irnwlimUP_quantiles(i,r,quantiles)      Upper limit per quantile (GW)
;

$gdxin database\setpar_%n%.gdx
$load hours, load_st, vrsc_s, vrsccorr, vrscincrease, irnwlimUP_quantiles
$gdxin

* Correct time series to match annual load and full-load hours of renewables and reflect improvements of solar PV
vrsc(s,i,v,r)$(vrsccorr(i,v,r) > 0) = round(vrsccorr(i,v,r) * vrsc_s(s,i,v,r) * vrscincrease(i,v,r), 4) + eps ;

* Calculation of annual demand
daref(r,t) = sum(s, hours(s) * load_st(s,r,t)) * 1e-3 ;

* Determine peakload and peak hour
peakload(r,t) = smax(s, load_st(s,r,t)) ;
peak(s,r,t) = YES$(load_st(s,r,t) eq peakload(r,t)) ;

* * * Generation technology
set
new(i)                           New generation technology
exi(i)                           Existing technologies (or capacities) in base year - EXISTING BLOCKS
ccs(i)                           CCS generation technologies (or capacities) - CCS BLOCKS
conv(i)                          Conventional generation technologies
irnw(i)                          Intermittent renewable generation technologies
gas(i)                           Gas technologies
bio(i)                           Biomass technologies
sol(i)                           Solar technologies
wind(i)                          Wind technologies
windon(i)                        Wind onshore technologies 
windoff(i)                       Wind offshore technologies 
rnw(i)                           Renewable technologies
lowcarb(i)                       Low-carbon technologies
nuc(i)                           Nuclear technologies
type                             Generation type
idef(i,type)                     Map between technology and type
iidef(i,type)                    Map between technology and type
chp(i)                           CHP technologies
nochp(i)                         No CHP technologies       
mapchp(i,i)                      Mapping of CHP technologies to "mother" technology in case of not modeling CHP
;

$gdxin database\setpar_%n%.gdx
$load new, exi, ccs, irnw, conv, sol, wind, windon, windoff, rnw, lowcarb, nuc, type, idef, gas, bio, chp, nochp, mapchp
$gdxin

iidef(i,type) = idef(i,type) ;
alias(i,ii) ;

PARAMETER
cap_int(i,v,r)                   Capacity installed by region (GW)
cap(i,v,r)                       Capacity installed by region (GW)
invlimUP(i,r,t)                  Upper bounds on investment based on potential (cumulative since last time period) (GW)
invlimLO(i,r,t)                  Lower bounds on investment based on current pipeline (cumulative since last time period) (GW)
capcost_int(i,v,r)               Capacity cost (investment) by region
capcost(i,v,r)                   Capacity cost (investment) by region
fomcost_int(i,v,r)               Fixed OM cost
fomcost(i,v,r)                   Fixed OM cost
vomcost(i,v,r)                   Variable OM cost
effrate(i,v,r)                   Efficiency
co2captured_int(i,v,r)           CCS capture rate
co2captured(i,v,r)               CCS capture rate
emit_int(i,v,r)                  Emission factor
emit(i,v,r)                      Emission factor
reliability(i,v,r)               Reliability factor by region and technology
capcred(i,v,r)                   Capacity credit by region and technology
mindisp(i,v,r)                   Min load by region and technology
sclim_int(r)                     Upper bound on geologic storage of carbon (GtCO2)
sclim_eu_int                     Upper bound on geologic storage of carbon (GtCO2)
sclim(r)                         Upper bound on geologic storage of carbon (GtCO2)
sclim_eu                         Upper bound on geologic storage of carbon (GtCO2)
biolim_int(r,t)                  Upper bounds by region on biomass use (MWh)
biolim_eu_int(t)                 Upper bounds by region on biomass use (MWh)
biolim(r,t)                      Upper bounds by region on biomass use (MWh)
biolim_eu(t)                     Upper bounds by region on biomass use (MWh)
;

* Interim parameters are necessary for sensitivity analysis
$gdxin database\setpar_%n%.gdx
$load cap, cap_int=cap, invlimUP, invlimLO, capcost, capcost_int=capcost, fomcost, fomcost_int=fomcost, vomcost, effrate, co2captured, co2captured_int=co2captured, emit, emit_int=emit, reliability, capcred, mindisp
$load sclim, sclim_int=sclim, sclim_eu, sclim_eu_int=sclim_eu, biolim, biolim_int=biolim, biolim_eu, biolim_eu_int=biolim_eu
$gdxin


* * * Wind and solar cost sensitivity module
* Onshore 
$if      set windoncap70       capcost(windon(i),"2050",r) = 0.7 * capcost_int(i,"2050",r) ;
$if      set windoncap80       capcost(windon(i),"2050",r) = 0.8 * capcost_int(i,"2050",r) ;
$if      set windoncap90       capcost(windon(i),"2050",r) = 0.9 * capcost_int(i,"2050",r) ;
$if      set windoncap110      capcost(windon(i),"2050",r) = 1.1 * capcost_int(i,"2050",r) ;
$if      set windoncap120      capcost(windon(i),"2050",r) = 1.2 * capcost_int(i,"2050",r) ;
$if      set windoncap130      capcost(windon(i),"2050",r) = 1.3 * capcost_int(i,"2050",r) ;
$if      set windoncap140      capcost(windon(i),"2050",r) = 1.4 * capcost_int(i,"2050",r) ;
* Linearly interpolate cost between 2022 and 2050
$if      set windoninterpol    capcost(windon(i),newv(v),r)$(not sameas(v,"2022") or not sameas(v,"2050")) = capcost(i,"2022",r) + (capcost(i,"2050",r) - capcost(i,"2022",r)) * (v.val - 2022) / (2050 - 2022) ;

* Offshore
set
rwindoff(r) regions with offshore potential
;

rwindoff(r)$(capcost_int("Windoff_q90","2022",r) > 0) = YES ;

$if      set windoffcap70      capcost(windoff(i),"2050",rwindoff(r)) = 0.7 * capcost_int(i,"2050",r) ;
$if      set windoffcap80      capcost(windoff(i),"2050",rwindoff(r)) = 0.8 * capcost_int(i,"2050",r) ;
$if      set windoffcap90      capcost(windoff(i),"2050",rwindoff(r)) = 0.9 * capcost_int(i,"2050",r) ;
$if      set windoffcap110     capcost(windoff(i),"2050",rwindoff(r)) = 1.1 * capcost_int(i,"2050",r) ;
$if      set windoffcap120     capcost(windoff(i),"2050",rwindoff(r)) = 1.2 * capcost_int(i,"2050",r) ;
$if      set windoffcap130     capcost(windoff(i),"2050",rwindoff(r)) = 1.3 * capcost_int(i,"2050",r) ;
$if      set windoffcap140     capcost(windoff(i),"2050",rwindoff(r)) = 1.4 * capcost_int(i,"2050",r) ;
* Linearly interpolate cost between 2022 and 2050
$if      set windoffinterpol   capcost(windoff(i),newv(v),rwindoff(r))$(not sameas(v,"2022") or not sameas(v,"2050")) = capcost(i,"2022",r) + (capcost(i,"2050",r) - capcost(i,"2022",r)) * (v.val - 2022) / (2050 - 2022) ;

* Solar PV
$if      set solcap70       capcost(sol(i),"2050",r) = 0.7 * capcost_int(i,"2050",r) ;
$if      set solcap80       capcost(sol(i),"2050",r) = 0.8 * capcost_int(i,"2050",r) ;
$if      set solcap90       capcost(sol(i),"2050",r) = 0.9 * capcost_int(i,"2050",r) ;
$if      set solcap110      capcost(sol(i),"2050",r) = 1.1 * capcost_int(i,"2050",r) ;
$if      set solcap120      capcost(sol(i),"2050",r) = 1.2 * capcost_int(i,"2050",r) ;
$if      set solcap130      capcost(sol(i),"2050",r) = 1.3 * capcost_int(i,"2050",r) ;
$if      set solcap140      capcost(sol(i),"2050",r) = 1.4 * capcost_int(i,"2050",r) ;
* Linearly interpolate cost between 2022 and 2050
$if      set solinterpol   capcost(sol(i),newv(v),r)$(not sameas(v,"2022") or not sameas(v,"2050")) = capcost(i,"2022",r) + (capcost(i,"2050",r) - capcost(i,"2022",r)) * (v.val - 2022) / (2050 - 2022) ;

* * * Nuclear module (cost update, cost changes, open for all countries)
* * Cost sensitivity 
* Determine nuclear and non-nuclear countries
set
rnonuc(r)
rnuc(r)
;

rnonuc(r)$(capcost("Nuclear","2022",r) = 0) = YES ;
rnuc(r)$(capcost("Nuclear","2022",r) > 0) = YES ;

* Real nuclear cost
$if      set nuclearcapreal    capcost("Nuclear","2022",rnuc(r)) = 8250 ;
* Real nuclear cost only for france
$if      set frnuccapreal      capcost("Nuclear","2022","France") = 8250 ;

* Nuclear cost sensitivity (all)
$if      set nuclearfom25      fomcost("Nuclear","2050",rnuc(r)) = 0.25 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom50      fomcost("Nuclear","2050",rnuc(r)) = 0.5 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom75      fomcost("Nuclear","2050",rnuc(r)) = 0.75 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom125     fomcost("Nuclear","2050",rnuc(r)) = 1.25 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom150     fomcost("Nuclear","2050",rnuc(r)) = 1.5 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom175     fomcost("Nuclear","2050",rnuc(r)) = 1.75 * fomcost_int("Nuclear","2050",r) ;
$if      set nuclearfom200     fomcost("Nuclear","2050",rnuc(r)) = 2.0 * fomcost_int("Nuclear","2050",r) ;

$if      set nuclearcap25      capcost("Nuclear","2050",rnuc(r)) = 0.25 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap50      capcost("Nuclear","2050",rnuc(r)) = 0.5 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap75      capcost("Nuclear","2050",rnuc(r)) = 0.75 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap125     capcost("Nuclear","2050",rnuc(r)) = 1.25 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap150     capcost("Nuclear","2050",rnuc(r)) = 1.5 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap175     capcost("Nuclear","2050",rnuc(r)) = 1.75 * capcost_int("Nuclear","2050",r) ;
$if      set nuclearcap200     capcost("Nuclear","2050",rnuc(r)) = 2.0 * capcost_int("Nuclear","2050",r) ;

* Nuclear cost sensitivity (France)
$if      set frnucfom25        fomcost("Nuclear","2050","France") = 0.25 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom50        fomcost("Nuclear","2050","France") = 0.5 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom75        fomcost("Nuclear","2050","France") = 0.75 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom125       fomcost("Nuclear","2050","France") = 1.25 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom150       fomcost("Nuclear","2050","France") = 1.5 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom175       fomcost("Nuclear","2050","France") = 1.75 * fomcost_int("Nuclear","2050","France") ;
$if      set frnucfom200       fomcost("Nuclear","2050","France") = 2.0 * fomcost_int("Nuclear","2050","France") ;

$if      set frnuccap25        capcost("Nuclear","2050","France") = 0.25 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap50        capcost("Nuclear","2050","France") = 0.5 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap75        capcost("Nuclear","2050","France") = 0.75 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap125       capcost("Nuclear","2050","France") = 1.25 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap150       capcost("Nuclear","2050","France") = 1.5 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap175       capcost("Nuclear","2050","France") = 1.75 * capcost_int("Nuclear","2050","France") ;
$if      set frnuccap200       capcost("Nuclear","2050","France") = 2.0 * capcost_int("Nuclear","2050","France") ;

* Linearly interpolate cost between 2022 and 2050 (necessay for the prior cost sensitivity)
$if      set nuclearinterpol   fomcost("Nuclear",newv(v),rnuc(r))$(not sameas(v,"2022") or not sameas(v,"2050")) = fomcost("Nuclear","2022",r) + (fomcost("Nuclear","2050",r) - fomcost("Nuclear","2022",r)) * (v.val - 2022) / (2050 - 2022) ;
$if      set nuclearinterpol   capcost("Nuclear",newv(v),rnuc(r))$(not sameas(v,"2022") or not sameas(v,"2050")) = capcost("Nuclear","2022",r) + (capcost("Nuclear","2050",r) - capcost("Nuclear","2022",r)) * (v.val - 2022) / (2050 - 2022) ;

* * Decommissioning cost
PARAMETER
deccost(i,v,r)
;

deccost("Nuclear",v,r)$(capcost("Nuclear",v,r) > 0) = 0 ;
$if     set deccost250    deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 250 ;
$if     set deccost500    deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 500 ;
$if     set deccost750    deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 500 ;
$if     set deccost1000   deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 1000 ;
$if     set deccost1250   deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 1250 ;
$if     set deccost1500   deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 1500 ;
$if     set deccost1750   deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 1500 ;
$if     set deccost2000   deccost("Nuclear",v,rnuc(r))$(capcost("Nuclear",v,r) > 0) = 2000 ;

* * Allow nuclear in all countries (parameters need to get added for non-nuclear countries, France serves as example; avoid this option with specific French nuclear runs)
$if     set nucall  capcost("Nuclear",newv(v),rnonuc)            = capcost("Nuclear",v,"France") ;
$if     set nucall  deccost("Nuclear",newv(v),rnonuc)            = deccost("Nuclear",v,"France") ;
$if     set nucall  fomcost("Nuclear",newv(v),rnonuc)            = fomcost("Nuclear",v,"France") ;
$if     set nucall  vomcost("Nuclear",newv(v),rnonuc)            = vomcost("Nuclear",v,"France") ;
$if     set nucall  reliability("Nuclear",newv(v),rnonuc)        = reliability("Nuclear",v,"France") ;
$if     set nucall  effrate("Nuclear",newv(v),rnonuc)            = effrate("Nuclear",v,"France") ;
$if     set nucall  capcred("Nuclear",newv(v),rnonuc)            = capcred("Nuclear",v,"France") ;
$if     set nucall  invlimUP("Nuclear",rnonuc,t)$(t.val ge 2022) = invlimUP("Nuclear","France",t) ;

* * * CHP module (add CHP capacity to those of the mother technology in case CHP is not modelled)
$if not  set chp    cap(nochp(i),v,r)   = cap_int(i,v,r) + sum(mapchp(i,ii), cap_int(ii,v,r)) ;
$if not  set chp    cap(chp(i),v,r)     = 0 ;

* * * CCS and biomass limits
* Correcting SC limits because much of this potential is needed after 2050 when direct air removal technologies become competitive (to establish pre-industrial climate)
$if      set sclim10           sclim_eu = 0.1 * sclim_eu_int ;
$if      set sclim20           sclim_eu = 0.2 * sclim_eu_int ;
$if      set sclim50           sclim_eu = 0.5 * sclim_eu_int ;
$if      set sclim100          sclim_eu = 1.0 * sclim_eu_int ;
$if      set sclim10           sclim(r) = 0.1 * sclim_int(r) ;
$if      set sclim20           sclim(r) = 0.2 * sclim_int(r) ;
$if      set sclim50           sclim(r) = 0.5 * sclim_int(r) ;
$if      set sclim100          sclim(r) = 1.0 * sclim_int(r) ;

* Correcting biomass limits because not all biomass can go into the power plants (food, traffic, ...)
$if      set biolimnormal      biolim_eu(t) = 1 * biolim_eu_int(t) ;
$if      set biolimhalf        biolim_eu(t) = 0.5 * biolim_eu_int(t) ;
$if      set biolimdouble      biolim_eu(t) = 2 * biolim_eu_int(t) ;
$if      set biolimnormal      biolim(r,t) = 1 * biolim_int(r,t) ;
$if      set biolimhalf        biolim(r,t) = 0.5 * biolim_int(r,t) ;
$if      set biolimdouble      biolim(r,t) = 2 * biolim_int(r,t) ;

* Biolim grows linearly over time with half of 2050 biomass available in 2050
$if      set bioliminterpol    biolim_eu(t) = round(0.5 * biolim_eu("2050") + (t.val - 2020)/30 * (biolim_eu(t) - 0.5 * biolim_eu("2050")), 4) ;
$if      set bioliminterpol    biolim(r,t)  = round(biolim_int(r,t) * biolim_eu(t) / biolim_eu("2050"), 4) ;
  
* * * Emission factors
* Correction of emission factors from database
emit(i,v,r)             = emit_int(i,v,r) ;
emit("Coal",v,r)        = 0.9 * emit_int("Coal",v,r) ;
emit("Coal_CCS",v,r)    = 0.9 * emit_int("Coal_CCS","2030",r) ;
emit("Lignite",v,r)     = 0.8 * emit_int("Lignite",v,r) ;
emit("Lignite_CCS",v,r) = 0.8 * emit_int("Lignite_CCS",v,r) ;
emit("Bioenergy",v,r)   = 0.8 * emit_int("Bioenergy",v,r) ;
emit("Bio_CHP",v,r)     = 0.8 * emit_int("Bioenergy",v,r) ;
emit("Bio_CCS",v,r)     = 0.8 * emit_int("Bio_CCS",v,r) ;
* Correction of capturing rates
co2captured(i,v,r)              = 0.9 * co2captured_int(i,v,r) ;
co2captured("Lignite_CCS",v,r)  = 0.8 * co2captured_int("Lignite_CCS",v,r) ;
co2captured("Bio_CCS",v,r)      = 0.8 * co2captured_int("Bio_CCS",v,r) ;
* Correcting biomass emissions factors according to biomass neutral treatment (socially and politically questionable)    
emit("Bioenergy",v,r)   = 0 ;
emit("Bio_CHP",v,r)     = 0 ;
emit("Bio_CCS",v,r)     = - co2captured("Bio_CCS",v,r) ;
 
* * * Storage technology
set
j                                Storage technology
newj(j)                          New storage technology
exij(j)                          Existing storage technology
;

$gdxin database\setpar_%n%.gdx
$load j, newj, exij
$gdxin

PARAMETER
gcap(j,v,r)                      Storage capacity by region (GW)
ghours(j,v,r)                    Hours of storage (room size relative to door size)
chrgpen(j,v,r)                   Charge efficiency penalty for storage by region (< 1)
dchrgpen(j,v,r)                  Discharge efficiency penalty for storage by region (< 1)
dischrg(j,v,r)                   Automatic storage discharge by region (in percent) (< 1)
gcapcost(j,v,r)                  Capital cost of storage charge-discharge capacity by region (EUR per MW)
gfomcost(j,v,r)                  Fixed OM cost for storage by region(EUR per MW)
gvomcost(j,v,r)                  Variable OM cost for storage by region (EUR per MWh)
greliability(j,v,r)              Storage reliability factor by region and technology
gcapcred(j,v,r)                  Storage capacity credit by region and technology
ginvlimLO(j,r,t)                 Storage investment lower bound (GW)
ginvlimUP(j,r,t)                 Storage investment upper bound (GW)
;

$gdxin database\setpar_%n%.gdx
$load gcap, ghours, chrgpen, dchrgpen, dischrg, gcapcost, gfomcost, gvomcost, greliability, gcapcred, ginvlimLO, ginvlimUP
$gdxin

* * * Transmission technology
set
k                    Transmission technologies
tmap(k,r,r)          Regions eligible for transmission exchange by technology

;

$gdxin database\setpar_%n%.gdx
$load k, tmap
$gdxin

PARAMETER
tcap(k,r,r)                     Transmission capacity from region X to region Y (GW)
tcapcost(k,r,r)                 Transmission investment cost ($ per kW)
tfomcost(k,r,r)                 Fixed O&M cost of new transmision capacity (euro per kW-year)
tvomcost(k,r,r)                 Variable O&M cost of new transmision capacity (euro per MWh)
trnspen(k,r,r)                  Transmission loss penalty
tinvlimUP(k,r,r,t)              Upper bound on total transmission capacity from region X to region Y (GW)
tinvlimLO(k,r,r,t)              Lower bound on total transmission capacity from region X to region Y (GW)
tcapcred(k,r,r)                 Capacity credit for transmisson by region and technology
;

$gdxin database\setpar_%n%.gdx
$load tcap, tcapcost, tfomcost, tvomcost, trnspen, tinvlimUP, tinvlimLO, tcapcred
$gdxin

* Database corrections to ensure feasibility of model (lower and upper limits must be in line with installed capacity)
tinvlimLO(k,r,rr,t)$(tinvlimLO(k,r,rr,t) < tcap(k,r,rr)) =  tcap(k,r,rr) ;
tinvlimUP(k,r,rr,t)$(tinvlimUP(k,r,rr,t) < tcap(k,r,rr)) =  tcap(k,r,rr) ;
tinvlimUP(k,r,rr,t)$(tinvlimUP(k,r,rr,t) < tinvlimLO(k,r,rr,t)) =  tcap(k,r,rr) ;

* * * Discounting
PARAMETER
nyrs(t)                         Number of years since last time step
dfact(t)                        Discount factor for time period t (reflects number of years) for both
annuity(i,v)                    Annuity factor for generation capacity
gannuity(j,v)                   Annuity factor for storage capacity
tannuity(k)                     Annuity factor for transmission capacity
;

$gdxin database\setpar_%n%.gdx
$load nyrs, dfact, annuity, gannuity, tannuity
$gdxin

* * * Lifetime and depreciation
PARAMETER
lifetime(i,v,r,t)               Lifetime coefficient for existing and new capacity (fraction of original capacity still online)
deprtime(i,v,r,t)               Depreciation coefficient for existing and new capacity (fraction of original capacity still depreciating)
glifetime(j,v,r,t)              Lifetime coefficient for existing and new capacity (fraction of original capacity still online)
gdeprtime(j,v,r,t)              Depreciation coefficient for existing and new capacity (fraction of original capacity still depreciating)
tlifetime(k,v,r,t)              Lifetime coefficient for existing and new capacity (fraction of original capacity still online)
tdeprtime(k,v,r,t)              Depreciation coefficient for existing and new capacity (fraction of original capacity still depreciating)
;

$gdxin database\setpar_%n%.gdx
$load lifetime, deprtime
$load glifetime, gdeprtime
$load tlifetime, tdeprtime
;

* toptimize is a subset of t that marks the optimization periods 
set
$if set shortrun  toptimize(t)     Optimization periods /2022,2023,2024,2025,2026,2027,2028,2029,2030,2035,2040,2045,2050/
$if set longrun   toptimize(t)     Optimization periods /2022,2025,2030,2035,2040,2045,2050/
;

* * * Prices
set
fuel                             Fuel
xfueli(fuel,i)                   Map fuel technology
price_sc                         Price adder scenarios
;

$gdxin database\setpar_%n%.gdx
$load fuel, xfueli, price_sc
$gdxin

PARAMETER
pfuel(fuel,r,t)                         Fuel price (EUR er MWh)
pfadd_rel_int(price_sc,fuel,r,t)        Relative fuel price adders (value between 0 (no adding) and x)
pfadd_rel(fuel,r,t)                     Relative fuel price adders (value between 0 (no adding) and x)
ccscost(r,t)                            CCS CO2 transportation cost (EUR per tCO2)
;

$gdxin database\setpar_%n%.gdx
$load pfuel, ccscost, pfadd_rel_int=pfadd_rel
$gdxin

* Selection of the relevant fuel price scenario (recovery = normal gas price, high = 50% higher gas price, long = 100% higher gas price)
$if      set bauprice                           pfadd_rel(fuel,r,t) = pfadd_rel_int("bauprice",fuel,r,t) ;
$if      set recovery                           pfadd_rel(fuel,r,t) = pfadd_rel_int("recovery",fuel,r,t) ;
$if      set high                               pfadd_rel(fuel,r,t) = pfadd_rel_int("high",fuel,r,t) ;
$if      set long                               pfadd_rel(fuel,r,t) = pfadd_rel_int("long",fuel,r,t) ;

* * * Sets of relevant generators
set
ivrt(i,v,r,t)  Active generation vintage-capacity blocks
jvrt(j,v,r,t)  Active storage vintage-capacity blocks
tvrt(k,v,r,t)  Active transmission vintage-capacity blocks
;

* * Generation technologies (ivrt)
ivrt(i,v,r,t)$(cap(i,v,r) * lifetime(i,v,r,t) or (new(i) and newv(v) and lifetime(i,v,r,t)))                                            = YES ;
ivrt(irnw(i),v,r,t)$(sum(s, vrsc(s,i,v,r)) * lifetime(i,v,r,t) eq 0)                                                                    = NO ;
ivrt(i,v,r,t)$(not toptimize(t))                                                                                                        = NO ;  

* * Storage technologies (jvrt)
jvrt(j,v,r,t)$(gcap(j,v,r) * glifetime(j,v,r,t) or (newj(j) and newv(v) and glifetime(j,v,r,t)))                                        = YES ;
$if not  set storage     jvrt(j,v,r,t)                                                                                                  = NO ;
jvrt("Storage_ST",v,r,t)$(v.val le 2022)                                                                                                = NO ;
jvrt("Storage_LT",v,r,t)$(v.val le 2022)                                                                                                = NO ;
jvrt(j,v,r,t)$(not toptimize(t))                                                                                                        = NO ;

* * Transmission (tvrt)
tvrt(k,v,r,t)$(sum(tmap(k,r,rr), tlifetime(k,v,r,t)))                                                                                   = YES ;
$if not  set trans       tvrt(k,v,r,t)                                                                                                  = NO ;                                                      
tvrt(k,v,r,t)$(not toptimize(t))                                                                                                        = NO ;

* * * Dispatch cost
PARAMETER
discost(i,v,r,t)         Dispatch cost (EUR per MWh el)
;

* * * Define dispatch cost
discost(i,v,r,t)$(ivrt(i,v,r,t) and effrate(i,v,r)) = 
*        Variable O&M costs
                           vomcost(i,v,r)
*        Fuel costs (including region-specific price delta) including regional adder relative                    
                         + round(sum(xfueli(fuel,i), pfuel(fuel,r,t)  * (1 + pfadd_rel(fuel,r,t))) / effrate(i,v,r), 8)
*        CCS costs (from transporting and storing)
                         + round(co2captured(i,v,r) * ccscost(r,t), 8)
;

* * * Loss, voll, and availability
PARAMETER
loss(r) distribution grid loss         
voll(r,t) value of lost load (EUR per MWh)
afnn(m,i,r,t) montly availability by technology and region
;

$onUndf
$gdxin database\setpar_%n%.gdx
$load loss, voll, afnn
$gdxin

voll(r,t)           = 3000 ;

* * Module to create availability factors (too large to read in)
PARAMETER
af(s,i,v,r,t)                Availability factor
;

* Set availability of old vintage nuclear, hydro, and bioenergy to zero 
af(s,ivrt(i,oldv(v),r,t))$(sameas(i,"Nuclear"))   = 1 ;
af(s,ivrt(i,oldv(v),r,t))$(sameas(i,"Hydro"))     = 1 ;
af(s,ivrt(i,oldv(v),r,t))$(sameas(i,"Bioenergy")) = 1 ;
af(s,ivrt(i,oldv(v),r,t))$(sameas(i,"Bio_CHP"))   = 1 ;

* Set reliability of the very same vintage technologies to 1 (availability is only steered via availability)
reliability(i,oldv(v),r)$(sameas(i,"Nuclear") and cap(i,v,r) > 0)   = 1 ;
reliability(i,oldv(v),r)$(sameas(i,"Hydro") and cap(i,v,r) > 0)     = 1 ;
reliability(i,oldv(v),r)$(sameas(i,"Bioenergy") and cap(i,v,r) > 0) = 1 ;
reliability(i,oldv(v),r)$(sameas(i,"Bio_CHP") and cap(i,v,r) > 0)   = 1 ;

* Set 2020 to real world availability
af(s,ivrt(i,oldv(v),r,t))$(sameas(t,"2020") and sameas(i,"Nuclear")  )      = sum(sm(s,m), afnn(m,i,r,t)) ;
af(s,ivrt(i,oldv(v),r,t))$(sameas(t,"2020") and sameas(i,"Bioenergy"))      = sum(sm(s,m), afnn(m,i,r,t)) ;
af(s,ivrt(i,oldv(v),r,t))$(sameas(t,"2020") and sameas(i,"Bio_CHP"))        = sum(sm(s,m), afnn(m,i,r,t)) ;

* Set 2021 to real world availability
af(s,ivrt(i,oldv(v),r,t))$(sameas(t,"2021") and sameas(i,"Nuclear")  )      = sum(sm(s,m), afnn(m,i,r,t)) ;
af(s,ivrt(i,oldv(v),r,t))$(sameas(t,"2021") and sameas(i,"Bioenergy"))      = sum(sm(s,m), afnn(m,i,r,t)) ;
af(s,ivrt(i,oldv(v),r,t))$(sameas(t,"2021") and sameas(i,"Bio_CHP"))        = sum(sm(s,m), afnn(m,i,r,t)) ;

* Set 2022 availability to real world availability
af(s,ivrt(i,oldv(v),r,t))$(sameas(t,"2022") and sameas(i,"Nuclear")  )      = sum(sm(s,m), afnn(m,i,r,t)) ;
af(s,ivrt(i,oldv(v),r,t))$(sameas(t,"2022") and sameas(i,"Bioenergy"))      = sum(sm(s,m), afnn(m,i,r,t)) ;
af(s,ivrt(i,oldv(v),r,t))$(sameas(t,"2022") and sameas(i,"Bio_CHP"))        = sum(sm(s,m), afnn(m,i,r,t)) ;

* Correction of German value (generation is devided by doubled capacity that was taken from the grid end of 2021)
af(s,ivrt(i,oldv(v),r,t))$(sameas(t,"2022") and sameas(i,"Nuclear") and sameas(r,"Germany") )      = 2 * sum(sm(s,m), afnn(m,i,r,t)) ;

* Set 2023+ availability to average of 2020 and 2021
af(s,ivrt(i,oldv(v),r,t))$(t.val ge 2023    and sameas(i,"Nuclear") and not sameas(r,"France")) = sum(sm(s,m), afnn(m,i,r,"2022")) ;
af(s,ivrt(i,oldv(v),r,t))$(t.val ge 2023    and sameas(i,"Nuclear") and     sameas(r,"France")) = round(sum(sm(s,m), (afnn(m,i,r,"2020") + afnn(m,i,r,"2021"))/2), 4) ;
af(s,ivrt(i,oldv(v),r,t))$(t.val ge 2023    and sameas(i,"Bioenergy"))      = sum(sm(s,m), afnn(m,i,r,"2022")) ;
af(s,ivrt(i,oldv(v),r,t))$(t.val ge 2023    and sameas(i,"Bio_CHP"))        = sum(sm(s,m), afnn(m,i,r,"2022")) ;

* New hydro timeseries out of calibration data created above and now higher availability used in case necessary
$if     set newhydrotimeseries  vrsc(s,i,v,r)$(sameas(i,"Hydro") and cap(i,v,r) > 0) = round(sum(sm(s,m), afnn(m,i,r,"2020")/2 + afnn(m,i,r,"2021")/2), 4) ;
* Set French nuclear availability in 2021 and 2022 equal to 2020 as what-if-not-case
$if     set frnucnormal     af(s,ivrt(i,oldv(v),r,t))$(t.val ge 2021 and t.val le 2022 and sameas(i,"Nuclear") and sameas(r,"France") and reliability(i,v,r) > 0) = af(s,i,v,r,"2020")/2 + af(s,i,v,r,"2021")/2 ;
* Lower hydro availability according to reduced hydro availability in 2022
$if not set hydronormal     af(s,ivrt(i,oldv(v),r,t))$(sameas(t,"2022")                and sameas(i,"Hydro") and reliability(i,v,r) > 0 and sum(sm(s,m), afnn(m,i,r,"2020") + afnn(m,i,r,"2021")) > 0) = round(sum(sm(s,m), afnn(m,i,r,"2022") / (afnn(m,i,r,"2020")/2 + afnn(m,i,r,"2021")/2)), 4) ;

* * * Declare Model
positive variable
* Demand
BS(s,r,t)               Lost load (backstop demand option) (GW)
* Generation
X(s,i,v,r,t)            Unit dispatch by segment (GW)
XTWH(i,v,r,t)           Annual generation for sparsity purposes (TWh)
XC(i,v,r,t)             Installed generation capacity (GW)
XCS(s,i,v,r,t)          Copies of XC over s for sparsity purposes (GW)
IX(i,r,t)               New vintage investment (total GW to be added from t-1 to t) (GW)
* Storage
G(s,j,v,r,t)            Energy storage charge (GW)
GD(s,j,v,r,t)           Energy storage discharge (GW)
GC(j,v,r,t)             Energy storage charge-discharge capacity (GW)
GCS(s,j,v,r,t)          Energy storage charge-discharge capacity (GW)
GB(s,j,v,r,t)           Energy storage accumulated balance (100 GWh)
IG(j,r,t)               Investment in storage charge-discharge capacity (GW)
* Transmission
E(s,k,r,r,t)            Bilateral trade flows by load segment (GW)
TC(k,r,r,t)             New Trade flow capacity (GW)
TCS(s,k,r,r,t)          New Trade flow capacity (GW)
IT(k,r,r,t)             Investment in transmission (total GW to be added from t-1 to t)
* Potential variables
BC_r(r,t)               Annual flow of biomass used (TWh)
BC(t)                   Annual flow of biomass used (TWh)
SC_r(r,t)               Annual flow of geologically stored carbon (MtCO2)
SC(t)                   Annual flow of geologically stored carbon (MtCO2)
;

variable
NPVCOST                        NPV of system cost (positive value in million EUR)
;

equation
objdef                           Objection function -- definition of cost
*Demand equations
demand(s,r,t)                    Electricity market clearing condition
demand_sos(s,r,t)                Regional system adequacy condition
* Generation and capacity
capacity(s,i,v,r,t)              Generation capacity constraint on dispatch
capacity_chp(s,i,v,r,t)          CHP generation capacity
capacity_bio(s,i,v,r,t)          Generation capacity constraint on dispatch of bioenergy (to avoid implementing a subsidy on bioenergy)
invest(i,v,r,t)                  Accumulation of annual investment flows
exlife(i,v,r,t)                  Existing capacity 
exlife2022(i,v,r,t)              Existing capacity in 2020 is fix
exlife_chp(i,v,r,t)              Existing capacity of CHP is fix until 2030
exlife_bio(i,v,r,t)              Existing capacity of bioenergy is fix until 2030
newlife(i,v,r,t)                 New vintages are subject to lifetime constraint
retire(i,v,r,t)                  Monotonicity constraint on installed capacity
investlimUP(i,r,t)               Upper limits on investment (region)
investlimLO(i,r,t)               Lower limits on investment (region)
investlimUP_irnw(i,r,t,quantiles) Upper limits on investment for intermittent renewables per quantile (region)
* Storage equations
chargelim(s,j,v,r,t)             Charge cannot exceed capacity
dischargelim(s,j,v,r,t)          Discharge cannot exceed capacity
storagebal(s,j,v,r,t)            Storage balance accumulation 
storagebal0(s,j,v,r,t)           Storage balance must be the same at the beginning and end
storagebal_st(s,j,v,r,t)         Storage balance accumulation of batteries
storagebal0_st(s,j,v,r,t)        Storage balance must be the same at the beginning and end
storagebalann_st(j,v,r,t)
storagelim(s,j,v,r,t)            Storage reservoir capacity
ginvest(j,v,r,t)                 Investment in storage charge-discharge capacity
gexlife(j,v,r,t)                 Existing storage capacity
gexlife2022(j,v,r,t)             Existing capacity in 2022 is fix
gexlife_pump(j,v,r,t)            Existing pump storage capacity is fix
gnewlife(j,v,r,t)                Storage wew vintages are subject to lifetime constraint
gretire(j,v,r,t)                 Monotonicity constraint on installed storage capacity
ginvestlimUP(j,r,t)              Upper limits on storage investment (region)
ginvestlimLO(j,r,t)              Lower limits on storage investment (region)
* Transmission equations
tcapacity(s,k,r,r,t)             Transmission capacity constraint on trade
tinvestexi(k,r,r,t)              Accumulation of annual transmission investment flows
tinvestnew(k,r,r,t)              Accumulation of annual transmission investment flows
tinvestlimUP(k,r,r,t)            Upper limit on total transmission capacity (between regions)
tinvestlimLO(k,r,r,t)            Lower limit on total transmission capacity (between regions)
* Market equations
biomarket(t)                     System-wide market for bioenergy or supply equal demand for bioenergy
biomarket_r(r,t)                 Supply equal demand for bioenergy (regional)
gasmarket(t)                     System-wide market for natural gas or supply equal demand for natural gas
gasmarket_r(r,t)                 Supply equal demand for natural gas (regional)
* Policy, market, and limit equations
bioflow(t)                       Annual flow of biomass
bioflow_r(r,t)                   Annual flow of biomass (per region)
cumbio(t)                        Limits on cumulative use of bioenergy
cumbio_r(r,t)                    Limits on cumulative use of bioenergy (per region)
ccsflow(t)                       Annual flow of captured carbon for geologic storage
ccsflow_r(r,t)                   Annual flow of captured carbon for geologic storage (per region)
cumccs                           Limits on cumulative geologic storage of carbon
cumccs_r(r)                      Limits on cumulative geologic storage of carbon (per region)
* Structual equations
xtwhdef(i,v,r,t)                 Calculate XTWH from X (structural equation to aid solver)
copyxc(s,i,v,r,t)                Make copies of XC in XCS (structural equation to aid solver)
copygc(s,j,v,r,t)                Make copies of GC in GCS (structural equation to aid solver)
copytc(s,k,r,r,t)                Make copies of TC in TCS (structural equation to aid solver)
;

* * * Objective function definition
objdef..
*        Net present value (NPV) of system cost is defined in million EUR
         NPVCOST =e=
*        Sum over all time period t
                !! begin period sum
                sum(t$toptimize(t),
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               DISCOUNTING                
                !! begin discounting
                dfact(t) * (               
*               INVESTMENT COST
*               Old, excluding discouting via normal annui, and ccost)                
                !! begin investment cost (old)
*               We need nyrs because investments happens once only but production nyrs-times)
                1 / nyrs(t) * ( 
*                                   Investment costs follow from annuities (%) of borrowed capital (EUR/kW * GW)
                        + sum(new(i),  sum((tt,v)$((tt.val le t.val) and tv(tt,v) and ivrt(i,v,r,tt)),             IX(i,r,tt)   * (capcost(i,v,r) + deccost(i,v,r)) *  deprtime(i,v,r,tt) *  annuity(i,v)  * nyrs(t)))
$if      set storage    + sum(newj(j), sum((tt,v)$((tt.val le t.val) and tv(tt,v) and jvrt(j,v,r,tt)),             IG(j,r,tt)   * gcapcost(j,v,r)                   * gdeprtime(j,v,r,tt) * gannuity(j,v)  * nyrs(t)))
$if      set trans      + sum((rr,k)$tmap(k,r,rr), sum((tt,v)$((tt.val le t.val) and tv(tt,v) and tvrt(k,v,r,tt)), IT(k,r,rr,t) * tcapcost(k,r,rr)                  * tdeprtime(k,v,r,tt) * tannuity(k)    * nyrs(t)))
                )
                !! end investment cost (old, excluding discount via investment cost factor)
*               DISPATCH COST
*               Are measured in €/MWh and generation in GWh, so that we need to correct by 1e-3
                !! begin dispatch cost (regional)
*                       Dispatch cost (EUR/MWh) for generation (GWh)
                        + 1e-3 * sum(ivrt(i,v,r,t),            discost(i,v,r,t) * sum(s, hours(s) * X(s,i,v,r,t)))
*                       Variable operation and maintenance cost (EUR/MWh) for storage charge and discharge (GWh)
$if      set storage    + 1e-3 * sum(jvrt(j,v,r,t),            gvomcost(j,v,r)  * sum(s, hours(s) * G(s,j,v,r,t) + hours(s) * GD(s,j,v,r,t)))
*                       Variable operation and maintenance cost (EUR/MWh) for exports only (GWh)
$if      set trans      + 1e-3 * sum((k,rr)$tmap(k,r,rr),      tvomcost(k,r,rr) * sum(s, hours(s) * E(s,k,r,rr,t)))
                !! end dispatch cost (regional)              
*               SOCIAL COST
                !! begin social cost
*                       Cost (EUR/MWh) of lost load (GWh)
                        + 1e-3 * voll(r,t) * sum(s, BS(s,r,t) * hours(s))
                !! end social cost
*               FIXED COST
                !! begin fixed cost
*                       Fixed operation and maintenance cost (EUR/kW) for generation capacity (GW)
                        + sum(ivrt(i,v,r,t),       XC(i,v,r,t)  *  fomcost(i,v,r))
*                       Fixed operation and maintenance cost (EUR/kW) for storage capacity (GW)
$if      set storage    + sum(jvrt(j,v,r,t),       GC(j,v,r,t)  * gfomcost(j,v,r))
*                       Fixed operation and maintenance cost (EUR/kW) for transmission capacity (GW)
$if      set trans      + sum((k,rr)$tmap(k,r,rr), TC(k,r,rr,t) * tfomcost(k,r,rr))
                !! end fixed cost
                )
                !! end discounting
                )
                !! end region sum
                )
                !! end time period sum
;

* * * * * Demand equations
* * * Electricity market clearance condition (in each segment)
demand(s,r,t)$toptimize(t)..
*        Scale from GW to TWh (so that dual variable (marginals/shadow price) is reported directly in EUR/MWh)
                         1e-3 * hours(s)
*        Dispatched generation in region
                         * (sum(ivrt(i,v,r,t), X(s,i,v,r,t))
*        Plus inter-region imports
$if      set trans       + sum((k,rr)$tmap(k,rr,r), E(s,k,rr,r,t))
*        Less inter-region exports (penalty for transmission losses is charged on the export site only)
$if      set trans       - sum((k,rr)$tmap(k,r,rr), E(s,k,r,rr,t) / trnspen(k,r,rr))
*        Plus discharges from storage times discharge efficiency (less supply than stored) less charges from storage (the penalties apply at the storage accumulation)
$if      set storage     + sum(jvrt(j,v,r,t), GD(s,j,v,r,t) * dchrgpen(j,v,r) - G(s,j,v,r,t))
*        Plus a backstop option (lost load) representing segment-level demand response
$if      set lostload    + BS(s,r,t) * (1 + loss(r))
         )
*        Equals (annually scaled) demand including losses
         =e=             1e-3 * hours(s) * round(load_st(s,r,t) * (1 + loss(r)),4)
;

* * * Regional security of supply constraint (ensures secured back-up capacity via capacity credits beyond the spot market equation above, holds from 2023 onwards because no investments in 2022)
demand_sos(peak(s,r,t))$(t.val ge 2023 and toptimize(t))..
*        Scale from GW to TWh (so that dual variable (marginals/shadow price) is reported directly in euro per MWh)
                         1e-3 * hours(s) * (
*        Upper bound on available generation in region
                         + sum(ivrt(i,v,r,t), XCS(s,i,v,r,t) *  capcred(i,v,r))
*        Plus discharges from storage less charges (plus penalty)
$if      set storage     + sum(jvrt(j,v,r,t),  GD(s,j,v,r,t) * gcapcred(j,v,r))
*        Plus inter-region imports
$if      set trans       + sum((k,rr)$tmap(k,rr,r), TC(k,rr,r,t) * tcapcred(k,rr,r))
         )
*        Equals (annually scaled) demand including losses
         =g=             1e-3 * hours(s) * round(load_st(s,r,t) * (1 + loss(r)),4)
;

* * * * * Generation and capacity equations
* * * Dispatch of units cannot exceed available capacity
* af are the monthly availability factor of dispatchable power and vrsc those of intermittent renewables (reliability is used if not af)
capacity(s,ivrt(i,v,r,t))$(toptimize(t))..
                 X(s,i,v,r,t) =l=  XCS(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (vrsc(s,i,v,r)-1)$vrsc(s,i,v,r)) ;
* CHP is assumed as mustrun
capacity_chp(s,ivrt(chp(i),oldv(v),r,t))$(t.val le 2030 and toptimize(t))..
                 X(s,i,v,r,t) =e=  XCS(s,i,v,r,t) * 0.5708 ;
* Bioenergy is assumed as must-run
capacity_bio(s,ivrt(i,oldv(v),r,t))$(sameas(i,"Bioenergy") and t.val le 2030 and toptimize(t))..
                 X(s,i,v,r,t) =e=  XCS(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r))* (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (vrsc(s,i,v,r)-1)$vrsc(s,i,v,r)) ;

* * * Investment flows accumulate as new vintage capacity
invest(new(i),newv(v),r,t)$(tv(t,v) and toptimize(t))..
         XC(i,v,r,t) =l= IX(i,r,t) ;
* * * Existing vintages have fixed lifetime
* Cannot be decommissioned in 2022
exlife2022(ivrt(i,oldv(v),r,t))$(t.val le 2022 and toptimize(t))..
         XC(i,v,r,t) =e= cap(i,v,r) * lifetime(i,v,r,t) ;
* Standard exlife constraint
exlife(ivrt(i,oldv(v),r,t))$(t.val ge 2023 and toptimize(t))..
         XC(i,v,r,t) =l= cap(i,v,r) * lifetime(i,v,r,t) ;
* CHP plants cannot be decommissioned before 2030 (in llne with 2030 mustrun)       
exlife_chp(ivrt(chp(i),oldv(v),r,t))$(t.val ge 2023 and t.val le 2030 and toptimize(t))..
         XC(i,v,r,t) =e= cap(i,v,r) * lifetime(i,v,r,t) ;
* Bioenergy cannot be decommissioned before 2030 (in line with 2030 mustrun)
exlife_bio(ivrt(i,oldv(v),r,t))$(sameas(i,"Bioenergy") and t.val ge 2023 and t.val le 2030 and toptimize(t))..
         XC(i,v,r,t) =e= cap(i,v,r) * lifetime(i,v,r,t) ;

* * * New vintages have a lifetime profile for enforced retirement
newlife(ivrt(new(i),newv(v),r,t))$(not sameas(v,"2050") and toptimize(t))..
        XC(i,v,r,t) =l= lifetime(i,v,r,t) * sum(tv(tt,v), IX(i,r,tt)) ;
* * * All vintages must be monotonically decreasing (except 2050)
retire(ivrt(i,v,r,t))$(not sameas(v,"2050") and toptimize(t))..
        XC(i,v,r,t+1) =l= XC(i,v,r,t) ;
* * * Upper and lower limits on investments based on current pipeline or other regional constraints
* Upper limit
investlimUP(i,r,t)$(new(i) and conv(i) and invlimUP(i,r,t) and t.val ge 2023 and toptimize(t))..
         IX(i,r,t) =l= invlimUP(i,r,t) ;
* Lower limit
investlimLO(i,r,t)$(new(i) and invlimLO(i,r,t) and t.val ge 2023 and toptimize(t))..
         IX(i,r,t) =g= invlimLO(i,r,t) ;
* Upper limit for intermittetn renewables
investlimUP_irnw(irnw(i),r,t,quantiles)$(irnwlimUP_quantiles(i,r,quantiles) and toptimize(t) and not sameas(i,"Hydro") and t.val ge 2023)..       
        sum(irnw_mapq(i,quantiles), sum(v$(v.val le t.val), XC(i,v,r,t))) 
        =l= irnwlimUP_quantiles(i,r,quantiles) ;

* * * * * Transmission equations
* * * Enforce capacity constraint on inter-region trade flows
tcapacity(s,k,r,rr,t)$(tmap(k,r,rr) and toptimize(t))..
         E(s,k,r,rr,t) =l= TCS(s,k,r,rr,t) ;
* Accumulation of transmission capacity investments
tinvestexi(k,r,rr,t)$(tmap(k,r,rr) and t.val le 2022 and toptimize(t))..
         TC(k,r,rr,t) =l= tcap(k,r,rr) + IT(k,r,rr,t) ;
tinvestnew(k,r,rr,t)$(tmap(k,r,rr) and t.val ge 2023 and toptimize(t))..
         TC(k,r,rr,t) =l= IT(k,r,rr,t) + TC(k,r,rr,t-1) ;
* Upper limit
tinvestlimUP(k,r,rr,t)$(tmap(k,r,rr) and tinvlimUP(k,r,rr,t) and t.val ge 2022 and toptimize(t))..
         TC(k,r,rr,t) =l= tinvlimUP(k,r,rr,t) ;
* Lower limit
tinvestlimLO(k,r,rr,t)$(tmap(k,r,rr) and tinvlimLO(k,r,rr,t) and t.val ge 2022 and toptimize(t))..
         TC(k,r,rr,t) =g= tinvlimLO(k,r,rr,t) ;

* * * * * Storage equations
* * * Storage charge-discharge and accumulation
* Charge must not exceed charge capacity (size of door - entry)
chargelim(s,jvrt(j,v,r,t))$toptimize(t)..
         G(s,j,v,r,t)  =l= GCS(s,j,v,r,t) ;
* Discharge must not exceed charge capacity (size of door - exit)
dischargelim(s,jvrt(j,v,r,t))$toptimize(t)..
         GD(s,j,v,r,t) =l= GCS(s,j,v,r,t) ;
* Dynamic accumulation of storage balance (automatic discharge and charge efficiency apply here) (measured in TWh for numerical reasons) (weighting with hours(s) reflects seasonal storage)
storagebal0(s,jvrt(j,v,r,t))$(toptimize(t) and sameas(s,"1") and not sameas(j,"Storage_ST"))..
         GB(s,j,v,r,t) =e=
$if     set simpledatabase     GB("31",j,v,r,t)
$if not set simpledatabase     GB("119",j,v,r,t)
                                * (1 - dischrg(j,v,r)) + hours(s) * (G(s,j,v,r,t) * chrgpen(j,v,r)  - GD(s,j,v,r,t)) * 1e-3 ;
storagebal(s,jvrt(j,v,r,t))$(toptimize(t) and s.val ge 2 and not sameas(j,"Storage_ST"))..
         GB(s,j,v,r,t) =e= GB(s-1,j,v,r,t) * (1 - dischrg(j,v,r)) + hours(s) * (G(s,j,v,r,t) * chrgpen(j,v,r)  - GD(s,j,v,r,t)) * 1e-3 ;        
* Dynamic accumulation of storage balance (automatic discharge and charge efficiency apply here) (measured in TWh for numerical reasons) (no weighting with hours(s) reflects daily storage) (annual storage balance ensures that there is no charge/discharge selection due to weighting of hours(s))
storagebal0_st(s,jvrt(j,v,r,t))$(toptimize(t) and sameas(s,"1") and sameas(j,"Storage_ST"))..
         GB(s,j,v,r,t) =e=
$if     set simpledatabase     GB("31",j,v,r,t)
$if not set simpledatabase     GB("119",j,v,r,t)
                                * (1 - dischrg(j,v,r)) + (G(s,j,v,r,t) * chrgpen(j,v,r)  - GD(s,j,v,r,t)) * 1e-3 ;
storagebal_st(s,jvrt(j,v,r,t))$(toptimize(t) and s.val ge 2 and sameas(j,"Storage_ST"))..
         GB(s,j,v,r,t) =e= GB(s-1,j,v,r,t) * (1 - dischrg(j,v,r)) + (G(s,j,v,r,t) * chrgpen(j,v,r)  - GD(s,j,v,r,t)) * 1e-3 ;
storagebalann_st(jvrt(j,v,r,t))$(toptimize(t) and sameas(j,"Storage_ST"))..
         sum(s, hours(s) * G(s,j,v,r,t) * chrgpen(j,v,r)) * 1e-3 =g= sum(s, hours(s) * GD(s,j,v,r,t)) * 1e-3 ;
* Accumulated balance must not exceed storage capacity (size of room - reservoir) (measured in TWh for numerical reasons)
storagelim(s,jvrt(j,v,r,t))$toptimize(t)..
         GB(s,j,v,r,t) =l= ghours(j,v,r) * GCS(s,j,v,r,t) * 1e-3 ;
* * * Allow accumulation of storage charge capacity investments
ginvest(jvrt(newj(j),newv(v),r,t))$(tv(t,v) and toptimize(t))..
         GC(j,v,r,t) =l= IG(j,r,t) + GC(j,v,r,t-1)$(sameas(v,"2050") and t.val > 2050);
* * * Existing storage vintages have fixed lifetime
gexlife2022(jvrt(j,oldv(v),r,t))$(t.val le 2022 and toptimize(t))..
         GC(j,v,r,t) =e= gcap(j,v,r) * glifetime(j,v,r,t) ;
* Decommissioning possible >2015
gexlife(jvrt(j,oldv(v),r,t))$(t.val ge 2023 and toptimize(t))..
         GC(j,v,r,t) =l= gcap(j,v,r) * glifetime(j,v,r,t) ;
* Avoid decommissioning of pump storage capacty
gexlife_pump(jvrt(j,oldv(v),r,t))$(sameas(j,"PumpStorage") and t.val ge 2023 and toptimize(t))..
         GC(j,v,r,t) =e= gcap(j,v,r) * glifetime(j,v,r,t) ;
* * * New storage vintages have a lifetime profile for enforced retirement
gnewlife(jvrt(newj(j),newv(v),r,t))$(not sameas(v,"2050") and toptimize(t))..
        GC(j,v,r,t) =l= glifetime(j,v,r,t) * sum(tv(tt,v), IG(j,r,tt));
* * * All storage vintages must be monotonically decreasing (except 2050)
gretire(jvrt(j,v,r,t))$(not sameas(v,"2050") and toptimize(t))..
        GC(j,v,r,t+1) =l= GC(j,v,r,t) ;
* * * Upper and lower limits
* Upper limit
ginvestlimUP(j,r,t)$(t.val ge 2023 and toptimize(t) and ginvlimUP(j,r,t) > 0)..
         IG(j,r,t) =l= ginvlimUP(j,r,t) ;
* Lower limit
ginvestlimLO(j,r,t)$(t.val ge 2023 and toptimize(t) and ginvlimLO(j,r,t) > 0)..
         IG(j,r,t) =g= ginvlimLO(j,r,t) ;

* * * Bioenergy potential
* for whole system (allows for trade and the marginal is then the "price")
bioflow(t)$(biolim_eu(t) and (biolim_eu(t) < inf) and toptimize(t))..         BC(t)           =e= sum(r, sum(ivrt(i,v,r,t)$(sameas(i,"Bioenergy") or sameas(i,"Bio_CCS") or sameas(i,"Bio_CHP")), round(1 / effrate(i,v,r), 4) * XTWH(i,v,r,t))) ;
cumbio(t)$(biolim_eu(t) and toptimize(t))..                                   BC(t)           =l= biolim_eu(t) ;
* for each region (does not allow for system-wide trade)
bioflow_r(r,t)$(biolim(r,t) and (biolim(r,t) < inf) and toptimize(t))..       BC_r(r,t)       =e=        sum(ivrt(i,v,r,t)$(sameas(i,"Bioenergy") or sameas(i,"Bio_CCS") or sameas(i,"Bio_CHP")), round(1 / effrate(i,v,r), 4) * XTWH(i,v,r,t)) ;
cumbio_r(r,t)$(biolim(r,t) and (biolim(r,t) < inf) and toptimize(t))..        BC_r(r,t)       =l= biolim(r,t) ;        
* * * Geologic storage of carbon
* for whole system (system-wide constraints allow for trade and the marginal is then the "price")
ccsflow(t)$(toptimize(t))..                        SC(t) =e=  sum(r, sum(ivrt(i,v,r,t), co2captured(i,v,r) * XTWH(i,v,r,t))) * 1e-3 ;
cumccs..                                         sum(t, nyrs(t) * SC(t)) =l= sclim_eu ;
* for each region (does not allow for system-wide trade)
ccsflow_r(r,t)$(toptimize(t))..                    SC_r(r,t)  =e= sum(ivrt(i,v,r,t), co2captured(i,v,r) * XTWH(i,v,r,t)) * 1e-3 ;
cumccs_r(r)..                                    sum(t, nyrs(t) * SC_r(r,t)) =l= sclim(r) ;

* * * Structural equations to aid solver
xtwhdef(ivrt(i,v,r,t))$toptimize(t)..                 XTWH(i,v,r,t)   =e= 1e-3 * sum(s, X(s,i,v,r,t) * hours(s)) ;
copyxc(s,ivrt(i,v,r,t))$toptimize(t)..                XCS(s,i,v,r,t)  =e= XC(i,v,r,t)$(ord(s) eq 1)  + XCS(s-1,i,v,r,t)$(ord(s) > 1) ;
copygc(s,jvrt(j,v,r,t))$toptimize(t)..                GCS(s,j,v,r,t)  =e= GC(j,v,r,t)$(ord(s) eq 1)  + GCS(s-1,j,v,r,t)$(ord(s) > 1) ;
copytc(s,k,r,rr,t)$(tmap(k,r,rr) and toptimize(t))..  TCS(s,k,r,rr,t) =e= TC(k,r,rr,t)$(ord(s) eq 1) + TCS(s-1,k,r,rr,t)$(ord(s) > 1) ;


* * * Calibration equations
PARAMETER
gen_sola(r,t)
gen_coal(r,t)
gen_wind(r,t)
lign_out(r,t)
;

$gdxin database\setpar_%n%.gdx
$load gen_sola
$load gen_coal
$load gen_wind
$load lign_out
$gdxin

EQUATION
genUP_wind(r,t)
genUP_sola(r,t)
genUP_coal(r,t)
genLO_coallign(r,t)
genUP_lign(r,t)
;

genUP_wind(r,t)$(toptimize(t))..
    sum(ivrt(wind(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_wind(r,t) ;
       
genUP_sola(r,t)$(toptimize(t) and sameas(t,"2022"))..
    sum(ivrt(sol(i),oldv(v),r,t), XTWH(i,v,r,t)) =l= gen_sola(r,t) ;
  
* * * Coal generation adjustments because some countries had no coal capacity but generation (from two different statistics)  
set
rcoal(r)
rcoaladded(r)
;

rcoal(r)$(sameas(r,"Bulgaria") or sameas(r,"Czech") or sameas(r,"Germany") or sameas(r,"Greece") or sameas(r,"Hungary") or sameas(r,"Poland") or sameas(r,"Romania") or sameas(r,"Slovakia") or sameas(r,"Slovenia")) = YES ;
rcoal(r)$(sameas(r,"Austria") or sameas(r,"Belgium") or sameas(r,"Britain") or sameas(r,"Croatia") or sameas(r,"Denmark") or sameas(r,"Estonia") or sameas(r,"Finland") or sameas(r,"France") or sameas(r,"Ireland")) = YES ;
rcoal(r)$(sameas(r,"Italy") or sameas(r,"Netherlands") or sameas(r,"Norway") or sameas(r    ,"Spain") or sameas(r,"Sweden") or sameas(r,"Portugal")) = YES ;
rcoaladded(r)$(sameas(r,"Belgium") or sameas(r,"Estonia") or sameas(r,"Ireland") or sameas(r,"Norway")) = YES ;
 
cap("Coal","2010",rcoaladded(r))            = round(gen_coal(r,"2022") / 8.760 * 2,4) ;
discost("Coal","2010",rcoaladded(r),t)      = discost("Coal","2010","Germany",t) ;
fomcost("Coal","2010",rcoaladded(r))        = fomcost("Coal","2010","Germany") ; 
reliability("Coal","2010",rcoaladded(r))    = reliability("Coal","2010","Germany") ;
capcred("Coal","2010",rcoaladded(r))        = capcred("Coal","2010","Germany") ;
emit("Coal","2010",rcoaladded(r))           = emit("Coal","2010","Germany") ;
effrate("Coal","2010",rcoaladded(r))        = effrate("Coal","2010","Germany") ;
lifetime("Coal","2010",rcoaladded(r),t)     = lifetime("Coal","2010","Germany",t) ;
ivrt("Coal","2010",rcoaladded(r),t)         = ivrt("Coal","2010","Germany",t) ;

PARAMETER
gen_af_coal(r,t)
gen_af_lign(r,t)
gen_af_both(r,t)
gen_min_both(r,t)
;

gen_coal(r,t)$(t.val ge 2023) = smax(tt$(tt.val le 2022), gen_coal(r,tt)) ;

gen_af_coal(r,t) = sum(s, hours(s) * sum(ivrt(i,oldv(v),r,t)$(sameas(i,"Coal") or sameas(i,"Coa_CHP")), cap(i,v,r) * reliability(i,v,r) * lifetime(i,v,r,t))) * 1e-3 ;
gen_af_lign(r,t) = sum(s, hours(s) * sum(ivrt(i,oldv(v),r,t)$(sameas(i,"Lignite") or sameas(i,"Lig_CHP")), cap(i,v,r) * reliability(i,v,r) * lifetime(i,v,r,t))) * 1e-3 ;
gen_af_both(r,t) = gen_af_coal(r,t) + gen_af_lign(r,t) ; 

gen_min_both(r,t) = min(gen_af_both(r,t), gen_coal(r,t)) ;
gen_min_both("Portugal",t) = 0 ;

genUP_coal(r,t)$(toptimize(t) and rcoal(r))..
    sum(ivrt("Coal",oldv(v),r,t), XTWH("Coal",v,r,t)) + sum(ivrt("Coa_CHP",oldv(v),r,t), XTWH("Coa_CHP",v,r,t)) +
    sum(ivrt("Lig_CHP",oldv(v),r,t), XTWH("Lig_CHP",v,r,t)) + sum(ivrt("Lignite",oldv(v),r,t), XTWH("Lignite",v,r,t)) =l= gen_min_both(r,t) ;

genLO_coallign(r,t)$(toptimize(t) and rcoal(r) and sameas(t,"2022"))..
    sum(ivrt("Coal",oldv(v),r,t), XTWH("Coal",v,r,t)) + sum(ivrt("Coa_CHP",oldv(v),r,t), XTWH("Coa_CHP",v,r,t)) +
    sum(ivrt("Lig_CHP",oldv(v),r,t), XTWH("Lig_CHP",v,r,t)) + sum(ivrt("Lignite",oldv(v),r,t), XTWH("Lignite",v,r,t)) =g= gen_min_both(r,t) * 0.95 ;


* * * Coal gion adjustments because some countries had no coal capacity but generation (from two different statistics) enerat
set
rlignite(r)
;

rlignite(r)$(sameas(r,"Bulgaria") or sameas(r,"Czech") or sameas(r,"Germany") or sameas(r,"Greece") or sameas(r,"Hungary") or sameas(r,"Poland") or sameas(r,"Romania") or sameas(r,"Slovakia") or sameas(r,"Slovenia")) = YES ;

PARAMETER
lign_af(r,t) Maximum possible lignite usage according to installed capacity (TWh)
lign_min(r,t) Final lignite limit as minimum of lignite available and possibly used and coal generation (TWh)
;

lign_af(r,t) = sum(s, hours(s) * sum(ivrt(i,oldv(v),r,t)$(sameas(i,"Lignite") or sameas(i,"Lig_CHP")), cap(i,v,r) * reliability(i,v,r) * lifetime(i,v,r,t) / effrate(i,v,r))) * 1e-3 ;
lign_out(r,t)$(t.val ge 2022) = smax(tt$(tt.val le 20201), lign_out(r,tt)) ;
lign_min(r,t) = min(lign_af(r,t), lign_out(r,t)) ;

genUP_lign(r,t)$(toptimize(t) and rlignite(r))..
    sum(ivrt("Lig_CHP",oldv(v),r,t), XTWH("Lig_CHP",v,r,t)/effrate("Lig_CHP",v,r)) + sum(ivrt("Lignite",oldv(v),r,t), XTWH("Lignite",v,r,t)/effrate("Lignite",v,r)) =l= lign_min(r,t) ;
   

* * * Module for renewable generation and capacity targets
PARAMETER
sha_constant(r,t) constant after 2030
sha_extra(r,t) interpolation of growth to 2050
gen_constant(r,t)  constant after 2030
gen_extra(r,t) interpolation of growth to 2050
cap_constant(r,superirnw,t)  constant after 2030
cap_constant_int(r,superirnw,t)  constant after 2030
cap_extra(r,superirnw,t) interpolation of growth to 2050
cap_extra_int(r,superirnw,t) interpolation of growth to 2050
;

$onecho >temp\gdxxrw.rsp
par=sha_constant     rng=sha_constant!a2    rdim=1 cdim=1
par=sha_extra        rng=sha_extra!a2       rdim=1 cdim=1
par=cap_constant     rng=cap_constant!a2    rdim=2 cdim=1
par=cap_extra        rng=cap_extra!a2       rdim=2 cdim=1
$offecho

$call 'gdxxrw i=restarget\restarget.xlsx o=restarget\restarget.gdx trace=3 log=temp\restarget.log @temp\gdxxrw.rsp';

$gdxin restarget\restarget
$load sha_constant
$load sha_extra
$load cap_constant
$load cap_extra
$gdxin

gen_constant(r,t) = round(sha_constant(r,t) * daref(r,t) * (1 + loss(r)), 4) ;
gen_extra(r,t)    = round(sha_extra(r,t) * daref(r,t) * (1 + loss(r)), 4) ;

cap_constant_int(r,superirnw,t) = cap_constant(r,superirnw,t) ;
cap_constant(r,superirnw,t)$(cap_constant(r,superirnw,t) > 0) = min(cap_constant(r,superirnw,t),sum(superirnw_mapq(i,quantiles,superirnw), irnwlimUP_quantiles(i,r,quantiles)));

cap_extra_int(r,superirnw,t) = cap_extra(r,superirnw,t) ;
cap_extra(r,superirnw,t)$(cap_extra(r,superirnw,t) > 0) = min(cap_extra(r,superirnw,t),sum(superirnw_mapq(i,quantiles,superirnw), irnwlimUP_quantiles(i,r,quantiles)));

equation
resmarket(r,t) equation that ensures renewable generation target
capmarket(superirnw,r,t) equation that ensure renewable cap target
;

resmarket(r,t)$(t.val ge 2023 and toptimize(t)
$if set constantgentargets        and gen_constant(r,t)
$if set extragentargets           and gen_extra(r,t) 
    )..
    sum(ivrt(rnw(i),v,r,t), XTWH(i,v,r,t)) =g=
$if set constantgentargets        gen_constant(r,t) +
$if set extragentargets           gen_extra(r,t) +
        0 ;
  
capmarket(superirnw,r,t)$(toptimize(t) 
$if not set uselimits             and t.val ge 2023
$if     set uselimits             and t.val ge 2026   
$if set constantcaptargets        and cap_constant(r,superirnw,t)
$if set extracaptargets           and cap_extra(r,superirnw,t) 
    )..
    sum(superirnw_mapq(i,quantiles,superirnw), sum(ivrt(i,v,r,t), XC(i,v,r,t))) =g=
$if set constantcaptargets        cap_constant(r,superirnw,t) +
$if set extracaptargets           cap_extra(r,superirnw,t) +
        0 ;

* * * Module for French nuclear targets
PARAMETER
frnuctgt(r,t)
gen_frnuctgt(r,t)
;

frnuctgt(r,t) = 0 ;
$if set frnuc10     frnuctgt("France",t) = 0.1 ;
$if set frnuc20     frnuctgt("France",t) = 0.2 ;
$if set frnuc30     frnuctgt("France",t) = 0.3 ;
$if set frnuc40     frnuctgt("France",t) = 0.4 ;
$if set frnuc50     frnuctgt("France",t) = 0.5 ;
$if set frnuc60     frnuctgt("France",t) = 0.6 ;
$if set frnuc70     frnuctgt("France",t) = 0.7 ;
$if set frnuc80     frnuctgt("France",t) = 0.8 ;
$if set frnuc90     frnuctgt("France",t) = 0.9 ;
$if set frnuc100    frnuctgt("France",t) = 1.0 ;

* Avoid infeasibilities between reaching renewable and nuclear targets in France
$if set resmarket   $if set constantgentargets        frnuctgt("France",t) = 1 - sha_constant("France",t) ;
$if set resmarket   $if set extragentargets           frnuctgt("France",t) = 1 - sha_extra("France",t) ;

* Create generation target
gen_frnuctgt("France",t) = round(frnuctgt("France",t) * daref("France",t) * (1 + loss("France")),4) ;

equation
convtarget(i,r,t) generation target (after 2035)
;

convtarget(i,r,t)$(sameas(i,"Nuclear") and sameas(r,"France") and t.val ge 2035 and toptimize(t))..
    sum(ivrt(i,v,r,t)$(v.val le t.val), XTWH(i,v,r,t)) =g= gen_frnuctgt(r,t) ;

* * * Carbon market modeling
PARAMETER
co2ele_int(t)
co2ind_int(t)
co2avi_int(t)
co2shi_int(t)
co2out_int(t)
co2can_int(t)
co2indfix_int(t)
co2indorgfix_int(t)
co2indshare_int(t)
co2ele_org_int(t)
co2ind_org_int(t)
co2add_int(t)
co2allocated_int(t)
co2auctioned_int(t)
tnac_int(t)
tnacuse_int(t)
msr_int(t)
msrin_int(t)
co2eleuk_int(t)
;

$onecho >temp\gdxxrw.rsp
par=co2ele_in           rng=co2ele_in!a2             rdim=1 cdim=0
par=co2ind_in           rng=co2ind_in!a2             rdim=1 cdim=0
par=co2can_in           rng=co2can_in!a2             rdim=1 cdim=0
par=co2indfix_in        rng=co2indfix_in!a2          rdim=1 cdim=0
par=co2indshare_in      rng=co2indshare_in!a2        rdim=1 cdim=0
par=co2ele_org          rng=co2ele_org!a2            rdim=1 cdim=0
par=co2ind_org          rng=co2ind_org!a2            rdim=1 cdim=0
par=co2add_in           rng=co2add_in!a2             rdim=1 cdim=0 
par=co2allocated_in     rng=co2allocated_in!a2       rdim=1 cdim=0
par=co2auctioned_in     rng=co2auctioned_in!a2       rdim=1 cdim=0
par=msr_in              rng=msr_in!a2                rdim=1 cdim=0
par=msrin_in            rng=msrin_in!a2              rdim=1 cdim=0
par=tnac_in             rng=tnac_in!a2               rdim=1 cdim=0
par=tnacuse_in          rng=tnacuse_in!a2            rdim=1 cdim=0
par=co2eleuk_in         rng=co2eleuk_in!a2           rdim=1 cdim=0
par=co2avi_in           rng=co2avi_in!a2             rdim=1 cdim=0
par=co2shi_in           rng=co2shi_in!a2             rdim=1 cdim=0
par=co2out_in           rng=co2out_in!a2             rdim=1 cdim=0
par=co2indorgfix_in     rng=co2indorgfix_in!a2       rdim=1 cdim=0
$offecho
* * Indformula routine
* Define iterative loading loop (iter 0 always loads from the "base" files)

$call 'gdxxrw i=euetsmsr\%p%\%s%.xlsx o=euetsmsr\%p%\%s%.gdx trace=3 log=temp\%p%_%s%.log @temp\gdxxrw.rsp';
$gdxin          euetsmsr\%p%\%s%

* * Final load
$load co2ele_int=co2ele_in
$load co2ind_int=co2ind_in
$load co2avi_int=co2avi_in
$load co2shi_int=co2shi_in
$load co2out_int=co2out_in
$load co2indfix_int=co2indfix_in
$load co2indorgfix_int=co2indorgfix_in
$load co2indshare_int=co2indshare_in
$load co2ind_org_int=co2ind_org
$load co2ele_org_int=co2ele_org
$load co2add_int=co2add_in
$load co2can_int=co2can_in
$load co2allocated_int=co2allocated_in
$load co2auctioned_int=co2auctioned_in
$load tnac_int=tnac_in
$load tnacuse_int=tnacuse_in
$load msr_int=msr_in
$load msrin_int=msrin_in
$load co2eleuk_int=co2eleuk_in
$gdxin

PARAMETER
co2ele_in(t)
co2ind_in(t)
co2avi_in(t)
co2shi_in(t)
co2out_in(t)
co2can_in(t)
co2indfix_in(t)
co2indorgfix_in(t)
co2indshare_in(t)
co2ele_org(t)
co2ind_org(t)
co2add_in(t)
co2allocated_in(t)
co2auctioned_in(t)
tnac_in(t)
tnacuse_in(t)
msr_in(t)
msrin_in(t)
msrstart_in
tnacstart_in
co2eleuk_in(t)
;

co2ele_in(t) = round(co2ele_int(t), 4) ;
co2ind_in(t) = round(co2ind_int(t), 4) ;
co2avi_in(t) = round(co2avi_int(t), 4) ;
co2shi_in(t) = round(co2shi_int(t), 4) ;
co2out_in(t) = round(co2out_int(t), 4) ;
co2indfix_in(t) = round(co2indfix_int(t), 4) ;
co2indorgfix_in(t) = round(co2indorgfix_int(t), 4) ;
co2indshare_in(t) = round(co2indshare_int(t), 4) ;
co2ele_org(t) = round(co2ele_org_int(t), 4) ;
co2ind_org(t) = round(co2ind_org_int(t), 4) ;
co2can_in(t) = round(co2can_int(t), 4) ;
co2add_in(t) = round(co2add_int(t), 4) ;
co2allocated_in(t) = round(co2allocated_int(t), 4) ;
co2auctioned_in(t) = round(co2auctioned_int(t), 4) ;
tnac_in(t) = round(tnac_int(t), 4) ;
tnacuse_in(t) = round(tnacuse_int(t), 4) ;
msr_in(t) = round(msr_int(t), 4) ;
msrin_in(t) = round(msrin_int(t), 4) ;
msrstart_in = round(msr_int("2021"), 4) ;
tnacstart_in = round(tnac_int("2021"), 4) ;
co2eleuk_in(t) = round(co2eleuk_int(t), 4) ;

PARAMETER
co2elecind(t)
;

$if not set oldsheets   co2elecind(t) = co2indshare_in(t) ;
$if     set oldsheets   co2elecind(t) = (1 - co2indfix_in(t)) * co2indshare_in(t) ;
$if     set oldsheets   co2indorgfix_in(t) = co2indfix_in(t) * co2ind_org(t) ;


POSITIVE VARIABLE
TNAC(t)             Cumulative banked allowances (Mt)
;

VARIABLE
ECEU(t)             Annual flow of CO2 emissions (MtCO2) in European Union (plus Norway and Switzerland and Northern Ireland)
ECUK(t)             Annual flow of CO2 emissions (MtCO2) in UK (not Northern Ireland)
TNACUSE(t)          Allowance usage from bank (Mt)
;

* Determine some general starting/ending variables and variable ranges
TNAC.FX("2020") = tnac_in("2020") ;
TNAC.FX("2021") = tnac_in("2021") ;
TNAC.FX("2045") = 0 ;
TNAC.FX("2050") = 0 ;
TNACUSE.FX("2020") = tnacuse_in("2020") ;
TNACUSE.FX("2021") = tnacuse_in("2021") ;
TNACUSE.FX("2050") = 0 ;

$if not set banking     TNAC.FX(t)     = tnac_in(t) ;
$if not set banking     TNACUSE.FX(t)  = tnacuse_in(t) ;

EQUATION
co2floweuets(t) Annual flow of CO2 emissions (EU) (Mt)
euets(t) Cap market for CO2 emissions (EU) (Mt)
co2tnaceuets(t) Total number of allowances in circulation (EU) (Mt)
co2flowukets(t) Annual flow of CO2 emissions (UK) (Mt)
ukets(t) Cap market for CO2 emissions (UK) (Mt)
co2real2022(t) Enforcing real 2022 emissions (Mt)
;

* EU ETS
co2floweuets(t)$(toptimize(t))..             ECEU(t) =e= sum(r$(not sameas(r,"Britain")), sum(ivrt(i,v,r,t), emit(i,v,r) * XTWH(i,v,r,t))) ;
euets(t)$(toptimize(t))..                    ECEU(t) * (1 + co2elecind(t)) + co2indorgfix_in(t) + co2can_in(t) + co2avi_in(t) + co2shi_in(t)
                                                     =e= co2add_in(t) + co2allocated_in(t) + (co2auctioned_in(t) - msrin_in(t)) + TNACUSE(t) ;
co2tnaceuets(t)$(toptimize(t) and t.val le 2045 and t.val ge 2022)..         TNAC(t) =e= tnacstart_in - sum(tt$(tt.val le t.val and tt.val ge 2022), TNACUSE(tt) * nyrs(tt)) ;

* UK ETS
co2flowukets(t)$(toptimize(t))..             ECUK(t) =e= sum(r$(sameas(r,"Britain")),     sum(ivrt(i,v,r,t), emit(i,v,r) * XTWH(i,v,r,t))) ;
ukets(t)$(toptimize(t))..                    ECUK(t) =l= co2eleuk_in(t) ;

* Enforcing real 2022 emissions
co2real2022(t)$(sameas(t,"2022"))..
                        ECEU(t) =e= co2ele_in(t) ;      
                        
* * * Model fixes
* No investments before base year 2022
IX.FX(i,r,t)$(t.val le 2021) = 0 ;
* Only gas investment in base year 2022
IX.FX(i,r,"2022")$(not gas(i)) = 0 ;
* No investments before base year 2022
$if     set trans           IT.FX(k,r,rr,t)$(t.val le 2021) = 0 ;
* No investments before and in base year 2022
$if     set storage         IG.FX(j,r,t)$(t.val le 2022) = 0 ;

IX.FX(ccs(i),r,t)$(t.val le 2030) = 0 ;

* Exclude non-used technologies for reporting reasons
IX.FX("RoofPV_q99",r,t) = 0 ;
IX.FX("RoofPV_q97",r,t) = 0 ;
IX.FX("RoofPV_q95",r,t) = 0 ;
IX.FX("RoofPV_q93",r,t) = 0 ;
IX.FX("RoofPV_q91",r,t) = 0 ;
IX.FX("RoofPV_q85",r,t) = 0 ;
IX.FX("RoofPV_q75",r,t) = 0 ;

IX.FX("OpenPV_q99",r,t) = 0 ;
IX.FX("OpenPV_q97",r,t) = 0 ;
IX.FX("OpenPV_q95",r,t) = 0 ;
IX.FX("OpenPV_q93",r,t) = 0 ;
IX.FX("OpenPV_q91",r,t) = 0 ;
IX.FX("OpenPV_q85",r,t) = 0 ;
IX.FX("OpenPV_q75",r,t) = 0 ;

IX.FX("WindOn_q99",r,t) = 0 ;
IX.FX("WindOn_q97",r,t) = 0 ;
IX.FX("WindOn_q95",r,t) = 0 ;
IX.FX("WindOn_q93",r,t) = 0 ;
IX.FX("WindOn_q91",r,t) = 0 ;
IX.FX("WindOn_q85",r,t) = 0 ;
IX.FX("WindOn_q75",r,t) = 0 ;

IX.FX("WindOff_q99",r,t) = 0 ;
IX.FX("WindOff_q97",r,t) = 0 ;
IX.FX("WindOff_q95",r,t) = 0 ;
IX.FX("WindOff_q93",r,t) = 0 ;
IX.FX("WindOff_q91",r,t) = 0 ;
IX.FX("WindOff_q85",r,t) = 0 ;
IX.FX("WindOff_q75",r,t) = 0 ;

* No transmission when not set transmission
$if not  set trans       IT.FX(k,r,rr,t) = 0 ;
$if not  set trans       E.FX(s,k,r,rr,t) = 0 ;
$if not  set trans       TC.FX(k,r,rr,t) = 0 ;

* No storage when not set storage
$if not  set storage     IG.FX(j,r,t) = 0 ;
$if not  set storage     G.FX(s,j,v,r,t) = 0 ;
$if not  set storage     GB.FX(s,j,v,r,t) = 0 ;
$if not  set storage     GC.FX(j,v,r,t) = 0 ;
$if not  set storage     GD.FX(s,j,v,r,t) = 0 ;

* Remove lost load unless explicitly allowed
$if not  set lostload    BS.FX(s,r,t) = 0 ;

* * * * * Model declaration and solution
model euregen /
objdef
* * * Demand 
demand
$if      set sos                                 demand_sos
* * * Generation
capacity
$if      set chp                                 capacity_chp
$if      set biosub                              capacity_bio
invest
exlife2022
exlife
$if      set chp                                 exlife_chp
$if      set biosub                              exlife_bio
newlife
retire
investlimUP
investlimLO
investlimUP_irnw
* * * Storage
$if      set storage                             ginvest
$if      set storage                             gexlife
$if      set storage                             gexlife2022
$if      set storage                             gexlife_pump
$if      set storage                             gnewlife
$if      set storage                             gretire
$if      set storage                             chargelim
$if      set storage                             dischargelim
$if      set storage                             storagebal0
$if      set storage                             storagebal
$if      set storage                             storagebal0_st
$if      set storage                             storagebal_st
$if      set storage                             storagebalann_st
$if      set storage                             storagelim
$if      set storage                             ginvestlimUP
$if      set storage                             ginvestlimLO
* * * Transmission
$if      set trans                               tcapacity
$if      set trans                               tinvestexi
$if      set trans                               tinvestnew
$if      set trans                               tinvestlimUP
$if      set trans                               tinvestlimLO
* * * Markets, policy, and limits
* Biomass, naturgal gas, and CCS markets (or limits)
$if      set biolim                              bioflow
$if      set biolim                              cumbio
$if      set biolim_r                            bioflow_r
$if      set biolim_r                            cumbio_r
$if      set sclim                               cumccs
$if      set sclim                               ccsflow
$if      set sclim_r                             cumccs_r
$if      set sclim_r                             ccsflow_r
* CO2 market without MSR dynamic and w/o banking
$if      set co2mark                             co2floweuets
$if      set co2mark                             euets
$if      set co2mark   $if      set banking      co2tnaceuets
$if      set co2mark                             co2flowukets
$if      set co2mark                             ukets
* Calibration of CO2 markets (2022 and steering)
$if      set real2022                               co2real2022
* Renewable and nuclear targets
$if      set frnuctgt                            convtarget
$if      set resmarket                           resmarket
$if      set capmarket                           capmarket
* * * Structural equations to aid solver
xtwhdef
copyxc
$if      set storage                             copygc
$if      set trans                               copytc
* * * Calibration
$if      set calibration2022wind                 genUP_wind
$if      set calibration2022sola                 genUP_sola
$if      set calibration2022coal                 genUP_coal
$if      set calibration2022lign                 genUP_lign
$if      set calibration2022coallignlo           genLO_coallign
/;

* Intialize different CO2 markets to ensure report compiles even when the constraint is excluded
euets.M(t)                      = 0 ;
ukets.M(t)                      = 0 ;
capmarket.M(superirnw,r,t)      = 0 ;
resmarket.M(r,t)                = 0 ;
convtarget.M(i,r,t)             = 0 ;

* * * Solver switch
$if not set solver $set solver gurobi
*$if not set solver $set solver cplex
option lp=%solver% ;

euregen.optfile     = 1 ;
euregen.holdfixed   = 1 ;
euregen.reslim      = 7200 ;
option solprint     = on ;

solve euregen using lp minimizing NPVCOST ;

*Don't include report so that restart file can be used with modified report without re-running model