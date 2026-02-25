******************************************************
* User-specific paths (Windows) — .dta only
******************************************************
clear all
set more off

local user = c(username)

* Defaults (edit if needed)
local DROPBOX_PATH "INSERT YOUR DIRECTORY PATH HERE"
local GITHUB_PATH  "INSERT YOUR DIRECTORY PATH HERE"

* Mapeamentos
if "`user'" == "ricar" {
    local DROPBOX_PATH "C:/Users/ricar/Dropbox/datasets_landslides"
    local GITHUB_PATH  "C:/Users/ricar/Documents/GitHub/Landslides-Project"
}
else if "`user'" == "DELL" | "`user'" == "ASSISTENTE" {
    local DROPBOX_PATH "D:/Dropbox/Research/datasets_landslides"
    local GITHUB_PATH  "C:/Users/DELL/Documents/GitHub/Landslides-Project"
}
else if "`user'" == "pedro" {
    local DROPBOX_PATH "C:/Users/pedro/Dropbox/datasets_landslides"
    local GITHUB_PATH  "C:/GitHub/Landslides-Project"
}

set seed 123

* IO
local path_input      "`DROPBOX_PATH'/build/input/"
local path_output     "`DROPBOX_PATH'/build/output/"
local path_output_git "`GITHUB_PATH'/analysis/output/"
capture mkdir "`path_output_git'"

******************************************************
* Abrir bases e preparar dados
******************************************************
quietly use "`path_output'database_panel.dta", clear

* Merge (bring only weights from the PSM dataset)
quietly merge m:1 code using "`path_output'restricted_PSM_database.dta", ///
    keepusing(weights) 
keep if _merge == 3
	
* Treatment
gen byte treated = (year >= first_year_landslide) if first_year_landslide < .
replace treated = 0 if first_year_landslide == 0
replace treated = 0 if missing(treated)

* Drop always-treated units (m == 1)
bys code: egen m = mean(treated)
drop if m == 1
drop m

* Ensure sprawl_index exists (if imported with a suffix)
capture confirm variable sprawl_index_x
if _rc==0 {
    rename sprawl_index_x sprawl_index
}

* Selection and sorting
keep code year lurban_size sprawl_index treated 
sort code year

* ================== Config ==================
local varlist lurban_size sprawl_index
local outdir "C:\Github\Landslides-Project\analysis\output\"
cap mkdir "`outdir'"

* ================== Loop ==================
foreach v of local varlist {

    sdid `v' code year treated, vce(bootstrap) seed(3854035)
    sdid_event `v' code year treated, placebo(all) vce(bootstrap)

    matrix H = e(H)
    local n  = rowsof(H)
    local rn : rownames H

    tempname R
    matrix `R' = J(`n' - 1, 4, .)   // col1=t, col2=coef, col3=lci, col4=uci

    local r = 0
    forvalues i = 1/`n' {
        local name : word `i' of `rn'
        if ("`name'" == "ATT") continue

        local t = .
        if (substr("`name'",1,7)=="Effect_")     local t = real(substr("`name'",8,.)) - 1   // 1→0, 2→1...
        else if (substr("`name'",1,8)=="Placebo_") {
            local k = real(substr("`name'",9,.))
            if (`k'==1) continue                        // do not plot Placebo_1
            local t = -`k'
        }
        else continue

        local r = `r' + 1
        matrix `R'[`r',1] = `t'
        matrix `R'[`r',2] = H[`i',1]
        matrix `R'[`r',3] = H[`i',3]
        matrix `R'[`r',4] = H[`i',4]
    }

    preserve
        clear
        quietly svmat double `R', names(col)
        rename (c1 c2 c3 c4) (t coeff lci uci)

        * Explicit baseline at t = -1
        local new_obs = _N + 1
        quietly set obs `new_obs'
        quietly replace t = -1 in `new_obs'
        quietly replace coeff = 0 in `new_obs'
        quietly replace lci = 0 in `new_obs'
        quietly replace uci = 0 in `new_obs'

        sort t

        quietly summarize t, meanonly
        local xlo = floor(r(min))
        local xhi = ceil(r(max))

        quietly summarize lci, meanonly
        local ymin = r(min)
        quietly summarize uci, meanonly
        local ymax = r(max)

        * -------- x-axis: step of 2, no label at 0 --------
        local xticks
        forvalues k = `xlo'(2)`xhi' {
            if (`k' != 0) local xticks "`xticks' `k'"
        }
        * Ensure -1 appears on the axis
        local has_m1 = 0
        foreach tick of local xticks {
            if (`tick' == -1) local has_m1 = 1
        }
        if (`has_m1' == 0) local xticks "`xticks' -1"

        * -------- ATT annotation (value + stars + SE) --------
        local ann1 "" 
        local ann2 ""
        local ann3 ""
        scalar r_att = rownumb(H,"ATT")
        if (r_att < .) {
            scalar att = H[r_att,1]
            scalar se  = H[r_att,2]

            local stars ""
            if (se>0) {
                local tstat = abs(att/se)
                if (`tstat' >= 2.576)      local stars "***"
                else if (`tstat' >= 1.96)  local stars "**"
                else if (`tstat' >= 1.645) local stars "*"
            }

            local att_s : display %6.4f att
            local se_s  : display %6.4f se

            local xrange = `xhi' - `xlo'
            local yrange = `ymax' - `ymin'

            if ("`v'" == "lurban_size") {
                local xtext = `xlo' + 0.08*`xrange'
                local y0    = `ymin' + 0.08*`yrange'
                local y1    = `y0'   + 0.04*`yrange'
                local y2    = `y1'   + 0.04*`yrange'
            }
            else {
                local xtext = `xlo' + 0.08*`xrange'
                local y2    = `ymax' - 0.08*`yrange'
                local y1    = `y2'   - 0.04*`yrange'
                local y0    = `y1'   - 0.04*`yrange'
            }

            local ann1 = `" text(`y2' `xtext' "ATT", size(small) color(black) placement(e)) "'
            local ann2 = `" text(`y1' `xtext' "`att_s'`stars'", size(small) color(black) placement(e)) "'
            local ann3 = `" text(`y0' `xtext' "(`se_s')", size(small) color(black) placement(e)) "'
        }

        twoway ///
            (rcap lci uci t, lwidth(medthin) lcolor(black)) ///
            (scatter coeff t, msymbol(O) msize(medium) mcolor(black) mlcolor(black)) ///
            , xtitle("Period") ytitle("Coefficient") ///
              xline(-1, lcolor(black) lpattern(dash)) ///
              yline(0,  lcolor(black)) ///
              xlabel(`xticks', nogrid) ///
              legend(off) ///
              graphregion(color(white)) ///
              plotregion(lcolor(white) fcolor(white) margin(medlarge)) ///
              scheme(s1mono) ///
              name(g_`v', replace) ///
              `ann1' `ann2' `ann3'

        local outfile "`outdir'\_graph_robustness_`v'_sdid_event.png"
        graph export "`outfile'", name(g_`v') width(2400) replace
    restore
}
