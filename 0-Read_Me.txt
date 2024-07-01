This R script annotates  BEAST trees calibrated with absolute time (eg., input/0-Acariformes.BEAST2.7.runs1-6.tree.TA.tre) with various metadata: 
(1) Geological time scale (R package deeptime)
(2)Temperature and CO2 levels on Earth (from Royer et al. 2004: input/All_palaeotemps_CO2_temperature.xlsx). For convenience, a summary of these data is saved in this file: input/Temp-summary-from-Royer-et-al-2004.xlsx.
(3) Major events on Earth (see Supplementary Table 1 for references)  defined in the file input/paleoEvents.xlsx. Paleoevents are visualized as:
	3.1. Shaded boxes (data are defined the worksheet "area")
	3.2. Vertical lines  (worksheet "line"))
	3.3. Double vertical lines (worksheet "Rhynie", used to show the Rhynie chert dating range).
(4) Fossil calibration points (worksheet "fossil")

Script Author: Qixin He (Purdue University, USA)




