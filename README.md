# rabbitdisease
Previous exposure to myxomatosis reduces survival of European rabbits during outbreaks of rabbit haemorrhagic disease

Accompanies paper:

Barnett, LK, TAA Prowse, DE Peacock, GJ Mutze, R Sinclair, J Kovaliski, B Cooke, CJA Bradshaw. 2018. <a href="http://doi.org/10.1111/1365-2664.13187">Previous exposure to myxoma virus reduces survival of European rabbits during outbreaks of rabbit haemorrhagic disease</a>. <em>Journal of Applied Ecology</em> 55: 2954-2962

Barnett, Louise K., Flinders University
Prowse, Thomas A. A., University of Adelaide
Peacock, David E, Biosecurity South AustraliaDepartment of Primary Industries and Regions Adelaide South Australia Australia
Mutze, Gregory J, Biosecurity South AustraliaDepartment of Primary Industries and Regions Adelaide South Australia Australia
Sinclair, Ron G., University of Adelaide
Kovaliski, John, Biosecurity South AustraliaDepartment of Primary Industries and Regions Adelaide South Australia Australia
Cooke, Brian D., University of Canberra
Bradshaw, Corey J. A., Flinders University
Publication date: May 18, 2019
Publisher: Dryad
https://doi.org/10.5061/dryad.j91d66c

## Data/code citation

Barnett, Louise K. et al. (2019), Data from: Previous exposure to myxomatosis reduces survival of European rabbits during outbreaks of rabbit haemorrhagic disease, Dryad, Dataset, https://doi.org/10.5061/dryad.j91d66c

## Abstract

1. Exploiting disease and parasite synergies could increase the efficacy of biological control of invasive species. In Australia, two viruses were introduced to control European rabbits Oryctolagus cuniculus — myxoma virus in 1950, and rabbit haemorrhagic disease virus in 1995. While these biological controls caused initial declines of > 95% in affected populations, today rabbits remain a problem in many areas, despite recurring outbreaks of both diseases. 
2. We used eighteen years of capture-mark-recapture, dead recovery, and antibody assay data from a sentinel population in South Australia to test whether these two diseases interact to modify the survival of individual wild rabbits. We compared four joint, multi-state, dead-recovery models to test the hypotheses that rabbit haemorrhagic disease and myxoma viruses have synergistic (i.e., previous exposure to one virus affects survival during outbreaks of the other virus) or additive effects (i.e., previous exposure to one virus does not affect survival during outbreaks of the other virus). 
3. Rabbit haemorrhagic disease outbreaks reduced the survival of individuals with no immunity by more than half during the 58-day capture-trip intervals, i.e., from 0.86–0.90 to 0.37–0.48. Myxomatosis outbreaks had a smaller effect, reducing survival to 0.74– 0.82; however, myxomatosis outbreaks were more prolonged, spanning more than twice as many trips. 
4. There was considerable information-theoretic support (wAICc = 0.69) for the model in which exposure to myxomatosis affected survival during rabbit haemorrhagic disease outbreaks. Rabbits previously exposed to myxoma virus had lower survival during rabbit haemorrhagic disease outbreaks than rabbits never exposed to either virus. There was negligible support for the model in which previous exposure to rabbit haemorrhagic disease affected survival in myxomatosis outbreaks (wAICc < 0.01). 
5. Synthesis and applications — Our results indicate that biological control agents can have a greater impact than single-pathogen challenge studies might suggest. Introducing additional biological control agents might therefore increase mortality of rabbits beyond the additive effects of individual biological controls. Furthermore, our results show that by understanding and exploiting disease synergies, managers could increase the efficacy of biological controls for other invasive animals.

# Usage Notes

## Rabbit capture histories

Individual capture histories by immunity state. "0" = not captured. "N" = captured with no immunity. "M" = captured with myxoma virus immunity, "R" = captured with rabbit haemorrhagic disease virus immunity, "B" = captured with immunity to both rabbit haemorrhagic disease virus and myxoma virus. Data collected by Biosecurity, South Australia, Department of Primary Industries and Regions
- CaptureHist.csv

## Trip Covariates

Details of each trapping trip, including 'date'; whether the trip was classified as an outbreak of rabbit haemorrhagic disease virus ('RHDV') or myxoma virus ('MV'); intervals between trips ('Ints') = time between one trip and the next, expressed as a ratio of the mean interval between trips (58 days)—the last interval is the time between the last trapping trip and the last dead recovery; trapping effort ('SEffort'), the number of rabbits known to be alive ('KTBA'), estimated population size based on a POPAN model ('POPAN') and associated lower and upper confidence limits ('POPANlcl' and 'POPANucl'); and the number of days since the first trip. Data collected by Biosecurity, South Australia, Department of Primary Industries and Regions.
- TripCovariates.csv

## Rabbit multi-state, dead recovery models

Multi-state, dead-recovery models for 18 years of data from Turretfield rabbit population, South Australia. Code by Louise K. Barnett, November 2017 Before running the script you will need to install program MARK from http://www.phidot.org/software/mark/downloads/ Mac and Linux users might find this post helpful: http://www.phidot.org/forum/viewtopic.php?f=21&t=3233&p=10967&hilit=install+RMark#p10967
- Rabbit_Multistate_DeadRecovery.R

## Plotting output of multi-state, dead-recovery model

This script is for plotting the output from the multi-state model script. Run the models and save the output first. Notes- Immunity state / previous exposure categories: N - Immunity to neither virus M - Immunity to myxoma virus only R - Immunity to rabbit haemorrhagic disease virus (RHDV) only B - Immunity to both viruses Age groups: Kittens ≤ 600 g (may have residual maternal immunity to RHD) Adults > 600 g (unlikely to have residual maternal immunity)
- Rabbit_Multistate_Plotting_Output.R
