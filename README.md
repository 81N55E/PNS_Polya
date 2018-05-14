# PNS_Polya's Urn Simulation (the Clinical Trial Dilemma)
Polya's Urn Simulation in R + Shiny for PNS-Course@UvA 

This is code for a Polya's Urn Simulation (https://en.wikipedia.org/wiki/P%C3%B3lya_urn_model) written in R+Shiny.

It was created for a course assignement at a stats course of the University of Amsterdam. 

This Simulation uses the Clinical Trial Dilemma (proposed by Wei, 1979¹) that can be represented by a Polya's Urn Simulation. The patients represent the number of trials. The number of treatments represents the number of balls (different treatments = different colors) in the urn. The number of simulations represents the number of urns.

The ui.R file contains the user interface for the Shiny app.

The server.R file contains the background calculation pipeline for the Shiny app.

The func.R file contains all necessary functions that are used for the background calculations.

# When you run the app you can choose between three options: 

[1] simple: you can choose your number of treatments and number of patients. The treatments are set to two treatments. The plot represents the first treatment (regardless whether it is the best treatment or not).

[2] intermediate: you can now also choose the number of treatments. In addition you have the option to plot up to three treatments. Where red represent the best treatment, blue the second best treatment and black the third best treatment.

[3] advanced: comming soon.


Please use the Issues-Section for any questions or remarks.

# References
¹ Wei, L. J. (1979). The generalized Polya's urn design for sequential medical trials. The Annals of Statistics, 291-296.
