#' Intro Print Function
#' 
#' function that prints the intro text in \code{\link{ui}}
#' 
#' @param None
#' 
#' @return print text
#' 
#' @author mitja seibold \email{mitja.seibold@@student.uva.nl}
#' 
#' @examples 
#' 
#' @export
introPrint <- function(){
  helpText(h2("The Clinical Trial Dilemma - a Polya's Urn Simulation"),
           h3("What is the Clinical Trial Dilemma?"),
           "The clinical trial dilemma represents the problem every doctor faces, when you like to test a number of treatment to see which works best.",
           "On the one side, you as the doctor, have to make sure that each treatment gets assigned to the patient randomly.",
           "On the other side, you as the doctor, want to make sure that each patient gets the best treatment available.",
           h3("What is a Polya's Urn Simulation?"),
           "Traditionally it represents an urn with two balls (e.g., one black, one white).",
           "At each trial one ball is drawn. The ball drawn (e.g., white) is doubled and then the two (e.g., white) balls are returned into the urn.",
           "Then the next trial starts where again, a ball is drawn randomly.",
           h3("How does the Clinical Trial Dilemma and the Polyas Urn interact?"),
           "The Clinical Trial Dilemma can be simulated by a Polya's Urn Model.",
           "Your Treatments represent the different colored balls.",
           "Your number of patients represent the number of draws.",
           "Each patient is tested one after another. Every time a treatment for the patient is drawn (out of the urn) randomly.",
           "If the treatment was successfull an additional 'ball' of the treatment is put in the urn",
           "The (new) ratio of the treatments effects the new draw for the next patient.",
           h3("How does this this Shiny App work?"),
           "When you are reading this you are currently on the Dashboard.",
           "As you can see on the left side of the screen you have two more options to choose from: [User Input] and [Charts].",
           "Click on the [User Input] and you will (not surprisingly) come to the User Input Page where you can change all necessary parameters.",
           "By clicking on [Charts] you will see the output of the simulation including a plot and a summary.",
           h4("[User Input Page]"),
           "Here you have the option between [simple] and [advanced] Mode.",
           h5("In [Simple] Mode you have four parameters that you can change:"),
           h5("[1 - Number of Patients]: Here you can decide how many patients you want to test."),
           h5("[2 - Number of Simulations]: Here you can decide how many times you want to test your clinical trial."),
           h5("[3 - Number of Treatments]: Here you can decide how many treatments you want to test."),
           h5("[4 - Success Rate of Treatments]: Here you can decide the probability of a successful treatments (e.g., 70% = Treatment is successful in 70%)."),
           "In [Advanced] Mode you have more specialised options (besides the first three like in simple mode):",
           h5("[4 - Success Rate of Treatment 1 and 2]: Here you have the option to change the probability of a successful treatment for Treatment 1 and Treatment 2 individually"),
           h5("[5 - Beginn Rate of Treatment 1 and 2]: Here you can change the begin rate of the first two treatments (e.g., instead of one ball each treatment you can change it to 2:3)"),
           h5("[6 - Return Rate of Treatment 1 and 2]: Similar to the begin rate you can also change the return rate after each successful treatment (e.g., instead of 1 ball to 2:3)"),
           h5("[7 - Relapse Rate of Treatment 1 and 2]: As patients can have a relapse at some point and this is treatment related you can change the relapse rate for treatment 1 and 2"),
           h4("[Charts Page]"),
           "On the top of the Charts Page you can see three panals that represent your standard input.",
           "The Plot represents the ration of the different treatments (max three treatments are plotted).",
           "The three panals on the right side of the plot represent the 3 best treatments.",
           "Below the plot you have the option to choose which and how many treatments should be plotted.",
           h3("FAQ"),
           h5("[1.] Is this for real?"),
           "Seems like it, yes",
           h5("[2.] Can I use this for my research?"),
           "Sure, if you want to.",
           h5("[3.] I'm an influencer on instagram  - blogging about new age tech and old age art. Can I have a copy of this for free?"),
           "No. Not you. Everyone else - yes."
  )
}