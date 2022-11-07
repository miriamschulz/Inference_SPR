// August 2022
// Self-paced-reading study: MS_Inference


PennController.ResetPrefix(null); // Shorten command names (keep this line here))

// DebugOff()   // Uncomment this line only when you are 100% done designing your experiment

var counterOverride = 0;  // 0 runs list 1; 1 runs list 2; etc.
var progressBarText = "Fortschritt";  // progress bar label

// Order of the experiment: consent+instructions, experiment, prolific code, etc
Sequence(//"consent", "consent2", "demographics", "instructions",
         "practice", "end_of_prac",
         // randomizeNoMoreThan(anyOf("A", "B", "C", "D", "filler"), 2),
         rshuffle("A", "B", "C", "D", "filler"),
         // randomize("experiment"),
         "outro", "postexp_survey", "postexp_survey2",
         SendResults(), "prolific_code")


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////  HTML FILES   /////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Set default formatting for all newText and newButton:
Header(
    defaultText
        .cssContainer({"font-size": "20px", "padding-top": "50px"})
        .center()
        .print()
    ,
    defaultButton
        .cssContainer({"height": "35px", "padding-top": "20px", "padding-bottom": "20px"})
        .center()
        .bold()
        .print()
    ,
    defaultHtml
        .cssContainer({"width": "35em",  "margin": "0 auto"})
        .center()
        .print()
        .log()
)

// Consent 1
PennController("consent",
    newHtml("consent", "consent.html")
    ,
    newButton("Fortfahren")
        .wait()
)
.log("Survey", "ExperimentalSurvey")

// Consent 2
PennController("consent2",
    newHtml("consent2", "consent2.html")
        .settings.checkboxWarning("Bitte stimmen Sie der Teilnahme zu, um fortzufahren.")
        .settings.inputWarning("Bitte tragen Sie Ihre Prolific-ID ein.")
    ,
    newButton("Fortfahren")
        // Continue only if the html has been filled in:
        .wait(
              getHtml("consent2").test.complete()
                  .failure(  getHtml("consent2").warn()  )
        )
 )
 .log("Survey", "ExperimentalSurvey")

 // Demographics
PennController("demographics",
    newHtml("demographics", "demographics.html")
        .settings.inputWarning("Bitte tragen Sie Ihre Muttersprache(n) und Ihr Alter ein.")
        .settings.radioWarning("Bitte geben Sie Ihr Geschlecht und Ihre Händigkeit an.")
    ,
    newButton("Fortfahren")
        // Continue only if the html has been filled in:
        .wait(
              getHtml("demographics").test.complete()
                  .failure(  getHtml("demographics").warn()  )
        )

)
.log("Survey", "ExperimentalSurvey")

// Instructions
PennController("instructions",
    newHtml("instructions", "instructions.html")
    ,
    newButton("Übungstexte beginnen")
        .wait()
)
.log("Survey", "ExperimentalSurvey")

// Transition screen between practice and experimental trials
PennController("end_of_prac",
    newHtml("end_of_prac", "end_of_prac.html")
    ,
    newButton("Experiment starten")
        .wait()
)
.log("Survey", "ExperimentalSurvey")

// Outro: transition to postexperimental survey
PennController("outro",
    newHtml("outro", "outro.html")
    ,
    newButton("Fortfahren")
        .wait()
)
.log("Survey", "ExperimentalSurvey")

// Post-experimental survey part 1: length, difficulty, strategy, guesses
PennController("postexp_survey",
    newHtml("postexp_survey", "postexp_survey.html")
    ,
    newButton("Fortfahren")
        .wait()
)
.log("Survey", "ExperimentalSurvey")

// Post-experimental survey part 2: problems and remarks
PennController("postexp_survey2",
    newHtml("postexp_survey2", "postexp_survey2.html")
    ,
    newButton("Fortfahren")
        .wait()
)
.log("Survey", "ExperimentalSurvey")

// Prolific code
PennController("prolific_code",
    newHtml("prolific_code", "prolific_code.html")
        .wait()  // added to stay on this page
)
.log("Survey", "ExperimentalSurvey")


////////////////////////////////////////////////////////////////////////////////
////////////////////////////  EXPERIMENTAL TRIALS  /////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Practice session
Template("practice.csv", row =>
    newTrial("practice",
        newText("insSpace", "Drücken Sie Enter, um den nächsten Übungstext zu lesen.")
        // .cssContainer({"font-family": "monospace", "font-size": "20px", "padding-top": "50px", "padding-bottom": "50px", "line-height": "400%"})
        .cssContainer({"font-family": "monospace", "font-size": "20px", "padding-top": "55px"})
        .italic()
        .center()
        .print()
    ,
    newKey("showContext", "Enter") // start reading time: context sentence
        .wait()
        .log()
    ,
    getText("insSpace")
        .remove()
    ,
    newText("ContextSentence", row.ContextSentence)
        // .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px", "padding-bottom": "50px", "line-height": "120%"})
        .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px", "text-align": "center"})
        .center()
        .log()
        .print()
    ,
    newKey("removeContext", "Enter") // end reading time: context sentence
        .wait()
        .log()
    ,
    getText("ContextSentence")
        .remove()
    ,
    // newController("DashedSentence", {s:row.TargetSentence , display:"dashed", hideUnderscores: true})
    newController("DashedSentence", {s: row.TargetSentence, display: "in place", blankText: "#"})
        // .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px", "padding-bottom": "50px", "line-height": "400%"})
        .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "15px"})
        .center()
        .print()
        .log()
        .wait()
        .remove()
    ,
    newVar("DecisionRT").global().set( v => Date.now() )  // start recording decision RT
    ,
    newText("PlausibilityQuestion", "War dieser Text plausibel?")
        .center()
        .italic()
        .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px"})
        .print()
    ,
    // newText("Links", row.Left)
    newText("Links", "Nein")
    ,
    // newText("Rechts", row.Right)
    newText("Rechts", "&nbsp;Ja&nbsp;")
    ,
    newCanvas("Respond",400,400)
        .add(30,50, newText("Taste D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(300,50, newText("Taste K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(30,100, getText("Links").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px", "padding": "10px 10px 10px 10px"}))
        .add(300,100, getText("Rechts").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px", "padding": "10px 10px 10px 10px"}))
        .center()
        .print()
        .log()
    ,
    newKey("Decide", "DK")
        .wait()
        .log()
    ,
    getVar("DecisionRT").set( v => Date.now() - v )  // end recording decision RT
  )
  .log("ExpItem", row.Item)
  .log("ExpCond", row.Cond)
  // .log("ExpList", row.Group)
  .log("ExpCritical", row.Critical)
  .log("ExpTarget", row.Target)
  .log("ExpDecisionRT" , getVar("DecisionRT"))
)

// Experimental trials
Template("experiment.csv", row =>
  // newTrial("experiment",
  newTrial(row.Cond,
      newText("insSpace", "Drücken Sie Enter, um den nächsten Text zu lesen.")
        // .cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px", "line-height": "400%"})
        .cssContainer({"font-family": "monospace", "font-size": "20px", "padding-top": "55px"})
        .italic()
        .center()
        .print()
    ,
    newKey("showContext", "Enter") // start reading time: context sentence
        .wait()
        .log()
    ,
    getText("insSpace")
        .remove()
    ,
    newText("ContextSentence", row.ContextSentence)
        .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px", "text-align": "center"})
        .center()
        .log()
        .print()
    ,
    newKey("removeContext", "Enter") // end reading time: context sentence
        .wait()
        .log()
    ,
    getText("ContextSentence")
        .remove()
    ,
    newController("DashedSentence", {s: row.TargetSentence, display: "in place", blankText: "#"})
        .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "15px"})
        .center()
        .print()
        .log()
        .wait()
        .remove()
    ,
    newVar("DecisionRT").global().set( v => Date.now() )  // start recording decision RT
    ,
    newText("PlausibilityQuestion", "War dieser Text plausibel?")
        .center()
        .italic()
        .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px"})
        .print()
    ,
    // newText("Links", row.Left)
    newText("Links", "Nein")
    ,
    // newText("Rechts", row.Right)
    newText("Rechts", "&nbsp;Ja&nbsp;")
    ,
    newCanvas("Respond",400,400)
        .add(30,50, newText("Taste D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(300,50, newText("Taste K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(30,100, getText("Links").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px", "padding": "10px 10px 10px 10px"}))
        .add(300,100, getText("Rechts").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px", "padding": "10px 10px 10px 10px"}))
        .center()
        .print()
        .log()
    ,
    newKey("Decide", "DK")
        .wait()
        .log()
    ,
    getVar("DecisionRT").set( v => Date.now() - v )  // end recording decision RT
  )
  .log("ExpItem", row.Item)
  .log("ExpCond", row.Cond)
  .log("ExpList", row.Group)
  .log("ExpCritical", row.Critical)
  .log("ExpTarget", row.Target)
  .log("ExpDecisionRT" , getVar("DecisionRT"))
)
