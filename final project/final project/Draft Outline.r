# ==============================================================================
# Finak Project
# Title: Re-Analyzing Ota, Hartsuiker and Haywood (2009).docx
# Author: Sandria Tran and Violet Manson
# Date: 11-21-2025
# ==============================================================================

#When to use: When accuracy is coded as binary (correct/incorrect, yes/no)
#The model:

#r   brm(accuracy ~ predictor,
#       family = bernoulli,
#       data = your_data)

# ==============================================================================
#Key characteristics:

#Uses logit link function
#Models probability of correct response
#Estimates are in log-odds

# EXPECTED MODEL:
#    (1) Bernoulli Regression Model with Logit Function (AKA Logistic Regression)
#    logit(π) = log(π / 1−π) = Xβ

#    (2) probability π is recovered by the inverse logit (sigmoid) function:
#    π = 1/ 1+e −Xβ
 
#    (3) The likelihood for observationsy_{i} given predictors X_{i} is modeled as Bernoulli with success probability P(y_{i} | X_{i},β) =π_i^{y_i}_{i}(1-π_{i})^{1-y_{i}}
#    (4) Parameters (β) are estimated to best fit the observed data under this model.
#    (5) SUMMARY:
#        (a) Bernoulli distribution models each outcome as 0/1 with some success probability.
#        (b) The logit function (log-odds) links the success probability to a linear predictor.
#        (c) Logistic regression is a generalized linear model with Bernoulli likelihood and the logit link.

# ==============================================================================
#Should follow this general structure:
#Paper: The KEY to the ROCK: Near-homophony in nonnative visual word recognition
# ==============================================================================
#Year: 2009
# ==============================================================================
#Authors: Mitsuhiko Ota, Robert J. Hartsuiker, Sara L. Haywood
# ==============================================================================
#Language: English
# ==============================================================================
#Population: Japanese
# ==============================================================================
#Abstract:
#To test the hypothesis that native language (L1) phonology can affect the lexical representations of nonnative words, a visual semantic-relatedness decision task in English was given to native speakers and nonnative speakers whose L1 was Japanese or Arabic. In the critical conditions, the word pair contained a homophone or near-homophone of a semantically associated word, where a near-homophone was defined as a phonological neighbor involving a contrast absent in the speaker’s L1 (e.g., ROCK–LOCK for native speakers of Japanese). In all participant groups, homophones elicited more false positive errors and slower processing than spelling controls. In the Japanese and Arabic groups, near-homophones also induced relatively more false positives and slower processing. The results show that, even when auditory perception is not involved, recognition of nonnative words and, by implication, their lexical representations are affected by the L1 phonology.
# ==============================================================================
#Data Description:

#    Subject : Participant ID
#    Procedure: Whether the trial is practice(Practice Pro) of a test trial (TrialProc)
#    Version: Trial Version
#    Contrast: Type of contrast (F filler, H homophone, LR /l~r/, P phonological, PB /p~b/).
#   Item: Item number.
#    Condition: Trial condition (whether the pair contains Control, Related, or Unrelated words),
#    WorldL: Word shown on the left-side of the screen.
#    WordR: Word shown on the right-side of the screen.
#    Words.ACC: Whether the participant correctly identified the pair being related or unrelated.
#    Words.RT: Reaction time of response in milliseconds.
# ==============================================================================
#DOI: https://doi.org/10.1016/j.cognition.2008.12.007
# ==============================================================================
#KEYWORDS: Nonnative language phonology
#          Visual Word Recognition
#          Homophone
#          Lexical representation
#          Bilingualism
#          Arabic
#          Japanese
# ==============================================================================
#OUTLINE:
#        I. Introduction & Background
#            (a) Problem: L1-L2 phonemic mismatches
#            (b) Example: Japanese /l/-/r/ difficulty
#            (c) Prior research on spoken word recognition
#            (d) Knowledge gap: effects on visual/written recognition

#        II. Theoretical Framework
#            (a) Phonological mediation in visual word recognition
#            (b) Homophone interference effects in reading
#           (c) Extension to nonnative speakers
#           (d) Near-homophones as test case

#        III. Methodology
#            (a) Participants: 20 native English, 20 Japanese, 20 Arabic speakers
#            (b) Screening task for phoneme identification
#            (c) Semantic-relatedness decision task
#            (d) Materials: 20 homophones, 20 /l-r/ pairs, 20 /p-b/ pairs
#            (e) Lexical knowledge test (offline verification)

#       IV. Results
#            (a) Accuracy analysis: Error rates by group and condition
#            (b) Reaction time analysis: Latency patterns
#            (c) Key findings: Near-homophone effects match prediction patterns

#        V. Discussion
#            (a) Confirmation of hypotheses
#            (b) Double dissociation evidence
#            (c) Implications for lexical representation theory
#            (d) Evidence against perceptual-only accounts
# ==============================================================================

#CRITICAL FINDING:
#    (a) The critical finding: participants showed processing difficulties (more errors and slower reaction times) not only with homophones but also with "near-homophones"—word pairs differing by phonological contrasts absent in the speaker's L1.

#    (b)For instance, Japanese speakers struggled with ROCK-LOCK pairs because Japanese lacks the /l/-/r/ contrast. Crucially, these effects occurred during silent visual word recognition, demonstrating that L1 phonology shapes L2 lexical representations independent of auditory perception. The results provide direct evidence that late bilinguals maintain indeterminate lexical entries for L2 words involving nonnative contrasts.
# ==============================================================================

#Key Vocabulary : Terms and Concepts
#    (a) L1: Native Language
#    (b) L2: Second Language
#    (c) Homophone: Words identicial in pronunciation but different in meaning
#    (d) Near-homophone: words differing by contrast abasent in speaker's L1
#    (e) Phonoloigcal mediation: accessing sound information during visual word recognition
#    (f) Lexical representation: mental storage of word properties including phonology
#    (g) Representational indeterminacy: Failure to maintain separate lexical entries for minimal pairs
#    (h) False positive error: Incorretly judging unrelated words as semantically related
#    (i) Semantic interference Phonological activation causing meaning-based processing difficulty

# ==============================================================================

#Paper challenges:  modularity assumption in bilingual cognitive science—the notion that knowledge and processing related to bilingual's ' first language (L1) and second language (L2) are completely independent systems or modules
#    The main idea of modularity is that cognitive processes involved in 1 language do not affect or overlap those involved in another language

# ==============================================================================
# TASK: The researchers used a visual semantic-relatedness decision task where participants judged if two words on a screen were related.

# ==============================================================================
#PARTICIPANTS: The study included native English speakers, native Japanese speakers (who lack the /l/–/r/ contrast), and native Arabic speakers (who lack the /p/–/b/ contrast). (However, only native Japanese speakers are avaialble)

#KEY CONDITIONS: The critical manipulation involved "near-homophones"—word pairs that differ by a phonological                contrast absent in a speaker's L1 (e.g., LOCK and ROCK for Japanese speakers).

# ==============================================================================
#Analysis: The study relied on frequentist statistics, specifically mixed-design ANOVAs, to analyze error rates and reaction times, reporting results separately for participants (F1) and items (F2).
# ==============================================================================
#Conclusion: They found that near-homophones caused significantly more false positive errors and slower reaction times for the relevant nonnative groups. This supported the idea that the L1 phonology leads to "indeterminate" or overlapping lexical representations for these L2 words, an effect independent of auditory misperception.

# ==============================================================================
#Goals:
#       (1) Test robustness of The KEY to the ROCK: Near-homophony in nonnative visual word         recognition (Mitsuhiko Ota, Robert J. Hartsuiker, Sara L. Haywood, 2009) by replacing the original study's by-participant (F1) and by-item (F2) ANOVAS with a Bayesian hierarchical (or multilevel model) or Multivariate regression model
#        (2) The original research Ota et al. (2009)  relied on frequentist statistics, specifically mixed-design ANOVAs, to analyze error rates and reaction times, reporting results separately for participants (F1) and items (F2) to find if the F-statistic for the interaction is large enough to yield a p-valie <.05, which allows the rejection of null hypothesis
#        (3) Re-analyze their data using a Bayesian approach is a well-founded strategy for testing the robustness of their finding
#        (4) New Bayesian model would provide nuanced interpretation of the evidence, moving beyond the binary significance testing of the original paper to estimate the magnitude and certainty of the near-homophone effect.
#        (5) Main analysis is to re-analyze the original data, your project can contribute to this discussion by confirming whether the conclusions from Ota, Hartsuiker, and Haywood (2009) hold up under a different and more modern statistical framework.
#        (6) Does the Bayesian uphold robustness of their influential findings on how L1 phonology shapes the L2 mental lexicon.
#        (7) Bayesian goal of parameter estimation (quantifying the size and uncertainty of an effect)

#        (8) Answer of the re-analyzed Bayesian model should:
#            (a) Numerically demonstrate magnitude (e..g., "an increase in probability of X&")
#            (b) compute certainty ("with a 95% credible interval of [X, Y]")
# ==============================================================================
#Re-Analyzing Ota, Hartsuiker and Haywood (2009)
#Hypothesis: The phonological system of a speaker's native language (L1) causes representational ambiguity for L2 words that contain non-native sound contrasts. This ambiguity, termed &quot;near-homophony,&quot; will cause processing interference similar to that of true homophones, even in a purely visual task where no sound is involved  (Ota, Hartsuiker, &amp; Haywood, 2009, p. 1)  (Ota, Hartsuiker, &amp; Haywood, 2009, p. 2).
# ==============================================================================

#Task:  A visual semantic-relatedness decision task in English
#Dependent Variables: Error Rates(false positives) )

# ==============================================================================
#Draft: TaskA.r: Word Accuracys of L2 Japanese Speakers in Visual Semantic-Relatedness Decision Task

#    (1) Read key-rock.csv data. Filter the dataset to only include trials where the Condition is unrelated (Isolate False Positives)

#    (2) Fit a Bayesian regression model approach to answer the following question: "When the addresser presents a message with an /l/-/r/ ambiguity, how much more likely is the addressee to misinterpret it (e.g. false positive error), compared to when the addresser presents a Spelling control (baseline message)?

#    (3) Write a paragraph reporting the model. Produce plots of the posterior distributions of the model parameters and the expected predictions of what should be the output of this bayesian model (e.g. Produce plots of the posterior distributions of the model parameters and the expected predictions of accuracy of Japanese L2 speakers' words accuracy.

#    (4) Discuss the results with your group (no need to write the discussion).

# ==============================================================================

#Data: in folder "data" -> "ota2009" -> "key-rock.csv"
# ==============================================================================
#Model Documentation:
#    (1) Load required packages
#    (2) Read the data | Read the ota2009/key-rock.csv (Data Documentation )
#    (3) Check the data structure
#    (4) Explore Visualization
#       (a) Visualize the raw data relationship
#       (b) Numeric (0/1) Conversion for visualisation
#    (5) Fit Bernoulli regression model with brms (Bayesian Model) ----
#        (a) Research question:
#        (b) (e.g. Model: X ~ Bernoulli(p) where logit(p) = β₀ + β₁ · cofactor
#    (6) Extract and visualize posterior distributions
#        (a) Plot 1: Posterior distribution of intercept (β₀)
#        (b) Plot 2: Posterior distribution of slope (β₁)
#        (c) Plot 3: Joint posterior
#        (d) Plot 4: Posterior predictive check
#    (7) Save plots to both central outputs
#    (8) Expected predictions | ???
#    (9) Use epred_draws for predictions
#    (10) Summarize predictions into mean and credible intervals
#    (11) Save p5 to both central outputs
#    (12)  Model report ----
# ==============================================================================
