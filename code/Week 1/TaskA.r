# ==============================================================================
# Week 1
# Title: Week 1: Quantitative methods and R basics
# Author: Sandria Tran
# Date: 2025-9-26
# Lecture: Research Methods
# Workshop: R Basics | Data Summaries | R Scripts, read and summarise data
# Topic:Research methods | Quantitative data analysis | RStudio and R basics
# Workshop Objective:
#    Instructions:
#   Go through Chapter 4 and Chapter 5 of the course textbook. You will start working in groups from next week since this week is probably easier to read and try things out solo, but feel free to work in group if it works for you! The tutors will go around to ask how things are going and to answer questions or help.
#
# Learning Objectives:
#    (i) Question:
#               (1) What are the components of research methods?
#               (2) What makes a good research question and research hypothesis?
#               (3) What are the three steps of quantitative data analysis?
#               (4) What is the computational workflow of quantitative data analysis?
#    (ii) Skills:
#                (1)  Think critically about research methods and research questions.
#                (2)  Master the basics of RStudio.
#                (3)  Master the basics of the programming language R.
#                (4)  Learn how to install and use R packages.
# Important Terms & Information:
#       Course Website: https://uoelel.github.io/qml/
#       Install R: Install R from https://cloud.r-project.org
#       Install RStudio from https://posit.co/download/rstudio-desktop/
#       Lecture: https://uoelel.github.io/qml/lectures/week-01.html
#       Textbook (PDF) https://stefanocoretta.github.io/qdal/Quantitative-Data-Analysis-for-Linguists-in-R.pdf
#       Textbook(Online):https://stefanocoretta.github.io/qdal/
#           (i) Week 1: Preface and Chapters 1, 2, 3

# Notes
# Chapter 4
# Week 1 – 4 R Basics (Reference: )[1]
# 
# ## 4.1 Why R?[1]
# - R can analyse all sorts of data**: tabular (“spreadsheets”), textual, GIS (maps), images.
# - Course focus**: tabular data—all covered techniques generalize to other types.
# - Community highlights**: R is very inclusive, with global support groups:
#   - [R-Ladies](https://www.r-consortium.org/all-projects/r-ladies)
#   - [Africa R](https://r4africa.org/)
#   - [Rainbow R](https://rainbowr.netlify.app/)
# - Open source & free: R is available to all.
# 
# ## 4.2 R vs RStudio[1]
# - R: Programming language to interact with computers—commands written in a console and executed.
# - RStudio: IDE (Integrated Development Environment) with GUI for efficiency.
# - Car analogy: R = engine, RStudio = dashboard.
# - Key interfaces:
#   - Blue (left): Console—where you run R commands.
#   - Green (top right): Environment tab—lists created variables/objects.
#   - Purple (bottom right): Files tab—navigate folders/files.
# 
# ## 4.3 RStudio[1]
# - Run code example:
#   - `sum(3, 1, 4)`
#     - Output: `[1] 8` (ignore `[1]` for now).
# 
# ### 4.3.1 RStudio and Quarto Projects[1]
# - Files & Data: Reside outside RStudio (on your computer).
# - Projects:
#   - RStudio Project: Folder with `.Rproj` file.
#   - Quarto Project: RStudio Project + `_quarto.yml` file—recommended to create one per course/project.
# - Creating Projects:
#   - *New Directory* → *Quarto Project*
#   - Name it (e.g., `qml-2025`), pick folder location (avoid OneDrive for reliability), confirm creation.
# 
# - Project Navigation:
#   - Open by double-clicking `.Rproj` file, via menu `File > Open Project`, or from project list top-right in RStudio.
# 
# ### 4.3.2 Essential RStudio Settings[1]
# - Tools > Global options...
#   - Untick “Restore .RData into workspace at startup”—ensures a clean environment.
#   - Set “Save workspace to .RData on exit” to *Never*—ensures code reproducibility.
# - **Quiz 1 – Concepts**:
#   - RStudio executes code.
#   - R is a programming language.
#   - An IDE is *not* necessary to run R.
#   - RStudio projects = `.Rproj` folders.
#   - Quarto projects *are* RStudio projects.
#   - Project name shown top-right.
#   - You should have disabled workspace restore/save.
# 
# ## 4.4 R Basics[1]
# 
# ### 4.4.1 R as a Calculator[1]
# - Enter `1 + 2` in Console: `[1] 3`
# - Operations:
#   - Subtraction: `67 - 13`
#   - Multiplication: `2 * 4`
#   - Division: `268 / 43`
#   - Chaining: `6 + 4 - 1 + 2`, `4 * 2 + 3 * 2`
# - Quiz 2:
#   - `3 * 2 / 4 == 3 * (2 / 4)` → TRUE
#   - `10 * 2 + 5 * 0.2 == (10 * 2 + 5) * 0.2` → FALSE
# 
# ### 4.4.2 Variables[1]
# - Assignment: `<-` operator creates variables:
#   ```r
#   my_num <- 156
#   my_num
#   ## [1] 156
#   ```
# - Numeric Vectors: Use `c()` to combine values:
#   ```r
#   two_i <- c(6, 8)
#   three_i <- c(6, 8, 42)
#   ```
#   - All values in a vector must be of the same type.
# 
# - Variable mutability:
#   ```r
#   my_num <- 88
#   my_num <- 63
#   my_num
#   ## [1] 63
#   ```
# 
# ### 4.4.3 Functions[1]
# - Syntax: `function(argument1, argument2, ...)`
# - Examples:
#   ```r
#   sum(3, 5)           ## [1] 8
#   my_nums <- c(3, 5, 7)
#   sum(my_nums)        ## [1] 15
#   mean(my_nums)       ## [1] 5
#   ```
# - Function properties:
#   - Can take other functions as arguments.
#   - Function arguments may have defaults.
#   - Some functions (e.g., `Sys.Date()`) need no arguments.
# 
# - Comparison to Python: R is functional (focus on functions), Python is object-oriented (focus on methods).
# 
# ### 4.4.4 String and Logical Vectors[1]
# - Strings: Enclosed in double quotes.
#   ```r
#   name <- "Stefano"
#   surname <- "Coretta"
#   cat("My name is", name, surname) ## My name is Stefano Coretta
#   ```
# - Character vectors:
#   ```r
#   fruit <- c("apple", "oranges", "bananas")
#   fruit
#   ## [1] "apple" "oranges" "bananas"
#   ```
# - Logical vector :
#   ```r
#   groceries <- c("apple", "flour", "margarine", "sugar")
#   in_pantry <- c(TRUE, TRUE, FALSE, TRUE)
#   data.frame(groceries, in_pantry)
#   # table of TRUE/FALSE for each grocery
#   ```
# - Check type with `class()`:
#   ```r
#   class(FALSE)           ## [1] "logical"
#   class(c(1, 45))        ## [1] "numeric"
#   class(c("a", "b"))     ## [1] "character"
#   ```
# - Quiz 5:
#   - Not a character vector: `c(apple)` if `apple <- 45`
#   - Not a logical vector: `"FALSE"`
# 
# - Vectors in R: Cannot mix types (numbers/strings in same vector become all strings).
# 
# ### For-loops/If-else Statements (Primer for Curious Students)[1]
#   ```r
#   fruits <- c("apples", "mangos", "durians")
#   for (fruit in fruits) { cat("I like", fruit, "\n") }
#   # I like apples, I like mangos, I like durians
# 
#   for (fruit in fruits) {
#     if (grepl("n", fruit)) cat(fruit, "has an 'n'\n")
#     else cat(fruit, "does not have an 'n'\n")
#   }
#   ```
# 
# - See also: [For loops — R4DS](https://r4ds.hadley.nz/base-r#for-loops), [If-else — DataMentor](https://www.datamentor.io/r-programming/if-else-statement)
# 
# ## 4.5 Summary[1]
# - R: Programming language; RStudio: IDE.
# - Projects: Organized folders, `.Rproj`.
# - Basic math & variables: Use operators and assignment.
# - Vectors: Core concept; types: numeric, character, logical.
# - Functions: Central to R computations.
# - Learn more: [R4DS functions](https://r4ds.hadley.nz/functions), [Advanced R](https://adv-r.hadley.nz/)
# 
# All in-text references  point directly to the content on the official course site for "4 R basics – Quantitative Data Analysis for Linguists in R". External links are community and further reading resources highlighted in the original text via https://stefanocoretta.github.io/qdal/ch-r-rstudio.html)
# 
# # ==============================================================================
# 
# 
# # Chapter 5
# 
# 1. R Packages and the R Library
# 
# R packages: Plug-ins that extend R’s functionality by providing additional functions.
# 
# Base R packages: Included with the default R installation.
# 
# R library: A folder on your computer where both base and user-installed packages are stored.
# 
# RStudio’s Role: It is an interface to R, not where packages are installed ([see "Tip: R library and packages"]).
# 
# Reference: ".libPaths() returns the path(s) to your R library."
# 
# 2. Checking and Installing Packages
# 
# Checking packages: Go to the “Packages” tab (bottom-right panel in RStudio).
# 
# Installing new packages: Two main methods:
# 
# Using install.packages():
# 
# Syntax: install.packages("package_name")
# 
# Do this in the Console—not in scripts (because installation only needs to happen once per computer) [see "Important"].
# 
# Via RStudio’s Packages tab:
# 
# Click “Install” and enter package names (“cowsay”, “fortunes”).
# 
# Installed packages will appear in the Packages tab.
# 
# Reference: "You need to install a package ONLY ONCE!"—installed packages work in any future project.
# 
# 3. Attaching Packages to Your R Session
# 
# Purpose: Makes package functions available in the current session using library(package_name) ([see "Tip: Attaching packages"]).
# 
# Frequency: Do this every time you start a new R session; unlike installation, attaching is needed per session.
# 
# Syntax: library(cowsay) or library("cowsay") (both work).
# 
# Multiple packages: Use separate library() calls for each package.
# 
# Reference: Think of install.packages() as mounting a lightbulb, and library() as flipping the switch.
# 
# 4. Using Package Functions and Documentation
# 
# Accessing functions: Once a package is attached, its functions (e.g., say() from cowsay) can be used.
# 
# Example: say("hot diggity", "frog")
# 
# Getting help/documentation: Prefix function name with ? in the Console (e.g., ?say) to open official documentation ([see "5.3 Package documentation"]).
# 
# Includes “Description,” “Usage” (default values and arguments), “Arguments,” and “Examples.”
# 
# Exploratory tip: Always try examples from documentation when learning new functions.
# 
# 5. Key Reminders & Quiz
# 
# Installation (install.packages): Only needed once per computer/project environment.
# 
# Attaching (library): Required at the start of every R session.
# 
# Finding your R library: Use .libPaths() in R Console to find the library path.
# 
# Quiz example:
# 
# You attach libraries with library().
# 
# install.packages() does not load packages.
# 
# The R library is a folder (all correct statements—use quiz to reinforce learning).
# 
# References / In-Text Annotations:
# 
# "https://stefanocoretta.github.io/qdal/ch-packages.html" (source page for all summaries, tips, and concepts).
# # ==============================================================================
