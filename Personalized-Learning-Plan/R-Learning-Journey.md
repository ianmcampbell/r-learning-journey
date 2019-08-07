---
title: "Getting Good at R Basics"
runtime: shiny
---



### Joy Payton’s Curriculum for the August 2019 R Workshop

It looks like you are just starting out with R and RStudio. I’ve created a curriculum for you that takes you from basic R syntax all the way to visual representations of your data and beyond. By following the steps outlined below, you can become very good at syntactical rules that are consistent no matter what you do down the line.

1.	Please inspect your packet. It contains Hadley Wickham’s [seminal article](https://vita.had.co.nz/papers/tidy-data.pdf) about “tidy data” (a concept that is at the heart of good coding in R), which you can read at your leisure, and numerous cheat sheets that are loosely arranged from least to most advanced, to which you can refer as you follow the instructions below.

2.	Open RStudio and follow along as you watch [Customizing RStudio](https://storage.googleapis.com/braunsb-media/Quick R Help/Customizing RStudio.mp4) (below). I have assumed that the viewer knows nothing about R and RStudio except how to launch RStudio. Go ahead and set up RStudio as you like.

<center><video width="80%" controls>
  <source src="https://storage.googleapis.com/braunsb-media/Quick R Help/Customizing RStudio.mp4" type="video/mp4">
</video></center>

Having completed these steps, you are ready to start coding. There are two alternatives here for learning R basics. Please use whichever works best for you. The first one has accompanying videos for many of the lessons. They go fast and can be helpful at anchoring concepts in your mind—especially if you are a visual learner.

[Lesson 1: Basic Building Blocks](http://www.shinyserver.tk:3838/Basic-Building-Blocks/) <br>[Lesson 2: Workspace and Files](http://www.shinyserver.tk:3838/Basic-Building-Blocks/) <br>[Lesson 3: Sequences of Numbers](http://www.shinyserver.tk:3838/Basic-Building-Blocks/) <br>[Lesson 4: Vectors](http://www.shinyserver.tk:3838/Basic-Building-Blocks/) <br>[Lesson 5: Missing Values](http://www.shinyserver.tk:3838/Basic-Building-Blocks/) <br>[Lesson 6: Subsetting Vectors](http://www.shinyserver.tk:3838/Basic-Building-Blocks/) <br>[Lesson 7: Matrices and Data Frames](http://www.shinyserver.tk:3838/Matrices-and-Data-Frames/) <br>[Lesson 8: Logic](http://www.shinyserver.tk:3838/Logic/) <br>[Lesson 9: Functions](http://www.shinyserver.tk:3838/Basic-Building-Blocks/) <br>[Lesson 10: lapply and sapply](http://www.shinyserver.tk:3838/Basic-Building-Blocks/) <br>[Lesson 11: One-Way ANOVA](http://www.shinyserver.tk:3838/One-Way-ANOVA/) <br>


### Another Alternative
There is another package besides swirl that you can use to learn R. It’s called adventr. adventr is a tutorial package created in R by Andy Field to accompany his rather unusual book starring a guy named Zach and a sarcastic cat. This tutorial uses the tidyverse rather heavily and gets into statistics earlier than the swirl tutorials do. You might find that this suits you better—and then again, you might find yourself lost and frustrated by it, in which case I suggest you go back to using swirl. 
Here is how to install and use adventr:

1.	At the console, install the package by typing 

>install.packages(“devtools”) # unless you have done this already     
library(devtools)    
install_github(“profandyfield/adventr”)

2.	Put the package in your library by typing

>library(adventr)

3.	Now run the first lesson by typing

>learnr::run_tutorial("adventr_02", package = "adventr")

No idea why it’s 02 instead of 01. But it is. Please contact me if these steps don’t work for you. 
4.	You can either read the background material about Andy Field’s book (a comic book for teaching R and statistics in R? Yes.) by following its link or read the information on the html page that opens and press continue or next topic. Along the way, you will encounter boxes where you can enter R code and see how it works. Once you get past adventr_02, the statistics begin, so you might want to stop after that. If you plan to attend the R statistics workshops and you like Any Field’s style of teaching, go on with the lessons.


### For Next Time
For next time, please ask yourself precisely how you will be using R in your workflow and, when you register, enter the specific things you need to know to be able to accomplish your goals. 
And finally, if you haven’t already, consider joining the CHOP R User Group, where you will find cookies (the edible ones), coffee, conversations, presentations, and help with your code. We have a newsletter to announce upcoming gatherings (usually held semimonthly) and an active slack user group.

<center>
  <div style="width:225px; height:150px">
[![chop-r](data/chop-r.gif "chop-r")](https://bit.ly/chop-r)
  </div>
</center><br><br>
![](data/footer.png)

