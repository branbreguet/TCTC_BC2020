# TCTC_BC2020
BC election 2020 code to run simulations

This code (and files) is allowing anyone to run the same simulations I do to project the 2020 BC election. You can see it as a complement to the traditional 'simulator' I post on my site.

1. What does it do?

Essentially, it does two randomizations, with some traditional projections based on the swing in-between.

The first randomization is at the province level. Basically, if (say) the BC Liberals are at 40% in average in the polls, they might actually be at 37% or at 43%. So the first randomization takes care of this uncertainty. It does so by simulating N draws of size n from a multinomial distribution where the probabilities are given by the voting intentions (what most of you will likely want to change). I use N=2000 by default but usually increase it to 10 or 20k for the final projections. Keep in mind however that the higher N, the longer it'll take. My desktop (few years old, core I5) takes roughly 5 seconds to run 2k simulations but would take closer to 30sec+ for 10k. The code itself isn't really optimized (and I suspect some people will find it quite inefficient, borderline stupid at times), but it does what I need it to do and I can wait 30 seconds once in a while. So that's all I care about.

The size n is the sample size and determines the actual margins of error. In other words, during the N simulations, how far from 40% should we go? A typical 1,000 respondent poll would have margins of error of roughly 3%. Projections, however, use a polling average which is equivalent to using one giant poll of size 5 or 6k and thus having really small MoE. On the other hand, using past averages and electoral results, I have estimated the actual, empirical accuracy of polls to be closer to 5% (see: https://www.tooclosetocall.ca/2019/09/how-accurate-are-polls-in-canada-less.html). It's quite often 2-3% but then, every once in a while, you have an Alberta 2012, BC 2013 or Quebec 2018 case where polls are really off. At the end, I use n=400 which creates MoE of 4.8%. You might think it's too wide and you can just increase n then, but I strongly suggest you keep n=400. I have calibrated this model over multiple elections and n=400 has worked perfectly so far (as far as modeling the uncertainty).

The second stage is to calculate the swing (simulated results province-wide minus the results last time around) for each party and apply this swing in every riding. This is your typical linear swing model, except you do it N times with variations. The 2020 BC model currently uses a basic linear swing. I'll likely change this (by adding regional effects, socio-demographic variables, etc) later on. You can try to be as fancy as it gets for this step (and I have in the past with regression coefficients), the truth is that what really matters is getting the voting % right in step 1. If you get that part right, your projections will likely be good. No matter how fancy your model is, if you input the wrong %, you'll wrong (or you'll be right because you are lucky and the errors are cancelling out). As opposed to the advanced simulator, the code here only uses the province-wide percentages. That's my favourite model. I usually have regional adjustments if I see the regional polling averages being very off though.

The second randomization exists because knowing the province-wide percentages isn't enough. Even knowing the regional ones would still not allow you to perfetly predict each riding. If a region (say Metro Vancouver) has an average swing of -5 for the Liberals, the actual swing in each riding could be 0% or could be -15% in some. The second randomization therefore re-sample at the riding level for every simulation. Basically, if you have (say) the Liberals at 35% in riding 1 and the NDP at 25%, these two parties could actually be at 31% and 29% for instance. The randomization will take care of those possibilities. For the second round, n=200. This creates pretty wide intervals but there as well, this is all calibrated based on past elections.

The two randomizations are independent from each other. It means a party could for instance be sampled higher than its polling average but then be inefficient at the riding level. Over 2,5 or 10k simulations, the average won't change much but the tails will become fatter (think a situation where a party not only beats its polling number but also has a crazy efficient vote -- Like the LPC in 2019).


2. How to use it?

It's a R code. I personally use Rstudio as I like a more visual interface but the code should absolutely work in R itself.

Because of previous elections, the BC Liberals are coded as 1, NDP as 2, Green as 3, BC Cons. as 4 and independents as 5.

You need the code as well as a few files, all provided:

a) Results of the 2017 election, per riding.

b) A file called 'adjustments' where I make manual (and systematic) adjustments at the riding level. For instance the BC Green losing Andrew Weaver is not good for them, so I dropped the Green by 10% in his former riding. Some adjustments can be arbitrary (the Andrew Weaver one definitely is) while others are the results of regional adjustments (using polling averages) or riding-level polls. I also have estimated that for the main parties, losing a long term incumbent (more than 2 mandates) usually causes a drop of 5%, although it can vary a lot. The file is provided as in with no explanation. Email me if you have questions. You can also reset all the cells to 0 if you think my adjustments are stupid.

That's all for now. If you see any mistake, please let me know. If you use this code to publih anything (blog, tweet, etc), please give me (or my blog) credit. If you think this code is garbbage, make sure to mention it on r/CanadaPolitics and collect free Karma.
