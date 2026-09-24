# Model-Based Network Visualization
A recommendation for visualization networks based on model results instead of "raw" empirical data.

In addition to archives of the manuscript (which I may remove later), this repository also includes:

- For Figure 3 - the **rap samples** visualization:
  - data/JL_samples.rds is the processed object being used here
    - src/JL_samples-raw_dataread.R provides the code for doing pulling that from original data (Note a re-IDing of the rap song IDs therein from what they worked with)
  - RapSamplesBackbone.Rmd then provides all steps for creating each of the visuals from those data.
  
**NOTE**: This example is adapted from Jennifer Lena's analysis in Lena, Jennifer C. 2004. “Meaning and Membership: Samples in Rap Music, 1979–1995.” Poetics 32(3–4):297–310. doi:10.1016/j.poetic.2004.05.006. I'm using data files provided by Mark Pachucki. The results here do not exactly match those reported in the paper, as used the "full" rather than trimmed analytic data set.

  
- For Figure 2 - the ***Grey's Anatomy* hookup** visualization:
  - data/greys.rds is the network data object used for this example
    - it was extracted from the bundle created by Chris Marcum, available at - https://github.com/cmarcum/TVHookupNetworks/blob/master/TvContacts.Rdata

**NOTE**: This example is adapted from Benjamin Lind's walkthrough: https://badhessian.org/2012/09/lessons-on-exponential-random-graph-modeling-from-greys-anatomy-hook-ups/)
  
- There are also archives of the paper drafts and early iterations, along with some presentations; I may sinkhole those later.