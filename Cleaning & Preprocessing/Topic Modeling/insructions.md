## Instructions for Katie and Aarushi

### Start with English-Speaking Countries
We’re going to begin with the UK and New Zealand. 

- The UK data has already been filtered. The filtered file has been added to the GitHub via a shared OneDrive link (see [here](https://github.com/knutley/green_policy_response/tree/1f4c23fb56d78ecb13d8c7744d332b30b384c3ab/Cleaning%20%26%20Preprocessing/Initial%20Filtering)). It was initially over 1 million observations and is now about 148k.
- The filtering code has also been added to GitHub so you can run it for New Zealand (and later for the other language datasets once those reference lists are sorted).

### Pilot Topic Modeling in the UK
Pilot the topic modeling in the UK first, then apply the full workflow (with filtering code) to New Zealand.

#### Key Considerations for Topic Modeling
Think carefully about your decisions:
- Are you giving the model inputs first?
- If so, what inputs are you giving the model?
- Are you clustering at different a high first to filter out additional noise (I recommend)?
- Which model are you using and why?
- What optimizes efficiency?

#### Technical Recommendations:
- I strongly recommend running the clustering code in parallel processing. If you’re unsure how to do this in R, see my filtering code.
- Think carefully about how to segment and clean the data and explain each decision.
- When testing models and efficiency, do it on a small subset of observations before scaling up.
- I recommend starting with a very high-level clustering first to filter out non-climate data noise, then performing a deeper clustering for the four categories.
- The more we can do upfront to sift out the noise, the better.
- Ask Elisa with any needs/questions/roadblocks. 
