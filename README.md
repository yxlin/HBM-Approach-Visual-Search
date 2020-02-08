HBM-Approach-Visual-Search
==========================

Using HBM to test Palmer et al's (2011) and Wolfe et al's (2010) benchmark visual search (slightly modified) paradigms in the context of target-distractors relation. The aim of this project is to see if we can probe, if there is any, cognitive processes, using HBM. The target cognitive process is the mechanism of search group segmentation, (see the SERR model of visual search in Humphreys & Müller, 1993).  We collected, relatively speaking, a small data set, using similar paradigms  and tested it in the HBM. The results were compared with Palmer et al's (2011) and Wolfe et al's (2010) findings. 

We thank Jeremy Wolfe and Evan Palmer kindly provide their visual search data at 
http://search.bwh.harvard.edu/new/data_set.html, as well as their permission for us to use the data set. 

## Getting Started

1. Find the data folder
The data folder archives all data sets used in this study as the tarball, 'rawRTData.tar.bz2'. The tarball can be unzipped by using the tools such as 7-zip (7-zip.org). After unzipping the tarball, individual data files can be accessed by renaming the unzipped folder as 'myData', which follows the path, from the project folder, 'HBM-Approach-Visual-Search', 'data', 'myData' and then 'featureData', if you are to analyze the feature search data set.

2. Build a data set
There are three data sets, each for the feature, conjunction, and spatial configuration searche tasks.We used the data set of the feature search task as an example. Firstly, we navigated to the 'featureData' folder and set the working directory.

```
setwd('~/HBM-Approach-Visual-Search-master/data/myData/featureData')
system("find . -name *.data | xargs ls -rt1 > nameList.txt")
```

We used the three Linux command line tools, 'find', 'xargs' and 'ls' to compile a list of file names from individual participants. See 'man find', 'man xargs' and 'man ls' for the options, '-name', '-r', and '-t' and '-l'.

After building the 'nameList.txt' (which should also be in the 'featureData' folder), we then ran the 'buildData.R' or 'bindAllParticipantsTogether.R' script. The difference between them is that only the latter collected the background information, such gender, handedness etc., from a csv file, 'prescreen_data17Feb2013, which stores questionnaire responses. Both scripts should result in identical response times and response choices. 

Next step branches off, depending on which analyses one wants to run. In the following, we illustrated fitting hierarchical Weibull model to the response time data as an example.

3. Fit Hierarchical Weibull Model
'BayesRuns' was to fit the data with hierarchical Weibull model. The purpose was to estimate three Weibull parameter.  First open the 'BayesRunFeature.R' to fit the feature search data set.


```
jagsfit <- jags(data=JAGSName, 
                model.file='./BayesRuns/model.txt', 
                inits=initList, parameters.to.save=parameters, 
                n.iter=ni, n.chain = nc, n.burnin = nb, 
                n.thin = nt)
```

The main result from the 'BayesRunFeature.R' script is the 'jagsfit', which stores
the results of model fits. These are the parameter estimations of the Weibull model.


Copyright (c) Yi-Shin Lin

Download the paper at [Lin, YS., Heinke, D. & Humphreys, G.W. Atten Percept Psychophys (2015) 77: 985. https://doi.org/10.3758/s13414-014-0825-x](https://link.springer.com/article/10.3758/s13414-014-0825-x)

Go direct to [the data folder](https://github.com/yxlin/HBM-Approach-Visual-Search/tree/master/data)


HBM-Approach-Visual-Search by Yi-Shin Lin, is licensed under a 
[Creative Commons Attribution-NonCommercial 4.0 International License.](http://creativecommons.org/licenses/by-nc/4.0/)

![alt text](http://i.creativecommons.org/l/by-nc/4.0/88x31.png)









