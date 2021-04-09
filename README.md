# Log Periodic Power Law Singularity (LPPLS) Model 

The LPPLS model provides a flexible framework to detect bubbles and predict regime changes of a financial asset. A bubble is defined as a faster-than-exponential increase in asset price, that reflects positive feedback loop of higher return anticipations competing with negative feedback spirals of crash expectations. It models a bubble price as a power law with a finite-time singularity decorated by oscillations with a frequency increasing with time. 

Here is the model:

![LPPLS Model](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/LPPLS_Model.svg)

The repository contains the script (fitter.R) to fit the model on multiple time windows (shrinking) and the script (new_search_xxx.R) to derive the confidence indicator based on the output of fitter.R
