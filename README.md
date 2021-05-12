# Log Periodic Power Law Singularity (LPPLS) Model 

The LPPLS model provides a flexible framework to detect bubbles and predict regime changes of a financial asset. A bubble is defined as a faster-than-exponential increase in asset price, that reflects positive feedback loop of higher return anticipations competing with negative feedback spirals of crash expectations. It models a bubble price as a power law with a finite-time singularity decorated by oscillations with a frequency increasing with time. 

Here is the model:

![LPPLS Model](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/LPPLS_Model.svg)

where:

  - ![Expected Log Price](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/Expected_Log_Price.svg) ![Colon Equals](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/coloneq.svg) expected log price at the date of the termination of the bubble
  - ![Critical Time](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/Critical_Time.svg) ![Colon Equals](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/coloneq.svg) critical time (date of termination of the bubble and transition in a new regime) 
  - ![A](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/A.svg) ![Colon Equals](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/coloneq.svg) expected log price at the peak when the end of the bubble is reached at ![Critical Time](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/Critical_Time.svg)
  - ![B](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/B.svg) ![Colon Equals](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/coloneq.svg) amplitude of the power law acceleration
  - ![C](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/C.svg) ![Colon Equals](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/coloneq.svg) amplitude of the log-periodic oscillations
  - ![m](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/m.svg) ![Colon Equals](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/coloneq.svg) degree of the super exponential growth
  - ![omega](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/omega.svg) ![Colon Equals](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/coloneq.svg) scaling ratio of the temporal hierarchy of oscillations
  - ![phi](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/phi.svg) ![Colon Equals](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/coloneq.svg) time scale of the oscillations

We implemented the Multi-Level Single-Linkage algorithm (with local optimizer L-BFGS-B) to fit the model using the reparametrization of the model according to recent papers of Dr. Sornette and others.

The repository contains the script (fitter.R) to fit the model on multiple time windows (shrinking) and the script (new_search_xxx.R) to derive the confidence indicator based on the output of fitter.R. However in the folder "data" you can find the .csv files with the confidence indicator already computed.

The confidence indicators are divided according to different time windows size.

1) SS -> Super-Short time scale: (40,183) days
2) S -> Short time scale: (40,365) days
3) M -> Medium time scale: (365,730) days
4) L -> Long time scale: (730,1460) days

They are also divided in Early Warning (EW) and End Flag (EF) based on filtering condition.
So for example, the Super-Short Early Warning indicator is SS_EW and the End Flag is SS_EF and so on.

DOWNLOAD FULL PROJECT AND RESULTS -> https://mega.nz/folder/Do9ESJaT#uoT8s09UZqGPYvrrHTgGmA
