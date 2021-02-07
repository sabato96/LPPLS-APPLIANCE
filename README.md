# Log Periodic Power Law Singularity (LPPLS) Model 

## Overview
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
    
The model has three components representing a bubble. The first, ![LPPLS Term 1](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/LPPLS_Term_1.svg), handles the hyperbolic power law. For ![m](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/m.svg) < 1 when the price growth becomes unsustainable, and at ![Critical Time](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/Critical_Time.svg) the growth rate becomes infinite. The second term, ![LPPLS Term 2](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/LPPLS_Term_2.svg), controls the amplitude of the oscillations. It drops to zero at the critical time ![Critical Time](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/Critical_Time.svg). The third term, ![LPPLS Term 3](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/LPPLS_Term_3.svg), models the frequency of the osciallations. They become infinite at ![Critical Time](https://github.com/Boulder-Investment-Technologies/lppls/raw/master/img/latex/Critical_Time.svg).
