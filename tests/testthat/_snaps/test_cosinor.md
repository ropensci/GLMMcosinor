# multi-component comparison works, print functions work, and mixed model spec

    
     Conditional Model 
    
     Raw formula: 
    Y ~ group + group:main_rrr1 + group:main_sss1 + group:main_rrr2 +      group:main_sss2 
    
     Raw Coefficients: 
         (Intercept)           group1 group0:main_rrr1 group1:main_rrr1 
                0.99            -0.48            -1.93             0.93 
    group0:main_sss1 group1:main_sss1 group0:main_rrr2 group1:main_rrr2 
               -0.23            -0.34            -2.07             0.99 
    group0:main_sss2 group1:main_sss2 
               -0.30            -0.26 
    
     Transformed Coefficients: 
       (Intercept)      [group=1] [group=0]:amp1 [group=1]:amp1 [group=0]:amp2 
              0.99          -0.48           1.94           0.99           2.10 
    [group=1]:amp2 [group=0]:acr1 [group=1]:acr1 [group=0]:acr2 [group=1]:acr2 
              1.03           3.02           0.35           3.00           0.26 

---

    
     Conditional Model 
    
     Raw formula: 
    Y ~ X + X:main_rrr1 + X:main_sss1 + X:main_rrr2 + X:main_sss2 +      X:main_rrr3 + X:main_sss3 + (1 | main_rrr1 + main_sss1) +      (X | main_rrr2 + main_sss2) 
    
     Raw Coefficients: 
     (Intercept)           X1 X0:main_rrr1 X1:main_rrr1 X0:main_sss1 X1:main_sss1 
           29.76         5.01         0.79        25.80         2.18         4.21 
    X0:main_rrr2 X1:main_rrr2 X0:main_sss2 X1:main_sss2 X0:main_rrr3 X1:main_rrr3 
            8.36        -0.48        -3.30        24.30        -8.63       -20.51 
    X0:main_sss3 X1:main_sss3 
            8.28       -34.38 
    
     Transformed Coefficients: 
    (Intercept)       [X=1]  [X=0]:amp1  [X=1]:amp1  [X=0]:amp2  [X=1]:amp2 
          29.76        5.01        2.32       26.14        8.99       24.30 
     [X=0]:amp3  [X=1]:amp3  [X=0]:acr1  [X=1]:acr1  [X=0]:acr2  [X=1]:acr2 
          11.96       40.03       -1.23       -0.16        0.38       -1.59 
     [X=0]:acr3  [X=1]:acr3 
          -2.38        2.11 

