# summary print works

    Code
      print(print_obj, digits = 2)
    Output
      
       Conditional Model 
      Raw model coefficients:
                   estimate standard.error lower.CI upper.CI p.value    
      (Intercept)     30.33           0.38    29.58     31.1  <2e-16 ***
      X0:main_rrr1     0.87           0.63    -0.38      2.1     0.2    
      X1:main_rrr1     6.48           0.94     4.63      8.3   7e-12 ***
      X0:main_sss1     6.24           0.68     4.91      7.6  <2e-16 ***
      X1:main_sss1     4.67           0.91     2.89      6.4   3e-07 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Transformed coefficients:
                  estimate standard.error lower.CI upper.CI p.value    
      (Intercept)    30.33           0.38    29.58     31.1  <2e-16 ***
      [X=0]:amp1      6.30           0.68     4.97      7.6  <2e-16 ***
      [X=1]:amp1      7.98           0.91     6.20      9.8  <2e-16 ***
      [X=0]:acr1      1.43           0.10     1.24      1.6  <2e-16 ***
      [X=1]:acr1      0.62           0.12     0.39      0.9   1e-07 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

    Code
      print(print_obj, digits = 2)
    Output
      
       Conditional Model 
      Raw model coefficients:
                   estimate standard.error lower.CI upper.CI p.value    
      (Intercept)     30.33           0.38    29.59     31.1  <2e-16 ***
      X0:main_rrr1     0.95           0.62    -0.27      2.2     0.1    
      X1:main_rrr1     6.60           0.98     4.67      8.5   2e-11 ***
      X0:main_sss1     6.28           0.70     4.91      7.7  <2e-16 ***
      X1:main_sss1     4.58           0.93     2.75      6.4   1e-06 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Transformed coefficients:
                  estimate standard.error lower.CI upper.CI p.value    
      (Intercept)   30.328          0.375   29.593     31.1  <2e-16 ***
      [X=0]:amp1     6.351          0.697    4.985      7.7  <2e-16 ***
      [X=1]:amp1     8.030          0.926    6.214      9.8  <2e-16 ***
      [X=0]:acr1     1.420          0.098    1.227      1.6  <2e-16 ***
      [X=1]:acr1     0.607          0.123    0.365      0.8   8e-07 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      ***********************
      
       Dispersion Model 
      Raw model coefficients:
                   estimate standard.error lower.CI upper.CI p.value    
      (Intercept)     1.657          0.050    1.558      1.8  <2e-16 ***
      X0:disp_rrr1    0.189          0.090    0.012      0.4    0.04 *  
      X1:disp_rrr1   -0.041          0.123   -0.281      0.2    0.74    
      X0:disp_sss1    0.052          0.099   -0.141      0.2    0.60    
      X1:disp_sss1    0.028          0.117   -0.201      0.3    0.81    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Transformed coefficients:
                  estimate standard.error lower.CI upper.CI p.value    
      (Intercept)    1.657          0.050    1.558      1.8  <2e-16 ***
      [X=0]:amp1     0.196          0.086    0.027      0.4    0.02 *  
      [X=1]:amp1     0.050          0.121   -0.188      0.3    0.68    
      [X=0]:acr1     0.269          0.521   -0.753      1.3    0.61    
      [X=1]:acr1     2.539          2.379   -2.124      7.2    0.29    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      ***********************
      
       Zero-Inflation Model 
      Raw model coefficients:
                  estimate standard.error lower.CI upper.CI p.value
      (Intercept) -2.2e+01        5.0e+03 -9.8e+03     9742       1
      X0:zi_rrr1  -1.5e+00        6.9e+03 -1.4e+04    13508       1
      X1:zi_rrr1   8.7e-02        1.1e+04 -2.1e+04    21286       1
      X0:zi_sss1   7.3e-01        7.0e+03 -1.4e+04    13636       1
      X1:zi_sss1   2.6e-01        1.0e+04 -2.0e+04    20486       1
      
      Transformed coefficients:
                  estimate standard.error lower.CI upper.CI p.value
      (Intercept) -2.2e+01        5.0e+03 -9.8e+03     9742       1
      [X=0]:amp1   1.7e+00        7.1e+03 -1.4e+04    13939       1
      [X=1]:amp1   2.8e-01        1.0e+04 -2.0e+04    20198       1
      [X=0]:acr1   2.7e+00        4.0e+03 -7.9e+03     7893       1
      [X=1]:acr1   1.2e+00        3.9e+04 -7.7e+04    77144       1

