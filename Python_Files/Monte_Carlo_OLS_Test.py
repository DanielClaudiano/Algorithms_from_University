import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt 
import scipy.stats as stats

def Monte_Carlo_OLS_Test(params,alpha,t):
    parametros = []
    standard_errors=[]
    nparams = len(params)
    nsimu = 10000
    nobs = 1000
    for i in range(0,nsimu):
###artificial data    
        X = np.random.random_sample((nobs, nparams-1))
        X = sm.add_constant(X)
        beta = params
        e = np.random.random_sample(nobs)
        y = np.dot(X, beta) + e
###results
        results = sm.OLS(y, X).fit()
        parametros.append(results.params)
        standard_errors.append(results.bse)
###test estatistic
    matrix_parametros = np.matrix(parametros)  
    matrix_standard_errors = np.matrix(standard_errors) 
    matrix_t = matrix_parametros/matrix_standard_errors
    matrix_t.sort(axis=0)
    dlimits = int(nobs/((alpha/2)*100))
    limit_inf = int(nobs/dlimits)
    limit_sup = int((nobs/dlimits)*(dlimits-1))
#hypothesis test    
    for k in range(0,nparams):
        if t[k] < (matrix_t[limit_inf,k]) or t[k] > (matrix_t[limit_sup,k]):
            print(' Rejeita-se a hipótese nula de que o parâmetro beta',k,' não é significativamente diferente da mediana')  
        else:
            print(' Não rejeita-se a hipótese nula de que o parâmetro beta',k,' não é significativamente diferente da mediana')
        test = stats.normaltest(matrix_t[:,k])     
        beta = matrix_t[:,k]
# Jarque Bera test results 
        print(test)
# a linha abaixo gera o histograma da distribuição simulada dos parâmetros, porém também é printado a lista dos valores dos paramêtros, poluindo um pouco os resultados        
        print(plt.hist(beta, bins = 100);)
  
        