library(acsr)

api.key.install(key="926983368457604ed165dc0d2618e58152397d22")

f = fread('notebooks/data/moe_FS_16.csv')

out = sumacs(f$formula, varname=f$myfield, method=f$type,
        level = 'county.subdivision', endyear=2016, 
        file = 'example.csv')
