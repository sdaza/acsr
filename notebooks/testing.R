library(acsr)

key = readLines('notebooks/data/api.txt')
api.key.install(key=key)

f = fread('notebooks/data/moe_FS_16.csv')

out = sumacs(f$formula, varname=f$myfield, method=f$type,
        level = 'county.subdivision', endyear=2016, 
        file = 'notebooks/example.csv')
