library(arules)

rFile = "shopping.csv"

# self-set criterion
# associativity results must be greater than the following to be included in the report
supp = 0.1
conf = 0.5            
# lift will be calculated separately

CSV = read.transactions(rFile, sep=",")

### apriori algorithm ###
result = apriori(CSV, parameter = list(support = supp, confidence = conf, minlen = 2))
result = sort(result, by="lift", decreasing = TRUE) # display according to lift in decreasing order

inspect(result[1:6])    # display first 6 results

