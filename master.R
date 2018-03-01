setwd('/Users/rickdale/Dropbox/new.projects/multiModal/Bhat and Dale/github/rnndfi')
source('esn_functions.R')
input = '123412341234'
input = clean_text(input) # do some cleaning
data = make_text_input(input) # create list of items such as localist representation, char codes
dataIn = data$data
codes = data$codes # characters generated
dataOut = dataIn[2:nrow(dataIn),] # it's prediction (then generation), so take one off
dataIn = dataIn[1:(nrow(dataIn)-1),] # one less, for prediction... so sizes match
targets = dataOut # for targets; in prior code, we might want to accumulate nepochs of target, so we separate from dataOut

sz_res = 20
esn = build_deep_esn(sizes=sz_res,in_size=ncol(dataIn),fac=1,a=.9)
history = run_deep_esn(esn,iterations=400*nrow(dataIn),inputs=c(),passInputToHistory=F)
write.table(history,row.names=F,col.names=F,file='data.txt')
system('/Users/rickdale/anaconda3/bin/python3.6m sindy_in.py > eqns.txt')
