setwd('/Users/rickdale/Dropbox/new.projects/multiModal/Bhat and Dale/github/rnndfi')
source('esn_functions.R')
library(ggplot2)
#input = '123412341234'
input = '1212121212'
input = clean_text(input) # do some cleaning
data = make_text_input(input) # create list of items such as localist representation, char codes
dataIn = data$data
codes = data$codes # characters generated
dataOut = dataIn[2:nrow(dataIn),] # it's prediction (then generation), so take one off
dataIn = dataIn[1:(nrow(dataIn)-1),] # one less, for prediction... so sizes match
targets = dataOut # for targets; in prior code, we might want to accumulate nepochs of target, so we separate from dataOut

#eqns = readChar('eqns.txt',nchars=file.info('eqns.txt')$size)
#eqns = unlist(strsplit(eqns,'\n'))
#eqns[1]

sz_res = 10
data_res = c()
for (fac in seq(from=.2,to=2,by=.4)) {
  print(fac)
  for (j in 1:5) {
    esn = build_deep_esn(sizes=sz_res,in_size=ncol(dataIn),fac=fac,a=.9)
    history = run_deep_esn(esn,iterations=1000*nrow(dataIn),inputs=c(),passInputToHistory=F)
    history = history[1000:nrow(history),]
    write.table(history,row.names=F,col.names=F,file='data.txt')
    system('/Users/rickdale/anaconda3/bin/python3.6m sindy_in.py > eqns.txt')
    prog_length = file.info('eqns.txt')$size    
    data_res = rbind(data_res,data.frame(fac=fac,i=j,prog_length=prog_length,type='no-input'))
    
    history = run_deep_esn(esn,iterations=1000*nrow(dataIn),inputs=dataIn,passInputToHistory=F)
    history = history[1000:nrow(history),]
    write.table(history,row.names=F,col.names=F,file='data.txt')
    system('/Users/rickdale/anaconda3/bin/python3.6m sindy_in.py > eqns.txt')
    prog_length = file.info('eqns.txt')$size    
    data_res = rbind(data_res,data.frame(fac=fac,i=j,prog_length=prog_length,type='with-input'))
  }
}
plot(data_res$fac,data_res$prog_length)
ggplot(data_res)+aes(x=fac,y=prog_length,col=type)+geom_smooth()


