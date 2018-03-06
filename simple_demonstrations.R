setwd('/Users/rickdale/Dropbox/new.projects/multiModal/Bhat and Dale/github/rnndfi')

# some testing

x = 0
y = runif(1)-.5
z = runif(1)-.5
history = data.frame(x,y,z)
for (i in 1:10000) {
  x = x + 2*z
  y = y - 2*x
  z = z + (runif(1)-.5)
  history = rbind(history,data.frame(x,y,z))  
}
write.table(history,row.names=F,col.names=F,file='data.txt')
system('/Users/rickdale/anaconda3/bin/python3.6m sindy_in.py > eqns.txt')
eqns = readChar('eqns.txt',nchars=file.info('eqns.txt')$size)
eqns = unlist(strsplit(eqns,'\n'))
eqns
