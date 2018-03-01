#
# Programmed by Rick Dale, Communication, UCLA
# 
# this work is motivated by the two projects
# Mantas Lukosevicius 2012, http://minds.jacobs-university.de/mantas; based heavily on his code
# Lukosevicius, M. (2012). A practical guide to applying echo state networks. In G. Orr, & K. R. Mueller (Eds.), Neural networks: Tricks of the trade (pp. 659-686). Springer Berlin Heidelberg.
# Gallicchio, C., & Micheli, A. (2017). Echo State Property of Deep Reservoir Computing Networks. Cognitive Computation, 1-14.
#

plot_columns = function(dat,typ) {
  pal = rainbow(ncol(dat))
  mx = max(dat)
  mn = min(dat)
  plot(1:nrow(dat),dat[,1],ylim=c(mn,mx),col=pal[1],type=typ)
  for (i in 2:ncol(dat)) {
    points(1:nrow(dat),dat[,i],col=pal[i],type=typ)
  }
}

run_rqa = function(ts,winsz=50,winshft=25) {
  require(crqa)
  res = crqa(ts,ts,delay=1,
       embed=1,rescale=F,radius=.001,normalize=F,mindiagline=2,
       minvertline=2,tw=1,whiteline=F,recpt=F,side="upper") # run aggregated auto RQA
  #winres = wincrqa(ts,ts,windowstep=winshft,windowsize=winsz,delay=1,
  #     embed=1,rescale=0,radius=.001,normalize=0,
  #     mindiagline=2,minvertline=2,tw=1,whiteline=F,trend=F) # get windowed RQA, across text in time
  l = min(length(ts),length(max.col(dataOut))) # fit to min length to get sq plot
  crqa_res = crqa(ts[1:l],max.col(dataOut)[1:l],delay=1,
       embed=1,rescale=0,radius=.001,normalize=0,mindiagline=2,
       minvertline=2,tw=0,whiteline=F,recpt=F,side="upper") # run aggregated auto RQA
  #crqa_winres = wincrqa(ts[1:l],max.col(dataOut)[1:l],windowstep=winshft,windowsize=winsz,delay=1,
  #     embed=1,rescale=0,radius=.001,normalize=0,
  #    mindiagline=2,minvertline=2,tw=1,whiteline=F,trend=F) # get windowed *C*RQA, across text in time
  return(list(rqa=res,crqa_res=crqa_res))
}

clean_text = function(input) { # simple clean up function, can add when needed (e.g., punctuation argument?)
  input = tolower(input)
  input = gsub("\r","",input) 
  input = gsub("\n"," ",input)
  return(input)
}

make_weight_matrix = function(x,y,connectivity='random') {
  if (connectivity=='random') {
    weight_matrix = matrix(runif(y*x,-0.5,0.5),y) # input to reservoir    
  } else if (connectivity=='small-world') { # only for reservoir-to-self weights 
    require(igraph)
    g = watts.strogatz.game(1, size=x, nei=round(log(x)), p=.1) 
    weight_matrix = (matrix(get.adjacency(g),x)) * matrix(runif(x*x,-0.5,0.5),x)
  }
  return(weight_matrix)
}

# a can be specified as a single parameter or in a range
build_deep_esn = function(sizes=c(100),in_size=25,fac=1.25,a=0.6,connectivity='random') {
  Win = make_weight_matrix(1+in_size,sizes[1],connectivity='random')
  Ws = list(Win)
  Wres = list()
  Xs = list()
  for (i in 1:length(sizes)) {
    sz = sizes[i]
    #print(i)
    W = make_weight_matrix(sz,sz,connectivity='random') # reservoir recurrent connections
    rhoW = abs(eigen(W,only.values=TRUE)$values[1]) # singular values of echo state W
    W = W * fac / rhoW # set spectral radius, related to ES property (Jaeger, 2007; see notes in Lukosevicius & Jaeger, 2009)
    Xs[[length(Xs)+1]] = matrix(rep(0,sz),ncol=1)
    if (i < length(sizes)) {
      nextsz = sizes[i+1]
      Wresres = make_weight_matrix(sz,nextsz,connectivity='random') # across reservoir connections
      rhoW = abs(eigen(W,only.values=TRUE)$values[1]) # singular values of echo state W
      Wresres = Wresres * fac / rhoW # set spectral radius, related to ES property (Jaeger, 2007; see notes in Lukosevicius & Jaeger, 2009)
      Wres[[length(Wres)+1]] = Wresres
    } 
    Ws[[length(Ws)+1]] = W
  }
  if (length(a)==1) {
    a = rep(a,length(sizes))
  }
  return(list(reservoir_states = Xs, internal_weights = Ws, layer_weights = Wres, a=a))
}

run_deep_esn = function(esn,iterations=1000,inputs=c(),passInputToHistory=F,withFeedback=0) {
  Xs = esn$reservoir_states
  Ws = esn$internal_weights
  Wres = esn$layer_weights
  #a = esn$a
  if (passInputToHistory) {
    history = matrix(0,nrow=iterations,ncol=length(c(unlist(Xs),inputs[1,])))    
  } else {
    history = matrix(0,nrow=iterations,ncol=length(unlist(Xs)))
  }
  for (t in 1:iterations){
    if (length(inputs)>0) {
      ix = t %% nrow(inputs)
      if (ix==0) { ix = nrow(inputs) }
      u = matrix(c(1,inputs[ix,]),ncol=1)
    } else {
      u = matrix(c(1,rep(0,ncol(Ws[[1]])-1)),ncol=1)
    }
    a = esn$a[1]
    if (length(Xs)>1) {
      feedback = withFeedback * t(t(Xs[[2]]) %*% Wres[[1]])
    } else {
      feedback = 0
    }
    Xs[[1]] = (1-a)*Xs[[1]] + a*tanh( Ws[[1]] %*% u + feedback + Ws[[2]] %*% Xs[[1]] )
    if (length(Xs)>1) {
      for (res in 2:length(Xs)) {
        a = esn$a[res]
        if (res<length(Xs)) {
          feedback = withFeedback * t(t(Xs[[res+1]]) %*% Wres[[res]])
        } else {
          feedback = 0
        }
        Xs[[res]] = (1-a)*Xs[[res]] + a*tanh( Wres[[res-1]] %*% Xs[[res-1]] + Ws[[res+1]] %*% Xs[[res]] )
      }
    }
    # collect reservoir activations
    if (passInputToHistory) {
      history[t,] = c(unlist(Xs),inputs[ix,])
    } else {
      history[t,] = unlist(Xs) 
    }
  }
  return(history)
}

iterate_deep_esn = function(esn,iterations=1000,Wout,passInputToHistory=F,withFeedback=0,luce=Inf,noise=0) {
  Xs = esn$reservoir_states # initialize reservoir to 0
  for (i in 1:length(Xs)) {
    Xs[[i]] = Xs[[i]]*0
  }
  Ws = esn$internal_weights
  Wres = esn$layer_weights
  a = esn$a
  if (passInputToHistory) {
    history = matrix(0,nrow=iterations,ncol=length(c(unlist(Xs)))+nrow(Wout))    
  } else {
    history = matrix(0,nrow=iterations,ncol=length(unlist(Xs)))
  }
  O = rep(0,nrow(Wout))
  for (t in 1:iterations){
    if (passInputToHistory) {
      O = c(unlist(Xs),O) %*% t(Wout)  
    } else {
      O = unlist(Xs) %*% t(Wout)  
    }
    if (luce==Inf) {
      ix = which.max(O) # for symbolic output
      O = 0*O
      O[ix] = 1
    } else {
      ix = which.max(O) # for symbolic output
      #O = O/max(O) # scale to 1 for max activated node
      O = O^luce
      O[ix] = .9
      #ix = which.max(O) # for symbolic output
      #O[ix] = 1
    }
    u = matrix(c(1,O),ncol=1)
    a = esn$a[1]
    if (length(Xs)>1) {
      feedback = withFeedback * t(t(Xs[[2]]) %*% Wres[[1]])
    } else {
      feedback = 0
    }
    Xs[[1]] = (1-a)*Xs[[1]] + a*tanh( Ws[[1]] %*% u + Ws[[2]] %*% Xs[[1]]  + noise*(runif(length(Xs[[1]]))-.5))
    if (length(Xs)>1) {
      for (res in 2:length(Xs)) {
        a = esn$a[res]
        if (res<length(Xs)) {
          feedback = withFeedback * t(t(Xs[[res+1]]) %*% Wres[[res]])
        } else {
          feedback = 0
        }
        Xs[[res]] = (1-a)*Xs[[res]] + a*tanh( Wres[[res-1]] %*% Xs[[res-1]] + feedback + Ws[[res+1]] %*% Xs[[res]]+ noise*(runif(length(Xs[[res]]))-.5) ) 
      }
    }
    # collect reservoir activations
    if (passInputToHistory) {
      history[t,] = c(unlist(Xs),O)
    } else {
      history[t,] = unlist(Xs) 
    }
  }
  readouts = history %*% t(Wout)
  return(list(history=history,readouts=readouts))  
}

make_text_input = function(input,uniqChars=c()) {
  input = tolower(input)
  if (length(uniqChars)==0) {
    uniqChars = unique(unlist(strsplit(input,'')))
  }
  dataVec = matrix(0,nrow=length(unlist(strsplit(input,''))),ncol=length(uniqChars))
  inputChars = unlist(strsplit(input,''))
  for (i in 1:length(inputChars)) {
    if (i %% 10000==0){ print(i) }
    x = inputChars[i]
    ix = which(x==uniqChars)
    dataVec[i,ix] = 1
  }  
  return(list(data=dataVec,codes=uniqChars))
}

train_esn_readout = function(history,targets) {
  reg = 1e-8  # regularization coefficient
  # regression coefficients
  Wout = t(targets) %*% history %*% solve( t(history) %*% history + reg*diag(ncol(history)) )
  preds = history %*% t(Wout)
  perf = mean(max.col(targets)==max.col(preds))
  return(list(Wout=Wout,perf=perf,preds=preds))
  # paste(codes[max.col(preds)][200:500],collapse='')
}

adaptive_esn = function(esn,inputs,targets,iterations=1000,Wout=F,withFeedback=0) {
  Xs = esn$reservoir_states
  Ws = esn$internal_weights
  Wres = esn$layer_weights
  
  if (length(Wout)==1) {
    Wout = make_weight_matrix(length(unlist(Xs)),ncol(targets),'random')
  } 
  
  # 
  P = .05 * diag(length(unlist(Xs)))
  
  errs = (1:iterations)*0
  
  for (iteration in 1:iterations){
    
    if (length(inputs)>0) {
      ix = iteration %% nrow(inputs) # get modulo if we have to loop
      if (ix==0) { ix = nrow(inputs) } # make sure last row (not 0) is used
      u = matrix(c(1,inputs[ix,]),ncol=1)
    } else { # if no input, let's just iterate with bias node on
      u = matrix(c(1,rep(0,ncol(Ws[[1]])-1)),ncol=1)
    }
    
    a = esn$a[1] # get leaky parameter for first layer
    
    if (length(Xs)>1) { # get some input from future layer... if withFeedback > 0
      feedback = withFeedback * t(t(Xs[[2]]) %*% Wres[[1]])
    } else {
      feedback = 0 # if not deep ESN, no feedback possible
    }
    
    Xs[[1]] = (1-a)*Xs[[1]] + a*tanh( Ws[[1]] %*% u + feedback + Ws[[2]] %*% Xs[[1]] )
    
    if (length(Xs)>1) { # if this is a deep ESN
      for (res in 2:length(Xs)) {
        a = esn$a[res]
        
        if (res<length(Xs)) {
          feedback = withFeedback * t(t(Xs[[res+1]]) %*% Wres[[res]])
        } else {
          feedback = 0
        }
        
        Xs[[res]] = (1-a)*Xs[[res]] + a*tanh( Wres[[res-1]] %*% Xs[[res-1]] + Ws[[res+1]] %*% Xs[[res]] )
      }
    }
    
    O = unlist(Xs) %*% t(Wout) # get output predicted from reservoir state
    
    # now, onto RLS here according to Jaeger and Soh & Demiris
    err = targets[ix,] - O
    errs[iteration] = mean(err^2)
    g = P %*% unlist(Xs) %*% pinv( .99995 + t(unlist(Xs)) %*% P %*% unlist(Xs) )
    P = (.99995)^(-1) * P - g %*% t(unlist(Xs)) %*% P * (.99995)^(-1)
    Wout = Wout + t(err) %*% t(g)
    
  }
  return(list(Wout=Wout,errs=errs))
}






