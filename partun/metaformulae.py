import re
import numpy as np
from scipy.integrate import odeint
from sklearn.model_selection import train_test_split

from sparsereg.model import SINDy

x = np.loadtxt("temp.txt")
print(x.shape)

#xd = x[:,0:3]
#print(xd.shape)

#x_train, x_test = x[:5000], x[5000:]

kw = dict(fit_intercept=True, normalize=False)
wsz = 1000
for i in range(450,500):
	x_train = x[i:(i+wsz)]
	model = SINDy(dt=0.1, degree=1, threshold=0.5, alpha=1.0, n_jobs=-1, kw=kw).fit(x_train)
	xs = []
	for i, eq in enumerate(model.equations()):
		xs.append(len(re.findall('\+',eq)))
	print(np.mean(xs))

#myops = {'sin': np.sin, 'cos': np.cos}
#myops = {'tanh': np.tanh}
#model = SINDy(dt=1, degree=1, threshold=0.05, operators=myops,
#              alpha=1.0, n_jobs=-1, kw=kw).fit(x_train)

#print("Score on test data ", model.score(x_test))

#for i, eq in enumerate(model.equations()):
#	print(len(re.findall('\+',eq)))

#for i, eq in enumerate(model.equations()):
#    print("dx_{} / dt = ".format(i), eq)

