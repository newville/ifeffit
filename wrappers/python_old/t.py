import Ifeffit
i = Ifeffit.Ifeffit()

i.ifeffit("m.x = range(1,2,0.01)")
i.ifeffit("m.y = sqrt(m.x)")

m = i.list_arrays()
print m
