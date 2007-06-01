import Ifeffit, time
iff = Ifeffit.Ifeffit()
iff.ifeffit("")
iff.ifeffit("my.x = range(0,100,1)")

iff.ifeffit("my.y = sin(my.x/23)")

iff.ifeffit("plot my.x, my.y")

iff.ifeffit("pause 'h' ")
print "xx "

time.sleep(1)

iff.ifeffit("x=1\n\n")

iff.ifeffit("show @arrays")
            



