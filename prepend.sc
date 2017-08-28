val a = Map("AL" -> "Alabama")
val b = a + ("AK" -> "Alaska")


val x = List(1, 2, 3, 4, 5, 6, 7, 8)

val xEnh = 1 to 9

xEnh reduce (_+_)



val xy = List(1.2, 2.3, 3.3, 4.6, 5.9, 6.4, 7.5, 8.0)

def mAverage(list: List[Double]): Double =
  list.foldLeft(0.0)(_+_) / list.foldLeft(0.0)((r,_) => r+1)

mAverage(xy)
