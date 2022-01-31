import load.* 
import tree.*

// Load data
val sample = Load.loadMeasurements()
val notSample = Load.loadMeasurements(Selection.NotSample)

// Task 2 
val X = sample._1
val Y = sample._2
val ids = sample._3
ids.length

val ttree = Tree(X,Y)
ttree.split
ttree.split 
ttree.split
ttree.split 
ttree.height
print(ttree)

// Task 3 - predictions 
val X2 = notSample._1
val Y2 = notSample._2 
val ids2 = notSample._3 
 
ids2.length

