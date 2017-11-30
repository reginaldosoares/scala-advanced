case class Dog (name: String)

case class Person (item: Any, name: String)

def echoWhatYouGaveMe(x: Any): String = x match {

  // constant patterns
  case 0 => "zero"
  case true => "true"
  case "hello" => "you said 'hello'"
  case Nil => "an empty List"

  // sequence patterns
  case List(0, _, _) => "a three-element list with 0 as the first element"
  case List(1, _*) => "a list beginning with 1, having any number of elements"
  case Vector(1, _*) => "a vector starting with 1, having any number of elements"

  // tuples
  case (a, b) => s"got $a and $b"
  case (a, b, c) => s"got $a, $b, and $c"

  // constructor patterns
  case Person(first, "Alexander") => s"found an Alexander, first name = $first"
  case Dog("Suka") => "found a dog named Suka"

  // constructors with wildcards

  // typed patterns
  case s: String => s"you gave me this string: $s"
  case i: Int => s"thanks for the int: $i"
  case f: Float => s"thanks for the float: $f"
  case a: Array[Int] => s"an array of int: ${a.mkString(",")}"
  case as: Array[String] => s"an array of strings: ${as.mkString(",")}"
  case d: Dog => s"dog: ${d.name}"
  case list: List[_] => s"thanks for the List: $list"
  case m: Map[_, _] => m.toString

  // infix operations patterns

  // boolean extractor patterns

  // matching with guards

  // using my personal extractor

  // matching partial functions

  // the default wildcard pattern
  case _ => "Unknown"
}

echoWhatYouGaveMe((1, "Segundo", false))


val oneToFour = List(1, 2, 3, 4)

def sumElements(listOfInts: List[Int]) : Int = listOfInts match {
  case List() => 0
  // If it is an empty list, just return zero
  case head :: rest => head + sumElements(rest)
  // Otherwise, recurse and sum elements
}

sumElements(oneToFour)


val odds = List(1, 3, 5, 7)

def findWhereSecondElemIsAThree(listOfInts: List[Int]) = listOfInts match {
  case List(_, 3, _*) => "Found a list where second element == 3"
  case List(_*) => "List with 0 or more elements"
}

findWhereSecondElemIsAThree(oneToFour)
findWhereSecondElemIsAThree(odds)


val doubleA1 = ("A", 1)
val doubleA2 = ("A", 2)
val doubleB1 = ("B", 3)

def tupleMatcher(tuple: (String, Int)) = tuple match {
  case (_,1) => "matched A1 !"
  case (_,2) => "matched A2 !"
  case (a, _) if a == "B" => "matched and using a guard for B !"
}

tupleMatcher(doubleA2)
tupleMatcher(doubleB1)


val head :: tail = List(1, 2, 3)


case class X(a: Int, b: String)
val a X b = X(1, "two")



val divide: (Int) => Int = (x: Int) => 42 / x

val divide2: PartialFunction[Int, Int] = {
  case d: Int if d != 0 => 42 / d
}

divide{10}
divide(10)


val integers = List(1,2,3,4,5)

integers.foreach(x => println (divide (x)))

integers.map(divide).foreach(println)

integers.map(divide(_))

integers map divide foreach println


List (1,2,3) map divide

List (0,1,2,3) collect divide2