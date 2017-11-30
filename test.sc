
val list: List[Any] = List(
  "a string",
  732,  // an integer
  'c',  // a character
  true, // a boolean value
  () => "an anonymous function returning a string"
)
list foreach (element => println(element))
list foreach println


val mixedItems: List[Any] = List (
  "Str Item",
  1,
  2,
  List (1,3,5)
)


val someItens = List("s")

val tp: (String, Int, Int, List[Int]) = ("Str Item", 1,2,List(1,3,5))




mixedItems.flatMap( m =>
  m match {
    case s: String => "a"
    case i: Int => "b"
    case l: Seq[_] => l flatMap (_.toString)
    case _ => "c"
  }

)



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


echoWhatYouGaveMe(mixedItems)
echoWhatYouGaveMe(true)
echoWhatYouGaveMe(() => "ahaha")


import java.io.File
sealed trait RandomThing
case class RandomFile(f: File) extends RandomThing
case class RandomString(s: String) extends RandomThing
//case class RandomList(l: List[_]) extends RandomThing

class RandomNoiseMaker {
  def makeRandomNoise(t: RandomThing): String = t match {
    case RandomFile(f) => f.getAbsolutePath
    case RandomString(s) => s.toString
  }
}




val rnm = new RandomNoiseMaker

println(rnm.makeRandomNoise(RandomString("aa")))
//println(rnm.makeRandomNoise(RandomList(List(1,2,3))))



def filter(xs: List[Int], p: Int => Boolean): List[Int] =
  if (xs.isEmpty) xs
  else if (p(xs.head)) xs.head :: filter(xs.tail, p)
  else filter(xs.tail, p)

def modN(n: Int)(x: Int) = (x % n) == 0


val nums = List(1, 2, 3, 4, 5, 6, 7, 8)

val ord = List("one", "two", "three", "four","five", "six", "seven", "eight")

println(filter(nums, modN(2)))
println(filter(nums, modN(3)))



val a: (Int) => Boolean = modN(3)


val b = a(2)


val conn = ("127.0.0.1", 8080)
val nConn = "localhost" -> 8081

val (server, host) = nConn

println(server)
println(host)


nums zip ord drop 5

def spone = (x: Int) => x + 1
nums reduceLeft(_ + spone(_))
nums reduceLeft(_ + _)
nums sum


sealed trait Elements
case class FireElement (power: Int, distance:Int, weight: Int) extends Elements
case class SandElement (power: Int, distance:Int) extends Elements
case class WaterElement (power: Int, distance:Int, volume: Int) extends Elements

val elements = Seq (
  FireElement(1,10,5) ,
  SandElement(10, 5) ,
  SandElement(15, 3),
  SandElement(30, 4),
  WaterElement(55, 10, 100),
  WaterElement(40,11,101),
  FireElement(10,1,2),
  WaterElement(1,2,3)
)


//
//def matchElement (element: Elements): (Int, Int, Int) = elements match {
//  case FireElement(f1, f2, f3) => (f1 - 1, f2 - 2, f3 - 3)
//  case WaterElement(w1, w2, w3) => (w1 + 5, w2 + 10, w3 + 6)
//  case s: SandElement => (s.distance, s.power, 0)
//  case _ => (0,0,0)
//}


val ka: (Int, Int, Int) = (1,2,3)
//ka ++ (1,2,3)
//ka.


//val ok = elements foldLeft Seq[Map[Int, Int]] {
//
//}


def tCount (list: List[Any]): Int = list.foldLeft(0) ((sum,_) => sum + 1)



  //val gen = elements flatMap (_)

//val x = Array(1, 2, 3)
val x = List(1, 2, 3, 4, 5, 6, 7, 8)
x.foldLeft(20)(_ + _)


nums reduce(_ + _)

nums.min
nums.max

nums.sum

(1 to 4).map { i => "Happy Birthday " + (if (i == 3) "dear NAME" else "to You") }.foreach { println }

val tc: Int = tCount(x)


val xy = List(1.2, 2.3, 3.3, 4.6, 5.9, 6.4, 7.5, 8.0)

def mAverage(list: List[Double]): Double =
  list.foldLeft(0.0)(_+_) / list.foldLeft(0.0)((r,c) => r+1)



def nAverage(list: List[Double]): Double = list match {
  case head :: tail => tail.foldLeft( (head,1.0) )((r,c) =>
    ((r._1 + (c/r._2)) * r._2 / (r._2+1), r._2+1) )._1
  case Nil => Double.NaN
}

val mmm = 2


mmm match {
  case xl => print ("aa =" + xl)
  case 2 => print ("bb =" + 2)
}

mAverage(xy)
nAverage(xy)



x match {
  case  head :: tail  => println(s"tail = $tail | head = $head")
  case Nil => print("Nil")
}


val sList = (1 to 20).toList

val content = sList.foldLeft(Map.empty[Int, String]) {
  (cont, value) => cont + (value -> value.toString)
}

content.foldLeft(Seq.empty[Int])((cont, value) => cont :+ value._1).partition(_ % 2 == 0)._1.sorted.foreach(println)

def parsed (i: String) = i.toInt


//def mathProd (target:Int) = {
//  val range = (1 to 10).toList
//  val values = range scanLeft (0) ((ddd, ccc) => ddd * ccc)
//}

val target = 5
val range = (1 to 10).toList
range.scan(0)((xf, kf) => kf * target)


range map (a => target)

range.contains()


Seq(2,4,6,5,7,8,9,11,12).span(x => x % 2 == 0)



def impSum (elSum: Seq[Int]) = (0  /: elSum) (_+_)




val sEl = Seq("soap", "towel", "water", "sink")

val upSel = (Seq[String]() /: sEl) ((c,v) => v.toUpperCase +: c)

val nullable: Option[Null] = Option(null)

// call by name

val aceptable = (v: Double) => if (v > 50) true else false

val param = aceptable(2.0)


def aceptablev (name: String, vl:Double, p: (Double) => Boolean) : (String, Boolean) =
{
  (name.toUpperCase, p(vl))
}


aceptablev ("DC200", 60, aceptable)




val transformer = Map ("a" -> 1, "b" -> 2, "c"-> 3)

transformer.transform((key,value) => key + value)

transformer.mapValues( c => c * 4)


//sorting !

List(1,2,3).sortBy(-_)

List("a"->1,"b"->2, "c"->3).sortBy(-_._2)

List(1,2,3).sortBy(x => -x)




/**
  * Checks whether the parenthesis are balanced or not.
  *
  * Time - O(n)
  * Space - O(n)
  */

def validateParenthesis(s: String): Boolean = {
  def left(i: Int, k: Int): Boolean =
    if (i == s.length) k == 0
    else if (s.charAt(i) == '(') right(i + 1, k + 1)
    else false

  def right(i: Int, k: Int): Boolean =
    if (i == s.length) false
    else if (s.charAt(i) == '(') right(i + 1, k + 1)
    else if (k == 1) left(i + 1, k - 1)
    else right(i + 1, k - 1)

  left(0, 0)
}


validateParenthesis("(()()()())")
validateParenthesis("(((())))")
validateParenthesis("(()((())()))")

validateParenthesis("((((((())")
validateParenthesis("()))")
validateParenthesis("(()()(()")




def intToRoman(x: Int): String = {
  val digits = List(1000 ->  "M", 900 -> "CM", 500 ->  "D",
    400 -> "CD", 100 ->  "C",  90 -> "XC",
    50 ->  "L",  40 -> "XL",  10 ->  "X",
    9 -> "IX",   5 ->  "V",   4 -> "IV",
    1 ->  "I")

  def loop(l: List[(Int, String)], y: Int): String =
    if (y == 0) ""
    else l.head match {
      case (v, s) if v <= y => s + loop(l, y - v)
      case _ => loop(l.tail, y)
    }

  loop(digits, x)
}



intToRoman(3999)




//def binarysearch[A <% Ordered[A]](list: List[A], key: A): Option[A] = {
def binarysearch[A](list: List[A], key: A)(implicit ev$1: A => Ordered[A]): Option[A] = {
  def search(l: List[A], r: List[A]): Option[A] =
    if (l == r) None
    else test(l, r, middle(l, r))

  def test(l: List[A], r: List[A], m: List[A]): Option[A] =
    if (key < m.head) search(l, m)
    else if (key > m.head) search(m.tail, r)
    else Some(m.head)

  def middle(l: List[A], r: List[A]): List[A] = {
    def race(t: List[A], h: List[A]): List[A] =
      if (h != r && h.tail != r)
        race(t.tail, h.tail.tail)
      else t

    race(l, l.tail)
  }

  search(list, Nil)
}


val searchableBS: List[Int] = List (100, 20,1,4,2,3,55,7,8,11,24,1000,40,31,27,25).sorted
binarysearch(searchableBS, 7)




val wordFrequencies = ("habitual", 6) :: ("and", 56) :: ("consuetudinary", 2) ::
  ("additionally", 27) :: ("homely", 5) :: ("society", 13) :: Nil





def wordsWithoutOutliers(wordFrequencies: Seq[(String, Int)]): Seq[String] =
  wordFrequencies.collect { case (word, freq) if freq > 3 && freq < 25 => word }

def wordsWithoutOutliersV1(wordFrequencies: Seq[(String, Int)]): Seq[String] =
  wordFrequencies.filter(wf => wf._2 > 3 && wf._2 < 25).map(_._1)

def wordsWithoutOutliersV2(wordFrequencies: Seq[(String, Int)]): Seq[String] =
  wordFrequencies.filter { case (_, f) => f > 3 && f < 25 } map { case (w, _) => w }



wordsWithoutOutliers(wordFrequencies)


//Partial Functions for pattern matching... (check above)
val pf: PartialFunction[(String, Int), String] = {
  case (word, freq) if freq > 3 && freq < 25 => word
}
val pfull = new PartialFunction[(String, Int), String] {
  def apply(wordFrequency: (String, Int)) = wordFrequency match {
    case (word, freq) if freq > 3 && freq < 25 => word
  }
  def isDefinedAt(wordFrequency: (String, Int)): Boolean = wordFrequency match {
    case (word, freq) if freq > 3 && freq < 25 => true
    case _ => false
  }
}




val xs = 3 :: 6 :: 12 :: 24 :: Nil
xs match {
  case List(_, b, c, _*) => b * c
  case _ => 0
}
  
  xs.drop(1)
  
//  xs.forall

//def unapplySeq(object: S): Option[(T1, .., Tn-1, Seq[T])]


object Names {
  def unapplySeq(name: String): Option[(String, String, Seq[String])] = {
    val names = name.trim.split(" ")
    if (names.size < 2) None
    else Some((names.last, names.head, names.drop(1).dropRight(1)))
  }
}

def greet(fullName: String) = fullName match {
  case Names(lastName, firstName, _*) => "Good morning, " + firstName + " " + lastName + "!"
  case _ => "Welcome! Please make sure to fill in your name!"
}


greet("Reginaldo Lopes Soares")


//def scores: List[Int] = List()
def scores: List[Int] = List(5,10,50,60).sortBy(-_)
val best :: rest = scores
//val (best, _*) = scores

println("The score of our champion is " + best)


def gameResults(): Seq[(String, Int)] =
  ("Daniel", 3500) :: ("Melissa", 13000) :: ("John", 7000) :: Nil

def hallOfFame = for {
  (name, score) <- gameResults()
  if score > 5000
} yield name

hallOfFame


val lists = List(1, 2, 3) :: List.empty :: List(5, 3) :: Nil

for {
  list @ head :: _ <- lists
} yield list.size


val ela = List.empty

lists.size



case class User(
                 id: Int,
                 firstName: String,
                 lastName: String,
                 age: Int,
                 gender: Option[String])

object UserRepository {
  private val users = Map(1 -> User(1, "John", "Doe", 32, Some("male")),
    2 -> User(2, "Johanna", "Doe", 30, None))
  def findById(id: Int): Option[User] = users.get(id)
  def findAll = users.values
}



val names: List[Option[String]] = List(Some("Johanna"), None, Some("Daniel"))
names.map(_.map(_.toUpperCase))
names.flatMap(xs => xs.map(_.toUpperCase))


UserRepository.findById(1).filter(_.age > 30) // Some(user), because age is > 30
UserRepository.findById(2).filter(_.age > 30) // None, because age is <= 30
UserRepository.findById(3).filter(_.age > 30) // None, because user is already None

for {
  user <- UserRepository.findById(1)
  gender <- user.gender
} yield gender // results in Some("male")

for {
  User(_, _, _, _, Some(gender)) <- UserRepository.findAll
} yield gender





import scala.util.Try
import java.net.URL
def parseURL(url: String): Try[URL] = Try(new URL(url))

val url = parseURL(Console.readLine("URL: ")) getOrElse new URL("http://duckduckgo.com")


parseURL("http://danielwestheide.com").map(_.getProtocol)
// results in Success("http")
parseURL("garbage").map(_.getProtocol)


def parseHttpURL(url: String) = parseURL(url).filter(_.getProtocol == "http")
parseHttpURL("http://apache.openmirror.de") // results in a Success[URL]
parseHttpURL("ftp://mirror.netcologne.de/apache.org") // results in a Failure[URL]


import scala.io.Source
def getURLContent(url: String): Try[Iterator[String]] =
  for {
    url <- parseURL(url)
    connection <- Try(url.openConnection())
    is <- Try(connection.getInputStream)
    source = Source.fromInputStream(is)
  } yield source.getLines()

getURLContent("notworking")


import scala.util.Success
import scala.util.Failure
getURLContent("http://danielwestheide.com/foobar") match {
  case Success(lines) => lines.foreach(println)
  case Failure(ex) => println(s"Problem rendering URL content: ${ex.getMessage}")
}




import scala.io.Source
import java.net.URL
def getContent(url: URL): Either[String, Source] =
  if (url.getHost.contains("google"))
    Left("Requested URL is blocked for the good of the people!")
  else
    Right(Source.fromURL(url))

//getContent(new URL("http://google.com")) match {
//  case Left(msg) => println(msg)
//  case Right(source) => source.getLines.foreach(println)
//}

getContent(new URL("https://plus.google.com"))



