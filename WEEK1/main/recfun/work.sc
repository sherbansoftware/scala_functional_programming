object rationals {
  val x = new Rational(1, 2)
  x.denom
  x.numer
}

class Rational(x: Int, y: Int){
  def numer = x
  def denom = y
}




object persoana{
  def main(args: Array[String]): Unit = {
   val p1: Persoana = new Persoana("Serban", "Mihai", 30)
    println(p1.toString)
  }
}

class Persoana(nume: String, prenume: String, varsta: Int){

  override def toString: String = {

    this.nume + " " + this.prenume + " si am " + this.varsta + " ani!"
  }
}

//Anonymous funciton syntax
val value1 = (x: Int) => x + 1
val value2 = new Function1[Int, Int] {
  def apply(x: Int): Int = x + 1
}

value1(23)
value1.apply(23)

value2(23)
value2.apply(23)

//Higher Order Functions
def sum(x: Int): Int = x //==> sum: sum[](val x: Int) => Int
def sumInts(a: Int, b: Int): Int = if (a > b) 0 else sum(a) + sumInts(a + 1, b) //==> sumInts: sumInts[](val a: Int,val b: Int) => Int

sumInts(0, 5) //res0: Int = 15
sumWithFunction(sum, 0, 5) //res1: Int = 15
sumWithReturnFunction(sum)(1,5) //res2: Int = 15

def cube(x: Int): Int = x * x * x //==> cube: cube[](val x: Int) => Int
def sumCubes(a: Int, b: Int): Int = if (a > b) 0 else cube(a) + sumCubes(a + 1, b) //==> sumCubes: sumCubes[](val a: Int,val b: Int) => Int

sumCubes(0, 5) //res3: Int = 225
sumWithFunction(cube, 0, 5) //res4: Int = 225
sumWithReturnFunction(cube)(0,5) //res5: Int = 225

def factorial(value: Int): Int = if (value == 0) 1 else factorial(value - 1) //==> factorial: factorial[](val value: Int) => Int
def sumFactorials(a: Int, b: Int): Int = if (a > b) 0 else factorial(a) + sumFactorials(a + 1, b) //==> sumFactorials: sumFactorials[](val a: Int,val b: Int) => Int

sumFactorials(0, 5) //res6: Int = 6
sumWithFunction(factorial, 0, 5) //res7: Int = 6
sumWithReturnFunction(factorial)(0,5) //res8: Int = 6

def sumWithFunction(f: Int => Int, a: Int, b: Int): Int = {
  //==> sumWithFunction: sumWithFunction[](val sumInts: Int => Int,val a: Int,val b: Int) => Int
  if (a > b) 0
  else f(a) + sumWithFunction(f, a + 1, b)
}

def sumWithReturnFunction(f: Int => Int): (Int, Int) => Int = {
  def sumF(nr1: Int, nr2: Int): Int = {
    if (nr1 > nr2) 0
    else f(nr1) + sumF(nr1 + 1, nr2)
  }
  sumF //return a local defined function which is Int
}

def prod(x : Int) = x * x
def productFunction(f: Int => Int, nr1: Int, nr2: Int): Int = {
  if(nr1 > nr2) 1
  else
    f(nr1) * productFunction(f, nr1 + 1, nr2)
}

 productFunction(prod, 1, 2)

// (x: Int) => x * x
// //anonimus function expresed as
// def f(x: Int) = x * x

