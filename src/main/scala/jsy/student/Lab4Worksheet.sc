/*
 * CSCI 3155: Lab 4 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab4.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab4.Parser.parse

// Imports the ast nodes
import jsy.lab4.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab4._

// Try compressRec
//val cr1 = compressRec(List(1, 2, 2, 3, 3, 3))

// Parse functions with possibly multiple parameters and type annotations.
parse("function fst(x: number, y: number): number { return x }")
parse("function (x: number) { return x }")
parse("function (f: (y: number) => number, x: number) { return f(x) }")

// Parse objects
parse("{ f: 0, g: true }")
parse("x.f")

// Mapping ============================================================

// Some fun functions for map!

def timesTwo(num: Int): Int = num * 2
def appendYes(str: String): String = str + "Yes"

// Multiply every value in a list by some factor
def scaleList(list: List[Double], factor: Double): List[Double] = list match {
  case Nil => list
  case head::tail => head*factor::scaleList(tail,factor)
}

// Apply some function to every element in a list
def ourMap[A](f: A=>A)(l: List[A]): List[A] = l match {
  case Nil => Nil
  case head::tail => f(head)::ourMap(f)(tail)
}

var ours = ourMap(timesTwo)(List(1,2,3,4))
val labrary_version = List(1,2,3,4) map timesTwo

// Folding =============================================================

// Some fun functions for fold
def times(num1: Int,num2: Int):Int = {num1*num2}
def concat(num1: Int,str1: String):String = str1 + num1

// Multiply everything in a list together and return sum
def productList(list: List[Double]): Double = list match {
  case Nil => 1
  case head::tail => head * productList(tail)
}

// Tail Recursive Product
def productListTail(list: List[Double], acc: Double): Double = list match {
  case Nil => acc
  case head::tail => productListTail(tail,head*acc)
}

// Accumulate using provided function
def ourFoldLeft[A,B](list: List[A], acc:B)(f:(A,B)=>B):B = list match {
  case Nil = acc
  case head::tail => ourFoldLeft(tail,f(acc,head))(f)
}

// Lets try foldLeft(and probably foldRight too)!

def productOurs(list: List[Int]) = ourFoldLeft(list,1)(times)

def productLibrary(list: List[Int])= (list foldLeft 1)(times)

productOurs(List(1,2,3,4))
productLibrary(List(1,2,3,4))

