import scala.math._

object HyperElliptic{
  
  def sqrtPoly(coeffs : Array[Double]): Option[Array[Double]] = {
      val k = sqrt(coeffs(0))
      var result = Array.fill(coeffs.length)(0.0)
      result(0) = k
      if (coeffs.length % 2 == 0){
          throw new IllegalArgumentException("The polynomial must have even degree")
      }
      val two_n_plus_one = coeffs.length
      val n = (two_n_plus_one - 1)/2
      for (i <- 1 to coeffs.length-1)
      {
          result(i) = (coeffs(i) - List.range(1, i).map(m => result(m)*result(i-m)).sum)/(2*k)
      }
      val isPoly = List.range(n+1,two_n_plus_one).forall(z => result(z)==0)
      val result2 = if (isPoly) Some(result.reverse.dropWhile(_ == 0).reverse) else None
      result2
  }

  def specialFormat[T](coeffs: Array[T]) : String = {
      val coeffsStrs = coeffs.map(x => x.toString).zipWithIndex.map({case (x,i) => f"($x%s*x^$i%d)"})
      val coeffsStr = coeffsStrs.mkString("+")
      coeffsStr
  }

  def main(args: Array[String]): Unit = {
      val inputPoly = args.map(x => x.toInt)
      println(specialFormat(inputPoly))
      val x = sqrtPoly(inputPoly.map(x => x.toDouble))
      x match{
          case None => println("Not a polynomial")
          case Some(z) => println(specialFormat(z))
      }
  }
}