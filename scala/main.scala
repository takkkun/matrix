object Main {
  import Matrix._

  def main(args: Array[String]) {
    val m1 = Matrix4x3(
       1,  2,  3,
       4,  5,  6,
       7,  8,  9,
      10, 11, 12
    )

    val m2 = Matrix3x1(
      2,
      3,
      4
    )

    val m3 = Matrix4x1(
       20,
       47,
       74,
      101
    )

    println(m1 * m2)
    println(m1 * m2 == m3)
    println(10 * m1)
  }
}
