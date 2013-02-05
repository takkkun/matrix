trait Matrix[M <: Matrix[M]] {
  def *(value: Double): M
}

trait Rows1 {
}

trait Rows2 {
}

trait Rows3 {
}

trait Rows4 {
}

trait Columns1 {
  def *(other: Matrix1x1): Columns1
  def *(other: Matrix1x2): Columns2
  def *(other: Matrix1x3): Columns3
  def *(other: Matrix1x4): Columns4
}

trait Columns2 {
  def *(other: Matrix2x1): Columns1
  def *(other: Matrix2x2): Columns2
  def *(other: Matrix2x3): Columns3
  def *(other: Matrix2x4): Columns4
}

trait Columns3 {
  def *(other: Matrix3x1): Columns1
  def *(other: Matrix3x2): Columns2
  def *(other: Matrix3x3): Columns3
  def *(other: Matrix3x4): Columns4
}

trait Columns4 {
  def *(other: Matrix4x1): Columns1
  def *(other: Matrix4x2): Columns2
  def *(other: Matrix4x3): Columns3
  def *(other: Matrix4x4): Columns4
}

trait SquareMatrix[M <: SquareMatrix[M]] {
  def inverse(): Option[M]
}

case class Matrix1x1(
  e1x1: Double
) extends Matrix[Matrix1x1] with Rows1 with Columns1 with SquareMatrix[Matrix1x1] {
  def inverse(): Option[Matrix1x1] = None

  def *(value: Double) =
    Matrix1x1(
      value * e1x1
    )

  def *(other: Matrix1x1) =
    Matrix1x1(
      e1x1 * other.e1x1
    )

  def *(other: Matrix1x2) =
    Matrix1x2(
      e1x1 * other.e1x1, e1x1 * other.e1x2
    )

  def *(other: Matrix1x3) =
    Matrix1x3(
      e1x1 * other.e1x1, e1x1 * other.e1x2, e1x1 * other.e1x3
    )

  def *(other: Matrix1x4) =
    Matrix1x4(
      e1x1 * other.e1x1, e1x1 * other.e1x2, e1x1 * other.e1x3, e1x1 * other.e1x4
    )
}

case class Matrix1x2(
  e1x1: Double, e1x2: Double
) extends Matrix[Matrix1x2] with Rows1 with Columns2 {
  def *(value: Double) =
    Matrix1x2(
      value * e1x1, value * e1x2
    )

  def *(other: Matrix2x1) =
    Matrix1x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1
    )

  def *(other: Matrix2x2) =
    Matrix1x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2
    )

  def *(other: Matrix2x3) =
    Matrix1x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2, e1x1 * other.e1x3 + e1x2 * other.e2x3
    )

  def *(other: Matrix2x4) =
    Matrix1x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2, e1x1 * other.e1x3 + e1x2 * other.e2x3, e1x1 * other.e1x4 + e1x2 * other.e2x4
    )
}

case class Matrix1x3(
  e1x1: Double, e1x2: Double, e1x3: Double
) extends Matrix[Matrix1x3] with Rows1 with Columns3 {
  def *(value: Double) =
    Matrix1x3(
      value * e1x1, value * e1x2, value * e1x3
    )

  def *(other: Matrix3x1) =
    Matrix1x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1
    )

  def *(other: Matrix3x2) =
    Matrix1x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2
    )

  def *(other: Matrix3x3) =
    Matrix1x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3
    )

  def *(other: Matrix3x4) =
    Matrix1x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3, e1x1 * other.e1x4 + e1x2 * other.e2x4 + e1x3 * other.e3x4
    )
}

case class Matrix1x4(
  e1x1: Double, e1x2: Double, e1x3: Double, e1x4: Double
) extends Matrix[Matrix1x4] with Rows1 with Columns4 {
  def *(value: Double) =
    Matrix1x4(
      value * e1x1, value * e1x2, value * e1x3, value * e1x4
    )

  def *(other: Matrix4x1) =
    Matrix1x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1
    )

  def *(other: Matrix4x2) =
    Matrix1x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2
    )

  def *(other: Matrix4x3) =
    Matrix1x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3 + e1x4 * other.e4x3
    )

  def *(other: Matrix4x4) =
    Matrix1x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3 + e1x4 * other.e4x3, e1x1 * other.e1x4 + e1x2 * other.e2x4 + e1x3 * other.e3x4 + e1x4 * other.e4x4
    )
}

case class Matrix2x1(
  e1x1: Double,
  e2x1: Double
) extends Matrix[Matrix2x1] with Rows2 with Columns1 {
  def *(value: Double) =
    Matrix2x1(
      value * e1x1,
      value * e2x1
    )

  def *(other: Matrix1x1) =
    Matrix2x1(
      e1x1 * other.e1x1,
      e2x1 * other.e1x1
    )

  def *(other: Matrix1x2) =
    Matrix2x2(
      e1x1 * other.e1x1, e1x1 * other.e1x2,
      e2x1 * other.e1x1, e2x1 * other.e1x2
    )

  def *(other: Matrix1x3) =
    Matrix2x3(
      e1x1 * other.e1x1, e1x1 * other.e1x2, e1x1 * other.e1x3,
      e2x1 * other.e1x1, e2x1 * other.e1x2, e2x1 * other.e1x3
    )

  def *(other: Matrix1x4) =
    Matrix2x4(
      e1x1 * other.e1x1, e1x1 * other.e1x2, e1x1 * other.e1x3, e1x1 * other.e1x4,
      e2x1 * other.e1x1, e2x1 * other.e1x2, e2x1 * other.e1x3, e2x1 * other.e1x4
    )
}

case class Matrix2x2(
  e1x1: Double, e1x2: Double,
  e2x1: Double, e2x2: Double
) extends Matrix[Matrix2x2] with Rows2 with Columns2 with SquareMatrix[Matrix2x2] {
  def inverse(): Option[Matrix2x2] = None

  def *(value: Double) =
    Matrix2x2(
      value * e1x1, value * e1x2,
      value * e2x1, value * e2x2
    )

  def *(other: Matrix2x1) =
    Matrix2x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1,
      e2x1 * other.e1x1 + e2x2 * other.e2x1
    )

  def *(other: Matrix2x2) =
    Matrix2x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2,
      e2x1 * other.e1x1 + e2x2 * other.e2x1, e2x1 * other.e1x2 + e2x2 * other.e2x2
    )

  def *(other: Matrix2x3) =
    Matrix2x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2, e1x1 * other.e1x3 + e1x2 * other.e2x3,
      e2x1 * other.e1x1 + e2x2 * other.e2x1, e2x1 * other.e1x2 + e2x2 * other.e2x2, e2x1 * other.e1x3 + e2x2 * other.e2x3
    )

  def *(other: Matrix2x4) =
    Matrix2x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2, e1x1 * other.e1x3 + e1x2 * other.e2x3, e1x1 * other.e1x4 + e1x2 * other.e2x4,
      e2x1 * other.e1x1 + e2x2 * other.e2x1, e2x1 * other.e1x2 + e2x2 * other.e2x2, e2x1 * other.e1x3 + e2x2 * other.e2x3, e2x1 * other.e1x4 + e2x2 * other.e2x4
    )
}

case class Matrix2x3(
  e1x1: Double, e1x2: Double, e1x3: Double,
  e2x1: Double, e2x2: Double, e2x3: Double
) extends Matrix[Matrix2x3] with Rows2 with Columns3 {
  def *(value: Double) =
    Matrix2x3(
      value * e1x1, value * e1x2, value * e1x3,
      value * e2x1, value * e2x2, value * e2x3
    )

  def *(other: Matrix3x1) =
    Matrix2x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1
    )

  def *(other: Matrix3x2) =
    Matrix2x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2
    )

  def *(other: Matrix3x3) =
    Matrix2x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2, e1x1 * other.e1x3 + e1x3 * other.e2x3 + e1x3 * other.e3x3,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2, e2x1 * other.e1x3 + e2x3 * other.e2x3 + e2x3 * other.e3x3
    )

  def *(other: Matrix3x4) =
    Matrix2x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2, e1x1 * other.e1x3 + e1x3 * other.e2x3 + e1x3 * other.e3x3, e1x1 * other.e1x4 + e1x3 * other.e2x4 + e1x3 * other.e3x4,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2, e2x1 * other.e1x3 + e2x3 * other.e2x3 + e2x3 * other.e3x3, e2x1 * other.e1x4 + e2x3 * other.e2x4 + e2x3 * other.e3x4
    )
}

case class Matrix2x4(
  e1x1: Double, e1x2: Double, e1x3: Double, e1x4: Double,
  e2x1: Double, e2x2: Double, e2x3: Double, e2x4: Double
) extends Matrix[Matrix2x4] with Rows2 with Columns4 {
  def *(value: Double) =
    Matrix2x4(
      value * e1x1, value * e1x2, value * e1x3, value * e1x4,
      value * e2x1, value * e2x2, value * e2x3, value * e2x4
    )

  def *(other: Matrix4x1) =
    Matrix2x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1
    )

  def *(other: Matrix4x2) =
    Matrix2x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2 + e2x4 * other.e4x2
    )

  def *(other: Matrix4x3) =
    Matrix2x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3 + e1x4 * other.e4x3,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2 + e2x4 * other.e4x2, e2x1 * other.e1x3 + e2x2 * other.e2x3 + e2x3 * other.e3x3 + e2x4 * other.e4x3
    )

  def *(other: Matrix4x4) =
    Matrix2x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3 + e1x4 * other.e4x3, e1x1 * other.e1x4 + e1x2 * other.e2x4 + e1x3 * other.e3x4 + e1x4 * other.e4x4,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2 + e2x4 * other.e4x2, e2x1 * other.e1x3 + e2x2 * other.e2x3 + e2x3 * other.e3x3 + e2x4 * other.e4x3, e2x1 * other.e1x4 + e2x2 * other.e2x4 + e2x3 * other.e3x4 + e2x4 * other.e4x4
    )
}

case class Matrix3x1(
  e1x1: Double,
  e2x1: Double,
  e3x1: Double
) extends Matrix[Matrix3x1] with Rows3 with Columns1 {
  def *(value: Double) =
    Matrix3x1(
      value * e1x1,
      value * e2x1,
      value * e3x1
    )

  def *(other: Matrix1x1) =
    Matrix3x1(
      e1x1 * other.e1x1,
      e2x1 * other.e1x1,
      e3x1 * other.e1x1
    )

  def *(other: Matrix1x2) =
    Matrix3x2(
      e1x1 * other.e1x1, e1x1 * other.e1x2,
      e2x1 * other.e1x1, e2x1 * other.e1x2,
      e3x1 * other.e1x1, e3x1 * other.e1x2
    )

  def *(other: Matrix1x3) =
    Matrix3x3(
      e1x1 * other.e1x1, e1x1 * other.e1x2, e1x1 * other.e1x3,
      e2x1 * other.e1x1, e2x1 * other.e1x2, e2x1 * other.e1x3,
      e3x1 * other.e1x1, e3x1 * other.e1x2, e3x1 * other.e1x3
    )

  def *(other: Matrix1x4) =
    Matrix3x4(
      e1x1 * other.e1x1, e1x1 * other.e1x2, e1x1 * other.e1x3, e1x1 * other.e1x4,
      e2x1 * other.e1x1, e2x1 * other.e1x2, e2x1 * other.e1x3, e2x1 * other.e1x4,
      e3x1 * other.e1x1, e3x1 * other.e1x2, e3x1 * other.e1x3, e3x1 * other.e1x4
    )
}

case class Matrix3x2(
  e1x1: Double, e1x2: Double,
  e2x1: Double, e2x2: Double,
  e3x1: Double, e3x2: Double
) extends Matrix[Matrix3x2] with Rows3 with Columns2 {
  def *(value: Double) =
    Matrix3x2(
      value * e1x1, value * e1x2,
      value * e2x1, value * e2x2,
      value * e3x1, value * e3x2
    )

  def *(other: Matrix2x1) =
    Matrix3x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1,
      e2x1 * other.e1x1 + e2x2 * other.e2x1,
      e3x1 * other.e1x1 + e3x2 * other.e2x1
    )

  def *(other: Matrix2x2) =
    Matrix3x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2,
      e2x1 * other.e1x1 + e2x2 * other.e2x1, e2x1 * other.e1x2 + e2x2 * other.e2x2,
      e3x1 * other.e1x1 + e3x2 * other.e2x1, e3x1 * other.e1x2 + e3x2 * other.e2x2
    )

  def *(other: Matrix2x3) =
    Matrix3x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2, e1x1 * other.e1x3 + e1x2 * other.e2x3,
      e2x1 * other.e1x1 + e2x2 * other.e2x1, e2x1 * other.e1x2 + e2x2 * other.e2x2, e2x1 * other.e1x3 + e2x2 * other.e2x3,
      e3x1 * other.e1x1 + e3x2 * other.e2x1, e3x1 * other.e1x2 + e3x2 * other.e2x2, e3x1 * other.e1x3 + e3x2 * other.e2x3
    )

  def *(other: Matrix2x4) =
    Matrix3x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2, e1x1 * other.e1x3 + e1x2 * other.e2x3, e1x1 * other.e1x4 + e1x2 * other.e2x4,
      e2x1 * other.e1x1 + e2x2 * other.e2x1, e2x1 * other.e1x2 + e2x2 * other.e2x2, e2x1 * other.e1x3 + e2x2 * other.e2x3, e2x1 * other.e1x4 + e2x2 * other.e2x4,
      e3x1 * other.e1x1 + e3x2 * other.e2x1, e3x1 * other.e1x2 + e3x2 * other.e2x2, e3x1 * other.e1x3 + e3x2 * other.e2x3, e3x1 * other.e1x4 + e3x2 * other.e2x4
    )
}

case class Matrix3x3(
  e1x1: Double, e1x2: Double, e1x3: Double,
  e2x1: Double, e2x2: Double, e2x3: Double,
  e3x1: Double, e3x2: Double, e3x3: Double
) extends Matrix[Matrix3x3] with Rows3 with Columns3 with SquareMatrix[Matrix3x3] {
  def inverse(): Option[Matrix3x3] = None

  def *(value: Double) =
    Matrix3x3(
      value * e1x1, value * e1x2, value * e1x3,
      value * e2x1, value * e2x2, value * e2x3,
      value * e3x1, value * e3x2, value * e3x3
    )

  def *(other: Matrix3x1) =
    Matrix3x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1
    )

  def *(other: Matrix3x2) =
    Matrix3x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2
    )

  def *(other: Matrix3x3) =
    Matrix3x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2, e1x1 * other.e1x3 + e1x3 * other.e2x3 + e1x3 * other.e3x3,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2, e2x1 * other.e1x3 + e2x3 * other.e2x3 + e2x3 * other.e3x3,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2, e3x1 * other.e1x3 + e3x3 * other.e2x3 + e3x3 * other.e3x3
    )

  def *(other: Matrix3x4) =
    Matrix3x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2, e1x1 * other.e1x3 + e1x3 * other.e2x3 + e1x3 * other.e3x3, e1x1 * other.e1x4 + e1x3 * other.e2x4 + e1x3 * other.e3x4,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2, e2x1 * other.e1x3 + e2x3 * other.e2x3 + e2x3 * other.e3x3, e2x1 * other.e1x4 + e2x3 * other.e2x4 + e2x3 * other.e3x4,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2, e3x1 * other.e1x3 + e3x3 * other.e2x3 + e3x3 * other.e3x3, e3x1 * other.e1x4 + e3x3 * other.e2x4 + e3x3 * other.e3x4
    )
}

case class Matrix3x4(
  e1x1: Double, e1x2: Double, e1x3: Double, e1x4: Double,
  e2x1: Double, e2x2: Double, e2x3: Double, e2x4: Double,
  e3x1: Double, e3x2: Double, e3x3: Double, e3x4: Double
) extends Matrix[Matrix3x4] with Rows3 with Columns4 {
  def *(value: Double) =
    Matrix3x4(
      value * e1x1, value * e1x2, value * e1x3, value * e1x4,
      value * e2x1, value * e2x2, value * e2x3, value * e2x4,
      value * e3x1, value * e3x2, value * e3x3, value * e3x4
    )

  def *(other: Matrix4x1) =
    Matrix3x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1 + e3x4 * other.e4x1
    )

  def *(other: Matrix4x2) =
    Matrix3x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2 + e2x4 * other.e4x2,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1 + e3x4 * other.e4x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2 + e3x4 * other.e4x2
    )

  def *(other: Matrix4x3) =
    Matrix3x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3 + e1x4 * other.e4x3,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2 + e2x4 * other.e4x2, e2x1 * other.e1x3 + e2x2 * other.e2x3 + e2x3 * other.e3x3 + e2x4 * other.e4x3,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1 + e3x4 * other.e4x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2 + e3x4 * other.e4x2, e3x1 * other.e1x3 + e3x2 * other.e2x3 + e3x3 * other.e3x3 + e3x4 * other.e4x3
    )

  def *(other: Matrix4x4) =
    Matrix3x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3 + e1x4 * other.e4x3, e1x1 * other.e1x4 + e1x2 * other.e2x4 + e1x3 * other.e3x4 + e1x4 * other.e4x4,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2 + e2x4 * other.e4x2, e2x1 * other.e1x3 + e2x2 * other.e2x3 + e2x3 * other.e3x3 + e2x4 * other.e4x3, e2x1 * other.e1x4 + e2x2 * other.e2x4 + e2x3 * other.e3x4 + e2x4 * other.e4x4,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1 + e3x4 * other.e4x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2 + e3x4 * other.e4x2, e3x1 * other.e1x3 + e3x2 * other.e2x3 + e3x3 * other.e3x3 + e3x4 * other.e4x3, e3x1 * other.e1x4 + e3x2 * other.e2x4 + e3x3 * other.e3x4 + e3x4 * other.e4x4
    )
}

case class Matrix4x1(
  e1x1: Double,
  e2x1: Double,
  e3x1: Double,
  e4x1: Double
) extends Matrix[Matrix4x1] with Rows4 with Columns1 {
  def *(value: Double) =
    Matrix4x1(
      value * e1x1,
      value * e2x1,
      value * e3x1,
      value * e4x1
    )

  def *(other: Matrix1x1) =
    Matrix4x1(
      e1x1 * other.e1x1,
      e2x1 * other.e1x1,
      e3x1 * other.e1x1,
      e4x1 * other.e1x1
    )

  def *(other: Matrix1x2) =
    Matrix4x2(
      e1x1 * other.e1x1, e1x1 * other.e1x2,
      e2x1 * other.e1x1, e2x1 * other.e1x2,
      e3x1 * other.e1x1, e3x1 * other.e1x2,
      e4x1 * other.e1x1, e4x1 * other.e1x2
    )

  def *(other: Matrix1x3) =
    Matrix4x3(
      e1x1 * other.e1x1, e1x1 * other.e1x2, e1x1 * other.e1x3,
      e2x1 * other.e1x1, e2x1 * other.e1x2, e2x1 * other.e1x3,
      e3x1 * other.e1x1, e3x1 * other.e1x2, e3x1 * other.e1x3,
      e4x1 * other.e1x1, e4x1 * other.e1x2, e4x1 * other.e1x3
    )

  def *(other: Matrix1x4) =
    Matrix4x4(
      e1x1 * other.e1x1, e1x1 * other.e1x2, e1x1 * other.e1x3, e1x1 * other.e1x4,
      e2x1 * other.e1x1, e2x1 * other.e1x2, e2x1 * other.e1x3, e2x1 * other.e1x4,
      e3x1 * other.e1x1, e3x1 * other.e1x2, e3x1 * other.e1x3, e3x1 * other.e1x4,
      e4x1 * other.e1x1, e4x1 * other.e1x2, e4x1 * other.e1x3, e4x1 * other.e1x4
    )
}

case class Matrix4x2(
  e1x1: Double, e1x2: Double,
  e2x1: Double, e2x2: Double,
  e3x1: Double, e3x2: Double,
  e4x1: Double, e4x2: Double
) extends Matrix[Matrix4x2] with Rows4 with Columns2 {
  def *(value: Double) =
    Matrix4x2(
      value * e1x1, value * e1x2,
      value * e2x1, value * e2x2,
      value * e3x1, value * e3x2,
      value * e4x1, value * e4x2
    )

  def *(other: Matrix2x1) =
    Matrix4x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1,
      e2x1 * other.e1x1 + e2x2 * other.e2x1,
      e3x1 * other.e1x1 + e3x2 * other.e2x1,
      e4x1 * other.e1x1 + e4x2 * other.e2x1
    )

  def *(other: Matrix2x2) =
    Matrix4x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2,
      e2x1 * other.e1x1 + e2x2 * other.e2x1, e2x1 * other.e1x2 + e2x2 * other.e2x2,
      e3x1 * other.e1x1 + e3x2 * other.e2x1, e3x1 * other.e1x2 + e3x2 * other.e2x2,
      e4x1 * other.e1x1 + e4x2 * other.e2x1, e4x1 * other.e1x2 + e4x2 * other.e2x2
    )

  def *(other: Matrix2x3) =
    Matrix4x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2, e1x1 * other.e1x3 + e1x2 * other.e2x3,
      e2x1 * other.e1x1 + e2x2 * other.e2x1, e2x1 * other.e1x2 + e2x2 * other.e2x2, e2x1 * other.e1x3 + e2x2 * other.e2x3,
      e3x1 * other.e1x1 + e3x2 * other.e2x1, e3x1 * other.e1x2 + e3x2 * other.e2x2, e3x1 * other.e1x3 + e3x2 * other.e2x3,
      e4x1 * other.e1x1 + e4x2 * other.e2x1, e4x1 * other.e1x2 + e4x2 * other.e2x2, e4x1 * other.e1x3 + e4x2 * other.e2x3
    )

  def *(other: Matrix2x4) =
    Matrix4x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1, e1x1 * other.e1x2 + e1x2 * other.e2x2, e1x1 * other.e1x3 + e1x2 * other.e2x3, e1x1 * other.e1x4 + e1x2 * other.e2x4,
      e2x1 * other.e1x1 + e2x2 * other.e2x1, e2x1 * other.e1x2 + e2x2 * other.e2x2, e2x1 * other.e1x3 + e2x2 * other.e2x3, e2x1 * other.e1x4 + e2x2 * other.e2x4,
      e3x1 * other.e1x1 + e3x2 * other.e2x1, e3x1 * other.e1x2 + e3x2 * other.e2x2, e3x1 * other.e1x3 + e3x2 * other.e2x3, e3x1 * other.e1x4 + e3x2 * other.e2x4,
      e4x1 * other.e1x1 + e4x2 * other.e2x1, e4x1 * other.e1x2 + e4x2 * other.e2x2, e4x1 * other.e1x3 + e4x2 * other.e2x3, e4x1 * other.e1x4 + e4x2 * other.e2x4
    )
}

case class Matrix4x3(
  e1x1: Double, e1x2: Double, e1x3: Double,
  e2x1: Double, e2x2: Double, e2x3: Double,
  e3x1: Double, e3x2: Double, e3x3: Double,
  e4x1: Double, e4x2: Double, e4x3: Double
) extends Matrix[Matrix4x3] with Rows4 with Columns3 {
  def *(value: Double) =
    Matrix4x3(
      value * e1x1, value * e1x2, value * e1x3,
      value * e2x1, value * e2x2, value * e2x3,
      value * e3x1, value * e3x2, value * e3x3,
      value * e4x1, value * e4x2, value * e4x3
    )

  def *(other: Matrix3x1) =
    Matrix4x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1,
      e4x1 * other.e1x1 + e4x2 * other.e2x1 + e4x3 * other.e3x1
    )

  def *(other: Matrix3x2) =
    Matrix4x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2,
      e4x1 * other.e1x1 + e4x2 * other.e2x1 + e4x3 * other.e3x1, e4x1 * other.e1x2 + e4x2 * other.e2x2 + e4x3 * other.e3x2
    )

  def *(other: Matrix3x3) =
    Matrix4x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2, e2x1 * other.e1x3 + e2x2 * other.e2x3 + e2x3 * other.e3x3,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2, e3x1 * other.e1x3 + e3x2 * other.e2x3 + e3x3 * other.e3x3,
      e4x1 * other.e1x1 + e4x2 * other.e2x1 + e4x3 * other.e3x1, e4x1 * other.e1x2 + e4x2 * other.e2x2 + e4x3 * other.e3x2, e4x1 * other.e1x3 + e4x2 * other.e2x3 + e4x3 * other.e3x3
    )

  def *(other: Matrix3x4) =
    Matrix4x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3, e1x1 * other.e1x4 + e1x2 * other.e2x4 + e1x3 * other.e3x4,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2, e2x1 * other.e1x3 + e2x2 * other.e2x3 + e2x3 * other.e3x3, e2x1 * other.e1x4 + e2x2 * other.e2x4 + e2x3 * other.e3x4,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2, e3x1 * other.e1x3 + e3x2 * other.e2x3 + e3x3 * other.e3x3, e3x1 * other.e1x4 + e3x2 * other.e2x4 + e3x3 * other.e3x4,
      e4x1 * other.e1x1 + e4x2 * other.e2x1 + e4x3 * other.e3x1, e4x1 * other.e1x2 + e4x2 * other.e2x2 + e4x3 * other.e3x2, e4x1 * other.e1x3 + e4x2 * other.e2x3 + e4x3 * other.e3x3, e4x1 * other.e1x4 + e4x2 * other.e2x4 + e4x3 * other.e3x4
    )
}

case class Matrix4x4(
  e1x1: Double, e1x2: Double, e1x3: Double, e1x4: Double,
  e2x1: Double, e2x2: Double, e2x3: Double, e2x4: Double,
  e3x1: Double, e3x2: Double, e3x3: Double, e3x4: Double,
  e4x1: Double, e4x2: Double, e4x3: Double, e4x4: Double
) extends Matrix[Matrix4x4] with Rows4 with Columns4 with SquareMatrix[Matrix4x4] {
  def inverse(): Option[Matrix4x4] = None

  def *(value: Double) =
    Matrix4x4(
      value * e1x1, value * e1x2, value * e1x3, value * e1x4,
      value * e2x1, value * e2x2, value * e2x3, value * e2x4,
      value * e3x1, value * e3x2, value * e3x3, value * e3x4,
      value * e4x1, value * e4x2, value * e4x3, value * e4x4
    )

  def *(other: Matrix4x1) =
    Matrix4x1(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1 + e3x4 * other.e4x1,
      e4x1 * other.e1x1 + e4x2 * other.e2x1 + e4x3 * other.e3x1 + e4x4 * other.e4x1
    )

  def *(other: Matrix4x2) =
    Matrix4x2(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2 + e2x4 * other.e4x2,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1 + e3x4 * other.e4x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2 + e3x4 * other.e4x2,
      e4x1 * other.e1x1 + e4x2 * other.e2x1 + e4x3 * other.e3x1 + e4x4 * other.e4x1, e4x1 * other.e1x2 + e4x2 * other.e2x2 + e4x3 * other.e3x2 + e4x4 * other.e4x2
    )

  def *(other: Matrix4x3) =
    Matrix4x3(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3 + e1x4 * other.e4x3,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2 + e2x4 * other.e4x2, e2x1 * other.e1x3 + e2x2 * other.e2x3 + e2x3 * other.e3x3 + e2x4 * other.e4x3,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1 + e3x4 * other.e4x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2 + e3x4 * other.e4x2, e3x1 * other.e1x3 + e3x2 * other.e2x3 + e3x3 * other.e3x3 + e3x4 * other.e4x3,
      e4x1 * other.e1x1 + e4x2 * other.e2x1 + e4x3 * other.e3x1 + e4x4 * other.e4x1, e4x1 * other.e1x2 + e4x2 * other.e2x2 + e4x3 * other.e3x2 + e4x4 * other.e4x2, e4x1 * other.e1x3 + e4x2 * other.e2x3 + e4x3 * other.e3x3 + e4x4 * other.e4x3
    )

  def *(other: Matrix4x4) =
    Matrix4x4(
      e1x1 * other.e1x1 + e1x2 * other.e2x1 + e1x3 * other.e3x1 + e1x4 * other.e4x1, e1x1 * other.e1x2 + e1x2 * other.e2x2 + e1x3 * other.e3x2 + e1x4 * other.e4x2, e1x1 * other.e1x3 + e1x2 * other.e2x3 + e1x3 * other.e3x3 + e1x4 * other.e4x3, e1x1 * other.e1x4 + e1x2 * other.e2x4 + e1x3 * other.e3x4 + e1x4 * other.e4x4,
      e2x1 * other.e1x1 + e2x2 * other.e2x1 + e2x3 * other.e3x1 + e2x4 * other.e4x1, e2x1 * other.e1x2 + e2x2 * other.e2x2 + e2x3 * other.e3x2 + e2x4 * other.e4x2, e2x1 * other.e1x3 + e2x2 * other.e2x3 + e2x3 * other.e3x3 + e2x4 * other.e4x3, e2x1 * other.e1x4 + e2x2 * other.e2x4 + e2x3 * other.e3x4 + e2x4 * other.e4x4,
      e3x1 * other.e1x1 + e3x2 * other.e2x1 + e3x3 * other.e3x1 + e3x4 * other.e4x1, e3x1 * other.e1x2 + e3x2 * other.e2x2 + e3x3 * other.e3x2 + e3x4 * other.e4x2, e3x1 * other.e1x3 + e3x2 * other.e2x3 + e3x3 * other.e3x3 + e3x4 * other.e4x3, e3x1 * other.e1x4 + e3x2 * other.e2x4 + e3x3 * other.e3x4 + e3x4 * other.e4x4,
      e4x1 * other.e1x1 + e4x2 * other.e2x1 + e4x3 * other.e3x1 + e4x4 * other.e4x1, e4x1 * other.e1x2 + e4x2 * other.e2x2 + e4x3 * other.e3x2 + e4x4 * other.e4x2, e4x1 * other.e1x3 + e4x2 * other.e2x3 + e4x3 * other.e3x3 + e4x4 * other.e4x3, e4x1 * other.e1x4 + e4x2 * other.e2x4 + e4x3 * other.e3x4 + e4x4 * other.e4x4
    )
}

object Matrix {
  implicit def intToMatrix1D(v: Int) =
    Matrix1x1(
      v
    )

  implicit def intToMatrix2D(v: Int) =
    Matrix2x2(
      v, 0,
      0, v
    )

  implicit def intToMatrix3D(v: Int) =
    Matrix3x3(
      v, 0, 0,
      0, v, 0,
      0, 0, v
    )

  implicit def intToMatrix4D(v: Int) =
    Matrix4x4(
      v, 0, 0, 0,
      0, v, 0, 0,
      0, 0, v, 0,
      0, 0, 0, v
    )

  implicit def doubleToMatrix1D(v: Double) =
    Matrix1x1(
      v
    )

  implicit def doubleToMatrix2D(v: Double) =
    Matrix2x2(
       v, .0,
      .0,  v
    )

  implicit def doubleToMatrix3D(v: Double) =
    Matrix3x3(
       v, .0, .0,
      .0,  v, .0,
      .0, .0,  v
    )

  implicit def doubleToMatrix4D(v: Double) =
    Matrix4x4(
       v, .0, .0, .0,
      .0,  v, .0, .0,
      .0, .0,  v, .0,
      .0, .0, .0,  v
    )
}
