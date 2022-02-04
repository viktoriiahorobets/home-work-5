package me.chuwy.otusbats


trait Show[A] {
  def show(a: A): String
}

object Show {

  // 1.1 Instances

  implicit val stringShow: Show[String] =
    new Show[String] {
      def show(a: String): String = a
    }

  implicit val intShow: Show[Int] = fromJvm


  implicit val boolShow: Show[Boolean] = fromJvm

  implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] =
    new Show[List[A]] {
      override def show(a: List[A]): String = a.foldLeft("")((accum, elem) => accum + ev.show(elem))
    }

  implicit def setShow[A](implicit ev: Show[A]): Show[Set[A]] =
    new Show[Set[A]] {
      override def show(a: Set[A]): String = a.foldLeft("")((accum, elem) => accum + ev.show(elem))
    }


  // 1.2 Instances with conditional implicit

  implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] =
    new Show[List[A]] {
      def show(as: List[A]): String = "[" ++ as.map(a => ev.show(a)).mkString(",") ++ "]"
    }


  // 2. Summoner

  def apply[A](implicit ev: Show[A]): Show[A] = ev

  // 3. Syntax extensions

  implicit class ShowOps[A](a: A) {
    def show(implicit ev: Show[A]): String =
      ev.show(a)
  }

  // 4. Helper constructors
  /** Just use JVM `toString` implementation, available on every object */
  def fromJvm[A]: Show[A] = new Show[A]{
    def show(a: A): String = a.toString
  }

  /** Provide a custom function to avoid `new Show { ... }` machinery */
  def fromFunction[A](f: A => String): Show[A] = new Show[A]{
    def show(a: A): String = f(a)
  }
}