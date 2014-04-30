package SetPractice

object Main extends App {
  {
    println("\033[1;31mIntFunSet testing\033[m")

    import IntFunSets._
  
    val x = union(singletonSet(1), singletonSet(2))
    printSet(x)
    printSet(map(x, (a:Int)=>a+1))

    println(forall(x, (a:Int)=> a>1))
    
    val y = intersect(singletonSet(1), singletonSet(2))
    exists(y, (a:Int)=> a<3)
  }

  {

    println("\033[1;31mGeneralFunSet testing\033[m")
    import GeneralFunSet._
    val s0 = singletonSet(AInt(0))
    val s1 = singletonSet(AInt(1))
    val s2 = singletonSet(AInt(2))
    val s3 = singletonSet(AInt(3))
    val s4 = singletonSet(AInt(4))
    val s5 = singletonSet(AInt(5))
    val b = BoundedMethods(AInt(0), AInt(10))

    val s01234 = union(s0, union(s1, union(s2, union(s3, s4))))
    val s135 = union(s1, union(s3, s5))

    println(s"""${b.toString(s01234)} \033[1;32mintersect\033[m ${b.toString(s135)}
               #  = ${b.toString(intersect(s01234, s135))}""".stripMargin('#') )
    println(s"""${b.toString(s01234)} \033[1;32mdiff\033[m ${b.toString(s135)}
               #  = ${b.toString(diff(s01234, s135))}""".stripMargin('#') )
    println(s"""${b.toString(s01234)} \033[1;32mmap\033[m _.next
               #  = ${b.toString(b.map(s01234, _.next))}""".stripMargin('#') )

  }

  {
    println("\033[1;31mRealSet testing\033[m")

    val s0123 = new RealSet + 1 + 2 +3 + 0
    val s135 = new RealSet + 1 + 3 + 5

    println(s"$s0123 remove 1 = ${s0123 - 1} ")
    println(s"$s0123 add 7 = ${s0123 + 7} ")
    println(s"$s0123 union $s135 = ${s0123 union s135}")
    println(s"$s135 forall (_%2==1) = ${s135 forall (_%2==1) }")
    println(s"$s135 exists (_%2==1) = ${s135 forall (_%2==1) }")
    println(s"$s0123 filter (_%3==0) = ${s0123 filter (_%3==0) }")
  }

}


