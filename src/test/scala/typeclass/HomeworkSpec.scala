package typeclass

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class HomeworkSpec extends AnyFlatSpec with should.Matchers {

  "task 1" should "pass" in {
    import Task1._

    val m1 = Money(7)
    val m2 = Money(3)
    List(m1, m2).sorted should be (List(m2, m1))
  }

  "task 2" should "pass" in {
    import Task2._

    val user = User("1", "Oleg")
    user.show should be ("Oleg")
  }

  "task 3" should "pass" in {
    import Task3._

    "asdasd".parse[User] should be (a[Left[Error, _]])
    "User(1,Oleg)".parse[User] should be (Right(User("1", "Oleg")))
  }

  "task 4" should "pass" in {
    import Task4._

    "1" ==== "2" should be (false)
    1 ==== 1 should be (true)
    """1 ==== "1"""" shouldNot compile
  }

  "FlatMap task" should "pass" in {
    import AdvancedHomework._

    Option(User("1", "aaa"))._flatMap {
      case User("1", name) => Some(Admin("1", name, "hello"))
      case _ => None
    } should contain (Admin("1", "aaa", "hello"))
  }
}
