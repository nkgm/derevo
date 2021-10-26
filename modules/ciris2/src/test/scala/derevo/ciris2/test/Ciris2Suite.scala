package derevo.ciris2.test

import derevo.derive
import derevo.ciris.cirisDecoder
import ciris.hocon._
import ciris.hocon.instances._

import com.typesafe.config.ConfigFactory
import cats.effect._
import cats.effect.unsafe.implicits._
import java.nio.file.Paths
import scala.concurrent.duration.Duration

import org.scalatest.funsuite.AnyFunSuite

@derive(cirisDecoder)
case class Data(name: String, list: List[String], map: Map[String, Int], rate: Rate)

@derive(cirisDecoder)
case class Rate(elements: Int, duration: Option[Duration])


class Ciris2Suite extends AnyFunSuite {

  val cfg = ConfigFactory.parseString(
    """
      |data {
      |  name = Demo
      |  list = [1, 2, 3]
      |  map.wtf = 1
      |  map.lol = 2
      |  map.wut = 3
      |  rate {
      |    elements = 2
      |    duration = 100 millis
      |  }
      |}
      """.stripMargin
  )

  test("hocon(cfg)(key) should work") {
    val res = hocon(cfg)("data").as[Data].load[IO].unsafeRunSync()
    //    assert(res == Rate(2, Option(Duration(100, "millis"))))
    assert(res == Data("Demo", List("1", "2", "3"), Map("wtf" -> 1, "lol" -> 2, "wut" -> 3), Rate(2, Option(Duration(100, "millis")))))
  }

  test("hocon(file)(key) should work") {
    val testHocon = Paths.get(ClassLoader.getSystemResource("test.hocon").toURI())
    val res = hocon(testHocon)("data.rate").as[Rate].load[IO].unsafeRunSync()
    assert(res == Rate(2, Option(Duration(100, "millis"))))
  }


  test("example in README.md should work") {
    @derive(cirisDecoder)
    case class DataConfig(name: String, addresses: List[String], mapping: Map[String, Int])

    val cfg = ConfigFactory.parseString(
      """
        |data {
        |  name = AAA
        |  addresses = [home, work, pub]
        |  mapping.until = 1
        |  mapping.from  = 2
        |  mapping.to    = 3
        |}
      """.stripMargin
    )

    val hoconSource = hocon(cfg) _
    val dataConfig = hoconSource("data").as[DataConfig].load[IO].unsafeRunSync()

    assert(dataConfig == DataConfig("AAA", List("home", "work", "pub"), Map("until" -> 1, "from" -> 2, "to" -> 3)))
  }

}
