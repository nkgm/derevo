package ciris

import com.typesafe.{config => typesafe}

import java.nio.file.Path
import scala.util.Try
import scala.util.control.NonFatal

package object hocon {

  type ConfigValueDecoder[A] = ConfigDecoder[typesafe.ConfigValue, A]

  object instances extends ConfigValueDecoderInstances

  def hocon(config: typesafe.Config)(key: String): ciris.ConfigValue[Effect, typesafe.ConfigValue] =
    ConfigValue.suspend((readEntry(config, key)) match {
      case Right(value) => ConfigValue.loaded(ciris.ConfigKey(key), value)
      case Left(value)  => ConfigValue.failed(value)
    })

  def hocon(path: Path)(key: String): ciris.ConfigValue[Effect, typesafe.ConfigValue] = {
    ciris.file(path).flatMap(s => hocon(typesafe.ConfigFactory.parseString(s))(key))
  }

  def readEntry(config: typesafe.Config, path: String): Either[ciris.ConfigError, typesafe.ConfigValue] =
    Try(config.getValue(path)).fold(
      {
        case _: typesafe.ConfigException.Missing => Right(typesafe.ConfigValueFactory.fromAnyRef(null))
        case NonFatal(ex)                        => Left(ConfigError(ex.getMessage))
      },
      Right(_)
    )

  private[hocon] val DummyPath         = "config-decoder-path"
  private[hocon] val SeqElementKeyType = ConfigKey("Seq element")
  private[hocon] val MapEntryKeyType   = ConfigKey("Map entry")

  private[hocon] def nonFatal[A](
      fn: typesafe.Config => String => A
  ): ConfigValueDecoder[A] =
    catchNonFatal(cfg => path => Right(fn(cfg)(path)))

  private[hocon] def catchNonFatal[A](
      fn: typesafe.Config => String => Either[ConfigError, A]
  ): ConfigValueDecoder[A] =
    ConfigDecoder.lift(value =>
      Try(fn(value.atKey(DummyPath))(DummyPath)).fold(err => Left(ConfigError(err.getMessage)), identity)
    )

}
