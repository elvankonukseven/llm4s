package org.llm4s.config

import io.github.cdimascio.dotenv.Dotenv
import org.llm4s.Result
import org.llm4s.error.NotFoundError
import org.llm4s.types.Result

import scala.util.Try

trait ConfigReader {
  def get(key: String): Option[String]
  def getOrElse(key: String, default: String): String = get(key).getOrElse(default)

  def require(key: String): Result[String] =
    get(key).filter(_.trim.nonEmpty).toRight(NotFoundError(s"Missing: $key", key))
}

object ConfigReader {

  def apply(map: Map[String, String]): ConfigReader = new ConfigReader {
    override def get(key: String): Option[String] = map.get(key)
  }

  def LLMConfig(): Result[ConfigReader] =
    Result.fromTry(
      Try {
        val env = Dotenv.configure().ignoreIfMissing().load() // Load throws DotenvException
        (key: String) => Option(env.get(key))
      }
    )
}
