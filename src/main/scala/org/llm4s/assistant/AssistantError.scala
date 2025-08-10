package org.llm4s.assistant

/**
 * Error types for assistant package operations.
 */
sealed abstract class AssistantError extends Exception {
  def message: String
  def cause: Option[Throwable] = None

  override def getMessage: String = message
}

object AssistantError {

  // Console errors
  final case class IOError(
    override val message: String,
    override val cause: Option[Throwable] = None
  ) extends AssistantError

  final case class EOFError(
    override val message: String
  ) extends AssistantError

  final case class DisplayError(
    override val message: String,
    override val cause: Option[Throwable] = None
  ) extends AssistantError

  // Session management errors
  final case class SessionError(
    override val message: String,
    sessionId: Option[String] = None,
    override val cause: Option[Throwable] = None
  ) extends AssistantError

  final case class FileError(
    override val message: String,
    path: String,
    override val cause: Option[Throwable] = None
  ) extends AssistantError

  final case class SerializationError(
    override val message: String,
    override val cause: Option[Throwable] = None
  ) extends AssistantError

  // LLM integration wrapper
  final case class LLMError(
    llmError: org.llm4s.error.LLMError
  ) extends AssistantError {
    override val message: String = llmError.formatted
  }

  // Smart constructors
  def sessionNotFound(sessionId: String): AssistantError =
    SessionError(s"Session not found: $sessionId", Some(sessionId))

  def fileReadFailed(path: String, cause: Throwable): AssistantError =
    FileError(s"Failed to read file: $path", path, Some(cause))

  def fileWriteFailed(path: String, cause: Throwable): AssistantError =
    FileError(s"Failed to write file: $path", path, Some(cause))

  def jsonSerializationFailed(dataType: String, cause: Throwable): AssistantError =
    SerializationError(s"Failed to serialize $dataType to JSON", Some(cause))

  def jsonDeserializationFailed(dataType: String, cause: Throwable): AssistantError =
    SerializationError(s"Failed to deserialize JSON to $dataType", Some(cause))

  def fromLLMError(error: org.llm4s.error.LLMError): AssistantError =
    LLMError(error)
}
