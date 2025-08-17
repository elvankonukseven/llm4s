package org.llm4s.error

/**
 * Enhanced comprehensive error hierarchy for LLM operations using ADTs.
 *
 * This replaces the simple org.llm4s.llmconnect.model.LLMError with a
 * much more comprehensive error system.
 *
 * Key improvements over legacy version:
 * - Structured context information
 * - Recovery guidance built-in
 * - Provider-agnostic design
 * - Rich error formatting
 * - Type-safe error handling
 */
sealed abstract class LLMError extends Product with Serializable {

  /** Human-readable error message */
  def message: String

  /** Optional error code for programmatic handling */
  def code: Option[String] = None

  /** Additional context information */
  def context: Map[String, String] = Map.empty

  /** Whether this error is recoverable with retry */
  def isRecoverable: Boolean = false

  /** Suggested retry delay in milliseconds for recoverable errors */
  def retryDelay: Option[Long] = None

  /** Converts to a formatted error message with context */
  def formatted: String = {
    val contextStr = if (context.nonEmpty) {
      s" [${context.map { case (k, v) => s"$k: $v" }.mkString(", ")}]"
    } else ""

    val codeStr = code.map(c => s" (Code: $c)").getOrElse("")
    s"${getClass.getSimpleName}: $message$codeStr$contextStr"
  }
}

object LLMError {

  // private val logger = LoggerFactory.getLogger(getClass)

  /**
   * Authentication-related errors
   */
  final case class AuthenticationError(
    override val message: String,
    provider: String,
    override val code: Option[String] = None
  ) extends LLMError {
    override val context: Map[String, String] = Map("provider" -> provider)
  }

  /**
   * Rate limiting errors - typically recoverable with exponential backoff
   */
  final case class RateLimitError(
    override val message: String,
    retryAfter: Option[Long] = None,
    provider: String,
    requestsRemaining: Option[Int] = None,
    resetTime: Option[Long] = None
  ) extends LLMError {
    override val isRecoverable: Boolean   = true
    override val retryDelay: Option[Long] = retryAfter.map(_ * 1000) // Convert to millis
    override val context: Map[String, String] = Map(
      "provider" -> provider
    ) ++ retryAfter.map("retryAfter" -> _.toString) ++
      requestsRemaining.map("requestsRemaining" -> _.toString) ++
      resetTime.map("resetTime" -> _.toString)
  }

  /**
   * Service-level errors from LLM providers (5xx HTTP responses)
   */
  final case class ServiceError(
    override val message: String,
    httpStatus: Int,
    provider: String,
    requestId: Option[String] = None
  ) extends LLMError {
    override val isRecoverable: Boolean   = httpStatus >= 500
    override val retryDelay: Option[Long] = Some(1000) // 1-second base delay
    override val context: Map[String, String] = Map(
      "provider"   -> provider,
      "httpStatus" -> httpStatus.toString
    ) ++ requestId.map("requestId" -> _)
  }

  /**
   * Validation errors for malformed requests
   */
  final case class ValidationError(
    override val message: String,
    field: String,
    violations: List[String] = List.empty
  ) extends LLMError {
    override val context: Map[String, String] = Map("field" -> field) ++
      (if (violations.nonEmpty) Map("violations" -> violations.mkString("; ")) else Map.empty)
  }

  /**
   * Network-related errors (timeouts, connection issues)
   */
  final case class NetworkError(
    override val message: String,
    cause: Option[Throwable] = None,
    endpoint: String
  ) extends LLMError {
    override val isRecoverable: Boolean       = true
    override val retryDelay: Option[Long]     = Some(2000) // 2-second base delay
    override val context: Map[String, String] = Map("endpoint" -> endpoint)
  }

  /**
   * Configuration-related errors
   */
  final case class ConfigurationError(
    override val message: String,
    missingKeys: List[String] = List.empty
  ) extends LLMError {
    override val context: Map[String, String] =
      if (missingKeys.nonEmpty) Map("missingKeys" -> missingKeys.mkString(", "))
      else Map.empty
  }

  /**
   * Unknown/unexpected errors with full exception context
   */
  final case class UnknownError(
    override val message: String,
    cause: Option[Throwable] = None
  ) extends LLMError {
    override val context: Map[String, String] = cause match {
      case Some(ex) =>
        Map(
          "exceptionType" -> ex.getClass.getSimpleName,
          "stackTrace"    -> ex.getStackTrace.take(3).mkString("; ")
        )
      case None => Map.empty
    }
  }

  /**
   * Image processing errors
   */
  final case class ProcessingError(
    override val message: String,
    operation: String,
    cause: Option[Throwable] = None
  ) extends LLMError {
    override val context: Map[String, String] = Map("operation" -> operation) ++
      cause.map(c => "cause" -> c.getMessage).toMap
  }

  /**
   * Invalid input errors for image processing
   */
  final case class InvalidInputError(
    override val message: String,
    field: String,
    value: String,
    reason: String
  ) extends LLMError {
    override val context: Map[String, String] = Map(
      "field"  -> field,
      "value"  -> value,
      "reason" -> reason
    )
  }

  /**
   * API errors for external image processing services
   */
  final case class APIError(
    override val message: String,
    provider: String,
    statusCode: Option[Int] = None,
    responseBody: Option[String] = None
  ) extends LLMError {
    override val isRecoverable: Boolean   = statusCode.exists(_ >= 500)
    override val retryDelay: Option[Long] = if (isRecoverable) Some(2000) else None
    override val context: Map[String, String] = Map("provider" -> provider) ++
      statusCode.map("statusCode" -> _.toString) ++
      responseBody.map("responseBody" -> _)
  }

  // Smart constructors
  def authenticationFailed(provider: String, details: String): LLMError =
    AuthenticationError(s"Authentication failed for $provider: $details", provider)

  def rateLimited(provider: String, retryAfter: Option[Long] = None): LLMError =
    RateLimitError(s"Rate limited by $provider", retryAfter, provider)

  def invalidField(field: String, reason: String): LLMError =
    ValidationError(s"Invalid $field: $reason", field)

  def missingConfig(keys: List[String]): LLMError =
    ConfigurationError(s"Missing configuration: ${keys.mkString(", ")}", keys)

  // Image processing error constructors
  def processingFailed(operation: String, message: String, cause: Option[Throwable] = None): LLMError =
    ProcessingError(s"Image processing failed during $operation: $message", operation, cause)

  def invalidImageInput(field: String, value: String, reason: String): LLMError =
    InvalidInputError(s"Invalid image input for $field: $value", field, value, reason)

  def apiCallFailed(
    provider: String,
    message: String,
    statusCode: Option[Int] = None,
    responseBody: Option[String] = None
  ): LLMError =
    APIError(s"API call to $provider failed: $message", provider, statusCode, responseBody)

  def fromThrowable(throwable: Throwable): LLMError = throwable match {
    case _: java.net.SocketTimeoutException =>
      NetworkError("Network error", Some(throwable), endpoint = "Unknown")
    case _: java.net.ConnectException =>
      NetworkError("Connection failed", Some(throwable), "Unknown")
    case ex if ex.getMessage != null && ex.getMessage.contains("401") =>
      AuthenticationError("Authentication failed", "Unknown Provider", Some("000"))
    case ex if ex.getMessage != null && ex.getMessage.contains("429") =>
      RateLimitError("Rate limited", None, "Unknown Provider")
    case ex =>
      UnknownError(Option(ex.getMessage).getOrElse("Unknown error"), Some(ex))
  }
}
