package org.llm4s.llmconnect.config

import org.llm4s.config.{ ConfigKeys, ConfigReader }
import ConfigKeys._
import org.llm4s.config.DefaultConfig._
sealed trait ProviderConfig {
  def model: String
  def contextWindow: Int
  def reserveCompletion: Int
}

case class OpenAIConfig(
  apiKey: String,
  model: String,
  organization: Option[String],
  baseUrl: String,
  contextWindow: Int,
  reserveCompletion: Int
) extends ProviderConfig

object OpenAIConfig {

  def from(modelName: String, config: ConfigReader): OpenAIConfig = {
    val (contextWindow, reserveCompletion) = getContextWindowForModel(modelName)
    OpenAIConfig(
      apiKey = config
        .get(OPENAI_API_KEY)
        .getOrElse(
          throw new IllegalArgumentException("OPENAI_API_KEY not set, required when using openai/ model.")
        ),
      model = modelName,
      organization = config.get(OPENAI_ORG),
      baseUrl = config.getOrElse(OPENAI_BASE_URL, DEFAULT_OPENAI_BASE_URL),
      contextWindow = contextWindow,
      reserveCompletion = reserveCompletion
    )
  }

  private def getContextWindowForModel(modelName: String): (Int, Int) = {
    val standardReserve = 4096 // 4K tokens reserved for completion

    modelName match {
      // GPT-4 family - 128K context window
      case name if name.contains("gpt-4o")      => (128000, standardReserve)
      case name if name.contains("gpt-4-turbo") => (128000, standardReserve)
      case name if name.contains("gpt-4")       => (8192, standardReserve) // Original GPT-4 was 8K
      // GPT-3.5 family - 16K context window
      case name if name.contains("gpt-3.5-turbo") => (16384, standardReserve)
      // o1 family - 128K context window
      case name if name.contains("o1-") => (128000, standardReserve)
      // Default fallback
      case _ => (8192, standardReserve)
    }
  }
}

case class AzureConfig(
  endpoint: String,
  apiKey: String,
  model: String,
  apiVersion: String,
  contextWindow: Int,
  reserveCompletion: Int
) extends ProviderConfig

object AzureConfig {

  def from(modelName: String, config: ConfigReader): AzureConfig = {
    val endpoint = config
      .get(AZURE_API_BASE)
      .getOrElse(
        throw new IllegalArgumentException("AZURE_API_BASE not set, required when using azure/ model.")
      )
    val apiKey = config
      .get(AZURE_API_KEY)
      .getOrElse(
        throw new IllegalArgumentException("AZURE_API_KEY not set, required when using azure/ model.")
      )
    val apiVersion                         = config.get(AZURE_API_VERSION).getOrElse(DEFAULT_AZURE_V2025_01_01_PREVIEW)
    val (contextWindow, reserveCompletion) = getContextWindowForModel(modelName)

    AzureConfig(
      endpoint = endpoint,
      apiKey = apiKey,
      model = modelName,
      apiVersion = apiVersion,
      contextWindow = contextWindow,
      reserveCompletion = reserveCompletion
    )
  }

  private def getContextWindowForModel(modelName: String): (Int, Int) = {
    val standardReserve = 4096 // 4K tokens reserved for completion

    // Azure uses OpenAI models, so apply same logic
    modelName match {
      // GPT-4 family - 128K context window
      case name if name.contains("gpt-4o")      => (128000, standardReserve)
      case name if name.contains("gpt-4-turbo") => (128000, standardReserve)
      case name if name.contains("gpt-4")       => (8192, standardReserve) // Original GPT-4 was 8K
      // GPT-3.5 family - 16K context window
      case name if name.contains("gpt-3.5-turbo") => (16384, standardReserve)
      // o1 family - 128K context window
      case name if name.contains("o1-") => (128000, standardReserve)
      // Default fallback
      case _ => (8192, standardReserve)
    }
  }
}

case class AnthropicConfig(
  apiKey: String,
  model: String,
  baseUrl: String,
  contextWindow: Int,
  reserveCompletion: Int
) extends ProviderConfig

object AnthropicConfig {

  def from(modelName: String, config: ConfigReader): AnthropicConfig = {
    val (contextWindow, reserveCompletion) = getContextWindowForModel(modelName)
    AnthropicConfig(
      apiKey = config
        .get(ANTHROPIC_API_KEY)
        .getOrElse(
          throw new IllegalArgumentException("ANTHROPIC_API_KEY not set, required when using anthropic/ model.")
        ),
      model = modelName,
      baseUrl = config.getOrElse(ANTHROPIC_BASE_URL, DEFAULT_ANTHROPIC_BASE_URL),
      contextWindow = contextWindow,
      reserveCompletion = reserveCompletion
    )
  }

  private def getContextWindowForModel(modelName: String): (Int, Int) = {
    val standardReserve = 4096 // 4K tokens reserved for completion

    modelName match {
      // Claude-3 family - 200K context window
      case name if name.contains("claude-3")   => (200000, standardReserve)
      case name if name.contains("claude-3.5") => (200000, standardReserve)
      // Claude Instant - 100K context window
      case name if name.contains("claude-instant") => (100000, standardReserve)
      // Default fallback for Claude models
      case _ => (200000, standardReserve)
    }
  }
}

case class OllamaConfig(
  model: String,
  baseUrl: String,
  contextWindow: Int,
  reserveCompletion: Int
) extends ProviderConfig

object OllamaConfig {
  def from(modelName: String, config: ConfigReader): OllamaConfig = {
    val baseUrl = config
      .get(OLLAMA_BASE_URL)
      .getOrElse(throw new IllegalArgumentException("OLLAMA_BASE_URL must be set for ollama provider"))
    val (contextWindow, reserveCompletion) = getContextWindowForModel(modelName)
    OllamaConfig(
      model = modelName,
      baseUrl = baseUrl,
      contextWindow = contextWindow,
      reserveCompletion = reserveCompletion
    )
  }

  private def getContextWindowForModel(modelName: String): (Int, Int) = {
    val standardReserve = 4096 // 4K tokens reserved for completion

    // Ollama model context windows vary, use reasonable defaults
    modelName match {
      case name if name.contains("llama2")    => (4096, standardReserve)
      case name if name.contains("llama3")    => (8192, standardReserve)
      case name if name.contains("codellama") => (16384, standardReserve)
      case name if name.contains("mistral")   => (32768, standardReserve)
      // Default fallback for unknown Ollama models
      case _ => (8192, standardReserve)
    }
  }
}
