package org.llm4s.trace

import org.llm4s.llmconnect.model.TokenUsage
import java.time.Instant

/**
 * Type-safe trace events for better composability and type safety
 */
sealed trait TraceEvent {
  def timestamp: Instant
  def eventType: String
  def toJson: ujson.Value
}

object TraceEvent {

  case class AgentInitialized(
    query: String,
    tools: Vector[String],
    timestamp: Instant = Instant.now()
  ) extends TraceEvent {
    def eventType: String = "agent_initialized"
    def toJson: ujson.Value = ujson.Obj(
      "event_type" -> eventType,
      "timestamp"  -> timestamp.toString,
      "query"      -> query,
      "tools"      -> ujson.Arr(tools.map(ujson.Str(_)): _*)
    )
  }

  case class CompletionReceived(
    id: String,
    model: String,
    toolCalls: Int,
    content: String,
    timestamp: Instant = Instant.now()
  ) extends TraceEvent {
    def eventType: String = "completion_received"
    def toJson: ujson.Value = ujson.Obj(
      "event_type"    -> eventType,
      "timestamp"     -> timestamp.toString,
      "completion_id" -> id,
      "model"         -> model,
      "tool_calls"    -> toolCalls,
      "content"       -> content
    )
  }

  case class ToolExecuted(
    name: String,
    input: String,
    output: String,
    duration: Long,
    success: Boolean,
    timestamp: Instant = Instant.now()
  ) extends TraceEvent {
    def eventType: String = "tool_executed"
    def toJson: ujson.Value = ujson.Obj(
      "event_type"  -> eventType,
      "timestamp"   -> timestamp.toString,
      "tool_name"   -> name,
      "input"       -> input,
      "output"      -> output,
      "duration_ms" -> duration,
      "success"     -> success
    )
  }

  case class ErrorOccurred(
    error: Throwable,
    context: String,
    timestamp: Instant = Instant.now()
  ) extends TraceEvent {
    def eventType: String = "error_occurred"
    def toJson: ujson.Value = ujson.Obj(
      "event_type"    -> eventType,
      "timestamp"     -> timestamp.toString,
      "error_type"    -> error.getClass.getSimpleName,
      "error_message" -> error.getMessage,
      "context"       -> context,
      "stack_trace"   -> error.getStackTrace.take(5).mkString("\n")
    )
  }

  case class TokenUsageRecorded(
    usage: TokenUsage,
    model: String,
    operation: String,
    timestamp: Instant = Instant.now()
  ) extends TraceEvent {
    def eventType: String = "token_usage_recorded"
    def toJson: ujson.Value = ujson.Obj(
      "event_type"        -> eventType,
      "timestamp"         -> timestamp.toString,
      "model"             -> model,
      "operation"         -> operation,
      "prompt_tokens"     -> usage.promptTokens,
      "completion_tokens" -> usage.completionTokens,
      "total_tokens"      -> usage.totalTokens
    )
  }

  case class AgentStateUpdated(
    status: String,
    messageCount: Int,
    logCount: Int,
    timestamp: Instant = Instant.now()
  ) extends TraceEvent {
    def eventType: String = "agent_state_updated"
    def toJson: ujson.Value = ujson.Obj(
      "event_type"    -> eventType,
      "timestamp"     -> timestamp.toString,
      "status"        -> status,
      "message_count" -> messageCount,
      "log_count"     -> logCount
    )
  }

  case class CustomEvent(
    name: String,
    data: ujson.Value,
    timestamp: Instant = Instant.now()
  ) extends TraceEvent {
    def eventType: String = "custom_event"
    def toJson: ujson.Value = ujson.Obj(
      "event_type" -> eventType,
      "timestamp"  -> timestamp.toString,
      "name"       -> name,
      "data"       -> data
    )
  }
}
