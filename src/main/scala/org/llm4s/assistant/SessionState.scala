package org.llm4s.assistant

import org.llm4s.agent.AgentState

import java.time.LocalDateTime
import java.util.UUID

/**
 * Represents the state of an interactive assistant session
 */
case class SessionState(
  agentState: Option[AgentState],
  sessionId: String,
  sessionDir: String,
  created: LocalDateTime = LocalDateTime.now()
) {
  def withAgentState(newState: AgentState): SessionState =
    copy(agentState = Some(newState))

  def withNewSession(): SessionState =
    copy(
      agentState = None,
      sessionId = UUID.randomUUID().toString,
      created = LocalDateTime.now()
    )
}

/**
 * Information about a saved session
 */
case class SessionInfo(
  id: String,
  title: String,
  filePath: String,
  created: LocalDateTime,
  messageCount: Int,
  fileSize: Long
)

/**
 * Summary of a session for listing purposes
 */
case class SessionSummary(
  id: String,
  title: String,
  filename: String,
  created: LocalDateTime
)
