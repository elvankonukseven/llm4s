package org.llm4s.assistant

import org.llm4s.toolapi.ToolRegistry
import cats.implicits._
import scala.io.StdIn
import scala.util.Try

/**
 * Handles console-based user interface using functional programming principles
 */
class ConsoleInterface(tools: ToolRegistry, sessionManager: SessionManager) {

  // ANSI color codes for terminal output formatting
  private val Colors = Map(
    "reset"  -> "\u001b[0m",
    "blue"   -> "\u001b[34m",
    "green"  -> "\u001b[32m",
    "yellow" -> "\u001b[33m",
    "red"    -> "\u001b[31m",
    "cyan"   -> "\u001b[36m",
    "bold"   -> "\u001b[1m"
  )

  def colorize(text: String, color: String): String =
    s"${Colors(color)}$text${Colors("reset")}"

  /**
   * Shows welcome message with recent sessions
   */
  def showWelcome(): Either[String, Unit] = {
    val recentSessions = sessionManager.listRecentSessions(5).getOrElse(Seq.empty)

    val recentSessionsDisplay = if (recentSessions.nonEmpty) {
      s"""
${colorize("Recent Sessions (load with /load \"name\"):", "bold")}
${recentSessions.zipWithIndex
          .map { case (title, index) =>
            s"  ${index + 1}. ${colorize(title, "green")}"
          }
          .mkString("\n")}
"""
    } else {
      ""
    }

    val welcome = s"""
${colorize("╭─────────────────────────────────────────────╮", "cyan")}
${colorize("│           🤖 LLM4S Assistant Agent          │", "cyan")}
${colorize("╰─────────────────────────────────────────────╯", "cyan")}

${colorize("Available Tools:", "bold")}
${tools.tools.map(tool => s"  • ${tool.name}").mkString("\n")}$recentSessionsDisplay

${colorize("Commands:", "bold")}
  • ${colorize("/load \"Session Name\"", "yellow")} - Continue a previous session
  • ${colorize("/help", "yellow")} - Show this help message
  • ${colorize("/new", "yellow")} - Start a new conversation
  • ${colorize("/save [title]", "yellow")} - Save current session
  • ${colorize("/sessions", "yellow")} - List recent sessions
  • ${colorize("/quit", "yellow")} - Save and exit

${colorize("Just type your message to start chatting or load a session!", "green")}
"""
    Try(println(welcome)).toEither.leftMap(ex => s"Failed to show welcome message: ${ex.getMessage}")
  }

  /**
   * Prompts user for input
   */
  def promptUser(): Either[String, String] =
    Try {
      print(colorize("\nUser> ", "cyan"))
      Option(StdIn.readLine())
    }.toEither
      .leftMap(ex => s"Failed to read input: ${ex.getMessage}")
      .flatMap {
        case Some(input) => Right(input)
        case None        => Left("EOF reached")
      }

  /**
   * Prompts user for input with custom prompt text
   */
  def promptForInput(promptText: String): Either[String, String] =
    Try {
      print(colorize(promptText, "yellow"))
      Option(StdIn.readLine())
    }.toEither
      .leftMap(ex => s"Failed to read input: ${ex.getMessage}")
      .flatMap {
        case Some(input) => Right(input)
        case None        => Left("EOF reached")
      }

  /**
   * Displays a message to the user
   */
  def displayMessage(message: String, messageType: MessageType = MessageType.Info): Either[String, Unit] = {
    val coloredMessage = messageType match {
      case MessageType.Info              => message
      case MessageType.Success           => colorize(message, "green")
      case MessageType.Warning           => colorize(message, "yellow")
      case MessageType.Error             => colorize(message, "red")
      case MessageType.AssistantResponse => s"\n🤖 ${colorize("Assistant:", "green")} $message"
    }

    Try(println(coloredMessage)).toEither.leftMap(ex => s"Failed to display message: ${ex.getMessage}")
  }

  /**
   * Shows help message
   */
  def showHelp(): String =
    s"""${colorize("Available Commands:", "bold")}
  • ${colorize("/load \"Session Name\"", "yellow")} - Continue a previous session
  • ${colorize("/help", "yellow")} - Show this help message
  • ${colorize("/new", "yellow")} - Start a new conversation (saves current)
  • ${colorize("/save [title]", "yellow")} - Save current session with optional title
  • ${colorize("/sessions", "yellow")} - List recent saved sessions
  • ${colorize("/quit", "yellow")} - Save current session and exit

${colorize("Available Tools:", "bold")}
${tools.tools.map(tool => s"  • ${colorize(tool.name, "green")} - ${tool.description}").mkString("\n")}

${colorize("Tips:", "bold")}
  • Conversations continue across multiple messages
  • Sessions are automatically saved as markdown files
  • Use natural language - the assistant will use tools as needed
"""

  /**
   * Formats session summaries for display
   */
  def formatSessionList(sessions: Seq[String]): String =
    if (sessions.isEmpty) {
      "No saved sessions found."
    } else {
      val formatted = sessions
        .map(title => s"  • ${colorize(title, "green")}")
        .mkString("\n")

      s"${colorize("Recent sessions:", "bold")}\n$formatted"
    }

  /**
   * Displays an error message
   */
  def displayError(error: String): Either[String, Unit] =
    displayMessage(s"Error: $error", MessageType.Error)

  /**
   * Displays a success message
   */
  def displaySuccess(message: String): Either[String, Unit] =
    displayMessage(message, MessageType.Success)
}

/**
 * Enumeration for different message types
 */
sealed trait MessageType
object MessageType {
  case object Info              extends MessageType
  case object Success           extends MessageType
  case object Warning           extends MessageType
  case object Error             extends MessageType
  case object AssistantResponse extends MessageType
}
