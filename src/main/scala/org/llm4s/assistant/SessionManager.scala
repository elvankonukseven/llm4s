package org.llm4s.assistant

import org.llm4s.agent.{ Agent, AgentState }
import cats.implicits._
import java.nio.file.{ Files, Path, Paths, StandardOpenOption }
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try
import org.slf4j.LoggerFactory

/**
 * Manages session persistence using functional programming principles
 */
class SessionManager(sessionDir: String, agent: Agent) {
  private val logger = LoggerFactory.getLogger(getClass)

  /**
   * Ensures the session directory exists
   */
  def ensureSessionDirectory(): Either[String, Path] = {
    val path = Paths.get(sessionDir)
    Try(Files.createDirectories(path)).toEither.leftMap(ex => s"Failed to create session directory: ${ex.getMessage}")
  }

  /**
   * Saves a session and returns information about the saved session
   */
  def saveSession(state: SessionState, title: Option[String] = None): Either[String, SessionInfo] =
    state.agentState match {
      case None =>
        logger.debug("No agent state to save")
        Left("No agent state to save")
      case Some(agentState) =>
        logger.info("Saving session {} with title: {}", state.sessionId, title.getOrElse("Session"))
        for {
          _           <- ensureSessionDirectory()
          filePath    <- createFilePath(state, title)
          content     <- formatSessionContent(agentState, title.getOrElse("Session"), state.created)
          fileSize    <- writeSessionFile(filePath, content)
          sessionInfo <- createSessionInfo(state, title.getOrElse("Session"), filePath, agentState, fileSize)
        } yield {
          logger.info("Successfully saved session to: {}", filePath)
          sessionInfo
        }
    }

  /**
   * Lists recent sessions
   */
  def listSessions(limit: Int = 10): Either[String, Seq[SessionSummary]] =
    Try {
      Files
        .list(Paths.get(sessionDir))
        .filter(_.toString.endsWith(".md"))
        .filter(_.getFileName.toString.startsWith("session-"))
        .toArray
        .map(_.asInstanceOf[Path])
        .sortBy(_.getFileName.toString)
        .reverse
        .take(limit)
    }.toEither
      .leftMap(ex => s"Failed to list sessions: ${ex.getMessage}")
      .flatMap(paths => extractSessionSummaries(paths))

  /**
   * Creates a file path for the session
   */
  private def createFilePath(state: SessionState, title: Option[String]): Either[String, Path] =
    Try {
      val timestamp      = state.created.format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss"))
      val sessionIdShort = state.sessionId.take(8)
      val safeTitlePart  = title.map(t => s"-${sanitizeFilename(t)}").getOrElse("")
      val filename       = s"session-$timestamp-$sessionIdShort$safeTitlePart.md"
      Paths.get(sessionDir, filename)
    }.toEither.leftMap(ex => s"Failed to create file path: ${ex.getMessage}")

  /**
   * Formats session content as markdown
   */
  private def formatSessionContent(
    agentState: AgentState,
    title: String,
    created: LocalDateTime
  ): Either[String, String] =
    Try {
      val header        = createSessionHeader(title, created, agentState)
      val agentMarkdown = agent.formatStateAsMarkdown(agentState)
      header + agentMarkdown
    }.toEither.leftMap(ex => s"Failed to format session content: ${ex.getMessage}")

  /**
   * Writes session content to file and returns file size
   */
  private def writeSessionFile(filePath: Path, content: String): Either[String, Long] =
    Try {
      Files.write(
        filePath,
        content.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      Files.size(filePath)
    }.toEither.leftMap(ex => s"Failed to write session file: ${ex.getMessage}")

  /**
   * Creates session info from the saved session
   */
  private def createSessionInfo(
    state: SessionState,
    title: String,
    filePath: Path,
    agentState: AgentState,
    fileSize: Long
  ): Either[String, SessionInfo] =
    Try {
      SessionInfo(
        id = state.sessionId,
        title = title,
        filePath = filePath.toString,
        created = state.created,
        messageCount = agentState.conversation.messages.length,
        fileSize = fileSize
      )
    }.toEither.leftMap(ex => s"Failed to create session info: ${ex.getMessage}")

  /**
   * Extracts session summaries from file paths
   */
  private def extractSessionSummaries(paths: Array[Path]): Either[String, Seq[SessionSummary]] = {
    val summaries           = paths.map(extractSessionSummary).toSeq
    val (errors, successes) = summaries.partitionMap(identity)

    if (errors.nonEmpty) {
      Left(s"Failed to extract some session summaries: ${errors.mkString(", ")}")
    } else {
      Right(successes)
    }
  }

  /**
   * Extracts session summary from a single file
   */
  private def extractSessionSummary(path: Path): Either[String, SessionSummary] = {
    val filename = path.getFileName.toString

    Try {
      val content = Files.readString(path, StandardCharsets.UTF_8)
      val title = content.linesIterator
        .find(_.startsWith("# "))
        .map(_.drop(2).trim)
        .getOrElse(filename.stripSuffix(".md"))

      val created = extractCreatedTime(filename)
        .getOrElse(LocalDateTime.now()) // Fallback if parsing fails

      val sessionId = extractSessionId(filename)
        .getOrElse("unknown")

      SessionSummary(
        id = sessionId,
        title = title,
        filename = filename,
        created = created
      )
    }.toEither.left.map(ex => s"Failed to extract summary from $filename: ${ex.getMessage}")
  }

  /**
   * Creates session header markdown
   */
  private def createSessionHeader(title: String, created: LocalDateTime, agentState: AgentState): String =
    s"""# $title
       |
       |**Created:** ${created.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))}
       |**Tools Available:** ${agentState.tools.tools.map(_.name).mkString(", ")}
       |
       |---
       |
       |""".stripMargin

  /**
   * Sanitizes filename by removing invalid characters
   */
  private def sanitizeFilename(filename: String): String =
    filename.replaceAll("[^a-zA-Z0-9.-]", "_").take(50)

  /**
   * Extracts created time from filename
   */
  private def extractCreatedTime(filename: String): Option[LocalDateTime] =
    Try {
      val pattern = """session-(\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}).*""".r
      filename match {
        case pattern(timestamp) =>
          LocalDateTime.parse(timestamp, DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss"))
        case _ => LocalDateTime.now()
      }
    }.toOption

  /**
   * Extracts session ID from filename
   */
  private def extractSessionId(filename: String): Option[String] =
    Try {
      val pattern = """session-\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}-([a-f0-9]{8}).*""".r
      filename match {
        case pattern(sessionId) => sessionId
        case _                  => "unknown"
      }
    }.toOption
}
