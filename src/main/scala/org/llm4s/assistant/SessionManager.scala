package org.llm4s.assistant

import org.llm4s.agent.{ Agent, AgentState, AgentStatus }
import org.llm4s.llmconnect.model._
import org.llm4s.toolapi.ToolRegistry
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
   * Converts SessionState to JSON format for persistence
   */
  private def sessionStateToJson(state: SessionState): ujson.Value =
    ujson.Obj(
      "sessionId"    -> state.sessionId,
      "sessionDir"   -> state.sessionDir,
      "created"      -> state.created.toString,
      "conversation" -> state.agentState.map(as => conversationToJson(as.conversation)).getOrElse(ujson.Null),
      "userQuery"    -> state.agentState.map(as => ujson.Str(as.userQuery)).getOrElse(ujson.Str("")),
      "status"       -> state.agentState.map(as => ujson.Str(as.status.toString)).getOrElse(ujson.Str("InProgress")),
      "logs"         -> state.agentState.map(as => ujson.Arr.from(as.logs.map(ujson.Str(_)))).getOrElse(ujson.Arr())
    )

  /**
   * Converts Conversation to JSON array
   */
  private def conversationToJson(conv: Conversation): ujson.Value =
    ujson.Arr.from(conv.messages.map(messageToJson))

  /**
   * Converts individual Message to JSON object
   */
  private def messageToJson(msg: Message): ujson.Value = msg match {
    case UserMessage(content) =>
      ujson.Obj("type" -> "user", "content" -> ujson.Str(Option(content).getOrElse("")))
    case SystemMessage(content) =>
      ujson.Obj("type" -> "system", "content" -> ujson.Str(Option(content).getOrElse("")))
    case ToolMessage(id, content) =>
      ujson.Obj(
        "type"       -> "tool",
        "toolCallId" -> ujson.Str(Option(id).getOrElse("")),
        "content"    -> ujson.Str(Option(content).getOrElse(""))
      )
    case AssistantMessage(content, toolCalls) =>
      ujson.Obj(
        "type"    -> "assistant",
        "content" -> ujson.Str(content.getOrElse("")),
        "toolCalls" -> ujson.Arr.from(
          toolCalls.map(tc =>
            ujson.Obj(
              "id"        -> ujson.Str(Option(tc.id).getOrElse("")),
              "name"      -> ujson.Str(Option(tc.name).getOrElse("")),
              "arguments" -> Option(tc.arguments).getOrElse(ujson.Obj())
            )
          )
        )
      )
  }

  /**
   * Ensures the session directory exists
   */
  private def ensureSessionDirectory(): Either[String, Path] =
    Paths.get(sessionDir).asRight[String].flatMap { path =>
      Try(Files.createDirectories(path)).toEither
        .leftMap(ex => s"Failed to create session directory: ${ex.getMessage}")
    }

  /**
   * Saves a session in both JSON and markdown formats
   */
  def saveSession(state: SessionState, title: Option[String] = None): Either[String, SessionInfo] =
    state.agentState match {
      case None =>
        Left("No agent state to save")
      case Some(agentState) =>
        val sessionTitle = title.getOrElse("Session")
        logger.info("Saving session {} with title: {}", state.sessionId, sessionTitle)
        ensureSessionDirectory().flatMap { _ =>
          createFilePaths(title).flatMap { case (jsonPath, markdownPath) =>
            for {
              jsonContent     <- createJsonContent(state)
              markdownContent <- formatSessionContent(agentState, sessionTitle, state.created)
              jsonSize        <- writeSessionFile(jsonPath, jsonContent)
              _               <- writeSessionFile(markdownPath, markdownContent)
              sessionInfo     <- createSessionInfo(state, sessionTitle, jsonPath, agentState, jsonSize)
            } yield {
              logger.info("Successfully saved session JSON: {} and markdown: {}", jsonPath, markdownPath)
              sessionInfo
            }
          }
        }
    }

  /**
   * Creates JSON content for session storage
   */
  private def createJsonContent(state: SessionState): Either[String, String] =
    Try(sessionStateToJson(state).toString()).toEither
      .leftMap(ex => s"Failed to serialize session to JSON: ${ex.getMessage}")

  /**
   * Converts JSON back to SessionState for loading
   */
  private def jsonToSessionState(json: ujson.Value, tools: ToolRegistry): SessionState = {
    val obj        = json.obj
    val sessionId  = obj("sessionId").str
    val sessionDir = obj("sessionDir").str
    val created    = LocalDateTime.parse(obj("created").str)

    val agentState =
      if (obj("conversation") == ujson.Null) None
      else {
        val conversation = jsonToConversation(obj("conversation"))
        val userQuery    = obj("userQuery").str
        val status = obj("status").str match {
          case "InProgress"      => AgentStatus.InProgress
          case "WaitingForTools" => AgentStatus.WaitingForTools
          case "Complete"        => AgentStatus.Complete
          case failureStr if failureStr.startsWith("Failed(") =>
            val errorMsg = failureStr.stripPrefix("Failed(").stripSuffix(")")
            AgentStatus.Failed(errorMsg)
          case other => AgentStatus.Failed(s"Unknown status: $other")
        }
        val logs = obj("logs").arr.map(_.str).toSeq
        Some(AgentState(conversation, tools, userQuery, status, logs))
      }

    SessionState(agentState, sessionId, sessionDir, created)
  }

  /**
   * Converts JSON array back to Conversation
   */
  private def jsonToConversation(json: ujson.Value): Conversation = {
    val messages = json.arr.map(jsonToMessage).toSeq
    Conversation(messages)
  }

  /**
   * Converts JSON object back to Message
   */
  private def jsonToMessage(json: ujson.Value): Message = {
    val obj = json.obj
    obj("type").str match {
      case "user"   => UserMessage(obj("content").str)
      case "system" => SystemMessage(obj("content").str)
      case "tool"   => ToolMessage(obj("toolCallId").str, obj("content").str)
      case "assistant" =>
        val content = obj("content").str match {
          case ""  => None
          case str => Some(str)
        }
        val toolCalls = obj("toolCalls").arr.map { tc =>
          val tcObj = tc.obj
          ToolCall(tcObj("id").str, tcObj("name").str, tcObj("arguments"))
        }.toSeq
        AssistantMessage(content, toolCalls)
    }
  }

  /**
   * Loads a session from JSON file by title
   */
  def loadSession(sessionTitle: String, tools: ToolRegistry): Either[String, SessionState] = {
    val jsonPath = Paths.get(sessionDir, s"${sanitizeFilename(sessionTitle)}.json")

    for {
      _ <- ensureSessionDirectory()
      _ <- Either.cond(Files.exists(jsonPath), (), s"Session '$sessionTitle' not found")
      jsonContent <- Try(Files.readString(jsonPath, StandardCharsets.UTF_8)).toEither
        .leftMap(ex => s"Failed to read session file: ${ex.getMessage}")
      json <- Try(ujson.read(jsonContent)).toEither
        .leftMap(ex => s"Failed to parse JSON: ${ex.getMessage}")
      state <- Try(jsonToSessionState(json, tools)).toEither
        .leftMap(ex => s"Failed to deserialize session: ${ex.getMessage}")
    } yield {
      logger.info("Successfully loaded session: {} from {}", sessionTitle, jsonPath)
      state
    }
  }

  /**
   * Lists recent sessions for welcome screen display
   */
  def listRecentSessions(limit: Int = 5): Either[String, Seq[String]] =
    Try {
      Files
        .list(Paths.get(sessionDir))
        .filter(_.toString.endsWith(".json"))
        .toArray
        .map(_.asInstanceOf[Path])
        .sortBy(path => Try(Files.getLastModifiedTime(path)).getOrElse(java.nio.file.attribute.FileTime.fromMillis(0)))
        .reverse
        .take(limit)
        .map(_.getFileName.toString.stripSuffix(".json"))
        .toSeq
    }.toEither
      .leftMap(ex => s"Failed to list recent sessions: ${ex.getMessage}")

  /**
   * Creates file paths for the session (both JSON and markdown)
   */
  private def createFilePaths(title: Option[String]): Either[String, (Path, Path)] =
    Try {
      val sessionTitle = title.getOrElse("Untitled Session")
      val baseFilename = sanitizeFilename(sessionTitle)

      // Handle naming collisions by appending numbers
      val uniqueBasename = findUniqueFilename(baseFilename)

      val jsonPath     = Paths.get(sessionDir, s"$uniqueBasename.json")
      val markdownPath = Paths.get(sessionDir, s"$uniqueBasename.md")
      (jsonPath, markdownPath)
    }.toEither.leftMap(ex => s"Failed to create file paths: ${ex.getMessage}")

  /**
   * Finds a unique filename by appending numbers if necessary
   */
  private def findUniqueFilename(baseFilename: String): String = {
    def checkExists(filename: String): Boolean =
      Files.exists(Paths.get(sessionDir, s"$filename.json")) ||
        Files.exists(Paths.get(sessionDir, s"$filename.md"))

    if (!checkExists(baseFilename)) {
      baseFilename
    } else {
      LazyList
        .from(2)
        .map(i => s"$baseFilename ($i)")
        .find(!checkExists(_))
        .getOrElse(s"$baseFilename (${System.currentTimeMillis()})")
    }
  }

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

}
