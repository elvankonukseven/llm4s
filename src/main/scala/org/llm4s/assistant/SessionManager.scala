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
  private def ensureSessionDirectory(): Either[AssistantError, Path] = {
    val path = Paths.get(sessionDir)
    Try(Files.createDirectories(path)).toEither
      .leftMap(ex => AssistantError.fileWriteFailed(sessionDir, ex))
      .map(_ => path)
  }

  /**
   * Saves a session in both JSON and markdown formats
   */
  def saveSession(state: SessionState, title: Option[String] = None): Either[AssistantError, SessionInfo] =
    state.agentState match {
      case None =>
        Left(AssistantError.SessionError("No agent state to save"))
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
  private def createJsonContent(state: SessionState): Either[AssistantError, String] =
    Try(sessionStateToJson(state).toString()).toEither
      .leftMap(ex => AssistantError.jsonSerializationFailed("SessionState", ex))

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
      case "user" =>
        UserMessage(obj("content") match {
          case ujson.Null => ""
          case str        => str.str
        })
      case "system" =>
        SystemMessage(obj("content") match {
          case ujson.Null => ""
          case str        => str.str
        })
      case "tool" =>
        ToolMessage(
          obj("toolCallId") match {
            case ujson.Null => ""
            case str        => str.str
          },
          obj("content") match {
            case ujson.Null => ""
            case str        => str.str
          }
        )
      case "assistant" =>
        val content = obj("content") match {
          case ujson.Null             => None
          case str if str.str.isEmpty => None
          case str                    => Some(str.str)
        }
        val toolCalls = obj("toolCalls").arr.map { tc =>
          val tcObj = tc.obj
          ToolCall(
            tcObj("id") match {
              case ujson.Null => ""
              case str        => str.str
            },
            tcObj("name") match {
              case ujson.Null => ""
              case str        => str.str
            },
            tcObj("arguments") match {
              case ujson.Null => ujson.Obj()
              case args       => args
            }
          )
        }.toSeq
        AssistantMessage(content, toolCalls)
    }
  }

  /**
   * Loads a session from JSON file by title
   */
  def loadSession(sessionTitle: String, tools: ToolRegistry): Either[AssistantError, SessionState] = {
    val jsonPath = Paths.get(sessionDir, s"${sanitizeFilename(sessionTitle)}.json")

    for {
      _ <- ensureSessionDirectory()
      _ <- Either.cond(Files.exists(jsonPath), (), AssistantError.sessionNotFound(sessionTitle))
      jsonContent <- Try(Files.readString(jsonPath, StandardCharsets.UTF_8)).toEither
        .leftMap(ex => AssistantError.fileReadFailed(jsonPath.toString, ex))
      json <- Try(ujson.read(jsonContent)).toEither
        .leftMap(ex => AssistantError.jsonDeserializationFailed("JSON", ex))
      _ = logger.debug("JSON content keys: {}", json.obj.keySet.mkString(", "))
      state <- Try(jsonToSessionState(json, tools)).toEither
        .leftMap { ex =>
          logger.error("Failed to deserialize SessionState. JSON content preview: {}", jsonContent.take(500))
          AssistantError.jsonDeserializationFailed("SessionState", ex)
        }
    } yield {
      logger.info("Successfully loaded session: {} from {}", sessionTitle, jsonPath)
      state
    }
  }

  /**
   * Lists recent sessions for welcome screen display
   */
  def listRecentSessions(limit: Int = 5): Either[AssistantError, Seq[String]] =
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
      .leftMap(ex => AssistantError.fileReadFailed(sessionDir, ex))

  /**
   * Creates file paths for the session (both JSON and markdown)
   */
  private def createFilePaths(title: Option[String]): Either[AssistantError, (Path, Path)] =
    Try {
      val sessionTitle = title.getOrElse("Untitled Session")
      val baseFilename = sanitizeFilename(sessionTitle)

      // Handle naming collisions by appending numbers
      val uniqueBasename = findUniqueFilename(baseFilename)

      val jsonPath     = Paths.get(sessionDir, s"$uniqueBasename.json")
      val markdownPath = Paths.get(sessionDir, s"$uniqueBasename.md")
      (jsonPath, markdownPath)
    }.toEither.leftMap(ex =>
      AssistantError.FileError(s"Failed to create file paths: ${ex.getMessage}", sessionDir, Some(ex))
    )

  /**
   * Finds a unique filename (by appending numbers if necessary)
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
  ): Either[AssistantError, String] =
    Try {
      val header        = createSessionHeader(title, created, agentState)
      val agentMarkdown = agent.formatStateAsMarkdown(agentState)
      header + agentMarkdown
    }.toEither.leftMap(ex =>
      AssistantError.SerializationError(s"Failed to format session content: ${ex.getMessage}", Some(ex))
    )

  /**
   * Writes session content to file and returns file size
   */
  private def writeSessionFile(filePath: Path, content: String): Either[AssistantError, Long] =
    Try {
      Files.write(
        filePath,
        content.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      Files.size(filePath)
    }.toEither.leftMap(ex => AssistantError.fileWriteFailed(filePath.toString, ex))

  /**
   * Creates session info from the saved session
   */
  private def createSessionInfo(
    state: SessionState,
    title: String,
    filePath: Path,
    agentState: AgentState,
    fileSize: Long
  ): Either[AssistantError, SessionInfo] =
    Try {
      SessionInfo(
        id = state.sessionId,
        title = title,
        filePath = filePath.toString,
        created = state.created,
        messageCount = agentState.conversation.messages.length,
        fileSize = fileSize
      )
    }.toEither.leftMap(ex =>
      AssistantError.SessionError(s"Failed to create session info: ${ex.getMessage}", Some(state.sessionId), Some(ex))
    )

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
