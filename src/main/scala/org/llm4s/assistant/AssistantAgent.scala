package org.llm4s.assistant

import org.llm4s.agent.{ Agent, AgentState, AgentStatus }
import org.llm4s.llmconnect.LLMClient
import org.llm4s.llmconnect.model._
import org.llm4s.error.{ AssistantError, ErrorBridge, LLMError }
import org.llm4s.toolapi.ToolRegistry
import org.llm4s.types.{ SessionId, DirectoryPath, FilePath }
import cats.implicits._

import java.util.UUID
import org.slf4j.LoggerFactory

/**
 * Interactive assistant agent that wraps the existing Agent functionality
 * in a user-friendly conversational loop with session management using functional programming principles.
 */
class AssistantAgent(
  client: LLMClient,
  tools: ToolRegistry,
  sessionDir: String = "./sessions",
  consoleConfig: ConsoleConfig = ConsoleConfig()
) {
  private val logger         = LoggerFactory.getLogger(getClass)
  private val agent          = new Agent(client)
  private val sessionManager = new SessionManager(DirectoryPath(sessionDir), agent)
  private val console        = new ConsoleInterface(tools, sessionManager, consoleConfig)

  /**
   * Starts the interactive session loop
   */
  def startInteractiveSession(): Either[AssistantError, Unit] = {
    logger.info("Starting interactive assistant session")
    for {
      _ <- console.showWelcome()
      initialState = SessionState(None, SessionId(UUID.randomUUID().toString), DirectoryPath(sessionDir))
      _            = logger.info("Created new session: {}", initialState.sessionId)
      _ <- runInteractiveLoop(initialState)
    } yield logger.info("Interactive assistant session ended")
  }

  /**
   * Main interactive loop that processes user input until quit
   */
  private def runInteractiveLoop(initialState: SessionState): Either[AssistantError, Unit] = {
    def loop(state: SessionState): Either[AssistantError, Unit] =
      console.promptUser().flatMap { input =>
        processInput(input.trim, state).fold(
          error =>
            // Continue on error
            console.displayError(error.message).flatMap(_ => loop(state)),
          { case (newState, response) =>
            // Display response if not empty
            val displayResult = if (response.nonEmpty) {
              console.displayMessage(response, MessageType.AssistantResponse)
            } else {
              Right(())
            }

            displayResult.flatMap { _ =>
              // Check if user wants to quit
              if (input.trim.toLowerCase == "/quit") {
                Right(())
              } else {
                loop(newState)
              }
            }
          }
        )
      }

    loop(initialState)
  }

  /**
   * Processes user input - either a command or a query for the agent
   */
  private def processInput(input: String, state: SessionState): Either[AssistantError, (SessionState, String)] =
    if (input.startsWith("/")) {
      handleCommand(input, state)
    } else if (input.nonEmpty) {
      processQuery(input, state)
    } else {
      Right((state, ""))
    }

  /**
   * Processes a user query through the agent
   */
  private def processQuery(query: String, state: SessionState): Either[AssistantError, (SessionState, String)] = {
    logger.debug("Processing user query: {}", query.take(100))
    for {
      updatedState <- addUserMessage(query, state)
      finalState <- runAgentToCompletion(updatedState).leftMap(llmError =>
        AssistantError.SessionError(s"Agent execution failed: ${llmError.message}", state.sessionId, "agent-execution")
      )
      response <- extractFinalResponse(finalState)
    } yield {
      logger.debug("Successfully processed query, response length: {}", response.length)
      (finalState, formatAssistantResponse(response))
    }
  }

  /**
   * Handles slash commands
   */
  private def handleCommand(command: String, state: SessionState): Either[AssistantError, (SessionState, String)] =
    command.toLowerCase.split("\\s+").toList match {
      case "/help" :: _ =>
        Right((state, console.showHelp()))

      case "/new" :: _ =>
        state.agentState match {
          case Some(agentState) if agentState.conversation.messages.nonEmpty =>
            // Prompt user for session name
            for {
              sessionName <- promptForSessionName(
                "Enter a name for the current session (or press Enter for 'Untitled Session'): "
              )
              title = if (sessionName.trim.nonEmpty) sessionName.trim else "Untitled Session"
              _ <- sessionManager.saveSession(state, Some(title))
              newState = state.withNewSession()
            } yield (newState, s"Previous session saved as '$title'. Started new session.")
          case _ =>
            // No session to save
            val newState = state.withNewSession()
            Right((newState, "Started new session."))
        }

      case "/save" :: title =>
        val sessionTitle = if (title.nonEmpty) title.mkString(" ") else "Saved Session"
        sessionManager.saveSession(state, Some(sessionTitle)).map(_ => (state, s"Session saved as: $sessionTitle"))

      case "/load" :: sessionTitle =>
        val title = sessionTitle.mkString(" ").trim.stripPrefix("\"").stripSuffix("\"")
        if (title.nonEmpty) {
          for {
            // Auto-save current session if it has content before loading new one
            _ <- state.agentState match {
              case Some(agentState) if agentState.conversation.messages.nonEmpty =>
                sessionManager.saveSession(state, Some("Auto-saved Session"))
              case _ =>
                Right(SessionInfo(SessionId(""), "No session to save", FilePath(""), state.created, 0, 0L))
            }
            loadedState <- sessionManager.loadSession(title, tools)
            messageCount = loadedState.agentState.map(_.conversation.messages.length).getOrElse(0)
          } yield (loadedState, s"✅ Session '$title' restored - $messageCount messages loaded")
        } else {
          Right((state, "Please specify a session title: /load \"Session Name\""))
        }

      case "/sessions" :: _ =>
        sessionManager.listRecentSessions().map(sessions => (state, console.formatSessionList(sessions)))

      case "/quit" :: _ =>
        state.agentState match {
          case Some(agentState) if agentState.conversation.messages.nonEmpty =>
            // Prompt user for session name before quitting
            for {
              sessionName <- promptForSessionName(
                "Enter a name for the current session (or press Enter for 'Untitled Session'): "
              )
              title = if (sessionName.trim.nonEmpty) sessionName.trim else "Untitled Session"
              _ <- sessionManager.saveSession(state, Some(title))
            } yield (state, s"Session saved as '$title'. Goodbye!")
          case _ =>
            // No session to save
            Right((state, "Goodbye!"))
        }

      case _ =>
        Right((state, s"Unknown command: $command. Type /help for available commands."))
    }

  /**
   * Adds user message to the conversation - initializes if first message
   */
  private def addUserMessage(query: String, state: SessionState): Either[AssistantError, SessionState] =
    state.agentState match {
      case None =>
        // First message - initialize the agent
        val initialState = agent.initialize(query, tools)
        Right(state.withAgentState(initialState))

      case Some(agentState) =>
        // Add to existing conversation
        val updatedAgentState = agentState
          .addMessage(UserMessage(query))
          .withStatus(AgentStatus.InProgress)
        Right(state.withAgentState(updatedAgentState))
    }

  /**
   * Runs the agent until completion or failure
   */
  private def runAgentToCompletion(state: SessionState): Either[LLMError, SessionState] =
    state.agentState match {
      case None => Left(LLMError.ConfigurationError("No agent state to run"))
      case Some(agentState) =>
        def runSteps(currentState: AgentState): Either[LLMError, AgentState] =
          currentState.status match {
            case AgentStatus.InProgress | AgentStatus.WaitingForTools =>
              agent.runStep(currentState) match {
                case Right(newState) => runSteps(newState)
                case Left(error)     => Left(ErrorBridge.toCore(error))
              }
            case _ => Right(currentState)
          }

        runSteps(agentState).map(finalAgentState => state.withAgentState(finalAgentState))
    }

  /**
   * Extracts the final response from the agent state
   */
  private def extractFinalResponse(state: SessionState): Either[AssistantError, String] =
    state.agentState match {
      case None => Left(AssistantError.SessionError("No agent state available", state.sessionId, "extract-response"))
      case Some(agentState) =>
        agentState.conversation.messages.reverse.collectFirst {
          case msg: AssistantMessage if msg.toolCalls.isEmpty => msg.content
        } match {
          case Some(content) => Right(content)
          case None =>
            Left(
              AssistantError.SessionError("No final response found from assistant", state.sessionId, "extract-response")
            )
        }
    }

  /**
   * Prompts user for a session name
   */
  private def promptForSessionName(prompt: String): Either[AssistantError, String] =
    console.promptForInput(prompt)

  /**
   * Formats the assistant's response for display
   */
  private def formatAssistantResponse(response: String): String =
    response // The ConsoleInterface handles assistant formatting via MessageType.AssistantResponse
}
