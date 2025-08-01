package org.llm4s.assistant

import org.llm4s.agent.{ Agent, AgentState, AgentStatus }
import org.llm4s.llmconnect.LLMClient
import org.llm4s.llmconnect.model._
import org.llm4s.toolapi.ToolRegistry

import java.util.UUID
import org.slf4j.LoggerFactory

/**
 * Interactive assistant agent that wraps the existing Agent functionality
 * in a user-friendly conversational loop with session management using functional programming principles.
 */
class AssistantAgent(
  client: LLMClient,
  tools: ToolRegistry,
  sessionDir: String = "./sessions"
) {
  private val logger         = LoggerFactory.getLogger(getClass)
  private val agent          = new Agent(client)
  private val sessionManager = new SessionManager(sessionDir, agent)
  private val console        = new ConsoleInterface(tools)

  /**
   * Starts the interactive session loop
   */
  def startInteractiveSession(): Either[String, Unit] = {
    logger.info("Starting interactive assistant session")
    for {
      _ <- console.showWelcome()
      initialState = SessionState(None, UUID.randomUUID().toString, sessionDir)
      _            = logger.info("Created new session: {}", initialState.sessionId)
      _ <- runInteractiveLoop(initialState)
    } yield logger.info("Interactive assistant session ended")
  }

  /**
   * Main interactive loop that processes user input until quit
   */
  private def runInteractiveLoop(initialState: SessionState): Either[String, Unit] = {
    def loop(state: SessionState): Either[String, Unit] =
      console.promptUser().flatMap { input =>
        processInput(input.trim, state).fold(
          error =>
            // Continue on error
            console.displayError(error).flatMap(_ => loop(state)),
          { case (newState, response) =>
            // Display response if not empty
            val displayResult = if (response.nonEmpty) {
              console.displayMessage(response, MessageType.Info)
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
  private def processInput(input: String, state: SessionState): Either[String, (SessionState, String)] =
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
  private def processQuery(query: String, state: SessionState): Either[String, (SessionState, String)] = {
    logger.debug("Processing user query: {}", query.take(100))
    for {
      updatedState <- addUserMessage(query, state)
      finalState   <- runAgentToCompletion(updatedState)
      _            <- sessionManager.saveSession(finalState)
      response     <- extractFinalResponse(finalState)
    } yield {
      logger.debug("Successfully processed query, response length: {}", response.length)
      (finalState, formatAssistantResponse(response))
    }
  }

  /**
   * Handles slash commands
   */
  private def handleCommand(command: String, state: SessionState): Either[String, (SessionState, String)] =
    command.toLowerCase.split("\\s+").toList match {
      case "/help" :: _ =>
        Right((state, console.showHelp()))

      case "/new" :: _ =>
        for {
          _ <- sessionManager.saveSession(state)
          newState = state.withNewSession()
        } yield (newState, "Started new session. Previous session saved.")

      case "/save" :: title =>
        val sessionTitle = if (title.nonEmpty) title.mkString(" ") else "Saved Session"
        sessionManager.saveSession(state, Some(sessionTitle)).map(_ => (state, s"Session saved as: $sessionTitle"))

      case "/sessions" :: _ =>
        sessionManager.listSessions().map(sessions => (state, console.formatSessionList(sessions)))

      case "/quit" :: _ =>
        sessionManager.saveSession(state).map(_ => (state, "Session saved. Goodbye!"))

      case _ =>
        Right((state, s"Unknown command: $command. Type /help for available commands."))
    }

  /**
   * Adds user message to the conversation - initializes if first message
   */
  private def addUserMessage(query: String, state: SessionState): Either[String, SessionState] =
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
  private def runAgentToCompletion(state: SessionState): Either[String, SessionState] =
    state.agentState match {
      case None => Left("No agent state to run")
      case Some(agentState) =>
        def runSteps(currentState: AgentState): Either[String, AgentState] =
          currentState.status match {
            case AgentStatus.InProgress | AgentStatus.WaitingForTools =>
              agent.runStep(currentState) match {
                case Right(newState) => runSteps(newState)
                case Left(error)     => Left(error.toString)
              }
            case _ => Right(currentState)
          }

        runSteps(agentState).map(finalAgentState => state.withAgentState(finalAgentState))
    }

  /**
   * Extracts the final response from the agent state
   */
  private def extractFinalResponse(state: SessionState): Either[String, String] =
    state.agentState match {
      case None => Left("No agent state available")
      case Some(agentState) =>
        agentState.conversation.messages.reverse.collectFirst {
          case msg: AssistantMessage if msg.toolCalls.isEmpty => msg.content
        } match {
          case Some(content) => Right(content)
          case None          => Left("No final response found from assistant")
        }
    }

  /**
   * Formats the assistant's response for display
   */
  private def formatAssistantResponse(response: String): String =
    s"🤖 Assistant: $response"
}
