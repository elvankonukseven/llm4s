package org.llm4s.samples.mcp

import org.llm4s.agent.{ Agent, AgentState }
import org.llm4s.llmconnect.LLM
import org.llm4s.llmconnect.model.{ Conversation, SystemMessage, UserMessage }
import org.llm4s.mcp._
import org.slf4j.LoggerFactory
import scala.concurrent.duration._
import org.llm4s.llmconnect.LLMClient

/**
 * This example shows how an LLM agent can use the Playwright MCP server
 * to perform browser automation tasks like navigation, clicking, and data extraction.
 *
 * Prerequisites:
 * 1. Node.js installed (required for npx)
 * 2. Internet connection - The example automatically downloads @playwright/mcp using npx
 *
 * The example will automatically launch the Playwright MCP server using npx.
 * In order for the example to complete successfully, do not interfere with the agent's navigation.
 * Run with: sbt "samples/runMain org.llm4s.samples.mcp.PlaywrightExample"
 *
 * ## Output:
 * - Console logs showing agent execution
 * - Trace files: playwright-agent-query-*.md with detailed tool usage
 */

object PlaywrightExample {
  private val logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    logger.info("🚀 Playwright MCP Agent Example")
    logger.info("🌐 Demonstrating browser automation with MCP integration")

    try
      // Validate prerequisites
      validatePrerequisites().fold(
        error => {
          logger.error("❌ Prerequisites not met: {}", error)
          logger.info("Please ensure Node.js is installed and accessible via PATH")
        },
        client => {
          logger.info("✅ Prerequisites validated and LLM client initialized")

          // Create MCP server configuration for Playwright
          // Using stdio transport to launch and communicate with Playwright MCP server
          val serverConfig = MCPServerConfig.stdio(
            name = "playwright-mcp-server",
            command = Seq("npx", "@playwright/mcp@latest"),
            timeout = 60.seconds
          )

          // Set up tool registry with Playwright MCP tools
          logger.info("🔧 Setting up tool registry with Playwright...")
          logger.info("   This may take a moment while downloading @playwright/mcp...")

          val mcpRegistry = new MCPToolRegistry(
            mcpServers = Seq(serverConfig),
            localTools = Seq.empty, // No local tools, using only Playwright MCP tools
            cacheTTL = 10.minutes
          )

          // Show available tools
          val allTools = mcpRegistry.getAllTools
          if (allTools.isEmpty) {
            logger.error("❌ No tools available from Playwright MCP server")
            logger.info("This could indicate:")
            logger.info("  1. The MCP server failed to start")
            logger.info("  2. Network issues preventing package download")
            logger.info("  3. Node.js or npx not properly installed")
            return
          }

          logger.info("📦 Available Playwright tools ({} total):", allTools.size)
          allTools.zipWithIndex.foreach { case (tool, index) =>
            logger.info("   {}. {}: {}", index + 1, tool.name, tool.description)
          }

          // Create and run agent
          runBrowserAutomationQueries(client, mcpRegistry)

          // Clean up
          logger.info("🧹 Cleaning up MCP connections...")
          mcpRegistry.closeMCPClients()

          logger.info("✨ Browser automation example completed successfully!")
        }
      )
    catch {
      case e: Exception =>
        logger.error("💥 Unexpected error: {}", e.getMessage, e)
        logger.info("Please check the logs above for more details")
    }
  }

  // Validate that prerequisites are installed
  private def checkCommand(cmd: String): Either[String, Unit] =
    try {
      val process = new ProcessBuilder(cmd, "--version").start()
      val exited  = process.waitFor(5, java.util.concurrent.TimeUnit.SECONDS)

      if (!exited) Left(s"$cmd process did not complete in time")
      else if (process.exitValue() != 0) Left(s"$cmd is not installed or not accessible")
      else Right(())
    } catch {
      case e: Exception => Left(s"Error running $cmd: ${e.getMessage}")
    }

  private def validatePrerequisites(): Either[String, LLMClient] =
    for {
      _ <- checkCommand("node")
      _ <- checkCommand("npx")
    } yield LLM.client()

  // Run multiple browser automation queries to test different capabilities
  private def runBrowserAutomationQueries(client: org.llm4s.llmconnect.LLMClient, registry: MCPToolRegistry): Unit = {
    val agent = new Agent(client)

    // Define browser automation test queries
    // These are designed to test common web automation tasks with Playwright MCP
    val queries = Seq(
      "Navigate to https://example.com and tell me what the main heading says",
      "Go to https://httpbin.org/get and extract the information about the request that was made",
      "Visit https://www.wikipedia.org and find the search box, then tell me what placeholder text it has",
      "Navigate to https://github.com and identify what navigation menu items are available in the header"
    )

    // Execute each query with full tracing
    queries.zipWithIndex.foreach { case (query, index) =>
      val queryNum  = index + 1
      val traceFile = s"playwright-agent-query-$queryNum.md"

      logger.info("=" * 60)
      logger.info("🎯 Query {}: {}", queryNum, query)
      logger.info("📝 Trace: {}", traceFile)
      logger.info("=" * 60)

      // Run the agent with comprehensive error handling
      logger.info("🤖 Starting agent execution...")

      val systemPrompt = Some(
        """You are a browser automation assistant using Playwright. 
        |Your task is to help users navigate websites and extract information.
        |Always be specific about what you find on the page and provide clear, detailed responses.
        |If you encounter any issues, explain what happened and suggest alternatives.""".stripMargin
      )

      // Create custom initial state with the browser automation system prompt
      val initialMessages = Seq(
        SystemMessage(systemPrompt.getOrElse("You are a helpful assistant with access to tools.")),
        UserMessage(query)
      )
      val initialState = AgentState(
        conversation = Conversation(initialMessages),
        tools = registry,
        userQuery = query
      )

      // Use the agent's runUntilCompletion method directly with our custom state
      runAgentWithCustomPrompt(agent, initialState, Some(15), Some(traceFile)) match {
        case Right(finalState) =>
          logger.info("✅ Query {} completed: {}", queryNum, finalState.status)

          // Show final answer
          finalState.conversation.messages.reverse
            .find(_.role == "assistant")
            .fold {
              logger.warn("❌ No final answer found")
            } { msg =>
              logger.info("💬 Final Answer:")
              logger.info(msg.content)
            }

          // Show execution summary
          logger.info("📊 Summary:")
          logger.info("   Status: {}", finalState.status)
          logger.info("   Steps: {}", finalState.logs.size)
          logger.info("   Messages: {}", finalState.conversation.messages.size)
          logger.info("   For detailed tool usage, see trace file: {}", traceFile)

        case Left(err) =>
          logger.error("❌ Query {} failed: {}", queryNum, err)

          // Try to provide helpful debugging information
          if (err.toString.contains("No tools available")) {
            logger.info("💡 Tip: This could mean the MCP server isn't responding properly")
          } else if (err.toString.contains("timeout")) {
            logger.info("💡 Tip: The browser operation might be taking too long - try a simpler query")
          } else if (err.toString.contains("connection")) {
            logger.info("💡 Tip: Check your internet connection and that the MCP server is running")
          }
      }

      // Pause between queries to avoid overwhelming the browser
      if (queryNum < queries.size) {
        logger.info("⏳ Waiting before next query...")
        Thread.sleep(3000)
      }
    }

  }

  // Helper method to run agent with custom initial state
  private def runAgentWithCustomPrompt(
    agent: Agent,
    initialState: AgentState,
    maxSteps: Option[Int],
    traceLogPath: Option[String]
  ) = {
    import scala.annotation.tailrec
    import org.llm4s.llmconnect.model.LLMError

    // Write initial state if tracing is enabled
    traceLogPath.foreach(path => agent.writeTraceLog(initialState, path))

    @tailrec
    def runUntilCompletion(state: AgentState, stepsRemaining: Option[Int] = maxSteps): Either[LLMError, AgentState] =
      (state.status, stepsRemaining) match {
        case (s, Some(0))
            if s == org.llm4s.agent.AgentStatus.InProgress || s == org.llm4s.agent.AgentStatus.WaitingForTools =>
          val updatedState = state
            .log("[system] Step limit reached")
            .withStatus(org.llm4s.agent.AgentStatus.Failed("Maximum step limit reached"))
          traceLogPath.foreach(path => agent.writeTraceLog(updatedState, path))
          Right(updatedState)

        case (org.llm4s.agent.AgentStatus.InProgress | org.llm4s.agent.AgentStatus.WaitingForTools, _) =>
          agent.runStep(state) match {
            case Right(newState) =>
              traceLogPath.foreach(path => agent.writeTraceLog(newState, path))
              runUntilCompletion(newState, stepsRemaining.map(_ - 1))
            case Left(error) =>
              val failedState = state.withStatus(org.llm4s.agent.AgentStatus.Failed(error.toString))
              traceLogPath.foreach(path => agent.writeTraceLog(failedState, path))
              Left(error)
          }

        case (org.llm4s.agent.AgentStatus.Complete, _) | (org.llm4s.agent.AgentStatus.Failed(_), _) =>
          Right(state)
      }

    runUntilCompletion(initialState)
  }

}
