package org.llm4s.samples.assistant

import org.llm4s.assistant.AssistantAgent
import org.llm4s.llmconnect.LLM
import org.llm4s.toolapi.{WorkspaceTools, ToolFunction}
import org.llm4s.toolapi.tools.WeatherTool
import org.llm4s.mcp.{MCPServerConfig, MCPToolRegistry}
import org.llm4s.workspace.ContainerisedWorkspace
import org.slf4j.LoggerFactory
import scala.concurrent.duration._

/**
 * Example demonstrating the interactive AssistantAgent with MCP integration running in a containerized workspace
 * 
 * This example shows how to create an interactive assistant that combines:
 * - Local tools (WeatherTool)
 * - MCP tools (Playwright for browser automation)
 * - Containerized workspace for secure execution
 * - Session management with conversation persistence
 * - Interactive terminal commands
 * 
 * ## Prerequisites:
 * - Set environment variables: OPENAI_API_KEY or ANTHROPIC_API_KEY
 * - Node.js installed (required for Playwright MCP server via npx)
 * - Docker installed and running
 * - Internet connection (for downloading @playwright/mcp)
 * - Built workspace runner image: `sbt docker:publishLocal`
 * 
 * ## How to run:
 * ```bash
 * # First build the workspace runner Docker image
 * sbt docker:publishLocal
 * 
 * # Then run the containerized assistant example
 * sbt "samples/runMain org.llm4s.samples.assistant.ContainerisedAssistantAgentExample"
 * ```
 * 
 * ## Interactive Commands:
 * - `/help` - Show available commands
 * - `/new` - Start a new session (saves current session)
 * - `/save [title]` - Save current session with optional title
 * - `/sessions` - List recent saved sessions
 * - `/quit` - Save session and exit
 * 
 * ## Usage Examples:
 * - "What's the weather in London?" (uses local WeatherTool)
 * - "Navigate to https://example.com and tell me the main heading" (uses Playwright MCP)
 * - "Check the weather in Paris, then visit Wikipedia and search for weather" (combines tools)
 * - "Create a file called test.txt with some content" (uses containerized workspace)
 * - "List files in the current directory" (uses containerized workspace)
 * 
 * Sessions are automatically saved to `./sessions/` directory as markdown files.
 * The containerized workspace provides secure execution environment with file system isolation.
 */
object ContainerisedAssistantAgentExample {
  private val logger = LoggerFactory.getLogger(getClass)
  
  def main(args: Array[String]): Unit = {
    logger.info("Starting LLM4S Containerised Interactive Assistant Agent Example...")
    
    // Sample workspace directory on the host machine
    val workspaceDir = System.getProperty("user.home") + "/assistant-workspace-demo"
    logger.info(s"Using workspace directory: $workspaceDir")
    
    // Create containerised workspace
    val workspace = new ContainerisedWorkspace(workspaceDir)
    
    // Get LLM client from environment variables
    val client = LLM.client()
    
    // Create MCP server configuration for Playwright
    val playwrightServerConfig = MCPServerConfig.stdio(
      name = "playwright-mcp-server",
      command = Seq("npx", "@playwright/mcp@latest"),
      timeout = 60.seconds
    )
    
    // Create workspace tools (will be initialized after container starts)
    var workspaceTools: Seq[ToolFunction[_, _]] = Seq.empty
    var tools: MCPToolRegistry = null
    
    try {
      // Start the workspace container
      if (!workspace.startContainer()) {
        logger.error("Failed to start the workspace container")
        System.exit(1)
      }
      
      logger.info("Container started successfully")
      
      // Create workspace tools after container is started
      workspaceTools = WorkspaceTools.createDefaultWorkspaceTools(workspace)
      
      // Create tool registry combining workspace, local and MCP tools
      tools = new MCPToolRegistry(
        mcpServers = Seq(playwrightServerConfig),
        localTools = Seq(WeatherTool.tool) ++ workspaceTools,
        cacheTTL = 10.minutes
      )
      
      // Create the assistant agent
      val assistant = new AssistantAgent(
        client = client,
        tools = tools,
        sessionDir = "./sessions" // Sessions will be saved here
      )
      
      // Display workspace info
      val workspaceInfo = workspace.getWorkspaceInfo()
      logger.info("Containerized workspace info: root={}", workspaceInfo.root)
      
      // Display initial workspace contents
      val dirContents = workspace.exploreFiles("/workspace")
      logger.info("Initial workspace contents: {}", dirContents.files.map(_.path).mkString(", "))
      
      // Display available tools
      val allTools = tools.getAllTools
      logger.info("Available tools ({} total):", allTools.size)
      allTools.zipWithIndex.foreach { case (tool, index) =>
        val source = tool.name match {
          case "weather" => "local"
          case name if name.startsWith("explore_") || name.startsWith("read_") || 
                       name.startsWith("write_") || name.startsWith("search_") || 
                       name.startsWith("execute_") || name.startsWith("modify_") => "workspace"
          case _ => "MCP"
        }
        logger.info("   {}. {} ({}): {}", index + 1, tool.name, source, tool.description)
      }
      
      // Start the interactive session
      logger.info("Launching interactive assistant session with containerized workspace")
      assistant.startInteractiveSession() match {
        case Right(_) =>
          logger.info("Assistant session completed successfully")
          
        case Left(error) =>
          logger.error("Assistant session failed: {}", error)
          System.exit(1)
      }
      
    } catch {
      case e: Exception =>
        logger.error(s"Error during containerised assistant demo: ${e.getMessage}", e)
        System.exit(1)
    } finally {
      // Clean up MCP connections
      if (tools != null) {
        logger.info("Cleaning up MCP connections...")
        tools.closeMCPClients()
      }
      
      // Always clean up the container
      if (workspace.stopContainer()) {
        logger.info("Container stopped successfully")
      } else {
        logger.error("Failed to stop the container")
      }
    }
  }
}