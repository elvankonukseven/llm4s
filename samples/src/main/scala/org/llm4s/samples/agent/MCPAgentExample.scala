package org.llm4s.samples.agent

import org.llm4s.agent.Agent
import org.llm4s.llmconnect.LLM
import org.llm4s.mcp._
import org.llm4s.toolapi.tools.WeatherTool
import org.slf4j.LoggerFactory
import scala.concurrent.duration._

/**
 * Example demonstrating agent execution with MCP tool integration
 *
 * This example shows how an LLM agent can seamlessly use both local tools 
 * and remote MCP tools to complete complex multi-step tasks.
 *
 * Start the MCPServer first - see DemonstrationMCPServer documentation for instructions
 * Then run: sbt "samples/runMain org.llm4s.samples.agent.MCPAgentExample"
 */
object MCPAgentExample {
  private val logger = LoggerFactory.getLogger(getClass)
  
  def main(args: Array[String]): Unit = {
    logger.info("🚀 MCP Agent Example")
    logger.info("📋 Demonstrating agent execution with MCP integration")
    
    // Get LLM client
    val client = LLM.client()
    
    // Create MCP server configuration
    val serverConfig = MCPServerConfig.sse(
      name = "test-mcp-server", 
      url = "http://localhost:8080",
      timeout = 30.seconds
    )
    
    // Set up tool registry with local and MCP tools
    logger.info("🔧 Setting up tool registry...")
    val localWeatherTool = WeatherTool.tool
    
    val mcpRegistry = new MCPToolRegistry(
      mcpServers = Seq(serverConfig),
      localTools = Seq(localWeatherTool),
      cacheTTL = 10.minutes
    )
    
    // Show available tools
    val allTools = mcpRegistry.getAllTools
    logger.info(s"📦 Available tools (${allTools.size} total):")
    allTools.zipWithIndex.foreach { case (tool, index) =>
      logger.info(s"   ${index + 1}. ${tool.name}: ${tool.description}")
    }
    
    // Create and run agent
    runAgentQueries(client, mcpRegistry)
    
    mcpRegistry.closeMCPClients()
    
    logger.info("✨ Agent example completed!")
  }
  
  // Run multiple agent queries to test different capabilities
  private def runAgentQueries(client: org.llm4s.llmconnect.LLMClient, registry: MCPToolRegistry): Unit = {
    val agent = new Agent(client)
    
    // Define test queries
    val queries = Seq(
      "What's the weather like in Tokyo, and then convert 100 USD to EUR?",
      "Could convert 43 british pounds into american dollars ? Also give me the exchange rate please ",
      "Could you convert 50 turkish lira to euro ?"
    )
    
    // Execute each query with full tracing
    queries.zipWithIndex.foreach { case (query, index) =>
      val queryNum = index + 1
      val traceFile = s"mcp-agent-query-$queryNum.md"
      
      logger.info(s"${"=" * 60}")
      logger.info(s"🎯 Query $queryNum: $query")
      logger.info(s"📝 Trace: $traceFile")
      logger.info(s"${"=" * 60}")
      
      // Run the agent
      agent.run(
        query = query,
        tools = registry,
        maxSteps = Some(8),
        traceLogPath = Some(traceFile)
      ) match {
        case Right(finalState) =>
          logger.info(s"✅ Query $queryNum completed: ${finalState.status}")
          
          // Show final answer
          finalState.conversation.messages.reverse.find(_.role == "assistant") match {
            case Some(msg) =>
              logger.info("💬 Final Answer:")
              logger.info(s"${msg.content}")
            case None => 
              logger.warn("❌ No final answer found")
          }
          
          // Show execution summary
          logger.info(s"📊 Summary:")
          logger.info(s"   Steps: ${finalState.logs.size}")
          logger.info(s"   Messages: ${finalState.conversation.messages.size}")
          
          // Show tools used
          val toolCalls = finalState.logs.filter(_.contains("[tool]"))
          if (toolCalls.nonEmpty) {
            logger.info("   Tools used:")
            toolCalls.foreach { log =>
              val toolName = log.split("\\s+").drop(1).headOption.getOrElse("unknown")
              logger.info(s"     - $toolName")
            }
          }
          
        case Left(error) =>
          logger.error(s"❌ Query $queryNum failed: $error")
      }
      
      // Pause between queries
      if (queryNum < queries.size) {
        Thread.sleep(1000)
      }
    }
    
  }
  
}