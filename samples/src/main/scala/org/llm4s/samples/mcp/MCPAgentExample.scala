package org.llm4s.samples.mcp

import org.llm4s.agent.Agent
import org.llm4s.llmconnect.LLM
import org.llm4s.mcp._
import org.llm4s.toolapi.tools.WeatherTool
import scala.concurrent.duration._

/**
 * Example demonstrating agent execution with MCP tool integration
 *
 * This example shows how an LLM agent can seamlessly use both local tools 
 * and remote MCP tools to complete complex multi-step tasks.
 *
 * Prerequisite:
 * - Start the MCPServer first
 */
object MCPAgentExample {
  
  def main(args: Array[String]): Unit = {
    println("🚀 MCP Agent Example")
    println("📋 Demonstrating agent execution with MCP integration")
    println()
    
    // Get LLM client
    val client = LLM.client()
    
    // Create MCP server configuration
    val serverConfig = MCPServerConfig.sse(
      name = "test-mcp-server", 
      url = "http://localhost:8080",
      timeout = 30.seconds
    )
    
    // Set up tool registry with local and MCP tools
    println("🔧 Setting up tool registry...")
    val localWeatherTool = WeatherTool.tool
    
    val mcpRegistry = new MCPToolRegistry(
      mcpServers = Seq(serverConfig),
      localTools = Seq(localWeatherTool),
      cacheTTL = 10.minutes
    )
    
    // Show available tools
    val allTools = mcpRegistry.getAllTools
    println(s"\n📦 Available tools (${allTools.size} total):")
    allTools.zipWithIndex.foreach { case (tool, index) =>
      println(s"   ${index + 1}. ${tool.name}: ${tool.description}")
    }
    
    // Create and run agent
    runAgentQueries(client, mcpRegistry)
    
    mcpRegistry.closeMCPClients()
    
    println("✨ Agent example completed!")
  }
  
  // Run multiple agent queries to test different capabilities
  private def runAgentQueries(client: org.llm4s.llmconnect.LLMClient, registry: MCPToolRegistry): Unit = {
    val agent = new Agent(client)
    
    // Define test queries
    val queries = Seq(
      "What's the weather like in Tokyo, and then convert 100 USD to EUR?",
      "Could convert 43 british pounds into american dollars ? Also give me the exchange rate please "
    )
    
    // Execute each query with full tracing
    queries.zipWithIndex.foreach { case (query, index) =>
      val queryNum = index + 1
      val traceFile = s"mcp-agent-query-$queryNum.md"
      
      println(s"\n${"=" * 60}")
      println(s"🎯 Query $queryNum: $query")
      println(s"📝 Trace: $traceFile")
      println(s"${"=" * 60}")
      
      // Run the agent
      agent.run(
        query = query,
        tools = registry,
        maxSteps = Some(8),
        traceLogPath = Some(traceFile)
      ) match {
        case Right(finalState) =>
          println(s"\n✅ Query $queryNum completed: ${finalState.status}")
          
          // Show final answer
          finalState.conversation.messages.reverse.find(_.role == "assistant") match {
            case Some(msg) =>
              println("\n💬 Final Answer:")
              println(s"${msg.content}")
            case None => 
              println("❌ No final answer found")
          }
          
          // Show execution summary
          println(s"\n📊 Summary:")
          println(s"   Steps: ${finalState.logs.size}")
          println(s"   Messages: ${finalState.conversation.messages.size}")
          
          // Show tools used
          val toolCalls = finalState.logs.filter(_.contains("[tool]"))
          if (toolCalls.nonEmpty) {
            println("   Tools used:")
            toolCalls.foreach { log =>
              val toolName = log.split("\\s+").drop(1).headOption.getOrElse("unknown")
              println(s"     - $toolName")
            }
          }
          
        case Left(error) =>
          println(s"❌ Query $queryNum failed: $error")
      }
      
      // Pause between queries
      if (queryNum < queries.size) {
        Thread.sleep(1000)
      }
    }
    
  }
  
}