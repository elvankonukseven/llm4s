package org.llm4s.samples.mcp

import org.llm4s.mcp._
import org.llm4s.toolapi._
import org.llm4s.toolapi.tools._
import org.slf4j.LoggerFactory
import upickle.default._
import scala.concurrent.duration._

/**
 * Example of direct low level interaction with MCP server via LLM4S tool registry.
 * 
 * Start the MCPServer first - see DemonstrationMCPServer documentation for instructions
 * Then run: sbt "samples/runMain org.llm4s.samples.mcp.MCPWeatherExample" 
 */
object MCPWeatherExample {
  private val logger = LoggerFactory.getLogger(getClass)
  
  def main(args: Array[String]): Unit = {
    logger.info("🚀 MCP Weather Example")
    logger.info("📋 Demonstrating Model Context Protocol integration")
    
    // Create MCP server configuration 
    val serverConfig = MCPServerConfig.sse(
      name = "test-server",
      url = "http://localhost:8080",
      timeout = 30.seconds
    )
    
    // Create local weather tool 
    val localWeatherTool = WeatherTool.tool
    
    // Create MCP registry combining local and MCP tools
    val mcpRegistry = new MCPToolRegistry(
      mcpServers = Seq(serverConfig),
      localTools = Seq(localWeatherTool),
      cacheTTL = 5.minutes
    )
    
    // Show all available tools
    val allTools = mcpRegistry.getAllTools
    logger.info(s"📦 Available tools (${allTools.size} total):")
    allTools.zipWithIndex.foreach { case (tool, index) =>
      logger.info(s"   ${index + 1}. ${tool.name}: ${tool.description}")
    }
    
    // Test the tools
    runToolTests(mcpRegistry)
    
    // Clean up
    mcpRegistry.closeMCPClients()
    
    logger.info("✨ Example completed!")
  }
  
  // Run various tool tests to demonstrate functionality
  private def runToolTests(registry: MCPToolRegistry): Unit = {
    logger.info("🧪 Running tool tests:")
    
    // Test 1: Weather (should use local tool)
    logger.info("1️⃣ Testing weather tool:")
    val weatherRequest = ToolCallRequest(
      functionName = "get_weather",
      arguments = ujson.Obj(
        "location" -> ujson.Str("Paris, France"),
        "units" -> ujson.Str("celsius")
      )
    )
    
    registry.execute(weatherRequest) match {
      case Right(result) =>
        logger.info("   ✅ Success:")
        logger.info(s"   ${result.render(indent = 6)}")
      case Left(error) =>
        logger.error(s"   ❌ Failed: $error")
    }
    
    // Test 2: Currency conversion (MCP tool)
    logger.info("2️⃣ Testing currency conversion tool:")
    val currencyRequest = ToolCallRequest(
      functionName = "currency_convert",
      arguments = ujson.Obj(
        "amount" -> ujson.Num(100),
        "from" -> ujson.Str("EUR"),
        "to" -> ujson.Str("USD")
      )
    )
    
    registry.execute(currencyRequest) match {
      case Right(result) =>
        logger.info("   ✅ Success:")
        logger.info(s"   ${result.render(indent = 6)}")
      case Left(error) =>
        logger.error(s"   ❌ Failed: $error")
    }
    
    
    // Test 3: Unknown tool (should fail gracefully)
    logger.info("4️⃣ Testing unknown tool (should fail):")
    val unknownRequest = ToolCallRequest(
      functionName = "nonexistent_tool",
      arguments = ujson.Obj()
    )
    
    registry.execute(unknownRequest) match {
      case Right(_) =>
        logger.warn("   🤔 Unexpected success")
      case Left(error) =>
        logger.info(s"   ✅ Expected failure: $error")
    }
  }
}