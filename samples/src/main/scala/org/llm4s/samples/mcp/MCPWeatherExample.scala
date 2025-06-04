package org.llm4s.samples.mcp

import org.llm4s.mcp._
import org.llm4s.toolapi._
import org.llm4s.toolapi.tools._
import upickle.default._
import scala.concurrent.duration._

/**
 * Example demonstrating MCP integration with the llm4s tool system
 * 
 * This example shows:
 * - How to start and connect to an MCP server
 * - How to discover and use MCP tools
 * - How to combine local and MCP tools in a unified registry
 * 
 * Prerequisites:
 * - Start the (mock) MCPServer first
 */
object MCPWeatherExample {
  
  def main(args: Array[String]): Unit = {
    println("🚀 MCP Weather Example")
    println("📋 Demonstrating Model Context Protocol integration")
    println()
    
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
    println(s"\n📦 Available tools (${allTools.size} total):")
    allTools.zipWithIndex.foreach { case (tool, index) =>
      println(s"   ${index + 1}. ${tool.name}: ${tool.description}")
    }
    
    // Test the tools
    runToolTests(mcpRegistry)
    
    // Clean up
    mcpRegistry.closeMCPClients()
    
    println("✨ Example completed!")
  }
  
  // Run various tool tests to demonstrate functionality
  private def runToolTests(registry: MCPToolRegistry): Unit = {
    println("\n🧪 Running tool tests:")
    
    // Test 1: Weather (should use local tool)
    println("\n1️⃣ Testing weather tool:")
    val weatherRequest = ToolCallRequest(
      functionName = "get_weather",
      arguments = ujson.Obj(
        "location" -> ujson.Str("Paris, France"),
        "units" -> ujson.Str("celsius")
      )
    )
    
    registry.execute(weatherRequest) match {
      case Right(result) =>
        println("   ✅ Success:")
        println(s"   ${result.render(indent = 6)}")
      case Left(error) =>
        println(s"   ❌ Failed: $error")
    }
    
    // Test 2: Currency conversion (MCP tool)
    println("\n2️⃣ Testing currency conversion tool:")
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
        println("   ✅ Success:")
        println(s"   ${result.render(indent = 6)}")
      case Left(error) =>
        println(s"   ❌ Failed: $error")
    }
    
    
    // Test 3: Unknown tool (should fail gracefully)
    println("\n4️⃣ Testing unknown tool (should fail):")
    val unknownRequest = ToolCallRequest(
      functionName = "nonexistent_tool",
      arguments = ujson.Obj()
    )
    
    registry.execute(unknownRequest) match {
      case Right(_) =>
        println("   🤔 Unexpected success")
      case Left(error) =>
        println(s"   ✅ Expected failure: $error")
    }
  }
}