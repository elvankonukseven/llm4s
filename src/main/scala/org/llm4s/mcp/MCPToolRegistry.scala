package org.llm4s.mcp

import org.llm4s.toolapi._
import upickle.default._
import ujson._
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}

// MCP-aware tool registry that integrates with the existing tool API
class MCPToolRegistry(
  mcpServers: Seq[MCPServerConfig],
  localTools: Seq[ToolFunction[_, _]] = Seq.empty,
  cacheTTL: Duration = 10.minutes 
) extends ToolRegistry(localTools) {

  private var mcpClients: Map[String, MCPClient] = Map.empty
  private var toolCache: Map[String, CachedTools] = Map.empty 

  // Override to return ALL tools (local + MCP) 
  override def tools: Seq[ToolFunction[_, _]] = getAllTools

  // Execute tools, trying local first then MCP
  override def execute(request: ToolCallRequest): Either[ToolCallError, ujson.Value] = {
    // Try local tools first
    super.execute(request) match {
      case Right(result) => Right(result)
      case Left(_) => 
        // If local tools fail, try MCP tools
        findMCPTool(request.functionName) match {
          case Some(tool) =>
            try {
              tool.execute(request.arguments)
            } catch {
              case e: Exception => Left(ToolCallError.ExecutionError(e))
            }
          case None => Left(ToolCallError.UnknownFunction(request.functionName))
        }
    }
  }

  // Get tools in OpenAI format
  override def getOpenAITools(strict: Boolean = true): ujson.Arr = {
    ujson.Arr.from(tools.map(_.toOpenAITool(strict)))
  }

  // Get all tools (local + MCP)
  def getAllTools: Seq[ToolFunction[_, _]] = {
    localTools ++ getAllMCPTools
  }

  // Find a specific MCP tool by name
  private def findMCPTool(name: String): Option[ToolFunction[_, _]] = {
    mcpServers.view
      .flatMap(server => getToolsFromServer(server).find(_.name == name))
      .headOption
  }

  // Get all MCP tools from all servers
  private def getAllMCPTools: Seq[ToolFunction[_, _]] = {
    mcpServers.flatMap(getToolsFromServer)
  }

  // Get tools from a specific server (with caching)
  private def getToolsFromServer(server: MCPServerConfig): Seq[ToolFunction[_, _]] = {
    val now = System.currentTimeMillis()
    toolCache.get(server.name) match {
      case Some(cached) if !cached.isExpired(now, cacheTTL) =>
        cached.tools
      case _ =>
        refreshToolsFromServer(server, now)
    }
  }

  // Refresh tools from server and update cache
  private def refreshToolsFromServer(server: MCPServerConfig, timestamp: Long): Seq[ToolFunction[_, _]] = {
    Try {
      val client = getOrCreateClient(server)
      val tools = client.getTools()
      toolCache = toolCache + (server.name -> CachedTools(tools, timestamp))
      tools
    } match {
      case Success(tools) => tools
      case Failure(exception) =>
        System.err.println(s"Failed to refresh tools from ${server.name}: ${exception.getMessage}")
        Seq.empty
    }
  }

  // Get or create MCP client for a server
  private def getOrCreateClient(server: MCPServerConfig): MCPClient = {
    mcpClients.getOrElse(
      server.name, {
        val client = new MCPClientImpl(server)
        mcpClients = mcpClients + (server.name -> client)
        client
      }
    )
  }

  // Utility methods for cache management
  def clearCache(): Unit = {
    toolCache = Map.empty
  }

  // Refresh cache for all servers
  def refreshCache(): Unit = {
    val now = System.currentTimeMillis()
    mcpServers.foreach(server => refreshToolsFromServer(server, now))
  }

  // Close all MCP clients
  def closeMCPClients(): Unit = {
    mcpClients.values.foreach(_.close())
    mcpClients = Map.empty
  }

  // Initialize MCP tools after all methods are defined
  private def initializeMCPTools(): Unit = {
    mcpServers.foreach(server => refreshToolsFromServer(server, System.currentTimeMillis()))
  }
  initializeMCPTools()
}

// Helper case class for cleaner caching
private case class CachedTools(tools: Seq[ToolFunction[_, _]], timestamp: Long) {
  def isExpired(now: Long, ttl: Duration): Boolean = {
    (now - timestamp) >= ttl.toMillis
  }
}