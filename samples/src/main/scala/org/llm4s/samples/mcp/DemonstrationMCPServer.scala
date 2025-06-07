package org.llm4s.samples.mcp

import com.sun.net.httpserver.{HttpServer, HttpHandler, HttpExchange}
import java.net.InetSocketAddress
import java.io.OutputStream
import org.slf4j.LoggerFactory
import upickle.default.{read => upickleRead, write => upickleWrite}
import org.llm4s.mcp._
import ujson.{Obj, Str, Num, Arr}
import scala.util.{Try, Success, Failure}

/**
 * A Sample MCP Server for testing.
 * This server implements SSE interface in line with MCP Spec (2024-11-05) that follows JSON-RPC 2.0 protocol.
 * 
 * Runs on port 8080
 * To run: sbt "runMain org.llm4s.samples.mcp.DemonstrationMCPServer"
 */
object DemonstrationMCPServer {
  private val logger = LoggerFactory.getLogger(getClass)
  
  def main(args: Array[String]): Unit = {
    val port = 8080
    val server = HttpServer.create(new InetSocketAddress(port), 0)
    
    logger.info(s"🚀 Starting Proper MCP Server on http://localhost:$port")
    logger.info("Protocol: JSON-RPC 2.0 over HTTP")
    logger.info("Available tools: get_weather, currency_convert")
    
    // Handle MCP endpoint for JSON-RPC communication
    server.createContext("/sse", new MCPHandler())
    
    server.setExecutor(null)
    server.start()
    
    logger.info("✨ MCP Server ready!")
    
    // Keep server running
    Thread.currentThread().join()
  }
  
  class MCPHandler extends HttpHandler {
    override def handle(exchange: HttpExchange): Unit = {
      if (exchange.getRequestMethod == "POST") {
        Try {
          // Read JSON-RPC request
          val requestBody = scala.io.Source.fromInputStream(exchange.getRequestBody).mkString
          val jsonRpcRequest = upickleRead[JsonRpcRequest](requestBody)
          
          logger.debug(s"📨 Received JSON-RPC request: ${jsonRpcRequest.method} (id: ${jsonRpcRequest.id})")
          
          // Handle the JSON-RPC method
          val response = handleJsonRpcMethod(jsonRpcRequest)
          
          // Send JSON-RPC response
          sendJsonRpcResponse(exchange, response)
          
        } match {
          case Success(_) => // Request handled successfully
          case Failure(exception) =>
            logger.error(s"❌ Error handling request: ${exception.getMessage}", exception)
            val errorResponse = JsonRpcResponse(
              id = "unknown",
              error = Some(JsonRpcError(-32700, "Parse error", None))
            )
            sendJsonRpcResponse(exchange, errorResponse)
        }
      } else {
        sendResponse(exchange, 405, "Method not allowed")
      }
    }
    
    private def handleJsonRpcMethod(request: JsonRpcRequest): JsonRpcResponse = {
      request.method match {
        case "initialize" =>
          logger.info("🤝 Handling initialization handshake")
          val initResponse = InitializeResponse(
            protocolVersion = "2024-11-05",
            capabilities = MCPCapabilities(
              tools = Some(Obj())
            ),
            serverInfo = ServerInfo(
              name = "MCPServer",
              version = "1.0.0"
            )
          )
          
          JsonRpcResponse(
            id = request.id,
            result = Some(upickle.default.writeJs(initResponse))
          )
          
        case "tools/list" =>
          logger.info("📋 Handling tools list request")
          val tools = Seq(
            // Weather tool
            MCPTool(
              name = "get_weather",
              description = "Get current weather for any city (MCP version)",
              inputSchema = Obj(
                "type" -> Str("object"),
                "properties" -> Obj(
                  "location" -> Obj(
                    "type" -> Str("string"),
                    "description" -> Str("City name (e.g., 'Paris, France')")
                  ),
                  "units" -> Obj(
                    "type" -> Str("string"),
                    "description" -> Str("Temperature units (celsius or fahrenheit)"),
                    "enum" -> Arr(Str("celsius"), Str("fahrenheit"))
                  )
                ),
                "required" -> Arr(Str("location"))
              )
            ),
            
            // Currency conversion tool
            MCPTool(
              name = "currency_convert",
              description = "Convert money between currencies",
              inputSchema = Obj(
                "type" -> Str("object"),
                "properties" -> Obj(
                  "amount" -> Obj(
                    "type" -> Str("number"),
                    "description" -> Str("Amount to convert")
                  ),
                  "from" -> Obj(
                    "type" -> Str("string"),
                    "description" -> Str("Source currency (e.g., 'USD', 'EUR', 'GBP')")
                  ),
                  "to" -> Obj(
                    "type" -> Str("string"),
                    "description" -> Str("Target currency (e.g., 'USD', 'EUR', 'GBP')")
                  )
                ),
                "required" -> Arr(Str("amount"), Str("from"), Str("to"))
              )
            )
            
          )
          
          val toolsResponse = ToolsListResponse(tools)
          
          JsonRpcResponse(
            id = request.id,
            result = Some(upickle.default.writeJs(toolsResponse))
          )
          
        case "tools/call" =>
          logger.info("🔧 Handling tool call request")
          request.params match {
            case Some(params) =>
              Try {
                val toolCallRequest = upickleRead[ToolsCallRequest](params.toString)
                val toolName = toolCallRequest.name
                val arguments = toolCallRequest.arguments.getOrElse(Obj())
                
                logger.debug(s"   Tool: $toolName")
                logger.debug(s"   Args: ${arguments.render()}")
                
                val result = executeTool(toolName, arguments)
                val content = Seq(MCPContent("text", result.render()))
                val toolResponse = ToolsCallResponse(content)
                
                JsonRpcResponse(
                  id = request.id,
                  result = Some(upickle.default.writeJs(toolResponse))
                )
              } match {
                case Success(response) => response
                case Failure(e) =>
                  logger.error(s"   Error parsing tool call: ${e.getMessage}", e)
                  JsonRpcResponse(
                    id = request.id,
                    error = Some(JsonRpcError(-32602, "Invalid params", Some(Str(e.getMessage))))
                  )
              }
              
            case None =>
              JsonRpcResponse(
                id = request.id,
                error = Some(JsonRpcError(-32602, "Invalid params", None))
              )
          }
          
        case _ =>
          logger.warn(s"❓ Unknown method: ${request.method}")
          JsonRpcResponse(
            id = request.id,
            error = Some(JsonRpcError(-32601, "Method not found", None))
          )
      }
    }
    
    private def executeTool(toolName: String, arguments: ujson.Value): ujson.Value = {
      toolName match {
        case "get_weather" =>
          val location = arguments("location").str
          val units = arguments.obj.get("units").map(_.str).getOrElse("celsius")
          val temp = if (units == "fahrenheit") 70 else 20
          
          Obj(
            "location" -> Str(location),
            "temperature" -> Num(temp),
            "units" -> Str(units),
            "conditions" -> Str("Partly cloudy (from MCP server)"),
            "humidity" -> Str("65%"),
            "wind" -> Str("8 mph"),
            "source" -> Str("MCP server")
          )
          
        case "currency_convert" =>
          val amount = arguments("amount").num
          val from = arguments("from").str.toUpperCase
          val to = arguments("to").str.toUpperCase
          
          // Simple exchange rate lookup table
          val exchangeRates = Map(
            ("USD", "EUR") -> 0.85,
            ("EUR", "USD") -> 1.18,
            ("USD", "GBP") -> 0.75,
            ("GBP", "USD") -> 1.33,
            ("EUR", "GBP") -> 0.88,
            ("GBP", "EUR") -> 1.14,
            ("USD", "JPY") -> 110.0,
            ("JPY", "USD") -> 0.009
          )
          
          exchangeRates.get((from, to)) match {
            case Some(rate) =>
              // Success response
              Obj(
                "success" -> ujson.True,
                "original_amount" -> Num(amount),
                "from_currency" -> Str(from),
                "to_currency" -> Str(to),
                "converted_amount" -> Num(amount * rate),
                "exchange_rate" -> Num(rate),
                "source" -> Str("MCP server currency API")
              )
            case None =>
              // Error response for unsupported pairs
              Obj(
                "success" -> ujson.False,
                "error" -> Str("UNSUPPORTED_CURRENCY_PAIR"),
                "message" -> Str(s"Currency conversion from $from to $to is not supported"),
                "from_currency" -> Str(from),
                "to_currency" -> Str(to),
                "supported_currencies" -> Arr(Str("USD"), Str("EUR"), Str("GBP"), Str("JPY")),
                "source" -> Str("MCP server currency API")
              )
          }
        
        case _ =>
          Obj("error" -> Str(s"Unknown tool: $toolName"))
      }
    }
    
    private def sendJsonRpcResponse(exchange: HttpExchange, response: JsonRpcResponse): Unit = {
      val responseJson = upickleWrite(response)
      exchange.getResponseHeaders.set("Content-Type", "application/json")
      sendResponse(exchange, 200, responseJson)
      
      logger.debug(s"📤 Sent JSON-RPC response (id: ${response.id})")
    }
    
    private def sendResponse(exchange: HttpExchange, status: Int, response: String): Unit = {
      exchange.sendResponseHeaders(status, response.length)
      val os: OutputStream = exchange.getResponseBody
      os.write(response.getBytes)
      os.close()
    }
  }
}