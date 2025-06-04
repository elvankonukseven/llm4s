package org.llm4s.samples.mcp

import com.sun.net.httpserver.{HttpServer, HttpHandler, HttpExchange}
import java.net.InetSocketAddress
import java.io.OutputStream
import upickle.default.{read => upickleRead, write => upickleWrite}
import org.llm4s.mcp._
import ujson.{Obj, Str, Num, Arr}
import scala.util.{Try, Success, Failure}

/**
 * Proper MCP server implementation that follows JSON-RPC 2.0 protocol
 * This server provides mock tools for testing MCP integration
 */
object MCPSseServer {
  
  def main(args: Array[String]): Unit = {
    val port = 8080
    val server = HttpServer.create(new InetSocketAddress(port), 0)
    
    println(s"🚀 Starting Proper MCP Server on http://localhost:$port")
    println("Protocol: JSON-RPC 2.0 over HTTP")
    println("Available tools: get_weather, currency_convert")
    
    // Handle MCP endpoint for JSON-RPC communication
    server.createContext("/sse", new MCPHandler())
    
    server.setExecutor(null)
    server.start()
    
    println("\n✨ MCP Server ready!")
    
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
          
          println(s"📨 Received JSON-RPC request: ${jsonRpcRequest.method} (id: ${jsonRpcRequest.id})")
          
          // Handle the JSON-RPC method
          val response = handleJsonRpcMethod(jsonRpcRequest)
          
          // Send JSON-RPC response
          sendJsonRpcResponse(exchange, response)
          
        } match {
          case Success(_) => // Request handled successfully
          case Failure(exception) =>
            println(s"❌ Error handling request: ${exception.getMessage}")
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
          println("🤝 Handling initialization handshake")
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
          println("📋 Handling tools list request")
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
          println("🔧 Handling tool call request")
          request.params match {
            case Some(params) =>
              Try {
                val toolCallRequest = upickleRead[ToolsCallRequest](params.toString)
                val toolName = toolCallRequest.name
                val arguments = toolCallRequest.arguments.getOrElse(Obj())
                
                println(s"   Tool: $toolName")
                println(s"   Args: ${arguments.render()}")
                
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
                  println(s"   Error parsing tool call: ${e.getMessage}")
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
          println(s"❓ Unknown method: ${request.method}")
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
          
          // Mock exchange rates
          val rate = (from, to) match {
            case ("USD", "EUR") => 0.85
            case ("EUR", "USD") => 1.18
            case ("USD", "GBP") => 0.75
            case ("GBP", "USD") => 1.33
            case ("EUR", "GBP") => 0.88
            case ("GBP", "EUR") => 1.14
            case ("USD", "JPY") => 110.0
            case ("JPY", "USD") => 0.009
            case _ => 1.0
          }
          
          Obj(
            "original_amount" -> Num(amount),
            "from_currency" -> Str(from),
            "to_currency" -> Str(to),
            "converted_amount" -> Num(amount * rate),
            "exchange_rate" -> Num(rate),
            "source" -> Str("MCP server currency API")
          )
        
        case _ =>
          Obj("error" -> Str(s"Unknown tool: $toolName"))
      }
    }
    
    private def sendJsonRpcResponse(exchange: HttpExchange, response: JsonRpcResponse): Unit = {
      val responseJson = upickleWrite(response)
      exchange.getResponseHeaders.set("Content-Type", "application/json")
      sendResponse(exchange, 200, responseJson)
      
      println(s"📤 Sent JSON-RPC response (id: ${response.id})")
    }
    
    private def sendResponse(exchange: HttpExchange, status: Int, response: String): Unit = {
      exchange.sendResponseHeaders(status, response.length)
      val os: OutputStream = exchange.getResponseBody
      os.write(response.getBytes)
      os.close()
    }
  }
}