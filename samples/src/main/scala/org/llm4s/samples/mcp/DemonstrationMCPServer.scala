package org.llm4s.samples.mcp

import com.sun.net.httpserver.{HttpServer, HttpHandler, HttpExchange}
import java.net.InetSocketAddress
import org.slf4j.LoggerFactory
import upickle.default.{read => upickleRead, write => upickleWrite}
import org.llm4s.mcp._
import ujson.{Obj, Str, Num, Arr}
import scala.util.{Try, Success, Failure}
import java.util.UUID
import scala.collection.mutable

/**
 * MCP Server implementing the 2025-03-26 Streamable HTTP specification.
 * 
 * Key features demonstrated:
 * - Server-generated session management with Mcp-Session-Id headers  
 * - Single /mcp endpoint supporting POST, GET, and DELETE methods
 * - Content negotiation between application/json and text/event-stream
 * - Automatic protocol version fallback to 2024-11-05
 * - Stateless and stateful operation modes
 * 
 * Run: sbt "samples/runMain org.llm4s.samples.mcp.DemonstrationMCPServer"
 */
object DemonstrationMCPServer {
  private val logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    val server = HttpServer.create(new InetSocketAddress(8080), 0)
    
    server.createContext("/mcp", new MCPHandler())
    server.setExecutor(null)
    server.start()

    logger.info("🚀 MCP Server started on http://localhost:8080/mcp")
    logger.info("✨ 2025-03-26 Streamable HTTP with session management")
    logger.info("🔧 Available tools: get_weather, currency_convert")
    
    Thread.currentThread().join()
  }

  // Session management for 2025-03-26 specification
  case class Session(
    id: String, 
    protocolVersion: String,
    created: Long = System.currentTimeMillis()
  )
  
  object SessionStore {
    private val sessions = mutable.Map[String, Session]()
    
    def createSession(protocolVersion: String): Session = {
      val session = Session(UUID.randomUUID().toString, protocolVersion)
      sessions(session.id) = session
      logger.debug(s"🆔 Created session: ${session.id} for protocol $protocolVersion")
      session
    }
    
    def getSession(id: String): Option[Session] = sessions.get(id)
    
    def removeSession(id: String): Boolean = {
      val existed = sessions.remove(id).isDefined
      if (existed) logger.debug(s"🗑️ Removed session: $id")
      existed
    }
    
    def sessionCount: Int = sessions.size
  }

  class MCPHandler extends HttpHandler {
    override def handle(exchange: HttpExchange): Unit = {
      val method = exchange.getRequestMethod
      logger.debug(s"📥 ${method} ${exchange.getRequestURI}")

      try {
        method match {
          case "POST" => handlePOST(exchange)
          case "GET" => handleGET(exchange) 
          case "DELETE" => handleDELETE(exchange)
          case _ => sendErrorResponse(exchange, 405, "Method not allowed")
        }
      } catch {
        case e: Exception =>
          logger.error(s"❌ Unhandled error in ${method}: ${e.getMessage}", e)
          sendErrorResponse(exchange, 500, "Internal server error")
      }
    }

    private def handlePOST(exchange: HttpExchange): Unit = {
      val result = for {
        body <- Try(scala.io.Source.fromInputStream(exchange.getRequestBody).mkString)
        request <- Try(upickleRead[JsonRpcRequest](body))
      } yield request

      result match {
        case Success(request) => 
          logger.debug(s"📨 Request: ${request.method} (id: ${request.id})")
          
          val sessionId = Option(exchange.getRequestHeaders.getFirst("mcp-session-id"))
          
          request.method match {
            case "initialize" =>
              handleInitialize(exchange, request)
            case "tools/list" =>
              handleWithSession(exchange, request, sessionId, handleToolsList)
            case "tools/call" =>
              handleWithSession(exchange, request, sessionId, handleToolsCall)
            case _ =>
              sendJsonRpcError(exchange, request.id, -32601, "Method not found")
          }
          
        case Failure(e) =>
          logger.error(s"❌ Failed to parse request: ${e.getMessage}")
          sendJsonRpcError(exchange, "unknown", -32700, "Parse error")
      }
    }

    private def handleInitialize(
      exchange: HttpExchange, 
      request: JsonRpcRequest
    ): Unit = {
      // Parse initialization request
      val initRequest = request.params.flatMap { params =>
        Try(upickleRead[InitializeRequest](params.toString)).toOption
      }.getOrElse(InitializeRequest("2024-11-05", MCPCapabilities(), ClientInfo("unknown", "1.0")))

      // Determine protocol version
      val clientVersion = initRequest.protocolVersion
      val protocolVersion = if (clientVersion.startsWith("2025-03-26")) "2025-03-26" else "2024-11-05"
      
      logger.info(s"🤝 Initializing with protocol: $protocolVersion")

      // Create session for 2025-03-26 (server-generated session IDs)
      val sessionOpt = if (protocolVersion == "2025-03-26") {
        Some(SessionStore.createSession(protocolVersion))
      } else None

      // Prepare response
      val response = JsonRpcResponse(
        id = request.id,
        result = Some(upickle.default.writeJs(InitializeResponse(
          protocolVersion = protocolVersion,
          capabilities = MCPCapabilities(tools = Some(Obj())),
          serverInfo = ServerInfo(name = "Demo MCP Server", version = "2.0.0")
        )))
      )

      // Send response with session header if applicable
      sendJsonRpcResponse(exchange, response, sessionOpt.map(_.id))
      
      sessionOpt.foreach { session =>
        logger.info(s"🆔 Session created: ${session.id} (total: ${SessionStore.sessionCount})")
      }
    }

    private def handleWithSession(
      exchange: HttpExchange,
      request: JsonRpcRequest,
      sessionId: Option[String],
      handler: JsonRpcRequest => JsonRpcResponse
    ): Unit = {
      // For 2025-03-26, validate session if provided
      sessionId.foreach { id =>
        SessionStore.getSession(id) match {
          case Some(session) => 
            logger.debug(s"✅ Using session: ${session.id}")
          case None =>
            logger.warn(s"⚠️ Unknown session: $id")
        }
      }
      
      val response = handler(request)
      sendJsonRpcResponse(exchange, response, sessionId)
    }

    private def handleGET(exchange: HttpExchange): Unit = {
      // SSE endpoint for real-time communication (2025-03-26 feature)
      val acceptHeader = Option(exchange.getRequestHeaders.getFirst("Accept")).getOrElse("")
      
      if (!acceptHeader.contains("text/event-stream")) {
        sendErrorResponse(exchange, 406, "GET requires Accept: text/event-stream")
        return
      }

      val sessionId = Option(exchange.getRequestHeaders.getFirst("mcp-session-id"))
      
      sessionId match {
        case Some(id) if SessionStore.getSession(id).isDefined =>
          logger.info(s"📡 Starting SSE stream for session: $id")
          sendSSEStream(exchange, id)
        case Some(id) =>
          sendErrorResponse(exchange, 400, s"Invalid session: $id")
        case None =>
          sendErrorResponse(exchange, 400, "Missing mcp-session-id header for SSE")
      }
    }

    private def handleDELETE(exchange: HttpExchange): Unit = {
      // Session termination (2025-03-26 feature)
      val sessionId = Option(exchange.getRequestHeaders.getFirst("mcp-session-id"))
      
      sessionId match {
        case Some(id) =>
          if (SessionStore.removeSession(id)) {
            logger.info(s"🗑️ Session terminated: $id")
            sendResponse(exchange, 200, "application/json", """{"status":"session_terminated"}""")
          } else {
            sendErrorResponse(exchange, 404, "Session not found")
          }
        case None =>
          sendErrorResponse(exchange, 400, "Missing mcp-session-id header")
      }
    }

    private def handleToolsList(request: JsonRpcRequest): JsonRpcResponse = {
      logger.info("📋 Listing tools")
      
      val tools = Seq(
        MCPTool(
          name = "get_weather",
          description = "Get current weather for any city",
          inputSchema = Obj(
            "type" -> Str("object"),
            "properties" -> Obj(
              "city" -> Obj(
                "type" -> Str("string"),
                "description" -> Str("City name")
              )
            ),
            "required" -> Arr(Str("city"))
          )
        ),
        MCPTool(
          name = "currency_convert",
          description = "Convert between currencies",
          inputSchema = Obj(
            "type" -> Str("object"),
            "properties" -> Obj(
              "amount" -> Obj("type" -> Str("number")),
              "from_currency" -> Obj("type" -> Str("string")),
              "to_currency" -> Obj("type" -> Str("string"))
            ),
            "required" -> Arr(Str("amount"), Str("from_currency"), Str("to_currency"))
          )
        )
      )
      
      JsonRpcResponse(
        id = request.id,
        result = Some(upickle.default.writeJs(ToolsListResponse(tools)))
      )
    }

    private def handleToolsCall(request: JsonRpcRequest): JsonRpcResponse = {
      val callRequest = request.params.flatMap { params =>
        Try(upickleRead[ToolsCallRequest](params.toString)).toOption
      }
      
      callRequest match {
        case Some(ToolsCallRequest(toolName, args)) =>
          logger.info(s"🔧 Calling tool: $toolName")
          
                    toolName match {
            case "get_weather" =>
              val city = args match {
                case Some(ujson.Obj(obj)) => obj.get("city") match {
                  case Some(ujson.Str(city)) => city
                  case Some(value) => value.toString
                  case None => "Unknown"
                }
                case _ => "Unknown"
              }
              JsonRpcResponse(
                id = request.id,
                result = Some(upickle.default.writeJs(ToolsCallResponse(
                  content = Seq(MCPContent(
                    `type` = "text",
                    text = s"🌤️ Weather in $city: 20°C, partly cloudy (MCP server response)"
                  ))
                )))
              )
              
             case "currency_convert" =>
               args match {
                 case Some(ujson.Obj(obj)) =>
                   val amount = obj.get("amount") match {
                     case Some(ujson.Num(value)) => value
                     case Some(value) => value.toString.toDoubleOption.getOrElse(0.0)
                     case None => 0.0
                   }
                   val from = obj.get("from_currency") match {
                     case Some(ujson.Str(value)) => value
                     case Some(value) => value.toString
                     case None => "USD"
                   }
                   val to = obj.get("to_currency") match {
                     case Some(ujson.Str(value)) => value
                     case Some(value) => value.toString
                     case None => "EUR"
                   }
                   val converted = amount * 0.85 // Mock conversion rate
                   
                   JsonRpcResponse(
                     id = request.id,
                     result = Some(upickle.default.writeJs(ToolsCallResponse(
                       content = Seq(MCPContent(
                         `type` = "text", 
                         text = s"💱 $amount $from = ${converted} $to"
                       ))
                     )))
                   )
                 case _ =>
                   JsonRpcResponse(
                     id = request.id,
                     error = Some(JsonRpcError(-32602, "Invalid currency conversion arguments", None))
                   )
               }
              
            case _ =>
              JsonRpcResponse(
                id = request.id,
                error = Some(JsonRpcError(-32602, s"Unknown tool: $toolName", None))
              )
          }
          
        case None =>
          JsonRpcResponse(
            id = request.id,
            error = Some(JsonRpcError(-32602, "Invalid tool call parameters", None))
          )
      }
    }

    private def sendJsonRpcResponse(
      exchange: HttpExchange, 
      response: JsonRpcResponse, 
      sessionId: Option[String] = None
    ): Unit = {
      val json = upickleWrite(response)
      
      // Set session header if provided (before sendResponseHeaders)
      sessionId.foreach(id => exchange.getResponseHeaders.set("mcp-session-id", id))
      
      sendResponse(exchange, 200, "application/json", json)
    }

    private def sendJsonRpcError(
      exchange: HttpExchange, 
      id: String, 
      code: Int, 
      message: String
    ): Unit = {
      val error = JsonRpcResponse(
        id = id,
        error = Some(JsonRpcError(code, message, None))
      )
      sendJsonRpcResponse(exchange, error)
    }

    private def sendSSEStream(exchange: HttpExchange, sessionId: String): Unit = {
      exchange.getResponseHeaders.set("Content-Type", "text/event-stream")
      exchange.getResponseHeaders.set("Cache-Control", "no-cache")
      exchange.getResponseHeaders.set("Connection", "keep-alive")
      exchange.getResponseHeaders.set("mcp-session-id", sessionId)
      
      val sseData = 
        ": SSE stream opened\n\n" +
        "data: {\"jsonrpc\":\"2.0\",\"method\":\"notification/stream_started\",\"params\":{\"session\":\"" + sessionId + "\"}}\n\n"
      
      sendResponse(exchange, 200, "text/event-stream", sseData)
    }

    private def sendErrorResponse(exchange: HttpExchange, code: Int, message: String): Unit = {
      sendResponse(exchange, code, "text/plain", message)
    }

    private def sendResponse(
      exchange: HttpExchange, 
      statusCode: Int, 
      contentType: String, 
      body: String
    ): Unit = {
      val bytes = body.getBytes("UTF-8")
      
      exchange.getResponseHeaders.set("Content-Type", contentType)
      exchange.getResponseHeaders.set("Content-Length", bytes.length.toString)
      
      exchange.sendResponseHeaders(statusCode, bytes.length.toLong)
      
      val outputStream = exchange.getResponseBody
      try {
        outputStream.write(bytes)
        outputStream.flush()
      } finally {
        outputStream.close()
      }
    }
  }
} 