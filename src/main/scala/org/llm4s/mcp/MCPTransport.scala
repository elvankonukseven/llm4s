package org.llm4s.mcp

import scala.util.{Try, Success, Failure}
import java.util.concurrent.atomic.AtomicLong
import upickle.default._
import org.llm4s.http.HttpClient

// Transport type definitions

// Base trait for MCP transport mechanisms
sealed trait MCPTransport

// Stdio transport using subprocess communication
case class StdioTransport(command: Seq[String]) extends MCPTransport

// Server-Sent Events transport using HTTP
case class SSETransport(url: String) extends MCPTransport

// Base trait for transport implementations
trait MCPTransportImpl {
  // Sends a JSON-RPC request and waits for response
  def sendRequest(request: JsonRpcRequest): Either[String, JsonRpcResponse]
  // Closes the transport connection
  def close(): Unit
}

// SSE transport implementation using HTTP client
class SSETransportImpl(url: String) extends MCPTransportImpl {
  private val client = new HttpClient(url)
  private val requestId = new AtomicLong(0)
  
  // Sends JSON-RPC request via HTTP POST
  override def sendRequest(request: JsonRpcRequest): Either[String, JsonRpcResponse] = {
    Try {
      val requestJson = write(request)
      val responseBody = client.post("/sse", requestJson)
      val response = read[JsonRpcResponse](responseBody)
      response
    } match {
      case Success(response) => 
        response.error match {
          case Some(error) => Left(s"JSON-RPC Error ${error.code}: ${error.message}")
          case None => Right(response)
        }
      case Failure(exception) => 
        Left(s"Transport error: ${exception.getMessage}")
    }
  }
  
  // Closes the HTTP client connection
  override def close(): Unit = {
    // SSE connections are stateless, nothing to close
  }
  
  // Generates unique request IDs
  def generateId(): String = requestId.incrementAndGet().toString
}

// Stdio transport implementation using subprocess communication
class StdioTransportImpl(command: Seq[String]) extends MCPTransportImpl {
  private var process: Option[Process] = None
  private val requestId = new AtomicLong(0)
  
  // Gets existing process or starts new one if needed
  private def getOrStartProcess(): Either[String, Process] = {
    process match {
      case Some(p) if p.isAlive => Right(p)
      case _ => 
        Try {
          val processBuilder = new ProcessBuilder(command: _*)
          val newProcess = processBuilder.start()
          process = Some(newProcess)
          newProcess
        } match {
          case Success(p) => Right(p)
          case Failure(e) => Left(s"Failed to start MCP server process: ${e.getMessage}")
        }
    }
  }
  
  // Sends JSON-RPC request via subprocess stdin/stdout
  override def sendRequest(request: JsonRpcRequest): Either[String, JsonRpcResponse] = {
    getOrStartProcess().flatMap { proc =>
      Try {
        val requestJson = write(request)
        
        // Write request to process stdin
        val writer = new java.io.OutputStreamWriter(proc.getOutputStream, "UTF-8")
        writer.write(requestJson + "\n")
        writer.flush()
        
        // Read response from process stdout
        val reader = new java.io.BufferedReader(
          new java.io.InputStreamReader(proc.getInputStream, "UTF-8")
        )
        val responseLine = reader.readLine()
        
        if (responseLine == null) {
          throw new RuntimeException("No response from MCP server")
        }
        
        val response = read[JsonRpcResponse](responseLine)
        response
      } match {
        case Success(response) =>
          response.error match {
            case Some(error) => Left(s"JSON-RPC Error ${error.code}: ${error.message}")
            case None => Right(response)
          }
        case Failure(exception) =>
          Left(s"Stdio transport error: ${exception.getMessage}")
      }
    }
  }
  
  // Terminates the subprocess and closes streams
  override def close(): Unit = {
    process.foreach { p =>
      Try {
        p.getOutputStream.close()
        p.getInputStream.close()
        p.getErrorStream.close()
        p.destroyForcibly()
      }
      process = None
    }
  }
  
  // Generates unique request IDs
  def generateId(): String = requestId.incrementAndGet().toString
}

// Factory for creating transport implementations
object MCPTransport {
  // Creates appropriate transport implementation based on configuration
  def create(config: MCPServerConfig): MCPTransportImpl = {
    config.transport match {
      case StdioTransport(command) => new StdioTransportImpl(command)
      case SSETransport(url) => new SSETransportImpl(url)
    }
  }
} 