package org.llm4s.http

import java.net.http.{HttpClient => JavaHttpClient, HttpRequest, HttpResponse}
import java.net.URI
import java.time.Duration

class HttpClient(baseUrl: String) {
  private val client = JavaHttpClient.newBuilder()
    .connectTimeout(Duration.ofSeconds(30))
    .build()

  def get(path: String): String = {
    val request = HttpRequest.newBuilder()
      .uri(URI.create(s"$baseUrl$path"))
      .GET()
      .build()
    
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    
    if (response.statusCode() >= 400) {
      throw new RuntimeException(s"HTTP ${response.statusCode()}: ${response.body()}")
    }
    
    response.body()
  }

  def post(path: String, body: String): String = {
    val request = HttpRequest.newBuilder()
      .uri(URI.create(s"$baseUrl$path"))
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(body))
      .build()
    
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    
    if (response.statusCode() >= 400) {
      throw new RuntimeException(s"HTTP ${response.statusCode()}: ${response.body()}")
    }
    
    response.body()
  }

  def postWithHeaders(path: String, body: String, headers: Map[String, String]): String = {
    val requestBuilder = HttpRequest.newBuilder()
      .uri(URI.create(s"$baseUrl$path"))
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(body))
    
    // Add custom headers
    headers.foreach { case (key, value) =>
      requestBuilder.header(key, value)
    }
    
    val request = requestBuilder.build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    
    if (response.statusCode() >= 400) {
      throw new RuntimeException(s"HTTP ${response.statusCode()}: ${response.body()}")
    }
    
    response.body()
  }
}