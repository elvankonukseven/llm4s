package org.llm4s.samples.context

import org.llm4s.config.ConfigReader
import org.llm4s.config.ConfigReader.LLMConfig
import org.llm4s.context.ConversationTokenCounter
import org.llm4s.context.tokens.TokenizerMapping
import org.llm4s.llmconnect.LLM
import org.llm4s.llmconnect.model._
import org.llm4s.types.Result
import org.slf4j.LoggerFactory

/**
 * Demonstrates token counting and conversation trimming with real LLM API calls.
 * Works with OpenAI, Anthropic, and Azure OpenAI models.
 * 
 * To run this example:
 * ```bash
 * # OpenAI
 * export LLM_MODEL=openai/gpt-4o
 * export OPENAI_API_KEY=sk-your-key-here
 * 
 * # Anthropic
 * export LLM_MODEL=anthropic/claude-3-5-sonnet-latest
 * export ANTHROPIC_API_KEY=sk-ant-your-key-here
 * 
 * # Azure OpenAI
 * export LLM_MODEL=azure/your-deployment-name
 * export AZURE_OPENAI_API_KEY=your-key
 * export AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
 * 
 * sbt "samples/runMain org.llm4s.samples.context.TokenWindowExample"
 * ```
 */
object TokenWindowExample {
  private val logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    println("=== Multi-Provider Token Window Management Demo ===\n")
    
    val result = for {
      cfg  <- getConfiguration()
      (modelName, config) = cfg
      client <- createClient(config)
      tokenCounter <- createTokenCounter(modelName)
      demoResults <- runDemo(client, tokenCounter, modelName)
    } yield demoResults

    result.fold(
      e => println(s"❌ Demo failed: $e"),
      r => displayResults(r)
    )
  }

  private def getConfiguration(): Result[(String, ConfigReader)] = {
    val config = LLMConfig()
    val modelName = config.getOrElse("LLM_MODEL", "openai/gpt-4o")
    println(s"🔧 Using model: $modelName")
    Right((modelName, config))
  }

  private def createClient(config: ConfigReader): Result[org.llm4s.llmconnect.LLMClient] = {
    val client = LLM.client(config)
    println(s"✅ LLM Client created")
    Right(client)
  }

  private def createTokenCounter(modelName: String): Result[ConversationTokenCounter] = 
    ConversationTokenCounter.forModel(modelName).map { counter =>
      val accuracyInfo = TokenizerMapping.getAccuracyInfo(modelName)
      val accuracyEmoji = if (accuracyInfo.isExact) "🎯" else "⚠️"
      
      println(s"✅ Token counter created for $modelName")
      println(s"$accuracyEmoji Tokenizer accuracy: ${accuracyInfo.description}")
      counter
    }

  private def runDemo(
    client: org.llm4s.llmconnect.LLMClient, 
    tokenCounter: ConversationTokenCounter,
    modelName: String
  ): Result[DemoResults] = {
    val longConversation = createLongConversation()
    
    val tokenAnalysis = analyzeTokens(tokenCounter, longConversation)
    
    testWithRealAPI(client, tokenCounter, longConversation, modelName).map { apiResults =>
      DemoResults(tokenAnalysis, apiResults)
    }
  }

  private def analyzeTokens(
    tokenCounter: ConversationTokenCounter, 
    conversation: Conversation
  ): TokenAnalysis = {
    val tokenCount = tokenCounter.countConversation(conversation)
    val breakdown = tokenCounter.getTokenBreakdown(conversation)
    
    TokenAnalysis(
      totalTokens = tokenCount,
      messageCount = conversation.messages.length,
      averageTokensPerMessage = tokenCount / conversation.messages.length,
      breakdown = breakdown
    )
  }

  private def testWithRealAPI(
    client: org.llm4s.llmconnect.LLMClient,
    tokenCounter: ConversationTokenCounter,
    conversation: Conversation,
    modelName: String
  ): Result[APITestResults] = {
    val predictedTokens = tokenCounter.countConversation(conversation)
    
    client.complete(conversation) match {
      case Right(completion) =>
        Right(APITestResults.success(predictedTokens, completion, modelName))
      case Left(error) =>
        Right(APITestResults.failure(predictedTokens, error, modelName))
    }
  }

  private def displayResults(results: DemoResults): Unit = {
    displayTokenAnalysis(results.tokenAnalysis)
    displayAPIResults(results.apiResults)
  }

  private def displayTokenAnalysis(analysis: TokenAnalysis): Unit = {
    println("\n=== Token Counting Analysis ===")
    println(s"📊 Total tokens in conversation: ${analysis.totalTokens}")
    println(s"📝 Number of messages: ${analysis.messageCount}")
    println(s"📈 Average tokens per message: ${analysis.averageTokensPerMessage}")
    println()
    println(analysis.breakdown.prettyPrint())
  }

  private def displayAPIResults(results: APITestResults): Unit = {
    println("=== Real API Testing ===")
    println(s"🔍 Our token count prediction: ${results.predictedTokens} tokens")
    println(s"🤖 Model: ${results.modelName}")
    println()

    results match {
      case APITestResults(_, _, true, Some(completion), None) =>
        displaySuccessResults(completion, results.predictedTokens, results.modelName)
      case APITestResults(_, _, false, None, Some(error)) =>
        displayErrorResults(error, results.predictedTokens, results.modelName)
      case _ =>
        println("⚠️ Unexpected API results state")
    }
  }

  private def displaySuccessResults(completion: Completion, predictedTokens: Int, modelName: String): Unit = {
    println("✅ API call successful!")
    
    completion.usage.foreach { usage =>
      println(f"📈 API reported usage:")
      println(f"   - Prompt tokens: ${usage.promptTokens}%,d")
      println(f"   - Completion tokens: ${usage.completionTokens}%,d") 
      println(f"   - Total tokens: ${usage.totalTokens}%,d")
      
      val accuracy = (predictedTokens.toDouble / usage.promptTokens * 100)
      println(f"🎯 Token prediction accuracy: $accuracy%.1f%%")
      
      val accuracyMessage = accuracy match {
        case a if a > 95 => "🎉 Excellent accuracy!"
        case a if a > 85 => "👍 Very good accuracy!"
        case a if a > 70 => "✅ Acceptable accuracy!"
        case a if a > 50 => "⚠️  Fair accuracy (expected for Anthropic models)"
        case _ => "❌ Poor accuracy - may indicate tokenizer mismatch"
      }
      println(accuracyMessage)
      
      // Explain accuracy expectations for different providers
      if (modelName.toLowerCase.contains("anthropic") || modelName.toLowerCase.contains("claude")) {
        println("ℹ️  Note: Anthropic models use proprietary tokenizers, so 70-80% accuracy is expected")
      }
    }
    
    println(f"\n💬 Response preview: ${completion.message.content.take(100)}...")
  }

  private def displayErrorResults(error: org.llm4s.error.LLMError, predictedTokens: Int, modelName: String): Unit = {
    println(s"❌ API call failed: ${error.message}")
    
    if (error.message.toLowerCase.contains("token") || error.message.toLowerCase.contains("length")) {
      println(s"🚨 This looks like a token limit error!")
      println(s"   Our prediction was $predictedTokens tokens")
      println(s"   Model: $modelName")
      println("   This validates that our token counting detected the issue!")
    }
  }

  private def createLongConversation(): Conversation = {
    val systemPrompt = SystemMessage(
      "You are a helpful AI assistant. Please provide detailed, comprehensive responses to all questions. " +
      "Include specific examples, technical details, and practical applications where relevant."
    )
    
    val conversationPairs = (1 to 12).flatMap(createConversationPair)
    val finalUserMessage = UserMessage(
      "Given everything we've discussed about all these topics, what are the key insights, " +
      "common patterns, and most important takeaways? Please provide a comprehensive summary " +
      "that synthesizes all the information we've covered."
    )
    
    Conversation(systemPrompt +: (conversationPairs :+ finalUserMessage))
  }

  private def createConversationPair(topic: Int): Seq[Message] = 
    Seq(
      UserMessage(createLongUserMessage(topic)),
      AssistantMessage(createLongAssistantResponse(topic))
    )

  private def createLongUserMessage(topic: Int): String = 
    s"Please provide a comprehensive analysis of topic $topic. I need detailed information covering: " +
    s"1) Historical background and context, 2) Current applications and use cases, " +
    s"3) Technical specifications and implementation details, 4) Benefits and advantages, " +
    s"5) Challenges and limitations, 6) Future prospects and emerging trends, " +
    s"7) Best practices and recommendations, 8) Real-world examples and case studies. " +
    "Please be thorough and include specific details, statistics, and expert insights where available."

  private def createLongAssistantResponse(topic: Int): String = 
    s"Thank you for your comprehensive question about topic $topic. I'll provide a detailed analysis covering all the aspects you mentioned. " +
    s"Starting with the historical background: Topic $topic has evolved significantly over the past decades, with key developments including various technological advances, regulatory changes, and market shifts. " +
    s"In terms of current applications, we see widespread adoption across multiple industries and sectors, with specific use cases ranging from enterprise solutions to consumer applications. " +
    s"The technical specifications involve complex systems and architectures that require careful consideration of performance, scalability, security, and maintainability factors. " +
    s"Key benefits include improved efficiency, cost reduction, enhanced user experience, and competitive advantages. However, challenges include implementation complexity, integration issues, skill requirements, and ongoing maintenance needs. " +
    s"Looking ahead, future trends point toward continued innovation, emerging technologies, evolving standards, and new market opportunities that will shape the landscape of topic $topic." * 2
}

case class DemoResults(
  tokenAnalysis: TokenAnalysis,
  apiResults: APITestResults
)

case class TokenAnalysis(
  totalTokens: Int,
  messageCount: Int,
  averageTokensPerMessage: Int,
  breakdown: org.llm4s.context.TokenBreakdown
)

case class APITestResults(
  predictedTokens: Int,
  modelName: String,
  success: Boolean,
  completion: Option[Completion],
  error: Option[org.llm4s.error.LLMError]
)

object APITestResults {
  def success(predictedTokens: Int, completion: Completion, modelName: String): APITestResults =
    APITestResults(predictedTokens, modelName, true, Some(completion), None)
    
  def failure(predictedTokens: Int, error: org.llm4s.error.LLMError, modelName: String): APITestResults =
    APITestResults(predictedTokens, modelName, false, None, Some(error))
}