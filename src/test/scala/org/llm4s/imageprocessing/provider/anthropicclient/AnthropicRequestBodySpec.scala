package org.llm4s.imageprocessing.provider.anthropicclient

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import upickle.default._

class AnthropicRequestBodySpec extends AnyFlatSpec with Matchers {

  "AnthropicRequestBody" should "create empty instance with apply()" in {
    val body = AnthropicRequestBody()
    body.model shouldBe ""
    body.max_tokens shouldBe 0
    body.messages shouldBe Nil
  }

  it should "correctly serialize request with prompt and image data" in {
    val configModel = "claude-3-opus-20240229"
    val maxTokens   = 1024
    val prompt      = "Describe this image"
    val imageData   = "base64EncodedImageData"

    val serialized   = AnthropicRequestBody.serialize(configModel, maxTokens, prompt, imageData)
    val deserialized = read[AnthropicRequestBody](serialized)

    deserialized.model shouldBe configModel
    deserialized.max_tokens shouldBe maxTokens

    val messages = deserialized.messages
    messages should have size 1

    val message = messages.head
    message.role shouldBe "user"

    val (promptType, imageType) = message.content
    promptType.`type` shouldBe "text"
    promptType.text shouldBe prompt

    imageType.`type` shouldBe "image"
    imageType.source.`type` shouldBe "base64"
    imageType.source.media_type shouldBe "image/jpeg"
    imageType.source.data shouldBe imageData
  }

  it should "be serializable and deserializable" in {
    val body = AnthropicRequestBody(
      model = "test-model",
      max_tokens = 100,
      messages = List(
        Message(
          role = "user",
          PromptType("text", "test prompt") ->
            ImageType("image", SourceType("base64", "image/jpeg", "test-data"))
        )
      )
    )

    val serialized   = write(body)
    val deserialized = read[AnthropicRequestBody](serialized)

    deserialized shouldBe body
  }
}
