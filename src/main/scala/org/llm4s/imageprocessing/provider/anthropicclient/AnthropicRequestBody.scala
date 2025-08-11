package org.llm4s.imageprocessing.provider.anthropicclient

import upickle.default.{ macroRW, ReadWriter => RW, _ }

private[anthropicclient] case class PromptType(`type`: String, text: String = "")
object PromptType {
  implicit val rw: RW[PromptType] = macroRW
}

private[anthropicclient] case class SourceType(`type`: String, media_type: String, data: String = "")
object SourceType {
  implicit val rw: RW[SourceType] = macroRW
}

private[anthropicclient] case class ImageType(`type`: String, source: SourceType)
object ImageType {
  implicit val rw: RW[ImageType] = macroRW
}

private[anthropicclient] case class Message(role: String = "", content: (PromptType, ImageType))
object Message {
  implicit val rw: RW[Message] = macroRW
}

private[anthropicclient] case class AnthropicRequestBody(model: String, max_tokens: Int, messages: List[Message])
object AnthropicRequestBody {
  implicit val rw: RW[AnthropicRequestBody] = macroRW

  def apply(): AnthropicRequestBody = new AnthropicRequestBody("", 0, Nil)
  def serialize(configModel: String, maxTokens: Int, prompt: String, data: String): String =
    write(
      AnthropicRequestBody(
        model = configModel,
        max_tokens = maxTokens,
        messages = List(
          Message(
            role = "user",
            content = PromptType("text", prompt) -> ImageType("image", SourceType("base64", "image/jpeg", data))
          )
        )
      )
    )
}
