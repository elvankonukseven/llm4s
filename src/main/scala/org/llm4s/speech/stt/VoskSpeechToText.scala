package org.llm4s.speech.stt

import org.llm4s.speech.{ AudioInput, AudioMeta }
import org.llm4s.types.Result
import org.llm4s.error.ProcessingError
import org.vosk.Model
import org.vosk.Recognizer
import java.io.ByteArrayInputStream
import java.nio.file.Files
import org.llm4s.speech.processing.AudioPreprocessing

/**
 * Vosk-based speech-to-text implementation.
 * Replaces Sphinx4 as it's more actively maintained and has better performance.
 */
final class VoskSpeechToText(
  modelPath: Option[String] = None
) extends SpeechToText {

  override val name: String = "vosk"

  override def transcribe(input: AudioInput, options: STTOptions): Result[Transcription] =
    try {
      // Use default English model if no path provided
      val model      = new Model(modelPath.getOrElse("models/vosk-model-small-en-us-0.15"))
      val recognizer = new Recognizer(model, 16000.0f) // Vosk expects 16kHz

      // Prepare audio for Vosk (16kHz mono PCM)
      val preparedAudio = prepareAudioForVosk(input)

      // Process audio in chunks
      val chunkSize   = 4096
      val audioStream = new ByteArrayInputStream(preparedAudio)
      val buffer      = new Array[Byte](chunkSize)

      var finalResult = ""

      var bytesRead = audioStream.read(buffer)
      while (bytesRead > 0) {
        if (recognizer.acceptWaveForm(buffer, bytesRead)) {
          val result = recognizer.getResult()
          // Parse JSON result to extract text and confidence
          // This is a simplified implementation
          finalResult += result
        }
        bytesRead = audioStream.read(buffer)
      }

      // Get final result
      val finalPartial = recognizer.getFinalResult()
      finalResult += finalPartial

      audioStream.close()
      recognizer.close()
      model.close()

      Right(
        Transcription(
          text = finalResult.trim,
          language = options.language.orElse(Some("en")),
          confidence = None,
          timestamps = Nil,
          meta = None
        )
      )

    } catch {
      case e: Exception =>
        Left(ProcessingError.audioValidation("Vosk processing failed", Some(e)))
    }

  private def prepareAudioForVosk(input: AudioInput): Array[Byte] =
    input match {
      case AudioInput.FileAudio(path) => Files.readAllBytes(path)
      case AudioInput.BytesAudio(bytes, sampleRate, channels) =>
        val meta = AudioMeta(sampleRate = sampleRate, numChannels = channels, bitDepth = 16)
        AudioPreprocessing
          .standardizeForSTT(bytes, meta, targetRate = 16000)
          .fold(_ => bytes, { case (b, _) => b })
      case AudioInput.StreamAudio(stream, sampleRate, channels) =>
        val bytes = stream.readAllBytes()
        val meta  = AudioMeta(sampleRate = sampleRate, numChannels = channels, bitDepth = 16)
        AudioPreprocessing
          .standardizeForSTT(bytes, meta, targetRate = 16000)
          .fold(_ => bytes, { case (b, _) => b })
    }
}
