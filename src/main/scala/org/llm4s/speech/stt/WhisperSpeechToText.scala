package org.llm4s.speech.stt

import org.llm4s.types.Result
import org.llm4s.speech.AudioInput
import org.llm4s.speech.io.WavFileGenerator
import org.llm4s.error.ProcessingError
import cats.implicits._

import java.nio.file.{ Files, Path }
import java.io.IOException
import scala.sys.process._

/**
 * Enhanced Whisper integration via CLI (whisper.cpp or openai-whisper).
 * Supports various Whisper models and output formats.
 */
final class WhisperSpeechToText(
  command: Seq[String] = Seq("whisper"),
  model: String = "base",
  outputFormat: String = "txt"
) extends SpeechToText {
  override val name: String = "whisper-cli"

  override def transcribe(input: AudioInput, options: STTOptions): Result[Transcription] =
    for {
      wav <- input match {
        case AudioInput.FileAudio(path) => Right(path)
        case AudioInput.BytesAudio(bytes, _, _) =>
          WavFileGenerator.createTempWavFile("llm4s-whisper-").flatMap { tmp =>
            try {
              Files.write(tmp, bytes)
              Right(tmp)
            } catch {
              case _: IOException => Left(ProcessingError.audioValidation("IO error writing bytes to temp WAV file"))
              case _: SecurityException =>
                Left(ProcessingError.audioValidation("Security error accessing temp WAV file"))
              case _: Exception => Left(ProcessingError.audioValidation("Failed to write bytes to temp WAV file"))
            }
          }
        case AudioInput.StreamAudio(stream, _, _) =>
          WavFileGenerator.createTempWavFile("llm4s-whisper-").flatMap { tmp =>
            try {
              Files.write(tmp, stream.readAllBytes())
              Right(tmp)
            } catch {
              case _: IOException => Left(ProcessingError.audioValidation("IO error writing stream to temp WAV file"))
              case _: SecurityException =>
                Left(ProcessingError.audioValidation("Security error accessing temp WAV file"))
              case _: Exception => Left(ProcessingError.audioValidation("Failed to write stream to temp WAV file"))
            }
          }
      }

      args = buildWhisperArgs(wav, options)

      output <-
        try
          Right(args.!!)
        catch {
          case _: java.io.IOException => Left(ProcessingError.audioValidation("Whisper CLI not found or IO error"))
          case _: RuntimeException =>
            Left(ProcessingError.audioValidation("Whisper CLI execution failed with non-zero exit code"))
          case _: Exception => Left(ProcessingError.audioValidation("Whisper CLI execution failed"))
        }

      transcript = parseWhisperOutput(output, options)

    } yield transcript

  private def buildWhisperArgs(inputPath: Path, options: STTOptions): Seq[String] = {
    val baseArgs = command ++ Seq(
      inputPath.toString,
      "--model",
      model,
      "--output_format",
      outputFormat
    )

    val optFlags = List(
      options.language.map(l => Seq("--language", l)),
      options.prompt.map(p => Seq("--initial_prompt", p)),
      if (options.enableTimestamps) Some(Seq("--word_timestamps", "True")) else None
    ).flatten

    baseArgs ++ optFlags.combineAll
  }

  private def parseWhisperOutput(output: String, options: STTOptions): Transcription = {
    // Parse output based on format and options
    val text       = output.trim
    val confidence = extractConfidence(output)
    val timestamps = if (options.enableTimestamps) extractTimestamps(output) else Nil

    Transcription(
      text = text,
      language = options.language,
      confidence = confidence,
      timestamps = timestamps,
      meta = None
    )
  }

  private def extractConfidence(output: String): Option[Double] =
    // Whisper CLI may output confidence scores in some formats
    // This is a placeholder - actual parsing depends on Whisper version
    None

  private def extractTimestamps(output: String): List[WordTimestamp] =
    // Parse word-level timestamps if available
    // Format varies by Whisper version and output format
    Nil
}
