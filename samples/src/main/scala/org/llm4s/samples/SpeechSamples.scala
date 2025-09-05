package org.llm4s.samples

import org.llm4s.speech.AudioInput
import org.llm4s.speech.stt.{ VoskSpeechToText, WhisperSpeechToText, STTOptions }
import org.llm4s.speech.tts.{ Tacotron2TextToSpeech, TTSOptions }
import org.llm4s.speech.util.PlatformCommands

import java.nio.file.{ Files, Paths }
import java.io.{ FileOutputStream, DataOutputStream }

object SpeechSamples {

  def createTestWavFile(): java.nio.file.Path = {
    val testFile = Files.createTempFile("whisper-test", ".wav")

    // Create a minimal valid WAV file with 1 second of silence
    val fos = new FileOutputStream(testFile.toFile)
    val dos = new DataOutputStream(fos)

    try {
      // Calculate sizes correctly
      val sampleRate     = 8000
      val channels       = 1
      val bitsPerSample  = 16
      val bytesPerSample = bitsPerSample / 8
      val blockAlign     = channels * bytesPerSample
      val byteRate       = sampleRate * blockAlign
      val dataSize       = sampleRate * channels * bytesPerSample // 1 second of audio
      val fileSize       = 36 + dataSize                          // 36 bytes header + data

      // WAV header
      dos.writeBytes("RIFF")
      writeLittleEndianInt(dos, fileSize)
      dos.writeBytes("WAVE")

      // fmt chunk
      dos.writeBytes("fmt ")
      writeLittleEndianInt(dos, 16)              // fmt chunk size
      writeLittleEndianShort(dos, 1)             // Audio format (PCM)
      writeLittleEndianShort(dos, channels)      // Channels (mono)
      writeLittleEndianInt(dos, sampleRate)      // Sample rate
      writeLittleEndianInt(dos, byteRate)        // Byte rate
      writeLittleEndianShort(dos, blockAlign)    // Block align
      writeLittleEndianShort(dos, bitsPerSample) // Bits per sample

      // data chunk
      dos.writeBytes("data")
      writeLittleEndianInt(dos, dataSize) // Data size

      // Write 1 second of silence
      for (_ <- 0 until sampleRate)
        writeLittleEndianShort(dos, 0) // 16-bit silence

    } finally {
      dos.close()
      fos.close()
    }

    testFile
  }

  def createToneWavFile(): java.nio.file.Path = {
    val testFile = Files.createTempFile("whisper-tone", ".wav")

    // Create a WAV file with a simple 440Hz tone (A note)
    val fos = new FileOutputStream(testFile.toFile)
    val dos = new DataOutputStream(fos)

    try {
      val sampleRate     = 16000 // Higher sample rate for better quality
      val channels       = 1
      val bitsPerSample  = 16
      val bytesPerSample = bitsPerSample / 8
      val blockAlign     = channels * bytesPerSample
      val byteRate       = sampleRate * blockAlign
      val duration       = 2     // 2 seconds
      val dataSize       = sampleRate * duration * channels * bytesPerSample
      val fileSize       = 36 + dataSize

      // WAV header
      dos.writeBytes("RIFF")
      writeLittleEndianInt(dos, fileSize)
      dos.writeBytes("WAVE")

      // fmt chunk
      dos.writeBytes("fmt ")
      writeLittleEndianInt(dos, 16)
      writeLittleEndianShort(dos, 1) // PCM
      writeLittleEndianShort(dos, channels)
      writeLittleEndianInt(dos, sampleRate)
      writeLittleEndianInt(dos, byteRate)
      writeLittleEndianShort(dos, blockAlign)
      writeLittleEndianShort(dos, bitsPerSample)

      // data chunk
      dos.writeBytes("data")
      writeLittleEndianInt(dos, dataSize)

      // Generate a 440Hz sine wave tone
      val frequency = 440.0 // A note
      val amplitude = 0.3   // Reduce amplitude to avoid clipping

      for (i <- 0 until sampleRate * duration) {
        val sample = (Math.sin(2 * Math.PI * frequency * i / sampleRate) * amplitude * 32767).toInt
        writeLittleEndianShort(dos, sample)
      }

    } finally {
      dos.close()
      fos.close()
    }

    testFile
  }

  def createSpeechLikeWavFile(): java.nio.file.Path = {
    val testFile = Files.createTempFile("whisper-speech", ".wav")

    // Create a WAV file with a speech-like pattern (modulated tone)
    val fos = new FileOutputStream(testFile.toFile)
    val dos = new DataOutputStream(fos)

    try {
      val sampleRate     = 16000
      val channels       = 1
      val bitsPerSample  = 16
      val bytesPerSample = bitsPerSample / 8
      val blockAlign     = channels * bytesPerSample
      val byteRate       = sampleRate * blockAlign
      val duration       = 3 // 3 seconds
      val dataSize       = sampleRate * duration * channels * bytesPerSample
      val fileSize       = 36 + dataSize

      // WAV header
      dos.writeBytes("RIFF")
      writeLittleEndianInt(dos, fileSize)
      dos.writeBytes("WAVE")

      // fmt chunk
      dos.writeBytes("fmt ")
      writeLittleEndianInt(dos, 16)
      writeLittleEndianShort(dos, 1) // PCM
      writeLittleEndianShort(dos, channels)
      writeLittleEndianInt(dos, sampleRate)
      writeLittleEndianInt(dos, byteRate)
      writeLittleEndianShort(dos, blockAlign)
      writeLittleEndianShort(dos, bitsPerSample)

      // data chunk
      dos.writeBytes("data")
      writeLittleEndianInt(dos, dataSize)

      // Generate a modulated tone that might be more speech-like
      val baseFreq  = 200.0 // Lower frequency, more speech-like
      val amplitude = 0.2

      for (i <- 0 until sampleRate * duration) {
        val time = i.toDouble / sampleRate
        // Create a modulated pattern with varying frequency
        val modFreq = baseFreq + 50 * Math.sin(2 * Math.PI * 2 * time) // Frequency modulation
        val sample  = (Math.sin(2 * Math.PI * modFreq * time) * amplitude * 32767).toInt
        writeLittleEndianShort(dos, sample)
      }

    } finally {
      dos.close()
      fos.close()
    }

    testFile
  }

  private def writeLittleEndianInt(dos: DataOutputStream, value: Int): Unit = {
    dos.writeByte(value & 0xff)
    dos.writeByte((value >> 8) & 0xff)
    dos.writeByte((value >> 16) & 0xff)
    dos.writeByte((value >> 24) & 0xff)
  }

  private def writeLittleEndianShort(dos: DataOutputStream, value: Int): Unit = {
    dos.writeByte(value & 0xff)
    dos.writeByte((value >> 8) & 0xff)
  }

  def demoWhisper(filePath: String): Unit = {
    println(s"Testing Whisper STT on ${PlatformCommands.platformName}")
    val stt = new WhisperSpeechToText(PlatformCommands.cat) // Cross-platform file content
    val res = stt.transcribe(AudioInput.FileAudio(Paths.get(filePath)), STTOptions(language = Some("en")))
    println(res.fold(err => s"STT error: ${err.formatted}", t => s"Transcript: ${t.text}"))
  }

  def demoWhisperReal(filePath: String): Unit = {
    println(s"Testing REAL Whisper STT on ${PlatformCommands.platformName}")
    // Use actual Whisper CLI if available
    val stt = new WhisperSpeechToText(Seq("whisper"), model = "base")
    val res = stt.transcribe(AudioInput.FileAudio(Paths.get(filePath)), STTOptions(language = Some("en")))
    println(res.fold(err => s"STT error: ${err.formatted}", t => s"Transcript: ${t.text}"))
  }

  def demoVosk(filePath: String): Unit = {
    println(s"Testing Vosk STT on ${PlatformCommands.platformName}")
    val stt = new VoskSpeechToText()
    val res = stt.transcribe(AudioInput.FileAudio(Paths.get(filePath)), STTOptions(language = Some("en")))
    println(res.fold(err => s"STT error: ${err.formatted}", t => s"Transcript: ${t.text}"))
  }

  def demoTacotron2(text: String): Unit = {
    println(s"Testing Tacotron2 TTS on ${PlatformCommands.platformName}")
    val tts = new Tacotron2TextToSpeech(PlatformCommands.echo) // Cross-platform echo
    val res = tts.synthesize(text, TTSOptions(voice = Some("default")))
    println(res.fold(err => s"TTS error: ${err.formatted}", _ => s"TTS synthesis completed."))
  }

  def main(args: Array[String]): Unit = {
    println("=== LLM4S Speech Demo ===")
    println(s"Platform: ${PlatformCommands.platformName}")

    println("\n--- Creating Test Audio Files ---")
    // Create all test files
    val silenceWavFile    = createTestWavFile()
    val toneWavFile       = createToneWavFile()
    val speechLikeWavFile = createSpeechLikeWavFile()

    println(
      s"""
         |Created test audio files:
         |  Silence WAV: ${silenceWavFile} (${Files.size(silenceWavFile)} bytes)
         |  Tone WAV: ${toneWavFile} (${Files.size(toneWavFile)} bytes)
         |  Speech-like WAV: ${speechLikeWavFile} (${Files.size(speechLikeWavFile)} bytes)
         |""".stripMargin
    )

    println("\n--- Testing STT (Mock) ---")
    demoWhisper(silenceWavFile.toString)
    demoVosk(silenceWavFile.toString)

    println("\n--- Testing STT (Real Whisper - Silence) ---")
    println("Note: This requires 'whisper' CLI to be installed (pip install openai-whisper)")
    demoWhisperReal(silenceWavFile.toString)

    println("\n--- Testing STT (Real Whisper - Tone) ---")
    println("Testing with a 440Hz tone (A note) - should be more interesting for Whisper")
    demoWhisperReal(toneWavFile.toString)

    println("\n--- Testing STT (Real Whisper - Speech-like) ---")
    println(
      "Testing with a speech-like modulated tone (200Hz base + 50Hz mod) - should be more interesting for Whisper"
    )
    demoWhisperReal(speechLikeWavFile.toString)

    println("\n--- Testing TTS ---")
    demoTacotron2("Hello from LLM4S")

    // Cleanup
    Files.deleteIfExists(silenceWavFile)
    Files.deleteIfExists(toneWavFile)
    Files.deleteIfExists(speechLikeWavFile)
    println("\n=== Demo Complete ===")
  }
}
