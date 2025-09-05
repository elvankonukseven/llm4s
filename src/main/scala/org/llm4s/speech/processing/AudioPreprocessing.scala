package org.llm4s.speech.processing

import org.llm4s.error.ProcessingError
import org.llm4s.types.Result
import org.llm4s.speech.{ AudioMeta, AudioFormat, GeneratedAudio }
import org.llm4s.speech.io.BinaryReader._

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, IOException }
import javax.sound.sampled.{
  AudioFormat => JAudioFormat,
  AudioInputStream,
  AudioSystem,
  UnsupportedAudioFileException,
  LineUnavailableException
}

/**
 * Functional audio preprocessing utilities.
 * These are pure transformations described as functions that return either errors or processed audio.
 */
object AudioPreprocessing {

  /** Resample PCM16 little-endian bytes to target sample rate using Java Sound. */
  def resamplePcm16(bytes: Array[Byte], source: AudioMeta, targetRate: Int): Result[(Array[Byte], AudioMeta)] =
    try {
      val srcFormat = new JAudioFormat(
        source.sampleRate.toFloat,
        source.bitDepth,
        source.numChannels,
        /* signed = */ true,
        /* bigEndian = */ false
      )
      val srcAis =
        new AudioInputStream(new ByteArrayInputStream(bytes), srcFormat, bytes.length / srcFormat.getFrameSize)
      val dstFormat = new JAudioFormat(
        targetRate.toFloat,
        source.bitDepth,
        source.numChannels,
        /* signed = */ true,
        /* bigEndian = */ false
      )
      val converted = AudioSystem.getAudioInputStream(dstFormat, srcAis)
      val out       = new ByteArrayOutputStream(bytes.length)
      val buf       = new Array[Byte](8192)
      var read      = 0
      while ({ read = converted.read(buf); read } != -1) out.write(buf, 0, read)
      Right(out.toByteArray -> source.copy(sampleRate = targetRate))
    } catch {
      case ex: UnsupportedAudioFileException =>
        Left(
          ProcessingError.audioResample(
            s"Unsupported audio format: ${source.bitDepth}-bit, ${source.numChannels} channels",
            Some(ex)
          )
        )
      case ex: LineUnavailableException =>
        Left(ProcessingError.audioResample("Audio line unavailable", Some(ex)))
      case ex: IOException =>
        Left(ProcessingError.audioResample("IO error during resampling", Some(ex)))
      case ex: IllegalArgumentException =>
        Left(ProcessingError.audioResample(s"Invalid audio parameters: rate=$targetRate", Some(ex)))
      case ex: Exception =>
        Left(ProcessingError.audioResample("Resample operation failed", Some(ex)))
    }

  /** Convert to mono by averaging channels (PCM16 little-endian). */
  def toMono(bytes: Array[Byte], meta: AudioMeta): Result[(Array[Byte], AudioMeta)] =
    if (meta.numChannels <= 1) Right((bytes, meta))
    else
      try {
        val frameSize     = (meta.bitDepth / 8) * meta.numChannels
        val numFrames     = bytes.length / frameSize
        val monoFrameSize = meta.bitDepth / 8
        val out           = new Array[Byte](numFrames * monoFrameSize)

        (0 until numFrames).foreach { frameIndex =>
          val sum = (0 until meta.numChannels).foldLeft(0) { (acc, ch) =>
            val base = frameIndex * frameSize + ch * (meta.bitDepth / 8)
            // Use implicit binary reader for cleaner code
            val (sample, _) = bytes.read[Short](base)
            acc + sample.toInt
          }

          val avg: Short   = (sum / meta.numChannels).toShort
          val outByteIndex = frameIndex * 2

          // Use implicit binary writer for cleaner little-endian writing
          val tempOut = new ByteArrayOutputStream(2)
          val dos     = new java.io.DataOutputStream(tempOut)
          dos.write(avg)
          val avgBytes = tempOut.toByteArray
          out(outByteIndex) = avgBytes(0)
          out(outByteIndex + 1) = avgBytes(1)
        }

        Right(out -> meta.copy(numChannels = 1))
      } catch {
        case ex: ArrayIndexOutOfBoundsException =>
          Left(
            ProcessingError.audioConversion(
              s"Invalid audio data size: expected multiple of ${(meta.bitDepth / 8) * meta.numChannels} bytes",
              Some(ex)
            )
          )
        case ex: ArithmeticException =>
          Left(ProcessingError.audioConversion("Arithmetic error (division by zero channels)", Some(ex)))
        case ex: IOException =>
          Left(ProcessingError.audioConversion("IO error during mono conversion", Some(ex)))
        case ex: Exception =>
          Left(ProcessingError.audioConversion("Mono conversion failed", Some(ex)))
      }

  /** Trim leading and trailing silence using a simple amplitude threshold on PCM16. */
  def trimSilence(bytes: Array[Byte], meta: AudioMeta, threshold: Int = 512): Result[(Array[Byte], AudioMeta)] =
    try {
      val sampleSize = meta.bitDepth / 8
      val frameSize  = sampleSize * meta.numChannels
      val numFrames  = bytes.length / frameSize
      def frameLoud(frameIdx: Int): Boolean = {
        val maxAmplitude = (0 until meta.numChannels).foldLeft(0) { (max, ch) =>
          val base = frameIdx * frameSize + ch * sampleSize
          // Use implicit binary reader for cleaner code
          val (sample, _) = bytes.read[Short](base)
          val amplitude   = math.abs(sample.toInt)
          math.max(max, amplitude)
        }
        maxAmplitude >= threshold
      }
      val start = {
        var s = 0
        while (s < numFrames && !frameLoud(s)) s += 1
        s
      }
      val end = {
        var e = numFrames - 1
        while (e >= start && !frameLoud(e)) e -= 1
        e
      }
      val outStart = start * frameSize
      val outEnd   = (end + 1) * frameSize
      val sliced =
        if (outEnd > outStart) java.util.Arrays.copyOfRange(bytes, outStart, outEnd) else Array.emptyByteArray
      Right(sliced -> meta)
    } catch {
      case ex: ArrayIndexOutOfBoundsException =>
        Left(
          ProcessingError.audioTrimming(
            s"Invalid audio data size: expected multiple of ${(meta.bitDepth / 8) * meta.numChannels} bytes",
            Some(ex)
          )
        )
      case ex: IllegalArgumentException =>
        Left(ProcessingError.audioTrimming(s"Invalid threshold value: $threshold", Some(ex)))
      case ex: Exception =>
        Left(ProcessingError.audioTrimming("Silence trimming failed", Some(ex)))
    }

  /** Compose multiple steps functionally */
  def standardizeForSTT(
    bytes: Array[Byte],
    meta: AudioMeta,
    targetRate: Int = 16000
  ): Result[(Array[Byte], AudioMeta)] =
    for {
      mono       <- toMono(bytes, meta)
      resampled  <- resamplePcm16(mono._1, mono._2, targetRate)
      normalized <- trimSilence(resampled._1, resampled._2)
    } yield normalized

  def wrap(bytes: Array[Byte], meta: AudioMeta, format: AudioFormat = AudioFormat.WavPcm16): GeneratedAudio =
    GeneratedAudio(bytes, meta, format)
}
