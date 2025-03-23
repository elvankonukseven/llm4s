package org.llm4s.samples.agent

import org.llm4s.agent.Agent
import org.llm4s.llmconnect.LLM
import org.llm4s.toolapi.ToolRegistry
import org.llm4s.toolapi.tools.WeatherTool

/**
 * Example demonstrating complete agent execution with multiple steps
 */
object MultiStepAgentExample {
  def main(args: Array[String]): Unit = {
    // Get a client using environment variables
    val client = LLM.client()

    // Create a tool registry
    val toolRegistry = new ToolRegistry(Seq(WeatherTool.tool))

    // Create an agent
    val agent = new Agent(client)

    // Define a multi-step query that requires comparing weather in different locations
    val query = "What's the weather like in London, and is it different from New York?"
    println(s"User Query: $query\n")

    println("=== Running Multi-Step Agent to Completion ===\n")

    // Example 1: Run the agent with no step limit
    println("Example 1: Running without a step limit")
    agent.run(query, toolRegistry, None) match {
      case Right(finalState) =>
        println(s"Final status: ${finalState.status}")

        // Print the final answer
        finalState.conversation.messages.last match {
          case msg if msg.role == "assistant" =>
            println("\nFinal Answer:")
            println(msg)

          case _ => println("No final answer found")
        }

        // Print execution info
        println(s"\nTotal steps executed: ${finalState.logs.size}")

        // Print logs with formatted output
        if (finalState.logs.nonEmpty) {
          println("\nExecution logs:")
          finalState.logs.foreach { log =>
            // Color-coded logs based on type
            val colorCode = log match {
              case l if l.startsWith("[assistant]") => Console.BLUE
              case l if l.startsWith("[tool]")      => Console.GREEN
              case l if l.startsWith("[tools]")     => Console.YELLOW
              case l if l.startsWith("[system]")    => Console.RED
              case _                                => Console.WHITE
            }

            println(s"${colorCode}${log}${Console.RESET}")
          }
        }

        // Dump the complete agent state for debugging
        println("\n=== Complete Agent State Dump ===\n")
        finalState.dump()

      case Left(error) =>
        println(s"Error running agent: $error")
    }

    // Example 2: Run the agent with a step limit of 1
    println("\n\n=== Running Multi-Step Agent with Step Limit ===\n")
    println("Example 2: Running with a step limit of 1")

    agent.run(query, toolRegistry, Some(1)) match {
      case Right(finalState) =>
        println(s"Final status: ${finalState.status}")

        // Print execution info
        println(s"\nTotal steps executed: ${finalState.logs.size}")

        // Print logs with formatted output
        if (finalState.logs.nonEmpty) {
          println("\nExecution logs:")
          finalState.logs.foreach { log =>
            // Color-coded logs based on type
            val colorCode = log match {
              case l if l.startsWith("[assistant]") => Console.BLUE
              case l if l.startsWith("[tool]")      => Console.GREEN
              case l if l.startsWith("[tools]")     => Console.YELLOW
              case l if l.startsWith("[system]")    => Console.RED
              case _                                => Console.WHITE
            }

            println(s"${colorCode}${log}${Console.RESET}")
          }
        }

      case Left(error) =>
        println(s"Error running agent: $error")
    }

    // Example 3: Manual step execution to show the two-phase flow
    println("\n\n=== Manual Step Execution to Demonstrate Two-Phase Flow ===\n")
    println("Example 3: Running with manual step execution")

    // Initialize the agent state
    val initialState = agent.initialize(query, toolRegistry)
    println(s"Initial state: ${initialState.status}")

    // Run first LLM step
    println("\nStep 1: Running LLM completion (usually generates tool calls)")
    val afterLLMStep = agent.runStep(initialState) match {
      case Right(state) =>
        println(s"After LLM step, status: ${state.status}")
        state
      case Left(error) =>
        println(s"Error: $error")
        return
    }

    // Display messages so far
    println("\nConversation after LLM step:")
    afterLLMStep.conversation.messages.foreach(msg => println(s"[${msg.role}] ${msg.content}"))

    // Run the tool execution step
    println("\nStep 2: Processing tool calls")
    val afterToolStep = agent.runStep(afterLLMStep) match {
      case Right(state) =>
        println(s"After tool execution step, status: ${state.status}")
        state
      case Left(error) =>
        println(s"Error: $error")
        return
    }

    // Display tool results
    println("\nConversation after tool execution:")
    afterToolStep.conversation.messages.takeRight(2).foreach { msg =>
      println(s"[${msg.role}] ${if (msg.role == "tool") msg.content.take(50) + "..." else msg.content}")
    }

    println("\nThe two-phase flow allows for more control and separation of concerns in the agent execution.")
  }
}
