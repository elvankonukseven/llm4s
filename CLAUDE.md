# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# LLM4S Development Guidelines

## Build & Test Commands
```bash
# Build the project (Scala 3)
sbt compile

# Build for all Scala versions (2.13 and 3)
sbt +compile

# Build and test all versions  
sbt buildAll

# Run a specific sample 
sbt "samples/runMain org.llm4s.samples.basic.BasicLLMCallingExample"

# Run a sample with Scala 2.13
sbt ++2.13.16 "samples/runMain org.llm4s.samples.basic.BasicLLMCallingExample"

# Run tests for the current Scala version
sbt test

# Run tests for all Scala versions
sbt +test

# Run a single test
sbt "testOnly org.llm4s.shared.WorkspaceAgentInterfaceTest"

# Format code
sbt scalafmtAll

# Publish local for cross-testing
sbt +publishLocal

# Run containerized workspace tests
sbt docker:publishLocal
sbt "samples/runMain org.llm4s.samples.workspace.ContainerisedWorkspaceDemo"

# Cross-version compatibility testing
sbt testCross
sbt fullCrossTest
```

## Cross Compilation Guidelines
- The project supports both Scala 2.13.16 and Scala 3.7.1
- Common code goes in `src/main/scala`
- Scala 2.13-specific code goes in `src/main/scala-2.13`
- Scala 3-specific code goes in `src/main/scala-3`
- Always test with both versions: `sbt +test`
- Use the cross-building commands: `buildAll`, `testAll`, `compileAll`
- Cross-test modules verify compatibility: see `crossTest/` directories

## Code Style Guidelines
- **Formatting**: Follow `.scalafmt.conf` settings (120 char line length)
- **Imports**: Use curly braces for imports (`import { x, y }`)
- **Error Handling**: Use `Result[T]` type (alias for `Either[LLMError, T]`) for operations that may fail
- **Types**: Prefer immutable data structures and pure functions
- **Naming**: Use camelCase for variables/methods, PascalCase for classes/objects
- **Documentation**: Use Asterisk style (`/** ... */`) for ScalaDoc comments
- **Code Organization**: Keep consistent with existing package structure
- **Functional Style**: Prefer pattern matching over if/else statements

## High-Level Architecture

### Core Modules
- **llm4s**: Main framework with LLM providers, agents, tools, and tracing
- **shared**: Common code between main project and workspace runner
- **workspaceRunner**: Containerized execution environment for secure tool execution
- **samples**: Usage examples for various features

### Key Components

#### LLM Connectivity (`org.llm4s.llmconnect`)
- **LLMConnect**: Factory for creating LLM clients based on environment configuration
- **LLMClient**: Core interface for LLM interactions
- **Providers**: OpenAI, Anthropic, Azure, OpenRouter implementations
- **Configuration**: Provider-specific configs loaded from environment

#### Type System (`org.llm4s.types`)
- **Result[T]**: Core error handling type (`Either[LLMError, T]`)
- **Type-safe IDs**: ModelName, ProviderName, ApiKey, ConversationId, etc.
- **Async Types**: AsyncResult, CompletionStream, StreamingCallback
- **Smart Constructors**: Validation and creation of type-safe values

#### Error Hierarchy (`org.llm4s.error`)
- **LLMError**: Base trait with structured context and recovery guidance
- **RecoverableError**: Network errors, rate limits, processing errors
- **NonRecoverableError**: Auth failures, configuration errors, validation errors
- **ErrorBridge**: Compatibility layer for legacy error types

#### Agent Framework (`org.llm4s.agent`)
- **Agent**: Orchestrates tool-calling conversations with LLMs
- **AgentState**: Immutable state tracking conversations and tool executions
- **Tool Integration**: Automatic tool discovery and execution

#### Tool System (`org.llm4s.toolapi`)
- **ToolRegistry**: Manages available tools and their schemas
- **ToolFunction**: Type-safe tool definition with parameters
- **SafeParameterExtractor**: Extracts and validates tool parameters
- **WorkspaceTools**: Containerized tool execution for safety

#### Tracing (`org.llm4s.trace`)
- **Multiple Backends**: Langfuse, Console, NoOp
- **Enhanced Tracing**: Detailed token usage and performance metrics
- **Configuration**: TRACING_MODE environment variable

#### MCP Support (`org.llm4s.mcp`)
- **MCPClient**: Model Context Protocol client implementation
- **MCPToolRegistry**: Tool discovery via MCP
- **MCPTransport**: WebSocket/stdio transport layers

#### Multimodal Support
- **Image Generation** (`org.llm4s.imagegeneration`): OpenAI DALL-E, Stable Diffusion, HuggingFace
- **Image Processing** (`org.llm4s.imageprocessing`): Vision APIs for OpenAI and Anthropic
- **Embeddings** (`org.llm4s.llmconnect`): Text embeddings for RAG applications

## Environment Setup
Required environment variables:
```bash
# LLM Provider (pick one)
LLM_MODEL=openai/gpt-4o  # or anthropic/claude-3-sonnet-latest
OPENAI_API_KEY=sk-...    # for OpenAI
ANTHROPIC_API_KEY=sk-... # for Anthropic

# Optional
TRACING_MODE=langfuse|print|none
LANGFUSE_PUBLIC_KEY=...
LANGFUSE_SECRET_KEY=...
```

## Testing Strategy
- Unit tests use ScalaTest with ScalaMock
- Cross-version testing via dedicated test projects
- Containerized workspace tests for tool execution
- Integration tests for LLM providers (require API keys)