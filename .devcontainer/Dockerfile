# Java 21 base with devcontainer conveniences
FROM mcr.microsoft.com/devcontainers/java:21

# Install sbt + Scala tools the reliable way (coursier)
# Also install bash conveniences and docker-cli deps
USER root
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      unzip zip gnupg ca-certificates curl git jq yj && \
    rm -rf /var/lib/apt/lists/*

# Install coursier (cs)
ENV COURSIER_VERSION=2.1.9
RUN curl -fL -o /usr/local/bin/cs \
      "https://github.com/coursier/coursier/releases/download/v${COURSIER_VERSION}/cs-x86_64-pc-linux.gz" && \
    gunzip /usr/local/bin/cs && chmod +x /usr/local/bin/cs

# Install sbt, scala, scalafmt, sbtn via coursier
RUN cs setup --yes && \
    cs install sbt scala scalafmt sbtn

# Make sure sbt uses enough memory
ENV SBT_OPTS="-Xms512m -Xmx4g -Xss2m -XX:+UseG1GC -Duser.timezone=UTC"

# Pre-create workspace directory
WORKDIR /workspaces/llm4s
