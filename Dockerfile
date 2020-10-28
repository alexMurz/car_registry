FROM hseeberger/scala-sbt:graalvm-ce-20.0.0-java8_1.3.13_2.11.12 as build

# Build
RUN mkdir /src
COPY . /src
WORKDIR /src

RUN sbt clean stage

# User
RUN mkdir /app

# Copy
FROM openjdk:latest
COPY --from=build /src/target/universal/stage/ /app
COPY --from=build /src/assets/* /app/assets/

# Entry Point
WORKDIR /app
CMD ["sh", "-c", "bin/scala_server"]