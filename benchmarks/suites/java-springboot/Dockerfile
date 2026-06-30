# Estágio de build
FROM maven:3.9-eclipse-temurin-17-alpine AS build
WORKDIR /app

# Copiar POM e baixar dependências (cache)
COPY benchmarks/suites/java-springboot/pom.xml ./
RUN mvn dependency:go-offline

# Copiar fontes e compilar
COPY benchmarks/suites/java-springboot/src ./src
RUN mvn package -DskipTests

# Estágio de runtime
FROM eclipse-temurin:17-jre-alpine
WORKDIR /app

COPY --from=build /app/target/bench-1.0.0.jar ./app.jar

ENV SERVER_PORT=9090
EXPOSE 9090

ENTRYPOINT ["java", "-jar", "app.jar"]
