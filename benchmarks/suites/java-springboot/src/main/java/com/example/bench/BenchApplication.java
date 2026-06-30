package com.example.bench;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@SpringBootApplication
@RestController
public class BenchApplication {

    public static void main(String[] args) {
        System.setProperty("logging.level.root", "WARN");
        SpringApplication.run(BenchApplication.class, args);
    }

    @GetMapping(value = "/ping", produces = "text/plain")
    public String ping() {
        return "pong";
    }
}
