package main

import (
	"log"

	"github.com/gofiber/fiber/v2"
)

func main() {
	app := fiber.New(fiber.Config{
		DisableStartupMessage: true,
	})

	app.Get("/ping", func(c *fiber.Ctx) error {
		c.Set("Content-Type", "text/plain")
		return c.SendString("pong")
	})

	log.Println("Servidor Go/Fiber rodando na porta 9090...")
	log.Fatal(app.Listen(":9090"))
}
