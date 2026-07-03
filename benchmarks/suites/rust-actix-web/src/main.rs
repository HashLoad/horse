use actix_web::{get, App, HttpResponse, HttpServer, Responder};

#[get("/ping")]
async fn ping() -> impl Responder {
    HttpResponse::Ok()
        .content_type(actix_web::http::header::ContentType::plaintext())
        .body("pong")
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new().service(ping)
    })
    .bind(("0.0.0.0", 9090))?
    .run()
    .await
}
