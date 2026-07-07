---
name: horse-security-auth
description: Guide for securing routes, configuring JWT and Basic-Auth middlewares, and handling route authentication groups.
---

# Horse Security & Authentication

## 1. Organizing Public and Protected Routes
Always separate public endpoints (e.g., authentication, login, system health) from protected ones. Use `THorse.Group` to configure middleware layers specifically for protected routes without affecting public endpoints:

```pascal
begin
  // Global Middlewares (CORS, Johnson, etc.)
  THorse.Use(CORS).Use(Jhonson);

  // 1. Public Routes
  THorse.Post('/login', DoLoginHandler);
  THorse.Get('/health', GetHealthHandler);

  // 2. Protected Routes (Using a Group with JWT Middleware)
  THorse.Group.Prefix('/api/v1')
    .Use(HorseJWT('my_secret_key')) // Authenticates all routes within this group
    .Get('/customers', GetCustomersHandler)
    .Get('/orders', GetOrdersHandler);

  THorse.Listen(9000);
end;
```

---

## 2. Setting Up JWT (JSON Web Token) Authentication
The official JWT middleware is [`horse-jwt`](https://github.com/HashLoad/horse-jwt).

### Acquiring Token Payload in Handlers
Once `HorseJWT` authenticates the request, it decodes the payload and stores it in the `Session` property of `THorseRequest`. You can retrieve claims using the `Session` dictionary:

```pascal
procedure GetProfileHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LClaims: TJWTClaims; // Or TJSONObject depending on the horse-jwt version used
  LUserId: string;
begin
  // Example of pulling JWT claims
  LUserId := Req.Session<TJSONObject>.GetValue<string>('sub');
  
  Res.Send(Format('Hello, User %s!', [LUserId]));
end;
```

---

## 3. Setting Up Basic Authentication
For quick, credential-based authentication, use the official [`horse-basic-auth`](https://github.com/HashLoad/horse-basic-auth) middleware:

```pascal
uses Horse.BasicAuth;

begin
  // Protect all API routes with Basic-Auth
  THorse.Group.Prefix('/api')
    .Use(HorseBasicAuth(
      function(AUsername, APassword: string): Boolean
      begin
        // Replace with secure database/credential lookup
        Result := (AUsername = 'admin') and (APassword = 'secret123');
      end
    ))
    .Get('/secrets', GetSecretsHandler);
end;
```

---

## 4. Security Best Practices
1.  **Secret Management**: Never hardcode encryption keys directly in the source code. Always pull them from Environment Variables or external configurations (e.g., `GetEnvironmentVariable('JWT_SECRET_KEY')`).
2.  **JWT Lifetime**: Set short expiration times (`exp` claim) on issued JWTs and implement a refresh token pattern for long-running client sessions.
3.  **Transport Security**: Never transmit JWTs or credentials over HTTP. Always enforce HTTPS (SSL/TLS) in production environments.
