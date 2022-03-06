---
title: Request Lifecycle
---

# First Steps

Nginx -> `public/index.php`
-> `bootstrap/app.php` (instance of the Laravel application)
-> bootstrap service containers.

# HTTP / Console Kernels

Request -> HTTP kernel at `app/Http/Kernel.php` (or console kernel)

Main method:

```
HttpKernel.handle: (Request) -> Response
```

Responsibilities of HTTP kernel:

- error handling
- configure logging
- detect the application environment
- ...

Which also defines middlewares:

- reading and writing the HTTP session
- determining if the application is in maintenance mode
- verifying the CSRF token
- auth
- ...

# Service Providers

Configured in `config/app.php`'s `providers` array.

Main methods:

```
ServiceProvider.register
ServiceProvider.boot
```

Responsibilities:

- **dependency injection**
- database
- queue
- validation
- routing
- ...

Every feature offered by Laravel
is bootstrapped and configured by a service provider.

# Routing

`App\Providers\RouteServiceProvider`

Responsibilities:

- load the route files (in `/routes`)
- dispatch Request by router

# Look Back

When we write an application,
we almost always need dependency injection,
we can learn from Laravel's way of organizing code.

When we solve a problem using client-server pattern
(or say `Request` & `Response` pattern),
we can use route to dispatch `Request`,
and use middlewares to filter and post-process `Request`.
