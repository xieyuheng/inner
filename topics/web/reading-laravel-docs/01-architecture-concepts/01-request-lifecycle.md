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
handle: (Request) -> Response
```

Responsibilities of HTTP kernel:

- error handling
- configure logging
- detect the application environment
- ...

Which also specifies middlewares:

- reading and writing the HTTP session
- determining if the application is in maintenance mode
- verifying the CSRF token
- ...
