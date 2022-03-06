---
title: flask
---

# getting start

## route

relation between url and view function
@app.route(<path>)

## view

view functions are registered by @app.route(<path>)
Request is passed to the function
by a dynamicly scoped variable

## Request

a dynamicly scoped variable
flask.request
can be used by view funciton

fields of Request :
TODO

methods of Request :
TODO

## Response

a view function creates and returns a Response object

fields of Response :
TODO

methods of Response :
TODO

sugars for returning Response object :

1. If a response object is returned
   it’s directly returned from the view.

   - x -
     if the explicit syntax is not over complicated
     I will always use the explicit syntax.

2. If it’s a string,
   a response object is created
   with that data and the default parameters.

3. if (response, status, headers)
   or (response, headers) is returned,
   The status value will override the status code
   and headers can be a list or dictionary of additional header values.

4. If none of that works,
   Flask will assume the return value is a valid WSGI application
   and convert that into a response object.

## Session -- wraped cookie

## API

## Application object

such an object is used to register all informations,
the name of which must be module or package name.
