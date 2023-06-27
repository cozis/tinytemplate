# TinyTemplate
TinyTemplate is a minimal templating engine for C. It can be used as a library or through the command-line interface.

* Very minimal in functionality
* Implemented in one file (plus header)
* No dynamic allocations
* Only depends on libc

## How it looks
Here's how it looks:
```html
<html>
    <head>
        <title>{{username}}s blog!</title>
    </head>
    <body>
        <p>Hei, welcome to my blog! I'm {{name}} {{surname}} and I'm {{age}} years old!</p>
        <ul>
        {% for post in posts %}
            <li>{{post.title}} - {{post.date}}</li>
        {% end %}
        </ul>
    </body>
</html>
```
if you've already used some templating engines, then
this won't blow your mind.

## Build
To build it you run 
```sh
make
```
which will generate the `tt` file, the command-line utility.

## Embed
If you want to embed TinyTemplate in your own C project,
just pop `tinytemplate.c` and `tinytemplate.h` in you own
source tree and compile then as they were your own.

## Command-line usage
The `tt` utility gets the template string from stdin and writes the evaluated version to stdout. Any errors are
reported to stderr, as you would expect. To provide any
parameters to the template, you can specify a json file
containing them. Here's an example usage which transforms
a template into an HTML page

```sh
cat page.tt | ./tt params.json > page.html
```

## Internals
TinyTemplate works by compiling a template string into a bytecode program, and then evaluating the program to generate the output. 