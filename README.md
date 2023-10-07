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

## Template language features
The templating language is extremely simple. You can use "if .. else" and "for" constructs but can't define new wariables or mutate already defined ones. TinyTemplate only behaves as a view of data provided by the parent application so it doesn't need to do any processing. 

## Build
To build it you run 
```sh
make
```
which will generate the `tt` file, the command-line utility.

## Embed
If you want to embed TinyTemplate in your own C project, just pop `tinytemplate.c` and `tinytemplate.h` in you own source tree and compile them as they were your own.

## Command-line usage
The `tt` utility gets the template string from stdin and writes the evaluated version to stdout. Any errors are reported to stderr, as you would expect. To provide any parameters to the template, you can specify a json file containing them. Here's an example usage which transforms a template into an HTML page

```sh
cat page.tt | ./tt params.json > page.html
```

## Library usage
TinyTemplate works by compiling a template string into a bytecode program, and then evaluating the program to generate the output. The library only exports two functions:

```c
tinytemplate_status_t 
tinytemplate_eval(const char *src, const tinytemplate_instr_t *program, 
                  void *userp, tinytemplate_getter_t params,
                  tinytemplate_callback_t callback,
                  char *errmsg, size_t errmax);

tinytemplate_status_t 
tinytemplate_compile(const char *src, size_t len, 
                     tinytemplate_instr_t *program,
                     size_t max_instr, size_t *num_instr,
                     char *errmsg, size_t errmax);
```

First you need to compile the template into a program and then you can evaluate it by executing the program.

### Compilation
The compilation function expects a source string of the template through `src` and with length in bytes `len`. The string doesn't need to be zero-terminated. The program will be compilated by writing its instructions in the `prog` buffer. The `prog` array is assumed to have space for a maximum of `max_instr` instructions. If more space would be required to memorize the program, the compilation fails returning the status code `TINYTEMPLATE_STATUS_EMEMORY`. At that point the caller program can either abort or try again with a bigger buffer. If compilation succeded, then the number of instruction is written to the variable `num_instr`. A status code of the compilation is returned through the main return value. The status `TINYTEMPLATE_STATUS_DONE` means all went well, while all other status codes refer to a specific error that occurred. If the compilation failed, an error message is written to the caller-provided buffer `errmsg` whith size `errmax`.

Here's an example of how you would use this function:
```c
#define COUNT(X) ((int) (sizeof(X) / sizeof((X)[0])))

int main(void)
{
    char message[128];
    tinytemplate_instr_t prog[32];
    tinytemplate_status_t status;

    static const char text[] = "Hello, my name is {{name}}!";

    size_t num_instr;
    status = tinytemplate_compile(text, strlen(text), program, COUNT(prog),
                                  NULL, message, sizeof(message));
    if (status != TINYTEMPLATE_STATUS_DONE) {
        fprintf(stderr, "Error: %s", message);
        return -1;
    }

    fprintf(stdout, "Program compiled to %ld instructions!\n", num_instr);
    return 0;
}
```

### Evaluation
To evaluate a program you need to provide both the program and the source string of the template. Also the caller must specify two callbacks: `params` and `callback`. The `params` callback, which has the following interface:
```c
bool params(void *data, const char *key, size_t len,
            tinytemplate_value_t *value);
```
provides the values of the parameters references by the template. For example if defining the parameter template as following:
```c
bool params(void *data, const char *key, size_t len, 
            tinytemplate_value_t *value)
{
    if (len == 3 && !strncmp(key, "age", len)) {
        tinytemplate_set_int(value, 24);
        return true;
    }
    return false;
}
```
the value `24` will be associated to the parameter `age`. All other parameters will be undefined.

The other callback `callback` is used by TinyTemplate to output the result of the evaluation. Instead of evaluating the result to a buffer and then returning a single pointer, it calls this function to notify the caller that some bytes should be written to output. This function usually writes the bytes provided by the library to a caller-owned buffer. Here's an example callback that redirects the evaluation result to `stdout`
```c
void callback(void *data, const char *str, size_t len)
{
    fwrite(str, 1, len, stdout);
}
```
The argument `data` of the two callbacks is always the pointer provided to the `tinytemplate_eval` function through `userp`.