#include <stdio.h>
#include <stddef.h>
#include "../src/tinytemplate.h"

/* this lets the source compile without afl-clang-fast/lto */
#ifndef __AFL_FUZZ_TESTCASE_LEN

ssize_t       fuzz_len;
unsigned char fuzz_buf[1024000];

#define __AFL_FUZZ_TESTCASE_LEN fuzz_len
#define __AFL_FUZZ_TESTCASE_BUF fuzz_buf
//#define __AFL_FUZZ_INIT() void sync(void);
//#define __AFL_LOOP(x) ((fuzz_len = read(0, fuzz_buf, sizeof(fuzz_buf))) > 0 ? 1 : 0)
//#define __AFL_INIT() sync()

#endif

__AFL_FUZZ_INIT();

//#pragma clang optimize off
//#pragma GCC            optimize("O0")

static void callback(void *userp, const char *label, size_t label_len,
                     const char *str, size_t len)
{
    (void) userp;
    (void) label;
    (void) label_len;
    (void) str;
    (void) len;
}

int main(void) 
{

#ifdef __AFL_HAVE_MANUAL_CONTROL
    __AFL_INIT();
#endif

    unsigned char *buf = __AFL_FUZZ_TESTCASE_BUF;   // must be after __AFL_INIT
                                                    // and before __AFL_LOOP!
    tinytemplate_instr_t program[1024];
    size_t max_instr = sizeof(program)/sizeof(program[0]);

    while (__AFL_LOOP(10000)) {

        int len = __AFL_FUZZ_TESTCASE_LEN;  // don't use the macro directly in a
                                            // call!

        tinytemplate_status_t status;

        status = tinytemplate_compile((char*) buf, len, program, max_instr, NULL, NULL, 0);
        if (status == TINYTEMPLATE_STATUS_DONE)
            status = tinytemplate_eval((char*) buf, program, NULL, NULL, callback, NULL, 0);
    }

    return 0;
}