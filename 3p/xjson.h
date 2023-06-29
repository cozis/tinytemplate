#ifndef XJSON_H
#define XJSON_H

typedef double    xj_f64;
typedef long long xj_i64;
typedef _Bool     xj_bool;

_Static_assert(sizeof(xj_f64) == 8, "double isn't 8 bytes long");
_Static_assert(sizeof(xj_i64) == 8, "long long isn't 8 bytes long");

enum {
    XJ_NULL,
    XJ_BOOL,
    XJ_INT,
    XJ_FLOAT,
    XJ_ARRAY,
    XJ_OBJECT,
    XJ_STRING,
};

typedef struct xj_value xj_value;
struct xj_value {
    int       type;
    int       size;
    xj_value *next;
    char     *key;
    union {
        xj_i64    as_int;
        xj_bool   as_bool;
        xj_f64    as_float;
        xj_value *as_array;
        xj_value *as_object;
        char     *as_string;
    };
};

typedef struct {
    xj_bool occurred;
    xj_bool truncated;
    int off, row, col;
    char message[128];
} xj_error;

typedef struct xj_alloc xj_alloc;
xj_alloc *xj_alloc_using(void *mem, int size, int ext, void (*free)(void*));
xj_alloc *xj_alloc_new(int size, int ext);
void      xj_alloc_del(xj_alloc *alloc);

void     *xj_bpalloc(xj_alloc *alloc, int size);
void      xj_preport(xj_error *error, const char *src, int off, const char *fmt, ...);
#define   xj_report(error, fmt, ...) xj_preport(error, NULL, -1, fmt, ## __VA_ARGS__)

xj_value *xj_value_null(xj_alloc *alloc, xj_error *error);
xj_value *xj_value_bool(xj_bool val, xj_alloc *alloc, xj_error *error);
xj_value *xj_value_int(xj_i64 val, xj_alloc *alloc, xj_error *error);
xj_value *xj_value_float(xj_f64 val, xj_alloc *alloc, xj_error *error);
xj_value *xj_value_array(xj_value *head, xj_alloc *alloc, xj_error *error);
xj_value *xj_value_object(xj_value *head, xj_alloc *alloc, xj_error *error);
xj_value *xj_value_string(const char *str, int len, xj_alloc *alloc, xj_error *error);
xj_value *xj_value_array__nocheck(xj_value *head, int count, xj_alloc *alloc, xj_error *error);
xj_value *xj_value_object__nocheck(xj_value *head, int count, xj_alloc *alloc, xj_error *error);

_Bool     xj_array_append(xj_value *array, xj_value *child, xj_error *error);

char     *xj_strdup(const char *str, int len, xj_alloc *alloc, xj_error *error);

xj_value *xj_decode(const char *str, int len, xj_alloc *alloc, xj_error *error);
char     *xj_encode(xj_value *value, int *len);

#endif /* XJSON_H */