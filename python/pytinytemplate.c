#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include "tinytemplate.h"

typedef struct {
    PyObject_HEAD
    PyObject *src;
    tinytemplate_instr_t *program;
} Template;

static void
TinyTemplate_dealloc(CustomObject *self)
{
    Py_XDECREF(self->src);
    Py_TYPE(self)->tp_free((PyObject *) self);
}

static PyObject *
TinyTemplate_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    Template *self;
    self = (Template *) type->tp_alloc(type, 0);
    if (self != NULL) {
        self->src = PyUnicode_FromString("");
        if (self->src == NULL) {
            Py_DECREF(self);
            return NULL;
        }
    }
    return (PyObject *) self;
}

static int
TinyTemplate_init(Template *self, PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = {"src", NULL};
    PyObject *src = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "|O", kwlist, &src))
        return -1;

    if (src) {
        PyObject *tmp = self->src;
        Py_INCREF(src);
        self->src = src;
        Py_XDECREF(tmp);
    }
    return 0;
}

static PyObject *
TinyTemplate_eval(Template *self, PyObject *args)
{
    PyObject *src = self->src;

    if (src == NULL) {
        PyErr_SetString(PyExc_AttributeError, "src");
        return NULL;
    }
    if (!PyUnicode_Check(src))
        return NULL; // Raise exception?

    PyObject *bytes = PyUnicode_AsEncodedString(src, "UTF-8", "strict"); // Owned reference
    if (bytes == NULL) {
        ..
        return NULL;
    }

    char *cstr = PyBytes_AS_STRING(bytes);

    ..

}

static PyMemberDef TinyTemplate_members[] = {
    {"src", T_OBJECT_EX, offsetof(Template, src), 0, "source string"},
    {NULL}  /* Sentinel */
};

static PyMethodDef TinyTemplate_methods[] = {
    {"eval", (PyCFunction) TinyTemplate_eval, METH_NOARGS, "Evaluate the template" },
    {NULL}  /* Sentinel */
};

static PyTypeObject TemplateType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "tinytemplate.Template",
    .tp_doc = PyDoc_STR("A template"),
    .tp_basicsize = sizeof(Template),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new = TinyTemplate_new,
    .tp_init = (initproc) TinyTemplate_init,
    .tp_dealloc = (destructor) TinyTemplate_dealloc,
    .tp_members = TinyTemplate_members,
    .tp_methods = TinyTemplate_methods,
};

static PyObject*
compile(PyObject *self, PyObject *args)
{
    PyObject *src;
    if (!PyArg_ParseTuple(args, "o", &src))
        return NULL;

    TinyTemplate_instr_t *prog = malloc();
    
    return PyObject_Call((PyObject*) &TemplateType, src);
}

static PyMethodDef methods[] = {
    {"compile", compile, METH_VARARGS, "Compile a template"},
    {NULL, NULL, 0, NULL}, /* Sentinel */
};

static struct PyModuleDef module = {
    PyModuleDef_HEAD_INIT,
    "tinytemplate",   /* name of module */
    spam_doc, /* module documentation, may be NULL */
    0,       /* size of per-interpreter state of the module,
                 or -1 if the module keeps state in global variables. */
    methods,
};

PyMODINIT_FUNC
PyInit_tinytemplate(void)
{
    if (PyType_Ready(&TemplateType) < 0)
        return NULL;

    PyObject *m = PyModule_Create(&module);
    if (m == NULL)
        return NULL;

    Py_INCREF(&TemplateType);
    if (PyModule_AddObject(m, "Template", (PyObject *) &TemplateType) < 0) {
        Py_DECREF(&TemplateType);
        Py_DECREF(m);
        return NULL;
    }

    return m;
}