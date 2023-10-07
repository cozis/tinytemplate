from distutils.core import setup, Extension
setup(name="tinytemplate", version="1.0",
      ext_modules=[Extension("tinytemplate", ["pytinytemplate.c"])])
