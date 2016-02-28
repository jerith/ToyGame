# ToyGameFS

## setup

I'm not entirely sure how to set up project from the repo, but your best bet is
probably to open it in Xamarin Studio or something similar and click some
buttons.

## build

The F# project needs to built separately from the Unity project. I do it from
the commandline, like so:

```
cd Code
xbuild /nologo
```

## test

To run tests from the command line, you need to invoke `mono` with some
instructions about where to find things:

```
MONO_PATH=packages/NUnit.2.6.4/lib mono \
    packages/NUnit.Runners.2.6.4/tools/nunit-console.exe \
    ToyGameTests/bin/Debug/ToyGameTests.dll -nologo -labels
```
