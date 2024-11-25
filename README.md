# Vide Learning Material

A simplified vide implementation designed to give interested people ways to gain a basic understanding of the various video implementations.

**Current state**: just a demo

Before you run the code in `./demos`, make sure you do this:

`dotnet publish`

**READ CAREFULLY**

In all the interactive demos, you have to evaluate the initial lines of code **BEFORE** you send
any other lines to FSI - otherwise, it won't work ;)

```
#r "nuget: Avalonia, 11.2.1"
#r "nuget: Avalonia.Desktop, 11.2.1"
#r "nuget: Avalonia.Themes.Fluent, 11.2.1"

#I "../src/Vide4Avalonia/bin/Debug/netstandard2.0/publish"
#r "Vide4Avalonia.dll"

Vide4Avalonia.FSI.guardInit ()

// ^ -------------------------------------------------------------
// |_ This is the boilerplate to make the sample work in fsi.
//    Evaluate this _once and separate_ from the rest of the sample.
// ---------------------------------------------------------------

```


Have fun :)

