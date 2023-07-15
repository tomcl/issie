@echo off
cls
dotnet tool restore
dotnet paket install
:: dotnet -v restore build.proj
dotnet fsi build.fsx  %*