@echo off
cls
dotnet tool restore
dotnet paket install
:: dotnet restore build.proj
dotnet fsi build.fsx  %*