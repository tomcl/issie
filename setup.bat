.paket\paket.exe install
dotnet nuget locals all --clear
dotnet restore src\Common\Common.fsproj
dotnet restore src\WidthInferer\WidthInferer.fsproj
dotnet restore src\Simulator\Simulator.fsproj
dotnet restore src\Tests\Tests.fsproj
dotnet restore src\Renderer\Renderer.fsproj
dotnet restore src\Main\Main.fsproj
dotnet restore src\Renderer\Renderer.fsproj
yarn install