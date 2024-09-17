(**
# What is Issie?

Issie (Intuitive Schematic Simulator with Interactive Editor) is a very easy to use and capable block schematic based digital logic editor and simulator, 
originally made for the 1st year undergraduate Digital Electronics and Computer Architecture module at the 
Department of Electronic and Electrical Engineering, Imperial College London.

Issie is coded in F# transpiled to Javascript by [FABLE](http://fable.io)
library.

## Acknowledgements:

* Marco Selvatici for the 8K lines of base code written for his 3rd year BEng [FYP](https://github.com/MarcoSelvatici/DEflow)
* Very many Imperial College students working on FYPs or UROPs

## Background and Technical Details

Marco's [Dissertation](https://tomcl.github.com/issie/marco-report.pdf) or his [Poster](https://tomcl.github.com/issie/marco-poster.pdf).





## How to get Issie

Go to the [releases](https://github.com/tomcl/issie/releases) page on the Github repo and download the 
package most applicable to your operating system. 

You <b>only</b> need to download the `.exe` for Windows or `.AppImage` for Linux. The other files are for auto-updating
which is handled by the application. (You will see an update button pop up in the top right-hand corner.)

With Linux:

 - Most distributions are supported, if you have an issue please let me know and I will try to fix it.
 - It is distributed in [AppImage](https://appimage.org/) format, which is plug and play for most popular distros.
   - You may need to right click and go to properties to give it permissions to execute.
   - Some distributions such as Debian will need the start up flag `--no-sandbox` in order to run. 

With Windows:

 - The installer is easy to install, a single click in fact.


<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <img src="img/communityAnnouncements.png"/>
  </div>
</div>

## Documentation ##

 - Functionality - Some example use cases for the application.
 - [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the functions.
   This is only helpful if you plan on contributing.
 
## Contributing ##

The project is hosted on [Github][gh] where you can [report issues][issues], fork the project and submit pull 
requests. Please take a look at the [contribution guidelines](contributing.html) for more information.

  [gh]: https://github.com/tomcl/issie
  [issues]: https://github.com/tomcl/issie/issues
  
*)
