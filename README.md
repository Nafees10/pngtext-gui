# pngtext-gui
A GUI for pngtext

---

## Building
pngtextgui is a Lazarus project (lazarus 1.8.4). To build it, you need lazarus and fpc installed. Both can be downloaded from [lazarus-ide.org](http://lazarus-ide.org).  

pngtextgui needs pngtext (libpngtext) to work. To install libpngtext, execute the following:
```
git clone github.com/Nafees10/pngtext
dub build --build=release --config=so
sudp mv libpngtext.so /lib/libpngtext.so.0.1.0
```
For these commands to work, you need: `git`, and `dmd` (including `dub`)

---

## Usage
pngtextgui can be used to write text inside png images' pixels, without damaging much quality unless you want to store megabytes of text (resulting quality depends on resolution of image, and amount of text).  
  
To store some text inside an image, you need an original image. This will be called the "Container Image". The text will be hidden inside this image, and the resulting image can be saved somewhere else, so the original image is not lost. You can select the Container Image using `Ctrl+O` or `File > Open`.  

To read text from an image that was written to using pngtextgui, you open the image the same way you would open a Container Image. Then press `F5` or `File > Read`.

---

## TODO for upcoming versions

1. Add option to store binary/non-plaintext files into images.
2. Integration with file managers, to add option to png images to be opened with pngtextgui
3. A better logo? 
4. Make it work with Windows. (I haven't tested it on Windows, but since the library name is hard-coded as `libpngtext.so.0.1.0`, it won't work on Windows)
