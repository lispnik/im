# CFFI bindings to IM

## IM

IM is a toolkit for image representation, storage, capture and
processing. For more information, refer to:

    http://webserver2.tecgraf.puc-rio.br/im/

    im@tecgraf.puc-rio.br
   
This project is unaffiliated with TecGraf.

## Documentation

All exported symbols have docstrings, however you will need to refer
to the library documentation http://webserver2.tecgraf.puc-rio.br/im/
for concepts and further details.

## Coverage

IM is defined in four parts. The Lisp binding will eventually cover
all four, however only the following are mostly implemented:

- [x] Representation
- [x] Storage¹
- [x] Capture
- [ ] Processing

¹ except memory file support

## CFFI deviations from the C API

The CFFI bindings are for the most part a one-to-one mapping with the
C API functions. The following are the deviations taken when binding
the C API to Lisp:

* Lisp functions are spelled out completely

* Bitfields and enumations are Lisp keywords.

* The C API has a notion of a *color mode*, which is the bitwise OR of
  a list of *color mode config* bit field options and a *color space*
  enumeration. The Lisp API separates *color mode* into the individual
  data types. e.g.
  
  Where the C API does:
  
  ```c
  imImageDataSize(width, height, color_mode, data_type)
  ```
  
  the Lisp API does:
  
  ```lisp
  (im:image-data-size width height color-mode-config color-space data-type)
  ```

* C API setters translated into `setf` functions.

* (non-deviation) Currently, the Lisp bindings provide raw data access
  via a foreign pointer rather than provide convenient accessors. The
  reason follows IM's own documentation:
  
    > As the library is designed to work with such a wide range of
    > image data organization, there are no general purpose functions
    > for getting/setting individual pixels, as they would be too
    > complicated and inefficient. Rather, you should use the
    > components of the imImage structure to access image pixels in
    > the most efficient way.
    
  Refer to
  http://webserver2.tecgraf.puc-rio.br/im/en/representation.html for
  more information.

## Examples

Load the examples system with `(ql:quickload '("im" "im-examples"))`.

### Device Capture

[cheese.lisp](https://github.com/lispnik/im/blob/master/examples/cheese.lisp)

Write an 8-bit, grayscale image using the first image capture
device (which is probably a webcam) to a JPEG file.

```
CL-USER> (im-examples.cheese:cheese)
Say CHEESE! (and press enter)

capture-3756857502.jpg
(COLOR-MODE-CONFIG-PACKED)
COLOR-SPACE-GRAY
DATA-TYPE-BYTE
640x480
NIL
```

### Image Copy

[image-copy.lisp](https://github.com/lispnik/im/blob/master/examples/image-copy.lisp)

Load a JPEG image, convert to TIFF with LZW compression and write the
output to file.

```lisp
CL-USER> (im-examples.image-copy:image-copy
          #p"/usr/share/backgrounds/Black_sand_beach_by_Mads_Rosendahl.jpg"
          #p"/tmp/Black_sand_beach_by_Mads_Rosendahl.tif"
          "TIFF"
          "LZW")
NIL
```

### Image Info

[image-info.lisp](https://github.com/lispnik/im/blob/master/examples/image-info.lisp)

Return information about a JPEG image as an associative list.

```lisp
CL-USER> (im-examples.image-info:image-info #p"/usr/share/backgrounds/Morning_by_Bernhard_Hanakam.jpg")
(:PATHNAME #P"/usr/share/backgrounds/Morning_by_Bernhard_Hanakam.jpg" :FORMAT
 "JPEG" :COMPRESSION "JPEG" :COUNT 1 :IMAGES
 ((:WIDTH 3840 :HEIGHT 2160 :COLOR-MODE
   (:COLOR-SPACE :COLOR-SPACE-RGB :COLOR-MODE-CONFIG
    (:COLOR-MODE-CONFIG-PACKED :COLOR-MODE-CONFIG-TOPDOWN))
   :DATA-TYPE :DATA-TYPE-BYTE :DATA-SIZE 24883200 :ATTRIBUTES
   (("YResolution" #(75.0) :DATA-TYPE-FLOAT 1)
    ("FileFormat" #(74 80 69 71 0) :DATA-TYPE-BYTE 5)
    ("Copyright" #(66 101 114 110 104 97 114 100 32 72 97 110 97 107 97 109 0)
     :DATA-TYPE-BYTE 17)
    ("XResolution" #(75.0) :DATA-TYPE-FLOAT 1)
    ("FileImageCount" #(1) :DATA-TYPE-INT 1)
    ("DateTime" #(50 48 49 56 58 48 57 58 50 56 32 49 51 58 50 54 58 48 53 0)
     :DATA-TYPE-BYTE 20)
    ("FlashPixVersion" #(48 49 48 48) :DATA-TYPE-BYTE 4)
    ("FileCompression" #(74 80 69 71 0) :DATA-TYPE-BYTE 5)
    ("ColorSpace" #(65535) :DATA-TYPE-USHORT 1)
    ("ResolutionUnit" #(68 80 73 0) :DATA-TYPE-BYTE 4)
    ("Software" #(71 73 77 80 32 50 46 49 48 46 54 0) :DATA-TYPE-BYTE 12)
    ("ExifVersion" #(48 50 49 48) :DATA-TYPE-BYTE 4)
    ("Interlaced" #(1) :DATA-TYPE-INT 1)
    ("Artist" #(66 101 114 110 104 97 114 100 32 72 97 110 97 107 97 109 0)
     :DATA-TYPE-BYTE 17)))))
```

**NOTE** No attempt is made to convert string attibutes. Use
[Babel](https://github.com/cl-babel/babel) or similar.
