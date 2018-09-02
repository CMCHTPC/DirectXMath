unit DirectX.PackedVector;
//-------------------------------------------------------------------------------------
// DirectXPackedVector.h -- SIMD C++ Math library

// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
// PARTICULAR PURPOSE.

// Copyright (c) Microsoft Corporation. All rights reserved.

// http://go.microsoft.com/fwlink/?LinkID=615560
//-------------------------------------------------------------------------------------


{$mode delphiunicode}{$H+}

interface

uses
    Windows, Classes, SysUtils;

type
    //------------------------------------------------------------------------------
    // ARGB Color; 8-8-8-8 bit unsigned normalized integer components packed into
    // a 32 bit integer.  The normalized color is packed into 32 bits using 8 bit
    // unsigned, normalized integers for the alpha, red, green, and blue components.
    // The alpha component is stored in the most significant bits and the blue
    // component in the least significant bits (A8R8G8B8):
    // [32] aaaaaaaa rrrrrrrr gggggggg bbbbbbbb [0]

    { TXMCOLOR }

    TXMCOLOR = record
        constructor Create(Color: uint32); overload;
        constructor Create(_r, _g, _b, _a: single);overload;
        constructor Create(pArray: Psingle); overload;
        class operator Implicit(a: TXMCOLOR): uint32;
        case integer of
            0: (b: byte;  // Blue:    0/255 to 255/255
                g: byte;  // Green:   0/255 to 255/255
                r: byte;  // Red:     0/255 to 255/255
                a: byte;);  // Alpha:   0/255 to 255/255
            1: (c: uint32);
    end;

    //------------------------------------------------------------------------------
    // 16 bit floating point number consisting of a sign bit, a 5 bit biased
    // exponent, and a 10 bit mantissa
    THALF = uint16;
    PHALF = ^THALF;

    //------------------------------------------------------------------------------
    // 2D Vector; 16 bit floating point components

    { TXMHALF2 }

    TXMHALF2 = record
        constructor Create(_x, _y: THALF); overload;
        constructor Create(pArray: PHALF); overload;
        constructor Create(pArray: Psingle); overload;
        case integer of
            0: (x: THALF;
                y: THALF;);
            1: (v: uint32);
    end;

    //------------------------------------------------------------------------------
    // 2D Vector; 16 bit signed normalized integer components
    TXMSHORTN2 = record
        case integer of
            0: (x: int16;
                y: int16;);
            1: (v: uint32);
    end;

    // 2D Vector; 16 bit signed integer components
    TXMSHORT2 = record
        case integer of
            0: (x: int16;
                y: int16;);
            1: (v: uint32);
    end;

implementation

{ TXMHALF2 }


constructor TXMHALF2.Create(_x, _y: THALF);
begin
    x := _x;
    y := _y;
end;

constructor TXMHALF2.Create(pArray: PHALF);
begin

end;

constructor TXMHALF2.Create(pArray: Psingle);
begin

end;

{ TXMCOLOR }

constructor TXMCOLOR.Create(Color: uint32);
begin

end;

constructor TXMCOLOR.Create(_r, _g, _b, _a: single);
begin

end;

constructor TXMCOLOR.Create(pArray: Psingle);
begin

end;

class operator TXMCOLOR.Implicit(a: TXMCOLOR): uint32;
begin
    Result := a.c;
end;

end.


