unit DirectX.Math;
//-------------------------------------------------------------------------------------
// DirectXMath.h -- SIMD C++ Math library

// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
// PARTICULAR PURPOSE.

// Copyright (c) Microsoft Corporation. All rights reserved.

// http://go.microsoft.com/fwlink/?LinkID=615560
//-------------------------------------------------------------------------------------
// Copyright (c) Pascal Translation
// Norbert Sonnleitner
//-------------------------------------------------------------------------------------

(* DirectXMath Library Compiler Directives
   see: https://msdn.microsoft.com/en-us/library/windows/desktop/ee415579(v=vs.85).aspx

   Compiler directives tune the functionality that the DirectXMath library uses.

   _XM_NO_INTRINSICS_
   When _XM_NO_INTRINSICS_ is defined, DirectXMath operations are implemented without using any platform-specific intrinsics.
   Instead, DirectXMath uses only standard floating point operations. By default, _XM_NO_INTRINSICS_ is not defined.
   This directive overrides all other directives. Therefore, we recommend that you check for _XM_NO_INTRINSICS_ before you
   determine the status of _XM_ARM_NEON_INTRINSICS_ or _XM_SSE_INTRINSICS_.

   _XM_SSE_INTRINSICS_
   When _XM_SSE_INTRINSICS_ is defined, code is compiled to use supporting SSE and SSE2 on platforms that support these instruction sets.
   The Windows versions providing SSE intrinsics support both SSE and SSE2.
   _XM_SSE_INTRINSICS_ has no effect on systems that do not support SSE and SSE2.
   By default, _XM_SSE_INTRINSICS_ is defined when users compile for a Windows platform.
   DirectXMath uses the standard compiler defines (_M_IX86 / _M_AMD64) to determine Windows x86 or Windows x64 targets.

   _XM_ARM_NEON_INTRINSICS_
   This indicates the DirectXMath implementation uses of the ARM-NEON intrinsics. By defualt, _XM_ARM_NEON_INTRINSICS_ is defined
   when users compile for a Windows RT platform. DirectXMath uses the standard compiler define (_M_ARM, _M_ARM64) to determine this binary target.
   ARM-NEON intrinsics support is new to DirectXMath and is not supported by XNAMath 2.x.

   _XM_SSE4_INTRINSICS_
   New for Windows 10 Anniversary SDK.
   When you specify this compiler directive, DirectXMath functions are implemented to make use of SSE3 and SSE4.1 instriniscs where applicable;
   otherwise it uses to SSE/SSE2.

   _XM_AVX_INTRINSICS_
   New for Windows 10 Anniversary SDK. Use of /arch:AVX will enable this directive.
   When you specify this compiler directive, DirectXMath functions are implemented to make use of SSE3, SSE4.1, and AVX 128-bit intrinsics where applicable; otherwise DirectXMath uses SSE/SSE2. When you specify this compiler directive, it also implies _XM_SSE4_INTRINSICS_ and _XM_SSE_INTRINSICS_, and includes a small subset of SSE3 instructions.

   _XM_F16C_INTRINSICS_
   New for Windows 10 Anniversary SDK.
   Use of /arch:AVX2 will enable this directive.
   When you specify this compiler directive, DirectXMath functions are implemented to make use of SSE3, SSE4.1, AVX 128-bit, and F16C/CVT16 intrinsics where applicable; otherwise DirectXMath uses SSE/SSE2. When you use _XM_F16C_INTRINSICS_, it implies _XM_AVX_INTRINSICS_, _XM_SSE4_INTRINSICS_, and _XM_SSE_INTRINSICS_.

   _XM_VECTORCALL_
   This enables the use of the new __vectorcall calling convention for Windows x86 or Windows x64 targets. It defaults to _XM_VECTORCALL_=1
   when the compiler supports this feature. You can manually set it to _XM_VECTORCALL_=0 to always disable it.
   __vectorcall is not supported for the Windows RT platform or Managed C++.
*)



{$DEFINE _XM_NO_INTRINSICS_}
//{$DEFINE __AVX2__}
//{$DEFINE __AVX__}


{$IF  not defined(_XM_AVX2_INTRINSICS_) AND defined(__AVX2__) AND not defined(_XM_NO_INTRINSICS_)}
    {$define _XM_AVX2_INTRINSICS_}
{$ENDIF}

{$IF  NOT defined(_XM_FMA3_INTRINSICS_) AND defined(_XM_AVX2_INTRINSICS_) AND NOT defined(_XM_NO_INTRINSICS_)}
    {$define _XM_FMA3_INTRINSICS_}
{$ENDIF}

{$IF NOT defined(_XM_F16C_INTRINSICS_) AND defined(_XM_AVX2_INTRINSICS_)  AND NOT defined(_XM_NO_INTRINSICS_)}
    {$define _XM_F16C_INTRINSICS_}
{$ENDIF}

{$IF defined(_XM_FMA3_INTRINSICS_) AND NOT defined(_XM_AVX_INTRINSICS_)}
    {$define _XM_AVX_INTRINSICS_}
{$ENDIF}

{$IF defined(_XM_F16C_INTRINSICS_) AND NOT defined(_XM_AVX_INTRINSICS_)}
   {$define _XM_AVX_INTRINSICS_}
{$ENDIF}

{$IF NOT defined(_XM_AVX_INTRINSICS_) AND defined(__AVX__) AND NOT defined(_XM_NO_INTRINSICS_)}
   {$define _XM_AVX_INTRINSICS_}
{$ENDIF}

{$IF defined(_XM_AVX_INTRINSICS_) AND NOT defined(_XM_SSE4_INTRINSICS_)}
    {$define _XM_SSE4_INTRINSICS_}
{$ENDIF}

{$IF defined(_XM_SSE4_INTRINSICS_) AND NOT defined(_XM_SSE3_INTRINSICS_)}
    {$define _XM_SSE3_INTRINSICS_}
{$ENDIF}

{$IF defined(_XM_SSE3_INTRINSICS_) AND NOT defined(_XM_SSE_INTRINSICS_)}
    {$define _XM_SSE_INTRINSICS_}
{$ENDIF}

{$IF NOT defined(_XM_ARM_NEON_INTRINSICS_) AND NOT defined(_XM_SSE_INTRINSICS_) AND NOT defined(_XM_NO_INTRINSICS_)}
     {$IF (defined(CPU386) OR defined(CPUX86_64)) AND NOT defined(_M_HYBRID_X86_ARM64)}
          {$define _XM_SSE_INTRINSICS_}
     {$ELSEIF defined(CPUARM ) OR  defined(_M_ARM64) OR defined(_M_HYBRID_X86_ARM64)}
          {$define _XM_ARM_NEON_INTRINSICS_}
     {$ELSEIF NOT defined(_XM_NO_INTRINSICS_)}
          {$ERROR DirectX Math does not support this target}
     {$ENDIF}
{$ENDIF}//  NOT _XM_ARM_NEON_INTRINSICS_  AND   NOT _XM_SSE_INTRINSICS_  AND   NOT _XM_NO_INTRINSICS_

{$IF NOT defined(_XM_NO_XMVECTOR_OVERLOADS_)  AND  defined(__clang__)}
     {$define _XM_NO_XMVECTOR_OVERLOADS_}
{$ENDIF}

interface

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}
{$ASMMODE intel}


uses
    Windows, Classes, SysUtils;

(*
{$IFDEF CPUX86_64}
       {$CODEALIGN CONSTMIN=16}
       {$CODEALIGN RECORDMIN=16}
       {$Z4}
{$ELSE}
       {$Z4}
       {$CODEALIGN CONSTMIN=16}
       {$CODEALIGN RECORDMIN=16}
{$ENDIF}

*)

const
    DIRECTX_MATH_VERSION = 311;

const
    _MM_FROUND_TO_NEAREST_INT = $0; // Round to nearest (even).
    _MM_FROUND_TO_NEG_INF = $1; // Round down (toward -∞).
    _MM_FROUND_TO_POS_INF = $2; // Round up (toward +∞).
    _MM_FROUND_TO_ZERO = $3; // Round toward zero (truncate).
    _MM_FROUND_CUR_DIRECTION = $4; // Use current MXCSR setting.
    _MM_FROUND_RAISE_EXC = $0; // Signal precision exception on SNaN.
    _MM_FROUND_NO_EXC = $8; // Do not signal precision exception on SNaN.

const
    {***************************************************************************
     *
     * Constant definitions
     *
     ***************************************************************************}
    XM_PI: single = 3.141592654;
    XM_2PI: single = 6.283185307;
    XM_1DIVPI: single = 0.318309886;
    XM_1DIV2PI: single = 0.159154943;
    XM_PIDIV2: single = 1.570796327;
    XM_PIDIV4: single = 0.785398163;

    XM_SELECT_0: UINT32 = $00000000;
    XM_SELECT_1: UINT32 = $FFFFFFFF;

    XM_PERMUTE_0X: UINT32 = 0;
    XM_PERMUTE_0Y: UINT32 = 1;
    XM_PERMUTE_0Z: UINT32 = 2;
    XM_PERMUTE_0W: UINT32 = 3;
    XM_PERMUTE_1X: UINT32 = 4;
    XM_PERMUTE_1Y: UINT32 = 5;
    XM_PERMUTE_1Z: UINT32 = 6;
    XM_PERMUTE_1W: UINT32 = 7;

    XM_SWIZZLE_X: UINT32 = 0;
    XM_SWIZZLE_Y: UINT32 = 1;
    XM_SWIZZLE_Z: UINT32 = 2;
    XM_SWIZZLE_W: UINT32 = 3;

    XM_CRMASK_CR6: UINT32 = $000000F0;
    XM_CRMASK_CR6TRUE: UINT32 = $00000080;
    XM_CRMASK_CR6FALSE: UINT32 = $00000020;
    XM_CRMASK_CR6BOUNDS: UINT32 = $00000020; // XM_CRMASK_CR6FALSE

    XM_CACHE_LINE_SIZE: Size_T = 64;
    XM3_DECOMP_EPSILON: single = 0.0001;


{***************************************************************************
 *
 * Macros
 *
 ***************************************************************************}
// Unit conversion
function XMConvertToRadians(fDegrees: single): single;
function XMConvertToDegrees(fRadians: single): single;
// Condition register evaluation proceeding a recording (R) comparison
function XMComparisonAllTrue(CR: uint32): boolean;
function XMComparisonAnyTrue(CR: uint32): boolean;
function XMComparisonAllFalse(CR: uint32): boolean;
function XMComparisonAnyFalse(CR: uint32): boolean;
function XMComparisonMixed(CR: uint32): boolean;
function XMComparisonAllInBounds(CR: uint32): boolean;
function XMComparisonAnyOutOfBounds(CR: uint32): boolean;


type

    TCPUType = (CPU_XM_SSE3, CPU_XM_SSE4, CPU_AVX, CPU_AVX2);
    TCPUInfo = array [0..3] of cardinal;

 //   {$PACKRECORDS 16}
    { TXMVECTOR }
    //------------------------------------------------------------------------------
    // Vector intrinsic: Four 32 bit floating point components aligned on a 16 byte
    // boundary and mapped to hardware vector registers
    TXMVECTOR = record
        constructor Create(x, y, z, w: single);
    //------------------------------------------------------------------------------
    // Vector operators
        class operator Positive(a: TXMVECTOR): TXMVECTOR;
        class operator Negative(a: TXMVECTOR): TXMVECTOR;
        class operator Add(a, b: TXMVECTOR): TXMVECTOR;
        class operator Subtract(a, b: TXMVECTOR): TXMVECTOR;
        class operator Multiply(a, b: TXMVECTOR): TXMVECTOR;
        class operator Divide(a, b: TXMVECTOR): TXMVECTOR;
        class operator Multiply(v: TXMVECTOR; s: single): TXMVECTOR;
        class operator Divide(v: TXMVECTOR; s: single): TXMVECTOR;
        case integer of
            0: (f32: array [0..3] of single);
            1: (u32: array [0..3] of uint32);
            2: (i32: array [0..3] of int32);
    end;

    PXMVECTOR = ^TXMVECTOR;
    TXMVECTORArray = array of TXMVECTOR;


    { TXMVECTORF32 }
    //------------------------------------------------------------------------------
    // Conversion types for constants
    TXMVECTORF32 = record
        class operator Implicit(a: TXMVECTORF32): PSingle;
        class operator Implicit(a: TXMVECTORF32): TXMVECTOR;
        case integer of
            0: (f: array [0..3] of single);
            1: (v: TXMVECTOR);
    end;

    { TXMVECTORU32 }

    TXMVECTORU32 = record
        class operator Implicit(a: TXMVECTORU32): Puint32;
        class operator Implicit(a: TXMVECTORU32): TXMVECTOR;
        case integer of
            0: (u: array [0..3] of uint32);
            1: (v: TXMVECTOR);
    end;


    { TXMVECTORI32 }

    TXMVECTORI32 = record
        class operator Implicit(a: TXMVECTORI32): Pint32;
        class operator Implicit(a: TXMVECTORI32): TXMVECTOR;
        case integer of
            0: (i: array [0..3] of int32);
            1: (v: TXMVECTOR);
    end;

    { TXMVECTORU8 }

    TXMVECTORU8 = record
        class operator Implicit(a: TXMVECTORU8): PByte;
        class operator Implicit(a: TXMVECTORU8): TXMVECTOR;
        case integer of
            0: (u: array [0..15] of byte);
            1: (v: TXMVECTOR);
    end;


    //------------------------------------------------------------------------------
    // Matrix type: Sixteen 32 bit floating point components aligned on a
    // 16 byte boundary and mapped to four hardware vector registers

    { TXMMATRIX }

    TXMMATRIX = record
        constructor Create(R0, R1, R2, R3: TXMVECTOR); overload;
        constructor Create(m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33: single);
                overload;
        constructor Create(pArray: PSingle); overload;
        class operator Positive(a: TXMMATRIX): TXMMATRIX;
        class operator Negative(a: TXMMATRIX): TXMMATRIX;
        class operator Add(a, b: TXMMATRIX): TXMMATRIX;
        class operator Subtract(a, b: TXMMATRIX): TXMMATRIX;
        class operator Multiply(a, b: TXMMATRIX): TXMMATRIX;
        class operator Multiply(M: TXMMATRIX; s: single): TXMMATRIX;
        class operator Divide(M: TXMMATRIX; s: single): TXMMATRIX;
        function Get(Row, Column: size_t): single;

        case integer of
            0: (r: array [0..3] of TXMVECTOR);
            1: (_11, _12, _13, _14: single;
                _21, _22, _23, _24: single;
                _31, _32, _33, _34: single;
                _41, _42, _43, _44: single;
            );
            2: (m: array[0..3, 0..3] of single);
            3: (n: array [0..15] of single);
            4: (u: array [0..15] of UINT32);
            5: (r0, r1, r2, r3: TXMVECTOR);
    end;


 //   {$PACKRECORDS 4}
    //------------------------------------------------------------------------------
    // 2D Vector; 32 bit floating point components

    { TXMFLOAT2 }

    TXMFLOAT2 = record
        x: single;
        y: single;
        constructor Create(_x, _y: single); overload;
        constructor Create(pArray: PSingle); overload;
    end;

    PXMFLOAT2 = ^TXMFLOAT2;


    { TXMINT2 - 2D Vector; 32 bit signed integer components }

    TXMINT2 = record
        x: int32;
        y: int32;
        constructor Create(_x, _y: int32); overload;
        constructor Create(pArray: PINT32); overload;
    end;

    PXMINT2 = ^TXMINT2;

    { TXMUINT2 - 2D Vector; 32 bit unsigned integer components  }

    TXMUINT2 = record
        x: uint32;
        y: uint32;
        constructor Create(_x, _y: uint32); overload;
        constructor Create(pArray: PUINT32); overload;
    end;

    PXMUINT2 = ^TXMUINT2;

    //------------------------------------------------------------------------------

    { TXMFLOAT3 - 3D Vector; 32 bit floating point components }

    TXMFLOAT3 = record
        x: single;
        y: single;
        z: single;
        constructor Create(_x, _y, _z: single); overload;
        constructor Create(pArray: Psingle); overload;
    end;
    PXMFLOAT3 = ^TXMFLOAT3;


    { TXMINT3 - 3D Vector; 32 bit signed integer components }

    TXMINT3 = record
        x: int32;
        y: int32;
        z: int32;
        constructor Create(_x, _y, _z: int32); overload;
        constructor Create(pArray: Pint32); overload;
    end;

    PXMINT3 = ^TXMINT3;

    { TXMUINT3 - 3D Vector; 32 bit unsigned integer components }

    TXMUINT3 = record
        x: uint32;
        y: uint32;
        z: uint32;
        constructor Create(_x, _y, _z: Uint32); overload;
        constructor Create(pArray: PUint32); overload;

    end;

    PXMUINT3 = ^TXMUINT3;

    //------------------------------------------------------------------------------

    { TXMFLOAT4 - 4D Vector; 32 bit floating point components }

    TXMFLOAT4 = record
        x: single;
        y: single;
        z: single;
        w: single;
        constructor Create(pArray: PSingle); overload;
        constructor Create(_X, _Y, _Z, _W: single); overload;
    end;

    PXMFLOAT4 = ^TXMFLOAT4;




    //------------------------------------------------------------------------------
    { TXMINT4 - 4D Vector; 32 bit signed integer components }

    TXMINT4 = record
        x: int32;
        y: int32;
        z: int32;
        w: int32;
        constructor Create(_X, _Y, _Z, _W: int32); overload;
        constructor Create(pArray: Pint32); overload;
    end;

    PXMINT4 = ^TXMINT4;

    // 4D Vector; 32 bit unsigned integer components

    { TXMUINT4 - 4D Vector; 32 bit unsigned integer components }

    TXMUINT4 = record
        x: uint32;
        y: uint32;
        z: uint32;
        w: uint32;
        constructor Create(_X, _Y, _Z, _W: uint32); overload;
        constructor Create(pArray: Puint32); overload;
    end;

    PXMUINT4 = ^TXMUINT4;

    //------------------------------------------------------------------------------
    { TXMFLOAT3X3 - 3x3 Matrix: 32 bit floating point components }

    TXMFLOAT3X3 = record
        constructor Create(constref pArray: PSingle); overload;
        constructor Create(m00, m01, m02, m10, m11, m12, m20, m21, m22: single); overload;
        function Get(Row, Column: size_t): single;
        case integer of
            0: (_11, _12, _13: single;
                _21, _22, _23: single;
                _31, _32, _33: single;);
            1: (m: array [0..2, 0..2] of single);
    end;

    PXMFLOAT3X3 = ^TXMFLOAT3X3;

    //------------------------------------------------------------------------------
    { TXMFLOAT4X3 - 4x3 Matrix: 32 bit floating point components }

    TXMFLOAT4X3 = record
        constructor Create(m00, m01, m02, m10, m11, m12, m20, m21, m22, m30, m31, m32: single); overload;
        constructor Create(constref pArray: PSingle);
        function Get(Row, Column: size_t): single;
        case integer of
            0: (_11, _12, _13: single;
                _21, _22, _23: single;
                _31, _32, _33: single;
                _41, _42, _43: single;);
            1: (m: array [0..3, 0..2] of single);

    end;

    PXMFLOAT4X3 = ^TXMFLOAT4X3;




    //------------------------------------------------------------------------------
    { TXMFLOAT4X4 - 4x4 Matrix: 32 bit floating point components }
    TXMFLOAT4X4 = record
        constructor Create(m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33: single); overload;
        constructor Create(constref pArray: PSingle); overload;
        function Get(Row, Column: size_t): single;
        case integer of
            0: (
                _11, _12, _13, _14: single;
                _21, _22, _23, _24: single;
                _31, _32, _33, _34: single;
                _41, _42, _43, _44: single;
            );
            1: (m: array [0..3, 0..3] of single);
    end;

    PXMFLOAT4X4 = ^TXMFLOAT4X4;

    { TXMFLOAT4X4A - 4x4 Matrix: 32 bit floating point components aligned on a 16 byte boundary }


(*
{$IFDEF CPUX86_64}
    {$PACKRECORDS 16}
    {$CODEALIGN RECORDMIN=16}
{$ELSE}
    {$PACKRECORDS 4}
{$ENDIF}
 *)


    // 2D Vector; 32 bit floating point components aligned on a 16 byte boundary

    { TXMFLOAT2A }
    TXMFLOAT2A = record
        x: single;
        y: single;
        constructor Create(_x, _y: single); overload;
        constructor Create(pArray: PSingle); overload;
        class operator Implicit(Float2: TXMFLOAT2): TXMFLOAT2A;
    end;

    TUINT32A_Array2 = array [0..1] of UINT32;

    // 3D Vector; 32 bit floating point components aligned on a 16 byte boundary
    { TXMFLOAT3A }

    TXMFLOAT3A = record
        x: single;
        y: single;
        z: single;
        constructor Create(_x, _y, _z: single); overload;
        constructor Create(pArray: Psingle); overload;
        class operator Implicit(Float3: TXMFLOAT3): TXMFLOAT3A;
    end;

    //------------------------------------------------------------------------------


    { TXMFLOAT4A - 4D Vector; 32 bit floating point components aligned on a 16 byte boundary }
    TXMFLOAT4A = record
        x: single;
        y: single;
        z: single;
        w: single;
        constructor Create(pArray: PSingle); overload;
        constructor Create(_X, _Y, _Z, _W: single); overload;
        class operator Implicit(Float4: TXMFLOAT4): TXMFLOAT4A;
    end;

    PXMFLOAT4A = ^TXMFLOAT4A;

    // 4x3 Matrix: 32 bit floating point components aligned on a 16 byte boundary


    { TXMFLOAT4X3A }

    TXMFLOAT4X3A = record
        constructor Create(m00, m01, m02, m10, m11, m12, m20, m21, m22, m30, m31, m32: single); overload;
        constructor Create(constref pArray: PSingle);
        function Get(Row, Column: size_t): single;
        case integer of
            0: (_11, _12, _13: single;
                _21, _22, _23: single;
                _31, _32, _33: single;
                _41, _42, _43: single;);
            1: (m: array [0..3, 0..2] of single);
    end;
    PXMFLOAT4X3A = ^TXMFLOAT4X3A;


    TXMFLOAT4X4A = record
        constructor Create(m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33: single); overload;
        constructor Create(constref pArray: PSingle); overload;
        function Get(Row, Column: size_t): single;
        case integer of
            0: (
                _11, _12, _13, _14: single;
                _21, _22, _23, _24: single;
                _31, _32, _33, _34: single;
                _41, _42, _43, _44: single;
            );
            1: (m: array [0..3, 0..3] of single);
    end;
    PXMFLOAT4X4A = ^TXMFLOAT4X4A;

 (*
{$IFDEF CPUX86_64}
    {$PACKRECORDS 8}
{$ELSE}
    {$PACKRECORDS 4}
{$ENDIF}
*)

{****************************************************************************
 *
 * Globals
 *
 ****************************************************************************}

// The purpose of the following global constants is to prevent redundant
// reloading of the constants when they are referenced by more than one
// separate inline math routine called within the same function.  Declaring
// a constant locally within a routine is sufficient to prevent redundant
// reloads of that constant when that single routine is called multiple
// times in a function, but if the constant is used (and declared) in a
// separate math routine it would be reloaded.

const
    g_XMSinCoefficients0: TXMVECTORF32 = (f: (-0.16666667, +0.0083333310, -0.00019840874, +2.7525562e-06));
    g_XMSinCoefficients1: TXMVECTORF32 = (f: (-2.3889859e-08, -0.16665852 {Est1}, +0.0083139502 {Est2}, -0.00018524670 {Est3}));
    g_XMCosCoefficients0: TXMVECTORF32 = (f: (-0.5, +0.041666638, -0.0013888378, +2.4760495e-05));
    g_XMCosCoefficients1: TXMVECTORF32 = (f: (-2.6051615e-07, -0.49992746 {Est1}, +0.041493919 {Est2}, -0.0012712436 {Est3}));
    g_XMTanCoefficients0: TXMVECTORF32 = (f: (1.0, 0.333333333, 0.133333333, 5.396825397e-2));
    g_XMTanCoefficients1: TXMVECTORF32 = (f: (2.186948854e-2, 8.863235530e-3, 3.592128167e-3, 1.455834485e-3));
    g_XMTanCoefficients2: TXMVECTORF32 = (f: (5.900274264e-4, 2.391290764e-4, 9.691537707e-5, 3.927832950e-5));
    g_XMArcCoefficients0: TXMVECTORF32 = (f: (+1.5707963050, -0.2145988016, +0.0889789874, -0.0501743046));
    g_XMArcCoefficients1: TXMVECTORF32 = (f: (+0.0308918810, -0.0170881256, +0.0066700901, -0.0012624911));
    g_XMATanCoefficients0: TXMVECTORF32 = (f: (-0.3333314528, +0.1999355085, -0.1420889944, +0.1065626393));
    g_XMATanCoefficients1: TXMVECTORF32 = (f: (-0.0752896400, +0.0429096138, -0.0161657367, +0.0028662257));
    g_XMATanEstCoefficients0: TXMVECTORF32 = (f: (+0.999866, +0.999866, +0.999866, +0.999866));
    g_XMATanEstCoefficients1: TXMVECTORF32 = (f: (-0.3302995, +0.180141, -0.085133, +0.0208351));
    g_XMTanEstCoefficients: TXMVECTORF32 = (f: (2.484, -1.954923183e-1, 2.467401101, {XM_1DIVPI}0.318309886));
    g_XMArcEstCoefficients: TXMVECTORF32 = (f: (+1.5707288, -0.2121144, +0.0742610, -0.0187293));
    g_XMPiConstants0: TXMVECTORF32 = (f: ( {XM_PI}3.141592654, {XM_2PI}6.283185307, {XM_1DIVPI}0.318309886,        {XM_1DIV2PI}0.159154943));
    g_XMIdentityR0: TXMVECTORF32 = (f: (1.0, 0.0, 0.0, 0.0));
    g_XMIdentityR1: TXMVECTORF32 = (f: (0.0, 1.0, 0.0, 0.0));
    g_XMIdentityR2: TXMVECTORF32 = (f: (0.0, 0.0, 1.0, 0.0));
    g_XMIdentityR3: TXMVECTORF32 = (f: (0.0, 0.0, 0.0, 1.0));
    g_XMNegIdentityR0: TXMVECTORF32 = (f: (-1.0, 0.0, 0.0, 0.0));
    g_XMNegIdentityR1: TXMVECTORF32 = (f: (0.0, -1.0, 0.0, 0.0));
    g_XMNegIdentityR2: TXMVECTORF32 = (f: (0.0, 0.0, -1.0, 0.0));
    g_XMNegIdentityR3: TXMVECTORF32 = (f: (0.0, 0.0, 0.0, -1.0));
    g_XMNegativeZero: TXMVECTORU32 = (u: ($80000000, $80000000, $80000000, $80000000));
    g_XMNegate3: TXMVECTORU32 = (u: ($80000000, $80000000, $80000000, $00000000));
    g_XMMaskXY: TXMVECTORU32 = (u: ($FFFFFFFF, $FFFFFFFF, $00000000, $00000000));
    g_XMMask3: TXMVECTORU32 = (u: ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $00000000));
    g_XMMaskX: TXMVECTORU32 = (u: ($FFFFFFFF, $00000000, $00000000, $00000000));
    g_XMMaskY: TXMVECTORU32 = (u: ($00000000, $FFFFFFFF, $00000000, $00000000));
    g_XMMaskZ: TXMVECTORU32 = (u: ($00000000, $00000000, $FFFFFFFF, $00000000));
    g_XMMaskW: TXMVECTORU32 = (u: ($00000000, $00000000, $00000000, $FFFFFFFF));
    g_XMOne: TXMVECTORF32 = (f: (1.0, 1.0, 1.0, 1.0));
    g_XMOne3: TXMVECTORF32 = (f: (1.0, 1.0, 1.0, 0.0));
    g_XMZero: TXMVECTORF32 = (f: (0.0, 0.0, 0.0, 0.0));
    g_XMTwo: TXMVECTORF32 = (f: (2.0, 2.0, 2.0, 2.0));
    g_XMFour: TXMVECTORF32 = (f: (4.0, 4.0, 4.0, 4.0));
    g_XMSix: TXMVECTORF32 = (f: (6.0, 6.0, 6.0, 6.0));
    g_XMNegativeOne: TXMVECTORF32 = (f: (-1.0, -1.0, -1.0, -1.0));
    g_XMOneHalf: TXMVECTORF32 = (f: (0.5, 0.5, 0.5, 0.5));
    g_XMNegativeOneHalf: TXMVECTORF32 = (f: (-0.5, -0.5, -0.5, -0.5));
    g_XMNegativeTwoPi: TXMVECTORF32 = (f: (-{XM_2PI}6.283185307, -{XM_2PI}6.283185307, -{XM_2PI}6.283185307, -{XM_2PI}6.283185307));
    g_XMNegativePi: TXMVECTORF32 = (f: (-{XM_PI}3.141592654, -{XM_PI}3.141592654, -{XM_PI}3.141592654, -{XM_PI}3.141592654));
    g_XMHalfPi: TXMVECTORF32 = (f: ( {XM_PIDIV2}1.570796327, {XM_PIDIV2}1.570796327, {XM_PIDIV2}1.570796327,          {XM_PIDIV2}1.570796327));
    g_XMPi: TXMVECTORF32 = (f: ( {XM_PI}3.141592654, {XM_PI}3.141592654,           {XM_PI}3.141592654, {XM_PI}3.141592654));
    g_XMReciprocalPi: TXMVECTORF32 = (f: ( {XM_1DIVPI}0.318309886, {XM_1DIVPI}0.318309886, {XM_1DIVPI}0.318309886,         {XM_1DIVPI}0.318309886));
    g_XMTwoPi: TXMVECTORF32 = (f: ( {XM_2PI}6.283185307, {XM_2PI}6.283185307, {XM_2PI}6.283185307,           {XM_2PI}6.283185307));
    g_XMReciprocalTwoPi: TXMVECTORF32 = (f: ( {XM_1DIV2PI}0.159154943, {XM_1DIV2PI}0.159154943, {XM_1DIV2PI}0.159154943,
        {XM_1DIV2PI}0.159154943));
    g_XMEpsilon: TXMVECTORF32 = (f: (1.192092896e-7, 1.192092896e-7, 1.192092896e-7, 1.192092896e-7));
    g_XMInfinity: TXMVECTORI32 = (i: ($7F800000, $7F800000, $7F800000, $7F800000));
    g_XMQNaN: TXMVECTORI32 = (i: ($7FC00000, $7FC00000, $7FC00000, $7FC00000));
    g_XMQNaNTest: TXMVECTORI32 = (i: ($007FFFFF, $007FFFFF, $007FFFFF, $007FFFFF));
    g_XMAbsMask: TXMVECTORI32 = (i: ($7FFFFFFF, $7FFFFFFF, $7FFFFFFF, $7FFFFFFF));
    g_XMFltMin: TXMVECTORI32 = (i: ($00800000, $00800000, $00800000, $00800000));
    g_XMFltMax: TXMVECTORI32 = (i: ($7F7FFFFF, $7F7FFFFF, $7F7FFFFF, $7F7FFFFF));
    g_XMNegOneMask: TXMVECTORU32 = (u: ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF));
    g_XMMaskA8R8G8B8: TXMVECTORU32 = (u: ($00FF0000, $0000FF00, $000000F, $FF000000));
    g_XMFlipA8R8G8B8: TXMVECTORU32 = (u: ($00000000, $00000000, $00000000, $80000000));
    g_XMFixAA8R8G8B8: TXMVECTORF32 = (f: (0.0, 0.0, 0.0, ($80000000)));
    g_XMNormalizeA8R8G8B8: TXMVECTORF32 = (f: (1.0 / (255.0 * ($10000)), 1.0 / (255.0 * ($100)), 1.0 / 255.0, 1.0 / (255.0 * ($1000000))));
    g_XMMaskA2B10G10R10: TXMVECTORU32 = (u: ($000003F, $000FFC00, $3FF00000, $C0000000));
    g_XMFlipA2B10G10R10: TXMVECTORU32 = (u: ($00000200, $00080000, $20000000, $80000000));
    g_XMFixAA2B10G10R10: TXMVECTORF32 = (f: (-512.0, -512.0 * ($400), -512.0 * ($100000), $80000000));
    g_XMNormalizeA2B10G10R10: TXMVECTORF32 = (f: (1.0 / 511.0, 1.0 / (511.0 * $400), 1.0 / (511.0 * $100000), 1.0 / (3.0 * $40000000)));
    g_XMMaskX16Y16: TXMVECTORU32 = (u: ($0000FFFF, $FFFF0000, $00000000, $00000000));
    g_XMFlipX16Y16: TXMVECTORI32 = (i: ($00008000, $00000000, $00000000, $00000000));
    g_XMFixX16Y16: TXMVECTORF32 = (f: (-32768.0, 0.0, 0.0, 0.0));
    g_XMNormalizeX16Y16: TXMVECTORF32 = (f: (1.0 / 32767.0, 1.0 / (32767.0 * 65536.0), 0.0, 0.0));
    g_XMMaskX16Y16Z16W16: TXMVECTORU32 = (u: ($0000FFFF, $0000FFFF, $FFFF0000, $FFFF0000));
    g_XMFlipX16Y16Z16W16: TXMVECTORI32 = (i: ($00008000, $00008000, $00000000, $00000000));
    g_XMFixX16Y16Z16W16: TXMVECTORF32 = (f: (-32768.0, -32768.0, 0.0, 0.0));
    g_XMNormalizeX16Y16Z16W16: TXMVECTORF32 = (f: (1.0 / 32767.0, 1.0 / 32767.0, 1.0 / (32767.0 * 65536.0), 1.0 / (32767.0 * 65536.0)));
    g_XMNoFraction: TXMVECTORF32 = (f: (8388608.0, 8388608.0, 8388608.0, 8388608.0));
    g_XMMaskByte: TXMVECTORI32 = (i: ($000000FF, $000000FF, $000000FF, $000000FF));
    g_XMNegateX: TXMVECTORF32 = (f: (-1.0, 1.0, 1.0, 1.0));
    g_XMNegateY: TXMVECTORF32 = (f: (1.0, -1.0, 1.0, 1.0));
    g_XMNegateZ: TXMVECTORF32 = (f: (1.0, 1.0, -1.0, 1.0));
    g_XMNegateW: TXMVECTORF32 = (f: (1.0, 1.0, 1.0, -1.0));
    g_XMSelect0101: TXMVECTORU32 = (u: ($00000000, $FFFFFFFF, $00000000, $FFFFFFFF));
    g_XMSelect1010: TXMVECTORU32 = (u: ($FFFFFFFF, $00000000, $FFFFFFFF, $00000000));
    g_XMOneHalfMinusEpsilon: TXMVECTORI32 = (i: ($3EFFFFFD, $3EFFFFFD, $3EFFFFFD, $3EFFFFFD));
    g_XMSelect1000: TXMVECTORU32 = (u: ($FFFFFFFF, $00000000, $00000000, $00000000));
    g_XMSelect1100: TXMVECTORU32 = (u: ($FFFFFFFF, $FFFFFFFF, $00000000, $00000000));
    g_XMSelect1110: TXMVECTORU32 = (u: ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $00000000));
    g_XMSelect1011: TXMVECTORU32 = (u: ($FFFFFFFF, $00000000, $FFFFFFFF, $FFFFFFFF));
    g_XMFixupY16: TXMVECTORF32 = (f: (1.0, 1.0 / 65536.0, 0.0, 0.0));
    g_XMFixupY16W16: TXMVECTORF32 = (f: (1.0, 1.0, 1.0 / 65536.0, 1.0 / 65536.0));
    g_XMFlipY: TXMVECTORU32 = (u: (0, $80000000, 0, 0));
    g_XMFlipZ: TXMVECTORU32 = (u: (0, 0, $80000000, 0));
    g_XMFlipW: TXMVECTORU32 = (u: (0, 0, 0, $80000000));
    g_XMFlipYZ: TXMVECTORU32 = (u: (0, $80000000, $80000000, 0));
    g_XMFlipZW: TXMVECTORU32 = (u: (0, 0, $80000000, $80000000));
    g_XMFlipYW: TXMVECTORU32 = (u: (0, $80000000, 0, $80000000));
    g_XMMaskDec4: TXMVECTORI32 = (i: ($3FF, $3FF shl 10, $3FF shl 20, $3 shl 30));
    g_XMXorDec4: TXMVECTORI32 = (i: ($200, $200 shl 10, $200 shl 20, 0));
    g_XMAddUDec4: TXMVECTORF32 = (f: (0, 0, 0, 32768.0 * 65536.0));
    g_XMAddDec4: TXMVECTORF32 = (f: (-512.0, -512.0 * 1024.0, -512.0 * 1024.0 * 1024.0, 0));
    g_XMMulDec4: TXMVECTORF32 = (f: (1.0, 1.0 / 1024.0, 1.0 / (1024.0 * 1024.0), 1.0 / (1024.0 * 1024.0 * 1024.0)));
    g_XMMaskByte4: TXMVECTORU32 = (u: ($FF, $FF00, $FF0000, $FF000000));
    g_XMXorByte4: TXMVECTORI32 = (i: ($80, $8000, $800000, $00000000));
    g_XMAddByte4: TXMVECTORF32 = (f: (-128.0, -128.0 * 256.0, -128.0 * 65536.0, 0));
    g_XMFixUnsigned: TXMVECTORF32 = (f: (32768.0 * 65536.0, 32768.0 * 65536.0, 32768.0 * 65536.0, 32768.0 * 65536.0));
    g_XMMaxInt: TXMVECTORF32 = (f: (65536.0 * 32768.0 - 128.0, 65536.0 * 32768.0 - 128.0, 65536.0 * 32768.0 - 128.0, 65536.0 * 32768.0 - 128.0));
    g_XMMaxUInt: TXMVECTORF32 = (f: (65536.0 * 65536.0 - 256.0, 65536.0 * 65536.0 - 256.0, 65536.0 * 65536.0 - 256.0, 65536.0 * 65536.0 - 256.0));
    g_XMUnsignedFix: TXMVECTORF32 = (f: (32768.0 * 65536.0, 32768.0 * 65536.0, 32768.0 * 65536.0, 32768.0 * 65536.0));
    g_XMsrgbScale: TXMVECTORF32 = (f: (12.92, 12.92, 12.92, 1.0));
    g_XMsrgbA: TXMVECTORF32 = (f: (0.055, 0.055, 0.055, 0.0));
    g_XMsrgbA1: TXMVECTORF32 = (f: (1.055, 1.055, 1.055, 1.0));
    g_XMExponentBias: TXMVECTORI32 = (i: (127, 127, 127, 127));
    g_XMSubnormalExponent: TXMVECTORI32 = (i: (-126, -126, -126, -126));
    g_XMNumTrailing: TXMVECTORI32 = (i: (23, 23, 23, 23));
    g_XMMinNormal: TXMVECTORI32 = (i: ($00800000, $00800000, $00800000, $00800000));
    g_XMNegInfinity: TXMVECTORU32 = (u: ($FF800000, $FF800000, $FF800000, $FF800000));
    g_XMNegQNaN: TXMVECTORU32 = (u: ($FFC00000, $FFC00000, $FFC00000, $FFC00000));
    g_XMBin128: TXMVECTORI32 = (i: ($43000000, $43000000, $43000000, $43000000));
    g_XMBinNeg150: TXMVECTORU32 = (u: ($C3160000, $C3160000, $C3160000, $C3160000));
    g_XM253: TXMVECTORI32 = (i: (253, 253, 253, 253));
    g_XMExpEst1: TXMVECTORF32 = (f: (-6.93147182e-1, -6.93147182e-1, -6.93147182e-1, -6.93147182e-1));
    g_XMExpEst2: TXMVECTORF32 = (f: (+2.40226462e-1, +2.40226462e-1, +2.40226462e-1, +2.40226462e-1));
    g_XMExpEst3: TXMVECTORF32 = (f: (-5.55036440e-2, -5.55036440e-2, -5.55036440e-2, -5.55036440e-2));
    g_XMExpEst4: TXMVECTORF32 = (f: (+9.61597636e-3, +9.61597636e-3, +9.61597636e-3, +9.61597636e-3));
    g_XMExpEst5: TXMVECTORF32 = (f: (-1.32823968e-3, -1.32823968e-3, -1.32823968e-3, -1.32823968e-3));
    g_XMExpEst6: TXMVECTORF32 = (f: (+1.47491097e-4, +1.47491097e-4, +1.47491097e-4, +1.47491097e-4));
    g_XMExpEst7: TXMVECTORF32 = (f: (-1.08635004e-5, -1.08635004e-5, -1.08635004e-5, -1.08635004e-5));
    g_XMLogEst0: TXMVECTORF32 = (f: (+1.442693, +1.442693, +1.442693, +1.442693));
    g_XMLogEst1: TXMVECTORF32 = (f: (-0.721242, -0.721242, -0.721242, -0.721242));
    g_XMLogEst2: TXMVECTORF32 = (f: (+0.479384, +0.479384, +0.479384, +0.479384));
    g_XMLogEst3: TXMVECTORF32 = (f: (-0.350295, -0.350295, -0.350295, -0.350295));
    g_XMLogEst4: TXMVECTORF32 = (f: (+0.248590, +0.248590, +0.248590, +0.248590));
    g_XMLogEst5: TXMVECTORF32 = (f: (-0.145700, -0.145700, -0.145700, -0.145700));
    g_XMLogEst6: TXMVECTORF32 = (f: (+0.057148, +0.057148, +0.057148, +0.057148));
    g_XMLogEst7: TXMVECTORF32 = (f: (-0.010578, -0.010578, -0.010578, -0.010578));
    g_XMLgE: TXMVECTORF32 = (f: (+1.442695, +1.442695, +1.442695, +1.442695));
    g_XMInvLgE: TXMVECTORF32 = (f: (+6.93147182e-1, +6.93147182e-1, +6.93147182e-1, +6.93147182e-1));
    g_UByteMax: TXMVECTORF32 = (f: (255.0, 255.0, 255.0, 255.0));
    g_ByteMin: TXMVECTORF32 = (f: (-127.0, -127.0, -127.0, -127.0));
    g_ByteMax: TXMVECTORF32 = (f: (127.0, 127.0, 127.0, 127.0));
    g_ShortMin: TXMVECTORF32 = (f: (-32767.0, -32767.0, -32767.0, -32767.0));
    g_ShortMax: TXMVECTORF32 = (f: (32767.0, 32767.0, 32767.0, 32767.0));
    g_UShortMax: TXMVECTORF32 = (f: (65535.0, 65535.0, 65535.0, 65535.0));




{***************************************************************************
 *
 * Data conversion operations
 *
 ***************************************************************************}

function XMConvertVectorIntToFloat(constref VInt: TXMVECTOR; constref DivExponent: UINT32): TXMVECTOR;
function XMConvertVectorFloatToInt(constref VFloat: TXMVECTOR; constref MulExponent: UINT32): TXMVECTOR;
function XMConvertVectorUIntToFloat(constref VUInt: TXMVECTOR; constref DivExponent: UINT32): TXMVECTOR;
function XMConvertVectorFloatToUInt(constref VFloat: TXMVECTOR; constref MulExponent: UINT32): TXMVECTOR;


function XMVectorSetBinaryConstant(const C0, C1, C2, c3: UINT32): TXMVECTOR;
function XMVectorSplatConstant(const IntConstant: INT32; const DivExponent: UINT32): TXMVECTOR;
function XMVectorSplatConstantInt(const IntConstant: INT32): TXMVECTOR;


{***************************************************************************
 *
 * Load operations
 *
 ***************************************************************************}

function XMLoadInt(constref pSource: PUINT32): TXMVECTOR;
function XMLoadFloat(constref pSource: PSingle): TXMVECTOR;

function XMLoadInt2(const pSource: PUINT32): TXMVECTOR;   // _In_reads_(2)
function XMLoadInt2A(const PSource: PUINT32): TXMVECTOR;  // _In_reads_(2)
function XMLoadFloat2(const pSource: TXMFLOAT2): TXMVECTOR;
function XMLoadFloat2A(const pSource: TXMFLOAT2A): TXMVECTOR;
function XMLoadSInt2(const pSource: TXMINT2): TXMVECTOR;
function XMLoadUInt2(const pSource: TXMUINT2): TXMVECTOR;

function XMLoadInt3(const pSource: PUINT32): TXMVECTOR;  // _In_reads_(3)
function XMLoadInt3A(const pSource: PUINT32): TXMVECTOR;  // _In_reads_(3)

function XMLoadFloat3(constref pSource: TXMFLOAT3): TXMVECTOR;  overload;
function XMLoadFloat3(constref pSource: pSingle): TXMVECTOR;  overload; // array [0..2] of single;

function XMLoadFloat3A(const pSource: TXMFLOAT3A): TXMVECTOR;
function XMLoadSInt3(const pSource: TXMINT3): TXMVECTOR;
function XMLoadUInt3(const pSource: TXMUINT3): TXMVECTOR;

function XMLoadInt4(const pSource: PUINT32): TXMVECTOR;  // _In_reads_(4)
function XMLoadInt4A(const pSource: PUINT32): TXMVECTOR;  // _In_reads_(4)

function XMLoadFloat4(constref pSource: TXMFLOAT4): TXMVECTOR; overload;
function XMLoadFloat4(constref pSource: PSingle): TXMVECTOR; overload;

function XMLoadFloat4A(const pSource: TXMFLOAT4A): TXMVECTOR;
function XMLoadSInt4(const pSource: TXMINT4): TXMVECTOR;
function XMLoadUInt4(const pSource: TXMUINT4): TXMVECTOR;

function XMLoadFloat3x3(const pSource: TXMFLOAT3X3): TXMMATRIX;
function XMLoadFloat4x3(const pSource: TXMFLOAT4X3): TXMMATRIX;
function XMLoadFloat4x3A(const pSource: TXMFLOAT4X3A): TXMMATRIX;
function XMLoadFloat4x4(const pSource: TXMFLOAT4X4): TXMMATRIX;
function XMLoadFloat4x4A(const pSource: TXMFLOAT4X4A): TXMMATRIX;

{***************************************************************************
 *
 * Store operations
 *
 ***************************************************************************}

//Stores an XMVECTOR in a uint32
procedure XMStoreInt(var pDestination: UINT32; constref V: TXMVECTOR);

// Stores an XMVECTOR in a float
procedure XMStoreFloat(var pDestination: single; V: TXMVECTOR);

// Stores an XMVECTOR in a 2-element uint32 array
procedure XMStoreInt2(var pDestination: array of UINT32; constref V: TXMVECTOR); // _Out_writes_(2)

// Stores an XMVECTOR in a 16-byte aligned 2 element uint32 array
procedure XMStoreInt2A(var pDestination: TUINT32A_Array2; constref V: TXMVECTOR); // _Out_writes_(2)

// Stores an XMVECTOR in an XMFLOAT2
procedure XMStoreFloat2(var pDestination: TXMFLOAT2; constref V: TXMVECTOR);
procedure XMStoreFloat2A(var pDestination: TXMFLOAT2A; constref V: TXMVECTOR);

// Stores signed integer data from an XMVECTOR in an XMINT2 structure.
procedure XMStoreSInt2(var pDestination: TXMINT2; constref V: TXMVECTOR);

// Stores unsigned integer data from an XMVECTOR in an XMUINT2 structure.
procedure XMStoreUInt2(var pDestination: TXMUINT2; constref V: TXMVECTOR);

procedure XMStoreInt3(var pDestination: array of UINT32; constref V: TXMVECTOR);  // _Out_writes_(3)
procedure XMStoreInt3A(var pDestination: array of UINT32; constref V: TXMVECTOR);  // _Out_writes_(3)
procedure XMStoreFloat3(var pDestination: TXMFLOAT3; constref V: TXMVECTOR);
procedure XMStoreFloat3A(var pDestination: TXMFLOAT3A; constref V: TXMVECTOR);

// Stores signed integer data from an XMVECTOR in an XMINT3 structure.
procedure XMStoreSInt3(var pDestination: TXMINT3; constref V: TXMVECTOR);

// Stores unsigned integer data from an XMVECTOR in an XMUINT3 structure.
procedure XMStoreUInt3(var pDestination: TXMUINT3; constref V: TXMVECTOR);

procedure XMStoreInt4(var pDestination: array of UINT32; constref V: TXMVECTOR);   // _Out_writes_(4)
procedure XMStoreInt4A(var pDestination: array of UINT32; constref V: TXMVECTOR);  // _Out_writes_(4)
procedure XMStoreFloat4(var pDestination: TXMFLOAT4; constref V: TXMVECTOR);
procedure XMStoreFloat4A(var pDestination: TXMFLOAT4A; constref V: TXMVECTOR);
procedure XMStoreSInt4(var pDestination: TXMINT4; constref V: TXMVECTOR);
procedure XMStoreUInt4(var pDestination: TXMUINT4; constref V: TXMVECTOR);

procedure XMStoreFloat3x3(var pDestination: TXMFLOAT3X3; constref M: TXMMATRIX);
procedure XMStoreFloat4x3(var pDestination: TXMFLOAT4X3; constref M: TXMMATRIX);
procedure XMStoreFloat4x3A(var pDestination: TXMFLOAT4X3A; constref M: TXMMATRIX);
procedure XMStoreFloat4x4(var pDestination: TXMFLOAT4X4; constref M: TXMMATRIX);
procedure XMStoreFloat4x4A(var pDestination: TXMFLOAT4X4A; constref M: TXMMATRIX);




{***************************************************************************
 *
 * General vector operations
 *
 ***************************************************************************}
function XMVectorZero(): TXMVECTOR;
function XMVectorSet(const x, y, z, w: single): TXMVECTOR;
function XMVectorSetInt(const x, y, z, w: UINT32): TXMVECTOR;
function XMVectorReplicate(const Value: single): TXMVECTOR;
function XMVectorReplicatePtr(pValue: PSingle): TXMVECTOR;
function XMVectorReplicateInt(constref Value: UINT32): TXMVECTOR;
function XMVectorReplicateIntPtr(pValue: PUINT32): TXMVECTOR;
function XMVectorTrueInt(): TXMVECTOR;
function XMVectorFalseInt(): TXMVECTOR;
function XMVectorSplatX(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorSplatY(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorSplatZ(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorSplatW(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorSplatOne(): TXMVECTOR;
function XMVectorSplatInfinity(): TXMVECTOR;
function XMVectorSplatQNaN(): TXMVECTOR;
function XMVectorSplatEpsilon(): TXMVECTOR;
function XMVectorSplatSignMask(): TXMVECTOR;

function XMVectorGetByIndex(V: TXMVECTOR; i: size_t): single;
function XMVectorGetX(constref V: TXMVECTOR): single;
function XMVectorGetY(constref V: TXMVECTOR): single;
function XMVectorGetZ(constref V: TXMVECTOR): single;
function XMVectorGetW(constref V: TXMVECTOR): single;

procedure XMVectorGetByIndexPtr(out f: single; V: TXMVECTOR; i: size_t);
procedure XMVectorGetXPtr(out x: single; constref V: TXMVECTOR);
procedure XMVectorGetYPtr(out y: single; constref V: TXMVECTOR);
procedure XMVectorGetZPtr(out z: single; constref V: TXMVECTOR);
procedure XMVectorGetWPtr(out w: single; constref V: TXMVECTOR);

function XMVectorGetIntByIndex(constref V: TXMVECTOR; constref i: size_t): UINT32;
function XMVectorGetIntX(constref V: TXMVECTOR): UINT32;
function XMVectorGetIntY(constref V: TXMVECTOR): UINT32;
function XMVectorGetIntZ(constref V: TXMVECTOR): UINT32;
function XMVectorGetIntW(constref V: TXMVECTOR): UINT32;

procedure XMVectorGetIntByIndexPtr(out x: UINT32; constref V: TXMVECTOR; constref i: size_t);
procedure XMVectorGetIntXPtr(out x: UINT32; constref V: TXMVECTOR);
procedure XMVectorGetIntYPtr(out y: UINT32; constref V: TXMVECTOR);
procedure XMVectorGetIntZPtr(out z: UINT32; constref V: TXMVECTOR);
procedure XMVectorGetIntWPtr(out w: UINT32; constref V: TXMVECTOR);

function XMVectorSetByIndex(constref V: TXMVECTOR; constref f: single; constref i: size_t): TXMVECTOR;
function XMVectorSetX(constref V: TXMVECTOR; constref x: single): TXMVECTOR;
function XMVectorSetY(constref V: TXMVECTOR; constref y: single): TXMVECTOR;
function XMVectorSetZ(constref V: TXMVECTOR; constref z: single): TXMVECTOR;
function XMVectorSetW(constref V: TXMVECTOR; constref w: single): TXMVECTOR;

function XMVectorSetByIndexPtr(constref V: TXMVECTOR; constref f: Psingle; constref i: size_t): TXMVECTOR;
function XMVectorSetXPtr(constref V: TXMVECTOR; constref x: Psingle): TXMVECTOR;
function XMVectorSetYPtr(constref V: TXMVECTOR; constref y: Psingle): TXMVECTOR;
function XMVectorSetZPtr(constref V: TXMVECTOR; constref z: Psingle): TXMVECTOR;
function XMVectorSetWPtr(constref V: TXMVECTOR; constref w: Psingle): TXMVECTOR;

function XMVectorSetIntByIndex(constref V: TXMVECTOR; constref x: UINT32; constref i: size_t): TXMVECTOR;
function XMVectorSetIntX(constref V: TXMVECTOR; const x: UINT32): TXMVECTOR;
function XMVectorSetIntY(constref V: TXMVECTOR; const y: UINT32): TXMVECTOR;
function XMVectorSetIntZ(constref V: TXMVECTOR; const z: UINT32): TXMVECTOR;
function XMVectorSetIntW(constref V: TXMVECTOR; const w: UINT32): TXMVECTOR;

function XMVectorSetIntByIndexPtr(constref V: TXMVECTOR; constref x: PUINT32; constref i: size_t): TXMVECTOR;
function XMVectorSetIntXPtr(constref V: TXMVECTOR; constref x: PUINT32): TXMVECTOR;
function XMVectorSetIntYPtr(constref V: TXMVECTOR; constref y: PUINT32): TXMVECTOR;
function XMVectorSetIntZPtr(constref V: TXMVECTOR; constref z: PUINT32): TXMVECTOR;
function XMVectorSetIntWPtr(constref V: TXMVECTOR; constref w: PUINT32): TXMVECTOR;

function XMVectorSwizzle(constref V: TXMVECTOR; constref SwizzleX, SwizzleY, SwizzleZ, SwizzleW: UINT32): TXMVECTOR;
function XMVectorPermute(V1: TXMVECTOR; V2: TXMVECTOR; PermuteX: UINT32; PermuteY: UINT32; PermuteZ: UINT32; PermuteW: UINT32): TXMVECTOR;

function XMVectorSelectControl(constref VectorIndex0: UINT32; constref VectorIndex1: UINT32; constref VectorIndex2: UINT32;
    constref VectorIndex3: UINT32): TXMVECTOR;
function XMVectorSelect(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Control: TXMVECTOR): TXMVECTOR;
function XMVectorMergeXY(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorMergeZW(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;


function XMVectorShiftLeft(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Elements: UINT32): TXMVECTOR;
function XMVectorRotateLeft(constref V: TXMVECTOR; constref Elements: UINT32): TXMVECTOR;
function XMVectorRotateRight(constref V: TXMVECTOR; constref Elements: UINT32): TXMVECTOR;
function XMVectorInsert(constref VD: TXMVECTOR; constref VS: TXMVECTOR; constref VSLeftRotateElements: UINT32;
    constref Select0: UINT32; constref Select1: UINT32; constref Select2: UINT32; constref Select3: UINT32): TXMVECTOR;

function XMVectorEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorEqualR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorEqualIntR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorNearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): TXMVECTOR;
function XMVectorNotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorNotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorGreater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorGreaterR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorGreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorGreaterOrEqualR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorLess(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorLessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorInBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): TXMVECTOR;
function XMVectorInBoundsR(out pCR: UINT32; constref V: TXMVECTOR; constref Bounds: TXMVECTOR): TXMVECTOR;

function XMVectorIsNaN(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorIsInfinite(constref V: TXMVECTOR): TXMVECTOR;

function XMVectorMin(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorMax(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorRound(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorTruncate(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorFloor(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorCeiling(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorClamp(constref V: TXMVECTOR; constref Min: TXMVECTOR; constref Max: TXMVECTOR): TXMVECTOR;
function XMVectorSaturate(constref V: TXMVECTOR): TXMVECTOR;

function XMVectorAndInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorAndCInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorOrInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorNorInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorXorInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;

function XMVectorNegate(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorAdd(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorSum(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorAddAngles(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorSubtract(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorSubtractAngles(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorMultiply(const V1: TXMVECTOR; const V2: TXMVECTOR): TXMVECTOR;
function XMVectorMultiplyAdd(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR;
function XMVectorDivide(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorNegativeMultiplySubtract(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR;
function XMVectorScale(constref V: TXMVECTOR; constref ScaleFactor: single): TXMVECTOR;
function XMVectorReciprocalEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorReciprocal(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorSqrtEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorSqrt(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorReciprocalSqrtEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorReciprocalSqrt(const V: TXMVECTOR): TXMVECTOR;
function XMVectorExp2(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorExpE(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorExp(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorLog2(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorLogE(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorLog(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorPow(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorAbs(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorMod(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVectorModAngles(constref Angles: TXMVECTOR): TXMVECTOR;
function XMVectorSin(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorSinEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorCos(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorCosEst(constref V: TXMVECTOR): TXMVECTOR;
procedure XMVectorSinCos(out pSin: TXMVECTOR; out pCos: TXMVECTOR; constref V: TXMVECTOR);
procedure XMVectorSinCosEst(out pSin: TXMVECTOR; out pCos: TXMVECTOR; constref V: TXMVECTOR);
function XMVectorTan(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorTanEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorSinH(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorCosH(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorTanH(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorASin(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorASinEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorACos(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorACosEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorATan(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorATanEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVectorATan2(constref Y: TXMVECTOR; constref X: TXMVECTOR): TXMVECTOR;
function XMVectorATan2Est(constref Y: TXMVECTOR; constref X: TXMVECTOR): TXMVECTOR;
function XMVectorLerp(constref V0: TXMVECTOR; constref V1: TXMVECTOR; constref t: single): TXMVECTOR;
function XMVectorLerpV(constref V0: TXMVECTOR; constref V1: TXMVECTOR; constref T: TXMVECTOR): TXMVECTOR;
function XMVectorHermite(constref Position0: TXMVECTOR; constref Tangent0: TXMVECTOR; constref Position1: TXMVECTOR;
    constref Tangent1: TXMVECTOR; constref t: single): TXMVECTOR;
function XMVectorHermiteV(constref Position0: TXMVECTOR; constref Tangent0: TXMVECTOR; constref Position1: TXMVECTOR;
    constref Tangent1: TXMVECTOR; constref T: TXMVECTOR): TXMVECTOR;
function XMVectorCatmullRom(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref Position3: TXMVECTOR; constref t: single): TXMVECTOR;
function XMVectorCatmullRomV(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref Position3: TXMVECTOR; constref T: TXMVECTOR): TXMVECTOR;
function XMVectorBaryCentric(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref f: single; constref g: single): TXMVECTOR;
function XMVectorBaryCentricV(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref F: TXMVECTOR; constref G: TXMVECTOR): TXMVECTOR;



{***************************************************************************
 *
 * 2D vector operations
 *
 ***************************************************************************}

function XMVector2Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector2EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector2EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector2EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector2NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean;
function XMVector2NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector2NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector2Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector2GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector2GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector2GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector2Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector2LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector2InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean;

function XMVector2IsNaN(constref V: TXMVECTOR): boolean;
function XMVector2IsInfinite(constref V: TXMVECTOR): boolean;

function XMVector2Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVector2Cross(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVector2LengthSq(constref V: TXMVECTOR): TXMVECTOR;
function XMVector2ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVector2ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR;
function XMVector2LengthEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVector2Length(constref V: TXMVECTOR): TXMVECTOR;
function XMVector2NormalizeEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVector2Normalize(constref V: TXMVECTOR): TXMVECTOR;
function XMVector2ClampLength(constref V: TXMVECTOR; constref LengthMin: single; constref LengthMax: single): TXMVECTOR;
function XMVector2ClampLengthV(constref V: TXMVECTOR; constref LengthMin: TXMVECTOR; constref LengthMax: TXMVECTOR): TXMVECTOR;
function XMVector2Reflect(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR): TXMVECTOR;
function XMVector2Refract(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR; constref RefractionIndex: single): TXMVECTOR;
function XMVector2RefractV(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR; constref RefractionIndex: TXMVECTOR): TXMVECTOR;
function XMVector2Orthogonal(constref V: TXMVECTOR): TXMVECTOR;
function XMVector2AngleBetweenNormalsEst(constref N1: TXMVECTOR; constref N2: TXMVECTOR): TXMVECTOR;
function XMVector2AngleBetweenNormals(constref N1: TXMVECTOR; constref N2: TXMVECTOR): TXMVECTOR;
function XMVector2AngleBetweenVectors(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVector2LinePointDistance(constref LinePoint1: TXMVECTOR; constref LinePoint2: TXMVECTOR; constref Point: TXMVECTOR): TXMVECTOR;
function XMVector2IntersectLine(constref Line1Point1: TXMVECTOR; constref Line1Point2: TXMVECTOR; constref Line2Point1: TXMVECTOR;
    constref Line2Point2: TXMVECTOR): TXMVECTOR;
function XMVector2Transform(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
function XMVector2TransformStream(out pOutputStream: PXMFLOAT4; constref OutputStride: size_t; constref pInputStream: PXMFLOAT2;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT4;
function XMVector2TransformCoord(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
function XMVector2TransformCoordStream(out pOutputStream: PXMFLOAT2; constref OutputStride: size_t; constref pInputStream: PXMFLOAT2;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT2;
function XMVector2TransformNormal(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
function XMVector2TransformNormalStream(out pOutputStream: PXMFLOAT2; constref OutputStride: size_t; constref pInputStream: PXMFLOAT2;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT2;


{***************************************************************************
 *
 * 3D vector operations
 *
 ***************************************************************************}

function XMVector3Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector3EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector3EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector3EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector3NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean;
function XMVector3NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector3NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector3Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector3GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector3GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector3GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector3Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector3LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector3InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean;

function XMVector3IsNaN(constref V: TXMVECTOR): boolean;
function XMVector3IsInfinite(constref V: TXMVECTOR): boolean;

function XMVector3Dot(const V1: TXMVECTOR; const V2: TXMVECTOR): TXMVECTOR;
function XMVector3Cross(const V1: TXMVECTOR; const V2: TXMVECTOR): TXMVECTOR;
function XMVector3LengthSq(const V: TXMVECTOR): TXMVECTOR;
function XMVector3ReciprocalLengthEst(const V: TXMVECTOR): TXMVECTOR;
function XMVector3ReciprocalLength(const V: TXMVECTOR): TXMVECTOR;
function XMVector3LengthEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVector3Length(constref V: TXMVECTOR): TXMVECTOR;
function XMVector3NormalizeEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVector3Normalize(constref V: TXMVECTOR): TXMVECTOR;
function XMVector3ClampLength(constref V: TXMVECTOR; constref LengthMin: single; constref LengthMax: single): TXMVECTOR;
function XMVector3ClampLengthV(constref V: TXMVECTOR; constref LengthMin: TXMVECTOR; constref LengthMax: TXMVECTOR): TXMVECTOR;
function XMVector3Reflect(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR): TXMVECTOR;
function XMVector3Refract(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR; constref RefractionIndex: single): TXMVECTOR;
function XMVector3RefractV(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR; constref RefractionIndex: TXMVECTOR): TXMVECTOR;
function XMVector3Orthogonal(constref V: TXMVECTOR): TXMVECTOR;
function XMVector3AngleBetweenNormalsEst(constref N1: TXMVECTOR; constref N2: TXMVECTOR): TXMVECTOR;
function XMVector3AngleBetweenNormals(constref N1: TXMVECTOR; constref N2: TXMVECTOR): TXMVECTOR;
function XMVector3AngleBetweenVectors(const V1: TXMVECTOR; const V2: TXMVECTOR): TXMVECTOR;
function XMVector3LinePointDistance(constref LinePoint1: TXMVECTOR; constref LinePoint2: TXMVECTOR; constref Point: TXMVECTOR): TXMVECTOR;
procedure XMVector3ComponentsFromNormal(out pParallel: TXMVECTOR; out pPerpendicular: TXMVECTOR; constref A: TXMVECTOR; constref Normal: TXMVECTOR);
function XMVector3Rotate(constref V: TXMVECTOR; constref RotationQuaternion: TXMVECTOR): TXMVECTOR;
function XMVector3InverseRotate(constref V: TXMVECTOR; constref RotationQuaternion: TXMVECTOR): TXMVECTOR;
function XMVector3Transform(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
function XMVector3TransformStream(out pOutputStream: PXMFLOAT4; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT4;
function XMVector3TransformCoord(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
function XMVector3TransformCoordStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT3;
function XMVector3TransformNormal(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
function XMVector3TransformNormalStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT3;
function XMVector3Project(V: TXMVECTOR; ViewportX: single; ViewportY: single; ViewportWidth: single; ViewportHeight: single;
    ViewportMinZ: single; ViewportMaxZ: single; Projection: TXMMATRIX; View: TXMMATRIX; World: TXMMATRIX): TXMVECTOR;
function XMVector3ProjectStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref ViewportX: single; constref ViewportY: single;
    constref ViewportWidth: single; constref ViewportHeight: single; constref ViewportMinZ: single; constref ViewportMaxZ: single;
    constref Projection: TXMMATRIX; constref View: TXMMATRIX; constref World: TXMMATRIX): PXMFLOAT3;

function XMVector3Unproject(V: TXMVECTOR; ViewportX: single; ViewportY: single; ViewportWidth: single; ViewportHeight: single;
    ViewportMinZ: single; ViewportMaxZ: single; Projection: TXMMATRIX; View: TXMMATRIX; World: TXMMATRIX): TXMVECTOR;


function XMVector3UnprojectStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref ViewportX: single; constref ViewportY: single;
    constref ViewportWidth: single; constref ViewportHeight: single; constref ViewportMinZ: single; constref ViewportMaxZ: single;
    constref Projection: TXMMATRIX; constref View: TXMMATRIX; constref World: TXMMATRIX): PXMFLOAT3;

{***************************************************************************
 *
 * 4D vector operations
 *
 ***************************************************************************}

function XMVector4Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector4EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector4EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector4EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector4NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean;
function XMVector4NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector4NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector4Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector4GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector4GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector4GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
function XMVector4Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector4LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
function XMVector4InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean;

function XMVector4IsNaN(constref V: TXMVECTOR): boolean;
function XMVector4IsInfinite(constref V: TXMVECTOR): boolean;

function XMVector4Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
function XMVector4Cross(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR;
function XMVector4LengthSq(constref V: TXMVECTOR): TXMVECTOR;
function XMVector4ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVector4ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR;
function XMVector4LengthEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVector4Length(constref V: TXMVECTOR): TXMVECTOR;
function XMVector4NormalizeEst(constref V: TXMVECTOR): TXMVECTOR;
function XMVector4Normalize(constref V: TXMVECTOR): TXMVECTOR;
function XMVector4ClampLength(V: TXMVECTOR; LengthMin: single; LengthMax: single): TXMVECTOR;
function XMVector4ClampLengthV(V: TXMVECTOR; LengthMin: TXMVECTOR; LengthMax: TXMVECTOR): TXMVECTOR;
function XMVector4Reflect(Incident: TXMVECTOR; Normal: TXMVECTOR): TXMVECTOR;
function XMVector4Refract(Incident: TXMVECTOR; Normal: TXMVECTOR; RefractionIndex: single): TXMVECTOR;
function XMVector4RefractV(Incident: TXMVECTOR; Normal: TXMVECTOR; RefractionIndex: TXMVECTOR): TXMVECTOR;
function XMVector4Orthogonal(constref V: TXMVECTOR): TXMVECTOR;
function XMVector4AngleBetweenNormalsEst(N1: TXMVECTOR; N2: TXMVECTOR): TXMVECTOR;
function XMVector4AngleBetweenNormals(N1: TXMVECTOR; N2: TXMVECTOR): TXMVECTOR;
function XMVector4AngleBetweenVectors(V1: TXMVECTOR; V2: TXMVECTOR): TXMVECTOR;
function XMVector4Transform(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
function XMVector4TransformStream(out pOutputStream: PXMFLOAT4; constref OutputStride: size_t; constref pInputStream: PXMFLOAT4;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT4;

{***************************************************************************
 *
 * Matrix operations
 *
 ***************************************************************************}

function XMMatrixIsNaN(constref M: TXMMATRIX): boolean;
function XMMatrixIsInfinite(M: TXMMATRIX): boolean;
function XMMatrixIsIdentity(M: TXMMATRIX): boolean;

function XMMatrixMultiply(M1: TXMMATRIX; M2: TXMMATRIX): TXMMATRIX;
function XMMatrixMultiplyTranspose(M1: TXMMATRIX; M2: TXMMATRIX): TXMMATRIX;
function XMMatrixTranspose(M: TXMMATRIX): TXMMATRIX;
function XMMatrixInverse(out pDeterminant: TXMVECTOR; M: TXMMATRIX): TXMMATRIX;
function XMMatrixDeterminant(M: TXMMATRIX): TXMVECTOR;
function XMMatrixDecompose(out outScale: TXMVECTOR; out outRotQuat: TXMVECTOR; out outTrans: TXMVECTOR; M: TXMMATRIX): boolean;

function XMMatrixIdentity(): TXMMATRIX;
function XMMatrixSet(m00: single; m01: single; m02: single; m03: single; m10: single; m11: single; m12: single; m13: single;
    m20: single; m21: single; m22: single; m23: single; m30: single; m31: single; m32: single; m33: single): TXMMATRIX;
function XMMatrixTranslation(OffsetX: single; OffsetY: single; OffsetZ: single): TXMMATRIX;
function XMMatrixTranslationFromVector(Offset: TXMVECTOR): TXMMATRIX;
function XMMatrixScaling(constref ScaleX, ScaleY, ScaleZ: single): TXMMATRIX;
function XMMatrixScalingFromVector(constref Scale: TXMVECTOR): TXMMATRIX;
function XMMatrixRotationX(Angle: single): TXMMATRIX;
function XMMatrixRotationY(Angle: single): TXMMATRIX;
function XMMatrixRotationZ(Angle: single): TXMMATRIX;
function XMMatrixRotationRollPitchYaw(Pitch: single; Yaw: single; Roll: single): TXMMATRIX;
function XMMatrixRotationRollPitchYawFromVector(Angles: TXMVECTOR): TXMMATRIX;
function XMMatrixRotationNormal(constref NormalAxis: TXMVECTOR; constref Angle: single): TXMMATRIX;
function XMMatrixRotationAxis(Axis: TXMVECTOR; Angle: single): TXMMATRIX;
function XMMatrixRotationQuaternion(Quaternion: TXMVECTOR): TXMMATRIX;
function XMMatrixTransformation2D(ScalingOrigin: TXMVECTOR; ScalingOrientation: single; Scaling: TXMVECTOR; RotationOrigin: TXMVECTOR;
    Rotation: single; Translation: TXMVECTOR): TXMMATRIX;
function XMMatrixTransformation(ScalingOrigin: TXMVECTOR; ScalingOrientationQuaternion: TXMVECTOR; Scaling: TXMVECTOR;
    RotationOrigin: TXMVECTOR; RotationQuaternion: TXMVECTOR; Translation: TXMVECTOR): TXMMATRIX;
function XMMatrixAffineTransformation2D(Scaling: TXMVECTOR; RotationOrigin: TXMVECTOR; Rotation: single; Translation: TXMVECTOR): TXMMATRIX;
function XMMatrixAffineTransformation(Scaling: TXMVECTOR; RotationOrigin: TXMVECTOR; RotationQuaternion: TXMVECTOR;
    Translation: TXMVECTOR): TXMMATRIX;
function XMMatrixReflect(ReflectionPlane: TXMVECTOR): TXMMATRIX;
function XMMatrixShadow(ShadowPlane: TXMVECTOR; LightPosition: TXMVECTOR): TXMMATRIX;

function XMMatrixLookAtLH(EyePosition: TXMVECTOR; FocusPosition: TXMVECTOR; UpDirection: TXMVECTOR): TXMMATRIX;
function XMMatrixLookAtRH(EyePosition: TXMVECTOR; FocusPosition: TXMVECTOR; UpDirection: TXMVECTOR): TXMMATRIX;
function XMMatrixLookToLH(EyePosition: TXMVECTOR; EyeDirection: TXMVECTOR; UpDirection: TXMVECTOR): TXMMATRIX;
function XMMatrixLookToRH(EyePosition: TXMVECTOR; EyeDirection: TXMVECTOR; UpDirection: TXMVECTOR): TXMMATRIX;
function XMMatrixPerspectiveLH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
function XMMatrixPerspectiveRH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
function XMMatrixPerspectiveFovLH(FovAngleY: single; AspectRatio: single; NearZ: single; FarZ: single): TXMMATRIX;
function XMMatrixPerspectiveFovRH(FovAngleY: single; AspectRatio: single; NearZ: single; FarZ: single): TXMMATRIX;
function XMMatrixPerspectiveOffCenterLH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single;
    FarZ: single): TXMMATRIX;
function XMMatrixPerspectiveOffCenterRH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single;
    FarZ: single): TXMMATRIX;
function XMMatrixOrthographicLH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
function XMMatrixOrthographicRH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
function XMMatrixOrthographicOffCenterLH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single;
    NearZ: single; FarZ: single): TXMMATRIX;
function XMMatrixOrthographicOffCenterRH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single;
    NearZ: single; FarZ: single): TXMMATRIX;


{***************************************************************************
 *
 * Quaternion operations
 *
 ***************************************************************************}

function XMQuaternionEqual(Q1: TXMVECTOR; Q2: TXMVECTOR): boolean;
function XMQuaternionNotEqual(Q1: TXMVECTOR; Q2: TXMVECTOR): boolean;

function XMQuaternionIsNaN(Q: TXMVECTOR): boolean;
function XMQuaternionIsInfinite(Q: TXMVECTOR): boolean;
function XMQuaternionIsIdentity(Q: TXMVECTOR): boolean;

function XMQuaternionDot(Q1: TXMVECTOR; Q2: TXMVECTOR): TXMVECTOR;
function XMQuaternionMultiply(Q1: TXMVECTOR; Q2: TXMVECTOR): TXMVECTOR;
function XMQuaternionLengthSq(Q: TXMVECTOR): TXMVECTOR;
function XMQuaternionReciprocalLength(Q: TXMVECTOR): TXMVECTOR;
function XMQuaternionLength(Q: TXMVECTOR): TXMVECTOR;
function XMQuaternionNormalizeEst(Q: TXMVECTOR): TXMVECTOR;
function XMQuaternionNormalize(Q: TXMVECTOR): TXMVECTOR;
function XMQuaternionConjugate(Q: TXMVECTOR): TXMVECTOR;
function XMQuaternionInverse(Q: TXMVECTOR): TXMVECTOR;
function XMQuaternionLn(Q: TXMVECTOR): TXMVECTOR;
function XMQuaternionExp(Q: TXMVECTOR): TXMVECTOR;
function XMQuaternionSlerp(Q0: TXMVECTOR; Q1: TXMVECTOR; t: single): TXMVECTOR;
function XMQuaternionSlerpV(Q0: TXMVECTOR; Q1: TXMVECTOR; T: TXMVECTOR): TXMVECTOR;
function XMQuaternionSquad(Q0: TXMVECTOR; Q1: TXMVECTOR; Q2: TXMVECTOR; Q3: TXMVECTOR; t: single): TXMVECTOR;
function XMQuaternionSquadV(Q0: TXMVECTOR; Q1: TXMVECTOR; Q2: TXMVECTOR; Q3: TXMVECTOR; T: TXMVECTOR): TXMVECTOR;
procedure XMQuaternionSquadSetup(out pA: TXMVECTOR; out pB: TXMVECTOR; out pC: TXMVECTOR; Q0: TXMVECTOR; Q1: TXMVECTOR;
    Q2: TXMVECTOR; Q3: TXMVECTOR);
function XMQuaternionBaryCentric(Q0: TXMVECTOR; Q1: TXMVECTOR; Q2: TXMVECTOR; f: single; g: single): TXMVECTOR;
function XMQuaternionBaryCentricV(Q0: TXMVECTOR; Q1: TXMVECTOR; Q2: TXMVECTOR; F: TXMVECTOR; G: TXMVECTOR): TXMVECTOR;

function XMQuaternionIdentity(): TXMVECTOR;
function XMQuaternionRotationRollPitchYaw(Pitch: single; Yaw: single; Roll: single): TXMVECTOR;
function XMQuaternionRotationRollPitchYawFromVector(Angles: TXMVECTOR): TXMVECTOR;
function XMQuaternionRotationNormal(NormalAxis: TXMVECTOR; Angle: single): TXMVECTOR;
function XMQuaternionRotationAxis(Axis: TXMVECTOR; Angle: single): TXMVECTOR;
function XMQuaternionRotationMatrix(M: TXMMATRIX): TXMVECTOR;

procedure XMQuaternionToAxisAngle(out pAxis: TXMVECTOR; out pAngle: single; Q: TXMVECTOR);

{***************************************************************************
 *
 * Plane operations
 *
 ***************************************************************************}

function XMPlaneEqual(P1: TXMVECTOR; P2: TXMVECTOR): boolean;
function XMPlaneNearEqual(P1: TXMVECTOR; P2: TXMVECTOR; Epsilon: TXMVECTOR): boolean;
function XMPlaneNotEqual(P1: TXMVECTOR; P2: TXMVECTOR): boolean;

function XMPlaneIsNaN(P: TXMVECTOR): boolean;
function XMPlaneIsInfinite(P: TXMVECTOR): boolean;

function XMPlaneDot(P: TXMVECTOR; V: TXMVECTOR): TXMVECTOR;
function XMPlaneDotCoord(P: TXMVECTOR; V: TXMVECTOR): TXMVECTOR;
function XMPlaneDotNormal(P: TXMVECTOR; V: TXMVECTOR): TXMVECTOR;
function XMPlaneNormalizeEst(P: TXMVECTOR): TXMVECTOR;
function XMPlaneNormalize(P: TXMVECTOR): TXMVECTOR;
function XMPlaneIntersectLine(P: TXMVECTOR; LinePoint1: TXMVECTOR; LinePoint2: TXMVECTOR): TXMVECTOR;
procedure XMPlaneIntersectPlane(out pLinePoint1: TXMVECTOR; out pLinePoint2: TXMVECTOR; P1: TXMVECTOR; P2: TXMVECTOR);
function XMPlaneTransform(P: TXMVECTOR; M: TXMMATRIX): TXMVECTOR;
function XMPlaneTransformStream(out pOutputStream: PXMFLOAT4; OutputStride: size_t; constref pInputStream: PXMFLOAT4;
    InputStride: size_t; PlaneCount: size_t; M: TXMMATRIX): PXMFLOAT4;

function XMPlaneFromPointNormal(Point: TXMVECTOR; Normal: TXMVECTOR): TXMVECTOR;
function XMPlaneFromPoints(Point1: TXMVECTOR; Point2: TXMVECTOR; Point3: TXMVECTOR): TXMVECTOR;

{***************************************************************************
 *
 * Color operations
 *
 ***************************************************************************}

function XMColorEqual(C1: TXMVECTOR; C2: TXMVECTOR): boolean;
function XMColorNotEqual(C1: TXMVECTOR; C2: TXMVECTOR): boolean;
function XMColorGreater(C1: TXMVECTOR; C2: TXMVECTOR): boolean;
function XMColorGreaterOrEqual(C1: TXMVECTOR; C2: TXMVECTOR): boolean;
function XMColorLess(C1: TXMVECTOR; C2: TXMVECTOR): boolean;
function XMColorLessOrEqual(C1: TXMVECTOR; C2: TXMVECTOR): boolean;

function XMColorIsNaN(C: TXMVECTOR): boolean;
function XMColorIsInfinite(C: TXMVECTOR): boolean;

function XMColorNegative(constref vColor: TXMVECTOR): TXMVECTOR;
function XMColorModulate(C1: TXMVECTOR; C2: TXMVECTOR): TXMVECTOR;
function XMColorAdjustSaturation(constref vColor: TXMVECTOR; Saturation: single): TXMVECTOR;
function XMColorAdjustContrast(constref vColor: TXMVECTOR; constref Contrast: single): TXMVECTOR;

function XMColorRGBToHSL(rgb: TXMVECTOR): TXMVECTOR;
function XMColorHSLToRGB(hsl: TXMVECTOR): TXMVECTOR;

function XMColorRGBToHSV(rgb: TXMVECTOR): TXMVECTOR;
function XMColorHSVToRGB(hsv: TXMVECTOR): TXMVECTOR;

function XMColorRGBToYUV(rgb: TXMVECTOR): TXMVECTOR;
function XMColorYUVToRGB(yuv: TXMVECTOR): TXMVECTOR;

function XMColorRGBToYUV_HD(rgb: TXMVECTOR): TXMVECTOR;
function XMColorYUVToRGB_HD(yuv: TXMVECTOR): TXMVECTOR;

function XMColorRGBToXYZ(rgb: TXMVECTOR): TXMVECTOR;
function XMColorXYZToRGB(xyz: TXMVECTOR): TXMVECTOR;

function XMColorXYZToSRGB(xyz: TXMVECTOR): TXMVECTOR;
function XMColorSRGBToXYZ(srgb: TXMVECTOR): TXMVECTOR;

function XMColorRGBToSRGB(rgb: TXMVECTOR): TXMVECTOR;
function XMColorSRGBToRGB(srgb: TXMVECTOR): TXMVECTOR;


{***************************************************************************
 *
 * Miscellaneous operations
 *
 ***************************************************************************}

function XMVerifyCPUSupport(CPURequired: TCPUType): boolean;

function XMFresnelTerm(CosIncidentAngle: TXMVECTOR; RefractionIndex: TXMVECTOR): TXMVECTOR;

function XMScalarNearEqual(S1: single; S2: single; Epsilon: single): boolean;
function XMScalarModAngle(Angle: single): single;

function XMScalarSin(Value: single): single;
function XMScalarSinEst(Value: single): single;

function XMScalarCos(Value: single): single;
function XMScalarCosEst(Value: single): single;

procedure XMScalarSinCos(out pSin: single; out pCos: single; Value: single);
procedure XMScalarSinCosEst(out pSin: single; out pCos: single; Value: single);

function XMScalarASin(Value: single): single;
function XMScalarASinEst(Value: single): single;

function XMScalarACos(Value: single): single;
function XMScalarACosEst(Value: single): single;

function XMMin(a, b: single): single;
function XMMax(a, b: single): single;



implementation

uses
    Math;

const
    { Define all possible Shuffle operations }
    // _MM_SHUFFLE_z_y_x_w
    // 0,0
    _MM_SHUFFLE_0_0_0_0 = (0 shl 6) or (0 shl 4) or (0 shl 2) or 0; // $00
    _MM_SHUFFLE_0_0_0_1 = (0 shl 6) or (0 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_0_0_0_2 = (0 shl 6) or (0 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_0_0_0_3 = (0 shl 6) or (0 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_0_0_1_0 = (0 shl 6) or (0 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_0_0_1_1 = (0 shl 6) or (0 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_0_0_1_2 = (0 shl 6) or (0 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_0_0_1_3 = (0 shl 6) or (0 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_0_0_2_0 = (0 shl 6) or (0 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_0_0_2_1 = (0 shl 6) or (0 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_0_0_2_2 = (0 shl 6) or (0 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_0_0_2_3 = (0 shl 6) or (0 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_0_0_3_0 = (0 shl 6) or (0 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_0_0_3_1 = (0 shl 6) or (0 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_0_0_3_2 = (0 shl 6) or (0 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_0_0_3_3 = (0 shl 6) or (0 shl 4) or (3 shl 2) or 3; // $

    // 0,1
    _MM_SHUFFLE_0_1_0_0 = (0 shl 6) or (1 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_0_1_0_1 = (0 shl 6) or (1 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_0_1_0_2 = (0 shl 6) or (1 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_0_1_0_3 = (0 shl 6) or (1 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_0_1_1_0 = (0 shl 6) or (1 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_0_1_1_1 = (0 shl 6) or (1 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_0_1_1_2 = (0 shl 6) or (1 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_0_1_1_3 = (0 shl 6) or (1 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_0_1_2_0 = (0 shl 6) or (1 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_0_1_2_1 = (0 shl 6) or (1 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_0_1_2_2 = (0 shl 6) or (1 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_0_1_2_3 = (0 shl 6) or (1 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_0_1_3_0 = (0 shl 6) or (1 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_0_1_3_1 = (0 shl 6) or (1 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_0_1_3_2 = (0 shl 6) or (1 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_0_1_3_3 = (0 shl 6) or (1 shl 4) or (3 shl 2) or 3; // $

    // 0,2
    _MM_SHUFFLE_0_2_0_0 = (0 shl 6) or (2 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_0_2_0_1 = (0 shl 6) or (2 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_0_2_0_2 = (0 shl 6) or (2 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_0_2_0_3 = (0 shl 6) or (2 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_0_2_1_0 = (0 shl 6) or (2 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_0_2_1_1 = (0 shl 6) or (2 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_0_2_1_2 = (0 shl 6) or (2 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_0_2_1_3 = (0 shl 6) or (2 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_0_2_2_0 = (0 shl 6) or (2 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_0_2_2_1 = (0 shl 6) or (2 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_0_2_2_2 = (0 shl 6) or (2 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_0_2_2_3 = (0 shl 6) or (2 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_0_2_3_0 = (0 shl 6) or (2 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_0_2_3_1 = (0 shl 6) or (2 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_0_2_3_2 = (0 shl 6) or (2 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_0_2_3_3 = (0 shl 6) or (2 shl 4) or (3 shl 2) or 3; // $

    // 0,3
    _MM_SHUFFLE_0_3_0_0 = (0 shl 6) or (3 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_0_3_0_1 = (0 shl 6) or (3 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_0_3_0_2 = (0 shl 6) or (3 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_0_3_0_3 = (0 shl 6) or (3 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_0_3_1_0 = (0 shl 6) or (3 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_0_3_1_1 = (0 shl 6) or (3 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_0_3_1_2 = (0 shl 6) or (3 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_0_3_1_3 = (0 shl 6) or (3 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_0_3_2_0 = (0 shl 6) or (3 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_0_3_2_1 = (0 shl 6) or (3 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_0_3_2_2 = (0 shl 6) or (3 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_0_3_2_3 = (0 shl 6) or (3 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_0_3_3_0 = (0 shl 6) or (3 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_0_3_3_1 = (0 shl 6) or (3 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_0_3_3_2 = (0 shl 6) or (3 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_0_3_3_3 = (0 shl 6) or (3 shl 4) or (3 shl 2) or 3; // $

    // 1,0
    _MM_SHUFFLE_1_0_0_0 = (1 shl 6) or (0 shl 4) or (0 shl 2) or 0; // $00
    _MM_SHUFFLE_1_0_0_1 = (1 shl 6) or (0 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_1_0_0_2 = (1 shl 6) or (0 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_1_0_0_3 = (1 shl 6) or (0 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_1_0_1_0 = (1 shl 6) or (0 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_1_0_1_1 = (1 shl 6) or (0 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_1_0_1_2 = (1 shl 6) or (0 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_1_0_1_3 = (1 shl 6) or (0 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_1_0_2_0 = (1 shl 6) or (0 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_1_0_2_1 = (1 shl 6) or (0 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_1_0_2_2 = (1 shl 6) or (0 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_1_0_2_3 = (1 shl 6) or (0 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_1_0_3_0 = (1 shl 6) or (0 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_1_0_3_1 = (1 shl 6) or (0 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_1_0_3_2 = (1 shl 6) or (0 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_1_0_3_3 = (1 shl 6) or (0 shl 4) or (3 shl 2) or 3; // $

    // 1,1
    _MM_SHUFFLE_1_1_0_0 = (1 shl 6) or (1 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_1_1_0_1 = (1 shl 6) or (1 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_1_1_0_2 = (1 shl 6) or (1 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_1_1_0_3 = (1 shl 6) or (1 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_1_1_1_0 = (1 shl 6) or (1 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_1_1_1_1 = (1 shl 6) or (1 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_1_1_1_2 = (1 shl 6) or (1 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_1_1_1_3 = (1 shl 6) or (1 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_1_1_2_0 = (1 shl 6) or (1 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_1_1_2_1 = (1 shl 6) or (1 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_1_1_2_2 = (1 shl 6) or (1 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_1_1_2_3 = (1 shl 6) or (1 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_1_1_3_0 = (1 shl 6) or (1 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_1_1_3_1 = (1 shl 6) or (1 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_1_1_3_2 = (1 shl 6) or (1 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_1_1_3_3 = (1 shl 6) or (1 shl 4) or (3 shl 2) or 3; // $

    // 1,2
    _MM_SHUFFLE_1_2_0_0 = (1 shl 6) or (2 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_1_2_0_1 = (1 shl 6) or (2 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_1_2_0_2 = (1 shl 6) or (2 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_1_2_0_3 = (1 shl 6) or (2 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_1_2_1_0 = (1 shl 6) or (2 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_1_2_1_1 = (1 shl 6) or (2 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_1_2_1_2 = (1 shl 6) or (2 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_1_2_1_3 = (1 shl 6) or (2 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_1_2_2_0 = (1 shl 6) or (2 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_1_2_2_1 = (1 shl 6) or (2 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_1_2_2_2 = (1 shl 6) or (2 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_1_2_2_3 = (1 shl 6) or (2 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_1_2_3_0 = (1 shl 6) or (2 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_1_2_3_1 = (1 shl 6) or (2 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_1_2_3_2 = (1 shl 6) or (2 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_1_2_3_3 = (1 shl 6) or (2 shl 4) or (3 shl 2) or 3; // $

    // 1,3
    _MM_SHUFFLE_1_3_0_0 = (1 shl 6) or (3 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_1_3_0_1 = (1 shl 6) or (3 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_1_3_0_2 = (1 shl 6) or (3 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_1_3_0_3 = (1 shl 6) or (3 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_1_3_1_0 = (1 shl 6) or (3 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_1_3_1_1 = (1 shl 6) or (3 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_1_3_1_2 = (1 shl 6) or (3 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_1_3_1_3 = (1 shl 6) or (3 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_1_3_2_0 = (1 shl 6) or (3 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_1_3_2_1 = (1 shl 6) or (3 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_1_3_2_2 = (1 shl 6) or (3 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_1_3_2_3 = (1 shl 6) or (3 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_1_3_3_0 = (1 shl 6) or (3 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_1_3_3_1 = (1 shl 6) or (3 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_1_3_3_2 = (1 shl 6) or (3 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_1_3_3_3 = (1 shl 6) or (3 shl 4) or (3 shl 2) or 3; // $


    // 2,0
    _MM_SHUFFLE_2_0_0_0 = (2 shl 6) or (0 shl 4) or (0 shl 2) or 0; // $00
    _MM_SHUFFLE_2_0_0_1 = (2 shl 6) or (0 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_2_0_0_2 = (2 shl 6) or (0 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_2_0_0_3 = (2 shl 6) or (0 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_2_0_1_0 = (2 shl 6) or (0 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_2_0_1_1 = (2 shl 6) or (0 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_2_0_1_2 = (2 shl 6) or (0 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_2_0_1_3 = (2 shl 6) or (0 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_2_0_2_0 = (2 shl 6) or (0 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_2_0_2_1 = (2 shl 6) or (0 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_2_0_2_2 = (2 shl 6) or (0 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_2_0_2_3 = (2 shl 6) or (0 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_2_0_3_0 = (2 shl 6) or (0 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_2_0_3_1 = (2 shl 6) or (0 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_2_0_3_2 = (2 shl 6) or (0 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_2_0_3_3 = (2 shl 6) or (0 shl 4) or (3 shl 2) or 3; // $

    // 2,1
    _MM_SHUFFLE_2_1_0_0 = (2 shl 6) or (1 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_2_1_0_1 = (2 shl 6) or (1 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_2_1_0_2 = (2 shl 6) or (1 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_2_1_0_3 = (2 shl 6) or (1 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_2_1_1_0 = (2 shl 6) or (1 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_2_1_1_1 = (2 shl 6) or (1 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_2_1_1_2 = (2 shl 6) or (1 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_2_1_1_3 = (2 shl 6) or (1 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_2_1_2_0 = (2 shl 6) or (1 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_2_1_2_1 = (2 shl 6) or (1 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_2_1_2_2 = (2 shl 6) or (1 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_2_1_2_3 = (2 shl 6) or (1 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_2_1_3_0 = (2 shl 6) or (1 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_2_1_3_1 = (2 shl 6) or (1 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_2_1_3_2 = (2 shl 6) or (1 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_2_1_3_3 = (2 shl 6) or (1 shl 4) or (3 shl 2) or 3; // $

    // 2,2
    _MM_SHUFFLE_2_2_0_0 = (2 shl 6) or (2 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_2_2_0_1 = (2 shl 6) or (2 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_2_2_0_2 = (2 shl 6) or (2 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_2_2_0_3 = (2 shl 6) or (2 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_2_2_1_0 = (2 shl 6) or (2 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_2_2_1_1 = (2 shl 6) or (2 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_2_2_1_2 = (2 shl 6) or (2 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_2_2_1_3 = (2 shl 6) or (2 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_2_2_2_0 = (2 shl 6) or (2 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_2_2_2_1 = (2 shl 6) or (2 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_2_2_2_2 = (2 shl 6) or (2 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_2_2_2_3 = (2 shl 6) or (2 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_2_2_3_0 = (2 shl 6) or (2 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_2_2_3_1 = (2 shl 6) or (2 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_2_2_3_2 = (2 shl 6) or (2 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_2_2_3_3 = (2 shl 6) or (2 shl 4) or (3 shl 2) or 3; // $

    // 2,3
    _MM_SHUFFLE_2_3_0_0 = (2 shl 6) or (3 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_2_3_0_1 = (2 shl 6) or (3 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_2_3_0_2 = (2 shl 6) or (3 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_2_3_0_3 = (2 shl 6) or (3 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_2_3_1_0 = (2 shl 6) or (3 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_2_3_1_1 = (2 shl 6) or (3 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_2_3_1_2 = (2 shl 6) or (3 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_2_3_1_3 = (2 shl 6) or (3 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_2_3_2_0 = (2 shl 6) or (3 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_2_3_2_1 = (2 shl 6) or (3 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_2_3_2_2 = (2 shl 6) or (3 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_2_3_2_3 = (2 shl 6) or (3 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_2_3_3_0 = (2 shl 6) or (3 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_2_3_3_1 = (2 shl 6) or (3 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_2_3_3_2 = (2 shl 6) or (3 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_2_3_3_3 = (2 shl 6) or (3 shl 4) or (3 shl 2) or 3; // $


    // 3,0
    _MM_SHUFFLE_3_0_0_0 = (3 shl 6) or (0 shl 4) or (0 shl 2) or 0; // $00
    _MM_SHUFFLE_3_0_0_1 = (3 shl 6) or (0 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_3_0_0_2 = (3 shl 6) or (0 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_3_0_0_3 = (3 shl 6) or (0 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_3_0_1_0 = (3 shl 6) or (0 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_3_0_1_1 = (3 shl 6) or (0 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_3_0_1_2 = (3 shl 6) or (0 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_3_0_1_3 = (3 shl 6) or (0 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_3_0_2_0 = (3 shl 6) or (0 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_3_0_2_1 = (3 shl 6) or (0 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_3_0_2_2 = (3 shl 6) or (0 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_3_0_2_3 = (3 shl 6) or (0 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_3_0_3_0 = (3 shl 6) or (0 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_3_0_3_1 = (3 shl 6) or (0 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_3_0_3_2 = (3 shl 6) or (0 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_3_0_3_3 = (3 shl 6) or (0 shl 4) or (3 shl 2) or 3; // $

    // 3,1
    _MM_SHUFFLE_3_1_0_0 = (3 shl 6) or (1 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_3_1_0_1 = (3 shl 6) or (1 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_3_1_0_2 = (3 shl 6) or (1 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_3_1_0_3 = (3 shl 6) or (1 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_3_1_1_0 = (3 shl 6) or (1 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_3_1_1_1 = (3 shl 6) or (1 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_3_1_1_2 = (3 shl 6) or (1 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_3_1_1_3 = (3 shl 6) or (1 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_3_1_2_0 = (3 shl 6) or (1 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_3_1_2_1 = (3 shl 6) or (1 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_3_1_2_2 = (3 shl 6) or (1 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_3_1_2_3 = (3 shl 6) or (1 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_3_1_3_0 = (3 shl 6) or (1 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_3_1_3_1 = (3 shl 6) or (1 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_3_1_3_2 = (3 shl 6) or (1 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_3_1_3_3 = (3 shl 6) or (1 shl 4) or (3 shl 2) or 3; // $

    // 3,2
    _MM_SHUFFLE_3_2_0_0 = (3 shl 6) or (2 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_3_2_0_1 = (3 shl 6) or (2 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_3_2_0_2 = (3 shl 6) or (2 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_3_2_0_3 = (3 shl 6) or (2 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_3_2_1_0 = (3 shl 6) or (2 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_3_2_1_1 = (3 shl 6) or (2 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_3_2_1_2 = (3 shl 6) or (2 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_3_2_1_3 = (3 shl 6) or (2 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_3_2_2_0 = (3 shl 6) or (2 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_3_2_2_1 = (3 shl 6) or (2 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_3_2_2_2 = (3 shl 6) or (2 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_3_2_2_3 = (3 shl 6) or (2 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_3_2_3_0 = (3 shl 6) or (2 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_3_2_3_1 = (3 shl 6) or (2 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_3_2_3_2 = (3 shl 6) or (2 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_3_2_3_3 = (3 shl 6) or (2 shl 4) or (3 shl 2) or 3; // $

    // 3,3
    _MM_SHUFFLE_3_3_0_0 = (3 shl 6) or (3 shl 4) or (0 shl 2) or 0; // $
    _MM_SHUFFLE_3_3_0_1 = (3 shl 6) or (3 shl 4) or (0 shl 2) or 1; // $
    _MM_SHUFFLE_3_3_0_2 = (3 shl 6) or (3 shl 4) or (0 shl 2) or 2; // $
    _MM_SHUFFLE_3_3_0_3 = (3 shl 6) or (3 shl 4) or (0 shl 2) or 3; // $

    _MM_SHUFFLE_3_3_1_0 = (3 shl 6) or (3 shl 4) or (1 shl 2) or 0; // $
    _MM_SHUFFLE_3_3_1_1 = (3 shl 6) or (3 shl 4) or (1 shl 2) or 1; // $
    _MM_SHUFFLE_3_3_1_2 = (3 shl 6) or (3 shl 4) or (1 shl 2) or 2; // $
    _MM_SHUFFLE_3_3_1_3 = (3 shl 6) or (3 shl 4) or (1 shl 2) or 3; // $

    _MM_SHUFFLE_3_3_2_0 = (3 shl 6) or (3 shl 4) or (2 shl 2) or 0; // $
    _MM_SHUFFLE_3_3_2_1 = (3 shl 6) or (3 shl 4) or (2 shl 2) or 1; // $
    _MM_SHUFFLE_3_3_2_2 = (3 shl 6) or (3 shl 4) or (2 shl 2) or 2; // $
    _MM_SHUFFLE_3_3_2_3 = (3 shl 6) or (3 shl 4) or (2 shl 2) or 3; // $

    _MM_SHUFFLE_3_3_3_0 = (3 shl 6) or (3 shl 4) or (3 shl 2) or 0; // $
    _MM_SHUFFLE_3_3_3_1 = (3 shl 6) or (3 shl 4) or (3 shl 2) or 1; // $
    _MM_SHUFFLE_3_3_3_2 = (3 shl 6) or (3 shl 4) or (3 shl 2) or 2; // $
    _MM_SHUFFLE_3_3_3_3 = (3 shl 6) or (3 shl 4) or (3 shl 2) or 3; // $



function XMISNAN(x: UINT32): boolean;
begin
    Result := ((x and $7F800000) = $7F800000) and ((x and $7FFFFF) <> 0);
end;



function XMISINF(x: UINT32): boolean;
begin
    Result := ((x and $7FFFFFFF) = $7F800000);
end;



function XMMin(a, b: single): single;
begin
    if a < b then
        Result := a
    else
        Result := b;
end;



function XMMax(a, b: single): single;
begin
    if a > b then
        Result := a
    else
        Result := b;
end;

{***************************************************************************
 *
 * Macros
 *
 ***************************************************************************}


// Unit conversion

function XMConvertToRadians(fDegrees: single): single;
begin
    Result := fDegrees * (XM_PI / 180.0);
end;



function XMConvertToDegrees(fRadians: single): single;
begin
    Result := fRadians * (180.0 / XM_PI);
end;

// Condition register evaluation proceeding a recording (R) comparison

function XMComparisonAllTrue(CR: uint32): boolean;
begin
    Result := ((CR and XM_CRMASK_CR6TRUE) = XM_CRMASK_CR6TRUE);
end;



function XMComparisonAnyTrue(CR: uint32): boolean;
begin
    Result := ((CR and XM_CRMASK_CR6FALSE) <> XM_CRMASK_CR6FALSE);
end;



function XMComparisonAllFalse(CR: uint32): boolean;
begin
    Result := ((CR and XM_CRMASK_CR6FALSE) = XM_CRMASK_CR6FALSE);
end;



function XMComparisonAnyFalse(CR: uint32): boolean;
begin
    Result := ((CR and XM_CRMASK_CR6TRUE) <> XM_CRMASK_CR6TRUE);
end;



function XMComparisonMixed(CR: uint32): boolean;
begin
    Result := ((CR and XM_CRMASK_CR6) = 0);
end;



function XMComparisonAllInBounds(CR: uint32): boolean;
begin
    Result := ((CR and XM_CRMASK_CR6BOUNDS) = XM_CRMASK_CR6BOUNDS);
end;



function XMComparisonAnyOutOfBounds(CR: uint32): boolean;
begin
    Result := ((CR and XM_CRMASK_CR6BOUNDS) <> XM_CRMASK_CR6BOUNDS);
end;


{***************************************************************************
 *
 * XMVECTOR operators
 *
 ***************************************************************************}

{ TXMVECTOR }

constructor TXMVECTOR.Create(x, y, z, w: single);
begin
    f32[0] := x;
    f32[1] := y;
    f32[2] := z;
    f32[3] := w;
end;



class operator TXMVECTOR.Positive(a: TXMVECTOR): TXMVECTOR;
begin
    Result := a;
end;



class operator TXMVECTOR.Negative(a: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVectorNegate(a);
end;



class operator TXMVECTOR.Subtract(a, b: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVectorSubtract(a, b);
end;



class operator TXMVECTOR.Multiply(a, b: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVectorMultiply(a, b);
end;



class operator TXMVECTOR.Divide(a, b: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVectorDivide(a, b);
end;



class operator TXMVECTOR.Multiply(v: TXMVECTOR; s: single): TXMVECTOR;
begin
    Result := XMVectorScale(v, s);
end;



class operator TXMVECTOR.Divide(v: TXMVECTOR; s: single): TXMVECTOR;
var
    vs: TXMVECTOR;
begin
    vS := XMVectorReplicate(S);
    Result := XMVectorDivide(V, vS);
end;



class operator TXMVECTOR.Add(a, b: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVectorAdd(a, b);
end;


{ TXMVECTORU32 }

class operator TXMVECTORU32.Implicit(a: TXMVECTORU32): Puint32;
begin
    Result := @a.u[0];
end;



class operator TXMVECTORU32.Implicit(a: TXMVECTORU32): TXMVECTOR;
begin
    Result := a.v;
end;

{ TXMVECTORF32 }

class operator TXMVECTORF32.Implicit(a: TXMVECTORF32): PSingle;
begin
    Result := @a.f[0];
end;



class operator TXMVECTORF32.Implicit(a: TXMVECTORF32): TXMVECTOR;

begin
    Result := a.v;
end;


{ TXMVECTORI32 }

class operator TXMVECTORI32.Implicit(a: TXMVECTORI32): Pint32;
begin
    Result := @a.i[0];
end;



class operator TXMVECTORI32.Implicit(a: TXMVECTORI32): TXMVECTOR;
begin
    Result := a.v;
end;

{ TXMVECTORU8 }

class operator TXMVECTORU8.Implicit(a: TXMVECTORU8): PByte;
begin
    Result := @a.u[0];
end;



class operator TXMVECTORU8.Implicit(a: TXMVECTORU8): TXMVECTOR;
begin
    Result := a.v;
end;


(****************************************************************************
 *
 * XMMATRIX operators and methods
 *
 ****************************************************************************)

//------------------------------------------------------------------------------


{ TXMMATRIX }

constructor TXMMATRIX.Create(R0, R1, R2, R3: TXMVECTOR);
begin
    r[0] := R0;
    r[1] := R1;
    r[2] := R2;
    r[3] := R3;
end;



constructor TXMMATRIX.Create(m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33: single);
begin
    r[0] := XMVectorSet(m00, m01, m02, m03);
    r[1] := XMVectorSet(m10, m11, m12, m13);
    r[2] := XMVectorSet(m20, m21, m22, m23);
    r[3] := XMVectorSet(m30, m31, m32, m33);
end;



constructor TXMMATRIX.Create(pArray: PSingle);
begin
    assert(pArray <> nil);
    r[0] := XMLoadFloat4(pArray);
    r[1] := XMLoadFloat4(pArray + 4);
    r[2] := XMLoadFloat4(pArray + 8);
    r[3] := XMLoadFloat4(pArray + 12);
end;



class operator TXMMATRIX.Positive(a: TXMMATRIX): TXMMATRIX;
begin
    Result := a;
end;



class operator TXMMATRIX.Negative(a: TXMMATRIX): TXMMATRIX;
begin
    Result.r[0] := XMVectorNegate(a.r[0]);
    Result.r[1] := XMVectorNegate(a.r[1]);
    Result.r[2] := XMVectorNegate(a.r[2]);
    Result.r[3] := XMVectorNegate(a.r[3]);
end;



class operator TXMMATRIX.Add(a, b: TXMMATRIX): TXMMATRIX;
begin
    Result.r[0] := XMVectorAdd(a.r[0], b.r[0]);
    Result.r[1] := XMVectorAdd(a.r[1], b.r[1]);
    Result.r[2] := XMVectorAdd(a.r[2], b.r[2]);
    Result.r[3] := XMVectorAdd(a.r[3], b.r[3]);
end;



class operator TXMMATRIX.Subtract(a, b: TXMMATRIX): TXMMATRIX;
begin
    Result.r[0] := XMVectorSubtract(a.r[0], b.r[0]);
    Result.r[1] := XMVectorSubtract(a.r[1], b.r[1]);
    Result.r[2] := XMVectorSubtract(a.r[2], b.r[2]);
    Result.r[3] := XMVectorSubtract(a.r[3], b.r[3]);
end;



class operator TXMMATRIX.Multiply(a, b: TXMMATRIX): TXMMATRIX;
begin
    Result := XMMatrixMultiply(a, b);
end;



class operator TXMMATRIX.Multiply(M: TXMMATRIX; s: single): TXMMATRIX;
begin
    Result.r[0] := XMVectorScale(M.r[0], S);
    Result.r[1] := XMVectorScale(M.r[1], S);
    Result.r[2] := XMVectorScale(M.r[2], S);
    Result.r[3] := XMVectorScale(M.r[3], S);
end;



function TXMMATRIX.Get(Row, Column: size_t): single;
begin
    Result := m[Row, Column];
end;

{ TXMFLOAT4X4A - 4x4 Matrix: 32 bit floating point components aligned on a 16 byte boundary }

constructor TXMFLOAT4X4A.Create(m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33: single);
begin
    _11 := m00;
    _12 := m01;
    _13 := m02;
    _14 := m03;
    _21 := m10;
    _22 := m11;
    _23 := m12;
    _24 := m13;
    _31 := m20;
    _32 := m21;
    _33 := m22;
    _34 := m23;
    _41 := m30;
    _42 := m31;
    _43 := m32;
    _44 := m33;
end;



constructor TXMFLOAT4X4A.Create(constref pArray: PSingle);
begin

end;



function TXMFLOAT4X4A.Get(Row, Column: size_t): single;
begin
    Result := m[Row, Column];
end;

{ TXMFLOAT4X4 - 4x4 Matrix: 32 bit floating point components }

constructor TXMFLOAT4X4.Create(m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33: single);
begin
    _11 := m00;
    _12 := m01;
    _13 := m02;
    _14 := m03;
    _21 := m10;
    _22 := m11;
    _23 := m12;
    _24 := m13;
    _31 := m20;
    _32 := m21;
    _33 := m22;
    _34 := m23;
    _41 := m30;
    _42 := m31;
    _43 := m32;
    _44 := m33;
end;



constructor TXMFLOAT4X4.Create(constref pArray: PSingle);
begin
    assert(pArray <> nil);

    m[0, 0] := pArray[0];
    m[0, 1] := pArray[1];
    m[0, 2] := pArray[2];
    m[0, 3] := pArray[3];

    m[1, 0] := pArray[4];
    m[1, 1] := pArray[5];
    m[1, 2] := pArray[6];
    m[1, 3] := pArray[7];

    m[2, 0] := pArray[8];
    m[2, 1] := pArray[9];
    m[2, 2] := pArray[10];
    m[2, 3] := pArray[11];

    m[3, 0] := pArray[12];
    m[3, 1] := pArray[13];
    m[3, 2] := pArray[14];
    m[3, 3] := pArray[15];
end;



function TXMFLOAT4X4.Get(Row, Column: size_t): single;
begin
    Result := m[Row, Column];
end;

{ TXMFLOAT4X3A - 4x3 Matrix: 32 bit floating point components aligned on a 16 byte boundary }

constructor TXMFLOAT4X3A.Create(m00, m01, m02, m10, m11, m12, m20, m21, m22, m30, m31, m32: single);
begin
    _11 := m00;
    _12 := m01;
    _13 := m02;
    _21 := m10;
    _22 := m11;
    _23 := m12;
    _31 := m20;
    _32 := m21;
    _33 := m22;
    _41 := m30;
    _42 := m31;
    _43 := m32;
end;



constructor TXMFLOAT4X3A.Create(constref pArray: PSingle);
begin

end;



function TXMFLOAT4X3A.Get(Row, Column: size_t): single;
begin
    Result := m[Row, Column];
end;

{****************************************************************************
 *
 * XMFLOAT4X3 operators
 *
 ****************************************************************************}

//------------------------------------------------------------------------------
{ TXMFLOAT4X3 - 4x3 Matrix: 32 bit floating point components }

constructor TXMFLOAT4X3.Create(m00, m01, m02, m10, m11, m12, m20, m21, m22, m30, m31, m32: single);
begin
    _11 := m00;
    _12 := m01;
    _13 := m02;
    _21 := m10;
    _22 := m11;
    _23 := m12;
    _31 := m20;
    _32 := m21;
    _33 := m22;
    _41 := m30;
    _42 := m31;
    _43 := m32;
end;



constructor TXMFLOAT4X3.Create(constref pArray: PSingle);
begin
    assert(pArray <> nil);

    m[0, 0] := pArray[0];
    m[0, 1] := pArray[1];
    m[0, 2] := pArray[2];

    m[1, 0] := pArray[3];
    m[1, 1] := pArray[4];
    m[1, 2] := pArray[5];

    m[2, 0] := pArray[6];
    m[2, 1] := pArray[7];
    m[2, 2] := pArray[8];

    m[3, 0] := pArray[9];
    m[3, 1] := pArray[10];
    m[3, 2] := pArray[11];
end;



function TXMFLOAT4X3.Get(Row, Column: size_t): single;
begin
    Result := m[Row, Column];
end;

{ TXMUINT4 - 4D Vector; 32 bit unsigned integer components }

constructor TXMUINT4.Create(_X, _Y, _Z, _W: uint32);
begin
    x := _x;
    y := _y;
    z := _z;
    w := _w;
end;



constructor TXMUINT4.Create(pArray: Puint32);
begin
    x := pArray[0];
    y := pArray[1];
    z := pArray[2];
    w := pArray[3];
end;

//------------------------------------------------------------------------------
{ TXMINT4 - 4D Vector; 32 bit signed integer components}

constructor TXMINT4.Create(_X, _Y, _Z, _W: int32);
begin
    x := _x;
    y := _y;
    z := _z;
    w := _w;
end;



constructor TXMINT4.Create(pArray: Pint32);
begin
    x := pArray[0];
    y := pArray[1];
    z := pArray[2];
    w := pArray[3];
end;

{ TXMFLOAT4A - 4D Vector; 32 bit floating point components aligned on a 16 byte boundary }

constructor TXMFLOAT4A.Create(pArray: PSingle);
begin
    x := pArray[0];
    y := pArray[1];
    z := pArray[2];
    w := pArray[3];
end;



constructor TXMFLOAT4A.Create(_X, _Y, _Z, _W: single);
begin
    x := _x;
    y := _y;
    z := _z;
    w := _w;
end;



class operator TXMFLOAT4A.Implicit(Float4: TXMFLOAT4): TXMFLOAT4A;
begin
    Result.x := Float4.x;
    Result.y := Float4.y;
    Result.z := Float4.z;
    Result.w := Float4.w;
end;

{ TXMUINT3 - 3D Vector; 32 bit unsigned integer components }

constructor TXMUINT3.Create(_x, _y, _z: Uint32);
begin
    x := _x;
    y := _y;
    z := _z;
end;



constructor TXMUINT3.Create(pArray: PUint32);
begin
    x := pArray[0];
    y := pArray[1];
    z := pArray[2];
end;

{ TXMINT3 }

constructor TXMINT3.Create(_x, _y, _z: int32);
begin
    x := _x;
    y := _y;
    z := _z;
end;



constructor TXMINT3.Create(pArray: Pint32);
begin
    x := pArray[0];
    y := pArray[1];
    z := pArray[2];
end;

{ TXMFLOAT3A - 3D Vector; 32 bit floating point components aligned on a 16 byte boundary }

constructor TXMFLOAT3A.Create(_x, _y, _z: single);
begin
    x := _x;
    y := _y;
    z := _z;
end;



constructor TXMFLOAT3A.Create(pArray: Psingle);
begin
    x := pArray[0];
    y := pArray[1];
    z := pArray[2];
end;



class operator TXMFLOAT3A.Implicit(Float3: TXMFLOAT3): TXMFLOAT3A;
begin
    Result.x := Float3.x;
    Result.y := Float3.y;
    Result.z := Float3.z;
end;

{ TXMUINT2 - 2D Vector; 32 bit unsigned integer components }

constructor TXMUINT2.Create(_x, _y: uint32);
begin
    x := _x;
    y := _y;
end;



constructor TXMUINT2.Create(pArray: PUINT32);
begin
    x := pArray[0];
    y := pArray[1];
end;

{ TXMINT2 - 2D Vector; 32 bit signed integer components}

constructor TXMINT2.Create(_x, _y: int32);
begin
    x := _x;
    y := _y;
end;



constructor TXMINT2.Create(pArray: PINT32);
begin
    x := pArray[0];
    y := pArray[1];
end;

{ TXMFLOAT2A }
// 2D Vector; 32 bit floating point components aligned on a 16 byte boundary
constructor TXMFLOAT2A.Create(_x, _y: single);
begin
    x := _x;
    y := _y;
end;



constructor TXMFLOAT2A.Create(pArray: PSingle);
begin
    x := pArray[0];
    y := pArray[1];
end;



class operator TXMFLOAT2A.Implicit(Float2: TXMFLOAT2): TXMFLOAT2A;
begin
    Result.x := Float2.x;
    Result.y := Float2.y;
end;


//------------------------------------------------------------------------------
// 2D Vector; 32 bit floating point components
{ TXMFLOAT2 }

constructor TXMFLOAT2.Create(_x, _y: single);
begin
    x := _x;
    y := _y;
end;



constructor TXMFLOAT2.Create(pArray: PSingle);
begin
    x := pArray[0];
    y := pArray[1];
end;




(****************************************************************************
 *
 * XMFLOAT3X3 operators
 *
 ****************************************************************************)
{ TXMFLOAT3X3 - 3x3 Matrix: 32 bit floating point components }

constructor TXMFLOAT3X3.Create(constref pArray: PSingle);
var
    Row, Column: size_t;
begin
    assert(pArray <> nil);
    for Row := 0 to 2 do
    begin
        for Column := 0 to 2 do
        begin
            m[Row, Column] := pArray[Row * 3 + Column];
        end;
    end;
end;



constructor TXMFLOAT3X3.Create(m00, m01, m02, m10, m11, m12, m20, m21, m22: single);
begin
    _11 := m00;
    _12 := m01;
    _13 := m02;
    _21 := m10;
    _22 := m11;
    _23 := m12;
    _31 := m20;
    _32 := m21;
    _33 := m22;
end;



function TXMFLOAT3X3.Get(Row, Column: size_t): single;
begin
    Result := m[Row, Column];
end;

{ TXMFLOAT4 - 4D Vector; 32 bit floating point components }

constructor TXMFLOAT4.Create(pArray: PSingle);
begin
    x := pArray[0];
    y := pArray[1];
    z := pArray[2];
    w := pArray[3];
end;



constructor TXMFLOAT4.Create(_X, _Y, _Z, _W: single);
begin
    x := _x;
    y := _y;
    z := _z;
    w := _w;
end;



{ TXMFLOAT3 - 3D Vector; 32 bit floating point components }

constructor TXMFLOAT3.Create(_x, _y, _z: single);
begin
    x := _x;
    y := _y;
    z := _z;
end;



constructor TXMFLOAT3.Create(pArray: Psingle);
begin
    x := pArray[0];
    y := pArray[1];
    z := pArray[2];
end;


// Return a floating point value via an index. This is not a recommended
// function to use due to performance loss.
function XMVectorGetByIndex(V: TXMVECTOR; i: size_t): single;
begin
    assert(i < 4);
    Result := V.f32[i];
end;

// Store a component indexed by i into a 32 bit  single  location in memory.

procedure XMVectorGetByIndexPtr(out f: single; V: TXMVECTOR; i: size_t);
begin
    assert(i < 4);
    f := V.f32[i];
end;

// Return an integer value via an index. This is not a recommended
// function to use due to performance loss.
function XMVectorGetIntByIndex(constref V: TXMVECTOR; constref i: size_t): UINT32;
begin
    assert(i < 4);
    Result := V.u32[i];
end;

// Store a component indexed by i into a 32 bit integer location in memory.

procedure XMVectorGetIntByIndexPtr(out x: UINT32; constref V: TXMVECTOR; constref i: size_t);
begin
    assert(i < 4);
    x := V.u32[i];
end;

// Set a single indexed floating point component

function XMVectorSetByIndex(constref V: TXMVECTOR; constref f: single; constref i: size_t): TXMVECTOR;
begin
    assert(i < 4);
    Result := V;
    Result.f32[i] := f;
end;

// Sets a component of a vector to a floating point value passed by pointer

function XMVectorSetByIndexPtr(constref V: TXMVECTOR; constref f: Psingle; constref i: size_t): TXMVECTOR;
begin
    assert(i < 4);
    Result := V;
    Result.f32[i] := f^;
end;


// Sets a component of a vector to an integer passed by value
function XMVectorSetIntByIndex(constref V: TXMVECTOR; constref x: UINT32; constref i: size_t): TXMVECTOR;
begin
    assert(i < 4);
    Result := V;
    Result.u32[i] := x;
end;

// Sets a component of a vector to an integer value passed by pointer

function XMVectorSetIntByIndexPtr(constref V: TXMVECTOR; constref x: PUINT32; constref i: size_t): TXMVECTOR;
begin
    assert(i < 4);
    Result := V;
    Result.u32[i] := x^;
end;



function XMVectorShiftLeft(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Elements: UINT32): TXMVECTOR;
begin
    assert(Elements < 4);
    Result := XMVectorPermute(V1, V2, Elements, ((Elements) + 1), ((Elements) + 2), ((Elements) + 3));
end;



function XMVectorRotateLeft(constref V: TXMVECTOR; constref Elements: UINT32): TXMVECTOR;
begin
    assert(Elements < 4);
    Result := XMVectorSwizzle(V, Elements and 3, (Elements + 1) and 3, (Elements + 2) and 3, (Elements + 3) and 3);
end;



function XMVectorRotateRight(constref V: TXMVECTOR; constref Elements: UINT32): TXMVECTOR;
begin
    assert(Elements < 4);
    Result := XMVectorSwizzle(V, (4 - (Elements)) and 3, (5 - (Elements)) and 3, (6 - (Elements)) and 3, (7 - (Elements)) and 3);
end;



function XMVectorInsert(constref VD: TXMVECTOR; constref VS: TXMVECTOR; constref VSLeftRotateElements: UINT32;
    constref Select0: UINT32; constref Select1: UINT32; constref Select2: UINT32; constref Select3: UINT32): TXMVECTOR;
var
    Control: TXMVECTOR;
begin
    Control := XMVectorSelectControl(Select0 and 1, Select1 and 1, Select2 and 1, Select3 and 1);
    Result := XMVectorSelect(VD, XMVectorRotateLeft(VS, VSLeftRotateElements), Control);
end;



function XMVectorExp(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVectorExp2(V);
end;



function XMVectorLog(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVectorLog2(V);
end;



function XMVector2LengthSq(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector2Dot(V, V);
end;



function XMVector2ClampLength(constref V: TXMVECTOR; constref LengthMin: single; constref LengthMax: single): TXMVECTOR;
var
    ClampMax, ClampMin: TXMVECTOR;
begin
    ClampMax := XMVectorReplicate(LengthMax);
    ClampMin := XMVectorReplicate(LengthMin);
    Result := XMVector2ClampLengthV(V, ClampMin, ClampMax);
end;



function XMVector2ClampLengthV(constref V: TXMVECTOR; constref LengthMin: TXMVECTOR; constref LengthMax: TXMVECTOR): TXMVECTOR;
var
    LengthSq, Zero, RcpLength, InfiniteLength, ZeroLength, Length, Normal, Select: TXMVECTOR;
    ControlMax, ControlMin, ClampLength, Control: TXMVECTOR;
begin
    assert((XMVectorGetY(LengthMin) = XMVectorGetX(LengthMin)));
    assert((XMVectorGetY(LengthMax) = XMVectorGetX(LengthMax)));
    assert(XMVector2GreaterOrEqual(LengthMin, g_XMZero));
    assert(XMVector2GreaterOrEqual(LengthMax, g_XMZero));
    assert(XMVector2GreaterOrEqual(LengthMax, LengthMin));

    LengthSq := XMVector2LengthSq(V);

    Zero := XMVectorZero();

    RcpLength := XMVectorReciprocalSqrt(LengthSq);

    InfiniteLength := XMVectorEqualInt(LengthSq, g_XMInfinity.v);
    ZeroLength := XMVectorEqual(LengthSq, Zero);

    Length := XMVectorMultiply(LengthSq, RcpLength);

    Normal := XMVectorMultiply(V, RcpLength);

    Select := XMVectorEqualInt(InfiniteLength, ZeroLength);
    Length := XMVectorSelect(LengthSq, Length, Select);
    Normal := XMVectorSelect(LengthSq, Normal, Select);


    ControlMax := XMVectorGreater(Length, LengthMax);
    ControlMin := XMVectorLess(Length, LengthMin);

    ClampLength := XMVectorSelect(Length, LengthMax, ControlMax);
    ClampLength := XMVectorSelect(ClampLength, LengthMin, ControlMin);

    Result := XMVectorMultiply(Normal, ClampLength);

    // Preserve the original vector (with no precision loss) if the length falls within the given range
    Control := XMVectorEqualInt(ControlMax, ControlMin);
    Result := XMVectorSelect(Result, V, Control);
end;



function XMVector2Reflect(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR): TXMVECTOR;
begin
    // Result  :=  Incident - (2 * dot(Incident, Normal)) * Normal
    Result := XMVector2Dot(Incident, Normal);
    Result := XMVectorAdd(Result, Result);
    Result := XMVectorNegativeMultiplySubtract(Result, Normal, Incident);
end;



function XMVector2Refract(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR; constref RefractionIndex: single): TXMVECTOR;
var
    Index: TXMVECTOR;
begin
    Index := XMVectorReplicate(RefractionIndex);
    Result := XMVector2RefractV(Incident, Normal, Index);
end;



function XMVector2AngleBetweenNormalsEst(constref N1: TXMVECTOR; constref N2: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector2Dot(N1, N2);
    Result := XMVectorClamp(Result, g_XMNegativeOne.v, g_XMOne.v);
    Result := XMVectorACosEst(Result);
end;



function XMVector2AngleBetweenNormals(constref N1: TXMVECTOR; constref N2: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector2Dot(N1, N2);
    Result := XMVectorClamp(Result, g_XMNegativeOne, g_XMOne);
    Result := XMVectorACos(Result);
end;



function XMVector2AngleBetweenVectors(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
var
    L1, L2, Dot, CosAngle: TXMVECTOR;
begin
    L1 := XMVector2ReciprocalLength(V1);
    L2 := XMVector2ReciprocalLength(V2);

    Dot := XMVector2Dot(V1, V2);

    L1 := XMVectorMultiply(L1, L2);

    CosAngle := XMVectorMultiply(Dot, L1);
    CosAngle := XMVectorClamp(CosAngle, g_XMNegativeOne.v, g_XMOne.v);

    Result := XMVectorACos(CosAngle);
end;



function XMVector2LinePointDistance(constref LinePoint1: TXMVECTOR; constref LinePoint2: TXMVECTOR; constref Point: TXMVECTOR): TXMVECTOR;
var
    PointVector, LineVector, LengthSq, PointProjectionScale, DistanceVector: TXMVECTOR;
begin
    // Given a vector PointVector from LinePoint1 to Point and a vector
    // LineVector from LinePoint1 to LinePoint2, the scaled distance
    // PointProjectionScale from LinePoint1 to the perpendicular projection
    // of PointVector onto the line is defined as:

    //     PointProjectionScale  :=  dot(PointVector, LineVector) / LengthSq(LineVector)

    PointVector := XMVectorSubtract(Point, LinePoint1);
    LineVector := XMVectorSubtract(LinePoint2, LinePoint1);

    LengthSq := XMVector2LengthSq(LineVector);

    PointProjectionScale := XMVector2Dot(PointVector, LineVector);
    PointProjectionScale := XMVectorDivide(PointProjectionScale, LengthSq);

    DistanceVector := XMVectorMultiply(LineVector, PointProjectionScale);
    DistanceVector := XMVectorSubtract(PointVector, DistanceVector);

    Result := XMVector2Length(DistanceVector);
end;



function XMVector2TransformCoord(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
var
    X, Y, W: TXMVECTOR;
begin
    Y := XMVectorSplatY(V);
    X := XMVectorSplatX(V);

    Result := XMVectorMultiplyAdd(Y, M.r[1], M.r[3]);
    Result := XMVectorMultiplyAdd(X, M.r[0], Result);

    W := XMVectorSplatW(Result);
    Result := XMVectorDivide(Result, W);
end;



function XMVector3LengthSq(const V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector3Dot(V, V);
end;



function XMVector3ClampLength(constref V: TXMVECTOR; constref LengthMin: single; constref LengthMax: single): TXMVECTOR;
var
    ClampMax, ClampMin: TXMVECTOR;
begin
    ClampMax := XMVectorReplicate(LengthMax);
    ClampMin := XMVectorReplicate(LengthMin);

    Result := XMVector3ClampLengthV(V, ClampMin, ClampMax);
end;



function XMVector3ClampLengthV(constref V: TXMVECTOR; constref LengthMin: TXMVECTOR; constref LengthMax: TXMVECTOR): TXMVECTOR;
var
    LengthSq, Zero, RcpLength, InfiniteLength, ZeroLength, Normal, Length, Select: TXMVECTOR;
    ControlMax, ControlMin, ClampLength, Control: TXMVECTOR;
begin
    assert((XMVectorGetY(LengthMin) = XMVectorGetX(LengthMin)) and (XMVectorGetZ(LengthMin) = XMVectorGetX(LengthMin)));
    assert((XMVectorGetY(LengthMax) = XMVectorGetX(LengthMax)) and (XMVectorGetZ(LengthMax) = XMVectorGetX(LengthMax)));
    assert(XMVector3GreaterOrEqual(LengthMin, XMVectorZero()));
    assert(XMVector3GreaterOrEqual(LengthMax, XMVectorZero()));
    assert(XMVector3GreaterOrEqual(LengthMax, LengthMin));

    LengthSq := XMVector3LengthSq(V);

    Zero := XMVectorZero();

    RcpLength := XMVectorReciprocalSqrt(LengthSq);

    InfiniteLength := XMVectorEqualInt(LengthSq, g_XMInfinity.v);
    ZeroLength := XMVectorEqual(LengthSq, Zero);

    Normal := XMVectorMultiply(V, RcpLength);

    Length := XMVectorMultiply(LengthSq, RcpLength);

    Select := XMVectorEqualInt(InfiniteLength, ZeroLength);
    Length := XMVectorSelect(LengthSq, Length, Select);
    Normal := XMVectorSelect(LengthSq, Normal, Select);


    ControlMax := XMVectorGreater(Length, LengthMax);
    ControlMin := XMVectorLess(Length, LengthMin);

    ClampLength := XMVectorSelect(Length, LengthMax, ControlMax);
    ClampLength := XMVectorSelect(ClampLength, LengthMin, ControlMin);

    Result := XMVectorMultiply(Normal, ClampLength);

    // Preserve the original vector (with no precision loss) if the length falls within the given range
    Control := XMVectorEqualInt(ControlMax, ControlMin);
    Result := XMVectorSelect(Result, V, Control);
end;



function XMVector3Reflect(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR): TXMVECTOR;
begin
    // Result  :=  Incident - (2 * dot(Incident, Normal)) * Normal
    Result := XMVector3Dot(Incident, Normal);
    Result := XMVectorAdd(Result, Result);
    Result := XMVectorNegativeMultiplySubtract(Result, Normal, Incident);
end;



function XMVector3Refract(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR; constref RefractionIndex: single): TXMVECTOR;
var
    Index: TXMVECTOR;
begin
    Index := XMVectorReplicate(RefractionIndex);
    Result := XMVector3RefractV(Incident, Normal, Index);
end;




function XMVector3Orthogonal(constref V: TXMVECTOR): TXMVECTOR;
var
    Zero, Z, YZYY, NegativeV, ZIsNegative, YZYYIsNegative, S, D, Select, R0, R1: TXMVECTOR;
begin
    Zero := XMVectorZero();
    Z := XMVectorSplatZ(V);
    YZYY := XMVectorSwizzle(V, XM_SWIZZLE_Y, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y);

    NegativeV := XMVectorSubtract(Zero, V);

    ZIsNegative := XMVectorLess(Z, Zero);
    YZYYIsNegative := XMVectorLess(YZYY, Zero);

    S := XMVectorAdd(YZYY, Z);
    D := XMVectorSubtract(YZYY, Z);

    Select := XMVectorEqualInt(ZIsNegative, YZYYIsNegative);

    R0 := XMVectorPermute(NegativeV, S, XM_PERMUTE_1X, XM_PERMUTE_0X, XM_PERMUTE_0X, XM_PERMUTE_0X);
    R1 := XMVectorPermute(V, D, XM_PERMUTE_1X, XM_PERMUTE_0X, XM_PERMUTE_0X, XM_PERMUTE_0X);

    Result := XMVectorSelect(R1, R0, Select);
end;



function XMVector3AngleBetweenNormalsEst(constref N1: TXMVECTOR; constref N2: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector3Dot(N1, N2);
    Result := XMVectorClamp(Result, g_XMNegativeOne.v, g_XMOne.v);
    Result := XMVectorACosEst(Result);
end;



function XMVector3AngleBetweenNormals(constref N1: TXMVECTOR; constref N2: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector3Dot(N1, N2);
    Result := XMVectorClamp(Result, g_XMNegativeOne.v, g_XMOne.v);
    Result := XMVectorACos(Result);
end;



function XMVector3AngleBetweenVectors(const V1: TXMVECTOR; const V2: TXMVECTOR): TXMVECTOR;
var
    L1, L2, Dot, CosAngle: TXMVECTOR;
begin
    L1 := XMVector3ReciprocalLength(V1);
    L2 := XMVector3ReciprocalLength(V2);

    Dot := XMVector3Dot(V1, V2);

    L1 := XMVectorMultiply(L1, L2);

    CosAngle := XMVectorMultiply(Dot, L1);
    CosAngle := XMVectorClamp(CosAngle, g_XMNegativeOne.v, g_XMOne.v);

    Result := XMVectorACos(CosAngle);
end;



function XMVector3LinePointDistance(constref LinePoint1: TXMVECTOR; constref LinePoint2: TXMVECTOR; constref Point: TXMVECTOR): TXMVECTOR;
var
    PointVector, LineVector, LengthSq, PointProjectionScale: TXMVECTOR;
    DistanceVector: TXMVECTOR;
begin
    // Given a vector PointVector from LinePoint1 to Point and a vector
    // LineVector from LinePoint1 to LinePoint2, the scaled distance
    // PointProjectionScale from LinePoint1 to the perpendicular projection
    // of PointVector onto the line is defined as:

    //     PointProjectionScale  :=  dot(PointVector, LineVector) / LengthSq(LineVector)

    PointVector := XMVectorSubtract(Point, LinePoint1);
    LineVector := XMVectorSubtract(LinePoint2, LinePoint1);

    LengthSq := XMVector3LengthSq(LineVector);

    PointProjectionScale := XMVector3Dot(PointVector, LineVector);
    PointProjectionScale := XMVectorDivide(PointProjectionScale, LengthSq);

    DistanceVector := XMVectorMultiply(LineVector, PointProjectionScale);
    DistanceVector := XMVectorSubtract(PointVector, DistanceVector);

    Result := XMVector3Length(DistanceVector);
end;



procedure XMVector3ComponentsFromNormal(out pParallel: TXMVECTOR; out pPerpendicular: TXMVECTOR; constref A: TXMVECTOR; constref Normal: TXMVECTOR);
var
    Scale: TXMVECTOR;
begin
    Scale := XMVector3Dot(A, Normal);

    pParallel := XMVectorMultiply(Normal, Scale);
    pPerpendicular := XMVectorSubtract(A, pParallel);
end;



//------------------------------------------------------------------------------
// Transform a vector using a rotation expressed as a unit quaternion
function XMVector3Rotate(constref V: TXMVECTOR; constref RotationQuaternion: TXMVECTOR): TXMVECTOR;
var
    A, Q: TXMVECTOR;
begin
    A := XMVectorSelect(g_XMSelect1110.v, V, g_XMSelect1110.v);
    Q := XMQuaternionConjugate(RotationQuaternion);
    Result := XMQuaternionMultiply(Q, A);
    Result := XMQuaternionMultiply(Result, RotationQuaternion);
end;


//------------------------------------------------------------------------------
// Transform a vector using the inverse of a rotation expressed as a unit quaternion
function XMVector3InverseRotate(constref V: TXMVECTOR; constref RotationQuaternion: TXMVECTOR): TXMVECTOR;
var
    A, Q: TXMVECTOR;
begin
    A := XMVectorSelect(g_XMSelect1110.v, V, g_XMSelect1110.v);
    Result := XMQuaternionMultiply(RotationQuaternion, A);
    Q := XMQuaternionConjugate(RotationQuaternion);
    Result := XMQuaternionMultiply(Result, Q);
end;



function XMVector3TransformCoord(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
var
    X, Y, Z, W: TXMVECTOR;
begin
    Z := XMVectorSplatZ(V);
    Y := XMVectorSplatY(V);
    X := XMVectorSplatX(V);

    Result := XMVectorMultiplyAdd(Z, M.r[2], M.r[3]);
    Result := XMVectorMultiplyAdd(Y, M.r[1], Result);
    Result := XMVectorMultiplyAdd(X, M.r[0], Result);

    W := XMVectorSplatW(Result);
    Result := XMVectorDivide(Result, W);
end;



function XMVector3Project(V: TXMVECTOR; ViewportX: single; ViewportY: single; ViewportWidth: single; ViewportHeight: single;
    ViewportMinZ: single; ViewportMaxZ: single; Projection: TXMMATRIX; View: TXMMATRIX; World: TXMMATRIX): TXMVECTOR;
var
    HalfViewportWidth, HalfViewportHeight: single;
    Scale, Offset: TXMVECTOR;
    Transform: TXMMATRIX;
begin
    HalfViewportWidth := ViewportWidth * 0.5;
    HalfViewportHeight := ViewportHeight * 0.5;

    Scale := XMVectorSet(HalfViewportWidth, -HalfViewportHeight, ViewportMaxZ - ViewportMinZ, 0.0);
    Offset := XMVectorSet(ViewportX + HalfViewportWidth, ViewportY + HalfViewportHeight, ViewportMinZ, 0.0);

    Transform := XMMatrixMultiply(World, View);
    Transform := XMMatrixMultiply(Transform, Projection);

    Result := XMVector3TransformCoord(V, Transform);

    Result := XMVectorMultiplyAdd(Result, Scale, Offset);
end;



function XMVector3Unproject(V: TXMVECTOR; ViewportX: single; ViewportY: single; ViewportWidth: single; ViewportHeight: single;
    ViewportMinZ: single; ViewportMaxZ: single; Projection: TXMMATRIX; View: TXMMATRIX; World: TXMMATRIX): TXMVECTOR;
const
    D: TXMVECTORF32 = (f: (-1.0, 1.0, 0.0, 0.0));
var
    Scale, Offset, Det: TXMVECTOR;
    Transform: TXMMATRIX;
begin
    Scale := XMVectorSet(ViewportWidth * 0.5, -ViewportHeight * 0.5, ViewportMaxZ - ViewportMinZ, 1.0);
    Scale := XMVectorReciprocal(Scale);

    Offset := XMVectorSet(-ViewportX, -ViewportY, -ViewportMinZ, 0.0);
    Offset := XMVectorMultiplyAdd(Scale, Offset, D.v);

    Transform := XMMatrixMultiply(World, View);
    Transform := XMMatrixMultiply(Transform, Projection);
    Transform := XMMatrixInverse(Det, Transform);

    Result := XMVectorMultiplyAdd(V, Scale, Offset);

    Result := XMVector3TransformCoord(Result, Transform);
end;



function XMVector4LengthSq(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4Dot(V, V);
end;



function XMVector4ClampLength(V: TXMVECTOR; LengthMin: single; LengthMax: single): TXMVECTOR;
var
    ClampMax, ClampMin: TXMVECTOR;
begin
    ClampMax := XMVectorReplicate(LengthMax);
    ClampMin := XMVectorReplicate(LengthMin);

    Result := XMVector4ClampLengthV(V, ClampMin, ClampMax);
end;



function XMVector4ClampLengthV(V: TXMVECTOR; LengthMin: TXMVECTOR; LengthMax: TXMVECTOR): TXMVECTOR;
var
    LengthSq, Zero: TXMVECTOR;
    RcpLength, InfiniteLength, ZeroLength: TXMVECTOR;
    Normal, Length, Select: TXMVECTOR;
    ControlMax, ControlMin, ClampLength, Control: TXMVECTOR;
begin
    assert((XMVectorGetY(LengthMin) = XMVectorGetX(LengthMin)) and (XMVectorGetZ(LengthMin) = XMVectorGetX(LengthMin)) and
        (XMVectorGetW(LengthMin) = XMVectorGetX(LengthMin)));
    assert((XMVectorGetY(LengthMax) = XMVectorGetX(LengthMax)) and (XMVectorGetZ(LengthMax) = XMVectorGetX(LengthMax)) and
        (XMVectorGetW(LengthMax) = XMVectorGetX(LengthMax)));
    assert(XMVector4GreaterOrEqual(LengthMin, XMVectorZero()));
    assert(XMVector4GreaterOrEqual(LengthMax, XMVectorZero()));
    assert(XMVector4GreaterOrEqual(LengthMax, LengthMin));

    LengthSq := XMVector4LengthSq(V);

    Zero := XMVectorZero();

    RcpLength := XMVectorReciprocalSqrt(LengthSq);

    InfiniteLength := XMVectorEqualInt(LengthSq, g_XMInfinity.v);
    ZeroLength := XMVectorEqual(LengthSq, Zero);

    Normal := XMVectorMultiply(V, RcpLength);

    Length := XMVectorMultiply(LengthSq, RcpLength);

    Select := XMVectorEqualInt(InfiniteLength, ZeroLength);
    Length := XMVectorSelect(LengthSq, Length, Select);
    Normal := XMVectorSelect(LengthSq, Normal, Select);


    ControlMax := XMVectorGreater(Length, LengthMax);
    ControlMin := XMVectorLess(Length, LengthMin);

    ClampLength := XMVectorSelect(Length, LengthMax, ControlMax);
    ClampLength := XMVectorSelect(ClampLength, LengthMin, ControlMin);

    Result := XMVectorMultiply(Normal, ClampLength);

    // Preserve the original vector (with no precision loss) if the length falls within the given range
    Control := XMVectorEqualInt(ControlMax, ControlMin);
    Result := XMVectorSelect(Result, V, Control);
end;



function XMVector4Reflect(Incident: TXMVECTOR; Normal: TXMVECTOR): TXMVECTOR;
begin
    // Result  :=  Incident - (2 * dot(Incident, Normal)) * Normal
    Result := XMVector4Dot(Incident, Normal);
    Result := XMVectorAdd(Result, Result);
    Result := XMVectorNegativeMultiplySubtract(Result, Normal, Incident);
end;



function XMVector4Refract(Incident: TXMVECTOR; Normal: TXMVECTOR; RefractionIndex: single): TXMVECTOR;
var
    Index: TXMVECTOR;
begin
    Index := XMVectorReplicate(RefractionIndex);
    Result := XMVector4RefractV(Incident, Normal, Index);
end;




function XMVector4AngleBetweenNormalsEst(N1: TXMVECTOR; N2: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4Dot(N1, N2);
    Result := XMVectorClamp(Result, g_XMNegativeOne.v, g_XMOne.v);
    Result := XMVectorACosEst(Result);
end;



function XMVector4AngleBetweenNormals(N1: TXMVECTOR; N2: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4Dot(N1, N2);
    Result := XMVectorClamp(Result, g_XMNegativeOne.v, g_XMOne.v);
    Result := XMVectorACos(Result);
end;



function XMVector4AngleBetweenVectors(V1: TXMVECTOR; V2: TXMVECTOR): TXMVECTOR;
var
    L1, L2, Dot, CosAngle: TXMVECTOR;
begin
    L1 := XMVector4ReciprocalLength(V1);
    L2 := XMVector4ReciprocalLength(V2);

    Dot := XMVector4Dot(V1, V2);

    L1 := XMVectorMultiply(L1, L2);

    CosAngle := XMVectorMultiply(Dot, L1);
    CosAngle := XMVectorClamp(CosAngle, g_XMNegativeOne.v, g_XMOne.v);

    Result := XMVectorACos(CosAngle);
end;



function XMMatrixDeterminant(M: TXMMATRIX): TXMVECTOR;
const
    Sign: TXMVECTORF32 = (f: (1.0, -1.0, 1.0, -1.0));
var
    V0, V1, V2, V3, V4, V5, P0, P1, P2, S, R: TXMVECTOR;
begin

    V0 := XMVectorSwizzle(M.r[2], XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X);
    V1 := XMVectorSwizzle(M.r[3], XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y);
    V2 := XMVectorSwizzle(M.r[2], XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X);
    V3 := XMVectorSwizzle(M.r[3], XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z);
    V4 := XMVectorSwizzle(M.r[2], XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y);
    V5 := XMVectorSwizzle(M.r[3], XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z);

    P0 := XMVectorMultiply(V0, V1);
    P1 := XMVectorMultiply(V2, V3);
    P2 := XMVectorMultiply(V4, V5);

    V0 := XMVectorSwizzle(M.r[2], XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y);
    V1 := XMVectorSwizzle(M.r[3], XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X);
    V2 := XMVectorSwizzle(M.r[2], XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z);
    V3 := XMVectorSwizzle(M.r[3], XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X);
    V4 := XMVectorSwizzle(M.r[2], XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z);
    V5 := XMVectorSwizzle(M.r[3], XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y);

    P0 := XMVectorNegativeMultiplySubtract(V0, V1, P0);
    P1 := XMVectorNegativeMultiplySubtract(V2, V3, P1);
    P2 := XMVectorNegativeMultiplySubtract(V4, V5, P2);

    V0 := XMVectorSwizzle(M.r[1], XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z);
    V1 := XMVectorSwizzle(M.r[1], XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y);
    V2 := XMVectorSwizzle(M.r[1], XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X);

    S := XMVectorMultiply(M.r[0], Sign.v);
    R := XMVectorMultiply(V0, P0);
    R := XMVectorNegativeMultiplySubtract(V1, P1, R);
    R := XMVectorMultiplyAdd(V2, P2, R);

    Result := XMVector4Dot(S, R);
end;



procedure XM3RANKDECOMPOSE(out a, b, c: size_t; x, y, z: single);
begin
    if ((x) < (y)) then
    begin
        if ((y) < (z)) then
        begin
            (a) := 2;
            (b) := 1;
            (c) := 0;
        end
        else
        begin
            (a) := 1;

            if ((x) < (z)) then
            begin
                (b) := 2;
                (c) := 0;
            end
            else
            begin
                (b) := 0;
                (c) := 2;
            end;
        end;
    end
    else
    begin
        if ((x) < (z)) then
        begin
            (a) := 2;
            (b) := 0;
            (c) := 1;
        end
        else
        begin
            (a) := 0;

            if ((y) < (z)) then
            begin
                (b) := 2;
                (c) := 1;
            end
            else
            begin
                (b) := 1;
                (c) := 2;
            end;
        end;
    end;
end;



function XMMatrixDecompose(out outScale: TXMVECTOR; out outRotQuat: TXMVECTOR; out outTrans: TXMVECTOR; M: TXMMATRIX): boolean;
var
    pvCanonicalBasis: array [0..2] of TXMVECTOR;
    ppvBasis: array [0..2] of TXMVECTOR;
    matTemp: TXMMATRIX;
    pfScales: TXMVECTOR;
    a, b, c: size_t;
    fDet: single;
    aa, bb, cc: size_t;
    fAbsX, fAbsY, fAbsZ: single;
begin
    pvCanonicalBasis[1] := g_XMIdentityR0.v;
    pvCanonicalBasis[1] := g_XMIdentityR1.v;
    pvCanonicalBasis[1] := g_XMIdentityR2.v;

    // Get the translation
    outTrans := M.r[3];


    ppvBasis[0] := matTemp.r[0];
    ppvBasis[1] := matTemp.r[1];
    ppvBasis[2] := matTemp.r[2];

    matTemp.r[0] := M.r[0];
    matTemp.r[1] := M.r[1];
    matTemp.r[2] := M.r[2];
    matTemp.r[3] := g_XMIdentityR3.v;

    //    pfScales := (float *)outScale;


    XMVectorGetXPtr(pfScales.f32[0], XMVector3Length(ppvBasis[0]));
    XMVectorGetXPtr(pfScales.f32[1], XMVector3Length(ppvBasis[1]));
    XMVectorGetXPtr(pfScales.f32[2], XMVector3Length(ppvBasis[2]));
    pfScales.f32[3] := 0.0;

    XM3RANKDECOMPOSE(a, b, c, pfScales.f32[0], pfScales.f32[1], pfScales.f32[2]);

    if (pfScales.f32[a] < XM3_DECOMP_EPSILON) then
    begin
        ppvBasis[a] := pvCanonicalBasis[a];
    end;
    ppvBasis[a] := XMVector3Normalize(ppvBasis[a]);

    if (pfScales.f32[b] < XM3_DECOMP_EPSILON) then
    begin

        fAbsX := abs(XMVectorGetX(ppvBasis[a]));
        fAbsY := abs(XMVectorGetY(ppvBasis[a]));
        fAbsZ := abs(XMVectorGetZ(ppvBasis[a]));

        XM3RANKDECOMPOSE(aa, bb, cc, fAbsX, fAbsY, fAbsZ);

        ppvBasis[b] := XMVector3Cross(ppvBasis[a], pvCanonicalBasis[cc]);
    end;

    ppvBasis[b] := XMVector3Normalize(ppvBasis[b]);

    if (pfScales.f32[c] < XM3_DECOMP_EPSILON) then
    begin
        ppvBasis[c] := XMVector3Cross(ppvBasis[a], ppvBasis[b]);
    end;

    ppvBasis[c] := XMVector3Normalize(ppvBasis[c]);

    fDet := XMVectorGetX(XMMatrixDeterminant(matTemp));

    // use Kramer's rule to check for handedness of coordinate system
    if (fDet < 0.0) then
    begin
        // switch coordinate system by negating the scale and inverting the basis vector on the x-axis
        pfScales.f32[a] := -pfScales.f32[a];
        ppvBasis[a] := XMVectorNegate(ppvBasis[a]);

        fDet := -fDet;
    end;

    fDet := fDet - 1.0;
    fDet := fDet * fDet;

    if (XM3_DECOMP_EPSILON < fDet) then
    begin
        // Non-SRT matrix encountered
        Result := False;
        Exit;
    end;

    // generate the quaternion from the matrix
    outRotQuat := XMQuaternionRotationMatrix(matTemp);

    outScale := pfScales;

    Result := True;
end;


//------------------------------------------------------------------------------
// Transformation operations
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
function XMMatrixIdentity: TXMMATRIX;
begin
    Result.r[0] := g_XMIdentityR0.v;
    Result.r[1] := g_XMIdentityR1.v;
    Result.r[2] := g_XMIdentityR2.v;
    Result.r[3] := g_XMIdentityR3.v;
end;



function XMMatrixRotationRollPitchYaw(Pitch: single; Yaw: single; Roll: single): TXMMATRIX;
var
    Angles: TXMVECTOR;
begin
    Angles := XMVectorSet(Pitch, Yaw, Roll, 0.0);
    Result := XMMatrixRotationRollPitchYawFromVector(Angles);
end;



function XMMatrixRotationRollPitchYawFromVector(Angles: TXMVECTOR): TXMMATRIX;
var
    Q: TXMVECTOR;
begin
    Q := XMQuaternionRotationRollPitchYawFromVector(Angles);
    Result := XMMatrixRotationQuaternion(Q);
end;



function XMMatrixRotationAxis(Axis: TXMVECTOR; Angle: single): TXMMATRIX;
var
    Normal: TXMVECTOR;
begin
    assert(not XMVector3Equal(Axis, XMVectorZero()));
    assert(not XMVector3IsInfinite(Axis));

    Normal := XMVector3Normalize(Axis);
    Result := XMMatrixRotationNormal(Normal, Angle);
end;



function XMMatrixTransformation2D(ScalingOrigin: TXMVECTOR; ScalingOrientation: single; Scaling: TXMVECTOR; RotationOrigin: TXMVECTOR;
    Rotation: single; Translation: TXMVECTOR): TXMMATRIX;
var
    VScalingOrigin, NegScalingOrigin: TXMVECTOR;
    VScaling, VRotationOrigin, VTranslation: TXMVECTOR;
    MScalingOriginI, MScalingOrientation, MScalingOrientationT, MScaling, MRotation: TXMMATRIX;
begin
    // M := Inverse(MScalingOrigin) * Transpose(MScalingOrientation) * MScaling * MScalingOrientation *
    //         MScalingOrigin * Inverse(MRotationOrigin) * MRotation * MRotationOrigin * MTranslation;

    VScalingOrigin := XMVectorSelect(g_XMSelect1100.v, ScalingOrigin, g_XMSelect1100.v);
    NegScalingOrigin := XMVectorNegate(VScalingOrigin);

    MScalingOriginI := XMMatrixTranslationFromVector(NegScalingOrigin);
    MScalingOrientation := XMMatrixRotationZ(ScalingOrientation);
    MScalingOrientationT := XMMatrixTranspose(MScalingOrientation);
    VScaling := XMVectorSelect(g_XMOne.v, Scaling, g_XMSelect1100.v);
    MScaling := XMMatrixScalingFromVector(VScaling);
    VRotationOrigin := XMVectorSelect(g_XMSelect1100.v, RotationOrigin, g_XMSelect1100.v);
    MRotation := XMMatrixRotationZ(Rotation);
    VTranslation := XMVectorSelect(g_XMSelect1100.v, Translation, g_XMSelect1100.v);

    Result := XMMatrixMultiply(MScalingOriginI, MScalingOrientationT);
    Result := XMMatrixMultiply(Result, MScaling);
    Result := XMMatrixMultiply(Result, MScalingOrientation);
    Result.r[3] := XMVectorAdd(Result.r[3], VScalingOrigin);
    Result.r[3] := XMVectorSubtract(Result.r[3], VRotationOrigin);
    Result := XMMatrixMultiply(Result, MRotation);
    Result.r[3] := XMVectorAdd(Result.r[3], VRotationOrigin);
    Result.r[3] := XMVectorAdd(Result.r[3], VTranslation);
end;



function XMMatrixTransformation(ScalingOrigin: TXMVECTOR; ScalingOrientationQuaternion: TXMVECTOR; Scaling: TXMVECTOR;
    RotationOrigin: TXMVECTOR; RotationQuaternion: TXMVECTOR; Translation: TXMVECTOR): TXMMATRIX;
var
    VScalingOrigin, NegScalingOrigin, VRotationOrigin, VTranslation: TXMVECTOR;
    MScalingOriginI, MScalingOrientation, MScalingOrientationT, MScaling, MRotation: TXMMATRIX;
begin
    // M := Inverse(MScalingOrigin) * Transpose(MScalingOrientation) * MScaling * MScalingOrientation *
    //         MScalingOrigin * Inverse(MRotationOrigin) * MRotation * MRotationOrigin * MTranslation;

    VScalingOrigin := XMVectorSelect(g_XMSelect1110.v, ScalingOrigin, g_XMSelect1110.v);
    NegScalingOrigin := XMVectorNegate(ScalingOrigin);

    MScalingOriginI := XMMatrixTranslationFromVector(NegScalingOrigin);
    MScalingOrientation := XMMatrixRotationQuaternion(ScalingOrientationQuaternion);
    MScalingOrientationT := XMMatrixTranspose(MScalingOrientation);
    MScaling := XMMatrixScalingFromVector(Scaling);
    VRotationOrigin := XMVectorSelect(g_XMSelect1110.v, RotationOrigin, g_XMSelect1110.v);
    MRotation := XMMatrixRotationQuaternion(RotationQuaternion);
    VTranslation := XMVectorSelect(g_XMSelect1110.v, Translation, g_XMSelect1110.v);

    Result := XMMatrixMultiply(MScalingOriginI, MScalingOrientationT);
    Result := XMMatrixMultiply(Result, MScaling);
    Result := XMMatrixMultiply(Result, MScalingOrientation);
    Result.r[3] := XMVectorAdd(Result.r[3], VScalingOrigin);
    Result.r[3] := XMVectorSubtract(Result.r[3], VRotationOrigin);
    Result := XMMatrixMultiply(Result, MRotation);
    Result.r[3] := XMVectorAdd(Result.r[3], VRotationOrigin);
    Result.r[3] := XMVectorAdd(Result.r[3], VTranslation);
end;



function XMMatrixAffineTransformation2D(Scaling: TXMVECTOR; RotationOrigin: TXMVECTOR; Rotation: single; Translation: TXMVECTOR): TXMMATRIX;
var
    VScaling, VRotationOrigin, VTranslation: TXMVECTOR;
    MScaling, MRotation: TXMMATRIX;
begin
    // M := MScaling * Inverse(MRotationOrigin) * MRotation * MRotationOrigin * MTranslation;

    VScaling := XMVectorSelect(g_XMOne.v, Scaling, g_XMSelect1100.v);
    MScaling := XMMatrixScalingFromVector(VScaling);
    VRotationOrigin := XMVectorSelect(g_XMSelect1100.v, RotationOrigin, g_XMSelect1100.v);
    MRotation := XMMatrixRotationZ(Rotation);
    VTranslation := XMVectorSelect(g_XMSelect1100.v, Translation, g_XMSelect1100.v);

    Result := MScaling;
    Result.r[3] := XMVectorSubtract(Result.r[3], VRotationOrigin);
    Result := XMMatrixMultiply(Result, MRotation);
    Result.r[3] := XMVectorAdd(Result.r[3], VRotationOrigin);
    Result.r[3] := XMVectorAdd(Result.r[3], VTranslation);
end;



function XMMatrixAffineTransformation(Scaling: TXMVECTOR; RotationOrigin: TXMVECTOR; RotationQuaternion: TXMVECTOR; Translation: TXMVECTOR): TXMMATRIX;
var
    MScaling, MRotation: TXMMATRIX;
    VRotationOrigin, VTranslation: TXMVECTOR;
begin
    // M := MScaling * Inverse(MRotationOrigin) * MRotation * MRotationOrigin * MTranslation;

    MScaling := XMMatrixScalingFromVector(Scaling);
    VRotationOrigin := XMVectorSelect(g_XMSelect1110.v, RotationOrigin, g_XMSelect1110.v);
    MRotation := XMMatrixRotationQuaternion(RotationQuaternion);
    VTranslation := XMVectorSelect(g_XMSelect1110.v, Translation, g_XMSelect1110.v);

    Result := MScaling;
    Result.r[3] := XMVectorSubtract(Result.r[3], VRotationOrigin);
    Result := XMMatrixMultiply(Result, MRotation);
    Result.r[3] := XMVectorAdd(Result.r[3], VRotationOrigin);
    Result.r[3] := XMVectorAdd(Result.r[3], VTranslation);
end;



function XMMatrixReflect(ReflectionPlane: TXMVECTOR): TXMMATRIX;
const
    NegativeTwo: TXMVECTOR = (f32: (-2.0, -2.0, -2.0, 0.0));
var
    P, S, A, B, C, D: TXMVECTOR;

begin
    assert(not XMVector3Equal(ReflectionPlane, XMVectorZero()));
    assert(not XMPlaneIsInfinite(ReflectionPlane));



    P := XMPlaneNormalize(ReflectionPlane);
    S := XMVectorMultiply(P, NegativeTwo);

    A := XMVectorSplatX(P);
    B := XMVectorSplatY(P);
    C := XMVectorSplatZ(P);
    D := XMVectorSplatW(P);

    Result.r[0] := XMVectorMultiplyAdd(A, S, g_XMIdentityR0.v);
    Result.r[1] := XMVectorMultiplyAdd(B, S, g_XMIdentityR1.v);
    Result.r[2] := XMVectorMultiplyAdd(C, S, g_XMIdentityR2.v);
    Result.r[3] := XMVectorMultiplyAdd(D, S, g_XMIdentityR3.v);
end;



function XMMatrixShadow(ShadowPlane: TXMVECTOR; LightPosition: TXMVECTOR): TXMMATRIX;

var
    P, Dot, A, B, C, D, Select0001: TXMVECTOR;
begin

    Select0001.u32[0] := XM_SELECT_0;
    Select0001.u32[1] := XM_SELECT_0;
    Select0001.u32[2] := XM_SELECT_0;
    Select0001.u32[3] := XM_SELECT_1;


    assert(not XMVector3Equal(ShadowPlane, XMVectorZero()));
    assert(not XMPlaneIsInfinite(ShadowPlane));

    P := XMPlaneNormalize(ShadowPlane);
    Dot := XMPlaneDot(P, LightPosition);
    P := XMVectorNegate(P);
    D := XMVectorSplatW(P);
    C := XMVectorSplatZ(P);
    B := XMVectorSplatY(P);
    A := XMVectorSplatX(P);
    Dot := XMVectorSelect(Select0001, Dot, Select0001);

    Result.r[3] := XMVectorMultiplyAdd(D, LightPosition, Dot);
    Dot := XMVectorRotateLeft(Dot, 1);
    Result.r[2] := XMVectorMultiplyAdd(C, LightPosition, Dot);
    Dot := XMVectorRotateLeft(Dot, 1);
    Result.r[1] := XMVectorMultiplyAdd(B, LightPosition, Dot);
    Dot := XMVectorRotateLeft(Dot, 1);
    Result.r[0] := XMVectorMultiplyAdd(A, LightPosition, Dot);
end;

//------------------------------------------------------------------------------
// View and projection initialization operations
//------------------------------------------------------------------------------


function XMMatrixLookAtLH(EyePosition: TXMVECTOR; FocusPosition: TXMVECTOR; UpDirection: TXMVECTOR): TXMMATRIX;
var
    EyeDirection: TXMVECTOR;
begin
    EyeDirection := XMVectorSubtract(FocusPosition, EyePosition);
    Result := XMMatrixLookToLH(EyePosition, EyeDirection, UpDirection);
end;



function XMMatrixLookAtRH(EyePosition: TXMVECTOR; FocusPosition: TXMVECTOR; UpDirection: TXMVECTOR): TXMMATRIX;
var
    NegEyeDirection: TXMVECTOR;
begin
    NegEyeDirection := XMVectorSubtract(EyePosition, FocusPosition);
    Result := XMMatrixLookToLH(EyePosition, NegEyeDirection, UpDirection);

end;



function XMMatrixLookToLH(EyePosition: TXMVECTOR; EyeDirection: TXMVECTOR; UpDirection: TXMVECTOR): TXMMATRIX;
var
    R0, R1, R2, NegEyePosition: TXMVECTOR;
    D0, D1, D2: TXMVECTOR;
begin
    assert(not XMVector3Equal(EyeDirection, XMVectorZero()));
    assert(not XMVector3IsInfinite(EyeDirection));
    assert(not XMVector3Equal(UpDirection, XMVectorZero()));
    assert(not XMVector3IsInfinite(UpDirection));

    R2 := XMVector3Normalize(EyeDirection);

    R0 := XMVector3Cross(UpDirection, R2);
    R0 := XMVector3Normalize(R0);

    R1 := XMVector3Cross(R2, R0);

    NegEyePosition := XMVectorNegate(EyePosition);

    D0 := XMVector3Dot(R0, NegEyePosition);
    D1 := XMVector3Dot(R1, NegEyePosition);
    D2 := XMVector3Dot(R2, NegEyePosition);

    Result.r[0] := XMVectorSelect(D0, R0, g_XMSelect1110.v);
    Result.r[1] := XMVectorSelect(D1, R1, g_XMSelect1110.v);
    Result.r[2] := XMVectorSelect(D2, R2, g_XMSelect1110.v);
    Result.r[3] := g_XMIdentityR3.v;

    Result := XMMatrixTranspose(Result);
end;



function XMMatrixLookToRH(EyePosition: TXMVECTOR; EyeDirection: TXMVECTOR; UpDirection: TXMVECTOR): TXMMATRIX;
var
    NegEyeDirection: TXMVECTOR;
begin
    NegEyeDirection := XMVectorNegate(EyeDirection);
    Result := XMMatrixLookToLH(EyePosition, NegEyeDirection, UpDirection);
end;


//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------
function XMQuaternionEqual(Q1: TXMVECTOR; Q2: TXMVECTOR): boolean;
begin
    Result := XMVector4Equal(Q1, Q2);
end;



function XMQuaternionNotEqual(Q1: TXMVECTOR; Q2: TXMVECTOR): boolean;
begin
    Result := XMVector4NotEqual(Q1, Q2);
end;



function XMQuaternionIsNaN(Q: TXMVECTOR): boolean;
begin
    Result := XMVector4IsNaN(Q);
end;



function XMQuaternionIsInfinite(Q: TXMVECTOR): boolean;
begin
    Result := XMVector4IsInfinite(Q);
end;



function XMQuaternionIsIdentity(Q: TXMVECTOR): boolean;
begin
    Result := XMVector4Equal(Q, g_XMIdentityR3.v);
end;


//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------
function XMQuaternionDot(Q1: TXMVECTOR; Q2: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4Dot(Q1, Q2);
end;



function XMQuaternionLengthSq(Q: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4LengthSq(Q);
end;



function XMQuaternionReciprocalLength(Q: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4ReciprocalLength(Q);
end;



function XMQuaternionLength(Q: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4Length(Q);
end;



function XMQuaternionNormalizeEst(Q: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4NormalizeEst(Q);
end;



function XMQuaternionNormalize(Q: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4Normalize(Q);
end;


// Returns the inverse of Q.
function XMQuaternionInverse(Q: TXMVECTOR): TXMVECTOR;
var
    Zero, L, Conjugate, Control: TXMVECTOR;
begin
    Zero := XMVectorZero();

    L := XMVector4LengthSq(Q);
    Conjugate := XMQuaternionConjugate(Q);

    Control := XMVectorLessOrEqual(L, g_XMEpsilon.v);

    Result := XMVectorDivide(Conjugate, L);

    Result := XMVectorSelect(Result, Zero, Control);
end;


// Returns the natural logarithm of Q.
function XMQuaternionLn(Q: TXMVECTOR): TXMVECTOR;
const
    OneMinusEpsilon: TXMVECTORF32 = (f: (1.0 - 0.00001, 1.0 - 0.00001, 1.0 - 0.00001, 1.0 - 0.00001));
var
    QW, Q0, ControlW, Theta, SinTheta, S: TXMVECTOR;
begin
    QW := XMVectorSplatW(Q);
    Q0 := XMVectorSelect(g_XMSelect1110.v, Q, g_XMSelect1110.v);

    ControlW := XMVectorInBounds(QW, OneMinusEpsilon.v);

    Theta := XMVectorACos(QW);
    SinTheta := XMVectorSin(Theta);

    S := XMVectorDivide(Theta, SinTheta);

    Result := XMVectorMultiply(Q0, S);
    Result := XMVectorSelect(Q0, Result, ControlW);
end;

// Returns the exponential of Q.

function XMQuaternionExp(Q: TXMVECTOR): TXMVECTOR;
var
    Theta, SinTheta, CosTheta, S, Zero, Control: TXMVECTOR;
begin
    Theta := XMVector3Length(Q);
    XMVectorSinCos(SinTheta, CosTheta, Theta);
    S := XMVectorDivide(SinTheta, Theta);
    Result := XMVectorMultiply(Q, S);
    Zero := XMVectorZero();
    Control := XMVectorNearEqual(Theta, Zero, g_XMEpsilon.v);
    Result := XMVectorSelect(Result, Q, Control);
    Result := XMVectorSelect(CosTheta, Result, g_XMSelect1110.v);
end;

// Returns the interpolated quaternion. If Q0 and Q1 are not unit quaternions, the resulting interpolation is undefined.

function XMQuaternionSlerp(Q0: TXMVECTOR; Q1: TXMVECTOR; t: single): TXMVECTOR;
var
    TV: TXMVECTOR;
begin
    TV := XMVectorReplicate(t);
    Result := XMQuaternionSlerpV(Q0, Q1, TV);
end;


// Returns the interpolated quaternion. If Q0, Q1, Q2, and Q3 are not all unit quaternions, the returned quaternion is undefined.
function XMQuaternionSquad(Q0: TXMVECTOR; Q1: TXMVECTOR; Q2: TXMVECTOR; Q3: TXMVECTOR; t: single): TXMVECTOR;
var
    TV: TXMVECTOR;
begin
    TV := XMVectorReplicate(t);
    Result := XMQuaternionSquadV(Q0, Q1, Q2, Q3, TV);
end;

//------------------------------------------------------------------------------
// Returns the interpolated quaternion. If Q0, Q1, Q2, and Q3 are not unit quaternions, the resulting interpolation is undefined.
function XMQuaternionSquadV(Q0: TXMVECTOR; Q1: TXMVECTOR; Q2: TXMVECTOR; Q3: TXMVECTOR; T: TXMVECTOR): TXMVECTOR;
var
    TP, Two, Q03, Q12: TXMVECTOR;
begin
    assert((XMVectorGetY(T) = XMVectorGetX(T)) and (XMVectorGetZ(T) = XMVectorGetX(T)) and (XMVectorGetW(T) = XMVectorGetX(T)),
        'Failure in XMQuaternionSquadV');

    TP := T;
    Two := XMVectorSplatConstant(2, 0);

    Q03 := XMQuaternionSlerpV(Q0, Q3, T);
    Q12 := XMQuaternionSlerpV(Q1, Q2, T);

    TP := XMVectorNegativeMultiplySubtract(TP, TP, TP);
    TP := XMVectorMultiply(TP, Two);

    Result := XMQuaternionSlerpV(Q03, Q12, TP);
end;


//------------------------------------------------------------------------------
// Provides addresses of setup control points for spherical quadrangle interpolation.
procedure XMQuaternionSquadSetup(out pA: TXMVECTOR; out pB: TXMVECTOR; out pC: TXMVECTOR; Q0: TXMVECTOR; Q1: TXMVECTOR;
    Q2: TXMVECTOR; Q3: TXMVECTOR);
var
    LS12, LD12, SQ2, Control1: TXMVECTOR;
    LS01, LD01, SQ0, LS23, LD23, SQ3: TXMVECTOR;
    Control0, Control2, InvQ1, InvQ2, LnQ0, LnQ1, LnQ2, LnQ3: TXMVECTOR;
    NegativeOneQuarter, ExpQ02, ExpQ13: TXMVECTOR;
begin
    LS12 := XMQuaternionLengthSq(XMVectorAdd(Q1, Q2));
    LD12 := XMQuaternionLengthSq(XMVectorSubtract(Q1, Q2));
    SQ2 := XMVectorNegate(Q2);

    Control1 := XMVectorLess(LS12, LD12);
    SQ2 := XMVectorSelect(Q2, SQ2, Control1);

    LS01 := XMQuaternionLengthSq(XMVectorAdd(Q0, Q1));
    LD01 := XMQuaternionLengthSq(XMVectorSubtract(Q0, Q1));
    SQ0 := XMVectorNegate(Q0);

    LS23 := XMQuaternionLengthSq(XMVectorAdd(SQ2, Q3));
    LD23 := XMQuaternionLengthSq(XMVectorSubtract(SQ2, Q3));
    SQ3 := XMVectorNegate(Q3);

    Control0 := XMVectorLess(LS01, LD01);
    Control2 := XMVectorLess(LS23, LD23);

    SQ0 := XMVectorSelect(Q0, SQ0, Control0);
    SQ3 := XMVectorSelect(Q3, SQ3, Control2);

    InvQ1 := XMQuaternionInverse(Q1);
    InvQ2 := XMQuaternionInverse(SQ2);

    LnQ0 := XMQuaternionLn(XMQuaternionMultiply(InvQ1, SQ0));
    LnQ2 := XMQuaternionLn(XMQuaternionMultiply(InvQ1, SQ2));
    LnQ1 := XMQuaternionLn(XMQuaternionMultiply(InvQ2, Q1));
    LnQ3 := XMQuaternionLn(XMQuaternionMultiply(InvQ2, SQ3));

    NegativeOneQuarter := XMVectorSplatConstant(-1, 2);

    ExpQ02 := XMVectorMultiply(XMVectorAdd(LnQ0, LnQ2), NegativeOneQuarter);
    ExpQ13 := XMVectorMultiply(XMVectorAdd(LnQ1, LnQ3), NegativeOneQuarter);
    ExpQ02 := XMQuaternionExp(ExpQ02);
    ExpQ13 := XMQuaternionExp(ExpQ13);

    pA := XMQuaternionMultiply(Q1, ExpQ02);
    pB := XMQuaternionMultiply(SQ2, ExpQ13);
    pC := SQ2;
end;

//------------------------------------------------------------------------------
// Returns a quaternion in barycentric coordinates.
function XMQuaternionBaryCentric(Q0: TXMVECTOR; Q1: TXMVECTOR; Q2: TXMVECTOR; f: single; g: single): TXMVECTOR;
var
    s: single;
    Q01, Q02: TXMVECTOR;
begin
    s := f + g;

    if ((s < 0.00001) and (s > -0.00001)) then
        Result := Q0
    else
    begin
        Q01 := XMQuaternionSlerp(Q0, Q1, s);
        Q02 := XMQuaternionSlerp(Q0, Q2, s);

        Result := XMQuaternionSlerp(Q01, Q02, g / s);
    end;
end;

//------------------------------------------------------------------------------
// Returns a point in barycentric coordinates, using the specified quaternions.
function XMQuaternionBaryCentricV(Q0: TXMVECTOR; Q1: TXMVECTOR; Q2: TXMVECTOR; F: TXMVECTOR; G: TXMVECTOR): TXMVECTOR;
var
    Epsilon, S, Q01, Q02, GS: TXMVECTOR;
begin
    assert((XMVectorGetY(F) = XMVectorGetX(F)) and (XMVectorGetZ(F) = XMVectorGetX(F)) and (XMVectorGetW(F) = XMVectorGetX(F)),
        'Failure in XMQuaternionBaryCentricV (1)');
    assert((XMVectorGetY(G) = XMVectorGetX(G)) and (XMVectorGetZ(G) = XMVectorGetX(G)) and (XMVectorGetW(G) = XMVectorGetX(G)),
        'Failure in XMQuaternionBaryCentricV (2)');

    Epsilon := XMVectorSplatConstant(1, 16);

    S := XMVectorAdd(F, G);

    if (XMVector4InBounds(S, Epsilon)) then
        Result := Q0
    else
    begin
        Q01 := XMQuaternionSlerpV(Q0, Q1, S);
        Q02 := XMQuaternionSlerpV(Q0, Q2, S);
        GS := XMVectorReciprocal(S);
        GS := XMVectorMultiply(G, GS);

        Result := XMQuaternionSlerpV(Q01, Q02, GS);
    end;
end;


//------------------------------------------------------------------------------
// Transformation operations
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// Returns the identity quaternion.
function XMQuaternionIdentity: TXMVECTOR;
begin
    Result := g_XMIdentityR3.v;
end;

//------------------------------------------------------------------------------
// Computes a rotation quaternion based on the pitch, yaw, and roll (Euler angles).
function XMQuaternionRotationRollPitchYaw(Pitch: single; Yaw: single; Roll: single): TXMVECTOR;
var
    Angles: TXMVECTOR;
begin
    Angles := XMVectorSet(Pitch, Yaw, Roll, 0.0);
    Result := XMQuaternionRotationRollPitchYawFromVector(Angles);
end;



// Computes a rotation quaternion based on a vector containing the Euler angles (pitch, yaw, and roll).
function XMQuaternionRotationRollPitchYawFromVector(Angles: TXMVECTOR): TXMVECTOR;
const
    Sign: TXMVECTORF32 = (f: (1.0, -1.0, -1.0, 1.0));
var
    HalfAngles: TXMVECTOR;
    SinAngles, CosAngles: TXMVECTOR;
    P0, Y0, R0, P1, Y1, R1: TXMVECTOR;
    Q1, Q0: TXMVECTOR;
begin
    HalfAngles := XMVectorMultiply(Angles, g_XMOneHalf.v);
    XMVectorSinCos(SinAngles, CosAngles, HalfAngles);

    P0 := XMVectorPermute(SinAngles, CosAngles, XM_PERMUTE_0X, XM_PERMUTE_1X, XM_PERMUTE_1X, XM_PERMUTE_1X);
    Y0 := XMVectorPermute(SinAngles, CosAngles, XM_PERMUTE_1Y, XM_PERMUTE_0Y, XM_PERMUTE_1Y, XM_PERMUTE_1Y);
    R0 := XMVectorPermute(SinAngles, CosAngles, XM_PERMUTE_1Z, XM_PERMUTE_1Z, XM_PERMUTE_0Z, XM_PERMUTE_1Z);
    P1 := XMVectorPermute(CosAngles, SinAngles, XM_PERMUTE_0X, XM_PERMUTE_1X, XM_PERMUTE_1X, XM_PERMUTE_1X);
    Y1 := XMVectorPermute(CosAngles, SinAngles, XM_PERMUTE_1Y, XM_PERMUTE_0Y, XM_PERMUTE_1Y, XM_PERMUTE_1Y);
    R1 := XMVectorPermute(CosAngles, SinAngles, XM_PERMUTE_1Z, XM_PERMUTE_1Z, XM_PERMUTE_0Z, XM_PERMUTE_1Z);

    Q1 := XMVectorMultiply(P1, Sign.v);
    Q0 := XMVectorMultiply(P0, Y0);
    Q1 := XMVectorMultiply(Q1, Y1);
    Q0 := XMVectorMultiply(Q0, R0);
    Result := XMVectorMultiplyAdd(Q1, R1, Q0);
end;



function XMQuaternionRotationAxis(Axis: TXMVECTOR; Angle: single): TXMVECTOR;
var
    Normal: TXMVECTOR;
begin
    assert(not XMVector3Equal(Axis, XMVectorZero()), 'Failure in XMQuaternionRotationAxis (1)');
    assert(not XMVector3IsInfinite(Axis), 'Failure in XMQuaternionRotationAxis (2)');

    Normal := XMVector3Normalize(Axis);
    Result := XMQuaternionRotationNormal(Normal, Angle);
end;


//------------------------------------------------------------------------------
// Conversion operations
//------------------------------------------------------------------------------

procedure XMQuaternionToAxisAngle(out pAxis: TXMVECTOR; out pAngle: single; Q: TXMVECTOR);
begin
    pAxis := Q;
    pAngle := 2.0 * XMScalarACos(XMVectorGetW(Q));
end;



{***************************************************************************
 *
 * Plane
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------

function XMPlaneEqual(P1: TXMVECTOR; P2: TXMVECTOR): boolean;
begin
    Result := XMVector4Equal(P1, P2);
end;



function XMPlaneNearEqual(P1: TXMVECTOR; P2: TXMVECTOR; Epsilon: TXMVECTOR): boolean;
var
    NP1, NP2: TXMVECTOR;
begin
    NP1 := XMPlaneNormalize(P1);
    NP2 := XMPlaneNormalize(P2);
    Result := XMVector4NearEqual(NP1, NP2, Epsilon);
end;



function XMPlaneNotEqual(P1: TXMVECTOR; P2: TXMVECTOR): boolean;
begin
    Result := XMVector4NotEqual(P1, P2);
end;



function XMPlaneIsNaN(P: TXMVECTOR): boolean;
begin
    Result := XMVector4IsNaN(P);
end;



function XMPlaneIsInfinite(P: TXMVECTOR): boolean;
begin
    Result := XMVector4IsInfinite(P);
end;


//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------


function XMPlaneDot(P: TXMVECTOR; V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4Dot(P, V);
end;



function XMPlaneDotCoord(P: TXMVECTOR; V: TXMVECTOR): TXMVECTOR;
var
    V3: TXMVECTOR;
begin
    // Result = P[0] * V[0] + P[1] * V[1] + P[2] * V[2] + P[3]
    V3 := XMVectorSelect(g_XMOne.v, V, g_XMSelect1110.v);
    Result := XMVector4Dot(P, V3);
end;



function XMPlaneDotNormal(P: TXMVECTOR; V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector3Dot(P, V);
end;



// Returns the intersection of the plane P and the line defined by LinePoint1 and LinePoint2. If the line is parallel to the plane, all components of the returned vector are equal to QNaN.
function XMPlaneIntersectLine(P: TXMVECTOR; LinePoint1: TXMVECTOR; LinePoint2: TXMVECTOR): TXMVECTOR;
var
    V1, V2, D, VT, Point, Zero, Control: TXMVECTOR;
begin
    V1 := XMVector3Dot(P, LinePoint1);
    V2 := XMVector3Dot(P, LinePoint2);
    D := XMVectorSubtract(V1, V2);

    VT := XMPlaneDotCoord(P, LinePoint1);
    VT := XMVectorDivide(VT, D);

    Point := XMVectorSubtract(LinePoint2, LinePoint1);
    Point := XMVectorMultiplyAdd(Point, VT, LinePoint1);

    Zero := XMVectorZero();
    Control := XMVectorNearEqual(D, Zero, g_XMEpsilon.v);

    Result := XMVectorSelect(Point, g_XMQNaN.v, Control);
end;


// Finds the intersection of two planes.
// If the planes are parallel to one another, all components of the returned point vectors are equal to QNaN.
procedure XMPlaneIntersectPlane(out pLinePoint1: TXMVECTOR; out pLinePoint2: TXMVECTOR; P1: TXMVECTOR; P2: TXMVECTOR);
var
    V1, V2, LengthSq, P1W, Point, V3, P2W, LinePoint1, LinePoint2, Control: TXMVECTOR;
begin
    V1 := XMVector3Cross(P2, P1);

    LengthSq := XMVector3LengthSq(V1);

    V2 := XMVector3Cross(P2, V1);

    P1W := XMVectorSplatW(P1);
    Point := XMVectorMultiply(V2, P1W);

    V3 := XMVector3Cross(V1, P1);

    P2W := XMVectorSplatW(P2);
    Point := XMVectorMultiplyAdd(V3, P2W, Point);

    LinePoint1 := XMVectorDivide(Point, LengthSq);

    LinePoint2 := XMVectorAdd(LinePoint1, V1);

    Control := XMVectorLessOrEqual(LengthSq, g_XMEpsilon.v);
    pLinePoint1 := XMVectorSelect(LinePoint1, g_XMQNaN.v, Control);
    pLinePoint2 := XMVectorSelect(LinePoint2, g_XMQNaN.v, Control);
end;



function XMPlaneTransform(P: TXMVECTOR; M: TXMMATRIX): TXMVECTOR;
var
    W, X, Y, Z: TXMVECTOR;
begin
    W := XMVectorSplatW(P);
    Z := XMVectorSplatZ(P);
    Y := XMVectorSplatY(P);
    X := XMVectorSplatX(P);

    Result := XMVectorMultiply(W, M.r[3]);
    Result := XMVectorMultiplyAdd(Z, M.r[2], Result);
    Result := XMVectorMultiplyAdd(Y, M.r[1], Result);
    Result := XMVectorMultiplyAdd(X, M.r[0], Result);
end;



function XMPlaneTransformStream(out pOutputStream: PXMFLOAT4; OutputStride: size_t; constref pInputStream: PXMFLOAT4;
    InputStride: size_t; PlaneCount: size_t; M: TXMMATRIX): PXMFLOAT4;
begin
    Result := XMVector4TransformStream(pOutputStream, OutputStride, pInputStream, InputStride, PlaneCount, M);
end;

//------------------------------------------------------------------------------
// Conversion operations
//------------------------------------------------------------------------------


function XMPlaneFromPointNormal(Point: TXMVECTOR; Normal: TXMVECTOR): TXMVECTOR;
var
    W: TXMVECTOR;
begin
    W := XMVector3Dot(Point, Normal);
    W := XMVectorNegate(W);
    Result := XMVectorSelect(W, Normal, g_XMSelect1110.v);
end;



function XMPlaneFromPoints(Point1: TXMVECTOR; Point2: TXMVECTOR; Point3: TXMVECTOR): TXMVECTOR;
var
    V21, V31, N, D: TXMVECTOR;
begin
    V21 := XMVectorSubtract(Point1, Point2);
    V31 := XMVectorSubtract(Point1, Point3);

    N := XMVector3Cross(V21, V31);
    N := XMVector3Normalize(N);

    D := XMPlaneDotNormal(N, Point1);
    D := XMVectorNegate(D);

    Result := XMVectorSelect(D, N, g_XMSelect1110.v);
end;


{***************************************************************************
 *
 * Color
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------

function XMColorEqual(C1: TXMVECTOR; C2: TXMVECTOR): boolean;
begin
    Result := XMVector4Equal(C1, C2);
end;



function XMColorNotEqual(C1: TXMVECTOR; C2: TXMVECTOR): boolean;
begin
    Result := XMVector4NotEqual(C1, C2);
end;



function XMColorGreater(C1: TXMVECTOR; C2: TXMVECTOR): boolean;
begin
    Result := XMVector4Greater(C1, C2);
end;



function XMColorGreaterOrEqual(C1: TXMVECTOR; C2: TXMVECTOR): boolean;
begin
    Result := XMVector4GreaterOrEqual(C1, C2);
end;



function XMColorLess(C1: TXMVECTOR; C2: TXMVECTOR): boolean;
begin
    Result := XMVector4Less(C1, C2);
end;



function XMColorLessOrEqual(C1: TXMVECTOR; C2: TXMVECTOR): boolean;
begin
    Result := XMVector4LessOrEqual(C1, C2);
end;



function XMColorIsNaN(C: TXMVECTOR): boolean;
begin
    Result := XMVector4IsNaN(C);
end;



function XMColorIsInfinite(C: TXMVECTOR): boolean;
begin
    Result := XMVector4IsInfinite(C);
end;



function XMColorModulate(C1: TXMVECTOR; C2: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVectorMultiply(C1, C2);
end;



function XMColorRGBToHSL(rgb: TXMVECTOR): TXMVECTOR;
var
    r, g, b, min, max, l, d, la: TXMVECTOR;
    s, h, d2, lha: TXMVECTOR;
begin
    r := XMVectorSplatX(rgb);
    g := XMVectorSplatY(rgb);
    b := XMVectorSplatZ(rgb);

    min := XMVectorMin(r, XMVectorMin(g, b));
    max := XMVectorMax(r, XMVectorMax(g, b));

    l := XMVectorMultiply(XMVectorAdd(min, max), g_XMOneHalf);

    d := XMVectorSubtract(max, min);

    la := XMVectorSelect(rgb, l, g_XMSelect1110);

    if (XMVector3Less(d, g_XMEpsilon)) then
    begin
        // Achromatic, assume H and S of 0
        Result := XMVectorSelect(la, g_XMZero, g_XMSelect1100);
        Exit;
    end
    else
    begin
        d2 := XMVectorAdd(min, max);

        if (XMVector3Greater(l, g_XMOneHalf)) then
        begin
            // d / (2-max-min)
            s := XMVectorDivide(d, XMVectorSubtract(g_XMTwo, d2));
        end
        else
        begin
            // d / (max+min)
            s := XMVectorDivide(d, d2);
        end;

        if (XMVector3Equal(r, max)) then
        begin
            // Red is max
            h := XMVectorDivide(XMVectorSubtract(g, b), d);
        end
        else if (XMVector3Equal(g, max)) then
        begin
            // Green is max
            h := XMVectorDivide(XMVectorSubtract(b, r), d);
            h := XMVectorAdd(h, g_XMTwo);
        end
        else
        begin
            // Blue is max
            h := XMVectorDivide(XMVectorSubtract(r, g), d);
            h := XMVectorAdd(h, g_XMFour);
        end;

        h := XMVectorDivide(h, g_XMSix);

        if (XMVector3Less(h, g_XMZero)) then
            h := XMVectorAdd(h, g_XMOne);

        lha := XMVectorSelect(la, h, g_XMSelect1100);
        Result := XMVectorSelect(s, lha, g_XMSelect1011);
    end;
end;



function XMColorHue2Clr(p, q, h: TXMVECTOR): TXMVECTOR;
const
    oneSixth: TXMVECTORF32 = (f: (1.0 / 6.0, 1.0 / 6.0, 1.0 / 6.0, 1.0 / 6.0));
    twoThirds: TXMVECTORF32 = (f: (2.0 / 3.0, 2.0 / 3.0, 2.0 / 3.0, 2.0 / 3.0));
var
    t, t1, t2: TXMVECTOR;
begin
    t := h;

    if (XMVector3Less(t, g_XMZero)) then
        t := XMVectorAdd(t, g_XMOne);

    if (XMVector3Greater(t, g_XMOne)) then
        t := XMVectorSubtract(t, g_XMOne);

    if (XMVector3Less(t, oneSixth)) then
    begin
        // p + (q - p) * 6 * t
        t1 := XMVectorSubtract(q, p);
        t2 := XMVectorMultiply(g_XMSix, t);
        Result := XMVectorMultiplyAdd(t1, t2, p);
        Exit;
    end;

    if (XMVector3Less(t, g_XMOneHalf)) then
    begin
        Result := q;
        Exit;
    end;

    if (XMVector3Less(t, twoThirds)) then
    begin
        // p + (q - p) * 6 * (2/3 - t)
        t1 := XMVectorSubtract(q, p);
        t2 := XMVectorMultiply(g_XMSix, XMVectorSubtract(twoThirds, t));
        Result := XMVectorMultiplyAdd(t1, t2, p);
        Exit;
    end;

    Result := p;
end;



function XMColorHSLToRGB(hsl: TXMVECTOR): TXMVECTOR;
const
    oneThird: TXMVECTORF32 = (f: (1.0 / 3.0, 1.0 / 3.0, 1.0 / 3.0, 1.0 / 3.0));
var
    s, l, h, q, p, r, g, b, rg, ba: TXMVECTOR;
begin
    s := XMVectorSplatY(hsl);
    l := XMVectorSplatZ(hsl);

    if (XMVector3NearEqual(s, g_XMZero, g_XMEpsilon)) then
    begin
        // Achromatic
        Result := XMVectorSelect(hsl, l, g_XMSelect1110);
    end
    else
    begin
        h := XMVectorSplatX(hsl);

        if (XMVector3Less(l, g_XMOneHalf)) then
        begin
            q := XMVectorMultiply(l, XMVectorAdd(g_XMOne, s));
        end
        else
        begin
            q := XMVectorSubtract(XMVectorAdd(l, s), XMVectorMultiply(l, s));
        end;

        p := XMVectorSubtract(XMVectorMultiply(g_XMTwo, l), q);

        r := XMColorHue2Clr(p, q, XMVectorAdd(h, oneThird));
        g := XMColorHue2Clr(p, q, h);
        b := XMColorHue2Clr(p, q, XMVectorSubtract(h, oneThird));

        rg := XMVectorSelect(g, r, g_XMSelect1000);
        ba := XMVectorSelect(hsl, b, g_XMSelect1110);

        Result := XMVectorSelect(ba, rg, g_XMSelect1100);
    end;
end;



function XMColorRGBToHSV(rgb: TXMVECTOR): TXMVECTOR;
var
    r, g, b, min, v, d, s: TXMVECTOR;
    h, hv, hva: TXMVECTOR;
begin
    r := XMVectorSplatX(rgb);
    g := XMVectorSplatY(rgb);
    b := XMVectorSplatZ(rgb);

    min := XMVectorMin(r, XMVectorMin(g, b));
    v := XMVectorMax(r, XMVectorMax(g, b));

    d := XMVectorSubtract(v, min);
    if (XMVector3NearEqual(v, g_XMZero, g_XMEpsilon)) then
        s := g_XMZero
    else
        s := XMVectorDivide(d, v);

    if (XMVector3Less(d, g_XMEpsilon)) then
    begin
        // Achromatic, assume H of 0
        hv := XMVectorSelect(v, g_XMZero, g_XMSelect1000);
        hva := XMVectorSelect(rgb, hv, g_XMSelect1110);
        Result := XMVectorSelect(s, hva, g_XMSelect1011);
    end
    else
    begin
        if (XMVector3Equal(r, v)) then
        begin
            // Red is max
            h := XMVectorDivide(XMVectorSubtract(g, b), d);

            if (XMVector3Less(g, b)) then
                h := XMVectorAdd(h, g_XMSix);
        end
        else if (XMVector3Equal(g, v)) then
        begin
            // Green is max
            h := XMVectorDivide(XMVectorSubtract(b, r), d);
            h := XMVectorAdd(h, g_XMTwo);
        end
        else
        begin
            // Blue is max
            h := XMVectorDivide(XMVectorSubtract(r, g), d);
            h := XMVectorAdd(h, g_XMFour);
        end;

        h := XMVectorDivide(h, g_XMSix);

        hv := XMVectorSelect(v, h, g_XMSelect1000);
        hva := XMVectorSelect(rgb, hv, g_XMSelect1110);
        Result := XMVectorSelect(s, hva, g_XMSelect1011);
    end;
end;



function XMColorHSVToRGB(hsv: TXMVECTOR): TXMVECTOR;
var
    h, s, v, h6, i, f, p, q, t: TXMVECTOR;
    _rgb: TXMVECTOR;
    ii: int32;
    vt, qv, pv, pq, tp, vp: TXMVECTOR;
begin
    h := XMVectorSplatX(hsv);
    s := XMVectorSplatY(hsv);
    v := XMVectorSplatZ(hsv);

    h6 := XMVectorMultiply(h, g_XMSix);

    i := XMVectorFloor(h6);
    f := XMVectorSubtract(h6, i);

    // p = v* (1-s)
    p := XMVectorMultiply(v, XMVectorSubtract(g_XMOne, s));

    // q = v*(1-f*s)
    q := XMVectorMultiply(v, XMVectorSubtract(g_XMOne, XMVectorMultiply(f, s)));

    // t = v*(1 - (1-f)*s)
    t := XMVectorMultiply(v, XMVectorSubtract(g_XMOne, XMVectorMultiply(XMVectorSubtract(g_XMOne, f), s)));

    ii := trunc((XMVectorGetX(XMVectorMod(i, g_XMSix))));

    case (ii) of
        0: // rgb = vtp
        begin
            vt := XMVectorSelect(t, v, g_XMSelect1000);
            _rgb := XMVectorSelect(p, vt, g_XMSelect1100);
        end;
        1: // rgb = qvp
        begin
            qv := XMVectorSelect(v, q, g_XMSelect1000);
            _rgb := XMVectorSelect(p, qv, g_XMSelect1100);
        end;
        2: // rgb = pvt
        begin
            pv := XMVectorSelect(v, p, g_XMSelect1000);
            _rgb := XMVectorSelect(t, pv, g_XMSelect1100);
        end;
        3: // rgb = pqv
        begin
            pq := XMVectorSelect(q, p, g_XMSelect1000);
            _rgb := XMVectorSelect(v, pq, g_XMSelect1100);
        end;
        4: // rgb = tpv
        begin
            tp := XMVectorSelect(p, t, g_XMSelect1000);
            _rgb := XMVectorSelect(v, tp, g_XMSelect1100);
        end;
        else // rgb = vpq
        begin
            vp := XMVectorSelect(p, v, g_XMSelect1000);
            _rgb := XMVectorSelect(q, vp, g_XMSelect1100);
        end;
    end;

    Result := XMVectorSelect(hsv, _rgb, g_XMSelect1110);
end;



function XMColorRGBToYUV(rgb: TXMVECTOR): TXMVECTOR;
const
    Scale0: TXMVECTORF32 = (f: (0.299, -0.147, 0.615, 0.0));
    Scale1: TXMVECTORF32 = (f: (0.587, -0.289, -0.515, 0.0));
    Scale2: TXMVECTORF32 = (f: (0.114, 0.436, -0.100, 0.0));
var
    m: TXMMATRIX;
    clr: TXMVECTOR;
begin

    M.Create(Scale0, Scale1, Scale2, g_XMZero);
    clr := XMVector3Transform(rgb, M);

    Result := XMVectorSelect(rgb, clr, g_XMSelect1110);
end;



function XMColorYUVToRGB(yuv: TXMVECTOR): TXMVECTOR;
const
    Scale1: TXMVECTORF32 = (f: (0.0, -0.395, 2.032, 0.0));
    Scale2: TXMVECTORF32 = (f: (1.140, -0.581, 0.0, 0.0));
var
    m: TXMMATRIX;
    clr: TXMVECTOR;
begin

    M.Create(g_XMOne, Scale1, Scale2, g_XMZero);
    clr := XMVector3Transform(yuv, M);

    Result := XMVectorSelect(yuv, clr, g_XMSelect1110);
end;



function XMColorRGBToYUV_HD(rgb: TXMVECTOR): TXMVECTOR;
const
    Scale0: TXMVECTORF32 = (f: (0.2126, -0.0997, 0.6150, 0.0));
    Scale1: TXMVECTORF32 = (f: (0.7152, -0.3354, -0.5586, 0.0));
    Scale2: TXMVECTORF32 = (f: (0.0722, 0.4351, -0.0564, 0.0));
var
    m: TXMMATRIX;
    clr: TXMVECTOR;
begin
    M.Create(Scale0, Scale1, Scale2, g_XMZero);
    clr := XMVector3Transform(rgb, M);
    Result := XMVectorSelect(rgb, clr, g_XMSelect1110);
end;



function XMColorYUVToRGB_HD(yuv: TXMVECTOR): TXMVECTOR;
const
    Scale1: TXMVECTORF32 = (f: (0.0, -0.2153, 2.1324, 0.0));
    Scale2: TXMVECTORF32 = (f: (1.2803, -0.3806, 0.0, 0.0));
var
    m: TXMMATRIX;
    clr: TXMVECTOR;
begin
    M.Create(g_XMOne, Scale1, Scale2, g_XMZero);
    clr := XMVector3Transform(yuv, M);
    Result := XMVectorSelect(yuv, clr, g_XMSelect1110);
end;



function XMColorRGBToXYZ(rgb: TXMVECTOR): TXMVECTOR;
const
    Scale0: TXMVECTORF32 = (f: (0.4887180, 0.1762044, 0.0000000, 0.0));
    Scale1: TXMVECTORF32 = (f: (0.3106803, 0.8129847, 0.0102048, 0.0));
    Scale2: TXMVECTORF32 = (f: (0.2006017, 0.0108109, 0.9897952, 0.0));
    Scale: TXMVECTORF32 = (f: (1.0 / 0.17697, 1.0 / 0.17697, 1.0 / 0.17697, 0.0));
var
    m: TXMMATRIX;
    clr: TXMVECTOR;
begin
    M.Create(Scale0, Scale1, Scale2, g_XMZero);
    clr := XMVectorMultiply(XMVector3Transform(rgb, M), Scale);
    Result := XMVectorSelect(rgb, clr, g_XMSelect1110);
end;



function XMColorXYZToRGB(xyz: TXMVECTOR): TXMVECTOR;
const
    Scale0: TXMVECTORF32 = (f: (2.3706743, -0.5138850, 0.0052982, 0.0));
    Scale1: TXMVECTORF32 = (f: (-0.9000405, 1.4253036, -0.0146949, 0.0));
    Scale2: TXMVECTORF32 = (f: (-0.4706338, 0.0885814, 1.0093968, 0.0));
    Scale: TXMVECTORF32 = (f: (0.17697, 0.17697, 0.17697, 0.0));
var
    m: TXMMATRIX;
    clr: TXMVECTOR;
begin
    M.Create(Scale0, Scale1, Scale2, g_XMZero);
    clr := XMVector3Transform(XMVectorMultiply(xyz, Scale), M);
    Result := XMVectorSelect(xyz, clr, g_XMSelect1110);
end;



function XMColorXYZToSRGB(xyz: TXMVECTOR): TXMVECTOR;
const
    Scale0: TXMVECTORF32 = (f: (3.2406, -0.9689, 0.0557, 0.0));
    Scale1: TXMVECTORF32 = (f: (-1.5372, 1.8758, -0.2040, 0.0));
    Scale2: TXMVECTORF32 = (f: (-0.4986, 0.0415, 1.0570, 0.0));
    Cutoff: TXMVECTORF32 = (f: (0.0031308, 0.0031308, 0.0031308, 0.0));
    Exp: TXMVECTORF32 = (f: (1.0 / 2.4, 1.0 / 2.4, 1.0 / 2.4, 1.0));
var
    lclr, clr, largeC, smallC, sel: TXMVECTOR;
    M: TXMMATRIX;
begin
    M.Create(Scale0, Scale1, Scale2, g_XMZero);
    lclr := XMVector3Transform(xyz, M);

    sel := XMVectorGreater(lclr, Cutoff);

    // clr := 12.92 * lclr for lclr <= 0.0031308f
    smallC := XMVectorMultiply(lclr, g_XMsrgbScale);

    // clr := (1+a)*pow(lclr, 1/2.4) - a for lclr > 0.0031308 (where a = 0.055)
    largeC := XMVectorSubtract(XMVectorMultiply(g_XMsrgbA1, XMVectorPow(lclr, Exp)), g_XMsrgbA);

    clr := XMVectorSelect(smallC, largeC, sel);

    Result := XMVectorSelect(xyz, clr, g_XMSelect1110);
end;



function XMColorSRGBToXYZ(srgb: TXMVECTOR): TXMVECTOR;
const
    Scale0: TXMVECTORF32 = (f: (0.4124, 0.2126, 0.0193, 0.0));
    Scale1: TXMVECTORF32 = (f: (0.3576, 0.7152, 0.1192, 0.0));
    Scale2: TXMVECTORF32 = (f: (0.1805, 0.0722, 0.9505, 0.0));
    Cutoff: TXMVECTORF32 = (f: (0.04045, 0.04045, 0.04045, 0.0));
    Exp: TXMVECTORF32 = (f: (2.4, 2.4, 2.4, 1.0));
var
    sel, smallC, largeC, lclr, clr: TXMVECTOR;
    M: TXMMATRIX;
begin
    sel := XMVectorGreater(srgb, Cutoff);
    // lclr = clr / 12.92
    smallC := XMVectorDivide(srgb, g_XMsrgbScale);
    // lclr = pow( (clr + a) / (1+a), 2.4 )
    largeC := XMVectorPow(XMVectorDivide(XMVectorAdd(srgb, g_XMsrgbA), g_XMsrgbA1), Exp);
    lclr := XMVectorSelect(smallC, largeC, sel);
    M.Create(Scale0, Scale1, Scale2, g_XMZero);
    clr := XMVector3Transform(lclr, M);
    Result := XMVectorSelect(srgb, clr, g_XMSelect1110);
end;



function XMColorRGBToSRGB(rgb: TXMVECTOR): TXMVECTOR;
const
    Cutoff: TXMVECTORF32 = (f: (0.0031308, 0.0031308, 0.0031308, 1.0));
    Linear: TXMVECTORF32 = (f: (12.92, 12.92, 12.92, 1.0));
    Scale: TXMVECTORF32 = (f: (1.055, 1.055, 1.055, 1.0));
    Bias: TXMVECTORF32 = (f: (0.055, 0.055, 0.055, 0.0));
    InvGamma: TXMVECTORF32 = (f: (1.0 / 2.4, 1.0 / 2.4, 1.0 / 2.4, 1.0));
var
    V, V0, V1, select: TXMVECTOR;
begin
    V := XMVectorSaturate(rgb);
    V0 := XMVectorMultiply(V, Linear);
    V1 := XMVectorSubtract(XMVectorMultiply(Scale, XMVectorPow(V, InvGamma)), Bias);
    select := XMVectorLess(V, Cutoff);
    V := XMVectorSelect(V1, V0, select);
    Result := XMVectorSelect(rgb, V, g_XMSelect1110);
end;



function XMColorSRGBToRGB(srgb: TXMVECTOR): TXMVECTOR;
const
    Cutoff: TXMVECTORF32 = (f: (0.04045, 0.040450, 0.040450, 1.0));
    ILinear: TXMVECTORF32 = (f: (1.0 / 12.920, 1.0 / 12.920, 1.0 / 12.920, 1.0));
    Scale: TXMVECTORF32 = (f: (1.0 / 1.0550, 1.0 / 1.0550, 1.0 / 1.0550, 1.0));
    Bias: TXMVECTORF32 = (f: (0.0550, 0.055, 0.0550, 0.0));
    Gamma: TXMVECTORF32 = (f: (2.40, 2.40, 2.40, 1.0));
var
    V, V0, V1, select: TXMVECTOR;
begin
    V := XMVectorSaturate(srgb);
    V0 := XMVectorMultiply(V, ILinear);
    V1 := XMVectorPow(XMVectorMultiply(XMVectorAdd(V, Bias), Scale), Gamma);
    select := XMVectorGreater(V, Cutoff);
    V := XMVectorSelect(V0, V1, select);
    Result := XMVectorSelect(srgb, V, g_XMSelect1110);
end;



function XMScalarNearEqual(S1: single; S2: single; Epsilon: single): boolean;
var
    Delta: single;
begin
    Delta := S1 - S2;
    Result := (abs(Delta) <= Epsilon);
end;


//------------------------------------------------------------------------------
// Modulo the range of the given angle such that -XM_PI <= Angle < XM_PI
function XMScalarModAngle(Angle: single): single;
var
    fTemp, fAngle: single;
begin
    // Note: The modulo is performed with unsigned math only to work
    // around a precision error on numbers that are close to PI

    // Normalize the range from 0.0f to XM_2PI
    fAngle := Angle + XM_PI;
    // Perform the modulo, unsigned
    fTemp := abs(fAngle);
    fTemp := fTemp - (XM_2PI * (fTemp / XM_2PI));
    // Restore the number to the range of -XM_PI to XM_PI-epsilon
    fTemp := fTemp - XM_PI;
    // If the modulo'd value was negative, restore negation
    if (fAngle < 0.0) then
        fTemp := -fTemp;

    Result := fTemp;
end;



function XMScalarSin(Value: single): single;
var
    quotient, y, y2: single;
begin
    // Map Value to y in [-pi,pi], x = 2*pi*quotient + remainder.
    quotient := XM_1DIV2PI * Value;
    if (Value >= 0.0) then
        quotient := (quotient + 0.5)
    else
        quotient := (quotient - 0.5);
    y := Value - XM_2PI * quotient;

    // Map y to [-pi/2,pi/2] with sin(y) = sin(Value).
    if (y > XM_PIDIV2) then
        y := XM_PI - y
    else if (y < -XM_PIDIV2) then
        y := -XM_PI - y;

    // 11-degree minimax approximation
    y2 := y * y;
    Result := (((((-2.3889859e-08 * y2 + 2.7525562e-06) * y2 - 0.00019840874) * y2 + 0.0083333310) * y2 - 0.16666667) * y2 + 1.0) * y;
end;



function XMScalarSinEst(Value: single): single;
var
    quotient, y, y2: single;
begin
    // Map Value to y in [-pi,pi], x = 2*pi*quotient + remainder.
    quotient := XM_1DIV2PI * Value;
    if (Value >= 0.0) then
        quotient := quotient + 0.5
    else
        quotient := quotient - 0.5;

    y := Value - XM_2PI * quotient;

    // Map y to [-pi/2,pi/2] with sin(y) = sin(Value).
    if (y > XM_PIDIV2) then
        y := XM_PI - y
    else if (y < -XM_PIDIV2) then
        y := -XM_PI - y;

    // 7-degree minimax approximation
    y2 := y * y;
    Result := (((-0.00018524670 * y2 + 0.0083139502) * y2 - 0.16665852) * y2 + 1.0) * y;
end;



function XMScalarCos(Value: single): single;
var
    quotient, y, y2, sign, p: single;
begin
    // Map Value to y in [-pi,pi], x = 2*pi*quotient + remainder.
    quotient := XM_1DIV2PI * Value;
    if (Value >= 0.0) then
        quotient := quotient + 0.5
    else
        quotient := quotient - 0.5;

    y := Value - XM_2PI * quotient;

    // Map y to [-pi/2,pi/2] with cos(y) = sign*cos(x).

    if (y > XM_PIDIV2) then
    begin
        y := XM_PI - y;
        sign := -1.0;
    end
    else if (y < -XM_PIDIV2) then
    begin
        y := -XM_PI - y;
        sign := -1.0;
    end
    else
    begin
        sign := +1.0;
    end;

    // 10-degree minimax approximation
    y2 := y * y;
    p := ((((-2.6051615e-07 * y2 + 2.4760495e-05) * y2 - 0.0013888378) * y2 + 0.041666638) * y2 - 0.5) * y2 + 1.0;
    Result := sign * p;
end;



function XMScalarCosEst(Value: single): single;
var
    quotient, y, y2, sign, p: single;
begin
    // Map Value to y in [-pi,pi], x = 2*pi*quotient + remainder.
    quotient := XM_1DIV2PI * Value;
    if (Value >= 0.0) then
        quotient := quotient + 0.5
    else
        quotient := quotient - 0.5;

    y := Value - XM_2PI * quotient;

    // Map y to [-pi/2,pi/2] with cos(y) = sign*cos(x).
    if (y > XM_PIDIV2) then
    begin
        y := XM_PI - y;
        sign := -1.0;
    end
    else if (y < -XM_PIDIV2) then
    begin
        y := -XM_PI - y;
        sign := -1.0;
    end
    else
    begin
        sign := +1.0;
    end;

    // 6-degree minimax approximation
    y2 := y * y;
    p := ((-0.0012712436 * y2 + 0.041493919) * y2 - 0.49992746) * y2 + 1.0;
    Result := sign * p;
end;



procedure XMScalarSinCos(out pSin: single; out pCos: single; Value: single);
var
    quotient, sign, y, y2, p: single;
begin
    // Map Value to y in [-pi,pi], x = 2*pi*quotient + remainder.
    quotient := XM_1DIV2PI * Value;
    if (Value >= 0.0) then
        quotient := (trunc(quotient) + 0.5)
    else
        quotient := (trunc(quotient) - 0.5);

    y := Value - XM_2PI * quotient;

    // Map y to [-pi/2,pi/2] with sin(y) = sin(Value).
    if (y > XM_PIDIV2) then
    begin
        y := XM_PI - y;
        sign := -1.0;
    end
    else if (y < -XM_PIDIV2) then
    begin
        y := -XM_PI - y;
        sign := -1.0;
    end
    else
    begin
        sign := +1.0;
    end;

    y2 := y * y;

    // 11-degree minimax approximation
    pSin := (((((-2.3889859e-08 * y2 + 2.7525562e-06) * y2 - 0.00019840874) * y2 + 0.0083333310) * y2 - 0.16666667) * y2 + 1.0) * y;

    // 10-degree minimax approximation
    p := ((((-2.6051615e-07 * y2 + 2.4760495e-05) * y2 - 0.0013888378) * y2 + 0.041666638) * y2 - 0.5) * y2 + 1.0;
    pCos := sign * p;
end;



procedure XMScalarSinCosEst(out pSin: single; out pCos: single; Value: single);
var
    quotient, y, sign, y2, p: single;
begin
    // Map Value to y in [-pi,pi], x = 2*pi*quotient + remainder.
    quotient := XM_1DIV2PI * Value;
    if (Value >= 0.0) then
        quotient := (quotient + 0.5)
    else
        quotient := (quotient - 0.5);

    y := Value - XM_2PI * quotient;

    // Map y to [-pi/2,pi/2] with sin(y) = sin(Value).
    if (y > XM_PIDIV2) then
    begin
        y := XM_PI - y;
        sign := -1.0;
    end
    else if (y < -XM_PIDIV2) then
    begin
        y := -XM_PI - y;
        sign := -1.0;
    end
    else
    begin
        sign := +1.0;
    end;

    y2 := y * y;

    // 7-degree minimax approximation
    pSin := (((-0.00018524670 * y2 + 0.0083139502) * y2 - 0.16665852) * y2 + 1.0) * y;

    // 6-degree minimax approximation
    p := ((-0.0012712436 * y2 + 0.041493919) * y2 - 0.49992746) * y2 + 1.0;
    pCos := sign * p;
end;



function XMScalarASin(Value: single): single;
var
    nonnegative: boolean;
    x, omx, root: single;
begin
    // Clamp input to [-1,1].
    nonnegative := (Value >= 0.0);
    x := abs(Value);
    omx := 1.0 - x;
    if (omx < 0.0) then
        omx := 0.0;
    root := sqrt(omx);

    // 7-degree minimax approximation
    Result := ((((((-0.0012624911 * x + 0.0066700901) * x - 0.0170881256) * x + 0.0308918810) * x - 0.0501743046) * x + 0.0889789874) *
        x - 0.2145988016) * x + 1.5707963050;
    Result := Result * root;  // acos(|x|)

    // acos(x) = pi - acos(-x) when x < 0, asin(x) = pi/2 - acos(x)
    if nonnegative then
        Result := XM_PIDIV2 - Result
    else
        Result := Result - XM_PIDIV2;
end;



function XMScalarASinEst(Value: single): single;
var
    nonnegative: boolean;
    x, omx, root: single;
begin
    // Clamp input to [-1,1].
    nonnegative := (Value >= 0.0);
    x := abs(Value);
    omx := 1.0 - x;
    if (omx < 0.0) then
        omx := 0.0;
    root := sqrt(omx);

    // 3-degree minimax approximation
    Result := ((-0.0187293 * x + 0.0742610) * x - 0.2121144) * x + 1.5707288;
    Result := Result * root;  // acos(|x|)

    // acos(x) = pi - acos(-x) when x < 0, asin(x) = pi/2 - acos(x)
    if nonnegative then
        Result := XM_PIDIV2 - Result
    else
        Result := Result - XM_PIDIV2;
end;



function XMScalarACos(Value: single): single;
var
    nonnegative: boolean;
    x, omx, root: single;
begin
    // Clamp input to [-1,1].
    nonnegative := (Value >= 0.0);
    x := abs(Value);
    omx := 1.0 - x;
    if (omx < 0.0) then
        omx := 0.0;
    root := sqrt(omx);

    // 7-degree minimax approximation
    Result := ((((((-0.0012624911 * x + 0.0066700901) * x - 0.0170881256) * x + 0.0308918810) * x - 0.0501743046) * x + 0.0889789874) *
        x - 0.2145988016) * x + 1.5707963050;
    Result := Result * root;
    // acos(x) = pi - acos(-x) when x < 0
    if not nonnegative then
        Result := XM_PI - Result;
end;




function XMScalarACosEst(Value: single): single;
var
    nonnegative: boolean;
    x, omx, root: single;
begin
    // Clamp input to [-1,1].
    nonnegative := (Value >= 0.0);
    x := abs(Value);
    omx := 1.0 - x;
    if (omx < 0.0) then
        omx := 0.0;
    root := sqrt(omx);

    // 3-degree minimax approximation
    Result := ((-0.0187293 * x + 0.0742610) * x - 0.2121144) * x + 1.5707288;
    Result := Result * root;

    // acos(x) = pi - acos(-x) when x < 0
    if not nonnegative then
        Result := XM_PI - Result;
end;


{$IFDEF FPC}
  {$IF DEFINED(_XM_NO_INTRINSICS_)}
  {$include DirectX.Math.XM_NO_INTRINSICS.inc}
  {$ELSEIF DEFINED(_XM_ARM_NEON_INTRINSICS)}
  {$include DirectX.Math.XM_ARM_NEON_INTRINSICs.inc}
  {$ELSE}// _XM_SSE_INTRINSICS_, etc
  {$include DirectX.Math.XM_SSE_INTRINSICS.inc}
  {$ENDIF}
{$ELSE}
    // Delphi Support only for non assembler ?
    {$include DirectX.Math.XM_NO_INTRINSICS.inc}
{$ENDIF}



end.
