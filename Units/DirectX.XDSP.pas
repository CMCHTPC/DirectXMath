unit DirectX.XDSP;
//--------------------------------------------------------------------------------------
// File: XDSP.h

// DirectXMath based Digital Signal Processing (DSP) functions for audio,
// primarily Fast Fourier Transform (FFT)

// All buffer parameters must be 16-byte aligned

// All FFT functions support only single-precision floating-point audio

// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
// PARTICULAR PURPOSE.

// Copyright (c) Microsoft Corporation. All rights reserved.

// http://go.microsoft.com/fwlink/?LinkID=615557
//--------------------------------------------------------------------------------------

{$mode delphi}{$H+}

interface

uses
    Windows, Classes, SysUtils, DirectX.Math;

implementation

function ISPOWEROF2(n: size_t): boolean; inline;
begin
    Result := ((n and (n - 1)) = 0) and (n <> 0);
end;

// Parallel multiplication of four complex numbers, assuming real and imaginary values are stored in separate vectors.
procedure vmulComplex(out rResult: TXMVECTOR; out iResult: TXMVECTOR; r1, i1, r2, i2: TXMVECTOR); inline; overload;
var
    vr1r2, vr1i2: TXMVECTOR;
begin
    // (r1, i1) * (r2, i2) := (r1r2 - i1i2, r1i2 + r2i1)
    vr1r2 := XMVectorMultiply(r1, r2);
    vr1i2 := XMVectorMultiply(r1, i2);
    rResult := XMVectorNegativeMultiplySubtract(i1, i2, vr1r2); // real: (r1*r2 - i1*i2)
    iResult := XMVectorMultiplyAdd(r2, i1, vr1i2); // imaginary: (r1*i2 + r2*i1)
end;

procedure vmulComplex(var r1, i1: TXMVECTOR; r2, i2: TXMVECTOR); inline; overload;
var
    vr1r2, vr1i2: TXMVECTOR;
begin
    // (r1, i1) * (r2, i2) := (r1r2 - i1i2, r1i2 + r2i1)
    vr1r2 := XMVectorMultiply(r1, r2);
    vr1i2 := XMVectorMultiply(r1, i2);
    r1 := XMVectorNegativeMultiplySubtract(i1, i2, vr1r2); // real: (r1*r2 - i1*i2)
    i1 := XMVectorMultiplyAdd(r2, i1, vr1i2); // imaginary: (r1*i2 + r2*i1)
end;


//----------------------------------------------------------------------------------
// Radix-4 decimation-in-time FFT butterfly.
// This version assumes that all four elements of the butterfly are
// adjacent in a single vector.

// Compute the product of the complex input vector and the
// 4-element DFT matrix:
//     | 1  1  1  1 |    | (r1X,i1X) |
//     | 1 -j -1  j |    | (r1Y,i1Y) |
//     | 1 -1  1 -1 |    | (r1Z,i1Z) |
//     | 1  j -1 -j |    | (r1W,i1W) |

// This matrix can be decomposed into two simpler ones to reduce the
// number of additions needed. The decomposed matrices look like this:
//     | 1  0  1  0 |    | 1  0  1  0 |
//     | 0  1  0 -j |    | 1  0 -1  0 |
//     | 1  0 -1  0 |    | 0  1  0  1 |
//     | 0  1  0  j |    | 0  1  0 -1 |

// Combine as follows:
//          | 1  0  1  0 |   | (r1X,i1X) |         | (r1X + r1Z, i1X + i1Z) |
// Temp   := | 1  0 -1  0 | * | (r1Y,i1Y) |       := | (r1X - r1Z, i1X - i1Z) |
//          | 0  1  0  1 |   | (r1Z,i1Z) |         | (r1Y + r1W, i1Y + i1W) |
//          | 0  1  0 -1 |   | (r1W,i1W) |         | (r1Y - r1W, i1Y - i1W) |

//          | 1  0  1  0 |   | (rTempX,iTempX) |   | (rTempX + rTempZ, iTempX + iTempZ) |
// Result := | 0  1  0 -j | * | (rTempY,iTempY) | := | (rTempY + iTempW, iTempY - rTempW) |
//          | 1  0 -1  0 |   | (rTempZ,iTempZ) |   | (rTempX - rTempZ, iTempX - iTempZ) |
//          | 0  1  0  j |   | (rTempW,iTempW) |   | (rTempY - iTempW, iTempY + rTempW) |
//----------------------------------------------------------------------------------
procedure ButterflyDIT4_1(var r1, i1: TXMVECTOR); inline;
const
    vDFT4SignBits1: TXMVECTORF32 = (f: (1.0, -1.0, 1.0, -1.0));
    vDFT4SignBits2: TXMVECTORF32 = (f: (1.0, 1.0, -1.0, -1.0));
    vDFT4SignBits3: TXMVECTORF32 = (f: (1.0, -1.0, -1.0, 1.0));

var
    r1L, r1H, i1L, i1H, rTemp, iTemp: TXMVECTOR;
    rZrWiZiW, rZiWrZiW, iZrWiZrW, rTempL, iTempL: TXMVECTOR;
begin
    // sign constants for radix-4 butterflies


    // calculating Temp
    // [r1X| r1X|r1Y| r1Y] + [r1Z|-r1Z|r1W|-r1W]
    // [i1X| i1X|i1Y| i1Y] + [i1Z|-i1Z|i1W|-i1W]
    r1L := XMVectorSwizzle(r1, 0, 0, 1, 1);
    r1H := XMVectorSwizzle(r1, 2, 2, 3, 3);
    i1L := XMVectorSwizzle(i1, 0, 0, 1, 1);
    i1H := XMVectorSwizzle(i1, 2, 2, 3, 3);

    rTemp := XMVectorMultiplyAdd(r1H, vDFT4SignBits1, r1L);
    iTemp := XMVectorMultiplyAdd(i1H, vDFT4SignBits1, i1L);

    // calculating Result
    rZrWiZiW := XMVectorPermute(rTemp, iTemp, 2, 3, 6, 7);  // [rTempZ|rTempW|iTempZ|iTempW]
    rZiWrZiW := XMVectorSwizzle(rZrWiZiW, 0, 3, 0, 3);     // [rTempZ|iTempW|rTempZ|iTempW]
    iZrWiZrW := XMVectorSwizzle(rZrWiZiW, 2, 1, 2, 1);     // [rTempZ|iTempW|rTempZ|iTempW]

    // [rTempX| rTempY| rTempX| rTempY] + [rTempZ| iTempW|-rTempZ|-iTempW]
    // [iTempX| iTempY| iTempX| iTempY] + // [iTempZ|-rTempW|-iTempZ| rTempW]
    rTempL := XMVectorSwizzle(rTemp, 0, 1, 0, 1);
    iTempL := XMVectorSwizzle(iTemp, 0, 1, 0, 1);

    r1 := XMVectorMultiplyAdd(rZiWrZiW, vDFT4SignBits2, rTempL);
    i1 := XMVectorMultiplyAdd(iZrWiZrW, vDFT4SignBits3, iTempL);
end;




//----------------------------------------------------------------------------------
// Radix-4 decimation-in-time FFT butterfly.
// This version assumes that elements of the butterfly are
// in different vectors, so that each vector in the input
// contains elements from four different butterflies.
// The four separate butterflies are processed in parallel.

// The calculations here are the same as the ones in the single-vector
// radix-4 DFT, but instead of being done on a single vector (X,Y,Z,W)
// they are done in parallel on sixteen independent complex values.
// There is no interdependence between the vector elements:
// | 1  0  1  0 |    | (rIn0,iIn0) |               | (rIn0 + rIn2, iIn0 + iIn2) |
// | 1  0 -1  0 | *  | (rIn1,iIn1) |  :=   Temp   := | (rIn0 - rIn2, iIn0 - iIn2) |
// | 0  1  0  1 |    | (rIn2,iIn2) |               | (rIn1 + rIn3, iIn1 + iIn3) |
// | 0  1  0 -1 |    | (rIn3,iIn3) |               | (rIn1 - rIn3, iIn1 - iIn3) |

//          | 1  0  1  0 |   | (rTemp0,iTemp0) |   | (rTemp0 + rTemp2, iTemp0 + iTemp2) |
// Result := | 0  1  0 -j | * | (rTemp1,iTemp1) | := | (rTemp1 + iTemp3, iTemp1 - rTemp3) |
//          | 1  0 -1  0 |   | (rTemp2,iTemp2) |   | (rTemp0 - rTemp2, iTemp0 - iTemp2) |
//          | 0  1  0  j |   | (rTemp3,iTemp3) |   | (rTemp1 - iTemp3, iTemp1 + rTemp3) |
//----------------------------------------------------------------------------------
procedure ButterflyDIT4_4(var r0, r1, r2, r3, i0, i1, i2, i3: TXMVECTOR; constref pUnityTableReal: PXMVECTOR; constref pUnityTableImaginary: PXMVECTOR;
    uStride: size_t; fLast: boolean); inline;

var
    rTemp0, iTemp0, rTemp2, iTemp2, rTemp1, iTemp1, rTemp3, iTemp3, rTemp4, iTemp4: TXMVECTOR;
    rTemp5, iTemp5, rTemp6, iTemp6, rTemp7, iTemp7: TXMVECTOR;
    lUnityTableReal: array of TXMVECTOR absolute pUnityTableReal;
    lUnityTableImaginary: array of TXMVECTOR absolute pUnityTableImaginary;
begin
    assert(pUnityTableReal <> nil);
    assert(pUnityTableImaginary <> nil);

    assert(ISPOWEROF2(uStride));

    // calculating Temp
    rTemp0 := XMVectorAdd(r0, r2);
    iTemp0 := XMVectorAdd(i0, i2);

    rTemp2 := XMVectorAdd(r1, r3);
    iTemp2 := XMVectorAdd(i1, i3);

    rTemp1 := XMVectorSubtract(r0, r2);
    iTemp1 := XMVectorSubtract(i0, i2);

    rTemp3 := XMVectorSubtract(r1, r3);
    iTemp3 := XMVectorSubtract(i1, i3);

    rTemp4 := XMVectorAdd(rTemp0, rTemp2);
    iTemp4 := XMVectorAdd(iTemp0, iTemp2);

    rTemp5 := XMVectorAdd(rTemp1, iTemp3);
    iTemp5 := XMVectorSubtract(iTemp1, rTemp3);

    rTemp6 := XMVectorSubtract(rTemp0, rTemp2);
    iTemp6 := XMVectorSubtract(iTemp0, iTemp2);

    rTemp7 := XMVectorSubtract(rTemp1, iTemp3);
    iTemp7 := XMVectorAdd(iTemp1, rTemp3);

    // calculating Result
    // vmulComplex(rTemp0, iTemp0, rTemp0, iTemp0, pUnityTableReal[0], pUnityTableImaginary[0]); // first one is always trivial
    vmulComplex(rTemp5, iTemp5, lUnityTableReal[uStride], lUnityTableImaginary[uStride]);
    vmulComplex(rTemp6, iTemp6, lUnityTableReal[uStride * 2], lUnityTableImaginary[uStride * 2]);
    vmulComplex(rTemp7, iTemp7, lUnityTableReal[uStride * 3], lUnityTableImaginary[uStride * 3]);

    if (fLast) then
    begin
        ButterflyDIT4_1(rTemp4, iTemp4);
        ButterflyDIT4_1(rTemp5, iTemp5);
        ButterflyDIT4_1(rTemp6, iTemp6);
        ButterflyDIT4_1(rTemp7, iTemp7);
    end;

    r0 := rTemp4;
    i0 := iTemp4;
    r1 := rTemp5;
    i1 := iTemp5;
    r2 := rTemp6;
    i2 := iTemp6;
    r3 := rTemp7;
    i3 := iTemp7;
end;

//==================================================================================
// F-U-N-C-T-I-O-N-S
//==================================================================================

//----------------------------------------------------------------------------------
// DESCRIPTION:
//  4-sample FFT.

// PARAMETERS:
//  pReal      - [inout] real components, must have at least uCount elements
//  pImaginary - [inout] imaginary components, must have at least uCount elements
//  uCount     - [in]    number of FFT iterations
//----------------------------------------------------------------------------------
procedure FFT4({var} pReal: PXMVECTOR; {var} pImaginary: PXMVECTOR; uCount: size_t = 1); inline;
var
    uIndex: size_t;
    lReal: array of TXMVECTOR absolute pReal;
    lImaginary: array of TXMVECTOR absolute pImaginary;
begin
    assert(pReal <> nil);
    assert(pImaginary <> nil);
    assert(ISPOWEROF2(uCount));

    for  uIndex := 0 to uCount - 1 do
    begin
        ButterflyDIT4_1(lReal[uIndex], lImaginary[uIndex]);
    end;
end;


//----------------------------------------------------------------------------------
// DESCRIPTION:
//  8-sample FFT.

// PARAMETERS:
//  pReal      - [inout] real components, must have at least uCount*2 elements
//  pImaginary - [inout] imaginary components, must have at least uCount*2 elements
//  uCount     - [in]    number of FFT iterations
//----------------------------------------------------------------------------------
procedure FFT8({var} pReal: PXMVECTOR; {var} pImaginary: PXMVECTOR; uCount: size_t = 1); inline;

const
    wr1: TXMVECTORF32 = (f: (1.0, 0.70710677, 0.0, -0.70710677));
    wi1: TXMVECTORF32 = (f: (0.0, -0.70710677, -1.0, -0.70710677));
    wr2: TXMVECTORF32 = (f: (-1.0, -0.70710677, 0.0, 0.70710677));
    wi2: TXMVECTORF32 = (f: (0.0, 0.70710677, 1.0, 0.70710677));
var
    oddsR, evensR, oddsI, evensI: TXMVECTOR;
    pR: array of TXMVECTOR absolute pReal;
    pI: array of TXMVECTOR absolute pImaginary;
    r, i: TXMVECTOR;
    uIndex: size_t;
    k, l: integer;
begin

    assert(pReal <> nil);
    assert(pImaginary <> nil);
    assert(ISPOWEROF2(uCount));



    for  uIndex := 0 to uCount - 1 do
    begin
        k := uIndex;
        l := k + 1;
        oddsR := XMVectorPermute(pR[k], pR[l], 1, 3, 5, 7);
        evensR := XMVectorPermute(pR[k], pR[l], 0, 2, 4, 6);
        oddsI := XMVectorPermute(pI[k], pI[l], 1, 3, 5, 7);
        evensI := XMVectorPermute(pI[k], pI[l], 0, 2, 4, 6);
        ButterflyDIT4_1(oddsR, oddsI);
        ButterflyDIT4_1(evensR, evensI);


        vmulComplex(r, i, oddsR, oddsI, wr1, wi1);
        pR[k] := XMVectorAdd(evensR, r);
        pI[k] := XMVectorAdd(evensI, i);

        vmulComplex(r, i, oddsR, oddsI, wr2, wi2);
        pR[l] := XMVectorAdd(evensR, r);
        pI[l] := XMVectorAdd(evensI, i);
    end;
end;

//----------------------------------------------------------------------------------
// DESCRIPTION:
//  16-sample FFT.

// PARAMETERS:
//  pReal      - [inout] real components, must have at least uCount*4 elements
//  pImaginary - [inout] imaginary components, must have at least uCount*4 elements
//  uCount     - [in]    number of FFT iterations
//----------------------------------------------------------------------------------
procedure FFT16({var} pReal: PXMVECTOR; {var} pImaginary: PXMVECTOR; uCount: size_t = 1); inline;

const
    aUnityTableReal: array [0..3] of TXMVECTORF32 =
        ((f: (1.0, 1.0, 1.0, 1.0)),
        (f: (1.0, 0.92387950, 0.70710677, 0.38268343)),
        (f: (1.0, 0.70710677, -4.3711388e-008, -0.70710677)),
        (f: (1.0, 0.38268343, -0.70710677, -0.92387950)));
    aUnityTableImaginary: array [0..3] of TXMVECTORF32 =
        ((f: (-0.0, -0.0, -0.0, -0.0)),
        (f: (-0.0, -0.38268343, -0.70710677, -0.92387950)),
        (f: (-0.0, -0.70710677, -1.0, -0.70710677)),
        (f: (-0.0, -0.92387950, -0.70710677, 0.38268343)));
var
    uIndex: size_t;
    lReal: array of TXMVECTOR absolute pReal;
    lImaginary: array of TXMVECTOR absolute pImaginary;
begin

    assert(pReal <> nil);
    assert(pImaginary <> nil);
    assert(ISPOWEROF2(uCount));

    for uIndex := 0 to uCount - 1 do
    begin
        ButterflyDIT4_4(lReal[uIndex * 4],
            lReal[uIndex * 4 + 1],
            lReal[uIndex * 4 + 2],
            lReal[uIndex * 4 + 3],
            lImaginary[uIndex * 4],
            lImaginary[uIndex * 4 + 1],
            lImaginary[uIndex * 4 + 2],
            lImaginary[uIndex * 4 + 3], @aUnityTableReal[0], @aUnityTableImaginary[0],
            1, True);
    end;
end;



//----------------------------------------------------------------------------------
// DESCRIPTION:
//  2^N-sample FFT.

// REMARKS:
//  For FFTs length 16 and below, call FFT16(), FFT8(), or FFT4().

// PARAMETERS:
//  pReal       - [inout] real components, must have at least (uLength*uCount)/4 elements
//  pImaginary  - [inout] imaginary components, must have at least (uLength*uCount)/4 elements
//  pUnityTable - [in]    unity table, must have at least uLength*uCount elements, see FFTInitializeUnityTable()
//  uLength     - [in]    FFT length in samples, must be a power of 2 > 16
//  uCount      - [in]    number of FFT iterations
//----------------------------------------------------------------------------------
procedure FFT({var} pReal: PXMVECTOR; {var} pImaginary: PXMVECTOR; pUnityTable: PXMVECTOR; uLength: size_t; uCount: size_t = 1); inline;

var
    uTotal, uTotal_vectors, uStage_vectors, uStage_vectors_mask, uStride, uStrideMask, uStride2, uStride3, uStrideInvMask: size_t;
    uIndex, n: size_t;
    pUnityTableReal: PXMVECTOR;
    pUnityTableImaginary: PXMVECTOR;
    lReal: array of TXMVECTOR absolute pReal;
    lImaginary: array of TXMVECTOR absolute pImaginary;
begin
    assert(pReal <> nil);
    assert(pImaginary <> nil);
    assert(pUnityTable <> nil);
    assert(uLength > 16);
    assert(ISPOWEROF2(uLength));
    assert(ISPOWEROF2(uCount));
    pUnityTableReal := @pUnityTable;
    pUnityTableImaginary := @pUnityTable + (uLength shr 2);

    uTotal := uCount * uLength;
    uTotal_vectors := uTotal shr 2;
    uStage_vectors := uLength shr 2;
    uStage_vectors_mask := uStage_vectors - 1;
    uStride := uLength shr 4; // stride between butterfly elements
    uStrideMask := uStride - 1;
    uStride2 := uStride * 2;
    uStride3 := uStride * 3;
    uStrideInvMask := not uStrideMask;

    for  uIndex := 0 to (uTotal_vectors shr 2) - 1 do
    begin
        n := ((uIndex and uStrideInvMask) shl 2) + (uIndex and uStrideMask);
        ButterflyDIT4_4(lReal[n],
            lReal[n + uStride],
            lReal[n + uStride2],
            lReal[n + uStride3],
            lImaginary[n],
            lImaginary[n + uStride],
            lImaginary[n + uStride2],
            lImaginary[n + uStride3], @pUnityTableReal + (n and uStage_vectors_mask), @pUnityTableImaginary + (n and uStage_vectors_mask),
            uStride, False);
    end;

    if (uLength > 16 * 4) then
    begin
        FFT(pReal, pImaginary, @pUnityTable + (uLength shr 1), uLength shr 2, uCount * 4);
    end
    else if (uLength = 16 * 4) then
    begin
        FFT16(pReal, pImaginary, uCount * 4);
    end
    else if (uLength = 8 * 4) then
    begin
        FFT8(pReal, pImaginary, uCount * 4);
    end
    else if (uLength = 4 * 4) then
    begin
        FFT4(pReal, pImaginary, uCount * 4);
    end;
end;

//----------------------------------------------------------------------------------
// DESCRIPTION:
//  Initializes unity roots lookup table used by FFT functions.
//  Once initialized, the table need not be initialized again unless a
//  different FFT length is desired.

// REMARKS:
//  The unity tables of FFT length 16 and below are hard coded into the
//  respective FFT functions and so need not be initialized.

// PARAMETERS:
//  pUnityTable - [out] unity table, receives unity roots lookup table, must have at least uLength elements
//  uLength     - [in]  FFT length in frames, must be a power of 2 > 16
//----------------------------------------------------------------------------------
procedure FFTInitializeUnityTable(out pUnityTable: PXMVECTOR {uLength}; uLength: size_t); inline;
var
    flStep: single;
    i, j, uIndex: size_t;
    pfUnityTable: array of single absolute pUnityTable;
begin
    assert(pUnityTable <> nil);
    assert(uLength > 16);

    assert(ISPOWEROF2(uLength));


    // initialize unity table for recursive FFT lengths: uLength, uLength/4, uLength/16... > 16
    repeat
        begin
            flStep := 6.283185307 / uLength; // 2PI / FFT length
            uLength := uLength shr 2;

            // pUnityTable[0 to uLength*4-1] contains real components for current FFT length
            // pUnityTable[uLength*4 to uLength*8-1] contains imaginary components for current FFT length
            for  i := 0 to 3 do
            begin
                for  j := 0 to uLength - 1 do
                begin
                    uIndex := (i * uLength) + j;
                    pfUnityTable[uIndex] := cos(i * j * flStep);  // real component
                    pfUnityTable[uIndex + uLength * 4] := -sin(i * j * flStep); // imaginary component
                end;
            end;
            pfUnityTable := @pfUnityTable + uLength * 8;
        end;
    until (uLength <= 16);
end;

//----------------------------------------------------------------------------------
// DESCRIPTION:
//  The FFT functions generate output in bit reversed order.
//  Use this function to re-arrange them into order of increasing frequency.

// REMARKS:

// PARAMETERS:
//  pOutput     - [out] output buffer, receives samples in order of increasing frequency, cannot overlap pInput, must have at least (1 SHL uLog2Length)/4 elements
//  pInput      - [in]  input buffer, samples in bit reversed order as generated by FFT functions, cannot overlap pOutput, must have at least (1 SHL uLog2Length)/4 elements
//  uLog2Length - [in]  LOG (base 2) of FFT length in samples, must be >= 2
//----------------------------------------------------------------------------------
procedure FFTUnswizzle({out} pOutput: PXMVECTOR; pInput: PXMVECTOR; uLog2Length: size_t); inline;
var
    uIndex, n: size_t;
    pfOutput: array of single absolute pOutput;
    pfInput: array of single absolute pInput;
    uLength: size_t;
begin
    assert(pInput <> nil);
    assert(uLog2Length >= 2);

    uLength := 1 shl uLog2Length;

    if ((uLog2Length and $1) = 0) then
    begin
        // even powers of two
        for  uIndex := 0 to uLength - 1 do
        begin
            n := uIndex;
            n := ((n and $cccccccc) shr 2) or ((n and $33333333) shl 2);
            n := ((n and $f0f0f0f0) shr 4) or ((n and $0f0f0f0f) shl 4);
            n := ((n and $ff00ff00) shr 8) or ((n and $00ff00ff) shl 8);
            n := ((n and $ffff0000) shr 16) or ((n and $0000ffff) shl 16);
            n := n shr (32 - uLog2Length);
            pfOutput[n] := pfInput[uIndex];
        end;
    end
    else
    begin
        // odd powers of two
        for uIndex := 0 to uLength - 1 do
        begin
            n := (uIndex shr 3);
            n := ((n and $cccccccc) shr 2) or ((n and $33333333) shl 2);
            n := ((n and $f0f0f0f0) shr 4) or ((n and $0f0f0f0f) shl 4);
            n := ((n and $ff00ff00) shr 8) or ((n and $00ff00ff) shl 8);
            n := ((n and $ffff0000) shr 16) or ((n and $0000ffff) shl 16);
            n := n shr (32 - (uLog2Length - 3));
            n := n or ((uIndex and $7) shl (uLog2Length - 3));
            pfOutput[n] := pfInput[uIndex];
        end;
    end;
end;

//----------------------------------------------------------------------------------
// DESCRIPTION:
//  Convert complex components to polar form.

// PARAMETERS:
//  pOutput         - [out] output buffer, receives samples in polar form, must have at least uLength/4 elements
//  pInputReal      - [in]  input buffer (real components), must have at least uLength/4 elements
//  pInputImaginary - [in]  input buffer (imaginary components), must have at least uLength/4 elements
//  uLength         - [in]  FFT length in samples, must be a power of 2 >= 4
//----------------------------------------------------------------------------------

procedure FFTPolar(out pOutput: PXMVECTOR; pInputReal: PXMVECTOR; pInputImaginary: PXMVECTOR; uLength: size_t); inline;
var
    flOneOverLength: single;
    vOneOverLength: TXMVECTOR;
    uIndex: size_t;
    vReal, vImaginary, vRR, vII, vRRplusII, vTotal: TXMVECTOR;
    lInputReal: array of TXMVECTOR absolute pInputReal;
    lInputImaginary: array of TXMVECTOR absolute pInputImaginary;
    lOutput: array of TXMVECTOR absolute pOutput;
begin

    assert(pInputReal <> nil);
    assert(pInputImaginary <> nil);
    assert(uLength >= 4);
    assert(ISPOWEROF2(uLength));

    flOneOverLength := 1.0 / uLength;

    // result := sqrtf((real/uLength)^2 + (imaginary/uLength)^2) * 2
    vOneOverLength := XMVectorReplicate(flOneOverLength);

    for uIndex := 0 to (uLength shr 2) - 1 do
    begin
        vReal := XMVectorMultiply(lInputReal[uIndex], vOneOverLength);
        vImaginary := XMVectorMultiply(lInputImaginary[uIndex], vOneOverLength);
        vRR := XMVectorMultiply(vReal, vReal);
        vII := XMVectorMultiply(vImaginary, vImaginary);
        vRRplusII := XMVectorAdd(vRR, vII);
        vTotal := XMVectorSqrt(vRRplusII);
        lOutput[uIndex] := XMVectorAdd(vTotal, vTotal);
    end;
end;

//----------------------------------------------------------------------------------
// DESCRIPTION:
//  Deinterleaves audio samples

// REMARKS:
//  For example, audio of the form [LRLRLR] becomes [LLLRRR].

// PARAMETERS:
//  pOutput       - [out] output buffer, receives samples in deinterleaved form, cannot overlap pInput, must have at least (uChannelCount*uFrameCount)/4 elements
//  pInput        - [in]  input buffer, cannot overlap pOutput, must have at least (uChannelCount*uFrameCount)/4 elements
//  uChannelCount - [in]  number of channels, must be > 1
//  uFrameCount   - [in]  number of frames of valid data, must be > 0
//----------------------------------------------------------------------------------
procedure Deinterleave({out}  pOutput: TXMVECTORArray; pInput: PXMVECTOR; uChannelCount: size_t; uFrameCount: size_t); inline;
var
    uChannel, uFrame: size_t;
    pfOutput: array of TXMVECTOR absolute pOutput;
    pfInput: array of TXMVECTOR absolute pInput;
begin

    assert(pInput <> nil);
    assert(uChannelCount > 1);
    assert(uFrameCount > 0);

    for  uChannel := 0 to uChannelCount - 1 do
    begin
        for  uFrame := 0 to uFrameCount - 1 do
        begin
            pfOutput[uChannel * uFrameCount + uFrame] := pfInput[uFrame * uChannelCount + uChannel];
        end;
    end;
end;

//----------------------------------------------------------------------------------
// DESCRIPTION:
//  Interleaves audio samples

// REMARKS:
//  For example, audio of the form [LLLRRR] becomes [LRLRLR].

// PARAMETERS:
//  pOutput       - [out] output buffer, receives samples in interleaved form, cannot overlap pInput, must have at least (uChannelCount*uFrameCount)/4 elements
//  pInput        - [in]  input buffer, cannot overlap pOutput, must have at least (uChannelCount*uFrameCount)/4 elements
//  uChannelCount - [in]  number of channels, must be > 1
//  uFrameCount   - [in]  number of frames of valid data, must be > 0
//----------------------------------------------------------------------------------
procedure Interleave(out pOutput: PXMVECTOR; pInput: PXMVECTOR; uChannelCount: size_t; uFrameCount: size_t); inline;
var
    uChannel, uFrame: size_t;
    pfOutput: array of TXMVECTOR absolute pOutput;
    pfInput: array of TXMVECTOR absolute pInput;
begin
    assert(pInput <> nil);
    assert(uChannelCount > 1);
    assert(uFrameCount > 0);

    for uChannel := 0 to uChannelCount - 1 do
    begin
        for uFrame := 0 to uFrameCount - 1 do
        begin
            pfOutput[uFrame * uChannelCount + uChannel] := pfInput[uChannel * uFrameCount + uFrame];
        end;
    end;
end;

//----------------------------------------------------------------------------------
// DESCRIPTION:
//  This function applies a 2^N-sample FFT and unswizzles the result such
//  that the samples are in order of increasing frequency.
//  Audio is first deinterleaved if multichannel.

// PARAMETERS:
//  pReal         - [inout] real components, must have at least (1 SHL uLog2Length*uChannelCount)/4 elements
//  pImaginary    - [out]   imaginary components, must have at least (1 SHL uLog2Length*uChannelCount)/4 elements
//  pUnityTable   - [in]    unity table, must have at least (1 SHL uLog2Length) elements, see FFTInitializeUnityTable()
//  uChannelCount - [in]    number of channels, must be within [1, 6]
//  uLog2Length   - [in]    LOG (base 2) of FFT length in frames, must within [2, 9]
//----------------------------------------------------------------------------------
procedure FFTInterleaved(var pReal: PXMVECTOR; out pImaginary: PXMVECTOR; pUnityTable: PXMVECTOR; uChannelCount: size_t; uLog2Length: size_t); inline;
var
    vRealTemp: array [0..767] of TXMVECTOR;
    vImaginaryTemp: array [0..767] of TXMVECTOR;
    uLength: size_t;
    uChannel: size_t;
    lReal: array of TXMVECTOR absolute pReal;
    lImaginary: array of TXMVECTOR absolute pImaginary;
begin
    assert(pReal <> nil);
    assert(pUnityTable <> nil);
    assert((uChannelCount > 0) and (uChannelCount <= 6));
    assert((uLog2Length >= 2) and (uLog2Length <= 9));


    uLength := 1 shl uLog2Length;

    if (uChannelCount > 1) then
    begin
        Deinterleave(@vRealTemp, @pReal, uChannelCount, uLength);
    end
    else
    begin
        move(pReal, vRealTemp, (uLength shr 2) * sizeof(TXMVECTOR));
        //            memcpy_s(vRealTemp, sizeof(vRealTemp), pReal, (uLength SHR 2)*sizeof(TXMVECTOR));
    end;

    ZeroMemory(@vImaginaryTemp, SizeOf(vImaginaryTemp));
    //memset( vImaginaryTemp, 0, (uChannelCount*(uLength SHR 2)) * sizeof(TXMVECTOR) );

    if (uLength > 16) then
    begin
        for  uChannel := 0 to uChannelCount - 1 do
        begin
            FFT(@vRealTemp[uChannel * (uLength shr 2)], @vImaginaryTemp[uChannel * (uLength shr 2)], pUnityTable, uLength);
        end;
    end
    else if (uLength = 16) then
    begin
        for  uChannel := 0 to uChannelCount - 1 do
        begin
            FFT16(@vRealTemp[uChannel * (uLength shr 2)], @vImaginaryTemp[uChannel * (uLength shr 2)]);
        end;
    end
    else if (uLength = 8) then
    begin
        for uChannel := 0 to uChannelCount - 1 do
        begin
            FFT8(@vRealTemp[uChannel * (uLength shr 2)], @vImaginaryTemp[uChannel * (uLength shr 2)]);
        end;
    end
    else if (uLength = 4) then
    begin
        for uChannel := 0 to uChannelCount - 1 do
        begin
            FFT4(@vRealTemp[uChannel * (uLength shr 2)], @vImaginaryTemp[uChannel * (uLength shr 2)]);
        end;
    end;

    for uChannel := 0 to uChannelCount - 1 do
    begin
        FFTUnswizzle(@lReal[uChannel * (uLength shr 2)], @vRealTemp[uChannel * (uLength shr 2)], uLog2Length);
        FFTUnswizzle(@lImaginary[uChannel * (uLength shr 2)], @vImaginaryTemp[uChannel * (uLength shr 2)], uLog2Length);
    end;
end;

//----------------------------------------------------------------------------------
// DESCRIPTION:
//  This function applies a 2^N-sample inverse FFT.
//  Audio is interleaved if multichannel.

// PARAMETERS:
//  pReal         - [inout] real components, must have at least (1 SHL uLog2Length*uChannelCount)/4 elements
//  pImaginary    - [in]    imaginary components, must have at least (1 SHL uLog2Length*uChannelCount)/4 elements
//  pUnityTable   - [in]    unity table, must have at least (1 SHL uLog2Length) elements, see FFTInitializeUnityTable()
//  uChannelCount - [in]    number of channels, must be > 0
//  uLog2Length   - [in]    LOG (base 2) of FFT length in frames, must within [2, 9]
//----------------------------------------------------------------------------------
procedure IFFTDeinterleaved(var pReal: PXMVECTOR; pImaginary: PXMVECTOR; pUnityTable: PXMVECTOR; uChannelCount: size_t; uLog2Length: size_t); inline;
var
    vRealTemp: array [0..767] of TXMVECTOR;
    vImaginaryTemp: array[0..767] of TXMVECTOR;
    uLength, u, uChannel: size_t;
    vRnp, vRnm: TXMVECTOR;
    lReal: array of TXMVECTOR absolute pReal;
    lImaginary: array of TXMVECTOR absolute pImaginary;
begin

    assert(pReal <> nil);
    assert(pImaginary <> nil);
    assert(pUnityTable <> nil);

    assert((uChannelCount > 0) and (uChannelCount <= 6));

    assert((uLog2Length >= 2) and (uLog2Length <= 9));




    uLength := 1 shl uLog2Length;

    vRnp := XMVectorReplicate(1.0 / uLength);
    vRnm := XMVectorReplicate(-1.0 / uLength);
    for  u := 0 to uChannelCount * (uLength shr 2) - 1 do
    begin
        vRealTemp[u] := XMVectorMultiply(lReal[u], vRnp);
        vImaginaryTemp[u] := XMVectorMultiply(lImaginary[u], vRnm);
    end;

    if (uLength > 16) then
    begin
        for uChannel := 0 to uChannelCount - 1 do
        begin
            FFT(@vRealTemp[uChannel * (uLength shr 2)], @vImaginaryTemp[uChannel * (uLength shr 2)], pUnityTable, uLength);
        end;
    end
    else if (uLength = 16) then
    begin
        for  uChannel := 0 to uChannelCount - 1 do
        begin
            FFT16(@vRealTemp[uChannel * (uLength shr 2)], @vImaginaryTemp[uChannel * (uLength shr 2)]);
        end;
    end
    else if (uLength = 8) then
    begin
        for  uChannel := 0 to uChannelCount - 1 do
        begin
            FFT8(@vRealTemp[uChannel * (uLength shr 2)], @vImaginaryTemp[uChannel * (uLength shr 2)]);
        end;
    end
    else if (uLength = 4) then
    begin
        for  uChannel := 0 to uChannelCount - 1 do
        begin
            FFT4(@vRealTemp[uChannel * (uLength shr 2)], @vImaginaryTemp[uChannel * (uLength shr 2)]);
        end;
    end;

    for uChannel := 0 to uChannelCount - 1 do
    begin
        FFTUnswizzle(@vImaginaryTemp[uChannel * (uLength shr 2)], @vRealTemp[uChannel * (uLength shr 2)], uLog2Length);
    end;

    if (uChannelCount > 1) then
    begin
        Interleave(pReal, vImaginaryTemp, uChannelCount, uLength);
    end
    else
    begin
        // memcpy_s(pReal, uLength * uChannelCount * sizeof(single), vImaginaryTemp, (uLength shr 2) * sizeof(TXMVECTOR));
        move(vImaginaryTemp, pReal, (uLength shr 2) * sizeof(TXMVECTOR));
    end;
end;

end.


