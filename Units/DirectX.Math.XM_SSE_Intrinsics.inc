{
// For formating only
unit Text;

interface

uses Classes;

implementation
}

{****************************************************************************
 *
 * Data conversion
 *
 ****************************************************************************}

{$IF DEFINED (_XM_AVX_INTRINSICS_)}
// Returns the converted vector, where each component has been divided by two raised to the DivExponent power.
function XMConvertVectorIntToFloat(constref VInt: TXMVECTOR; constref DivExponent: UINT32): TXMVECTOR; assembler;
const
    uScale: UINT32 = $3F800000; // 1;
asm
           // VInt located in register eax
           // DivExponent located in register edx
           // $result located in register ecx
           // Convert to floats
           VCVTDQ2PS XMM0, [VInt]
           // Convert DivExponent into 1.0f/(1<<DivExponent)
           VPBROADCASTD XMM1,[uScale]
           VPBROADCASTD XMM2,[DivExponent]
           PSLLD   XMM2, 23
           VPSUBB  XMM1, XMM1,XMM2
           // Splat the scalar value
           VMULPS  XMM0,XMM0, XMM1
           VMOVUPS   [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMConvertVectorIntToFloat(constref VInt: TXMVECTOR; constref DivExponent: UINT32): TXMVECTOR; assembler;
asm
           // VInt located in register eax
           // DivExponent located in register edx
           // $result located in register ecx

           // Convert to floats
           CVTDQ2PS XMM0, [VInt] //  vResult = _mm_cvtepi32_ps(_mm_castps_si128(VInt));
           // Convert DivExponent into 1.0f/(1<<DivExponent)
           // uScale = 0x3F800000U - (DivExponent << 23);
           MOV     EDX,[DivExponent]
           SHL     EDX, 23
           MOV     EAX, $3F800000
           SUB     EAX, EDX
           // Splat the scalar value
           MOVD    XMM1, EAX  // vScale = _mm_set1_epi32(uScale);
           SHUFPS  XMM1,XMM1 ,0
           MULPS   XMM0, XMM1 // vResult = _mm_mul_ps(vResult,_mm_castsi128_ps(vScale));
           MOVUPS  [result],XMM0 // return vResult;
end;
{$ENDIF}


{$IF DEFINED (_XM_AVX_INTRINSICS_)}
//Returns the converted vector, where each component has been multiplied by two raised to the MulExponent power.
function XMConvertVectorFloatToInt(constref VFloat: TXMVECTOR; constref MulExponent: UINT32): TXMVECTOR; assembler;
const
    uScale: UINT32 = 1;// $3F800000; // 1;
asm
           VPBROADCASTD XMM1,[uScale]
           VPBROADCASTD XMM2, [MulExponent]
           VPSLLVD   XMM1, XMM1, XMM2
           VCVTDQ2PS XMM1, XMM1
           VMULPS  XMM0, XMM1, [VFloat]
           // In case of positive overflow, detect it
           {$IFDEF CPU32}
           VCMPPS  XMM1, XMM0, [g_XMMaxInt] , 6 // overflow in xmm1
           VCVTTPS2DQ XMM3, XMM0 // Float to int conversion
           // If there was positive overflow, set to 0x7FFFFFFF
           VANDPS  XMM0, XMM1, [g_XMAbsMask]
           {$ELSE}
           VCMPPS  XMM1, XMM0, [RIP+g_XMMaxInt] , 6 // overflow in xmm1
           VCVTTPS2DQ XMM3, XMM0 // Float to int conversion
           // If there was positive overflow, set to 0x7FFFFFFF
           VANDPS  XMM0, XMM1, [RIP+g_XMAbsMask]
           {$ENDIF}
           VANDNPS XMM1, XMM1, XMM3
           VORPS   XMM0, XMM1,XMM0
           VMOVUPS   [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMConvertVectorFloatToInt(constref VFloat: TXMVECTOR; constref MulExponent: UINT32): TXMVECTOR; nostackframe;register; assembler;
asm

           //  VFloat located in register eax
           //  MulExponent located in register edx
           //  $result located in register ecx
           // vResult = _mm_set_ps1((float)(1U << MulExponent));
           MOVUPS  XMM1, [VFloat]
           MOV     EAX, ECX
           MOV     ECX,  [MulExponent]
           MOV     EDX, $1
           SHL     EDX, CL
           CVTSI2SS XMM0, EDX
           MOV     ECX, EAX

           SHUFPS  XMM0, XMM0, 0 // XMM0 = vResult
           MULPS   XMM0, XMM1  // vResult = _mm_mul_ps(vResult,VFloat);
           // In case of positive overflow, detect it
           MOVUPS  XMM2, XMM0
           {$IFDEF CPU32}
           CMPNLEPS XMM2, [g_XMMaxInt]
           {$ELSE}
           CMPNLEPS XMM2, [RIP+g_XMMaxInt]    //  vOverflow = _mm_cmpgt_ps(vResult,g_XMMaxInt);
           {$ENDIF}
           // Float to int conversion
           CVTTPS2DQ XMM1, XMM0 //  vResulti = _mm_cvttps_epi32(vResult);
           // If there was positive overflow, set to 0x7FFFFFFF
           MOVUPS  XMM0, XMM2
           {$IFDEF CPU32}
           ANDPS   XMM0, [g_XMAbsMask]
           {$ELSE}
           ANDPS   XMM0, [RIP+g_XMAbsMask] // vResult = _mm_and_ps(vOverflow,g_XMAbsMask);
           {$ENDIF}
           ANDNPS  XMM2, XMM1 // vOverflow = _mm_andnot_ps(vOverflow,_mm_castsi128_ps(vResulti));
           ORPS    XMM0, XMM2  // vOverflow = _mm_or_ps(vOverflow,vResult);
           MOVUPS  [result], XMM0// return vOverflow;
end;
{$ENDIF}

{$IF DEFINED (_XM_AVX_INTRINSICS_)}
function XMConvertVectorUIntToFloat(constref VUInt: TXMVECTOR; constref DivExponent: UINT32): TXMVECTOR; assembler;
const
    uScale: UINT32 = $3F800000; // 1
asm
           // For the values that are higher than 0x7FFFFFFF, a fixup is needed
           // Determine which ones need the fix.
           VMOVUPS XMM0,[VUInt]
           {$IFDEF CPU32}
           VANDPS  XMM1, XMM0, [g_XMNegativeZero] // vMask = _mm_and_ps(VUInt,g_XMNegativeZero);
           {$ELSE}
           VANDPS  XMM1, XMM0, [RIP+g_XMNegativeZero] // vMask = _mm_and_ps(VUInt,g_XMNegativeZero);
           {$ENDIF}

           // Force all values positive
           VXORPS  XMM3, XMM0, XMM1 // vResult = _mm_xor_ps(VUInt,vMask);
           // Convert to floats
           VCVTDQ2PS XMM3, XMM3  // vResult = _mm_cvtepi32_ps(_mm_castps_si128(vResult));
           // Convert 0x80000000 -> 0xFFFFFFFF
           VPSRAD  XMM1,XMM1, 31  // iMask = _mm_srai_epi32(_mm_castps_si128(vMask),31);
           // For only the ones that are too big, add the fixup
           {$IFDEF CPU32}
           VANDPS  XMM1, XMM1, [g_XMFixUnsigned] // vMask = _mm_and_ps(_mm_castsi128_ps(iMask),g_XMFixUnsigned);
           {$ELSE}
           VANDPS  XMM1, XMM1, [RIP+g_XMFixUnsigned] // vMask = _mm_and_ps(_mm_castsi128_ps(iMask),g_XMFixUnsigned);
           {$ENDIF}
           VADDPS  XMM3, XMM3, XMM1  // vResult = _mm_add_ps(vResult,vMask);
           // Convert DivExponent into 1.0f/(1<<DivExponent)
           // uScale = 0x3F800000U - (DivExponent << 23);
           VPBROADCASTD XMM5,[uScale]
           VPBROADCASTD XMM6,[DivExponent]
           PSLLD   XMM6, 23
           VPSUBB  XMM5, XMM5,XMM6 //iMask = _mm_set1_epi32(uScale);
           // Splat
           VMULPS  XMM0,XMM3, XMM5  //vResult = _mm_mul_ps(vResult,_mm_castsi128_ps(iMask));
           VMOVUPS   [Result], XMM0 //return vResult;
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMConvertVectorUIntToFloat(constref VUInt: TXMVECTOR; constref DivExponent: UINT32): TXMVECTOR; assembler;
var
    temp: uint32;
asm
           // VUInt located in register eax
           // DivExponent located in register edx
           // $result located in register ecx
           // temp located at ebp-4, size=OS_32

           // For the values that are higher than 0x7FFFFFFF, a fixup is needed
           // Determine which ones need the fix.
           // vMask = _mm_and_ps(VUInt,g_XMNegativeZero);
           MOVUPS  XMM0, [VUInt]
           MOVUPS  XMM1, XMM0
           {$IFDEF CPU32}
           ANDPS   XMM0, [g_XMNegativeZero]
           {$ELSE}
           ANDPS   XMM0, [RIP+g_XMNegativeZero]
           {$ENDIF}

           // Force all values positive
           XORPS   XMM1, XMM0 // vResult = _mm_xor_ps(VUInt,vMask);
           // Convert to floats
           CVTDQ2PS XMM1, XMM1 //vResult = _mm_cvtepi32_ps(_mm_castps_si128(vResult));
           // Convert 0x80000000 -> 0xFFFFFFFF
           PSRAD   XMM0, 31 // iMask = _mm_srai_epi32(_mm_castps_si128(vMask),31);
           // For only the ones that are too big, add the fixup
           {$IFDEF CPU32}
           ANDPS   XMM0,[g_XMFixUnsigned] // vMask = _mm_and_ps(_mm_castsi128_ps(iMask),g_XMFixUnsigned);
           {$ELSE}
           ANDPS   XMM0,[RIP+g_XMFixUnsigned] // vMask = _mm_and_ps(_mm_castsi128_ps(iMask),g_XMFixUnsigned);
           {$ENDIF}
           ADDPS   XMM1, XMM0 //vResult = _mm_add_ps(vResult,vMask);
           // Convert DivExponent into 1.0f/(1<<DivExponent)
           // uint32_t uScale = 0x3F800000U - (DivExponent << 23);
           MOV     EDX, [DivExponent]
           SHL     EDX, 23
           MOV     EAX, $3F800000
           SUB     EAX, EDX
           // Splat
           MOVD    XMM0, EAX
           SHUFPS  XMM0, XMM0, 0  //iMask = _mm_set1_epi32(uScale);
           MULPS   XMM0, XMM1  //vResult = _mm_mul_ps(vResult,_mm_castsi128_ps(iMask));
           MOVUPS  [result],XMM0 //return vResult;
end;
{$ENDIF}


{$IF DEFINED (_XM_AVX_INTRINSICS_)}
function XMConvertVectorFloatToUInt(constref VFloat: TXMVECTOR; constref MulExponent: UINT32): TXMVECTOR; assembler;
const
    uScale: UINT32 = 1;// $3F800000; // 1;
asm
           VPBROADCASTD XMM1,[uScale]
           VPBROADCASTD XMM2, [MulExponent]
           VPSLLVD   XMM1, XMM1, XMM2
           VCVTDQ2PS XMM1, XMM1
           VMULPS  XMM0, XMM1, [VFloat]
           // In case of positive overflow, detect it
           {$IFDEF CPU32}
           VCMPPS  XMM1, XMM0, [g_XMMaxInt] , 6 // overflow in xmm1
           {$ELSE}
           VCMPPS  XMM1, XMM0, [RIP+g_XMMaxInt] , 6 // overflow in xmm1
           {$ENDIF}
           // Float to int conversion
           VCVTTPS2DQ XMM3, XMM0
           // If there was positive overflow, set to 0x7FFFFFFF
           {$IFDEF CPU32}
           VANDPS  XMM0, XMM1, [g_XMAbsMask]
           {$ELSE}
           VANDPS  XMM0, XMM1, [RIP+g_XMAbsMask]
           {$ENDIF}
           VANDNPS XMM1, XMM1, XMM3
           VORPS   XMM0, XMM1,XMM0
           VMOVUPS   [Result], XMM0
end;
{$ELSE}


function XMConvertVectorFloatToUInt(constref VFloat: TXMVECTOR; constref MulExponent: UINT32): TXMVECTOR; assembler;
asm
           // VFloat located in register eax
           // MulExponent located in register edx
           // $result located in register ecx

           MOVUPS  XMM1, [VFloat]
           // vResult = _mm_set_ps1(static_cast<float>(1U << MulExponent));
           MOV     EAX,ECX
           MOV     ECX,  [MulExponent]
           MOV     EDX, $1

           SHL     EDX, CL
           CVTSI2SS XMM0, EDX
           MOV     ECX, EAX

           SHUFPS  XMM0, XMM0, 0
           MULPS   XMM0, XMM1 // vResult = _mm_mul_ps(vResult,VFloat);

           // Clamp to >=0
           {$IFDEF CPU32}
           MAXPS   XMM0, [g_XMZero] // vResult = _mm_max_ps(vResult,g_XMZero);
           {$ELSE}
           MAXPS   XMM0, [RIP+g_XMZero] // vResult = _mm_max_ps(vResult,g_XMZero);
           {$ENDIF}
           // Any numbers that are too big, set to 0xFFFFFFFFU
           // vOverflow = _mm_cmpgt_ps(vResult,g_XMMaxUInt);
           MOVUPS  XMM2, XMM0
           {$IFDEF CPU32}
           CMPNLEPS   XMM2, [g_XMMaxUInt] // XMM2 = vOverflow
           {$ELSE}
           CMPNLEPS   XMM2, [RIP+g_XMMaxUInt] // XMM2 = vOverflow
           {$ENDIF}
           // vValue = g_XMUnsignedFix;
           {$IFDEF CPU32}
           MOVUPS  XMM3, [g_XMUnsignedFix]
           {$ELSE}
           MOVUPS  XMM3, [RIP+g_XMUnsignedFix]
           {$ENDIF}
           // Too large for a signed integer?
           // vMask = _mm_cmpge_ps(vResult,vValue);
           MOVUPS  XMM4, XMM0
           CMPNLTPS   XMM4, XMM3
           // Zero for number's lower than 0x80000000, 32768.0f*65536.0f otherwise
           ANDPS   XMM3, XMM4 // vValue = _mm_and_ps(vValue,vMask);
           // Perform fixup only on numbers too large (Keeps low bit precision)
           SUBPS   XMM0, XMM3 // vResult = _mm_sub_ps(vResult,vValue);
           CVTTPS2DQ XMM0, XMM0 // vResulti = _mm_cvttps_epi32(vResult);
           // Convert from signed to unsigned pnly if greater than 0x80000000
           {$IFDEF CPU32}
           ANDPS   XMM4, [g_XMNegativeZero] // vMask = _mm_and_ps(vMask,g_XMNegativeZero);
           {$ELSE}
           ANDPS   XMM4, [RIP+g_XMNegativeZero] // vMask = _mm_and_ps(vMask,g_XMNegativeZero);
           {$ENDIF}
           XORPS   XMM0, XMM4 // vResult = _mm_xor_ps(_mm_castsi128_ps(vResulti),vMask);
           // On those that are too large, set to 0xFFFFFFFF
           ORPS    XMM0, XMM2 // vResult = _mm_or_ps(vResult,vOverflow);
           MOVUPS  [result], XMM0 // return vResult;
end;

{$ENDIF}

function XMVectorSetBinaryConstant(const C0, C1, C2, c3: UINT32): TXMVECTOR; assembler;
const
    g_vMask1: TXMVECTOR = (u32: (1, 1, 1, 1));
asm
           // Move the parms to a vector
           // __m128i vTemp = _mm_set_epi32(C3,C2,C1,C0);
           MOVD        XMM0, [C3]
           MOVD        XMM1, C2
           MOVD        XMM2, C1
           MOVD        XMM3, C0
           PUNPCKLDQ   XMM3,XMM1
           PUNPCKLDQ   XMM2,XMM0
           PUNPCKLDQ   XMM3,XMM2
           // Mask off the low bits
           {$IFDEF CPU32}
           PAND    XMM3, [g_vMask1] // vTemp = _mm_and_si128(vTemp,g_vMask1);
           // 0xFFFFFFFF on true bits
           PCMPEQD XMM3, [g_vMask1] // vTemp = _mm_cmpeq_epi32(vTemp,g_vMask1);
           // 0xFFFFFFFF -> 1.0f, 0x00000000 -> 0.0f
           PAND    XMM3, [g_XMOne] // vTemp = _mm_and_si128(vTemp,g_XMOne);
           {$ELSE}
           PAND    XMM3, [RIP+g_vMask1] // vTemp = _mm_and_si128(vTemp,g_vMask1);
           // 0xFFFFFFFF on true bits
           PCMPEQD XMM3, [RIP+g_vMask1] // vTemp = _mm_cmpeq_epi32(vTemp,g_vMask1);
           // 0xFFFFFFFF -> 1.0f, 0x00000000 -> 0.0f
           PAND    XMM3, [RIP+g_XMOne] // vTemp = _mm_and_si128(vTemp,g_XMOne);
           {$ENDIF}
           MOV     EAX, [result]
           MOVUPS  [EAX], XMM3 // return _mm_castsi128_ps(vTemp);
end;



function XMVectorSplatConstant(const IntConstant: INT32; const DivExponent: UINT32): TXMVECTOR; assembler;
asm
           // Var IntConstant located in register eax
           // Var DivExponent located in register edx
           // Var $result located in register ecx
           // Splat the int
           MOVD    XMM1, IntConstant // vScale = _mm_set1_epi32(IntConstant);
           SHUFPS  XMM1, XMM1 ,0
           // Convert to a float
           CVTDQ2PS XMM1, XMM1 // vResult = _mm_cvtepi32_ps(vScale);
           // Convert DivExponent into 1.0f/(1<<DivExponent)
           // uScale = 0x3F800000U - (DivExponent << 23);
           SHL     EDX, 23
           MOV     EAX,  $3F800000
           SUB     EAX, EDX
           // Splat the scalar value (It's really a float)
           MOVD    XMM0 ,EAX
           SHUFPS  XMM0, XMM0 ,0 // vScale = _mm_set1_epi32(uScale);
           // Multiply by the reciprocal (Perform a right shift by DivExponent)
           MULPS   XMM0, XMM1 // vResult = _mm_mul_ps(vResult,_mm_castsi128_ps(vScale));
           MOVUPS  [result], XMM0 // return vResult;
end;



function XMVectorSplatConstantInt(const IntConstant: INT32): TXMVECTOR; nostackframe; assembler;
asm
           CVTSI2SS XMM0, IntConstant // V = _mm_set1_epi32( IntConstant );
           SHUFPS  XMM0,XMM0 ,0
           MOVUPS  [result], XMM0 // return _mm_castsi128_ps(V);
end;


{****************************************************************************
 *
 * Vector and matrix load operations
 *
 ****************************************************************************}

function XMLoadInt(constref pSource: PUINT32): TXMVECTOR; assembler;
asm
           //   return _mm_load_ss( reinterpret_cast<const float*>(pSource) );
           MOVSS   XMM0, [pSource];
           MOVUPS  [result],XMM0
end;



function XMLoadFloat(constref pSource: PSingle): TXMVECTOR; assembler;
asm
           //   return _mm_load_ss( pSource );
           MOVSS   XMM0, [pSource];
           MOVUPS  [result],XMM0
end;



function XMLoadInt2(const pSource: PUINT32): TXMVECTOR; assembler;
asm
           MOVSS   XMM0, [pSource]; // x = _mm_load_ss( reinterpret_cast<const float*>(pSource) );
           MOVSS   XMM1, [pSource+1]; // y = _mm_load_ss( reinterpret_cast<const float*>(pSource+1) );
           UNPCKLPS XMM0, XMM1    // return _mm_unpacklo_ps( x, y );
           MOVUPS  [result],XMM0
end;



function XMLoadInt2A(const PSource: PUINT32): TXMVECTOR; assembler;
asm
           MOVQ    XMM0, [PSource]  // V = _mm_loadl_epi64( reinterpret_cast<const __m128i*>(pSource) );
           MOVAPS  [result],XMM0 // return _mm_castsi128_ps(V);
end;



function XMLoadFloat2(const pSource: TXMFLOAT2): TXMVECTOR; assembler;
asm
           MOVSS   XMM0, [pSource]; //  x = _mm_load_ss( &pSource->x );
           MOVSS   XMM1, [pSource+1]; //  y = _mm_load_ss( &pSource->y );
           UNPCKLPS XMM0, XMM1 // return _mm_unpacklo_ps( x, y );
           MOVUPS  [result],XMM0
end;



function XMLoadFloat2A(const pSource: TXMFLOAT2A): TXMVECTOR; assembler;
asm
           MOVQ    XMM0, [pSource] // V = _mm_loadl_epi64( reinterpret_cast<const __m128i*>(pSource) );
           MOVUPS  [result],XMM0  // return _mm_castsi128_ps(V);
end;



function XMLoadSInt2(const pSource: TXMINT2): TXMVECTOR; assembler;
asm
           MOVSS   XMM0, TXMINT2([pSource]).x; // x = _mm_load_ss( reinterpret_cast<const float*>(&pSource->x) );
           MOVSS   XMM1, TXMINT2([pSource]).Y; // y = _mm_load_ss( reinterpret_cast<const float*>(&pSource->y) );
           UNPCKLPS XMM0, XMM1 // V = _mm_unpacklo_ps( x, y );
           CVTDQ2PS XMM0, XMM0 // return _mm_cvtepi32_ps(_mm_castps_si128(V));
           MOVUPS  [result],XMM0
end;



function XMLoadUInt2(const pSource: TXMUINT2): TXMVECTOR; assembler;
asm
           MOVSS   XMM0, TXMUINT2([pSource]).X; // x = _mm_load_ss( reinterpret_cast<const float*>(&pSource->x) );
           MOVSS   XMM1, TXMUINT2([pSource]).y; // y = _mm_load_ss( reinterpret_cast<const float*>(&pSource->y) );
           UNPCKLPS XMM0, XMM1 // V = _mm_unpacklo_ps( x, y );
           // For the values that are higher than $7FFFFFFF, a fixup is needed
           // Determine which ones need the fix.
           MOVUPS  XMM2, XMM0
           {$IFDEF CPU32}
           ANDPS   XMM2,[g_XMNegativeZero] // vMask = _mm_and_ps(V,g_XMNegativeZero);
           {$ELSE}
           ANDPS   XMM2,[RIP+g_XMNegativeZero] // vMask = _mm_and_ps(V,g_XMNegativeZero);
           {$ENDIF}
           // Force all values positive
           MOVUPS  XMM3, XMM0
           XORPS   XMM3,XMM2 // vResult = _mm_xor_ps(V,vMask);
           // Convert to floats
           CVTDQ2PS XMM3, XMM3 // vResult = _mm_cvtepi32_ps(_mm_castps_si128(vResult));
           // Convert $80000000 -> $FFFFFFFF
           MOVUPS  XMM4, XMM2  // iMask = _mm_srai_epi32(_mm_castps_si128(vMask),31);
           PSRAD   XMM4, 31
           // For only the ones that are too big, add the fixup
           {$IFDEF CPU32}
           ANDPS   XMM4,[g_XMFixUnsigned] // vMask = _mm_and_ps(_mm_castsi128_ps(iMask),g_XMFixUnsigned);
           {$ELSE}
           ANDPS   XMM4,[RIP+g_XMFixUnsigned] // vMask = _mm_and_ps(_mm_castsi128_ps(iMask),g_XMFixUnsigned);
           {$ENDIF}
           ADDPS   XMM3, XMM4 // vResult = _mm_add_ps(vResult,vMask);
           MOVUPS  [result],XMM3// return vResult;
end;



function XMLoadInt3(const pSource: PUINT32): TXMVECTOR; assembler;
asm
           MOVSS   XMM0, UINT32([pSource]); // x = _mm_load_ss( reinterpret_cast<const float*>(pSource) );
           MOVSS   XMM1, UINT32([pSource+1]); // y = _mm_load_ss( reinterpret_cast<const float*>(pSource+1) );
           MOVSS   XMM2, UINT32([pSource+2]); // z = _mm_load_ss( reinterpret_cast<const float*>(pSource+2) );
           UNPCKLPS XMM0, XMM1 // xy = _mm_unpacklo_ps( x, y );
           MOVLHPS XMM0, XMM2 // return _mm_movelh_ps( xy, z );
           MOVUPS  [result],XMM0
end;



function XMLoadInt3A(const pSource: PUINT32): TXMVECTOR; assembler;
asm
           // Reads an extra integer which is zero'd
           MOVDQA  XMM0, [pSource]   // V = _mm_load_si128( reinterpret_cast<const __m128i*>(pSource) );
           {$IFDEF CPU32}
           PAND    XMM0, [g_XMMask3] // V = _mm_and_si128( V, g_XMMask3 );
           {$ELSE}
           PAND    XMM0, [RIP+g_XMMask3] // V = _mm_and_si128( V, g_XMMask3 );
           {$ENDIF}
           MOVAPS  [result],XMM0 // return _mm_castsi128_ps(V);
end;



function XMLoadFloat3(constref pSource: TXMFLOAT3): TXMVECTOR; inline; assembler;
asm
           MOVSS   XMM0, TXMFLOAT3([pSource]).X; // x = _mm_load_ss( &pSource->x );
           MOVSS   XMM1, TXMFLOAT3([pSource]).y; // y = _mm_load_ss( &pSource->y );
           MOVSS   XMM2, TXMFLOAT3([pSource]).z; // z = _mm_load_ss( &pSource->z );
           UNPCKLPS XMM0, XMM1 // xy = _mm_unpacklo_ps( x, y );
           MOVLHPS XMM0, XMM2  // return _mm_movelh_ps( xy, z );
           MOVUPS  [result],XMM0
end;



function XMLoadFloat3(constref pSource: pSingle): TXMVECTOR; assembler;
asm
           MOVUPS  XMM0, [pSource]
           MOVUPS  [result],XMM0
end;



function XMLoadFloat3A(const pSource: TXMFLOAT3A): TXMVECTOR; assembler;
asm
           // Reads an extra float which is zero'd
           MOVAPS  XMM0, TXMFLOAT3A([pSource]).x // V = _mm_load_ps( &pSource->x );
           {$IFDEF CPU32}
           ANDPS   XMM0,[g_XMMask3] // return _mm_and_ps( V, g_XMMask3 );
           {$ELSE}
           ANDPS   XMM0,[RIP+g_XMMask3] // return _mm_and_ps( V, g_XMMask3 );
           {$ENDIF}
           MOVAPS  [result],XMM0
end;



function XMLoadSInt3(const pSource: TXMINT3): TXMVECTOR; assembler;
asm
           MOVSS   XMM0, TXMINT3([pSource]).x // x = _mm_load_ss( reinterpret_cast<const float*>(&pSource->x) );
           MOVSS   XMM1, TXMINT3([pSource]).y // y = _mm_load_ss( reinterpret_cast<const float*>(&pSource->y) );
           MOVSS   XMM2, TXMINT3([pSource]).y // z = _mm_load_ss( reinterpret_cast<const float*>(&pSource->z) );
           UNPCKLPS XMM0, XMM1 // xy = _mm_unpacklo_ps( x, y );
           MOVLHPS XMM0, XMM2 // V = _mm_movelh_ps( xy, z );
           CVTDQ2PS XMM0, XMM0  // return _mm_cvtepi32_ps(_mm_castps_si128(V));
           MOVUPS  [result],XMM0
end;



function XMLoadUInt3(const pSource: TXMUINT3): TXMVECTOR; assembler;
asm
           MOVSS   XMM0, TXMUINT3([pSource]).x // x = _mm_load_ss( reinterpret_cast<const float*>(&pSource->x) );
           MOVSS   XMM1, TXMUINT3([pSource]).y // y = _mm_load_ss( reinterpret_cast<const float*>(&pSource->y) );
           MOVSS   XMM2, TXMUINT3([pSource]).z // z = _mm_load_ss( reinterpret_cast<const float*>(&pSource->z) );
           UNPCKLPS XMM0, XMM1 // xy = _mm_unpacklo_ps( x, y );
           MOVLHPS XMM0, XMM2 // V = _mm_movelh_ps( xy, z );
           // For the values that are higher than 0x7FFFFFFF, a fixup is needed
           // Determine which ones need the fix.
           // vMask = _mm_and_ps(V,g_XMNegativeZero);
           MOVUPS  XMM3,XMM0
           {$IFDEF CPU32}
           ANDPS   XMM3, [g_XMNegativeZero]
           {$ELSE}
           ANDPS   XMM3, [RIP+g_XMNegativeZero]
           {$ENDIF}
           // Force all values positive
           XORPS   XMM0, XMM3 // vResult = _mm_xor_ps(V,vMask);
           // Convert to floats
           CVTDQ2PS XMM0, XMM0 // vResult = _mm_cvtepi32_ps(_mm_castps_si128(vResult));
           // Convert 0x80000000 -> 0xFFFFFFFF
           // __m128i iMask = _mm_srai_epi32(_mm_castps_si128(vMask),31);
           PSRAD   XMM3, 31
           // For only the ones that are too big, add the fixup
           {$IFDEF CPU32}
           ANDPS   XMM3,[g_XMFixUnsigned] // vMask = _mm_and_ps(_mm_castsi128_ps(iMask),g_XMFixUnsigned);
           {$ELSE}
           ANDPS   XMM3,[RIP+g_XMFixUnsigned] // vMask = _mm_and_ps(_mm_castsi128_ps(iMask),g_XMFixUnsigned);
           {$ENDIF}
           ADDPS   XMM0,XMM3 // vResult = _mm_add_ps(vResult,vMask);
           MOVUPS  [Result],XMM0 // return vResult;
end;



function XMLoadInt4(const pSource: PUINT32): TXMVECTOR; assembler;
asm
           MOVDQU  XMM0, [pSource] // V = _mm_loadu_si128( reinterpret_cast<const __m128i*>(pSource) );
           MOVAPS  [result],XMM0 // return _mm_castsi128_ps(V);
end;



function XMLoadInt4A(const pSource: PUINT32): TXMVECTOR; assembler;
asm
           MOVDQA  XMM0, [pSource] // V = _mm_load_si128( reinterpret_cast<const __m128i*>(pSource) );
           MOVAPS  [result],XMM0 // return _mm_castsi128_ps(V);
end;



function XMLoadFloat4(constref pSource: TXMFLOAT4): TXMVECTOR; assembler;
asm
           MOVUPS  XMM0, [pSource] // return _mm_loadu_ps( &pSource->x );
           MOVUPS  [result],XMM0
end;



function XMLoadFloat4(constref pSource: pSingle): TXMVECTOR; assembler;
asm
           MOVUPS  XMM0, [pSource]
           MOVUPS  [result],XMM0
end;



function XMLoadFloat4A(const pSource: TXMFLOAT4A): TXMVECTOR; assembler;
asm
           MOVAPS  XMM0, [pSource] // return _mm_load_ps( &pSource.x );
           MOVUPS  [result],XMM0
end;



function XMLoadSInt4(const pSource: TXMINT4): TXMVECTOR; assembler;
asm
           MOVDQU  XMM0, [pSource] // V = _mm_loadu_si128( reinterpret_cast<const __m128i*>(pSource) );
           CVTDQ2PS XMM0, XMM0 // return _mm_cvtepi32_ps(V);
           MOVUPS  [result],XMM0
end;



function XMLoadUInt4(const pSource: TXMUINT4): TXMVECTOR; assembler;
asm
           MOVDQU  XMM0, [pSource] // V = _mm_loadu_si128( reinterpret_cast<const __m128i*>(pSource) );
           // For the values that are higher than 0x7FFFFFFF, a fixup is needed
           // Determine which ones need the fix.
           MOVUPS  XMM1,XMM0
           {$IFDEF CPU32}
           ANDPS   XMM1, [g_XMNegativeZero] // XMM1 = vMask, vMask = _mm_and_ps(_mm_castsi128_ps(V),g_XMNegativeZero);
           {$ELSE}
           ANDPS   XMM1, [RIP+g_XMNegativeZero] // XMM1 = vMask, vMask = _mm_and_ps(_mm_castsi128_ps(V),g_XMNegativeZero);
           {$ENDIF}
           // Force all values positive
           XORPS   XMM0,XMM1 // vResult = _mm_xor_ps(_mm_castsi128_ps(V),vMask);
           // Convert to floats
           CVTDQ2PS XMM0, XMM0  // vResult = _mm_cvtepi32_ps(_mm_castps_si128(vResult));
           // Convert 0x80000000 -> 0xFFFFFFFF
           PSRAD   XMM1, 31 // iMask = _mm_srai_epi32(_mm_castps_si128(vMask),31);
           // For only the ones that are too big, add the fixup
           {$IFDEF CPU32}
           ANDPS   XMM1, [g_XMFixUnsigned] // vMask = _mm_and_ps(_mm_castsi128_ps(iMask),g_XMFixUnsigned);
           {$ELSE}
           ANDPS   XMM1, [RIP+g_XMFixUnsigned] // vMask = _mm_and_ps(_mm_castsi128_ps(iMask),g_XMFixUnsigned);
           {$ENDIF}
           ADDPS   XMM0,XMM1 // vResult = _mm_add_ps(vResult,vMask);
           MOVUPS  [Result],XMM0 // return vResult;
end;



function XMLoadFloat3x3(const pSource: TXMFLOAT3X3): TXMMATRIX; assembler;
asm
           XORPS   XMM0, XMM0 // Z = _mm_setzero_ps();
           MOVUPS  XMM1, TXMFLOAT3X3([pSource])._11 // V1 = _mm_loadu_ps( &pSource.m[0,0] );
           MOVUPS  XMM2, TXMFLOAT3X3([pSource])._22 // V2 = _mm_loadu_ps( &pSource.m[1,1] );
           MOVUPS  XMM3, TXMFLOAT3X3([pSource])._33 // V3 = _mm_load_ss( &pSource.m[2,2] );
           MOVUPS  XMM4,XMM1
           UNPCKHPS XMM4, XMM0 // T1 = _mm_unpackhi_ps( V1, Z );
           MOVUPS  XMM5,XMM2
           UNPCKHPS XMM5, XMM0 // T2 = _mm_unpacklo_ps( V2, Z );
           MOVUPS  XMM6,XMM3
           SHUFPS  XMM6, XMM5, _MM_SHUFFLE_0_1_0_0 // T3 = _mm_shuffle_ps( V3, T2, _MM_SHUFFLE( 0, 1, 0, 0 ) );
           MOVHLPS XMM5, XMM6 // T4= XMM5 // T4 = _mm_movehl_ps( T2, T3 );
           MOVHLPS XMM0, XMM4 // T5 = XMM0 // T5 = _mm_movehl_ps( Z, T1 );
           MOVHLPS XMM1, XMM4  // result.r[0] = _mm_movelh_ps( V1, T1 );
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r0), XMM1
           ADDPS   XMM5,XMM0  // result.r[1] = _mm_add_ps( T4, T5 );
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r1), XMM0
           SHUFPS  XMM2, XMM3, _MM_SHUFFLE_1_0_3_2  // result.r[2] = _mm_shuffle_ps( V2, V3, _MM_SHUFFLE(1, 0, 3, 2) );
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r2), XMM2
           {$IFDEF CPU32}
           MOVUPS  XMM0, [g_XMIdentityR3] // result.r[3] = g_XMIdentityR3;
           {$ELSE}
           MOVUPS  XMM0, [RIP+g_XMIdentityR3] // result.r[3] = g_XMIdentityR3;
           {$ENDIF}
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r3), XMM0
end;



function XMLoadFloat4x3(const pSource: TXMFLOAT4X3): TXMMATRIX; assembler;
asm
           // Use unaligned load instructions to load the 12 floats
           // vTemp1 = x1,y1,z1,x2
           MOVUPS  XMM1, TXMFLOAT4X3([pSource])._11; // vTemp1 = _mm_loadu_ps(&pSource.m[0,0]);
           // vTemp2 = y2,z2,x3,y3
           MOVUPS  XMM2, TXMFLOAT4X3([pSource])._22; // vTemp2 = _mm_loadu_ps(&pSource.m[1,1]);
           // vTemp4 = z3,x4,y4,z4
           MOVUPS  XMM4, TXMFLOAT4X3([pSource])._33; // vTemp4 = _mm_loadu_ps(&pSource.m[2,2]);
           // vTemp3 = x3,y3,z3,z3
           MOVUPS  XMM3, XMM2
           SHUFPS  XMM3, XMM4, _MM_SHUFFLE_0_0_3_2 // vTemp3 = _mm_shuffle_ps(vTemp2,vTemp4,_MM_SHUFFLE(0,0,3,2));
           // vTemp2 = y2,z2,x2,x2
           SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0 // vTemp2 = _mm_shuffle_ps(vTemp2,vTemp1,_MM_SHUFFLE(3,3,1,0));
           // vTemp2 = x2,y2,z2,z2
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2 // vTemp2 = XM_PERMUTE_PS(vTemp2,_MM_SHUFFLE(1,1,0,2));
           // vTemp1 = x1,y1,z1,0
           {$IFDEF CPU32}
           ANDPS   XMM1, [g_XMMask3] // vTemp1 = _mm_and_ps(vTemp1,g_XMMask3);
           // vTemp2 = x2,y2,z2,0
           ANDPS   XMM2, [g_XMMask3] //vTemp2 = _mm_and_ps(vTemp2,g_XMMask3);
           // vTemp3 = x3,y3,z3,0
           ANDPS   XMM3, [g_XMMask3] // vTemp3 = _mm_and_ps(vTemp3,g_XMMask3);
           {$ELSE}
           ANDPS   XMM1, [RIP+g_XMMask3] // vTemp1 = _mm_and_ps(vTemp1,g_XMMask3);
           // vTemp2 = x2,y2,z2,0
           ANDPS   XMM2, [RIP+g_XMMask3] //vTemp2 = _mm_and_ps(vTemp2,g_XMMask3);
           // vTemp3 = x3,y3,z3,0
           ANDPS   XMM3, [RIP+g_XMMask3] // vTemp3 = _mm_and_ps(vTemp3,g_XMMask3);
           {$ENDIF}
            // vTemp4i = x4,y4,z4,0
           PSRLDQ  XMM4, 4 // vTemp4i = _mm_srli_si128(_mm_castps_si128(vTemp4),32/8);
           {$IFDEF CPU32}
           // vTemp4i = x4,y4,z4,1.0f
           POR     XMM4, [g_XMIdentityR3] // vTemp4i = _mm_or_si128(vTemp4i,g_XMIdentityR3);
           {$ELSE}
           // vTemp4i = x4,y4,z4,1.0f
           POR     XMM4, [RIP+g_XMIdentityR3] // vTemp4i = _mm_or_si128(vTemp4i,g_XMIdentityR3);
           {$ENDIF}
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r0), XMM1
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r1), XMM2
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r2), XMM3
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r3), XMM4
end;



function XMLoadFloat4x3A(const pSource: TXMFLOAT4X3A): TXMMATRIX; assembler;
asm
           // Use aligned load instructions to load the 12 floats
           // vTemp1 = x1,y1,z1,x2
           MOVAPS  XMM1, TXMFLOAT4X3A([pSource])._11 // vTemp1 = _mm_load_ps(&pSource.m[0,0]);
           // vTemp2 = y2,z2,x3,y3
           MOVAPS  XMM2, TXMFLOAT4X3A([pSource])._22 // vTemp2 = _mm_load_ps(&pSource.m[1,1]);
           // vTemp4 = z3,x4,y4,z4
           MOVAPS  XMM4, TXMFLOAT4X3A([pSource])._33 // vTemp4 = _mm_load_ps(&pSource.m[2,2]);
           // vTemp3 = x3,y3,z3,z3
           MOVUPS  XMM3,XMM2
           SHUFPS  XMM3, XMM4, _MM_SHUFFLE_0_0_3_2 // vTemp3 = _mm_shuffle_ps(vTemp2,vTemp4,_MM_SHUFFLE(0,0,3,2));
           // vTemp2 = y2,z2,x2,x2
           SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0 // vTemp2 = _mm_shuffle_ps(vTemp2,vTemp1,_MM_SHUFFLE(3,3,1,0));
           // vTemp2 = x2,y2,z2,z2
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2 // vTemp2 = XM_PERMUTE_PS(vTemp2,_MM_SHUFFLE(1,1,0,2));
           // vTemp1 = x1,y1,z1,0
           // vTemp1 = _mm_and_ps(vTemp1,g_XMMask3);
           {$IFDEF CPU32}
           ANDPS   XMM1, [g_XMMask3]
           // vTemp2 = x2,y2,z2,0
           ANDPS   XMM2,[g_XMMask3] // vTemp2 = _mm_and_ps(vTemp2,g_XMMask3);
           // vTemp3 = x3,y3,z3,0
           ANDPS   XMM3,[g_XMMask3] // vTemp3 = _mm_and_ps(vTemp3,g_XMMask3);
           // vTemp4i = x4,y4,z4,0
           PSRLDQ  XMM4, 4 // vTemp4i = _mm_srli_si128(_mm_castps_si128(vTemp4),32/8);
           // vTemp4i = x4,y4,z4,1.0f
           POR     XMM4, [g_XMIdentityR3] // vTemp4i = _mm_or_si128(vTemp4i,g_XMIdentityR3);
           {$ELSE}
           ANDPS   XMM1, [RIP+g_XMMask3]
           // vTemp2 = x2,y2,z2,0
           ANDPS   XMM2,[RIP+g_XMMask3] // vTemp2 = _mm_and_ps(vTemp2,g_XMMask3);
           // vTemp3 = x3,y3,z3,0
           ANDPS   XMM3,[RIP+g_XMMask3] // vTemp3 = _mm_and_ps(vTemp3,g_XMMask3);
           // vTemp4i = x4,y4,z4,0
           PSRLDQ  XMM4, 4 // vTemp4i = _mm_srli_si128(_mm_castps_si128(vTemp4),32/8);
           // vTemp4i = x4,y4,z4,1.0f
           POR     XMM4, [RIP+g_XMIdentityR3] // vTemp4i = _mm_or_si128(vTemp4i,g_XMIdentityR3);
           {$ENDIF}
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r0), XMM1
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r1), XMM2
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r2), XMM3
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r3), XMM4
end;



function XMLoadFloat4x4(const pSource: TXMFLOAT4X4): TXMMATRIX; assembler;
asm
           MOVUPS  XMM0, TXMFLOAT4X4([pSource])._11 // result.r[0] = _mm_loadu_ps( &pSource._11 );
           MOVUPS  TXMMATRIX([result]).r0, XMM0
           MOVUPS  XMM0, TXMFLOAT4X4([pSource])._21 // result.r[1] = _mm_loadu_ps( &pSource._21 );
           MOVUPS  TXMMATRIX([result]).r1, XMM0
           MOVUPS  XMM0, TXMFLOAT4X4([pSource])._31 // result.r[2] = _mm_loadu_ps( &pSource._31 );
           MOVUPS  TXMMATRIX([result]).r2, XMM0
           MOVUPS  XMM0, TXMFLOAT4X4([pSource])._41 // result.r[3] = _mm_loadu_ps( &pSource._41 );
           MOVUPS  TXMMATRIX([result]).r3, XMM0
end;



function XMLoadFloat4x4A(const pSource: TXMFLOAT4X4A): TXMMATRIX; assembler;
asm
           MOVAPS  XMM0, TXMFLOAT4X4([pSource])._11 // result.r[0] = _mm_load_ps( &pSource._11 );
           MOVAPS  TXMMATRIX([result]).r0, XMM0
           MOVAPS  XMM0, TXMFLOAT4X4([pSource])._21 // result.r[1] = _mm_load_ps( &pSource._21 );
           MOVAPS  TXMMATRIX([result]).r1, XMM0
           MOVAPS  XMM0, TXMFLOAT4X4([pSource])._31 //  result.r[2] = _mm_load_ps( &pSource._31 );
           MOVAPS  TXMMATRIX([result]).r2, XMM0
           MOVAPS  XMM0, TXMFLOAT4X4([pSource])._41 // result.r[3] = _mm_load_ps( &pSource._41 );
           MOVAPS  TXMMATRIX([result]).r3, XMM0
end;


{****************************************************************************
 *
 * Vector and matrix store operations
 *
 ****************************************************************************}

procedure XMStoreInt(var pDestination: UINT32; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [V] // _mm_store_ss( reinterpret_cast<float*>(pDestination), V );
           MOVSS   [pDestination], XMM0
end;



procedure XMStoreFloat(var pDestination: single; V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [V] // _mm_store_ss( pDestination, V );
           MOVSS   [pDestination], XMM0
end;



procedure XMStoreInt2(var pDestination: array of uint32; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [V]
           MOVUPS  XMM1, XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1 // T = XM_PERMUTE_PS( V, _MM_SHUFFLE( 1, 1, 1, 1 ) );
           MOVSS   [pDestination], XMM0 //_mm_store_ss( reinterpret_cast<float*>(&pDestination[0]), V );
           MOVSS   [pDestination+4], XMM1 // _mm_store_ss( reinterpret_cast<float*>(&pDestination[1]), T );
end;



procedure XMStoreInt2A(var pDestination: TUINT32A_Array2; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [V] // _mm_storel_epi64( reinterpret_cast<__m128i*>(pDestination), _mm_castps_si128(V) );
           MOVQ    [pDestination], XMM0
end;



procedure XMStoreFloat2(var pDestination: TXMFLOAT2; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [V]
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1 // T = XM_PERMUTE_PS( V, _MM_SHUFFLE( 1, 1, 1, 1 ) );
           MOVSS   TXMFLOAT2([pDestination]).x, XMM0 // _mm_store_ss( &pDestination.x, V );
           MOVSS   TXMFLOAT2([pDestination]).y, XMM1 // _mm_store_ss( &pDestination.y, T );
end;



procedure XMStoreFloat2A(var pDestination: TXMFLOAT2A; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0,[V] //_mm_storel_epi64( reinterpret_cast<__m128i*>(pDestination), _mm_castps_si128(V) );
           MOVQ    [pDestination], XMM0
end;



procedure XMStoreSInt2(var pDestination: TXMINT2; constref V: TXMVECTOR); assembler;
asm
           // In case of positive overflow, detect it
           MOVUPS  XMM0, [V]
           MOVUPS  XMM1, XMM0
           {$IFDEF CPU32}
           CMPNLEPS  XMM0, [g_XMMaxInt] // vOverflow = _mm_cmpgt_ps(V,g_XMMaxInt);
           {$ELSE}
           CMPNLEPS  XMM0, [RIP+g_XMMaxInt] // vOverflow = _mm_cmpgt_ps(V,g_XMMaxInt);
           {$ENDIF}
           // Float to int conversion
           CVTTPS2DQ XMM1, XMM1 // vResulti = _mm_cvttps_epi32(V);
           // If there was positive overflow, set to 0x7FFFFFFF
           MOVUPS  XMM2,XMM0
           {$IFDEF CPU32}
           ANDPS   XMM2,[g_XMAbsMask] // vResult = _mm_and_ps(vOverflow,g_XMAbsMask);
           {$ELSE}
           ANDPS   XMM2,[RIP+g_XMAbsMask] // vResult = _mm_and_ps(vOverflow,g_XMAbsMask);
           {$ENDIF}
           ANDNPS  XMM0, XMM1 // vOverflow = _mm_andnot_ps(vOverflow,_mm_castsi128_ps(vResulti));
           ORPS    XMM0, XMM2 // vOverflow = _mm_or_ps(vOverflow,vResult);
           // Write two ints
           MOVUPS  XMM3,XMM0
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_1_1_1_1 // T = XM_PERMUTE_PS( vOverflow, _MM_SHUFFLE( 1, 1, 1, 1 ) );
           MOVSS   TXMINT2([pDestination]).x, XMM0 // _mm_store_ss( reinterpret_cast<float*>(&pDestination.x), vOverflow );
           MOVSS   TXMINT2([pDestination]).y, XMM3 //_mm_store_ss( reinterpret_cast<float*>(&pDestination.y), T );
end;



procedure XMStoreUInt2(var pDestination: TXMUINT2; constref V: TXMVECTOR); assembler;
asm
           // Clamp to >=0
           MOVUPS  XMM0, [V]
           {$IFDEF CPU32}
           MAXPS   XMM0, [g_XMZero] // vResult = _mm_max_ps(V,g_XMZero);
           // Any numbers that are too big, set to 0xFFFFFFFFU
           MOVUPS  XMM1, [g_XMMaxUInt] // vOverflow = _mm_cmpgt_ps(vResult,g_XMMaxUInt);
           CMPPS   XMM1, XMM0, 1
           MOVUPS  XMM2, [g_XMUnsignedFix] // vValue = g_XMUnsignedFix;
           {$ELSE}
           MAXPS   XMM0, [RIP+g_XMZero] // vResult = _mm_max_ps(V,g_XMZero);
           // Any numbers that are too big, set to 0xFFFFFFFFU
           MOVUPS  XMM1, [RIP+g_XMMaxUInt] // vOverflow = _mm_cmpgt_ps(vResult,g_XMMaxUInt);
           CMPPS   XMM1, XMM0, 1
           MOVUPS  XMM2, [RIP+g_XMUnsignedFix] // vValue = g_XMUnsignedFix;
           {$ENDIF}
           // Too large for a signed integer?
           MOVUPS  XMM3, XMM2
           CMPPS   XMM3, XMM0, 2 // vMask = _mm_cmpge_ps(vResult,vValue);
           // Zero for number's lower than 0x80000000, 32768.0f*65536.0f otherwise
           ANDPS   XMM2, XMM3 // vValue = _mm_and_ps(vValue,vMask);
           // Perform fixup only on numbers too large (Keeps low bit precision)
           SUBPS   XMM0, XMM2 // vResult = _mm_sub_ps(vResult,vValue);
           CVTTPS2DQ XMM4, XMM0 // vResulti = _mm_cvttps_epi32(vResult);
           // Convert from signed to unsigned pnly if greater than 0x80000000
           {$IFDEF CPU32}
           ANDPS   XMM3, [g_XMNegativeZero] // vMask = _mm_and_ps(vMask,g_XMNegativeZero);
           {$ELSE}
           ANDPS   XMM3, [RIP+g_XMNegativeZero] // vMask = _mm_and_ps(vMask,g_XMNegativeZero);
           {$ENDIF}
           XORPS   XMM4, XMM3 // vResult = _mm_xor_ps(_mm_castsi128_ps(vResulti),vMask);
           // On those that are too large, set to 0xFFFFFFFF
           ORPS    XMM4, XMM1 // vResult = _mm_or_ps(vResult,vOverflow);
           // Write two uints
           MOVUPS  XMM5,XMM4
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_1_1_1_1 // T = XM_PERMUTE_PS( vResult, _MM_SHUFFLE( 1, 1, 1, 1 ) );
           MOVSS   TXMUINT2([pDestination]).x, XMM4 // _mm_store_ss( reinterpret_cast<float*>(&pDestination.x), vResult );
           MOVSS   TXMUINT2([pDestination]).y, XMM5 // _mm_store_ss( reinterpret_cast<float*>(&pDestination.y), T );
end;



procedure XMStoreInt3(var pDestination: array of UINT32; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_1_1_1 // T1 = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1, [V]
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_2_2_2 // T2 = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
           MOVUPS  XMM2, [V]
           MOVSS   [pDestination], XMM2 // _mm_store_ss( reinterpret_cast<float*>(pDestination), V );
           MOVSS   [pDestination+4], XMM0 //_mm_store_ss( reinterpret_cast<float*>(&pDestination[1]), T1 );
           MOVSS   [pDestination+8], XMM1 //_mm_store_ss( reinterpret_cast<float*>(&pDestination[2]), T2 );
end;



procedure XMStoreInt3A(var pDestination: array of UINT32; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [V]
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_2_2_2 // T = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
           MOVQ    [pDestination], XMM0 //_mm_storel_epi64( reinterpret_cast<__m128i*>(pDestination), _mm_castps_si128(V) );
           MOVSS   pDestination[2], XMM1 // _mm_store_ss( reinterpret_cast<float*>(&pDestination[2]), T );
end;



procedure XMStoreFloat3(var pDestination: TXMFLOAT3; constref V: TXMVECTOR); inline; assembler;
asm
           MOVUPS  XMM0, [V]
           MOVUPS  XMM1,XMM0
           MOVUPS  XMM2,XMM0
           SHUFPS  XMM0,XMM0, _MM_SHUFFLE_1_1_1_1  // T1 = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_2_2_2 // T2 = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
           MOVSS   TXMFLOAT3([pDestination]).x, XMM2 // _mm_store_ss( &pDestination.x, V );
           MOVSS   TXMFLOAT3([pDestination]).y, XMM0 // _mm_store_ss( &pDestination.y, T1 );
           MOVSS   TXMFLOAT3([pDestination]).z, XMM1 // _mm_store_ss( &pDestination.z, T2 );
end;



procedure XMStoreFloat3A(var pDestination: TXMFLOAT3A; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0,[v]
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_2_2_2 // T = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
           MOVQ    TXMFLOAT3A([pDestination]), XMM1 // _mm_storel_epi64( reinterpret_cast<__m128i*>(pDestination), _mm_castps_si128(V) );
           MOVSS   TXMFLOAT3A([pDestination]).z, XMM1 //_mm_store_ss( &pDestination.z, T );
end;



procedure XMStoreSInt3(var pDestination: TXMINT3; constref V: TXMVECTOR); assembler;
asm
           // In case of positive overflow, detect it
           {$IFDEF CPU32}
           MOVUPS  XMM0,[g_XMMaxInt]
           {$ELSE}
           MOVUPS  XMM0,[RIP+g_XMMaxInt]
           {$ENDIF}
           MOVUPS  XMM1,[V]
           CMPPS   XMM0, XMM1, 1  // vOverflow = XMM0 // vOverflow = _mm_cmpgt_ps(V,g_XMMaxInt);
           // Float to int conversion
           CVTTPS2DQ XMM1, XMM1  // vResulti = XMM1 // vResulti = _mm_cvttps_epi32(V);
           // If there was positive overflow, set to 0x7FFFFFFF
           MOVUPS  XMM2,XMM0
           {$IFDEF CPU32}
           ANDPS   XMM2, [g_XMAbsMask] // vResult = _mm_and_ps(vOverflow,g_XMAbsMask);
           {$ELSE}
           ANDPS   XMM2, [RIP+g_XMAbsMask] // vResult = _mm_and_ps(vOverflow,g_XMAbsMask);
           {$ENDIF}
           ANDNPS  XMM0, XMM1 // vOverflow = _mm_andnot_ps(vOverflow,_mm_castsi128_ps(vResulti));
           ORPS    XMM0, XMM2 // vOverflow = _mm_or_ps(vOverflow,vResult);
           // Write 3 uints
           MOVUPS  XMM3,XMM0
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_1_1_1_1 // T1 = XMM3 // T1 = XM_PERMUTE_PS(vOverflow,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM4,XMM0
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_2_2_2_2 // T2 = XMM4 // T2 = XM_PERMUTE_PS(vOverflow,_MM_SHUFFLE(2,2,2,2));
           MOVSS   TXMINT3([pDestination]).x, XMM0  //_mm_store_ss( reinterpret_cast<float*>(&pDestination.x), vOverflow );
           MOVSS   TXMINT3([pDestination]).y, XMM3 //_mm_store_ss( reinterpret_cast<float*>(&pDestination.y), T1 );
           MOVSS   TXMINT3([pDestination]).z, XMM4  // _mm_store_ss( reinterpret_cast<float*>(&pDestination.z), T2 );
end;



procedure XMStoreUInt3(var pDestination: TXMUINT3; constref V: TXMVECTOR); assembler;
asm
           // Clamp to >=0
           // vResult = _mm_max_ps(V,g_XMZero);
           MOVUPS  XMM0,[V]
           {$IFDEF CPU32}
           MAXPS   XMM0, [g_XMZero] // vResult = XMM0
           MOVUPS  XMM1, [g_XMMaxUInt]
           MOVUPS  XMM2, [g_XMUnsignedFix] // vValue = g_XMUnsignedFix;
           {$ELSE}
           MAXPS   XMM0, [RIP+g_XMZero] // vResult = XMM0
           MOVUPS  XMM1, [RIP+g_XMMaxUInt]
           MOVUPS  XMM2, [RIP+g_XMUnsignedFix] // vValue = g_XMUnsignedFix;
           {$ENDIF}
           // Any numbers that are too big, set to 0xFFFFFFFFU
           CMPPS   XMM1, XMM0, 1  // vOverflow = _mm_cmpgt_ps(vResult,g_XMMaxUInt);
           // Too large for a signed integer?
           MOVUPS  XMM3,XMM2
           CMPPS   XMM3, XMM0, 2 // vMask = _mm_cmpge_ps(vResult,vValue);
           // Zero for number's lower than 0x80000000, 32768.0f*65536.0f otherwise
           ANDPS   XMM2,XMM3 // vValue = _mm_and_ps(vValue,vMask);
           // Perform fixup only on numbers too large (Keeps low bit precision)
           SUBPS   XMM0, XMM2 // vResult = _mm_sub_ps(vResult,vValue);
           CVTTPS2DQ XMM0, XMM0  // vResulti = _mm_cvttps_epi32(vResult);
           // Convert from signed to unsigned pnly if greater than 0x80000000
           {$IFDEF CPU32}
           ANDPS   XMM3, [g_XMNegativeZero]  // vMask = _mm_and_ps(vMask,g_XMNegativeZero);
           {$ELSE}
           ANDPS   XMM3, [RIP+g_XMNegativeZero]  // vMask = _mm_and_ps(vMask,g_XMNegativeZero);
           {$ENDIF}
           XORPS   XMM0, XMM3 // vResult = _mm_xor_ps(_mm_castsi128_ps(vResulti),vMask);
           // On those that are too large, set to 0xFFFFFFFF
           ORPS    XMM0, XMM1 // vResult = _mm_or_ps(vResult,vOverflow);
           // Write 3 uints
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1  // T1 = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM2,XMM0
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2  // T2 = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(2,2,2,2));
           MOVSS   TXMUINT3([pDestination]).x, XMM0 //_mm_store_ss( reinterpret_cast<float*>(&pDestination.x), vResult );
           MOVSS   TXMUINT3([pDestination]).y, XMM1 //_mm_store_ss( reinterpret_cast<float*>(&pDestination.y), T1 );
           MOVSS   TXMUINT3([pDestination]).z, XMM2 //_mm_store_ss( reinterpret_cast<float*>(&pDestination.z), T2 );
end;



procedure XMStoreInt4(var pDestination: array of UINT32; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [V] // _mm_storeu_si128( reinterpret_cast<__m128i*>(pDestination), _mm_castps_si128(V) );
           MOVDQU  [pDestination], XMM0
end;



procedure XMStoreInt4A(var pDestination: array of UINT32; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [V] //_mm_store_si128( reinterpret_cast<__m128i*>(pDestination), _mm_castps_si128(V) );
           MOVDQA  [pDestination], XMM0
end;



procedure XMStoreFloat4(var pDestination: TXMFLOAT4; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [V] // _mm_storeu_ps( &pDestination.x, V );
           MOVUPS  [pDestination], XMM0
end;



procedure XMStoreFloat4A(var pDestination: TXMFLOAT4A; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [V] // _mm_store_ps( &pDestination.x, V );
           MOVAPS  [pDestination], XMM0
end;



procedure XMStoreSInt4(var pDestination: TXMINT4; constref V: TXMVECTOR); assembler;
asm
           // In case of positive overflow, detect it
           {$IFDEF CPU32}
           MOVUPS  XMM0, [g_XMMaxInt]  // vOverflow = _mm_cmpgt_ps(V,g_XMMaxInt);
           {$ELSE}
           MOVUPS  XMM0, [RIP+g_XMMaxInt]  // vOverflow = _mm_cmpgt_ps(V,g_XMMaxInt);
           {$ENDIF}
           MOVUPS  XMM1, [V]
           CMPPS   XMM0, XMM1, 1
           // Float to int conversion
           CVTTPS2DQ XMM1, XMM1   // vResulti = _mm_cvttps_epi32(V);
           // If there was positive overflow, set to 0x7FFFFFFF
           MOVUPS  XMM2, XMM0
           {$IFDEF CPU32}
           ANDPS   XMM2, [g_XMAbsMask] // vResult = _mm_and_ps(vOverflow,g_XMAbsMask);
           {$ELSE}
           ANDPS   XMM2, [RIP+g_XMAbsMask] // vResult = _mm_and_ps(vOverflow,g_XMAbsMask);
           {$ENDIF}
           ANDNPS  XMM0, XMM1  // vOverflow = _mm_andnot_ps(vOverflow,_mm_castsi128_ps(vResulti));
           ORPS    XMM0, XMM2  // vOverflow = _mm_or_ps(vOverflow,vResult);
           MOVDQU  [pDestination], XMM0 // _mm_storeu_si128( reinterpret_cast<__m128i*>(pDestination), _mm_castps_si128(vOverflow) );
end;



procedure XMStoreUInt4(var pDestination: TXMUINT4; constref V: TXMVECTOR); assembler;
asm
           // Clamp to >=0
           MOVUPS  XMM0, [V]
           {$IFDEF CPU32}
           MAXPS   XMM0, [g_XMZero] // vResult = _mm_max_ps(V,g_XMZero);
           // Any numbers that are too big, set to 0xFFFFFFFFU
           MOVUPS  XMM1,[g_XMMaxUInt] // vOverflow = _mm_cmpgt_ps(vResult,g_XMMaxUInt);
           CMPPS   XMM1, XMM0, 1
           MOVUPS  XMM2,[g_XMUnsignedFix] // vValue = g_XMUnsignedFix;
           {$ELSE}
           MAXPS   XMM0, [RIP+g_XMZero] // vResult = _mm_max_ps(V,g_XMZero);
           // Any numbers that are too big, set to 0xFFFFFFFFU
           MOVUPS  XMM1,[RIP+g_XMMaxUInt] // vOverflow = _mm_cmpgt_ps(vResult,g_XMMaxUInt);
           CMPPS   XMM1, XMM0, 1
           MOVUPS  XMM2,[RIP+g_XMUnsignedFix] // vValue = g_XMUnsignedFix;
           {$ENDIF}
           // Too large for a signed integer?
           MOVUPS  XMM3, XMM2
           CMPPS   XMM3, XMM0, 2 // vMask = _mm_cmpge_ps(vResult,vValue);
           // Zero for number's lower than 0x80000000, 32768.0f*65536.0f otherwise
           ANDPS   XMM2, XMM3  // vValue = _mm_and_ps(vValue,vMask);
           // Perform fixup only on numbers too large (Keeps low bit precision)
           SUBPS   XMM0, XMM2 // vResult = _mm_sub_ps(vResult,vValue);
           CVTTPS2DQ XMM0, XMM0 // vResulti = _mm_cvttps_epi32(vResult);
           // Convert from signed to unsigned pnly if greater than 0x80000000
           {$IFDEF CPU32}
           ANDPS   XMM3, [g_XMNegativeZero] // vMask = _mm_and_ps(vMask,g_XMNegativeZero);
           {$ELSE}
           ANDPS   XMM3, [RIP+g_XMNegativeZero] // vMask = _mm_and_ps(vMask,g_XMNegativeZero);
           {$ENDIF}
           XORPS   XMM0, XMM3 // vResult = _mm_xor_ps(_mm_castsi128_ps(vResulti),vMask);
           // On those that are too large, set to 0xFFFFFFFF
           ORPS    XMM0, XMM1 // vResult = _mm_or_ps(vResult,vOverflow);
           MOVDQU  [pDestination], XMM0 // _mm_storeu_si128( reinterpret_cast<__m128i*>(pDestination), _mm_castps_si128(vResult) );
end;



procedure XMStoreFloat3x3(var pDestination: TXMFLOAT3X3; constref M: TXMMATRIX); assembler;
asm
           MOVUPS  XMM1, TXMMATRIX([M]).r0 // vTemp1 = M.r[0];
           MOVUPS  XMM2, TXMMATRIX([M]).r1 // vTemp2 = M.r[1];
           MOVUPS  XMM3, TXMMATRIX([M]).r2 // vTemp3 = M.r[2];
           MOVUPS  XMM4, XMM1
           SHUFPS  XMM4, XMM2, _MM_SHUFFLE_0_0_2_2 // vWork = _mm_shuffle_ps(vTemp1,vTemp2,_MM_SHUFFLE(0,0,2,2));
           SHUFPS  XMM1, XMM4, _MM_SHUFFLE_2_0_1_0 // vTemp1 = _mm_shuffle_ps(vTemp1,vWork,_MM_SHUFFLE(2,0,1,0));
           MOVUPS  TXMFLOAT3X3([pDestination])._11, XMM1 // _mm_storeu_ps(&pDestination.m[0,0],vTemp1);
           SHUFPS  XMM2, XMM3, _MM_SHUFFLE_1_0_2_1  // vTemp2 = _mm_shuffle_ps(vTemp2,vTemp3,_MM_SHUFFLE(1,0,2,1));
           MOVUPS  TXMFLOAT3X3([pDestination])._22, XMM2 // _mm_storeu_ps(&pDestination.m[1,1],vTemp2);
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_2_2_2_2  // vTemp3 = XM_PERMUTE_PS(vTemp3,_MM_SHUFFLE(2,2,2,2));
           MOVSS   TXMFLOAT3X3([pDestination])._33, XMM3 // _mm_store_ss(&pDestination.m[2,2],vTemp3);
end;



procedure XMStoreFloat4x3(var pDestination: TXMFLOAT4X3; constref M: TXMMATRIX); assembler;
asm
           MOVUPS  XMM1, TXMMATRIX([M]).r0 // vTemp1 = M.r[0];
           MOVUPS  XMM2, TXMMATRIX([M]).r1 // vTemp2 = M.r[1];
           MOVUPS  XMM3, TXMMATRIX([M]).r2 // vTemp3 = M.r[2];
           MOVUPS  XMM4, TXMMATRIX([M]).r3  // vTemp4 = M.r[3];
           MOVUPS  XMM5,XMM2
           SHUFPS  XMM5, XMM3, _MM_SHUFFLE_1_0_2_1  // vTemp2x = _mm_shuffle_ps(vTemp2,vTemp3,_MM_SHUFFLE(1,0,2,1));
           SHUFPS  XMM2, XMM1, _MM_SHUFFLE_2_2_0_0 // vTemp2 = _mm_shuffle_ps(vTemp2,vTemp1,_MM_SHUFFLE(2,2,0,0));
           SHUFPS  XMM1, XMM2, _MM_SHUFFLE_0_2_1_0 // vTemp1 = _mm_shuffle_ps(vTemp1,vTemp2,_MM_SHUFFLE(0,2,1,0));
           SHUFPS  XMM3, XMM4, _MM_SHUFFLE_0_0_2_2 // vTemp3 = _mm_shuffle_ps(vTemp3,vTemp4,_MM_SHUFFLE(0,0,2,2));
           SHUFPS  XMM3, XMM4, _MM_SHUFFLE_2_1_2_0 // vTemp3 = _mm_shuffle_ps(vTemp3,vTemp4,_MM_SHUFFLE(2,1,2,0));
           MOVUPS  TXMFLOAT4X3([pDestination])._11, XMM1 // _mm_storeu_ps(&pDestination.m[0,0],vTemp1);
           MOVUPS  TXMFLOAT4X3([pDestination])._22, XMM5 // _mm_storeu_ps(&pDestination.m[1,1],vTemp2x);
           MOVUPS  TXMFLOAT4X3([pDestination])._33, XMM3 // _mm_storeu_ps(&pDestination.m[2,2],vTemp3);
end;



procedure XMStoreFloat4x3A(var pDestination: TXMFLOAT4X3A; constref M: TXMMATRIX);
asm
           // x1,y1,z1,w1
           // vTemp1 = M.r[0];
           MOVUPS  XMM1, TXMMATRIX([M]).r0
           // x2,y2,z2,w2
           // vTemp2 = M.r[1];
           MOVUPS  XMM2, TXMMATRIX([M]).r1
           // x3,y3,z3,w3
           // vTemp3 = M.r[2];
           MOVUPS  XMM3, TXMMATRIX([M]).r2
           // x4,y4,z4,w4
           // vTemp4 = M.r[3];
           MOVUPS  XMM4, TXMMATRIX([M]).r3
           // z1,z1,x2,y2
           // vTemp = _mm_shuffle_ps(vTemp1,vTemp2,_MM_SHUFFLE(1,0,2,2));
           MOVUPS  XMM5, XMM1
           SHUFPS  XMM5, XMM2, _MM_SHUFFLE_1_0_2_2
           // y2,z2,x3,y3 (Final)
           // vTemp2 = _mm_shuffle_ps(vTemp2,vTemp3,_MM_SHUFFLE(1,0,2,1));
           SHUFPS  XMM2, XMM3, _MM_SHUFFLE_1_0_2_1
           // x1,y1,z1,x2 (Final)
           // vTemp1 = _mm_shuffle_ps(vTemp1,vTemp,_MM_SHUFFLE(2,0,1,0));
           SHUFPS  XMM1, XMM5, _MM_SHUFFLE_2_0_1_0
           // z3,z3,x4,x4
           // vTemp3 = _mm_shuffle_ps(vTemp3,vTemp4,_MM_SHUFFLE(0,0,2,2));
           SHUFPS  XMM3, XMM4, _MM_SHUFFLE_0_0_2_2
           // z3,x4,y4,z4 (Final)
           // vTemp3 = _mm_shuffle_ps(vTemp3,vTemp4,_MM_SHUFFLE(2,1,2,0));
           SHUFPS  XMM3, XMM4, _MM_SHUFFLE_2_1_2_0
           // Store in 3 operations
           // _mm_store_ps(&pDestination.m[0,0],vTemp1);
           MOVAPS  TXMFLOAT4X3A([pDestination])._11, XMM1
           // _mm_store_ps(&pDestination.m[1,1],vTemp2);
           MOVAPS  TXMFLOAT4X3A([pDestination])._22, XMM2
           // _mm_store_ps(&pDestination.m[2,2],vTemp3);
           MOVAPS  TXMFLOAT4X3A([pDestination])._33, XMM3
end;



procedure XMStoreFloat4x4(var pDestination: TXMFLOAT4X4; constref M: TXMMATRIX);
asm
           // _mm_storeu_ps( &pDestination._11, M.r[0] );
           MOVUPS  XMM0, TXMMATRIX([M]).r0
           MOVUPS  TXMFLOAT4X4([pDestination])._11, XMM0
           // _mm_storeu_ps( &pDestination._21, M.r[1] );
           MOVUPS  XMM0, TXMMATRIX([M]).r1
           MOVUPS  TXMFLOAT4X4([pDestination])._21, XMM0
           // _mm_storeu_ps( &pDestination._31, M.r[2] );
           MOVUPS  XMM0, TXMMATRIX([M]).r2
           MOVUPS  TXMFLOAT4X4([pDestination])._31, XMM0
           // _mm_storeu_ps( &pDestination._41, M.r[3] );
           MOVUPS  XMM0, TXMMATRIX([M]).r3
           MOVUPS  TXMFLOAT4X4([pDestination])._41, XMM0
end;



procedure XMStoreFloat4x4A(var pDestination: TXMFLOAT4X4A; constref M: TXMMATRIX);
asm
           // _mm_store_ps( &pDestination._11, M.r[0] );
           MOVUPS  XMM0, TXMMATRIX([M]).r0
           MOVAPS  TXMFLOAT4X4A([pDestination])._11, XMM0
           // _mm_store_ps( &pDestination._21, M.r[1] );
           MOVUPS  XMM0, TXMMATRIX([M]).r1
           MOVAPS  TXMFLOAT4X4A([pDestination])._21, XMM0
           // _mm_store_ps( &pDestination._31, M.r[2] );
           MOVUPS  XMM0, TXMMATRIX([M]).r2
           MOVAPS  TXMFLOAT4X4A([pDestination])._31, XMM0
           // _mm_store_ps( &pDestination._41, M.r[3] );
           MOVUPS  XMM0, TXMMATRIX([M]).r3
           MOVAPS  TXMFLOAT4X4A([pDestination])._41, XMM0
end;



{****************************************************************************
 *
 * General Vector
 *
 ****************************************************************************}

//------------------------------------------------------------------------------
// Assignment operations
//------------------------------------------------------------------------------


function XMVectorZero: TXMVECTOR; assembler;
asm
           XORPS   XMM0, XMM0
           MOVUPS   [Result], XMM0
end;


// Initialize a vector with four floating point values
function XMVectorSet(const x, y, z, w: single): TXMVECTOR; assembler;
asm
           MOVD        XMM0, [w]
           MOVD        XMM1, [z]
           MOVD        XMM2, [y]
           MOVD        XMM3, [x]
           PUNPCKLDQ   XMM3,XMM1
           PUNPCKLDQ   XMM2,XMM0
           PUNPCKLDQ   XMM3,XMM2
           MOVUPS  [result], XMM3 // _mm_set_ps( w, z, y, x );
end;


// Initialize a vector with four integer values
function XMVectorSetInt(const x, y, z, w: UINT32): TXMVECTOR;
asm
           // Var x located in register eax
           // Var y located in register edx
           // Var z located in register ecx
           // Var w located at ebp+12, size=OS_32
           // Var $result located at ebp+8, size=OS_32
           // V = _mm_set_epi32( w, z, y, x );
           MOVD        XMM0, [W]
           MOVD        XMM1, z
           MOVD        XMM2, y
           MOVD        XMM3, x
           PUNPCKLDQ   XMM3,XMM1
           PUNPCKLDQ   XMM2,XMM0
           PUNPCKLDQ   XMM3,XMM2
           {$IFDEF CPUX86_64}
           MOVUPS  [result], XMM3  // return _mm_castsi128_ps(V);
           {$ELSE}
           PUSH        EBX
           MOV     EBX,[result]
           MOVUPS  [EBX], XMM3  // return _mm_castsi128_ps(V);
           POP     EBX
           {$ENDIF}
end;


{$IF DEFINED(_XM_AVX_INTRINSICS_)}
// Initialize a vector with a replicated floating point value passed by pointer
function XMVectorReplicate(constref Value: single): TXMVECTOR; nostackframe; register; assembler;
asm
           VBROADCASTSS XMM0, [Value]
           VMOVUPS   [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorReplicate(constref Value: single): TXMVECTOR; assembler;
asm
           MOVSS   XMM0, [Value]  // return _mm_set_ps1( Value );
           SHUFPS  XMM0, XMM0, 0
           MOVUPS   [Result], XMM0
end;
{$ENDIF}

{$IF DEFINED(_XM_AVX_INTRINSICS_)}
// Initialize a vector with a replicated floating point value passed by pointer
function XMVectorReplicatePtr(pValue: PSingle): TXMVECTOR; assembler;
asm
           VBROADCASTSS XMM0, [pValue] // return _mm_broadcast_ss( pValue );
           VMOVUPS   [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorReplicatePtr(pValue: PSingle): TXMVECTOR; assembler;
asm
           MOVSS   XMM0, [pValue] // return _mm_load_ps1( pValue );
           SHUFPS  XMM0, XMM0, 0
           MOVUPS   [Result], XMM0
end;
{$ENDIF}



{$IF DEFINED(_XM_AVX_INTRINSICS_)}
// Initialize a vector with a replicated integer value
function XMVectorReplicateInt(constref Value: UINT32): TXMVECTOR; assembler;
asm
           VPBROADCASTD XMM0, [Value]
           VMOVUPS   [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorReplicateInt(constref Value: UINT32): TXMVECTOR; assembler;
asm
           MOVUPS  XMM0,[value] // vTemp = _mm_set1_epi32( Value );
           MOVUPS  [result],XMM0 // return _mm_castsi128_ps(vTemp);
end;
{$ENDIF}


{$IF DEFINED(_XM_AVX_INTRINSICS_)}
// Initialize a vector with a replicated integer value passed by pointer
function XMVectorReplicateIntPtr(pValue: PUINT32): TXMVECTOR; assembler;
asm
           VPBROADCASTD XMM0, [pValue]
           VMOVUPS   [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_
// Initialize a vector with a replicated integer value passed by pointer

function XMVectorReplicateIntPtr(pValue: PUINT32): TXMVECTOR; assembler;
asm
           MOVSS   XMM0, [pValue] // return _mm_load_ps1(reinterpret_cast<const float *>(pValue));
           SHUFPS  XMM0, XMM0, 0
           MOVUPS  [result],XMM0
end;
{$ENDIF}


{$IF DEFINED(_XM_AVX_INTRINSICS_)}
// Initialize a vector with all bits set (true mask)
function XMVectorTrueInt: TXMVECTOR; assembler;
const
    c: uint32 = $FFFFFFFF;
asm
           VPBROADCASTD XMM0, [c]
           VMOVUPS   [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorTrueInt: TXMVECTOR; assembler; nostackframe; // ??????
const
    v: array[0..3] of UINT32 = ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF);
asm
           MOVAPS  XMM0, [v] // V = _mm_set1_epi32(-1);
           MOVAPS  [Result],XMM0 // return _mm_castsi128_ps(V);
end;
{$ENDIF}

// Initialize a vector with all bits clear (false mask)
function XMVectorFalseInt: TXMVECTOR; assembler;
asm
           XORPS   XMM0, XMM0
           MOVUPS  [Result], XMM0
end;


{$IF DEFINED(_XM_AVX2_INTRINSICS_)}
// Returns a vector, all of whose components are equal to the x component of V.
function XMVectorSplatX(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           VBROADCASTSS XMM0, [v]
           VMOVUPS   [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorSplatX(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           MOVUPS  XMM0,[V]
           MOVAPS  XMM1, XMM0
           SHUFPS  XMM0, XMM1, _MM_SHUFFLE_0_0_0_0
           MOVUPS  [Result], XMM0
end;
{$ENDIF}

{$IF DEFINED(_XM_AVX2_INTRINSICS_)}
// Returns a vector, all of whose components are equal to the y component of V.
function XMVectorSplatY(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           VMOVUPS XMM0,[v]
           VPERMILPS XMM0, XMM0, $55
           VMOVUPS   [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorSplatY(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           MOVUPS  XMM0,[V]
           MOVAPS  XMM1, XMM0
           SHUFPS  XMM0, XMM1, _MM_SHUFFLE_1_1_1_1
           MOVUPS  [Result], XMM0
end;
{$ENDIF}


{$IF DEFINED(_XM_AVX2_INTRINSICS_)}
// Replicate the z component of the vector
// Returns a vector, all of whose components are equal to the z component of V.
function XMVectorSplatZ(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           VMOVUPS XMM0,[v]
           VPERMILPS XMM0, XMM0, $AA
           VMOVUPS   [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorSplatZ(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           MOVUPS  XMM0,[V]
           MOVAPS  XMM1, XMM0
           SHUFPS  XMM0, XMM1, _MM_SHUFFLE_2_2_2_2
           MOVUPS  [Result], XMM0
end;
{$ENDIF}

{$IF DEFINED(_XM_AVX2_INTRINSICS_)}
// Returns a vector, all of whose components are equal to the w component of V.
function XMVectorSplatW(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           VMOVUPS XMM0,[v]
           VPERMILPS XMM0, XMM0, $FF
           VMOVUPS   [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorSplatW(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           MOVUPS  XMM0,[V]
           MOVAPS  XMM1, XMM0
           SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_3_3_3
           MOVUPS   [Result], XMM0
end;
{$ENDIF}


// Return a vector of 1.0f,1.0f,1.0f,1.0f
function XMVectorSplatOne: TXMVECTOR; assembler;
asm
           {$IFDEF CPU32}
           MOVUPS  XMM0,[g_XMOne]
           {$ELSE}
           MOVUPS  XMM0,[RIP+g_XMOne]
           {$ENDIF}
           MOVUPS   [Result], XMM0
end;


// Return a vector of INF,INF,INF,INF
function XMVectorSplatInfinity: TXMVECTOR; assembler;
asm
           {$IFDEF CPU32}
           MOVUPS  XMM0,[g_XMInfinity]
           {$ELSE}
           MOVUPS  XMM0,[RIP+g_XMInfinity]
           {$ENDIF}
           MOVUPS   [Result], XMM0
end;



function XMVectorSplatQNaN: TXMVECTOR; assembler;
asm
           {$IFDEF CPU32}
           MOVUPS  XMM0,[g_XMQNaN]
           {$ELSE}
           MOVUPS  XMM0,[RIP+g_XMQNaN]
           {$ENDIF}
           MOVUPS   [Result], XMM0
end;



function XMVectorSplatEpsilon: TXMVECTOR; assembler;
asm
           MOVUPS  XMM0,[g_XMEpsilon]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           MOVUPS   [Result], XMM0
end;



function XMVectorSplatSignMask: TXMVECTOR; assembler;
const
    v: array [0..3] of UINT32 = ($80000000, $80000000, $80000000, $80000000);
asm
           MOVUPS  XMM0,[v]
           MOVUPS   [Result], XMM0
end;



function XMVectorGetX(constref V: TXMVECTOR): single; assembler;
asm
           MOVUPS  XMM0, [v]
           MOVSS   [result], XMM0
end;



function XMVectorGetY(constref V: TXMVECTOR): single; assembler;
asm
           MOVUPS  XMM0, [v]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_1_1_1
           MOVSS   [result], XMM0
end;



function XMVectorGetZ(constref V: TXMVECTOR): single; assembler;
asm
           MOVUPS  XMM0, [v]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_2_2_2
           MOVSS   [result], XMM0
end;



function XMVectorGetW(constref V: TXMVECTOR): single; assembler;
asm
           MOVUPS  XMM0, [v]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_3_3_3
           MOVSS   [result], XMM0
end;



procedure XMVectorGetXPtr(out x: single; constref V: TXMVECTOR); assembler;
asm
           // _mm_store_ss(x,V);
           MOVUPS  XMM0, [v]
           MOVSS   [x], XMM0
end;

{$IF DEFINED(_XM_SSE4_INTRINSICS_)}
// Store the Y component into a 32 bit  single  location in memory.
procedure XMVectorGetYPtr(out y: single; constref V: TXMVECTOR); assembler;
asm
           //  *((int*)y) = _mm_extract_ps( V, 1 );
           MOVUPS  XMM0, [v]
           EXTRACTPS [y], XMM0, 32
end;

// Store the Z component into a 32 bit  single  location in memory.

procedure XMVectorGetZPtr(out z: single; constref V: TXMVECTOR); assembler;
asm

           // *((int*)z) = _mm_extract_ps( V, 2 );
           MOVUPS  XMM0 ,[V]
           EXTRACTPS EAX, XMM0, 2
           MOV     [z], EAX
end;

// Store the W component into a 32 bit  single  location in memory.

procedure XMVectorGetWPtr(out w: single; constref V: TXMVECTOR); assembler;
asm
           //((int*)w) = _mm_extract_ps( V, 3 );
           MOVUPS  XMM0 ,[V]
           EXTRACTPS EAX, XMM0, 3
           MOV     [w], EAX
end;
{$ELSE}// _XM_SSE_INTRINSICS_


procedure XMVectorGetYPtr(out y: single; constref V: TXMVECTOR); assembler;
asm
           MOVUPS  XMM0, [v]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_1_1_1
           MOVSS   [y], XMM0
end;



procedure XMVectorGetZPtr(out z: single; constref V: TXMVECTOR); assembler;
asm
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
           MOVUPS  XMM0, [v]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_2_2_2
           // _mm_store_ss(z,vResult);
           MOVSS   [z], XMM0
end;



procedure XMVectorGetWPtr(out w: single; constref V: TXMVECTOR); assembler;
asm
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,3,3,3));
           MOVUPS  XMM0,[V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_3_3_3
           // _mm_store_ss(w,vResult);
           MOVSS   [w], XMM0
end;
{$ENDIF}


// Return the X component in an integer register.
function XMVectorGetIntX(constref V: TXMVECTOR): UINT32; assembler;
asm
           // return static_cast<uint32_t>(_mm_cvtsi128_si32(_mm_castps_si128(V)));
           MOVUPS  XMM0, [V]
           MOVD    [result], XMM0
end;

{$IF DEFINED(_XM_SSE4_INTRINSICS_)}
// Return the Y component in an integer register.
function XMVectorGetIntY(constref V: TXMVECTOR): UINT32; assembler;
asm
           // V1 = _mm_castps_si128( V );
           MOVUPS  XMM0, [V]
           // return static_cast<uint32_t>( _mm_extract_epi32( V1, 1 ) );
           PEXTRD  EAX, XMM0, 1
           MOV     [result],EAX
end;

// Return the Z component in an integer register.

function XMVectorGetIntZ(constref V: TXMVECTOR): UINT32; assembler;
asm
           // V1 = _mm_castps_si128( V );
           MOVUPS  XMM0,[V]
           // return static_cast<uint32_t>( _mm_extract_epi32( V1, 2 ) );
           PEXTRD  EAX, XMM0, 2
           MOV     [result],EAX
end;

// Return the W component in an integer register.

function XMVectorGetIntW(constref V: TXMVECTOR): UINT32; assembler;
asm
           // V1 = _mm_castps_si128( V );
           MOVUPS  XMM0,[V]
           // static_cast<uint32_t>( _mm_extract_epi32( V1, 3 ) );
           PEXTRD  EAX, XMM0, 3
           MOV     [result],EAX
end;

{$ELSE}// _XM_SSE_INTRINSICS_

// Return the Y component in an integer register.
function XMVectorGetIntY(constref V: TXMVECTOR): UINT32; assembler;
asm
           // __m128i vResulti = _mm_shuffle_epi32(_mm_castps_si128(V),_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM0,[V]
           PSHUFD  XMM0, XMM0, _MM_SHUFFLE_1_1_1_1
           // return static_cast<uint32_t>(_mm_cvtsi128_si32(vResulti));
           MOVD    [result], XMM0
end;

// Return the Z component in an integer register.

function XMVectorGetIntZ(constref V: TXMVECTOR): UINT32; assembler;
asm
           // __m128i vResulti = _mm_shuffle_epi32(_mm_castps_si128(V),_MM_SHUFFLE(2,2,2,2));
           MOVUPS  XMM0,[V]
           PSHUFD  XMM0, XMM0, _MM_SHUFFLE_2_2_2_2
           // return static_cast<uint32_t>(_mm_cvtsi128_si32(vResulti));
           MOVD    [result], XMM0
end;



function XMVectorGetIntW(constref V: TXMVECTOR): UINT32; assembler;
asm
           // __m128i vResulti = _mm_shuffle_epi32(_mm_castps_si128(V),_MM_SHUFFLE(3,3,3,3));
           MOVUPS  XMM0,[V]
           PSHUFD  XMM0, XMM0, _MM_SHUFFLE_3_3_3_3
           // return static_cast<uint32_t>(_mm_cvtsi128_si32(vResulti));
           MOVD    [result], XMM0
end;

{$ENDIF}

// Store the X component into a 32 bit integer location in memory.
procedure XMVectorGetIntXPtr(out x: UINT32; constref V: TXMVECTOR); assembler;
asm
           //_mm_store_ss(reinterpret_cast<float *>(x),V);
           MOVUPS  XMM0,[V]
           MOVSS   [x], XMM0
end;

{$IF DEFINED(_XM_SSE4_INTRINSICS_)}
// Store the Y component into a 32 bit integer location in memory.
procedure XMVectorGetIntYPtr(out y: UINT32; constref V: TXMVECTOR); assembler;
asm
           // V1 = _mm_castps_si128( V );
           MOVUPS  XMM0,[V]
           // *y = static_cast<uint32_t>( _mm_extract_epi32( V1, 1 ) );
           PEXTRD  EAX, XMM0, 1
           MOV     [y],EAX
end;

// Store the Z component into a 32 bit integer location in memory.

procedure XMVectorGetIntZPtr(out z: UINT32; constref V: TXMVECTOR); assembler;
asm
           // V1 = _mm_castps_si128( V );
           MOVUPS  XMM0,[V]
           // *z = static_cast<uint32_t>( _mm_extract_epi32( V1, 2 ) );
           PEXTRD  EAX, XMM0, 2
           MOV     [z],EAX
end;

// Store the W component into a 32 bit integer location in memory.

procedure XMVectorGetIntWPtr(out w: UINT32; constref V: TXMVECTOR); assembler;
asm
           // V1 = _mm_castps_si128( V );
           MOVUPS  XMM0 ,[V]
           // *w = static_cast<uint32_t>( _mm_extract_epi32( V1, 3 ) );
           PEXTRD  EAX, XMM0, 3
           MOV     [w],EAX
end;

{$ELSE}// _XM_SSE_INTRINSICS_


procedure XMVectorGetIntYPtr(out y: UINT32; constref V: TXMVECTOR); assembler;
asm
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM0,[V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_1_1_1
           //    _mm_store_ss(reinterpret_cast<float *>(y),vResult);
           MOVSS   [y], XMM0
end;

// Store the Z component into a 32 bit integer location in memory.

procedure XMVectorGetIntZPtr(out z: UINT32; constref V: TXMVECTOR); assembler;
asm
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
           MOVUPS  XMM0,[V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_2_2_2
           //_mm_store_ss(reinterpret_cast<float *>(z),vResult);
           MOVSS   [z], XMM0
end;



procedure XMVectorGetIntWPtr(out w: UINT32; constref V: TXMVECTOR); assembler;
asm
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,3,3,3));
           MOVUPS  XMM0,[V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_3_3_3
           // _mm_store_ss(reinterpret_cast<float *>(w),vResult);
           MOVSS   [w], XMM0
end;

{$ENDIF}

// Sets the X component of a vector to a passed floating point value
function XMVectorSetX(constref V: TXMVECTOR; constref x: single): TXMVECTOR; assembler;
asm
           //  vResult = _mm_set_ss(x);
           MOVSS   XMM1, [x]
           // vResult = _mm_move_ss(V,vResult);
           MOVUPS  XMM0,[V]
           MOVSS   XMM0,XMM1
           // return vResult;
           MOVUPS  [result],XMM0
end;

{$IF DEFINED(_XM_SSE4_INTRINSICS_)}
// Sets the Y component of a vector to a passed floating point value
function XMVectorSetY(constref V: TXMVECTOR; constref y: single): TXMVECTOR; assembler;
asm
           // vResult = _mm_set_ss(y);
           MOVSS   XMM1,[y]
           // vResult = _mm_insert_ps( V, vResult, 0x10 );
           MOVUPS  XMM0,[V]
           INSERTPS XMM0, XMM1, $10
           // return vResult;
           MOVUPS  [result],XMM0
end;

// Sets the Z component of a vector to a passed floating point value

function XMVectorSetZ(constref V: TXMVECTOR; constref z: single): TXMVECTOR; assembler;
asm
           // vResult = _mm_set_ss(z);
           MOVSS   XMM1,[z]
           //vResult = _mm_insert_ps( V, vResult, $20 );
           MOVUPS  XMM0,[V]
           INSERTPS XMM0, XMM1, $20
           //return vResult;
           MOVUPS  [result],XMM0
end;

// Sets the W component of a vector to a passed floating point value

function XMVectorSetW(constref V: TXMVECTOR; constref w: single): TXMVECTOR; assembler;
asm
           // vResult = _mm_set_ss(w);
           MOVSS   XMM1,[w]
           // vResult = _mm_insert_ps( V, vResult, $30 );
           MOVUPS  XMM0,[V]
           INSERTPS XMM0, XMM1, $30
           // return vResult;
           MOVUPS  [result], XMM0
end;

{$ELSE}// _XM_SSE_INTRINSICS_

// Sets the Y component of a vector to a passed floating point value
function XMVectorSetY(constref V: TXMVECTOR; constref y: single): TXMVECTOR; assembler;
asm
           // Swap y and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,2,0,1));
           MOVUPS  XMM0, [V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_2_0_1
           // Convert input to vector
           // vTemp = _mm_set_ss(y);
           MOVSS   XMM1,[y]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,vTemp);
           MOVSS   XMM0, XMM1
           // Swap y and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(3,2,0,1));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_2_0_1
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVectorSetZ(constref V: TXMVECTOR; constref z: single): TXMVECTOR; assembler;
asm
           // Swap z and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,0,1,2));
           MOVUPS  XMM0, [V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_0_1_2
           // Convert input to vector
           // vTemp = _mm_set_ss(z);
           MOVSS   XMM1,[z]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,vTemp);
           MOVSS   XMM0,XMM1
           // Swap z and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(3,0,1,2));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_0_1_2
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVectorSetW(constref V: TXMVECTOR; constref w: single): TXMVECTOR; assembler;
asm
           // Swap w and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,2,1,3));
           MOVUPS  XMM0, [V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_2_1_3
           // Convert input to vector
           // vTemp = _mm_set_ss(w);
           MOVSS   XMM1,[w]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,vTemp);
           MOVSS   XMM0, XMM1
           // Swap w and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(0,2,1,3));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_2_1_3
           // return vResult;
           MOVUPS  [result],XMM0
end;

{$ENDIF}


// Sets the X component of a vector to a floating point value passed by p
function XMVectorSetXPtr(constref V: TXMVECTOR; constref x: Psingle): TXMVECTOR; assembler;
asm
           // XMVECTOR vResult = _mm_load_ss(x);
           MOVSS   XMM1,[x]
           // vResult = _mm_move_ss(V,vResult);
           MOVUPS  XMM0, [V]
           MOVSS   XMM0,XMM1
           // return vResult;
           MOVUPS  [result],XMM0
end;

// Sets the Y component of a vector to a floating point value passed by pointer

function XMVectorSetYPtr(constref V: TXMVECTOR; constref y: Psingle): TXMVECTOR; assembler;
asm
           // Swap y and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,2,0,1));
           MOVUPS  XMM0, [V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_2_0_1
           // Convert input to vector
           // vTemp = _mm_load_ss(y);
           MOVSS   XMM1, [y]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,vTemp);
           MOVSS   XMM0, XMM1
           // Swap y and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(3,2,0,1));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_2_0_1
           // return vResult;
           MOVUPS  [result],XMM0
end;

// Sets the Z component of a vector to a floating point value passed by pointer

function XMVectorSetZPtr(constref V: TXMVECTOR; constref z: Psingle): TXMVECTOR; assembler;
asm
           // Swap z and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,0,1,2));
           MOVUPS  XMM0, [V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_0_1_2
           // Convert input to vector
           // vTemp = _mm_load_ss(z);
           MOVSS   XMM1, [z]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,vTemp);
           MOVSS   XMM0, XMM1
           // Swap z and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(3,0,1,2));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_0_1_2
           // return vResult;
           MOVUPS  [result],XMM0
end;

// Sets the W component of a vector to a floating point value passed by pointer

function XMVectorSetWPtr(constref V: TXMVECTOR; constref w: Psingle): TXMVECTOR; assembler;
asm
           // Swap w and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,2,1,3));
           MOVUPS  XMM0, [V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_2_1_3
           // Convert input to vector
           // vTemp = _mm_load_ss(w);
           MOVSS   XMM1,[w]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,vTemp);
           MOVSS   XMM0, XMM1
           // Swap w and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(0,2,1,3));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_2_1_3
           // return vResult;
           MOVUPS  [result],XMM0
end;

// Sets the X component of a vector to an integer passed by value

function XMVectorSetIntX(constref V: TXMVECTOR; const x: UINT32): TXMVECTOR; assembler;
asm
           // __m128i vTemp = _mm_cvtsi32_si128(x);
           MOVD    XMM1, [x]
           // vResult = _mm_move_ss(V,_mm_castsi128_ps(vTemp));
           MOVUPS  XMM0,[V]
           MOVSS   XMM0,XMM1
           // return vResult;
           MOVUPS  [result],XMM0
end;

{$IF DEFINED(_XM_SSE4_INTRINSICS_)}
// Sets the Y component of a vector to an integer passed by value
function XMVectorSetIntY(constref V: TXMVECTOR; const y: UINT32): TXMVECTOR; assembler;
asm
           //  __m128i vResult = _mm_castps_si128( V );
           MOVUPS  XMM0,[V]
           //vResult = _mm_insert_epi32( vResult, static_cast<int>(y), 1 );
           PINSRD  XMM0, [y], 1
           //return _mm_castsi128_ps( vResult );
           MOVUPS  [result],XMM0
end;

// Sets the Z component of a vector to an integer passed by value

function XMVectorSetIntZ(constref V: TXMVECTOR; const z: UINT32): TXMVECTOR; assembler;
asm
           // __m128i vResult = _mm_castps_si128( V );
           MOVUPS  XMM0,[V]
           // vResult = _mm_insert_epi32( vResult, static_cast<int>(z), 2 );
           PINSRD  XMM0, [z], 2
           // return _mm_castsi128_ps( vResult );
           MOVUPS  [result],XMM0
end;


// Sets the W component of a vector to an integer passed by value
function XMVectorSetIntW(constref V: TXMVECTOR; const w: UINT32): TXMVECTOR; assembler;
asm
           // vResult = _mm_castps_si128( V );
           MOVUPS  XMM0,[V]
           // vResult = _mm_insert_epi32( vResult, static_cast<int>(w), 3 );
           PINSRD  XMM0, [w], 3
           // return _mm_castsi128_ps( vResult );
           MOVUPS  [result],XMM0
end;

{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorSetIntY(constref V: TXMVECTOR; const y: UINT32): TXMVECTOR; assembler;
asm
           // Swap y and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,2,0,1));
           MOVUPS  XMM0,[V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_2_0_1
           // Convert input to vector
           // __m128i vTemp = _mm_cvtsi32_si128(y);
           MOVD    XMM1, [y]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,_mm_castsi128_ps(vTemp));
           MOVSS   XMM0,XMM1
           // Swap y and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(3,2,0,1));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_2_0_1
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVectorSetIntZ(constref V: TXMVECTOR; const z: UINT32): TXMVECTOR; assembler;
asm
           // Swap z and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,0,1,2));
           MOVUPS  XMM0,[V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_0_1_2
           // Convert input to vector
           // __m128i vTemp = _mm_cvtsi32_si128(z);
           MOVD    XMM1, [z]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,_mm_castsi128_ps(vTemp));
           MOVSS   XMM0,XMM1
           // Swap z and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(3,0,1,2));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_0_1_2
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVectorSetIntW(constref V: TXMVECTOR; const w: UINT32): TXMVECTOR; assembler;
asm
           // Swap w and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,2,1,3));
           MOVUPS  XMM0,[V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_2_1_3
           // Convert input to vector
           // __m128i vTemp = _mm_cvtsi32_si128(w);
           MOVD    XMM1, [w]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,_mm_castsi128_ps(vTemp));
           MOVSS   XMM0,XMM1
           // Swap w and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(0,2,1,3));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_2_1_3
           // return vResult;
           MOVUPS  [result],XMM0
end;


{$ENDIF}

// Sets the X component of a vector to an integer value passed by pointer
function XMVectorSetIntXPtr(constref V: TXMVECTOR; constref x: PUINT32): TXMVECTOR; assembler;
asm
           // vTemp = _mm_load_ss(reinterpret_cast<const float *>(x));
           MOVSS   XMM1, [x]
           MOVUPS  XMM0,[V]
           // vResult = _mm_move_ss(V,vTemp);
           MOVSS   XMM0,XMM1
           // return vResult;
           MOVUPS  [result],XMM0
end;


// Sets the Y component of a vector to an integer value passed by pointer
function XMVectorSetIntYPtr(constref V: TXMVECTOR; constref y: PUINT32): TXMVECTOR; assembler;
asm
           // Swap y and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,2,0,1));
           MOVUPS  XMM0,[V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_2_0_1
           // Convert input to vector
           // vTemp = _mm_load_ss(reinterpret_cast<const float *>(y));
           MOVSS   XMM1,[y]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,vTemp);
           MOVSS   XMM0,XMM1
           // Swap y and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(3,2,0,1));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_2_0_1
           // return vResult;
           MOVUPS  [result],XMM0
end;

// Sets the Z component of a vector to an integer value passed by pointer

function XMVectorSetIntZPtr(constref V: TXMVECTOR; constref z: PUINT32): TXMVECTOR; assembler;
asm
           // Swap z and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,0,1,2));
           MOVUPS  XMM0,[V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_0_1_2
           // Convert input to vector
           // vTemp = _mm_load_ss(reinterpret_cast<const float *>(z));
           MOVSS   XMM1,[z]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,vTemp);
           MOVSS   XMM0,XMM1
           // Swap z and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(3,0,1,2));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_0_1_2
           // return vResult;
           MOVUPS  [result],XMM0
end;

// Sets the W component of a vector to an integer value passed by pointer

function XMVectorSetIntWPtr(constref V: TXMVECTOR; constref w: PUINT32): TXMVECTOR; assembler;
asm
           // Swap w and x
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,2,1,3));
           MOVUPS  XMM0,[V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_2_1_3
           // Convert input to vector
           // vTemp = _mm_load_ss(reinterpret_cast<const float *>(w));
           MOVSS   XMM1,[w]
           // Replace the x component
           // vResult = _mm_move_ss(vResult,vTemp);
           MOVSS   XMM0,XMM1
           // Swap w and x again
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(0,2,1,3));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_2_1_3
           // return vResult;
           MOVUPS  [result],XMM0
end;



//------------------------------------------------------------------------------
// Define a control vector to be used in XMVectorSelect
// operations.  The four integers specified in XMVectorSelectControl
// serve as indices to select between components in two vectors.
// The first index controls selection for the first component of
// the vectors involved in a select operation, the second index
// controls selection for the second component etc.  A value of
// zero for an index causes the corresponding component from the first
// vector to be selected whereas a one causes the component from the
// second vector to be selected instead.
function XMVectorSelectControl(constref VectorIndex0: UINT32; constref VectorIndex1: UINT32; constref VectorIndex2: UINT32;
    constref VectorIndex3: UINT32): TXMVECTOR; assembler;
asm
           // x=Index0,y=Index1,z=Index2,w=Index3
           //__m128i vTemp = _mm_set_epi32(VectorIndex3,VectorIndex2,VectorIndex1,VectorIndex0);
           MOVAPS        XMM0,[VectorIndex3]
           MOVAPS        XMM1,[VectorIndex2]
           MOVAPS        XMM2,[VectorIndex1]
           MOVAPS        XMM3,[VectorIndex0]
           PUNPCKLDQ   XMM3,XMM1
           PUNPCKLDQ   XMM2,XMM0
           PUNPCKLDQ   XMM3,XMM2
           // Any non-zero entries become 0xFFFFFFFF else 0
           //  vTemp = _mm_cmpgt_epi32(vTemp,g_XMZero);
           PCMPGTD XMM3,[g_XMZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // return _mm_castsi128_ps(vTemp);
           MOVUPS  [result],XMM3
end;



function XMVectorSelect(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Control: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp1 = _mm_andnot_ps(Control,V1);
           MOVUPS  XMM0, [Control]
           ANDNPS  XMM0, [V1]
           // vTemp2 = _mm_and_ps(V2,Control);
           MOVUPS  XMM1, [V2]
           ANDPS   XMM1 , [Control]
           // return _mm_or_ps(vTemp1,vTemp2);
           ORPS    XMM0, XMM1
           MOVUPS  [result],XMM0
end;



function XMVectorMergeXY(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  return _mm_unpacklo_ps( V1, V2 );
           MOVUPS  XMM0, [V1]
           UNPCKLPS XMM0, [V2]
           MOVUPS  [result],XMM0
end;



function XMVectorMergeZW(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_unpackhi_ps( V1, V2 );
           MOVUPS  XMM0, [V1]
           UNPCKHPS XMM0, [V2]
           MOVUPS  [result],XMM0
end;


//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------


function XMVectorEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           MOVUPS  XMM0,[v1]
           MOVUPS  XMM2,[v2]
           CMPPS   XMM0, XMM2, 0
           MOVUPS  [Result], XMM0
end;



function XMVectorEqualR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
var
    CR: uint32 = 0;
asm
           // vTemp = _mm_cmpeq_ps(V1,V2);
           MOVUPS  XMM0,[V1]
           CMPPS   XMM0, [V2], 0
           MOVUPS  [result],XMM0
           // int iTest = _mm_movemask_ps(vTemp);
           MOVMSKPS EAX, XMM0
           MOV     EDX,XM_CRMASK_CR6TRUE
           AND     EAX, $F
           CMP     EAX, $F
           JE      @Finished
           // All elements are not greater
           MOV     EDX,XM_CRMASK_CR6FALSE
           @Finished:
           MOV     [pCR],EDX
end;


// Treat the components of the vectors as unsigned integers and
// compare individual bits between the two.  This is useful for
// comparing control vectors and result vectors returned from
// other comparison operations.
function XMVectorEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // __m128i V = _mm_cmpeq_epi32( _mm_castps_si128(V1),_mm_castps_si128(V2) );
           MOVUPS  XMM0,[V1]
           PCMPEQD XMM0, [V2]
           //  return _mm_castsi128_ps(V);
           MOVUPS  [result],XMM0
end;



function XMVectorEqualIntR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // __m128i V = _mm_cmpeq_epi32( _mm_castps_si128(V1),_mm_castps_si128(V2) );
           MOVUPS  XMM0,[V1]
           PCMPEQD XMM0, [V2]
           MOVUPS  [result],XMM0
           // int iTemp = _mm_movemask_ps(_mm_castsi128_ps(V));
           MOVMSKPS EAX, XMM0
           MOV     EDX,XM_CRMASK_CR6TRUE
           AND     EAX, $F
           CMP     EAX, $F
           JE      @Finished
           MOV     EDX,XM_CRMASK_CR6FALSE
           @Finished:
           MOV     [pCR],EDX
end;



function XMVectorNearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Get the difference
           // vDelta = _mm_sub_ps(V1,V2);
           MOVUPS  XMM0,[V1]
           SUBPS   XMM0,[V2]
           // Get the absolute value of the difference
           // vTemp = _mm_setzero_ps();
           XORPS   XMM1,XMM1
           // vTemp = _mm_sub_ps(vTemp,vDelta);
           SUBPS   XMM1,XMM0
           // vTemp = _mm_max_ps(vTemp,vDelta);
           MAXPS   XMM1, XMM0
           // vTemp = _mm_cmple_ps(vTemp,Epsilon);
           CMPPS   XMM1, [Epsilon], 6
           // return vTemp;
           MOVUPS  [result],XMM1
end;



function XMVectorNotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_cmpneq_ps( V1, V2 );
           MOVUPS  XMM0,[V1]
           CMPPS   XMM0, [V2], 4
           MOVUPS  [result],XMM0
end;



function XMVectorNotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
asm
           // __m128i V = _mm_cmpeq_epi32( _mm_castps_si128(V1),_mm_castps_si128(V2) );
           MOVUPS  XMM0,[V1]
           PCMPEQD XMM0, [V2]
           // return _mm_xor_ps(_mm_castsi128_ps(V),g_XMNegOneMask);
           XORPS   XMM0,[g_XMNegOneMask]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           MOVUPS  [result],XMM0
end;



function XMVectorGreater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
asm
           // return _mm_cmpgt_ps( V1, V2 );
           MOVUPS  XMM0,[V2]
           CMPPS   XMM0, [V1], 1

           MOVUPS  [result],XMM0
end;



function XMVectorGreaterR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_cmpgt_ps(V1,V2);
           MOVUPS  XMM0,[V2]
           CMPPS   XMM0, [V1], 1
           MOVUPS  [result],XMM0
           // uint32_t CR = 0;
           //  int iTest = _mm_movemask_ps(vTemp);
           MOVMSKPS EAX, XMM0
           MOV     EDX,XM_CRMASK_CR6TRUE
           AND     EAX, $F
           CMP     EAX, $F
           JE      @Finished
           MOV     EDX,XM_CRMASK_CR6FALSE
           @Finished:
           MOV     [pCR],EDX
end;



function XMVectorGreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
asm
           // return _mm_cmpge_ps( V1, V2 );
           MOVUPS  XMM0,[V2]
           CMPPS   XMM0, [V1], 2
           MOVUPS  [result],XMM0
end;



function XMVectorGreaterOrEqualR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
asm
           // vTemp = _mm_cmpge_ps(V1,V2);
           MOVUPS  XMM0,[v2]
           CMPPS   XMM0, [v1], 2
           MOVUPS  [result],XMM0
           //uint32_t CR = 0;
           //int iTest = _mm_movemask_ps(vTemp);
           MOVMSKPS EAX, XMM0
           MOV     EDX,XM_CRMASK_CR6TRUE
           AND     EAX, $F
           CMP     EAX, $F
           JE      @Finished
           MOV     EDX,XM_CRMASK_CR6FALSE
           @Finished:
           MOV     [pCR],EDX
end;



function XMVectorLess(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_cmplt_ps( V1, V2 );
           MOVUPS  XMM0,[V1]
           CMPPS   XMM0, [V2], 1
           MOVUPS  [result],XMM0
end;



function XMVectorLessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_cmple_ps( V1, V2 );
           MOVUPS  XMM0,[V1]
           CMPPS   XMM0, [V2], 2
           MOVUPS  [result],XMM0
end;



function XMVectorInBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): TXMVECTOR;
asm
           // Test if less than or equal
           // vTemp1 = _mm_cmple_ps(V,Bounds);
           MOVUPS  XMM0, [V]
           MOVUPS  XMM1, [Bounds]
           CMPPS   XMM0,XMM1 , 2
           // Negate the bounds
           // vTemp2 = _mm_mul_ps(Bounds,g_XMNegativeOne);
           MULPS   XMM1,[g_XMNegativeOne]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Test if greater or equal (Reversed)
           // vTemp2 = _mm_cmple_ps(vTemp2,V);
           CMPPS   XMM1,[V] , 2
           // Blend answers
           // vTemp1 = _mm_and_ps(vTemp1,vTemp2);
           ANDPS   XMM0,XMM1
           // return vTemp1;
           MOVUPS  [result], XMM0
end;



function XMVectorInBoundsR(out pCR: UINT32; constref V: TXMVECTOR; constref Bounds: TXMVECTOR): TXMVECTOR;
asm
           // Test if less than or equal
           //  vTemp1 = _mm_cmple_ps(V,Bounds);
           MOVUPS  XMM0,[V]
           CMPPS   XMM0, [Bounds], 2
           // Negate the bounds
           // vTemp2 = _mm_mul_ps(Bounds,g_XMNegativeOne);
           MOVUPS  XMM1,[Bounds]
           MULPS   XMM1,[g_XMNegativeOne]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Test if greater or equal (Reversed)
           // vTemp2 = _mm_cmple_ps(vTemp2,V);
           CMPPS   XMM1, [V], 2
           // Blend answers
           // vTemp1 = _mm_and_ps(vTemp1,vTemp2);
           ANDPS   XMM0, XMM1
           MOVUPS  [result],XMM0
           MOVMSKPS EAX, XMM0

           // All elements are in bounds
           MOV     EDX,XM_CRMASK_CR6TRUE
           AND     EAX, $F
           CMP     EAX, $F
           JE      @Finished
           MOV     EDX,0
           @Finished:
           MOV     [pCR],EDX
end;



function XMVectorIsNaN(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Test against itself. NaN is always not equal
           // return _mm_cmpneq_ps(V,V);
           MOVUPS  XMM0,[V]
           CMPPS   XMM0, XMM0, 4
           MOVUPS  [result],XMM0
end;



function XMVectorIsInfinite(constref V: TXMVECTOR): TXMVECTOR;
asm
           // Mask off the sign bit
           // __m128 vTemp = _mm_and_ps(V,g_XMAbsMask);
           MOVUPS  XMM0, [V]
           ANDPS   XMM0,[g_XMAbsMask]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Compare to infinity
           // vTemp = _mm_cmpeq_ps(vTemp,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 0
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // If any are infinity, the signs are true.
           // return vTemp;
           MOVUPS  [result],XMM0
end;

//------------------------------------------------------------------------------
// Rounding and clamping operations
//------------------------------------------------------------------------------

function XMVectorMin(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  return _mm_min_ps( V1, V2 );
           MOVUPS  XMM0,[V1]
           MINPS   XMM0, [V2]
           MOVUPS  [result],XMM0
end;



function XMVectorMax(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           //   return _mm_max_ps( V1, V2 );
           MOVUPS  XMM0, [V1]
           MAXPS   XMM0, [V2]
           MOVUPS  [result],XMM0
end;


 {$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVectorRound(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_round_ps( V, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC );
           MOVUPS  XMM0, [V]
           ROUNDPS XMM0, XMM0, _MM_FROUND_TO_NEAREST_INT or _MM_FROUND_NO_EXC
           MOVUPS  [result],XMM0
end;



function XMVectorTruncate(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_round_ps( V, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC );
           MOVUPS  XMM0,[V]
           ROUNDPS XMM0, XMM0, _MM_FROUND_TO_ZERO or _MM_FROUND_NO_EXC
           MOVUPS  [result],XMM0
end;



function XMVectorFloor(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_floor_ps( V );
           MOVUPS  XMM0,[V]
           ROUNDPS XMM0, XMM0, _MM_FROUND_TO_NEAREST_INT
           MOVUPS  [result],XMM0
end;



function XMVectorCeiling(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // _mm_ceil_ps( V );
           MOVUPS  XMM0, [V]
           ROUNDPS XMM0, XMM0, _MM_FROUND_TO_POS_INF
           MOVUPS  [result],XMM0
end;

{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorRound(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // __m128 sign = _mm_and_ps( V, g_XMNegativeZero );
           MOVUPS  XMM0, [V]
           MOVUPS  XMM1,XMM0
           MOVUPS  XMM2,XMM0
           ANDPS   XMM0, [g_XMNegativeZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128 sMagic = _mm_or_ps( g_XMNoFraction, sign );
           ORPS    XMM0, [g_XMNoFraction]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128 R1 = _mm_add_ps( V, sMagic );
           ADDPS   XMM1, XMM0
           // R1 = _mm_sub_ps( R1, sMagic );
           SUBPS   XMM1, XMM0
           //__m128 R2 = _mm_and_ps( V, g_XMAbsMask );
           ANDPS   XMM2, [g_XMAbsMask]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128 mask = _mm_cmple_ps( R2, g_XMNoFraction );
           MOVUPS  XMM3,XMM2
           CMPPS   XMM3, [g_XMNoFraction], 2
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // R2 = _mm_andnot_ps(mask,V);
           MOVUPS  XMM2,XMM3
           ANDNPS  XMM2, [V]
           // R1 = _mm_and_ps(R1,mask);
           ANDPS   XMM1, XMM3
           // vResult = _mm_xor_ps(R1, R2);
           XORPS   XMM1,XMM2
           // return vResult;
           MOVUPS  [result],XMM1
end;



function XMVectorTruncate(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // To handle NAN, INF and numbers greater than 8388608, use masking
           // Get the abs value
           //__m128i vTest = _mm_and_si128(_mm_castps_si128(V),g_XMAbsMask);
           MOVUPS  XMM0, [V]
           PAND    XMM0, [g_XMAbsMask]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Test for greater than 8388608 (All floats with NO fractionals, NAN and INF
           // vTest = _mm_cmplt_epi32(vTest,g_XMNoFraction);
           PCMPGTD XMM0, [g_XMNoFraction]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Convert to int and back to float for rounding with truncation
           // __m128i vInt = _mm_cvttps_epi32(V);
           CVTTPS2DQ XMM1, [V]
           // Convert back to floats
           // vResult = _mm_cvtepi32_ps(vInt);
           CVTDQ2PS XMM1, XMM1
           // All numbers less than 8388608 will use the round to int
           // vResult = _mm_and_ps(vResult,_mm_castsi128_ps(vTest));
           ANDPS   XMM1,XMM0
           // All others, use the ORIGINAL value
           // vTest = _mm_andnot_si128(vTest,_mm_castps_si128(V));
           PANDN   XMM0, [V]
           // vResult = _mm_or_ps(vResult,_mm_castsi128_ps(vTest));
           ORPS    XMM1, XMM0
           // return vResult;
           MOVUPS  [result],XMM1
end;



function XMVectorFloor(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // To handle NAN, INF and numbers greater than 8388608, use masking
           // __m128i vTest = _mm_and_si128(_mm_castps_si128(V),g_XMAbsMask);
           MOVUPS  XMM0, [V]
           PAND    XMM0, [g_XMAbsMask]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // vTest = _mm_cmplt_epi32(vTest,g_XMNoFraction);
           PCMPGTD XMM0, [g_XMNoFraction]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Truncate
           //__m128i vInt = _mm_cvttps_epi32(V);
           CVTTPS2DQ XMM1, [V]
           // vResult = _mm_cvtepi32_ps(vInt);
           CVTDQ2PS XMM1, XMM1
           // __m128 vLarger = _mm_cmpgt_ps( vResult, V );
           MOVUPS  XMM2,[V]
           CMPPS   XMM2, XMM1, 1  // vLarger = XMM2
           // 0 -> 0, 0xffffffff -> -1.0f
           // vLarger = _mm_cvtepi32_ps( _mm_castps_si128( vLarger ) );
           CVTDQ2PS XMM2, XMM2
           // vResult = _mm_add_ps( vResult, vLarger );
           ADDPS   XMM1,XMM2
           // All numbers less than 8388608 will use the round to int
           // vResult = _mm_and_ps(vResult,_mm_castsi128_ps(vTest));
           ANDPS   XMM1, XMM0
           // All others, use the ORIGINAL value
           // vTest = _mm_andnot_si128(vTest,_mm_castps_si128(V));
           PANDN   XMM0, [V]
           // vResult = _mm_or_ps(vResult,_mm_castsi128_ps(vTest));
           ORPS    XMM1, XMM0
           // return vResult;
           MOVUPS  [result],XMM1
end;



function XMVectorCeiling(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // To handle NAN, INF and numbers greater than 8388608, use masking
           // __m128i vTest = _mm_and_si128(_mm_castps_si128(V),g_XMAbsMask);
           MOVUPS  XMM0,[V]
           PAND    XMM0, [g_XMAbsMask]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // vTest = _mm_cmplt_epi32(vTest,g_XMNoFraction);
           PCMPGTD XMM0, [g_XMNoFraction]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Truncate
           // __m128i vInt = _mm_cvttps_epi32(V);
           CVTTPS2DQ XMM1, [V]
           // vResult = _mm_cvtepi32_ps(vInt);
           CVTDQ2PS XMM1, XMM1
           // __m128 vSmaller = _mm_cmplt_ps( vResult, V );
           MOVUPS  XMM2,XMM1
           CMPPS   XMM2, [V], 1 // vSmaller = XMM2
           // 0 -> 0, 0xffffffff -> -1.0f
           // vSmaller = _mm_cvtepi32_ps( _mm_castps_si128( vSmaller ) );
           CVTDQ2PS XMM2, XMM2
           // vResult = _mm_sub_ps( vResult, vSmaller );
           SUBPS   XMM1, XMM2
           // All numbers less than 8388608 will use the round to int
           // vResult = _mm_and_ps(vResult,_mm_castsi128_ps(vTest));
           ANDPS   XMM1, XMM0
           // All others, use the ORIGINAL value
           // vTest = _mm_andnot_si128(vTest,_mm_castps_si128(V));
           PANDN   XMM0, [V]
           // vResult = _mm_or_ps(vResult,_mm_castsi128_ps(vTest));
           ORPS    XMM1, XMM0
           // return vResult;
           MOVUPS  [result],XMM1
end;

{$ENDIF}

function XMVectorClamp(constref V: TXMVECTOR; constref Min: TXMVECTOR; constref Max: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vResult = _mm_max_ps(Min,V);
           MOVUPS  XMM0, [Min]
           MAXPS   XMM0, [V]
           // vResult = _mm_min_ps(vResult,Max);
           MINPS   XMM0, [Max]
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVectorSaturate(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Set <0 to 0
           // vResult = _mm_max_ps(V,g_XMZero);
           MOVUPS  XMM0,[V]
           MAXPS   XMM0, [g_XMZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Set>1 to 1
           // return _mm_min_ps(vResult,g_XMOne);
           MINPS   XMM0, [g_XMOne]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           MOVUPS  [result],XMM0
end;


//------------------------------------------------------------------------------
// Bitwise logical operations
//------------------------------------------------------------------------------

function XMVectorAndInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_and_ps(V1,V2);
           MOVUPS  XMM0,[V1]
           ANDPS   XMM0, [V2]
           MOVUPS  [result],XMM0
end;



function XMVectorAndCInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // __m128i V = _mm_andnot_si128( _mm_castps_si128(V2), _mm_castps_si128(V1) );
           MOVUPS  XMM0,[v1]
           PANDN   XMM0, [V2]
           //  return _mm_castsi128_ps(V);
           MOVUPS  [result], XMM0
end;



function XMVectorOrInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // __m128i V = _mm_or_si128( _mm_castps_si128(V1), _mm_castps_si128(V2) );
           MOVUPS  XMM0,[V1]
           POR     XMM0, [V2]
           // return _mm_castsi128_ps(V);
           MOVUPS  [result],XMM0
end;



function XMVectorNorInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // __m128i Result;
           // Result = _mm_or_si128( _mm_castps_si128(V1), _mm_castps_si128(V2) );
           MOVUPS  XMM0,[V1]
           POR     XMM0, [V2]
           // Result = _mm_andnot_si128( Result,g_XMNegOneMask);
           PANDN   XMM0, [g_XMNegOneMask]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // return _mm_castsi128_ps(Result);
           MOVUPS  [result],XMM0
end;



function XMVectorXorInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // __m128i V = _mm_xor_si128( _mm_castps_si128(V1), _mm_castps_si128(V2) );
           MOVUPS  XMM0, [V1]
           PXOR    XMM0, [V2]
           // return _mm_castsi128_ps(V);
           MOVUPS  [result],XMM0
end;



function XMVectorAdd(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_add_ps( V1, V2 );
           MOVUPS  XMM0,[V1]
           ADDPS   XMM0,[V2]
           MOVUPS  [result],XMM0
end;



{$IF DEFINED(_XM_SSE3_INTRINSICS_)}

function XMVectorSum(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_hadd_ps(V, V);
           MOVUPS  XMM0,[V]
           HADDPS  XMM0, XMM0
           // return _mm_hadd_ps(vTemp,vTemp);
           HADDPS  XMM0, XMM0
           MOVUPS  [result],XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorSum(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = XM_PERMUTE_PS(V, _MM_SHUFFLE(2, 3, 0, 1));
           MOVUPS  XMM0,[V]
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_3_0_1
           // vTemp2 = _mm_add_ps(V, vTemp);
           ADDPS   XMM1, XMM0
           // vTemp = XM_PERMUTE_PS(vTemp2, _MM_SHUFFLE(1, 0, 3, 2));
           MOVUPS  XMM0, XMM1
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_0_3_2
           // return _mm_add_ps(vTemp, vTemp2);
           ADDPS   XMM0, XMM1
           MOVUPS  [result],XMM0
end;
{$ENDIF}



function XMVectorAddAngles(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Adjust the angles
           // vResult = _mm_add_ps(V1,V2);
           MOVUPS  XMM0,[V1]
           ADDPS   XMM0,[V2]

           // Less than Pi?
           // vOffset = _mm_cmplt_ps(vResult,g_XMNegativePi);
           MOVUPS  XMM1,XMM0
           CMPPS   XMM1, [g_XMNegativePi], 1
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

           // vOffset = _mm_and_ps(vOffset,g_XMTwoPi);
           ANDPS   XMM1, [g_XMTwoPi]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Add 2Pi to all entries less than -Pi
           // vResult = _mm_add_ps(vResult,vOffset);
           ADDPS   XMM0, XMM1
           // Greater than or equal to Pi?
           // vOffset = _mm_cmpge_ps(vResult,g_XMPi);
           MOVUPS  XMM1,XMM0
           CMPPS   XMM1, [g_XMPi], 2
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // vOffset = _mm_and_ps(vOffset,g_XMTwoPi);
           ANDPS   XMM1, [g_XMTwoPi]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Sub 2Pi to all entries greater than Pi
           // vResult = _mm_sub_ps(vResult,vOffset);
           SUBPS   XMM0, XMM1
           // return vResult;
           MOVUPS  [result], XMM0
end;



function XMVectorSubtract(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_sub_ps( V1, V2 );
           MOVUPS  XMM0,[v1]
           SUBPS   XMM0,[v2]
           MOVUPS  [result],XMM0
end;



function XMVectorSubtractAngles(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Adjust the angles
           // vResult = _mm_sub_ps(V1,V2);
           MOVUPS  XMM0, [V1]
           SUBPS   XMM0, [V2]
           // Less than Pi?
           // vOffset = _mm_cmplt_ps(vResult,g_XMNegativePi);
           MOVUPS  XMM1,XMM0
           CMPPS   XMM1, [g_XMNegativePi], 1
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           //  vOffset = _mm_and_ps(vOffset,g_XMTwoPi);
           ANDPS   XMM1, [g_XMTwoPi]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Add 2Pi to all entries less than -Pi
           // vResult = _mm_add_ps(vResult,vOffset);
           ADDPS   XMM0,XMM1
           // Greater than or equal to Pi?
           // vOffset = _mm_cmpge_ps(vResult,g_XMPi);
           MOVUPS  XMM1, XMM0
           CMPPS   XMM1, [g_XMPi], 2
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // vOffset = _mm_and_ps(vOffset,g_XMTwoPi);
           ANDPS   XMM1, [g_XMTwoPi]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // Sub 2Pi to all entries greater than Pi
           // vResult = _mm_sub_ps(vResult,vOffset);
           SUBPS   XMM0, XMM1
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVectorMultiply(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_mul_ps( V1, V2 );
           MOVUPS  XMM0,[V1]
           MULPS   XMM0, [V2]
           MOVUPS  [result],XMM0

end;

{$IF DEFINED(_XM_FMA3_INTRINSICS_)}

function XMVectorMultiplyAdd(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR; assembler;
asm
           // _mm_fmadd_ps( V1, V2, V3 );
           MOVUPS  XMM0,[V1]
           MOVUPS  XMM2, [V2]
           MOVUPS  XMM1,[V3]
           VFMADD132PS XMM0, XMM1, XMM2
           // Multiply packed single-precision floating-point values from xmm0 and xmm2/mem, add to xmm1 and put result in xmm0.
           MOVUPS  [result],XMM0

end;



function XMVectorNegativeMultiplySubtract(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return  _mm_fnmadd_ps(V1, V2, V3);
           MOVUPS  XMM0,[V1]
           MOVUPS  XMM2,[V2]
           MOVUPS  XMM1, [V3]
           VFNMADD132PS XMM0, XMM1, XMM2
           // Multiply packed single-precision floating-point val-ues from xmm0 and xmm2/mem, negate the multi-plication result and add to xmm1 and put result in xmm0.
           MOVUPS  [result],XMM0
end;

{$ELSE}// _XM_SSE_INTRINSICS_


function XMVectorMultiplyAdd(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vResult = _mm_mul_ps( V1, V2 );
           MOVUPS  XMM0,[v1]
           MULPS   XMM0, [v2]
           // return _mm_add_ps(vResult, V3 );
           ADDPS   XMM0,[v3]
           MOVUPS  [result],XMM0
end;



function XMVectorNegativeMultiplySubtract(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR; assembler;
asm
           // R = _mm_mul_ps( V1, V2 );
           MOVUPS  XMM1,[V1]
           MULPS   XMM1, [V2]
           // return _mm_sub_ps( V3, R );
           MOVUPS  XMM0,[V3]
           SUBPS   XMM0,XMM1
           MOVUPS  [result],XMM0
end;

{$ENDIF}


function XMVectorScale(constref V: TXMVECTOR; constref ScaleFactor: single): TXMVECTOR; assembler;
asm
           // vResult = _mm_set_ps1(ScaleFactor);
           MOVSS   XMM1, [ScaleFactor]
           SHUFPS  XMM1, XMM1, 0
           // return _mm_mul_ps(vResult,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM1
           MOVUPS  [result],XMM0
end;



function XMVectorReciprocalEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_rcp_ps(V);
           RCPPS   XMM0, [V]
           MOVUPS  [result],XMM0
end;




function XMVectorReciprocal(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_div_ps(g_XMOne,V);
           MOVUPS  XMM0,[g_XMOne]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           DIVPS   XMM0, [V]
           MOVUPS  [result],XMM0
end;


// Return an estimated square root
function XMVectorSqrtEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_sqrt_ps(V);
           MOVUPS  XMM0,[V]
           SQRTPS  XMM0, XMM0
           MOVUPS  [result],XMM0
end;



function XMVectorSqrt(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_sqrt_ps(V);
           MOVUPS  XMM0,[V]
           SQRTPS  XMM0, XMM0
           MOVUPS  [result],XMM0
end;



function XMVectorReciprocalSqrtEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_rsqrt_ps(V);
           MOVUPS  XMM0,[V]
           RSQRTPS XMM0, XMM0
           MOVUPS  [result],XMM0
end;



function XMVectorReciprocalSqrt(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vResult = _mm_sqrt_ps(V);
           MOVUPS  XMM1,[V]
           SQRTPS  XMM1, XMM1
           // vResult = _mm_div_ps(g_XMOne,vResult);
           MOVUPS  XMM0,[g_XMOne]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           DIVPS   XMM0, XMM1
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVectorExp2(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // __m128i itrunc = _mm_cvttps_epi32(V);
           MOVUPS  XMM4,[V]
           CVTTPS2DQ XMM4, XMM4 // itrunc = xmm4
           // __m128 ftrunc = _mm_cvtepi32_ps(itrunc);
           CVTDQ2PS XMM0, XMM4
           // __m128 y = _mm_sub_ps(V, ftrunc);
           MOVUPS  XMM1,[V]
           SUBPS   XMM1,XMM0  // y = XMM1
           // __m128 poly = _mm_mul_ps(g_XMExpEst7, y);
           MOVUPS  XMM0, [g_XMExpEst7]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           MULPS   XMM0, XMM1 // poly = XMM0
           // poly = _mm_add_ps(g_XMExpEst6, poly);
           ADDPS   XMM0, [g_XMExpEst6]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // poly = _mm_mul_ps(poly, y);
           MULPS   XMM0, XMM1
           // poly = _mm_add_ps(g_XMExpEst5, poly);
           ADDPS   XMM0,[g_XMExpEst5]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // poly = _mm_mul_ps(poly, y);
           MULPS   XMM0, XMM1
           // poly = _mm_add_ps(g_XMExpEst4, poly);
           ADDPS   XMM0, [g_XMExpEst4]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // poly = _mm_mul_ps(poly, y);
           MULPS   XMM0, XMM1
           // poly = _mm_add_ps(g_XMExpEst3, poly);
           ADDPS   XMM0, [g_XMExpEst3]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // poly = _mm_mul_ps(poly, y);
           MULPS   XMM0, XMM1
           // poly = _mm_add_ps(g_XMExpEst2, poly);
           ADDPS   XMM0,[g_XMExpEst2]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // poly = _mm_mul_ps(poly, y);
           MULPS   XMM0, XMM1
           // poly = _mm_add_ps(g_XMExpEst1, poly);
           ADDPS   XMM0, [g_XMExpEst1]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // poly = _mm_mul_ps(poly, y);
           MULPS   XMM0, XMM1
           // poly = _mm_add_ps(g_XMOne, poly);
           ADDPS   XMM0,[g_XMOne]  // XMM0 = poly, XMM4 = itrunc
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

           //__m128i biased = _mm_add_epi32(itrunc, g_XMExponentBias);
           MOVUPS  XMM1, XMM4
           PADDD   XMM1, [g_XMExponentBias]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // biased = _mm_slli_epi32(biased, 23);
           PSLLD   XMM1, 23
           //__m128 result0 = _mm_div_ps(_mm_castsi128_ps(biased), poly);
           DIVPS   XMM1, XMM0 // XMM0 = poly, XMM1 = result0,  XMM4 = itrunc

           // biased = _mm_add_epi32(itrunc, g_XM253);
           MOVUPS  XMM2, XMM4
           PADDD   XMM2, [g_XM253]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // biased = _mm_slli_epi32(biased, 23);
           PSLLD   XMM2, 23
           // __m128 result1 = _mm_div_ps(_mm_castsi128_ps(biased), poly);
           DIVPS   XMM2, XMM0
           // result1 = _mm_mul_ps(g_XMMinNormal.v, result1);
           MULPS   XMM2, [g_XMMinNormal] // XMM1 = result0, XMM2 = result1, XMM4 = itrunc
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

           // Use selection to handle the cases
           //  if (V is NaN) -> QNaN;
           //  else if (V sign bit set)
           //      if (V > -150)
           //         if (V.exponent < -126) -> result1
           //         else -> result0
           //      else -> +0
           //  else
           //      if (V < 128) -> result0
           //      else -> +inf

           // __m128i comp = _mm_cmplt_epi32( _mm_castps_si128(V), g_XMBin128);
           MOVUPS  XMM0,[V]
           PCMPGTD XMM0, [g_XMBin128]  // XMM0 = comp
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i select0 = _mm_and_si128(comp, _mm_castps_si128(result0));
           MOVUPS  XMM3, XMM0
           PAND    XMM3, XMM1   // select0 = XMM3
           // __m128i select1 = _mm_andnot_si128(comp, g_XMInfinity);
           MOVUPS  XMM5, XMM0
           PANDN   XMM5, [g_XMInfinity]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i result2 = _mm_or_si128(select0, select1);
           POR     XMM3, XMM5      // result2 = XMM3

           // comp = _mm_cmplt_epi32(itrunc, g_XMSubnormalExponent);
           PCMPGTD XMM4, [g_XMSubnormalExponent] // comp = XMM4
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // select1 = _mm_and_si128(comp, _mm_castps_si128(result1));
           MOVUPS  XMM5, XMM4
           PAND    XMM5, XMM2
           // select0 = _mm_andnot_si128(comp, _mm_castps_si128(result0));
           PANDN   XMM6, XMM4
           // __m128i result3 = _mm_or_si128(select0, select1);
           POR     XMM6, XMM5 // XMM6 = result3

           // comp = _mm_cmplt_epi32(_mm_castps_si128(V), g_XMBinNeg150);
           MOVUPS  XMM0, [V]
           PCMPGTD XMM0, [g_XMBinNeg150]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // select0 = _mm_and_si128(comp, result3);
           MOVUPS  XMM1, XMM0
           PAND    XMM1, XMM6
           // select1 = _mm_andnot_si128(comp, g_XMZero);
           PANDN   XMM0, [g_XMZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i result4 = _mm_or_si128(select0, select1);
           POR     XMM0, XMM1 // XMM0 = result4, result2 = XMM3

           // __m128i sign = _mm_and_si128(_mm_castps_si128(V), g_XMNegativeZero);
           MOVUPS  XMM1, [V]
           PAND    XMM1, [g_XMNegativeZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // comp = _mm_cmpeq_epi32(sign, g_XMNegativeZero);
           PCMPEQD XMM1, [g_XMNegativeZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // select0 = _mm_and_si128(comp, result4);
           PAND    XMM0, XMM1
           // select1 = _mm_andnot_si128(comp, result2);
           PANDN   XMM3, XMM1
           // __m128i result5 = _mm_or_si128(select0, select1);
           POR     XMM0, XMM3 // XMM0 = result5

           // __m128i t0 = _mm_and_si128(_mm_castps_si128(V), g_XMQNaNTest);
           MOVUPS  XMM1,[V]
           MOVUPS  XMM2,XMM1
           PAND    XMM1, [g_XMQNaNTest]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i t1 = _mm_and_si128(_mm_castps_si128(V), g_XMInfinity);
           PAND    XMM2, [g_XMInfinity]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // t0 = _mm_cmpeq_epi32(t0, g_XMZero);
           PCMPEQD XMM1, [g_XMZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // t1 = _mm_cmpeq_epi32(t1, g_XMInfinity);
           PCMPEQD XMM2, [g_XMInfinity]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i isNaN = _mm_andnot_si128(t0, t1);
           PANDN   XMM1, XMM2 // XMM1 = isNaN

           // select0 = _mm_and_si128(isNaN, g_XMQNaN);
           MOVUPS  XMM2,XMM1
           PAND    XMM1, [g_XMQNaN]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // select1 = _mm_andnot_si128(isNaN, result5);
           PANDN   XMM2, XMM0
           // __m128i vResult = _mm_or_si128(select0, select1);
           POR     XMM1, XMM2

           // return _mm_castsi128_ps(vResult);
           MOVUPS  [result],XMM1
end;



function XMVectorExpE(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // expE(V) = exp2(vin*log2(e))
           //__m128 Ve = _mm_mul_ps(g_XMLgE, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, [g_XMLgE]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

           //__m128i itrunc = _mm_cvttps_epi32(Ve);
           CVTTPS2DQ XMM1, XMM0  // XMM0 = Ve, XMM1 = itrunc
           // __m128 ftrunc = _mm_cvtepi32_ps(itrunc);
           CVTDQ2PS XMM2, XMM1
           // __m128 y = _mm_sub_ps(Ve, ftrunc);
           MOVUPS  XMM3,XMM0
           SUBPS   XMM3, XMM2  // XMM0 = Ve, XMM1 = itrunc, XMM3 = y
           // __m128 poly = _mm_mul_ps(g_XMExpEst7, y);
           MOVUPS  XMM2, [g_XMExpEst7]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           MULPS   XMM2, XMM3
           // poly = _mm_add_ps(g_XMExpEst6, poly);
           ADDPS   XMM2, [g_XMExpEst6]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // poly = _mm_mul_ps(poly, y);
           MULPS   XMM2, XMM3
           // poly = _mm_add_ps(g_XMExpEst5, poly);
           ADDPS   XMM2, [g_XMExpEst5]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // poly = _mm_mul_ps(poly, y);
           MULPS   XMM2, XMM3
           // poly = _mm_add_ps(g_XMExpEst4, poly);
           ADDPS   XMM2, [g_XMExpEst4]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // poly = _mm_mul_ps(poly, y);
           MULPS   XMM2, XMM3
           // poly = _mm_add_ps(g_XMExpEst3, poly);
           ADDPS   XMM2, [g_XMExpEst3]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // poly = _mm_mul_ps(poly, y);
           MULPS   XMM2, XMM3
           // poly = _mm_add_ps(g_XMExpEst2, poly);
           ADDPS   XMM2, [g_XMExpEst2]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           //poly = _mm_mul_ps(poly, y);
           MULPS   XMM2, XMM3
           // poly = _mm_add_ps(g_XMExpEst1, poly);
           ADDPS   XMM2, [g_XMExpEst1]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // poly = _mm_mul_ps(poly, y);
           MULPS   XMM2, XMM3
           // poly = _mm_add_ps(g_XMOne, poly);
           ADDPS   XMM2, [g_XMOne] // XMM0 = Ve, XMM1 = itrunc, XMM2 = poly
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

           // __m128i biased = _mm_add_epi32(itrunc, g_XMExponentBias);
           MOVUPS  XMM3, [g_XMExponentBias]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           PADDD   XMM3, XMM1
           // biased = _mm_slli_epi32(biased, 23);
           PSLLD   XMM3, 23
           // __m128 result0 = _mm_div_ps(_mm_castsi128_ps(biased), poly);
           DIVPS   XMM3, XMM2

           // biased = _mm_add_epi32(itrunc, g_XM253);
           MOVUPS  XMM4, [g_XM253]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           PADDD   XMM4, XMM1
           // biased = _mm_slli_epi32(biased, 23);
           PSLLD   XMM4, 23
           //__m128 result1 = _mm_div_ps(_mm_castsi128_ps(biased), poly);
           DIVPS   XMM4, XMM2
           // result1 = _mm_mul_ps(g_XMMinNormal.v, result1);
           MULPS   XMM4, [g_XMMinNormal] // XMM0 = Ve, XMM1 = itrunc,  xmm3 = result0, xmm4 = result1
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

           // Use selection to handle the cases
           //  if (V is NaN) -> QNaN;
           //  else if (V sign bit set)
           //      if (V > -150)
           //         if (V.exponent < -126) -> result1
           //         else -> result0
           //      else -> +0
           //  else
           //      if (V < 128) -> result0
           //      else -> +inf

           //__m128i comp = _mm_cmplt_epi32( _mm_castps_si128(Ve), g_XMBin128);
           MOVUPS  XMM2, XMM0
           PCMPGTD XMM2, [g_XMBin128]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i select0 = _mm_and_si128(comp, _mm_castps_si128(result0));
           MOVUPS  XMM5, XMM2
           PAND    XMM5, XMM3
           // __m128i select1 = _mm_andnot_si128(comp, g_XMInfinity);
           PANDN   XMM2, [g_XMInfinity]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           //__m128i result2 = _mm_or_si128(select0, select1);
           POR     XMM5, XMM2  // XMM0 = Ve, XMM1 = itrunc, xmm3 = result0, xmm4 = result1, XMM5 = result2

           // comp = _mm_cmplt_epi32(itrunc, g_XMSubnormalExponent);
           PCMPGTD XMM1, [g_XMSubnormalExponent]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           MOVUPS  XMM2, XMM1
           // select1 = _mm_and_si128(comp, _mm_castps_si128(result1));
           PAND    XMM1, XMM4
           // select0 = _mm_andnot_si128(comp, _mm_castps_si128(result0));
           PANDN   XMM2, XMM3
           // __m128i result3 = _mm_or_si128(select0, select1);
           POR     XMM2, XMM1 // XMM0 = Ve, xmm2 = result3, XMM5 = result2

           // comp = _mm_cmplt_epi32(_mm_castps_si128(Ve), g_XMBinNeg150);
           MOVUPS  XMM1, XMM0
           PCMPGTD XMM1, [g_XMBinNeg150]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // select0 = _mm_and_si128(comp, result3);
           PAND    XMM2, XMM1
           // select1 = _mm_andnot_si128(comp, g_XMZero);
           MOVUPS  XMM3, [g_XMZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           PANDN   XMM3, XMM1
           // __m128i result4 = _mm_or_si128(select0, select1);
           POR     XMM2, XMM3    // XMM0 = Ve, xmm2 = result4, XMM5 = result2

           // __m128i sign = _mm_and_si128(_mm_castps_si128(Ve), g_XMNegativeZero);
           MOVUPS  XMM1, XMM0
           PAND    XMM1, [g_XMNegativeZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // comp = _mm_cmpeq_epi32(sign, g_XMNegativeZero);
           PCMPEQD XMM1, [g_XMNegativeZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           MOVUPS  XMM3, XMM1
           // select0 = _mm_and_si128(comp, result4);
           PAND    XMM1, XMM2
           // select1 = _mm_andnot_si128(comp, result2);
           PANDN   XMM3, XMM5
           // __m128i result5 = _mm_or_si128(select0, select1);
           POR     XMM1, XMM3 // XMM0 = Ve, XMM1 = result5

           // __m128i t0 = _mm_and_si128(_mm_castps_si128(Ve), g_XMQNaNTest);
           MOVUPS  XMM2, XMM0
           PAND    XMM2, [g_XMQNaNTest]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i t1 = _mm_and_si128(_mm_castps_si128(Ve), g_XMInfinity);
           MOVUPS  XMM3, XMM0
           PAND    XMM3, [g_XMInfinity]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // t0 = _mm_cmpeq_epi32(t0, g_XMZero);
           PCMPEQD XMM2, [g_XMZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // t1 = _mm_cmpeq_epi32(t1, g_XMInfinity);
           PCMPEQD XMM3, [g_XMInfinity]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i isNaN = _mm_andnot_si128(t0, t1);
           PANDN   XMM2, XMM3  // XMM1 = result5 ,xmm2 = isNaN

           // select0 = _mm_and_si128(isNaN, g_XMQNaN);
           MOVUPS  XMM0, XMM2
           PAND    XMM0, [g_XMQNaN]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // select1 = _mm_andnot_si128(isNaN, result5);
           PANDN   XMM2, XMM1
           // __m128i vResult = _mm_or_si128(select0, select1);
           POR     XMM0, XMM2

           // return _mm_castsi128_ps(vResult);
           MOVUPS  [result],XMM0
end;




function multi_sll_epi32(Value, Count: TXMVECTOR {__m128i}): TXMVECTOR {__m128i}; assembler;
asm

           // __m128i v = _mm_shuffle_epi32(value, _MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM0, [value]
           PSHUFD  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // __m128i c = _mm_shuffle_epi32(count, _MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM1, [count]
           PSHUFD  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0
           // c = _mm_and_si128(c, g_XMMaskX);
           PAND    XMM1, [g_XMMaskX]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i r0 = _mm_sll_epi32(v, c);
           PSLLD   XMM0, XMM1 // XMM0 = r0

           // v = _mm_shuffle_epi32(value, _MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1, [value]
           PSHUFD  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // c = _mm_shuffle_epi32(count, _MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM2, [count]
           PSHUFD  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
           // c = _mm_and_si128(c, g_XMMaskX);
           PAND    XMM2, [g_XMMaskX]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i r1 = _mm_sll_epi32(v, c);
           PSLLD   XMM1, XMM2 // XMM1 = r1

           // v = _mm_shuffle_epi32(value, _MM_SHUFFLE(2,2,2,2));
           MOVUPS  XMM2, [value]
           PSHUFD  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
           // c = _mm_shuffle_epi32(count, _MM_SHUFFLE(2,2,2,2));
           MOVUPS  XMM3, [count]
           PSHUFD  XMM3, XMM3, _MM_SHUFFLE_2_2_2_2
           // c = _mm_and_si128(c, g_XMMaskX);
           PAND    XMM3, [g_XMMaskX]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i r2 = _mm_sll_epi32(v, c);
           PSLLD   XMM2, XMM3 // XMM2 = r2

           // v = _mm_shuffle_epi32(value, _MM_SHUFFLE(3,3,3,3));
           MOVUPS  XMM3, [value]
           PSHUFD  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           // c = _mm_shuffle_epi32(count, _MM_SHUFFLE(3,3,3,3));
           MOVUPS  XMM4, [count]
           PSHUFD  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3
           // c = _mm_and_si128(c, g_XMMaskX);
           PAND    XMM4, [g_XMMaskX]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i r3 = _mm_sll_epi32(v, c);
           PSLLD   XMM3, XMM4 // XMM3 = r3

           // (r0,r0,r1,r1)
           // __m128 r01 = _mm_shuffle_ps(_mm_castsi128_ps(r0), _mm_castsi128_ps(r1), _MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM1, _MM_SHUFFLE_0_0_0_0
           // (r2,r2,r3,r3)
           // __m128 r23 = _mm_shuffle_ps(_mm_castsi128_ps(r2), _mm_castsi128_ps(r3), _MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM2, XMM3, _MM_SHUFFLE_0_0_0_0
           // (r0,r1,r2,r3)
           // __m128 result = _mm_shuffle_ps(r01, r23, _MM_SHUFFLE(2,0,2,0));
           SHUFPS  XMM0, XMM2, _MM_SHUFFLE_2_0_2_0
           // return _mm_castps_si128(result);
           MOVUPS  [result],XMM0
end;



function multi_srl_epi32(Value, Count: TXMVECTOR): TXMVECTOR; assembler;
asm
           // __m128i v = _mm_shuffle_epi32(value, _MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM0, [value]
           PSHUFD  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // __m128i c = _mm_shuffle_epi32(count, _MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM1, [count]
           PSHUFD  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0
           // c = _mm_and_si128(c, g_XMMaskX);
           PAND    XMM1, [g_XMMaskX]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i r0 = _mm_srl_epi32(v, c);
           PSRLD   XMM0, XMM1

           // v = _mm_shuffle_epi32(value, _MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1, [value]
           PSHUFD  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // c = _mm_shuffle_epi32(count, _MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM2, [count]
           PSHUFD  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
           // c = _mm_and_si128(c, g_XMMaskX);
           PAND    XMM2, [g_XMMaskX]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i r1 = _mm_srl_epi32(v, c);
           PSRLD   XMM1, XMM2

           // v = _mm_shuffle_epi32(value, _MM_SHUFFLE(2,2,2,2));
           MOVUPS  XMM2, [value]
           PSHUFD  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
           // c = _mm_shuffle_epi32(count, _MM_SHUFFLE(2,2,2,2));
           MOVUPS  XMM3, [count]
           PSHUFD  XMM3, XMM3, _MM_SHUFFLE_2_2_2_2
           // c = _mm_and_si128(c, g_XMMaskX);
           PAND    XMM3, [g_XMMaskX]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i r2 = _mm_srl_epi32(v, c);
           PSRLD   XMM2, XMM3

           // v = _mm_shuffle_epi32(value, _MM_SHUFFLE(3,3,3,3));
           MOVUPS  XMM3, [value]
           PSHUFD  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           // c = _mm_shuffle_epi32(count, _MM_SHUFFLE(3,3,3,3));
           MOVUPS  XMM4, [count]
           PSHUFD  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3
           // c = _mm_and_si128(c, g_XMMaskX);
           PAND    XMM4, [g_XMMaskX]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128i r3 = _mm_srl_epi32(v, c);
           PSRLD   XMM3, XMM4

           // (r0,r0,r1,r1)
           // __m128 r01 = _mm_shuffle_ps(_mm_castsi128_ps(r0), _mm_castsi128_ps(r1), _MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM1, _MM_SHUFFLE_0_0_0_0
           // (r2,r2,r3,r3)
           // __m128 r23 = _mm_shuffle_ps(_mm_castsi128_ps(r2), _mm_castsi128_ps(r3), _MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM2, XMM3, _MM_SHUFFLE_0_0_0_0
           // (r0,r1,r2,r3)
           // __m128 result = _mm_shuffle_ps(r01, r23, _MM_SHUFFLE(2,0,2,0));
           SHUFPS  XMM0, XMM2, _MM_SHUFFLE_2_0_2_0
           // return _mm_castps_si128(result);
           MOVUPS  [result],XMM0
end;



function GetLeadingBit(const Value: TXMVECTOR): TXMVECTOR; inline;
const
    g_XM0000FFFF: TXMVECTORI32 = (i: ($0000FFFF, $0000FFFF, $0000FFFF, $0000FFFF));
    g_XM000000FF: TXMVECTORI32 = (i: ($000000FF, $000000FF, $000000FF, $000000FF));
    g_XM0000000F: TXMVECTORI32 = (i: ($0000000F, $0000000F, $0000000F, $0000000F));
    g_XM00000003: TXMVECTORI32 = (i: ($00000003, $00000003, $00000003, $00000003));
var
    v, r, s: TXMVECTOR;
begin
    asm
               //__m128i v = value, r, c, b, s;

               // c = _mm_cmpgt_epi32(v, g_XM0000FFFF);   // c = (v > 0xFFFF)
               MOVUPS  XMM0, [value]
               MOVUPS  [v], XMM0
               PCMPGTD XMM0, [g_XM0000FFFF]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // b = _mm_srli_epi32(c, 31);              // b = (c ? 1 : 0)
               PSRLD   XMM0, 31
               // r = _mm_slli_epi32(b, 4);               // r = (b << 4)
               PSLLD   XMM0, 4
               MOVUPS  [r], XMM0
    end;
    // v = multi_srl_epi32(v, r);              // v = (v >> r)
    v := multi_srl_epi32(v, r);

    asm
               // c = _mm_cmpgt_epi32(v, g_XM000000FF);   // c = (v > 0xFF)
               MOVUPS  XMM0,[v]
               PCMPGTD XMM0, [g_XM000000FF]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // b = _mm_srli_epi32(c, 31);              // b = (c ? 1 : 0)
               PSRLD   XMM0, 31
               // s = _mm_slli_epi32(b, 3);               // s = (b << 3)
               PSLLD   XMM0, 3
               MOVUPS  [s],XMM0
    end;
    // v = multi_srl_epi32(v, s);              // v = (v >> s)
    v := multi_srl_epi32(v, s);
    asm
               // r = _mm_or_si128(r, s);                 // r = (r | s)
               MOVUPS  XMM0,[r]
               POR     XMM0, [s]
               MOVUPS  [r],XMM0

               // c = _mm_cmpgt_epi32(v, g_XM0000000F);   // c = (v > 0xF)
               MOVUPS  XMM2, [v]
               PCMPGTD XMM2, [g_XM0000000F]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // b = _mm_srli_epi32(c, 31);              // b = (c ? 1 : 0)
               PSRLD   XMM2, 31
               // s = _mm_slli_epi32(b, 2);               // s = (b << 2)
               PSLLD   XMM2, 2
               MOVUPS  [s],XMM2
    end;
    // v = multi_srl_epi32(v, s);              // v = (v >> s)
    v := multi_srl_epi32(v, s);
    asm
               // r = _mm_or_si128(r, s);                 // r = (r | s)
               MOVUPS  XMM0,[r]
               POR     XMM0, [s]
               MOVUPS  [r],XMM0

               // c = _mm_cmpgt_epi32(v, g_XM00000003);   // c = (v > 0x3)
               MOVUPS  XMM2,[v]
               PCMPGTD XMM2, [g_XM00000003]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // b = _mm_srli_epi32(c, 31);              // b = (c ? 1 : 0)
               PSRLD   XMM2, 31
               // s = _mm_slli_epi32(b, 1);               // s = (b << 1)
               PSLLD   XMM2, 1
               MOVUPS  [s],XMM2
    end;
    // v = multi_srl_epi32(v, s);              // v = (v >> s)
    v := multi_srl_epi32(v, s);
    asm
               // r = _mm_or_si128(r, s);                 // r = (r | s)
               MOVUPS  XMM0,[r]
               POR     XMM0, [s]

               // s = _mm_srli_epi32(v, 1);
               MOVUPS  XMM1,[v]
               PSRLD   XMM1, 1
               // r = _mm_or_si128(r, s);
               POR     XMM0, XMM1
               // return r;
               MOVUPS  [result],XMM0

    end;
end;



function XMVectorLog2(constref V: TXMVECTOR): TXMVECTOR; inline;
var
    trailing, trailingNor, isExponentZero, exponentNor, leading: TXMVECTOR;
    shift, trailingSub, exponentSub: TXMVECTOR;
begin
    asm
               // __m128i rawBiased = _mm_and_si128(_mm_castps_si128(V), g_XMInfinity);
               MOVUPS  XMM0,[V]
               MOVUPS  XMM1,XMM0
               PAND    XMM0, [g_XMInfinity] // XMM0 = rawBiased
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // __m128i trailing = _mm_and_si128(_mm_castps_si128(V), g_XMQNaNTest);
               PAND    XMM1, [g_XMQNaNTest] // XMM1 = trailing
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MOVUPS  [trailing],XMM1
               MOVUPS  [trailingNor],XMM1
               //__m128i isExponentZero = _mm_cmpeq_epi32(g_XMZero, rawBiased);
               MOVUPS  XMM2, [g_XMZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               PCMPEQD XMM2, XMM0  // XMM2 = isExponentZero
               MOVUPS  [isExponentZero],XMM2
               // Compute exponent and significand for normals.
               //__m128i biased = _mm_srli_epi32(rawBiased, 23);
               MOVUPS  XMM3,XMM0
               PSRLD   XMM3, 23
               //__m128i exponentNor = _mm_sub_epi32(biased, g_XMExponentBias);
               PSUBD   XMM3, [g_XMExponentBias]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MOVUPS  [exponentNor],XMM3
               //__m128i trailingNor = trailing;
    end;
    // Compute exponent and significand for subnormals.
    leading := GetLeadingBit(trailing);
    asm
               //__m128i shift = _mm_sub_epi32(g_XMNumTrailing, leading);
               MOVUPS  XMM0, [g_XMNumTrailing]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               PSUBD   XMM0, [leading]
               // __m128i exponentSub = _mm_sub_epi32(g_XMSubnormalExponent, shift);
               MOVUPS  XMM1, [g_XMSubnormalExponent]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               PSUBD   XMM1, XMM0
               MOVUPS  [shift],XMM0
               MOVUPS  [exponentSub],XMM1
    end;
    trailingSub := multi_sll_epi32(trailing, shift);
    asm

               // trailingSub = _mm_and_si128(trailingSub, g_XMQNaNTest);
               MOVUPS  XMM0, [trailingSub]
               PAND    XMM0, [g_XMQNaNTest] // trailingSub = XMM0
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

               // __m128i select0 = _mm_and_si128(isExponentZero, exponentSub);
               MOVUPS  XMM1, [isExponentZero]
               PAND    XMM1, [exponentSub]
               // __m128i select1 = _mm_andnot_si128(isExponentZero, exponentNor);
               MOVUPS  XMM2, [isExponentZero]
               PANDN   XMM2, [exponentNor]
               //__m128i e = _mm_or_si128(select0, select1);
               POR     XMM1, XMM2 // XMM1 = e

               // select0 = _mm_and_si128(isExponentZero, trailingSub);
               MOVUPS  XMM2,[isExponentZero]
               PAND    XMM2, [trailingSub]
               // select1 = _mm_andnot_si128(isExponentZero, trailingNor);
               MOVUPS  XMM3,[isExponentZero]
               PANDN   XMM3, [trailingNor]
               // __m128i t = _mm_or_si128(select0, select1);
               POR     XMM2, XMM3 // XMM2 = t

               // Compute the approximation.
               //__m128i tmp = _mm_or_si128(g_XMOne, t);

               POR     XMM2, [g_XMOne] // XMM2 = tmp
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // __m128 y = _mm_sub_ps(_mm_castsi128_ps(tmp), g_XMOne);
               SUBPS   XMM2, [g_XMOne] // XMM2 = y
               {$IFDEF CPU32}
               MOVUPS  XMM3, [g_XMLogEst7]
               {$ELSE}
               MOVUPS  XMM3, [RIP+g_XMLogEst7]
               {$ENDIF}
               // __m128 log2 = _mm_mul_ps(g_XMLogEst7, y);

               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MULPS   XMM3, XMM2
               // log2 = _mm_add_ps(g_XMLogEst6, log2);
               ADDPS   XMM3, [g_XMLogEst6]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM2
               // log2 = _mm_add_ps(g_XMLogEst5, log2);
               ADDPS   XMM3, [g_XMLogEst5]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM2
               // log2 = _mm_add_ps(g_XMLogEst4, log2);
               ADDPS   XMM3, [g_XMLogEst4]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM2
               // log2 = _mm_add_ps(g_XMLogEst3, log2);
               ADDPS   XMM3, [g_XMLogEst3]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM2
               // log2 = _mm_add_ps(g_XMLogEst2, log2);
               ADDPS   XMM3, [g_XMLogEst2]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM2
               // log2 = _mm_add_ps(g_XMLogEst1, log2);
               ADDPS   XMM3, [g_XMLogEst1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM2
               // log2 = _mm_add_ps(g_XMLogEst0, log2);
               ADDPS   XMM3, [g_XMLogEst0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM2
               // log2 = _mm_add_ps(log2, _mm_cvtepi32_ps(e));
               CVTDQ2PS XMM1, XMM1
               ADDPS   XMM3, XMM1 // XMM3 = log2

               //  if (x is NaN) -> QNaN
               //  else if (V is positive)
               //      if (V is infinite) -> +inf
               //      else -> log2(V)
               //  else
               //      if (V is zero) -> -inf
               //      else -> -QNaN

               // __m128i isInfinite = _mm_and_si128(_mm_castps_si128(V), g_XMAbsMask);
               MOVUPS  XMM0, [V]
               PAND    XMM0, [g_XMAbsMask]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // isInfinite = _mm_cmpeq_epi32(isInfinite, g_XMInfinity);
               PCMPEQD XMM0, [g_XMInfinity] // XMM0 = isInfinite
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

               // __m128i isGreaterZero = _mm_cmpgt_epi32(_mm_castps_si128(V), g_XMZero);
               MOVUPS  XMM1,[V]
               PCMPGTD XMM1, [g_XMZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // __m128i isNotFinite = _mm_cmpgt_epi32(_mm_castps_si128(V), g_XMInfinity);
               MOVUPS  XMM2,[V]
               PCMPGTD XMM2, [g_XMInfinity]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // __m128i isPositive = _mm_andnot_si128(isNotFinite, isGreaterZero);
               PANDN   XMM2, XMM1 // XMM2 = isPositive

               // __m128i isZero = _mm_and_si128(_mm_castps_si128(V), g_XMAbsMask);
               MOVUPS  XMM1, [V]
               PAND    XMM1, [g_XMAbsMask]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // isZero = _mm_cmpeq_epi32(isZero, g_XMZero);
               PCMPEQD XMM1, [g_XMZero] // XMM1 = isZero
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

               // __m128i t0 = _mm_and_si128(_mm_castps_si128(V), g_XMQNaNTest);
               MOVUPS  XMM4, [V]
               PAND    XMM4, [g_XMQNaNTest]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // __m128i t1 = _mm_and_si128(_mm_castps_si128(V), g_XMInfinity);
               MOVUPS  XMM5, [V]
               PAND    XMM5, [g_XMInfinity]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // t0 = _mm_cmpeq_epi32(t0, g_XMZero);
               PCMPEQD XMM4, [g_XMZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // t1 = _mm_cmpeq_epi32(t1, g_XMInfinity);
               PCMPEQD XMM5, [g_XMInfinity]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // __m128i isNaN = _mm_andnot_si128(t0, t1);
               PANDN   XMM4, XMM5 // XMM4 = isNaN

               // select0 = _mm_and_si128(isInfinite, g_XMInfinity);
               MOVUPS  XMM5, XMM0
               PAND    XMM5, [g_XMInfinity]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // select1 = _mm_andnot_si128(isInfinite, _mm_castps_si128(log2));
               PANDN   XMM0, XMM3
               // __m128i result = _mm_or_si128(select0, select1);
               POR     XMM0, XMM5 // XMM0 = result

               // select0 = _mm_and_si128(isZero, g_XMNegInfinity);
               MOVUPS  XMM5,XMM1
               PAND    XMM5, [g_XMNegInfinity]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // select1 = _mm_andnot_si128(isZero, g_XMNegQNaN);
               PANDN   XMM1, [g_XMNegQNaN]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // tmp = _mm_or_si128(select0, select1);
               POR     XMM1, XMM5 // XMM1 = tmp

               // select0 = _mm_and_si128(isPositive, result);
               PAND    XMM0, XMM2
               // select1 = _mm_andnot_si128(isPositive, tmp);
               PANDN   XMM2, XMM1
               // result = _mm_or_si128(select0, select1);
               POR     XMM0, XMM2

               // select0 = _mm_and_si128(isNaN, g_XMQNaN);
               MOVUPS  XMM2, XMM4
               PAND    XMM2, [g_XMQNaN]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // select1 = _mm_andnot_si128(isNaN, result);
               PANDN   XMM0, XMM4
               // result = _mm_or_si128(select0, select1);
               POR     XMM0, XMM2

               // return _mm_castsi128_ps(result);
               MOVUPS  [result],XMM0
    end;
end;



function XMVectorLogE(constref V: TXMVECTOR): TXMVECTOR; inline;
var
    leading, trailing, isExponentZero, shift, exponentSub, trailingSub, trailingNor, exponentNor: TXMVECTOR;
begin
    asm
               // rawBiased = _mm_and_si128(_mm_castps_si128(V), g_XMInfinity);
               MOVUPS  XMM0,[V]
               MOVUPS  XMM1,XMM0
               PAND    XMM0, [g_XMInfinity] // XMM0 = rawBiased
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // trailing = _mm_and_si128(_mm_castps_si128(V), g_XMQNaNTest);
               PAND    XMM1, [g_XMQNaNTest] // XMM1  = trailing
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MOVUPS  [trailing],XMM1

               // isExponentZero = _mm_cmpeq_epi32(g_XMZero, rawBiased);
               MOVUPS  XMM2, [g_XMZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               PCMPEQD XMM2, XMM0 // XMM2 = isExponentZero
               MOVUPS  [isExponentZero],XMM2

               // Compute exponent and significand for normals.
               // biased = _mm_srli_epi32(rawBiased, 23);
               PSRLD   XMM0, 23
               // exponentNor = _mm_sub_epi32(biased, g_XMExponentBias);
               PSUBD   XMM0, [g_XMExponentBias]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MOVUPS  [exponentNor],XMM0
               // trailingNor = trailing;
               MOVUPS  [trailingNor],XMM1
    end;
    // Compute exponent and significand for subnormals.
    leading := GetLeadingBit(trailing);
    asm
               // shift = _mm_sub_epi32(g_XMNumTrailing, leading);
               MOVUPS  XMM0, [g_XMNumTrailing]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               PSUBD   XMM0, [leading]
               MOVUPS  [shift],XMM0
               // exponentSub = _mm_sub_epi32(g_XMSubnormalExponent, shift);
               MOVUPS  XMM1,[g_XMSubnormalExponent]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               PSUBD   XMM1, XMM0
               MOVUPS  [exponentSub],XMM1
    end;
    trailingSub := multi_sll_epi32(trailing, shift);
    asm

               //    trailingSub = _mm_and_si128(trailingSub, g_XMQNaNTest);
               MOVUPS  XMM0, [trailingSub]
               PAND    XMM0, [g_XMQNaNTest]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

               // select0 = _mm_and_si128(isExponentZero, exponentSub);
               MOVUPS  XMM1, [isExponentZero]
               PAND    XMM1, [exponentSub]
               // select1 = _mm_andnot_si128(isExponentZero, exponentNor);
               MOVUPS  XMM2, [isExponentZero]
               PANDN   XMM2, [exponentNor]
               // e = _mm_or_si128(select0, select1);
               POR     XMM1, XMM2 // XMM1 = e

               // select0 = _mm_and_si128(isExponentZero, trailingSub);
               MOVUPS  XMM2, [isExponentZero]
               PAND    XMM2, XMM0
               // select1 = _mm_andnot_si128(isExponentZero, trailingNor);
               MOVUPS  XMM3, [isExponentZero]
               PANDN   XMM3, [trailingNor]
               // t = _mm_or_si128(select0, select1);
               POR     XMM2, XMM3  // XMM2 = t
               // Compute the approximation.
               // tmp = _mm_or_si128(g_XMOne, t);
               MOVUPS  XMM0, [g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               POR     XMM0, XMM2 // XMM0 = tmp
               // y = _mm_sub_ps(_mm_castsi128_ps(tmp), g_XMOne);
               SUBPS   XMM0, [g_XMOne] // XMM0 = y
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

               // log2 = _mm_mul_ps(g_XMLogEst7, y);
               MOVUPS  XMM3, [g_XMLogEst7]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MULPS   XMM3, XMM0
               // log2 = _mm_add_ps(g_XMLogEst6, log2);
               ADDPS   XMM3, [g_XMLogEst6]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM0
               // log2 = _mm_add_ps(g_XMLogEst5, log2);
               ADDPS   XMM3, [g_XMLogEst5]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM0
               // log2 = _mm_add_ps(g_XMLogEst4, log2);
               ADDPS   XMM3, [g_XMLogEst4]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM0
               // log2 = _mm_add_ps(g_XMLogEst3, log2);
               ADDPS   XMM3, [g_XMLogEst3]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM0
               // log2 = _mm_add_ps(g_XMLogEst2, log2);
               ADDPS   XMM3, [g_XMLogEst2]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM0
               // log2 = _mm_add_ps(g_XMLogEst1, log2);
               ADDPS   XMM3, [g_XMLogEst1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM0
               // log2 = _mm_add_ps(g_XMLogEst0, log2);
               ADDPS   XMM3, [g_XMLogEst0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // log2 = _mm_mul_ps(log2, y);
               MULPS   XMM3, XMM0
               // log2 = _mm_add_ps(log2, _mm_cvtepi32_ps(e));
               CVTDQ2PS XMM1, XMM1
               ADDPS   XMM3, XMM1

               // log2 = _mm_mul_ps(g_XMInvLgE, log2);
               MULPS   XMM3, [g_XMInvLgE]  // XMM3 = log2
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

               //  if (x is NaN) -> QNaN
               //  else if (V is positive)
               //      if (V is infinite) -> +inf
               //      else -> log2(V)
               //  else
               //      if (V is zero) -> -inf
               //      else -> -QNaN

               // isInfinite = _mm_and_si128(_mm_castps_si128(V), g_XMAbsMask);
               MOVUPS  XMM0,[V]
               PAND    XMM0, [g_XMAbsMask]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // isInfinite = _mm_cmpeq_epi32(isInfinite, g_XMInfinity);
               PCMPEQD XMM0, [g_XMInfinity]  // XMM0 = isInfinite
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

               //  isGreaterZero = _mm_cmpgt_epi32(_mm_castps_si128(V), g_XMZero);
               MOVUPS  XMM1,[V]
               PCMPGTD XMM1, [g_XMZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  isNotFinite = _mm_cmpgt_epi32(_mm_castps_si128(V), g_XMInfinity);
               MOVUPS  XMM2,[V]
               PCMPGTD XMM2, [g_XMInfinity]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  isPositive = _mm_andnot_si128(isNotFinite, isGreaterZero);
               PANDN   XMM2, XMM1   // XMM2 = isPositive

               //  isZero = _mm_and_si128(_mm_castps_si128(V), g_XMAbsMask);
               MOVUPS  XMM1, [V]
               PAND    XMM1, [g_XMAbsMask]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // isZero = _mm_cmpeq_epi32(isZero, g_XMZero);
               PCMPEQD XMM1, [g_XMZero] // XMM1 = isZero
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}

               //  t0 = _mm_and_si128(_mm_castps_si128(V), g_XMQNaNTest);
               MOVUPS  XMM4, [V]
               PAND    XMM4, [g_XMQNaNTest]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  t1 = _mm_and_si128(_mm_castps_si128(V), g_XMInfinity);
               MOVUPS  XMM5, [V]
               PAND    XMM5, [g_XMInfinity]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // t0 = _mm_cmpeq_epi32(t0, g_XMZero);
               PCMPEQD XMM4, [g_XMZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // t1 = _mm_cmpeq_epi32(t1, g_XMInfinity);
               PCMPEQD XMM5, [g_XMInfinity]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  isNaN = _mm_andnot_si128(t0, t1);
               PANDN   XMM4, XMM5 // XMM4 = isNaN

               // select0 = _mm_and_si128(isInfinite, g_XMInfinity);
               MOVUPS  XMM5, [g_XMInfinity]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               PAND    XMM5, XMM0
               // select1 = _mm_andnot_si128(isInfinite, _mm_castps_si128(log2));
               PANDN   XMM3, XMM0
               //  result = _mm_or_si128(select0, select1);
               POR     XMM5, XMM3 // XMM5 = result

               // select0 = _mm_and_si128(isZero, g_XMNegInfinity);
               MOVUPS  XMM6, [g_XMNegInfinity]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               PAND    XMM6, XMM1
               // select1 = _mm_andnot_si128(isZero, g_XMNegQNaN);
               PANDN   XMM1, [g_XMNegQNaN]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // tmp = _mm_or_si128(select0, select1);
               POR     XMM6, XMM1 // XMM6 = tmp

               // select0 = _mm_and_si128(isPositive, result);
               PAND    XMM5, XMM2
               // select1 = _mm_andnot_si128(isPositive, tmp);
               PANDN   XMM2, XMM6
               // result = _mm_or_si128(select0, select1);
               POR     XMM5, XMM2

               // select0 = _mm_and_si128(isNaN, g_XMQNaN);
               MOVUPS  XMM6, [g_XMQNaN]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               PAND    XMM6, XMM4
               // select1 = _mm_andnot_si128(isNaN, result);
               PANDN   XMM4, XMM5
               // result = _mm_or_si128(select0, select1);
               POR     XMM6, XMM4

               // return _mm_castsi128_ps(result);
               MOVUPS  [result],XMM6
    end;
end;



function XMVectorPow(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; inline;
begin
    Result.f32[0] := power(V1.f32[0], V2.f32[0]);
    Result.f32[1] := power(V1.f32[1], V2.f32[1]);
    Result.f32[2] := power(V1.f32[2], V2.f32[2]);
    Result.f32[3] := power(V1.f32[3], V2.f32[3]);
end;



function XMVectorAbs(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vResult = _mm_setzero_ps();
           XORPS   XMM0,XMM0
           // vResult = _mm_sub_ps(vResult,V);
           SUBPS   XMM0,[V]
           // vResult = _mm_max_ps(vResult,V);
           MAXPS   XMM0, [V]
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVectorMod(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vResult = _mm_div_ps(V1, V2);
           MOVUPS  XMM0,[V1]
           DIVPS   XMM0, [V2] // vResult = XMM0
           // vResult := XMVectorTruncate(vResult);
           // ToDo: check this code
           MOVUPS  XMM4, XMM0
           PAND    XMM4, [g_XMAbsMask]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           PCMPGTD XMM4, [g_XMNoFraction]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           CVTTPS2DQ XMM5, XMM0
           CVTDQ2PS XMM5, XMM5
           ANDPS   XMM5,XMM4
           PANDN   XMM4, XMM0
           ORPS    XMM5, XMM4 // vResult = XMM5

           // vResult = _mm_mul_ps(vResult,V2);
           MOVUPS  XMM1,XMM5
           MULPS   XMM1,[V2]
           // vResult = _mm_sub_ps(V1,vResult);
           MOVUPS  XMM0,[V1]
           SUBPS   XMM0, XMM1
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVectorModAngles(constref Angles: TXMVECTOR): TXMVECTOR; inline;
var
    vResult: TXMVECTOR;
begin
    asm
               // Modulo the range of the given angles such that -XM_PI <= Angles < XM_PI
               // vResult = _mm_mul_ps(Angles,g_XMReciprocalTwoPi);
               MOVUPS  XMM0,[Angles]
               MULPS   XMM0, g_XMReciprocalTwoPi
               MOVUPS  [vResult],XMM0
    end;

    // Use the inline function due to complexity for rounding
    vResult := XMVectorRound(vResult);
    asm
               // vResult = _mm_mul_ps(vResult,g_XMTwoPi);
               MOVUPS  XMM1,[vResult]
               MULPS   XMM1, [g_XMTwoPi]
{$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //vResult = _mm_sub_ps(Angles,vResult);
               MOVUPS  XMM0, [Angles]
               SUBPS   XMM0,XMM1
               // return vResult;
               MOVUPS  [result],XMM0
    end;
end;


// 11-degree minimax approximation
function XMVectorSin(constref V: TXMVECTOR): TXMVECTOR; inline;
var
    x: TXMVECTOR;
begin
    // Force the value within the bounds of pi
    x := XMVectorModAngles(V);
    asm
               // Map in [-pi/2,pi/2] with sin(y) = sin(x).
               // sign = _mm_and_ps(x, g_XMNegativeZero);
               MOVUPS  XMM0,[x]
               ANDPS   XMM0, [g_XMNegativeZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // c = _mm_or_ps(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
               MOVUPS  XMM1,[g_XMPi]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               ORPS    XMM1, XMM0
               //  absx = _mm_andnot_ps(sign, x);  // |x|
               ANDNPS  XMM0, [x]
               // rflx = _mm_sub_ps(c, x);
               SUBPS   XMM1, [x]
               // comp = _mm_cmple_ps(absx, g_XMHalfPi);
               CMPPS   XMM0, [g_XMHalfPi], 2
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // __m128 select1 = _mm_andnot_ps(comp, rflx);
               ANDNPS  XMM1, XMM0
               // __m128 select0 = _mm_and_ps(comp, x);
               ANDPS   XMM0, [x]
               // x = _mm_or_ps(select0, select1);
               ORPS    XMM0, XMM1
               MOVUPS  [x],XMM0
               //  x2 = _mm_mul_ps(x, x);
               MULPS   XMM0, XMM0

               // Compute polynomial approximation
               // SC1 = g_XMSinCoefficients1;
               MOVUPS  XMM7, [g_XMSinCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // vConstants = XM_PERMUTE_PS( SC1, _MM_SHUFFLE(0, 0, 0, 0) );
               SHUFPS  XMM7, XMM7, _MM_SHUFFLE_0_0_0_0
               // Result = _mm_mul_ps(vConstants, x2);
               MULPS   XMM7, XMM0

               // SC0 = g_XMSinCoefficients0;
               MOVUPS  XMM6, [g_XMSinCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // vConstants = XM_PERMUTE_PS( SC0, _MM_SHUFFLE(3, 3, 3, 3) );
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_3_3_3_3
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7,XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( SC0, _MM_SHUFFLE(2, 2, 2, 2) );
               MOVUPS  XMM6, [g_XMSinCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_2_2_2_2
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6

               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( SC0,  _MM_SHUFFLE(1, 1, 1, 1) );
               MOVUPS  XMM6, [g_XMSinCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_1_1_1_1
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6

               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( SC0, _MM_SHUFFLE(0, 0, 0, 0) );
               MOVUPS  XMM6, [g_XMSinCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_0_0_0_0
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0
               // Result = _mm_add_ps(Result, g_XMOne);
               ADDPS   XMM7, [g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  Result = _mm_mul_ps(Result, x);
               MULPS   XMM7, [x]
               // return Result;
               MOVUPS  [result],XMM7
    end;
end;

// 7-degree minimax approximation

function XMVectorSinEst(constref V: TXMVECTOR): TXMVECTOR; inline;
var
    x: TXMVECTOR;
begin
    // Force the value within the bounds of pi
    x := XMVectorModAngles(V);
    asm
               // Map in [-pi/2,pi/2] with sin(y) = sin(x).
               // sign = _mm_and_ps(x, g_XMNegativeZero);
               MOVUPS  XMM0,[x]
               ANDPS   XMM0, [g_XMNegativeZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  c = _mm_or_ps(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
               MOVUPS  XMM1,XMM0
               ORPS    XMM1, [g_XMPi]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  absx = _mm_andnot_ps(sign, x);  // |x|
               ANDNPS  XMM0, [x]
               //  rflx = _mm_sub_ps(c, x);
               SUBPS   XMM1, [x]
               //  comp = _mm_cmple_ps(absx, g_XMHalfPi);
               CMPPS   XMM0, [g_XMHalfPi], 2
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  select1 = _mm_andnot_ps(comp, rflx);
               ANDNPS  XMM1, XMM0
               //  select0 = _mm_and_ps(comp, x);
               ANDPS   XMM0, [x]

               // x = _mm_or_ps(select0, select1);
               ORPS    XMM0, XMM1
               MOVUPS  [x],XMM0
               //  x2 = _mm_mul_ps(x, x);
               MULPS   XMM0, XMM0

               // Compute polynomial approximation
               //  SEC = g_XMSinCoefficients1;
               MOVUPS  XMM7,[g_XMSinCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  vConstants = XM_PERMUTE_PS( SEC, _MM_SHUFFLE(3, 3, 3, 3) );
               SHUFPS  XMM7, XMM7, _MM_SHUFFLE_3_3_3_3
               //  Result = _mm_mul_ps(vConstants, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( SEC, _MM_SHUFFLE(2, 2, 2, 2) );
               MOVUPS  XMM6,[g_XMSinCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_2_2_2_2
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( SEC, _MM_SHUFFLE(1, 1, 1, 1) );
               MOVUPS  XMM6,[g_XMSinCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_1_1_1_1
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // Result = _mm_add_ps(Result, g_XMOne);
               ADDPS   XMM7, [g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // Result = _mm_mul_ps(Result, x);
               MULPS   XMM7, [x]
               // return Result;
               MOVUPS  [result],XMM7
    end;
end;


// 10-degree minimax approximation
function XMVectorCos(constref V: TXMVECTOR): TXMVECTOR; inline;
var
    x: TXMVECTOR;
begin
    // Map V to x in [-pi,pi].
    x := XMVectorModAngles(V);
    asm
               // Map in [-pi/2,pi/2] with cos(y) = sign*cos(x).
               // sign = _mm_and_ps(x, g_XMNegativeZero);
               MOVUPS  XMM0,[x]
               ANDPS   XMM0, [g_XMNegativeZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  c = _mm_or_ps(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
               MOVUPS  XMM1,XMM0
               ORPS    XMM1, [g_XMPi]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  absx = _mm_andnot_ps(sign, x);  // |x|
               ANDNPS  XMM0, [x]
               //  rflx = _mm_sub_ps(c, x);
               SUBPS   XMM1, [x]
               //  comp = _mm_cmple_ps(absx, g_XMHalfPi);
               CMPPS   XMM0, [g_XMHalfPi], 2
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MOVUPS  XMM4,XMM0
               //  select1 = _mm_andnot_ps(comp, rflx);
               ANDNPS  XMM1, XMM0
               //  select0 = _mm_and_ps(comp, x);
               ANDPS   XMM0, [x]

               // x = _mm_or_ps(select0, select1);
               ORPS    XMM0, XMM1
               // select0 = _mm_and_ps(comp, g_XMOne);
               MOVUPS  XMM2, [g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               ANDPS   XMM2, XMM4
               // select1 = _mm_andnot_ps(comp, g_XMNegativeOne);
               ANDNPS  XMM4, [g_XMNegativeOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // sign = _mm_or_ps(select0, select1);
               ORPS    XMM2, XMM4  // XMM2 = sign

               //  x2 = _mm_mul_ps(x, x);
               MULPS   XMM0, XMM0 // XMM0 = x2

               // Compute polynomial approximation
               //  CC1 = g_XMCosCoefficients1;
               MOVUPS  XMM7, [g_XMCosCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // vConstants = XM_PERMUTE_PS( CC1, _MM_SHUFFLE(0, 0, 0, 0) );
               SHUFPS  XMM7, XMM7, _MM_SHUFFLE_0_0_0_0
               // Result = _mm_mul_ps(vConstants, x2);
               MULPS   XMM7, XMM0

               // CC0 = g_XMCosCoefficients0;
               MOVUPS  XMM6, [g_XMCosCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // vConstants = XM_PERMUTE_PS( CC0, _MM_SHUFFLE(3, 3, 3, 3) );
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_3_3_3_3
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( CC0, _MM_SHUFFLE(2, 2, 2, 2) );
               MOVUPS  XMM6, [g_XMCosCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_2_2_2_2
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( CC0, _MM_SHUFFLE(1, 1, 1, 1) );
               MOVUPS  XMM6, [g_XMCosCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_1_1_1_1
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( CC0, _MM_SHUFFLE(0, 0, 0, 0) );
               MOVUPS  XMM6, [g_XMCosCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_0_0_0_0
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0
               // Result = _mm_add_ps(Result, g_XMOne);
               ADDPS   XMM7, [g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // Result = _mm_mul_ps(Result, sign);
               MULPS   XMM7, XMM2
               // return Result;
               MOVUPS  [result],XMM7
    end;
end;


// 6-degree minimax approximation
function XMVectorCosEst(constref V: TXMVECTOR): TXMVECTOR; inline;
var
    x: TXMVECTOR;
begin
    // Map V to x in [-pi,pi].
    x := XMVectorModAngles(V);
    asm
               // Map in [-pi/2,pi/2] with cos(y) = sign*cos(x).
               //  sign = _mm_and_ps(x, g_XMNegativeZero);
               MOVUPS  XMM0,[x]
               ANDPS   XMM0, [g_XMNegativeZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  c = _mm_or_ps(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
               MOVUPS  XMM1, [g_XMPi]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               ORPS    XMM1, XMM0
               //  absx = _mm_andnot_ps(sign, x);  // |x|
               ANDNPS  XMM0, [x]
               //  rflx = _mm_sub_ps(c, x);
               SUBPS   XMM1, [x]
               //  comp = _mm_cmple_ps(absx, g_XMHalfPi);
               CMPPS   XMM0, [g_XMHalfPi], 2
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MOVUPS  XMM4,XMM0
               //  select1 = _mm_andnot_ps(comp, rflx);
               ANDNPS  XMM1, XMM0
               //  select0 = _mm_and_ps(comp, x);
               ANDPS   XMM0, [x]

               // x = _mm_or_ps(select0, select1);
               ORPS    XMM0, XMM1
               // select0 = _mm_and_ps(comp, g_XMOne);
               MOVUPS  XMM2,[g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               ANDPS   XMM2, XMM4
               // select1 = _mm_andnot_ps(comp, g_XMNegativeOne);
               ANDNPS  XMM4, [g_XMNegativeOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // sign = _mm_or_ps(select0, select1);
               ORPS    XMM2, XMM4  // XMM2 = sign

               //  x2 = _mm_mul_ps(x, x);
               MULPS   XMM0, XMM0 // XMM0 = x2

               // Compute polynomial approximation
               // CEC = g_XMCosCoefficients1;
               MOVUPS  XMM7, [g_XMCosCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  vConstants = XM_PERMUTE_PS( CEC, _MM_SHUFFLE(3, 3, 3, 3) );
               SHUFPS  XMM7, XMM7, _MM_SHUFFLE_3_3_3_3
               //  Result = _mm_mul_ps(vConstants, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( CEC, _MM_SHUFFLE(2, 2, 2, 2) );
               MOVUPS  XMM6, [g_XMCosCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_2_2_2_2
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( CEC, _MM_SHUFFLE(1, 1, 1, 1) );
               MOVUPS  XMM6, [g_XMCosCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_1_1_1_1
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM0, XMM7

               // Result = _mm_add_ps(Result, g_XMOne);
               ADDPS   XMM0, [g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // Result = _mm_mul_ps(Result, sign);
               MULPS   XMM0, XMM2

               // return Result;
               MOVUPS  [result],XMM0
    end;
end;


// 11/10-degree minimax approximation
procedure XMVectorSinCos(out pSin: TXMVECTOR; out pCos: TXMVECTOR; constref V: TXMVECTOR); inline;
var
    x: TXMVECTOR;
begin
    // Force the value within the bounds of pi
    x := XMVectorModAngles(V);
    asm
               // Map in [-pi/2,pi/2] with sin(y) = sin(x), cos(y) = sign*cos(x).
               // sign = _mm_and_ps(x, g_XMNegativeZero);
               MOVUPS  XMM0, [x]
               ANDPS   XMM0, [g_XMNegativeZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  c = _mm_or_ps(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
               MOVUPS  XMM1, [g_XMPi]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               ORPS    XMM1, XMM0
               //  absx = _mm_andnot_ps(sign, x);  // |x|
               ANDNPS  XMM0, [x]
               //  rflx = _mm_sub_ps(c, x);
               SUBPS   XMM1, [x]
               //  comp = _mm_cmple_ps(absx, g_XMHalfPi);
               CMPPS   XMM0, [g_XMHalfPi], 2
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MOVUPS  XMM4,XMM0
               //  select1 = _mm_andnot_ps(comp, rflx);
               ANDNPS  XMM1, XMM0
               //  select0 = _mm_and_ps(comp, x);
               ANDPS   XMM0, [x]

               // x = _mm_or_ps(select0, select1);
               ORPS    XMM0, XMM1
               // select0 = _mm_and_ps(comp, g_XMOne);
               MOVUPS  XMM2, [g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               ANDPS   XMM2, XMM4
               // select1 = _mm_andnot_ps(comp, g_XMNegativeOne);
               ANDNPS  XMM4, [g_XMNegativeOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // sign = _mm_or_ps(select0, select1);
               ORPS    XMM2, XMM4 // XMM2 = sign

               //  x2 = _mm_mul_ps(x, x);
               MOVUPS  XMM1, XMM0
               MULPS   XMM0, XMM0  // XMM0 = x2, XMM1 = x

               // Compute polynomial approximation of sine
               //  SC1 = g_XMSinCoefficients1;
               MOVUPS  XMM7, [g_XMSinCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  vConstants = XM_PERMUTE_PS( SC1, _MM_SHUFFLE(0, 0, 0, 0) );
               SHUFPS  XMM7, XMM7, _MM_SHUFFLE_0_0_0_0
               //  Result = _mm_mul_ps(vConstants, x2);
               MULPS   XMM7, XMM0

               //  SC0 = g_XMSinCoefficients0;
               MOVUPS  XMM6, [g_XMSinCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // vConstants = XM_PERMUTE_PS( SC0, _MM_SHUFFLE(3, 3, 3, 3) );
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_3_3_3_3
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( SC0, _MM_SHUFFLE(2, 2, 2, 2) );
               MOVUPS  XMM6, [g_XMSinCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_2_2_2_2
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( SC0, _MM_SHUFFLE(1, 1, 1, 1) );
               MOVUPS  XMM6, [g_XMSinCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_1_1_1_1
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( SC0, _MM_SHUFFLE(0, 0, 0, 0) );
               MOVUPS  XMM6, [g_XMSinCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_0_0_0_0
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0
               // Result = _mm_add_ps(Result, g_XMOne);
               ADDPS   XMM7,[g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // Result = _mm_mul_ps(Result, x);
               MULPS   XMM7, XMM1
               //*pSin = Result;
               MOVUPS  [pSin],XMM7

               // Compute polynomial approximation of cosine
               //  CC1 = g_XMCosCoefficients1;
               MOVUPS  XMM7, [g_XMCosCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // vConstants = XM_PERMUTE_PS( CC1, _MM_SHUFFLE(0, 0, 0, 0) );
               SHUFPS  XMM7, XMM7, _MM_SHUFFLE_0_0_0_0
               // Result = _mm_mul_ps(vConstants, x2);
               MULPS   XMM7, XMM0

               // CC0 = g_XMCosCoefficients0;
               MOVUPS  XMM6, [g_XMCosCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // vConstants = XM_PERMUTE_PS( CC0, _MM_SHUFFLE(3, 3, 3, 3) );
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_3_3_3_3
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( CC0,  _MM_SHUFFLE(2, 2, 2, 2) );
               MOVUPS  XMM6, [g_XMCosCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_2_2_2_2
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( CC0,  _MM_SHUFFLE(1, 1, 1, 1) );
               MOVUPS  XMM6, [g_XMCosCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_1_1_1_1
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( CC0, _MM_SHUFFLE(0, 0, 0, 0) );
               MOVUPS  XMM6, [g_XMCosCoefficients0]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_0_0_0_0
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7,XMM0
               // Result = _mm_add_ps(Result, g_XMOne);
               ADDPS   XMM7, [g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // Result = _mm_mul_ps(Result, sign);
               MULPS   XMM7,XMM2
               // *pCos = Result;
               MOVUPS  [pCos],XMM7
    end;
end;



procedure XMVectorSinCosEst(out pSin: TXMVECTOR; out pCos: TXMVECTOR; constref V: TXMVECTOR); inline;
var
    x: TXMVECTOR;
begin
    // Force the value within the bounds of pi
    x := XMVectorModAngles(V);
    asm
               // Map in [-pi/2,pi/2] with sin(y) = sin(x), cos(y) = sign*cos(x).
               //  sign = _mm_and_ps(x, g_XMNegativeZero);
               MOVUPS  XMM0,[x]
               ANDPS   XMM0, [g_XMNegativeZero]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               //  c = _mm_or_ps(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
               MOVUPS  XMM1,[g_XMPi]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               ORPS    XMM1, XMM0
               //  absx = _mm_andnot_ps(sign, x);  // |x|
               ANDNPS  XMM0, [x]
               //  rflx = _mm_sub_ps(c, x);
               SUBPS   XMM1,[x]
               //  comp = _mm_cmple_ps(absx, g_XMHalfPi);
               CMPPS   XMM0, [g_XMHalfPi], 2
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MOVUPS  XMM4,XMM0
               //  select1 = _mm_andnot_ps(comp, rflx);
               ANDNPS  XMM1, XMM0
               //  select0 = _mm_and_ps(comp, x);
               ANDPS   XMM0, [x]

               // x = _mm_or_ps(select0, select1);
               ORPS    XMM0,XMM1
               // select0 = _mm_and_ps(comp, g_XMOne);
               MOVUPS  XMM2, [g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               ANDPS   XMM2, XMM4
               // select1 = _mm_andnot_ps(comp, g_XMNegativeOne);
               ANDNPS  XMM4, [g_XMNegativeOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // sign = _mm_or_ps(select0, select1);
               ORPS    XMM2, XMM4
               //  x2 = _mm_mul_ps(x, x);
               MOVUPS  XMM1,XMM0
               MULPS   XMM0,XMM0

               // Compute polynomial approximation for sine
               //  SEC = g_XMSinCoefficients1;
               MOVUPS  XMM7,[g_XMSinCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // vConstants = XM_PERMUTE_PS( SEC, _MM_SHUFFLE(3, 3, 3, 3) );
               SHUFPS  XMM7, XMM7, _MM_SHUFFLE_3_3_3_3
               //  Result = _mm_mul_ps(vConstants, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( SEC, _MM_SHUFFLE(2, 2, 2, 2) );
               MOVUPS  XMM6,[g_XMSinCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_2_2_2_2
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( SEC, _MM_SHUFFLE(1, 1, 1, 1) );
               MOVUPS  XMM6,[g_XMSinCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_1_1_1_1
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // Result = _mm_add_ps(Result, g_XMOne);
               ADDPS   XMM7, [g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // Result = _mm_mul_ps(Result, x);
               MULPS   XMM7, XMM1

               // *pSin = Result;
               MOVUPS  [pSin],XMM7

               // Compute polynomial approximation for cosine
               //  CEC = g_XMCosCoefficients1;
               MOVUPS  XMM7,[g_XMCosCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // vConstants = XM_PERMUTE_PS( CEC, _MM_SHUFFLE(3, 3, 3, 3) );
               SHUFPS  XMM7, XMM7, _MM_SHUFFLE_3_3_3_3
               // Result = _mm_mul_ps(vConstants, x2);
               MULPS   XMM7, XMM0
               // vConstants = XM_PERMUTE_PS( CEC, _MM_SHUFFLE(2, 2, 2, 2) );
               MOVUPS  XMM6,[g_XMCosCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_2_2_2_2

               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // vConstants = XM_PERMUTE_PS( CEC, _MM_SHUFFLE(1, 1, 1, 1) );
               MOVUPS  XMM6,[g_XMCosCoefficients1]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SHUFPS  XMM6, XMM6, _MM_SHUFFLE_1_1_1_1
               // Result = _mm_add_ps(Result, vConstants);
               ADDPS   XMM7, XMM6
               // Result = _mm_mul_ps(Result, x2);
               MULPS   XMM7, XMM0

               // Result = _mm_add_ps(Result, g_XMOne);
               ADDPS   XMM7, [g_XMOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               // Result = _mm_mul_ps(Result, sign);
               MULPS   XMM7, XMM2
               // *pCos = Result;
               MOVUPS  [pCos],XMM7
    end;
end;



function XMVectorTan(constref V: TXMVECTOR): TXMVECTOR; inline;
const
    TanCoefficients0: TXMVECTORF32 = (f: (1.0, -4.667168334e-1, 2.566383229e-2, -3.118153191e-4));
    TanCoefficients1: TXMVECTORF32 = (f: (4.981943399e-7, -1.333835001e-1, 3.424887824e-3, -1.786170734e-5));
    TanConstants: TXMVECTORF32 = (f: (1.570796371, 6.077100628e-11, 0.000244140625, 0.63661977228));
    Mask: TXMVECTORU32 = (u: ($1, $1, $1, $1));
var
    TwoDivPi, Zero, C0, C1, Epsilon, VA: TXMVECTOR;
    VC, VB, VC2, T0, T1, T2, T3, T4, T5, T6, T7: TXMVECTOR;
    VBIsEven, N, D, VCNearZero, R0, R1: TXMVECTOR;
    VIsZero: TXMVECTOR;
begin
    TwoDivPi := XMVectorSplatW(TanConstants.v);

    Zero := XMVectorZero();

    C0 := XMVectorSplatX(TanConstants.v);
    C1 := XMVectorSplatY(TanConstants.v);
    Epsilon := XMVectorSplatZ(TanConstants.v);

    VA := XMVectorMultiply(V, TwoDivPi);

    VA := XMVectorRound(VA);

    VC := XMVectorNegativeMultiplySubtract(VA, C0, V);

    VB := XMVectorAbs(VA);

    VC := XMVectorNegativeMultiplySubtract(VA, C1, VC);


    asm
               // reinterpret_cast<__m128i *>(&VB)[0] = _mm_cvttps_epi32(VB);
               MOVUPS  XMM0, [VB]
               CVTTPS2DQ XMM0, XMM0
               MOVUPS  [VB],XMM0
    end;

    VC2 := XMVectorMultiply(VC, VC);

    T7 := XMVectorSplatW(TanCoefficients1.v);
    T6 := XMVectorSplatZ(TanCoefficients1.v);
    T4 := XMVectorSplatX(TanCoefficients1.v);
    T3 := XMVectorSplatW(TanCoefficients0.v);
    T5 := XMVectorSplatY(TanCoefficients1.v);
    T2 := XMVectorSplatZ(TanCoefficients0.v);
    T1 := XMVectorSplatY(TanCoefficients0.v);
    T0 := XMVectorSplatX(TanCoefficients0.v);


    VBIsEven := XMVectorAndInt(VB, Mask.v);
    VBIsEven := XMVectorEqualInt(VBIsEven, Zero);

    N := XMVectorMultiplyAdd(VC2, T7, T6);
    D := XMVectorMultiplyAdd(VC2, T4, T3);
    N := XMVectorMultiplyAdd(VC2, N, T5);
    D := XMVectorMultiplyAdd(VC2, D, T2);
    N := XMVectorMultiply(VC2, N);
    D := XMVectorMultiplyAdd(VC2, D, T1);
    N := XMVectorMultiplyAdd(VC, N, VC);


    VCNearZero := XMVectorInBounds(VC, Epsilon);
    D := XMVectorMultiplyAdd(VC2, D, T0);

    N := XMVectorSelect(N, VC, VCNearZero);
    D := XMVectorSelect(D, g_XMOne.v, VCNearZero);

    R0 := XMVectorNegate(N);
    R1 := XMVectorDivide(N, D);
    R0 := XMVectorDivide(D, R0);

    VIsZero := XMVectorEqual(V, Zero);

    Result := XMVectorSelect(R0, R1, VBIsEven);
    Result := XMVectorSelect(Result, Zero, VIsZero);

end;



function XMVectorTanEst(constref V: TXMVECTOR): TXMVECTOR;
var
    OneOverPi, V1, T0, T1, T2, V2T2: TXMVECTOR;
    V2, V1T0, V1T1, D, N: TXMVECTOR;
begin
    OneOverPi := XMVectorSplatW(g_XMTanEstCoefficients.v);

    V1 := XMVectorMultiply(V, OneOverPi);
    V1 := XMVectorRound(V1);

    V1 := XMVectorNegativeMultiplySubtract(g_XMPi.v, V1, V);

    T0 := XMVectorSplatX(g_XMTanEstCoefficients.v);
    T1 := XMVectorSplatY(g_XMTanEstCoefficients.v);
    T2 := XMVectorSplatZ(g_XMTanEstCoefficients.v);

    V2T2 := XMVectorNegativeMultiplySubtract(V1, V1, T2);
    V2 := XMVectorMultiply(V1, V1);
    V1T0 := XMVectorMultiply(V1, T0);
    V1T1 := XMVectorMultiply(V1, T1);

    D := XMVectorReciprocalEst(V2T2);
    N := XMVectorMultiplyAdd(V2, V1T1, V1T0);

    Result := XMVectorMultiply(N, D);
end;



function XMVectorSinH(constref V: TXMVECTOR): TXMVECTOR; {assembler;}
const
    Scale: TXMVECTORF32 = (f: (1.442695040888963, 1.442695040888963, 1.442695040888963, 1.442695040888963)); // 1.0 / ln(2.0)
var
    v1, v2, E1, E2: TXMVECTOR;
begin
    asm
               MOVUPS  XMM0,[V]
               MOVUPS  XMM1,XMM0
               MULPS   XMM1, [Scale]//  V1 = _mm_mul_ps(V, Scale);
               ADDPS   XMM1, [g_XMNegativeOne] // V1 = _mm_add_ps(V1,g_XMNegativeOne);
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MOVUPS  [v1], XMM1
               MOVUPS  XMM2,XMM0
               MULPS   XMM2, [Scale] // V2 = _mm_mul_ps(V, Scale);
               MOVUPS  XMM3,[g_XMNegativeOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SUBPS   XMM3, XMM2 // V2 = _mm_sub_ps(g_XMNegativeOne,V2);
               MOVUPS  [v2], XMM3
    end;
    E1 := XMVectorExp(V1);
    E2 := XMVectorExp(V2);
    asm
               MOVUPS  XMM0,[E1]
               SUBPS   XMM0, [E2] // return _mm_sub_ps(E1, E2);
               MOVUPS  [result], XMM0
    end;
end;



function XMVectorCosH(constref V: TXMVECTOR): TXMVECTOR;
const
    Scale: TXMVECTORF32 = (f: (1.442695040888963, 1.442695040888963, 1.442695040888963, 1.442695040888963)); // 1.0f / ln(2.0f)
var
    V1, V2, E1, E2: TXMVECTOR;
begin
    asm
               MOVUPS  XMM0, [V]
               MOVUPS  XMM1, XMM0
               MULPS   XMM1,[Scale] // V1 = _mm_mul_ps(V,Scale.v);
               ADDPS   XMM1,[g_XMNegativeOne] // V1 = _mm_add_ps(V1,g_XMNegativeOne.v);
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MOVUPS  [V1], XMM1
               MOVUPS  XMM1, XMM0
               MULPS   XMM1,[Scale] // V2 = _mm_mul_ps(V, Scale.v);
               MOVUPS  XMM2, [g_XMNegativeOne]
               {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SUBPS   XMM2, XMM1 // V2 = _mm_sub_ps(g_XMNegativeOne.v,V2);
               MOVUPS  [V2], XMM2
    end;
    E1 := XMVectorExp(V1);
    E2 := XMVectorExp(V2);
    asm
               MOVUPS  XMM0, [E1]
               ADDPS   XMM0, [E2]
               MOVUPS  [Result], XMM0 // return _mm_add_ps(E1, E2);
    end;
end;



function XMVectorTanH(constref V: TXMVECTOR): TXMVECTOR;
const
    Scale: TXMVECTORF32 = (f: (2.8853900817779268, 2.8853900817779268, 2.8853900817779268, 2.8853900817779268)); // 2.0f / ln(2.0f)
var
    E: TXMVECTOR;
begin
    asm
               MOVUPS  XMM0,[V];
               MULPS   XMM0, [Scale];
               MOVUPS  [E],XMM0 // E = _mm_mul_ps(V, Scale.v);
    end;
    E := XMVectorExp(E);
    asm
               MOVUPS  XMM0,[E]
               MULPS   XMM0, [g_XMOneHalf] //  E = _mm_mul_ps(E,g_XMOneHalf.v);
{$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               ADDPS   XMM0, [g_XMOneHalf] // E = _mm_add_ps(E,g_XMOneHalf.v);
{$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               MOVUPS  XMM1,[g_XMOne]
{$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               DIVPS   XMM1, XMM0 //E = _mm_div_ps(g_XMOne.v,E);
               MOVUPS  XMM0,[g_XMOne]
{$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
               SUBPS   XMM0, XMM1 // return _mm_sub_ps(g_XMOne.v,E);
               MOVUPS  [result],XMM0
    end;
end;


// Computes the arcsine of each component of an XMVECTOR.
// 7-degree minimax approximation

function XMVectorASin(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // nonnegative = _mm_cmpge_ps(V, g_XMZero);
           MOVUPS  XMM0, [V]
           CMPPS   XMM0, [g_XMZero], 2
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           //  mvalue = _mm_sub_ps(g_XMZero, V);
           MOVUPS  XMM1, [g_XMZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SUBPS   XMM1 ,[V]
           //  x = _mm_max_ps(V, mvalue);  // |V|
           MAXPS   XMM1, [V] // XMM1 = x

           // Compute (1-|V|), clamp to zero to avoid sqrt of negative number.
           //  oneMValue = _mm_sub_ps(g_XMOne, x);
           MOVUPS  XMM2, [g_XMOne]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SUBPS   XMM2, XMM1
           //  clampOneMValue = _mm_max_ps(g_XMZero, oneMValue);
           MAXPS   XMM2, [g_XMZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           //  root = _mm_sqrt_ps(clampOneMValue);  // sqrt(1-|V|)
           SQRTPS  XMM2, XMM2  // XMM2 = root

           // Compute polynomial approximation
           //  AC1 = g_XMArcCoefficients1;
           MOVUPS  XMM7,[g_XMArcCoefficients1]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // vConstants = XM_PERMUTE_PS( AC1, _MM_SHUFFLE(3, 3, 3, 3) );
           SHUFPS  XMM7, XMM7, _MM_SHUFFLE_3_3_3_3
           //  t0 = _mm_mul_ps(vConstants, x);
           MULPS   XMM7, XMM1

           // vConstants = XM_PERMUTE_PS( AC1, _MM_SHUFFLE(2, 2, 2, 2) );
           MOVUPS  XMM6,[g_XMArcCoefficients1]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SHUFPS  XMM6, XMM6, _MM_SHUFFLE_2_2_2_2
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM7, XMM6
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM7, XMM1

           // vConstants = XM_PERMUTE_PS( AC1, _MM_SHUFFLE(1, 1, 1, 1) );
           MOVUPS  XMM6,[g_XMArcCoefficients1]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SHUFPS  XMM6, XMM6, _MM_SHUFFLE_1_1_1_1
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM7, XMM6
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM7, XMM1

           // vConstants = XM_PERMUTE_PS( AC1, _MM_SHUFFLE(0, 0, 0, 0) );
           MOVUPS  XMM6,[g_XMArcCoefficients1]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SHUFPS  XMM6, XMM6, _MM_SHUFFLE_0_0_0_0
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM7, XMM6
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM7, XMM1

           //  AC0 = g_XMArcCoefficients0;
           MOVUPS  XMM6,[g_XMArcCoefficients0]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // vConstants = XM_PERMUTE_PS( AC0, _MM_SHUFFLE(3, 3, 3, 3) );
           SHUFPS  XMM6, XMM6, _MM_SHUFFLE_3_3_3_3
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM7, XMM6
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM7, XMM1

           // vConstants = XM_PERMUTE_PS( AC0,_MM_SHUFFLE(2, 2, 2, 2) );
           MOVUPS  XMM6,[g_XMArcCoefficients0]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SHUFPS  XMM6, XMM6, _MM_SHUFFLE_2_2_2_2
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM7, XMM6
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM7, XMM1

           // vConstants = XM_PERMUTE_PS( AC0, _MM_SHUFFLE(1, 1, 1, 1) );
           MOVUPS  XMM6,[g_XMArcCoefficients0]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SHUFPS  XMM6, XMM6, _MM_SHUFFLE_1_1_1_1
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM7, XMM6
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM7, XMM1

           // vConstants = XM_PERMUTE_PS( AC0, _MM_SHUFFLE(0, 0, 0, 0) );
           MOVUPS  XMM6,[g_XMArcCoefficients0]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SHUFPS  XMM6, XMM6, _MM_SHUFFLE_0_0_0_0
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM7, XMM6
           // t0 = _mm_mul_ps(t0, root);
           MULPS   XMM7, XMM1

           //  t1 = _mm_sub_ps(g_XMPi, t0);
           MOVUPS  XMM6, [g_XMPi]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SUBPS   XMM6, XMM7
           // t0 = _mm_and_ps(nonnegative, t0);
           ANDPS   XMM7, XMM0
           // t1 = _mm_andnot_ps(nonnegative, t1);
           ANDNPS  XMM6, XMM0
           // t0 = _mm_or_ps(t0, t1);
           ORPS    XMM7,XMM6
           // t0 = _mm_sub_ps(g_XMHalfPi, t0);
           MOVUPS  XMM0,[g_XMHalfPi]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SUBPS   XMM0, XMM7
           // return t0;
           MOVUPS  [result], XMM0
end;



function XMVectorASinEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // nonnegative = _mm_cmpge_ps(V, g_XMZero);
           MOVUPS  XMM0, [g_XMZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           MOVUPS  XMM1,XMM0
           CMPPS   XMM0, [V], 2
           //  mvalue = _mm_sub_ps(g_XMZero, V);
           SUBPS   XMM1, [V]
           //  x = _mm_max_ps(V, mvalue);  // |V|
           MAXPS   XMM1, [V]

           // Compute (1-|V|), clamp to zero to avoid sqrt of negative number.
           //  oneMValue = _mm_sub_ps(g_XMOne, x);
           MOVUPS  XMM2, [g_XMOne]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SUBPS   XMM2, XMM1
           //  clampOneMValue = _mm_max_ps(g_XMZero, oneMValue);
           MAXPS   XMM2, g_XMZero
           //  root = _mm_sqrt_ps(clampOneMValue);  // sqrt(1-|V|)
           SQRTPS  XMM2, XMM2
           // Compute polynomial approximation
           //  AEC = g_XMArcEstCoefficients;
           MOVUPS  XMM3,[g_XMArcEstCoefficients]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           //  vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(3, 3, 3, 3) );
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           //  t0 = _mm_mul_ps(vConstants, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(2, 2, 2, 2) );
           MOVUPS  XMM4,[g_XMArcEstCoefficients]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_2_2_2_2
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(1, 1, 1, 1) );
           MOVUPS  XMM4,[g_XMArcEstCoefficients]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_1_1_1_1
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(0, 0, 0, 0) );
           MOVUPS  XMM4,[g_XMArcEstCoefficients]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_0_0_0_0
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, root);
           MULPS   XMM3, XMM2

           //  t1 = _mm_sub_ps(g_XMPi, t0);
           MOVUPS  XMM5, [g_XMPi]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SUBPS   XMM5, XMM3
           // t0 = _mm_and_ps(nonnegative, t0);
           ANDPS   XMM3, XMM0
           // t1 = _mm_andnot_ps(nonnegative, t1);
           ANDNPS  XMM5, XMM0
           // t0 = _mm_or_ps(t0, t1);
           ORPS    XMM3,XMM5
           // t0 = _mm_sub_ps(g_XMHalfPi, t0);
           MOVUPS  XMM6, [g_XMHalfPi]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SUBPS   XMM6, XMM3
           // return t0;
           MOVUPS  [result], XMM6
end;



function XMVectorACos(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // __m128 nonnegative = _mm_cmpge_ps(V, g_XMZero);
           MOVUPS  XMM0,[g_XMZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           MOVUPS  XMM1, XMM0
           CMPPS   XMM0, [V], 2 // nonnegative = XMM0
           // __m128 mvalue = _mm_sub_ps(g_XMZero, V);
           SUBPS   XMM1, [V]  // mvalue = XMM1
           //    __m128 x = _mm_max_ps(V, mvalue);  // |V|
           MAXPS   XMM1, [V] // x = XMM1

           // Compute (1-|V|), clamp to zero to avoid sqrt of negative number.
           // __m128 oneMValue = _mm_sub_ps(g_XMOne, x);
           MOVUPS  XMM2, [g_XMOne]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           SUBPS   XMM2, XMM1
           //  __m128 clampOneMValue = _mm_max_ps(g_XMZero, oneMValue);
           MAXPS   XMM2, [g_XMZero]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // __m128 root = _mm_sqrt_ps(clampOneMValue);  // sqrt(1-|V|)
           SQRTPS  XMM2, XMM2    // root = XMM2

           // Compute polynomial approximation
           // AC1 = g_XMArcCoefficients1;
           MOVUPS  XMM3, [g_XMArcCoefficients1]
           {$IFDEF CPU32}
           {$ELSE}
           {$ENDIF}
           // vConstants = XM_PERMUTE_PS( AC1, _MM_SHUFFLE(3, 3, 3, 3) );
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           // C t0 = _mm_mul_ps(vConstants, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AC1, _MM_SHUFFLE(2, 2, 2, 2) );
           MOVUPS  XMM4, [g_XMArcCoefficients1]
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_2_2_2_2
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AC1, _MM_SHUFFLE(1, 1, 1, 1) );
           MOVUPS  XMM4, [g_XMArcCoefficients1]
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_1_1_1_1
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AC1, _MM_SHUFFLE(0, 0, 0, 0) );
           MOVUPS  XMM4, [g_XMArcCoefficients1]
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_0_0_0_0
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM3, XMM1

           //  AC0 = g_XMArcCoefficients0;
           MOVUPS  XMM4, [g_XMArcCoefficients0]
           // vConstants = XM_PERMUTE_PS( AC0, _MM_SHUFFLE(3, 3, 3, 3) );
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3

           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AC0, _MM_SHUFFLE(2, 2, 2, 2) );
           MOVUPS  XMM4, [g_XMArcCoefficients0]
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_2_2_2_2
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AC0, _MM_SHUFFLE(1, 1, 1, 1) );
           MOVUPS  XMM4, [g_XMArcCoefficients0]
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_1_1_1_1
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AC0, _MM_SHUFFLE(0, 0, 0, 0) );
           MOVUPS  XMM4, [g_XMArcCoefficients0]
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_0_0_0_0
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, root);
           MULPS   XMM3, XMM2

           //  t1 = _mm_sub_ps(g_XMPi, t0);
           MOVUPS  XMM5, [g_XMPi]
           SUBPS   XMM5, XMM3
           // t0 = _mm_and_ps(nonnegative, t0);
           ANDPS   XMM3, XMM0
           // t1 = _mm_andnot_ps(nonnegative, t1);
           ANDNPS  XMM5, XMM0
           // t0 = _mm_or_ps(t0, t1);
           ORPS    XMM3,XMM5
           // return t0;
           MOVUPS  [result],XMM3
end;



function XMVectorACosEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  nonnegative = _mm_cmpge_ps(V, g_XMZero);
           MOVUPS  XMM0,[g_XMZero]
           MOVUPS  XMM1, XMM0
           CMPPS   XMM0, [V], 2
           //  mvalue = _mm_sub_ps(g_XMZero, V);

           SUBPS   XMM1, [V]
           //  x = _mm_max_ps(V, mvalue);  // |V|
           MAXPS   XMM1, [V]

           // Compute (1-|V|), clamp to zero to avoid sqrt of negative number.
           //  oneMValue = _mm_sub_ps(g_XMOne, x);
           MOVUPS  XMM2,[g_XMOne]
           SUBPS   XMM2, XMM1
           //  clampOneMValue = _mm_max_ps(g_XMZero, oneMValue);
           MAXPS   XMM2, [g_XMZero]
           //  root = _mm_sqrt_ps(clampOneMValue);  // sqrt(1-|V|)
           SQRTPS  XMM2, XMM2

           // Compute polynomial approximation
           //  AEC = g_XMArcEstCoefficients;
           MOVUPS  XMM3, [g_XMArcEstCoefficients]
           // vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(3, 3, 3, 3) );
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           //  t0 = _mm_mul_ps(vConstants, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(2, 2, 2, 2) );
           MOVUPS  XMM4, [g_XMArcEstCoefficients]
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_2_2_2_2
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(1, 1, 1, 1) );
           MOVUPS  XMM4, [g_XMArcEstCoefficients]
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_1_1_1_1
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, x);
           MULPS   XMM3, XMM1

           // vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(0, 0, 0, 0) );
           MOVUPS  XMM4, [g_XMArcEstCoefficients]
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_0_0_0_0
           // t0 = _mm_add_ps(t0, vConstants);
           ADDPS   XMM3, XMM4
           // t0 = _mm_mul_ps(t0, root);
           MULPS   XMM3, XMM2

           //  t1 = _mm_sub_ps(g_XMPi, t0);
           MOVUPS  XMM5, [g_XMPi]
           SUBPS   XMM5, XMM3
           // t0 = _mm_and_ps(nonnegative, t0);
           ANDPS   XMM3, XMM0
           // t1 = _mm_andnot_ps(nonnegative, t1);
           ANDNPS  XMM5, XMM0
           // t0 = _mm_or_ps(t0, t1);
           ORPS    XMM3,XMM5
           // return t0;
           MOVUPS  [result], XMM3
end;



function XMVectorATan(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // XMVectorAbs(V);
           XORPS   XMM0,XMM0
           SUBPS   XMM0,[V]
           MAXPS   XMM0,[V] // XMM0 = absV

           // invV = _mm_div_ps(g_XMOne, V);
           MOVUPS  XMM1, [g_XMOne]
           DIVPS   XMM1, [V]
           //  comp = _mm_cmpgt_ps(V, g_XMOne);
           MOVUPS  XMM2,[g_XMOne]
           CMPPS   XMM2, [V], 1
           //  select0 = _mm_and_ps(comp, g_XMOne);
           MOVUPS  XMM3, [g_XMOne]
           ANDPS   XMM3, XMM2
           //  select1 = _mm_andnot_ps(comp, g_XMNegativeOne);
           ANDNPS  XMM2, [g_XMNegativeOne]
           //  sign = _mm_or_ps(select0, select1);
           ORPS    XMM3,XMM2
           // comp = _mm_cmple_ps(absV, g_XMOne);
           CMPPS   XMM0, [g_XMOne], 2
           // select0 = _mm_and_ps(comp, g_XMZero);
           MOVUPS  XMM7, [g_XMZero]
           ANDPS   XMM7, XMM0
           // select1 = _mm_andnot_ps(comp, sign);
           ANDNPS  XMM3, XMM0
           // sign = _mm_or_ps(select0, select1);
           ORPS    XMM7, XMM3 // XMM7 = sign
           // select0 = _mm_and_ps(comp, V);
           MOVUPS  XMM3, XMM0
           ANDPS   XMM3, [V]
           // select1 = _mm_andnot_ps(comp, invV);
           ANDNPS  XMM0, XMM1
           //  x = _mm_or_ps(select0, select1);
           ORPS    XMM0,XMM3
           //  x2 = _mm_mul_ps(x, x);
           MOVUPS  XMM1,XMM0
           MULPS   XMM1, XMM1

           // Compute polynomial approximation
           //  TC1 = g_XMATanCoefficients1;
           MOVUPS  XMM4, [g_XMATanCoefficients1]
           //  vConstants = XM_PERMUTE_PS( TC1, _MM_SHUFFLE(3, 3, 3, 3) );
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3
           //  Result = _mm_mul_ps(vConstants, x2);
           MULPS   XMM4, XMM1

           //  vConstants = XM_PERMUTE_PS( TC1, _MM_SHUFFLE(2, 2, 2, 2) );
           MOVUPS  XMM5, [g_XMATanCoefficients1]
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_2_2_2_2
           // Result = _mm_add_ps(Result, vConstants);
           ADDPS   XMM4, XMM5
           // Result = _mm_mul_ps(Result, x2);
           MULPS   XMM4, XMM1

           // vConstants = XM_PERMUTE_PS( TC1, _MM_SHUFFLE(1, 1, 1, 1) );
           MOVUPS  XMM5, [g_XMATanCoefficients1]
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_1_1_1_1
           // Result = _mm_add_ps(Result, vConstants);
           ADDPS   XMM4, XMM5
           // Result = _mm_mul_ps(Result, x2);
           MULPS   XMM4, XMM1

           // vConstants = XM_PERMUTE_PS( TC1, _MM_SHUFFLE(0, 0, 0, 0) );
           MOVUPS  XMM5, [g_XMATanCoefficients1]
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_0_0_0_0
           // Result = _mm_add_ps(Result, vConstants);
           ADDPS   XMM4, XMM5
           // Result = _mm_mul_ps(Result, x2);
           MULPS   XMM4, XMM1

           //  TC0 = g_XMATanCoefficients0;
           MOVUPS  XMM5, [g_XMATanCoefficients0]
           // vConstants = XM_PERMUTE_PS( TC0, _MM_SHUFFLE(3, 3, 3, 3) );
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_3_3_3_3
           // Result = _mm_add_ps(Result, vConstants);
           ADDPS   XMM4, XMM5
           // Result = _mm_mul_ps(Result, x2);
           MULPS   XMM4, XMM1

           // vConstants = XM_PERMUTE_PS( TC0, _MM_SHUFFLE(2, 2, 2, 2) );
           MOVUPS  XMM5, [g_XMATanCoefficients0]
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_2_2_2_2
           // Result = _mm_add_ps(Result, vConstants);
           ADDPS   XMM4, XMM5
           // Result = _mm_mul_ps(Result, x2);
           MULPS   XMM4, XMM1

           // vConstants = XM_PERMUTE_PS( TC0, _MM_SHUFFLE(1, 1, 1, 1) );
           MOVUPS  XMM5, [g_XMATanCoefficients0]
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_1_1_1_1
           // Result = _mm_add_ps(Result, vConstants);
           ADDPS   XMM4, XMM5
           // Result = _mm_mul_ps(Result, x2);
           MULPS   XMM4, XMM1

           // vConstants = XM_PERMUTE_PS( TC0, _MM_SHUFFLE(0, 0, 0, 0) );
           MOVUPS  XMM5, [g_XMATanCoefficients0]
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_0_0_0_0
           // Result = _mm_add_ps(Result, vConstants);
           ADDPS   XMM4, XMM5
           // Result = _mm_mul_ps(Result, x2);
           MULPS   XMM4, XMM1
           // Result = _mm_add_ps(Result, g_XMOne);
           ADDPS   XMM4, [g_XMOne]
           // Result = _mm_mul_ps(Result, x);
           MULPS   XMM4, XMM0
           //  result1 = _mm_mul_ps(sign, g_XMHalfPi);
           MOVUPS  XMM5,[g_XMHalfPi]
           MULPS   XMM5, XMM7
           // result1 = _mm_sub_ps(result1, Result);
           SUBPS   XMM5, XMM4

           // comp = _mm_cmpeq_ps(sign, g_XMZero);
           CMPPS   XMM7, [g_XMZero], 0
           // select0 = _mm_and_ps(comp, Result);
           ANDPS   XMM4, XMM7
           // select1 = _mm_andnot_ps(comp, result1);
           ANDNPS  XMM7, XMM5
           // Result = _mm_or_ps(select0, select1);
           ORPS    XMM4, XMM7
           // return Result;
           MOVUPS  [result], XMM4
end;



function XMVectorATanEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // XMVectorAbs(V);
           XORPS   XMM0,XMM0
           SUBPS   XMM0,[V]
           MAXPS   XMM0,[V] // XMM0 = absV

           //  invV = _mm_div_ps(g_XMOne, V);
           MOVUPS  XMM1,[g_XMOne]
           MOVUPS  XMM3,XMM1
           DIVPS   XMM1, [V]  // XMM1 = invV
           //  comp = _mm_cmpgt_ps(V, g_XMOne);
           MOVUPS  XMM2, [g_XMOne]
           CMPPS   XMM2, [V], 1
           //  select0 = _mm_and_ps(comp, g_XMOne);
           ANDPS   XMM3, XMM2
           //  select1 = _mm_andnot_ps(comp, g_XMNegativeOne);
           ANDNPS  XMM2, [g_XMNegativeOne]
           //  sign = _mm_or_ps(select0, select1);
           ORPS    XMM2, XMM3 //  XMM2 = sign

           // comp = _mm_cmple_ps(absV, g_XMOne);
           CMPPS   XMM0, [g_XMOne], 2
           // select0 = _mm_and_ps(comp, g_XMZero);
           MOVUPS  XMM3, [g_XMZero]
           ANDPS   XMM3, XMM0
           // select1 = _mm_andnot_ps(comp, sign);
           ANDNPS  XMM2, XMM0
           // sign = _mm_or_ps(select0, select1);
           ORPS    XMM2, XMM3  //  XMM2 = sign
           // select0 = _mm_and_ps(comp, V);
           MOVUPS  XMM3,[V]
           ANDPS   XMM3, XMM0
           // select1 = _mm_andnot_ps(comp, invV);
           ANDNPS  XMM0, XMM1
           //  x = _mm_or_ps(select0, select1);
           ORPS    XMM0, XMM3 // XMM0 = x

           //  x2 = _mm_mul_ps(x, x);
           MOVUPS  XMM1,XMM0
           MULPS   XMM1, XMM1 // XMM1 = x2

           // Compute polynomial approximation
           //  AEC = g_XMATanEstCoefficients1;
           MOVUPS  XMM4, [g_XMATanEstCoefficients1]
           //  vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(3, 3, 3, 3) );
           SHUFPS  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3
           //  Result = _mm_mul_ps(vConstants, x2);
           MULPS   XMM4, XMM1

           // vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(2, 2, 2, 2) );
           MOVUPS  XMM5, [g_XMATanEstCoefficients1]
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_2_2_2_2
           // Result = _mm_add_ps(Result, vConstants);
           ADDPS   XMM4, XMM5
           // Result = _mm_mul_ps(Result, x2);
           MULPS   XMM4, XMM1

           // vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(1, 1, 1, 1) );
           MOVUPS  XMM5, [g_XMATanEstCoefficients1]
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_1_1_1_1
           // Result = _mm_add_ps(Result, vConstants);
           ADDPS   XMM4, XMM5
           // Result = _mm_mul_ps(Result, x2);
           MULPS   XMM4, XMM1

           // vConstants = XM_PERMUTE_PS( AEC, _MM_SHUFFLE(0, 0, 0, 0) );
           MOVUPS  XMM5, [g_XMATanEstCoefficients1]
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_0_0_0_0
           // Result = _mm_add_ps(Result, vConstants);
           ADDPS   XMM4, XMM5
           // Result = _mm_mul_ps(Result, x2);
           MULPS   XMM4, XMM1

           // ATanEstCoefficients0 is already splatted
           // Result = _mm_add_ps(Result, g_XMATanEstCoefficients0);
           ADDPS   XMM4, [g_XMATanEstCoefficients0]
           // Result = _mm_mul_ps(Result, x);
           MULPS   XMM4, XMM0
           //  result1 = _mm_mul_ps(sign, g_XMHalfPi);
           MOVUPS  XMM5, [g_XMHalfPi]
           MULPS   XMM5, XMM2
           // result1 = _mm_sub_ps(result1, Result);
           SUBPS   XMM5, XMM4

           // comp = _mm_cmpeq_ps(sign, g_XMZero);
           CMPPS   XMM2, [g_XMZero], 0
           // select0 = _mm_and_ps(comp, Result);
           ANDPS   XMM4, XMM2
           // select1 = _mm_andnot_ps(comp, result1);
           ANDNPS  XMM5, XMM2
           // Result = _mm_or_ps(select0, select1);
           ORPS    XMM4, XMM5
           // return Result;
           MOVUPS  [result], XMM4
end;



function XMVectorATan2(constref Y: TXMVECTOR; constref X: TXMVECTOR): TXMVECTOR;
const
    ATan2Constants: TXMVECTORF32 = (f: (3.141592654, 1.570796327, 0.785398163, 3.141592654 * 3.0 / 4.0));
var
    Zero, ATanResultValid, Pi, PiOverTwo, PiOverFour, ThreePiOverFour: TXMVECTOR;
    YEqualsZero, XEqualsZero, XIsPositive: TXMVECTOR;
    YEqualsInfinity, XEqualsInfinity, YSign: TXMVECTOR;
    R1, R2, R3, R4, R5: TXMVECTOR;
    V, R0: TXMVECTOR;
begin
    // Return the inverse tangent of Y / X in the range of -Pi to Pi with the following exceptions:

    //     Y == 0 and X is Negative         -> Pi with the sign of Y
    //     y == 0 and x is positive         -> 0 with the sign of y
    //     Y != 0 and X == 0                -> Pi / 2 with the sign of Y
    //     Y != 0 and X is Negative         -> atan(y/x) + (PI with the sign of Y)
    //     X == -Infinity and Finite Y      -> Pi with the sign of Y
    //     X == +Infinity and Finite Y      -> 0 with the sign of Y
    //     Y == Infinity and X is Finite    -> Pi / 2 with the sign of Y
    //     Y == Infinity and X == -Infinity -> 3Pi / 4 with the sign of Y
    //     Y == Infinity and X == +Infinity -> Pi / 4 with the sign of Y



    Zero := XMVectorZero();
    ATanResultValid := XMVectorTrueInt();

    Pi := XMVectorSplatX(ATan2Constants);
    PiOverTwo := XMVectorSplatY(ATan2Constants);
    PiOverFour := XMVectorSplatZ(ATan2Constants);
    ThreePiOverFour := XMVectorSplatW(ATan2Constants);

    YEqualsZero := XMVectorEqual(Y, Zero);
    XEqualsZero := XMVectorEqual(X, Zero);
    XIsPositive := XMVectorAndInt(X, g_XMNegativeZero.v);
    XIsPositive := XMVectorEqualInt(XIsPositive, Zero);
    YEqualsInfinity := XMVectorIsInfinite(Y);
    XEqualsInfinity := XMVectorIsInfinite(X);

    YSign := XMVectorAndInt(Y, g_XMNegativeZero.v);
    Pi := XMVectorOrInt(Pi, YSign);
    PiOverTwo := XMVectorOrInt(PiOverTwo, YSign);
    PiOverFour := XMVectorOrInt(PiOverFour, YSign);
    ThreePiOverFour := XMVectorOrInt(ThreePiOverFour, YSign);

    R1 := XMVectorSelect(Pi, YSign, XIsPositive);
    R2 := XMVectorSelect(ATanResultValid, PiOverTwo, XEqualsZero);
    R3 := XMVectorSelect(R2, R1, YEqualsZero);
    R4 := XMVectorSelect(ThreePiOverFour, PiOverFour, XIsPositive);
    R5 := XMVectorSelect(PiOverTwo, R4, XEqualsInfinity);
    Result := XMVectorSelect(R3, R5, YEqualsInfinity);
    ATanResultValid := XMVectorEqualInt(Result, ATanResultValid);

    V := XMVectorDivide(Y, X);

    R0 := XMVectorATan(V);

    R1 := XMVectorSelect(Pi, g_XMNegativeZero, XIsPositive);
    R2 := XMVectorAdd(R0, R1);

    Result := XMVectorSelect(Result, R2, ATanResultValid);

end;



function XMVectorATan2Est(constref Y: TXMVECTOR; constref X: TXMVECTOR): TXMVECTOR; inline;
const
    ATan2Constants: TXMVECTORF32 = (f: (3.141592654, 1.570796327, 0.785398163, 2.3561944905));
var
    Zero, ATanResultValid, Pi, PiOverTwo, PiOverFour, ThreePiOverFour, YEqualsZero, XEqualsZero, XIsPositive: TXMVECTOR;
    YEqualsInfinity, XEqualsInfinity, YSign: TXMVECTOR;
    R1, R2, R3, R4, R5: TXMVECTOR;
    Reciprocal, V, R0: TXMVECTOR;
begin
    Zero := XMVectorZero();
    ATanResultValid := XMVectorTrueInt();

    Pi := XMVectorSplatX(ATan2Constants);
    PiOverTwo := XMVectorSplatY(ATan2Constants);
    PiOverFour := XMVectorSplatZ(ATan2Constants);
    ThreePiOverFour := XMVectorSplatW(ATan2Constants);

    YEqualsZero := XMVectorEqual(Y, Zero);
    XEqualsZero := XMVectorEqual(X, Zero);
    XIsPositive := XMVectorAndInt(X, g_XMNegativeZero.v);
    XIsPositive := XMVectorEqualInt(XIsPositive, Zero);
    YEqualsInfinity := XMVectorIsInfinite(Y);
    XEqualsInfinity := XMVectorIsInfinite(X);

    YSign := XMVectorAndInt(Y, g_XMNegativeZero.v);
    Pi := XMVectorOrInt(Pi, YSign);
    PiOverTwo := XMVectorOrInt(PiOverTwo, YSign);
    PiOverFour := XMVectorOrInt(PiOverFour, YSign);
    ThreePiOverFour := XMVectorOrInt(ThreePiOverFour, YSign);

    R1 := XMVectorSelect(Pi, YSign, XIsPositive);
    R2 := XMVectorSelect(ATanResultValid, PiOverTwo, XEqualsZero);
    R3 := XMVectorSelect(R2, R1, YEqualsZero);
    R4 := XMVectorSelect(ThreePiOverFour, PiOverFour, XIsPositive);
    R5 := XMVectorSelect(PiOverTwo, R4, XEqualsInfinity);
    Result := XMVectorSelect(R3, R5, YEqualsInfinity);
    ATanResultValid := XMVectorEqualInt(Result, ATanResultValid);

    Reciprocal := XMVectorReciprocalEst(X);
    V := XMVectorMultiply(Y, Reciprocal);
    R0 := XMVectorATanEst(V);

    R1 := XMVectorSelect(Pi, g_XMNegativeZero, XIsPositive);
    R2 := XMVectorAdd(R0, R1);

    Result := XMVectorSelect(Result, R2, ATanResultValid);
end;



function XMVectorLerp(constref V0: TXMVECTOR; constref V1: TXMVECTOR; constref t: single): TXMVECTOR; assembler;
asm

           //  L = _mm_sub_ps( V1, V0 );
           MOVUPS  XMM0,[V1]
           SUBPS   XMM0, [V0]
           //  S = _mm_set_ps1( t );
           MOVSS   XMM1, [t]
           SHUFPS  XMM1, XMM1, 0
           //  Result = _mm_mul_ps( L, S );
           MULPS   XMM0, XMM1
           //  return _mm_add_ps( Result, V0 );
           ADDPS   XMM0, [V0]
           MOVUPS  [result], XMM0
end;



function XMVectorLerpV(constref V0: TXMVECTOR; constref V1: TXMVECTOR; constref T: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Length = _mm_sub_ps( V1, V0 );
           MOVUPS  XMM0,[V1]
           SUBPS   XMM0, [V0]
           //  Result = _mm_mul_ps( Length, T );
           MULPS   XMM0, [T]
           // return _mm_add_ps( Result, V0 );
           ADDPS   XMM0, [V0]
           MOVUPS  [result], XMM0
end;



function XMVectorHermite(constref Position0: TXMVECTOR; constref Tangent0: TXMVECTOR; constref Position1: TXMVECTOR;
    constref Tangent1: TXMVECTOR; constref t: single): TXMVECTOR;
var
    t2, t3: single;
    f1, f2, f3, f4: single;
begin
    t2 := t * t;
    t3 := t * t2;
    f1 := 2.0 * t3 - 3.0 * t2 + 1.0;
    f2 := t3 - 2.0 * t2 + t;
    f3 := -2.0 * t3 + 3.0 * t2;
    f4 := t3 - t2;

    asm
               //  P0 = _mm_set_ps1(2.0 * t3 - 3.0 * t2 + 1.0);
               MOVSS   XMM0, [f1]
               SHUFPS  XMM0, XMM0, 0
               //  T0 = _mm_set_ps1(t3 - 2.0 * t2 + t);
               MOVSS   XMM1, [f2]
               SHUFPS  XMM1, XMM1, 0
               //  P1 = _mm_set_ps1(-2.0 * t3 + 3.0 * t2);
               MOVSS   XMM2, [f3]
               SHUFPS  XMM2, XMM2, 0
               //  T1 = _mm_set_ps1(t3 - t2);
               MOVSS   XMM3, [f4]
               SHUFPS  XMM3, XMM3, 0

               //  vResult = _mm_mul_ps(P0, Position0);
               MULPS   XMM0, [Position0]
               //  vTemp = _mm_mul_ps(T0, Tangent0);
               MULPS   XMM1, [Tangent0]
               // vResult = _mm_add_ps(vResult,vTemp);
               ADDPS   XMM0, XMM1
               // vTemp = _mm_mul_ps(P1, Position1);
               MULPS   XMM2, [Position1]
               // vResult = _mm_add_ps(vResult,vTemp);
               ADDPS   XMM0, XMM2
               // vTemp = _mm_mul_ps(T1, Tangent1);
               MULPS   XMM3, [Tangent1]
               // vResult = _mm_add_ps(vResult,vTemp);
               ADDPS   XMM0, XMM3
               // return vResult;
               MOVUPS  [result], XMM0
    end;
end;



function XMVectorHermiteV(constref Position0: TXMVECTOR; constref Tangent0: TXMVECTOR; constref Position1: TXMVECTOR;
    constref Tangent1: TXMVECTOR; constref T: TXMVECTOR): TXMVECTOR; assembler;
const
    CatMulT2: TXMVECTORF32 = (f: (-3.0, -2.0, 3.0, -1.0));
    CatMulT3: TXMVECTORF32 = (f: (2.0, 1.0, -2.0, 1.0));
asm
           //  T2 = _mm_mul_ps(T,T);
           MOVUPS  XMM0, [T]
           MULPS   XMM0, XMM0
           //  T3 = _mm_mul_ps(T,T2);
           MULPS   XMM1, [T]
           // Mul by the constants against t^2
           // T2 = _mm_mul_ps(T2,CatMulT2);
           MULPS   XMM0, [CatMulT2]
           // Mul by the constants against t^3
           // T3 = _mm_mul_ps(T3,CatMulT3);
           MULPS   XMM1, [CatMulT3]
           // T3 now has the pre-result.
           // T3 = _mm_add_ps(T3,T2);
           ADDPS   XMM1, XMM0
           // I need to add t.y only
           // T2 = _mm_and_ps(T,g_XMMaskY);
           MOVUPS  XMM0,[T]
           ANDPS   XMM0, [g_XMMaskY]
           // T3 = _mm_add_ps(T3,T2);
           ADDPS   XMM1, XMM0
           // Add 1.0f to x
           // T3 = _mm_add_ps(T3,g_XMIdentityR0);
           ADDPS   XMM1, [g_XMIdentityR0]
           // Now, I have the constants created
           // Mul the x constant to Position0
           //  vResult = XM_PERMUTE_PS(T3,_MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM5,XMM1
           SHUFPS  XMM5, XMM5, _MM_SHUFFLE_0_0_0_0
           // vResult = _mm_mul_ps(vResult,Position0);
           MULPS   XMM5, [Position0]
           // Mul the y constant to Tangent0
           // T2 = XM_PERMUTE_PS(T3,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM6,XMM1
           SHUFPS  XMM6, XMM6, _MM_SHUFFLE_1_1_1_1
           // T2 = _mm_mul_ps(T2,Tangent0);
           MULPS   XMM6, [Tangent0]
           // vResult = _mm_add_ps(vResult,T2);
           ADDPS   XMM5, XMM6
           // Mul the z constant to Position1
           // T2 = XM_PERMUTE_PS(T3,_MM_SHUFFLE(2,2,2,2));
           MOVUPS  XMM6,XMM1
           SHUFPS  XMM6, XMM6, _MM_SHUFFLE_2_2_2_2
           // T2 = _mm_mul_ps(T2,Position1);
           MULPS   XMM6, [Position1]
           // vResult = _mm_add_ps(vResult,T2);
           ADDPS   XMM5, XMM6
           // Mul the w constant to Tangent1
           // T3 = XM_PERMUTE_PS(T3,_MM_SHUFFLE(3,3,3,3));
           MOVUPS  XMM6,XMM1
           SHUFPS  XMM6, XMM6, _MM_SHUFFLE_3_3_3_3
           // T3 = _mm_mul_ps(T3,Tangent1);
           MULPS   XMM6, [Tangent1]
           // vResult = _mm_add_ps(vResult,T3);
           ADDPS   XMM5, XMM6
           // return vResult;
           MOVUPS  [result], XMM5
end;



function XMVectorCatmullRom(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref Position3: TXMVECTOR; constref t: single): TXMVECTOR; inline;
var
    t2, t3, f1, f2, f3, f4: single;
begin
    t2 := t * t;
    t3 := t * t2;
    f1 := (-t3 + 2.0 * t2 - t) * 0.5;
    f2 := (3.0 * t3 - 5.0 * t2 + 2.0) * 0.5;
    f3 := (-3.0 * t3 + 4.0 * t2 + t) * 0.5;
    f4 := (t3 - t2) * 0.5;
    asm
               //  P0 = _mm_set_ps1((-t3 + 2.0 * t2 - t) * 0.5);
               MOVSS   XMM0, [f1]
               SHUFPS  XMM0, XMM0, 0
               //  P1 = _mm_set_ps1((3.0f * t3 - 5.0f * t2 + 2.0f) * 0.5f);
               MOVSS   XMM1, [f2]
               SHUFPS  XMM1, XMM1, 0
               //  P2 = _mm_set_ps1((-3.0f * t3 + 4.0f * t2 + t) * 0.5f);
               MOVSS   XMM2, [f3]
               SHUFPS  XMM2, XMM2, 0
               //  P3 = _mm_set_ps1((t3 - t2) * 0.5f);
               MOVSS   XMM3, [f4]
               SHUFPS  XMM3, XMM3, 0

               // P0 = _mm_mul_ps(P0, Position0);
               MULPS   XMM0, [Position0]
               // P1 = _mm_mul_ps(P1, Position1);
               MULPS   XMM1, [Position1]
               // P2 = _mm_mul_ps(P2, Position2);
               MULPS   XMM2, [Position2]
               // P3 = _mm_mul_ps(P3, Position3);
               MULPS   XMM3, [Position3]
               // P0 = _mm_add_ps(P0,P1);
               ADDPS   XMM0, XMM1
               // P2 = _mm_add_ps(P2,P3);
               ADDPS   XMM2, XMM3
               // P0 = _mm_add_ps(P0,P2);
               ADDPS   XMM0, XMM2
               // return P0;
               MOVUPS  [result], XMM0

    end;
end;



function XMVectorCatmullRomV(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref Position3: TXMVECTOR; constref T: TXMVECTOR): TXMVECTOR; assembler;
const
    Catmul2: TXMVECTORF32 = (f: (2.0, 2.0, 2.0, 2.0));
    Catmul3: TXMVECTORF32 = (f: (3.0, 3.0, 3.0, 3.0));
    Catmul4: TXMVECTORF32 = (f: (4.0, 4.0, 4.0, 4.0));
    Catmul5: TXMVECTORF32 = (f: (5.0, 5.0, 5.0, 5.0));
asm

           // Cache T^2 and T^3
           //  T2 = _mm_mul_ps(T,T);
           MOVUPS  XMM0,[T]
           MULPS   XMM0, XMM0
           //  T3 = _mm_mul_ps(T,T2);
           MOVUPS  XMM1,XMM0
           MULPS   XMM1, [T]
           // Perform the Position0 term
           //  vResult = _mm_add_ps(T2,T2);
           MOVUPS  XMM3, XMM0
           ADDPS   XMM3, XMM0
           // vResult = _mm_sub_ps(vResult,T);
           SUBPS   XMM3, [T]
           // vResult = _mm_sub_ps(vResult,T3);
           SUBPS   XMM3, XMM1
           // vResult = _mm_mul_ps(vResult,Position0);
           MULPS   XMM3, [Position0]
           // Perform the Position1 term and add
           //  vTemp = _mm_mul_ps(T3,Catmul3);
           MOVUPS  XMM4, XMM1
           MULPS   XMM4, [Catmul3]
           //  vTemp2 = _mm_mul_ps(T2,Catmul5);
           MOVUPS  XMM5, XMM0
           MULPS   XMM5, [Catmul5]
           // vTemp = _mm_sub_ps(vTemp,vTemp2);
           SUBPS   XMM4, XMM5
           // vTemp = _mm_add_ps(vTemp,Catmul2);
           ADDPS   XMM4, [Catmul2]
           // vTemp = _mm_mul_ps(vTemp,Position1);
           MULPS   XMM4, [Position1]
           // vResult = _mm_add_ps(vResult,vTemp);
           ADDPS   XMM3, XMM4
           // Perform the Position2 term and add
           // vTemp = _mm_mul_ps(T2,Catmul4);
           MOVUPS  XMM4,XMM0
           MULPS   XMM4, [Catmul4]
           // vTemp2 = _mm_mul_ps(T3,Catmul3);
           MOVUPS  XMM5, XMM1
           MULPS   XMM5, [Catmul3]
           // vTemp = _mm_sub_ps(vTemp,vTemp2);
           SUBPS   XMM4, XMM5
           // vTemp = _mm_add_ps(vTemp,T);
           ADDPS   XMM4, [T]
           // vTemp = _mm_mul_ps(vTemp,Position2);
           MULPS   XMM4, [Position2]
           // vResult = _mm_add_ps(vResult,vTemp);
           ADDPS   XMM3, XMM4
           // Position3 is the last term
           // T3 = _mm_sub_ps(T3,T2);
           SUBPS   XMM1, XMM0
           // T3 = _mm_mul_ps(T3,Position3);
           MULPS   XMM1, [Position3]
           // vResult = _mm_add_ps(vResult,T3);
           ADDPS   XMM3, XMM1
           // Multiply by 0.5f and exit
           // vResult = _mm_mul_ps(vResult,g_XMOneHalf);
           MULPS   XMM3, [g_XMOneHalf]
           // return vResult;
           MOVUPS  [result], XMM3
end;




function XMVectorBaryCentric(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref f: single; constref g: single): TXMVECTOR; assembler;
asm
           // R1 = _mm_sub_ps(Position1,Position0);
           MOVUPS  XMM0, [Position1]
           SUBPS   XMM0, [Position0]
           //  SF = _mm_set_ps1(f);
           MOVSS   XMM1, [f]
           SHUFPS  XMM1, XMM1, 0
           //  R2 = _mm_sub_ps(Position2,Position0);
           MOVUPS  XMM2, [Position2]
           SUBPS   XMM2, [Position0]
           //  SG = _mm_set_ps1(g);
           MOVSS   XMM3, [g]
           SHUFPS  XMM3, XMM3, 0
           // R1 = _mm_mul_ps(R1,SF);
           MULPS   XMM0, XMM1
           // R2 = _mm_mul_ps(R2,SG);
           MULPS   XMM2, XMM3
           // R1 = _mm_add_ps(R1,Position0);
           ADDPS   XMM0, [Position0]
           // R1 = _mm_add_ps(R1,R2);
           ADDPS   XMM0, XMM2
           // return R1;
           MOVUPS  [result], XMM0
end;



function XMVectorBaryCentricV(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref F: TXMVECTOR; constref G: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  R1 = _mm_sub_ps(Position1,Position0);
           MOVUPS  XMM0, [Position1]
           SUBPS   XMM0, [Position0]
           //  R2 = _mm_sub_ps(Position2,Position0);
           MOVUPS  XMM1, [Position2]
           SUBPS   XMM1, [Position0]
           // R1 = _mm_mul_ps(R1,F);
           MULPS   XMM0, [F]
           // R2 = _mm_mul_ps(R2,G);
           MULPS   XMM1, [G]
           // R1 = _mm_add_ps(R1,Position0);
           ADDPS   XMM0, [Position0]
           // R1 = _mm_add_ps(R1,R2);
           ADDPS   XMM0, XMM1
           // return R1;
           MOVUPS  [result], XMM0
end;


{***************************************************************************
 *
 * 2D Vector
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------


function XMVector2Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           // vTemp = _mm_cmpeq_ps(V1,V2);
           MOVUPS  XMM0, [V1]
           CMPPS   XMM0, [V2], 0
           // z and w are don't care
           // return (((_mm_movemask_ps(vTemp)&3)==3) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;



function XMVector2EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32; assembler;
asm
           // vTemp = _mm_cmpeq_ps(V1,V2);
           MOVUPS  XMM0,[V1]
           CMPPS   XMM0, [V2], 0
           // z and w are don't care
           // int iTest = _mm_movemask_ps(vTemp)&3;
           MOVMSKPS EAX, XMM0
           MOV     EDX,XM_CRMASK_CR6FALSE
           CMP     EAX, $0
           JE      @Finished

           MOV     EDX,XM_CRMASK_CR6TRUE
           AND     EAX, $3
           CMP     EAX, $3
           JE      @Finished
           MOV     EDX,$0
           @Finished:
           MOV     [result],EDX
end;



function XMVector2EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           // vTemp = _mm_cmpeq_epi32(_mm_castps_si128(V1),_mm_castps_si128(V2));
           MOVUPS  XMM0,[V1]
           PCMPEQD XMM0, [V2]
           // return (((_mm_movemask_ps(_mm_castsi128_ps(vTemp))&3)==3) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;



function XMVector2EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32; // assembler
asm
           // vTemp = _mm_cmpeq_epi32(_mm_castps_si128(V1),_mm_castps_si128(V2));
           MOVUPS  XMM0, [V1]
           PCMPEQD XMM0, [V2]
           // int iTest = _mm_movemask_ps(_mm_castsi128_ps(vTemp))&3;
           MOVMSKPS EAX, XMM0
           MOV     EDX,XM_CRMASK_CR6FALSE
           CMP     EAX, $0
           JE      @Finished

           MOV     EDX,XM_CRMASK_CR6TRUE
           AND     EAX, $3
           CMP     EAX, $3
           JE      @Finished
           MOV     EDX,$0
           @Finished:
           MOV     [result],EDX
end;



function XMVector2NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean; assembler;
asm
           // Get the difference
           //  vDelta = _mm_sub_ps(V1,V2);
           MOVUPS  XMM0, [V1]
           SUBPS   XMM0, [V2]
           // Get the absolute value of the difference
           // vTemp = _mm_setzero_ps();
           XORPS   XMM1,XMM1
           // vTemp = _mm_sub_ps(vTemp,vDelta);
           SUBPS   XMM1, XMM0
           // vTemp = _mm_max_ps(vTemp,vDelta);
           MAXPS   XMM1, XMM0
           // vTemp = _mm_cmple_ps(vTemp,Epsilon);
           CMPPS   XMM1, [Epsilon], 2
           // z and w are don't care
           // return (((_mm_movemask_ps(vTemp)&3)==0x3) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;



function XMVector2NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           // vTemp = _mm_cmpeq_ps(V1,V2);
           MOVUPS  XMM0, [V1]
           CMPPS   XMM0, [V2], 0
           // z and w are don't care
           // return (((_mm_movemask_ps(vTemp)&3)!=3) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;



function XMVector2NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           // vTemp = _mm_cmpeq_epi32(_mm_castps_si128(V1),_mm_castps_si128(V2));
           MOVUPS  XMM0,[V1]
           PCMPEQD XMM0, [V2]
           //return (((_mm_movemask_ps(_mm_castsi128_ps(vTemp))&3)!=3) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;



function XMVector2Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           // vTemp = _mm_cmpgt_ps(V1,V2);
           MOVUPS  XMM0, [V1]
           CMPPS   XMM0, [V2], 1
           // z and w are don't care
           // return (((_mm_movemask_ps(vTemp)&3)==3) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;



function XMVector2GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32; assembler;
asm
           // vTemp = _mm_cmpgt_ps(V1,V2);
           MOVUPS  XMM0, [V2]
           CMPPS   XMM0, [V1], 1
           // int iTest = _mm_movemask_ps(vTemp)&3;
           MOVMSKPS EAX, XMM0
           MOV     EDX,XM_CRMASK_CR6FALSE
           CMP     EAX, $0
           JE      @Finished

           MOV     EDX,XM_CRMASK_CR6TRUE
           AND     EAX, $3
           CMP     EAX, $3
           JE      @Finished
           MOV     EDX,$0
           @Finished:
           MOV     [result],EDX
end;



function XMVector2GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           // vTemp = _mm_cmpge_ps(V1,V2);
           MOVUPS  XMM0,[V2]
           CMPPS   XMM0, [V1], 2
           // return (((_mm_movemask_ps(vTemp)&3)==3) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;



function XMVector2GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32; assembler;
asm
           // vTemp = _mm_cmpge_ps(V1,V2);
           MOVUPS  XMM0,[V2]
           CMPPS   XMM0, [V1], 2
           // int iTest = _mm_movemask_ps(vTemp)&3;
           MOVMSKPS EAX, XMM0
           MOV     EDX,XM_CRMASK_CR6FALSE
           CMP     EAX, $0
           JE      @Finished

           MOV     EDX,XM_CRMASK_CR6TRUE
           AND     EAX, $3
           CMP     EAX, $3
           JE      @Finished
           MOV     EDX,$0
           @Finished:
           MOV     [result],EDX
end;



function XMVector2Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           MOVUPS  XMM0,[v1]
           CMPPS   XMM0, [v2] , 1
           MOVMSKPS EAX, XMM0
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;




function XMVector2LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           MOVUPS  XMM1,[v1]
           CMPPS   XMM1, [v2], 2
           MOVMSKPS EAX, XMM1
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;



function XMVector2InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean; assembler;
asm
           // Test if less than or equal
           // XMVECTOR vTemp1 = _mm_cmple_ps(V,Bounds);
           MOVUPS  XMM0, [V]
           CMPPS   XMM0, [Bounds], 2
           // Negate the bounds
           // XMVECTOR vTemp2 = _mm_mul_ps(Bounds,g_XMNegativeOne);
           MOVUPS  XMM1, [Bounds]
           MULPS   XMM1, [g_XMNegativeOne]
           // Test if greater or equal (Reversed)
           // vTemp2 = _mm_cmple_ps(vTemp2,V);
           CMPPS   XMM1, [V], 2
           // Blend answers
           // vTemp1 = _mm_and_ps(vTemp1,vTemp2);
           ANDPS   XMM0, XMM1
           // x and y in bounds? (z and w are don't care)
           // return (((_mm_movemask_ps(vTemp1)&0x3)==0x3) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;



function XMVector2IsNaN(constref V: TXMVECTOR): boolean; assembler;
asm
           // Test against itself. NaN is always not equal
           //  vTempNan = _mm_cmpneq_ps(V,V);
           MOVUPS  XMM0,[V]
           CMPPS   XMM0, [V], 4
           // If x or y are NaN, the mask is non-zero
           // return ((_mm_movemask_ps(vTempNan)&3) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;



function XMVector2IsInfinite(constref V: TXMVECTOR): boolean; assembler;
asm
           // Mask off the sign bit
           // vTemp = _mm_and_ps(V,g_XMAbsMask);
           MOVUPS  XMM0,[V]
           ANDPS   XMM0, [g_XMAbsMask]
           // Compare to infinity
           // vTemp = _mm_cmpeq_ps(vTemp,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 0
           // If x or z are infinity, the signs are true.
           // return ((_mm_movemask_ps(vTemp)&3) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $3
           CMP     EAX, $3
           SETE    [result]
end;


//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------


{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector2Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_dp_ps( V1, V2, $3f );
           MOVUPS  XMM0,[V1]
           DPPS    XMM0, [V2], $3f
           MOVUPS  [result], XMM0
end;



function XMVector2ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0x3f );
           MOVUPS  XMM0, [V]
           DPPS    XMM0, XMM0, $3F
           // return _mm_rsqrt_ps( vTemp );
           RSQRTPS XMM0, XMM0
           MOVUPS  [result],XMM0
end;



function XMVector2ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0x3f );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $3F
           //  vLengthSq = _mm_sqrt_ps( vTemp );
           SQRTPS  XMM1, XMM0
           // return _mm_div_ps( g_XMOne, vLengthSq );
           MOVUPS  XMM0,[g_XMOne]
           DIVPS   XMM0, XMM1
           MOVUPS  [result],XMM0
end;

{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector2Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vDot = _mm_mul_ps(V1, V2);
           MOVUPS  XMM0,[V1]
           MULPS   XMM0, [V2]
           // vDot = _mm_hadd_ps(vDot, vDot);
           HADDPS  XMM0, XMM0
           // vDot = _mm_moveldup_ps(vDot);
           MOVSLDUP XMM0, XMM0
           // return vDot;
           MOVUPS  [result],XMM0
end;



function XMVector2ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           //  vTemp = _mm_hadd_ps(vLengthSq, vLengthSq);
           MOVUPS  XMM1,XMM0
           HADDPS  XMM1, XMM1
           // vLengthSq = _mm_rsqrt_ss(vTemp);
           RSQRTSS XMM0, XMM1
           // vLengthSq = XM_PERMUTE_PS(vLengthSq, _MM_SHUFFLE(0, 0, 0, 0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;




function XMVector2ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp = _mm_hadd_ps(vLengthSq, vLengthSq);
           MOVUPS  XMM1,XMM0
           HADDPS  XMM1, XMM1
           //vLengthSq = _mm_sqrt_ss(vTemp);
           SQRTSS  XMM1, XMM1
           //vLengthSq = _mm_div_ss(g_XMOne, vLengthSq);
           MOVUPS  XMM0 , [g_XMOne]
           DIVPS   XMM0 ,XMM1
           //vLengthSq = XM_PERMUTE_PS(vLengthSq, _MM_SHUFFLE(0, 0, 0, 0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           //return vLengthSq;
           MOVUPS  [result], XMM0
end;

{$ELSE}// _XM_SSE_INTRINSICS_

function XMVector2Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x and y
           //  vLengthSq = _mm_mul_ps(V1,V2);
           MOVUPS  XMM0, [V1]
           MULPS   XMM0, [V2]
           // vTemp has y splatted
           //  vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1, XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // x+y
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;



function XMVector2ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x and y
           //  vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp has y splatted
           //  vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1, XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // x+y
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDPS   XMM0,XMM1
           // vLengthSq = _mm_rsqrt_ss(vLengthSq);
           RSQRTSS XMM0, XMM0
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;



function XMVector2ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x and y
           // vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0, [V]
           MULPS   XMM0, XMM0
           // vTemp has y splatted
           // vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1, XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // x+y
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDPS   XMM0,XMM1
           // vLengthSq = _mm_sqrt_ss(vLengthSq);
           SQRTSS  XMM0, XMM0
           // vLengthSq = _mm_div_ss(g_XMOne,vLengthSq);
           MOVUPS  XMM1, [g_XMOne]
           DIVSS   XMM1, XMM0
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0
           // return vLengthSq;
           MOVUPS  [result],XMM1
end;

{$ENDIF}


// [ V1.x*V2.y - V1.y*V2.x, V1.x*V2.y - V1.y*V2.x ]
function XMVector2Cross(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Swap x and y
           // vResult = XM_PERMUTE_PS(V2,_MM_SHUFFLE(0,1,0,1));
           MOVUPS  XMM0, [V2]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_1_0_1
           // Perform the muls
           // vResult = _mm_mul_ps(vResult,V1);
           MULPS   XMM0, [V1]
           // Splat y
           //  vTemp = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // Sub the values
           // vResult = _mm_sub_ss(vResult,vTemp);
           SUBPS   XMM0, XMM1
           // Splat the cross product
           // vResult = XM_PERMUTE_PS(vResult,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // return vResult;
           MOVUPS  [result],XMM0
end;




{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector2LengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0x3f );
           MOVUPS  XMM0, [V]
           DPPS    XMM0, XMM0, $3F
           // return _mm_sqrt_ps( vTemp );
           SQRTPS  XMM0, XMM0
           MOVUPS  [result],XMM0
end;



function XMVector2Length(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0x3f );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $3F
           // return _mm_sqrt_ps( vTemp );
           SQRTPS  XMM0, XMM0
           MOVUPS  [result],XMM0
end;




{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector2LengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp = _mm_hadd_ps(vLengthSq, vLengthSq);
           MOVUPS  XMM1,XMM0
           HADDPS  XMM1, XMM1
           //vLengthSq = _mm_sqrt_ss(vTemp);
           SQRTSS  XMM0, XMM1
           //vLengthSq = XM_PERMUTE_PS(vLengthSq, _MM_SHUFFLE(0, 0, 0, 0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           //return vLengthSq;
           MOVUPS  [result],XMM0
end;



function XMVector2Length(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           //vLengthSq = _mm_sqrt_ss(vTemp);
           SQRTSS  XMM0, XMM0
           //vLengthSq = XM_PERMUTE_PS(vLengthSq, _MM_SHUFFLE(0, 0, 0, 0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           //return vLengthSq;
           MOVUPS  [result],XMM0
end;




{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector2LengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x and y
           // vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0, [V]
           MULPS   XMM0, XMM0
           // vTemp has y splatted
           //XMVECTOR vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // x+y
           //vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           //vLengthSq = _mm_sqrt_ss(vLengthSq);
           SQRTSS  XMM0, XMM0
           //vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           //return vLengthSq;
           MOVUPS  [result],XMM0
end;



function XMVector2Length(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x and y
           // vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0, [V]
           MULPS   XMM0, XMM0
           // vTemp has y splatted
           // vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // x+y
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDSS   XMM0, XMM1
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // vLengthSq = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;


{$ENDIF}




{$IF DEFINED(_XM_SSE4_INTRINSICS_)}
// XMVector2NormalizeEst uses a reciprocal estimate and
// returns QNaN on zero and infinite vectors.
function XMVector2NormalizeEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0x3f );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $3F
           //  vResult = _mm_rsqrt_ps( vTemp );
           RSQRTPS XMM0, XMM0
           // return _mm_mul_ps(vResult, V);
           MULPS   XMM0, [V]
           MOVUPS  [result],XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector2NormalizeEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_rsqrt_ss(vLengthSq);
           RSQRTSS XMM0, XMM0
           // vLengthSq = XM_PERMUTE_PS(vLengthSq, _MM_SHUFFLE(0, 0, 0, 0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // vLengthSq = _mm_mul_ps(vLengthSq, V);
           MULPS   XMM0, [V]
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector2NormalizeEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x and y
           // vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp has y splatted
           // vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // x+y
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // vLengthSq = _mm_rsqrt_ss(vLengthSq);
           RSQRTSS XMM0, XMM0
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // vLengthSq = _mm_mul_ps(vLengthSq,V);
           MULPS   XMM0, [V]
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;
{$ENDIF}


{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector2Normalize(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vLengthSq = _mm_dp_ps( V, V, 0x3f );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $3F
           // Prepare for the division
           // vResult = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM2, XMM0
           // Create zero with a single instruction
           // vZeroMask = _mm_setzero_ps();
           XORPS   XMM1,XMM1
           // Test for a divide by zero (Must be FP to detect -0.0)
           //vZeroMask = _mm_cmpneq_ps(vZeroMask,vResult);
           CMPPS   XMM1, XMM2, 4
           // Failsafe on zero (Or epsilon) length planes
           // If the length is infinity, set the elements to zero
           //vLengthSq = _mm_cmpneq_ps(vLengthSq,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 4
           // Reciprocal mul to perform the normalization
           //vResult = _mm_div_ps(V,vResult);
           MOVUPS  XMM3, [V]
           DIVPS   XMM3, XMM2
           // Any that are infinity, set to zero
           //vResult = _mm_and_ps(vResult,vZeroMask);
           ANDPS   XMM3, XMM1
           // Select qnan or result based on infinite length
           // vTemp1 = _mm_andnot_ps(vLengthSq,g_XMQNaN);
           MOVUPS  XMM1,[g_XMQNaN]
           ANDNPS  XMM1, XMM0
           // vTemp2 = _mm_and_ps(vResult,vLengthSq);
           ANDPS   XMM3, XMM0
           //vResult = _mm_or_ps(vTemp1,vTemp2);
           ORPS    XMM1, XMM3
           // return vResult;
           MOVUPS  [result],XMM3
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector2Normalize(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x and y only
           // vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0 ,[V]
           MULPS   XMM0, XMM0
           //vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           //vLengthSq = _mm_moveldup_ps(vLengthSq);
           MOVSLDUP XMM0, XMM0
           // Prepare for the division
           // vResult = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM1, XMM0
           // Create zero with a single instruction
           // vZeroMask = _mm_setzero_ps();
           XORPS   XMM2,XMM2
           // Test for a divide by zero (Must be FP to detect -0.0)
           // vZeroMask = _mm_cmpneq_ps(vZeroMask, vResult);
           CMPPS   XMM2, XMM1, 4
           // Failsafe on zero (Or epsilon) length planes
           // If the length is infinity, set the elements to zero
           //vLengthSq = _mm_cmpneq_ps(vLengthSq, g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 4
           // Reciprocal mul to perform the normalization
           //vResult = _mm_div_ps(V, vResult);
           MOVUPS  XMM3, [V]
           DIVPS   XMM3, XMM1
           // Any that are infinity, set to zero
           //vResult = _mm_and_ps(vResult, vZeroMask);
           ANDPS   XMM1, XMM2
           // Select qnan or result based on infinite length
           // vTemp1 = _mm_andnot_ps(vLengthSq, g_XMQNaN);
           MOVUPS  XMM2, [g_XMQNaN]
           ANDNPS  XMM2, XMM0
           // vTemp2 = _mm_and_ps(vResult, vLengthSq);
           ANDPS   XMM0, XMM1
           //vResult = _mm_or_ps(vTemp1, vTemp2);
           ORPS    XMM0, XMM2
           //return vResult;
           MOVUPS  [result],XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector2Normalize(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x and y only
           //  vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           //  vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // Prepare for the division
           //  vResult = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM1, XMM0
           // Create zero with a single instruction
           //  vZeroMask = _mm_setzero_ps();
           XORPS   XMM2,XMM2
           // Test for a divide by zero (Must be FP to detect -0.0)
           // vZeroMask = _mm_cmpneq_ps(vZeroMask,vResult);
           CMPPS   XMM2, XMM1, 4
           // Failsafe on zero (Or epsilon) length planes
           // If the length is infinity, set the elements to zero
           // vLengthSq = _mm_cmpneq_ps(vLengthSq,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 4
           // Reciprocal mul to perform the normalization
           // vResult = _mm_div_ps(V,vResult);
           MOVUPS  XMM3, [V]
           DIVPS   XMM3, XMM1
           // Any that are infinity, set to zero
           // vResult = _mm_and_ps(vResult,vZeroMask);
           ANDPS   XMM3, XMM2
           // Select qnan or result based on infinite length
           //  vTemp1 = _mm_andnot_ps(vLengthSq,g_XMQNaN);
           MOVUPS  XMM7,XMM0
           ANDNPS  XMM7, [g_XMQNaN]
           //  vTemp2 = _mm_and_ps(vResult,vLengthSq);
           ANDPS   XMM3, XMM0
           // vResult = _mm_or_ps(vTemp1,vTemp2);
           ORPS    XMM3, XMM7
           // return vResult;
           MOVUPS  [result],XMM3
end;
{$ENDIF}


function XMVector2RefractV(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR; constref RefractionIndex: TXMVECTOR): TXMVECTOR; inline;
var
    IDotN: TXMVECTOR;
begin
    // Result = RefractionIndex * Incident - Normal * (RefractionIndex * dot(Incident, Normal) +
    // sqrt(1 - RefractionIndex * RefractionIndex * (1 - dot(Incident, Normal) * dot(Incident, Normal))))
    // Get the 2D Dot product of Incident-Normal
    IDotN := XMVector2Dot(Incident, Normal);
    asm
               // vTemp = 1.0f - RefractionIndex * RefractionIndex * (1.0f - IDotN * IDotN)
               //  vTemp = _mm_mul_ps(IDotN,IDotN);
               MOVUPS  XMM0, [IDotN]
               MULPS   XMM0, XMM0
               // vTemp = _mm_sub_ps(g_XMOne,vTemp);
               MOVUPS  XMM1, [g_XMOne]
               SUBPS   XMM1, XMM0
               // vTemp = _mm_mul_ps(vTemp,RefractionIndex);
               MULPS   XMM1, [RefractionIndex]
               // vTemp = _mm_mul_ps(vTemp,RefractionIndex);
               MULPS   XMM1, [RefractionIndex]
               // vTemp = _mm_sub_ps(g_XMOne,vTemp);
               MOVUPS  XMM0, [g_XMOne]
               SUBPS   XMM0, XMM1
               // If any terms are <=0, sqrt() will fail, punt to zero
               //  vMask = _mm_cmpgt_ps(vTemp,g_XMZero);
               MOVUPS  XMM2,[g_XMZero]
               CMPPS   XMM2, XMM0, 1
               // R = RefractionIndex * IDotN + sqrt(R)
               // vTemp = _mm_sqrt_ps(vTemp);
               SQRTPS  XMM0, XMM0
               //  vResult = _mm_mul_ps(RefractionIndex,IDotN);
               MOVUPS  XMM3, [RefractionIndex]
               MULPS   XMM2, [IDotN]
               // vTemp = _mm_add_ps(vTemp,vResult);
               ADDPS   XMM0, XMM3
               // Result = RefractionIndex * Incident - Normal * R
               // vResult = _mm_mul_ps(RefractionIndex,Incident);
               MOVUPS  XMM3, [RefractionIndex]
               MULPS   XMM3, [Incident]
               // vTemp = _mm_mul_ps(vTemp,Normal);
               MULPS   XMM0, [Normal]
               // vResult = _mm_sub_ps(vResult,vTemp);
               SUBPS   XMM3, XMM0
               // vResult = _mm_and_ps(vResult,vMask);
               ANDPS   XMM3, XMM2
               // return vResult;
               MOVUPS  [result],XMM3

    end;
end;



function XMVector2Orthogonal(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,2,0,1));
           MOVUPS  XMM0, [V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_2_0_1
           // vResult = _mm_mul_ps(vResult,g_XMNegateX);
           MULPS   XMM0, [g_XMNegateX]
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVector2IntersectLine(constref Line1Point1: TXMVECTOR; constref Line1Point2: TXMVECTOR; constref Line2Point1: TXMVECTOR;
    constref Line2Point2: TXMVECTOR): TXMVECTOR;
var
    V1, V2, V3, C1, C2: TXMVECTOR;
begin
    asm
               //  V1 = _mm_sub_ps(Line1Point2, Line1Point1);
               MOVUPS  XMM0,[Line1Point2]
               SUBPS   XMM0, [Line1Point1]
               MOVUPS  [V1],XMM0
               //  V2 = _mm_sub_ps(Line2Point2, Line2Point1);
               MOVUPS  XMM0,[Line2Point2]
               SUBPS   XMM0, [Line2Point1]
               MOVUPS  [V2],XMM0
               //  V3 = _mm_sub_ps(Line1Point1, Line2Point1);
               MOVUPS  XMM0,[Line1Point1]
               SUBPS   XMM0, [Line2Point1]
               MOVUPS  [V3],XMM0
    end;
    // Generate the cross products
    C1 := XMVector2Cross(V1, V2);
    C2 := XMVector2Cross(V2, V3);
    asm
               // If C1 is not close to epsilon, use the calculated value
               //  vResultMask = _mm_setzero_ps();
               XORPS   XMM0,XMM0
               // vResultMask = _mm_sub_ps(vResultMask,C1);
               SUBPS   XMM0, [C1]
               //  vResultMask = _mm_max_ps(vResultMask,C1);
               MAXPS   XMM0, [C1]
               // 0xFFFFFFFF if the calculated value is to be used
               //   vResultMask = _mm_cmpgt_ps(vResultMask,g_XMEpsilon);
               CMPPS   XMM0, [g_XMEpsilon], 1
               // If C1 is close to epsilon, which fail type is it? INFINITY or NAN?
               //   vFailMask = _mm_setzero_ps();
               XORPS   XMM1 ,XMM1
               //  vFailMask = _mm_sub_ps(vFailMask,C2);
               SUBPS   XMM1, [C2]
               // vFailMask = _mm_max_ps(vFailMask,C2);
               MAXPS   XMM1, [C2]
               // vFailMask = _mm_cmple_ps(vFailMask,g_XMEpsilon);
               CMPPS   XMM1, [g_XMEpsilon], 2 // XMM1 = vFailMask


               //  vFail = _mm_and_ps(vFailMask,g_XMInfinity);
               MOVUPS  XMM2, XMM1
               ANDPS   XMM2, [g_XMInfinity] // XMM2 = vFail
               // vFailMask = _mm_andnot_ps(vFailMask,g_XMQNaN);
               ANDNPS  XMM1, [g_XMQNaN]
               // vFail is NAN or INF
               // vFail = _mm_or_ps(vFail,vFailMask);
               ORPS    XMM2, XMM1
               // Intersection point = Line1Point1 + V1 * (C2 / C1)
               //  vResult = _mm_div_ps(C2,C1);
               MOVUPS  XMM3, [C2]
               DIVPS   XMM3, [C1]
               // vResult = _mm_mul_ps(vResult,V1);
               MULPS   XMM3, [V1]
               // vResult = _mm_add_ps(vResult,Line1Point1);
               ADDPS   XMM3, [Line1Point1]
               // Use result, or failure value
               // vResult = _mm_and_ps(vResult,vResultMask);
               ANDPS   XMM3, XMM0
               // vResultMask = _mm_andnot_ps(vResultMask,vFail);
               ANDNPS  XMM0, XMM2
               // vResult = _mm_or_ps(vResult,vResultMask);
               ORPS    XMM3, XMM0
               // return vResult;
               MOVUPS  [result],XMM3
    end;
end;



function XMVector2Transform(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR; assembler;
asm
           //  vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM0,[V]
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // vResult = _mm_mul_ps(vResult,M.r[0]);
           MULPS   XMM0, TXMMATRIX([M]).r0
           //  vTemp = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // vTemp = _mm_mul_ps(vTemp,M.r[1]);
           MULPS   XMM1, TXMMATRIX([M]).r1
           // vResult = _mm_add_ps(vResult,vTemp);
           ADDPS   XMM0, XMM1
           // vResult = _mm_add_ps(vResult,M.r[3]);
           ADDPS   XMM0, TXMMATRIX([M]).r3
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVector2TransformStream(out pOutputStream: PXMFLOAT4; constref OutputStride: size_t; constref pInputStream: PXMFLOAT2;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT4;
var
    row0, row1, row3: TXMVECTOR;
    i, two: size_t;
    j: size_t;
    s: integer;
    pInputVector: pointer;
    pOutputVector: pointer;
begin
    // const uint8_t* pInputVector = (const uint8_t*)pInputStream;
    pInputVector := pInputStream;
    // uint8_t* pOutputVector = (uint8_t*)pOutputStream;
    pOutputVector := pOutputStream;

    row0 := M.r[0];
    row1 := M.r[1];
    row3 := M.r[3];

    i := 0;
    two := VectorCount shr 1;
    if (two > 0) then
    begin
        if (InputStride = sizeof(TXMFLOAT2)) then
        begin
            if (not ((uintptr(pOutputStream) and $F) = $F) and not ((OutputStride and $F) = $F)) then
            begin
                // Packed input, aligned output
                s := sizeof(TXMFLOAT2) * 2;
                for  j := 0 to two - 1 do
                begin
                    asm
                               // V = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                               MOVUPS  XMM0, [pInputVector]
                               MOV     EDX, pInputVector
                               ADD     EDX, [s]
                               MOV     pInputVector,EDX // pInputVector += s;

                               //  Y = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                               //  X = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                               //  vTemp = _mm_mul_ps( Y, row1 );
                               MULPS   XMM2, [row1]
                               //  vTemp2 = _mm_mul_ps( X, row0 );
                               MULPS   XMM1, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1

                               // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVAPS  [pOutputVector], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Y = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,3,3,3));
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_3_3_3_3
                               // X = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_2_2_2

                               // vTemp = _mm_mul_ps( Y, row1 );
                               MULPS   XMM2, [row1]
                               // vTemp2 = _mm_mul_ps( X, row0 );
                               MULPS   XMM1, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1

                               // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVAPS  [pOutputVector], XMM2

                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX
                    end;
                    Inc(i, 2);
                end;
            end
            else
            begin
                // Packed input, unaligned output
                s := sizeof(TXMFLOAT2) * 2;
                for  j := 0 to two - 1 do
                begin
                    asm
                               // V = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                               MOVUPS  XMM0, [pInputVector]
                               // pInputVector += sizeof(XMFLOAT2)*2;
                               MOV     EDX, pInputVector
                               ADD     EDX, [s]
                               MOV     pInputVector,EDX

                               //  Y = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                               //  X = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                               //  vTemp = _mm_mul_ps( Y, row1 );
                               MULPS   XMM2, [row1]
                               //  vTemp2 = _mm_mul_ps( X, row0 );
                               MULPS   XMM1, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1

                               // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVUPS  [pOutputVector], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Y = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,3,3,3));
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_3_3_3_3
                               // X = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_2_2_2

                               // vTemp = _mm_mul_ps( Y, row1 );
                               MULPS   XMM2, [row1]
                               // vTemp2 = _mm_mul_ps( X, row0 );
                               MULPS   XMM1, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1

                               // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVUPS  [pOutputVector], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX
                    end;
                    Inc(i, 2);
                end;
            end;
        end;
    end;

    // if ( !((uintptr_t)pInputVector & 0xF) && !(InputStride & 0xF) )   then
    if (not (uintptr(pInputVector) and $F = $F) and not (InputStride and $F = $F)) then
    begin
        if (not (uintptr(pOutputStream) and $F = $F) and not (OutputStride and $F = $F)) then
        begin
            // Aligned input, aligned output
            while i < VectorCount do
                // for  i < VectorCount; i++) do
            begin
                asm
                           // V = _mm_castsi128_ps( _mm_loadl_epi64( reinterpret_cast<const __m128i*>(pInputVector) ) );
                           MOVQ    XMM0, [pInputVector]


                           // pInputVector += InputStride;
                           MOV     EDX, pInputVector
                           ADD     EDX, [InputStride]
                           MOV     pInputVector,EDX

                           //  Y = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
                           MOVUPS  XMM1,XMM0
                           MOVUPS  XMM2,XMM0
                           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                           //  X = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
                           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                           //  vTemp = _mm_mul_ps( Y, row1 );
                           MULPS   XMM2, [row1]
                           //  vTemp2 = _mm_mul_ps( X, row0 );
                           MULPS   XMM1, [row0]
                           // vTemp = _mm_add_ps( vTemp, row3 );
                           ADDPS   XMM2, [row3]
                           // vTemp = _mm_add_ps( vTemp, vTemp2 );
                           ADDPS   XMM2, XMM1

                           // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTemp );
                           MOVUPS  [pOutputVector], XMM2
                           // pOutputVector += OutputStride;
                           MOV     EDX, pOutputVector
                           ADD     EDX, [OutputStride]
                           MOV     pOutputVector,EDX
                end;
                Inc(i);
            end;
        end
        else
        begin
            // Aligned input, unaligned output
            // for (; i < VectorCount; i++) do
            while i < VectorCount do
            begin
                asm
                           // V = _mm_castsi128_ps( _mm_loadl_epi64( reinterpret_cast<const __m128i*>(pInputVector) ) );
                           MOVQ    XMM0, [pInputVector]
                           // pInputVector += InputStride;
                           MOV     EDX, pInputVector
                           ADD     EDX, [InputStride]
                           MOV     pInputVector,EDX

                           //  Y = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
                           MOVUPS  XMM1,XMM0
                           MOVUPS  XMM2,XMM0
                           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                           //  X = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
                           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                           //  vTemp = _mm_mul_ps( Y, row1 );
                           MULPS   XMM2, [row1]
                           //  vTemp2 = _mm_mul_ps( X, row0 );
                           MULPS   XMM1, [row0]
                           // vTemp = _mm_add_ps( vTemp, row3 );
                           ADDPS   XMM2, [row3]
                           // vTemp = _mm_add_ps( vTemp, vTemp2 );
                           ADDPS   XMM2, XMM1

                           // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTemp );
                           MOVUPS  [pOutputVector], XMM2
                           //  pOutputVector += OutputStride;
                           MOV     EDX, pOutputVector
                           ADD     EDX, [OutputStride]
                           MOV     pOutputVector,EDX
                end;
                Inc(i);
            end;
        end;
    end
    else
    begin
        // Unaligned input
        // for (; i < VectorCount; i++) do
        while i < VectorCount do
        begin
            asm
                       // __m128 x = _mm_load_ss( reinterpret_cast<const float*>(pInputVector) );
                       MOVSS   XMM0, [pInputVector]
                       // __m128 y = _mm_load_ss( reinterpret_cast<const float*>(pInputVector+4) );
                       MOVSS   XMM1, [pInputVector+4]
                       // pInputVector += InputStride;
                       MOV     EDX, pInputVector
                       ADD     EDX, [InputStride]
                       MOV     pInputVector,EDX

                       //  Y = XM_PERMUTE_PS(y,_MM_SHUFFLE(0,0,0,0));
                       SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0
                       //  X = XM_PERMUTE_PS(x,_MM_SHUFFLE(0,0,0,0));
                       SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                       //  vTemp = _mm_mul_ps( Y, row1 );
                       MULPS   XMM1, [row1]
                       //  vTemp2 = _mm_mul_ps( X, row0 );
                       MULPS   XMM0, [row0]
                       // vTemp = _mm_add_ps( vTemp, row3 );
                       ADDPS   XMM1, [row3]
                       // vTemp = _mm_add_ps( vTemp, vTemp2 );
                       ADDPS   XMM1, XMM0

                       // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTemp );
                       MOVUPS  [pOutputVector], XMM1
                       // pOutputVector += OutputStride;
                       MOV     EDX, pOutputVector
                       ADD     EDX, [OutputStride]
                       MOV     pOutputVector,EDX
            end;
            Inc(i);
        end;
    end;

    // XM_SFENCE();

    Result := pOutputStream;
end;



function XMVector2TransformCoordStream(out pOutputStream: PXMFLOAT2; constref OutputStride: size_t; constref pInputStream: PXMFLOAT2;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT2;
var
    pInputVector: pointer;
    pOutputVector: pointer;
    row0, row1, row3: TXMVECTOR;
    i, j, two: size_t;
    s: uint32;
begin
    pInputVector := pInputStream;
    pOutputVector := pOutputStream;

    row0 := M.r[0];
    row1 := M.r[1];
    row3 := M.r[3];

    i := 0;
    two := VectorCount shr 1;
    if (two > 0) then
    begin
        if (InputStride = sizeof(TXMFLOAT2)) then
        begin
            if (OutputStride = sizeof(TXMFLOAT2)) then
            begin
                if (not ((uintptr(pOutputStream) and $F = $F))) then
                begin
                    // Packed input, aligned & packed output
                    s := sizeof(TXMFLOAT2) * 2;
                    for  j := 0 to two - 1 do
                    begin
                        asm
                                   //  V = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM0, [pInputVector]
                                   // pInputVector += sizeof(XMFLOAT2)*2;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [s]
                                   MOV     pInputVector,EDX

                                   // Result 1
                                   //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                                   //  vTemp = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM2, [row1]
                                   //  vTemp2 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM1, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM2, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1

                                   //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                                   //  V1 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3

                                   // Result 2
                                   // Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM0
                                   MOVUPS  XMM4,XMM0
                                   SHUFPS  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3
                                   // X = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_2_2_2_2

                                   // vTemp = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM4, [row1]
                                   // vTemp2 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM3, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM4, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM4, XMM3

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3, XMM4
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                                   //  V2 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM4, XMM3

                                   // vTemp = _mm_movelh_ps( V1, V2 );
                                   MOVLHPS XMM2, XMM4
                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTemp );
                                   MOVUPS  [pOutputVector],XMM2
                                   // pOutputVector += sizeof(XMFLOAT2)*2;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        Inc(i, 2);
                    end;
                end
                else
                begin
                    // Packed input, unaligned & packed output
                    s := sizeof(TXMFLOAT2) * 2;
                    for j := 0 to two - 1 do
                    begin
                        asm
                                   //  V = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM0, [pInputVector]
                                   // pInputVector += sizeof(TXMFLOAT2)*2;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [s]
                                   MOV     pInputVector,EDX

                                   // Result 1
                                   //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                                   //  vTemp = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM2, [row1]
                                   //  vTemp2 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM1, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM2, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1

                                   //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3, XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                                   //  V1 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3

                                   // Result 2
                                   // Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM0
                                   MOVUPS  XMM4,XMM0
                                   SHUFPS  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3

                                   // X = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_2_2_2_2

                                   // vTemp = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM4, [row1]
                                   // vTemp2 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM3, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM4, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM4, XMM3

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM4
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                                   //  V2 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM4, XMM3

                                   // vTemp = _mm_movelh_ps( V1, V2 );
                                   MOVLHPS XMM2, XMM4

                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTemp );
                                   MOVUPS  [pOutputVector], XMM2
                                   // pOutputVector += sizeof(XMFLOAT2)*2;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        Inc(i, 2);
                    end;
                end;
            end
            else
            begin
                // Packed input, unpacked output
                s := sizeof(TXMFLOAT2) * 2;
                for j := 0 to two - 1 do
                begin
                    asm
                               //  V = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                               MOVUPS  XMM0, [pInputVector]
                               // pInputVector += sizeof(XMFLOAT2)*2;
                               MOV     EDX, pInputVector
                               ADD     EDX, [s]
                               MOV     pInputVector,EDX

                               // Result 1
                               //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                               //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                               //  vTemp = _mm_mul_ps( Y, row1 );
                               MULPS   XMM2, [row1]
                               //  vTemp2 = _mm_mul_ps( X, row0 );
                               MULPS   XMM1, [row0]
                               //  vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1

                               //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3, XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                               //  vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3
                               // vTemp2 = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(1, 1, 1, 1) );
                               MOVUPS  XMM4,XMM2
                               SHUFPS  XMM4, XMM4, _MM_SHUFFLE_1_1_1_1

                               // _mm_store_ss( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVSS   [pOutputVector], XMM2
                               // _mm_store_ss( reinterpret_cast<float*>(pOutputVector+4), vTemp2 );
                               MOVSS   [pOutputVector+4], XMM4
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 2
                               // Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_3_3_3_3
                               // X = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_2_2_2

                               // vTemp = _mm_mul_ps( Y, row1 );
                               MULPS   XMM2, [row1]
                               // vTemp2 = _mm_mul_ps( X, row0 );
                               MULPS   XMM1, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1

                               // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3,XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3
                               // vTemp2 = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(1, 1, 1, 1) );
                               MOVUPS  XMM4, XMM2
                               SHUFPS  XMM4, XMM4, _MM_SHUFFLE_1_1_1_1

                               // _mm_store_ss( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVSS   [pOutputVector], XMM2
                               // _mm_store_ss( reinterpret_cast<float*>(pOutputVector+4), vTemp2 );
                               MOVSS   [pOutputVector+4], XMM4
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX
                    end;
                    Inc(i, 2);
                end;
            end;
        end;
    end;

    if (not (uintptr(pInputVector) and $F = $F) and not (InputStride and $F = $F)) then
    begin
        // Aligned input
        //for (; i < VectorCount; i++)
        while i < VectorCount do
        begin
            asm
                       // V = _mm_castsi128_ps( _mm_loadl_epi64( reinterpret_cast<const __m128i*>(pInputVector) ) );
                       MOVQ    XMM0, [pInputVector]
                       // pInputVector += InputStride;
                       MOV     EDX, pInputVector
                       ADD     EDX, [InputStride]
                       MOV     pInputVector,EDX

                       // Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                       MOVUPS  XMM1,XMM0
                       MOVUPS  XMM2,XMM0
                       SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                       // X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                       SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                       // vTemp = _mm_mul_ps( Y, row1 );
                       MULPS   XMM2, [row1]
                       // vTemp2 = _mm_mul_ps( X, row0 );
                       MULPS   XMM1,[row0]
                       // vTemp = _mm_add_ps( vTemp, row3 );
                       ADDPS   XMM2, [row3]
                       // vTemp2 = _mm_add_ps( vTemp, vTemp2 );
                       ADDPS   XMM2, XMM1

                       // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                       MOVUPS  XMM3,XMM2
                       SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                       // vTemp = _mm_div_ps( vTemp, W );
                       DIVPS   XMM2, XMM3
                       // vTemp2 = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(1, 1, 1, 1) );
                       MOVUPS  XMM4, XMM2
                       SHUFPS  XMM4, XMM4, _MM_SHUFFLE_1_1_1_1

                       // _mm_store_ss( reinterpret_cast<float*>(pOutputVector), vTemp );
                       MOVSS   [pOutputVector], XMM2
                       // _mm_store_ss( reinterpret_cast<float*>(pOutputVector+4), vTemp2 );
                       MOVSS   [pOutputVector+4], XMM4
                       // pOutputVector += OutputStride;
                       MOV     EDX, pOutputVector
                       ADD     EDX, [OutputStride]
                       MOV     pOutputVector,EDX
            end;
            Inc(i);
        end;
    end
    else
    begin
        // Unaligned input
        // for (; i < VectorCount; i++)
        while i < VectorCount do
        begin
            asm
                       //  x = _mm_load_ss( reinterpret_cast<const float*>(pInputVector) );
                       MOVSS   XMM0, [pInputVector]
                       //  y = _mm_load_ss( reinterpret_cast<const float*>(pInputVector+4) );
                       MOVSS   XMM1, [pInputVector+4]
                       // pInputVector += InputStride;
                       MOV     EDX, pInputVector
                       ADD     EDX, [InputStride]
                       MOV     pInputVector,EDX

                       //  Y = XM_PERMUTE_PS( y, _MM_SHUFFLE(0, 0, 0, 0) );
                       SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0
                       //  X = XM_PERMUTE_PS( x, _MM_SHUFFLE(0, 0, 0, 0) );
                       SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                       //  vTemp = _mm_mul_ps( Y, row1 );
                       MULPS   XMM1, [row1]
                       //  vTemp2 = _mm_mul_ps( X, row0 );
                       MULPS   XMM0,[row0]
                       // vTemp = _mm_add_ps( vTemp, row3 );
                       ADDPS   XMM1, [row3]
                       // vTemp = _mm_add_ps( vTemp, vTemp2 );
                       ADDPS   XMM1, XMM0

                       //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                       MOVUPS  XMM3,XMM1
                       SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                       // vTemp = _mm_div_ps( vTemp, W );
                       DIVPS   XMM1, XMM3
                       // vTemp2 = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(1, 1, 1, 1) );
                       MOVUPS  XMM3,XMM1
                       SHUFPS  XMM3, XMM3, _MM_SHUFFLE_1_1_1_1

                       //  _mm_store_ss( reinterpret_cast<float*>(pOutputVector), vTemp );
                       MOVSS   [pOutputVector], XMM1
                       // _mm_store_ss( reinterpret_cast<float*>(pOutputVector+4), vTemp2 );
                       MOVSS   [pOutputVector+4], XMM3
                       // pOutputVector += OutputStride;
                       MOV     EDX, pOutputVector
                       ADD     EDX, [OutputStride]
                       MOV     pOutputVector,EDX
            end;
            Inc(i);
        end;
    end;

    // XM_SFENCE();

    Result := pOutputStream;
end;



function XMVector2TransformNormal(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR; assembler;
asm
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM0,[V]
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // vResult = _mm_mul_ps(vResult,M.r[0]);
           MULPS   XMM0 , TXMMATRIX([M]).r0
           //  vTemp = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // vTemp = _mm_mul_ps(vTemp,M.r[1]);
           MULPS   XMM1 , TXMMATRIX([M]).r1
           // vResult = _mm_add_ps(vResult,vTemp);
           ADDPS   XMM0, XMM1
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVector2TransformNormalStream(out pOutputStream: PXMFLOAT2; constref OutputStride: size_t; constref pInputStream: PXMFLOAT2;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT2;
var
    pInputVector: pointer;
    pOutputVector: pointer;
    row0, row1: TXMVECTOR;
    i, two, j: size_t;
    s: uint32;
begin
    pInputVector := pInputStream;
    pOutputVector := pOutputStream;

    row0 := M.r[0];
    row1 := M.r[1];

    i := 0;
    two := VectorCount shr 1;
    if (two > 0) then
    begin
        if (InputStride = sizeof(TXMFLOAT2)) then
        begin
            if (OutputStride = sizeof(TXMFLOAT2)) then
            begin
                if (not (uintptr(pOutputStream) and $F = $F)) then
                begin
                    // Packed input, aligned & packed output
                    s := sizeof(TXMFLOAT2) * 2;
                    for  j := 0 to two - 1 do
                    begin
                        asm
                                   // V = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM0, [pInputVector]
                                   // pInputVector += sizeof(XMFLOAT2)*2;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [s]
                                   MOV     pInputVector,EDX

                                   // Result 1
                                   //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                                   //  vTemp = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM2, [row1]
                                   //  vTemp2 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM1, [row0]
                                   //  V1 = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1

                                   // Result 2
                                   // Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM0
                                   MOVUPS  XMM4,XMM0
                                   SHUFPS  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3
                                   // X = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_2_2_2_2

                                   // vTemp = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM4, [row1]
                                   // vTemp2 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM3, [row0]
                                   //  V2 = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM4, XMM3

                                   // vTemp = _mm_movelh_ps( V1, V2 );
                                   MOVLHPS XMM2, XMM4

                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTemp );
                                   MOVUPS  [pOutputVector], XMM2
                                   // pOutputVector += sizeof(XMFLOAT2)*2;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        Inc(i, 2);
                    end;
                end
                else
                begin
                    // Packed input, unaligned & packed output
                    s := sizeof(TXMFLOAT2) * 2;
                    for  j := 0 to two - 1 do
                    begin
                        asm
                                   //  V = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM0, [pInputVector]
                                   // pInputVector += sizeof(XMFLOAT2)*2;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [InputStride]
                                   MOV     pInputVector,EDX

                                   // Result 1
                                   //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                                   //  vTemp = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM2, [row1]
                                   //  vTemp2 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM1, [row0]
                                   //  V1 = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1

                                   // Result 2
                                   // Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM0
                                   MOVUPS  XMM4,XMM0
                                   SHUFPS  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3
                                   // X = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_2_2_2_2

                                   // vTemp = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM4, [row1]
                                   // vTemp2 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM3, [row0]
                                   //  V2 = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM4, XMM3

                                   // vTemp = _mm_movelh_ps( V1, V2 );
                                   MOVLHPS XMM2, XMM4

                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTemp );
                                   MOVUPS  [pOutputVector], XMM2
                                   // pOutputVector += sizeof(XMFLOAT2)*2;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        Inc(i, 2);
                    end;
                end;
            end
            else
            begin
                // Packed input, unpacked output
                s := sizeof(TXMFLOAT2) * 2;
                for  j := 0 to two - 1 do
                begin
                    asm
                               //  V = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                               MOVUPS  XMM0, [pInputVector]
                               // pInputVector += sizeof(XMFLOAT2)*2;
                               MOV     EDX, pInputVector
                               ADD     EDX, [s]
                               MOV     pInputVector,EDX

                               // Result 1
                               //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                               //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                               //  vTemp = _mm_mul_ps( Y, row1 );
                               MULPS   XMM2, [row1]
                               //  vTemp2 = _mm_mul_ps( X, row0 );
                               MULPS   XMM1, [row0]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp2 = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(1, 1, 1, 1) );
                               MOVUPS  XMM1,XMM2
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1

                               // _mm_store_ss( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVSS   [pOutputVector], XMM1
                               // _mm_store_ss( reinterpret_cast<float*>(pOutputVector+4), vTemp2 );
                               MOVSS   [pOutputVector+4], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 2
                               // Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_3_3_3_3
                               // X = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_2_2_2

                               // vTemp = _mm_mul_ps( Y, row1 );
                               MULPS   XMM2, [row1]
                               // vTemp2 = _mm_mul_ps( X, row0 );
                               MULPS   XMM1, [row0]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp2 = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(1, 1, 1, 1) );
                               MOVUPS  XMM1,XMM2
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1

                               // _mm_store_ss( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVSS   [pOutputVector], XMM1
                               // _mm_store_ss( reinterpret_cast<float*>(pOutputVector+4), vTemp2 );
                               MOVSS   [pOutputVector+4], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX
                    end;
                    Inc(i, 2);
                end;
            end;
        end;
    end;

    if (not ((uintptr(pInputVector) and $F = $F) and not (InputStride and $F = $F))) then
    begin
        // Aligned input
        // for (; i < VectorCount; i++) do
        while i < VectorCount do
        begin
            asm
                       //  V = _mm_castsi128_ps( _mm_loadl_epi64( reinterpret_cast<const __m128i*>(pInputVector) ) );
                       MOVQ    XMM0, [pInputVector]
                       // pInputVector += InputStride;
                       MOV     EDX, pInputVector
                       ADD     EDX, [InputStride]
                       MOV     pInputVector,EDX

                       //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                       MOVUPS  XMM1,XMM0
                       MOVUPS  XMM2,XMM0
                       SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1
                       //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                       SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0

                       //  vTemp = _mm_mul_ps( Y, row1 );
                       MULPS   XMM2, [row1]
                       //  vTemp2 = _mm_mul_ps( X, row0 );
                       MULPS   XMM1, [row0]
                       //  vTemp = _mm_add_ps( vTemp, vTemp2 );
                       ADDPS   XMM2, XMM1
                       // vTemp2 = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(1, 1, 1, 1) );
                       MOVUPS  XMM1,XMM2
                       SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_1_1

                       // _mm_store_ss( reinterpret_cast<float*>(pOutputVector), vTemp );
                       MOVSS   [pOutputVector], XMM1
                       // _mm_store_ss( reinterpret_cast<float*>(pOutputVector+4), vTemp2 );
                       MOVSS   [pOutputVector+4], XMM2
                       // pOutputVector += OutputStride;
            end;
            Inc(i);
        end;
    end
    else
    begin
        // Unaligned input
        // for (; i < VectorCount; i++) do
        while i < VectorCount do
        begin
            asm
                       //  x = _mm_load_ss( reinterpret_cast<const float*>(pInputVector) );
                       MOVSS   XMM0, [pInputVector]
                       //  y = _mm_load_ss( reinterpret_cast<const float*>(pInputVector+4) );
                       MOVSS   XMM1, [pInputVector+4]
                       // pInputVector += InputStride;
                       MOV     EDX, pInputVector
                       ADD     EDX, [InputStride]
                       MOV     pInputVector,EDX

                       //  Y = XM_PERMUTE_PS( y, _MM_SHUFFLE(0, 0, 0, 0) );
                       SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_0
                       //  X = XM_PERMUTE_PS( x, _MM_SHUFFLE(0, 0, 0, 0) );
                       SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                       //  vTemp = _mm_mul_ps( Y, row1 );
                       MULPS   XMM1, [row1]
                       //  vTemp2 = _mm_mul_ps( X, row0 );
                       MULPS   XMM0, [row0]
                       // vTemp = _mm_add_ps( vTemp, vTemp2 );
                       ADDPS   XMM1, XMM0
                       // vTemp2 = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(1, 1, 1, 1) );
                       MOVUPS  XMM0,XMM1
                       SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1

                       // _mm_store_ss( reinterpret_cast<float*>(pOutputVector), vTemp );
                       MOVSS   [pOutputVector], XMM0
                       // _mm_store_ss( reinterpret_cast<float*>(pOutputVector+4), vTemp2 );
                       MOVSS   [pOutputVector+4], XMM1
                       // pOutputVector += OutputStride;
                       MOV     EDX, pOutputVector
                       ADD     EDX, [OutputStride]
                       MOV     pOutputVector,EDX
            end;
            Inc(i);
        end;
    end;

    // XM_SFENCE();

    Result := pOutputStream;
end;



{***************************************************************************
 *
 * 3D Vector
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------


function XMVector3Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           //XMVECTOR vTemp = _mm_cmpeq_ps(V1,V2);
           MOVUPS  XMM0, [V1]
           CMPPS   XMM0, [V2], 0
           //return (((_mm_movemask_ps(vTemp)&7)==7) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;



function XMVector3EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
asm
           // vTemp = _mm_cmpeq_ps(V1,V2);
           MOVUPS  XMM0,[V1]
           CMPPS   XMM0, [V2], 0
           // int iTest = _mm_movemask_ps(vTemp)&7;
    {movmskps r32, xmm
    uint32_t CR = 0;
    if (iTest==7) then
       CR = XM_CRMASK_CR6TRUE;

    else if (!iTest)

        CR = XM_CRMASK_CR6FALSE;

    return CR; }

           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           MOV     EDX,XM_CRMASK_CR6FALSE
           CMP     EAX, $0
           JE      @Finished
           MOV     EDX,XM_CRMASK_CR6TRUE
           CMP     EAX, $7
           JE      @Finished
           MOV     EDX,$0
           @Finished:
           MOV     [result],EDX
end;



function XMVector3EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
asm
           // vTemp = _mm_cmpeq_epi32(_mm_castps_si128(V1),_mm_castps_si128(V2));
           MOVUPS  XMM0,[V1]
           PCMPEQD XMM0, [V2]
           //return (((_mm_movemask_ps(_mm_castsi128_ps(vTemp))&7)==7) != 0);

           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;



function XMVector3EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
asm
           // vTemp = _mm_cmpeq_epi32(_mm_castps_si128(V1),_mm_castps_si128(V2));
           MOVUPS  XMM0,[V1]
           PCMPEQD XMM0, [V2]
           // int iTemp = _mm_movemask_ps(_mm_castsi128_ps(vTemp))&7;
   { movmskps r32, xmm
    uint32_t CR = 0;
    if (iTemp==7)

        CR = XM_CRMASK_CR6TRUE;

    else if (!iTemp)

        CR = XM_CRMASK_CR6FALSE;

    return CR; }
           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           MOV     EDX,XM_CRMASK_CR6FALSE
           CMP     EAX, $0
           JE      @Finished
           MOV     EDX,XM_CRMASK_CR6TRUE
           CMP     EAX, $7
           JE      @Finished
           MOV     EDX,$0
           @Finished:
           MOV     [result],EDX
end;



function XMVector3NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean;
asm
           // Get the difference
           // vDelta = _mm_sub_ps(V1,V2);
           MOVUPS  XMM0,[V1]
           SUBPS   XMM0, [V2]
           // Get the absolute value of the difference
           //  vTemp = _mm_setzero_ps();
           XORPS   XMM1,XMM1
           // vTemp = _mm_sub_ps(vTemp,vDelta);
           SUBPS   XMM1, XMM0
           // vTemp = _mm_max_ps(vTemp,vDelta);
           MAXPS   XMM1, XMM0
           // vTemp = _mm_cmple_ps(vTemp,Epsilon);
           CMPPS   XMM1, [Epsilon], 2
           // w is don't care
           // return (((_mm_movemask_ps(vTemp)&7)==0x7) != 0);

           MOVMSKPS EAX, XMM1
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;



function XMVector3NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
asm
           // vTemp = _mm_cmpeq_ps(V1,V2);
           MOVUPS  XMM0, [V1]
           CMPPS   XMM0, [V2], 0
           // return (((_mm_movemask_ps(vTemp)&7)!=7) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;



function XMVector3NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
asm
           // vTemp = _mm_cmpeq_epi32(_mm_castps_si128(V1),_mm_castps_si128(V2));
           MOVUPS  XMM0, [V1]
           PCMPEQD XMM0, [V2]
           // return (((_mm_movemask_ps(_mm_castsi128_ps(vTemp))&7)!=7) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;



function XMVector3Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
asm
           // vTemp = _mm_cmpgt_ps(V1,V2);
           MOVUPS  XMM0,[V2]
           CMPPS   XMM0, [V1], 1
           //return (((_mm_movemask_ps(vTemp)&7)==7) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;



function XMVector3GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
asm
           // vTemp = _mm_cmpgt_ps(V1,V2);
           MOVUPS  XMM0,[V2]
           CMPPS   XMM0, [V1], 1
 {   uint32_t CR = 0;
    int iTest = _mm_movemask_ps(vTemp)&7;
    movmskps r32, xmm
    if (iTest==7)

        CR =  XM_CRMASK_CR6TRUE;

    else if (!iTest)

        CR = XM_CRMASK_CR6FALSE;

    return CR;   }

           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           MOV     EDX,XM_CRMASK_CR6FALSE
           CMP     EAX, $0
           JE      @Finished

           MOV     EDX,XM_CRMASK_CR6TRUE
           CMP     EAX, $7
           JE      @Finished
           MOV     EDX,$0
           @Finished:
           MOV     [result],EDX
end;



function XMVector3GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
asm
           // vTemp = _mm_cmpge_ps(V1,V2);
           MOVUPS  XMM0,[V2]
           CMPPS   XMM0, [V1], 2
           //return (((_mm_movemask_ps(vTemp)&7)==7) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;



function XMVector3GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32; assembler;
asm
           // vTemp = _mm_cmpge_ps(V1,V2);
           MOVUPS  XMM0, [V2]
           CMPPS   XMM0, [V1], 2
    {uint32_t CR = 0;
    int iTest = _mm_movemask_ps(vTemp)&7;
    movmskps r32, xmm
    if (iTest==7)
        CR =  XM_CRMASK_CR6TRUE;
    else if (!iTest)
        CR = XM_CRMASK_CR6FALSE;

    return CR;  }

           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           MOV     EDX,XM_CRMASK_CR6FALSE
           CMP     EAX, $0
           JE      @Finished
           MOV     EDX,XM_CRMASK_CR6TRUE
           CMP     EAX, $7
           JE      @Finished
           MOV     EDX,$0
           @Finished:
           MOV     [result],EDX
end;



function XMVector3Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           MOVUPS  XMM0,[v1]
           CMPPS   XMM0, [v2],  1
           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;



function XMVector3LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           MOVUPS  XMM0,[v1]
           CMPPS   XMM0, [v2], 2
           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;

{$IF DEFINED(_XM_SSE_INTRINSICS_)}

function XMVector3InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean; assembler;
asm
           // Test if less than or equal
           // vTemp1 = _mm_cmple_ps(V,Bounds);
           MOVUPS  XMM0,[V]
           CMPPS   XMM0, [Bounds], 2
           // Negate the bounds
           // vTemp2 = _mm_mul_ps(Bounds,g_XMNegativeOne);
           MOVUPS  XMM1,[Bounds]
           MULPS   XMM1, [g_XMNegativeOne]
           // Test if greater or equal (Reversed)
           // vTemp2 = _mm_cmple_ps(vTemp2,V);
           CMPPS   XMM1, [V], 2
           // Blend answers
           // vTemp1 = _mm_and_ps(vTemp1,vTemp2);
           ANDPS   XMM0, XMM1
           // x,y and z in bounds? (w is don't care)
           // return (((_mm_movemask_ps(vTemp1)&0x7)==0x7) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;

{$ELSE}


function XMVector3InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean;
begin
    Result := XMComparisonAllInBounds(XMVector3InBoundsR(V, Bounds));
end;

{$ENDIF}


function XMVector3IsNaN(constref V: TXMVECTOR): boolean; assembler;
asm
           // Test against itself. NaN is always not equal
           // vTempNan = _mm_cmpneq_ps(V,V);
           MOVUPS  XMM0, [V]
           CMPPS   XMM0, XMM0, 4
           // If x or y or z are NaN, the mask is non-zero
           // return ((_mm_movemask_ps(vTempNan)&7) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;



function XMVector3IsInfinite(constref V: TXMVECTOR): boolean; assembler;
asm
           // Mask off the sign bit
           // __m128 vTemp = _mm_and_ps(V,g_XMAbsMask);
           MOVUPS  XMM0,[V]
           ANDPS   XMM0, [g_XMAbsMask]
           // Compare to infinity
           // vTemp = _mm_cmpeq_ps(vTemp,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 0
           // If x,y or z are infinity, the signs are true.
           // return ((_mm_movemask_ps(vTemp)&7) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $7
           CMP     EAX, $7
           SETE    [result]
end;




//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------

{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector3Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // _mm_dp_ps( V1, V2, 0x7f );
           MOVUPS  XMM0, [V1]
           DPPS    XMM0, [V2], $7F
           MOVUPS  [result], XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector3Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vTemp = _mm_mul_ps(V1,V2);
           MOVUPS  XMM0, [V1]
           MULPS   XMM0, [V2]
           // vTemp = _mm_and_ps(vTemp, g_XMMask3);
           ANDPS   XMM0, [g_XMMask3]
           // vTemp = _mm_hadd_ps(vTemp,vTemp);
           HADDPS  XMM0, XMM0
           // return _mm_hadd_ps(vTemp,vTemp);
           HADDPS  XMM0, XMM0
           MOVUPS  [result],XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector3Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product
           // vDot = _mm_mul_ps(V1,V2);
           MOVUPS  XMM0, [v1]
           MULPS   XMM0,[V2]
           // x=Dot.f32[1], y=Dot.f32[2]
           // vTemp = XM_PERMUTE_PS(vDot,_MM_SHUFFLE(2,1,2,1));
           MOVUPS  XMM1, XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_1_2_1
           // Result.f32[0] = x+y
           // vDot = _mm_add_ss(vDot,vTemp);
           ADDSS   XMM0, XMM1
           // x=Dot.f32[2]
           // vTemp = XM_PERMUTE_PS(vTemp,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // Result.f32[0] = (x+y)+z
           // vDot = _mm_add_ss(vDot,vTemp);
           ADDSS   XMM0, XMM1
           // Splat x
           // return XM_PERMUTE_PS(vDot,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           MOVUPS  [result], XMM0
end;
{$ENDIF}


function XMVector3Cross(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // y1,z1,x1,w1
           // vTemp1 = XM_PERMUTE_PS(V1,_MM_SHUFFLE(3,0,2,1));
           MOVUPS  XMM1,[V1]
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_3_0_2_1
           // z2,x2,y2,w2
           //  vTemp2 = XM_PERMUTE_PS(V2,_MM_SHUFFLE(3,1,0,2));
           MOVUPS  XMM2,[V2]
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_3_1_0_2
           // Perform the left operation
           //  vResult = _mm_mul_ps(vTemp1,vTemp2);
           MOVUPS  XMM0,XMM1
           MULPS   XMM0, XMM2
           // z1,x1,y1,w1
           // vTemp1 = XM_PERMUTE_PS(vTemp1,_MM_SHUFFLE(3,0,2,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_3_0_2_1
           // y2,z2,x2,w2
           // vTemp2 = XM_PERMUTE_PS(vTemp2,_MM_SHUFFLE(3,1,0,2));
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_3_1_0_2
           // Perform the right operation
           // vTemp1 = _mm_mul_ps(vTemp1,vTemp2);
           MULPS   XMM1, XMM2
           // Subract the right from left, and return answer
           // vResult = _mm_sub_ps(vResult,vTemp1);
           SUBPS   XMM0, XMM1
           // Set w to zero
           // return _mm_and_ps(vResult,g_XMMask3);
           ANDPS   XMM0, [g_XMMask3]
           MOVUPS  [result],XMM0
end;



{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector3ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0x7f );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $7F
           // return _mm_rsqrt_ps( vTemp );
           RSQRTPS XMM0, XMM0
           MOVUPS  [result], XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector3ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vLengthSq  = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vLengthSq = _mm_and_ps(vLengthSq, g_XMMask3);
           ANDPS   XMM0, [g_XMMask3]
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_rsqrt_ps(vLengthSq);
           RSQRTPS XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector3ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y and z
           //  vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp has z and y
           //  vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,2,1,2));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_2_1_2
           // x+z, y
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDSS   XMM0, XMM1

           // y,y,y,y
           // vTemp = XM_PERMUTE_PS(vTemp,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // x+z+y,??,??,??
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDSS   XMM0, XMM1
           // Splat the length squared
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // Get the reciprocal
           // vLengthSq = _mm_rsqrt_ps(vLengthSq);
           RSQRTPS XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result], XMM0
end;
{$ENDIF}


{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector3ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0x7f );
           MOVUPS  XMM1,[V]
           DPPS    XMM1, XMM1, $7F
           //  vLengthSq = _mm_sqrt_ps( vTemp );
           SQRTPS  XMM1, XMM1
           // return _mm_div_ps( g_XMOne, vLengthSq );
           MOVUPS  XMM0,[g_XMOne]
           DIVPS   XMM0, XMM1
           MOVUPS  [result], XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector3ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vDot = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vDot = _mm_and_ps(vDot, g_XMMask3);
           ANDPS   XMM0, [g_XMMask3]
           // vDot = _mm_hadd_ps(vDot, vDot);
           HADDPS  XMM0, XMM0
           // vDot = _mm_hadd_ps(vDot, vDot);
           HADDPS  XMM0, XMM0
           // vDot = _mm_sqrt_ps(vDot);
           SQRTPS  XMM1, XMM0
           // vDot = _mm_div_ps(g_XMOne,vDot);
           MOVUPS  XMM0,[g_XMOne]
           DIVPS   XMM0, XMM1
           // return vDot;
           MOVUPS  [result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector3ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product
           // vDot = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // x=Dot.y, y=Dot.z
           // vTemp = XM_PERMUTE_PS(vDot,_MM_SHUFFLE(2,1,2,1));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_1_2_1
           // Result.x = x+y
           //vDot = _mm_add_ss(vDot,vTemp);
           ADDSS   XMM0, XMM1
           // x=Dot.z
           //vTemp = XM_PERMUTE_PS(vTemp,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // Result.x = (x+y)+z
           //vDot = _mm_add_ss(vDot,vTemp);
           ADDSS   XMM0, XMM1
           // Splat x
           //vDot = XM_PERMUTE_PS(vDot,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // Get the reciprocal
           //vDot = _mm_sqrt_ps(vDot);
           SQRTPS  XMM0, XMM0
           // Get the reciprocal
           //vDot = _mm_div_ps(g_XMOne,vDot);
           MOVUPS  XMM1,[g_XMOne]
           DIVPS   XMM1, XMM0
           // return vDot;
           MOVUPS  [result] ,XMM1
end;
{$ENDIF}


{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector3LengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0x7f );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $7F
           // return _mm_sqrt_ps( vTemp );
           SQRTPS  XMM0, XMM0
           MOVUPS  [result] ,XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector3LengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vLengthSq = _mm_and_ps(vLengthSq, g_XMMask3);
           ANDPS   XMM0, [g_XMMask3]
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result] ,XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector3LengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y and z
           //  vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp has z and y
           //  vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,2,1,2));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_2_1_2
           // x+z, y
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDSS   XMM0, XMM1
           // y,y,y,y
           // vTemp = XM_PERMUTE_PS(vTemp,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // x+z+y,??,??,??
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDSS   XMM0, XMM1
           // Splat the length squared
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // Get the length
           // vLengthSq = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result] ,XMM0
end;
{$ENDIF}



{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector3Length(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0x7f );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $7F
           // return _mm_sqrt_ps( vTemp );
           SQRTPS  XMM0, XMM0
           MOVUPS  [result] ,XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector3Length(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vLengthSq = _mm_and_ps(vLengthSq, g_XMMask3);
           ANDPS   XMM0, [g_XMMask3]
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result] ,XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector3Length(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y and z
           // vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp has z and y
           // vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,2,1,2));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_2_1_2
           // x+z, y
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDSS   XMM0, XMM1
           // y,y,y,y
           // vTemp = XM_PERMUTE_PS(vTemp,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // x+z+y,??,??,??
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDSS   XMM0, XMM1
           // Splat the length squared
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // Get the length
           // vLengthSq = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result] ,XMM0
end;
{$ENDIF}


{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector3NormalizeEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0x7f );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $7F
           //  vResult = _mm_rsqrt_ps( vTemp );
           RSQRTPS XMM0, XMM0
           // return _mm_mul_ps(vResult, V);
           MULPS   XMM0, [V]
           MOVUPS  [result] ,XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector3NormalizeEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vDot = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vDot = _mm_and_ps(vDot, g_XMMask3);
           ANDPS   XMM0, [g_XMMask3]
           // vDot = _mm_hadd_ps(vDot, vDot);
           HADDPS  XMM0, XMM0
           // vDot = _mm_hadd_ps(vDot, vDot);
           HADDPS  XMM0, XMM0
           // vDot = _mm_rsqrt_ps(vDot);
           RSQRTPS XMM0, XMM0
           // vDot = _mm_mul_ps(vDot,V);
           MULPS   XMM0, [V]
           // return vDot;
           MOVUPS  [result] ,XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector3NormalizeEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product
           //  vDot = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // x=Dot.y, y=Dot.z
           //  vTemp = XM_PERMUTE_PS(vDot,_MM_SHUFFLE(2,1,2,1));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_1_2_1
           // Result.x = x+y
           // vDot = _mm_add_ss(vDot,vTemp);
           ADDSS   XMM0, XMM1
           // x=Dot.z
           // vTemp = XM_PERMUTE_PS(vTemp,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // Result.x = (x+y)+z
           // vDot = _mm_add_ss(vDot,vTemp);
           ADDSS   XMM0, XMM1
           // Splat x
           // vDot = XM_PERMUTE_PS(vDot,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // Get the reciprocal
           // vDot = _mm_rsqrt_ps(vDot);
           RSQRTPS XMM0, XMM0
           // Perform the normalization
           // vDot = _mm_mul_ps(vDot,V);
           MULPS   XMM0, [V]
           // return vDot;
           MOVUPS  [result] ,XMM0
end;
{$ENDIF}


{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector3Normalize(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vLengthSq = _mm_dp_ps( V, V, 0x7f );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $7F
           // Prepare for the division
           //  vResult = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM1, XMM0
           // Create zero with a single instruction
           //  vZeroMask = _mm_setzero_ps();
           XORPS   XMM2, XMM2
           // Test for a divide by zero (Must be FP to detect -0.0)
           // vZeroMask = _mm_cmpneq_ps(vZeroMask,vResult);
           CMPPS   XMM2, XMM1, 4
           // Failsafe on zero (Or epsilon) length planes
           // If the length is infinity, set the elements to zero
           // vLengthSq = _mm_cmpneq_ps(vLengthSq,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 4
           // Divide to perform the normalization
           // vResult = _mm_div_ps(V,vResult);
           MOVUPS  XMM3,[V]
           DIVPS   XMM3, XMM1
           // Any that are infinity, set to zero
           // vResult = _mm_and_ps(vResult,vZeroMask);
           ANDPS   XMM3, XMM2
           // Select qnan or result based on infinite length
           //  vTemp1 = _mm_andnot_ps(vLengthSq,g_XMQNaN);
           MOVUPS  XMM2, [g_XMQNaN]
           ANDNPS  XMM2, XMM0
           //  vTemp2 = _mm_and_ps(vResult,vLengthSq);
           ANDPS   XMM3, XMM0
           // vResult = _mm_or_ps(vTemp1,vTemp2);
           ORPS    XMM3, XMM2
           // return vResult;
           MOVUPS  [result] ,XMM3
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector3Normalize(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y and z only
           //  vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vLengthSq = _mm_and_ps(vLengthSq, g_XMMask3);
           ANDPS   XMM0, [g_XMMask3]
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           //  vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // Prepare for the division
           //  vResult = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM1, XMM0
           // Create zero with a single instruction
           //  vZeroMask = _mm_setzero_ps();
           XORPS   XMM2, XMM2
           // Test for a divide by zero (Must be FP to detect -0.0)
           // vZeroMask = _mm_cmpneq_ps(vZeroMask,vResult);
           CMPPS   XMM2, XMM1, 4
           // Failsafe on zero (Or epsilon) length planes
           // If the length is infinity, set the elements to zero
           // vLengthSq = _mm_cmpneq_ps(vLengthSq,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 4
           // Divide to perform the normalization
           // vResult = _mm_div_ps(V,vResult);
           MOVUPS  XMM3, [V]
           DIVPS   XMM3, XMM1
           // Any that are infinity, set to zero
           // vResult = _mm_and_ps(vResult,vZeroMask);
           ANDPS   XMM1, XMM2
           // Select qnan or result based on infinite length
           //  vTemp1 = _mm_andnot_ps(vLengthSq,g_XMQNaN);
           MOVUPS  XMM2, [g_XMQNaN]
           ANDNPS  XMM2, XMM0
           //  vTemp2 = _mm_and_ps(vResult,vLengthSq);
           ANDPS   XMM0, XMM1
           // vResult = _mm_or_ps(vTemp1,vTemp2);
           ORPS    XMM0, XMM2
           // return vResult;
           MOVUPS  [result] ,XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector3Normalize(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y and z only
           //  vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           //  vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(2,1,2,1));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_1_2_1
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDSS   XMM0, XMM1
           // vTemp = XM_PERMUTE_PS(vTemp,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDSS   XMM0, XMM1
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // Prepare for the division
           //  vResult = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM2, XMM0
           // Create zero with a single instruction
           //  vZeroMask = _mm_setzero_ps();
           XORPS   XMM1, XMM1
           // Test for a divide by zero (Must be FP to detect -0.0)
           // vZeroMask = _mm_cmpneq_ps(vZeroMask,vResult);
           CMPPS   XMM1, XMM2, 4
           // Failsafe on zero (Or epsilon) length planes
           // If the length is infinity, set the elements to zero
           // vLengthSq = _mm_cmpneq_ps(vLengthSq,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 4
           // Divide to perform the normalization
           // vResult = _mm_div_ps(V,vResult);
           MOVUPS  XMM3,[V]
           DIVPS   XMM3, XMM2
           // Any that are infinity, set to zero
           // vResult = _mm_and_ps(vResult,vZeroMask);
           ANDPS   XMM3, XMM1
           // Select qnan or result based on infinite length
           //  vTemp1 = _mm_andnot_ps(vLengthSq,g_XMQNaN);
           MOVUPS  XMM1,[g_XMQNaN]
           ANDNPS  XMM1, XMM0
           //  vTemp2 = _mm_and_ps(vResult,vLengthSq);
           ANDPS   XMM3, XMM0
           // vResult = _mm_or_ps(vTemp1,vTemp2);
           ORPS    XMM1, XMM3
           // return vResult;
           MOVUPS  [result] ,XMM1
end;
{$ENDIF}



function XMVector3RefractV(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR; constref RefractionIndex: TXMVECTOR): TXMVECTOR; inline;
var
    IDotN: TXMVECTOR;
begin
    // Result = RefractionIndex * Incident - Normal * (RefractionIndex * dot(Incident, Normal) +
    // sqrt(1 - RefractionIndex * RefractionIndex * (1 - dot(Incident, Normal) * dot(Incident, Normal))))
    IDotN := XMVector3Dot(Incident, Normal);
    // R = 1.0f - RefractionIndex * RefractionIndex * (1.0f - IDotN * IDotN)
    asm
               //  R = _mm_mul_ps(IDotN, IDotN);
               MOVUPS  XMM0, [IDotN]
               MULPS   XMM0, XMM0
               // R = _mm_sub_ps(g_XMOne,R);
               MOVUPS  XMM1, [g_XMOne]
               SUBPS   XMM1, XMM0
               // R = _mm_mul_ps(R, RefractionIndex);
               MULPS   XMM1, [RefractionIndex]
               // R = _mm_mul_ps(R, RefractionIndex);
               MULPS   XMM1, [RefractionIndex]
               // R = _mm_sub_ps(g_XMOne,R);
               MOVUPS  XMM0, [g_XMOne]
               SUBPS   XMM0, XMM1
               MOVUPS  XMM1,XMM0 // XMM1 = R
               //  vResult = _mm_cmple_ps(R,g_XMZero);
               CMPPS   XMM0, [g_XMZero], 2


               MOVMSKPS EAX, XMM0
               CMP     EAX, $0F
               JE      @InternalReflection  // if (_mm_movemask_ps(vResult)==0x0f) -> Total internal reflection
               // R = RefractionIndex * IDotN + sqrt(R)
               // R = _mm_sqrt_ps(R);
               sqrtps XMM1, XMM1
               // vResult = _mm_mul_ps(RefractionIndex,IDotN);
               MOVUPS  XMM2, [RefractionIndex]
               MULPS   XMM2, [IDotN]
               // R = _mm_add_ps(R,vResult);
               ADDPS   XMM1, XMM2
               // Result = RefractionIndex * Incident - Normal * R
               // vResult = _mm_mul_ps(RefractionIndex, Incident);
               MOVUPS  XMM0, [RefractionIndex]
               MULPS   XMM0, [Incident]
               // R = _mm_mul_ps(R,Normal);
               MULPS   XMM1, [Normal]
               // vResult = _mm_sub_ps(vResult,R);
               SUBPS   XMM0, XMM1
               JMP     @Finished

               @InternalReflection:
               MOVUPS  XMM0, [g_XMZero] // vResult = g_XMZero;
               @Finished:
               MOVUPS  [result],XMM0
    end;
end;



function XMVector3Transform(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR; assembler;
asm
           //  vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM0, [V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // vResult = _mm_mul_ps(vResult,M.r[0]);
           MULPS   XMM0, TXMMATRIX([M]).r0
           //  vTemp = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM1, [V]
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // vTemp = _mm_mul_ps(vTemp,M.r[1]);
           MULPS   XMM1, TXMMATRIX([M]).r1
           // vResult = _mm_add_ps(vResult,vTemp);
           ADDPS   XMM0, XMM1
           // vTemp = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
           MOVUPS  XMM1, [V]
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_2_2_2
           // vTemp = _mm_mul_ps(vTemp,M.r[2]);
           MULPS   XMM1, TXMMATRIX([M]).r2
           //  vResult = _mm_add_ps(vResult,vTemp);
           ADDPS   XMM0, XMM1
           // vResult = _mm_add_ps(vResult,M.r[3]);
           ADDPS   XMM0, TXMMATRIX([M]).r3
           // return vResult;
           MOVUPS  [result] ,XMM0
end;



function XMVector3TransformStream(out pOutputStream: PXMFLOAT4; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT4;
var
    pInputVector, pOutputVector: pointer;
    row0, row1, row2, row3: TXMVECTOR;
    i, four, j: size_t;
    s: uint32;
    V, V1, L2, L3, V2, V3, V4: TXMVECTOR;
begin
    pInputVector := pInputStream;
    pOutputVector := pOutputStream;

    row0 := M.r[0];
    row1 := M.r[1];
    row2 := M.r[2];
    row3 := M.r[3];

    i := 0;
    four := VectorCount shr 2;
    if (four > 0) then
    begin
        if (InputStride = sizeof(TXMFLOAT3)) then
        begin
            if (not (uintptr(pOutputStream) and $F = $F) and not (OutputStride and $F = $F)) then
            begin
                // Packed input, aligned output
                s := sizeof(TXMFLOAT3) * 4;
                for j := 0 to four - 1 do
                begin
                    asm
                               // V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                               MOVSS  XMM0, [pInputVector]
                               MOVUPS  [V1],XMM0
                               //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                               MOVUPS  XMM1, [pInputVector+16]
                               //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                               MOVUPS  XMM2, [pInputVector+32]
                               // pInputVector += sizeof(XMFLOAT3)*4;
                               MOV     EDX, pInputVector
                               ADD     EDX, [s]
                               MOV     pInputVector,EDX

                               // Unpack the 4 vectors (.w components are junk)
                               // XM3UNPACK3INTO4(V1,L2,L3);
                               MOVUPS  XMM3, XMM1
                               SHUFPS  XMM3, XMM2, _MM_SHUFFLE_0_0_3_2 // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                               MOVUPS  [V3],XMM3
                               SHUFPS  XMM1, XMM0, _MM_SHUFFLE_3_3_1_0 // V2 := _mm_shuffle_ps(l2,l1,_MM_SHUFFLE(3,3,1,0));

                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_0_2 // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                               MOVUPS  [V2] , XMM1

                               PSRLDQ  XMM2, 4 //  V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                               MOVUPS  [V4] , XMM2

                               // Result 1
                               //  Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM2, [V1]
                               MOVUPS  XMM1, XMM2
                               MOVUPS  XMM0, XMM2
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               //  vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               //  vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               //  vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0
                               // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVUPS  [pOutputVector], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 2
                               // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM2, [V2]
                               MOVUPS  XMM1, XMM2
                               MOVUPS  XMM0, XMM2
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0
                               // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVUPS  [pOutputVector],XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 3
                               // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM2, [V3]
                               MOVUPS  XMM1, XMM2
                               MOVUPS  XMM0, XMM2
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0
                               // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVUPS  [pOutputVector],XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 4
                               // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM2, [V4]
                               MOVUPS  XMM1, XMM2
                               MOVUPS  XMM0, XMM2
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1

                               // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0
                               // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVUPS  [pOutputVector],XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX
                    end;
                    Inc(i, 4);
                end;
            end
            else
            begin
                // Packed input, unaligned output
                s := sizeof(TXMFLOAT3) * 4;
                for j := 0 to four - 1 do
                begin
                    asm
                               //  V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                               MOVUPS  XMM0, [pInputVector]
                               MOVUPS  [V1],XMM0
                               //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                               MOVUPS  XMM1, [pInputVector+16]
                               //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                               MOVUPS  XMM2, [pInputVector+32]
                               // pInputVector += sizeof(XMFLOAT3)*4;
                               MOV     EDX, pInputVector
                               ADD     EDX, [s]
                               MOV     pInputVector,EDX

                               // Unpack the 4 vectors (.w components are junk)
                               // XM3UNPACK3INTO4(V1,L2,L3);
                               MOVUPS  XMM3, XMM1
                               SHUFPS  XMM3, XMM2, _MM_SHUFFLE_0_0_3_2  // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                               MOVUPS  [V3],XMM3
                               SHUFPS  XMM1, XMM0, _MM_SHUFFLE_3_3_1_0  // V2 := _mm_shuffle_ps(l2,l1,_MM_SHUFFLE(3,3,1,0));
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_0_2 // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                               MOVUPS  [V2],XMM1
                               PSRLDQ  XMM2, 4   // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                               MOVUPS  [V4],XMM2

                               // Result 1
                               MOVUPS  XMM0,[V1]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               // Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               //  vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               //  vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               //  vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0
                               // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVUPS  [pOutputVector], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 2
                               // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,[V2]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0
                               // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVUPS  [pOutputVector], XMM2
                               //  pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 3
                               // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,[V3]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               //  X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0
                               // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVUPS  [pOutputVector], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 4
                               // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,[V4]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0
                               // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTemp );
                               MOVUPS  [pOutputVector], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX
                    end;
                    Inc(i, 4);
                end;
            end;
        end;
    end;

    if (not (uintptr(pOutputStream) and $F = $F) and not (OutputStride and $F = $F)) then
    begin
        // Aligned output
        // for (; i < VectorCount; ++i)
        while i < VectorCount do
        begin
            V := XMLoadFloat3(pInputVector);
            asm

                       // pInputVector += InputStride;
                       MOV     EDX, pInputVector
                       ADD     EDX, [InputStride]
                       MOV     pInputVector,EDX

                       //  Z = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                       MOVUPS  XMM0,[V]
                       MOVUPS  XMM1,XMM0
                       MOVUPS  XMM2,XMM0
                       SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                       //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                       SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                       //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                       SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                       //  vTemp = _mm_mul_ps( Z, row2 );
                       MULPS   XMM2, [row2]
                       //  vTemp2 = _mm_mul_ps( Y, row1 );
                       MULPS   XMM1, [row1]
                       //  vTemp3 = _mm_mul_ps( X, row0 );
                       MULPS   XMM0, [row0]
                       // vTemp = _mm_add_ps( vTemp, row3 );
                       ADDPS   XMM2, [row3]
                       // vTemp = _mm_add_ps( vTemp, vTemp2 );
                       ADDPS   XMM2, XMM1
                       // vTemp = _mm_add_ps( vTemp, vTemp3 );
                       ADDPS   XMM2, XMM0

                       // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTemp );
                       MOVUPS  [pOutputVector],XMM2
                       // pOutputVector += OutputStride;
                       MOV     EDX, pOutputVector
                       ADD     EDX, [OutputStride]
                       MOV     pOutputVector,EDX
            end;
            Inc(i);
        end;
    end
    else
    begin
        // Unaligned output
        // for (; i < VectorCount; ++i)
        while i < VectorCount do
        begin
            V := XMLoadFloat3(pInputVector);
            asm

                       // pInputVector += InputStride;
                       MOV     EDX, pInputVector
                       ADD     EDX, [InputStride]
                       MOV     pInputVector,EDX

                       //  Z = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                       MOVUPS  XMM0,[V]
                       MOVUPS  XMM1,XMM0
                       MOVUPS  XMM2,XMM0
                       SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                       //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                       SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                       //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                       SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                       //  vTemp = _mm_mul_ps( Z, row2 );
                       MULPS   XMM2, [row2]
                       //  vTemp2 = _mm_mul_ps( Y, row1 );
                       MULPS   XMM1, [row1]
                       //  vTemp3 = _mm_mul_ps( X, row0 );
                       MULPS   XMM0, [row0]
                       // vTemp = _mm_add_ps( vTemp, row3 );
                       ADDPS   XMM2, [row3]
                       // vTemp = _mm_add_ps( vTemp, vTemp2 );
                       ADDPS   XMM2, XMM1
                       // vTemp = _mm_add_ps( vTemp, vTemp3 );
                       ADDPS   XMM2, XMM0

                       // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTemp );
                       MOVUPS  [pOutputVector],XMM2
                       // pOutputVector += OutputStride;
                       MOV     EDX, pOutputVector
                       ADD     EDX, [OutputStride]
                       MOV     pOutputVector,EDX
            end;
            Inc(i);
        end;
    end;

    // XM_SFENCE();

    Result := pOutputStream;
end;



function XMVector3TransformCoordStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT3;
var
    pInputVector, pOutputVector: pointer;
    row0, row1, row2, row3: TXMVECTOR;
    i, j, four: size_t;
    s: uint32;
    V, V1, V2, V3, V4: TXMVECTOR;
begin
    pInputVector := pInputStream;
    pOutputVector := pOutputStream;

    row0 := M.r[0];
    row1 := M.r[1];
    row2 := M.r[2];
    row3 := M.r[3];

    i := 0;
    four := VectorCount shr 2;
    if (four > 0) then
    begin
        if (InputStride = sizeof(TXMFLOAT3)) then
        begin
            if (OutputStride = sizeof(TXMFLOAT3)) then
            begin
                if (not (uintptr(pOutputStream) and $F = $F)) then
                begin
                    // Packed input, aligned & packed output
                    s := sizeof(TXMFLOAT3) * 4;
                    for j := 0 to four - 1 do
                    begin
                        asm
                                   //  V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM1, [pInputVector]
                                   MOVUPS  [V1], XMM1
                                   //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                                   MOVUPS  XMM2, [pInputVector+16]
                                   //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                                   MOVUPS  XMM3, [pInputVector+32]
                                   // pInputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [s]
                                   MOV     pInputVector,EDX

                                   // Unpack the 4 vectors (.w components are junk)
                                   // XM3UNPACK3INTO4(V1,L2,L3);
                                   // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                                   MOVUPS  XMM4,XMM2
                                   SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                                   MOVUPS  [V3], XMM4
                                   // V2 := _mm_shuffle_ps(l2,l1,_MM_SHUFFLE(3,3,1,0));
                                   SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0
                                   // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                                   MOVUPS  [V2], XMM2
                                   // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                                   PSRLDQ  XMM3, 4
                                   MOVUPS  [V4], XMM3

                                   // Result 1
                                   //  Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V1]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   //  vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   //  vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   //  vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM2, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM4,XMM2
                                   SHUFPS  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3

                                   //  V1 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM4
                                   MOVUPS  [V1], XMM2

                                   // Result 2
                                   // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V2]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM2, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM4,XMM2
                                   SHUFPS  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3

                                   // V2 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM4
                                   MOVUPS  [V2], XMM2

                                   // Result 3
                                   // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V3]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2

                                   // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM2, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM4,XMM2
                                   SHUFPS  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3

                                   // V3 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM4
                                   MOVUPS  [V3], XMM2

                                   // Result 4
                                   // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V4]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM2, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM4,XMM2
                                   SHUFPS  XMM4, XMM4, _MM_SHUFFLE_3_3_3_3

                                   // V4 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM4
                                   MOVUPS  [V4], XMM2

                                   // Pack and store the vectors
                                   // XM3PACK4INTO3(vTemp);


                                   MOVUPS  XMM0, [v2]
                                   SHUFPS  XMM0, [v3], _MM_SHUFFLE_1_0_2_1 // vTemp := _mm_shuffle_ps(V2,V3,_MM_SHUFFLE(1,0,2,1));
                                   MOVUPS  XMM2, [v2]
                                   SHUFPS  XMM2, [v1], _MM_SHUFFLE_2_2_0_0 // V2 := _mm_shuffle_ps(V2,V1,_MM_SHUFFLE(2,2,0,0));
                                   MOVUPS  XMM1, [v1]
                                   SHUFPS  XMM1, XMM2, _MM_SHUFFLE_0_2_1_0 // V1 := _mm_shuffle_ps(V1,V2,_MM_SHUFFLE(0,2,1,0));
                                   MOVUPS  XMM3, [v3]
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_0_0_2_2 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(0,0,2,2));
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_2_1_2_0 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(2,1,2,0));



                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), V1 );
                                   MOVUPS  [pOutputVector], XMM1
                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector+16), vTemp );
                                   MOVUPS  [pOutputVector+16], XMM0
                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector+32), V3 );
                                   MOVUPS  [pOutputVector+32], XMM3
                                   //  pOutputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        i += 4;
                    end;
                end
                else
                begin
                    // Packed input, unaligned & packed output
                    s := sizeof(TXMFLOAT3) * 4;
                    for j := 0 to four - 1 do
                    begin
                        asm
                                   //  V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM1, [pInputVector]
                                   //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                                   MOVUPS  XMM2, [pInputVector+16]
                                   //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                                   MOVUPS  XMM3, [pInputVector+32]
                                   //  pInputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [s]
                                   MOV     pInputVector,EDX

                                   // Unpack the 4 vectors (.w components are junk)
                                   // XM3UNPACK3INTO4(V1,L2,L3);
                                   // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                                   MOVUPS  XMM4, XMM2
                                   SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                                   MOVUPS  [V3], XMM4
                                   // V2 := _mm_shuffle_ps(l2,V1,_MM_SHUFFLE(3,3,1,0));
                                   SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0
                                   // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                                   MOVUPS  [v2],XMM2
                                   // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                                   PSRLDQ  XMM3, 4
                                   MOVUPS  [v4],XMM3

                                   // Result 1
                                   //  Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V1]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   //  vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   //  vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   //  vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM2, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                                   // V1 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V1],XMM2

                                   // Result 2
                                   // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V2]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM2, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                                   // V2 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V2],XMM2

                                   // Result 3
                                   // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V3]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM2, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3, XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                                   // V3 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V3],XMM2

                                   // Result 4
                                   // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V4]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, row3 );
                                   ADDPS   XMM2, [row3]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM2,XMM3
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                                   // V4 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V4],XMM2

                                   // Pack and store the vectors
                                   // XM3PACK4INTO3(vTemp);
                                   MOVUPS  XMM0, [V2]
                                   SHUFPS  XMM0, [v3], _MM_SHUFFLE_1_0_2_1 // temp := _mm_shuffle_ps(V2,V3,_MM_SHUFFLE(1,0,2,1));
                                   MOVUPS  XMM2, [v2]
                                   SHUFPS  XMM2, [v1], _MM_SHUFFLE_2_2_0_0 // V2 := _mm_shuffle_ps(V2,V1,_MM_SHUFFLE(2,2,0,0));
                                   MOVUPS  XMM1, [v1]
                                   SHUFPS  XMM1, XMM2, _MM_SHUFFLE_0_2_1_0 // V1 := _mm_shuffle_ps(V1,V2,_MM_SHUFFLE(0,2,1,0));
                                   MOVUPS  XMM3, [v3]
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_0_0_2_2 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(0,0,2,2));
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_2_1_2_0 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(2,1,2,0));



                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), V1 );
                                   MOVUPS  [pOutputVector], XMM1
                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector+16), vTemp );
                                   MOVUPS  [pOutputVector+16], XMM0
                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector+32), V3 );
                                   MOVUPS  [pOutputVector+32], XMM3
                                   // pOutputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        Inc(i, 4);
                    end;
                end;
            end
            else
            begin
                // Packed input, unpacked output
                s := sizeof(TXMFLOAT3) * 4;
                for  j := 0 to four - 1 do
                begin
                    asm
                               //  V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                               MOVUPS  XMM1, [pInputVector]
                               MOVUPS  [V1],XMM1
                               //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                               MOVUPS  XMM2, [pInputVector+16]
                               //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                               MOVUPS  XMM3, [pInputVector+32]
                               // pInputVector += sizeof(TXMFLOAT3)*4;
                               MOV     EDX, pInputVector
                               ADD     EDX, [s]
                               MOV     pInputVector,EDX

                               // Unpack the 4 vectors (.w components are junk)
                               // XM3UNPACK3INTO4(V1,L2,L3);
                               // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                               MOVUPS  XMM4,XMM2
                               SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                               MOVUPS  [V3],XMM4
                               // V2 := _mm_shuffle_ps(l2,V1,_MM_SHUFFLE(3,3,1,0));
                               SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0
                               // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                               MOVUPS  [V2],XMM2
                               // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                               PSRLDQ  XMM3, 4
                               MOVUPS  [V4],XMM3

                               // Result 1
                               //  Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,[V1]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               //  vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               //  vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               //  vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3,XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3
                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 2
                               // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,[V2]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3,XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3
                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               MOVUPS  [pOutputVector],XMM2
                               //  pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 3
                               // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,[V3]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3,XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3
                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               MOVUPS  [pOutputVector],XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 4
                               // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,[V4]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, row3 );
                               ADDPS   XMM2, [row3]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3,XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3
                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               MOVUPS  [pOutputVector],XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX
                    end;
                    i += 4;
                end;
            end;
        end;
    end;

    //for (; i < VectorCount; i++)
    while i < VectorCount do
    begin
        V := XMLoadFloat3(pInputVector);
        asm
                   // pInputVector += InputStride;
                   MOV     EDX, pInputVector
                   ADD     EDX, [InputStride]
                   MOV     pInputVector,EDX

                   //  Z = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                   MOVUPS  XMM2, [V]
                   MOVUPS  XMM1, XMM2
                   MOVUPS  XMM0, XMM2
                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                   //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                   //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                   //  vTemp = _mm_mul_ps( Z, row2 );
                   MULPS   XMM2, [row2]
                   //  vTemp2 = _mm_mul_ps( Y, row1 );
                   MULPS   XMM1, [row1]
                   //  vTemp3 = _mm_mul_ps( X, row0 );
                   MULPS   XMM0, [row0]
                   // vTemp = _mm_add_ps( vTemp, row3 );
                   ADDPS   XMM2, [row3]
                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                   ADDPS   XMM2, XMM1
                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                   ADDPS   XMM2, XMM0

                   //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                   MOVUPS  XMM3,XMM2
                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                   // vTemp = _mm_div_ps( vTemp, W );
                   DIVPS   XMM2, XMM3

                   // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                   MOVUPS  [pOutputVector], XMM2
                   // pOutputVector += OutputStride;
                   MOV     EDX, pOutputVector
                   ADD     EDX, [OutputStride]
                   MOV     pOutputVector,EDX
        end;
        Inc(i);
    end;

    // XM_SFENCE();

    Result := pOutputStream;
end;



function XMVector3TransformNormal(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR; assembler;
asm

           //  vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM0,[V]
           MOVUPS  XMM1,XMM0
           MOVUPS  XMM2,XMM0

           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // vResult = _mm_mul_ps(vResult,M.r[0]);
           MULPS   XMM0, TXMMATRIX([M]).r0
           //  vTemp = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // vTemp = _mm_mul_ps(vTemp,M.r[1]);
           MULPS   XMM1, TXMMATRIX([M]).r1
           // vResult = _mm_add_ps(vResult,vTemp);
           ADDPS   XMM0, XMM1
           // vTemp = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
           // vTemp = _mm_mul_ps(vTemp,M.r[2]);
           MULPS   XMM2, TXMMATRIX([M]).r2
           // vResult = _mm_add_ps(vResult,vTemp);
           ADDPS   XMM0, XMM2
           // return vResult;
           MOVUPS  [result] ,XMM0
end;



function XMVector3TransformNormalStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT3;
var
    pInputVector, pOutputVector: pointer;
    row0, row1, row2: TXMVECTOR;
    i, j, four: size_t;
    s: uint32;
    V, V1, V2, V3, V4: TXMVECTOR;
begin
    pInputVector := pInputStream;
    pOutputVector := pOutputStream;

    row0 := M.r[0];
    row1 := M.r[1];
    row2 := M.r[2];

    i := 0;
    four := VectorCount shr 2;
    if (four > 0) then
    begin
        if (InputStride = sizeof(TXMFLOAT3)) then
        begin
            if (OutputStride = sizeof(TXMFLOAT3)) then
            begin
                if (not (uintptr(pOutputStream) and $F = $F)) then
                begin
                    // Packed input, aligned & packed output
                    s := sizeof(TXMFLOAT3) * 4;
                    for  j := 0 to four - 1 do
                    begin
                        asm
                                   // V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM1, [pInputVector]
                                   MOVUPS  [V1],XMM1
                                   //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                                   MOVUPS  XMM2, [pInputVector+16]
                                   //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                                   MOVUPS  XMM3, [pInputVector+32]
                                   // pInputVector += sizeof(TXMFLOAT3)*4;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [s]
                                   MOV     pInputVector,EDX

                                   // Unpack the 4 vectors (.w components are junk)
                                   //XM3UNPACK3INTO4(V1,L2,L3);
                                   // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                                   MOVUPS  XMM4, XMM2
                                   SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                                   MOVUPS  [V3],XMM4
                                   // V2 := _mm_shuffle_ps(l2,V1,_MM_SHUFFLE(3,3,1,0));
                                   SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0
                                   // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                                   MOVUPS  [V2],XMM2
                                   // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                                   PSRLDQ  XMM3, 4
                                   MOVUPS  [V4],XMM3

                                   // Result 1
                                   //  Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V1]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   //  vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   //  vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   //  vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // V1 = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0
                                   MOVUPS  [V1],XMM2

                                   // Result 2
                                   // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V2]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, [row2] );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // V2 = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0
                                   MOVUPS  [V2],XMM2

                                   // Result 3
                                   // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V3]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, [row2] );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // V3 = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0
                                   MOVUPS  [V3],XMM2

                                   // Result 4
                                   // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V4]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // V4 = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0
                                   MOVUPS  [V4],XMM2

                                   // Pack and store the vectors
                                   // XM3PACK4INTO3(vTemp);
                                   MOVUPS  XMM0, [v2]
                                   SHUFPS  XMM0, [v3], _MM_SHUFFLE_1_0_2_1 // vTemp := _mm_shuffle_ps(V2,V3,_MM_SHUFFLE(1,0,2,1));
                                   MOVUPS  XMM2, [v2]
                                   SHUFPS  XMM2, [v1], _MM_SHUFFLE_2_2_0_0 // V2 := _mm_shuffle_ps(V2,V1,_MM_SHUFFLE(2,2,0,0));
                                   MOVUPS  XMM1, [v1]
                                   SHUFPS  XMM1, XMM2, _MM_SHUFFLE_0_2_1_0 // V1 := _mm_shuffle_ps(V1,V2,_MM_SHUFFLE(0,2,1,0));
                                   MOVUPS  XMM3, [v3]
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_0_0_2_2 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(0,0,2,2));\
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_2_1_2_0 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(2,1,2,0));\


                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), V1 );
                                   MOVUPS  [pOutputVector], XMM1
                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector+16), vTemp );
                                   MOVUPS  [pOutputVector], XMM0
                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector+32), V3 );
                                   MOVUPS  [pOutputVector], XMM3
                                   // pOutputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        Inc(i, 4);
                    end;
                end
                else
                begin
                    // Packed input, unaligned & packed output
                    s := sizeof(TXMFLOAT3) * 4;
                    for  j := 0 to four - 1 do
                    begin
                        asm
                                   //  V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM1, [pInputVector]
                                   MOVUPS  [V1],XMM1
                                   //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                                   MOVUPS  XMM2, [pInputVector+16]
                                   //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                                   MOVUPS  XMM3, [pInputVector+32]
                                   // pInputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [s]
                                   MOV     pInputVector,EDX

                                   // Unpack the 4 vectors (.w components are junk)
                                   // XM3UNPACK3INTO4(V1,L2,L3);
                                   // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                                   MOVUPS  XMM4,XMM2
                                   SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                                   MOVUPS  [V3],XMM4
                                   // V2 := _mm_shuffle_ps(l2,V1,_MM_SHUFFLE(3,3,1,0));
                                   SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0
                                   // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                                   MOVUPS  [V2],XMM2
                                   // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                                   PSRLDQ  XMM3, 4
                                   MOVUPS  [V4],XMM3

                                   // Result 1
                                   //  Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0, [V1]
                                   MOVUPS  XMM1, XMM0
                                   MOVUPS  XMM2, XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   //  vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   //  vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   //  vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // V1 = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0
                                   MOVUPS  [V1],XMM2

                                   // Result 2
                                   // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0, [V2]
                                   MOVUPS  XMM1, XMM0
                                   MOVUPS  XMM2, XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // V2 = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0
                                   MOVUPS  [V2],XMM2

                                   // Result 3
                                   // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0, [V3]
                                   MOVUPS  XMM1, XMM0
                                   MOVUPS  XMM2, XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // V3 = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0
                                   MOVUPS  [V3],XMM2

                                   // Result 4
                                   // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0, [V4]
                                   MOVUPS  XMM1, XMM0
                                   MOVUPS  XMM2, XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, row2 );
                                   MULPS   XMM2, [row2]
                                   // vTemp2 = _mm_mul_ps( Y, row1 );
                                   MULPS   XMM1, [row1]
                                   // vTemp3 = _mm_mul_ps( X, row0 );
                                   MULPS   XMM0, [row0]
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // V4 = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0
                                   MOVUPS  [V4],XMM2

                                   // Pack and store the vectors
                                   // XM3PACK4INTO3(vTemp);
                                   MOVUPS  XMM0, [v2]
                                   SHUFPS  XMM0, [v3], _MM_SHUFFLE_1_0_2_1 // vTemp := _mm_shuffle_ps(V2,V3,_MM_SHUFFLE(1,0,2,1));
                                   MOVUPS  XMM2, [v2]
                                   SHUFPS  XMM2, [v1], _MM_SHUFFLE_2_2_0_0 // V2 := _mm_shuffle_ps(V2,V1,_MM_SHUFFLE(2,2,0,0));
                                   MOVUPS  XMM1, [v1]
                                   SHUFPS  XMM1, XMM2, _MM_SHUFFLE_0_2_1_0 // V1 := _mm_shuffle_ps(V1,V2,_MM_SHUFFLE(0,2,1,0));
                                   MOVUPS  XMM3, [v3]
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_0_0_2_2 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(0,0,2,2));\
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_2_1_2_0 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(2,1,2,0));\

                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), V1 );
                                   MOVUPS  [pOutputVector], XMM1
                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector+16), vTemp );
                                   MOVUPS  [pOutputVector+16], XMM0
                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector+32), V3 );
                                   MOVUPS  [pOutputVector+32], XMM3
                                   //  pOutputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        Inc(i, 4);
                    end;
                end;
            end
            else
            begin
                // Packed input, unpacked output
                s := sizeof(TXMFLOAT3) * 4;
                for j := 0 to four - 1 do
                begin
                    asm
                               //  V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                               MOVUPS  XMM1, [pInputVector]
                               MOVUPS  [V1],XMM1
                               //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                               MOVUPS  XMM2, [pInputVector+16]
                               //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                               MOVUPS  XMM3, [pInputVector+32]
                               // pInputVector += sizeof(XMFLOAT3)*4;
                               MOV     EDX, pInputVector
                               ADD     EDX, [s]
                               MOV     pInputVector,EDX

                               // Unpack the 4 vectors (.w components are junk)
                               // XM3UNPACK3INTO4(V1,L2,L3);
                               // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                               MOVUPS  XMM4,XMM2
                               SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                               MOVUPS  [V3],XMM4
                               // V2 := _mm_shuffle_ps(l2,l1,_MM_SHUFFLE(3,3,1,0));
                               SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0
                               // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                               MOVUPS  [V2],XMM2
                               // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                               PSRLDQ  XMM3, 4
                               MOVUPS  [V4],XMM3

                               // Result 1
                               //  Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0, [V1]
                               MOVUPS  XMM1, XMM0
                               MOVUPS  XMM2, XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               //  vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               //  vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               //  vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 2
                               // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0, [V2]
                               MOVUPS  XMM1, XMM0
                               MOVUPS  XMM2, XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 3
                               // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0, [V3]
                               MOVUPS  XMM1, XMM0
                               MOVUPS  XMM2, XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 4
                               // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0, [V4]
                               MOVUPS  XMM1, XMM0
                               MOVUPS  XMM2, XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, row2 );
                               MULPS   XMM2, [row2]
                               // vTemp2 = _mm_mul_ps( Y, row1 );
                               MULPS   XMM1, [row1]
                               // vTemp3 = _mm_mul_ps( X, row0 );
                               MULPS   XMM0, [row0]
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX
                    end;
                    Inc(i, 4);
                end;
            end;
        end;
    end;

    // for (; i < VectorCount; i++)
    while i < VectorCount do
    begin
        asm
                   //  V = XMLoadFloat3(reinterpret_cast<const XMFLOAT3*>(pInputVector));
                   // pInputVector += InputStride;
                   MOV     EDX, pInputVector
                   ADD     EDX, [InputStride]
                   MOV     pInputVector,EDX

                   //  Z = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                   MOVUPS  XMM0, [V]
                   MOVUPS  XMM1, XMM0
                   MOVUPS  XMM2, XMM0

                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                   //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                   //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                   //  vTemp = _mm_mul_ps( Z, row2 );
                   MULPS   XMM2, [row2]
                   //  vTemp2 = _mm_mul_ps( Y, row1 );
                   MULPS   XMM1, [row1]
                   //  vTemp3 = _mm_mul_ps( X, row0 );
                   MULPS   XMM0, [row0]
                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                   ADDPS   XMM2, XMM1
                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                   ADDPS   XMM2, XMM0

                   // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                   MOVUPS  [pOutputVector], XMM2
                   // pOutputVector += OutputStride;
                   MOV     EDX, pOutputVector
                   ADD     EDX, [OutputStride]
                   MOV     pOutputVector,EDX
        end;
        Inc(i);
    end;

    // XM_SFENCE();

    Result := pOutputStream;
end;



function XMVector3ProjectStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref ViewportX: single; constref ViewportY: single;
    constref ViewportWidth: single; constref ViewportHeight: single; constref ViewportMinZ: single; constref ViewportMaxZ: single;
    constref Projection: TXMMATRIX; constref View: TXMMATRIX; constref World: TXMMATRIX): PXMFLOAT3;
var
    HalfViewportWidth: single;
    HalfViewportHeight: single;
    Scale, _Offset: TXMVECTOR;
    Transform: TXMMATRIX;
    pInputVector, pOutputVector: pointer;
    i, j, four: size_t;
    s: uint32;
    V, V1, V2, V3, V4: TXMVECTOR;
begin
    HalfViewportWidth := ViewportWidth * 0.5;
    HalfViewportHeight := ViewportHeight * 0.5;

    Scale := XMVectorSet(HalfViewportWidth, -HalfViewportHeight, ViewportMaxZ - ViewportMinZ, 1.0);
    _Offset := XMVectorSet(ViewportX + HalfViewportWidth, ViewportY + HalfViewportHeight, ViewportMinZ, 0.0);

    Transform := XMMatrixMultiply(World, View);
    Transform := XMMatrixMultiply(Transform, Projection);

    pInputVector := pInputStream;
    pOutputVector := pOutputStream;

    i := 0;
    four := VectorCount shr 2;
    if (four > 0) then
    begin
        if (InputStride = sizeof(TXMFLOAT3)) then
        begin
            if (OutputStride = sizeof(TXMFLOAT3)) then
            begin
                if (not (uintptr(pOutputStream) and $F = $F)) then
                begin
                    // Packed input, aligned & packed output
                    s := sizeof(TXMFLOAT3) * 4;
                    for j := 0 to four - 1 do
                    begin
                        asm
                                   // V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM1, [pInputVector]
                                   MOVUPS  [V1], XMM1
                                   // L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                                   MOVUPS  XMM2, [pInputVector+16]
                                   // L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                                   MOVUPS  XMM3, [pInputVector+32]
                                   //pInputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [s]
                                   MOV     pInputVector,EDX

                                   // Unpack the 4 vectors (.w components are junk)
                                   // XM3UNPACK3INTO4(V1,L2,L3);
                                   // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                                   MOVUPS  XMM4,XMM2
                                   SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                                   MOVUPS  [V3], XMM4
                                   // V2 := _mm_shuffle_ps(l2,l1,_MM_SHUFFLE(3,3,1,0));
                                   SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0
                                   // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                                   MOVUPS  [V2], XMM2
                                   // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                                   PSRLDQ  XMM3, 4
                                   MOVUPS  [V4], XMM3

                                   // Result 1
                                   // Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V1]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   //vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   //vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   //vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   //vTemp = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3

                                   //vTemp = _mm_mul_ps( vTemp, Scale );
                                   MULPS   XMM2, [Scale]
                                   //V1 = _mm_add_ps( vTemp, Offset );
                                   ADDPS   XMM2, [_Offset]
                                   MOVUPS  [V1], XMM2

                                   // Result 2
                                   //Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V2]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   //Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   //X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   //vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   //vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   //vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   //vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   //vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   //vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   //W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   //vTemp = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3

                                   //vTemp = _mm_mul_ps( vTemp, Scale );
                                   MULPS   XMM2, [Scale]
                                   //V2 = _mm_add_ps( vTemp, Offset );
                                   ADDPS   XMM2, [_Offset]
                                   MOVUPS  [V2], XMM2

                                   // Result 3
                                   //Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V3]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   //Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   //X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // vTemp = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3

                                   // vTemp = _mm_mul_ps( vTemp, Scale );
                                   MULPS   XMM2, [Scale]
                                   // V3 = _mm_add_ps( vTemp, Offset );
                                   ADDPS   XMM2, [_Offset]
                                   MOVUPS  [V3], XMM2

                                   // Result 4
                                   // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V4]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // vTemp = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3

                                   // vTemp = _mm_mul_ps( vTemp, Scale );
                                   MULPS   XMM2, [Scale]
                                   // V4 = _mm_add_ps( vTemp, Offset );
                                   ADDPS   XMM2, [_Offset]
                                   MOVUPS  [V4], XMM2

                                   // Pack and store the vectors
                                   // XM3PACK4INTO3(vTemp);
                                   MOVUPS  XMM0, [v2]
                                   SHUFPS  XMM0, [v3], _MM_SHUFFLE_1_0_2_1 // vTemp := _mm_shuffle_ps(V2,V3,_MM_SHUFFLE(1,0,2,1));
                                   MOVUPS  XMM2, [v2]
                                   SHUFPS  XMM2, [v1], _MM_SHUFFLE_2_2_0_0 // V2 := _mm_shuffle_ps(V2,V1,_MM_SHUFFLE(2,2,0,0));
                                   MOVUPS  XMM1, [v1]
                                   SHUFPS  XMM1, XMM2, _MM_SHUFFLE_0_2_1_0 // V1 := _mm_shuffle_ps(V1,V2,_MM_SHUFFLE(0,2,1,0));
                                   MOVUPS  XMM3, [v3]
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_0_0_2_2 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(0,0,2,2));\
                                   SHUFPS  XMM4, [v4], _MM_SHUFFLE_2_1_2_0 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(2,1,2,0));\

                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), V1 );
                                   MOVUPS  [pOutputVector], XMM1
                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector+16), vTemp );
                                   MOVUPS  [pOutputVector+16], XMM0
                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector+32), V3 );
                                   MOVUPS  [pOutputVector+32], XMM3
                                   // pOutputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        Inc(i, 4);
                    end;
                end
                else
                begin
                    // Packed input, unaligned & packed output
                    s := sizeof(TXMFLOAT3) * 4;
                    for j := 0 to four - 1 do
                    begin
                        asm
                                   //  V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM1, [pInputVector]
                                   MOVUPS  [V1],XMM1
                                   //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                                   MOVUPS  XMM2, [pInputVector+16]
                                   //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                                   MOVUPS  XMM3, [pInputVector+32]
                                   // pInputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [s]
                                   MOV     pInputVector,EDX

                                   // Unpack the 4 vectors (.w components are junk)
                                   // XM3UNPACK3INTO4(V1,L2,L3);
                                   // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                                   MOVUPS  XMM4,XMM2
                                   SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                                   MOVUPS  [V3],XMM4

                                   // V2 := _mm_shuffle_ps(l2,l1,_MM_SHUFFLE(3,3,1,0));
                                   SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0

                                   // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                                   MOVUPS  [V2],XMM2

                                   // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                                   PSRLDQ  XMM3, 4
                                   MOVUPS  [V4],XMM3

                                   // Result 1
                                   //  Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V1]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   //  vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   //  vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   //  vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // vTemp = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3

                                   // vTemp = _mm_mul_ps( vTemp, Scale );
                                   MULPS   XMM2, [Scale]

                                   // V1 = _mm_add_ps( vTemp, Offset );
                                   ADDPS   XMM2, [_Offset]
                                   MOVUPS  [V1],XMM2

                                   // Result 2
                                   // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V2]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // vTemp = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3

                                   // vTemp = _mm_mul_ps( vTemp, Scale );
                                   MULPS   XMM2, [Scale]
                                   // V2 = _mm_add_ps( vTemp, Offset );
                                   ADDPS   XMM2, [_Offset]
                                   MOVUPS  [V2],XMM2

                                   // Result 3
                                   // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V3]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // vTemp = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3

                                   // vTemp = _mm_mul_ps( vTemp, Scale );
                                   MULPS   XMM2, [Scale]
                                   // V3 = _mm_add_ps( vTemp, Offset );
                                   ADDPS   XMM2, [_Offset]
                                   MOVUPS  [V3],XMM2

                                   // Result 4
                                   // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,[V4]
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // vTemp = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3

                                   // vTemp = _mm_mul_ps( vTemp, Scale );
                                   MULPS   XMM2, [Scale]
                                   // V4 = _mm_add_ps( vTemp, Offset );
                                   ADDPS   XMM2, [_Offset]
                                   MOVUPS  [V4],XMM2

                                   // Pack and store the vectors
                                   // XM3PACK4INTO3(vTemp);
                                   MOVUPS  XMM0, [v2]
                                   SHUFPS  XMM0, [v3], _MM_SHUFFLE_1_0_2_1 // vTemp := _mm_shuffle_ps(V2,V3,_MM_SHUFFLE(1,0,2,1));
                                   MOVUPS  XMM2, [v2]
                                   SHUFPS  XMM2, [v1], _MM_SHUFFLE_2_2_0_0 // V2 := _mm_shuffle_ps(V2,V1,_MM_SHUFFLE(2,2,0,0));
                                   MOVUPS  XMM1, [v1]
                                   SHUFPS  XMM1, XMM2, _MM_SHUFFLE_0_2_1_0 // V1 := _mm_shuffle_ps(V1,V2,_MM_SHUFFLE(0,2,1,0));
                                   MOVUPS  XMM3, [v3]
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_0_0_2_2 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(0,0,2,2));\
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_2_1_2_0 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(2,1,2,0));\
                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), V1 );
                                   MOVUPS  [pOutputVector], XMM1
                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector+16), vTemp );
                                   MOVUPS  [pOutputVector+16], XMM0
                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector+32), V3 );
                                   MOVUPS  [pOutputVector+32], XMM3
                                   // pOutputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        Inc(i, 4);
                    end;
                end;
            end
            else
            begin
                // Packed input, unpacked output
                s := sizeof(TXMFLOAT3) * 4;
                for j := 0 to four - 1 do
                begin
                    asm
                               //  V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                               MOVUPS  XMM1, [pInputVector]
                               MOVUPS  [V1],XMM1
                               //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                               MOVUPS  XMM2, [pInputVector+16]
                               //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                               MOVUPS  XMM3, [pInputVector+32]
                               // pInputVector += sizeof(XMFLOAT3)*4;
                               MOV     EDX, pInputVector
                               ADD     EDX, [s]
                               MOV     pInputVector,EDX

                               // Unpack the 4 vectors (.w components are junk)
                               // XM3UNPACK3INTO4(V1,L2,L3);
                               // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                               MOVUPS  XMM4,XMM2
                               SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                               MOVUPS  [V3],XMM4
                               // V2 := _mm_shuffle_ps(l2,l1,_MM_SHUFFLE(3,3,1,0));
                               SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0
                               // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                               MOVUPS  [V2],XMM2
                               // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                               PSRLDQ  XMM3, 4
                               MOVUPS  [V4],XMM3

                               // Result 1
                               //  Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,[V1]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               //  vTemp = _mm_mul_ps( Z, Transform.r[2] );
                               MULPS   XMM2, TXMMATRIX([Transform]).r2
                               //  vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                               MULPS   XMM1, TXMMATRIX([Transform]).r1
                               //  vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                               MULPS   XMM0, TXMMATRIX([Transform]).r0
                               // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                               ADDPS   XMM2, TXMMATRIX([Transform]).r3
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3,XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3

                               // vTemp = _mm_mul_ps( vTemp, Scale );
                               MULPS   XMM2, [Scale]
                               // vTemp = _mm_add_ps( vTemp, Offset );
                               ADDPS   XMM2, [_Offset]

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               MOVUPS  [pOutputVector],XMM2
                               //  pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 2
                               // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,[V2]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                               MULPS   XMM2, TXMMATRIX([Transform]).r2
                               // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                               MULPS   XMM1, TXMMATRIX([Transform]).r1
                               // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                               MULPS   XMM0, TXMMATRIX([Transform]).r0
                               // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                               ADDPS   XMM2, TXMMATRIX([Transform]).r3
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3,XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3

                               // vTemp = _mm_mul_ps( vTemp, Scale );
                               MULPS   XMM2, [Scale]
                               // vTemp = _mm_add_ps( vTemp, Offset );
                               ADDPS   XMM2, [_Offset]

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               MOVUPS  [pOutputVector],XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 3
                               // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,[V3]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
                               // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                               MULPS   XMM2, TXMMATRIX([Transform]).r2
                               // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                               MULPS   XMM1, TXMMATRIX([Transform]).r1
                               // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                               MULPS   XMM0, TXMMATRIX([Transform]).r0
                               // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                               ADDPS   XMM2, TXMMATRIX([Transform]).r3
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3,XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3

                               // vTemp = _mm_mul_ps( vTemp, Scale );
                               MULPS   XMM2, [Scale]
                               // vTemp = _mm_add_ps( vTemp, Offset );
                               ADDPS   XMM2, [_Offset]

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               MOVUPS  [pOutputVector],XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 4
                               //  Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,[V4]
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                               MULPS   XMM2, TXMMATRIX([Transform]).r2
                               // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                               MULPS   XMM1, TXMMATRIX([Transform]).r1
                               // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                               MULPS   XMM0, TXMMATRIX([Transform]).r0
                               // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                               ADDPS   XMM2, TXMMATRIX([Transform]).r3
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3,XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3

                               // vTemp = _mm_mul_ps( vTemp, Scale );
                               MULPS   XMM2, [Scale]
                               // vTemp = _mm_add_ps( vTemp, Offset );
                               ADDPS   XMM2, [_Offset]

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               MOVUPS  [pOutputVector],XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX
                    end;
                    Inc(i, 4);
                end;
            end;
        end;
    end;

    // for (; i < VectorCount; i++)
    while i < VectorCount do
    begin
        V := XMLoadFloat3(pInputVector);
        asm
                   // pInputVector += InputStride;
                   MOV     EDX, pInputVector
                   ADD     EDX, [InputStride]
                   MOV     pInputVector,EDX

                   //  Z = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                   MOVUPS  XMM0,[V]
                   MOVUPS  XMM1,XMM0
                   MOVUPS  XMM2,XMM0
                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                   //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                   //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                   //  vTemp = _mm_mul_ps( Z, Transform.r[2] );
                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                   //  vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                   //  vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                   ADDPS   XMM2, XMM1
                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                   ADDPS   XMM2, XMM0

                   //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                   MOVUPS  XMM3,XMM2
                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                   // vTemp = _mm_div_ps( vTemp, W );
                   DIVPS   XMM2, XMM3

                   // vTemp = _mm_mul_ps( vTemp, Scale );
                   MULPS   XMM2, [Scale]
                   // vTemp = _mm_add_ps( vTemp, Offset );
                   ADDPS   XMM2, [_Offset]

                   // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                   MOVUPS  [pOutputVector],XMM2
                   // pOutputVector += OutputStride;
                   MOV     EDX, pOutputVector
                   ADD     EDX, [OutputStride]
                   MOV     pOutputVector,EDX
        end;
        Inc(i);
    end;

    // XM_SFENCE();

    Result := pOutputStream;
end;



function XMVector3UnprojectStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref ViewportX: single; constref ViewportY: single;
    constref ViewportWidth: single; constref ViewportHeight: single; constref ViewportMinZ: single; constref ViewportMaxZ: single;
    constref Projection: TXMMATRIX; constref View: TXMMATRIX; constref World: TXMMATRIX): PXMFLOAT3;
const
    D: TXMVECTORF32 = (f: (-1.0, 1.0, 0.0, 0.0));
var
    Scale, _Offset: TXMVECTOR;
    Transform: TXMMATRIX;
    pInputVector, pOutputVector: pointer;
    s: uint32;
    i, j, four: size_t;
    V, V1, V2, V3, V4: TXMVECTOR;
begin
    Scale := XMVectorSet(ViewportWidth * 0.5, -ViewportHeight * 0.5, ViewportMaxZ - ViewportMinZ, 1.0);
    Scale := XMVectorReciprocal(Scale);

    _Offset := XMVectorSet(-ViewportX, -ViewportY, -ViewportMinZ, 0.0);
    asm
               // Offset = _mm_mul_ps(Scale, Offset);
               MOVUPS  XMM0,[Scale]
               MULPS   XMM0, [_Offset]
               // Offset = _mm_add_ps(Offset, D);
               ADDPS   XMM0, [D]
               MOVUPS  [_Offset],XMM0
    end;

    Transform := XMMatrixMultiply(World, View);
    Transform := XMMatrixMultiply(Transform, Projection);
    Transform := XMMatrixInverse(TXMVECTOR(nil^), Transform);

    pInputVector := pInputStream;
    pOutputVector := pOutputStream;

    i := 0;
    four := VectorCount shr 2;
    if (four > 0) then
    begin
        if (InputStride = sizeof(TXMFLOAT3)) then
        begin
            if (OutputStride = sizeof(TXMFLOAT3)) then
            begin
                if (not (uintptr(pOutputStream) and $F = $F)) then
                begin
                    // Packed input, aligned & packed output
                    s := sizeof(TXMFLOAT3) * 4;
                    for j := 0 to four - 1 do
                    begin
                        asm
                                   // V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM1, [pInputVector]

                                   //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                                   MOVUPS  XMM2, [pInputVector+16]
                                   //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                                   MOVUPS  XMM3, [pInputVector+32]
                                   // pInputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [s]
                                   MOV     pInputVector,EDX

                                   // Unpack the 4 vectors (.w components are junk)
                                   // XM3UNPACK3INTO4(V1,L2,L3);
                                   // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                                   MOVUPS  XMM4,XMM2
                                   SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                                   MOVUPS  [V3],XMM4
                                   // V2 := _mm_shuffle_ps(l2,V1,_MM_SHUFFLE(3,3,1,0));
                                   SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0
                                   // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                                   MOVUPS  [V2],XMM2
                                   // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                                   PSRLDQ  XMM3, 4
                                   MOVUPS  [V4],XMM3

                                   // Result 1
                                   // V1 = _mm_mul_ps( V1, Scale );
                                   MULPS   XMM1, [Scale]
                                   // V1 = _mm_add_ps( V1, Offset );
                                   ADDPS   XMM1, [_Offset]
                                   MOVUPS  [V1],XMM1

                                   // Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,XMM1
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   //  vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   //  vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   //  vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // V1 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V1],XMM2

                                   // Result 2
                                   // V2 = _mm_mul_ps( V2, Scale );
                                   MOVUPS  XMM0,[V2]
                                   MULPS   XMM0, [Scale]
                                   // V2 = _mm_add_ps( V2, Offset );
                                   ADDPS   XMM0, [_Offset]

                                   // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // V2 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V2],XMM2

                                   // Result 3
                                   // V3 = _mm_mul_ps( V3, Scale );
                                   MOVUPS  XMM0, [V3]
                                   MULPS   XMM0, [Scale]
                                   // V3 = _mm_add_ps( V3, Offset );
                                   ADDPS   XMM0, [_Offset]

                                   // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // V3 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V3],XMM2

                                   // Result 4
                                   // V4 = _mm_mul_ps( V4, Scale );
                                   MOVUPS  XMM0,[V4]
                                   MULPS   XMM0, [Scale]
                                   // V4 = _mm_add_ps( V4, Offset );
                                   ADDPS   XMM0, [_Offset]

                                   // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // V4 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V4],XMM2

                                   // Pack and store the vectors
                                   // XM3PACK4INTO3(vTemp);
                                   MOVUPS  XMM0, [v2]
                                   SHUFPS  XMM0, [v3], _MM_SHUFFLE_1_0_2_1 // vTemp := _mm_shuffle_ps(V2,V3,_MM_SHUFFLE(1,0,2,1));
                                   MOVUPS  XMM2, [v2]
                                   SHUFPS  XMM2, [v1], _MM_SHUFFLE_2_2_0_0 // V2 := _mm_shuffle_ps(V2,V1,_MM_SHUFFLE(2,2,0,0));
                                   MOVUPS  XMM1, [v1]
                                   SHUFPS  XMM1, XMM2, _MM_SHUFFLE_0_2_1_0 // V1 := _mm_shuffle_ps(V1,V2,_MM_SHUFFLE(0,2,1,0));
                                   MOVUPS  XMM3, [v3]
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_0_0_2_2 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(0,0,2,2));\
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_2_1_2_0 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(2,1,2,0));\
                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), V1 );
                                   MOVUPS  [pOutputVector],XMM1
                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector+16), vTemp );
                                   MOVUPS  [pOutputVector+16],XMM0
                                   // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector+32), V3 );
                                   MOVUPS  [pOutputVector+32],XMM3
                                   // pOutputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        Inc(i, 4);
                    end;
                end
                else
                begin
                    // Packed input, unaligned & packed output
                    s := sizeof(TXMFLOAT3) * 4;
                    for j := 0 to four - 1 do
                    begin
                        asm
                                   //  V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                                   MOVUPS  XMM1, [pInputVector]

                                   //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                                   MOVUPS  XMM2, [pInputVector+16]
                                   //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                                   MOVUPS  XMM3, [pInputVector+32]
                                   // pInputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pInputVector
                                   ADD     EDX, [s]
                                   MOV     pInputVector,EDX

                                   // Unpack the 4 vectors (.w components are junk)
                                   // XM3UNPACK3INTO4(V1,L2,L3);
                                   // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                                   MOVUPS  XMM4,XMM2
                                   SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                                   MOVUPS  [V3],XMM4
                                   // V2 := _mm_shuffle_ps(l2,l1,_MM_SHUFFLE(3,3,1,0));
                                   SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0
                                   // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                                   MOVUPS  [V2],XMM2
                                   // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                                   PSRLDQ  XMM3, 4
                                   MOVUPS  [V4],XMM3
                                   // Result 1
                                   // V1 = _mm_mul_ps( V1, Scale );
                                   MULPS   XMM1, [Scale]
                                   // V1 = _mm_add_ps( V1, Offset );
                                   ADDPS   XMM1, [_Offset]

                                   // Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM0,XMM1
                                   MOVUPS  XMM2,XMM1
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   //  vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   //  vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   //  vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // V1 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V1],XMM2

                                   // Result 2
                                   // V2 = _mm_mul_ps( V2, Scale );
                                   MOVUPS  XMM0,[V2]
                                   MULPS   XMM0, [Scale]
                                   // V2 = _mm_add_ps( V2, Offset );
                                   ADDPS   XMM0, [_Offset]
                                   // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // V2 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V2],XMM2

                                   // Result 3
                                   // V3 = _mm_mul_ps( V3, Scale );
                                   MOVUPS  XMM0,[V3]
                                   MULPS   XMM0, [Scale]
                                   // V3 = _mm_add_ps( V3, Offset );
                                   ADDPS   XMM0, [_Offset]


                                   // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // V3 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V3],XMM2

                                   // Result 4
                                   // V4 = _mm_mul_ps( V4, Scale );
                                   MOVUPS  XMM0,[V4]
                                   MULPS   XMM0, [Scale]
                                   // V4 = _mm_add_ps( V4, Offset );
                                   ADDPS   XMM0, [_Offset]

                                   // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                                   MOVUPS  XMM1,XMM0
                                   MOVUPS  XMM2,XMM0
                                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                                   // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                                   // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                                   // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                                   // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                                   // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                                   ADDPS   XMM2, XMM1
                                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                                   ADDPS   XMM2, XMM0

                                   // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                                   MOVUPS  XMM3,XMM2
                                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                                   // V4 = _mm_div_ps( vTemp, W );
                                   DIVPS   XMM2, XMM3
                                   MOVUPS  [V4],XMM2

                                   // Pack and store the vectors
                                   // XM3PACK4INTO3(vTemp);
                                   MOVUPS  XMM0, [v2]
                                   SHUFPS  XMM0, [v3], _MM_SHUFFLE_1_0_2_1 // vTemp := _mm_shuffle_ps(V2,V3,_MM_SHUFFLE(1,0,2,1));
                                   MOVUPS  XMM2, [v2]
                                   SHUFPS  XMM2, [v1], _MM_SHUFFLE_2_2_0_0 // V2 := _mm_shuffle_ps(V2,V1,_MM_SHUFFLE(2,2,0,0));
                                   MOVUPS  XMM1, [v1]
                                   SHUFPS  XMM1, XMM2, _MM_SHUFFLE_0_2_1_0 // V1 := _mm_shuffle_ps(V1,V2,_MM_SHUFFLE(0,2,1,0));
                                   MOVUPS  XMM3, [v3]
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_0_0_2_2 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(0,0,2,2));\
                                   SHUFPS  XMM3, [v4], _MM_SHUFFLE_2_1_2_0 // V3 := _mm_shuffle_ps(V3,V4,_MM_SHUFFLE(2,1,2,0));\
                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), V1 );
                                   MOVUPS  [pOutputVector], XMM1
                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector+16), vTemp );
                                   MOVUPS  [pOutputVector+16], XMM0
                                   // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector+32), V3 );
                                   MOVUPS  [pOutputVector+32], XMM3
                                   // pOutputVector += sizeof(XMFLOAT3)*4;
                                   MOV     EDX, pOutputVector
                                   ADD     EDX, [s]
                                   MOV     pOutputVector,EDX
                        end;
                        Inc(i, 4);
                    end;
                end;
            end
            else
            begin
                // Packed input, unpacked output
                s := sizeof(TXMFLOAT3) * 4;
                for j := 0 to four - 1 do
                begin
                    asm
                               //  V1 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                               MOVUPS  XMM1, [pInputVector]
                               //  L2 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+16) );
                               MOVUPS  XMM2, [pInputVector+16]
                               //  L3 = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector+32) );
                               MOVUPS  XMM3, [pInputVector+32]
                               // pInputVector += sizeof(XMFLOAT3)*4;
                               MOV     EDX, pInputVector
                               ADD     EDX, [s]
                               MOV     pInputVector,EDX

                               // Unpack the 4 vectors (.w components are junk)
                               // XM3UNPACK3INTO4(V1,L2,L3);
                               // V3 := _mm_shuffle_ps(l2,l3,_MM_SHUFFLE(0,0,3,2));
                               MOVUPS  XMM4,XMM2
                               SHUFPS  XMM4, XMM3, _MM_SHUFFLE_0_0_3_2
                               MOVUPS  [V3],XMM4
                               // V2 := _mm_shuffle_ps(l2,V1,_MM_SHUFFLE(3,3,1,0));
                               SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_3_1_0
                               // V2 := XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,1,0,2));
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_1_0_2
                               MOVUPS  [V2],XMM2
                               // V4 := _mm_castsi128_ps( _mm_srli_si128(_mm_castps_si128(L3),32/8) );
                               PSRLDQ  XMM3, 4
                               MOVUPS  [V4],XMM3

                               // Result 1
                               // V1 = _mm_mul_ps( V1, Scale );
                               MULPS   XMM1, [Scale]
                               // V1 = _mm_add_ps( V1, Offset );
                               ADDPS   XMM1, [_Offset]

                               //  Z = XM_PERMUTE_PS( V1, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM0,XMM1
                               MOVUPS  XMM2,XMM1
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               //  Y = XM_PERMUTE_PS( V1, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               //  X = XM_PERMUTE_PS( V1, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               //  vTemp = _mm_mul_ps( Z, Transform.r[2] );
                               MULPS   XMM2, TXMMATRIX([Transform]).r2
                               //  vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                               MULPS   XMM1, TXMMATRIX([Transform]).r1
                               //  vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                               MULPS   XMM0, TXMMATRIX([Transform]).r0
                               // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                               ADDPS   XMM2, TXMMATRIX([Transform]).r3
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3, XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               MOVUPS  [pOutputVector], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 2
                               // V2 = _mm_mul_ps( V2, Scale );
                               MOVUPS  XMM0,[V2]
                               MULPS   XMM0, [Scale]
                               // V2 = _mm_add_ps( V2, Offset );
                               ADDPS   XMM0, [_Offset]

                               // Z = XM_PERMUTE_PS( V2, _MM_SHUFFLE(2, 2, 2, 2) );

                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V2, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V2, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                               MULPS   XMM2, TXMMATRIX([Transform]).r2
                               // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                               MULPS   XMM1, TXMMATRIX([Transform]).r1
                               // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                               MULPS   XMM0, TXMMATRIX([Transform]).r0
                               // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                               ADDPS   XMM2, TXMMATRIX([Transform]).r3
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3, XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               MOVUPS  [pOutputVector], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 3
                               // V3 = _mm_mul_ps( V3, Scale );
                               MOVUPS  XMM0,[V3]
                               MULPS   XMM0, [Scale]
                               // V3 = _mm_add_ps( V3, Offset );
                               ADDPS   XMM0, [_Offset]

                               // Z = XM_PERMUTE_PS( V3, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V3, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V3, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                               MULPS   XMM2, TXMMATRIX([Transform]).r2
                               // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                               MULPS   XMM1, TXMMATRIX([Transform]).r1
                               // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                               MULPS   XMM0, TXMMATRIX([Transform]).r0
                               // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                               ADDPS   XMM2, TXMMATRIX([Transform]).r3
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3, XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               MOVUPS  [pOutputVector], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX

                               // Result 4
                               // V4 = _mm_mul_ps( V4, Scale );
                               MOVUPS  XMM0,[V4]
                               MULPS   XMM0, [Scale]
                               // V4 = _mm_add_ps( V4, Offset );
                               ADDPS   XMM0, [_Offset]

                               // Z = XM_PERMUTE_PS( V4, _MM_SHUFFLE(2, 2, 2, 2) );
                               MOVUPS  XMM1,XMM0
                               MOVUPS  XMM2,XMM0
                               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                               // Y = XM_PERMUTE_PS( V4, _MM_SHUFFLE(1, 1, 1, 1) );
                               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                               // X = XM_PERMUTE_PS( V4, _MM_SHUFFLE(0, 0, 0, 0) );
                               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0

                               // vTemp = _mm_mul_ps( Z, Transform.r[2] );
                               MULPS   XMM2, TXMMATRIX([Transform]).r2
                               // vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                               MULPS   XMM1, TXMMATRIX([Transform]).r1
                               // vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                               MULPS   XMM0, TXMMATRIX([Transform]).r0
                               // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                               ADDPS   XMM2, TXMMATRIX([Transform]).r3
                               // vTemp = _mm_add_ps( vTemp, vTemp2 );
                               ADDPS   XMM2, XMM1
                               // vTemp = _mm_add_ps( vTemp, vTemp3 );
                               ADDPS   XMM2, XMM0

                               // W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                               MOVUPS  XMM3, XMM2
                               SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                               // vTemp = _mm_div_ps( vTemp, W );
                               DIVPS   XMM2, XMM3

                               // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                               MOVUPS  [pOutputVector], XMM2
                               // pOutputVector += OutputStride;
                               MOV     EDX, pOutputVector
                               ADD     EDX, [OutputStride]
                               MOV     pOutputVector,EDX
                    end;
                    Inc(i, 4);
                end;
            end;
        end;
    end;

    // for (; i < VectorCount; i++)
    while i < VectorCount do
    begin
        V := XMLoadFloat3(pInputVector);
        asm
                   // pInputVector += InputStride;
                   MOV     EDX, pInputVector
                   ADD     EDX, [InputStride]
                   MOV     pInputVector,EDX

                   // V = _mm_mul_ps( V, Scale );
                   MOVUPS  XMM0, [V]
                   MULPS   XMM0, [Scale]
                   // V = _mm_add_ps( V, Offset );
                   ADDPS   XMM0, [_Offset]

                   //  Z = XM_PERMUTE_PS( V, _MM_SHUFFLE(2, 2, 2, 2) );
                   MOVUPS  XMM1,XMM0
                   MOVUPS  XMM2,XMM0
                   SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                   //  Y = XM_PERMUTE_PS( V, _MM_SHUFFLE(1, 1, 1, 1) );
                   SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                   //  X = XM_PERMUTE_PS( V, _MM_SHUFFLE(0, 0, 0, 0) );
                   SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0


                   //  vTemp = _mm_mul_ps( Z, Transform.r[2] );
                   MULPS   XMM2, TXMMATRIX([Transform]).r2
                   //  vTemp2 = _mm_mul_ps( Y, Transform.r[1] );
                   MULPS   XMM1, TXMMATRIX([Transform]).r1
                   //  vTemp3 = _mm_mul_ps( X, Transform.r[0] );
                   MULPS   XMM0, TXMMATRIX([Transform]).r0
                   // vTemp = _mm_add_ps( vTemp, Transform.r[3] );
                   ADDPS   XMM2, TXMMATRIX([Transform]).r3
                   // vTemp = _mm_add_ps( vTemp, vTemp2 );
                   ADDPS   XMM2, XMM1
                   // vTemp = _mm_add_ps( vTemp, vTemp3 );
                   ADDPS   XMM2, XMM0

                   //  W = XM_PERMUTE_PS( vTemp, _MM_SHUFFLE(3, 3, 3, 3) );
                   MOVUPS  XMM3,XMM2
                   SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
                   // vTemp = _mm_div_ps( vTemp, W );
                   DIVPS   XMM2, XMM3

                   // XMStoreFloat3(reinterpret_cast<XMFLOAT3*>(pOutputVector), vTemp);
                   MOVUPS  [pOutputVector], XMM2
                   // pOutputVector += OutputStride;
                   MOV     EDX, pOutputVector
                   ADD     EDX, [OutputStride]
                   MOV     pOutputVector,EDX
        end;
        Inc(i);
    end;

    // XM_SFENCE();

    Result := pOutputStream;
end;




{***************************************************************************
 *
 * 4D vector operations
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------


{$IF DEFINED(_XM_SSE_INTRINSICS_)}

function XMVector4Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;  {nostackframe;} register; assembler;
asm
           MOVUPS  XMM0,[v1]
           CMPPS   XMM0,[v2], 0
           MOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ELSE}


function XMVector4Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := XMComparisonAllTrue(XMVector4EqualR(V1, V2));
end;

{$ENDIF}



function XMVector4EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32; {nostackframe; register;} assembler;
asm
           MOVUPS  XMM0,[v1]
           CMPPS   XMM0,[v2], 0
           MOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           MOV     EDX,XM_CRMASK_CR6TRUE
           JE      @Finished
           CMP     EAX, 0
           JE      @Less
           MOV     EDX,0
           JMP     @finished
           @Less:
           MOV     EDX,XM_CRMASK_CR6FALSE
           @Finished:
           MOV     [result],EDX
end;


{$IF DEFINED(_XM_SSE_INTRINSICS_)}

function XMVector4EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           MOVUPS  XMM0,[v1]
           PCMPEQD  XMM0, [v2]
           MOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ELSE}


function XMVector4EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := XMComparisonAllTrue(XMVector4EqualIntR(V1, V2));
end;

{$ENDIF}


{$IF DEFINED(_XM_AVX_INTRINSICS_)}

function XMVector4EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32; assembler;
asm
           VMOVUPS XMM1,[v1]
           VMOVUPS XMM2,[v2]
           VPCMPEQD  XMM0, XMM1, XMM2
           VMOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           MOV     EDX,XM_CRMASK_CR6TRUE
           JE      @Finished
           CMP     EAX, 0
           JE      @Less
           MOV     EDX,0
           JMP     @finished
           @Less:
           MOV     EDX,XM_CRMASK_CR6FALSE
           @Finished:
           MOV     [result],EDX
end;
{$ELSE}


function XMVector4EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32; assembler;
asm
           // vTemp = _mm_cmpeq_epi32(_mm_castps_si128(V1),_mm_castps_si128(V2));
           MOVUPS  XMM0, [V1]
           PCMPEQD XMM0, [V2]
    {
    int iTest = _mm_movemask_ps(_mm_castsi128_ps(vTemp));
    movmskps r32, xmm
    uint32_t CR = 0;
    if (iTest==0xf)     // All equal?
        CR = XM_CRMASK_CR6TRUE;
    else if (iTest==0)  // All not equal?
        CR = XM_CRMASK_CR6FALSE;
    return CR;
    }
           MOVMSKPS EAX, XMM0
           MOV     EDX,XM_CRMASK_CR6FALSE
           CMP     EAX, $0
           JE      @Finished

           MOV     EDX,XM_CRMASK_CR6TRUE
           AND     EAX, $F
           CMP     EAX, $F
           JE      @Finished
           MOV     EDX,$0
           @Finished:
           MOV     [result],EDX
end;


{$IF DEFINED(_XM_AVX_INTRINSICS_)}

function XMVector4NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean; assembler;
asm
           VMOVUPS XMM1,[v1]
           VMOVUPS XMM2,[v2]

           VSUBPS  XMM0, XMM1, XMM2 // difference vDelta in XMM0
           VXORPS  XMM2, XMM2, XMM2 // Zero in XMM2
           VSUBPS  XMM2, XMM2, XMM0  // negativ differenz in XMM2
           VMAXPS  XMM2, XMM2, XMM0   // Get the absolute value of the difference in XMM2
           VMOVUPS XMM1,[Epsilon]
           VCMPPS  XMM0, XMM2, XMM1, 2 // CMPLEPS
           VMOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ELSE}


function XMVector4NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean; assembler;
asm
           // Get the difference
           //  vDelta = _mm_sub_ps(V1,V2);
           MOVUPS  XMM1, [V1]
           SUBPS   XMM1, [V2]
           // Get the absolute value of the difference
           //  vTemp = _mm_setzero_ps();
           XORPS   XMM0, XMM0
           // vTemp = _mm_sub_ps(vTemp,vDelta);
           SUBPS   XMM0, XMM1
           // vTemp = _mm_max_ps(vTemp,vDelta);
           MAXPS   XMM0, XMM1
           // vTemp = _mm_cmple_ps(vTemp,Epsilon);
           CMPPS   XMM0, [Epsilon], 2
           // return ((_mm_movemask_ps(vTemp)==0xf) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ENDIF}




{$IF DEFINED(_XM_AVX_INTRINSICS_)}

function XMVector4NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           VMOVUPS XMM1,[v1]
           VMOVUPS XMM2,[v2]
           VCMPPS  XMM0, XMM1, XMM2, 4 // CMPNEQPS
           VMOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ELSEIF DEFINED(_XM_SSE_INTRINSICS_)}


function XMVector4NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           // vTemp = _mm_cmpneq_ps(V1,V2);
           MOVUPS  XMM0, [V1]
           CMPPS   XMM0, [V2], 4
           // return ((_mm_movemask_ps(vTemp)) != 0);
           MOVMSKPS EAX, XMM0
           CMP     EAX, $0
           SETNE    [result]
end;
{$ELSE}


function XMVector4NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := XMComparisonAnyFalse(XMVector4EqualR(V1, V2));
end;

{$ENDIF}



{$IF DEFINED(_XM_SSE_INTRINSICS_)}

function XMVector4NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           //  __m128i vTemp = _mm_cmpeq_epi32(_mm_castps_si128(V1),_mm_castps_si128(V2));
           //    return ((_mm_movemask_ps(_mm_castsi128_ps(vTemp))!=0xF) != 0);

           MOVUPS  XMM0,[v1]
           MOVUPS  XMM1,[v2]
           PCMPEQD  XMM0, XMM1
           MOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETNE    [result]
end;
{$ELSE}


function XMVector4NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := XMComparisonAnyFalse(XMVector4EqualIntR(V1, V2));
end;

{$ENDIF}


{$IF DEFINED(_XM_SSE_INTRINSICS_)}

function XMVector4Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           // XMVECTOR vTemp = _mm_cmpgt_ps(V1,V2);
           MOVUPS  XMM0,[V2]
           CMPPS   XMM0, [V1], 1
           //    return ((_mm_movemask_ps(vTemp)==0x0f) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ELSE}


function XMVector4Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := XMComparisonAllTrue(XMVector4GreaterR(V1, V2));
end;

{$ENDIF}



function XMVector4GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32; assembler;
asm
{    uint32_t CR = 0;
    XMVECTOR vTemp = _mm_cmpgt_ps(V1,V2);
    cmpps xmm, xmm, 1
    int iTest = _mm_movemask_ps(vTemp);
    movmskps r32, xmm
    if (iTest==0xf) then
        CR = XM_CRMASK_CR6TRUE;

    else if (!iTest) then
        CR = XM_CRMASK_CR6FALSE;
    return CR; }
           VMOVUPS XMM1,[v1]
           VMOVUPS XMM2,[v2]
           VCMPPS  XMM0, XMM1, XMM2, 6 // CMPNLEPS
           VMOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           MOV     EDX,XM_CRMASK_CR6TRUE
           JE      @Finished
           CMP     EAX, 0
           JE      @Less
           MOV     EDX,0
           JMP     @finished
           @Less:
           MOV     EDX,XM_CRMASK_CR6FALSE
           @Finished:
           MOV     [result],EDX
end;


{$IF DEFINED(_XM_SSE_INTRINSICS_)}

function XMVector4GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           // XMVECTOR vTemp = _mm_cmpge_ps(V1,V2);
           MOVUPS  XMM0,[v2]
           CMPPS   XMM0, [v1], 2
           //    return ((_mm_movemask_ps(vTemp)==0x0f) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ELSE}


function XMVector4GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := XMComparisonAllTrue(XMVector4GreaterOrEqualR(V1, V2));
end;

{$ENDIF}



function XMVector4GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32; assembler;
asm
           VMOVUPS XMM1,[v1]
           VMOVUPS XMM2,[v2]
           VCMPPS  XMM0, XMM1, XMM2, 5 // CMPNLTPS
           VMOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           MOV     EDX,XM_CRMASK_CR6TRUE
           JE      @Finished
           CMP     EAX, 0
           JE      @Less
           MOV     EDX,0
           JMP     @finished
           @Less:
           MOV     EDX,XM_CRMASK_CR6FALSE
           @Finished:
           MOV     [result],EDX
end;


{$IF DEFINED(_XM_AVX_INTRINSICS_)}

function XMVector4Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           VMOVUPS XMM1,[v1]
           VMOVUPS XMM2,[v2]
           VCMPPS  XMM0, XMM1, XMM2, 1
           VMOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ELSEIF DEFINED(_XM_SSE_INTRINSICS_)}


function XMVector4Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           // vTemp = _mm_cmplt_ps(V1,V2);
           MOVUPS  XMM0,[V1]
           CMPPS   XMM0, [V2], 1
           // return ((_mm_movemask_ps(vTemp)==0x0f) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ELSE}


function XMVector4Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := XMComparisonAllTrue(XMVector4GreaterR(V2, V1));
end;

{$ENDIF}




{$IF DEFINED(_XM_SSE_INTRINSICS_)}

function XMVector4LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           //  vTemp = _mm_cmple_ps(V1,V2);
           MOVUPS  XMM0,[V1]
           CMPPS   XMM0, [V2], 2
           // return ((_mm_movemask_ps(vTemp)==0x0f) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ELSEIF DEFINED(_XM_AVX_INTRINSICS_)}


function XMVector4LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean; assembler;
asm
           VMOVUPS XMM1,[v1]
           VMOVUPS XMM2,[v2]
           VCMPPS  XMM0, XMM1, XMM2, 2
           VMOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ELSE}


function XMVector4LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := XMComparisonAllTrue(XMVector4GreaterOrEqualR(V2, V1));
end;

{$ENDIF}



{$IF DEFINED(_XM_SSE_INTRINSICS_)}

function XMVector4InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean; assembler;
asm
           // Test if less than or equal
           // vTemp1 = _mm_cmple_ps(V,Bounds);
           MOVUPS  XMM0, [V]
           CMPPS   XMM0, [Bounds], 2
           // Negate the bounds
           //  vTemp2 = _mm_mul_ps(Bounds,g_XMNegativeOne);
           MOVUPS  XMM1, [Bounds]
           MULPS   XMM1, [g_XMNegativeOne]
           // Test if greater or equal (Reversed)
           // vTemp2 = _mm_cmple_ps(vTemp2,V);
           CMPPS   XMM1, [V], 2
           // Blend answers
           // vTemp1 = _mm_and_ps(vTemp1,vTemp2);
           ANDPS   XMM0, XMM1
           // All in bounds?
           // return ((_mm_movemask_ps(vTemp1)==0x0f) != 0);
           MOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]
end;
{$ELSE}


function XMVector4InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean;
begin
    Result := XMComparisonAllInBounds(XMVector4InBoundsR(V, Bounds));
end;

{$ENDIF}



function XMVector4IsNaN(constref V: TXMVECTOR): boolean; assembler;
asm
           // Test against itself. NaN is always not equal
           //  vTempNan = _mm_cmpneq_ps(V,V);
           MOVUPS  XMM0, [V]
           CMPPS   XMM0, XMM0, 4
           // If any are NaN, the mask is non-zero
           //return (_mm_movemask_ps(vTempNan)!=0);
           MOVMSKPS EAX, XMM0
           CMP     EAX, $0
           SETE    [result]
end;




function XMVector4IsInfinite(constref V: TXMVECTOR): boolean; assembler;
asm
           // Mask off the sign bit
           //  vTemp = _mm_and_ps(V,g_XMAbsMask);
           MOVUPS  XMM0, [V]
           ANDPS   XMM0, [g_XMAbsMask]
           // Compare to infinity
           // vTemp = _mm_cmpeq_ps(vTemp,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 0
           // If any are infinity, the signs are true.
           // return (_mm_movemask_ps(vTemp) != 0);
           MOVMSKPS EAX, XMM0
           CMP     EAX, $0
           SETE    [result]
end;



//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------



{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector4Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           //return _mm_dp_ps( V1, V2, 0xff );
           MOVUPS  XMM0,[V1]
           DPPS    XMM0, [V2], $FF
           MOVUPS  [result],XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector4Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_mul_ps(V1, V2);
           MOVUPS  XMM0,[V1]
           MULPS   XMM0, [V2]
           // vTemp = _mm_hadd_ps(vTemp, vTemp);
           HADDPS  XMM0, XMM0
           // return _mm_hadd_ps(vTemp, vTemp);
           HADDPS  XMM0, XMM0
           MOVUPS  [result],XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector4Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vTemp2 = V2;
           MOVUPS  XMM1, [V2]
           //  vTemp = _mm_mul_ps(V1,vTemp2);
           MOVUPS  XMM0,[V1]
           MULPS   XMM0, XMM1
           // vTemp2 = _mm_shuffle_ps(vTemp2,vTemp,_MM_SHUFFLE(1,0,0,0)); // Copy X to the Z position and Y to the W position
           SHUFPS  XMM1, XMM0, _MM_SHUFFLE_1_0_0_0
           // vTemp2 = _mm_add_ps(vTemp2,vTemp);          // Add Z = X+Z; W = Y+W;
           ADDPS   XMM1, XMM0
           // vTemp = _mm_shuffle_ps(vTemp,vTemp2,_MM_SHUFFLE(0,3,0,0));  // Copy W to the Z position
           SHUFPS  XMM0, XMM1, _MM_SHUFFLE_0_3_0_0
           // vTemp = _mm_add_ps(vTemp,vTemp2);           // Add Z and W together
           ADDPS   XMM0, XMM1
           // return XM_PERMUTE_PS(vTemp,_MM_SHUFFLE(2,2,2,2));    // Splat Z and return
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_2_2_2
           MOVUPS  [Result],XMM0
end;
{$ENDIF}


function XMVector4Cross(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR; assembler;
asm
           // V2zwyz * V3wzwy
           //  vResult = XM_PERMUTE_PS(V2,_MM_SHUFFLE(2,1,3,2));
           MOVUPS  XMM0,[V2]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_1_3_2
           //  vTemp3 = XM_PERMUTE_PS(V3,_MM_SHUFFLE(1,3,2,3));
           MOVUPS  XMM3, [V3]
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_1_3_2_3
           // vResult = _mm_mul_ps(vResult,vTemp3);
           MULPS   XMM0, XMM3
           // - V2wzwy * V3zwyz
           //  vTemp2 = XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,3,2,3));
           MOVUPS  XMM2 ,[V2]
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_3_2_3
           // vTemp3 = XM_PERMUTE_PS(vTemp3,_MM_SHUFFLE(1,3,0,1));
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_1_3_0_1
           // vTemp2 = _mm_mul_ps(vTemp2,vTemp3);
           MULPS   XMM2, XMM3
           //  vResult = _mm_sub_ps(vResult,vTemp2);
           SUBPS   XMM0, XMM2
           // term1 * V1yxxx
           //  vTemp1 = XM_PERMUTE_PS(V1,_MM_SHUFFLE(0,0,0,1));
           MOVUPS  XMM1, [V1]
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_0_0_0_1
           // vResult = _mm_mul_ps(vResult,vTemp1);
           MULPS   XMM0, XMM1

           // V2ywxz * V3wxwx
           // vTemp2 = XM_PERMUTE_PS(V2,_MM_SHUFFLE(2,0,3,1));
           MOVUPS  XMM2, [V2]
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_0_3_1
           // vTemp3 = XM_PERMUTE_PS(V3,_MM_SHUFFLE(0,3,0,3));
           MOVUPS  XMM3, [V3]
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_0_3_0_3
           // vTemp3 = _mm_mul_ps(vTemp3,vTemp2);
           MULPS   XMM3, XMM2
           // - V2wxwx * V3ywxz
           // vTemp2 = XM_PERMUTE_PS(vTemp2,_MM_SHUFFLE(2,1,2,1));
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_1_2_1
           // vTemp1 = XM_PERMUTE_PS(V3,_MM_SHUFFLE(2,0,3,1));
           MOVUPS  XMM1, [V3]
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_0_3_1
           // vTemp2 = _mm_mul_ps(vTemp2,vTemp1);
           MULPS   XMM2, XMM1
           // vTemp3 = _mm_sub_ps(vTemp3,vTemp2);
           SUBPS   XMM3, XMM2
           // vResult - temp * V1zzyy
           // vTemp1 = XM_PERMUTE_PS(V1,_MM_SHUFFLE(1,1,2,2));
           MOVUPS  XMM1, [V1]
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_2_2
           // vTemp1 = _mm_mul_ps(vTemp1,vTemp3);
           MULPS   XMM1, XMM3
           // vResult = _mm_sub_ps(vResult,vTemp1);
           SUBPS   XMM0, XMM1

           // V2yzxy * V3zxyx
           // vTemp2 = XM_PERMUTE_PS(V2,_MM_SHUFFLE(1,0,2,1));
           MOVUPS  XMM2, [V2]
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_0_2_1
           // vTemp3 = XM_PERMUTE_PS(V3,_MM_SHUFFLE(0,1,0,2));
           MOVUPS  XMM3, [V3]
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_0_1_0_2
           // vTemp3 = _mm_mul_ps(vTemp3,vTemp2);
           MULPS   XMM3, XMM2
           // - V2zxyx * V3yzxy
           // vTemp2 = XM_PERMUTE_PS(vTemp2,_MM_SHUFFLE(2,0,2,1));
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_0_2_1
           // vTemp1 = XM_PERMUTE_PS(V3,_MM_SHUFFLE(1,0,2,1));
           MOVUPS  XMM1, [V3]
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_0_2_1
           // vTemp1 = _mm_mul_ps(vTemp1,vTemp2);
           MULPS   XMM1, XMM2
           // vTemp3 = _mm_sub_ps(vTemp3,vTemp1);
           SUBPS   XMM3, XMM1
           // vResult + term * V1wwwz
           // vTemp1 = XM_PERMUTE_PS(V1,_MM_SHUFFLE(2,3,3,3));
           MOVUPS  XMM1 ,[V1]
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_3_3_3
           // vTemp3 = _mm_mul_ps(vTemp3,vTemp1);
           MULPS   XMM3, XMM1
           // vResult = _mm_add_ps(vResult,vTemp3);
           ADDPS   XMM0, XMM3
           // return vResult;
           MOVUPS  [result],XMM0
end;


{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector4ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0xff );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $FF
           // return _mm_rsqrt_ps( vTemp );
           RSQRTPS XMM0, XMM0
           MOVUPS  [result] , XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector4ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_rsqrt_ps(vLengthSq);
           RSQRTPS XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result] , XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector4ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y,z and w
           //  vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp has z and w
           //  vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(3,2,3,2));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_3_2_3_2
           // x+z, y+w
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // x+z,x+z,x+z,y+w
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_0_0_0
           // ??,??,y+w,y+w
           // vTemp = _mm_shuffle_ps(vTemp,vLengthSq,_MM_SHUFFLE(3,3,0,0));
           SHUFPS  XMM1, XMM0, _MM_SHUFFLE_3_3_0_0
           // ??,??,x+z+y+w,??
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // Splat the length
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_2_2_2
           // Get the reciprocal
           // vLengthSq = _mm_rsqrt_ps(vLengthSq);
           RSQRTPS XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;
{$ENDIF}



{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector4ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0xff );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $FF
           //  vLengthSq = _mm_sqrt_ps( vTemp );
           SQRTPS  XMM1, XMM0
           // return _mm_div_ps( g_XMOne, vLengthSq );
           MOVUPS  XMM0,[g_XMOne]
           DIVPS   XMM0, XMM1
           MOVUPS  [result],XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector4ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM1, XMM0
           // vLengthSq = _mm_div_ps(g_XMOne, vLengthSq);
           MOVUPS  XMM0,[g_XMOne]
           DIVPS   XMM0, XMM1
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector4ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y,z and w
           //  vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp has z and w
           //  vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(3,2,3,2));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_3_2_3_2
           // x+z, y+w
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // x+z,x+z,x+z,y+w
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_0_0_0
           // ??,??,y+w,y+w
           // vTemp = _mm_shuffle_ps(vTemp,vLengthSq,_MM_SHUFFLE(3,3,0,0));
           SHUFPS  XMM1, XMM0, _MM_SHUFFLE_3_3_0_0
           // ??,??,x+z+y+w,??
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // Splat the length
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_2_2_2
           // Get the reciprocal
           // vLengthSq = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM0, XMM0
           // Accurate!
           // vLengthSq = _mm_div_ps(g_XMOne,vLengthSq);
           MOVUPS  XMM1,[g_XMOne]
           DIVPS   XMM1, XMM0
           // return vLengthSq;
           MOVUPS  [result],XMM1
end;
{$ENDIF}


{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector4LengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0xff );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $FF
           // return _mm_sqrt_ps( vTemp );
           SQRTPS  XMM0, XMM0
           MOVUPS  [result],XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector4LengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector4LengthEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y,z and w
           //  vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0, [V]
           MULPS   XMM0, XMM0
           // vTemp has z and w
           //  vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(3,2,3,2));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_3_2_3_2
           // x+z, y+w
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // x+z,x+z,x+z,y+w
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_0_0_0
           // ??,??,y+w,y+w
           // vTemp = _mm_shuffle_ps(vTemp,vLengthSq,_MM_SHUFFLE(3,3,0,0));
           SHUFPS  XMM1, XMM0, _MM_SHUFFLE_3_3_0_0
           // ??,??,x+z+y+w,??
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // Splat the length
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_2_2_2
           // Get the length
           // vLengthSq = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;
{$ENDIF}


{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector4Length(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0xff );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $FF
           // return _mm_sqrt_ps( vTemp );
           SQRTPS  XMM0, XMM0
           MOVUPS  [result],XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector4Length(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector4Length(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y,z and w
           // vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp has z and w
           // vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(3,2,3,2));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_3_2_3_2
           // x+z, y+w
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // x+z,x+z,x+z,y+w
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_0_0_0
           // ??,??,y+w,y+w
           // vTemp = _mm_shuffle_ps(vTemp,vLengthSq,_MM_SHUFFLE(3,3,0,0));
           SHUFPS  XMM1, XMM0, _MM_SHUFFLE_3_3_0_0
           // ??,??,x+z+y+w,??
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // Splat the length
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_2_2_2
           // Get the length
           // vLengthSq = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM0, XMM0
           // return vLengthSq;
           MOVUPS  [result],XMM0
end;
{$ENDIF}



{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector4NormalizeEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vTemp = _mm_dp_ps( V, V, 0xff );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $FF
           //  vResult = _mm_rsqrt_ps( vTemp );
           RSQRTPS XMM0, XMM0
           // return _mm_mul_ps(vResult, V);
           MULPS   XMM0, [V]
           MOVUPS  [result],XMM0
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector4NormalizeEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vDot = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vDot = _mm_hadd_ps(vDot, vDot);
           HADDPS  XMM0, XMM0
           // vDot = _mm_hadd_ps(vDot, vDot);
           HADDPS  XMM0, XMM0
           // vDot = _mm_rsqrt_ps(vDot);
           RSQRTPS XMM0, XMM0
           // vDot = _mm_mul_ps(vDot, V);
           MULPS   XMM0, [V]
           // return vDot;
           MOVUPS  [result],XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector4NormalizeEst(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y,z and w
           //  vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp has z and w
           //  vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(3,2,3,2));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_3_2_3_2
           // x+z, y+w
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // x+z,x+z,x+z,y+w
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_0_0_0
           // ??,??,y+w,y+w
           // vTemp = _mm_shuffle_ps(vTemp,vLengthSq,_MM_SHUFFLE(3,3,0,0));
           SHUFPS  XMM1, XMM0, _MM_SHUFFLE_3_3_0_0
           // ??,??,x+z+y+w,??
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // Splat the length
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_2_2_2
           // Get the reciprocal
           //  vResult = _mm_rsqrt_ps(vLengthSq);
           RSQRTPS XMM0, XMM0
           // Reciprocal mul to perform the normalization
           // vResult = _mm_mul_ps(vResult,V);
           MULPS   XMM0, [V]
           // return vResult;
           MOVUPS  [result],XMM0
end;
{$ENDIF}


{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMVector4Normalize(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // vLengthSq = _mm_dp_ps( V, V, 0xff );
           MOVUPS  XMM0,[V]
           DPPS    XMM0, XMM0, $FF
           // Prepare for the division
           //  vResult = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM1, XMM0
           // Create zero with a single instruction
           //  vZeroMask = _mm_setzero_ps();
           XORPS   XMM2, XMM2
           // Test for a divide by zero (Must be FP to detect -0.0)
           // vZeroMask = _mm_cmpneq_ps(vZeroMask,vResult);
           CMPPS   XMM2, XMM1, 4
           // Failsafe on zero (Or epsilon) length planes
           // If the length is infinity, set the elements to zero
           // vLengthSq = _mm_cmpneq_ps(vLengthSq,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 4
           // Divide to perform the normalization
           // vResult = _mm_div_ps(V,vResult);
           MOVUPS  XMM3,[V]
           DIVPS   XMM3, XMM1
           // Any that are infinity, set to zero
           // vResult = _mm_and_ps(vResult,vZeroMask);
           ANDPS   XMM3, XMM2
           // Select qnan or result based on infinite length
           //  vTemp1 = _mm_andnot_ps(vLengthSq,g_XMQNaN);
           MOVUPS  XMM2, [g_XMQNaN]
           ANDNPS  XMM2, XMM0
           //  vTemp2 = _mm_and_ps(vResult,vLengthSq);
           ANDPS   XMM1, XMM0
           // vResult = _mm_or_ps(vTemp1,vTemp2);
           ORPS    XMM1, XMM2
           // return vResult;
           MOVUPS  [result],XMM1
end;
{$ELSEIF DEFINED(_XM_SSE3_INTRINSICS_)}


function XMVector4Normalize(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y,z and w
           //  vLengthSq = _mm_mul_ps(V, V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // vLengthSq = _mm_hadd_ps(vLengthSq, vLengthSq);
           HADDPS  XMM0, XMM0
           // Prepare for the division
           //  vResult = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM1, XMM0
           // Create zero with a single instruction
           //  vZeroMask = _mm_setzero_ps();
           XORPS   XMM2, XMM2
           // Test for a divide by zero (Must be FP to detect -0.0)
           // vZeroMask = _mm_cmpneq_ps(vZeroMask,vResult);
           CMPPS   XMM2, XMM1, 4
           // Failsafe on zero (Or epsilon) length planes
           // If the length is infinity, set the elements to zero
           // vLengthSq = _mm_cmpneq_ps(vLengthSq,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 4
           // Divide to perform the normalization
           // vResult = _mm_div_ps(V,vResult);
           MOVUPS  XMM3,[V]
           DIVPS   XMM3, XMM1
           // Any that are infinity, set to zero
           // vResult = _mm_and_ps(vResult,vZeroMask);
           ANDPS   XMM1, XMM2
           // Select qnan or result based on infinite length
           //  vTemp1 = _mm_andnot_ps(vLengthSq,g_XMQNaN);
           MOVUPS  XMM2, [g_XMQNaN]
           ANDNPS  XMM2, XMM0
           //  vTemp2 = _mm_and_ps(vResult,vLengthSq);
           ANDPS   XMM0, XMM1
           // vResult = _mm_or_ps(vTemp1,vTemp2);
           ORPS    XMM0, XMM2
           // return vResult;
           MOVUPS  [result],XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMVector4Normalize(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y,z and w
           //  vLengthSq = _mm_mul_ps(V,V);
           MOVUPS  XMM0,[V]
           MULPS   XMM0, XMM0
           // vTemp has z and w
           //  vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(3,2,3,2));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_3_2_3_2
           // x+z, y+w
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // x+z,x+z,x+z,y+w
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(1,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_0_0_0
           // ??,??,y+w,y+w
           // vTemp = _mm_shuffle_ps(vTemp,vLengthSq,_MM_SHUFFLE(3,3,0,0));
           SHUFPS  XMM1, XMM0, _MM_SHUFFLE_3_3_0_0
           // ??,??,x+z+y+w,??
           // vLengthSq = _mm_add_ps(vLengthSq,vTemp);
           ADDPS   XMM0, XMM1
           // Splat the length
           // vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_2_2_2
           // Prepare for the division
           //  vResult = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM2, XMM0
           // Create zero with a single instruction
           //  vZeroMask = _mm_setzero_ps();
           XORPS   XMM3, XMM3
           // Test for a divide by zero (Must be FP to detect -0.0)
           // vZeroMask = _mm_cmpneq_ps(vZeroMask,vResult);
           CMPPS   XMM3, XMM2, 4
           // Failsafe on zero (Or epsilon) length planes
           // If the length is infinity, set the elements to zero
           // vLengthSq = _mm_cmpneq_ps(vLengthSq,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 4
           // Divide to perform the normalization
           // vResult = _mm_div_ps(V,vResult);
           MOVUPS  XMM4, [V]
           DIVPS   XMM4, XMM2
           // Any that are infinity, set to zero
           // vResult = _mm_and_ps(vResult,vZeroMask);
           ANDPS   XMM4, XMM3
           // Select qnan or result based on infinite length
           //  vTemp1 = _mm_andnot_ps(vLengthSq,g_XMQNaN);
           MOVUPS  XMM1, [g_XMQNaN]
           ANDNPS  XMM1, XMM0
           //  vTemp2 = _mm_and_ps(vResult,vLengthSq);
           ANDPS   XMM4, XMM0
           // vResult = _mm_or_ps(vTemp1,vTemp2);
           ORPS    XMM1, XMM4
           // return vResult;
           MOVUPS  [result],XMM1
end;
{$ENDIF}



function XMVector4RefractV(Incident: TXMVECTOR; Normal: TXMVECTOR; RefractionIndex: TXMVECTOR): TXMVECTOR; inline;
var
    IDotN: TXMVECTOR;
begin
    IDotN := XMVector4Dot(Incident, Normal);
    asm


               // R = 1.0f - RefractionIndex * RefractionIndex * (1.0f - IDotN * IDotN)
               MOVUPS  XMM0,[IDotN]
               // R = _mm_mul_ps(IDotN,IDotN);
               MULPS   XMM0, XMM0
               // R = _mm_sub_ps(g_XMOne,R);
               MOVUPS  XMM1, [g_XMOne]
               SUBPS   XMM1, XMM0
               // R = _mm_mul_ps(R, RefractionIndex);
               MULPS   XMM1, [RefractionIndex]
               // R = _mm_mul_ps(R, RefractionIndex);
               MULPS   XMM1, [RefractionIndex]
               // R = _mm_sub_ps(g_XMOne,R);
               MOVUPS  XMM0, [g_XMOne]
               SUBPS   XMM0, XMM1

               //  vResult = _mm_cmple_ps(R,g_XMZero);
               MOVUPS  XMM1, XMM0  // XMM1 = R
               CMPPS   XMM0, [g_XMZero], 2
               // if (_mm_movemask_ps(vResult)==0x0f)
               MOVMSKPS EAX, XMM0
               AND     EAX, $F
               CMP     EAX, $F
               JE      @TotalInternalReflection

               // R = RefractionIndex * IDotN + sqrt(R)
               // R = _mm_sqrt_ps(R);
               SQRTPS  XMM1, XMM1
               //vResult = _mm_mul_ps(RefractionIndex, IDotN);
               MOVUPS  XMM0,[RefractionIndex]
               MULPS   XMM0, [IDotN]
               //R = _mm_add_ps(R,vResult);
               ADDPS   XMM1, XMM0
               // Result = RefractionIndex * Incident - Normal * R
               //vResult = _mm_mul_ps(RefractionIndex, Incident);
               MOVUPS  XMM0, [RefractionIndex]
               MULPS   XMM0, [Incident]
               //R = _mm_mul_ps(R,Normal);
               MULPS   XMM1, [Normal]
               //vResult = _mm_sub_ps(vResult,R);
               SUBPS   XMM0, XMM1
               JMP     @Finished
               @TotalInternalReflection:
               MOVUPS  XMM0,[g_XMZero]
               @Finished:
               // return vResult;
               MOVUPS  [result],XMM0
    end;
end;



function XMVector4Orthogonal(constref V: TXMVECTOR): TXMVECTOR; assembler;
const
    FlipZW: TXMVECTORF32 = (f: (1.0, 1.0, -1.0, -1.0));
asm
           // vResult = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,0,3,2));
           MOVUPS  XMM0, [V]
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_1_0_3_2
           // vResult = _mm_mul_ps(vResult,FlipZW);
           MULPS   XMM0 ,[FlipZW]
           // return vResult;
           MOVUPS  [result],XMM0
end;



function XMVector4Transform(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR; assembler;
asm
           // Splat x,y,z and w
           // vTempX = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM0, [V]
           MOVUPS  XMM1, XMM0
           MOVUPS  XMM2, XMM0
           MOVUPS  XMM3, XMM0
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           //  vTempY = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // vTempZ = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
           // vTempW = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,3,3,3));
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           // Mul by the matrix
           // vTempX = _mm_mul_ps(vTempX,M.r[0]);
           MULPS   XMM0, TXMMATRIX([M]).r0
           // vTempY = _mm_mul_ps(vTempY,M.r[1]);
           MULPS   XMM1, TXMMATRIX([M]).r1
           // vTempZ = _mm_mul_ps(vTempZ,M.r[2]);
           MULPS   XMM2, TXMMATRIX([M]).r2
           // vTempW = _mm_mul_ps(vTempW,M.r[3]);
           MULPS   XMM3, TXMMATRIX([M]).r3
           // Add them all together
           // vTempX = _mm_add_ps(vTempX,vTempY);
           ADDPS   XMM0, XMM1
           // vTempZ = _mm_add_ps(vTempZ,vTempW);
           ADDPS   XMM2, XMM3
           // vTempX = _mm_add_ps(vTempX,vTempZ);
           ADDPS   XMM0, XMM2
           // return vTempX;
           MOVUPS  [result],XMM0
end;



function XMVector4TransformStream(out pOutputStream: PXMFLOAT4; constref OutputStride: size_t; constref pInputStream: PXMFLOAT4;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT4; inline;
var
    pInputVector, pOutputVector: pointer;
    row0, row1, row2, row3: TXMVECTOR;
    V: TXMVECTOR;
    i: size_t;
begin
    pInputVector := pInputStream;
    pOutputVector := pOutputStream;

    row0 := M.r[0];
    row1 := M.r[1];
    row2 := M.r[2];
    row3 := M.r[3];

    if (not (uintptr(pOutputStream) and $F = $F) and not (OutputStride and $F = $F)) then
    begin
        if (not (uintptr(pInputStream) and $F = $F) and not (InputStride and $F = $F)) then
        begin
            // Aligned input, aligned output
            for  i := 0 to VectorCount - 1 do
            begin
                asm
                           //  V = _mm_load_ps( reinterpret_cast<const float*>(pInputVector) );
                           MOVAPS  XMM0, [pInputVector]
                           // pInputVector += InputStride;
                           MOV     EDX, pInputVector
                           ADD     EDX, [InputStride]
                           MOV     pInputVector,EDX

                           //  vTempX = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));

                           MOVUPS  XMM1,XMM0
                           MOVUPS  XMM2,XMM0
                           MOVUPS  XMM3,XMM0
                           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
                           //  vTempY = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
                           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                           //  vTempZ = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
                           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                           //  vTempW = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,3,3,3));
                           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                           // vTempX = _mm_mul_ps(vTempX,row0);
                           MULPS   XMM0, [row0]
                           // vTempY = _mm_mul_ps(vTempY,row1);
                           MULPS   XMM1, [row1]
                           // vTempZ = _mm_mul_ps(vTempZ,row2);
                           MULPS   XMM2, [row2]
                           // vTempW = _mm_mul_ps(vTempW,row3);
                           MULPS   XMM3, [row3]

                           // vTempX = _mm_add_ps(vTempX,vTempY);
                           ADDPS   XMM0, XMM1
                           // vTempZ = _mm_add_ps(vTempZ,vTempW);
                           ADDPS   XMM2, XMM3
                           // vTempX = _mm_add_ps(vTempX,vTempZ);
                           ADDPS   XMM0, XMM2

                           // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTempX );
                           MOVUPS  [pOutputVector], XMM0
                           // pOutputVector += OutputStride;
                           MOV     EDX, pOutputVector
                           ADD     EDX, [OutputStride]
                           MOV     pOutputVector,EDX
                end;
            end;
        end
        else
        begin
            // Unaligned input, aligned output
            for i := 0 to VectorCount - 1 do
            begin
                asm
                           // V = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                           MOVUPS  XMM0, [pInputVector]
                           // pInputVector += InputStride;
                           MOV     EDX, pInputVector
                           ADD     EDX, [InputStride]
                           MOV     pInputVector,EDX

                           //  vTempX = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
                           MOVUPS  XMM1,XMM0
                           MOVUPS  XMM2,XMM0
                           MOVUPS  XMM3,XMM0
                           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
                           //  vTempY = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
                           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                           //  vTempZ = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
                           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                           //  vTempW = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,3,3,3));
                           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                           // vTempX = _mm_mul_ps(vTempX,row0);
                           MULPS   XMM0, [row0]
                           // vTempY = _mm_mul_ps(vTempY,row1);
                           MULPS   XMM1, [row1]
                           // vTempZ = _mm_mul_ps(vTempZ,row2);
                           MULPS   XMM2, [row2]
                           // vTempW = _mm_mul_ps(vTempW,row3);
                           MULPS   XMM3, [row3]

                           // vTempX = _mm_add_ps(vTempX,vTempY);
                           ADDPS   XMM0, XMM1
                           // vTempZ = _mm_add_ps(vTempZ,vTempW);
                           ADDPS   XMM2, XMM3
                           // vTempX = _mm_add_ps(vTempX,vTempZ);
                           ADDPS   XMM0, XMM2

                           // XM_STREAM_PS( reinterpret_cast<float*>(pOutputVector), vTempX );
                           MOVUPS  [pOutputVector], XMM0
                           // pOutputVector += OutputStride;
                           MOV     EDX, pOutputVector
                           ADD     EDX, [OutputStride]
                           MOV     pOutputVector,EDX
                end;
            end;
        end;
    end
    else
    begin
        if (not (uintptr(pInputStream) and $F = $F) and not (InputStride and $F = $F)) then
        begin
            // Aligned input, unaligned output
            for i := 0 to VectorCount - 1 do
            begin
                asm
                           //  V = _mm_load_ps( reinterpret_cast<const float*>(pInputVector) );
                           MOVAPS  XMM0, [pInputVector]
                           // pInputVector += InputStride;
                           MOV     EDX, pInputVector
                           ADD     EDX, [InputStride]
                           MOV     pInputVector,EDX

                           //  vTempX = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
                           MOVUPS  XMM1,XMM0
                           MOVUPS  XMM2,XMM0
                           MOVUPS  XMM3,XMM0
                           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
                           //  vTempY = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
                           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                           //  vTempZ = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
                           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                           //  vTempW = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,3,3,3));
                           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                           // vTempX = _mm_mul_ps(vTempX,row0);
                           MULPS   XMM0, [row0]
                           // vTempY = _mm_mul_ps(vTempY,row1);
                           MULPS   XMM1, [row1]
                           // vTempZ = _mm_mul_ps(vTempZ,row2);
                           MULPS   XMM2, [row2]
                           // vTempW = _mm_mul_ps(vTempW,row3);
                           MULPS   XMM3, [row3]

                           // vTempX = _mm_add_ps(vTempX,vTempY);
                           ADDPS   XMM0, XMM1
                           // vTempZ = _mm_add_ps(vTempZ,vTempW);
                           ADDPS   XMM2, XMM3
                           // vTempX = _mm_add_ps(vTempX,vTempZ);
                           ADDPS   XMM0, XMM2

                           // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTempX );
                           MOVUPS  [pOutputVector], XMM0
                           //  pOutputVector += OutputStride;
                           MOV     EDX, pOutputVector
                           ADD     EDX, [OutputStride]
                           MOV     pOutputVector,EDX
                end;
            end;
        end
        else
        begin
            // Unaligned input, unaligned output
            for i := 0 to VectorCount - 1 do
            begin
                asm
                           // V = _mm_loadu_ps( reinterpret_cast<const float*>(pInputVector) );
                           MOVUPS  XMM0, [pInputVector]
                           // pInputVector += InputStride;
                           MOV     EDX, pInputVector
                           ADD     EDX, [InputStride]
                           MOV     pInputVector,EDX


                           //  vTempX = XM_PERMUTE_PS(V,_MM_SHUFFLE(0,0,0,0));
                           MOVUPS  XMM1,XMM0
                           MOVUPS  XMM2,XMM0
                           MOVUPS  XMM3,XMM0
                           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
                           //  vTempY = XM_PERMUTE_PS(V,_MM_SHUFFLE(1,1,1,1));
                           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
                           //  vTempZ = XM_PERMUTE_PS(V,_MM_SHUFFLE(2,2,2,2));
                           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
                           //  vTempW = XM_PERMUTE_PS(V,_MM_SHUFFLE(3,3,3,3));
                           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3

                           // vTempX = _mm_mul_ps(vTempX,row0);
                           MULPS   XMM0, [row0]
                           // vTempY = _mm_mul_ps(vTempY,row1);
                           MULPS   XMM1, [row1]
                           // vTempZ = _mm_mul_ps(vTempZ,row2);
                           MULPS   XMM2, [row2]
                           // vTempW = _mm_mul_ps(vTempW,row3);
                           MULPS   XMM3, [row3]

                           // vTempX = _mm_add_ps(vTempX,vTempY);
                           ADDPS   XMM0, XMM1
                           // vTempZ = _mm_add_ps(vTempZ,vTempW);
                           ADDPS   XMM2, XMM3
                           // vTempX = _mm_add_ps(vTempX,vTempZ);
                           ADDPS   XMM0, XMM2

                           // _mm_storeu_ps( reinterpret_cast<float*>(pOutputVector), vTempX );
                           MOVUPS  [pOutputVector], XMM0
                           // pOutputVector += OutputStride;
                           MOV     EDX, pOutputVector
                           ADD     EDX, [OutputStride]
                           MOV     pOutputVector,EDX
                end;
            end;
        end;
    end;

    // XM_SFENCE();

    Result := pOutputStream;
end;


{***************************************************************************
 *
 * Matrix
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------


function XMMatrixIsNaN(constref M: TXMMATRIX): boolean; assembler;
asm

           // Load in registers
           MOVUPS  XMM0, TXMVECTOR(TXMMATRIX([M]).r[0]) // vX
           MOVUPS  XMM1, TXMVECTOR(TXMMATRIX([M]).r[1]) // vY
           MOVUPS  XMM2, TXMVECTOR(TXMMATRIX([M]).r[2]) // vZ
           MOVUPS  XMM3, TXMVECTOR(TXMMATRIX([M]).r[3]) // vW
           // Test themselves to check for NaN
           CMPPS   XMM0, XMM0, $4
           CMPPS   XMM1, XMM1, $4
           CMPPS   XMM2, XMM2, $4
           CMPPS   XMM3, XMM3, $4
           // Or all the results
           ORPS    XMM0, XMM2 // _mm_or_ps(vX,vZ);
           ORPS    XMM1, XMM3// vY = _mm_or_ps(vY,vW);
           ORPS    XMM0, XMM1// vX = _mm_or_ps(vX,vY);
           // If any tested true, return true
           // return (_mm_movemask_ps(vX)!=0);
           MOVMSKPS EAX, XMM0
           TEST    EAX,EAX  // set ZF to 1 if eax == 0
           JE      @zero  // jump if ZF == 1 (eax==0)
           mov [result],$1  // return TRUE
           @zero:
           MOV     [result],$0  // return FALSE
end;



function XMMatrixIsInfinite(M: TXMMATRIX): boolean; assembler;
asm

           // Mask off the sign bits
           //  vTemp1 = _mm_and_ps(M.r[0],g_XMAbsMask);
           MOVUPS  XMM1, TXMMATRIX([M]).r0
           ANDPS   XMM1, [g_XMAbsMask]
           //  vTemp2 = _mm_and_ps(M.r[1],g_XMAbsMask);
           MOVUPS  XMM2, TXMMATRIX([M]).r1
           ANDPS   XMM2, [g_XMAbsMask]
           //  vTemp3 = _mm_and_ps(M.r[2],g_XMAbsMask);
           MOVUPS  XMM3, TXMMATRIX([M]).r2
           ANDPS   XMM3, [g_XMAbsMask]
           //  vTemp4 = _mm_and_ps(M.r[3],g_XMAbsMask);
           MOVUPS  XMM4, TXMMATRIX([M]).r3
           ANDPS   XMM4, [g_XMAbsMask]
           // Compare to infinity
           // vTemp1 = _mm_cmpeq_ps(vTemp1,g_XMInfinity);
           CMPPS   XMM1, [g_XMInfinity], 0
           // vTemp2 = _mm_cmpeq_ps(vTemp2,g_XMInfinity);
           CMPPS   XMM2, [g_XMInfinity], 0
           // vTemp3 = _mm_cmpeq_ps(vTemp3,g_XMInfinity);
           CMPPS   XMM3, [g_XMInfinity], 0
           // vTemp4 = _mm_cmpeq_ps(vTemp4,g_XMInfinity);
           CMPPS   XMM4, [g_XMInfinity], 0
           // Or the answers together
           // vTemp1 = _mm_or_ps(vTemp1,vTemp2);
           ORPS    XMM1, XMM2
           // vTemp3 = _mm_or_ps(vTemp3,vTemp4);
           ORPS    XMM3, XMM4
           // vTemp1 = _mm_or_ps(vTemp1,vTemp3);
           ORPS    XMM1, XMM3
           // If any are infinity, the signs are true.
           // return (_mm_movemask_ps(vTemp1)!=0);
           MOVMSKPS EAX, XMM1
           CMP     EAX, $0
           SETNE    [result]

end;



function XMMatrixIsIdentity(M: TXMMATRIX): boolean; assembler;
asm
           // vTemp1 = _mm_cmpeq_ps(M.r[0],g_XMIdentityR0);
           MOVUPS  XMM1,  TXMMATRIX([M]).r0
           CMPPS   XMM1, [g_XMIdentityR0], 0
           //  vTemp2 = _mm_cmpeq_ps(M.r[1],g_XMIdentityR1);
           MOVUPS  XMM2,  TXMMATRIX([M]).r1
           CMPPS   XMM2, [g_XMIdentityR1], 0
           //  vTemp3 = _mm_cmpeq_ps(M.r[2],g_XMIdentityR2);
           MOVUPS  XMM3,  TXMMATRIX([M]).r2
           CMPPS   XMM3, [g_XMIdentityR2], 0
           //  vTemp4 = _mm_cmpeq_ps(M.r[3],g_XMIdentityR3);
           MOVUPS  XMM4,  TXMMATRIX([M]).r3
           CMPPS   XMM4, [g_XMIdentityR3], 0
           // vTemp1 = _mm_and_ps(vTemp1,vTemp2);
           ANDPS   XMM1, XMM2
           // vTemp3 = _mm_and_ps(vTemp3,vTemp4);
           ANDPS   XMM3, XMM4
           // vTemp1 = _mm_and_ps(vTemp1,vTemp3);
           ANDPS   XMM1, XMM3
           // return (_mm_movemask_ps(vTemp1)==0x0f);
           MOVMSKPS EAX, XMM0
           AND     EAX, $F
           CMP     EAX, $F
           SETE    [result]

end;


//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------

{$IF DEFINED(_XM_AVX_INTRINSICS_)}

function XMMatrixMultiply(M1: TXMMATRIX; M2: TXMMATRIX): TXMMATRIX; assembler;
asm
           // Load the M2 to XMM4 to XMM7, a littel bit different than on MSDN, but more efficiency
           MOVUPS  XMM4, TXMVECTOR(TXMMATRIX([M2]).r[0]) // vX
           MOVUPS  XMM5, TXMVECTOR(TXMMATRIX([M2]).r[1]) // vY
           MOVUPS  XMM6, TXMVECTOR(TXMMATRIX([M2]).r[2]) // vZ
           MOVUPS  XMM7, TXMVECTOR(TXMMATRIX([M2]).r[3]) // vW

           // Splat the component X,Y,Z then W
           VBROADCASTSS XMM0, TXMVECTOR(TXMMATRIX([M1]).r[0]).f32[0] // vX
           VBROADCASTSS XMM1, TXMVECTOR(TXMMATRIX([M1]).r[0]).f32[1] // vY
           VBROADCASTSS XMM2, TXMVECTOR(TXMMATRIX([M1]).r[0]).f32[2] // vZ
           VBROADCASTSS XMM3, TXMVECTOR(TXMMATRIX([M1]).r[0]).f32[3] // vW
           // Perform the operation on the first row
           MULPS   XMM0, XMM4 // vX
           MULPS   XMM1, XMM5 // vY
           MULPS   XMM2, XMM6 // vZ
           MULPS   XMM3, XMM7 // vW
           // Perform a binary add to reduce cumulative errors
           ADDPS   XMM0, XMM2  // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM1, XMM3  // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM0, XMM1  // vX = _mm_add_ps(vX,vY);
           MOVUPS  TXMMATRIX([Result]).r[0], XMM0   //  mResult.r[0] = vX;
           // Repeat for the other 3 rows
           VBROADCASTSS XMM0, TXMVECTOR(TXMMATRIX([M1]).r[1]).f32[0] // vX
           VBROADCASTSS XMM1, TXMVECTOR(TXMMATRIX([M1]).r[1]).f32[1] // vY
           VBROADCASTSS XMM2, TXMVECTOR(TXMMATRIX([M1]).r[1]).f32[2] // vZ
           VBROADCASTSS XMM3, TXMVECTOR(TXMMATRIX([M1]).r[1]).f32[3] // vW
           MULPS   XMM0, XMM4 // vX
           MULPS   XMM1, XMM5 // vY
           MULPS   XMM2, XMM6 // vZ
           MULPS   XMM3, XMM7 // vW
           ADDPS   XMM0, XMM2  // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM1, XMM3  // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM0, XMM1  // vX = _mm_add_ps(vX,vY);
           MOVUPS  TXMMATRIX([Result]).r[1], XMM0   //  mResult.r[1] = vX;

           // Repeat for the other 2 rows
           VBROADCASTSS XMM0, TXMVECTOR(TXMMATRIX([M1]).r[2]).f32[0] // vX
           VBROADCASTSS XMM1, TXMVECTOR(TXMMATRIX([M1]).r[2]).f32[1] // vY
           VBROADCASTSS XMM2, TXMVECTOR(TXMMATRIX([M1]).r[2]).f32[2] // vZ
           VBROADCASTSS XMM3, TXMVECTOR(TXMMATRIX([M1]).r[2]).f32[3] // vW
           MULPS   XMM0, XMM4 // vX
           MULPS   XMM1, XMM5 // vY
           MULPS   XMM2, XMM6 // vZ
           MULPS   XMM3, XMM7 // vW
           ADDPS   XMM0, XMM2  // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM1, XMM3  // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM0, XMM1  // vX = _mm_add_ps(vX,vY);
           MOVUPS  TXMMATRIX([Result]).r[2], XMM0   //  mResult.r[2] = vX;
           // Repeat for the last row
           VBROADCASTSS XMM0, TXMVECTOR(TXMMATRIX([M1]).r[3]).f32[0] // vX
           VBROADCASTSS XMM1, TXMVECTOR(TXMMATRIX([M1]).r[3]).f32[1] // vY
           VBROADCASTSS XMM2, TXMVECTOR(TXMMATRIX([M1]).r[3]).f32[2] // vZ
           VBROADCASTSS XMM3, TXMVECTOR(TXMMATRIX([M1]).r[3]).f32[3] // vW
           MULPS   XMM0, XMM4 // vX
           MULPS   XMM1, XMM5 // vY
           MULPS   XMM2, XMM6 // vZ
           MULPS   XMM3, XMM7 // vW
           ADDPS   XMM0, XMM2  // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM1, XMM3  // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM0, XMM1  // vX = _mm_add_ps(vX,vY);
           MOVUPS  TXMMATRIX([Result]).r[3], XMM0   //  mResult.r[2] = vX;
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMMatrixMultiply(M1: TXMMATRIX; M2: TXMMATRIX): TXMMATRIX; assembler;
asm
           // Splat the component X,Y,Z then W
           // Use vW to hold the original row
           // vW = M1.r[0];
           MOVUPS  XMM3, TXMMATRIX([M1]).r0
           //  vX = XM_PERMUTE_PS(vW,_MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM0,XMM3
           MOVUPS  XMM1,XMM3
           MOVUPS  XMM2,XMM3

           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           //  vY = XM_PERMUTE_PS(vW,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           //  vZ = XM_PERMUTE_PS(vW,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
           // vW = XM_PERMUTE_PS(vW,_MM_SHUFFLE(3,3,3,3));
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           // Perform the operation on the first row
           // vX = _mm_mul_ps(vX,M2.r[0]);
           MULPS   XMM0, TXMMATRIX([M2]).r0
           // vY = _mm_mul_ps(vY,M2.r[1]);
           MULPS   XMM1, TXMMATRIX([M2]).r1
           // vZ = _mm_mul_ps(vZ,M2.r[2]);
           MULPS   XMM2, TXMMATRIX([M2]).r2
           // vW = _mm_mul_ps(vW,M2.r[3]);
           MULPS   XMM3, TXMMATRIX([M2]).r3
           // Perform a binary add to reduce cumulative errors
           // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM0, XMM2
           // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM1, XMM3
           // vX = _mm_add_ps(vX,vY);
           ADDPS   XMM0, XMM1
           // mResult.r[0] = vX;
           MOVUPS  TXMMATRIX([result]).r0, XMM0
           // Repeat for the other 3 rows
           //  vW = M1.r[1];
           MOVUPS  XMM3, TXMMATRIX([M1]).r1
           // vX = XM_PERMUTE_PS(vW,_MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM0,XMM3
           MOVUPS  XMM1,XMM3
           MOVUPS  XMM2,XMM3
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // vY = XM_PERMUTE_PS(vW,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // vZ = XM_PERMUTE_PS(vW,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
           // vW = XM_PERMUTE_PS(vW,_MM_SHUFFLE(3,3,3,3));
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           // vX = _mm_mul_ps(vX,M2.r[0]);
           MULPS   XMM0, TXMMATRIX([M2]).r0
           // vY = _mm_mul_ps(vY,M2.r[1]);
           MULPS   XMM1, TXMMATRIX([M2]).r1
           // vZ = _mm_mul_ps(vZ,M2.r[2]);
           MULPS   XMM2, TXMMATRIX([M2]).r2
           // vW = _mm_mul_ps(vW,M2.r[3]);
           MULPS   XMM3, TXMMATRIX([M2]).r3
           // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM0, XMM2
           // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM1, XMM3
           // vX = _mm_add_ps(vX,vY);
           ADDPS   XMM0, XMM1
           // mResult.r[1] = vX;
           MOVUPS  TXMMATRIX([result]).r1, XMM0
           // vW = M1.r[2];
           MOVUPS  XMM3, TXMMATRIX([M1]).r2
           // vX = XM_PERMUTE_PS(vW,_MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM0,XMM3
           MOVUPS  XMM1,XMM3
           MOVUPS  XMM2,XMM3
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // vY = XM_PERMUTE_PS(vW,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // vZ = XM_PERMUTE_PS(vW,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
           // vW = XM_PERMUTE_PS(vW,_MM_SHUFFLE(3,3,3,3));
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           // vX = _mm_mul_ps(vX,M2.r[0]);
           MULPS   XMM0, TXMMATRIX([M2]).r0
           // vY = _mm_mul_ps(vY,M2.r[1]);
           MULPS   XMM1, TXMMATRIX([M2]).r1
           // vZ = _mm_mul_ps(vZ,M2.r[2]);
           MULPS   XMM2, TXMMATRIX([M2]).r2
           // vW = _mm_mul_ps(vW,M2.r[3]);
           MULPS   XMM3, TXMMATRIX([M2]).r3
           // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM0, XMM2
           // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM1, XMM3
           // vX = _mm_add_ps(vX,vY);
           ADDPS   XMM0, XMM1
           // mResult.r[2] = vX;
           MOVUPS  TXMMATRIX([result]).r2, XMM0
           // vW = M1.r[3];
           MOVUPS  XMM3, TXMMATRIX([M1]).r3
           // vX = XM_PERMUTE_PS(vW,_MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM0,XMM3
           MOVUPS  XMM1,XMM3
           MOVUPS  XMM2,XMM3
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // vY = XM_PERMUTE_PS(vW,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           // vZ = XM_PERMUTE_PS(vW,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
           // vW = XM_PERMUTE_PS(vW,_MM_SHUFFLE(3,3,3,3));
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           // vX = _mm_mul_ps(vX,M2.r[0]);
           MULPS   XMM0, TXMMATRIX([M2]).r0
           // vY = _mm_mul_ps(vY,M2.r[1]);
           MULPS   XMM1, TXMMATRIX([M2]).r1
           // vZ = _mm_mul_ps(vZ,M2.r[2]);
           MULPS   XMM2, TXMMATRIX([M2]).r2
           // vW = _mm_mul_ps(vW,M2.r[3]);
           MULPS   XMM3, TXMMATRIX([M2]).r3
           // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM0, XMM2
           // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM1, XMM3
           // vX = _mm_add_ps(vX,vY);
           ADDPS   XMM0, XMM1
           // mResult.r[3] = vX;
           MOVUPS  TXMMATRIX([result]).r3, XMM0
           // return mResult;
end;

{$ENDIF}


{$IF DEFINED(_XM_AVX_INTRINSICS_)}

function XMMatrixMultiplyTranspose(M1: TXMMATRIX; M2: TXMMATRIX): TXMMATRIX; assembler;
var
    r0, r1, r2, r3: TXMVECTOR;
asm
           // Load the M2 to XMM4 to XMM7, a littel bit different than on MSDN, but more efficiency
           MOVUPS  XMM4, TXMVECTOR(TXMMATRIX([M2]).r[0]) // vX
           MOVUPS  XMM5, TXMVECTOR(TXMMATRIX([M2]).r[1]) // vY
           MOVUPS  XMM6, TXMVECTOR(TXMMATRIX([M2]).r[2]) // vZ
           MOVUPS  XMM7, TXMVECTOR(TXMMATRIX([M2]).r[3]) // vW


           // Splat the component X,Y,Z then W
           VBROADCASTSS XMM0, TXMVECTOR(TXMMATRIX([M1]).r[0]).f32[0] // vX
           VBROADCASTSS XMM1, TXMVECTOR(TXMMATRIX([M1]).r[0]).f32[1] // vY
           VBROADCASTSS XMM2, TXMVECTOR(TXMMATRIX([M1]).r[0]).f32[2] // vZ
           VBROADCASTSS XMM3, TXMVECTOR(TXMMATRIX([M1]).r[0]).f32[3] // vW
           // Perform the operation on the first row
           MULPS   XMM0, XMM4 // vX
           MULPS   XMM1, XMM5 // vY
           MULPS   XMM2, XMM6 // vZ
           MULPS   XMM3, XMM7 // vW
           // Perform a binary add to reduce cumulative errors
           ADDPS   XMM0, XMM2  // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM1, XMM3  // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM0, XMM1  // vX = _mm_add_ps(vX,vY);
           MOVUPS  TXMMATRIX([r0]), XMM0   //  r0 = vX;
           // Repeat for the other 3 rows
           VBROADCASTSS XMM0, TXMVECTOR(TXMMATRIX([M1]).r[1]).f32[0] // vX
           VBROADCASTSS XMM1, TXMVECTOR(TXMMATRIX([M1]).r[1]).f32[1] // vY
           VBROADCASTSS XMM2, TXMVECTOR(TXMMATRIX([M1]).r[1]).f32[2] // vZ
           VBROADCASTSS XMM3, TXMVECTOR(TXMMATRIX([M1]).r[1]).f32[3] // vW
           MULPS   XMM0, XMM4 // vX
           MULPS   XMM1, XMM5 // vY
           MULPS   XMM2, XMM6 // vZ
           MULPS   XMM3, XMM7 // vW
           ADDPS   XMM0, XMM2  // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM1, XMM3  // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM0, XMM1  // vX = _mm_add_ps(vX,vY);
           MOVUPS  TXMVECTOR([r1]), XMM0   //  r1 = vX;

           VBROADCASTSS XMM0, TXMVECTOR(TXMMATRIX([M1]).r[2]).f32[0] // vX
           VBROADCASTSS XMM1, TXMVECTOR(TXMMATRIX([M1]).r[2]).f32[1] // vY
           VBROADCASTSS XMM2, TXMVECTOR(TXMMATRIX([M1]).r[2]).f32[2] // vZ
           VBROADCASTSS XMM3, TXMVECTOR(TXMMATRIX([M1]).r[2]).f32[3] // vW
           MULPS   XMM0, XMM4 // vX
           MULPS   XMM1, XMM5 // vY
           MULPS   XMM2, XMM6 // vZ
           MULPS   XMM3, XMM7 // vW
           ADDPS   XMM0, XMM2  // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM1, XMM3  // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM0, XMM1  // vX = _mm_add_ps(vX,vY);
           MOVUPS  TXMVECTOR([r2]), XMM0   //  r1 = vX;

           VBROADCASTSS XMM0, TXMVECTOR(TXMMATRIX([M1]).r[3]).f32[0] // vX
           VBROADCASTSS XMM1, TXMVECTOR(TXMMATRIX([M1]).r[3]).f32[1] // vY
           VBROADCASTSS XMM2, TXMVECTOR(TXMMATRIX([M1]).r[3]).f32[2] // vZ
           VBROADCASTSS XMM3, TXMVECTOR(TXMMATRIX([M1]).r[3]).f32[3] // vW
           MULPS   XMM0, XMM4 // vX
           MULPS   XMM1, XMM5 // vY
           MULPS   XMM2, XMM6 // vZ
           MULPS   XMM3, XMM7 // vW
           ADDPS   XMM0, XMM2  // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM1, XMM3  // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM0, XMM1  // vX = _mm_add_ps(vX,vY);
           MOVUPS  TXMVECTOR([r3]), XMM0   //  r1 = vX;

           MOVUPS  XMM0,TXMVECTOR([r0])
           MOVUPS  XMM1,TXMVECTOR([r1])
           MOVUPS  XMM2,TXMVECTOR([r2])
           MOVUPS  XMM3,TXMVECTOR([r3])

           // x.x,x.y,y.x,y.y
           VSHUFPS XMM4, XMM0, XMM1, _MM_SHUFFLE_1_0_1_0 // vTemp1 = _mm_shuffle_ps(r0,r1,_MM_SHUFFLE(1,0,1,0));
           // x.z,x.w,y.z,y.w
           VSHUFPS XMM5, XMM0, XMM1, _MM_SHUFFLE_3_2_3_2 // vTemp3 = _mm_shuffle_ps(r0,r1,_MM_SHUFFLE(3,2,3,2));
           // z.x,z.y,w.x,w.y
           VSHUFPS XMM6, XMM2, XMM3, _MM_SHUFFLE_1_0_1_0 // vTemp2 = _mm_shuffle_ps(r2,r3,_MM_SHUFFLE(1,0,1,0));
           // z.z,z.w,w.z,w.w
           VSHUFPS XMM7, XMM2, XMM3, _MM_SHUFFLE_3_2_3_2 // vTemp4 = _mm_shuffle_ps(r2,r3,_MM_SHUFFLE(3,2,3,2));


           // x.x,y.x,z.x,w.x
           VSHUFPS XMM0, XMM4, XMM5, _MM_SHUFFLE_2_0_2_0 // mResult.r[0] = _mm_shuffle_ps(vTemp1, vTemp2,_MM_SHUFFLE(2,0,2,0));
           // x.y,y.y,z.y,w.y
           VSHUFPS XMM1, XMM4, XMM5, _MM_SHUFFLE_3_1_3_1 // mResult.r[1] = _mm_shuffle_ps(vTemp1, vTemp2,_MM_SHUFFLE(3,1,3,1));
           // x.z,y.z,z.z,w.z
           VSHUFPS XMM2, XMM6, XMM7, _MM_SHUFFLE_2_0_2_0 // mResult.r[2] = _mm_shuffle_ps(vTemp3, vTemp4,_MM_SHUFFLE(2,0,2,0));
           // x.w,y.w,z.w,w.w
           VSHUFPS XMM3, XMM6, XMM7, _MM_SHUFFLE_3_1_3_1 // mResult.r[3] = _mm_shuffle_ps(vTemp3, vTemp4,_MM_SHUFFLE(3,1,3,1));

           // return Result;
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[0]) ,XMM0
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[1]) ,XMM1
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[2]) ,XMM2
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[3]) ,XMM3
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMMatrixMultiplyTranspose(M1: TXMMATRIX; M2: TXMMATRIX): TXMMATRIX; assembler;
var
    r0, r1, r2, r3: TXMVECTOR;
asm
           // Splat the component X,Y,Z then W
           // Use vW to hold the original row
           MOVUPS  XMM3, TXMVECTOR(TXMMATRIX([M1]).r[0]) // vW = M1.r[0];
           MOVUPS  XMM0, XMM3
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0 // vX = XM_PERMUTE_PS(vW,_MM_SHUFFLE(0,0,0,0));
           MOVUPS  XMM1, XMM3
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1 //  vY = XM_PERMUTE_PS(vW,_MM_SHUFFLE(1,1,1,1));
           MOVUPS  XMM2, XMM3
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2 //  vZ = XM_PERMUTE_PS(vW,_MM_SHUFFLE(2,2,2,2));
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3 // vW = XM_PERMUTE_PS(vW,_MM_SHUFFLE(3,3,3,3));
           // Perform the operation on the first row
           MULPS   XMM0, TXMVECTOR(TXMMATRIX([M2]).r[0]) // vX = _mm_mul_ps(vX,M2.r[0]);
           MULPS   XMM1, TXMVECTOR(TXMMATRIX([M2]).r[1]) // vY = _mm_mul_ps(vY,M2.r[1]);
           MULPS   XMM2, TXMVECTOR(TXMMATRIX([M2]).r[2]) // vZ = _mm_mul_ps(vZ,M2.r[2]);
           MULPS   XMM3, TXMVECTOR(TXMMATRIX([M2]).r[3]) // vW = _mm_mul_ps(vW,M2.r[3]);
           // Perform a binary add to reduce cumulative errors
           ADDPS   XMM0, XMM2 // vX = _mm_add_ps(vX,vZ);
           ADDPS   XMM1, XMM3 // vY = _mm_add_ps(vY,vW);
           ADDPS   XMM0, XMM1 // vX = _mm_add_ps(vX,vY);
           MOVUPS  [r0], XMM0 // r0 = vX;
           // Repeat for the other 3 rows
           MOVUPS  XMM3, TXMVECTOR(TXMMATRIX([M1]).r[1]) // vW = M1.r[1];
           MOVUPS  XMM0, XMM3
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           MOVUPS  XMM1, XMM3
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           MOVUPS  XMM2, XMM3
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           MULPS   XMM0, TXMVECTOR(TXMMATRIX([M2]).r[0])
           MULPS   XMM1, TXMVECTOR(TXMMATRIX([M2]).r[1])
           MULPS   XMM2, TXMVECTOR(TXMMATRIX([M2]).r[2])
           MULPS   XMM3, TXMVECTOR(TXMMATRIX([M2]).r[3])
           ADDPS   XMM0, XMM2
           ADDPS   XMM1, XMM3
           ADDPS   XMM0, XMM1
           MOVUPS  [r1], XMM0 // r1 = vX;

           MOVUPS  XMM3, TXMVECTOR(TXMMATRIX([M1]).r[2]) // vW = M1.r[2];
           MOVUPS  XMM0, XMM3
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           MOVUPS  XMM1, XMM3
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           MOVUPS  XMM2, XMM3
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           MULPS   XMM0, TXMVECTOR(TXMMATRIX([M2]).r[0])
           MULPS   XMM1, TXMVECTOR(TXMMATRIX([M2]).r[1])
           MULPS   XMM2, TXMVECTOR(TXMMATRIX([M2]).r[2])
           MULPS   XMM3, TXMVECTOR(TXMMATRIX([M2]).r[3])
           ADDPS   XMM0, XMM2
           ADDPS   XMM1, XMM3
           ADDPS   XMM0, XMM1
           MOVUPS  [r2], XMM0 // r1 = vX;

           MOVUPS  XMM3, TXMVECTOR(TXMMATRIX([M1]).r[2]) // vW = M1.r[3];
           MOVUPS  XMM0, XMM3
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           MOVUPS  XMM1, XMM3
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           MOVUPS  XMM2, XMM3
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_3_3_3_3
           MULPS   XMM0, TXMVECTOR(TXMMATRIX([M2]).r[0])
           MULPS   XMM1, TXMVECTOR(TXMMATRIX([M2]).r[1])
           MULPS   XMM2, TXMVECTOR(TXMMATRIX([M2]).r[2])
           MULPS   XMM3, TXMVECTOR(TXMMATRIX([M2]).r[3])
           ADDPS   XMM0, XMM2
           ADDPS   XMM1, XMM3
           ADDPS   XMM0, XMM1
           MOVUPS  [r3], XMM0 // r1 = vX;



           // x.x,x.y,y.x,y.y
           MOVUPS  XMM0 ,[r0]
           SHUFPS  XMM0, [r1], _MM_SHUFFLE_1_0_1_0 // vTemp1 = _mm_shuffle_ps(r0,r1,_MM_SHUFFLE(1,0,1,0));
           // x.z,x.w,y.z,y.w
           MOVUPS  XMM2 ,[r0]
           SHUFPS  XMM2, [r1], _MM_SHUFFLE_3_2_3_2 // vTemp3 = _mm_shuffle_ps(r0,r1,_MM_SHUFFLE(3,2,3,2));
           // z.x,z.y,w.x,w.y
           MOVUPS  XMM1 ,[r2]
           SHUFPS  XMM1, [r3], _MM_SHUFFLE_1_0_1_0 // vTemp2 = _mm_shuffle_ps(r2,r3,_MM_SHUFFLE(1,0,1,0));
           // z.z,z.w,w.z,w.w
           MOVUPS  XMM3 ,[r2]
           SHUFPS  XMM3, [r3], _MM_SHUFFLE_3_2_3_2 // vTemp4 = _mm_shuffle_ps(r2,r3,_MM_SHUFFLE(3,2,3,2));

           // x.x,y.x,z.x,w.x
           MOVUPS  XMM4, XMM0
           SHUFPS  XMM4, XMM1, _MM_SHUFFLE_2_0_2_0 // mResult.r[0] = _mm_shuffle_ps(vTemp1, vTemp2,_MM_SHUFFLE(2,0,2,0));
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[0]), XMM4
           // x.y,y.y,z.y,w.y
           SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_1_3_1 // mResult.r[1] = _mm_shuffle_ps(vTemp1, vTemp2,_MM_SHUFFLE(3,1,3,1));
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[1]), XMM0
           // x.z,y.z,z.z,w.z
           MOVUPS  XMM4, XMM2
           SHUFPS  XMM4, XMM3, _MM_SHUFFLE_2_0_2_0 // mResult.r[2] = _mm_shuffle_ps(vTemp3, vTemp4,_MM_SHUFFLE(2,0,2,0));
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[2]), XMM4
           // x.w,y.w,z.w,w.w
           SHUFPS  XMM2, XMM3, _MM_SHUFFLE_3_1_3_1 // mResult.r[3] = _mm_shuffle_ps(vTemp3, vTemp4,_MM_SHUFFLE(3,1,3,1));
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[3]), XMM2
end;
{$ENDIF}


function XMMatrixTranspose(M: TXMMATRIX): TXMMATRIX; assembler;
asm
           MOVUPS  XMM0, TXMVECTOR(TXMMATRIX([M]).r[0])
           MOVUPS  XMM1, TXMVECTOR(TXMMATRIX([M]).r[1])
           MOVAPS  XMM2,XMM0
           MOVAPS  XMM3,XMM1

           MOVUPS  XMM4, TXMVECTOR(TXMMATRIX([M]).r[2])
           MOVUPS  XMM5, TXMVECTOR(TXMMATRIX([M]).r[3])
           MOVAPS  XMM6,XMM4
           MOVAPS  XMM7,XMM5

           // x.x,x.y,y.x,y.y
           SHUFPS  XMM0, XMM1, _MM_SHUFFLE_1_0_1_0 // vTemp1 = XMM0 = _mm_shuffle_ps(M.r[0],M.r[1],_MM_SHUFFLE(1,0,1,0));
           // x.z,x.w,y.z,y.w
           SHUFPS  XMM2, XMM3, _MM_SHUFFLE_3_2_3_2 // vTemp3 = XMM2 = _mm_shuffle_ps(M.r[0],M.r[1],_MM_SHUFFLE(3,2,3,2));
           // z.x,z.y,w.x,w.y
           SHUFPS  XMM4, XMM5, _MM_SHUFFLE_3_2_3_2 // vTemp2 = XMM4 = _mm_shuffle_ps(M.r[2],M.r[3],_MM_SHUFFLE(1,0,1,0));
           // z.z,z.w,w.z,w.w
           SHUFPS  XMM6, XMM7, _MM_SHUFFLE_3_2_3_2 // vTemp4 = XMM6 = _mm_shuffle_ps(M.r[2],M.r[3],_MM_SHUFFLE(3,2,3,2));

           MOVAPS  XMM1,XMM0  // vTemp1 = XMM0 = XMM1
           MOVAPS  XMM5,XMM4  // vTemp2 = XMM4 = XMM5

           MOVAPS  XMM3,XMM2  // vTemp3 = XMM2 = XMM3
           MOVAPS  XMM7,XMM6  // vTemp4 = XMM6 = XMM7

           // x.x,y.x,z.x,w.x
           SHUFPS  XMM0, XMM4, _MM_SHUFFLE_2_0_2_0 // mResult.r[0] = _mm_shuffle_ps(vTemp1, vTemp2,_MM_SHUFFLE(2,0,2,0));
           // x.y,y.y,z.y,w.y
           SHUFPS  XMM1, XMM5, _MM_SHUFFLE_3_1_3_1 // mResult.r[1] = _mm_shuffle_ps(vTemp1, vTemp2,_MM_SHUFFLE(3,1,3,1));
           // x.z,y.z,z.z,w.z
           SHUFPS  XMM2, XMM6, _MM_SHUFFLE_2_0_2_0 // mResult.r[2] = _mm_shuffle_ps(vTemp3, vTemp4,_MM_SHUFFLE(2,0,2,0));
           // x.w,y.w,z.w,w.w
           SHUFPS  XMM3, XMM7, _MM_SHUFFLE_3_1_3_1 // mResult.r[3] = _mm_shuffle_ps(vTemp3, vTemp4,_MM_SHUFFLE(3,1,3,1));

           // return mResult;
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[0]) ,XMM0
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[1]) ,XMM1
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[2]) ,XMM2
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[3]) ,XMM3
end;



function XMMatrixInverse(out pDeterminant: TXMVECTOR; M: TXMMATRIX): TXMMATRIX; inline;
var
    MT: TXMMATRIX;
    D0, D1, D2: TXMVECTOR;
    V10, V11, V12, V13: TXMVECTOR;
    C0, C2, C4, C6: TXMVECTOR;
    C1, C3, C5, C7: TXMVECTOR;
    vTemp: TXMVECTOR;
begin
    MT := XMMatrixTranspose(M);
    asm
               // XMVECTOR V00 = XM_PERMUTE_PS(MT.r[2],_MM_SHUFFLE(1,1,0,0));
               MOVUPS  XMM0, TXMVECTOR(TXMMATRIX([MT]).r[2])
               MOVAPS  XMM1,XMM0
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_1_1_0_0  // XMVECTOR V00 = XMM0

               // XMVECTOR V10 = XM_PERMUTE_PS(MT.r[3],_MM_SHUFFLE(3,2,3,2));
               MOVUPS  XMM2, TXMVECTOR(TXMMATRIX([MT]).r[3])
               MOVAPS  XMM3,XMM2
               SHUFPS  XMM2, XMM3, _MM_SHUFFLE_3_2_3_2  // XMVECTOR V10 = XMM2

               // XMVECTOR V01 = XM_PERMUTE_PS(MT.r[0],_MM_SHUFFLE(1,1,0,0));
               MOVUPS  XMM4, TXMVECTOR(TXMMATRIX([MT]).r[0])
               MOVAPS  XMM5,XMM4
               SHUFPS  XMM4, XMM5, _MM_SHUFFLE_1_1_0_0  // XMVECTOR V01 = XMM4

               // XMVECTOR V11 = XM_PERMUTE_PS(MT.r[1],_MM_SHUFFLE(3,2,3,2));
               MOVUPS  XMM6, TXMVECTOR(TXMMATRIX([MT]).r[1])
               MOVAPS  XMM7,XMM6
               SHUFPS  XMM6, XMM7, _MM_SHUFFLE_3_2_3_2  // XMVECTOR V11 = XMM6


               // XMVECTOR V02 = _mm_shuffle_ps(MT.r[2], MT.r[0],_MM_SHUFFLE(2,0,2,0));
               MOVUPS  XMM1, TXMVECTOR(TXMMATRIX([MT]).r[2])
               MOVUPS  XMM3, TXMVECTOR(TXMMATRIX([MT]).r[0])
               SHUFPS  XMM1, XMM3, _MM_SHUFFLE_2_0_2_0  // XMVECTOR V02 = XMM1

               // XMVECTOR V12 = _mm_shuffle_ps(MT.r[3], MT.r[1],_MM_SHUFFLE(3,1,3,1));
               MOVUPS  XMM3, TXMVECTOR(TXMMATRIX([MT]).r[3])
               MOVUPS  XMM5, TXMVECTOR(TXMMATRIX([MT]).r[1])
               SHUFPS  XMM3, XMM5, _MM_SHUFFLE_3_1_3_1  // XMVECTOR V12 = XMM3


               // XMVECTOR D0 = _mm_mul_ps(V00,V10);
               MULPS   XMM0, XMM2
               MOVUPS  [D0], XMM0

               // XMVECTOR D1 = _mm_mul_ps(V01,V11);
               MULPS   XMM4, XMM6
               MOVUPS  [D1], XMM4

               // XMVECTOR D2 = _mm_mul_ps(V02,V12);
               MULPS   XMM1, XMM3
               MOVUPS  [D2], XMM1

               // V00 = XM_PERMUTE_PS(MT.r[2],_MM_SHUFFLE(3,2,3,2));
               MOVUPS  XMM0, TXMVECTOR(TXMMATRIX([MT]).r[2])
               MOVAPS  XMM1,XMM0
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_2_3_2 // V00 = XMM0

               // V10 = XM_PERMUTE_PS(MT.r[3],_MM_SHUFFLE(1,1,0,0));
               MOVUPS  XMM2, TXMVECTOR(TXMMATRIX([MT]).r[3])
               MOVAPS  XMM3,XMM2
               SHUFPS  XMM2, XMM3, _MM_SHUFFLE_1_1_0_0 // V10 = XMM2

               // V01 = XM_PERMUTE_PS(MT.r[0],_MM_SHUFFLE(3,2,3,2));
               MOVUPS  XMM4, TXMVECTOR(TXMMATRIX([MT]).r[0])
               MOVAPS  XMM5,XMM4
               SHUFPS  XMM4, XMM5, _MM_SHUFFLE_3_2_3_2 // V01 = XMM4

               // V11 = XM_PERMUTE_PS(MT.r[1],_MM_SHUFFLE(1,1,0,0));
               MOVUPS  XMM6, TXMVECTOR(TXMMATRIX([MT]).r[1])
               MOVAPS  XMM7,XMM6
               SHUFPS  XMM6, XMM7, _MM_SHUFFLE_1_1_0_0 // V11 = XMM6

               // V02 = _mm_shuffle_ps(MT.r[2],MT.r[0],_MM_SHUFFLE(3,1,3,1));
               MOVUPS  XMM1, TXMVECTOR(TXMMATRIX([MT]).r[2])
               MOVUPS  XMM3, TXMVECTOR(TXMMATRIX([MT]).r[0])
               SHUFPS  XMM1, XMM3, _MM_SHUFFLE_3_1_3_1 // V02 = XMM1

               // V12 = _mm_shuffle_ps(MT.r[3],MT.r[1],_MM_SHUFFLE(2,0,2,0));
               MOVUPS  XMM3, TXMVECTOR(TXMMATRIX([MT]).r[3])
               MOVUPS  XMM5, TXMVECTOR(TXMMATRIX([MT]).r[1])
               SHUFPS  XMM3, XMM5, _MM_SHUFFLE_2_0_2_0 // V12 = XMM3

               // V00 = _mm_mul_ps(V00,V10);
               MULPS   XMM0, XMM2 // V00 = XMM0

               // V01 = _mm_mul_ps(V01,V11);
               MULPS   XMM4, XMM6  // V01 = XMM4

               // V02 = _mm_mul_ps(V02,V12);
               MULPS   XMM1, XMM3  // V02 = XMM1

               // D0 = _mm_sub_ps(D0,V00);
               MOVUPS  XMM2, [D0]
               SUBPS   XMM2, XMM0 // D0 = XMM2
               MOVUPS  [D0], XMM2

               // D1 = _mm_sub_ps(D1,V01);
               MOVUPS  XMM3, [D1]
               SUBPS   XMM3, XMM4  // D1 = XMM3
               MOVUPS  [D1], XMM3

               // D2 = _mm_sub_ps(D2,V02);
               MOVUPS  XMM5, [D2]
               SUBPS   XMM5, XMM1 // D2 = XMM5
               MOVUPS  [D2], XMM5

               // V11 = D0Y,D0W,D2Y,D2Y
               // V11 = _mm_shuffle_ps(D0,D2,_MM_SHUFFLE(1,1,3,1));
               SHUFPS  XMM2, XMM5, _MM_SHUFFLE_1_1_3_1 // Temp V11 = XMM2

               // V00 = XM_PERMUTE_PS(MT.r[1], _MM_SHUFFLE(1,0,2,1));
               MOVUPS  XMM0, TXMVECTOR(TXMMATRIX([MT]).r[1])
               MOVAPS  XMM1,XMM0
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_1_0_2_1  // V00 = XMM0

               // V10 = _mm_shuffle_ps(V11,D0,_MM_SHUFFLE(0,3,0,2));
               MOVUPS  XMM3, TXMVECTOR([D0])
               MOVAPS  XMM1,XMM2
               SHUFPS  XMM1, XMM3, _MM_SHUFFLE_0_3_0_2
               MOVUPS  TXMVECTOR([V10]), XMM1

               // V01 = XM_PERMUTE_PS(MT.r[0], _MM_SHUFFLE(0,1,0,2));
               MOVUPS  XMM4, TXMVECTOR(TXMMATRIX([MT]).r[0])
               MOVAPS  XMM5,XMM4
               SHUFPS  XMM4, XMM5, _MM_SHUFFLE_0_1_0_2  // V01 = XMM4

               // V11 = _mm_shuffle_ps(V11,D0,_MM_SHUFFLE(2,1,2,1));
               MOVUPS  XMM5, TXMVECTOR([D0])
               SHUFPS  XMM2, XMM5, _MM_SHUFFLE_2_1_2_1
               MOVUPS  TXMVECTOR([V11]), XMM2

               // V13 = D1Y,D1W,D2W,D2W
               // XMVECTOR V13 = _mm_shuffle_ps(D1,D2,_MM_SHUFFLE(3,3,3,1));
               MOVUPS  XMM6, TXMVECTOR([D1])
               MOVUPS  XMM7, TXMVECTOR([D2])
               SHUFPS  XMM6, XMM7, _MM_SHUFFLE_3_3_3_1
               MOVUPS  TXMVECTOR([V13]), XMM6           // temp V13 = XMM6

               // V02 = XM_PERMUTE_PS(MT.r[3], _MM_SHUFFLE(1,0,2,1));
               MOVUPS  XMM1, TXMVECTOR(TXMMATRIX([MT]).r[3])
               MOVAPS  XMM2,XMM1
               SHUFPS  XMM1, XMM2, _MM_SHUFFLE_1_0_2_1   // V02 = XMM1

               // V12 = _mm_shuffle_ps(V13,D1,_MM_SHUFFLE(0,3,0,2));
               MOVAPS  XMM2,XMM6
               MOVUPS  XMM3, TXMVECTOR([D1])
               SHUFPS  XMM2, XMM3, _MM_SHUFFLE_0_3_0_2
               MOVUPS  TXMVECTOR([V12]), XMM2

               //  V03 = XM_PERMUTE_PS(MT.r[2],_MM_SHUFFLE(0,1,0,2));
               MOVUPS  XMM2, TXMVECTOR(TXMMATRIX([MT]).r[2])
               MOVAPS  XMM3,XMM2
               SHUFPS  XMM2, XMM3, _MM_SHUFFLE_0_1_0_2   // V03 = XMM2

               // V13 = _mm_shuffle_ps(V13,D1,_MM_SHUFFLE(2,1,2,1));
               MOVUPS  XMM7, TXMVECTOR([D1])
               SHUFPS  XMM6, XMM7, _MM_SHUFFLE_2_1_2_1  // V13 = XMM6
               MOVUPS  TXMVECTOR([V13]), XMM6

               // XMVECTOR C0 = _mm_mul_ps(V00,V10);
               MULPS   XMM0, TXMVECTOR([V10])
               MOVUPS  TXMVECTOR([C0]), XMM0

               // XMVECTOR C2 = _mm_mul_ps(V01,V11);
               MULPS   XMM4, TXMVECTOR([V11])
               MOVUPS  TXMVECTOR([C2]), XMM4

               // XMVECTOR C4 = _mm_mul_ps(V02,V12);
               MULPS   XMM1, TXMVECTOR([V12])
               MOVUPS  TXMVECTOR([C4]), XMM1

               // XMVECTOR C6 = _mm_mul_ps(V03,V13);
               MULPS   XMM2, TXMVECTOR([V13])
               MOVUPS  TXMVECTOR([C6]), XMM2

               // V11 = D0X,D0Y,D2X,D2X
               // V11 = _mm_shuffle_ps(D0,D2,_MM_SHUFFLE(0,0,1,0));
               MOVUPS  XMM0, TXMVECTOR([D0])
               MOVUPS  XMM1, TXMVECTOR([D2])
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_0_0_1_0  // V11 = XMM0

               // V00 = XM_PERMUTE_PS(MT.r[1], _MM_SHUFFLE(2,1,3,2));
               MOVUPS  XMM1, TXMVECTOR(TXMMATRIX([MT]).r[1])
               MOVAPS  XMM2,XMM1
               SHUFPS  XMM1, XMM2, _MM_SHUFFLE_2_1_3_2  // V00 = XMM1

               // V10 = _mm_shuffle_ps(D0,V11,_MM_SHUFFLE(2,1,0,3));
               MOVUPS  XMM2, TXMVECTOR([D0])
               SHUFPS  XMM2, XMM0, _MM_SHUFFLE_2_1_0_3
               MOVUPS  TXMVECTOR([V10]), XMM2

               // V01 = XM_PERMUTE_PS(MT.r[0], _MM_SHUFFLE(1,3,2,3));
               MOVUPS  XMM3, TXMVECTOR(TXMMATRIX([MT]).r[0])
               MOVAPS  XMM4,XMM3
               SHUFPS  XMM3, XMM4, _MM_SHUFFLE_1_3_2_3 // V01 = XMM3

               // V11 = _mm_shuffle_ps(D0,V11,_MM_SHUFFLE(0,2,1,2));
               MOVUPS  XMM4, TXMVECTOR([D0])
               SHUFPS  XMM4, XMM0, _MM_SHUFFLE_0_2_1_2
               MOVUPS  TXMVECTOR([V11]), XMM4        // XMM0 is free

               // V13 = D1X,D1Y,D2Z,D2Z
               // V13 = _mm_shuffle_ps(D1,D2,_MM_SHUFFLE(2,2,1,0));
               MOVUPS  XMM6, TXMVECTOR([D1])
               MOVUPS  XMM7, TXMVECTOR([D2])
               SHUFPS  XMM6, XMM7, _MM_SHUFFLE_2_2_1_0
               MOVUPS  TXMVECTOR([V13]), XMM6

               // V02 = XM_PERMUTE_PS(MT.r[3], _MM_SHUFFLE(2,1,3,2));
               MOVUPS  XMM2, TXMVECTOR(TXMMATRIX([MT]).r[3])
               MOVAPS  XMM0,XMM2
               SHUFPS  XMM0, XMM2, _MM_SHUFFLE_2_1_3_2   // V02 = XMM0

               // V12 = _mm_shuffle_ps(D1,V13,_MM_SHUFFLE(2,1,0,3));
               MOVUPS  XMM6, TXMVECTOR([D1])
               SHUFPS  XMM6, TXMVECTOR([V13]), _MM_SHUFFLE_2_1_0_3 // V12 = XMM6

               // V03 = XM_PERMUTE_PS(MT.r[2],_MM_SHUFFLE(1,3,2,3));
               MOVUPS  XMM5, TXMVECTOR(TXMMATRIX([MT]).r[2])
               SHUFPS  XMM5, TXMVECTOR(TXMMATRIX([MT]).r[2]), _MM_SHUFFLE_1_3_2_3 // V03 = XMM5

               // V13 = _mm_shuffle_ps(D1,V13,_MM_SHUFFLE(0,2,1,2));
               MOVUPS  XMM7, TXMVECTOR([D1])
               SHUFPS  XMM7, TXMVECTOR([V13]), _MM_SHUFFLE_0_2_1_2  // V13 = XMM7

               // V00 = _mm_mul_ps(V00,V10);
               MULPS   XMM1, TXMVECTOR([V10])

               // V01 = _mm_mul_ps(V01,V11);
               MULPS   XMM3, TXMVECTOR([V11])

               // V02 = _mm_mul_ps(V02,V12);
               MULPS   XMM0, XMM6

               // V03 = _mm_mul_ps(V03,V13);
               MULPS   XMM5, XMM7

               // C0 = _mm_sub_ps(C0,V00);
               MOVUPS  XMM7, TXMVECTOR([C0])
               SUBPS   XMM7, XMM1
               MOVUPS  TXMVECTOR([C0]),XMM7

               // C2 = _mm_sub_ps(C2,V01);
               MOVUPS  XMM7, TXMVECTOR([C2])
               SUBPS   XMM7, XMM3
               MOVUPS  TXMVECTOR([C2]),XMM7

               // C4 = _mm_sub_ps(C4,V02);
               MOVUPS  XMM7, TXMVECTOR([C4])
               SUBPS   XMM7, XMM0
               MOVUPS  TXMVECTOR([C4]),XMM7

               // C6 = _mm_sub_ps(C6,V03);
               MOVUPS  XMM7, TXMVECTOR([C6])
               SUBPS   XMM7, XMM5
               MOVUPS  TXMVECTOR([C6]),XMM7

               // V00 = XM_PERMUTE_PS(MT.r[1],_MM_SHUFFLE(0,3,0,3));
               MOVUPS  XMM0, TXMVECTOR(TXMMATRIX([MT]).r[1])
               SHUFPS  XMM0, TXMVECTOR(TXMMATRIX([MT]).r[1]), _MM_SHUFFLE_0_3_0_3 // V00 = XMM0

               // V10 = D0Z,D0Z,D2X,D2Y
               // V10 = _mm_shuffle_ps(D0,D2,_MM_SHUFFLE(1,0,2,2));
               MOVUPS  XMM1, TXMVECTOR([D0])
               SHUFPS  XMM1, TXMVECTOR([D2]), _MM_SHUFFLE_1_0_2_2 // V10 = XMM1

               // V10 = XM_PERMUTE_PS(V10,_MM_SHUFFLE(0,2,3,0));
               MOVAPS  XMM2,XMM1
               SHUFPS  XMM1, XMM2, _MM_SHUFFLE_0_2_3_0 // V10 = XMM1

               // V01 = XM_PERMUTE_PS(MT.r[0],_MM_SHUFFLE(2,0,3,1));
               MOVUPS  XMM2, TXMVECTOR(TXMMATRIX([MT]).r[0])
               SHUFPS  XMM2, TXMVECTOR(TXMMATRIX([MT]).r[0]), _MM_SHUFFLE_2_0_3_1 // V01 = XMM2

               // V11 = D0X,D0W,D2X,D2Y
               // V11 = _mm_shuffle_ps(D0,D2,_MM_SHUFFLE(1,0,3,0));
               MOVUPS  XMM3, TXMVECTOR([D0])
               SHUFPS  XMM3, TXMVECTOR([D2]), _MM_SHUFFLE_1_0_3_0 // V11 = XMM3

               // V11 = XM_PERMUTE_PS(V11,_MM_SHUFFLE(2,1,0,3));
               MOVAPS  XMM4,XMM3
               SHUFPS  XMM3, XMM4, _MM_SHUFFLE_2_1_0_3 // V11 = XMM3

               // V02 = XM_PERMUTE_PS(MT.r[3],_MM_SHUFFLE(0,3,0,3));
               MOVUPS  XMM4, TXMVECTOR(TXMMATRIX([MT]).r[3])
               SHUFPS  XMM4, TXMVECTOR(TXMMATRIX([MT]).r[3]), _MM_SHUFFLE_0_3_0_3 // V02 = XMM4

               // V12 = D1Z,D1Z,D2Z,D2W
               // V12 = _mm_shuffle_ps(D1,D2,_MM_SHUFFLE(3,2,2,2));
               MOVUPS  XMM5, TXMVECTOR([D1])
               SHUFPS  XMM5, TXMVECTOR([D2]), _MM_SHUFFLE_3_2_2_2 // V12 = XMM5

               // V12 = XM_PERMUTE_PS(V12,_MM_SHUFFLE(0,2,3,0));
               MOVAPS  XMM6,XMM5
               SHUFPS  XMM5, XMM6, _MM_SHUFFLE_0_2_3_0  // V12 = XMM5

               // V13 = D1X,D1W,D2Z,D2W
               // V13 = _mm_shuffle_ps(D1,D2,_MM_SHUFFLE(3,2,3,0));
               MOVUPS  XMM6, TXMVECTOR([D1])
               SHUFPS  XMM6, TXMVECTOR([D2]), _MM_SHUFFLE_3_2_3_0

               // V13 = XM_PERMUTE_PS(V13,_MM_SHUFFLE(2,1,0,3));
               MOVAPS  XMM7,XMM6
               SHUFPS  XMM6, XMM7, _MM_SHUFFLE_2_1_0_3  // V13 = XMM6
               // V03 = XM_PERMUTE_PS(MT.r[2],_MM_SHUFFLE(2,0,3,1));
               MOVUPS  XMM7, TXMVECTOR(TXMMATRIX([MT]).r[2])
               SHUFPS  XMM7, TXMVECTOR(TXMMATRIX([MT]).r[2]), _MM_SHUFFLE_2_0_3_1 // V03 = XMM7

               // V00 = _mm_mul_ps(V00,V10);
               MULPS   XMM0, XMM1 // V00 = XMM0
               // V01 = _mm_mul_ps(V01,V11);
               MULPS   XMM2, XMM3  // V01 = XMM2
               // V02 = _mm_mul_ps(V02,V12);
               MULPS   XMM4, XMM5  // V02 = XMM4
               // V03 = _mm_mul_ps(V03,V13);
               MULPS   XMM7, XMM6  // V03 = XMM7

               // C1 = _mm_sub_ps(C0,V00);
               MOVUPS  XMM1, TXMVECTOR([C0])
               SUBPS   XMM1, XMM0;
               MOVUPS  TXMVECTOR([C1]),XMM1

               // C0 = _mm_add_ps(C0,V00);
               ADDPS   XMM0, TXMVECTOR([C0]) // a+b = b+a
               MOVUPS  TXMVECTOR([C0]),XMM0

               // C3 = _mm_add_ps(C2,V01);
               MOVUPS  XMM0, TXMVECTOR([C2])
               ADDPS   XMM0, XMM2
               MOVUPS  TXMVECTOR([C3]),XMM0

               // C2 = _mm_sub_ps(C2,V01);
               MOVUPS  XMM3, TXMVECTOR([C0])
               SUBPS   XMM3, XMM2
               MOVUPS  TXMVECTOR([C2]),XMM3

               // C5 = _mm_sub_ps(C4,V02);
               MOVUPS  XMM1, TXMVECTOR([C4])
               SUBPS   XMM1, XMM4;
               MOVUPS  TXMVECTOR([C5]),XMM1

               // C4 = _mm_add_ps(C4,V02);
               ADDPS   XMM4, TXMVECTOR([C4]); // a+b = b+a
               MOVUPS  TXMVECTOR([C4]),XMM4

               // C7 = _mm_add_ps(C6,V03);
               MOVUPS  XMM0, TXMVECTOR([C6])
               ADDPS   XMM0, XMM7
               MOVUPS  TXMVECTOR([C7]),XMM0

               // C6 = _mm_sub_ps(C6,V03);
               MOVUPS  XMM3, TXMVECTOR([C6])
               SUBPS   XMM3, XMM7
               MOVUPS  TXMVECTOR([C6]),XMM3

               // C0 = _mm_shuffle_ps(C0,C1,_MM_SHUFFLE(3,1,2,0));
               MOVUPS  XMM0, TXMVECTOR([C0])
               SHUFPS  XMM0, TXMVECTOR([C1]), _MM_SHUFFLE_3_1_2_0
               MOVUPS   TXMVECTOR([C0]),XMM0

               // C2 = _mm_shuffle_ps(C2,C3,_MM_SHUFFLE(3,1,2,0));
               MOVUPS  XMM0, TXMVECTOR([C2])
               SHUFPS  XMM0, TXMVECTOR([C3]), _MM_SHUFFLE_3_1_2_0
               MOVUPS   TXMVECTOR([C2]),XMM0

               // C4 = _mm_shuffle_ps(C4,C5,_MM_SHUFFLE(3,1,2,0));
               MOVUPS  XMM0, TXMVECTOR([C4])
               SHUFPS  XMM0, TXMVECTOR([C5]), _MM_SHUFFLE_3_1_2_0
               MOVUPS   TXMVECTOR([C4]),XMM0

               // C6 = _mm_shuffle_ps(C6,C7,_MM_SHUFFLE(3,1,2,0));
               MOVUPS  XMM0, TXMVECTOR([C6])
               SHUFPS  XMM0, TXMVECTOR([C7]), _MM_SHUFFLE_3_1_2_0
               MOVUPS   TXMVECTOR([C6]),XMM0

               // C0 = XM_PERMUTE_PS(C0,_MM_SHUFFLE(3,1,2,0));
               MOVUPS  XMM0, TXMVECTOR([C0])
               MOVAPS  XMM1,XMM0
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_1_2_0
               MOVUPS   TXMVECTOR([C0]),XMM0

               // C2 = XM_PERMUTE_PS(C2,_MM_SHUFFLE(3,1,2,0));
               MOVUPS  XMM0, TXMVECTOR([C2])
               MOVAPS  XMM1,XMM0
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_1_2_0
               MOVUPS   TXMVECTOR([C2]),XMM0

               // C4 = XM_PERMUTE_PS(C4,_MM_SHUFFLE(3,1,2,0));
               MOVUPS  XMM0, TXMVECTOR([C4])
               MOVAPS  XMM1,XMM0
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_1_2_0
               MOVUPS   TXMVECTOR([C4]),XMM0

               // C6 = XM_PERMUTE_PS(C6,_MM_SHUFFLE(3,1,2,0));
               MOVUPS  XMM0, TXMVECTOR([C6])
               MOVAPS  XMM1,XMM0
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_1_2_0
               MOVUPS   TXMVECTOR([C6]),XMM0
    end;
    // Get the determinate
    vTemp := XMVector4Dot(C0, MT.r[0]);
    pDeterminant := vTemp;
    asm
               // vTemp = _mm_div_ps(g_XMOne,vTemp);
               MOVUPS  XMM0, TXMVECTOR([g_XMOne])
               MOVUPS  XMM1, TXMVECTOR([vTemp])
               DIVPS   XMM0, XMM1
               MOVUPS   TXMVECTOR([vTemp]),XMM0  // vTemp = XMM0
               // mResult.r[0] = _mm_mul_ps(C0,vTemp);
               MOVUPS  XMM1, TXMVECTOR([C0])
               MULPS   XMM1, XMM0
               MOVUPS   TXMVECTOR(TXMMATRIX([result]).r[0]) ,XMM1
               // mResult.r[1] = _mm_mul_ps(C2,vTemp);
               MOVUPS  XMM1, TXMVECTOR([C2])
               MULPS   XMM1, XMM0
               MOVUPS   TXMVECTOR(TXMMATRIX([result]).r[1]) ,XMM1
               // mResult.r[2] = _mm_mul_ps(C4,vTemp);
               MOVUPS  XMM1, TXMVECTOR([C4])
               MULPS   XMM1, XMM0
               MOVUPS   TXMVECTOR(TXMMATRIX([result]).r[2]) ,XMM1
               // mResult.r[3] = _mm_mul_ps(C6,vTemp);
               MOVUPS  XMM1, TXMVECTOR([C6])
               MULPS   XMM1, XMM0
               MOVUPS   TXMVECTOR(TXMMATRIX([result]).r[3]) ,XMM1
    end;
end;



function XMMatrixSet(m00: single; m01: single; m02: single; m03: single; m10: single; m11: single; m12: single; m13: single;
    m20: single; m21: single; m22: single; m23: single; m30: single; m31: single; m32: single; m33: single): TXMMATRIX;
begin
    Result.r[0] := XMVectorSet(m00, m01, m02, m03);
    Result.r[1] := XMVectorSet(m10, m11, m12, m13);
    Result.r[2] := XMVectorSet(m20, m21, m22, m23);
    Result.r[3] := XMVectorSet(m30, m31, m32, m33);
end;



function XMMatrixTranslation(OffsetX: single; OffsetY: single; OffsetZ: single): TXMMATRIX;
begin
    Result.r[0] := g_XMIdentityR0.v;
    Result.r[1] := g_XMIdentityR1.v;
    Result.r[2] := g_XMIdentityR2.v;
    Result.r[3] := XMVectorSet(OffsetX, OffsetY, OffsetZ, 1.0);
end;



function XMMatrixTranslationFromVector(Offset: TXMVECTOR): TXMMATRIX;
begin
    Result.r[0] := g_XMIdentityR0.v;
    Result.r[1] := g_XMIdentityR1.v;
    Result.r[2] := g_XMIdentityR2.v;
    Result.r[3] := XMVectorSelect(g_XMIdentityR3.v, Offset, g_XMSelect1110.v);
end;



function XMMatrixScaling(constref ScaleX, ScaleY, ScaleZ: single): TXMMATRIX; assembler;
asm
           XORPS    XMM0, XMM0 // set to zero
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[0]),XMM0
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[1]),XMM0
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[2]),XMM0
           MOVUPS  XMM1,TXMVECTOR([g_XMIdentityR3.v]);
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[3]), XMM1
           // result.r[0] := _mm_set_ps( 0, 0, 0, ScaleX );
           MOV     EAX, [ScaleX]
           MOV     TXMVECTOR(TXMMATRIX([result]).r[0]).f32[3],EAX

           // result.r[1] := _mm_set_ps( 0, 0, ScaleY, 0 );
           MOV     EAX, [ScaleY]
           MOV     TXMVECTOR(TXMMATRIX([result]).r[1]).f32[2],EAX

           // result.r[2] := _mm_set_ps( 0, ScaleZ, 0, 0 );
           MOV     EAX, [ScaleZ]
           MOV     TXMVECTOR(TXMMATRIX([result]).r[2]).f32[1],EAX
end;



function XMMatrixScalingFromVector(constref Scale: TXMVECTOR): TXMMATRIX; assembler;
asm
           MOVUPS  XMM0,[g_XMMaskX]
           MOVUPS  XMM1,[g_XMMaskY]
           MOVUPS  XMM2,[g_XMMaskZ]
           MOVUPS  XMM3,[Scale]
           ANDPS   XMM0, XMM3 // _mm_and_ps(Scale,g_XMMaskX);
           ANDPS   XMM1, XMM3 // _mm_and_ps(Scale,g_XMMaskX);
           ANDPS   XMM2, XMM3 // _mm_and_ps(Scale,g_XMMaskZ);
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[0]),XMM0
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[1]),XMM1
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[2]),XMM2
           MOVUPS  XMM3,[g_XMIdentityR3.v]
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[3]),XMM3
end;



function XMMatrixRotationX(Angle: single): TXMMATRIX;
var
    SinAngle, CosAngle: single;
    vSin, vCos: TXMVECTOR;
begin
    XMScalarSinCos(SinAngle, CosAngle, Angle);
    asm
               MOVSS   XMM0, [SinAngle] // vSin
               MOVSS   XMM1, [CosAngle] // vCos
               // x = 0,y = cos,z = sin, w = 0
               SHUFPS  XMM1, XMM0, _MM_SHUFFLE_3_0_0_3 // vCos = _mm_shuffle_ps(vCos,vSin,_MM_SHUFFLE(3,0,0,3));

               MOVUPS  XMM2,[g_XMIdentityR0]
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[0]) ,XMM2
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[1]),XMM1 // M.r[1] := vCos;
               // x = 0,y = sin,z = cos, w = 0
               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_3_1_2_0 // vCos = XM_PERMUTE_PS(vCos,_MM_SHUFFLE(3,1,2,0));
               // x = 0,y = -sin,z = cos, w = 0
               MOVSS   XMM2, [g_XMNegateY]
               MULPS   XMM1, XMM2 // vCos = _mm_mul_ps(vCos,g_XMNegateY);
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[2]),XMM1 // M.r[2] := vCos;
               MOVUPS  XMM2,[g_XMIdentityR3]
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[3]),XMM2
    end;
end;



function XMMatrixRotationY(Angle: single): TXMMATRIX;
var
    SinAngle: single;
    CosAngle: single;
begin
    XMScalarSinCos(SinAngle, CosAngle, Angle);
    asm
               MOVSS   XMM0, [SinAngle] // vSin
               MOVSS   XMM1, [CosAngle] // vCos
               // x = sin,y = 0,z = cos, w = 0
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_0_3_0 // vSin = _mm_shuffle_ps(vSin,vCos,_MM_SHUFFLE(3,0,3,0));

               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[2]) ,XMM0 // M.r[2] = vSin;
               MOVUPS  XMM2,[g_XMIdentityR1]
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[1]) ,XMM2 // M.r[1] = g_XMIdentityR1;
               // x = cos,y = 0,z = sin, w = 0
               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_0_1_2 // vSin = XM_PERMUTE_PS(vSin,_MM_SHUFFLE(3,0,1,2));
               // x = cos,y = 0,z = -sin, w = 0
               MOVSS   XMM2, [g_XMNegateZ]
               MULPS   XMM0, XMM2 // vSin = _mm_mul_ps(vSin,g_XMNegateZ);
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[0]) ,XMM0 // M.r[0] = vSin;

               MOVSS   XMM2, [g_XMIdentityR3]
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[3]) ,XMM2 // M.r[3] = g_XMIdentityR3;
    end;
end;



function XMMatrixRotationZ(Angle: single): TXMMATRIX;
var
    SinAngle: single;
    CosAngle: single;
begin

    XMScalarSinCos(SinAngle, CosAngle, Angle);
    asm
               MOVSS   XMM0, [SinAngle] // vSin
               MOVSS   XMM1, [CosAngle] // vCos

               // x = cos,y = sin,z = 0, w = 0
               UNPCKLPS XMM1, XMM0 // vCos = _mm_unpacklo_ps(vCos,vSin);

               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[0]),XMM1 // M.r[0] = vCos;
               // x = sin,y = cos,z = 0, w = 0
               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_3_2_0_1 // vCos = XM_PERMUTE_PS(vCos,_MM_SHUFFLE(3,2,0,1));
               // x = cos,y = -sin,z = 0, w = 0
               MOVSS   XMM2, [g_XMNegateX]
               MULPS   XMM1, XMM2 // vCos = _mm_mul_ps(vCos,g_XMNegateX);
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[1]),XMM1 // M.r[1] = vCos;
               MOVSS   XMM2, [g_XMIdentityR2]
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[2]),XMM2 // M.r[2] = g_XMIdentityR2;
               MOVSS   XMM2, [g_XMIdentityR3]
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[3]),XMM2 // M.r[3] = g_XMIdentityR3;
    end;
end;



function XMMatrixRotationNormal(constref NormalAxis: TXMVECTOR; constref Angle: single): TXMMATRIX;
const
    v: single = 1.0;
var
    fSinAngle: single;
    fCosAngle: single;
    C0, C1, C2, N0, N1: TXMVECTOR;

begin

    XMScalarSinCos(fSinAngle, fCosAngle, Angle);
    asm
               // C2 = _mm_set_ps1(1.0f - fCosAngle);
               MOVSS       XMM0,[v]
               SUBSS       XMM0,[fCosAngle]
               SHUFPS      XMM0,XMM0,0
               MOVAPS      [C2],XMM0
               // C1 = _mm_set_ps1(fCosAngle);
               MOVSS       XMM0,[fCosAngle]
               MOVAPS      [C1],XMM0
               // C0 = _mm_set_ps1(fSinAngle);
               MOVSS       XMM0,[fSinAngle]
               MOVAPS      [C0],XMM0

               //  N0 = XM_PERMUTE_PS(NormalAxis,_MM_SHUFFLE(3,0,2,1));
               MOVSS       XMM0,[NormalAxis]
               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_3_0_2_1 // N0 = XMM0

               // N1 = XM_PERMUTE_PS(NormalAxis,_MM_SHUFFLE(3,1,0,2));
               MOVSS       XMM1,[NormalAxis]
               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_3_1_0_2

               MULPS   XMM0, [C2] // V0 = _mm_mul_ps(C2, N0); // a*b = b*a
               MULPS   XMM0, XMM1 // V0 = _mm_mul_ps(V0, N1); // V0 = xmm0

               // R0 = _mm_mul_ps(C2, NormalAxis);
               MOVUPS  XMM1,[C2]
               MULPS   XMM1, [NormalAxis]   // R0 = XMM1

               MULPS   XMM1, [NormalAxis]  // R0 = _mm_mul_ps(R0, NormalAxis);
               ADDPS   XMM1, [C1] // R0 = _mm_add_ps(R0, C1);

               // R1 = _mm_mul_ps(C0, NormalAxis);
               MOVUPS  XMM2,[C0]
               MULPS   XMM2, [NormalAxis]   // R1 = XMM2
               ADDPS   XMM2, XMM0 // R1 = _mm_add_ps(R1, V0);

               // R2 = _mm_mul_ps(C0, NormalAxis);
               MOVUPS  XMM3,[C0]
               MULPS   XMM3, [NormalAxis]   // R2 = XMM3
               // R2 = _mm_sub_ps(V0,R2);
               MOVUPS  XMM4,XMM0
               SUBPS   XMM4, XMM3  // R2 = XMM4

               // V0 = _mm_and_ps(R0,g_XMMask3);
               ADDPS   XMM1, [g_XMMask3] // V0 =XMM1

               // V1 = _mm_shuffle_ps(R1,R2,_MM_SHUFFLE(2,1,2,0));
               MOVUPS  XMM5,XMM2 // save copy R1
               SHUFPS  XMM2, XMM4, _MM_SHUFFLE_2_1_2_0  // XMM2 = V1

               // V1 = XM_PERMUTE_PS(V1,_MM_SHUFFLE(0,3,2,1));
               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_0_3_2_1

               // V2 = _mm_shuffle_ps(R1,R2,_MM_SHUFFLE(0,0,1,1));
               SHUFPS  XMM5, XMM4, _MM_SHUFFLE_0_0_1_1  // XMM5 = V2
               // V2 = XM_PERMUTE_PS(V2,_MM_SHUFFLE(2,0,2,0));
               SHUFPS  XMM5, XMM5, _MM_SHUFFLE_2_0_2_0

               // R2 = _mm_shuffle_ps(V0,V1,_MM_SHUFFLE(1,0,3,0));
               MOVUPS  XMM4, XMM1
               SHUFPS  XMM4, XMM2, _MM_SHUFFLE_1_0_3_0  // R2 = XMM4

               // R2 = XM_PERMUTE_PS(R2,_MM_SHUFFLE(1,3,2,0));
               SHUFPS  XMM4, XMM4, _MM_SHUFFLE_1_3_2_0


               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[0]), XMM4 // M.r[0] = R2;

               // R2 = _mm_shuffle_ps(V0,V1,_MM_SHUFFLE(3,2,3,1));
               MOVUPS  XMM4, XMM1
               SHUFPS  XMM4, XMM2, _MM_SHUFFLE_3_2_3_1  // R2 = XMM4
               // R2 = XM_PERMUTE_PS(R2,_MM_SHUFFLE(1,3,0,2));
               SHUFPS  XMM4, XMM4, _MM_SHUFFLE_1_3_0_2
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[1]), XMM4 // M.r[1] = R2;

               // V2 = _mm_shuffle_ps(V2,V0,_MM_SHUFFLE(3,2,1,0));
               SHUFPS  XMM5, XMM1, _MM_SHUFFLE_3_2_1_0

               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[2]), XMM5 // M.r[2] = V2;
               // M.r[3] = g_XMIdentityR3.v;
               MOVUPS  XMM5, [g_XMIdentityR3];
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[3]), XMM5

    end;
end;



function XMMatrixRotationQuaternion(Quaternion: TXMVECTOR): TXMMATRIX; assembler;
const
    Constant1110: TXMVECTORF32 = (f: (1.0, 1.0, 1.0, 0.0));
asm
           // Q0 = _mm_add_ps(Quaternion,Quaternion);
           MOVUPS  XMM0, [Quaternion]
           ADDPS   XMM0, XMM0  // Q0 = XMM0

           // Q1 = _mm_mul_ps(Quaternion,Q0);
           MOVUPS  XMM1, [Quaternion]
           MULPS   XMM1, XMM0  // Q1 = XMM1

           // V0 = XM_PERMUTE_PS(Q1,_MM_SHUFFLE(3,0,0,1));
           MOVUPS  XMM2, XMM1
           SHUFPS  XMM2, XMM1, _MM_SHUFFLE_3_0_0_1

           // V0 = _mm_and_ps(V0,g_XMMask3);
           ADDPS   XMM2, [g_XMMask3]  // V0 = XMM2

           // V1 = XM_PERMUTE_PS(Q1,_MM_SHUFFLE(3,1,2,2));
           MOVUPS  XMM3, XMM1
           SHUFPS  XMM3, XMM1, _MM_SHUFFLE_3_1_2_2

           // V1 = _mm_and_ps(V1,g_XMMask3);
           ADDPS   XMM3, [g_XMMask3]  // V1 = XMM3

           // R0 = _mm_sub_ps(Constant1110,V0);
           MOVUPS  XMM4, [Constant1110]
           SUBPS   XMM4, XMM2 // R0 = XMM4

           // R0 = _mm_sub_ps(R0, V1);
           SUBPS   XMM4, XMM3

           // V0 = XM_PERMUTE_PS(Quaternion,_MM_SHUFFLE(3,1,0,0));
           MOVUPS  XMM2, [Quaternion]
           SHUFPS  XMM2, [Quaternion] , _MM_SHUFFLE_3_1_0_0

           // V1 = XM_PERMUTE_PS(Q0,_MM_SHUFFLE(3,2,1,2));
           MOVUPS  XMM3, XMM1
           SHUFPS  XMM3, XMM1 , _MM_SHUFFLE_3_2_1_2

           // V0 = _mm_mul_ps(V0, V1);
           MULPS   XMM2, XMM3 // V0 = XMM2

           // V1 = XM_PERMUTE_PS(Quaternion,_MM_SHUFFLE(3,3,3,3));
           MOVUPS  XMM3, [Quaternion]
           SHUFPS  XMM3, [Quaternion] , _MM_SHUFFLE_3_3_3_3

           // V2 = XM_PERMUTE_PS(Q0,_MM_SHUFFLE(3,0,2,1));
           MOVUPS  XMM5, XMM0
           SHUFPS  XMM5, XMM0 , _MM_SHUFFLE_3_0_2_1

           // V1 = _mm_mul_ps(V1, V2);
           MULPS   XMM3, XMM5

           // R1 = _mm_add_ps(V0, V1);
           MOVUPS  XMM6, XMM2
           ADDPS   XMM6, XMM3 // R1 = XMM6

           // R2 = _mm_sub_ps(V0, V1);
           MOVUPS  XMM7, XMM2
           SUBPS   XMM7, XMM2 // R2 = XMM7

           // V0 = _mm_shuffle_ps(R1,R2,_MM_SHUFFLE(1,0,2,1));
           MOVUPS  XMM2, XMM6
           SHUFPS  XMM2, XMM7, _MM_SHUFFLE_1_0_2_1 // V0 = XMM2

           // V0 = XM_PERMUTE_PS(V0,_MM_SHUFFLE(1,3,2,0));
           SHUFPS  XMM2, XMM2, _MM_SHUFFLE_1_3_2_0

           // V1 = _mm_shuffle_ps(R1,R2,_MM_SHUFFLE(2,2,0,0));
           MOVUPS  XMM3, XMM6
           SHUFPS  XMM3, XMM7, _MM_SHUFFLE_2_2_0_0 // V1 = XMM3

           // V1 = XM_PERMUTE_PS(V1,_MM_SHUFFLE(2,0,2,0));
           SHUFPS  XMM3, XMM3, _MM_SHUFFLE_2_0_2_0

           // Q1 = _mm_shuffle_ps(R0,V0,_MM_SHUFFLE(1,0,3,0));
           MOVUPS  XMM1,XMM4
           SHUFPS  XMM1, XMM2, _MM_SHUFFLE_1_0_3_0 // Q1 = XMM1

           // Q1 = XM_PERMUTE_PS(Q1,_MM_SHUFFLE(1,3,2,0));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_3_2_0

           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[0]),XMM1 // M.r[0] = Q1;

           // Q1 = _mm_shuffle_ps(R0,V0,_MM_SHUFFLE(3,2,3,1));
           MOVUPS  XMM1,XMM4
           SHUFPS  XMM1, XMM2, _MM_SHUFFLE_3_2_3_1 // Q1 = XMM1

           // Q1 = XM_PERMUTE_PS(Q1,_MM_SHUFFLE(1,3,0,2));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_3_0_2

           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[1]),XMM1 // M.r[1] = Q1;

           // Q1 = _mm_shuffle_ps(V1,R0,_MM_SHUFFLE(3,2,1,0));
           MOVUPS  XMM1,XMM3
           SHUFPS  XMM1, XMM4, _MM_SHUFFLE_3_2_1_0

           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[2]),XMM1 // M.r[2] = Q1;

           MOVUPS  XMM1,[g_XMIdentityR3]
           MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[3]),XMM1 // M.r[3] = g_XMIdentityR3;

end;



function XMMatrixPerspectiveLH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    TwoNearZ, fRange: single;
    rMem: TXMVECTOR;
begin
    TwoNearZ := NearZ + NearZ;
    fRange := FarZ / (FarZ - NearZ);
    // Note: This is recorded on the stack
    rMem.Create(TwoNearZ / ViewWidth,
        TwoNearZ / ViewHeight,
        fRange, -fRange * NearZ);
    asm

               // Copy from memory to SSE register
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // Copy x only
               MOVSS   XMM0, [rMem] // vTemp = _mm_move_ss(vTemp,vValues);
               // TwoNearZ / ViewWidth,0,0,0
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[0]),XMM0 // M.r[0] = vTemp;

               // 0,TwoNearZ / ViewHeight,0,0
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               MOVUPS  XMM2, [g_XMMaskY]
               ANDPS   XMM0,XMM2 // vTemp = _mm_and_ps(vTemp,g_XMMaskY);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[1]),XMM0 // M.r[1] = vTemp;

               // x=fRange,y=-fRange * NearZ,0,1.0f
               MOVSS   XMM1, [rMem]
               MOVUPS  XMM2, [g_XMIdentityR3]
               SHUFPS  XMM1, XMM2, _MM_SHUFFLE_3_2_3_2 // vValues = _mm_shuffle_ps(vValues,g_XMIdentityR3,_MM_SHUFFLE(3,2,3,2));
               // 0,0,fRange,1.0f
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_0_0_0 // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(3,0,0,0));
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[2]),XMM0 // M.r[2] = vTemp;
               // 0,0,-fRange * NearZ,0
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_2_1_0_0 // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(2,1,0,0));
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[3]),XMM0 // M.r[3] = vTemp;
    end;
end;



function XMMatrixPerspectiveRH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    TwoNearZ, fRange: single;
    rMem: TXMVECTOR;
begin
    TwoNearZ := NearZ + NearZ;
    fRange := FarZ / (NearZ - FarZ);
    // Note: This is recorded on the stack
    rMem.Create(TwoNearZ / ViewWidth,
        TwoNearZ / ViewHeight,
        fRange,
        fRange * NearZ);
    asm
               // Copy from memory to SSE register
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // Copy x only
               MOVSS   XMM0, [rMem] // vTemp = _mm_move_ss(vTemp,vValues);
               // TwoNearZ / ViewWidth,0,0,0
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[0]),XMM0 // M.r[0] = vTemp;
               // 0,TwoNearZ / ViewHeight,0,0
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               MOVUPS  XMM2, [g_XMMaskY]
               ANDPS   XMM0,XMM2 // vTemp = _mm_and_ps(vTemp,g_XMMaskY);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[1]),XMM0 // M.r[1] = vTemp;
               // x=fRange,y=-fRange * NearZ,0,-1.0f
               MOVSS   XMM1, [rMem]
               MOVUPS  XMM2, [g_XMNegIdentityR3]
               SHUFPS  XMM1, XMM2, _MM_SHUFFLE_3_2_3_2 // vValues = _mm_shuffle_ps(vValues,g_XMNegIdentityR3,_MM_SHUFFLE(3,2,3,2));
               // 0,0,fRange,-1.0f
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_0_0_0 // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(3,0,0,0));
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[2]),XMM0 // M.r[2] = vTemp;
               // 0,0,-fRange * NearZ,0
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_2_1_0_0 // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(2,1,0,0));
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[3]),XMM0 // M.r[3] = vTemp;
    end;
end;



function XMMatrixPerspectiveFovLH(FovAngleY: single; AspectRatio: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    SinFov: single;
    CosFov: single;
    TwoNearZ, fRange, Height: single;
    rMem: TXMVECTOR;
begin
    XMScalarSinCos(SinFov, CosFov, 0.5 * FovAngleY);

    fRange := FarZ / (FarZ - NearZ);
    // Note: This is recorded on the stack
    Height := CosFov / SinFov;
    rMem.Create(
        Height / AspectRatio,
        Height,
        fRange, -fRange * NearZ
        );
    asm
               // Copy from memory to SSE register
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // Copy x only
               MOVSS   XMM0, [rMem] // vTemp = _mm_move_ss(vTemp,vValues);
               // CosFov / SinFov,0,0,0
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[0]),XMM0 // M.r[0] = vTemp;
               // 0,Height / AspectRatio,0,0
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               MOVUPS  XMM2, [g_XMMaskY]
               ANDPS   XMM0,XMM2 // vTemp = _mm_and_ps(vTemp,g_XMMaskY);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[1]),XMM0 // M.r[1] = vTemp;

               // x=fRange,y=-fRange * NearZ,0,1.0f
               MOVSS   XMM1, [rMem]
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               MOVUPS  XMM2, [g_XMIdentityR3]
               SHUFPS  XMM1, XMM2, _MM_SHUFFLE_3_2_3_2 // vValues = _mm_shuffle_ps(vValues,g_XMIdentityR3,_MM_SHUFFLE(3,2,3,2));
               // 0,0,fRange,1.0f
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_0_0_0 // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(3,0,0,0));
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[2]),XMM0 // M.r[2] = vTemp;
               // 0,0,-fRange * NearZ,0.0f
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_2_1_0_0 // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(2,1,0,0));
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[3]),XMM0 // M.r[3] = vTemp;
    end;
end;



function XMMatrixPerspectiveFovRH(FovAngleY: single; AspectRatio: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    SinFov, CosFov, fRange, Height: single;
    rMem: TXMVECTOR;
begin
    XMScalarSinCos(SinFov, CosFov, 0.5 * FovAngleY);
    fRange := FarZ / (NearZ - FarZ);
    // Note: This is recorded on the stack
    Height := CosFov / SinFov;
    rMem.Create(
        Height / AspectRatio,
        Height,
        fRange,
        fRange * NearZ
        );
    asm
               // Copy from memory to SSE register
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // Copy x only
               MOVSS   XMM0, [rMem] // vTemp = _mm_move_ss(vTemp,vValues);
               // CosFov / SinFov,0,0,0

               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[0]),XMM0 // M.r[0] = vTemp;
               // 0,Height / AspectRatio,0,0
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               MOVUPS  XMM2, [g_XMMaskY]
               ANDPS   XMM0,XMM2 // vTemp = _mm_and_ps(vTemp,g_XMMaskY);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[1]),XMM0 // M.r[1] = vTemp;
               // x=fRange,y=-fRange * NearZ,0,-1.0f
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               MOVSS   XMM1, [rMem]
               MOVUPS  XMM2, [g_XMNegIdentityR3]
               SHUFPS  XMM1, XMM2, _MM_SHUFFLE_3_2_3_2 // vValues = _mm_shuffle_ps(vValues,g_XMNegIdentityR3,_MM_SHUFFLE(3,2,3,2));
               // 0,0,fRange,-1.0f
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_0_0_0 // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(3,0,0,0));
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[2]),XMM0 // M.r[2] = vTemp;
               // 0,0,fRange * NearZ,0.0f
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_2_1_0_0 // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(2,1,0,0));
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[3]),XMM0 // M.r[3] = vTemp;
    end;
end;



function XMMatrixPerspectiveOffCenterLH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    TwoNearZ, fRange, ReciprocalWidth, ReciprocalHeight: single;
    rMem: TXMVECTOR;
begin

    TwoNearZ := NearZ + NearZ;
    ReciprocalWidth := 1.0 / (ViewRight - ViewLeft);
    ReciprocalHeight := 1.0 / (ViewTop - ViewBottom);
    fRange := FarZ / (FarZ - NearZ);
    // Note: This is recorded on the stack
    rMem.Create(
        TwoNearZ * ReciprocalWidth,
        TwoNearZ * ReciprocalHeight, -fRange * NearZ,
        0
        );
    asm
               // Copy from memory to SSE register
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // Copy x only
               MOVSS   XMM0, [rMem] // vTemp = _mm_move_ss(vTemp,vValues);
               // TwoNearZ*ReciprocalWidth,0,0,0
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[0]),XMM0 // M.r[0] = vTemp;
               // 0,TwoNearZ*ReciprocalHeight,0,0
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               MOVUPS  XMM2, [g_XMMaskY]
               ANDPS   XMM0,XMM2 // vTemp = _mm_and_ps(vTemp,g_XMMaskY);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[1]),XMM0 // M.r[1] = vTemp;
               // 0,0,fRange,1.0f
    end;
    Result.r[2] := XMVectorSet(-(ViewLeft + ViewRight) * ReciprocalWidth, -(ViewTop + ViewBottom) * ReciprocalHeight, fRange, 1.0);
    asm
               // 0,0,-fRange * NearZ,0.0f
               MOVSS   XMM1, [rMem]
               ANDPS   XMM1,[g_XMMaskZ] // vValues = _mm_and_ps(vValues,g_XMMaskZ);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[3]),XMM1 // M.r[3] = vValues;
    end;
end;



function XMMatrixPerspectiveOffCenterRH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    TwoNearZ, fRange, ReciprocalWidth, ReciprocalHeight: single;
    rMem: TXMVECTOR;
begin

    TwoNearZ := NearZ + NearZ;
    ReciprocalWidth := 1.0 / (ViewRight - ViewLeft);
    ReciprocalHeight := 1.0 / (ViewTop - ViewBottom);
    fRange := FarZ / (NearZ - FarZ);
    // Note: This is recorded on the stack
    rMem.Create(
        TwoNearZ * ReciprocalWidth,
        TwoNearZ * ReciprocalHeight,
        fRange * NearZ,
        0
        );
    asm
               // Copy from memory to SSE register
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // Copy x only
               MOVSS   XMM0, [rMem] // vTemp = _mm_move_ss(vTemp,vValues);
               // TwoNearZ*ReciprocalWidth,0,0,0
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[0]),XMM0 // M.r[0] = vTemp;
               // 0,TwoNearZ*ReciprocalHeight,0,0
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               ANDPS   XMM0,[g_XMMaskY] // vTemp = _mm_and_ps(vTemp,g_XMMaskY);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[1]),XMM0 // M.r[1] = vTemp;
    end;
    // 0,0,fRange,1.0f
    Result.r[2] := XMVectorSet((ViewLeft + ViewRight) * ReciprocalWidth, (ViewTop + ViewBottom) * ReciprocalHeight, fRange, -1.0);
    // 0,0,-fRange * NearZ,0.0f
    asm
               MOVSS   XMM0, [rMem]
               ANDPS   XMM0 , [g_XMMaskZ] // vValues = _mm_and_ps(vValues,g_XMMaskZ);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[3]),XMM0 // M.r[3] = vValues;
    end;
end;



function XMMatrixOrthographicLH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    fRange: single;
    rMem: TXMVECTOR;
begin

    fRange := 1.0 / (FarZ - NearZ);
    // Note: This is recorded on the stack
    rMem.Create(
        2.0 / ViewWidth,
        2.0 / ViewHeight,
        fRange, -fRange * NearZ
        );
    asm
               // Copy from memory to SSE register
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // Copy x only
               MOVSS   XMM0, [rMem] // vTemp = _mm_move_ss(vTemp,vValues);
               // 2.0f / ViewWidth,0,0,0
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[0]),XMM0 // M.r[0] = vTemp;
               // 0,2.0f / ViewHeight,0,0
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               ANDPS   XMM0,[g_XMMaskY] // vTemp = _mm_and_ps(vTemp,g_XMMaskY);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[1]),XMM0 // M.r[1] = vTemp;
               // x=fRange,y=-fRange * NearZ,0,1.0f
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // vValues = _mm_shuffle_ps(vValues,g_XMIdentityR3,_MM_SHUFFLE(3,2,3,2));
               MOVSS   XMM1, [rMem]
               SHUFPS  XMM1, [g_XMIdentityR3], _MM_SHUFFLE_3_2_3_2

               // 0,0,fRange,0.0f
               // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(2,0,0,0));
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_2_0_0_0
               MOVUPS  TXMVECTOR(TXMMATRIX([result]).r[2]),XMM1 //M.r[2] = vTemp;
               // 0,0,-fRange * NearZ,1.0f
               // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(3,1,0,0));
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_1_0_0
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[3]),XMM0 // M.r[3] = vTemp;
    end;
end;



function XMMatrixOrthographicRH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    fRange: single;
    rMem: TXMVECTOR;
begin

    fRange := 1.0 / (NearZ - FarZ);
    // Note: This is recorded on the stack
    rMem.Create(
        2.0 / ViewWidth,
        2.0 / ViewHeight,
        fRange,
        fRange * NearZ
        );
    asm
               // Copy from memory to SSE register
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // Copy x only
               MOVSS   XMM0, [rMem] // vTemp = _mm_move_ss(vTemp,vValues);
               // 2.0f / ViewWidth,0,0,0
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[0]),XMM0 // M.r[0] = vTemp;
               // 0,2.0f / ViewHeight,0,0
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               ANDPS   XMM0,[g_XMMaskY] // vTemp = _mm_and_ps(vTemp,g_XMMaskY);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[1]),XMM0 // M.r[1] = vTemp;
               // x=fRange,y=fRange * NearZ,0,1.0f
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // vValues = _mm_shuffle_ps(vValues,g_XMIdentityR3,_MM_SHUFFLE(3,2,3,2));
               MOVSS   XMM1, [rMem]
               SHUFPS  XMM1, [g_XMIdentityR3], _MM_SHUFFLE_3_2_3_2

               // 0,0,fRange,0.0f
               // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(2,0,0,0));
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_2_0_0_0
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[2]),XMM0 // M.r[2] = vTemp;
               // 0,0,fRange * NearZ,1.0f
               // vTemp = _mm_shuffle_ps(vTemp,vValues,_MM_SHUFFLE(3,1,0,0));
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_1_0_0
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[3]),XMM0 // M.r[3] = vTemp;
    end;
end;



function XMMatrixOrthographicOffCenterLH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    fReciprocalWidth, fReciprocalHeight, fRange: single;
    rMem, rMem2: TXMVECTOR;
begin
    fReciprocalWidth := 1.0 / (ViewRight - ViewLeft);
    fReciprocalHeight := 1.0 / (ViewTop - ViewBottom);
    fRange := 1.0 / (FarZ - NearZ);
    // Note: This is recorded on the stack
    rMem.Create(
        fReciprocalWidth,
        fReciprocalHeight,
        fRange,
        1.0
        );
    rMem2.Create(-(ViewLeft + ViewRight), -(ViewTop + ViewBottom), -NearZ,
        1.0
        );
    asm
               // Copy from memory to SSE register
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // Copy x only
               MOVSS   XMM0, [rMem] // vTemp = _mm_move_ss(vTemp,vValues);
               // fReciprocalWidth*2,0,0,0
               ADDPS   XMM0,XMM0 // vTemp = _mm_add_ss(vTemp,vTemp);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[0]),XMM0 // M.r[0] = vTemp;
               // 0,fReciprocalHeight*2,0,0
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               ADDPS   XMM0, [g_XMMaskY] // vTemp = _mm_and_ps(vTemp,g_XMMaskY);
               ADDPS   XMM0,XMM0 // vTemp = _mm_add_ps(vTemp,vTemp);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[1]),XMM0 // M.r[1] = vTemp;
               // 0,0,fRange,0.0f
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               ANDPS   XMM0, [g_XMMaskZ] // vTemp = _mm_and_ps(vTemp,g_XMMaskZ);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[2]),XMM0 // M.r[2] = vTemp;
               // -(ViewLeft + ViewRight)*fReciprocalWidth,-(ViewTop + ViewBottom)*fReciprocalHeight,fRange*-NearZ,1.0f
               // vValues = _mm_mul_ps(vValues,rMem2);
               MOVSS   XMM0, [rMem]
               MULPS   XMM0, [rMem2]
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[3]),XMM0 // M.r[3] = vValues;
    end;
end;



function XMMatrixOrthographicOffCenterRH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    fReciprocalWidth, fReciprocalHeight, fRange: single;
    rMem, rMem2: TXMVECTOR;
begin

    fReciprocalWidth := 1.0 / (ViewRight - ViewLeft);
    fReciprocalHeight := 1.0 / (ViewTop - ViewBottom);
    fRange := 1.0 / (NearZ - FarZ);
    // Note: This is recorded on the stack
    rMem.Create(
        fReciprocalWidth,
        fReciprocalHeight,
        fRange,
        1.0
        );
    rMem2.Create(-(ViewLeft + ViewRight), -(ViewTop + ViewBottom),
        NearZ,
        1.0
        );
    asm
               // Copy from memory to SSE register
               XORPS   XMM0, XMM0 // vTemp = _mm_setzero_ps();
               // Copy x only
               MOVSS   XMM0, [rMem] // vTemp = _mm_move_ss(vTemp,vValues);
               // fReciprocalWidth*2,0,0,0
               ADDPS   XMM0,XMM0 // vTemp = _mm_add_ss(vTemp,vTemp);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[0]),XMM0 // M.r[0] = vTemp;
               // 0,fReciprocalHeight*2,0,0
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               ADDPS   XMM0, [g_XMMaskY] // vTemp = _mm_and_ps(vTemp,g_XMMaskY);
               ADDPS   XMM0,XMM0 // vTemp = _mm_add_ps(vTemp,vTemp);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[1]),XMM0 // M.r[1] = vTemp;
               // 0,0,fRange,0.0f
               MOVSS   XMM0, [rMem] // vTemp = vValues;
               ANDPS   XMM0, [g_XMMaskZ] // vTemp = _mm_and_ps(vTemp,g_XMMaskZ);
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[2]),XMM0 // M.r[2] = vTemp;
               // -(ViewLeft + ViewRight)*fReciprocalWidth,-(ViewTop + ViewBottom)*fReciprocalHeight,fRange*-NearZ,1.0f
               // vValues = _mm_mul_ps(vValues,rMem2);
               MOVSS   XMM0, [rMem]
               MULPS   XMM0, [rMem2]
               MOVUPS  TXMVECTOR(TXMMATRIX(result).r[3]),XMM0 // M.r[3] = vValues;
    end;
end;



function XMQuaternionMultiply(Q1: TXMVECTOR; Q2: TXMVECTOR): TXMVECTOR;
const
    ControlWZYX: TXMVECTORF32 = (f: (1.0, -1.0, 1.0, -1.0));
    ControlZWXY: TXMVECTORF32 = (f: (1.0, 1.0, -1.0, -1.0));
    ControlYXWZ: TXMVECTORF32 = (f: (-1.0, 1.0, 1.0, -1.0));
var
    Q2X, Q2Y, Q2Z, vResult, Q1Shuffle: TXMVECTOR;
begin
    // Copy to SSE registers and use as few as possible for x86
    Q2X := Q2;
    Q2Y := Q2;
    Q2Z := Q2;
    //  vResult := Q2;
    // Splat with one instruction
    asm
               MOVUPS  XMM0, [Q2]
               MOVUPS  XMM1, [Q2X]
               MOVUPS  XMM2, [Q2Y]
               MOVUPS  XMM3, [Q2Z]
               SHUFPS  XMM0, XMM0,_MM_SHUFFLE_3_3_3_3 // vResult := XM_PERMUTE_PS(vResult,_MM_SHUFFLE(3,3,3,3));
               SHUFPS  XMM1, XMM1,_MM_SHUFFLE_0_0_0_0 // Q2X := XM_PERMUTE_PS(Q2X,_MM_SHUFFLE(0,0,0,0));
               SHUFPS  XMM2, XMM2,_MM_SHUFFLE_1_1_1_1 // Q2Y := XM_PERMUTE_PS(Q2Y,_MM_SHUFFLE(1,1,1,1));
               SHUFPS  XMM3, XMM3,_MM_SHUFFLE_2_2_2_2 // Q2Z := XM_PERMUTE_PS(Q2Z,_MM_SHUFFLE(2,2,2,2));
               // Retire Q1 and perform Q1*Q2W
               MULPS   XMM0, [Q1] // vResult := _mm_mul_ps(vResult,Q1);
               //  Q1Shuffle := Q1;
               // Shuffle the copies of Q1
               MOVUPS  XMM4, [Q1]
               SHUFPS  XMM4, XMM4,_MM_SHUFFLE_0_1_2_3 // Q1Shuffle := XM_PERMUTE_PS(Q1Shuffle,_MM_SHUFFLE(0,1,2,3));
               // Mul by Q1WZYX
               MULPS   XMM1, XMM4 // Q2X := _mm_mul_ps(Q2X,Q1Shuffle);
               SHUFPS  XMM4, XMM4,_MM_SHUFFLE_2_3_0_1 // Q1Shuffle := XM_PERMUTE_PS(Q1Shuffle,_MM_SHUFFLE(2,3,0,1));
               // Flip the signs on y and z
               MULPS   XMM1, [ControlWZYX] // Q2X := _mm_mul_ps(Q2X,ControlWZYX);
               // Mul by Q1ZWXY
               MULPS   XMM1, XMM4 // Q2Y := _mm_mul_ps(Q2Y,Q1Shuffle);
               SHUFPS  XMM4, XMM4,_MM_SHUFFLE_0_1_2_3 // Q1Shuffle := XM_PERMUTE_PS(Q1Shuffle,_MM_SHUFFLE(0,1,2,3));
               // Flip the signs on z and w
               MULPS   XMM2,  [ControlZWXY] // Q2Y := _mm_mul_ps(Q2Y,ControlZWXY);
               // Mul by Q1YXWZ
               MULPS   XMM3, XMM4 // Q2Z := _mm_mul_ps(Q2Z,Q1Shuffle);
               ADDPS   XMM0, XMM1 // vResult := _mm_add_ps(vResult,Q2X);
               // Flip the signs on x and w
               MULPS   XMM3,  [ControlYXWZ] // Q2Z := _mm_mul_ps(Q2Z,ControlYXWZ);
               ADDPS   XMM2, XMM3// Q2Y := _mm_add_ps(Q2Y,Q2Z);
               ADDPS   XMM0, XMM1 // vResult := _mm_add_ps(vResult,Q2Y);
               MOVUPS  [result], XMM0// result:= vResult;
    end;

end;



function XMQuaternionConjugate(Q: TXMVECTOR): TXMVECTOR; assembler;
const
    NegativeOne3: TXMVECTORF32 = (f: (-1.0, -1.0, -1.0, 1.0));
asm
           VMOVUPS XMM0,[Q]
           VMULPS  XMM0, XMM0,[NegativeOne3]
           VMOVUPS [Result], XMM0
end;




function XMQuaternionSlerpV(Q0: TXMVECTOR; Q1: TXMVECTOR; T: TXMVECTOR): TXMVECTOR; inline;
const
    OneMinusEpsilon: TXMVECTORF32 = (f: (1.0 - 0.00001, 1.0 - 0.00001, 1.0 - 0.00001, 1.0 - 0.00001));
    SignMask2: TXMVECTORU32 = (u: ($80000000, $00000000, $00000000, $00000000));
var
    CosOmega, Zero, Control, Sign, Omega, SinOmega, V01, S1, S0: TXMVECTOR;
begin

    CosOmega := XMQuaternionDot(Q0, Q1);

    Zero := XMVectorZero();
    Control := XMVectorLess(CosOmega, Zero);
    Sign := XMVectorSelect(g_XMOne, g_XMNegativeOne, Control);
    asm
               // CosOmega = _mm_mul_ps(CosOmega, Sign);
               MOVUPS  XMM0, [CosOmega]
               MULPS   XMM0, [Sign]
               MOVUPS  [CosOmega],XMM0
    end;
    Control := XMVectorLess(CosOmega, OneMinusEpsilon);

    asm
               //  SinOmega = _mm_mul_ps(CosOmega,CosOmega);
               MOVUPS  XMM1,[CosOmega]
               MULPS   XMM1, XMM1
               // SinOmega = _mm_sub_ps(g_XMOne,SinOmega);
               MOVUPS  XMM0,[g_XMOne]
               SUBPS   XMM0, XMM1
               // SinOmega = _mm_sqrt_ps(SinOmega);
               SQRTPS  XMM0, XMM0
               MOVUPS  [SinOmega],XMM0
    end;

    Omega := XMVectorATan2(SinOmega, CosOmega);

    asm
               //  V01 = XM_PERMUTE_PS(T,_MM_SHUFFLE(2,3,0,1));
               MOVUPS  XMM0 ,[T]
               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_2_3_0_1
               // V01 = _mm_and_ps(V01,g_XMMaskXY);
               ANDPS   XMM0, [g_XMMaskXY]
               // V01 = _mm_xor_ps(V01,SignMask2);
               XORPS   XMM0, [SignMask2]
               // V01 = _mm_add_ps(g_XMIdentityR0, V01);
               ADDPS   XMM0, [g_XMIdentityR0]
               MOVUPS  [V01],XMM0
               //  S0 = _mm_mul_ps(V01, Omega);
               MULPS   XMM0, [Omega]
               MOVUPS  [S0],XMM0
    end;
    S0 := XMVectorSin(S0);
    asm
               // S0 = _mm_div_ps(S0, SinOmega);
               MOVUPS  XMM0, [S0]
               DIVPS   XMM0, [SinOmega]
               MOVUPS  [S0],XMM0
    end;
    S0 := XMVectorSelect(V01, S0, Control);

    S1 := XMVectorSplatY(S0);
    S0 := XMVectorSplatX(S0);

    asm
               //S1 = _mm_mul_ps(S1, Sign);
               MOVUPS  XMM1,[S1]
               MULPS   XMM1, [Sign]
               // Result = _mm_mul_ps(Q0, S0);
               MOVUPS  XMM0, [Q0]
               MULPS   XMM0, [S0]
               //S1 = _mm_mul_ps(S1, Q1);
               MULPS   XMM1, [Q1]
               //Result = _mm_add_ps(Result,S1);
               ADDPS   XMM0, XMM1
               //return Result;
               MOVUPS  [result],XMM0
    end;
end;



function XMQuaternionRotationNormal(NormalAxis: TXMVECTOR; Angle: single): TXMVECTOR; inline;
var
    N, vSine, vCosine, Scale: TXMVECTOR;
    f: single;
begin
    f := 0.5 * Angle;
    asm
               // N = _mm_and_ps(NormalAxis,g_XMMask3);
               MOVUPS  XMM0, [NormalAxis]
               ANDPS   XMM0, [g_XMMask3]
               // N = _mm_or_ps(N,g_XMIdentityR3);
               ORPS    XMM0, [g_XMIdentityR3]
               MOVUPS  [N],XMM0
               // Scale = _mm_set_ps1(0.5f * Angle);
               MOVSS   XMM1, [f]
               SHUFPS  XMM1, XMM1, 0
               MOVUPS  [Scale],XMM1
    end;
    XMVectorSinCos(vSine, vCosine, Scale);
    asm
               // Scale = _mm_and_ps(vSine,g_XMMask3);
               MOVUPS  XMM0, [vSine]
               ANDPS   XMM0, [g_XMMask3]
               // vCosine = _mm_and_ps(vCosine,g_XMMaskW);
               MOVUPS  XMM1, [vCosine]
               ANDPS   XMM1, [g_XMMaskW]
               // Scale = _mm_or_ps(Scale,vCosine);
               ORPS    XMM0,  XMM1
               // N = _mm_mul_ps(N,Scale);
               MULPS   XMM0, [N]
               // return N;
               MOVUPS  [result],XMM0
    end;
end;



function XMQuaternionRotationMatrix(M: TXMMATRIX): TXMVECTOR; inline;
const
    XMPMMP: TXMVECTORF32 = (f: (+1.0, -1.0, -1.0, +1.0));
    XMMPMP: TXMVECTORF32 = (f: (-1.0, +1.0, -1.0, +1.0));
    XMMMPP: TXMVECTORF32 = (f: (-1.0, -1.0, +1.0, +1.0));
var
    r0, r1, r2: TXMVECTOR;
    r00, r11, r22: TXMVECTOR;
    x2gey2, z2gew2, x2py2gez2pw2: TXMVECTOR;
    t0, t1, t2, x2y2z2w2: TXMVECTOR;
    xyxzyz, xwywzw: TXMVECTOR;
    tensor0, tensor1, tensor2, tensor3: TXMVECTOR;
begin

    r0 := M.r[0];  // (r00, r01, r02, 0)
    r1 := M.r[1];  // (r10, r11, r12, 0)
    r2 := M.r[2];  // (r20, r21, r22, 0)

    // (r00, r00, r00, r00)
    asm
               //  r00 = XM_PERMUTE_PS(r0, _MM_SHUFFLE(0,0,0,0));
               MOVUPS  XMM0,  [r0]
               SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
               MOVUPS  [r00],XMM0

               // (r11, r11, r11, r11)
               //  r11 = XM_PERMUTE_PS(r1, _MM_SHUFFLE(1,1,1,1));
               MOVUPS  XMM1, [r1]
               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
               MOVUPS  [R11],XMM1
               // (r22, r22, r22, r22)
               //  r22 = XM_PERMUTE_PS(r2, _MM_SHUFFLE(2,2,2,2));
               MOVUPS  XMM2, [r2]
               SHUFPS  XMM2, XMM2, _MM_SHUFFLE_2_2_2_2
               MOVUPS  [r22],XMM2


               // x^2 >= y^2 equivalent to r11 - r00 <= 0
               // (r11 - r00, r11 - r00, r11 - r00, r11 - r00)
               //  r11mr00 = _mm_sub_ps(r11, r00);
               MOVUPS  XMM3,XMM1
               SUBPS   XMM3, XMM0
               //  x2gey2 = _mm_cmple_ps(r11mr00, g_XMZero);
               CMPPS   XMM3, [g_XMZero], 2
               MOVUPS  [x2gey2],XMM3

               // z^2 >= w^2 equivalent to r11 + r00 <= 0
               // (r11 + r00, r11 + r00, r11 + r00, r11 + r00)
               // r11pr00 = _mm_add_ps(r11, r00);
               MOVUPS  XMM3,XMM1
               ADDPS   XMM3, XMM0
               //  z2gew2 = _mm_cmple_ps(r11pr00, g_XMZero);
               CMPPS   XMM3, [g_XMZero], 2
               MOVUPS  [z2gew2],XMM3

               // x^2 + y^2 >= z^2 + w^2 equivalent to r22 <= 0
               // x2py2gez2pw2 = _mm_cmple_ps(r22, g_XMZero);
               MOVUPS  XMM3,XMM2
               CMPPS   XMM3, [g_XMZero], 2
               MOVUPS  [x2py2gez2pw2],XMM3

               // (+r00, -r00, -r00, +r00)
               // t0 = _mm_mul_ps(XMPMMP, r00);
               MULPS   XMM0, [XMPMMP]

               // (-r11, +r11, -r11, +r11)
               // t1 = _mm_mul_ps(XMMPMP, r11);
               MULPS   XMM1, [XMMPMP]

               // (-r22, -r22, +r22, +r22)
               //  t2 = _mm_mul_ps(XMMMPP, r22);
               MULPS   XMM2, [XMMMPP]

               // (4*x^2, 4*y^2, 4*z^2, 4*w^2)
               //  x2y2z2w2 = _mm_add_ps(t0, t1);
               MOVUPS  XMM3,XMM0
               ADDPS   XMM3, XMM1
               // x2y2z2w2 = _mm_add_ps(t2, x2y2z2w2);
               ADDPS   XMM3, XMM2
               // x2y2z2w2 = _mm_add_ps(x2y2z2w2, g_XMOne);
               ADDPS   XMM3, [g_XMOne]
               MOVUPS  [x2y2z2w2],XMM3

               // (r01, r02, r12, r11)
               // t0 = _mm_shuffle_ps(r0, r1, _MM_SHUFFLE(1,2,2,1));
               MOVUPS  XMM0, [r0]
               SHUFPS  XMM0, [r1], _MM_SHUFFLE_1_2_2_1
               // (r10, r10, r20, r21)
               // t1 = _mm_shuffle_ps(r1, r2, _MM_SHUFFLE(1,0,0,0));
               MOVUPS  XMM1, [r1]
               SHUFPS  XMM1, [r2], _MM_SHUFFLE_1_0_0_0
               // (r10, r20, r21, r10)
               // t1 = XM_PERMUTE_PS(t1, _MM_SHUFFLE(1,3,2,0));
               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_3_2_0
               // (4*x*y, 4*x*z, 4*y*z, unused)
               // xyxzyz = _mm_add_ps(t0, t1);
               ADDPS   XMM0, XMM1
               MOVUPS  [xyxzyz], XMM0


               // (r21, r20, r10, r10)
               // t0 = _mm_shuffle_ps(r2, r1, _MM_SHUFFLE(0,0,0,1));
               MOVUPS  XMM0, [r2]
               SHUFPS  XMM0, [r1], _MM_SHUFFLE_0_0_0_1
               // (r12, r12, r02, r01)
               // t1 = _mm_shuffle_ps(r1, r0, _MM_SHUFFLE(1,2,2,2));
               MOVUPS  XMM1, [r1]
               SHUFPS  XMM1, [r0], _MM_SHUFFLE_1_2_2_2
               // (r12, r02, r01, r12)
               // t1 = XM_PERMUTE_PS(t1, _MM_SHUFFLE(1,3,2,0));
               SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_3_2_0
               // (4*x*w, 4*y*w, 4*z*w, unused)
               //  xwywzw = _mm_sub_ps(t0, t1);
               SUBPS   XMM0, XMM1
               // xwywzw = _mm_mul_ps(XMMPMP, xwywzw);
               MULPS   XMM0, [XMMPMP]
               MOVUPS  [xwywzw], XMM0

               // (4*x^2, 4*y^2, 4*x*y, unused)
               //t0 = _mm_shuffle_ps(x2y2z2w2, xyxzyz, _MM_SHUFFLE(0,0,1,0));
               MOVUPS  XMM0,[x2y2z2w2]
               SHUFPS  XMM0, [xyxzyz], _MM_SHUFFLE_0_0_1_0
               // (4*z^2, 4*w^2, 4*z*w, unused)
               //t1 = _mm_shuffle_ps(x2y2z2w2, xwywzw, _MM_SHUFFLE(0,2,3,2));
               MOVUPS  XMM1, [x2y2z2w2]
               SHUFPS  XMM1, [xwywzw], _MM_SHUFFLE_0_2_3_2
               // (4*x*z, 4*y*z, 4*x*w, 4*y*w)
               //t2 = _mm_shuffle_ps(xyxzyz, xwywzw, _MM_SHUFFLE(1,0,2,1));
               MOVUPS  XMM2,[xyxzyz]
               SHUFPS  XMM2,[xwywzw], _MM_SHUFFLE_1_0_2_1

               // (4*x*x, 4*x*y, 4*x*z, 4*x*w)
               //  tensor0 = _mm_shuffle_ps(t0, t2, _MM_SHUFFLE(2,0,2,0));
               MOVUPS  XMM3, XMM0
               SHUFPS  XMM3, XMM2, _MM_SHUFFLE_2_0_2_0
               MOVUPS  [tensor0],XMM3
               // (4*y*x, 4*y*y, 4*y*z, 4*y*w)
               //  tensor1 = _mm_shuffle_ps(t0, t2, _MM_SHUFFLE(3,1,1,2));
               MOVUPS  XMM3,XMM0
               SHUFPS  XMM3, XMM2, _MM_SHUFFLE_3_1_1_2
               MOVUPS  [tensor1],XMM3
               // (4*z*x, 4*z*y, 4*z*z, 4*z*w)
               //  tensor2 = _mm_shuffle_ps(t2, t1, _MM_SHUFFLE(2,0,1,0));
               MOVUPS  XMM3,XMM2
               SHUFPS  XMM3, XMM1, _MM_SHUFFLE_2_0_1_0
               MOVUPS  [tensor2],XMM3
               // (4*w*x, 4*w*y, 4*w*z, 4*w*w)
               //  tensor3 = _mm_shuffle_ps(t2, t1, _MM_SHUFFLE(1,2,3,2));
               MOVUPS  XMM3,XMM2
               SHUFPS  XMM3, XMM1, _MM_SHUFFLE_1_2_3_2
               MOVUPS  [tensor3],XMM3

               // Select the row of the tensor-product matrix that has the largest
               // magnitude.
               // t0 = _mm_and_ps(x2gey2, tensor0);
               MOVUPS  XMM0, [x2gey2]
               ANDPS   XMM0, [tensor0]
               // t1 = _mm_andnot_ps(x2gey2, tensor1);
               MOVUPS  XMM1, [x2gey2]
               ANDNPS  XMM1, [tensor1]
               // t0 = _mm_or_ps(t0, t1);
               ORPS    XMM0, XMM1
               // t1 = _mm_and_ps(z2gew2, tensor2);
               MOVUPS  XMM1, [z2gew2]
               ANDPS   XMM1, [tensor2]
               // t2 = _mm_andnot_ps(z2gew2, tensor3);
               MOVUPS  XMM2,[z2gew2]
               ANDNPS  XMM2, [tensor3]
               // t1 = _mm_or_ps(t1, t2);
               ORPS    XMM1, XMM2
               // t0 = _mm_and_ps(x2py2gez2pw2, t0);
               ANDPS   XMM0, [x2py2gez2pw2]
               // t1 = _mm_andnot_ps(x2py2gez2pw2, t1);
               ANDNPS  XMM1, [x2py2gez2pw2]
               // t2 = _mm_or_ps(t0, t1);
               ORPS    XMM0, XMM1
               MOVUPS  [t2],XMM0
    end;
    // Normalize the row.  No division by zero is possible because the
    // quaternion is unit-length (and the row is a nonzero multiple of
    // the quaternion).
    t0 := XMVector4Length(t2);
    asm
               // return _mm_div_ps(t2, t0);
               MOVUPS  XMM0,[t2]
               DIVPS   XMM0, [t0]
               MOVUPS  [result],XMM0
    end;
end;



{$IF  DEFINED(_XM_SSE4_INTRINSICS_) }

function XMPlaneNormalizeEst(P: TXMVECTOR): TXMVECTOR; assembler;
asm
           VMOVUPS XMM0, [P]
           VDPPS   XMM1, XMM0,XMM0, $7F  // vTemp in XMM1, P in XMM0
           VRSQRTPS XMM2, XMM1
           VMULPS  XMM0, XMM2, XMM0
           VMOVUPS [Result], XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMPlaneNormalizeEst(P: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product
           MOVUPS  XMM0, [P]
           MULPS   XMM0, XMM0  // vDot = _mm_mul_ps(P,P);

           // x=Dot.y, y=Dot.z
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_1_2_1 // vTemp = XM_PERMUTE_PS(vDot,_MM_SHUFFLE(2,1,2,1));
           // Result.x = x+y
           ADDSS   XMM0, XMM1 // vDot = _mm_add_ss(vDot,vTemp);

           // x=Dot.z
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1 // vTemp = XM_PERMUTE_PS(vTemp,_MM_SHUFFLE(1,1,1,1));

           // Result.x = (x+y)+z
           ADDSS   XMM0, XMM1 // vDot = _mm_add_ss(vDot,vTemp);

           // Splat x
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0 // vDot = XM_PERMUTE_PS(vDot,_MM_SHUFFLE(0,0,0,0));

           // Get the reciprocal
           RSQRTPS XMM0, XMM0 // vDot = _mm_rsqrt_ps(vDot);

           // Get the reciprocal
           MULPS   XMM0, [P] // vDot = _mm_mul_ps(vDot,P);
           MOVUPS  [result], XMM0 // return vDot;
end;

{$ENDIF}


{$IF DEFINED(_XM_SSE4_INTRINSICS_)}

function XMPlaneNormalize(P: TXMVECTOR): TXMVECTOR; assembler;
asm
           //  vLengthSq = _mm_dp_ps( P, P, $7f );
           MOVUPS  XMM0 ,[P]
           DPPS    XMM0, XMM0, $7f
           // Prepare for the division
           //  vResult = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM1, XMM0
           // Failsafe on zero (Or epsilon) length planes
           // If the length is infinity, set the elements to zero
           // vLengthSq = _mm_cmpneq_ps(vLengthSq,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 4
           // Reciprocal mul to perform the normalization
           // vResult = _mm_div_ps(P,vResult);
           MOVUPS  XMM2, [P]
           DIVPS   XMM2, XMM1
           // Any that are infinity, set to zero
           // vResult = _mm_and_ps(vResult,vLengthSq);
           ANDPS   XMM0, XMM2
           // return vResult;
           MOVUPS  [result],XMM0
end;
{$ELSE}// _XM_SSE_INTRINSICS_


function XMPlaneNormalize(P: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Perform the dot product on x,y and z only
           // vLengthSq = _mm_mul_ps(P,P);
           MOVUPS  XMM0,[P]
           MULPS   XMM0, XMM0
           // vTemp = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(2,1,2,1));
           MOVUPS  XMM1,XMM0
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_2_1_2_1
           //vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDSS   XMM0, XMM1
           //vTemp = XM_PERMUTE_PS(vTemp,_MM_SHUFFLE(1,1,1,1));
           SHUFPS  XMM1, XMM1, _MM_SHUFFLE_1_1_1_1
           //vLengthSq = _mm_add_ss(vLengthSq,vTemp);
           ADDSS   XMM0, XMM1
           //vLengthSq = XM_PERMUTE_PS(vLengthSq,_MM_SHUFFLE(0,0,0,0));
           SHUFPS  XMM0, XMM0, _MM_SHUFFLE_0_0_0_0
           // Prepare for the division
           // vResult = _mm_sqrt_ps(vLengthSq);
           SQRTPS  XMM1, XMM0
           // Failsafe on zero (Or epsilon) length planes
           // If the length is infinity, set the elements to zero
           //vLengthSq = _mm_cmpneq_ps(vLengthSq,g_XMInfinity);
           CMPPS   XMM0, [g_XMInfinity], 4
           // Reciprocal mul to perform the normalization
           //vResult = _mm_div_ps(P,vResult);
           MOVUPS  XMM2, [P]
           DIVPS   XMM2, XMM1
           // Any that are infinity, set to zero
           //vResult = _mm_and_ps(vResult,vLengthSq);
           ANDPS   XMM0, XMM2
           //return vResult;
           MOVUPS  [result],XMM0

end;
{$ENDIF}


//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------


function XMColorNegative(constref vColor: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Negate only x,y and z.
           VMOVUPS XMM0,[vColor]
           VMOVUPS XMM1, [g_XMNegate3]
           VXORPS  XMM1,XMM0,XMM1 // vTemp in XMM1
           // Add 1,1,1,0 to -x,-y,-z,w
           VADDPS  XMM0, XMM1, [g_XMOne3]
           VMOVUPS [Result], XMM0
end;



function XMColorAdjustSaturation(constref vColor: TXMVECTOR; Saturation: single): TXMVECTOR; inline;
const
    gvLuminance: TXMVECTORF32 = (f: (0.2125, 0.7154, 0.0721, 0.0));
var
    vLuminance: TXMVECTOR;
begin
    vLuminance := XMVector3Dot(vColor, gvLuminance);
    // Splat fSaturation
    asm
               // vSaturation = _mm_set_ps1(fSaturation);
               MOVSS   XMM1, [Saturation]
               SHUFPS  XMM1, XMM1, 0
               // vResult = ((vColor-vLuminance)*vSaturation)+vLuminance;
               // vResult = _mm_sub_ps(vColor,vLuminance);
               MOVUPS  XMM0 , [vColor]
               SUBPS   XMM0, [vLuminance]
               // vResult = _mm_mul_ps(vResult,vSaturation);
               MULPS   XMM0, XMM1
               // vResult = _mm_add_ps(vResult,vLuminance);
               ADDPS   XMM0, [vLuminance]
               // Retain w from the source color
               // vLuminance = _mm_shuffle_ps(vResult,vColor,_MM_SHUFFLE(3,2,2,2));   // x = vResult.z,y = vResult.z,z = vColor.z,w=vColor.w
               MOVUPS  XMM1, XMM0
               SHUFPS  XMM1, [vColor], _MM_SHUFFLE_3_2_2_2
               // vResult = _mm_shuffle_ps(vResult,vLuminance,_MM_SHUFFLE(3,0,1,0));  // x = vResult.x,y = vResult.y,z = vResult.z,w=vColor.w
               SHUFPS  XMM0, XMM1, _MM_SHUFFLE_3_0_1_0
               // return vResult;
               MOVUPS  [result],XMM0
    end;
end;



function XMColorAdjustContrast(constref vColor: TXMVECTOR; constref Contrast: single): TXMVECTOR; assembler;
asm
           VMOVUPS XMM0, [vColor]         // vColor in XMM0
           VBROADCASTSS XMM1,[Contrast]  // Splat the scale , vScale in XMM1

           VSUBPS  XMM2, XMM0, [g_XMOneHalf] // Subtract 0.5f from the source (Saving source), vResult in XMM2
           VMULPS  XMM2, XMM2, XMM1          // Mul by scale
           VADDPS  XMM2, XMM2, [g_XMOneHalf]   // Add 0.5f

           // Retain w from the source color
           VSHUFPS XMM1, XMM2,XMM0, _MM_SHUFFLE_3_2_2_2       // x = vResult.z,y = vResult.z,z = vColor.z,w=vColor.w
           VSHUFPS XMM0, XMM2, XMM1, _MM_SHUFFLE_3_0_1_0      // x = vResult.x,y = vResult.y,z = vResult.z,w=vColor.w

           VMOVUPS [Result], XMM0
end;




{****************************************************************************
 *
 * Miscellaneous
 *
 ****************************************************************************}


// see https://stackoverflow.com/questions/41507019/code-detects-mmx-sse-avx-but-not-avx2
function GetCPUID(Leaf, Subleaf: integer): TCPUInfo;
asm
{$IF Defined(CPUX86)}
           PUSH    EBX
           PUSH    EDI
           MOV     EDI, ECX
           MOV     ECX, EDX
           CPUID
           MOV     [EDI+$0], EAX
           MOV     [EDI+$4], EBX
           MOV     [EDI+$8], ECX
           MOV     [EDI+$c], EDX
           POP     EDI
           POP     EBX
{$ELSEIF Defined(CPUX64)}
           MOV     R9,RCX
           MOV     ECX,R8D
           MOV     R8,RBX
           MOV     EAX,EDX
           CPUID
           MOV     [R9+$0], EAX
           MOV     [R9+$4], EBX
           MOV     [R9+$8], ECX
           MOV     [R9+$c], EDX
           MOV     RBX, R8
{$ELSE}
  {$Message Fatal 'GetCPUID has not been implemented for this architecture.'}
{$IFEND}
end;



function XMVerifyCPUSupport(CPURequired: TCPUType): boolean;
var

    CPUInfo: TCPUInfo;
    Leaf: cardinal = 0;
    Subleaf: cardinal = 0;

begin
    CPUInfo := GetCPUID(0, 0);

    if CPURequired = CPU_AVX2 then
    begin
        if (CPUInfo[0] < 7) then
        begin
            Result := False;
            Exit;
        end;
    end
    else
    begin
        if (CPUInfo[0] < 1) then
        begin
            Result := False;
            Exit;
        end;
    end;

    CPUInfo := GetCPUID(1, 0);
    if CPURequired = CPU_AVX2 then
    begin
        // The compiler can emit FMA3 instructions even without explicit intrinsics use
        if ((CPUInfo[2] and $38081001) <> $38081001) then
        begin
            Result := False; // No F16C/AVX/OSXSAVE/SSE4.1/FMA3/SSE3 support
            Exit;
        end;
    end;
    (* ToDo
#elif defined(_XM_FMA3_INTRINSICS_) && defined(_XM_F16C_INTRINSICS_)
    if ((CPUInfo[2] and $38081001) <> $38081001) then
    begin
        result:= false; // No F16C/AVX/OSXSAVE/SSE4.1/FMA3/SSE3 support
        Exit;
    end;
#elif defined(_XM_FMA3_INTRINSICS_)
    if ((CPUInfo[2] and $18081001) <> $18081001) then
    begin
        result:= false; // No AVX/OSXSAVE/SSE4.1/FMA3/SSE3 support
        Exit;
    end;
#elif defined(_XM_F16C_INTRINSICS_)
    if ((CPUInfo[2] and $38080001) <> $38080001) then
    begin
        result:= false; // No F16C/AVX/OSXSAVE/SSE4.1/SSE3 support
        Exit;
    end;
        *)
    if (CPURequired = CPU_AVX2) then
    begin
        if ((CPUInfo[2] and $18080001) <> $18080001) then
        begin
            Result := False; // No AVX/OSXSAVE/SSE4.1/SSE3 support
            Exit;
        end;
    end;
    if (CPURequired = CPU_XM_SSE4) then
    begin
        if ((CPUInfo[2] and $80001) <> $80001) then
        begin
            Result := False; // No SSE3/SSE4.1 support
            Exit;
        end;
    end;
    if (CPURequired = CPU_XM_SSE3) then
    begin
        if ((CPUInfo[2] and $1) <> $1) then
        begin
            Result := False; // No SSE3 support
            Exit;
        end;
    end;

    // The x64 processor model requires SSE2 support, but no harm in checking
    if ((CPUInfo[3] and $6000000) <> $6000000) then
    begin
        Result := False; // No SSE2/SSE support
        Exit;
    end;
    if CPURequired = CPU_AVX2 then
    begin
        CPUInfo := GetCPUID(7, 0);
        if ((CPUInfo[1] and $20) <> $20) then
        begin
            Result := False; // No AVX2 support
            exit;
        end;
    end;
    Result := True;
end;

{$ENDIF}

//------------------------------------------------------------------------------

function XMVerifyCPUSupport2(): boolean; inline;
begin
    (* ToDo
{$IF  defined(_XM_SSE_INTRINSICS_) AND NOT defined(_XM_NO_INTRINSICS_)}
      GetCPUID
    int CPUInfo[4] = { -1 };
    __cpuid(CPUInfo, 0);

    {$ifdef __AVX2__ }
        if (CPUInfo[0] < 7) then
        begin
            result:= false;
            Exit;
        end;
    {$ELSE}
        if (CPUInfo[0] < 1) then
        begin
            result:= false;
            Exit;
        end;
    {$ENDIF}

    __cpuid(CPUInfo, 1);

    {$IF  defined(__AVX2__) OR defined(_XM_AVX2_INTRINSICS_)}
        // The compiler can emit FMA3 instructions even without explicit intrinsics use
        if ((CPUInfo[2] & $38081001) <> $38081001) then
        begin
            result:= false; // No F16C/AVX/OSXSAVE/SSE4.1/FMA3/SSE3 support
            Exit;
        end;
    {$ELSEIF  defined(_XM_FMA3_INTRINSICS_) AND defined(_XM_F16C_INTRINSICS_)
        if ((CPUInfo[2] & $38081001) <> $38081001) then
        begin
            result:= false; // No F16C/AVX/OSXSAVE/SSE4.1/FMA3/SSE3 support
            Exit;
        end;
    {$ELSEIF  defined(_XM_FMA3_INTRINSICS_)
        if ((CPUInfo[2] & $18081001) <> $18081001) then
        begin
            result:= false; // No AVX/OSXSAVE/SSE4.1/FMA3/SSE3 support
            Exit;
        end;
    {$ELSEIF  defined(_XM_F16C_INTRINSICS_)
        if ((CPUInfo[2] & $38080001) <> $38080001) then
        begin
            result:= false; // No F16C/AVX/OSXSAVE/SSE4.1/SSE3 support
            Exit;
        end;
    {$ELSEIF  defined(__AVX__)  OR  defined(_XM_AVX_INTRINSICS_)
        if ((CPUInfo[2] & $18080001) <> $18080001)then
        begin
            result:= false; // No AVX/OSXSAVE/SSE4.1/SSE3 support
            Exit;
        end;
    {$ELSEIF  defined(_XM_SSE4_INTRINSICS_)}
        if ((CPUInfo[2] & $80001) <> $80001)then
        begin
            result:= false; // No SSE3/SSE4.1 support
            Exit;
        end;
    {$ELSEIF  defined(_XM_SSE3_INTRINSICS_)}
        if (!(CPUInfo[2] & $1))  then
        begin
            result:= false; // No SSE3 support
            Exit;
        end;
    {$ENDIF}

    // The x64 processor model requires SSE2 support, but no harm in checking
    if ((CPUInfo[3] & $6000000) <> $6000000)  then
    begin
        result:= false; // No SSE2/SSE support
        Exit;
    end;

    {$IF  defined(__AVX2__)  OR  defined(_XM_AVX2_INTRINSICS_)}
        __cpuidex(CPUInfo, 7, 0);
        if (!(CPUInfo[1] & $20)) then
        begin
            result:= false; // No AVX2 support
            Exit;
            end;
    {$ENDIF}

    result:= true;
{$ELSEIF  defined(_XM_ARM_NEON_INTRINSICS_) AND NOT defined(_XM_NO_INTRINSICS_)}
    // ARM-NEON support is required for the Windows on ARM platform
    result:= true;
{$ELSE}
    // No intrinsics path always supported
    result:= true;
{$ENDIF}
*)
end;



function XMFresnelTerm(CosIncidentAngle: TXMVECTOR; RefractionIndex: TXMVECTOR): TXMVECTOR; assembler;
var
    GAddC, GSubC, vResult: TXMVECTOR;
asm
           // G = sqrt(abs((RefractionIndex^2-1) + CosIncidentAngle^2))
           //  G = _mm_mul_ps(RefractionIndex,RefractionIndex);
           MOVUPS  XMM0,[RefractionIndex]
           MULPS   XMM0, XMM0
           //  vTemp = _mm_mul_ps(CosIncidentAngle,CosIncidentAngle);
           MOVUPS  XMM1,[CosIncidentAngle]
           MULPS   XMM1, XMM1
           // G = _mm_sub_ps(G,g_XMOne);
           SUBPS   XMM0, [g_XMOne]
           // vTemp = _mm_add_ps(vTemp,G);
           ADDPS   XMM1, XMM0
           // max((0-vTemp),vTemp) == abs(vTemp)
           // The abs is needed to deal with refraction and cosine being zero
           // G = _mm_setzero_ps();
           XORPS   XMM0, XMM0
           // G = _mm_sub_ps(G,vTemp);
           SUBPS   XMM0, XMM1
           // G = _mm_max_ps(G,vTemp);
           MAXPS   XMM0, XMM1
           // Last operation, the sqrt()
           // G = _mm_sqrt_ps(G);
           SQRTPS  XMM0, XMM0

           // Calc G-C and G+C
           //  GAddC = _mm_add_ps(G,CosIncidentAngle);
           MOVUPS  XMM1, [CosIncidentAngle]
           ADDPS   XMM1, XMM0
           MOVUPS  [GAddC],XMM1
           //  GSubC = _mm_sub_ps(G,CosIncidentAngle);
           MOVUPS  XMM2, XMM0
           SUBPS   XMM2, [CosIncidentAngle]
           MOVUPS  [GSubC],XMM2
           // Perform the term (0.5f *(g - c)^2) / (g + c)^2
           //  vResult = _mm_mul_ps(GSubC,GSubC);
           MULPS   XMM2, XMM2
           // vTemp = _mm_mul_ps(GAddC,GAddC);
           MULPS   XMM1, XMM1
           // vResult = _mm_mul_ps(vResult,g_XMOneHalf);
           MULPS   XMM2, [g_XMOneHalf]
           // vResult = _mm_div_ps(vResult,vTemp);
           DIVPS   XMM2, XMM1
           MOVUPS  [vResult],XMM2

           // Perform the term ((c * (g + c) - 1)^2 / (c * (g - c) + 1)^2 + 1)
           // GAddC = _mm_mul_ps(GAddC,CosIncidentAngle);
           MOVUPS  XMM0, [GAddC]
           MULPS   XMM0, [CosIncidentAngle]
           // GSubC = _mm_mul_ps(GSubC,CosIncidentAngle);
           MOVUPS  XMM1, [GSubC]
           MULPS   XMM1, [CosIncidentAngle]
           // GAddC = _mm_sub_ps(GAddC,g_XMOne);
           SUBPS   XMM0, [g_XMOne]
           // GSubC = _mm_add_ps(GSubC,g_XMOne);
           ADDPS   XMM1, [g_XMOne]
           // GAddC = _mm_mul_ps(GAddC,GAddC);
           MULPS   XMM0, XMM0
           // GSubC = _mm_mul_ps(GSubC,GSubC);
           MULPS   XMM1, XMM1
           // GAddC = _mm_div_ps(GAddC,GSubC);
           DIVPS   XMM0, XMM1
           // GAddC = _mm_add_ps(GAddC,g_XMOne);
           ADDPS   XMM0, [g_XMOne]
           // Multiply the two term parts
           // vResult = _mm_mul_ps(vResult,GAddC);
           MULPS   XMM0, [vResult]
           // Clamp to 0.0 - 1.0
           // vResult = _mm_max_ps(vResult,g_XMZero);
           MAXPS   XMM0, [g_XMZero]
           // vResult = _mm_min_ps(vResult,g_XMOne);
           MINPS   XMM0, [g_XMOne]
           // return vResult;
           MOVUPS  [result],XMM0
end;




function XMVectorDivide(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR; assembler;
asm
           // return _mm_div_ps( V1, V2 );
           MOVUPS  XMM0, [V1]
           DIVPS   XMM0, [V2]
           MOVUPS  [result],XMM0
end;


//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------

function XMVectorNegate(constref V: TXMVECTOR): TXMVECTOR; assembler;
asm
           // Z = _mm_setzero_ps();
           XORPS   XMM0,XMM0
           // return _mm_sub_ps( Z, V );
           SUBPS   XMM0,[V]
           MOVUPS  [result],XMM0
end;




class operator TXMMATRIX.Divide(M: TXMMATRIX; s: single): TXMMATRIX; assembler;
asm
           // __m128 vS = _mm_set_ps1( S );
           MOVSS   XMM4, S
           SHUFPS  XMM4, XMM0, 0
           // r[0] = _mm_div_ps( r[0], vS );
           MOVUPS   XMM0, TXMVECTOR(TXMMATRIX([M]).R[0])
           DIVPS   XMM0, XMM4
           MOVUPS   TXMVECTOR(TXMMATRIX([result]).R[0]),XMM0
           // r[1] = _mm_div_ps( r[1], vS );
           MOVUPS   XMM0, TXMVECTOR(TXMMATRIX([M]).R[1])
           DIVPS   XMM0, XMM4
           MOVUPS   TXMVECTOR(TXMMATRIX([result]).R[1]),XMM0
           // r[2] = _mm_div_ps( r[2], vS );
           MOVUPS   XMM0, TXMVECTOR(TXMMATRIX([M]).R[2])
           DIVPS   XMM0, XMM4
           MOVUPS   TXMVECTOR(TXMMATRIX([result]).R[2]),XMM0
           // r[3] = _mm_div_ps( r[3], vS );
           MOVUPS   XMM0, TXMVECTOR(TXMMATRIX([M]).R[3])
           DIVPS   XMM0, XMM4
           MOVUPS   TXMVECTOR(TXMMATRIX([result]).R[3]),XMM0

end;


{$IF DEFINED(_XM_AVX_INTRINSICS_)}

function XMVectorSwizzle(constref V: TXMVECTOR; constref SwizzleX, SwizzleY, SwizzleZ, SwizzleW: UINT32): TXMVECTOR; inline;
var
    elem: array[0..3] of UINT32;
begin
    elem[0] := SwizzleX;
    elem[1] := SwizzleY;
    elem[2] := SwizzleZ;
    elem[3] := SwizzleW;
    asm
               // unsigned int elem[4] = { E0, E1, E2, E3 };
               //_m128i vControl = _mm_loadu_si128( reinterpret_cast<const __m128i *>(&elem[0]) );
               MOVDQU  XMM0, [elem]
               // return _mm_permutevar_ps( V, vControl );
               MOVUPS  XMM1,[V]
               VPERMILPS XMM0, XMM1, XMM0
               MOVUPS  [result],XMM0
    end;
end;

{$ELSE}
//------------------------------------------------------------------------------
function XMVectorSwizzle(constref V: TXMVECTOR; constref SwizzleX, SwizzleY, SwizzleZ, SwizzleW: UINT32): TXMVECTOR;
begin
    assert((SwizzleX < 4) and (SwizzleY < 4) and (SwizzleZ < 4) and (SwizzleW < 4));

    Result.f32[0] := V.f32[SwizzleX];
    Result.f32[1] := V.f32[SwizzleY];
    Result.f32[2] := V.f32[SwizzleZ];
    Result.f32[3] := V.f32[SwizzleW];
end;

{$ENDIF}



{$IF DEFINED(_XM_AVX_INTRINSICS_)}

function XMVectorPermute(V1: TXMVECTOR; V2: TXMVECTOR; PermuteX: UINT32; PermuteY: UINT32; PermuteZ: UINT32; PermuteW: UINT32): TXMVECTOR; inline;
const
    three: TXMVECTORU32 = (u: (3, 3, 3, 3));
var
    elem: array[0..3] of uint32;
begin
    elem[0] := PermuteX;
    elem[1] := PermuteY;
    elem[2] := PermuteZ;
    elem[3] := PermuteW;
    asm

               //  vControl = _mm_load_si128( reinterpret_cast<const __m128i *>(&elem[0]) );
               MOVUPS  XMM0,[elem]
               //  vSelect = _mm_cmpgt_epi32( vControl, three );
               MOVUPS  XMM1,XMM0
               PCMPGTD XMM1, [three]
               //vControl = _mm_castps_si128( _mm_and_ps( _mm_castsi128_ps( vControl ), three ) );
               ANDPS   XMM0, [three]
               //  shuffled1 = _mm_permutevar_ps( V1, vControl );
               MOVUPS  XMM2, [V1]
               VPERMILPS XMM2, XMM2, XMM0
               //  shuffled2 = _mm_permutevar_ps( V2, vControl );
               MOVUPS  XMM3, [V2]
               VPERMILPS XMM3, XMM3, XMM0
               //  masked1 = _mm_andnot_ps( _mm_castsi128_ps( vSelect ), shuffled1 );
               ANDNPS  XMM2, XMM1
               //  masked2 = _mm_and_ps( _mm_castsi128_ps( vSelect ), shuffled2 );
               ANDPS   XMM3,XMM1
               // return _mm_or_ps( masked1, masked2 );
               ORPS    XMM2, XMM3
               MOVUPS  [result],XMM2
    end;
end;

{$ELSE}

function XMVectorPermute(V1: TXMVECTOR; V2: TXMVECTOR; PermuteX: UINT32; PermuteY: UINT32; PermuteZ: UINT32; PermuteW: UINT32): TXMVECTOR; inline;
var
    aPtr: array [0..1] of TXMVECTOR;
    i0, i1, i2, i3: UINT32;
    vi0, vi1, vi2, vi3: UINT32;
begin
    assert((PermuteX <= 7) and (PermuteY <= 7) and (PermuteZ <= 7) and (PermuteW <= 7));
    aPtr[0] := V1;
    aPtr[1] := V2;

    i0 := PermuteX and 3;
    vi0 := PermuteX shr 2;
    Result.f32[0] := aPtr[vi0].f32[i0];

    i1 := PermuteY and 3;
    vi1 := PermuteY shr 2;
    Result.f32[1] := aPtr[vi1].f32[i1];

    i2 := PermuteZ and 3;
    vi2 := PermuteZ shr 2;
    Result.f32[2] := aPtr[vi2].f32[i2];

    i3 := PermuteW and 3;
    vi3 := PermuteW shr 2;
    Result.f32[3] := aPtr[vi3].f32[i3];
end;

{$ENDIF}

//end.
