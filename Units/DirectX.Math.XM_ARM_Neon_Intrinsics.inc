{****************************************************************************
 *
 * Data conversion
 *
 ****************************************************************************}

// Returns the converted vector, where each component has been divided by two raised to the DivExponent power.
function XMConvertVectorIntToFloat(constref VInt: TXMVECTOR; constref DivExponent: UINT32): TXMVECTOR;
begin
    (*
    float fScale = 1.0f / (float)(1U << DivExponent);
    float32x4_t vResult = vcvtq_f32_s32( VInt );
    return vmulq_n_f32( vResult, fScale );
    *)
end;

//Returns the converted vector, where each component has been multiplied by two raised to the MulExponent power.
function XMConvertVectorFloatToInt(constref VFloat: TXMVECTOR; constref MulExponent: UINT32): TXMVECTOR;
begin
    (* ToDo ARM
   float32x4_t vResult = vmulq_n_f32(VFloat, (float)(1U << MulExponent));
    // In case of positive overflow, detect it
    uint32x4_t vOverflow = vcgtq_f32(vResult,g_XMMaxInt);
    // Float to int conversion
    int32x4_t vResulti = vcvtq_s32_f32(vResult);
    // If there was positive overflow, set to 0x7FFFFFFF
    vResult = vandq_u32(vOverflow,g_XMAbsMask);
    vOverflow = vbicq_u32(vResulti,vOverflow);
    vOverflow = vorrq_u32(vOverflow,vResult);
    return vOverflow;
*)
end;

function XMConvertVectorUIntToFloat(constref VUInt: TXMVECTOR; constref DivExponent: UINT32): TXMVECTOR;
begin
     (* ToDo_ARM
      float fScale = 1.0f / (float)(1U << DivExponent);
    float32x4_t vResult = vcvtq_f32_u32( VUInt );
    return vmulq_n_f32( vResult, fScale );
     *)
end;

function XMConvertVectorFloatToUInt(constref VFloat: TXMVECTOR; constref MulExponent: UINT32): TXMVECTOR;
begin
    (*

    float32x4_t vResult = vmulq_n_f32(VFloat,(float)(1U << MulExponent));
    // In case of overflow, detect it
    uint32x4_t vOverflow = vcgtq_f32(vResult,g_XMMaxUInt);
    // Float to int conversion
    uint32x4_t vResulti = vcvtq_u32_f32(vResult);
    // If there was overflow, set to 0xFFFFFFFFU
    vResult = vbicq_u32(vResulti,vOverflow);
    vOverflow = vorrq_u32(vOverflow,vResult);
    return vOverflow;

    *)
end;


function XMVectorSetBinaryConstant(const C0, C1, C2, c3: UINT32): TXMVECTOR;
begin
    Result.u32[0] := (0 - (C0 and 1)) and $3F800000;
    Result.u32[1] := (0 - (C1 and 1)) and $3F800000;
    Result.u32[2] := (0 - (C2 and 1)) and $3F800000;
    Result.u32[3] := (0 - (C3 and 1)) and $3F800000;
end;


function XMVectorSplatConstant(const IntConstant: INT32;const  DivExponent: UINT32): TXMVECTOR;
begin
     (* ToDoARM
     // Splat the int
    int32x4_t vScale = vdupq_n_s32(IntConstant);
    // Convert to a float
    XMVECTOR vResult = vcvtq_f32_s32(vScale);
    // Convert DivExponent into 1.0f/(1<<DivExponent)
    uint32_t uScale = 0x3F800000U - (DivExponent << 23);
    // Splat the scalar value (It's really a float)
    vScale = vdupq_n_s32(uScale);
    // Multiply by the reciprocal (Perform a right shift by DivExponent)
    vResult = vmulq_f32(vResult,reinterpret_cast<const float32x4_t *>(&vScale)[0]);
    return vResult;
    *)
end;


function XMVectorSplatConstantInt(const IntConstant: INT32): TXMVECTOR;
begin
    {int32x4_t V = vdupq_n_s32( IntConstant );
    return reinterpret_cast<float32x4_t *>(&V)[0];   }
end;



{****************************************************************************
 *
 * Vector and matrix load operations
 *
 ****************************************************************************}


function XMLoadInt(constref pSource: PUINT32): TXMVECTOR;
begin
    (* ToDo
    uint32x4_t zero = vdupq_n_u32(0);
    return vld1q_lane_u32( pSource, zero, 0 );
*)
end;


function XMLoadFloat(constref pSource: PSingle): TXMVECTOR;
begin
    (* ToDo
    float32x4_t zero = vdupq_n_f32(0);
    return vld1q_lane_f32( pSource, zero, 0 );
*)
end;

function XMLoadInt2(const pSource: PUINT32): TXMVECTOR;
begin
    (* ToDo
    uint32x2_t x = vld1_u32( pSource );
    uint32x2_t zero = vdup_n_u32(0);
    return vcombine_u32( x, zero );
*)
end;


function XMLoadInt2A(const PSource: PUINT32): TXMVECTOR;
begin
    (* ToDo
    uint32x2_t x = vld1_u32_ex( pSource, 64 );
    uint32x2_t zero = vdup_n_u32(0);
    return vcombine_u32( x, zero );
*)
end;


function XMLoadFloat2(const pSource: TXMFLOAT2): TXMVECTOR;
begin
    (* ToDo
    float32x2_t x = vld1_f32( reinterpret_cast<const float*>(pSource) );
    float32x2_t zero = vdup_n_f32(0);
    return vcombine_f32( x, zero );
*)
end;


function XMLoadFloat2A(const pSource: TXMFLOAT2A): TXMVECTOR;
begin
    (* ToDo
   float32x2_t x = vld1_f32_ex( reinterpret_cast<const float*>(pSource), 64 );
    float32x2_t zero = vdup_n_f32(0);
    return vcombine_f32( x, zero );
*)
end;

function XMLoadSInt2(const pSource: TXMINT2): TXMVECTOR;
begin
    (* ToDo
     int32x2_t x = vld1_s32( reinterpret_cast<const int32_t*>(pSource) );
    float32x2_t v = vcvt_f32_s32( x );
    float32x2_t zero = vdup_n_f32(0);
    return vcombine_f32( v, zero );
*)
end;

function XMLoadUInt2(const pSource: TXMUINT2): TXMVECTOR;
begin
    (* ToDo
    uint32x2_t x = vld1_u32( reinterpret_cast<const uint32_t*>(pSource) );
    float32x2_t v = vcvt_f32_u32( x );
    float32x2_t zero = vdup_n_f32(0);
    return vcombine_f32( v, zero );
*)
end;



function XMLoadInt3(const pSource: PUINT32): TXMVECTOR;
begin
    (* ToDo
    uint32x2_t x = vld1_u32( pSource );
    uint32x2_t zero = vdup_n_u32(0);
    uint32x2_t y = vld1_lane_u32( pSource+2, zero, 0 );
    return vcombine_u32( x, y );
*)
end;



function XMLoadInt3A(const pSource: PUINT32): TXMVECTOR;
begin
    (* ToDo
    // Reads an extra integer which is zero'd
    uint32x4_t V = vld1q_u32_ex( pSource, 128 );
    return vsetq_lane_u32( 0, V, 3 );
*)
end;



function XMLoadFloat3(constref pSource: TXMFLOAT3): TXMVECTOR; inline;
begin
    (* ToDo
    float32x2_t x = vld1_f32( reinterpret_cast<const float*>(pSource) );
    float32x2_t zero = vdup_n_f32(0);
    float32x2_t y = vld1_lane_f32( reinterpret_cast<const float*>(pSource)+2, zero, 0 );
    return vcombine_f32( x, y );
*)
end;

function XMLoadFloat3(constref pSource: pSingle): TXMVECTOR;
begin
    Result.f32[0] := pSource[0];
    Result.f32[1] := pSource[1];
    Result.f32[2] := pSource[2];
    Result.f32[3] := 0;
end;


function XMLoadFloat3A(const pSource: TXMFLOAT3A): TXMVECTOR;
begin
    (* ToDo
    // Reads an extra float which is zero'd
    float32x4_t V = vld1q_f32_ex( reinterpret_cast<const float*>(pSource), 128 );
    return vsetq_lane_f32( 0, V, 3 );
*)
end;


function XMLoadSInt3(const pSource: TXMINT3): TXMVECTOR;
begin
    (* ToDo
    int32x2_t x = vld1_s32( reinterpret_cast<const int32_t*>(pSource) );
    int32x2_t zero = vdup_n_s32(0);
    int32x2_t y = vld1_lane_s32( reinterpret_cast<const int32_t*>(pSource)+2, zero, 0 );
    int32x4_t v = vcombine_s32( x, y );
    return vcvtq_f32_s32( v );
*)
end;



function XMLoadUInt3(const pSource: TXMUINT3): TXMVECTOR;
begin
    (* ToDo
    uint32x2_t x = vld1_u32( reinterpret_cast<const uint32_t*>(pSource) );
    uint32x2_t zero = vdup_n_u32(0);
    uint32x2_t y = vld1_lane_u32( reinterpret_cast<const uint32_t*>(pSource)+2, zero, 0 );
    uint32x4_t v = vcombine_u32( x, y );
    return vcvtq_f32_u32( v );
*)
end;



function XMLoadInt4(const pSource: PUINT32): TXMVECTOR;
begin
    (* ToDo
    return vld1q_u32( pSource );
*)
end;



function XMLoadInt4A(const pSource: PUINT32): TXMVECTOR;
begin
    (* ToDo
    return vld1q_u32_ex( pSource, 128 );
    *)
end;



function XMLoadFloat4(constref pSource: TXMFLOAT4): TXMVECTOR;
begin
    (* ToDo
   return vld1q_f32( reinterpret_cast<const float*>(pSource) );
*)
end;

function XMLoadFloat4(constref pSource: PSingle): TXMVECTOR;
begin
    Result.f32[0] := pSource[0];
    Result.f32[1] := pSource[1];
    Result.f32[2] := pSource[2];
    Result.f32[3] := pSource[3];
end;


function XMLoadFloat4A(const pSource: TXMFLOAT4A): TXMVECTOR;
begin
    (* ToDo
     return vld1q_f32_ex( reinterpret_cast<const float*>(pSource), 128 );
*)
end;


function XMLoadSInt4(const pSource: TXMINT4): TXMVECTOR;
begin
    (* ToDo
    int32x4_t v = vld1q_s32( reinterpret_cast<const int32_t*>(pSource) );
    return vcvtq_f32_s32( v );
*)
end;


function XMLoadUInt4(const pSource: TXMUINT4): TXMVECTOR;
begin
    (* ToDo
    uint32x4_t v = vld1q_u32( reinterpret_cast<const uint32_t*>(pSource) );
    return vcvtq_f32_u32( v );
*)
end;


function XMLoadFloat3x3(const pSource: TXMFLOAT3X3): TXMMATRIX;
begin
    (* ToDo
    float32x4_t v0 = vld1q_f32( &pSource.m[0,0] );
    float32x4_t v1 = vld1q_f32( &pSource.m[1,1] );
    float32x2_t v2 = vcreate_f32( (uint64_t)*(const uint32_t* )&pSource.m[2,2] );
    float32x4_t T = vextq_f32( v0, v1, 3 );

    XMMATRIX M;
    result.r[0] = vandq_u32( v0, g_XMMask3 );
    result.r[1] = vandq_u32( T, g_XMMask3 );
    result.r[2] = vcombine_f32( vget_high_f32(v1), v2 );
    result.r[3] = g_XMIdentityR3;
    return M;
*)
end;


function XMLoadFloat4x3(const pSource: TXMFLOAT4X3): TXMMATRIX;
begin
    (* ToDo
     float32x4_t v0 = vld1q_f32( &pSource.m[0,0] );
    float32x4_t v1 = vld1q_f32( &pSource.m[1,1] );
    float32x4_t v2 = vld1q_f32( &pSource.m[2,2] );

    float32x4_t T1 = vextq_f32( v0, v1, 3 );
    float32x4_t T2 = vcombine_f32( vget_high_f32(v1), vget_low_f32(v2) );
    float32x4_t T3 = vextq_f32( v2, v2, 1 );

    XMMATRIX M;
    result.r[0] = vandq_u32( v0, g_XMMask3 );
    result.r[1] = vandq_u32( T1, g_XMMask3 );
    result.r[2] = vandq_u32( T2, g_XMMask3 );
    result.r[3] = vsetq_lane_f32( 1.f, T3, 3 );
    return M;
*)
end;


function XMLoadFloat4x3A(const pSource: TXMFLOAT4X3A): TXMMATRIX;
begin
    (* ToDo
    float32x4_t v0 = vld1q_f32_ex( &pSource.m[0,0], 128 );
    float32x4_t v1 = vld1q_f32_ex( &pSource.m[1,1], 128 );
    float32x4_t v2 = vld1q_f32_ex( &pSource.m[2,2], 128 );

    float32x4_t T1 = vextq_f32( v0, v1, 3 );
    float32x4_t T2 = vcombine_f32( vget_high_f32(v1), vget_low_f32(v2) );
    float32x4_t T3 = vextq_f32( v2, v2, 1 );

    XMMATRIX M;
    result.r[0] = vandq_u32( v0, g_XMMask3 );
    result.r[1] = vandq_u32( T1, g_XMMask3 );
    result.r[2] = vandq_u32( T2, g_XMMask3 );
    result.r[3] = vsetq_lane_f32( 1.f, T3, 3 );
    return M;
*)
end;


function XMLoadFloat4x4(const pSource: TXMFLOAT4X4): TXMMATRIX;
begin
    (* ToDo
     XMMATRIX M;
    result.r[0] = vld1q_f32( reinterpret_cast<const float*>(&pSource._11) );
    result.r[1] = vld1q_f32( reinterpret_cast<const float*>(&pSource._21) );
    result.r[2] = vld1q_f32( reinterpret_cast<const float*>(&pSource._31) );
    result.r[3] = vld1q_f32( reinterpret_cast<const float*>(&pSource._41) );
    return M;
*)
end;


function XMLoadFloat4x4A(const pSource: TXMFLOAT4X4A): TXMMATRIX;
begin
    (* ToDo
      XMMATRIX M;
    result.r[0] = vld1q_f32_ex( reinterpret_cast<const float*>(&pSource._11), 128 );
    result.r[1] = vld1q_f32_ex( reinterpret_cast<const float*>(&pSource._21), 128 );
    result.r[2] = vld1q_f32_ex( reinterpret_cast<const float*>(&pSource._31), 128 );
    result.r[3] = vld1q_f32_ex( reinterpret_cast<const float*>(&pSource._41), 128 );
    return M;
*)
end;

{****************************************************************************
 *
 * Vector and matrix store operations
 *
 ****************************************************************************}



procedure XMStoreInt(var pDestination: UINT32;constref V: TXMVECTOR);
begin
    (* ToDo
     vst1q_lane_u32( pDestination, *reinterpret_cast<const uint32x4_t*>(&V), 0 );
*)
end;


procedure XMStoreFloat(var pDestination: single; V: TXMVECTOR);
begin
    (* ToDo
   vst1q_lane_f32( pDestination, V, 0 );
*)
end;


procedure XMStoreInt2(var pDestination: array of UINT32; constref V: TXMVECTOR);
begin
    (* ToDo
    uint32x2_t VL = vget_low_u32(V);
    vst1_u32( pDestination, VL );
*)
end;


procedure XMStoreInt2A(var pDestination: TUINT32A_Array2; constref V: TXMVECTOR);
begin
    (* ToDo
     uint32x2_t VL = vget_low_u32(V);
    vst1_u32_ex( pDestination, VL, 64 );
*)
end;


procedure XMStoreFloat2(var pDestination: TXMFLOAT2; constref V: TXMVECTOR);
begin
    (* ToDo
    float32x2_t VL = vget_low_f32(V);
    vst1_f32( reinterpret_cast<float*>(pDestination), VL );
*)
end;


procedure XMStoreFloat2A(var pDestination: TXMFLOAT2A; constref V: TXMVECTOR);
begin
    (* ToDo
    float32x2_t VL = vget_low_f32(V);
    vst1_f32_ex( reinterpret_cast<float*>(pDestination), VL, 64 );
*)
end;


procedure XMStoreSInt2(var pDestination: TXMINT2; constref V: TXMVECTOR);
begin
    (* ToDo
     int32x2_t v = vget_low_s32(V);
    v = vcvt_s32_f32( v );
    vst1_s32( reinterpret_cast<int32_t*>(pDestination), v );
*)
end;


procedure XMStoreUInt2(var pDestination: TXMUINT2; constref V: TXMVECTOR);
begin
    (* ToDo
    float32x2_t v = vget_low_f32(V);
    uint32x2_t iv = vcvt_u32_f32( v );
    vst1_u32( reinterpret_cast<uint32_t*>(pDestination), iv );
*)
end;


procedure XMStoreInt3(var pDestination: array of UINT32; constref V: TXMVECTOR);
begin
    (* ToDo
    uint32x2_t VL = vget_low_u32(V);
    vst1_u32( pDestination, VL );
    vst1q_lane_u32( pDestination+2, *reinterpret_cast<const uint32x4_t*>(&V), 2 );
*)
end;


procedure XMStoreInt3A(var pDestination: array of UINT32; constref V: TXMVECTOR);
begin
    (* ToDo
    uint32x2_t VL = vget_low_u32(V);
    vst1_u32_ex( pDestination, VL, 64 );
    vst1q_lane_u32( pDestination+2, *reinterpret_cast<const uint32x4_t*>(&V), 2 );
*)
end;


procedure XMStoreFloat3(var pDestination: TXMFLOAT3; constref V: TXMVECTOR); inline;
begin
    (* ToDo
   float32x2_t VL = vget_low_f32(V);
    vst1_f32( reinterpret_cast<float*>(pDestination), VL );
    vst1q_lane_f32( reinterpret_cast<float*>(pDestination)+2, V, 2 );
*)
end;


procedure XMStoreFloat3A(var pDestination: TXMFLOAT3A; constref V: TXMVECTOR);
begin
    (* ToDo
     float32x2_t VL = vget_low_f32(V);
    vst1_f32_ex( reinterpret_cast<float*>(pDestination), VL, 64 );
    vst1q_lane_f32( reinterpret_cast<float*>(pDestination)+2, V, 2 );
*)
end;


procedure XMStoreSInt3(var pDestination: TXMINT3; constref V: TXMVECTOR);
begin
    (* ToDo
    int32x4_t v = vcvtq_s32_f32(V);
    int32x2_t vL = vget_low_s32(v);
    vst1_s32( reinterpret_cast<int32_t*>(pDestination), vL );
    vst1q_lane_s32( reinterpret_cast<int32_t*>(pDestination)+2, v, 2 );
*)
end;


procedure XMStoreUInt3(var pDestination: TXMUINT3; constref V: TXMVECTOR);
begin
    (* ToDo
    uint32x4_t v = vcvtq_u32_f32(V);
    uint32x2_t vL = vget_low_u32(v);
    vst1_u32( reinterpret_cast<uint32_t*>(pDestination), vL );
    vst1q_lane_u32( reinterpret_cast<uint32_t*>(pDestination)+2, v, 2 );
*)
end;


procedure XMStoreInt4(var pDestination: array of UINT32; constref V: TXMVECTOR);
begin
    (* ToDo
    vst1q_u32( pDestination, V );
*)
end;


procedure XMStoreInt4A(var pDestination: array of UINT32; constref V: TXMVECTOR);
begin
    (* ToDo
    vst1q_u32_ex( pDestination, V, 128 );
*)
end;


procedure XMStoreFloat4(var pDestination: TXMFLOAT4; constref V: TXMVECTOR);
begin
    (* ToDo
    vst1q_f32( reinterpret_cast<float*>(pDestination), V );
*)
end;


procedure XMStoreFloat4A(var pDestination: TXMFLOAT4A; constref V: TXMVECTOR);
begin
    (* ToDo
   vst1q_f32_ex( reinterpret_cast<float*>(pDestination), V, 128 );
*)
end;

procedure XMStoreSInt4(var pDestination: TXMINT4; constref V: TXMVECTOR);
begin
    (* ToDo
    int32x4_t v = vcvtq_s32_f32(V);
    vst1q_s32( reinterpret_cast<int32_t*>(pDestination), v );
*)
end;


procedure XMStoreUInt4(var pDestination: TXMUINT4; constref V: TXMVECTOR);
begin
    (* ToDo
    uint32x4_t v = vcvtq_u32_f32(V);
    vst1q_u32( reinterpret_cast<uint32_t*>(pDestination), v );
*)
end;


procedure XMStoreFloat3x3(var pDestination: TXMFLOAT3X3; constref M: TXMMATRIX);
begin
    (* ToDo
     float32x4_t T1 = vextq_f32( M.r[0], M.r[1], 1 );
    float32x4_t T2 = vbslq_f32( g_XMMask3, M.r[0], T1 );
    vst1q_f32( &pDestination.m[0,0], T2 );

    T1 = vextq_f32( M.r[1], M.r[1], 1 );
    T2 = vcombine_f32( vget_low_f32(T1), vget_low_f32(M.r[2]) );
    vst1q_f32( &pDestination.m[1,1], T2 );

    vst1q_lane_f32( &pDestination.m[2,2], M.r[2], 2 );
*)
end;


procedure XMStoreFloat4x3(var pDestination: TXMFLOAT4X3; constref M: TXMMATRIX);
begin
    (* ToDo
    float32x4_t T1 = vextq_f32( M.r[0], M.r[1], 1 );
    float32x4_t T2 = vbslq_f32( g_XMMask3, M.r[0], T1 );
    vst1q_f32( &pDestination.m[0,0], T2 );

    T1 = vextq_f32( M.r[1], M.r[1], 1 );
    T2 = vcombine_f32( vget_low_f32(T1), vget_low_f32(M.r[2]) );
    vst1q_f32( &pDestination.m[1,1], T2 );

    T1 = vdupq_lane_f32( vget_high_f32( M.r[2] ), 0 );
    T2 = vextq_f32( T1, M.r[3], 3 );
    vst1q_f32( &pDestination.m[2,2], T2 );
*)
end;


procedure XMStoreFloat4x3A(var pDestination: TXMFLOAT4X3A; constref M: TXMMATRIX);
begin
    (* ToDo
    float32x4_t T1 = vextq_f32( M.r[0], M.r[1], 1 );
    float32x4_t T2 = vbslq_f32( g_XMMask3, M.r[0], T1 );
    vst1q_f32_ex( &pDestination.m[0,0], T2, 128 );

    T1 = vextq_f32( M.r[1], M.r[1], 1 );
    T2 = vcombine_f32( vget_low_f32(T1), vget_low_f32(M.r[2]) );
    vst1q_f32_ex( &pDestination.m[1,1], T2, 128 );

    T1 = vdupq_lane_f32( vget_high_f32( M.r[2] ), 0 );
    T2 = vextq_f32( T1, M.r[3], 3 );
    vst1q_f32_ex( &pDestination.m[2,2], T2, 128 );
*)
end;



procedure XMStoreFloat4x4(var pDestination: TXMFLOAT4X4; constref M: TXMMATRIX);
begin
    (* ToDo
     vst1q_f32( reinterpret_cast<float*>(&pDestination._11), M.r[0] );
    vst1q_f32( reinterpret_cast<float*>(&pDestination._21), M.r[1] );
    vst1q_f32( reinterpret_cast<float*>(&pDestination._31), M.r[2] );
    vst1q_f32( reinterpret_cast<float*>(&pDestination._41), M.r[3] );
*)
end;



procedure XMStoreFloat4x4A(var pDestination: TXMFLOAT4X4A; constref M: TXMMATRIX);
begin
    (* ToDo
    vst1q_f32_ex( reinterpret_cast<float*>(&pDestination._11), M.r[0], 128 );
    vst1q_f32_ex( reinterpret_cast<float*>(&pDestination._21), M.r[1], 128 );
    vst1q_f32_ex( reinterpret_cast<float*>(&pDestination._31), M.r[2], 128 );
    vst1q_f32_ex( reinterpret_cast<float*>(&pDestination._41), M.r[3], 128 );
*)
end;

function XMVectorZero: TXMVECTOR;
begin
    (* ToDo
     return vdupq_n_f32(0);
*)
end;

function XMVectorSwizzle(constref V: TXMVECTOR; constref SwizzleX, SwizzleY, SwizzleZ, SwizzleW: UINT32): TXMVECTOR;
begin
    (*
     static const uint32_t ControlElement[ 4 ] =
    {
        0x03020100, // XM_SWIZZLE_X
        0x07060504, // XM_SWIZZLE_Y
        0x0B0A0908, // XM_SWIZZLE_Z
        0x0F0E0D0C, // XM_SWIZZLE_W
    };

    int8x8x2_t tbl;
    tbl.val[0] = vget_low_f32(V);
    tbl.val[1] = vget_high_f32(V);

    uint32x2_t idx = vcreate_u32( ((uint64_t)ControlElement[E0]) | (((uint64_t)ControlElement[E1]) << 32) );
    const uint8x8_t rL = vtbl2_u8( tbl, idx );

    idx = vcreate_u32( ((uint64_t)ControlElement[E2]) | (((uint64_t)ControlElement[E3]) << 32) );
    const uint8x8_t rH = vtbl2_u8( tbl, idx );

    return vcombine_f32( rL, rH );
    *)
end;

// Initialize a vector with four floating point values
function XMVectorSet(constref x, y, z, w: single): TXMVECTOR;
begin
    (*
float32x2_t V0 = vcreate_f32(((uint64_t) * (const uint32_t * )&x) | ((uint64_t)( *(const uint32_t * )&y) << 32));
    float32x2_t V1 = vcreate_f32(((uint64_t) * (const uint32_t * )&z) | ((uint64_t)( *(const uint32_t * )&w) << 32));
    return vcombine_f32(V0, V1);
    *)
end;

// Initialize a vector with four integer values
function XMVectorSetInt(const x, y, z, w: UINT32): TXMVECTOR;
begin
    (*
     uint32x2_t V0 = vcreate_u32(((uint64_t)x) | ((uint64_t)y << 32));
    uint32x2_t V1 = vcreate_u32(((uint64_t)z) | ((uint64_t)w << 32));
    return vcombine_u32(V0, V1);
    *)
end;

// Initialize a vector with a replicated floating point value passed by pointer
function XMVectorReplicate(constref Value: single): TXMVECTOR;
begin
    (* ToDo
     return vld1q_dup_f32( pValue );
*)
end;


// Initialize a vector with a replicated floating point value passed by pointer
function XMVectorReplicatePtr(pValue: PSingle): TXMVECTOR;
begin
    (*
      return vld1q_dup_f32( pValue );
    *)
end;


// Initialize a vector with a replicated integer value
function XMVectorReplicateInt(constref Value: UINT32): TXMVECTOR;
begin
    (*
     return vdupq_n_u32( Value );
    *)
end;


// Initialize a vector with a replicated integer value passed by pointer
function XMVectorReplicateIntPtr(pValue: PUINT32): TXMVECTOR;
begin
    (*
     return vld1q_dup_u32(pValue);
    *)
end;

// Initialize a vector with all bits set (true mask)
function XMVectorTrueInt: TXMVECTOR;
begin
    (*
    return vdupq_n_s32(-1);
    *)
end;

// Initialize a vector with all bits clear (false mask)
function XMVectorFalseInt: TXMVECTOR;
begin
    (* ToDo
    return vdupq_n_u32(0);
*)
end;


// Returns a vector, all of whose components are equal to the x component of V.
function XMVectorSplatX(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vdupq_lane_f32( vget_low_f32( V ), 0 );
*)
end;


// Returns a vector, all of whose components are equal to the y component of V.
function XMVectorSplatY(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
  return vdupq_lane_f32( vget_low_f32( V ), 1 );
*)
end;


// Replicate the z component of the vector
// Returns a vector, all of whose components are equal to the z component of V.
function XMVectorSplatZ(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
   return vdupq_lane_f32( vget_high_f32( V ), 0 );
*)
end;


// Returns a vector, all of whose components are equal to the w component of V.
function XMVectorSplatW(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vdupq_lane_f32( vget_high_f32( V ), 1 );
*)
end;


// Return a vector of 1.0f,1.0f,1.0f,1.0f
function XMVectorSplatOne: TXMVECTOR; inline;
begin
    (* ToDo
     return vdupq_n_f32(1.0f);
*)
end;


// Return a vector of INF,INF,INF,INF
function XMVectorSplatInfinity: TXMVECTOR;
begin
    (* ToDo
    return vdupq_n_u32(0x7F800000);
*)
end;


function XMVectorSplatQNaN: TXMVECTOR;
begin
    (* ToDo
    return vdupq_n_u32(0x7FC00000);
*)
end;


function XMVectorSplatEpsilon: TXMVECTOR;
begin
    (* ToDo
    return vdupq_n_u32(0x34000000);
*)
end;

//------------------------------------------------------------------------------
// Return a vector of -0.0f (0x80000000),-0.0f,-0.0f,-0.0f
function XMVectorSplatSignMask: TXMVECTOR;
begin
    (* ToDo
    return vdupq_n_u32(0x80000000U);
*)
end;

function XMVectorGetX(constref V: TXMVECTOR): single;
begin
    (* ToDo
      return vgetq_lane_f32(V, 0);
*)
end;


function XMVectorGetY(constref V: TXMVECTOR): single;
begin
    (*  return vgetq_lane_f32(V, 1); *)
end;


function XMVectorGetZ(constref V: TXMVECTOR): single;
begin
    (*  return vgetq_lane_f32(V, 2); *)
end;


function XMVectorGetW(constref V: TXMVECTOR): single;
begin
    (*  return vgetq_lane_f32(V, 3); *)
end;

// Store the X component into a 32 bit  single  location in memory.
procedure XMVectorGetXPtr(out x: single; constref V: TXMVECTOR);
begin
    (*  return vgetq_lane_f32(V, 0); *)
end;

// Store the Y component into a 32 bit  single  location in memory.
procedure XMVectorGetYPtr(out y: single; constref V: TXMVECTOR);
begin
    (*  return vgetq_lane_f32(V, 1); *)
end;

// Store the Z component into a 32 bit  single  location in memory.
procedure XMVectorGetZPtr(out z: single; constref V: TXMVECTOR);
begin
    (* ToDo
     vst1q_lane_f32(z,V,2);
*)
end;

// Store the W component into a 32 bit  single  location in memory.
procedure XMVectorGetWPtr(out w: single; constref V: TXMVECTOR);
begin
    (* ToDo
    vst1q_lane_f32(w,V,3);
*)
end;

// Return the X component in an integer register.
function XMVectorGetIntX(constref V: TXMVECTOR): UINT32;
begin
    (* ToDo_ARM
     return vgetq_lane_u32(V, 0);
     *)
end;

// Return the Y component in an integer register.
function XMVectorGetIntY(constref V: TXMVECTOR): UINT32;
begin
    (* ToDo_ARM
    return vgetq_lane_u32(V, 1);
*)
end;

// Return the Z component in an integer register.
function XMVectorGetIntZ(constref V: TXMVECTOR): UINT32;
begin
    (* ToDo
    return vgetq_lane_u32(V, 2);
*)
end;

// Return the W component in an integer register.
function XMVectorGetIntW(constref V: TXMVECTOR): UINT32;
begin
    (* ToDo
    return vgetq_lane_u32(V, 3);
*)
end;

// Store the X component into a 32 bit integer location in memory.
procedure XMVectorGetIntXPtr(out x: UINT32; constref V: TXMVECTOR);
begin
    (* ToDo
     vst1q_lane_u32(x,*reinterpret_cast<const uint32x4_t*>(&V),0);
*)
end;

// Store the Y component into a 32 bit integer location in memory.
procedure XMVectorGetIntYPtr(out y: UINT32; constref V: TXMVECTOR);
begin
    (* ToDo
     vst1q_lane_u32(y,*reinterpret_cast<const uint32x4_t*>(&V),1);
*)
end;


// Store the Z component into a 32 bit integer location in memory.
procedure XMVectorGetIntZPtr(out z: UINT32; constref V: TXMVECTOR);
begin
    (* ToDo
     vst1q_lane_u32(z,*reinterpret_cast<const uint32x4_t*>(&V),2);
*)
end;

// Store the W component into a 32 bit integer location in memory.
procedure XMVectorGetIntWPtr(out w: UINT32; constref V: TXMVECTOR);
begin
    (* ToDo
   vst1q_lane_u32(w,*reinterpret_cast<const uint32x4_t*>(&V),3);
*)
end;

// Sets the X component of a vector to a passed floating point value
function XMVectorSetX(constref V: TXMVECTOR; constref x: single): TXMVECTOR;
begin
    (* ToDo
     return vsetq_lane_f32(x,V,0);
*)
end;

// Sets the Y component of a vector to a passed floating point value
function XMVectorSetY(constref V: TXMVECTOR; constref y: single): TXMVECTOR;
begin
    (* ToDo
    return vsetq_lane_f32(y,V,1);
*)
end;


// Sets the Z component of a vector to a passed floating point value
function XMVectorSetZ(constref V: TXMVECTOR; constref z: single): TXMVECTOR;
begin
    (* ToDo
     return vsetq_lane_f32(z,V,2);
*)
end;


// Sets the W component of a vector to a passed floating point value
function XMVectorSetW(constref V: TXMVECTOR; constref w: single): TXMVECTOR;
begin
    (* ToDo
     return vsetq_lane_f32(w,V,3);
*)
end;


// Sets the X component of a vector to a floating point value passed by p
function XMVectorSetXPtr(constref V: TXMVECTOR; constref x: Psingle): TXMVECTOR;
begin
    (* ToDo
    return vld1q_lane_f32(x,V,0);
*)
end;


// Sets the Y component of a vector to a floating point value passed by pointer
function XMVectorSetYPtr(constref V: TXMVECTOR; constref y: Psingle): TXMVECTOR;
begin
    (* ToDo
     return vld1q_lane_f32(y,V,1);
*)
end;


// Sets the Z component of a vector to a floating point value passed by pointer
function XMVectorSetZPtr(constref V: TXMVECTOR; constref z: Psingle): TXMVECTOR;
begin
    (* ToDo
     return vld1q_lane_f32(z,V,2);
*)
end;


// Sets the W component of a vector to a floating point value passed by pointer
function XMVectorSetWPtr(constref V: TXMVECTOR; constref w: Psingle): TXMVECTOR;
begin
    (* ToDo
    return vld1q_lane_f32(w,V,3);
*)
end;


// Sets the X component of a vector to an integer passed by value
function XMVectorSetIntX(constref V: TXMVECTOR; const x: UINT32): TXMVECTOR;
begin
    (* ToDo
    return vsetq_lane_u32(x,V,0);
*)
end;


// Sets the Y component of a vector to an integer passed by value
function XMVectorSetIntY(constref V: TXMVECTOR; const y: UINT32): TXMVECTOR;
begin
    (* ToDo
    return vsetq_lane_u32(y,V,1);
*)
end;

// Sets the Z component of a vector to an integer passed by value
function XMVectorSetIntZ(constref V: TXMVECTOR; const z: UINT32): TXMVECTOR;
begin
    (* ToDo
    return vsetq_lane_u32(z,V,2);
*)
end;


// Sets the W component of a vector to an integer passed by value
function XMVectorSetIntW(constref V: TXMVECTOR; const w: UINT32): TXMVECTOR;
begin
    (* ToDo
    return vsetq_lane_u32(w,V,3);
*)
end;

// Sets the X component of a vector to an integer value passed by pointer
function XMVectorSetIntXPtr(constref V: TXMVECTOR; constref x: PUINT32): TXMVECTOR;
begin
    (* ToDo
    return vld1q_lane_u32(x,*reinterpret_cast<const uint32x4_t *>(&V),0);
*)
end;


// Sets the Y component of a vector to an integer value passed by pointer
function XMVectorSetIntYPtr(constref V: TXMVECTOR; constref y: PUINT32): TXMVECTOR;
begin
    (* ToDo
    return vld1q_lane_u32(y,*reinterpret_cast<const uint32x4_t *>(&V),1);
*)
end;


// Sets the Z component of a vector to an integer value passed by pointer
function XMVectorSetIntZPtr(constref V: TXMVECTOR; constref z: PUINT32): TXMVECTOR;
begin
    (* ToDo
     return vld1q_lane_u32(z,*reinterpret_cast<const uint32x4_t *>(&V),2);
*)
end;


// Sets the W component of a vector to an integer value passed by pointer
function XMVectorSetIntWPtr(constref V: TXMVECTOR; constref w: PUINT32): TXMVECTOR;
begin
    (* ToDo
    return vld1q_lane_u32(w,*reinterpret_cast<const uint32x4_t *>(&V),3);
*)
end;


function XMVectorPermute(V1, V2: TXMVECTOR; PermuteX, PermuteY, PermuteZ, PermuteW: uint32): TXMVECTOR; inline;
begin
    (*
     static const uint32_t ControlElement[ 8 ] =
    {
        0x03020100, // XM_PERMUTE_0X
        0x07060504, // XM_PERMUTE_0Y
        0x0B0A0908, // XM_PERMUTE_0Z
        0x0F0E0D0C, // XM_PERMUTE_0W
        0x13121110, // XM_PERMUTE_1X
        0x17161514, // XM_PERMUTE_1Y
        0x1B1A1918, // XM_PERMUTE_1Z
        0x1F1E1D1C, // XM_PERMUTE_1W
    };

    int8x8x4_t tbl;
    tbl.val[0] = vget_low_f32(V1);
    tbl.val[1] = vget_high_f32(V1);
    tbl.val[2] = vget_low_f32(V2);
    tbl.val[3] = vget_high_f32(V2);

    uint32x2_t idx = vcreate_u32( ((uint64_t)ControlElement[PermuteX]) | (((uint64_t)ControlElement[PermuteY]) << 32) );
    const uint8x8_t rL = vtbl4_u8( tbl, idx );

    idx = vcreate_u32( ((uint64_t)ControlElement[PermuteZ]) | (((uint64_t)ControlElement[PermuteW]) << 32) );
    const uint8x8_t rH = vtbl4_u8( tbl, idx );

    return vcombine_f32( rL, rH );
    *)
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
    constref VectorIndex3: UINT32): TXMVECTOR;
begin
    (* ToDo
     int32x2_t V0 = vcreate_s32(((uint64_t)VectorIndex0) | ((uint64_t)VectorIndex1 << 32));
    int32x2_t V1 = vcreate_s32(((uint64_t)VectorIndex2) | ((uint64_t)VectorIndex3 << 32));
    int32x4_t vTemp = vcombine_s32(V0, V1);
    // Any non-zero entries become 0xFFFFFFFF else 0
    return vcgtq_s32(vTemp,g_XMZero);
*)
end;

function XMVectorSelect(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Control: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vbslq_f32( Control, V2, V1 );
*)
end;


function XMVectorMergeXY(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     return vzipq_f32( V1, V2 ).val[0];
*)
end;


function XMVectorMergeZW(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     return vzipq_f32( V1, V2 ).val[1];
*)
end;

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------

function XMVectorEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     return vceqq_f32( V1, V2 );
*)
end;

function XMVectorEqualR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (*
     uint32x4_t vResult = vceqq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);
    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFU )
    {
        // All elements are equal
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        // All elements are not equal
        CR = XM_CRMASK_CR6FALSE;
    }
    *pCR = CR;
    return vResult;
    *)
end;


// Treat the components of the vectors as unsigned integers and
// compare individual bits between the two.  This is useful for
// comparing control vectors and result vectors returned from
// other comparison operations.

function XMVectorEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vceqq_u32( V1, V2 );
*)
end;

function XMVectorEqualIntR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    uint32x4_t vResult = vceqq_u32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);
    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFU )
    {
        // All elements are equal
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        // All elements are not equal
        CR = XM_CRMASK_CR6FALSE;
    }
    *pCR = CR;
    return vResult;
*)
end;


function XMVectorNearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    XMVECTOR vDelta = vsubq_f32(V1,V2);
    return vacleq_f32( vDelta, Epsilon );
*)
end;

function XMVectorNotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vmvnq_u32(vceqq_f32(V1, V2));
*)
end;

function XMVectorNotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
   return vmvnq_u32(vceqq_u32(V1, V2));
*)
end;

function XMVectorGreater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
   return vcgtq_f32( V1, V2 );
*)
end;


function XMVectorGreaterR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    uint32x4_t vResult = vcgtq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);
    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFU )
    {
        // All elements are greater
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        // All elements are not greater
        CR = XM_CRMASK_CR6FALSE;
    }
    *pCR = CR;
    return vResult;
*)
end;


function XMVectorGreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     return vcgeq_f32( V1, V2 );
*)
end;

function XMVectorGreaterOrEqualR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    uint32x4_t vResult = vcgeq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);
    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFU )
    {
        // All elements are greater or equal
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        // All elements are not greater or equal
        CR = XM_CRMASK_CR6FALSE;
    }
    *pCR = CR;
    return vResult;
*)
end;

function XMVectorLess(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     return vcltq_f32( V1, V2 );
*)
end;

function XMVectorLessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vcleq_f32( V1, V2 );
*)
end;

function XMVectorInBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     // Test if less than or equal
    XMVECTOR vTemp1 = vcleq_f32(V,Bounds);
    // Negate the bounds
    XMVECTOR vTemp2 = vnegq_f32(Bounds);
    // Test if greater or equal (Reversed)
    vTemp2 = vcleq_f32(vTemp2,V);
    // Blend answers
    vTemp1 = vandq_u32(vTemp1,vTemp2);
    return vTemp1;
*)
end;


function XMVectorInBoundsR(out pCR: UINT32; constref V: TXMVECTOR; constref Bounds: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     // Test if less than or equal
    XMVECTOR vTemp1 = vcleq_f32(V,Bounds);
    // Negate the bounds
    XMVECTOR vTemp2 = vnegq_f32(Bounds);
    // Test if greater or equal (Reversed)
    vTemp2 = vcleq_f32(vTemp2,V);
    // Blend answers
    vTemp1 = vandq_u32(vTemp1,vTemp2);
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vTemp1), vget_high_u8(vTemp1));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);
    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFU )
    {
        // All elements are in bounds
        CR = XM_CRMASK_CR6BOUNDS;
    }
    *pCR = CR;
    return vTemp1;
*)
end;

function XMVectorIsNaN(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Test against itself. NaN is always not equal
    uint32x4_t vTempNan = vceqq_f32( V, V );
    // Flip results
    return vmvnq_u32( vTempNan );
*)
end;

function XMVectorIsInfinite(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Mask off the sign bit
    uint32x4_t vTemp = vandq_u32(V,g_XMAbsMask);
    // Compare to infinity
    vTemp = vceqq_f32(vTemp,g_XMInfinity);
    // If any are infinity, the signs are true.
    return vTemp;
*)
end;


//------------------------------------------------------------------------------
// Rounding and clamping operations
//------------------------------------------------------------------------------

function XMVectorMin(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vminq_f32( V1, V2 );
*)
end;

function XMVectorMax(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
     (* ToDo
     return vmaxq_f32( V1, V2 );
 *)
end;


function XMVectorRound(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     #if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
    return vrndnq_f32(V);
#else
    uint32x4_t sign = vandq_u32( V, g_XMNegativeZero );
    uint32x4_t sMagic = vorrq_u32( g_XMNoFraction, sign );
    float32x4_t R1 = vaddq_f32( V, sMagic );
    R1 = vsubq_f32( R1, sMagic );
    float32x4_t R2 = vabsq_f32( V );
    uint32x4_t mask = vcleq_f32( R2, g_XMNoFraction );
    XMVECTOR vResult = vbslq_f32( mask, R1, V );
    return vResult;
#endif
*)
end;

 function XMVectorTruncate(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    #if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
    return vrndq_f32(V);
#else
    float32x4_t vTest = vabsq_f32( V );
    vTest = vcltq_f32( vTest, g_XMNoFraction );

    int32x4_t vInt = vcvtq_s32_f32( V );
    XMVECTOR vResult = vcvtq_f32_s32( vInt );

    // All numbers less than 8388608 will use the round to int
    // All others, use the ORIGINAL value
    return vbslq_f32( vTest, vResult, V );
#endif
*)
end;

function XMVectorFloor(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    #if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
    return vrndmq_f32(V);
#else
    float32x4_t vTest = vabsq_f32( V );
    vTest = vcltq_f32( vTest, g_XMNoFraction );
    // Truncate
    int32x4_t vInt = vcvtq_s32_f32( V );
    XMVECTOR vResult = vcvtq_f32_s32( vInt );
    XMVECTOR vLarger = vcgtq_f32( vResult, V );
    // 0 -> 0, 0xffffffff -> -1.0f
    vLarger = vcvtq_f32_s32( vLarger );
    vResult = vaddq_f32( vResult, vLarger );
    // All numbers less than 8388608 will use the round to int
    // All others, use the ORIGINAL value
    return vbslq_f32( vTest, vResult, V );
#endif
*)
end;


function XMVectorCeiling(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    #if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
    return vrndpq_f32(V);
#else
    float32x4_t vTest = vabsq_f32( V );
    vTest = vcltq_f32( vTest, g_XMNoFraction );
    // Truncate
    int32x4_t vInt = vcvtq_s32_f32( V );
    XMVECTOR vResult = vcvtq_f32_s32( vInt );
    XMVECTOR vSmaller = vcltq_f32( vResult, V );
    // 0 -> 0, 0xffffffff -> -1.0f
    vSmaller = vcvtq_f32_s32( vSmaller );
    vResult = vsubq_f32( vResult, vSmaller );
    // All numbers less than 8388608 will use the round to int
    // All others, use the ORIGINAL value
    return vbslq_f32( vTest, vResult, V );
#endif
*)
end;


 function XMVectorClamp(constref V: TXMVECTOR; constref Min: TXMVECTOR; constref Max: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    XMVECTOR vResult;
    vResult = vmaxq_f32(Min,V);
    vResult = vminq_f32(vResult,Max);
    return vResult;
*)
end;

function XMVectorSaturate(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Set <0 to 0
    XMVECTOR vResult = vmaxq_f32(V, vdupq_n_f32(0) );
    // Set>1 to 1
    return vminq_f32(vResult, vdupq_n_f32(1.0f) );s
*)
end;

function XMVectorAndInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vandq_u32(V1,V2);
*)
end;


function XMVectorAndCInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
   return vbicq_u32(V1,V2);
*)
end;


function XMVectorOrInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     return vorrq_u32(V1,V2);
*)
end;

function XMVectorNorInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    uint32x4_t Result = vorrq_u32(V1,V2);
    return vbicq_u32(g_XMNegOneMask, Result);
*)
end;

 function XMVectorXorInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     return veorq_u32(V1,V2);
     *)
end;


function XMVectorAdd(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     return vaddq_f32( V1, V2 );
*)
end;


function XMVectorSum(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    #if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
    XMVECTOR vTemp = vpaddq_f32(V, V);
    return vpaddq_f32(vTemp,vTemp);
#else
    float32x2_t v1 = vget_low_f32(V);
    float32x2_t v2 = vget_high_f32(V);
    v1 = vadd_f32(v1, v2);
    v1 = vpadd_f32(v1, v1);
    return vcombine_f32(v1, v1);
#endif
*)
end;



function XMVectorAddAngles(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     // Adjust the angles
    XMVECTOR vResult = vaddq_f32(V1,V2);
    // Less than Pi?
    uint32x4_t vOffset = vcltq_f32(vResult,g_XMNegativePi);
    vOffset = vandq_u32(vOffset,g_XMTwoPi);
    // Add 2Pi to all entries less than -Pi
    vResult = vaddq_f32(vResult,vOffset);
    // Greater than or equal to Pi?
    vOffset = vcgeq_f32(vResult,g_XMPi);
    vOffset = vandq_u32(vOffset,g_XMTwoPi);
    // Sub 2Pi to all entries greater than Pi
    vResult = vsubq_f32(vResult,vOffset);
    return vResult;
*)
end;

function XMVectorSubtract(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     return vsubq_f32( V1, V2 );
*)
end;


function XMVectorSubtractAngles(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     // Adjust the angles
    XMVECTOR vResult = vsubq_f32(V1,V2);
    // Less than Pi?
    uint32x4_t vOffset = vcltq_f32(vResult,g_XMNegativePi);
    vOffset = vandq_u32(vOffset,g_XMTwoPi);
    // Add 2Pi to all entries less than -Pi
    vResult = vaddq_f32(vResult,vOffset);
    // Greater than or equal to Pi?
    vOffset = vcgeq_f32(vResult,g_XMPi);
    vOffset = vandq_u32(vOffset,g_XMTwoPi);
    // Sub 2Pi to all entries greater than Pi
    vResult = vsubq_f32(vResult,vOffset);
    return vResult;
*)
end;

function XMVectorMultiply(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vmulq_f32( V1, V2 );
*)
end;

function XMVectorMultiplyAdd(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vmlaq_f32( V3, V1, V2 );
*)
end;


function XMVectorDivide(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    #if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
    return vdivq_f32( V1, V2 );
#else
    // 2 iterations of Newton-Raphson refinement of reciprocal
    float32x4_t Reciprocal = vrecpeq_f32(V2);
    float32x4_t S = vrecpsq_f32( Reciprocal, V2 );
    Reciprocal = vmulq_f32( S, Reciprocal );
    S = vrecpsq_f32( Reciprocal, V2 );
    Reciprocal = vmulq_f32( S, Reciprocal );
    return vmulq_f32( V1, Reciprocal );
#endif
*)
end;


function XMVectorNegativeMultiplySubtract(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vmlsq_f32( V3, V1, V2 );
*)
end;


function XMVectorScale(constref V: TXMVECTOR; constref ScaleFactor: single): TXMVECTOR;
begin
    (* ToDo
    return vmulq_n_f32( V, ScaleFactor );
*)
end;


function XMVectorReciprocalEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     return vrecpeq_f32(V);
*)
end;



function XMVectorReciprocal(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    #if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
    float32x4_t one = vdupq_n_f32(1.0f);
    return vdivq_f32(one,V);
#else
    // 2 iterations of Newton-Raphson refinement
    float32x4_t Reciprocal = vrecpeq_f32(V);
    float32x4_t S = vrecpsq_f32( Reciprocal, V );
    Reciprocal = vmulq_f32( S, Reciprocal );
    S = vrecpsq_f32( Reciprocal, V );
    return vmulq_f32( S, Reciprocal );
#endif
*)
end;


function XMVectorSqrtEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // 1 iteration of Newton-Raphson refinment of sqrt
    float32x4_t S0 = vrsqrteq_f32(V);
    float32x4_t P0 = vmulq_f32( V, S0 );
    float32x4_t R0 = vrsqrtsq_f32( P0, S0 );
    float32x4_t S1 = vmulq_f32( S0, R0 );

    XMVECTOR VEqualsInfinity = XMVectorEqualInt(V, g_XMInfinity.v);
    XMVECTOR VEqualsZero = XMVectorEqual(V, vdupq_n_f32(0) );
    XMVECTOR Result = vmulq_f32( V, S1 );
    XMVECTOR Select = XMVectorEqualInt(VEqualsInfinity, VEqualsZero);
    return XMVectorSelect(V, Result, Select);
*)
end;


function XMVectorSqrt(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     // 3 iterations of Newton-Raphson refinment of sqrt
    float32x4_t S0 = vrsqrteq_f32(V);
    float32x4_t P0 = vmulq_f32( V, S0 );
    float32x4_t R0 = vrsqrtsq_f32( P0, S0 );
    float32x4_t S1 = vmulq_f32( S0, R0 );
    float32x4_t P1 = vmulq_f32( V, S1 );
    float32x4_t R1 = vrsqrtsq_f32( P1, S1 );
    float32x4_t S2 = vmulq_f32( S1, R1 );
    float32x4_t P2 = vmulq_f32( V, S2 );
    float32x4_t R2 = vrsqrtsq_f32( P2, S2 );
    float32x4_t S3 = vmulq_f32( S2, R2 );

    XMVECTOR VEqualsInfinity = XMVectorEqualInt(V, g_XMInfinity.v);
    XMVECTOR VEqualsZero = XMVectorEqual(V, vdupq_n_f32(0) );
    XMVECTOR Result = vmulq_f32( V, S3 );
    XMVECTOR Select = XMVectorEqualInt(VEqualsInfinity, VEqualsZero);
    return XMVectorSelect(V, Result, Select);
*)
end;


function XMVectorReciprocalSqrtEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    return vrsqrteq_f32(V);
*)
end;

function XMVectorReciprocalSqrt(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // 2 iterations of Newton-Raphson refinement of reciprocal
    float32x4_t S0 = vrsqrteq_f32(V);

    float32x4_t P0 = vmulq_f32( V, S0 );
    float32x4_t R0 = vrsqrtsq_f32( P0, S0 );

    float32x4_t S1 = vmulq_f32( S0, R0 );
    float32x4_t P1 = vmulq_f32( V, S1 );
    float32x4_t R1 = vrsqrtsq_f32( P1, S1 );

    return vmulq_f32( S1, R1 );
*)
end;

function XMVectorExp2(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    int32x4_t itrunc = vcvtq_s32_f32(V);
    float32x4_t ftrunc = vcvtq_f32_s32(itrunc);
    float32x4_t y = vsubq_f32(V, ftrunc);

    float32x4_t poly = vmlaq_f32( g_XMExpEst6, g_XMExpEst7, y );
    poly = vmlaq_f32( g_XMExpEst5, poly, y );
    poly = vmlaq_f32( g_XMExpEst4, poly, y );
    poly = vmlaq_f32( g_XMExpEst3, poly, y );
    poly = vmlaq_f32( g_XMExpEst2, poly, y );
    poly = vmlaq_f32( g_XMExpEst1, poly, y );
    poly = vmlaq_f32( g_XMOne, poly, y );

    int32x4_t biased = vaddq_s32(itrunc, g_XMExponentBias);
    biased = vshlq_n_s32(biased, 23);
    float32x4_t result0 = XMVectorDivide(biased, poly);

    biased = vaddq_s32(itrunc, g_XM253);
    biased = vshlq_n_s32(biased, 23);
    float32x4_t result1 = XMVectorDivide(biased, poly);
    result1 = vmulq_f32(g_XMMinNormal.v, result1);

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

    int32x4_t comp = vcltq_s32( V, g_XMBin128);
    float32x4_t result2 = vbslq_f32( comp, result0, g_XMInfinity );

    comp = vcltq_s32(itrunc, g_XMSubnormalExponent);
    float32x4_t result3 = vbslq_f32( comp, result1, result0 );

    comp = vcltq_s32(V, g_XMBinNeg150);
    float32x4_t result4 = vbslq_f32( comp, result3, g_XMZero );

    int32x4_t sign = vandq_s32(V, g_XMNegativeZero);
    comp = vceqq_s32(sign, g_XMNegativeZero);
    float32x4_t result5 = vbslq_f32( comp, result4, result2 );

    int32x4_t t0 = vandq_s32(V, g_XMQNaNTest);
    int32x4_t t1 = vandq_s32(V, g_XMInfinity);
    t0 = vceqq_s32(t0, g_XMZero);
    t1 = vceqq_s32(t1, g_XMInfinity);
    int32x4_t isNaN = vbicq_s32( t1,t0);

    float32x4_t vResult = vbslq_f32( isNaN, g_XMQNaN, result5 );
    return vResult;
*)
end;

function XMVectorExpE(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
   // expE(V) = exp2(vin*log2(e))
    float32x4_t Ve = vmulq_f32(g_XMLgE, V);

    int32x4_t itrunc = vcvtq_s32_f32(Ve);
    float32x4_t ftrunc = vcvtq_f32_s32(itrunc);
    float32x4_t y = vsubq_f32(Ve, ftrunc);


    float32x4_t poly = vmlaq_f32( g_XMExpEst6, g_XMExpEst7, y );
    poly = vmlaq_f32( g_XMExpEst5, poly, y );
    poly = vmlaq_f32( g_XMExpEst4, poly, y );
    poly = vmlaq_f32( g_XMExpEst3, poly, y );
    poly = vmlaq_f32( g_XMExpEst2, poly, y );
    poly = vmlaq_f32( g_XMExpEst1, poly, y );
    poly = vmlaq_f32( g_XMOne, poly, y );

    int32x4_t biased = vaddq_s32(itrunc, g_XMExponentBias);
    biased = vshlq_n_s32(biased, 23);
    float32x4_t result0 = XMVectorDivide(biased, poly);

    biased = vaddq_s32(itrunc, g_XM253);
    biased = vshlq_n_s32(biased, 23);
    float32x4_t result1 = XMVectorDivide(biased, poly);
    result1 = vmulq_f32(g_XMMinNormal.v, result1);

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

    int32x4_t comp = vcltq_s32( Ve, g_XMBin128);
    float32x4_t result2 = vbslq_f32( comp, result0, g_XMInfinity );

    comp = vcltq_s32(itrunc, g_XMSubnormalExponent);
    float32x4_t result3 = vbslq_f32( comp, result1, result0 );

    comp = vcltq_s32(Ve, g_XMBinNeg150);
    float32x4_t result4 = vbslq_f32( comp, result3, g_XMZero );

    int32x4_t sign = vandq_s32(Ve, g_XMNegativeZero);
    comp = vceqq_s32(sign, g_XMNegativeZero);
    float32x4_t result5 = vbslq_f32( comp, result4, result2 );

    int32x4_t t0 = vandq_s32(Ve, g_XMQNaNTest);
    int32x4_t t1 = vandq_s32(Ve, g_XMInfinity);
    t0 = vceqq_s32(t0, g_XMZero);
    t1 = vceqq_s32(t1, g_XMInfinity);
    int32x4_t isNaN = vbicq_s32( t1,t0);

    float32x4_t vResult = vbslq_f32( isNaN, g_XMQNaN, result5 );
    return vResult;
*)
end;



 function XMVectorLog2(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    int32x4_t rawBiased = vandq_s32(V, g_XMInfinity);
    int32x4_t trailing = vandq_s32(V, g_XMQNaNTest);
    int32x4_t isExponentZero = vceqq_s32(g_XMZero, rawBiased);

    // Compute exponent and significand for normals.
    int32x4_t biased = vshrq_n_u32(rawBiased, 23);
    int32x4_t exponentNor = vsubq_s32(biased, g_XMExponentBias);
    int32x4_t trailingNor = trailing;

    // Compute exponent and significand for subnormals.
    int32x4_t leading = Internal::GetLeadingBit(trailing);
    int32x4_t shift = vsubq_s32(g_XMNumTrailing, leading);
    int32x4_t exponentSub = vsubq_s32(g_XMSubnormalExponent, shift);
    int32x4_t trailingSub = vshlq_u32(trailing, shift);
    trailingSub = vandq_s32(trailingSub, g_XMQNaNTest);
    int32x4_t e = vbslq_f32( isExponentZero, exponentSub, exponentNor );
    int32x4_t t = vbslq_f32( isExponentZero, trailingSub, trailingNor );

    // Compute the approximation.
    int32x4_t tmp = vorrq_s32(g_XMOne, t);
    float32x4_t y = vsubq_f32(tmp, g_XMOne);

    float32x4_t log2 = vmlaq_f32( g_XMLogEst6, g_XMLogEst7, y );
    log2 = vmlaq_f32( g_XMLogEst5, log2, y );
    log2 = vmlaq_f32( g_XMLogEst4, log2, y );
    log2 = vmlaq_f32( g_XMLogEst3, log2, y );
    log2 = vmlaq_f32( g_XMLogEst2, log2, y );
    log2 = vmlaq_f32( g_XMLogEst1, log2, y );
    log2 = vmlaq_f32( g_XMLogEst0, log2, y );
    log2 = vmlaq_f32( vcvtq_f32_s32(e), log2, y );

    //  if (x is NaN) -> QNaN
    //  else if (V is positive)
    //      if (V is infinite) -> +inf
    //      else -> log2(V)
    //  else
    //      if (V is zero) -> -inf
    //      else -> -QNaN

    int32x4_t isInfinite = vandq_s32((V), g_XMAbsMask);
    isInfinite = vceqq_s32(isInfinite, g_XMInfinity);

    int32x4_t isGreaterZero = vcgtq_s32((V), g_XMZero);
    int32x4_t isNotFinite = vcgtq_s32((V), g_XMInfinity);
    int32x4_t isPositive = vbicq_s32( isGreaterZero,isNotFinite);

    int32x4_t isZero = vandq_s32((V), g_XMAbsMask);
    isZero = vceqq_s32(isZero, g_XMZero);

    int32x4_t t0 = vandq_s32((V), g_XMQNaNTest);
    int32x4_t t1 = vandq_s32((V), g_XMInfinity);
    t0 = vceqq_s32(t0, g_XMZero);
    t1 = vceqq_s32(t1, g_XMInfinity);
    int32x4_t isNaN = vbicq_s32( t1,t0);

    float32x4_t result = vbslq_f32( isInfinite, g_XMInfinity, log2 );
    tmp = vbslq_f32( isZero, g_XMNegInfinity, g_XMNegQNaN );
    result = vbslq_f32(isPositive, result, tmp);
    result = vbslq_f32(isNaN, g_XMQNaN, result );
    return result;
*)
end;


function XMVectorLogE(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    int32x4_t rawBiased = vandq_s32(V, g_XMInfinity);
    int32x4_t trailing = vandq_s32(V, g_XMQNaNTest);
    int32x4_t isExponentZero = vceqq_s32(g_XMZero, rawBiased);

    // Compute exponent and significand for normals.
    int32x4_t biased = vshrq_n_u32(rawBiased, 23);
    int32x4_t exponentNor = vsubq_s32(biased, g_XMExponentBias);
    int32x4_t trailingNor = trailing;

    // Compute exponent and significand for subnormals.
    int32x4_t leading = Internal::GetLeadingBit(trailing);
    int32x4_t shift = vsubq_s32(g_XMNumTrailing, leading);
    int32x4_t exponentSub = vsubq_s32(g_XMSubnormalExponent, shift);
    int32x4_t trailingSub = vshlq_u32(trailing, shift);
    trailingSub = vandq_s32(trailingSub, g_XMQNaNTest);
    int32x4_t e = vbslq_f32( isExponentZero, exponentSub, exponentNor );
    int32x4_t t = vbslq_f32( isExponentZero, trailingSub, trailingNor );

    // Compute the approximation.
    int32x4_t tmp = vorrq_s32(g_XMOne, t);
    float32x4_t y = vsubq_f32(tmp, g_XMOne);

    float32x4_t log2 = vmlaq_f32( g_XMLogEst6, g_XMLogEst7, y );
    log2 = vmlaq_f32( g_XMLogEst5, log2, y );
    log2 = vmlaq_f32( g_XMLogEst4, log2, y );
    log2 = vmlaq_f32( g_XMLogEst3, log2, y );
    log2 = vmlaq_f32( g_XMLogEst2, log2, y );
    log2 = vmlaq_f32( g_XMLogEst1, log2, y );
    log2 = vmlaq_f32( g_XMLogEst0, log2, y );
    log2 = vmlaq_f32( vcvtq_f32_s32(e), log2, y );

    log2 = vmulq_f32(g_XMInvLgE, log2);

    //  if (x is NaN) -> QNaN
    //  else if (V is positive)
    //      if (V is infinite) -> +inf
    //      else -> log2(V)
    //  else
    //      if (V is zero) -> -inf
    //      else -> -QNaN

    int32x4_t isInfinite = vandq_s32((V), g_XMAbsMask);
    isInfinite = vceqq_s32(isInfinite, g_XMInfinity);

    int32x4_t isGreaterZero = vcgtq_s32((V), g_XMZero);
    int32x4_t isNotFinite = vcgtq_s32((V), g_XMInfinity);
    int32x4_t isPositive = vbicq_s32( isGreaterZero,isNotFinite);

    int32x4_t isZero = vandq_s32((V), g_XMAbsMask);
    isZero = vceqq_s32(isZero, g_XMZero);

    int32x4_t t0 = vandq_s32((V), g_XMQNaNTest);
    int32x4_t t1 = vandq_s32((V), g_XMInfinity);
    t0 = vceqq_s32(t0, g_XMZero);
    t1 = vceqq_s32(t1, g_XMInfinity);
    int32x4_t isNaN = vbicq_s32( t1,t0);

    float32x4_t result = vbslq_f32( isInfinite, g_XMInfinity, log2 );
    tmp = vbslq_f32( isZero, g_XMNegInfinity, g_XMNegQNaN );
    result = vbslq_f32(isPositive, result, tmp);
    result = vbslq_f32(isNaN, g_XMQNaN, result );
    return result;
*)
end;


function XMVectorPow(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    XMVECTORF32 vResult = { { {
            powf(vgetq_lane_f32(V1, 0), vgetq_lane_f32(V2, 0)),
            powf(vgetq_lane_f32(V1, 1), vgetq_lane_f32(V2, 1)),
            powf(vgetq_lane_f32(V1, 2), vgetq_lane_f32(V2, 2)),
            powf(vgetq_lane_f32(V1, 3), vgetq_lane_f32(V2, 3))
        } } };
    return vResult.v;
*)
end;


function XMVectorAbs(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
   return vabsq_f32( V );
*)
end;


function XMVectorMod(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     XMVECTOR vResult = XMVectorDivide(V1, V2);
    vResult = XMVectorTruncate(vResult);
    return vmlsq_f32( V1, vResult, V2 );
*)
end;


function XMVectorModAngles(constref Angles: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     // Modulo the range of the given angles such that -XM_PI <= Angles < XM_PI
    XMVECTOR vResult = vmulq_f32(Angles,g_XMReciprocalTwoPi);
    // Use the inline function due to complexity for rounding
    vResult = XMVectorRound(vResult);
    return vmlsq_f32( Angles, vResult, g_XMTwoPi );
*)
end;


function XMVectorSin(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Force the value within the bounds of pi
    XMVECTOR x = XMVectorModAngles(V);

    // Map in [-pi/2,pi/2] with sin(y) = sin(x).
    uint32x4_t sign = vandq_u32(x, g_XMNegativeZero);
    uint32x4_t c = vorrq_u32(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
    float32x4_t absx = vabsq_f32( x );
    float32x4_t rflx = vsubq_f32(c, x);
    uint32x4_t comp = vcleq_f32(absx, g_XMHalfPi);
    x = vbslq_f32( comp, x, rflx );

    float32x4_t x2 = vmulq_f32(x, x);

    // Compute polynomial approximation
    const XMVECTOR SC1 = g_XMSinCoefficients1;
    const XMVECTOR SC0 = g_XMSinCoefficients0;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(SC0), 1);
    XMVECTOR Result = vmlaq_lane_f32(vConstants, x2, vget_low_f32(SC1), 0);

    vConstants = vdupq_lane_f32(vget_high_f32(SC0), 0);
    Result = vmlaq_f32(vConstants, Result, x2);

    vConstants = vdupq_lane_f32(vget_low_f32(SC0), 1);
    Result = vmlaq_f32(vConstants, Result, x2);

    vConstants = vdupq_lane_f32(vget_low_f32(SC0), 0);
    Result = vmlaq_f32(vConstants, Result, x2);

    Result = vmlaq_f32(g_XMOne, Result, x2);
    Result = vmulq_f32(Result, x);
    return Result;
*)
end;


// 7-degree minimax approximation
function XMVectorSinEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Force the value within the bounds of pi
    XMVECTOR x = XMVectorModAngles(V);

    // Map in [-pi/2,pi/2] with sin(y) = sin(x).
    uint32x4_t sign = vandq_u32(x, g_XMNegativeZero);
    uint32x4_t c = vorrq_u32(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
    float32x4_t absx = vabsq_f32( x );
    float32x4_t rflx = vsubq_f32(c, x);
    uint32x4_t comp = vcleq_f32(absx, g_XMHalfPi);
    x = vbslq_f32( comp, x, rflx );

    float32x4_t x2 = vmulq_f32(x, x);

    // Compute polynomial approximation
    const XMVECTOR SEC = g_XMSinCoefficients1;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(SEC), 0);
    XMVECTOR Result = vmlaq_lane_f32(vConstants, x2, vget_high_f32(SEC), 1);

    vConstants = vdupq_lane_f32(vget_low_f32(SEC), 1);
    Result = vmlaq_f32(vConstants, Result, x2);

    Result = vmlaq_f32(g_XMOne, Result, x2);
    Result = vmulq_f32(Result, x);
    return Result;
*)
end;


// 10-degree minimax approximation
function XMVectorCos(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Map V to x in [-pi,pi].
    XMVECTOR x = XMVectorModAngles(V);

    // Map in [-pi/2,pi/2] with cos(y) = sign*cos(x).
    uint32x4_t sign = vandq_u32(x, g_XMNegativeZero);
    uint32x4_t c = vorrq_u32(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
    float32x4_t absx = vabsq_f32( x );
    float32x4_t rflx = vsubq_f32(c, x);
    uint32x4_t comp = vcleq_f32(absx, g_XMHalfPi);
    x = vbslq_f32( comp, x, rflx );
    sign = vbslq_f32( comp, g_XMOne, g_XMNegativeOne );

    float32x4_t x2 = vmulq_f32(x, x);

    // Compute polynomial approximation
    const XMVECTOR CC1 = g_XMCosCoefficients1;
    const XMVECTOR CC0 = g_XMCosCoefficients0;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(CC0), 1);
    XMVECTOR Result = vmlaq_lane_f32(vConstants, x2, vget_low_f32(CC1), 0 );

    vConstants = vdupq_lane_f32(vget_high_f32(CC0), 0);
    Result = vmlaq_f32(vConstants, Result, x2);

    vConstants = vdupq_lane_f32(vget_low_f32(CC0), 1);
    Result = vmlaq_f32(vConstants, Result, x2);

    vConstants = vdupq_lane_f32(vget_low_f32(CC0), 0);
    Result = vmlaq_f32(vConstants, Result, x2);

    Result = vmlaq_f32(g_XMOne, Result, x2);
    Result = vmulq_f32(Result, sign);
    return Result;
*)
end;

// 6-degree minimax approximation
function XMVectorCosEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Map V to x in [-pi,pi].
    XMVECTOR x = XMVectorModAngles(V);

    // Map in [-pi/2,pi/2] with cos(y) = sign*cos(x).
    uint32x4_t sign = vandq_u32(x, g_XMNegativeZero);
    uint32x4_t c = vorrq_u32(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
    float32x4_t absx = vabsq_f32( x );
    float32x4_t rflx = vsubq_f32(c, x);
    uint32x4_t comp = vcleq_f32(absx, g_XMHalfPi);
    x = vbslq_f32( comp, x, rflx );
    sign = vbslq_f32( comp, g_XMOne, g_XMNegativeOne );

    float32x4_t x2 = vmulq_f32(x, x);

    // Compute polynomial approximation
    const XMVECTOR CEC = g_XMCosCoefficients1;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(CEC), 0);
    XMVECTOR Result = vmlaq_lane_f32(vConstants, x2, vget_high_f32(CEC), 1);

    vConstants = vdupq_lane_f32(vget_low_f32(CEC), 1);
    Result = vmlaq_f32(vConstants, Result, x2);

    Result = vmlaq_f32(g_XMOne, Result, x2);
    Result = vmulq_f32(Result, sign);
    return Result;
*)
end;


// 11/10-degree minimax approximation
procedure XMVectorSinCos(out pSin: TXMVECTOR; out pCos: TXMVECTOR; constref V: TXMVECTOR);
begin
    (* ToDo
     // Force the value within the bounds of pi
    XMVECTOR x = XMVectorModAngles(V);

    // Map in [-pi/2,pi/2] with cos(y) = sign*cos(x).
    uint32x4_t sign = vandq_u32(x, g_XMNegativeZero);
    uint32x4_t c = vorrq_u32(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
    float32x4_t absx = vabsq_f32( x );
    float32x4_t  rflx = vsubq_f32(c, x);
    uint32x4_t comp = vcleq_f32(absx, g_XMHalfPi);
    x = vbslq_f32( comp, x, rflx );
    sign = vbslq_f32( comp, g_XMOne, g_XMNegativeOne );

    float32x4_t x2 = vmulq_f32(x, x);

    // Compute polynomial approximation for sine
    const XMVECTOR SC1 = g_XMSinCoefficients1;
    const XMVECTOR SC0 = g_XMSinCoefficients0;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(SC0), 1);
    XMVECTOR Result = vmlaq_lane_f32(vConstants, x2, vget_low_f32(SC1), 0);

    vConstants = vdupq_lane_f32(vget_high_f32(SC0), 0);
    Result = vmlaq_f32(vConstants, Result, x2);

    vConstants = vdupq_lane_f32(vget_low_f32(SC0), 1);
    Result = vmlaq_f32(vConstants, Result, x2);

    vConstants = vdupq_lane_f32(vget_low_f32(SC0), 0);
    Result = vmlaq_f32(vConstants, Result, x2);

    Result = vmlaq_f32(g_XMOne, Result, x2);
    *pSin = vmulq_f32(Result, x);

    // Compute polynomial approximation for cosine
    const XMVECTOR CC1 = g_XMCosCoefficients1;
    const XMVECTOR CC0 = g_XMCosCoefficients0;
    vConstants = vdupq_lane_f32(vget_high_f32(CC0), 1);
    Result = vmlaq_lane_f32(vConstants, x2, vget_low_f32(CC1), 0);

    vConstants = vdupq_lane_f32(vget_high_f32(CC0), 0);
    Result = vmlaq_f32(vConstants, Result, x2);

    vConstants = vdupq_lane_f32(vget_low_f32(CC0), 1);
    Result = vmlaq_f32(vConstants, Result, x2);

    vConstants = vdupq_lane_f32(vget_low_f32(CC0), 0);
    Result = vmlaq_f32(vConstants, Result, x2);

    Result = vmlaq_f32(g_XMOne, Result, x2);
    *pCos = vmulq_f32(Result, sign);
*)
end;


procedure XMVectorSinCosEst(out pSin: TXMVECTOR; out pCos: TXMVECTOR; constref V: TXMVECTOR);
begin
    (* ToDo
     // Force the value within the bounds of pi
    XMVECTOR x = XMVectorModAngles(V);

    // Map in [-pi/2,pi/2] with cos(y) = sign*cos(x).
    uint32x4_t sign = vandq_u32(x, g_XMNegativeZero);
    uint32x4_t c = vorrq_u32(g_XMPi, sign);  // pi when x >= 0, -pi when x < 0
    float32x4_t absx = vabsq_f32( x );
    float32x4_t rflx = vsubq_f32(c, x);
    uint32x4_t comp = vcleq_f32(absx, g_XMHalfPi);
    x = vbslq_f32( comp, x, rflx );
    sign = vbslq_f32( comp, g_XMOne, g_XMNegativeOne );

    float32x4_t x2 = vmulq_f32(x, x);

    // Compute polynomial approximation for sine
    const XMVECTOR SEC = g_XMSinCoefficients1;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(SEC), 0);
    XMVECTOR Result = vmlaq_lane_f32(vConstants, x2, vget_high_f32(SEC), 1);

    vConstants = vdupq_lane_f32(vget_low_f32(SEC), 1);
    Result = vmlaq_f32(vConstants, Result, x2);

    Result = vmlaq_f32(g_XMOne, Result, x2);
    *pSin = vmulq_f32(Result, x);

    // Compute polynomial approximation
    const XMVECTOR CEC = g_XMCosCoefficients1;
    vConstants = vdupq_lane_f32(vget_high_f32(CEC), 0);
    Result = vmlaq_lane_f32(vConstants, x2, vget_high_f32(CEC), 1);

    vConstants = vdupq_lane_f32(vget_low_f32(CEC), 1);
    Result = vmlaq_f32(vConstants, Result, x2);

    Result = vmlaq_f32(g_XMOne, Result, x2);
    *pCos = vmulq_f32(Result, sign);
*)
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
// ToDo	VB = vcvtq_u32_f32( VB );
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


function XMVectorSinH(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    static const XMVECTORF32 Scale = { { { 1.442695040888963f, 1.442695040888963f, 1.442695040888963f, 1.442695040888963f } } }; // 1.0f / ln(2.0f)

    XMVECTOR V1 = vmlaq_f32( g_XMNegativeOne.v, V, Scale.v );
    XMVECTOR V2 = vmlsq_f32( g_XMNegativeOne.v, V, Scale.v );
    XMVECTOR E1 = XMVectorExp(V1);
    XMVECTOR E2 = XMVectorExp(V2);

    return vsubq_f32(E1, E2);
*)
end;

function XMVectorCosH(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    static const XMVECTORF32 Scale = { { { 1.442695040888963f, 1.442695040888963f, 1.442695040888963f, 1.442695040888963f } } }; // 1.0f / ln(2.0f)

    XMVECTOR V1 = vmlaq_f32(g_XMNegativeOne.v, V, Scale.v);
    XMVECTOR V2 = vmlsq_f32(g_XMNegativeOne.v, V, Scale.v);
    XMVECTOR E1 = XMVectorExp(V1);
    XMVECTOR E2 = XMVectorExp(V2);
    return vaddq_f32(E1, E2);
*)
end;


function XMVectorTanH(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    static const XMVECTORF32 Scale = { { { 2.8853900817779268f, 2.8853900817779268f, 2.8853900817779268f, 2.8853900817779268f } } }; // 2.0f / ln(2.0f)

    XMVECTOR E = vmulq_f32(V, Scale.v);
    E = XMVectorExp(E);
    E = vmlaq_f32( g_XMOneHalf.v, E, g_XMOneHalf.v );
    E = XMVectorReciprocal(E);
    return vsubq_f32(g_XMOne.v, E);
*)
end;

function XMVectorASin(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    uint32x4_t nonnegative = vcgeq_f32(V, g_XMZero);
    float32x4_t x = vabsq_f32(V);

    // Compute (1-|V|), clamp to zero to avoid sqrt of negative number.
    float32x4_t oneMValue = vsubq_f32(g_XMOne, x);
    float32x4_t clampOneMValue = vmaxq_f32(g_XMZero, oneMValue);
    float32x4_t root = XMVectorSqrt(clampOneMValue);

    // Compute polynomial approximation
    const XMVECTOR AC1 = g_XMArcCoefficients1;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(AC1), 0);
    XMVECTOR t0 = vmlaq_lane_f32( vConstants, x, vget_high_f32(AC1), 1 );

    vConstants = vdupq_lane_f32(vget_low_f32(AC1), 1);
    t0 = vmlaq_f32( vConstants, t0, x );

    vConstants = vdupq_lane_f32(vget_low_f32(AC1), 0);
    t0 = vmlaq_f32( vConstants, t0, x );

    const XMVECTOR AC0 = g_XMArcCoefficients0;
    vConstants = vdupq_lane_f32(vget_high_f32(AC0), 1);
    t0 = vmlaq_f32( vConstants, t0, x );

    vConstants = vdupq_lane_f32(vget_high_f32(AC0), 0);
    t0 = vmlaq_f32( vConstants, t0, x );

    vConstants = vdupq_lane_f32(vget_low_f32(AC0), 1);
    t0 = vmlaq_f32( vConstants, t0, x );

    vConstants = vdupq_lane_f32(vget_low_f32(AC0), 0);
    t0 = vmlaq_f32( vConstants, t0, x );
    t0 = vmulq_f32(t0, root);

    float32x4_t t1 = vsubq_f32(g_XMPi, t0);
    t0 = vbslq_f32( nonnegative, t0, t1 );
    t0 = vsubq_f32(g_XMHalfPi, t0);
    return t0;
*)
end;


function XMVectorASinEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo

    uint32x4_t nonnegative = vcgeq_f32(V, g_XMZero);
    float32x4_t x = vabsq_f32(V);

    // Compute (1-|V|), clamp to zero to avoid sqrt of negative number.
    float32x4_t oneMValue = vsubq_f32(g_XMOne, x);
    float32x4_t clampOneMValue = vmaxq_f32(g_XMZero, oneMValue);
    float32x4_t root = XMVectorSqrt(clampOneMValue);

    // Compute polynomial approximation
    const XMVECTOR AEC = g_XMArcEstCoefficients;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(AEC), 0);
    XMVECTOR t0 = vmlaq_lane_f32( vConstants, x, vget_high_f32(AEC), 1 );

    vConstants = vdupq_lane_f32(vget_low_f32(AEC), 1);
    t0 = vmlaq_f32( vConstants, t0, x );

    vConstants = vdupq_lane_f32(vget_low_f32(AEC), 0);
    t0 = vmlaq_f32( vConstants, t0, x );
    t0 = vmulq_f32(t0, root);

    float32x4_t t1 = vsubq_f32(g_XMPi, t0);
    t0 = vbslq_f32( nonnegative, t0, t1 );
    t0 = vsubq_f32(g_XMHalfPi, t0);
    return t0;
    *)
end;

function XMVectorACos(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    uint32x4_t nonnegative = vcgeq_f32(V, g_XMZero);
    float32x4_t x = vabsq_f32(V);

    // Compute (1-|V|), clamp to zero to avoid sqrt of negative number.
    float32x4_t oneMValue = vsubq_f32(g_XMOne, x);
    float32x4_t clampOneMValue = vmaxq_f32(g_XMZero, oneMValue);
    float32x4_t root = XMVectorSqrt(clampOneMValue);

    // Compute polynomial approximation
    const XMVECTOR AC1 = g_XMArcCoefficients1;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(AC1), 0);
    XMVECTOR t0 = vmlaq_lane_f32( vConstants, x, vget_high_f32(AC1), 1 );

    vConstants = vdupq_lane_f32(vget_low_f32(AC1), 1);
    t0 = vmlaq_f32( vConstants, t0, x );

    vConstants = vdupq_lane_f32(vget_low_f32(AC1), 0);
    t0 = vmlaq_f32( vConstants, t0, x );

    const XMVECTOR AC0 = g_XMArcCoefficients0;
    vConstants = vdupq_lane_f32(vget_high_f32(AC0), 1);
    t0 = vmlaq_f32( vConstants, t0, x );

    vConstants = vdupq_lane_f32(vget_high_f32(AC0), 0);
    t0 = vmlaq_f32( vConstants, t0, x );

    vConstants = vdupq_lane_f32(vget_low_f32(AC0), 1);
    t0 = vmlaq_f32( vConstants, t0, x );

    vConstants = vdupq_lane_f32(vget_low_f32(AC0), 0);
    t0 = vmlaq_f32( vConstants, t0, x );
    t0 = vmulq_f32(t0, root);

    float32x4_t t1 = vsubq_f32(g_XMPi, t0);
    t0 = vbslq_f32( nonnegative, t0, t1 );
    return t0;
*)
end;



function XMVectorACosEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    uint32x4_t nonnegative = vcgeq_f32(V, g_XMZero);
    float32x4_t x = vabsq_f32(V);

    // Compute (1-|V|), clamp to zero to avoid sqrt of negative number.
    float32x4_t oneMValue = vsubq_f32(g_XMOne, x);
    float32x4_t clampOneMValue = vmaxq_f32(g_XMZero, oneMValue);
    float32x4_t root = XMVectorSqrt(clampOneMValue);

    // Compute polynomial approximation
    const XMVECTOR AEC = g_XMArcEstCoefficients;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(AEC), 0);
    XMVECTOR t0 = vmlaq_lane_f32( vConstants, x, vget_high_f32(AEC), 1 );

    vConstants = vdupq_lane_f32(vget_low_f32(AEC), 1);
    t0 = vmlaq_f32( vConstants, t0, x );

    vConstants = vdupq_lane_f32(vget_low_f32(AEC), 0);
    t0 = vmlaq_f32( vConstants, t0, x );
    t0 = vmulq_f32(t0, root);

    float32x4_t t1 = vsubq_f32(g_XMPi, t0);
    t0 = vbslq_f32( nonnegative, t0, t1 );
    return t0;
*)
end;


function XMVectorATan(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    float32x4_t absV = vabsq_f32(V);
    float32x4_t invV = XMVectorReciprocal(V);
    uint32x4_t comp = vcgtq_f32(V, g_XMOne);
    uint32x4_t sign = vbslq_f32(comp, g_XMOne, g_XMNegativeOne);
    comp = vcleq_f32(absV, g_XMOne);
    sign = vbslq_f32(comp, g_XMZero, sign);
    uint32x4_t x = vbslq_f32(comp, V, invV);

    float32x4_t x2 = vmulq_f32(x, x);

    // Compute polynomial approximation
    const XMVECTOR TC1 = g_XMATanCoefficients1;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(TC1), 0);
    XMVECTOR Result = vmlaq_lane_f32( vConstants, x2, vget_high_f32(TC1), 1 );

    vConstants = vdupq_lane_f32(vget_low_f32(TC1), 1);
    Result = vmlaq_f32( vConstants, Result, x2 );

    vConstants = vdupq_lane_f32(vget_low_f32(TC1), 0);
    Result = vmlaq_f32( vConstants, Result, x2 );

    const XMVECTOR TC0 = g_XMATanCoefficients0;
    vConstants = vdupq_lane_f32(vget_high_f32(TC0), 1);
    Result = vmlaq_f32( vConstants, Result, x2 );

    vConstants = vdupq_lane_f32(vget_high_f32(TC0), 0);
    Result = vmlaq_f32( vConstants, Result, x2 );

    vConstants = vdupq_lane_f32(vget_low_f32(TC0), 1);
    Result = vmlaq_f32( vConstants, Result, x2 );

    vConstants = vdupq_lane_f32(vget_low_f32(TC0), 0);
    Result = vmlaq_f32( vConstants, Result, x2 );

    Result = vmlaq_f32( g_XMOne, Result, x2 );
    Result = vmulq_f32( Result, x );

    float32x4_t result1 = vmulq_f32(sign, g_XMHalfPi);
    result1 = vsubq_f32(result1, Result);

    comp = vceqq_f32(sign, g_XMZero);
    Result = vbslq_f32( comp, Result, result1 );
    return Result;
*)
end;
function XMVectorATanEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    float32x4_t absV = vabsq_f32(V);
    float32x4_t invV = XMVectorReciprocalEst(V);
    uint32x4_t comp = vcgtq_f32(V, g_XMOne);
    uint32x4_t sign = vbslq_f32(comp, g_XMOne, g_XMNegativeOne );
    comp = vcleq_f32(absV, g_XMOne);
    sign = vbslq_f32(comp, g_XMZero, sign );
    uint32x4_t x = vbslq_f32(comp, V, invV );

    float32x4_t x2 = vmulq_f32(x, x);

    // Compute polynomial approximation
    const XMVECTOR AEC = g_XMATanEstCoefficients1;
    XMVECTOR vConstants = vdupq_lane_f32(vget_high_f32(AEC), 0);
    XMVECTOR Result = vmlaq_lane_f32( vConstants, x2, vget_high_f32(AEC), 1 );

    vConstants = vdupq_lane_f32(vget_low_f32(AEC), 1);
    Result = vmlaq_f32( vConstants, Result, x2 );

    vConstants = vdupq_lane_f32(vget_low_f32( AEC), 0);
    Result = vmlaq_f32( vConstants, Result, x2 );

    // ATanEstCoefficients0 is already splatted
    Result = vmlaq_f32( g_XMATanEstCoefficients0, Result, x2 );
    Result = vmulq_f32( Result, x );

    float32x4_t result1 = vmulq_f32(sign, g_XMHalfPi);
    result1 = vsubq_f32(result1, Result);

    comp = vceqq_f32(sign, g_XMZero);
    Result = vbslq_f32( comp, Result, result1 );
    return Result;
*)
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

function XMVectorLerp(constref V0: TXMVECTOR; constref V1: TXMVECTOR; constref t: single): TXMVECTOR;
begin
    (* ToDo
    XMVECTOR L = vsubq_f32( V1, V0 );
    return vmlaq_n_f32( V0, L, t );
*)
end;


function XMVectorLerpV(constref V0: TXMVECTOR; constref V1: TXMVECTOR; constref T: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
   XMVECTOR L = vsubq_f32( V1, V0 );
    return vmlaq_f32( V0, L, T );
*)
end;

function XMVectorHermite(constref Position0: TXMVECTOR;constref Tangent0: TXMVECTOR;constref Position1: TXMVECTOR; constref Tangent1: TXMVECTOR;constref t: single): TXMVECTOR;
begin
    (* ToDo
    float t2 = t * t;
    float t3 = t * t2;

    float p0 = 2.0f * t3 - 3.0f * t2 + 1.0;
    float t0 = t3 - 2.0f * t2 + t;
    float p1 = -2.0f * t3 + 3.0f * t2;
    float t1 = t3 - t2;

    XMVECTOR vResult = vmulq_n_f32(Position0, p0 );
    vResult = vmlaq_n_f32( vResult, Tangent0, t0 );
    vResult = vmlaq_n_f32( vResult, Position1, p1 );
    vResult = vmlaq_n_f32( vResult, Tangent1, t1 );
    return vResult;
*)
end;



function XMVectorHermiteV(constref Position0: TXMVECTOR;constref  Tangent0: TXMVECTOR;constref  Position1: TXMVECTOR;constref  Tangent1: TXMVECTOR;constref  T: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     static const XMVECTORF32 CatMulT2 = { { { -3.0f, -2.0f, 3.0f, -1.0f } } };
    static const XMVECTORF32 CatMulT3 = { { { 2.0f, 1.0f, -2.0f, 1.0f } } };

    XMVECTOR T2 = vmulq_f32(T,T);
    XMVECTOR T3 = vmulq_f32(T,T2);
    // Mul by the constants against t^2
    T2 = vmulq_f32(T2,CatMulT2);
    // Mul by the constants against t^3
    T3 = vmlaq_f32(T2, T3, CatMulT3 );
    // T3 now has the pre-result.
    // I need to add t.y only
    T2 = vandq_u32(T,g_XMMaskY);
    T3 = vaddq_f32(T3,T2);
    // Add 1.0f to x
    T3 = vaddq_f32(T3,g_XMIdentityR0);
    // Now, I have the constants created
    // Mul the x constant to Position0
    XMVECTOR vResult = vmulq_lane_f32( Position0, vget_low_f32( T3 ), 0 ); // T3[0]
    // Mul the y constant to Tangent0
    vResult = vmlaq_lane_f32(vResult, Tangent0, vget_low_f32( T3 ), 1 ); // T3[1]
    // Mul the z constant to Position1
    vResult = vmlaq_lane_f32(vResult, Position1, vget_high_f32( T3 ), 0  ); // T3[2]
    // Mul the w constant to Tangent1
    vResult = vmlaq_lane_f32(vResult, Tangent1, vget_high_f32( T3 ), 1 ); // T3[3]
    return vResult;
*)
end;

function XMVectorCatmullRom(constref Position0: TXMVECTOR;constref  Position1: TXMVECTOR;constref  Position2: TXMVECTOR;constref  Position3: TXMVECTOR;constref  t: single): TXMVECTOR;
begin
    (* ToDo
    float t2 = t * t;
    float t3 = t * t2;

    float p0 = (-t3 + 2.0f * t2 - t) * 0.5f;
    float p1 = (3.0f * t3 - 5.0f * t2 + 2.0f) * 0.5f;
    float p2 = (-3.0f * t3 + 4.0f * t2 + t) * 0.5f;
    float p3 = (t3 - t2) * 0.5f;

    XMVECTOR P1 = vmulq_n_f32(Position1, p1);
    XMVECTOR P0 = vmlaq_n_f32(P1, Position0, p0);
    XMVECTOR P3 = vmulq_n_f32(Position3, p3);
    XMVECTOR P2 = vmlaq_n_f32(P3, Position2, p2);
    P0 = vaddq_f32(P0,P2);
    return P0;
*)
end;




function XMVectorCatmullRomV(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR;constref  Position2: TXMVECTOR;constref  Position3: TXMVECTOR; constref T: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
   static const XMVECTORF32 Catmul2 = { { { 2.0f, 2.0f, 2.0f, 2.0f } } };
    static const XMVECTORF32 Catmul3 = { { { 3.0f, 3.0f, 3.0f, 3.0f } } };
    static const XMVECTORF32 Catmul4 = { { { 4.0f, 4.0f, 4.0f, 4.0f } } };
    static const XMVECTORF32 Catmul5 = { { { 5.0f, 5.0f, 5.0f, 5.0f } } };
    // Cache T^2 and T^3
    XMVECTOR T2 = vmulq_f32(T,T);
    XMVECTOR T3 = vmulq_f32(T,T2);
    // Perform the Position0 term
    XMVECTOR vResult = vaddq_f32(T2,T2);
    vResult = vsubq_f32(vResult,T);
    vResult = vsubq_f32(vResult,T3);
    vResult = vmulq_f32(vResult,Position0);
    // Perform the Position1 term and add
    XMVECTOR vTemp = vmulq_f32(T3,Catmul3);
    vTemp = vmlsq_f32(vTemp, T2, Catmul5);
    vTemp = vaddq_f32(vTemp,Catmul2);
    vResult = vmlaq_f32(vResult, vTemp, Position1);
    // Perform the Position2 term and add
    vTemp = vmulq_f32(T2,Catmul4);
    vTemp = vmlsq_f32(vTemp, T3, Catmul3);
    vTemp = vaddq_f32(vTemp,T);
    vResult = vmlaq_f32(vResult, vTemp, Position2);
    // Position3 is the last term
    T3 = vsubq_f32(T3,T2);
    vResult = vmlaq_f32(vResult, T3, Position3);
    // Multiply by 0.5f and exit
    vResult = vmulq_f32(vResult,g_XMOneHalf);
    return vResult;
*)
end;


function XMVectorBaryCentric(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR;constref  Position2: TXMVECTOR;constref  f: single; constref g: single): TXMVECTOR;
begin
    (* ToDo
     XMVECTOR R1 = vsubq_f32(Position1,Position0);
    XMVECTOR R2 = vsubq_f32(Position2,Position0);
    R1 = vmlaq_n_f32( Position0, R1, f);
    return vmlaq_n_f32( R1, R2, g );
*)
end;


function XMVectorBaryCentricV(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;constref  F: TXMVECTOR;constref  G: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    XMVECTOR R1 = vsubq_f32(Position1,Position0);
    XMVECTOR R2 = vsubq_f32(Position2,Position0);
    R1 = vmlaq_f32( Position0, R1, F );
    return vmlaq_f32( R1, R2, G);
*)
end;


{***************************************************************************
 *
 * 2D Vector
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------


function XMVector2Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
    uint32x2_t vTemp = vceq_f32( vget_low_f32(V1), vget_low_f32(V2) );
    return ( vget_lane_u64( vTemp, 0 ) == 0xFFFFFFFFFFFFFFFFU );
*)
end;


function XMVector2EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    (* ToDo
    uint32x2_t vTemp = vceq_f32( vget_low_f32(V1), vget_low_f32(V2) );
    uint64_t r = vget_lane_u64( vTemp, 0 );
    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFFFFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
*)
end;

function XMVector2EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
    uint32x2_t vTemp = vceq_u32( vget_low_u32(V1), vget_low_u32(V2) );
    return ( vget_lane_u64( vTemp, 0 ) == 0xFFFFFFFFFFFFFFFFU );
*)
end;

function XMVector2EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    (* ToDo
    uint32x2_t vTemp = vceq_u32( vget_low_u32(V1), vget_low_u32(V2) );
    uint64_t r = vget_lane_u64( vTemp, 0 );
    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFFFFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
*)
end;

function XMVector2NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean;
begin
    (* ToDo
     float32x2_t vDelta = vsub_f32(vget_low_u32(V1), vget_low_u32(V2));
    uint32x2_t vTemp = vacle_f32( vDelta, vget_low_u32(Epsilon) );
    uint64_t r = vget_lane_u64( vTemp, 0 );
    return ( r == 0xFFFFFFFFFFFFFFFFU );
*)
end;


function XMVector2NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
     uint32x2_t vTemp = vceq_f32( vget_low_f32(V1), vget_low_f32(V2) );
    return ( vget_lane_u64( vTemp, 0 ) != 0xFFFFFFFFFFFFFFFFU );
*)
end;

function XMVector2NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
    uint32x2_t vTemp = vceq_u32( vget_low_u32(V1), vget_low_u32(V2) );
    return ( vget_lane_u64( vTemp, 0 ) != 0xFFFFFFFFFFFFFFFFU );
*)
end;

function XMVector2Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
    uint32x2_t vTemp = vcgt_f32( vget_low_f32(V1), vget_low_f32(V2) );
    return ( vget_lane_u64( vTemp, 0 ) == 0xFFFFFFFFFFFFFFFFU );
*)
end;


function XMVector2GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    (* ToDo
    uint32x2_t vTemp = vcgt_f32( vget_low_f32(V1), vget_low_f32(V2) );
    uint64_t r = vget_lane_u64( vTemp, 0 );
    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFFFFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
*)
end;


function XMVector2GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
   uint32x2_t vTemp = vcge_f32( vget_low_f32(V1), vget_low_f32(V2) );
    return ( vget_lane_u64( vTemp, 0 ) == 0xFFFFFFFFFFFFFFFFU );
*)
end;


function XMVector2GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    (* ToDo
    uint32x2_t vTemp = vcge_f32( vget_low_f32(V1), vget_low_f32(V2) );
    uint64_t r = vget_lane_u64( vTemp, 0 );
    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFFFFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
*)
end;


function XMVector2Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
    uint32x2_t vTemp = vclt_f32( vget_low_f32(V1), vget_low_f32(V2) );
    return ( vget_lane_u64( vTemp, 0 ) == 0xFFFFFFFFFFFFFFFFU );
    *)
end;


function XMVector2LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
     uint32x2_t vTemp = vcle_f32( vget_low_f32(V1), vget_low_f32(V2) );
    return ( vget_lane_u64( vTemp, 0 ) == 0xFFFFFFFFFFFFFFFFU );
    *)
end;

function XMVector2InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean;
begin
    (* ToDo
    float32x2_t VL = vget_low_f32( V );
    float32x2_t B = vget_low_f32( Bounds );
    // Test if less than or equal
    uint32x2_t ivTemp1 = vcle_f32(VL,B);
    // Negate the bounds
    float32x2_t vTemp2 = vneg_f32(B);
    // Test if greater or equal (Reversed)
    uint32x2_t ivTemp2 = vcle_f32(vTemp2,VL);
    // Blend answers
    ivTemp1 = vand_u32(ivTemp1,ivTemp2);
    // x and y in bounds?
    return ( vget_lane_u64( ivTemp1, 0 ) == 0xFFFFFFFFFFFFFFFFU );
*)
end;

function XMVector2IsNaN(constref V: TXMVECTOR): boolean;
begin
    (* ToDo
    float32x2_t VL = vget_low_f32( V );
    // Test against itself. NaN is always not equal
    uint32x2_t vTempNan = vceq_f32( VL, VL );
    // If x or y are NaN, the mask is zero
    return ( vget_lane_u64( vTempNan, 0 ) != 0xFFFFFFFFFFFFFFFFU );
*)
end;

function XMVector2IsInfinite(constref V: TXMVECTOR): boolean;
begin
    (* ToDo
    // Mask off the sign bit
    uint32x2_t vTemp = vand_u32( vget_low_f32( V ) , vget_low_f32( g_XMAbsMask ) );
    // Compare to infinity
    vTemp = vceq_f32(vTemp, vget_low_f32( g_XMInfinity) );
    // If any are infinity, the signs are true.
    return vget_lane_u64( vTemp, 0 ) != 0;
*)
end;



//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------

function XMVector2Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Perform the dot product on x and y
    float32x2_t vTemp = vmul_f32( vget_low_f32(V1), vget_low_f32(V2) );
    vTemp = vpadd_f32( vTemp, vTemp );
    return vcombine_f32( vTemp, vTemp );
*)
end;

function XMVector2Cross(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    static const XMVECTORF32 Negate = { { { 1.f, -1.f, 0, 0 } } };

    float32x2_t vTemp = vmul_f32( vget_low_f32( V1 ), vrev64_f32( vget_low_f32( V2 ) ) );
    vTemp = vmul_f32( vTemp, vget_low_f32( Negate ) );
    vTemp = vpadd_f32( vTemp, vTemp );
    return vcombine_f32( vTemp, vTemp );
*)
end;


function XMVector2ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     float32x2_t VL = vget_low_f32(V);
    // Dot2
    float32x2_t vTemp = vmul_f32( VL, VL );
    vTemp = vpadd_f32( vTemp, vTemp );
    // Reciprocal sqrt (estimate)
    vTemp = vrsqrte_f32( vTemp );
    return vcombine_f32( vTemp, vTemp );
*)
end;


function XMVector2ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    float32x2_t VL = vget_low_f32(V);
    // Dot2
    float32x2_t vTemp = vmul_f32( VL, VL );
    vTemp = vpadd_f32( vTemp, vTemp );
    // Reciprocal sqrt
    float32x2_t  S0 = vrsqrte_f32(vTemp);
    float32x2_t  P0 = vmul_f32( vTemp, S0 );
    float32x2_t  R0 = vrsqrts_f32( P0, S0 );
    float32x2_t  S1 = vmul_f32( S0, R0 );
    float32x2_t  P1 = vmul_f32( vTemp, S1 );
    float32x2_t  R1 = vrsqrts_f32( P1, S1 );
    float32x2_t Result = vmul_f32( S1, R1 );
    return vcombine_f32( Result, Result );
*)
end;

function XMVector2LengthEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    float32x2_t VL = vget_low_f32(V);
    // Dot2
    float32x2_t vTemp = vmul_f32( VL, VL );
    vTemp = vpadd_f32( vTemp, vTemp );
    const float32x2_t zero = vdup_n_f32(0);
    uint32x2_t VEqualsZero = vceq_f32( vTemp, zero );
    // Sqrt (estimate)
    float32x2_t Result = vrsqrte_f32( vTemp );
    Result = vmul_f32( vTemp, Result );
    Result = vbsl_f32( VEqualsZero, zero, Result );
    return vcombine_f32( Result, Result );
*)
end;

function XMVector2Length(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     float32x2_t VL = vget_low_f32(V);
    // Dot2
    float32x2_t vTemp = vmul_f32( VL, VL );
    vTemp = vpadd_f32( vTemp, vTemp );
    const float32x2_t zero = vdup_n_f32(0);
    uint32x2_t VEqualsZero = vceq_f32( vTemp, zero );
    // Sqrt
    float32x2_t S0 = vrsqrte_f32( vTemp );
    float32x2_t P0 = vmul_f32( vTemp, S0 );
    float32x2_t R0 = vrsqrts_f32( P0, S0 );
    float32x2_t S1 = vmul_f32( S0, R0 );
    float32x2_t P1 = vmul_f32( vTemp, S1 );
    float32x2_t R1 = vrsqrts_f32( P1, S1 );
    float32x2_t Result = vmul_f32( S1, R1 );
    Result = vmul_f32( vTemp, Result );
    Result = vbsl_f32( VEqualsZero, zero, Result );
    return vcombine_f32( Result, Result );
*)
end;


// XMVector2NormalizeEst uses a reciprocal estimate and
// returns QNaN on zero and infinite vectors.
function XMVector2NormalizeEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
   float32x2_t VL = vget_low_f32(V);
    // Dot2
    float32x2_t vTemp = vmul_f32( VL, VL );
    vTemp = vpadd_f32( vTemp, vTemp );
    // Reciprocal sqrt (estimate)
    vTemp = vrsqrte_f32( vTemp );
    // Normalize
    float32x2_t Result = vmul_f32( VL, vTemp );
    return vcombine_f32( Result, Result );
*)
end;

function XMVector2Normalize(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    float32x2_t VL = vget_low_f32(V);
    // Dot2
    float32x2_t vTemp = vmul_f32( VL, VL );
    vTemp = vpadd_f32( vTemp, vTemp );
    uint32x2_t VEqualsZero = vceq_f32( vTemp, vdup_n_f32(0) );
    uint32x2_t VEqualsInf = vceq_f32( vTemp, vget_low_f32(g_XMInfinity) );
    // Reciprocal sqrt (2 iterations of Newton-Raphson)
    float32x2_t S0 = vrsqrte_f32( vTemp );
    float32x2_t P0 = vmul_f32( vTemp, S0 );
    float32x2_t R0 = vrsqrts_f32( P0, S0 );
    float32x2_t S1 = vmul_f32( S0, R0 );
    float32x2_t P1 = vmul_f32( vTemp, S1 );
    float32x2_t R1 = vrsqrts_f32( P1, S1 );
    vTemp = vmul_f32( S1, R1 );
    // Normalize
    float32x2_t Result = vmul_f32( VL, vTemp );
    Result = vbsl_f32( VEqualsZero, vdup_n_f32(0), Result );
    Result = vbsl_f32( VEqualsInf, vget_low_f32(g_XMQNaN), Result );
    return vcombine_f32( Result, Result );
*)
end;



function XMVector2RefractV(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR; constref RefractionIndex: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     float32x2_t IL = vget_low_f32( Incident );
    float32x2_t NL = vget_low_f32( Normal );
    float32x2_t RIL = vget_low_f32( RefractionIndex );
    // Get the 2D Dot product of Incident-Normal
    float32x2_t vTemp = vmul_f32(IL, NL);
    float32x2_t IDotN = vpadd_f32( vTemp, vTemp );
    // vTemp = 1.0f - RefractionIndex * RefractionIndex * (1.0f - IDotN * IDotN)
    vTemp = vmls_f32( vget_low_f32( g_XMOne ), IDotN, IDotN);
    vTemp = vmul_f32(vTemp,RIL);
    vTemp = vmls_f32(vget_low_f32( g_XMOne ), vTemp, RIL );
    // If any terms are <=0, sqrt() will fail, punt to zero
    uint32x2_t vMask = vcgt_f32(vTemp, vget_low_f32(g_XMZero) );
    // Sqrt(vTemp)
    float32x2_t S0 = vrsqrte_f32(vTemp);
    float32x2_t P0 = vmul_f32( vTemp, S0 );
    float32x2_t R0 = vrsqrts_f32( P0, S0 );
    float32x2_t S1 = vmul_f32( S0, R0 );
    float32x2_t P1 = vmul_f32( vTemp, S1 );
    float32x2_t R1 = vrsqrts_f32( P1, S1 );
    float32x2_t S2 = vmul_f32( S1, R1 );
    vTemp = vmul_f32( vTemp, S2 );
    // R = RefractionIndex * IDotN + sqrt(R)
    vTemp = vmla_f32( vTemp, RIL, IDotN );
    // Result = RefractionIndex * Incident - Normal * R
    float32x2_t vResult = vmul_f32(RIL,IL);
    vResult = vmls_f32( vResult, vTemp, NL );
    vResult = vand_u32(vResult,vMask);
    return vcombine_f32(vResult, vResult);
*)
end;


function XMVector2Orthogonal(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    static const XMVECTORF32 Negate = { { { -1.f, 1.f, 0, 0 } } };
    const float32x2_t zero = vdup_n_f32(0);

    float32x2_t VL = vget_low_f32( V );
    float32x2_t Result = vmul_f32( vrev64_f32( VL ), vget_low_f32( Negate ) );
    return vcombine_f32( Result, zero );
*)
end;

function XMVector2IntersectLine(constref Line1Point1: TXMVECTOR; constref Line1Point2: TXMVECTOR; constref Line2Point1: TXMVECTOR;
    constref Line2Point2: TXMVECTOR): TXMVECTOR;
var
    V1, V2, V3, C1, C2, Zero, Scale: TXMVECTOR;
begin
    V1 := XMVectorSubtract(Line1Point2, Line1Point1);
    V2 := XMVectorSubtract(Line2Point2, Line2Point1);
    V3 := XMVectorSubtract(Line1Point1, Line2Point1);

    C1 := XMVector2Cross(V1, V2);
    C2 := XMVector2Cross(V2, V3);


    Zero := XMVectorZero();
    if (XMVector2NearEqual(C1, Zero, g_XMEpsilon.v)) then
    begin
        if (XMVector2NearEqual(C2, Zero, g_XMEpsilon.v)) then
        begin
            // Coincident
            Result := g_XMInfinity.v;
        end
        else
        begin
            // Parallel
            Result := g_XMQNaN.v;
        end;
    end
    else
    begin
        // Intersection point  :=  Line1Point1 + V1 * (C2 / C1)
        Scale := XMVectorReciprocal(C1);
        Scale := XMVectorMultiply(C2, Scale);
        Result := XMVectorMultiplyAdd(V1, Scale, Line1Point1);
    end;
end;

function XMVector2Transform(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
begin
    (* ToDo
    float32x2_t VL = vget_low_f32( V );
    float32x4_t Result = vmlaq_lane_f32( M.r[3], M.r[1], VL, 1 ); // Y
    return vmlaq_lane_f32( Result, M.r[0], VL, 0 ); // X
*)
end;

function XMVector2TransformStream(out pOutputStream: PXMFLOAT4;constref  OutputStride: size_t;constref  pInputStream: PXMFLOAT2;
    constref InputStride: size_t;constref  VectorCount: size_t;constref  M: TXMMATRIX): PXMFLOAT4;
begin
    (* ToDo
    const uint8_t* pInputVector = (const uint8_t* )pInputStream;
    uint8_t* pOutputVector = (uint8_t* )pOutputStream;

    const XMVECTOR row0 = M.r[0];
    const XMVECTOR row1 = M.r[1];
    const XMVECTOR row3 = M.r[3];

    size_t i = 0;
    size_t four = VectorCount >> 2;
    if ( four > 0 )
    {
        if ((InputStride == sizeof(XMFLOAT2)) && (OutputStride == sizeof(XMFLOAT4)))
        {
            for (size_t j = 0; j < four; ++j)
            {
                float32x4x2_t V = vld2q_f32( reinterpret_cast<const float*>(pInputVector) );
                pInputVector += sizeof(XMFLOAT2)*4;

                float32x2_t r3 = vget_low_f32( row3 );
                float32x2_t r = vget_low_f32( row0 );
                XMVECTOR vResult0 = vmlaq_lane_f32( vdupq_lane_f32( r3, 0 ), V.val[0], r, 0 ); // Ax+M
                XMVECTOR vResult1 = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), V.val[0], r, 1 ); // Bx+N

                __prefetch( pInputVector );

                r3 = vget_high_f32( row3 );
                r = vget_high_f32( row0 );
                XMVECTOR vResult2 = vmlaq_lane_f32( vdupq_lane_f32( r3, 0 ), V.val[0], r, 0 ); // Cx+O
                XMVECTOR vResult3 = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), V.val[0], r, 1 ); // Dx+P

                __prefetch( pInputVector+XM_CACHE_LINE_SIZE );

                r = vget_low_f32( row1 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[1], r, 0 ); // Ax+Ey+M
                vResult1 = vmlaq_lane_f32( vResult1, V.val[1], r, 1 ); // Bx+Fy+N

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*2) );

                r = vget_high_f32( row1 );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[1], r, 0 ); // Cx+Gy+O
                vResult3 = vmlaq_lane_f32( vResult3, V.val[1], r, 1 ); // Dx+Hy+P

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*3) );

                float32x4x4_t R;
                R.val[0] = vResult0;
                R.val[1] = vResult1;
                R.val[2] = vResult2;
                R.val[3] = vResult3;

                vst4q_f32( reinterpret_cast<float*>(pOutputVector), R );
                pOutputVector += sizeof(XMFLOAT4)*4;

                i += 4;
            }
        }
    }

    for (; i < VectorCount; i++)
    {
        float32x2_t V = vld1_f32( reinterpret_cast<const float*>(pInputVector) );
        pInputVector += InputStride;

        XMVECTOR vResult = vmlaq_lane_f32( row3, row0, V, 0 ); // X
        vResult = vmlaq_lane_f32( vResult, row1, V, 1 ); // Y

        vst1q_f32( reinterpret_cast<float*>(pOutputVector), vResult );
        pOutputVector += OutputStride;
    }

    return pOutputStream;
    *)
end;



function XMVector2TransformCoordStream(out pOutputStream: PXMFLOAT2;constref  OutputStride: size_t; constref pInputStream: PXMFLOAT2;
   constref  InputStride: size_t;constref  VectorCount: size_t;constref  M: TXMMATRIX): PXMFLOAT2;
begin
    (* ToDo
    const uint8_t* pInputVector = (const uint8_t* )pInputStream;
    uint8_t* pOutputVector = (uint8_t* )pOutputStream;

    const XMVECTOR row0 = M.r[0];
    const XMVECTOR row1 = M.r[1];
    const XMVECTOR row3 = M.r[3];

    size_t i = 0;
    size_t four = VectorCount >> 2;
    if ( four > 0 )
    {
        if ((InputStride == sizeof(XMFLOAT2)) && (OutputStride == sizeof(XMFLOAT2)))
        {
            for (size_t j = 0; j < four; ++j)
            {
                float32x4x2_t V = vld2q_f32( reinterpret_cast<const float*>(pInputVector) );
                pInputVector += sizeof(XMFLOAT2)*4;

                float32x2_t r3 = vget_low_f32( row3 );
                float32x2_t r = vget_low_f32( row0 );
                XMVECTOR vResult0 = vmlaq_lane_f32( vdupq_lane_f32( r3, 0 ), V.val[0], r, 0 ); // Ax+M
                XMVECTOR vResult1 = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), V.val[0], r, 1 ); // Bx+N

                __prefetch( pInputVector );

                r3 = vget_high_f32( row3 );
                r = vget_high_f32( row0 );
                XMVECTOR W = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), V.val[0], r, 1 ); // Dx+P

                __prefetch( pInputVector+XM_CACHE_LINE_SIZE );

                r = vget_low_f32( row1 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[1], r, 0 ); // Ax+Ey+M
                vResult1 = vmlaq_lane_f32( vResult1, V.val[1], r, 1 ); // Bx+Fy+N

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*2) );

                r = vget_high_f32( row1 );
                W = vmlaq_lane_f32( W, V.val[1], r, 1 ); // Dx+Hy+P

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*3) );

#if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
                V.val[0] = vdivq_f32( vResult0, W );
                V.val[1] = vdivq_f32( vResult1, W );
#else
                // 2 iterations of Newton-Raphson refinement of reciprocal
                float32x4_t Reciprocal = vrecpeq_f32(W);
                float32x4_t S = vrecpsq_f32( Reciprocal, W );
                Reciprocal = vmulq_f32( S, Reciprocal );
                S = vrecpsq_f32( Reciprocal, W );
                Reciprocal = vmulq_f32( S, Reciprocal );

                V.val[0] = vmulq_f32( vResult0, Reciprocal );
                V.val[1] = vmulq_f32( vResult1, Reciprocal );
#endif

                vst2q_f32( reinterpret_cast<float*>(pOutputVector),V );
                pOutputVector += sizeof(XMFLOAT2)*4;

                i += 4;
            }
        }
    }

    for (; i < VectorCount; i++)
    {
        float32x2_t V = vld1_f32( reinterpret_cast<const float*>(pInputVector) );
        pInputVector += InputStride;

        XMVECTOR vResult = vmlaq_lane_f32( row3, row0, V, 0 ); // X
        vResult = vmlaq_lane_f32( vResult, row1, V, 1 ); // Y

        V = vget_high_f32( vResult );
        float32x2_t W = vdup_lane_f32( V, 1 );

#if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
        V = vget_low_f32( vResult );
        V = vdiv_f32( V, W );
#else
        // 2 iterations of Newton-Raphson refinement of reciprocal for W
        float32x2_t Reciprocal = vrecpe_f32( W );
        float32x2_t S = vrecps_f32( Reciprocal, W );
        Reciprocal = vmul_f32( S, Reciprocal );
        S = vrecps_f32( Reciprocal, W );
        Reciprocal = vmul_f32( S, Reciprocal );

        V = vget_low_f32( vResult );
        V = vmul_f32( V, Reciprocal );
#endif

        vst1_f32( reinterpret_cast<float*>(pOutputVector), V );
        pOutputVector += OutputStride;
    }

    return pOutputStream;
*)
end;


function XMVector2TransformNormal(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
begin
    (* ToDo
      float32x2_t VL = vget_low_f32( V );
    float32x4_t Result = vmulq_lane_f32( M.r[1], VL, 1 ); // Y
    return vmlaq_lane_f32( Result, M.r[0], VL, 0 ); // X
*)
end;


function XMVector2TransformNormalStream(out pOutputStream: PXMFLOAT2;constref  OutputStride: size_t;constref  pInputStream: PXMFLOAT2;
   constref  InputStride: size_t;constref  VectorCount: size_t;constref  M: TXMMATRIX): PXMFLOAT2;
begin
    (* ToDo
    const uint8_t* pInputVector = (const uint8_t* )pInputStream;
    uint8_t* pOutputVector = (uint8_t* )pOutputStream;

    const XMVECTOR row0 = M.r[0];
    const XMVECTOR row1 = M.r[1];

    size_t i = 0;
    size_t four = VectorCount >> 2;
    if ( four > 0 )
    {
        if ((InputStride == sizeof(XMFLOAT2)) && (OutputStride == sizeof(XMFLOAT2)))
        {
            for (size_t j = 0; j < four; ++j)
            {
                float32x4x2_t V = vld2q_f32( reinterpret_cast<const float*>(pInputVector) );
                pInputVector += sizeof(XMFLOAT2)*4;

                float32x2_t r = vget_low_f32( row0 );
                XMVECTOR vResult0 = vmulq_lane_f32( V.val[0], r, 0 ); // Ax
                XMVECTOR vResult1 = vmulq_lane_f32( V.val[0], r, 1 ); // Bx

                __prefetch( pInputVector );
                __prefetch( pInputVector+XM_CACHE_LINE_SIZE );

                r = vget_low_f32( row1 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[1], r, 0 ); // Ax+Ey
                vResult1 = vmlaq_lane_f32( vResult1, V.val[1], r, 1 ); // Bx+Fy

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*2) );
                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*3) );

                V.val[0] = vResult0;
                V.val[1] = vResult1;

                vst2q_f32( reinterpret_cast<float*>(pOutputVector), V );
                pOutputVector += sizeof(XMFLOAT2)*4;

                i += 4;
            }
        }
    }

    for (; i < VectorCount; i++)
    {
        float32x2_t V = vld1_f32( reinterpret_cast<const float*>(pInputVector) );
        pInputVector += InputStride;

        XMVECTOR vResult = vmulq_lane_f32( row0, V, 0 ); // X
        vResult = vmlaq_lane_f32( vResult, row1, V, 1 ); // Y

        V = vget_low_f32( vResult );
        vst1_f32( reinterpret_cast<float*>(pOutputVector), V );
        pOutputVector += OutputStride;
    }

    return pOutputStream;
*)
end;



{***************************************************************************
 *
 * 3D Vector
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------


function XMVector3Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
    uint32x4_t vResult = vceqq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( (vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU) == 0xFFFFFFU );
*)
end;

function XMVector3EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    (* ToDo
    uint32x4_t vResult = vceqq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU;

    uint32_t CR = 0;
    if ( r == 0xFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
*)
end;


function XMVector3EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
     uint32x4_t vResult = vceqq_u32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( (vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU) == 0xFFFFFFU );
*)
end;

function XMVector3EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    (* ToDo
    uint32x4_t vResult = vceqq_u32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU;

    uint32_t CR = 0;
    if ( r == 0xFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
*)
end;


function XMVector3NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean;
begin
    (* ToDo
    float32x4_t vDelta = vsubq_f32( V1, V2 );
    uint32x4_t vResult = vacleq_f32( vDelta, Epsilon );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( (vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU) == 0xFFFFFFU );
*)
end;

function XMVector3NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
    uint32x4_t vResult = vceqq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( (vget_lane_u32(vTemp.val[1], 1)  & 0xFFFFFFU) != 0xFFFFFFU );
*)
end;

function XMVector3NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
    uint32x4_t vResult = vceqq_u32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( (vget_lane_u32(vTemp.val[1], 1)  & 0xFFFFFFU) != 0xFFFFFFU );
*)
end;

function XMVector3Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
   uint32x4_t vResult = vcgtq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( (vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU) == 0xFFFFFFU );
*)
end;



function XMVector3GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    (* ToDo
    uint32x4_t vResult = vcgtq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU;

    uint32_t CR = 0;
    if ( r == 0xFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
*)
end;

function XMVector3GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (* ToDo
    uint32x4_t vResult = vcgeq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( (vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU) == 0xFFFFFFU );
*)
end;


function XMVector3GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    (* ToDo
    uint32x4_t vResult = vcgeq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU;

    uint32_t CR = 0;
    if ( r == 0xFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
*)
end;


function XMVector3Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
     uint32x4_t vResult = vcltq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( (vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU) == 0xFFFFFFU );
    *)
end;



function XMVector3LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
     uint32x4_t vResult = vcleq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( (vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU) == 0xFFFFFFU );
    *)
end;



function XMVector3InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean;
begin
    (* ToDo
     // Test if less than or equal
    uint32x4_t ivTemp1 = vcleq_f32(V,Bounds);
    // Negate the bounds
    float32x4_t vTemp2 = vnegq_f32(Bounds);
    // Test if greater or equal (Reversed)
    uint32x4_t ivTemp2 = vcleq_f32(vTemp2,V);
    // Blend answers
    ivTemp1 = vandq_u32(ivTemp1,ivTemp2);
    // in bounds?
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(ivTemp1), vget_high_u8(ivTemp1));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( (vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU) == 0xFFFFFFU );
*)
end;



function XMVector3IsNaN(constref V: TXMVECTOR): boolean;
begin
    (* ToDo
    // Test against itself. NaN is always not equal
    uint32x4_t vTempNan = vceqq_f32( V, V );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vTempNan), vget_high_u8(vTempNan));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    // If x or y or z are NaN, the mask is zero
    return ( (vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU) != 0xFFFFFFU );
*)
end;

function XMVector3IsInfinite(constref V: TXMVECTOR): boolean;
begin
    (* ToDo
    // Mask off the sign bit
    uint32x4_t vTempInf = vandq_u32( V, g_XMAbsMask );
    // Compare to infinity
    vTempInf = vceqq_f32(vTempInf, g_XMInfinity );
    // If any are infinity, the signs are true.
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vTempInf), vget_high_u8(vTempInf));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( (vget_lane_u32(vTemp.val[1], 1) & 0xFFFFFFU) != 0 );
*)
end;


//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------



function XMVector3Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    float32x4_t vTemp = vmulq_f32( V1, V2 );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vpadd_f32( v1, v1 );
    v2 = vdup_lane_f32( v2, 0 );
    v1 = vadd_f32( v1, v2 );
    return vcombine_f32( v1, v1 );
*)
end;



function XMVector3Cross(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    float32x2_t v1xy = vget_low_f32(V1);
    float32x2_t v2xy = vget_low_f32(V2);

    float32x2_t v1yx = vrev64_f32( v1xy );
    float32x2_t v2yx = vrev64_f32( v2xy );

    float32x2_t v1zz = vdup_lane_f32( vget_high_f32(V1), 0 );
    float32x2_t v2zz = vdup_lane_f32( vget_high_f32(V2), 0 );

    XMVECTOR vResult = vmulq_f32( vcombine_f32(v1yx,v1xy), vcombine_f32(v2zz,v2yx) );
    vResult = vmlsq_f32( vResult, vcombine_f32(v1zz,v1yx), vcombine_f32(v2yx,v2xy) );
    vResult = veorq_u32( vResult, g_XMFlipY );
    return vandq_u32( vResult, g_XMMask3 );
*)
end;



function XMVector3ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Dot3
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vpadd_f32( v1, v1 );
    v2 = vdup_lane_f32( v2, 0 );
    v1 = vadd_f32( v1, v2 );
    // Reciprocal sqrt (estimate)
    v2 = vrsqrte_f32( v1 );
    return vcombine_f32(v2, v2);
*)
end;


function XMVector3ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Dot3
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vpadd_f32( v1, v1 );
    v2 = vdup_lane_f32( v2, 0 );
    v1 = vadd_f32( v1, v2 );
    // Reciprocal sqrt
    float32x2_t  S0 = vrsqrte_f32(v1);
    float32x2_t  P0 = vmul_f32( v1, S0 );
    float32x2_t  R0 = vrsqrts_f32( P0, S0 );
    float32x2_t  S1 = vmul_f32( S0, R0 );
    float32x2_t  P1 = vmul_f32( v1, S1 );
    float32x2_t  R1 = vrsqrts_f32( P1, S1 );
    float32x2_t Result = vmul_f32( S1, R1 );
    return vcombine_f32( Result, Result );
*)
end;



function XMVector3LengthEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     // Dot3
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vpadd_f32( v1, v1 );
    v2 = vdup_lane_f32( v2, 0 );
    v1 = vadd_f32( v1, v2 );
    const float32x2_t zero = vdup_n_f32(0);
    uint32x2_t VEqualsZero = vceq_f32( v1, zero );
    // Sqrt (estimate)
    float32x2_t Result = vrsqrte_f32( v1 );
    Result = vmul_f32( v1, Result );
    Result = vbsl_f32( VEqualsZero, zero, Result );
    return vcombine_f32( Result, Result );
*)
end;



function XMVector3Length(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Dot3
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vpadd_f32( v1, v1 );
    v2 = vdup_lane_f32( v2, 0 );
    v1 = vadd_f32( v1, v2 );
    const float32x2_t zero = vdup_n_f32(0);
    uint32x2_t VEqualsZero = vceq_f32( v1, zero );
    // Sqrt
    float32x2_t S0 = vrsqrte_f32( v1 );
    float32x2_t P0 = vmul_f32( v1, S0 );
    float32x2_t R0 = vrsqrts_f32( P0, S0 );
    float32x2_t S1 = vmul_f32( S0, R0 );
    float32x2_t P1 = vmul_f32( v1, S1 );
    float32x2_t R1 = vrsqrts_f32( P1, S1 );
    float32x2_t Result = vmul_f32( S1, R1 );
    Result = vmul_f32( v1, Result );
    Result = vbsl_f32( VEqualsZero, zero, Result );
    return vcombine_f32( Result, Result );
*)
end;



// XMVector3NormalizeEst uses a reciprocal estimate and
// returns QNaN on zero and infinite vectors.
function XMVector3NormalizeEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Dot3
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vpadd_f32( v1, v1 );
    v2 = vdup_lane_f32( v2, 0 );
    v1 = vadd_f32( v1, v2 );
    // Reciprocal sqrt (estimate)
    v2 = vrsqrte_f32( v1 );
    // Normalize
    return vmulq_f32( V, vcombine_f32(v2,v2) );
*)
end;



function XMVector3Normalize(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     // Dot3
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vpadd_f32( v1, v1 );
    v2 = vdup_lane_f32( v2, 0 );
    v1 = vadd_f32( v1, v2 );
    uint32x2_t VEqualsZero = vceq_f32( v1, vdup_n_f32(0) );
    uint32x2_t VEqualsInf = vceq_f32( v1, vget_low_f32(g_XMInfinity) );
    // Reciprocal sqrt (2 iterations of Newton-Raphson)
    float32x2_t S0 = vrsqrte_f32( v1 );
    float32x2_t P0 = vmul_f32( v1, S0 );
    float32x2_t R0 = vrsqrts_f32( P0, S0 );
    float32x2_t S1 = vmul_f32( S0, R0 );
    float32x2_t P1 = vmul_f32( v1, S1 );
    float32x2_t R1 = vrsqrts_f32( P1, S1 );
    v2 = vmul_f32( S1, R1 );
    // Normalize
    XMVECTOR vResult = vmulq_f32( V, vcombine_f32(v2,v2) );
    vResult = vbslq_f32( vcombine_f32(VEqualsZero,VEqualsZero), vdupq_n_f32(0), vResult );
    return vbslq_f32( vcombine_f32(VEqualsInf,VEqualsInf), g_XMQNaN, vResult );
*)
end;



function XMVector3RefractV(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR;constref  RefractionIndex: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    XMVECTOR IDotN = XMVector3Dot(Incident,Normal);

    // R = 1.0f - RefractionIndex * RefractionIndex * (1.0f - IDotN * IDotN)
    float32x4_t R = vmlsq_f32( g_XMOne, IDotN, IDotN);
    R = vmulq_f32(R, RefractionIndex);
    R = vmlsq_f32(g_XMOne, R, RefractionIndex );

    uint32x4_t vResult = vcleq_f32(R,g_XMZero);
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    if ( vget_lane_u32(vTemp.val[1], 1) == 0xFFFFFFFFU )
    {
        // Total internal reflection
        vResult = g_XMZero;
    }
    else
    {
        // Sqrt(R)
        float32x4_t S0 = vrsqrteq_f32(R);
        float32x4_t P0 = vmulq_f32( R, S0 );
        float32x4_t R0 = vrsqrtsq_f32( P0, S0 );
        float32x4_t S1 = vmulq_f32( S0, R0 );
        float32x4_t P1 = vmulq_f32( R, S1 );
        float32x4_t R1 = vrsqrtsq_f32( P1, S1 );
        float32x4_t S2 = vmulq_f32( S1, R1 );
        R = vmulq_f32( R, S2 );
        // R = RefractionIndex * IDotN + sqrt(R)
        R = vmlaq_f32( R, RefractionIndex, IDotN );
        // Result = RefractionIndex * Incident - Normal * R
        vResult = vmulq_f32(RefractionIndex, Incident);
        vResult = vmlsq_f32( vResult, R, Normal );
    }
    return vResult;
*)
end;



function XMVector3Transform(constref V: TXMVECTOR;constref  M: TXMMATRIX): TXMVECTOR;
begin
    (* ToDo
     float32x2_t VL = vget_low_f32( V );
    XMVECTOR vResult = vmlaq_lane_f32( M.r[3], M.r[0], VL, 0 ); // X
    vResult = vmlaq_lane_f32( vResult, M.r[1], VL, 1 ); // Y
    return vmlaq_lane_f32( vResult, M.r[2], vget_high_f32( V ), 0 ); // Z
*)
end;



function XMVector3TransformStream(out pOutputStream: PXMFLOAT4;constref  OutputStride: size_t; constref  pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t;constref  M: TXMMATRIX): PXMFLOAT4;
begin
    (* ToDo
     const uint8_t* pInputVector = (const uint8_t* )pInputStream;
    uint8_t* pOutputVector = (uint8_t* )pOutputStream;

    const XMVECTOR row0 = M.r[0];
    const XMVECTOR row1 = M.r[1];
    const XMVECTOR row2 = M.r[2];
    const XMVECTOR row3 = M.r[3];

    size_t i = 0;
    size_t four = VectorCount >> 2;
    if ( four > 0 )
    {
        if ((InputStride == sizeof(XMFLOAT3)) && (OutputStride == sizeof(XMFLOAT4)))
        {
            for (size_t j = 0; j < four; ++j)
            {
                float32x4x3_t V = vld3q_f32( reinterpret_cast<const float*>(pInputVector) );
                pInputVector += sizeof(XMFLOAT3)*4;

                float32x2_t r3 = vget_low_f32( row3 );
                float32x2_t r = vget_low_f32( row0 );
                XMVECTOR vResult0 = vmlaq_lane_f32( vdupq_lane_f32( r3, 0 ), V.val[0], r, 0 ); // Ax+M
                XMVECTOR vResult1 = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), V.val[0], r, 1 ); // Bx+N

                __prefetch( pInputVector );

                r3 = vget_high_f32( row3 );
                r = vget_high_f32( row0 );
                XMVECTOR vResult2 = vmlaq_lane_f32( vdupq_lane_f32( r3, 0 ), V.val[0], r, 0 ); // Cx+O
                XMVECTOR vResult3 = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), V.val[0], r, 1 ); // Dx+P

                __prefetch( pInputVector+XM_CACHE_LINE_SIZE );

                r = vget_low_f32( row1 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[1], r, 0 ); // Ax+Ey+M
                vResult1 = vmlaq_lane_f32( vResult1, V.val[1], r, 1 ); // Bx+Fy+N

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*2) );

                r = vget_high_f32( row1 );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[1], r, 0 ); // Cx+Gy+O
                vResult3 = vmlaq_lane_f32( vResult3, V.val[1], r, 1 ); // Dx+Hy+P

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*3) );

                r = vget_low_f32( row2 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[2], r, 0 ); // Ax+Ey+Iz+M
                vResult1 = vmlaq_lane_f32( vResult1, V.val[2], r, 1 ); // Bx+Fy+Jz+N

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*4) );

                r = vget_high_f32( row2 );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[2], r, 0 ); // Cx+Gy+Kz+O
                vResult3 = vmlaq_lane_f32( vResult3, V.val[2], r, 1 ); // Dx+Hy+Lz+P

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*5) );

                float32x4x4_t R;
                R.val[0] = vResult0;
                R.val[1] = vResult1;
                R.val[2] = vResult2;
                R.val[3] = vResult3;

                vst4q_f32( reinterpret_cast<float*>(pOutputVector), R );
                pOutputVector += sizeof(XMFLOAT4)*4;

                i += 4;
            }
        }
    }

    for (; i < VectorCount; i++)
    {
        float32x2_t VL = vld1_f32( reinterpret_cast<const float*>(pInputVector) );
        float32x2_t zero = vdup_n_f32(0);
        float32x2_t VH = vld1_lane_f32( reinterpret_cast<const float*>(pInputVector)+2, zero, 0 );
        pInputVector += InputStride;

        XMVECTOR vResult = vmlaq_lane_f32( row3, row0, VL, 0 ); // X
        vResult = vmlaq_lane_f32( vResult, row1, VL, 1); // Y
        vResult = vmlaq_lane_f32( vResult, row2, VH, 0 ); // Z

        vst1q_f32( reinterpret_cast<float*>(pOutputVector), vResult );
        pOutputVector += OutputStride;
    }

    return pOutputStream;
*)
end;



function XMVector3TransformCoordStream(out pOutputStream: PXMFLOAT3;constref  OutputStride: size_t; constref  pInputStream: PXMFLOAT3;
   constref  InputStride: size_t;constref  VectorCount: size_t;constref  M: TXMMATRIX): PXMFLOAT3;
begin
    (* ToDo
     const uint8_t* pInputVector = (const uint8_t* )pInputStream;
    uint8_t* pOutputVector = (uint8_t* )pOutputStream;

    const XMVECTOR row0 = M.r[0];
    const XMVECTOR row1 = M.r[1];
    const XMVECTOR row2 = M.r[2];
    const XMVECTOR row3 = M.r[3];

    size_t i = 0;
    size_t four = VectorCount >> 2;
    if ( four > 0 )
    {
        if ((InputStride == sizeof(XMFLOAT3)) && (OutputStride == sizeof(XMFLOAT3)))
        {
            for (size_t j = 0; j < four; ++j)
            {
                float32x4x3_t V = vld3q_f32( reinterpret_cast<const float*>(pInputVector) );
                pInputVector += sizeof(XMFLOAT3)*4;

                float32x2_t r3 = vget_low_f32( row3 );
                float32x2_t r = vget_low_f32( row0 );
                XMVECTOR vResult0 = vmlaq_lane_f32( vdupq_lane_f32( r3, 0 ), V.val[0], r, 0 ); // Ax+M
                XMVECTOR vResult1 = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), V.val[0], r, 1 ); // Bx+N

                __prefetch( pInputVector );

                r3 = vget_high_f32( row3 );
                r = vget_high_f32( row0 );
                XMVECTOR vResult2 = vmlaq_lane_f32( vdupq_lane_f32( r3, 0 ), V.val[0], r, 0 ); // Cx+O
                XMVECTOR W = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), V.val[0], r, 1 ); // Dx+P

                __prefetch( pInputVector+XM_CACHE_LINE_SIZE );

                r = vget_low_f32( row1 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[1], r, 0 ); // Ax+Ey+M
                vResult1 = vmlaq_lane_f32( vResult1, V.val[1], r, 1 ); // Bx+Fy+N

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*2) );

                r = vget_high_f32( row1 );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[1], r, 0 ); // Cx+Gy+O
                W = vmlaq_lane_f32( W, V.val[1], r, 1 ); // Dx+Hy+P

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*3) );

                r = vget_low_f32( row2 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[2], r, 0 ); // Ax+Ey+Iz+M
                vResult1 = vmlaq_lane_f32( vResult1, V.val[2], r, 1 ); // Bx+Fy+Jz+N

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*4) );

                r = vget_high_f32( row2 );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[2], r, 0 ); // Cx+Gy+Kz+O
                W = vmlaq_lane_f32( W, V.val[2], r, 1 ); // Dx+Hy+Lz+P

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*5) );

#if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
                V.val[0] = vdivq_f32( vResult0, W );
                V.val[1] = vdivq_f32( vResult1, W );
                V.val[2] = vdivq_f32( vResult2, W );
#else
                // 2 iterations of Newton-Raphson refinement of reciprocal
                float32x4_t Reciprocal = vrecpeq_f32(W);
                float32x4_t S = vrecpsq_f32( Reciprocal, W );
                Reciprocal = vmulq_f32( S, Reciprocal );
                S = vrecpsq_f32( Reciprocal, W );
                Reciprocal = vmulq_f32( S, Reciprocal );

                V.val[0] = vmulq_f32( vResult0, Reciprocal );
                V.val[1] = vmulq_f32( vResult1, Reciprocal );
                V.val[2] = vmulq_f32( vResult2, Reciprocal );
#endif

                vst3q_f32( reinterpret_cast<float*>(pOutputVector),V );
                pOutputVector += sizeof(XMFLOAT3)*4;

                i += 4;
            }
        }
    }

    for (; i < VectorCount; i++)
    {
        float32x2_t VL = vld1_f32( reinterpret_cast<const float*>(pInputVector) );
        float32x2_t zero = vdup_n_f32(0);
        float32x2_t VH = vld1_lane_f32( reinterpret_cast<const float*>(pInputVector)+2, zero, 0 );
        pInputVector += InputStride;

        XMVECTOR vResult = vmlaq_lane_f32( row3, row0, VL, 0 ); // X
        vResult = vmlaq_lane_f32( vResult, row1, VL, 1 ); // Y
        vResult = vmlaq_lane_f32( vResult, row2, VH, 0 ); // Z

        VH = vget_high_f32(vResult);
        XMVECTOR W = vdupq_lane_f32( VH, 1 );

#if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
        vResult = vdivq_f32( vResult, W );
#else
        // 2 iterations of Newton-Raphson refinement of reciprocal for W
        float32x4_t Reciprocal = vrecpeq_f32( W );
        float32x4_t S = vrecpsq_f32( Reciprocal, W );
        Reciprocal = vmulq_f32( S, Reciprocal );
        S = vrecpsq_f32( Reciprocal, W );
        Reciprocal = vmulq_f32( S, Reciprocal );

        vResult = vmulq_f32( vResult, Reciprocal );
#endif

        VL = vget_low_f32( vResult );
        vst1_f32( reinterpret_cast<float*>(pOutputVector), VL );
        vst1q_lane_f32( reinterpret_cast<float*>(pOutputVector)+2, vResult, 2 );
        pOutputVector += OutputStride;
    }

    return pOutputStream;
*)
end;


function XMVector3TransformNormal(constref V: TXMVECTOR;constref  M: TXMMATRIX): TXMVECTOR;
begin
    (* ToDo
    float32x2_t VL = vget_low_f32( V );
    XMVECTOR vResult = vmulq_lane_f32( M.r[0], VL, 0 ); // X
    vResult = vmlaq_lane_f32( vResult, M.r[1], VL, 1 ); // Y
    return vmlaq_lane_f32( vResult, M.r[2], vget_high_f32( V ), 0 ); // Z
*)
end;



function XMVector3TransformNormalStream(out pOutputStream: PXMFLOAT3;constref  OutputStride: size_t; constref  pInputStream: PXMFLOAT3;
   constref  InputStride: size_t;constref  VectorCount: size_t;constref  M: TXMMATRIX): PXMFLOAT3;
begin
    (* ToDo
    const uint8_t* pInputVector = (const uint8_t* )pInputStream;
    uint8_t* pOutputVector = (uint8_t* )pOutputStream;

    const XMVECTOR row0 = M.r[0];
    const XMVECTOR row1 = M.r[1];
    const XMVECTOR row2 = M.r[2];

    size_t i = 0;
    size_t four = VectorCount >> 2;
    if ( four > 0 )
    {
        if ((InputStride == sizeof(XMFLOAT3)) && (OutputStride == sizeof(XMFLOAT3)))
        {
            for (size_t j = 0; j < four; ++j)
            {
                float32x4x3_t V = vld3q_f32( reinterpret_cast<const float*>(pInputVector) );
                pInputVector += sizeof(XMFLOAT3)*4;

                float32x2_t r = vget_low_f32( row0 );
                XMVECTOR vResult0 = vmulq_lane_f32( V.val[0], r, 0 ); // Ax
                XMVECTOR vResult1 = vmulq_lane_f32( V.val[0], r, 1 ); // Bx

                __prefetch( pInputVector );

                r = vget_high_f32( row0 );
                XMVECTOR vResult2 = vmulq_lane_f32( V.val[0], r, 0 ); // Cx

                __prefetch( pInputVector+XM_CACHE_LINE_SIZE );

                r = vget_low_f32( row1 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[1], r, 0 ); // Ax+Ey
                vResult1 = vmlaq_lane_f32( vResult1, V.val[1], r, 1 ); // Bx+Fy

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*2) );

                r = vget_high_f32( row1 );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[1], r, 0 ); // Cx+Gy

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*3) );

                r = vget_low_f32( row2 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[2], r, 0 ); // Ax+Ey+Iz
                vResult1 = vmlaq_lane_f32( vResult1, V.val[2], r, 1 ); // Bx+Fy+Jz

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*4) );

                r = vget_high_f32( row2 );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[2], r, 0 ); // Cx+Gy+Kz

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*5) );

                V.val[0] = vResult0;
                V.val[1] = vResult1;
                V.val[2] = vResult2;

                vst3q_f32( reinterpret_cast<float*>(pOutputVector), V );
                pOutputVector += sizeof(XMFLOAT3)*4;

                i += 4;
            }
        }
    }

    for (; i < VectorCount; i++)
    {
        float32x2_t VL = vld1_f32( reinterpret_cast<const float*>(pInputVector) );
        float32x2_t zero = vdup_n_f32(0);
        float32x2_t VH = vld1_lane_f32( reinterpret_cast<const float*>(pInputVector)+2, zero, 0 );
        pInputVector += InputStride;

        XMVECTOR vResult = vmulq_lane_f32( row0, VL, 0 ); // X
        vResult = vmlaq_lane_f32( vResult, row1, VL, 1 ); // Y
        vResult = vmlaq_lane_f32( vResult, row2, VH, 0 ); // Z

        VL = vget_low_f32( vResult );
        vst1_f32( reinterpret_cast<float*>(pOutputVector), VL );
        vst1q_lane_f32( reinterpret_cast<float*>(pOutputVector)+2, vResult, 2 );
        pOutputVector += OutputStride;
    }

    return pOutputStream;
*)
end;


function XMVector3ProjectStream(out pOutputStream: PXMFLOAT3;constref  OutputStride: size_t; constref  pInputStream: PXMFLOAT3;
  constref   InputStride: size_t; constref VectorCount: size_t;constref  ViewportX: single;constref  ViewportY: single;constref  ViewportWidth: single; constref ViewportHeight: single;
   constref  ViewportMinZ: single;constref  ViewportMaxZ: single; constref Projection: TXMMATRIX; constref View: TXMMATRIX; constref World: TXMMATRIX): PXMFLOAT3;
begin
    (* ToDo
     const float HalfViewportWidth = ViewportWidth * 0.5f;
    const float HalfViewportHeight = ViewportHeight * 0.5f;

    XMMATRIX Transform = XMMatrixMultiply(World, View);
    Transform = XMMatrixMultiply(Transform, Projection);
    const uint8_t* pInputVector = (const uint8_t* )pInputStream;
    uint8_t* pOutputVector = (uint8_t* )pOutputStream;

    size_t i = 0;
    size_t four = VectorCount >> 2;
    if ( four > 0 )
    {
        if ((InputStride == sizeof(XMFLOAT3)) && (OutputStride == sizeof(XMFLOAT3)))
        {
            XMVECTOR ScaleX = vdupq_n_f32(HalfViewportWidth);
            XMVECTOR ScaleY = vdupq_n_f32(-HalfViewportHeight);
            XMVECTOR ScaleZ = vdupq_n_f32(ViewportMaxZ - ViewportMinZ);

            XMVECTOR OffsetX = vdupq_n_f32(ViewportX + HalfViewportWidth);
            XMVECTOR OffsetY = vdupq_n_f32(ViewportY + HalfViewportHeight);
            XMVECTOR OffsetZ = vdupq_n_f32(ViewportMinZ);

            for (size_t j = 0; j < four; ++j)
            {
                float32x4x3_t V = vld3q_f32( reinterpret_cast<const float*>(pInputVector) );
                pInputVector += sizeof(XMFLOAT3)*4;

                float32x2_t r3 = vget_low_f32( Transform.r[3] );
                float32x2_t r = vget_low_f32( Transform.r[0] );
                XMVECTOR vResult0 = vmlaq_lane_f32( vdupq_lane_f32( r3, 0 ), V.val[0], r, 0 ); // Ax+M
                XMVECTOR vResult1 = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), V.val[0], r, 1 ); // Bx+N

                __prefetch( pInputVector );

                r3 = vget_high_f32( Transform.r[3] );
                r = vget_high_f32( Transform.r[0] );
                XMVECTOR vResult2 = vmlaq_lane_f32( vdupq_lane_f32( r3, 0 ), V.val[0], r, 0 ); // Cx+O
                XMVECTOR W = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), V.val[0], r, 1 ); // Dx+P

                __prefetch( pInputVector+XM_CACHE_LINE_SIZE );

                r = vget_low_f32( Transform.r[1] );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[1], r, 0 ); // Ax+Ey+M
                vResult1 = vmlaq_lane_f32( vResult1, V.val[1], r, 1 ); // Bx+Fy+N

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*2) );

                r = vget_high_f32( Transform.r[1] );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[1], r, 0 ); // Cx+Gy+O
                W = vmlaq_lane_f32( W, V.val[1], r, 1 ); // Dx+Hy+P

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*3) );

                r = vget_low_f32( Transform.r[2] );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[2], r, 0 ); // Ax+Ey+Iz+M
                vResult1 = vmlaq_lane_f32( vResult1, V.val[2], r, 1 ); // Bx+Fy+Jz+N

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*4) );

                r = vget_high_f32( Transform.r[2] );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[2], r, 0 ); // Cx+Gy+Kz+O
                W = vmlaq_lane_f32( W, V.val[2], r, 1 ); // Dx+Hy+Lz+P

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*5) );

#if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
                vResult0 = vdivq_f32( vResult0, W );
                vResult1 = vdivq_f32( vResult1, W );
                vResult2 = vdivq_f32( vResult2, W );
#else
                // 2 iterations of Newton-Raphson refinement of reciprocal
                float32x4_t Reciprocal = vrecpeq_f32(W);
                float32x4_t S = vrecpsq_f32( Reciprocal, W );
                Reciprocal = vmulq_f32( S, Reciprocal );
                S = vrecpsq_f32( Reciprocal, W );
                Reciprocal = vmulq_f32( S, Reciprocal );

                vResult0 = vmulq_f32( vResult0, Reciprocal );
                vResult1 = vmulq_f32( vResult1, Reciprocal );
                vResult2 = vmulq_f32( vResult2, Reciprocal );
#endif

                V.val[0] = vmlaq_f32( OffsetX, vResult0, ScaleX );
                V.val[1] = vmlaq_f32( OffsetY, vResult1, ScaleY );
                V.val[2] = vmlaq_f32( OffsetZ, vResult2, ScaleZ );

                vst3q_f32( reinterpret_cast<float*>(pOutputVector),V );
                pOutputVector += sizeof(XMFLOAT3)*4;

                i += 4;
            }
        }
    }

    if ( i < VectorCount)
    {
        XMVECTOR Scale = XMVectorSet(HalfViewportWidth, -HalfViewportHeight, ViewportMaxZ - ViewportMinZ, 1.0f);
        XMVECTOR Offset = XMVectorSet(ViewportX + HalfViewportWidth, ViewportY + HalfViewportHeight, ViewportMinZ, 0.0f);

        for (; i < VectorCount; i++)
        {
            float32x2_t VL = vld1_f32( reinterpret_cast<const float*>(pInputVector) );
            float32x2_t zero = vdup_n_f32(0);
            float32x2_t VH = vld1_lane_f32( reinterpret_cast<const float*>(pInputVector)+2, zero, 0 );
            pInputVector += InputStride;

            XMVECTOR vResult = vmlaq_lane_f32( Transform.r[3], Transform.r[0], VL, 0 ); // X
            vResult = vmlaq_lane_f32( vResult, Transform.r[1], VL, 1 ); // Y
            vResult = vmlaq_lane_f32( vResult, Transform.r[2], VH, 0 ); // Z

            VH = vget_high_f32(vResult);
            XMVECTOR W = vdupq_lane_f32( VH, 1 );

#if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
            vResult = vdivq_f32( vResult, W );
#else
            // 2 iterations of Newton-Raphson refinement of reciprocal for W
            float32x4_t Reciprocal = vrecpeq_f32( W );
            float32x4_t S = vrecpsq_f32( Reciprocal, W );
            Reciprocal = vmulq_f32( S, Reciprocal );
            S = vrecpsq_f32( Reciprocal, W );
            Reciprocal = vmulq_f32( S, Reciprocal );

            vResult = vmulq_f32( vResult, Reciprocal );
#endif

            vResult = vmlaq_f32( Offset, vResult, Scale );

            VL = vget_low_f32( vResult );
            vst1_f32( reinterpret_cast<float*>(pOutputVector), VL );
            vst1q_lane_f32( reinterpret_cast<float*>(pOutputVector)+2, vResult, 2 );
            pOutputVector += OutputStride;
        }
    }

    return pOutputStream;
*)
end;



function XMVector3UnprojectStream(out pOutputStream: PXMFLOAT3;constref  OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t;constref  VectorCount: size_t; constref ViewportX: single; constref ViewportY: single; constref ViewportWidth: single;constref  ViewportHeight: single;
    constref ViewportMinZ: single; constref ViewportMaxZ: single;constref  Projection: TXMMATRIX;constref  View: TXMMATRIX;constref  World: TXMMATRIX): PXMFLOAT3;

begin
    (* ToDo
    XMMATRIX Transform = XMMatrixMultiply(World, View);
    Transform = XMMatrixMultiply(Transform, Projection);
    Transform = XMMatrixInverse(nullptr, Transform);

    const uint8_t* pInputVector = (const uint8_t* )pInputStream;
    uint8_t* pOutputVector = (uint8_t* )pOutputStream;

    float sx = 1.f / (ViewportWidth * 0.5f);
    float sy = 1.f / (-ViewportHeight * 0.5f);
    float sz = 1.f / (ViewportMaxZ - ViewportMinZ);

    float ox = (-ViewportX * sx) - 1.f;
    float oy = (-ViewportY * sy) + 1.f;
    float oz = (-ViewportMinZ * sz);

    size_t i = 0;
    size_t four = VectorCount >> 2;
    if ( four > 0 )
    {
        if ((InputStride == sizeof(XMFLOAT3)) && (OutputStride == sizeof(XMFLOAT3)))
        {
            for (size_t j = 0; j < four; ++j)
            {
                float32x4x3_t V = vld3q_f32( reinterpret_cast<const float*>(pInputVector) );
                pInputVector += sizeof(XMFLOAT3)*4;

                XMVECTOR ScaleX = vdupq_n_f32(sx);
                XMVECTOR OffsetX = vdupq_n_f32(ox);
                XMVECTOR VX = vmlaq_f32( OffsetX, ScaleX, V.val[0] );

                float32x2_t r3 = vget_low_f32( Transform.r[3] );
                float32x2_t r = vget_low_f32( Transform.r[0] );
                XMVECTOR vResult0 = vmlaq_lane_f32( vdupq_lane_f32( r3, 0 ), VX, r, 0 ); // Ax+M
                XMVECTOR vResult1 = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), VX, r, 1 ); // Bx+N

                __prefetch( pInputVector );

                r3 = vget_high_f32( Transform.r[3] );
                r = vget_high_f32( Transform.r[0] );
                XMVECTOR vResult2 = vmlaq_lane_f32( vdupq_lane_f32( r3, 0 ), VX, r, 0 ); // Cx+O
                XMVECTOR W = vmlaq_lane_f32( vdupq_lane_f32( r3, 1 ), VX, r, 1 ); // Dx+P

                __prefetch( pInputVector+XM_CACHE_LINE_SIZE );

                XMVECTOR ScaleY = vdupq_n_f32(sy);
                XMVECTOR OffsetY = vdupq_n_f32(oy);
                XMVECTOR VY = vmlaq_f32( OffsetY, ScaleY, V.val[1] );

                r = vget_low_f32( Transform.r[1] );
                vResult0 = vmlaq_lane_f32( vResult0, VY, r, 0 ); // Ax+Ey+M
                vResult1 = vmlaq_lane_f32( vResult1, VY, r, 1 ); // Bx+Fy+N

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*2) );

                r = vget_high_f32( Transform.r[1] );
                vResult2 = vmlaq_lane_f32( vResult2, VY, r, 0 ); // Cx+Gy+O
                W = vmlaq_lane_f32( W, VY, r, 1 ); // Dx+Hy+P

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*3) );

                XMVECTOR ScaleZ = vdupq_n_f32(sz);
                XMVECTOR OffsetZ = vdupq_n_f32(oz);
                XMVECTOR VZ = vmlaq_f32( OffsetZ, ScaleZ, V.val[2] );

                r = vget_low_f32( Transform.r[2] );
                vResult0 = vmlaq_lane_f32( vResult0, VZ, r, 0 ); // Ax+Ey+Iz+M
                vResult1 = vmlaq_lane_f32( vResult1, VZ, r, 1 ); // Bx+Fy+Jz+N

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*4) );

                r = vget_high_f32( Transform.r[2] );
                vResult2 = vmlaq_lane_f32( vResult2, VZ, r, 0 ); // Cx+Gy+Kz+O
                W = vmlaq_lane_f32( W, VZ, r, 1 ); // Dx+Hy+Lz+P

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*5) );

#if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
                V.val[0] = vdivq_f32( vResult0, W );
                V.val[1] = vdivq_f32( vResult1, W );
                V.val[2] = vdivq_f32( vResult2, W );
#else
                // 2 iterations of Newton-Raphson refinement of reciprocal
                float32x4_t Reciprocal = vrecpeq_f32(W);
                float32x4_t S = vrecpsq_f32( Reciprocal, W );
                Reciprocal = vmulq_f32( S, Reciprocal );
                S = vrecpsq_f32( Reciprocal, W );
                Reciprocal = vmulq_f32( S, Reciprocal );

                V.val[0] = vmulq_f32( vResult0, Reciprocal );
                V.val[1] = vmulq_f32( vResult1, Reciprocal );
                V.val[2] = vmulq_f32( vResult2, Reciprocal );
#endif

                vst3q_f32( reinterpret_cast<float*>(pOutputVector),V );
                pOutputVector += sizeof(XMFLOAT3)*4;

                i += 4;
            }
        }
    }

    if (i < VectorCount)
    {
        float32x2_t ScaleL = vcreate_f32(((uint64_t)*(const uint32_t* )&sx) | ((uint64_t)(*(const uint32_t* )&sy) << 32));
        float32x2_t ScaleH = vcreate_f32((uint64_t)*(const uint32_t* )&sz);

        float32x2_t OffsetL = vcreate_f32(((uint64_t)*(const uint32_t* )&ox) | ((uint64_t)(*(const uint32_t* )&oy) << 32));
        float32x2_t OffsetH = vcreate_f32((uint64_t)*(const uint32_t* )&oz);

        for (; i < VectorCount; i++)
        {
            float32x2_t VL = vld1_f32( reinterpret_cast<const float*>(pInputVector) );
            float32x2_t zero = vdup_n_f32(0);
            float32x2_t VH = vld1_lane_f32( reinterpret_cast<const float*>(pInputVector)+2, zero, 0 );
            pInputVector += InputStride;

            VL = vmla_f32( OffsetL, VL, ScaleL );
            VH = vmla_f32( OffsetH, VH, ScaleH );

            XMVECTOR vResult = vmlaq_lane_f32( Transform.r[3], Transform.r[0], VL, 0 ); // X
            vResult = vmlaq_lane_f32( vResult, Transform.r[1], VL, 1 ); // Y
            vResult = vmlaq_lane_f32( vResult, Transform.r[2], VH, 0 ); // Z

            VH = vget_high_f32(vResult);
            XMVECTOR W = vdupq_lane_f32( VH, 1 );

#if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
            vResult = vdivq_f32( vResult, W );
#else
            // 2 iterations of Newton-Raphson refinement of reciprocal for W
            float32x4_t Reciprocal = vrecpeq_f32( W );
            float32x4_t S = vrecpsq_f32( Reciprocal, W );
            Reciprocal = vmulq_f32( S, Reciprocal );
            S = vrecpsq_f32( Reciprocal, W );
            Reciprocal = vmulq_f32( S, Reciprocal );

            vResult = vmulq_f32( vResult, Reciprocal );
#endif

            VL = vget_low_f32( vResult );
            vst1_f32( reinterpret_cast<float*>(pOutputVector), VL );
            vst1q_lane_f32( reinterpret_cast<float*>(pOutputVector)+2, vResult, 2 );
            pOutputVector += OutputStride;
        }
    }

    return pOutputStream;
*)
end;


{***************************************************************************
 *
 * 4D vector operations
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------


function XMVector4Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
      uint32x4_t vResult = vceqq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( vget_lane_u32(vTemp.val[1], 1) == 0xFFFFFFFFU );
    *)
end;


function XMVector4EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    (*
      uint32x4_t vResult = vceqq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);

    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
    *)
end;


function XMVector4EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
    uint32x4_t vResult = vceqq_u32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( vget_lane_u32(vTemp.val[1], 1) == 0xFFFFFFFFU );
    *)
end;


function XMVector4EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
     (*
       uint32x4_t vResult = vceqq_u32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);

    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
     *)
end;


function XMVector4NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean;
begin
    (*
     float32x4_t vDelta = vsubq_f32( V1, V2 );
    uint32x4_t vResult = vacleq_f32( vDelta, Epsilon );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( vget_lane_u32(vTemp.val[1], 1) == 0xFFFFFFFFU );
    *)
end;


function XMVector4NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
     uint32x4_t vResult = vceqq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( vget_lane_u32(vTemp.val[1], 1) != 0xFFFFFFFFU );
    *)
end;



function XMVector4NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
      uint32x4_t vResult = vceqq_u32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( vget_lane_u32(vTemp.val[1], 1) != 0xFFFFFFFFU );
    *)
end;


function XMVector4Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
     uint32x4_t vResult = vcgtq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( vget_lane_u32(vTemp.val[1], 1) == 0xFFFFFFFFU );
    *)
end;


function XMVector4GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    (*
     uint32x4_t vResult = vcgtq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);

    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
    *)
end;



function XMVector4GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
    uint32x4_t vResult = vcgeq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( vget_lane_u32(vTemp.val[1], 1) == 0xFFFFFFFFU );
    *)
end;



function XMVector4GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    (*
     uint32x4_t vResult = vcgeq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);

    uint32_t CR = 0;
    if ( r == 0xFFFFFFFFU )
    {
        CR = XM_CRMASK_CR6TRUE;
    }
    else if ( !r )
    {
        CR = XM_CRMASK_CR6FALSE;
    }
    return CR;
    *)
end;



function XMVector4Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
     uint32x4_t vResult = vcltq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( vget_lane_u32(vTemp.val[1], 1) == 0xFFFFFFFFU );
    *)
end;

function XMVector4LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    (*
     uint32x4_t vResult = vcleq_f32( V1, V2 );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( vget_lane_u32(vTemp.val[1], 1) == 0xFFFFFFFFU );
    *)
end;



function XMVector4InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean;
begin
    (* ToDo
     // Test if less than or equal
    uint32x4_t ivTemp1 = vcleq_f32(V,Bounds);
    // Negate the bounds
    float32x4_t vTemp2 = vnegq_f32(Bounds);
    // Test if greater or equal (Reversed)
    uint32x4_t ivTemp2 = vcleq_f32(vTemp2,V);
    // Blend answers
    ivTemp1 = vandq_u32(ivTemp1,ivTemp2);
    // in bounds?
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(ivTemp1), vget_high_u8(ivTemp1));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( vget_lane_u32(vTemp.val[1], 1) == 0xFFFFFFFFU );
*)
end;




function XMVector4IsNaN(constref V: TXMVECTOR): boolean;
begin
    (* ToDo
   // Test against itself. NaN is always not equal
    uint32x4_t vTempNan = vceqq_f32( V, V );
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vTempNan), vget_high_u8(vTempNan));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    // If any are NaN, the mask is zero
    return ( vget_lane_u32(vTemp.val[1], 1) != 0xFFFFFFFFU );
*)
end;



function XMVector4IsInfinite(constref V: TXMVECTOR): boolean;
begin
    (* ToDo
    // Mask off the sign bit
    uint32x4_t vTempInf = vandq_u32( V, g_XMAbsMask );
    // Compare to infinity
    vTempInf = vceqq_f32(vTempInf, g_XMInfinity );
    // If any are infinity, the signs are true.
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vTempInf), vget_high_u8(vTempInf));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    return ( vget_lane_u32(vTemp.val[1], 1) != 0 );
*)
end;



function XMVector4Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     float32x4_t vTemp = vmulq_f32( V1, V2 );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vadd_f32( v1, v2 );
    v1 = vpadd_f32( v1, v1 );
    return vcombine_f32( v1, v1 );
*)
end;


function XMVector4Cross(constref V1: TXMVECTOR;constref  V2: TXMVECTOR;constref  V3: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    const float32x2_t select = vget_low_f32( g_XMMaskX );

    // Term1: V2zwyz * V3wzwy
    const float32x2_t v2xy = vget_low_f32(V2);
    const float32x2_t v2zw = vget_high_f32(V2);
    const float32x2_t v2yx = vrev64_f32(v2xy);
    const float32x2_t v2wz = vrev64_f32(v2zw);
    const float32x2_t v2yz = vbsl_f32( select, v2yx, v2wz );

    const float32x2_t v3zw = vget_high_f32(V3);
    const float32x2_t v3wz = vrev64_f32(v3zw);
    const float32x2_t v3xy = vget_low_f32(V3);
    const float32x2_t v3wy = vbsl_f32( select, v3wz, v3xy );

    float32x4_t vTemp1 = vcombine_f32(v2zw,v2yz);
    float32x4_t vTemp2 = vcombine_f32(v3wz,v3wy);
    XMVECTOR vResult = vmulq_f32( vTemp1, vTemp2 );

    // - V2wzwy * V3zwyz
    const float32x2_t v2wy = vbsl_f32( select, v2wz, v2xy );

    const float32x2_t v3yx = vrev64_f32(v3xy);
    const float32x2_t v3yz = vbsl_f32( select, v3yx, v3wz );

    vTemp1 = vcombine_f32(v2wz,v2wy);
    vTemp2 = vcombine_f32(v3zw,v3yz);
    vResult = vmlsq_f32( vResult, vTemp1, vTemp2 );

    // term1 * V1yxxx
    const float32x2_t v1xy = vget_low_f32(V1);
    const float32x2_t v1yx = vrev64_f32(v1xy);

    vTemp1 = vcombine_f32( v1yx, vdup_lane_f32( v1yx, 1 ) );
    vResult = vmulq_f32( vResult, vTemp1 );

    // Term2: V2ywxz * V3wxwx
    const float32x2_t v2yw = vrev64_f32(v2wy);
    const float32x2_t v2xz = vbsl_f32( select, v2xy, v2wz );

    const float32x2_t v3wx = vbsl_f32( select, v3wz, v3yx );

    vTemp1 = vcombine_f32(v2yw,v2xz);
    vTemp2 = vcombine_f32(v3wx,v3wx);
    float32x4_t vTerm = vmulq_f32( vTemp1, vTemp2 );

    // - V2wxwx * V3ywxz
    const float32x2_t v2wx = vbsl_f32( select, v2wz, v2yx );

    const float32x2_t v3yw = vrev64_f32(v3wy);
    const float32x2_t v3xz = vbsl_f32( select, v3xy, v3wz );

    vTemp1 = vcombine_f32(v2wx,v2wx);
    vTemp2 = vcombine_f32(v3yw,v3xz);
    vTerm = vmlsq_f32( vTerm, vTemp1, vTemp2 );

    // vResult - term2 * V1zzyy
    const float32x2_t v1zw = vget_high_f32(V1);

    vTemp1 = vcombine_f32( vdup_lane_f32(v1zw, 0), vdup_lane_f32(v1yx, 0) );
    vResult = vmlsq_f32( vResult, vTerm, vTemp1 );

    // Term3: V2yzxy * V3zxyx
    const float32x2_t v3zx = vrev64_f32(v3xz);

    vTemp1 = vcombine_f32(v2yz,v2xy);
    vTemp2 = vcombine_f32(v3zx,v3yx);
    vTerm = vmulq_f32( vTemp1, vTemp2 );

    // - V2zxyx * V3yzxy
    const float32x2_t v2zx = vrev64_f32(v2xz);

    vTemp1 = vcombine_f32(v2zx,v2yx);
    vTemp2 = vcombine_f32(v3yz,v3xy);
    vTerm = vmlsq_f32( vTerm, vTemp1, vTemp2 );

    // vResult + term3 * V1wwwz
    const float32x2_t v1wz = vrev64_f32(v1zw);

    vTemp1 = vcombine_f32( vdup_lane_f32( v1wz, 0 ), v1wz );
    return vmlaq_f32( vResult, vTerm, vTemp1 );
*)
end;



function XMVector4ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Dot4
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vadd_f32( v1, v2 );
    v1 = vpadd_f32( v1, v1 );
    // Reciprocal sqrt (estimate)
    v2 = vrsqrte_f32( v1 );
    return vcombine_f32(v2, v2);
*)
end;


function XMVector4ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Dot4
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vadd_f32( v1, v2 );
    v1 = vpadd_f32( v1, v1 );
    // Reciprocal sqrt
    float32x2_t  S0 = vrsqrte_f32(v1);
    float32x2_t  P0 = vmul_f32( v1, S0 );
    float32x2_t  R0 = vrsqrts_f32( P0, S0 );
    float32x2_t  S1 = vmul_f32( S0, R0 );
    float32x2_t  P1 = vmul_f32( v1, S1 );
    float32x2_t  R1 = vrsqrts_f32( P1, S1 );
    float32x2_t Result = vmul_f32( S1, R1 );
    return vcombine_f32( Result, Result );
*)
end;


function XMVector4LengthEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Dot4
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vadd_f32( v1, v2 );
    v1 = vpadd_f32( v1, v1 );
    const float32x2_t zero = vdup_n_f32(0);
    uint32x2_t VEqualsZero = vceq_f32( v1, zero );
    // Sqrt (estimate)
    float32x2_t Result = vrsqrte_f32( v1 );
    Result = vmul_f32( v1, Result );
    Result = vbsl_f32( VEqualsZero, zero, Result );
    return vcombine_f32( Result, Result );
*)
end;



function XMVector4Length(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    // Dot4
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vadd_f32( v1, v2 );
    v1 = vpadd_f32( v1, v1 );
    const float32x2_t zero = vdup_n_f32(0);
    uint32x2_t VEqualsZero = vceq_f32( v1, zero );
    // Sqrt
    float32x2_t S0 = vrsqrte_f32( v1 );
    float32x2_t P0 = vmul_f32( v1, S0 );
    float32x2_t R0 = vrsqrts_f32( P0, S0 );
    float32x2_t S1 = vmul_f32( S0, R0 );
    float32x2_t P1 = vmul_f32( v1, S1 );
    float32x2_t R1 = vrsqrts_f32( P1, S1 );
    float32x2_t Result = vmul_f32( S1, R1 );
    Result = vmul_f32( v1, Result );
    Result = vbsl_f32( VEqualsZero, zero, Result );
    return vcombine_f32( Result, Result );
*)
end;



function XMVector4NormalizeEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     // Dot4
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vadd_f32( v1, v2 );
    v1 = vpadd_f32( v1, v1 );
    // Reciprocal sqrt (estimate)
    v2 = vrsqrte_f32( v1 );
    // Normalize
    return vmulq_f32( V, vcombine_f32(v2,v2) );
*)
end;



function XMVector4Normalize(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     // Dot4
    float32x4_t vTemp = vmulq_f32( V, V );
    float32x2_t v1 = vget_low_f32( vTemp );
    float32x2_t v2 = vget_high_f32( vTemp );
    v1 = vadd_f32( v1, v2 );
    v1 = vpadd_f32( v1, v1 );
    uint32x2_t VEqualsZero = vceq_f32( v1, vdup_n_f32(0) );
    uint32x2_t VEqualsInf = vceq_f32( v1, vget_low_f32(g_XMInfinity) );
    // Reciprocal sqrt (2 iterations of Newton-Raphson)
    float32x2_t S0 = vrsqrte_f32( v1 );
    float32x2_t P0 = vmul_f32( v1, S0 );
    float32x2_t R0 = vrsqrts_f32( P0, S0 );
    float32x2_t S1 = vmul_f32( S0, R0 );
    float32x2_t P1 = vmul_f32( v1, S1 );
    float32x2_t R1 = vrsqrts_f32( P1, S1 );
    v2 = vmul_f32( S1, R1 );
    // Normalize
    XMVECTOR vResult = vmulq_f32( V, vcombine_f32(v2,v2) );
    vResult = vbslq_f32( vcombine_f32(VEqualsZero,VEqualsZero), vdupq_n_f32(0), vResult );
    return vbslq_f32( vcombine_f32(VEqualsInf,VEqualsInf), g_XMQNaN, vResult );
*)
end;


function XMVector4RefractV(Incident: TXMVECTOR; Normal: TXMVECTOR; RefractionIndex: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
    XMVECTOR IDotN = XMVector4Dot(Incident,Normal);

    // R = 1.0f - RefractionIndex * RefractionIndex * (1.0f - IDotN * IDotN)
    float32x4_t R = vmlsq_f32( g_XMOne, IDotN, IDotN);
    R = vmulq_f32(R, RefractionIndex);
    R = vmlsq_f32(g_XMOne, R, RefractionIndex );

    uint32x4_t vResult = vcleq_f32(R,g_XMZero);
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vResult), vget_high_u8(vResult));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    if ( vget_lane_u32(vTemp.val[1], 1) == 0xFFFFFFFFU )
    {
        // Total internal reflection
        vResult = g_XMZero;
    }
    else
    {
        // Sqrt(R)
        float32x4_t S0 = vrsqrteq_f32(R);
        float32x4_t P0 = vmulq_f32( R, S0 );
        float32x4_t R0 = vrsqrtsq_f32( P0, S0 );
        float32x4_t S1 = vmulq_f32( S0, R0 );
        float32x4_t P1 = vmulq_f32( R, S1 );
        float32x4_t R1 = vrsqrtsq_f32( P1, S1 );
        float32x4_t S2 = vmulq_f32( S1, R1 );
        R = vmulq_f32( R, S2 );
        // R = RefractionIndex * IDotN + sqrt(R)
        R = vmlaq_f32( R, RefractionIndex, IDotN );
        // Result = RefractionIndex * Incident - Normal * R
        vResult = vmulq_f32(RefractionIndex, Incident);
        vResult = vmlsq_f32( vResult, R, Normal );
    }
    return vResult;
*)
end;


function XMVector4Orthogonal(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo
     static const XMVECTORF32 Negate = { { { 1.f, 1.f, -1.f, -1.f } } };

    float32x4_t Result = vcombine_f32( vget_high_f32( V ), vget_low_f32( V ) );
    return vmulq_f32( Result, Negate );
*)
end;




function XMVector4Transform(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
begin
    (* ToDo
   float32x2_t VL = vget_low_f32( V );
    XMVECTOR vResult = vmulq_lane_f32( M.r[0], VL, 0 ); // X
    vResult = vmlaq_lane_f32( vResult, M.r[1], VL, 1 ); // Y
    float32x2_t VH = vget_high_f32( V );
    vResult = vmlaq_lane_f32( vResult, M.r[2], VH, 0  ); // Z
    return vmlaq_lane_f32( vResult, M.r[3], VH, 1 ); // W
*)
end;



function XMVector4TransformStream(out pOutputStream: PXMFLOAT4;constref  OutputStride: size_t; constref  pInputStream: PXMFLOAT4;
   constref  InputStride: size_t; constref VectorCount: size_t;constref  M: TXMMATRIX): PXMFLOAT4;
begin
    (* ToDo
     const uint8_t* pInputVector = (const uint8_t* )pInputStream;
    uint8_t* pOutputVector = (uint8_t* )pOutputStream;

    const XMVECTOR row0 = M.r[0];
    const XMVECTOR row1 = M.r[1];
    const XMVECTOR row2 = M.r[2];
    const XMVECTOR row3 = M.r[3];

    size_t i = 0;
    size_t four = VectorCount >> 2;
    if ( four > 0 )
    {
        if ((InputStride == sizeof(XMFLOAT4)) && (OutputStride == sizeof(XMFLOAT4)))
        {
            for (size_t j = 0; j < four; ++j)
            {
                float32x4x4_t V = vld4q_f32( reinterpret_cast<const float*>(pInputVector) );
                pInputVector += sizeof(XMFLOAT4)*4;

                float32x2_t r = vget_low_f32( row0 );
                XMVECTOR vResult0 = vmulq_lane_f32( V.val[0], r, 0 ); // Ax
                XMVECTOR vResult1 = vmulq_lane_f32( V.val[0], r, 1 ); // Bx

                __prefetch( pInputVector );

                r = vget_high_f32( row0 );
                XMVECTOR vResult2 = vmulq_lane_f32( V.val[0], r, 0 ); // Cx
                XMVECTOR vResult3 = vmulq_lane_f32( V.val[0], r, 1 ); // Dx

                __prefetch( pInputVector+XM_CACHE_LINE_SIZE );

                r = vget_low_f32( row1 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[1], r, 0 ); // Ax+Ey
                vResult1 = vmlaq_lane_f32( vResult1, V.val[1], r, 1 ); // Bx+Fy

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*2) );

                r = vget_high_f32( row1 );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[1], r, 0 ); // Cx+Gy
                vResult3 = vmlaq_lane_f32( vResult3, V.val[1], r, 1 ); // Dx+Hy

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*3) );

                r = vget_low_f32( row2 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[2], r, 0 ); // Ax+Ey+Iz
                vResult1 = vmlaq_lane_f32( vResult1, V.val[2], r, 1 ); // Bx+Fy+Jz

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*4) );

                r = vget_high_f32( row2 );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[2], r, 0 ); // Cx+Gy+Kz
                vResult3 = vmlaq_lane_f32( vResult3, V.val[2], r, 1 ); // Dx+Hy+Lz

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*5) );

                r = vget_low_f32( row3 );
                vResult0 = vmlaq_lane_f32( vResult0, V.val[3], r, 0 ); // Ax+Ey+Iz+Mw
                vResult1 = vmlaq_lane_f32( vResult1, V.val[3], r, 1 ); // Bx+Fy+Jz+Nw

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*6) );

                r = vget_high_f32( row3 );
                vResult2 = vmlaq_lane_f32( vResult2, V.val[3], r, 0 ); // Cx+Gy+Kz+Ow
                vResult3 = vmlaq_lane_f32( vResult3, V.val[3], r, 1 ); // Dx+Hy+Lz+Pw

                __prefetch( pInputVector+(XM_CACHE_LINE_SIZE*7) );

                V.val[0] = vResult0;
                V.val[1] = vResult1;
                V.val[2] = vResult2;
                V.val[3] = vResult3;

                vst4q_f32( reinterpret_cast<float*>(pOutputVector), V );
                pOutputVector += sizeof(XMFLOAT4)*4;

                i += 4;
            }
        }
    }

    for (; i < VectorCount; i++)
    {
        XMVECTOR V = vld1q_f32( reinterpret_cast<const float*>(pInputVector) );
        pInputVector += InputStride;

        float32x2_t VL = vget_low_f32( V );
        XMVECTOR vResult = vmulq_lane_f32( row0, VL, 0 ); // X
        vResult = vmlaq_lane_f32( vResult, row1, VL, 1 ); // Y
        float32x2_t VH = vget_high_f32( V );
        vResult = vmlaq_lane_f32( vResult, row2, VH, 0 ); // Z
        vResult = vmlaq_lane_f32( vResult, row3, VH, 1 ); // W

        vst1q_f32( reinterpret_cast<float*>(pOutputVector), vResult );
        pOutputVector += OutputStride;
    }

    return pOutputStream;
*)
end;

{***************************************************************************
 *
 * Matrix
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------


function XMMatrixIsNaN(constref M: TXMMATRIX): boolean;
begin
    (* ToDo
      // Load in registers
    XMVECTOR vX = M.r[0];
    XMVECTOR vY = M.r[1];
    XMVECTOR vZ = M.r[2];
    XMVECTOR vW = M.r[3];
    // Test themselves to check for NaN
    vX = vmvnq_u32(vceqq_f32(vX, vX));
    vY = vmvnq_u32(vceqq_f32(vY, vY));
    vZ = vmvnq_u32(vceqq_f32(vZ, vZ));
    vW = vmvnq_u32(vceqq_f32(vW, vW));
    // Or all the results
    vX = vorrq_u32(vX,vZ);
    vY = vorrq_u32(vY,vW);
    vX = vorrq_u32(vX,vY);
    // If any tested true, return true
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vX), vget_high_u8(vX));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);
    return (r != 0);
    *)
end;


function XMMatrixIsInfinite(M: TXMMATRIX): boolean;
begin
    (* ToDo
    // Mask off the sign bits
    XMVECTOR vTemp1 = vandq_u32(M.r[0],g_XMAbsMask);
    XMVECTOR vTemp2 = vandq_u32(M.r[1],g_XMAbsMask);
    XMVECTOR vTemp3 = vandq_u32(M.r[2],g_XMAbsMask);
    XMVECTOR vTemp4 = vandq_u32(M.r[3],g_XMAbsMask);
    // Compare to infinity
    vTemp1 = vceqq_f32(vTemp1,g_XMInfinity);
    vTemp2 = vceqq_f32(vTemp2,g_XMInfinity);
    vTemp3 = vceqq_f32(vTemp3,g_XMInfinity);
    vTemp4 = vceqq_f32(vTemp4,g_XMInfinity);
    // Or the answers together
    vTemp1 = vorrq_u32(vTemp1,vTemp2);
    vTemp3 = vorrq_u32(vTemp3,vTemp4);
    vTemp1 = vorrq_u32(vTemp1,vTemp3);
    // If any are infinity, the signs are true.
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vTemp1), vget_high_u8(vTemp1));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);
    return (r != 0);
    *)
end;


function XMMatrixIsIdentity(M: TXMMATRIX): boolean;
begin
    (* ToDo
    XMVECTOR vTemp1 = vceqq_f32(M.r[0],g_XMIdentityR0);
    XMVECTOR vTemp2 = vceqq_f32(M.r[1],g_XMIdentityR1);
    XMVECTOR vTemp3 = vceqq_f32(M.r[2],g_XMIdentityR2);
    XMVECTOR vTemp4 = vceqq_f32(M.r[3],g_XMIdentityR3);
    vTemp1 = vandq_u32(vTemp1,vTemp2);
    vTemp3 = vandq_u32(vTemp3,vTemp4);
    vTemp1 = vandq_u32(vTemp1,vTemp3);
    int8x8x2_t vTemp = vzip_u8(vget_low_u8(vTemp1), vget_high_u8(vTemp1));
    vTemp = vzip_u16(vTemp.val[0], vTemp.val[1]);
    uint32_t r = vget_lane_u32(vTemp.val[1], 1);
    return ( r == 0xFFFFFFFFU );
    *)
end;

//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------


function XMMatrixMultiply(M1: TXMMATRIX; M2: TXMMATRIX): TXMMATRIX;
begin
    (* ToDo
     XMMATRIX mResult;
    float32x2_t VL = vget_low_f32( M1.r[0] );
    float32x2_t VH = vget_high_f32( M1.r[0] );
    // Perform the operation on the first row
    XMVECTOR vX = vmulq_lane_f32(M2.r[0], VL, 0);
    XMVECTOR vY = vmulq_lane_f32(M2.r[1], VL, 1);
    XMVECTOR vZ = vmlaq_lane_f32(vX, M2.r[2], VH, 0);
    XMVECTOR vW = vmlaq_lane_f32(vY, M2.r[3], VH, 1);
    mResult.r[0] = vaddq_f32( vZ, vW );
    // Repeat for the other 3 rows
    VL = vget_low_f32( M1.r[1] );
    VH = vget_high_f32( M1.r[1] );
    vX = vmulq_lane_f32(M2.r[0], VL, 0);
    vY = vmulq_lane_f32(M2.r[1], VL, 1);
    vZ = vmlaq_lane_f32(vX, M2.r[2], VH, 0);
    vW = vmlaq_lane_f32(vY, M2.r[3], VH, 1);
    mResult.r[1] = vaddq_f32( vZ, vW );
    VL = vget_low_f32( M1.r[2] );
    VH = vget_high_f32( M1.r[2] );
    vX = vmulq_lane_f32(M2.r[0], VL, 0);
    vY = vmulq_lane_f32(M2.r[1], VL, 1);
    vZ = vmlaq_lane_f32(vX, M2.r[2], VH, 0);
    vW = vmlaq_lane_f32(vY, M2.r[3], VH, 1);
    mResult.r[2] = vaddq_f32( vZ, vW );
    VL = vget_low_f32( M1.r[3] );
    VH = vget_high_f32( M1.r[3] );
    vX = vmulq_lane_f32(M2.r[0], VL, 0);
    vY = vmulq_lane_f32(M2.r[1], VL, 1);
    vZ = vmlaq_lane_f32(vX, M2.r[2], VH, 0);
    vW = vmlaq_lane_f32(vY, M2.r[3], VH, 1);
    mResult.r[3] = vaddq_f32( vZ, vW );
    return mResult;
    *)
end;


function XMMatrixMultiplyTranspose(M1: TXMMATRIX; M2: TXMMATRIX): TXMMATRIX;
begin
    (* ToDo
      float32x2_t VL = vget_low_f32( M1.r[0] );
    float32x2_t VH = vget_high_f32( M1.r[0] );
    // Perform the operation on the first row
    XMVECTOR vX = vmulq_lane_f32(M2.r[0], VL, 0);
    XMVECTOR vY = vmulq_lane_f32(M2.r[1], VL, 1);
    XMVECTOR vZ = vmlaq_lane_f32(vX, M2.r[2], VH, 0);
    XMVECTOR vW = vmlaq_lane_f32(vY, M2.r[3], VH, 1);
    float32x4_t r0 = vaddq_f32( vZ, vW );
    // Repeat for the other 3 rows
    VL = vget_low_f32( M1.r[1] );
    VH = vget_high_f32( M1.r[1] );
    vX = vmulq_lane_f32(M2.r[0], VL, 0);
    vY = vmulq_lane_f32(M2.r[1], VL, 1);
    vZ = vmlaq_lane_f32(vX, M2.r[2], VH, 0);
    vW = vmlaq_lane_f32(vY, M2.r[3], VH, 1);
    float32x4_t r1 = vaddq_f32( vZ, vW );
    VL = vget_low_f32( M1.r[2] );
    VH = vget_high_f32( M1.r[2] );
    vX = vmulq_lane_f32(M2.r[0], VL, 0);
    vY = vmulq_lane_f32(M2.r[1], VL, 1);
    vZ = vmlaq_lane_f32(vX, M2.r[2], VH, 0);
    vW = vmlaq_lane_f32(vY, M2.r[3], VH, 1);
    float32x4_t r2 = vaddq_f32( vZ, vW );
    VL = vget_low_f32( M1.r[3] );
    VH = vget_high_f32( M1.r[3] );
    vX = vmulq_lane_f32(M2.r[0], VL, 0);
    vY = vmulq_lane_f32(M2.r[1], VL, 1);
    vZ = vmlaq_lane_f32(vX, M2.r[2], VH, 0);
    vW = vmlaq_lane_f32(vY, M2.r[3], VH, 1);
    float32x4_t r3 = vaddq_f32( vZ, vW );

    // Transpose result
    float32x4x2_t P0 = vzipq_f32( r0, r2 );
    float32x4x2_t P1 = vzipq_f32( r1, r3 );

    float32x4x2_t T0 = vzipq_f32( P0.val[0], P1.val[0] );
    float32x4x2_t T1 = vzipq_f32( P0.val[1], P1.val[1] );

    XMMATRIX mResult;
    mResult.r[0] = T0.val[0];
    mResult.r[1] = T0.val[1];
    mResult.r[2] = T1.val[0];
    mResult.r[3] = T1.val[1];
    return mResult;
    *)
end;



function XMMatrixTranspose(M: TXMMATRIX): TXMMATRIX;
begin
    (* ToDo
    float32x4x2_t P0 = vzipq_f32( M.r[0], M.r[2] );
    float32x4x2_t P1 = vzipq_f32( M.r[1], M.r[3] );

    float32x4x2_t T0 = vzipq_f32( P0.val[0], P1.val[0] );
    float32x4x2_t T1 = vzipq_f32( P0.val[1], P1.val[1] );

    XMMATRIX mResult;
    mResult.r[0] = T0.val[0];
    mResult.r[1] = T0.val[1];
    mResult.r[2] = T1.val[0];
    mResult.r[3] = T1.val[1];
    return mResult;
    *)
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




function XMMatrixScaling(constref ScaleX: single; constref ScaleY: single;constref  ScaleZ: single): TXMMATRIX;
begin
    (* ToDo
    const XMVECTOR Zero = vdupq_n_f32(0);
    XMMATRIX M;
    M.r[0] = vsetq_lane_f32( ScaleX, Zero, 0 );
    M.r[1] = vsetq_lane_f32( ScaleY, Zero, 1 );
    M.r[2] = vsetq_lane_f32( ScaleZ, Zero, 2 );
    M.r[3] = g_XMIdentityR3.v;
    return M;
    *)
end;


function XMMatrixScalingFromVector(constref Scale: TXMVECTOR): TXMMATRIX;
begin
    (* ToDo
    XMMATRIX M;
    M.r[0] = vandq_u32(Scale,g_XMMaskX);
    M.r[1] = vandq_u32(Scale,g_XMMaskY);
    M.r[2] = vandq_u32(Scale,g_XMMaskZ);
    M.r[3] = g_XMIdentityR3.v;
    return M;
    *)
end;



function XMMatrixRotationX(Angle: single): TXMMATRIX;
begin
    (* ToDo
    float    fSinAngle;
    float    fCosAngle;
    XMScalarSinCos(&fSinAngle, &fCosAngle, Angle);

    const XMVECTOR Zero = vdupq_n_f32(0);

    XMVECTOR T1 = vsetq_lane_f32( fCosAngle, Zero, 1 );
    T1 = vsetq_lane_f32( fSinAngle, T1, 2 );

    XMVECTOR T2 = vsetq_lane_f32( -fSinAngle, Zero, 1 );
    T2 = vsetq_lane_f32( fCosAngle, T2, 2 );

    XMMATRIX M;
    M.r[0] = g_XMIdentityR0.v;
    M.r[1] = T1;
    M.r[2] = T2;
    M.r[3] = g_XMIdentityR3.v;
    return M;
*)
end;



function XMMatrixRotationY(Angle: single): TXMMATRIX;
begin
    (* ToDo
    float    fSinAngle;
    float    fCosAngle;
    XMScalarSinCos(&fSinAngle, &fCosAngle, Angle);

    const XMVECTOR Zero = vdupq_n_f32(0);

    XMVECTOR T0 = vsetq_lane_f32( fCosAngle, Zero, 0 );
    T0 = vsetq_lane_f32( -fSinAngle, T0, 2 );

    XMVECTOR T2 = vsetq_lane_f32( fSinAngle, Zero, 0 );
    T2 = vsetq_lane_f32( fCosAngle, T2, 2 );

    XMMATRIX M;
    M.r[0] = T0;
    M.r[1] = g_XMIdentityR1.v;
    M.r[2] = T2;
    M.r[3] = g_XMIdentityR3.v;
    return M;
*)
end;



function XMMatrixRotationZ(Angle: single): TXMMATRIX;
begin
    (* ToDo
    float    fSinAngle;
    float    fCosAngle;
    XMScalarSinCos(&fSinAngle, &fCosAngle, Angle);

    const XMVECTOR Zero = vdupq_n_f32(0);

    XMVECTOR T0 = vsetq_lane_f32( fCosAngle, Zero, 0 );
    T0 = vsetq_lane_f32( fSinAngle, T0, 1 );

    XMVECTOR T1 = vsetq_lane_f32( -fSinAngle, Zero, 0 );
    T1 = vsetq_lane_f32( fCosAngle, T1, 1 );

    XMMATRIX M;
    M.r[0] = T0;
    M.r[1] = T1;
    M.r[2] = g_XMIdentityR2.v;
    M.r[3] = g_XMIdentityR3.v;
    return M;
*)
end;

function XMMatrixRotationNormal(constref NormalAxis: TXMVECTOR; constref Angle: single): TXMMATRIX;
var
    fSinAngle: single;
    fCosAngle: single;
    A, C0, C1, C2, N0, N1, V0, V1, V2, R0, R1, R2: TXMVECTOR;
begin

    XMScalarSinCos(fSinAngle, fCosAngle, Angle);

    A := XMVectorSet(fSinAngle, fCosAngle, 1.0 - fCosAngle, 0.0);

    C2 := XMVectorSplatZ(A);
    C1 := XMVectorSplatY(A);
    C0 := XMVectorSplatX(A);

    N0 := XMVectorSwizzle(NormalAxis, XM_SWIZZLE_Y, XM_SWIZZLE_Z, XM_SWIZZLE_X, XM_SWIZZLE_W);
    N1 := XMVectorSwizzle(NormalAxis, XM_SWIZZLE_Z, XM_SWIZZLE_X, XM_SWIZZLE_Y, XM_SWIZZLE_W);

    V0 := XMVectorMultiply(C2, N0);
    V0 := XMVectorMultiply(V0, N1);

    R0 := XMVectorMultiply(C2, NormalAxis);
    R0 := XMVectorMultiplyAdd(R0, NormalAxis, C1);

    R1 := XMVectorMultiplyAdd(C0, NormalAxis, V0);
    R2 := XMVectorNegativeMultiplySubtract(C0, NormalAxis, V0);

    V0 := XMVectorSelect(A, R0, g_XMSelect1110.v);
    V1 := XMVectorPermute(R1, R2, XM_PERMUTE_0Z, XM_PERMUTE_1Y, XM_PERMUTE_1Z, XM_PERMUTE_0X);
    V2 := XMVectorPermute(R1, R2, XM_PERMUTE_0Y, XM_PERMUTE_1X, XM_PERMUTE_0Y, XM_PERMUTE_1X);


    Result.r[0] := XMVectorPermute(V0, V1, XM_PERMUTE_0X, XM_PERMUTE_1X, XM_PERMUTE_1Y, XM_PERMUTE_0W);
    Result.r[1] := XMVectorPermute(V0, V1, XM_PERMUTE_1Z, XM_PERMUTE_0Y, XM_PERMUTE_1W, XM_PERMUTE_0W);
    Result.r[2] := XMVectorPermute(V0, V2, XM_PERMUTE_1X, XM_PERMUTE_1Y, XM_PERMUTE_0Z, XM_PERMUTE_0W);
    Result.r[3] := g_XMIdentityR3.v;
end;



function XMMatrixRotationQuaternion(Quaternion: TXMVECTOR): TXMMATRIX;
const
    Constant1110: TXMVECTOR = (f32: (1.0, 1.0, 1.0, 0.0));
var
    Q0, Q1, V0, V1, V2, R0, R1, R2: TXMVECTOR;
begin

    Q0 := XMVectorAdd(Quaternion, Quaternion);
    Q1 := XMVectorMultiply(Quaternion, Q0);

    V0 := XMVectorPermute(Q1, Constant1110, XM_PERMUTE_0Y, XM_PERMUTE_0X, XM_PERMUTE_0X, XM_PERMUTE_1W);
    V1 := XMVectorPermute(Q1, Constant1110, XM_PERMUTE_0Z, XM_PERMUTE_0Z, XM_PERMUTE_0Y, XM_PERMUTE_1W);
    R0 := XMVectorSubtract(Constant1110, V0);
    R0 := XMVectorSubtract(R0, V1);

    V0 := XMVectorSwizzle(Quaternion, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_Y, XM_SWIZZLE_W);
    V1 := XMVectorSwizzle(Q0, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Z, XM_SWIZZLE_W);
    V0 := XMVectorMultiply(V0, V1);

    V1 := XMVectorSplatW(Quaternion);
    V2 := XMVectorSwizzle(Q0, XM_SWIZZLE_Y, XM_SWIZZLE_Z, XM_SWIZZLE_X, XM_SWIZZLE_W);
    V1 := XMVectorMultiply(V1, V2);

    R1 := XMVectorAdd(V0, V1);
    R2 := XMVectorSubtract(V0, V1);

    V0 := XMVectorPermute(R1, R2, XM_PERMUTE_0Y, XM_PERMUTE_1X, XM_PERMUTE_1Y, XM_PERMUTE_0Z);
    V1 := XMVectorPermute(R1, R2, XM_PERMUTE_0X, XM_PERMUTE_1Z, XM_PERMUTE_0X, XM_PERMUTE_1Z);


    Result.r[0] := XMVectorPermute(R0, V0, XM_PERMUTE_0X, XM_PERMUTE_1X, XM_PERMUTE_1Y, XM_PERMUTE_0W);
    Result.r[1] := XMVectorPermute(R0, V0, XM_PERMUTE_1Z, XM_PERMUTE_0Y, XM_PERMUTE_1W, XM_PERMUTE_0W);
    Result.r[2] := XMVectorPermute(R0, V1, XM_PERMUTE_1X, XM_PERMUTE_1Y, XM_PERMUTE_0Z, XM_PERMUTE_0W);
    Result.r[3] := g_XMIdentityR3.v;
end;


function XMMatrixPerspectiveLH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
begin
    (* ToDo
    float TwoNearZ = NearZ + NearZ;
    float fRange = FarZ / (FarZ - NearZ);
    const XMVECTOR Zero = vdupq_n_f32(0);
    XMMATRIX M;
    M.r[0] = vsetq_lane_f32( TwoNearZ / ViewWidth, Zero, 0 );
    M.r[1] = vsetq_lane_f32( TwoNearZ / ViewHeight, Zero, 1 );
    M.r[2] = vsetq_lane_f32( fRange, g_XMIdentityR3.v, 2 );
    M.r[3] = vsetq_lane_f32( -fRange * NearZ, Zero, 2 );
    return M;
*)
end;



function XMMatrixPerspectiveRH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
begin
    (* ToDo
     float TwoNearZ = NearZ + NearZ;
    float fRange = FarZ / (NearZ - FarZ);
    const XMVECTOR Zero = vdupq_n_f32(0);

    XMMATRIX M;
    M.r[0] = vsetq_lane_f32( TwoNearZ / ViewWidth, Zero, 0 );
    M.r[1] = vsetq_lane_f32( TwoNearZ / ViewHeight, Zero, 1 );
    M.r[2] = vsetq_lane_f32( fRange, g_XMNegIdentityR3.v, 2 );
    M.r[3] = vsetq_lane_f32( fRange * NearZ, Zero, 2 );
    return M;
*)
end;


function XMMatrixPerspectiveFovLH(FovAngleY: single; AspectRatio: single; NearZ: single; FarZ: single): TXMMATRIX;
begin
    (* ToDo
    float    SinFov;
    float    CosFov;
    XMScalarSinCos(&SinFov, &CosFov, 0.5f * FovAngleY);

    float fRange = FarZ / (FarZ-NearZ);
    float Height = CosFov / SinFov;
    float Width = Height / AspectRatio;
    const XMVECTOR Zero = vdupq_n_f32(0);

    XMMATRIX M;
    M.r[0] = vsetq_lane_f32( Width, Zero, 0 );
    M.r[1] = vsetq_lane_f32( Height, Zero, 1 );
    M.r[2] = vsetq_lane_f32( fRange, g_XMIdentityR3.v, 2 );
    M.r[3] = vsetq_lane_f32( -fRange * NearZ, Zero, 2 );
    return M;
*)
end;



function XMMatrixPerspectiveFovRH(FovAngleY: single; AspectRatio: single; NearZ: single; FarZ: single): TXMMATRIX;
begin
    (* ToDo
     float    SinFov;
    float    CosFov;
    XMScalarSinCos(&SinFov, &CosFov, 0.5f * FovAngleY);
    float fRange = FarZ / (NearZ-FarZ);
    float Height = CosFov / SinFov;
    float Width = Height / AspectRatio;
    const XMVECTOR Zero = vdupq_n_f32(0);

    XMMATRIX M;
    M.r[0] = vsetq_lane_f32( Width, Zero, 0 );
    M.r[1] = vsetq_lane_f32( Height, Zero, 1 );
    M.r[2] = vsetq_lane_f32( fRange, g_XMNegIdentityR3.v, 2 );
    M.r[3] = vsetq_lane_f32( fRange * NearZ, Zero, 2 );
    return M;
*)
end;


function XMMatrixPerspectiveOffCenterLH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
begin
    (* ToDo
     float TwoNearZ = NearZ + NearZ;
    float ReciprocalWidth = 1.0f / (ViewRight - ViewLeft);
    float ReciprocalHeight = 1.0f / (ViewTop - ViewBottom);
    float fRange = FarZ / (FarZ-NearZ);
    const XMVECTOR Zero = vdupq_n_f32(0);

    XMMATRIX M;
    M.r[0] = vsetq_lane_f32( TwoNearZ * ReciprocalWidth, Zero, 0 );
    M.r[1] = vsetq_lane_f32( TwoNearZ * ReciprocalHeight, Zero, 1 );
    M.r[2] = XMVectorSet(-(ViewLeft + ViewRight) * ReciprocalWidth,
                         -(ViewTop + ViewBottom) * ReciprocalHeight,
                         fRange,
                         1.0f);
    M.r[3] = vsetq_lane_f32( -fRange * NearZ, Zero, 2 );
    return M;
*)
end;



function XMMatrixPerspectiveOffCenterRH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
begin
    (* ToDo
    float TwoNearZ = NearZ + NearZ;
    float ReciprocalWidth = 1.0f / (ViewRight - ViewLeft);
    float ReciprocalHeight = 1.0f / (ViewTop - ViewBottom);
    float fRange = FarZ / (NearZ-FarZ);
    const XMVECTOR Zero = vdupq_n_f32(0);

    XMMATRIX M;
    M.r[0] = vsetq_lane_f32( TwoNearZ * ReciprocalWidth, Zero, 0 );
    M.r[1] = vsetq_lane_f32( TwoNearZ * ReciprocalHeight, Zero, 1 );
    M.r[2] = XMVectorSet((ViewLeft + ViewRight) * ReciprocalWidth,
                         (ViewTop + ViewBottom) * ReciprocalHeight,
                         fRange,
                         -1.0f);
    M.r[3] = vsetq_lane_f32( fRange * NearZ, Zero, 2 );
    return M;
*)
end;



function XMMatrixOrthographicLH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
begin
    (* ToDo
     float fRange = 1.0f / (FarZ-NearZ);

    const XMVECTOR Zero = vdupq_n_f32(0);
    XMMATRIX M;
    M.r[0] = vsetq_lane_f32( 2.0f / ViewWidth, Zero, 0 );
    M.r[1] = vsetq_lane_f32( 2.0f / ViewHeight, Zero, 1 );
    M.r[2] = vsetq_lane_f32( fRange, Zero, 2 );
    M.r[3] = vsetq_lane_f32( -fRange * NearZ, g_XMIdentityR3.v, 2 );
    return M;
*)
end;



function XMMatrixOrthographicRH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
begin
    (* ToDo
     float fRange = 1.0f / (NearZ-FarZ);

    const XMVECTOR Zero = vdupq_n_f32(0);
    XMMATRIX M;
    M.r[0] = vsetq_lane_f32( 2.0f / ViewWidth, Zero, 0 );
    M.r[1] = vsetq_lane_f32( 2.0f / ViewHeight, Zero, 1 );
    M.r[2] = vsetq_lane_f32( fRange, Zero, 2 );
    M.r[3] = vsetq_lane_f32( fRange * NearZ, g_XMIdentityR3.v, 2 );
    return M;
*)
end;


function XMMatrixOrthographicOffCenterLH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
begin
    (* ToDo
    float ReciprocalWidth = 1.0f / (ViewRight - ViewLeft);
    float ReciprocalHeight = 1.0f / (ViewTop - ViewBottom);
    float fRange = 1.0f / (FarZ-NearZ);
    const XMVECTOR Zero = vdupq_n_f32(0);
    XMMATRIX M;
    M.r[0] = vsetq_lane_f32( ReciprocalWidth + ReciprocalWidth, Zero, 0 );
    M.r[1] = vsetq_lane_f32( ReciprocalHeight + ReciprocalHeight, Zero, 1 );
    M.r[2] = vsetq_lane_f32( fRange, Zero, 2 );
    M.r[3] = XMVectorSet(-(ViewLeft + ViewRight) * ReciprocalWidth,
                         -(ViewTop + ViewBottom) * ReciprocalHeight,
                         -fRange * NearZ,
                         1.0f);
    return M;
*)
end;


function XMMatrixOrthographicOffCenterRH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
begin
    (* ToDo
     float ReciprocalWidth = 1.0f / (ViewRight - ViewLeft);
    float ReciprocalHeight = 1.0f / (ViewTop - ViewBottom);
    float fRange = 1.0f / (NearZ-FarZ);
    const XMVECTOR Zero = vdupq_n_f32(0);
    XMMATRIX M;
    M.r[0] = vsetq_lane_f32( ReciprocalWidth + ReciprocalWidth, Zero, 0 );
    M.r[1] = vsetq_lane_f32( ReciprocalHeight + ReciprocalHeight, Zero, 1 );
    M.r[2] = vsetq_lane_f32( fRange, Zero, 2 );
    M.r[3] = XMVectorSet(-(ViewLeft + ViewRight) * ReciprocalWidth,
                         -(ViewTop + ViewBottom) * ReciprocalHeight,
                         fRange * NearZ,
                         1.0f);
    return M;
*)
end;


function XMQuaternionMultiply(Q1: TXMVECTOR; Q2: TXMVECTOR): TXMVECTOR;
const
    ControlWZYX: TXMVECTORF32 = (f: (1.0, -1.0, 1.0, -1.0));
    ControlZWXY: TXMVECTORF32 = (f: (1.0, 1.0, -1.0, -1.0));
    ControlYXWZ: TXMVECTORF32 = (f: (-1.0, 1.0, 1.0, -1.0));
begin
 (* ToDo

    float32x2_t Q2L := vget_low_f32(Q2);
    float32x2_t Q2H := vget_high_f32(Q2);

    float32x4_t Q2X := vdupq_lane_f32( Q2L, 0 );
    float32x4_t Q2Y := vdupq_lane_f32( Q2L, 1 );
    float32x4_t Q2Z := vdupq_lane_f32( Q2H, 0 );
    XMVECTOR vResult := vmulq_lane_f32(Q1, Q2H, 1);

    // Mul by Q1WZYX
    float32x4_t vTemp := vrev64q_f32(Q1);
    vTemp := vcombine_f32( vget_high_f32(vTemp), vget_low_f32(vTemp) );
    Q2X := vmulq_f32(Q2X,vTemp);
    vResult = vmlaq_f32( vResult, Q2X, ControlWZYX );

    // Mul by Q1ZWXY
    vTemp = vrev64q_u32(vTemp);
    Q2Y = vmulq_f32(Q2Y,vTemp);
    vResult = vmlaq_f32(vResult, Q2Y, ControlZWXY);

    // Mul by Q1YXWZ
    vTemp = vrev64q_u32(vTemp);
    vTemp = vcombine_f32(vget_high_f32(vTemp), vget_low_f32(vTemp));
    Q2Z = vmulq_f32(Q2Z,vTemp);
    vResult = vmlaq_f32(vResult, Q2Z, ControlYXWZ);
    return vResult;
    *)
end;



function XMQuaternionConjugate(Q: TXMVECTOR): TXMVECTOR;
const
    NegativeOne3: TXMVECTORF32 = (f: (-1.0, -1.0, -1.0, 1.0));
begin
    (* ToDo
    return vmulq_f32(Q, NegativeOne3.v );
    *)
end;


 
// Returns the interpolated quaternion. If Q0 and Q1 are not unit quaternions, the resulting interpolation is undefined.
// Result = Q0 * sin((1.0 - t) * Omega) / sin(Omega) + Q1 * sin(t * Omega) / sin(Omega)

function XMQuaternionSlerpV(Q0: TXMVECTOR; Q1: TXMVECTOR; T: TXMVECTOR): TXMVECTOR; inline;
const
    OneMinusEpsilon: TXMVECTORF32 = (f: (1.0 - 0.00001, 1.0 - 0.00001, 1.0 - 0.00001, 1.0 - 0.00001));
var
    CosOmega, Zero, Control, Sign, SinOmega, Omega, SignMask, V01, InvSinOmega, S0, S1: TXMVECTOR;
begin
    assert((XMVectorGetY(T) = XMVectorGetX(T)) and (XMVectorGetZ(T) = XMVectorGetX(T)) and (XMVectorGetW(T) = XMVectorGetX(T)));
    CosOmega := XMQuaternionDot(Q0, Q1);

    Zero := XMVectorZero();
    Control := XMVectorLess(CosOmega, Zero);
    Sign := XMVectorSelect(g_XMOne.v, g_XMNegativeOne.v, Control);

    CosOmega := XMVectorMultiply(CosOmega, Sign);

    Control := XMVectorLess(CosOmega, OneMinusEpsilon);

    SinOmega := XMVectorNegativeMultiplySubtract(CosOmega, CosOmega, g_XMOne.v);
    SinOmega := XMVectorSqrt(SinOmega);

    Omega := XMVectorATan2(SinOmega, CosOmega);

    SignMask := XMVectorSplatSignMask();
    V01 := XMVectorShiftLeft(T, Zero, 2);
    SignMask := XMVectorShiftLeft(SignMask, Zero, 3);
    V01 := XMVectorXorInt(V01, SignMask);
    V01 := XMVectorAdd(g_XMIdentityR0.v, V01);

    InvSinOmega := XMVectorReciprocal(SinOmega);

    S0 := XMVectorMultiply(V01, Omega);
    S0 := XMVectorSin(S0);
    S0 := XMVectorMultiply(S0, InvSinOmega);

    S0 := XMVectorSelect(V01, S0, Control);

    S1 := XMVectorSplatY(S0);
    S0 := XMVectorSplatX(S0);

    S1 := XMVectorMultiply(S1, Sign);

    Result := XMVectorMultiply(Q0, S0);
    Result := XMVectorMultiplyAdd(Q1, S1, Result);
end;


function XMQuaternionRotationNormal(NormalAxis: TXMVECTOR; Angle: single): TXMVECTOR; inline;
var
    N, Scale: TXMVECTOR;
    SinV, CosV: single;
begin
    N := XMVectorSelect(g_XMOne.v, NormalAxis, g_XMSelect1110.v);

    XMScalarSinCos(SinV, CosV, 0.5 * Angle);

    Scale := XMVectorSet(SinV, SinV, SinV, CosV);
    Result := XMVectorMultiply(N, Scale);
end;



function XMQuaternionRotationMatrix(M: TXMMATRIX): TXMVECTOR;
begin
    (* ToDo
    static const XMVECTORF32 XMPMMP     = { { { +1.0f, -1.0f, -1.0f, +1.0f } } };
    static const XMVECTORF32 XMMPMP     = { { { -1.0f, +1.0f, -1.0f, +1.0f } } };
    static const XMVECTORF32 XMMMPP     = { { { -1.0f, -1.0f, +1.0f, +1.0f } } };
    static const XMVECTORU32 Select0110 = { { { XM_SELECT_0, XM_SELECT_1, XM_SELECT_1, XM_SELECT_0 } } };
    static const XMVECTORU32 Select0010 = { { { XM_SELECT_0, XM_SELECT_0, XM_SELECT_1, XM_SELECT_0 } } };

    XMVECTOR r0 = M.r[0];
    XMVECTOR r1 = M.r[1];
    XMVECTOR r2 = M.r[2];

    XMVECTOR r00 = vdupq_lane_f32(vget_low_f32(r0), 0);
    XMVECTOR r11 = vdupq_lane_f32(vget_low_f32(r1), 1);
    XMVECTOR r22 = vdupq_lane_f32(vget_high_f32(r2), 0);

    // x^2 >= y^2 equivalent to r11 - r00 <= 0
    XMVECTOR r11mr00 = vsubq_f32(r11, r00);
    XMVECTOR x2gey2 = vcleq_f32(r11mr00, g_XMZero);

    // z^2 >= w^2 equivalent to r11 + r00 <= 0
    XMVECTOR r11pr00 = vaddq_f32(r11, r00);
    XMVECTOR z2gew2 = vcleq_f32(r11pr00, g_XMZero);

    // x^2 + y^2 >= z^2 + w^2 equivalent to r22 <= 0
    XMVECTOR x2py2gez2pw2 = vcleq_f32(r22, g_XMZero);

    // (4*x^2, 4*y^2, 4*z^2, 4*w^2)
    XMVECTOR t0 = vmulq_f32( XMPMMP, r00 );
    XMVECTOR x2y2z2w2 = vmlaq_f32( t0, XMMPMP, r11 );
    x2y2z2w2 = vmlaq_f32( x2y2z2w2, XMMMPP, r22 );
    x2y2z2w2 = vaddq_f32( x2y2z2w2, g_XMOne );

    // (r01, r02, r12, r11)
    t0 = vextq_f32(r0, r0, 1);
    XMVECTOR t1 = vextq_f32(r1, r1, 1);
    t0 = vcombine_f32( vget_low_f32(t0), vrev64_f32( vget_low_f32( t1 ) ) );

    // (r10, r20, r21, r10)
    t1 = vextq_f32(r2, r2, 3);
    XMVECTOR r10 = vdupq_lane_f32( vget_low_f32(r1), 0 );
    t1 = vbslq_f32( Select0110, t1, r10 );

    // (4*x*y, 4*x*z, 4*y*z, unused)
    XMVECTOR xyxzyz = vaddq_f32(t0, t1);

    // (r21, r20, r10, r10)
    t0 = vcombine_f32( vrev64_f32( vget_low_f32(r2) ), vget_low_f32(r10) );

    // (r12, r02, r01, r12)
    XMVECTOR t2 = vcombine_f32( vrev64_f32( vget_high_f32(r0) ), vrev64_f32( vget_low_f32(r0) ) );
    XMVECTOR t3 = vdupq_lane_f32( vget_high_f32(r1), 0 );
    t1 = vbslq_f32( Select0110, t2, t3 );

    // (4*x*w, 4*y*w, 4*z*w, unused)
    XMVECTOR xwywzw = vsubq_f32(t0, t1);
    xwywzw = vmulq_f32(XMMPMP, xwywzw);

    // (4*x*x, 4*x*y, 4*x*z, 4*x*w)
    t0 = vextq_f32( xyxzyz, xyxzyz, 3 );
    t1 = vbslq_f32( Select0110, t0, x2y2z2w2 );
    t2 = vdupq_lane_f32( vget_low_f32(xwywzw), 0 );
    XMVECTOR tensor0 = vbslq_f32( g_XMSelect1110, t1, t2 );

    // (4*y*x, 4*y*y, 4*y*z, 4*y*w)
    t0 = vbslq_f32( g_XMSelect1011, xyxzyz, x2y2z2w2 );
    t1 = vdupq_lane_f32( vget_low_f32(xwywzw), 1 );
    XMVECTOR tensor1 = vbslq_f32( g_XMSelect1110, t0, t1 );

    // (4*z*x, 4*z*y, 4*z*z, 4*z*w)
    t0 = vextq_f32(xyxzyz, xyxzyz, 1);
    t1 = vcombine_f32( vget_low_f32(t0), vrev64_f32( vget_high_f32(xwywzw) ) );
    XMVECTOR tensor2 = vbslq_f32( Select0010, x2y2z2w2, t1 );

    // (4*w*x, 4*w*y, 4*w*z, 4*w*w)
    XMVECTOR tensor3 = vbslq_f32( g_XMSelect1110, xwywzw, x2y2z2w2 );

    // Select the row of the tensor-product matrix that has the largest
    // magnitude.
    t0 = vbslq_f32( x2gey2, tensor0, tensor1 );
    t1 = vbslq_f32( z2gew2, tensor2, tensor3 );
    t2 = vbslq_f32( x2py2gez2pw2, t0, t1 );

    // Normalize the row.  No division by zero is possible because the
    // quaternion is unit-length (and the row is a nonzero multiple of
    // the quaternion).
    t0 = XMVector4Length(t2);
    return XMVectorDivide(t2, t0);
    *)
end;


// XMPlaneNormalizeEst uses a reciprocal estimate and
// returns QNaN on zero and infinite vectors.
function XMPlaneNormalizeEst(P: TXMVECTOR): TXMVECTOR; inline;
begin
    Result := XMVector3ReciprocalLengthEst(P);
    Result := XMVectorMultiply(P, Result);
end;


 function XMPlaneNormalize(P: TXMVECTOR): TXMVECTOR; inline;
var
    vLength: TXMVECTOR;
begin
    vLength := XMVector3ReciprocalLength(P);
    Result := XMVectorMultiply(P, vLength);
end;


//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------


function XMColorNegative(constref vColor: TXMVECTOR): TXMVECTOR; inline;
var
    vTemp: TXMVECTOR;
begin
    (* ToDo
     vTemp := veorq_u32(vColor,g_XMNegate3);
    result:= vaddq_f32(vTemp,g_XMOne3);
    *)
end;


function XMColorAdjustSaturation(constref vColor: TXMVECTOR; Saturation: single): TXMVECTOR; inline;
begin
    (* ToDo
    XMVECTOR vLuminance = XMVector3Dot( vColor, gvLuminance );
    XMVECTOR vResult = vsubq_f32(vColor, vLuminance);
    vResult = vmlaq_n_f32( vLuminance, vResult, fSaturation );
    return vbslq_f32( g_XMSelect1110, vResult, vColor );
    *)
end;


function XMColorAdjustContrast(constref vColor: TXMVECTOR; constref Contrast: single): TXMVECTOR; inline;
var
    vResult: TXMVECTOR;
begin
    (* ToDo
     vResult = vsubq_f32(vColor, g_XMOneHalf.v);
    vResult := vmlaq_n_f32( g_XMOneHalf.v, vResult, fContrast );
    result:= vbslq_f32( g_XMSelect1110, vResult, vColor );
    *)
end;

 
{****************************************************************************
 *
 * Miscellaneous
 *
 ****************************************************************************}

 
function XMFresnelTerm(CosIncidentAngle: TXMVECTOR; RefractionIndex: TXMVECTOR): TXMVECTOR;
var
    G, S, D, V0, V1, V2, V3: TXMVECTOR;
begin
    assert(not XMVector4IsInfinite(CosIncidentAngle));
    // Result = 0.5f * (g - c)^2 / (g + c)^2 * ((c * (g + c) - 1)^2 / (c * (g - c) + 1)^2 + 1) where
    // c = CosIncidentAngle
    // g = sqrt(c^2 + RefractionIndex^2 - 1)

    G := XMVectorMultiplyAdd(RefractionIndex, RefractionIndex, g_XMNegativeOne.v);
    G := XMVectorMultiplyAdd(CosIncidentAngle, CosIncidentAngle, G);
    G := XMVectorAbs(G);
    G := XMVectorSqrt(G);

    S := XMVectorAdd(G, CosIncidentAngle);
    D := XMVectorSubtract(G, CosIncidentAngle);

    V0 := XMVectorMultiply(D, D);
    V1 := XMVectorMultiply(S, S);
    V1 := XMVectorReciprocal(V1);
    V0 := XMVectorMultiply(g_XMOneHalf.v, V0);
    V0 := XMVectorMultiply(V0, V1);

    V2 := XMVectorMultiplyAdd(CosIncidentAngle, S, g_XMNegativeOne.v);
    V3 := XMVectorMultiplyAdd(CosIncidentAngle, D, g_XMOne.v);
    V2 := XMVectorMultiply(V2, V2);
    V3 := XMVectorMultiply(V3, V3);
    V3 := XMVectorReciprocal(V3);
    V2 := XMVectorMultiplyAdd(V2, V3, g_XMOne.v);

    Result := XMVectorMultiply(V0, V2);

    Result := XMVectorSaturate(Result);
end;








//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------

function XMVectorNegate(constref V: TXMVECTOR): TXMVECTOR;
begin
    (* ToDo_ARM
    return vnegq_f32(V);
*)
end;


class operator TXMMATRIX.Divide(M: TXMMATRIX; s: single): TXMMATRIX;
begin
    (* ToDo
   #if defined(_M_ARM64) || defined(_M_HYBRID_X86_ARM64)
    float32x4_t vS = vdupq_n_f32( S );
    r[0] = vdivq_f32( r[0], vS );
    r[1] = vdivq_f32( r[1], vS );
    r[2] = vdivq_f32( r[2], vS );
    r[3] = vdivq_f32( r[3], vS );
#else
    // 2 iterations of Newton-Raphson refinement of reciprocal
    float32x2_t vS = vdup_n_f32( S );
    float32x2_t R0 = vrecpe_f32( vS );
    float32x2_t S0 = vrecps_f32( R0, vS );
    R0 = vmul_f32( S0, R0 );
    S0 = vrecps_f32( R0, vS );
    R0 = vmul_f32( S0, R0 );
    float32x4_t Reciprocal = vcombine_u32(R0, R0);
    r[0] = vmulq_f32( r[0], Reciprocal );
    r[1] = vmulq_f32( r[1], Reciprocal );
    r[2] = vmulq_f32( r[2], Reciprocal );
    r[3] = vmulq_f32( r[3], Reciprocal );
#endif
    return *this;
*)
end;



// Return the inverse and the determinant of a 4x4 matrix
function XMMatrixInverse(out pDeterminant: TXMVECTOR; M: TXMMATRIX): TXMMATRIX;
var
    MT: TXMMATRIX;
    V0, V1: array [0..3] of TXMVECTOR;
    D0, D1, D2: TXMVECTOR;
    C0, C2, C4, C6: TXMVECTOR;
    C1, C3, C5, C7: TXMVECTOR;
    R: TXMMATRIX;
    Reciprocal: TXMVECTOR;
begin

    MT := XMMatrixTranspose(M);
    V0[0] := XMVectorSwizzle(MT.r[2], XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_Y, XM_SWIZZLE_Y);
    V1[0] := XMVectorSwizzle(MT.r[3], XM_SWIZZLE_Z, XM_SWIZZLE_W, XM_SWIZZLE_Z, XM_SWIZZLE_W);
    V0[1] := XMVectorSwizzle(MT.r[0], XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_Y, XM_SWIZZLE_Y);
    V1[1] := XMVectorSwizzle(MT.r[1], XM_SWIZZLE_Z, XM_SWIZZLE_W, XM_SWIZZLE_Z, XM_SWIZZLE_W);
    V0[2] := XMVectorPermute(MT.r[2], MT.r[0], XM_PERMUTE_0X, XM_PERMUTE_0Z, XM_PERMUTE_1X, XM_PERMUTE_1Z);
    V1[2] := XMVectorPermute(MT.r[3], MT.r[1], XM_PERMUTE_0Y, XM_PERMUTE_0W, XM_PERMUTE_1Y, XM_PERMUTE_1W);

    D0 := XMVectorMultiply(V0[0], V1[0]);
    D1 := XMVectorMultiply(V0[1], V1[1]);
    D2 := XMVectorMultiply(V0[2], V1[2]);

    V0[0] := XMVectorSwizzle(MT.r[2], XM_SWIZZLE_Z, XM_SWIZZLE_W, XM_SWIZZLE_Z, XM_SWIZZLE_W);
    V1[0] := XMVectorSwizzle(MT.r[3], XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_Y, XM_SWIZZLE_Y);
    V0[1] := XMVectorSwizzle(MT.r[0], XM_SWIZZLE_Z, XM_SWIZZLE_W, XM_SWIZZLE_Z, XM_SWIZZLE_W);
    V1[1] := XMVectorSwizzle(MT.r[1], XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_Y, XM_SWIZZLE_Y);
    V0[2] := XMVectorPermute(MT.r[2], MT.r[0], XM_PERMUTE_0Y, XM_PERMUTE_0W, XM_PERMUTE_1Y, XM_PERMUTE_1W);
    V1[2] := XMVectorPermute(MT.r[3], MT.r[1], XM_PERMUTE_0X, XM_PERMUTE_0Z, XM_PERMUTE_1X, XM_PERMUTE_1Z);

    D0 := XMVectorNegativeMultiplySubtract(V0[0], V1[0], D0);
    D1 := XMVectorNegativeMultiplySubtract(V0[1], V1[1], D1);
    D2 := XMVectorNegativeMultiplySubtract(V0[2], V1[2], D2);

    V0[0] := XMVectorSwizzle(MT.r[1], XM_SWIZZLE_Y, XM_SWIZZLE_Z, XM_SWIZZLE_X, XM_SWIZZLE_Y);
    V1[0] := XMVectorPermute(D0, D2, XM_PERMUTE_1Y, XM_PERMUTE_0Y, XM_PERMUTE_0W, XM_PERMUTE_0X);
    V0[1] := XMVectorSwizzle(MT.r[0], XM_SWIZZLE_Z, XM_SWIZZLE_X, XM_SWIZZLE_Y, XM_SWIZZLE_X);
    V1[1] := XMVectorPermute(D0, D2, XM_PERMUTE_0W, XM_PERMUTE_1Y, XM_PERMUTE_0Y, XM_PERMUTE_0Z);
    V0[2] := XMVectorSwizzle(MT.r[3], XM_SWIZZLE_Y, XM_SWIZZLE_Z, XM_SWIZZLE_X, XM_SWIZZLE_Y);
    V1[2] := XMVectorPermute(D1, D2, XM_PERMUTE_1W, XM_PERMUTE_0Y, XM_PERMUTE_0W, XM_PERMUTE_0X);
    V0[3] := XMVectorSwizzle(MT.r[2], XM_SWIZZLE_Z, XM_SWIZZLE_X, XM_SWIZZLE_Y, XM_SWIZZLE_X);
    V1[3] := XMVectorPermute(D1, D2, XM_PERMUTE_0W, XM_PERMUTE_1W, XM_PERMUTE_0Y, XM_PERMUTE_0Z);


    C0 := XMVectorMultiply(V0[0], V1[0]);
    C2 := XMVectorMultiply(V0[1], V1[1]);
    C4 := XMVectorMultiply(V0[2], V1[2]);
    C6 := XMVectorMultiply(V0[3], V1[3]);

    V0[0] := XMVectorSwizzle(MT.r[1], XM_SWIZZLE_Z, XM_SWIZZLE_W, XM_SWIZZLE_Y, XM_SWIZZLE_Z);
    V1[0] := XMVectorPermute(D0, D2, XM_PERMUTE_0W, XM_PERMUTE_0X, XM_PERMUTE_0Y, XM_PERMUTE_1X);
    V0[1] := XMVectorSwizzle(MT.r[0], XM_SWIZZLE_W, XM_SWIZZLE_Z, XM_SWIZZLE_W, XM_SWIZZLE_Y);
    V1[1] := XMVectorPermute(D0, D2, XM_PERMUTE_0Z, XM_PERMUTE_0Y, XM_PERMUTE_1X, XM_PERMUTE_0X);
    V0[2] := XMVectorSwizzle(MT.r[3], XM_SWIZZLE_Z, XM_SWIZZLE_W, XM_SWIZZLE_Y, XM_SWIZZLE_Z);
    V1[2] := XMVectorPermute(D1, D2, XM_PERMUTE_0W, XM_PERMUTE_0X, XM_PERMUTE_0Y, XM_PERMUTE_1Z);
    V0[3] := XMVectorSwizzle(MT.r[2], XM_SWIZZLE_W, XM_SWIZZLE_Z, XM_SWIZZLE_W, XM_SWIZZLE_Y);
    V1[3] := XMVectorPermute(D1, D2, XM_PERMUTE_0Z, XM_PERMUTE_0Y, XM_PERMUTE_1Z, XM_PERMUTE_0X);

    C0 := XMVectorNegativeMultiplySubtract(V0[0], V1[0], C0);
    C2 := XMVectorNegativeMultiplySubtract(V0[1], V1[1], C2);
    C4 := XMVectorNegativeMultiplySubtract(V0[2], V1[2], C4);
    C6 := XMVectorNegativeMultiplySubtract(V0[3], V1[3], C6);

    V0[0] := XMVectorSwizzle(MT.r[1], XM_SWIZZLE_W, XM_SWIZZLE_X, XM_SWIZZLE_W, XM_SWIZZLE_X);
    V1[0] := XMVectorPermute(D0, D2, XM_PERMUTE_0Z, XM_PERMUTE_1Y, XM_PERMUTE_1X, XM_PERMUTE_0Z);
    V0[1] := XMVectorSwizzle(MT.r[0], XM_SWIZZLE_Y, XM_SWIZZLE_W, XM_SWIZZLE_X, XM_SWIZZLE_Z);
    V1[1] := XMVectorPermute(D0, D2, XM_PERMUTE_1Y, XM_PERMUTE_0X, XM_PERMUTE_0W, XM_PERMUTE_1X);
    V0[2] := XMVectorSwizzle(MT.r[3], XM_SWIZZLE_W, XM_SWIZZLE_X, XM_SWIZZLE_W, XM_SWIZZLE_X);
    V1[2] := XMVectorPermute(D1, D2, XM_PERMUTE_0Z, XM_PERMUTE_1W, XM_PERMUTE_1Z, XM_PERMUTE_0Z);
    V0[3] := XMVectorSwizzle(MT.r[2], XM_SWIZZLE_Y, XM_SWIZZLE_W, XM_SWIZZLE_X, XM_SWIZZLE_Z);
    V1[3] := XMVectorPermute(D1, D2, XM_PERMUTE_1W, XM_PERMUTE_0X, XM_PERMUTE_0W, XM_PERMUTE_1Z);

    C1 := XMVectorNegativeMultiplySubtract(V0[0], V1[0], C0);
    C0 := XMVectorMultiplyAdd(V0[0], V1[0], C0);
    C3 := XMVectorMultiplyAdd(V0[1], V1[1], C2);
    C2 := XMVectorNegativeMultiplySubtract(V0[1], V1[1], C2);
    C5 := XMVectorNegativeMultiplySubtract(V0[2], V1[2], C4);
    C4 := XMVectorMultiplyAdd(V0[2], V1[2], C4);
    C7 := XMVectorMultiplyAdd(V0[3], V1[3], C6);
    C6 := XMVectorNegativeMultiplySubtract(V0[3], V1[3], C6);


    R.r[0] := XMVectorSelect(C0, C1, g_XMSelect0101.v);
    R.r[1] := XMVectorSelect(C2, C3, g_XMSelect0101.v);
    R.r[2] := XMVectorSelect(C4, C5, g_XMSelect0101.v);
    R.r[3] := XMVectorSelect(C6, C7, g_XMSelect0101.v);

    pDeterminant := XMVector4Dot(R.r[0], MT.r[0]);

    Reciprocal := XMVectorReciprocal(pDeterminant);

    Result.r[0] := XMVectorMultiply(R.r[0], Reciprocal);
    Result.r[1] := XMVectorMultiply(R.r[1], Reciprocal);
    Result.r[2] := XMVectorMultiply(R.r[2], Reciprocal);
    Result.r[3] := XMVectorMultiply(R.r[3], Reciprocal);

end;

function XMMatrixSet(m00: single; m01: single; m02: single; m03: single; m10: single; m11: single; m12: single; m13: single;
    m20: single; m21: single; m22: single; m23: single; m30: single; m31: single; m32: single; m33: single): TXMMATRIX;
begin
    Result.r[0] := XMVectorSet(m00, m01, m02, m03);
    Result.r[1] := XMVectorSet(m10, m11, m12, m13);
    Result.r[2] := XMVectorSet(m20, m21, m22, m23);
    Result.r[3] := XMVectorSet(m30, m31, m32, m33);
end;


function XMVerifyCPUSupport(CPURequired: TCPUType): boolean;
begin
    Result:=TRUE;
end;
