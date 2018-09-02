{// For formating only
unit Text;

interface

uses Classes;

implementation  }

{****************************************************************************
 *
 * Data conversion
 *
 ****************************************************************************}

// Returns the converted vector, where each component has been divided by two raised to the DivExponent power.
function XMConvertVectorIntToFloat(constref VInt: TXMVECTOR; constref DivExponent: UINT32): TXMVECTOR;
var
    fScale: single;
    ElementIndex: UINT32;
begin
    fScale := 1.0 / (1 shl DivExponent);
    ElementIndex := 0;
    while ElementIndex < 4 do
    begin
        Result.f32[ElementIndex] := VInt.u32[ElementIndex] * fScale;
        Inc(ElementIndex);
    end;
end;

//Returns the converted vector, where each component has been multiplied by two raised to the MulExponent power.

function XMConvertVectorFloatToInt(constref VFloat: TXMVECTOR; constref MulExponent: UINT32): TXMVECTOR;
var
    fScale, fTemp: single;
    ElementIndex: uint32 = 0;
    iResult: int32;
begin
    // Get the scalar factor.
    fScale := (1 shl MulExponent);

    while ElementIndex < 4 do
    begin
        fTemp := VFloat.f32[ElementIndex] * fScale;
        if (fTemp <= -(65536.0 * 32768.0)) then
            iResult := (-$7FFFFFFF) - 1
        else if (fTemp > (65536.0 * 32768.0) - 128.0) then
            iResult := $7FFFFFFF
        else
            iResult := Trunc(fTemp);
        Result.u32[ElementIndex] := iResult;
        Inc(ElementIndex);
    end;
end;



function XMConvertVectorUIntToFloat(constref VUInt: TXMVECTOR; constref DivExponent: UINT32): TXMVECTOR;
var
    fScale: single;
    ElementIndex: uint32 = 0;
begin
    assert(DivExponent < 32);
    fScale := 1.0 / (1 shl DivExponent);
    while ElementIndex < 4 do
    begin
        Result.f32[ElementIndex] := VUInt.u32[ElementIndex] * fScale;
        Inc(ElementIndex);
    end;
end;



function XMConvertVectorFloatToUInt(constref VFloat: TXMVECTOR; constref MulExponent: UINT32): TXMVECTOR;
var
    fScale, fTemp: single;
    ElementIndex, uResult: uint32;
begin
    // Get the scalar factor.
    fScale := (1 shl MulExponent);
    ElementIndex := 0;
    while ElementIndex < 4 do
    begin
        fTemp := VFloat.f32[ElementIndex] * fScale;
        if (fTemp <= 0.0) then
            uResult := 0
        else if (fTemp >= (65536.0 * 65536.0)) then
            uResult := $FFFFFFFF
        else
            uResult := trunc(fTemp);
        Result.u32[ElementIndex] := uResult;
    end;
end;



function XMVectorSetBinaryConstant(const C0, C1, C2, c3: UINT32): TXMVECTOR;
begin
    Result.u32[0] := (0 - (C0 and 1)) and $3F800000;
    Result.u32[1] := (0 - (C1 and 1)) and $3F800000;
    Result.u32[2] := (0 - (C2 and 1)) and $3F800000;
    Result.u32[3] := (0 - (C3 and 1)) and $3F800000;
end;



function XMVectorSplatConstant(const IntConstant: INT32; const DivExponent: UINT32): TXMVECTOR;
var
    V: TXMVECTOR;
begin
    assert((IntConstant >= -16) and (IntConstant <= 15));
    assert(DivExponent < 32);

    V.i32[0] := IntConstant;
    V.i32[1] := IntConstant;
    V.i32[2] := IntConstant;
    V.i32[3] := IntConstant;
    Result := XMConvertVectorIntToFloat(V, DivExponent);
end;



function XMVectorSplatConstantInt(const IntConstant: INT32): TXMVECTOR;
begin
    assert((IntConstant >= -16) and (IntConstant <= 15));

    Result.f32[0] := IntConstant;
    Result.f32[1] := IntConstant;
    Result.f32[2] := IntConstant;
    Result.f32[3] := IntConstant;
end;


{****************************************************************************
 *
 * Vector and matrix load operations
 *
 ****************************************************************************}

function XMLoadInt(constref pSource: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := pSource[0];
    Result.u32[1] := 0;
    Result.u32[2] := 0;
    Result.u32[3] := 0;
end;



function XMLoadFloat(constref pSource: PSingle): TXMVECTOR;
begin
    Result.f32[0] := pSource[0];
    Result.f32[1] := 0.0;
    Result.f32[2] := 0.0;
    Result.f32[3] := 0.0;
end;



function XMLoadInt2(const pSource: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := pSource[0];
    Result.u32[1] := pSource[1];
    Result.u32[2] := 0;
    Result.u32[3] := 0;
end;



function XMLoadInt2A(const PSource: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := pSource[0];
    Result.u32[1] := pSource[1];
    Result.u32[2] := 0;
    Result.u32[3] := 0;
end;



function XMLoadFloat2(const pSource: TXMFLOAT2): TXMVECTOR;
begin
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := 0.0;
    Result.f32[3] := 0.0;
end;



function XMLoadFloat2A(const pSource: TXMFLOAT2A): TXMVECTOR;
begin
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := 0.0;
    Result.f32[3] := 0.0;
end;



function XMLoadSInt2(const pSource: TXMINT2): TXMVECTOR;
begin
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := 0.0;
    Result.f32[3] := 0.0;
end;



function XMLoadUInt2(const pSource: TXMUINT2): TXMVECTOR;
begin
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := 0.0;
    Result.f32[3] := 0.0;
end;



function XMLoadInt3(const pSource: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := pSource[0];
    Result.u32[1] := pSource[1];
    Result.u32[2] := pSource[2];
    Result.u32[3] := 0;
end;



function XMLoadInt3A(const pSource: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := pSource[0];
    Result.u32[1] := pSource[1];
    Result.u32[2] := pSource[2];
    Result.u32[3] := 0;
end;



function XMLoadFloat3(constref pSource: TXMFLOAT3): TXMVECTOR;
begin
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := pSource.z;
    Result.f32[3] := 0;
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
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := pSource.z;
    Result.f32[3] := 0.0;
end;



function XMLoadSInt3(const pSource: TXMINT3): TXMVECTOR;
begin
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := pSource.z;
    Result.f32[3] := 0.0;
end;



function XMLoadUInt3(const pSource: TXMUINT3): TXMVECTOR;
begin
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := pSource.z;
    Result.f32[3] := 0.0;
end;



function XMLoadInt4(const pSource: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := pSource[0];
    Result.u32[1] := pSource[1];
    Result.u32[2] := pSource[2];
    Result.u32[3] := pSource[3];
end;



function XMLoadInt4A(const pSource: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := pSource[0];
    Result.u32[1] := pSource[1];
    Result.u32[2] := pSource[2];
    Result.u32[3] := pSource[3];
end;



function XMLoadFloat4(constref pSource: TXMFLOAT4): TXMVECTOR;
begin
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := pSource.z;
    Result.f32[3] := pSource.w;
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
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := pSource.z;
    Result.f32[3] := pSource.w;
end;



function XMLoadSInt4(const pSource: TXMINT4): TXMVECTOR;
begin
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := pSource.z;
    Result.f32[3] := pSource.w;
end;



function XMLoadUInt4(const pSource: TXMUINT4): TXMVECTOR;
begin
    Result.f32[0] := pSource.x;
    Result.f32[1] := pSource.y;
    Result.f32[2] := pSource.z;
    Result.f32[3] := pSource.w;
end;



function XMLoadFloat3x3(const pSource: TXMFLOAT3X3): TXMMATRIX;
begin
    Result.r[0].f32[0] := pSource.m[0, 0];
    Result.r[0].f32[1] := pSource.m[0, 1];
    Result.r[0].f32[2] := pSource.m[0, 2];
    Result.r[0].f32[3] := 0.0;

    Result.r[1].f32[0] := pSource.m[1, 0];
    Result.r[1].f32[1] := pSource.m[1, 1];
    Result.r[1].f32[2] := pSource.m[1, 2];
    Result.r[1].f32[3] := 0.0;

    Result.r[2].f32[0] := pSource.m[2, 0];
    Result.r[2].f32[1] := pSource.m[2, 1];
    Result.r[2].f32[2] := pSource.m[2, 2];
    Result.r[2].f32[3] := 0.0;
    Result.r[3].f32[0] := 0.0;
    Result.r[3].f32[1] := 0.0;
    Result.r[3].f32[2] := 0.0;
    Result.r[3].f32[3] := 1.0;
end;



function XMLoadFloat4x3(const pSource: TXMFLOAT4X3): TXMMATRIX;
begin
    Result.r[0].f32[0] := pSource.m[0, 0];
    Result.r[0].f32[1] := pSource.m[0, 1];
    Result.r[0].f32[2] := pSource.m[0, 2];
    Result.r[0].f32[3] := 0.0;

    Result.r[1].f32[0] := pSource.m[1, 0];
    Result.r[1].f32[1] := pSource.m[1, 1];
    Result.r[1].f32[2] := pSource.m[1, 2];
    Result.r[1].f32[3] := 0.0;

    Result.r[2].f32[0] := pSource.m[2, 0];
    Result.r[2].f32[1] := pSource.m[2, 1];
    Result.r[2].f32[2] := pSource.m[2, 2];
    Result.r[2].f32[3] := 0.0;

    Result.r[3].f32[0] := pSource.m[3, 0];
    Result.r[3].f32[1] := pSource.m[3, 1];
    Result.r[3].f32[2] := pSource.m[3, 2];
    Result.r[3].f32[3] := 1.0;
end;



function XMLoadFloat4x3A(const pSource: TXMFLOAT4X3A): TXMMATRIX;
begin
    Result.r[0].f32[0] := pSource.m[0, 0];
    Result.r[0].f32[1] := pSource.m[0, 1];
    Result.r[0].f32[2] := pSource.m[0, 2];
    Result.r[0].f32[3] := 0.0;

    Result.r[1].f32[0] := pSource.m[1, 0];
    Result.r[1].f32[1] := pSource.m[1, 1];
    Result.r[1].f32[2] := pSource.m[1, 2];
    Result.r[1].f32[3] := 0.0;

    Result.r[2].f32[0] := pSource.m[2, 0];
    Result.r[2].f32[1] := pSource.m[2, 1];
    Result.r[2].f32[2] := pSource.m[2, 2];
    Result.r[2].f32[3] := 0.0;

    Result.r[3].f32[0] := pSource.m[3, 0];
    Result.r[3].f32[1] := pSource.m[3, 1];
    Result.r[3].f32[2] := pSource.m[3, 2];
    Result.r[3].f32[3] := 1.0;
end;



function XMLoadFloat4x4(const pSource: TXMFLOAT4X4): TXMMATRIX;
begin
    Result.r[0].f32[0] := pSource.m[0, 0];
    Result.r[0].f32[1] := pSource.m[0, 1];
    Result.r[0].f32[2] := pSource.m[0, 2];
    Result.r[0].f32[3] := pSource.m[0, 3];

    Result.r[1].f32[0] := pSource.m[1, 0];
    Result.r[1].f32[1] := pSource.m[1, 1];
    Result.r[1].f32[2] := pSource.m[1, 2];
    Result.r[1].f32[3] := pSource.m[1, 3];

    Result.r[2].f32[0] := pSource.m[2, 0];
    Result.r[2].f32[1] := pSource.m[2, 1];
    Result.r[2].f32[2] := pSource.m[2, 2];
    Result.r[2].f32[3] := pSource.m[2, 3];

    Result.r[3].f32[0] := pSource.m[3, 0];
    Result.r[3].f32[1] := pSource.m[3, 1];
    Result.r[3].f32[2] := pSource.m[3, 2];
    Result.r[3].f32[3] := pSource.m[3, 3];
end;



function XMLoadFloat4x4A(const pSource: TXMFLOAT4X4A): TXMMATRIX;
begin
    Result.r[0].f32[0] := pSource.m[0, 0];
    Result.r[0].f32[1] := pSource.m[0, 1];
    Result.r[0].f32[2] := pSource.m[0, 2];
    Result.r[0].f32[3] := pSource.m[0, 3];

    Result.r[1].f32[0] := pSource.m[1, 0];
    Result.r[1].f32[1] := pSource.m[1, 1];
    Result.r[1].f32[2] := pSource.m[1, 2];
    Result.r[1].f32[3] := pSource.m[1, 3];

    Result.r[2].f32[0] := pSource.m[2, 0];
    Result.r[2].f32[1] := pSource.m[2, 1];
    Result.r[2].f32[2] := pSource.m[2, 2];
    Result.r[2].f32[3] := pSource.m[2, 3];

    Result.r[3].f32[0] := pSource.m[3, 0];
    Result.r[3].f32[1] := pSource.m[3, 1];
    Result.r[3].f32[2] := pSource.m[3, 2];
    Result.r[3].f32[3] := pSource.m[3, 3];
end;


{****************************************************************************
 *
 * Vector and matrix store operations
 *
 ****************************************************************************}

procedure XMStoreInt(var pDestination: UINT32; constref V: TXMVECTOR);
begin
    pDestination := XMVectorGetIntX(V);
end;



procedure XMStoreFloat(var pDestination: single; V: TXMVECTOR);
begin
    pDestination := XMVectorGetX(V);
end;



procedure XMStoreInt2(var pDestination: array of uint32; constref V: TXMVECTOR);
begin
    pDestination[0] := V.u32[0];
    pDestination[1] := V.u32[1];
end;



procedure XMStoreInt2A(var pDestination: TUINT32A_Array2; constref V: TXMVECTOR);
begin
    pDestination[0] := V.u32[0];
    pDestination[1] := V.u32[1];
end;



procedure XMStoreFloat2(var pDestination: TXMFLOAT2; constref V: TXMVECTOR);
begin
    pDestination.x := V.f32[0];
    pDestination.y := V.f32[1];
end;



procedure XMStoreFloat2A(var pDestination: TXMFLOAT2A; constref V: TXMVECTOR);
begin
    pDestination.x := v.f32[0];
    pDestination.y := V.f32[1];
end;



procedure XMStoreSInt2(var pDestination: TXMINT2; constref V: TXMVECTOR);
begin
    pDestination.x := trunc(V.f32[0]);
    pDestination.y := trunc(V.f32[1]);
end;



procedure XMStoreUInt2(var pDestination: TXMUINT2; constref V: TXMVECTOR);
begin
    pDestination.x := trunc(V.f32[0]);
    pDestination.y := trunc(V.f32[1]);
end;



procedure XMStoreInt3(var pDestination: array of UINT32; constref V: TXMVECTOR);
begin
    pDestination[0] := V.u32[0];
    pDestination[1] := V.u32[1];
    pDestination[2] := V.u32[2];
end;



procedure XMStoreInt3A(var pDestination: array of UINT32; constref V: TXMVECTOR);
begin
    pDestination[0] := V.u32[0];
    pDestination[1] := V.u32[1];
    pDestination[2] := V.u32[2];
end;



procedure XMStoreFloat3(var pDestination: TXMFLOAT3; constref V: TXMVECTOR); inline;
begin
    pDestination.x := V.f32[0];
    pDestination.y := V.f32[1];
    pDestination.z := V.f32[2];
end;



procedure XMStoreFloat3A(var pDestination: TXMFLOAT3A; constref V: TXMVECTOR);
begin
    pDestination.x := V.f32[0];
    pDestination.y := V.f32[1];
    pDestination.z := V.f32[2];
end;



procedure XMStoreSInt3(var pDestination: TXMINT3; constref V: TXMVECTOR);
begin
    pDestination.x := trunc(V.f32[0]);
    pDestination.y := trunc(V.f32[1]);
    pDestination.z := trunc(V.f32[2]);
end;



procedure XMStoreUInt3(var pDestination: TXMUINT3; constref V: TXMVECTOR);
begin
    pDestination.x := trunc(V.f32[0]);
    pDestination.y := trunc(V.f32[1]);
    pDestination.z := trunc(V.f32[2]);
end;



procedure XMStoreInt4(var pDestination: array of UINT32; constref V: TXMVECTOR);
begin
    pDestination[0] := V.u32[0];
    pDestination[1] := V.u32[1];
    pDestination[2] := V.u32[2];
    pDestination[3] := V.u32[3];
end;



procedure XMStoreInt4A(var pDestination: array of UINT32; constref V: TXMVECTOR);
begin
    pDestination[0] := V.u32[0];
    pDestination[1] := V.u32[1];
    pDestination[2] := V.u32[2];
    pDestination[3] := V.u32[3];
end;



procedure XMStoreFloat4(var pDestination: TXMFLOAT4; constref V: TXMVECTOR);
begin
    pDestination.x := V.f32[0];
    pDestination.y := V.f32[1];
    pDestination.z := V.f32[2];
    pDestination.w := V.f32[3];
end;



procedure XMStoreFloat4A(var pDestination: TXMFLOAT4A; constref V: TXMVECTOR);
begin
    pDestination.x := V.f32[0];
    pDestination.y := V.f32[1];
    pDestination.z := V.f32[2];
    pDestination.w := V.f32[3];
end;



procedure XMStoreSInt4(var pDestination: TXMINT4; constref V: TXMVECTOR);
begin
    pDestination.x := trunc(V.f32[0]);
    pDestination.y := trunc(V.f32[1]);
    pDestination.z := trunc(V.f32[2]);
    pDestination.w := trunc(V.f32[3]);
end;



procedure XMStoreUInt4(var pDestination: TXMUINT4; constref V: TXMVECTOR);
begin
    pDestination.x := trunc(V.f32[0]);
    pDestination.y := trunc(V.f32[1]);
    pDestination.z := trunc(V.f32[2]);
    pDestination.w := trunc(V.f32[3]);
end;



procedure XMStoreFloat3x3(var pDestination: TXMFLOAT3X3; constref M: TXMMATRIX);
begin
    pDestination.m[0, 0] := M.r[0].f32[0];
    pDestination.m[0, 1] := M.r[0].f32[1];
    pDestination.m[0, 2] := M.r[0].f32[2];

    pDestination.m[1, 0] := M.r[1].f32[0];
    pDestination.m[1, 1] := M.r[1].f32[1];
    pDestination.m[1, 2] := M.r[1].f32[2];

    pDestination.m[2, 0] := M.r[2].f32[0];
    pDestination.m[2, 1] := M.r[2].f32[1];
    pDestination.m[2, 2] := M.r[2].f32[2];
end;



procedure XMStoreFloat4x3(var pDestination: TXMFLOAT4X3; constref M: TXMMATRIX);
begin
    pDestination.m[0, 0] := M.r[0].f32[0];
    pDestination.m[0, 1] := M.r[0].f32[1];
    pDestination.m[0, 2] := M.r[0].f32[2];

    pDestination.m[1, 0] := M.r[1].f32[0];
    pDestination.m[1, 1] := M.r[1].f32[1];
    pDestination.m[1, 2] := M.r[1].f32[2];

    pDestination.m[2, 0] := M.r[2].f32[0];
    pDestination.m[2, 1] := M.r[2].f32[1];
    pDestination.m[2, 2] := M.r[2].f32[2];

    pDestination.m[3, 0] := M.r[3].f32[0];
    pDestination.m[3, 1] := M.r[3].f32[1];
    pDestination.m[3, 2] := M.r[3].f32[2];
end;



procedure XMStoreFloat4x3A(var pDestination: TXMFLOAT4X3A; constref M: TXMMATRIX);
begin
    pDestination.m[0, 0] := M.r[0].f32[0];
    pDestination.m[0, 1] := M.r[0].f32[1];
    pDestination.m[0, 2] := M.r[0].f32[2];

    pDestination.m[1, 0] := M.r[1].f32[0];
    pDestination.m[1, 1] := M.r[1].f32[1];
    pDestination.m[1, 2] := M.r[1].f32[2];

    pDestination.m[2, 0] := M.r[2].f32[0];
    pDestination.m[2, 1] := M.r[2].f32[1];
    pDestination.m[2, 2] := M.r[2].f32[2];

    pDestination.m[3, 0] := M.r[3].f32[0];
    pDestination.m[3, 1] := M.r[3].f32[1];
    pDestination.m[3, 2] := M.r[3].f32[2];
end;



procedure XMStoreFloat4x4(var pDestination: TXMFLOAT4X4; constref M: TXMMATRIX);
begin
    pDestination.m[0, 0] := M.r[0].f32[0];
    pDestination.m[0, 1] := M.r[0].f32[1];
    pDestination.m[0, 2] := M.r[0].f32[2];
    pDestination.m[0, 3] := M.r[0].f32[3];

    pDestination.m[1, 0] := M.r[1].f32[0];
    pDestination.m[1, 1] := M.r[1].f32[1];
    pDestination.m[1, 2] := M.r[1].f32[2];
    pDestination.m[1, 3] := M.r[1].f32[3];

    pDestination.m[2, 0] := M.r[2].f32[0];
    pDestination.m[2, 1] := M.r[2].f32[1];
    pDestination.m[2, 2] := M.r[2].f32[2];
    pDestination.m[2, 3] := M.r[2].f32[3];

    pDestination.m[3, 0] := M.r[3].f32[0];
    pDestination.m[3, 1] := M.r[3].f32[1];
    pDestination.m[3, 2] := M.r[3].f32[2];
    pDestination.m[3, 3] := M.r[3].f32[3];
end;



procedure XMStoreFloat4x4A(var pDestination: TXMFLOAT4X4A; constref M: TXMMATRIX);
begin
    pDestination.m[0, 0] := M.r[0].f32[0];
    pDestination.m[0, 1] := M.r[0].f32[1];
    pDestination.m[0, 2] := M.r[0].f32[2];
    pDestination.m[0, 3] := M.r[0].f32[3];

    pDestination.m[1, 0] := M.r[1].f32[0];
    pDestination.m[1, 1] := M.r[1].f32[1];
    pDestination.m[1, 2] := M.r[1].f32[2];
    pDestination.m[1, 3] := M.r[1].f32[3];

    pDestination.m[2, 0] := M.r[2].f32[0];
    pDestination.m[2, 1] := M.r[2].f32[1];
    pDestination.m[2, 2] := M.r[2].f32[2];
    pDestination.m[2, 3] := M.r[2].f32[3];

    pDestination.m[3, 0] := M.r[3].f32[0];
    pDestination.m[3, 1] := M.r[3].f32[1];
    pDestination.m[3, 2] := M.r[3].f32[2];
    pDestination.m[3, 3] := M.r[3].f32[3];
end;


{****************************************************************************
 *
 * General Vector
 *
 ****************************************************************************}

//------------------------------------------------------------------------------
// Assignment operations
//------------------------------------------------------------------------------


// Return a vector with all elements equaling zero
function XMVectorZero: TXMVECTOR;
begin
    Result.f32[0] := 0;
    Result.f32[1] := 0;
    Result.f32[2] := 0;
    Result.f32[3] := 0;
end;

// Initialize a vector with four floating point values

function XMVectorSet(const x, y, z, w: single): TXMVECTOR;
begin
    Result.f32[0] := x;
    Result.f32[1] := y;
    Result.f32[2] := z;
    Result.f32[3] := w;
end;


// Initialize a vector with four integer values
function XMVectorSetInt(const x, y, z, w: UINT32): TXMVECTOR;
begin
    Result.u32[0] := x;
    Result.u32[1] := y;
    Result.u32[2] := z;
    Result.u32[3] := w;
end;

// Initialize a vector with a replicated floating point value passed by pointer

function XMVectorReplicate(const Value: single): TXMVECTOR;
begin
    Result.f32[0] := Value;
    Result.f32[1] := Value;
    Result.f32[2] := Value;
    Result.f32[3] := Value;
end;

// Initialize a vector with a replicated floating point value passed by pointer

function XMVectorReplicatePtr(pValue: PSingle): TXMVECTOR;
begin
    Result.f32[0] := pValue^;
    Result.f32[1] := pValue^;
    Result.f32[2] := pValue^;
    Result.f32[3] := pValue^;
end;

// Initialize a vector with a replicated integer value

function XMVectorReplicateInt(constref Value: UINT32): TXMVECTOR;
begin
    Result.u32[0] := Value;
    Result.u32[1] := Value;
    Result.u32[2] := Value;
    Result.u32[3] := Value;
end;


// Initialize a vector with a replicated integer value passed by pointer
function XMVectorReplicateIntPtr(pValue: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := pValue^;
    Result.u32[1] := pValue^;
    Result.u32[2] := pValue^;
    Result.u32[3] := pValue^;
end;

// Initialize a vector with all bits set (true mask)

function XMVectorTrueInt: TXMVECTOR;
begin
    Result.u32[0] := $FFFFFFFF;
    Result.u32[1] := $FFFFFFFF;
    Result.u32[2] := $FFFFFFFF;
    Result.u32[3] := $FFFFFFFF;
end;

// Initialize a vector with all bits clear (false mask)

function XMVectorFalseInt: TXMVECTOR;
begin
    Result.u32[0] := $0;
    Result.u32[1] := $0;
    Result.u32[2] := $0;
    Result.u32[3] := $0;
end;


// Returns a vector, all of whose components are equal to the x component of V.
function XMVectorSplatX(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := v.f32[0];
    Result.f32[1] := v.f32[0];
    Result.f32[2] := v.f32[0];
    Result.f32[3] := v.f32[0];
end;


// Returns a vector, all of whose components are equal to the y component of V.
function XMVectorSplatY(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := v.f32[1];
    Result.f32[1] := v.f32[1];
    Result.f32[2] := v.f32[1];
    Result.f32[3] := v.f32[1];
end;

// Returns a vector, all of whose components are equal to the z component of V.

function XMVectorSplatZ(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := v.f32[2];
    Result.f32[1] := v.f32[2];
    Result.f32[2] := v.f32[2];
    Result.f32[3] := v.f32[2];
end;

// Returns a vector, all of whose components are equal to the w component of V.

function XMVectorSplatW(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := v.f32[3];
    Result.f32[1] := v.f32[3];
    Result.f32[2] := v.f32[3];
    Result.f32[3] := v.f32[3];
end;

// Return a vector of 1.0f,1.0f,1.0f,1.0f

function XMVectorSplatOne: TXMVECTOR; inline;
begin
    Result.f32[0] := 1.0;
    Result.f32[1] := 1.0;
    Result.f32[2] := 1.0;
    Result.f32[3] := 1.0;
end;

// Return a vector of INF,INF,INF,INF

function XMVectorSplatInfinity: TXMVECTOR;
begin
    Result.u32[0] := $7F800000;
    Result.u32[1] := $7F800000;
    Result.u32[2] := $7F800000;
    Result.u32[3] := $7F800000;
end;


// Return a vector of Q_NAN,Q_NAN,Q_NAN,Q_NAN
function XMVectorSplatQNaN: TXMVECTOR;
begin
    Result.u32[0] := $7FC00000;
    Result.u32[1] := $7FC00000;
    Result.u32[2] := $7FC00000;
    Result.u32[3] := $7FC00000;
end;


// Return a vector of 1.192092896e-7f,1.192092896e-7f,1.192092896e-7f,1.192092896e-7f
function XMVectorSplatEpsilon: TXMVECTOR;
begin
    Result.u32[0] := $34000000;
    Result.u32[1] := $34000000;
    Result.u32[2] := $34000000;
    Result.u32[3] := $34000000;
end;

// Return a vector of -0.0  (0x80000000),-0.0 ,-0.0 ,-0.0

function XMVectorSplatSignMask: TXMVECTOR;
begin
    Result.u32[0] := $80000000;
    Result.u32[1] := $80000000;
    Result.u32[2] := $80000000;
    Result.u32[3] := $80000000;
end;


// Return the X component in an FPU register.
function XMVectorGetX(constref V: TXMVECTOR): single;
begin
    Result := V.f32[0];
end;

// Return the Y component in an FPU register.

function XMVectorGetY(constref V: TXMVECTOR): single;
begin
    Result := V.f32[1];
end;


// Return the Z component in an FPU register.
function XMVectorGetZ(constref V: TXMVECTOR): single;
begin
    Result := V.f32[2];
end;

// Return the W component in an FPU register.

function XMVectorGetW(constref V: TXMVECTOR): single;
begin
    Result := V.f32[3];
end;


// Store the X component into a 32 bit  single  location in memory.
procedure XMVectorGetXPtr(out x: single; constref V: TXMVECTOR);
begin
    x := V.f32[0];
end;

// Store the Y component into a 32 bit  single  location in memory.

procedure XMVectorGetYPtr(out y: single; constref V: TXMVECTOR);
begin
    y := V.f32[1];
end;


// Store the Z component into a 32 bit  single  location in memory.
procedure XMVectorGetZPtr(out z: single; constref V: TXMVECTOR);
begin
    z := V.f32[2];
end;


// Store the W component into a 32 bit  single  location in memory.
procedure XMVectorGetWPtr(out w: single; constref V: TXMVECTOR);
begin
    w := V.f32[3];
end;

// Return the X component in an integer register.

function XMVectorGetIntX(constref V: TXMVECTOR): UINT32;
begin
    Result := V.u32[0];
end;

// Return the Y component in an integer register.

function XMVectorGetIntY(constref V: TXMVECTOR): UINT32;
begin
    Result := V.u32[1];
end;

// Return the Z component in an integer register.

function XMVectorGetIntZ(constref V: TXMVECTOR): UINT32;
begin
    Result := V.u32[2];
end;

// Return the W component in an integer register.

function XMVectorGetIntW(constref V: TXMVECTOR): UINT32;
begin
    Result := V.u32[3];
end;

// Store the X component into a 32 bit integer location in memory.

procedure XMVectorGetIntXPtr(out x: UINT32; constref V: TXMVECTOR);
begin
    x := V.u32[0];
end;

// Store the Y component into a 32 bit integer location in memory.

procedure XMVectorGetIntYPtr(out y: UINT32; constref V: TXMVECTOR);
begin
    y := V.u32[1];
end;

// Store the Z component into a 32 bit integer location in memory.

procedure XMVectorGetIntZPtr(out z: UINT32; constref V: TXMVECTOR);
begin
    z := V.u32[2];
end;

// Store the W component into a 32 bit integer location in memory.

procedure XMVectorGetIntWPtr(out w: UINT32; constref V: TXMVECTOR);
begin
    w := V.u32[3];
end;


// Sets the X component of a vector to a passed floating point value
function XMVectorSetX(constref V: TXMVECTOR; constref x: single): TXMVECTOR;
begin
    Result.f32[0] := x;
    Result.f32[1] := V.f32[1];
    Result.f32[2] := V.f32[2];
    Result.f32[3] := V.f32[3];
end;

// Sets the Y component of a vector to a passed floating point value

function XMVectorSetY(constref V: TXMVECTOR; constref y: single): TXMVECTOR;
begin
    Result.f32[0] := V.f32[0];
    Result.f32[1] := y;
    Result.f32[2] := V.f32[2];
    Result.f32[3] := V.f32[3];
end;

// Sets the Z component of a vector to a passed floating point value

function XMVectorSetZ(constref V: TXMVECTOR; constref z: single): TXMVECTOR;
begin
    Result.f32[0] := V.f32[0];
    Result.f32[1] := V.f32[1];
    Result.f32[2] := z;
    Result.f32[3] := V.f32[3];
end;

// Sets the W component of a vector to a passed floating point value

function XMVectorSetW(constref V: TXMVECTOR; constref w: single): TXMVECTOR;
begin
    Result.f32[0] := V.f32[0];
    Result.f32[1] := V.f32[1];
    Result.f32[2] := V.f32[2];
    Result.f32[3] := w;
end;

// Sets the X component of a vector to a floating point value passed by p

function XMVectorSetXPtr(constref V: TXMVECTOR; constref x: Psingle): TXMVECTOR;
begin
    Result.f32[0] := x^;
    Result.f32[1] := V.f32[1];
    Result.f32[2] := V.f32[2];
    Result.f32[3] := V.f32[3];
end;

// Sets the Y component of a vector to a floating point value passed by pointer

function XMVectorSetYPtr(constref V: TXMVECTOR; constref y: Psingle): TXMVECTOR;
begin
    Result.f32[0] := V.f32[0];
    Result.f32[1] := y^;
    Result.f32[2] := V.f32[2];
    Result.f32[3] := V.f32[3];
end;

// Sets the Z component of a vector to a floating point value passed by pointer

function XMVectorSetZPtr(constref V: TXMVECTOR; constref z: Psingle): TXMVECTOR;
begin
    Result.f32[0] := V.f32[0];
    Result.f32[1] := V.f32[1];
    Result.f32[2] := z^;
    Result.f32[3] := V.f32[3];
end;

// Sets the W component of a vector to a floating point value passed by pointer

function XMVectorSetWPtr(constref V: TXMVECTOR; constref w: Psingle): TXMVECTOR;
begin
    Result.f32[0] := V.f32[0];
    Result.f32[1] := V.f32[1];
    Result.f32[2] := V.f32[2];
    Result.f32[3] := w^;
end;


// Sets the X component of a vector to an integer passed by value
function XMVectorSetIntX(constref V: TXMVECTOR; const x: UINT32): TXMVECTOR;
begin
    Result.u32[0] := x;
    Result.u32[1] := V.u32[1];
    Result.u32[2] := V.u32[2];
    Result.u32[3] := V.u32[3];
end;

// Sets the Y component of a vector to an integer passed by value

function XMVectorSetIntY(constref V: TXMVECTOR; const y: UINT32): TXMVECTOR;
begin
    Result.u32[0] := V.u32[0];
    Result.u32[1] := y;
    Result.u32[2] := V.u32[2];
    Result.u32[3] := V.u32[3];
end;

// Sets the Z component of a vector to an integer passed by value

function XMVectorSetIntZ(constref V: TXMVECTOR; const z: UINT32): TXMVECTOR;
begin
    Result.u32[0] := V.u32[0];
    Result.u32[1] := V.u32[1];
    Result.u32[2] := z;
    Result.u32[3] := V.u32[3];
end;


// Sets the W component of a vector to an integer passed by value
function XMVectorSetIntW(constref V: TXMVECTOR; const w: UINT32): TXMVECTOR;
begin
    Result.u32[0] := V.u32[0];
    Result.u32[1] := V.u32[1];
    Result.u32[2] := V.u32[2];
    Result.u32[3] := w;
end;

// Sets the X component of a vector to an integer value passed by pointer

function XMVectorSetIntXPtr(constref V: TXMVECTOR; constref x: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := x^;
    Result.u32[1] := V.u32[1];
    Result.u32[2] := V.u32[2];
    Result.u32[3] := V.u32[3];
end;


// Sets the Y component of a vector to an integer value passed by pointer
function XMVectorSetIntYPtr(constref V: TXMVECTOR; constref y: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := V.u32[0];
    Result.u32[1] := y^;
    Result.u32[2] := V.u32[2];
    Result.u32[3] := V.u32[3];
end;


// Sets the Z component of a vector to an integer value passed by pointer
function XMVectorSetIntZPtr(constref V: TXMVECTOR; constref z: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := V.u32[0];
    Result.u32[1] := V.u32[1];
    Result.u32[2] := z^;
    Result.u32[3] := V.u32[3];
end;

// Sets the W component of a vector to an integer value passed by pointer

function XMVectorSetIntWPtr(constref V: TXMVECTOR; constref w: PUINT32): TXMVECTOR;
begin
    Result.u32[0] := V.u32[0];
    Result.u32[1] := V.u32[1];
    Result.u32[2] := V.u32[2];
    Result.u32[3] := w^;
end;



function XMVectorSwizzle(constref V: TXMVECTOR; constref SwizzleX, SwizzleY, SwizzleZ, SwizzleW: UINT32): TXMVECTOR;
var
    c: byte;
begin
    {$WARNING 'This code is not implemented in ASM, it would be inefficient cause no pre-compiling is possible'}
    assert((SwizzleX < 4) and (SwizzleY < 4) and (SwizzleZ < 4) and (SwizzleW < 4));
    Result.f32[0] := V.f32[SwizzleX];
    Result.f32[1] := V.f32[SwizzleY];
    Result.f32[2] := V.f32[SwizzleZ];
    Result.f32[3] := V.f32[SwizzleW];
end;



function XMVectorPermute(V1, V2: TXMVECTOR; PermuteX, PermuteY, PermuteZ, PermuteW: uint32): TXMVECTOR; inline;
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
const
    ControlElement: array [0..1] of UINT32 = ($00000000, $FFFFFFFF);
begin
    assert(VectorIndex0 < 2);
    assert(VectorIndex1 < 2);
    assert(VectorIndex2 < 2);
    assert(VectorIndex3 < 2);

    Result.u32[0] := ControlElement[VectorIndex0];
    Result.u32[1] := ControlElement[VectorIndex1];
    Result.u32[2] := ControlElement[VectorIndex2];
    Result.u32[3] := ControlElement[VectorIndex3];
end;


// Performs a per-component selection between two input vectors and returns the resulting vector.
function XMVectorSelect(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Control: TXMVECTOR): TXMVECTOR;
begin
    Result.u32[0] := (V1.u32[0] and not Control.u32[0]) or (V2.u32[0] and Control.u32[0]);
    Result.u32[1] := (V1.u32[1] and not Control.u32[1]) or (V2.u32[1] and Control.u32[1]);
    Result.u32[2] := (V1.u32[2] and not Control.u32[2]) or (V2.u32[2] and Control.u32[2]);
    Result.u32[3] := (V1.u32[3] and not Control.u32[3]) or (V2.u32[3] and Control.u32[3]);
end;



function XMVectorMergeXY(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    Result.u32[0] := V1.u32[0];
    Result.u32[1] := V2.u32[0];
    Result.u32[2] := V1.u32[1];
    Result.u32[3] := V2.u32[1];
end;



function XMVectorMergeZW(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    Result.u32[0] := V1.u32[2];
    Result.u32[1] := V2.u32[2];
    Result.u32[2] := V1.u32[3];
    Result.u32[3] := V2.u32[3];
end;



//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------

function XMVectorEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    if (V1.f32[0] = V2.f32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if (V1.f32[1] = V2.f32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if (V1.f32[2] = V2.f32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if (V1.f32[3] = V2.f32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;



function XMVectorEqualR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
var
    ux, uy, uz, uw, CR: UINT32;
begin
    if (V1.f32[0] = V2.f32[0]) then
        ux := $FFFFFFFF
    else
        ux := 0;
    if (V1.f32[1] = V2.f32[1]) then
        uy := $FFFFFFFF
    else
        uy := 0;
    if (V1.f32[2] = V2.f32[2]) then
        uz := $FFFFFFFF
    else
        uz := 0;
    if (V1.f32[3] = V2.f32[3]) then
        uw := $FFFFFFFF
    else
        uw := 0;
    CR := 0;
    if (ux and uy and uz and uw) <> 0 then
    begin
        // All elements are greater
        CR := XM_CRMASK_CR6TRUE;
    end
    else if (not (ux or uy or uz or uw) <> 0) then
    begin
        // All elements are not greater
        CR := XM_CRMASK_CR6FALSE;
    end;
    pCR := CR;

    Result.f32[0] := ux;
    Result.f32[0] := uy;
    Result.f32[0] := uz;
    Result.f32[0] := uw;
end;


//------------------------------------------------------------------------------
// Treat the components of the vectors as unsigned integers and
// compare individual bits between the two.  This is useful for
// comparing control vectors and result vectors returned from
// other comparison operations.

function XMVectorEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    if (V1.u32[0] = V2.u32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if (V1.u32[1] = V2.u32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if (V1.u32[2] = V2.u32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if (V1.u32[3] = V2.u32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;



function XMVectorEqualIntR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
var
    Control: TXMVECTOR;
begin
    Control := XMVectorEqualInt(V1, V2);

    pCR := 0;
    if (XMVector4EqualInt(Control, XMVectorTrueInt())) then
    begin
        // All elements are equal
        pCR := pCR or XM_CRMASK_CR6TRUE;
    end
    else if (XMVector4EqualInt(Control, XMVectorFalseInt())) then
    begin
        // All elements are not equal
        pCR := pCR or XM_CRMASK_CR6FALSE;
    end;
    Result := Control;
end;



function XMVectorNearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): TXMVECTOR;
var
    fDeltax, fDeltay, fDeltaz, fDeltaw: single;
begin
    fDeltax := V1.f32[0] - V2.f32[0];
    fDeltay := V1.f32[1] - V2.f32[1];
    fDeltaz := V1.f32[2] - V2.f32[2];
    fDeltaw := V1.f32[3] - V2.f32[3];

    fDeltax := abs(fDeltax);
    fDeltay := abs(fDeltay);
    fDeltaz := abs(fDeltaz);
    fDeltaw := abs(fDeltaw);

    if (fDeltax <= Epsilon.f32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if (fDeltay <= Epsilon.f32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if (fDeltaz <= Epsilon.f32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if (fDeltaw <= Epsilon.f32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;



function XMVectorNotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    if (V1.f32[0] <> V2.f32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if (V1.f32[1] <> V2.f32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if (V1.f32[2] <> V2.f32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if (V1.f32[3] <> V2.f32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;



function XMVectorNotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    if (V1.u32[0] <> V2.u32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if (V1.u32[1] <> V2.u32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if (V1.u32[2] <> V2.u32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if (V1.u32[3] <> V2.u32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;



function XMVectorGreater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    if (V1.f32[0] > V2.f32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if (V1.f32[1] > V2.f32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if (V1.f32[2] > V2.f32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if (V1.f32[3] > V2.f32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;



function XMVectorGreaterR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
var
    ux, uy, uz, uw, cr: UINT32;
begin
    if (V1.f32[0] > V2.f32[0]) then
        ux := $FFFFFFFF
    else
        ux := 0;
    if (V1.f32[1] > V2.f32[1]) then
        uy := $FFFFFFFF
    else
        uy := 0;
    if (V1.f32[2] > V2.f32[2]) then
        uz := $FFFFFFFF
    else
        uz := 0;
    if (V1.f32[3] > V2.f32[3]) then
        uw := $FFFFFFFF
    else
        uw := 0;
    CR := 0;
    if (ux and uy and uz and uw) <> 0 then
    begin
        // All elements are greater
        CR := XM_CRMASK_CR6TRUE;
    end
    else if (not (ux or uy or uz or uw) <> 0) then
    begin
        // All elements are not greater
        CR := XM_CRMASK_CR6FALSE;
    end;
    pCR := CR;
    Result.u32[0] := ux;
    Result.u32[1] := uy;
    Result.u32[2] := uz;
    Result.u32[3] := uw;
end;



function XMVectorGreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    if (V1.f32[0] >= V2.f32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if (V1.f32[1] >= V2.f32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if (V1.f32[2] >= V2.f32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if (V1.f32[3] >= V2.f32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;



function XMVectorGreaterOrEqualR(out pCR: UINT32; constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
var
    ux, uy, uz, uw, Cr: UINT32;
begin
    if (V1.f32[0] >= V2.f32[0]) then
        ux := $FFFFFFFF
    else
        ux := 0;
    if (V1.f32[1] >= V2.f32[1]) then
        uy := $FFFFFFFF
    else
        uy := 0;
    if (V1.f32[2] >= V2.f32[2]) then
        uz := $FFFFFFFF
    else
        uz := 0;
    if (V1.f32[3] >= V2.f32[3]) then
        uw := $FFFFFFFF
    else
        uw := 0;
    CR := 0;
    if (ux and uy and uz and uw) <> 0 then
    begin
        // All elements are greater
        CR := XM_CRMASK_CR6TRUE;
    end
    else if (not (ux or uy or uz or uw) <> 0) then
    begin
        // All elements are not greater
        CR := XM_CRMASK_CR6FALSE;
    end;
    pCR := CR;
    Result.u32[0] := ux;
    Result.u32[1] := uy;
    Result.u32[2] := uz;
    Result.u32[3] := uw;
end;



function XMVectorLess(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    if (V1.f32[0] < V2.f32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if (V1.f32[1] < V2.f32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if (V1.f32[2] < V2.f32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if (V1.f32[3] < V2.f32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;



function XMVectorLessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    if (V1.f32[0] <= V2.f32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if (V1.f32[1] <= V2.f32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if (V1.f32[2] <= V2.f32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if (V1.f32[3] <= V2.f32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;



function XMVectorInBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): TXMVECTOR;
begin
    if (V.f32[0] <= Bounds.f32[0]) and (V.f32[0] >= -Bounds.f32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if (V.f32[1] <= Bounds.f32[1]) and (V.f32[1] >= -Bounds.f32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if (V.f32[2] <= Bounds.f32[2]) and (V.f32[2] >= -Bounds.f32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if (V.f32[3] <= Bounds.f32[3]) and (V.f32[3] >= -Bounds.f32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;



function XMVectorInBoundsR(out pCR: UINT32; constref V: TXMVECTOR; constref Bounds: TXMVECTOR): TXMVECTOR;
var
    ux, uy, uz, uw, cr: UINT32;
begin
    if ((V.f32[0] <= Bounds.f32[0]) and (V.f32[0] >= -Bounds.f32[0])) then
        ux := $FFFFFFFF
    else
        ux := 0;
    if ((V.f32[1] <= Bounds.f32[1]) and (V.f32[1] >= -Bounds.f32[1])) then
        uy := $FFFFFFFF
    else
        uy := 0;
    if ((V.f32[2] <= Bounds.f32[2]) and (V.f32[2] >= -Bounds.f32[2])) then
        uz := $FFFFFFFF
    else
        uz := 0;
    if ((V.f32[3] <= Bounds.f32[3]) and (V.f32[3] >= -Bounds.f32[3])) then
        uw := $FFFFFFFF
    else
        uw := 0;

    CR := 0;
    if (ux and uy and uz and uw) <> 0 then
    begin
        // All elements are in bounds
        CR := XM_CRMASK_CR6BOUNDS;
    end;
    pCR := CR;
    Result.u32[0] := ux;
    Result.u32[1] := uy;
    Result.u32[2] := uz;
    Result.u32[3] := uw;
end;



function XMVectorIsNaN(constref V: TXMVECTOR): TXMVECTOR;
begin
    if XMISNAN(V.u32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if XMISNAN(V.u32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if XMISNAN(V.u32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if XMISNAN(V.u32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;



function XMVectorIsInfinite(constref V: TXMVECTOR): TXMVECTOR;
begin
    if XMISINF(V.u32[0]) then
        Result.u32[0] := $FFFFFFFF
    else
        Result.u32[0] := 0;
    if XMISINF(V.u32[1]) then
        Result.u32[1] := $FFFFFFFF
    else
        Result.u32[1] := 0;
    if XMISINF(V.u32[2]) then
        Result.u32[2] := $FFFFFFFF
    else
        Result.u32[2] := 0;
    if XMISINF(V.u32[3]) then
        Result.u32[3] := $FFFFFFFF
    else
        Result.u32[3] := 0;
end;


//------------------------------------------------------------------------------
// Rounding and clamping operations
//------------------------------------------------------------------------------

function XMVectorMin(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    if (V1.f32[0] < V2.f32[0]) then
        Result.f32[0] := V1.f32[0]
    else
        Result.f32[0] := V2.f32[0];
    if (V1.f32[1] < V2.f32[1]) then
        Result.f32[1] := V1.f32[1]
    else
        Result.f32[1] := V2.f32[1];
    if (V1.f32[2] < V2.f32[2]) then
        Result.f32[2] := V1.f32[2]
    else
        Result.f32[2] := V2.f32[2];
    if (V1.f32[3] < V2.f32[3]) then
        Result.f32[3] := V1.f32[3]
    else
        Result.f32[3] := V2.f32[3];
end;



function XMVectorMax(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    if (V1.f32[0] > V2.f32[0]) then
        Result.f32[0] := V1.f32[0]
    else
        Result.f32[0] := V2.f32[0];
    if (V1.f32[1] > V2.f32[1]) then
        Result.f32[1] := V1.f32[1]
    else
        Result.f32[1] := V2.f32[1];
    if (V1.f32[2] > V2.f32[2]) then
        Result.f32[2] := V1.f32[2]
    else
        Result.f32[2] := V2.f32[2];
    if (V1.f32[3] > V2.f32[3]) then
        Result.f32[3] := V1.f32[3]
    else
        Result.f32[3] := V2.f32[3];
end;


// Round to nearest (even) a.k.a. banker's rounding
function round_to_nearest(x: single): single; inline;
var
    i: single;
    int_part: single;
begin
    i := floor(x);
    x := x - i;
    if (x < 0.5) then
    begin
        Result := i;
        Exit;
    end;
    if (x > 0.5) then
    begin
        Result := i + 1.0;
        Exit;
    end;

    int_part := round(i / 2.0);
    if ((2.0 * int_part) = i) then
    begin
        Result := i;
        Exit;
    end;
    Result := i + 1.0;
end;



function XMVectorRound(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := round_to_nearest(V.f32[0]);
    Result.f32[1] := round_to_nearest(V.f32[1]);
    Result.f32[2] := round_to_nearest(V.f32[2]);
    Result.f32[3] := round_to_nearest(V.f32[3]);
end;



function XMVectorTruncate(constref V: TXMVECTOR): TXMVECTOR;
var
    i: uint32;
begin

    // Avoid C4701 -> Pascal never does such shit
    Result.f32[0] := 0.0;

    for i := 0 to 3 do
    begin
        if (XMISNAN(V.u32[i])) then
        begin
            Result.u32[i] := $7FC00000;
        end
        else if (abs(V.f32[i]) < 8388608.0) then
        begin
            Result.f32[i] := V.f32[i];
        end
        else
        begin
            Result.f32[i] := V.f32[i];

        end;
    end;
end;



function XMVectorFloor(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := floor(V.f32[0]);
    Result.f32[1] := floor(V.f32[1]);
    Result.f32[2] := floor(V.f32[2]);
    Result.f32[3] := floor(V.f32[3]);
end;



function XMVectorCeiling(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := ceil(V.f32[0]);
    Result.f32[1] := ceil(V.f32[1]);
    Result.f32[2] := ceil(V.f32[2]);
    Result.f32[3] := ceil(V.f32[3]);
end;



function XMVectorClamp(constref V: TXMVECTOR; constref Min: TXMVECTOR; constref Max: TXMVECTOR): TXMVECTOR;
begin
    assert(XMVector4LessOrEqual(Min, Max));
    Result := XMVectorMax(Min, V);
    Result := XMVectorMin(Max, Result);
end;



function XMVectorSaturate(constref V: TXMVECTOR): TXMVECTOR;
var
    Zero: TXMVECTOR;
begin
    Zero := XMVectorZero();
    Result := XMVectorClamp(V, Zero, g_XMOne.v);
end;

//------------------------------------------------------------------------------
// Bitwise logical operations
//------------------------------------------------------------------------------

function XMVectorAndInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    Result.u32[0] := V1.u32[0] and V2.u32[0];
    Result.u32[1] := V1.u32[1] and V2.u32[1];
    Result.u32[2] := V1.u32[2] and V2.u32[2];
    Result.u32[3] := V1.u32[3] and V2.u32[3];
end;



function XMVectorAndCInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    Result.u32[0] := V1.u32[0] and not V2.u32[0];
    Result.u32[1] := V1.u32[1] and not V2.u32[1];
    Result.u32[2] := V1.u32[2] and not V2.u32[2];
    Result.u32[3] := V1.u32[3] and not V2.u32[3];
end;



function XMVectorOrInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    Result.u32[0] := V1.u32[0] or V2.u32[0];
    Result.u32[1] := V1.u32[1] or V2.u32[1];
    Result.u32[2] := V1.u32[2] or V2.u32[2];
    Result.u32[3] := V1.u32[3] or V2.u32[3];
end;



function XMVectorNorInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    Result.u32[0] := not (V1.u32[0] or V2.u32[0]);
    Result.u32[1] := not (V1.u32[1] or V2.u32[1]);
    Result.u32[2] := not (V1.u32[2] or V2.u32[2]);
    Result.u32[3] := not (V1.u32[3] or V2.u32[3]);
end;



function XMVectorXorInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    Result.u32[0] := V1.u32[0] xor V2.u32[0];
    Result.u32[1] := V1.u32[1] xor V2.u32[1];
    Result.u32[2] := V1.u32[2] xor V2.u32[2];
    Result.u32[3] := V1.u32[3] xor V2.u32[3];
end;



function XMVectorAdd(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := V1.f32[0] + V2.f32[0];
    Result.f32[1] := V1.f32[1] + V2.f32[1];
    Result.f32[2] := V1.f32[2] + V2.f32[2];
    Result.f32[3] := V1.f32[3] + V2.f32[3];
end;



function XMVectorSum(constref V: TXMVECTOR): TXMVECTOR;
var
    f: single;
begin
    f := V.f32[0] + V.f32[1] + V.f32[2] + V.f32[3];
    Result.f32[0] := f;
    Result.f32[1] := f;
    Result.f32[2] := f;
    Result.f32[3] := f;
end;



function XMVectorAddAngles(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
var
    Zero, Mask, Offset: TXMVECTOR;
begin
    Zero := XMVectorZero();

    // Add the given angles together.  If the range of V1 is such
    // that -Pi <= V1 < Pi and the range of V2 is such that
    // -2Pi <= V2 <= 2Pi, then the range of the resulting angle
    // will be -Pi <= Result < Pi.
    Result := XMVectorAdd(V1, V2);

    Mask := XMVectorLess(Result, g_XMNegativePi.v);
    Offset := XMVectorSelect(Zero, g_XMTwoPi.v, Mask);

    Mask := XMVectorGreaterOrEqual(Result, g_XMPi.v);
    Offset := XMVectorSelect(Offset, g_XMNegativeTwoPi.v, Mask);

    Result := XMVectorAdd(Result, Offset);
end;



function XMVectorSubtract(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := V1.f32[0] - V2.f32[0];
    Result.f32[1] := V1.f32[1] - V2.f32[1];
    Result.f32[2] := V1.f32[2] - V2.f32[2];
    Result.f32[3] := V1.f32[3] - V2.f32[3];
end;



function XMVectorSubtractAngles(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
var
    Zero, Mask, Offset: TXMVECTOR;
begin
    Zero := XMVectorZero();

    // Subtract the given angles.  If the range of V1 is such
    // that -Pi <= V1 < Pi and the range of V2 is such that
    // -2Pi <= V2 <= 2Pi, then the range of the resulting angle
    // will be -Pi <= Result < Pi.
    Result := XMVectorSubtract(V1, V2);

    Mask := XMVectorLess(Result, g_XMNegativePi.v);
    Offset := XMVectorSelect(Zero, g_XMTwoPi.v, Mask);

    Mask := XMVectorGreaterOrEqual(Result, g_XMPi.v);
    Offset := XMVectorSelect(Offset, g_XMNegativeTwoPi.v, Mask);

    Result := XMVectorAdd(Result, Offset);
end;



function XMVectorMultiply(const V1: TXMVECTOR; const V2: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := V1.f32[0] * V2.f32[0];
    Result.f32[1] := V1.f32[1] * V2.f32[1];
    Result.f32[2] := V1.f32[2] * V2.f32[2];
    Result.f32[3] := V1.f32[3] * V2.f32[3];
end;



function XMVectorMultiplyAdd(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := V1.f32[0] * V2.f32[0] + V3.f32[0];
    Result.f32[1] := V1.f32[1] * V2.f32[1] + V3.f32[1];
    Result.f32[2] := V1.f32[2] * V2.f32[2] + V3.f32[2];
    Result.f32[3] := V1.f32[3] * V2.f32[3] + V3.f32[3];
end;



function XMVectorDivide(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := V1.f32[0] / V2.f32[0];
    Result.f32[1] := V1.f32[1] / V2.f32[1];
    Result.f32[2] := V1.f32[2] / V2.f32[2];
    Result.f32[3] := V1.f32[3] / V2.f32[3];
end;


// Computes the difference of a third vector and the product of the first two vectors.
// Result := V3-V1*V2;
function XMVectorNegativeMultiplySubtract(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := V3.f32[0] - (V1.f32[0] * V2.f32[0]);
    Result.f32[1] := V3.f32[1] - (V1.f32[1] * V2.f32[1]);
    Result.f32[2] := V3.f32[2] - (V1.f32[2] * V2.f32[2]);
    Result.f32[3] := V3.f32[3] - (V1.f32[3] * V2.f32[3]);
end;



function XMVectorScale(constref V: TXMVECTOR; constref ScaleFactor: single): TXMVECTOR;
begin
    Result.f32[0] := V.f32[0] * ScaleFactor;
    Result.f32[1] := V.f32[1] * ScaleFactor;
    Result.f32[2] := V.f32[2] * ScaleFactor;
    Result.f32[3] := V.f32[3] * ScaleFactor;
end;



function XMVectorReciprocalEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := 1.0 / V.f32[0];
    Result.f32[1] := 1.0 / V.f32[1];
    Result.f32[2] := 1.0 / V.f32[2];
    Result.f32[3] := 1.0 / V.f32[3];
end;



function XMVectorReciprocal(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := 1.0 / V.f32[0];
    Result.f32[1] := 1.0 / V.f32[1];
    Result.f32[2] := 1.0 / V.f32[2];
    Result.f32[3] := 1.0 / V.f32[3];
end;


// Return an estimated square root
function XMVectorSqrtEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := sqrt(V.f32[0]);
    Result.f32[1] := sqrt(V.f32[1]);
    Result.f32[2] := sqrt(V.f32[2]);
    Result.f32[3] := sqrt(V.f32[3]);
end;



function XMVectorSqrt(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := sqrt(V.f32[0]);
    Result.f32[1] := sqrt(V.f32[1]);
    Result.f32[2] := sqrt(V.f32[2]);
    Result.f32[3] := sqrt(V.f32[3]);
end;



function XMVectorReciprocalSqrtEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := 1.0 / sqrt(V.f32[0]);
    Result.f32[1] := 1.0 / sqrt(V.f32[1]);
    Result.f32[2] := 1.0 / sqrt(V.f32[2]);
    Result.f32[3] := 1.0 / sqrt(V.f32[3]);
end;



function XMVectorReciprocalSqrt(const V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := 1.0 / sqrt(V.f32[0]);
    Result.f32[1] := 1.0 / sqrt(V.f32[1]);
    Result.f32[2] := 1.0 / sqrt(V.f32[2]);
    Result.f32[3] := 1.0 / sqrt(V.f32[3]);
end;



function XMVectorExp2(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := power(2.0, V.f32[0]);
    Result.f32[1] := power(2.0, V.f32[1]);
    Result.f32[2] := power(2.0, V.f32[2]);
    Result.f32[3] := power(2.0, V.f32[3]);
end;



function XMVectorExpE(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := exp(V.f32[0]);
    Result.f32[1] := exp(V.f32[1]);
    Result.f32[2] := exp(V.f32[2]);
    Result.f32[3] := exp(V.f32[3]);
end;

// Computes the base two logarithm of each component of a vector.

function XMVectorLog2(constref V: TXMVECTOR): TXMVECTOR;
const
    fScale: single = 1.4426950; // (1.0  / logf(2.0 ));
begin
    Result.f32[0] := ln(V.f32[0]) * fScale;
    Result.f32[0] := ln(V.f32[1]) * fScale;
    Result.f32[0] := ln(V.f32[2]) * fScale;
    Result.f32[0] := ln(V.f32[3]) * fScale;
end;



function XMVectorLogE(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := ln(V.f32[0]);
    Result.f32[1] := ln(V.f32[1]);
    Result.f32[2] := ln(V.f32[2]);
    Result.f32[3] := ln(V.f32[3]);
end;



function XMVectorPow(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := power(V1.f32[0], V2.f32[0]);
    Result.f32[1] := power(V1.f32[1], V2.f32[1]);
    Result.f32[2] := power(V1.f32[2], V2.f32[2]);
    Result.f32[3] := power(V1.f32[3], V2.f32[3]);
end;



function XMVectorAbs(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := abs(V.f32[0]);
    Result.f32[1] := abs(V.f32[1]);
    Result.f32[2] := abs(V.f32[2]);
    Result.f32[3] := abs(V.f32[3]);
end;

// V1 % V2  :=  V1 - V2 * truncate(V1 / V2)

function XMVectorMod(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
var
    Quotient: TXMVECTOR;
begin
    Quotient := XMVectorDivide(V1, V2);
    Quotient := XMVectorTruncate(Quotient);
    Result := XMVectorNegativeMultiplySubtract(V2, Quotient, V1);
end;



function XMVectorModAngles(constref Angles: TXMVECTOR): TXMVECTOR;
var
    V: TXMVECTOR;
begin
    // Modulo the range of the given angles such that -XM_PI <= Angles < XM_PI
    V := XMVectorMultiply(Angles, g_XMReciprocalTwoPi.v);
    V := XMVectorRound(V);
    Result := XMVectorNegativeMultiplySubtract(g_XMTwoPi.v, V, Angles);
end;

// 11-degree minimax approximation

function XMVectorSin(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := sin(V.f32[0]);
    Result.f32[1] := sin(V.f32[1]);
    Result.f32[2] := sin(V.f32[2]);
    Result.f32[3] := sin(V.f32[3]);
end;


// 7-degree minimax approximation
function XMVectorSinEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := sin(V.f32[0]);
    Result.f32[1] := sin(V.f32[1]);
    Result.f32[2] := sin(V.f32[2]);
    Result.f32[3] := sin(V.f32[3]);
end;


// 10-degree minimax approximation
function XMVectorCos(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := cos(V.f32[0]);
    Result.f32[1] := cos(V.f32[1]);
    Result.f32[2] := cos(V.f32[2]);
    Result.f32[3] := cos(V.f32[3]);
end;


// 6-degree minimax approximation
function XMVectorCosEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := cos(V.f32[0]);
    Result.f32[1] := cos(V.f32[1]);
    Result.f32[2] := cos(V.f32[2]);
    Result.f32[3] := cos(V.f32[3]);
end;


// 11/10-degree minimax approximation
procedure XMVectorSinCos(out pSin: TXMVECTOR; out pCos: TXMVECTOR; constref V: TXMVECTOR);
begin
    pSin.f32[0] := sin(V.f32[0]);
    pSin.f32[1] := sin(V.f32[1]);
    pSin.f32[2] := sin(V.f32[2]);
    pSin.f32[3] := sin(V.f32[3]);

    pCos.f32[0] := cos(V.f32[0]);
    pCos.f32[0] := cos(V.f32[1]);
    pCos.f32[0] := cos(V.f32[2]);
    pCos.f32[0] := cos(V.f32[3]);
end;

// 7/6-degree minimax approximation

procedure XMVectorSinCosEst(out pSin: TXMVECTOR; out pCos: TXMVECTOR; constref V: TXMVECTOR);
begin
    pSin.f32[0] := sin(V.f32[0]);
    pSin.f32[1] := sin(V.f32[1]);
    pSin.f32[2] := sin(V.f32[2]);
    pSin.f32[3] := sin(V.f32[3]);

    pCos.f32[0] := cos(V.f32[0]);
    pCos.f32[0] := cos(V.f32[1]);
    pCos.f32[0] := cos(V.f32[2]);
    pCos.f32[0] := cos(V.f32[3]);
end;

// Cody and Waite algorithm to compute tangent.

function XMVectorTan(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := tan(V.f32[0]);
    Result.f32[1] := tan(V.f32[1]);
    Result.f32[2] := tan(V.f32[2]);
    Result.f32[3] := tan(V.f32[3]);
end;



function XMVectorTanEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := tan(V.f32[0]);
    Result.f32[1] := tan(V.f32[1]);
    Result.f32[2] := tan(V.f32[2]);
    Result.f32[3] := tan(V.f32[3]);
end;



function XMVectorSinH(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := sinh(V.f32[0]);
    Result.f32[1] := sinh(V.f32[1]);
    Result.f32[2] := sinh(V.f32[2]);
    Result.f32[3] := sinh(V.f32[3]);
end;



function XMVectorCosH(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := cosh(V.f32[0]);
    Result.f32[1] := cosh(V.f32[1]);
    Result.f32[2] := cosh(V.f32[2]);
    Result.f32[3] := cosh(V.f32[3]);
end;



function XMVectorTanH(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := tanh(V.f32[0]);
    Result.f32[1] := tanh(V.f32[1]);
    Result.f32[2] := tanh(V.f32[2]);
    Result.f32[3] := tanh(V.f32[3]);
end;

// Computes the arcsine of each component of an XMVECTOR.
// 7-degree minimax approximation
function XMVectorASin(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := arcsin(V.f32[0]);
    Result.f32[1] := arcsin(V.f32[1]);
    Result.f32[2] := arcsin(V.f32[2]);
    Result.f32[3] := arcsin(V.f32[3]);
end;


// 3-degree minimax approximation
function XMVectorASinEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := arcsin(V.f32[0]);
    Result.f32[1] := arcsin(V.f32[1]);
    Result.f32[2] := arcsin(V.f32[2]);
    Result.f32[3] := arcsin(V.f32[3]);
end;


// 7-degree minimax approximation
function XMVectorACos(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := arccos(V.f32[0]);
    Result.f32[1] := arccos(V.f32[1]);
    Result.f32[2] := arccos(V.f32[2]);
    Result.f32[3] := arccos(V.f32[3]);
end;


// 3-degree minimax approximation
function XMVectorACosEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := arccos(V.f32[0]);
    Result.f32[1] := arccos(V.f32[1]);
    Result.f32[2] := arccos(V.f32[2]);
    Result.f32[3] := arccos(V.f32[3]);
end;


// 17-degree minimax approximation
function XMVectorATan(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := arctan(V.f32[0]);
    Result.f32[1] := arctan(V.f32[1]);
    Result.f32[2] := arctan(V.f32[2]);
    Result.f32[3] := arctan(V.f32[3]);
end;

// 9-degree minimax approximation

function XMVectorATanEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := arctan(V.f32[0]);
    Result.f32[1] := arctan(V.f32[1]);
    Result.f32[2] := arctan(V.f32[2]);
    Result.f32[3] := arctan(V.f32[3]);
end;



// Computes the arctangent of Y/X.
function XMVectorATan2(constref Y: TXMVECTOR; constref X: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := arctan2(Y.f32[0], X.f32[0]);
    Result.f32[1] := arctan2(Y.f32[1], X.f32[1]);
    Result.f32[2] := arctan2(Y.f32[2], X.f32[2]);
    Result.f32[3] := arctan2(Y.f32[3], X.f32[3]);
end;



function XMVectorATan2Est(constref Y: TXMVECTOR; constref X: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := arctan2(Y.f32[0], X.f32[0]);
    Result.f32[1] := arctan2(Y.f32[1], X.f32[1]);
    Result.f32[2] := arctan2(Y.f32[2], X.f32[2]);
    Result.f32[3] := arctan2(Y.f32[3], X.f32[3]);
end;

// V0 + t * (V1 - V0)

function XMVectorLerp(constref V0: TXMVECTOR; constref V1: TXMVECTOR; constref t: single): TXMVECTOR;
var
    Scale, Length: TXMVECTOR;
begin
    Scale := XMVectorReplicate(t);
    Length := XMVectorSubtract(V1, V0);
    Result := XMVectorMultiplyAdd(Length, Scale, V0);
end;

// V0 + T * (V1 - V0)

function XMVectorLerpV(constref V0: TXMVECTOR; constref V1: TXMVECTOR; constref T: TXMVECTOR): TXMVECTOR;
var
    Length: TXMVECTOR;
begin
    Length := XMVectorSubtract(V1, V0);
    Result := XMVectorMultiplyAdd(Length, T, V0);
end;



// Result  :=  (2 * t^3 - 3 * t^2 + 1) * Position0 +
//          (t^3 - 2 * t^2 + t) * Tangent0 +
//          (-2 * t^3 + 3 * t^2) * Position1 +
//          (t^3 - t^2) * Tangent1
function XMVectorHermite(constref Position0: TXMVECTOR; constref Tangent0: TXMVECTOR; constref Position1: TXMVECTOR;
    constref Tangent1: TXMVECTOR; constref t: single): TXMVECTOR;
var
    t2, t3: single;
    P0, T0, P1, T1: TXMVECTOR;
begin
    t2 := t * t;
    t3 := t * t2;

    P0 := XMVectorReplicate(2.0 * t3 - 3.0 * t2 + 1.0);
    T0 := XMVectorReplicate(t3 - 2.0 * t2 + t);
    P1 := XMVectorReplicate(-2.0 * t3 + 3.0 * t2);
    T1 := XMVectorReplicate(t3 - t2);

    Result := XMVectorMultiply(P0, Position0);
    Result := XMVectorMultiplyAdd(T0, Tangent0, Result);
    Result := XMVectorMultiplyAdd(P1, Position1, Result);
    Result := XMVectorMultiplyAdd(T1, Tangent1, Result);
end;


// Result  :=  (2 * t^3 - 3 * t^2 + 1) * Position0 +
//          (t^3 - 2 * t^2 + t) * Tangent0 +
//          (-2 * t^3 + 3 * t^2) * Position1 +
//          (t^3 - t^2) * Tangent1
function XMVectorHermiteV(constref Position0: TXMVECTOR; constref Tangent0: TXMVECTOR; constref Position1: TXMVECTOR;
    constref Tangent1: TXMVECTOR; constref T: TXMVECTOR): TXMVECTOR;
var
    T2, T3, P0, T0, P1, T1: TXMVECTOR;
begin
    T2 := XMVectorMultiply(T, T);
    T3 := XMVectorMultiply(T, T2);

    P0 := XMVectorReplicate(2.0 * T3.f32[0] - 3.0 * T2.f32[0] + 1.0);
    T0 := XMVectorReplicate(T3.f32[1] - 2.0 * T2.f32[1] + T.f32[1]);
    P1 := XMVectorReplicate(-2.0 * T3.f32[2] + 3.0 * T2.f32[2]);
    T1 := XMVectorReplicate(T3.f32[3] - T2.f32[3]);

    Result := XMVectorMultiply(P0, Position0);
    Result := XMVectorMultiplyAdd(T0, Tangent0, Result);
    Result := XMVectorMultiplyAdd(P1, Position1, Result);
    Result := XMVectorMultiplyAdd(T1, Tangent1, Result);
end;

// Result  :=  ((-t^3 + 2 * t^2 - t) * Position0 +
//           (3 * t^3 - 5 * t^2 + 2) * Position1 +
//           (-3 * t^3 + 4 * t^2 + t) * Position2 +
//           (t^3 - t^2) * Position3) * 0.5
function XMVectorCatmullRom(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref Position3: TXMVECTOR; constref t: single): TXMVECTOR;
var
    t2, t3: single;
    P0, P1, P2, P3: TXMVECTOR;
begin
    t2 := t * t;
    t3 := t * t2;

    P0 := XMVectorReplicate((-t3 + 2.0 * t2 - t) * 0.5);
    P1 := XMVectorReplicate((3.0 * t3 - 5.0 * t2 + 2.0) * 0.5);
    P2 := XMVectorReplicate((-3.0 * t3 + 4.0 * t2 + t) * 0.5);
    P3 := XMVectorReplicate((t3 - t2) * 0.5);

    Result := XMVectorMultiply(P0, Position0);
    Result := XMVectorMultiplyAdd(P1, Position1, Result);
    Result := XMVectorMultiplyAdd(P2, Position2, Result);
    Result := XMVectorMultiplyAdd(P3, Position3, Result);
end;



function XMVectorCatmullRomV(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref Position3: TXMVECTOR; constref T: TXMVECTOR): TXMVECTOR;
var
    fx, fy, fz, fw: single;
begin
    fx := T.f32[0];
    fy := T.f32[1];
    fz := T.f32[2];
    fw := T.f32[3];

    Result.f32[0] := 0.5 * ((-fx * fx * fx + 2 * fx * fx - fx) * Position0.f32[0] + (3 * fx * fx * fx - 5 * fx * fx + 2) *
        Position1.f32[0] + (-3 * fx * fx * fx + 4 * fx * fx + fx) * Position2.f32[0] + (fx * fx * fx - fx * fx) * Position3.f32[0]);

    Result.f32[1] := 0.5 * ((-fy * fy * fy + 2 * fy * fy - fy) * Position0.f32[1] + (3 * fy * fy * fy - 5 * fy * fy + 2) *
        Position1.f32[1] + (-3 * fy * fy * fy + 4 * fy * fy + fy) * Position2.f32[1] + (fy * fy * fy - fy * fy) * Position3.f32[1]);

    Result.f32[2] := 0.5 * ((-fz * fz * fz + 2 * fz * fz - fz) * Position0.f32[2] + (3 * fz * fz * fz - 5 * fz * fz + 2) *
        Position1.f32[2] + (-3 * fz * fz * fz + 4 * fz * fz + fz) * Position2.f32[2] + (fz * fz * fz - fz * fz) * Position3.f32[2]);

    Result.f32[3] := 0.5 * ((-fw * fw * fw + 2 * fw * fw - fw) * Position0.f32[3] + (3 * fw * fw * fw - 5 * fw * fw + 2) *
        Position1.f32[3] + (-3 * fw * fw * fw + 4 * fw * fw + fw) * Position2.f32[3] + (fw * fw * fw - fw * fw) * Position3.f32[3]);
end;


// Result  :=  Position0 + f * (Position1 - Position0) + g * (Position2 - Position0)
function XMVectorBaryCentric(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref f: single; constref g: single): TXMVECTOR;
var
    P10, ScaleF, P20, ScaleG: TXMVECTOR;
begin
    P10 := XMVectorSubtract(Position1, Position0);
    ScaleF := XMVectorReplicate(f);

    P20 := XMVectorSubtract(Position2, Position0);
    ScaleG := XMVectorReplicate(g);

    Result := XMVectorMultiplyAdd(P10, ScaleF, Position0);
    Result := XMVectorMultiplyAdd(P20, ScaleG, Result);
end;

// Result  :=  Position0 + f * (Position1 - Position0) + g * (Position2 - Position0)

function XMVectorBaryCentricV(constref Position0: TXMVECTOR; constref Position1: TXMVECTOR; constref Position2: TXMVECTOR;
    constref F: TXMVECTOR; constref G: TXMVECTOR): TXMVECTOR;
var
    P10, P20: TXMVECTOR;
begin
    P10 := XMVectorSubtract(Position1, Position0);
    P20 := XMVectorSubtract(Position2, Position0);

    Result := XMVectorMultiplyAdd(P10, F, Position0);
    Result := XMVectorMultiplyAdd(P20, G, Result);
end;




{***************************************************************************
 *
 * 2D Vector
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------


function XMVector2Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] = V2.f32[0]) and (V1.f32[1] = V2.f32[1]));
end;



function XMVector2EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;
    if ((V1.f32[0] = V2.f32[0]) and (V1.f32[1] = V2.f32[1])) then
    begin
        Result := XM_CRMASK_CR6TRUE;
    end
    else if ((V1.f32[0] <> V2.f32[0]) and (V1.f32[1] <> V2.f32[1])) then
    begin
        Result := XM_CRMASK_CR6FALSE;
    end;
end;



function XMVector2EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.u32[0] = V2.u32[0]) and (V1.u32[1] = V2.u32[1]));
end;



function XMVector2EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;
    if ((V1.u32[0] = V2.u32[0]) and (V1.u32[1] = V2.u32[1])) then
    begin
        Result := XM_CRMASK_CR6TRUE;
    end
    else if ((V1.u32[0] <> V2.u32[0]) and (V1.u32[1] <> V2.u32[1])) then
    begin
        Result := XM_CRMASK_CR6FALSE;
    end;
end;



function XMVector2NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean;
var
    dx, dy: single;
begin
    dx := abs(V1.f32[0] - V2.f32[0]);
    dy := abs(V1.f32[1] - V2.f32[1]);
    Result := ((dx <= Epsilon.f32[0]) and (dy <= Epsilon.f32[1]));
end;



function XMVector2NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] <> V2.f32[0]) or (V1.f32[1] <> V2.f32[1]));
end;



function XMVector2NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.u32[0] <> V2.u32[0]) or (V1.u32[1] <> V2.u32[1]));
end;



function XMVector2Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] > V2.f32[0]) and (V1.f32[1] > V2.f32[1]));
end;



function XMVector2GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;
    if ((V1.f32[0] > V2.f32[0]) and (V1.f32[1] > V2.f32[1])) then
    begin
        Result := XM_CRMASK_CR6TRUE;
    end
    else if ((V1.f32[0] <= V2.f32[0]) and (V1.f32[1] <= V2.f32[1])) then
    begin
        Result := XM_CRMASK_CR6FALSE;
    end;
end;



function XMVector2GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] >= V2.f32[0]) and (V1.f32[1] >= V2.f32[1]));
end;



function XMVector2GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;
    if ((V1.f32[0] >= V2.f32[0]) and (V1.f32[1] >= V2.f32[1])) then
    begin
        Result := XM_CRMASK_CR6TRUE;
    end
    else if ((V1.f32[0] < V2.f32[0]) and (V1.f32[1] < V2.f32[1])) then
    begin
        Result := XM_CRMASK_CR6FALSE;
    end;
end;



function XMVector2Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := (V1.f32[0] < V2.f32[0]) and (V1.f32[1] < V2.f32[1]);
end;



function XMVector2LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := (V1.f32[0] <= V2.f32[0]) and (V1.f32[1] <= V2.f32[1]);
end;



function XMVector2InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean;
begin
    Result := ((V.f32[0] <= Bounds.f32[0]) and (V.f32[0] >= -Bounds.f32[0]) and (V.f32[1] <= Bounds.f32[1]) and (V.f32[1] >= -Bounds.f32[1]));
end;



function XMVector2IsNaN(constref V: TXMVECTOR): boolean;
begin
    Result := (XMISNAN(V.u32[0]) or XMISNAN(V.u32[1]));
end;



function XMVector2IsInfinite(constref V: TXMVECTOR): boolean;
begin
    Result := (XMISINF(V.u32[0]) or XMISINF(V.u32[1]));
end;


//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------

function XMVector2Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
var
    f: single;
begin
    f := V1.f32[0] * V2.f32[0] + V1.f32[1] * V2.f32[1];
    Result.f32[0] := f;
    Result.f32[1] := f;
    Result.f32[2] := f;
    Result.f32[3] := f;
end;



// [ V1.x*V2.y - V1.y*V2.x, V1.x*V2.y - V1.y*V2.x ]
function XMVector2Cross(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
var
    fCross: single;
begin
    fCross := (V1.f32[0] * V2.f32[1]) - (V1.f32[1] * V2.f32[0]);
    Result.f32[0] := fCross;
    Result.f32[1] := fCross;
    Result.f32[2] := fCross;
    Result.f32[3] := fCross;
end;



function XMVector2ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector2LengthSq(V);
    Result := XMVectorReciprocalSqrtEst(Result);
end;



function XMVector2ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector2LengthSq(V);
    Result := XMVectorReciprocalSqrt(Result);
end;



function XMVector2LengthEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector2LengthSq(V);
    Result := XMVectorSqrtEst(Result);
end;



function XMVector2Length(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector2LengthSq(V);
    Result := XMVectorSqrt(Result);
end;


// XMVector2NormalizeEst uses a reciprocal estimate and
// returns QNaN on zero and infinite vectors.
function XMVector2NormalizeEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector2ReciprocalLength(V);
    Result := XMVectorMultiply(V, Result);
end;



function XMVector2Normalize(constref V: TXMVECTOR): TXMVECTOR;
var
    fLength: single;
    vResult: TXMVECTOR;
begin
    vResult := XMVector2Length(V);
    fLength := vResult.f32[0];

    // Prevent divide by zero
    if (fLength > 0) then
        fLength := 1.0 / fLength;

    Result.f32[0] := V.f32[0] * fLength;
    Result.f32[1] := V.f32[1] * fLength;
    Result.f32[2] := V.f32[2] * fLength;
    Result.f32[3] := V.f32[3] * fLength;
end;

// Return the refraction of a 2D vector
// Result  :=  RefractionIndex * Incident - Normal * (RefractionIndex * dot(Incident, Normal) +
// sqrt(1 - RefractionIndex * RefractionIndex * (1 - dot(Incident, Normal) * dot(Incident, Normal))))
function XMVector2RefractV(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR; constref RefractionIndex: TXMVECTOR): TXMVECTOR;
var
    IDotN, RX, RY: single;
begin
    IDotN := (Incident.f32[0] * Normal.f32[0]) + (Incident.f32[1] * Normal.f32[1]);
    // R  :=  1.0  - RefractionIndex * RefractionIndex * (1.0  - IDotN * IDotN)
    RY := 1.0 - (IDotN * IDotN);
    RX := 1.0 - (RY * RefractionIndex.f32[0] * RefractionIndex.f32[0]);
    RY := 1.0 - (RY * RefractionIndex.f32[1] * RefractionIndex.f32[1]);
    if (RX >= 0.0) then
    begin
        RX := (RefractionIndex.f32[0] * Incident.f32[0]) - (Normal.f32[0] * ((RefractionIndex.f32[0] * IDotN) + sqrt(RX)));
    end
    else
    begin
        RX := 0.0;
    end;
    if (RY >= 0.0) then
    begin
        RY := (RefractionIndex.f32[1] * Incident.f32[1]) - (Normal.f32[1] * ((RefractionIndex.f32[1] * IDotN) + sqrt(RY)));
    end
    else
    begin
        RY := 0.0;
    end;

    Result.f32[0] := RX;
    Result.f32[1] := RY;
    Result.f32[2] := 0.0;
    Result.f32[3] := 0.0;
end;



function XMVector2Orthogonal(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := -V.f32[1];
    Result.f32[1] := V.f32[0];
    Result.f32[2] := 0.0;
    Result.f32[3] := 0.0;
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
var
    Y, X: TXMVECTOR;
begin
    Y := XMVectorSplatY(V);
    X := XMVectorSplatX(V);

    Result := XMVectorMultiplyAdd(Y, M.r[1], M.r[3]);
    Result := XMVectorMultiplyAdd(X, M.r[0], Result);
end;



function XMVector2TransformStream(out pOutputStream: PXMFLOAT4; constref OutputStride: size_t; constref pInputStream: PXMFLOAT2;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT4;
var
    pInputVector: array of TXMFLOAT2 absolute pInputStream;
    pOutputVector: array of TXMFLOAT4;
    row0, row1, row3: TXMVECTOR;
    i: size_t;
    V, Y, X, vResult: TXMVECTOR;
begin
    assert(pOutputStream <> nil);
    assert(pInputStream <> nil);

    assert(InputStride >= sizeof(TXMFLOAT2));
    assert(OutputStride >= sizeof(TXMFLOAT4));

    row0 := M.r[0];
    row1 := M.r[1];
    row3 := M.r[3];

    setLength(pOutputVector, VectorCount);

    for i := 0 to VectorCount - 1 do
    begin
        V := XMLoadFloat2(pInputVector[i]);
        Y := XMVectorSplatY(V);
        X := XMVectorSplatX(V);

        vResult := XMVectorMultiplyAdd(Y, row1, row3);
        vResult := XMVectorMultiplyAdd(X, row0, vResult);

        XMStoreFloat4(pOutputVector[i], vResult);
    end;
    pOutputStream := @pOutputVector;
    Result := pOutputStream;
end;



function XMVector2TransformCoordStream(out pOutputStream: PXMFLOAT2; constref OutputStride: size_t; constref pInputStream: PXMFLOAT2;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT2;
var
    pInputVector: array of TXMFLOAT2 absolute pInputStream;
    pOutputVector: array of TXMFLOAT2;
    row0, row1, row3: TXMVECTOR;
    i: size_t;
    vResult, W, V, Y, X: TXMVECTOR;
begin
    assert(pOutputStream <> nil);
    assert(pInputStream <> nil);

    assert(InputStride >= sizeof(TXMFLOAT2));


    assert(OutputStride >= sizeof(TXMFLOAT2));
    SetLength(pOutputVector, VectorCount);


    row0 := M.r[0];
    row1 := M.r[1];
    row3 := M.r[3];


    for i := 0 to VectorCount - 1 do
    begin
        V := XMLoadFloat2(pInputVector[i]);
        Y := XMVectorSplatY(V);
        X := XMVectorSplatX(V);

        vResult := XMVectorMultiplyAdd(Y, row1, row3);
        vResult := XMVectorMultiplyAdd(X, row0, vResult);

        W := XMVectorSplatW(vResult);

        vResult := XMVectorDivide(vResult, W);

        XMStoreFloat2(pOutputVector[i], vResult);
    end;
    pOutputStream := @pOutputVector;
    Result := pOutputStream;
end;



function XMVector2TransformNormal(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
var
    X, Y: TXMVECTOR;
begin
    Y := XMVectorSplatY(V);
    X := XMVectorSplatX(V);

    Result := XMVectorMultiply(Y, M.r[1]);
    Result := XMVectorMultiplyAdd(X, M.r[0], Result);
end;



function XMVector2TransformNormalStream(out pOutputStream: PXMFLOAT2; constref OutputStride: size_t; constref pInputStream: PXMFLOAT2;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT2;
var
    pInputVector: array of TXMFLOAT2 absolute pInputStream;
    pOutputVector: array of TXMFLOAT2;
    row0, row1: TXMVECTOR;
    i: size_t;
    vResult, W, V, Y, X: TXMVECTOR;
begin
    assert(pOutputStream <> nil);
    assert(pInputStream <> nil);

    assert(InputStride >= sizeof(TXMFLOAT2));

    assert(OutputStride >= sizeof(TXMFLOAT2));
    SetLength(pOutputVector, VectorCount);

    row0 := M.r[0];
    row1 := M.r[1];

    for  i := 0 to VectorCount - 1 do
    begin
        V := XMLoadFloat2(pInputVector[i]);
        Y := XMVectorSplatY(V);
        X := XMVectorSplatX(V);

        vResult := XMVectorMultiply(Y, row1);
        vResult := XMVectorMultiplyAdd(X, row0, vResult);


        XMStoreFloat2(pOutputVector[i], vResult);
    end;
    pOutputStream := @pOutputVector;
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

function XMVector3Equal(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] = V2.f32[0]) and (V1.f32[1] = V2.f32[1]) and (V1.f32[2] = V2.f32[2]));
end;



function XMVector3EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;
    if ((V1.f32[0] = V2.f32[0]) and (V1.f32[1] = V2.f32[1]) and (V1.f32[2] = V2.f32[2])) then
    begin
        Result := XM_CRMASK_CR6TRUE;
    end
    else if ((V1.f32[0] <> V2.f32[0]) and (V1.f32[1] <> V2.f32[1]) and (V1.f32[2] <> V2.f32[2])) then
    begin
        Result := XM_CRMASK_CR6FALSE;
    end;
end;



function XMVector3EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.u32[0] = V2.u32[0]) and (V1.u32[1] = V2.u32[1]) and (V1.u32[2] = V2.u32[2]));
end;



function XMVector3EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;
    if ((V1.u32[0] = V2.u32[0]) and (V1.u32[1] = V2.u32[1]) and (V1.u32[2] = V2.u32[2])) then
    begin
        Result := XM_CRMASK_CR6TRUE;
    end
    else if ((V1.u32[0] <> V2.u32[0]) and (V1.u32[1] <> V2.u32[1]) and (V1.u32[2] <> V2.u32[2])) then
    begin
        Result := XM_CRMASK_CR6FALSE;
    end;
end;




function XMVector3NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean;
var
    dx, dy, dz: single;
begin
    dx := abs(V1.f32[0] - V2.f32[0]);
    dy := abs(V1.f32[1] - V2.f32[1]);
    dz := abs(V1.f32[2] - V2.f32[2]);
    Result := ((dx <= Epsilon.f32[0]) and (dy <= Epsilon.f32[1]) and (dz <= Epsilon.f32[2]));
end;



function XMVector3NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] <> V2.f32[0]) or (V1.f32[1] <> V2.f32[1]) or (V1.f32[2] <> V2.f32[2]));
end;



function XMVector3NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.u32[0] <> V2.u32[0]) or (V1.u32[1] <> V2.u32[1]) or (V1.u32[2] <> V2.u32[2]));
end;



function XMVector3Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] > V2.f32[0]) and (V1.f32[1] > V2.f32[1]) and (V1.f32[2] > V2.f32[2]));
end;



function XMVector3GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;
    if ((V1.f32[0] > V2.f32[0]) and (V1.f32[1] > V2.f32[1]) and (V1.f32[2] > V2.f32[2])) then
    begin
        Result := XM_CRMASK_CR6TRUE;
    end
    else if ((V1.f32[0] <= V2.f32[0]) and (V1.f32[1] <= V2.f32[1]) and (V1.f32[2] <= V2.f32[2])) then
    begin
        Result := XM_CRMASK_CR6FALSE;
    end;
end;



function XMVector3GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] >= V2.f32[0]) and (V1.f32[1] >= V2.f32[1]) and (V1.f32[2] >= V2.f32[2]));
end;



function XMVector3GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;
    if ((V1.f32[0] >= V2.f32[0]) and (V1.f32[1] >= V2.f32[1]) and (V1.f32[2] >= V2.f32[2])) then
    begin
        Result := XM_CRMASK_CR6TRUE;
    end
    else if ((V1.f32[0] < V2.f32[0]) and (V1.f32[1] < V2.f32[1]) and (V1.f32[2] < V2.f32[2])) then
    begin
        Result := XM_CRMASK_CR6FALSE;
    end;
end;



function XMVector3Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] < V2.f32[0]) and (V1.f32[1] < V2.f32[1]) and (V1.f32[2] < V2.f32[2]));
end;



function XMVector3LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] <= V2.f32[0]) and (V1.f32[1] <= V2.f32[1]) and (V1.f32[2] <= V2.f32[2]));
end;



function XMVector3InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean;
begin
    Result := ((V.f32[0] <= Bounds.f32[0]) and (V.f32[0] >= -Bounds.f32[0]) and (V.f32[1] <= Bounds.f32[1]) and
        (V.f32[1] >= -Bounds.f32[1]) and (V.f32[2] <= Bounds.f32[2]) and (V.f32[2] >= -Bounds.f32[2]));
end;



function XMVector3IsNaN(constref V: TXMVECTOR): boolean;
begin
    Result := (XMISNAN(V.u32[0]) or XMISNAN(V.u32[1]) or XMISNAN(V.u32[2]));
end;



function XMVector3IsInfinite(constref V: TXMVECTOR): boolean;
begin
    Result := (XMISINF(V.u32[0]) or XMISINF(V.u32[1]) or XMISINF(V.u32[2]));
end;


//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------

function XMVector3Dot(const V1: TXMVECTOR; const V2: TXMVECTOR): TXMVECTOR;
var
    fValue: single;
begin
    fvalue:=(V1.f32[0] * V2.f32[0]) + (V1.f32[1] * V2.f32[1]) + (V1.f32[2] * V2.f32[2]);
    Result.f32[0] := fValue;
    Result.f32[1] := fValue;
    Result.f32[2] := fValue;
    Result.f32[3] := fValue;
end;

// [ V1.y*V2.z - V1.z*V2.y, V1.z*V2.x - V1.x*V2.z, V1.x*V2.y - V1.y*V2.x ]

function XMVector3Cross(const V1: TXMVECTOR; const V2: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := (V1.f32[1] * V2.f32[2]) - (V1.f32[2] * V2.f32[1]);
    Result.f32[1] := (V1.f32[2] * V2.f32[0]) - (V1.f32[0] * V2.f32[2]);
    Result.f32[2] := (V1.f32[0] * V2.f32[1]) - (V1.f32[1] * V2.f32[0]);
    Result.f32[2] := 0.0;
end;



function XMVector3ReciprocalLength(const V: TXMVECTOR): TXMVECTOR;
var
    x:TXMVECTOR;
begin
    x := XMVector3LengthSq(V);
    Result := XMVectorReciprocalSqrt(x);
end;



function XMVector3ReciprocalLengthEst(const V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector3LengthSq(V);
    Result := XMVectorReciprocalSqrtEst(Result);
end;



function XMVector3LengthEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector3LengthSq(V);
    Result := XMVectorSqrtEst(Result);
end;



function XMVector3Length(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector3LengthSq(V);
    Result := XMVectorSqrt(Result);
end;


// XMVector3NormalizeEst uses a reciprocal estimate and
// returns QNaN on zero and infinite vectors.
function XMVector3NormalizeEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector3ReciprocalLength(V);
    Result := XMVectorMultiply(V, Result);
end;



function XMVector3Normalize(constref V: TXMVECTOR): TXMVECTOR;
var
    fLength: single;
    vResult: TXMVECTOR;
begin
    vResult := XMVector3Length(V);
    fLength := vResult.f32[0];

    // Prevent divide by zero
    if (fLength > 0) then
        fLength := 1.0 / fLength;

    Result.f32[0] := V.f32[0] * fLength;
    Result.f32[1] := V.f32[1] * fLength;
    Result.f32[2] := V.f32[2] * fLength;
    Result.f32[3] := V.f32[3] * fLength;
end;



function XMVector3RefractV(constref Incident: TXMVECTOR; constref Normal: TXMVECTOR; constref RefractionIndex: TXMVECTOR): TXMVECTOR;
var
    Zero, IDotN, R: TXMVECTOR;
begin
    // Result  :=  RefractionIndex * Incident - Normal * (RefractionIndex * dot(Incident, Normal) +
    // sqrt(1 - RefractionIndex * RefractionIndex * (1 - dot(Incident, Normal) * dot(Incident, Normal))))
    Zero := XMVectorZero();

    IDotN := XMVector3Dot(Incident, Normal);

    // R  :=  1.0  - RefractionIndex * RefractionIndex * (1.0  - IDotN * IDotN)
    R := XMVectorNegativeMultiplySubtract(IDotN, IDotN, g_XMOne.v);
    R := XMVectorMultiply(R, RefractionIndex);
    R := XMVectorNegativeMultiplySubtract(R, RefractionIndex, g_XMOne.v);

    if (XMVector4LessOrEqual(R, Zero)) then
    begin
        // Total internal reflection
        Result := Zero;
    end
    else
    begin
        // R  :=  RefractionIndex * IDotN + sqrt(R)
        R := XMVectorSqrt(R);
        R := XMVectorMultiplyAdd(RefractionIndex, IDotN, R);

        // Result  :=  RefractionIndex * Incident - Normal * R
        Result := XMVectorMultiply(RefractionIndex, Incident);
        Result := XMVectorNegativeMultiplySubtract(Normal, R, Result);
    end;
end;



function XMVector3Transform(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
var
    X, Y, Z: TXMVECTOR;
begin
    Z := XMVectorSplatZ(V);
    Y := XMVectorSplatY(V);
    X := XMVectorSplatX(V);

    Result := XMVectorMultiplyAdd(Z, M.r[2], M.r[3]);
    Result := XMVectorMultiplyAdd(Y, M.r[1], Result);
    Result := XMVectorMultiplyAdd(X, M.r[0], Result);
end;



function XMVector3TransformStream(out pOutputStream: PXMFLOAT4; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT4;
var
    pInputVector: array of TXMFLOAT3 absolute pInputStream;
    pOutputVector: array of TXMFLOAT4;
    row0, row1, row2, row3: TXMVECTOR;
    i: size_t;
    V, Z, X, Y: TXMVECTOR;
    vResult: TXMVECTOR;
begin
    assert(pOutputStream <> nil);
    assert(pInputStream <> nil);

    assert(InputStride >= sizeof(TXMFLOAT3));

    assert(OutputStride >= sizeof(TXMFLOAT4));


    SetLength(pOutputVector, VectorCount);
    row0 := M.r[0];
    row1 := M.r[1];
    row2 := M.r[2];
    row3 := M.r[3];

    for i := 0 to VectorCount - 1 do
    begin
        V := XMLoadFloat3(pInputVector[i]);
        Z := XMVectorSplatZ(V);
        Y := XMVectorSplatY(V);
        X := XMVectorSplatX(V);

        vResult := XMVectorMultiplyAdd(Z, row2, row3);
        vResult := XMVectorMultiplyAdd(Y, row1, vResult);
        vResult := XMVectorMultiplyAdd(X, row0, vResult);

        XMStoreFloat4(pOutputVector[i], vResult);
    end;
    pOutputStream := @pOutputVector;
    Result := pOutputStream;
end;



function XMVector3TransformCoordStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT3;
var
    pInputVector: array of TXMFLOAT3 absolute pInputStream;
    pOutputVector: array of TXMFLOAT3;
    row0, row1, row2, row3: TXMVECTOR;
    i: size_t;
    V, Z, X, Y, W: TXMVECTOR;
    vResult: TXMVECTOR;
begin
    assert(pOutputStream <> nil);
    assert(pInputStream <> nil);

    assert(InputStride >= sizeof(TXMFLOAT3));

    assert(OutputStride >= sizeof(TXMFLOAT3));

    SetLength(pOutputVector, VectorCount);


    row0 := M.r[0];
    row1 := M.r[1];
    row2 := M.r[2];
    row3 := M.r[3];

    for  i := 0 to VectorCount - 1 do
    begin
        V := XMLoadFloat3(pInputVector[i]);
        Z := XMVectorSplatZ(V);
        Y := XMVectorSplatY(V);
        X := XMVectorSplatX(V);

        vResult := XMVectorMultiplyAdd(Z, row2, row3);
        vResult := XMVectorMultiplyAdd(Y, row1, vResult);
        vResult := XMVectorMultiplyAdd(X, row0, vResult);

        W := XMVectorSplatW(vResult);

        vResult := XMVectorDivide(vResult, W);

        XMStoreFloat3(pOutputVector[i], vResult);
    end;
    pOutputStream := @pOutputVector;
    Result := pOutputStream;
end;



function XMVector3TransformNormal(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
var
    X, Y, Z: TXMVECTOR;
begin
    Z := XMVectorSplatZ(V);
    Y := XMVectorSplatY(V);
    X := XMVectorSplatX(V);

    Result := XMVectorMultiply(Z, M.r[2]);
    Result := XMVectorMultiplyAdd(Y, M.r[1], Result);
    Result := XMVectorMultiplyAdd(X, M.r[0], Result);
end;



function XMVector3TransformNormalStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT3;
var
    pInputVector: array of TXMFLOAT3 absolute pInputStream;
    pOutputVector: array of TXMFLOAT3;
    row0, row1, row2, row3: TXMVECTOR;
    i: size_t;
    V, Z, X, Y: TXMVECTOR;
    vResult: TXMVECTOR;
begin
    assert(pOutputStream <> nil);
    assert(pInputStream <> nil);

    assert(InputStride >= sizeof(TXMFLOAT3));

    assert(OutputStride >= sizeof(TXMFLOAT3));

    SetLength(pOutputVector, VectorCount);


    row0 := M.r[0];
    row1 := M.r[1];
    row2 := M.r[2];

    for i := 0 to VectorCount - 1 do
    begin
        V := XMLoadFloat3(pInputVector[i]);
        Z := XMVectorSplatZ(V);
        Y := XMVectorSplatY(V);
        X := XMVectorSplatX(V);

        vResult := XMVectorMultiply(Z, row2);
        vResult := XMVectorMultiplyAdd(Y, row1, vResult);
        vResult := XMVectorMultiplyAdd(X, row0, vResult);

        XMStoreFloat3(pOutputVector[i], vResult);
    end;
    pOutputStream := @pOutputVector;
    Result := pOutputStream;
end;



function XMVector3ProjectStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref ViewportX: single; constref ViewportY: single;
    constref ViewportWidth: single; constref ViewportHeight: single; constref ViewportMinZ: single; constref ViewportMaxZ: single;
    constref Projection: TXMMATRIX; constref View: TXMMATRIX; constref World: TXMMATRIX): PXMFLOAT3;
var
    HalfViewportWidth: single;
    HalfViewportHeight: single;

    Scale, Offset, V, vResult: TXMVECTOR;
    Transform: TXMMATRIX;
    i: size_t;

    pInputVector: array of TXMFLOAT3 absolute pInputStream;
    pOutputVector: array of TXMFLOAT3;
begin
    assert(pOutputStream <> nil);
    assert(pInputStream <> nil);
    assert(InputStride >= sizeof(TXMFLOAT3));
    assert(OutputStride >= sizeof(TXMFLOAT3));

    HalfViewportWidth := ViewportWidth * 0.5;
    HalfViewportHeight := ViewportHeight * 0.5;

    Scale := XMVectorSet(HalfViewportWidth, -HalfViewportHeight, ViewportMaxZ - ViewportMinZ, 1.0);
    Offset := XMVectorSet(ViewportX + HalfViewportWidth, ViewportY + HalfViewportHeight, ViewportMinZ, 0.0);

    Transform := XMMatrixMultiply(World, View);
    Transform := XMMatrixMultiply(Transform, Projection);

    SetLength(pOutputVector, VectorCount);

    for i := 0 to VectorCount - 1 do
    begin
        V := XMLoadFloat3(pInputVector[i]);
        vResult := XMVector3TransformCoord(V, Transform);
        vResult := XMVectorMultiplyAdd(vResult, Scale, Offset);
        XMStoreFloat3(pOutputVector[i], vResult);
    end;

    pOutputStream := @pOutputVector[0];
    Result := @pOutputVector[0];
end;



function XMVector3UnprojectStream(out pOutputStream: PXMFLOAT3; constref OutputStride: size_t; constref pInputStream: PXMFLOAT3;
    constref InputStride: size_t; constref VectorCount: size_t; constref ViewportX: single; constref ViewportY: single;
    constref ViewportWidth: single; constref ViewportHeight: single; constref ViewportMinZ: single; constref ViewportMaxZ: single;
    constref Projection: TXMMATRIX; constref View: TXMMATRIX; constref World: TXMMATRIX): PXMFLOAT3;
const
    D: TXMVECTORF32 = (f: (-1.0, 1.0, 0.0, 0.0));
var
    i: size_t;
    Scale, Offset: TXMVECTOR;
    Transform: TXMMATRIX;
    V, vResult: TXMVECTOR;
    det: TXMVECTOR;
    pInputVector: array of TXMFLOAT3 absolute pInputStream;
    pOutputVector: array of TXMFLOAT3;

begin
    assert(pOutputStream <> nil);
    assert(pInputStream <> nil);
    assert(InputStride >= sizeof(TXMFLOAT3));
    assert(OutputStride >= sizeof(TXMFLOAT3));

    Scale := XMVectorSet(ViewportWidth * 0.5, -ViewportHeight * 0.5, ViewportMaxZ - ViewportMinZ, 1.0);
    Scale := XMVectorReciprocal(Scale);

    Offset := XMVectorSet(-ViewportX, -ViewportY, -ViewportMinZ, 0.0);
    Offset := XMVectorMultiplyAdd(Scale, Offset, D.v);

    Transform := XMMatrixMultiply(World, View);
    Transform := XMMatrixMultiply(Transform, Projection);
    Transform := XMMatrixInverse(det, Transform);


    SetLength(pOutputVector, VectorCount);

    for i := 0 to VectorCount - 1 do
    begin
        V := XMLoadFloat3(pInputVector[i]);
        vResult := XMVectorMultiplyAdd(V, Scale, Offset);
        vResult := XMVector3TransformCoord(vResult, Transform);
        XMStoreFloat3(pOutputVector[i], vResult);
    end;
    pOutputStream := @pOutputVector;
    Result := @pOutputVector;
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
    Result := ((V1.f32[0] = V2.f32[0]) and (V1.f32[1] = V2.f32[1]) and (V1.f32[2] = V2.f32[2]) and (V1.f32[3] = V2.f32[3]));
end;



function XMVector4EqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;

    if ((V1.f32[0] = V2.f32[0]) and (V1.f32[1] = V2.f32[1]) and (V1.f32[2] = V2.f32[2]) and (V1.f32[3] = V2.f32[3])) then
        Result := XM_CRMASK_CR6TRUE
    else if ((V1.f32[0] <> V2.f32[0]) and (V1.f32[1] <> V2.f32[1]) and (V1.f32[2] <> V2.f32[2]) and (V1.f32[3] <> V2.f32[3])) then
        Result := XM_CRMASK_CR6FALSE;
end;



function XMVector4EqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.u32[0] = V2.u32[0]) and (V1.u32[1] = V2.u32[1]) and (V1.u32[2] = V2.u32[2]) and (V1.u32[3] = V2.u32[3]));
end;



function XMVector4EqualIntR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;
    if ((V1.u32[0] = V2.u32[0]) and (V1.u32[1] = V2.u32[1]) and (V1.u32[2] = V2.u32[2]) and (V1.u32[3] = V2.u32[3])) then
        Result := XM_CRMASK_CR6TRUE
    else if ((V1.u32[0] <> V2.u32[0]) and (V1.u32[1] <> V2.u32[1]) and (V1.u32[2] <> V2.u32[2]) and (V1.u32[3] <> V2.u32[3])) then
        Result := XM_CRMASK_CR6FALSE;
end;



function XMVector4NearEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref Epsilon: TXMVECTOR): boolean;
var
    dx, dy, dz, dw: single;
begin
    dx := abs(V1.f32[0] - V2.f32[0]);
    dy := abs(V1.f32[1] - V2.f32[1]);
    dz := abs(V1.f32[2] - V2.f32[2]);
    dw := abs(V1.f32[3] - V2.f32[3]);
    Result := ((dx <= Epsilon.f32[0]) and (dy <= Epsilon.f32[1]) and (dz <= Epsilon.f32[2]) and (dw <= Epsilon.f32[3]));
end;


// Returns true if the 4D vectors are not equal and false otherwise.
function XMVector4NotEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] <> V2.f32[0]) or (V1.f32[1] <> V2.f32[1]) or (V1.f32[2] <> V2.f32[2]) or (V1.f32[3] <> V2.f32[3]));
end;


// Test whether two 4D vectors are not equal, treating each component as an unsigned integer.
// Returns true if the 4D vectors are not equal and false otherwise.
function XMVector4NotEqualInt(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.u32[0] <> V2.u32[0]) or (V1.u32[1] <> V2.u32[1]) or (V1.u32[2] <> V2.u32[2]) or (V1.u32[3] <> V2.u32[3]));
end;



function XMVector4Greater(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] > V2.f32[0]) and (V1.f32[1] > V2.f32[1]) and (V1.f32[2] > V2.f32[2]) and (V1.f32[3] > V2.f32[3]));
end;


// Tests whether one 4D vector is greater than another 4D vector and returns a comparison value that can
// be examined using functions such as XMComparisonAllTrue.
function XMVector4GreaterR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;
    if ((V1.f32[0] > V2.f32[0]) and (V1.f32[1] > V2.f32[1]) and (V1.f32[2] > V2.f32[2]) and (V1.f32[3] > V2.f32[3])) then
        Result := XM_CRMASK_CR6TRUE
    else if ((V1.f32[0] <= V2.f32[0]) and (V1.f32[1] <= V2.f32[1]) and (V1.f32[2] <= V2.f32[2]) and (V1.f32[3] <= V2.f32[3])) then
        Result := XM_CRMASK_CR6FALSE;
end;


// Returns true if V1 is greater-than-or-equal-to V2 and false otherwise
function XMVector4GreaterOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] >= V2.f32[0]) and (V1.f32[1] >= V2.f32[1]) and (V1.f32[2] >= V2.f32[2]) and (V1.f32[3] >= V2.f32[3]));
end;


// Tests whether one 4D vector is greater-than-or-equal-to another 4D vector and returns a comparison value that
// can be examined using functions such as XMComparisonAllTrue.

function XMVector4GreaterOrEqualR(constref V1: TXMVECTOR; constref V2: TXMVECTOR): UINT32;
begin
    Result := 0;
    if ((V1.f32[0] >= V2.f32[0]) and (V1.f32[1] >= V2.f32[1]) and (V1.f32[2] >= V2.f32[2]) and (V1.f32[3] >= V2.f32[3])) then
        Result := XM_CRMASK_CR6TRUE
    else if ((V1.f32[0] < V2.f32[0]) and (V1.f32[1] < V2.f32[1]) and (V1.f32[2] < V2.f32[2]) and (V1.f32[3] < V2.f32[3])) then
        Result := XM_CRMASK_CR6FALSE;
end;



function XMVector4Less(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] < V2.f32[0]) and (V1.f32[1] < V2.f32[1]) and (V1.f32[2] < V2.f32[2]) and (V1.f32[3] < V2.f32[3]));
end;



function XMVector4LessOrEqual(constref V1: TXMVECTOR; constref V2: TXMVECTOR): boolean;
begin
    Result := ((V1.f32[0] <= V2.f32[0]) and (V1.f32[1] <= V2.f32[1]) and (V1.f32[2] <= V2.f32[2]) and (V1.f32[3] <= V2.f32[3]));
end;



function XMVector4InBounds(constref V: TXMVECTOR; constref Bounds: TXMVECTOR): boolean;
begin
    Result :=
        ((V.f32[0] <= Bounds.f32[0]) and (V.f32[0] >= -Bounds.f32[0])) and ((V.f32[1] <= Bounds.f32[1]) and (V.f32[1] >= -Bounds.f32[1])) and
        ((V.f32[2] <= Bounds.f32[2]) and (V.f32[2] >= -Bounds.f32[2])) and ((V.f32[3] <= Bounds.f32[3]) and (V.f32[3] >= -Bounds.f32[3]));
end;



function XMVector4IsNaN(constref V: TXMVECTOR): boolean;
begin
    Result := XMISNAN(V.u32[0]) or XMISNAN(V.u32[1]) or XMISNAN(V.u32[2]) or XMISNAN(V.u32[3]);
end;



function XMVector4IsInfinite(constref V: TXMVECTOR): boolean;
begin
    Result := XMISINF(V.u32[0]) or XMISINF(V.u32[1]) or XMISINF(V.u32[2]) or XMISINF(V.u32[3]);
end;



//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------


function XMVector4Dot(constref V1: TXMVECTOR; constref V2: TXMVECTOR): TXMVECTOR;
var
    f: single;
begin
    f := V1.f32[0] * V2.f32[0] + V1.f32[1] * V2.f32[1] + V1.f32[2] * V2.f32[2] + V1.f32[3] * V2.f32[3];
    Result.f32[0] := f;
    Result.f32[1] := f;
    Result.f32[2] := f;
    Result.f32[3] := f;
end;


// [ ((v2.z*v3.w-v2.w*v3.z)*v1.y)-((v2.y*v3.w-v2.w*v3.y)*v1.z)+((v2.y*v3.z-v2.z*v3.y)*v1.w),
//   ((v2.w*v3.z-v2.z*v3.w)*v1.x)-((v2.w*v3.x-v2.x*v3.w)*v1.z)+((v2.z*v3.x-v2.x*v3.z)*v1.w),
//   ((v2.y*v3.w-v2.w*v3.y)*v1.x)-((v2.x*v3.w-v2.w*v3.x)*v1.y)+((v2.x*v3.y-v2.y*v3.x)*v1.w),
//   ((v2.z*v3.y-v2.y*v3.z)*v1.x)-((v2.z*v3.x-v2.x*v3.z)*v1.y)+((v2.y*v3.x-v2.x*v3.y)*v1.z) ]
function XMVector4Cross(constref V1: TXMVECTOR; constref V2: TXMVECTOR; constref V3: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := (((V2.f32[2] * V3.f32[3]) - (V2.f32[3] * V3.f32[2])) * V1.f32[1]) -
        (((V2.f32[1] * V3.f32[3]) - (V2.f32[3] * V3.f32[1])) * V1.f32[2]) + (((V2.f32[1] * V3.f32[2]) - (V2.f32[2] * V3.f32[1])) * V1.f32[3]);
    Result.f32[1] := (((V2.f32[3] * V3.f32[2]) - (V2.f32[2] * V3.f32[3])) * V1.f32[0]) -
        (((V2.f32[3] * V3.f32[0]) - (V2.f32[0] * V3.f32[3])) * V1.f32[2]) + (((V2.f32[2] * V3.f32[0]) - (V2.f32[0] * V3.f32[2])) * V1.f32[3]);
    Result.f32[2] := (((V2.f32[1] * V3.f32[3]) - (V2.f32[3] * V3.f32[1])) * V1.f32[0]) -
        (((V2.f32[0] * V3.f32[3]) - (V2.f32[3] * V3.f32[0])) * V1.f32[1]) + (((V2.f32[0] * V3.f32[1]) - (V2.f32[1] * V3.f32[0])) * V1.f32[3]);
    Result.f32[3] := (((V2.f32[2] * V3.f32[1]) - (V2.f32[1] * V3.f32[2])) * V1.f32[0]) -
        (((V2.f32[2] * V3.f32[0]) - (V2.f32[0] * V3.f32[2])) * V1.f32[1]) + (((V2.f32[1] * V3.f32[0]) - (V2.f32[0] * V3.f32[1])) * V1.f32[2]);
end;



function XMVector4ReciprocalLengthEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4LengthSq(V);
    Result := XMVectorReciprocalSqrtEst(Result);
end;



function XMVector4ReciprocalLength(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4LengthSq(V);
    Result := XMVectorReciprocalSqrt(Result);
end;



function XMVector4LengthEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4LengthSq(V);
    Result := XMVectorSqrtEst(Result);
end;



function XMVector4Length(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4LengthSq(V);
    Result := XMVectorSqrt(Result);
end;



// XMVector4NormalizeEst uses a reciprocal estimate and
// returns QNaN on zero and infinite vectors.
function XMVector4NormalizeEst(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result := XMVector4ReciprocalLength(V);
    Result := XMVectorMultiply(V, Result);
end;



function XMVector4Normalize(constref V: TXMVECTOR): TXMVECTOR;
var
    fLength: single;
    vResult: TXMVECTOR;
begin
    vResult := XMVector4Length(V);
    fLength := vResult.f32[0];

    // Prevent divide by zero
    if (fLength > 0) then
        fLength := 1.0 / fLength;

    Result.f32[0] := V.f32[0] * fLength;
    Result.f32[1] := V.f32[1] * fLength;
    Result.f32[2] := V.f32[2] * fLength;
    Result.f32[3] := V.f32[3] * fLength;
end;



function XMVector4RefractV(Incident: TXMVECTOR; Normal: TXMVECTOR; RefractionIndex: TXMVECTOR): TXMVECTOR;
var
    IDotN: TXMVECTOR;
    R: TXMVECTOR;
    Zero: TXMVECTOR;
begin
    Zero := XMVectorZero();

    // Result  :=  RefractionIndex * Incident - Normal * (RefractionIndex * dot(Incident, Normal) +
    // sqrt(1 - RefractionIndex * RefractionIndex * (1 - dot(Incident, Normal) * dot(Incident, Normal))))

    IDotN := XMVector4Dot(Incident, Normal);

    // R  :=  1.0f - RefractionIndex * RefractionIndex * (1.0f - IDotN * IDotN)
    R := XMVectorNegativeMultiplySubtract(IDotN, IDotN, g_XMOne.v);
    R := XMVectorMultiply(R, RefractionIndex);
    R := XMVectorNegativeMultiplySubtract(R, RefractionIndex, g_XMOne.v);

    if (XMVector4LessOrEqual(R, Zero)) then
    begin
        // Total internal reflection
        Result := Zero;
    end
    else
    begin
        // R  :=  RefractionIndex * IDotN + sqrt(R)
        R := XMVectorSqrt(R);
        R := XMVectorMultiplyAdd(RefractionIndex, IDotN, R);

        // Result  :=  RefractionIndex * Incident - Normal * R
        Result := XMVectorMultiply(RefractionIndex, Incident);
        Result := XMVectorNegativeMultiplySubtract(Normal, R, Result);
    end;
end;



function XMVector4Orthogonal(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := V.f32[2];
    Result.f32[1] := V.f32[3];
    Result.f32[2] := -V.f32[0];
    Result.f32[3] := -V.f32[1];
end;



function XMVector4Transform(constref V: TXMVECTOR; constref M: TXMMATRIX): TXMVECTOR;
begin
    Result.f32[0] := (M.m[0, 0] * V.f32[0]) + (M.m[1, 0] * V.f32[1]) + (M.m[2, 0] * V.f32[2]) + (M.m[3, 0] * V.f32[3]);
    Result.f32[1] := (M.m[0, 1] * V.f32[0]) + (M.m[1, 1] * V.f32[1]) + (M.m[2, 1] * V.f32[2]) + (M.m[3, 1] * V.f32[3]);
    Result.f32[2] := (M.m[0, 2] * V.f32[0]) + (M.m[1, 2] * V.f32[1]) + (M.m[2, 2] * V.f32[2]) + (M.m[3, 2] * V.f32[3]);
    Result.f32[3] := (M.m[0, 3] * V.f32[0]) + (M.m[1, 3] * V.f32[1]) + (M.m[2, 3] * V.f32[2]) + (M.m[3, 3] * V.f32[3]);
end;



function XMVector4TransformStream(out pOutputStream: PXMFLOAT4; constref OutputStride: size_t; constref pInputStream: PXMFLOAT4;
    constref InputStride: size_t; constref VectorCount: size_t; constref M: TXMMATRIX): PXMFLOAT4;
var
    row0, row1, row2, row3: TXMVECTOR;
    i: size_t;
    vResult, V, W, Z, Y, X: TXMVECTOR;
    pInputVector: array of TXMFloat4 absolute pInputStream;
    pOutputVector: array of TXMFloat4;
begin
    assert(pOutputStream <> nil);
    assert(pInputStream <> nil);
    assert(InputStride >= sizeof(TXMFLOAT4));
    assert(OutputStride >= sizeof(TXMFLOAT4));

    SetLength(pOutputVector, VectorCount);

    row0 := M.r[0];
    row1 := M.r[1];
    row2 := M.r[2];
    row3 := M.r[3];

    for  i := 0 to VectorCount - 1 do
    begin
        V := XMLoadFloat4(pInputVector[i]);
        W := XMVectorSplatW(V);
        Z := XMVectorSplatZ(V);
        Y := XMVectorSplatY(V);
        X := XMVectorSplatX(V);

        vResult := XMVectorMultiply(W, row3);
        vResult := XMVectorMultiplyAdd(Z, row2, vResult);
        vResult := XMVectorMultiplyAdd(Y, row1, vResult);
        vResult := XMVectorMultiplyAdd(X, row0, vResult);
        XMStoreFloat4(pOutputVector[i], vResult);
    end;
    pOutputStream := @pOutputVector;
    Result := @pOutputVector;
end;



{***************************************************************************
 *
 * Matrix
 *
 ***************************************************************************}

//------------------------------------------------------------------------------
// Comparison operations
//------------------------------------------------------------------------------

// Return true if any entry in the matrix is NaN
function XMMatrixIsNaN(constref M: TXMMATRIX): boolean;
var
    i: size_t = 16;
    pWork: uint32;
    uTest: uint32;
begin
    while i > 0 do
    begin
        // Fetch value into integer unit
        uTest := M.u[i - 1];
        // Remove sign
        uTest := uTest and $7FFFFFFF;
        // NaN is 0x7F800001 through 0x7FFFFFFF inclusive
        uTest := uTest - $7F800001;
        if (uTest < $007FFFFF) then
            break;      // NaN found
        Dec(i); // Next entry
    end;
    Result := (i <> 0);      // i == 0 if nothing matched
end;

// Return true if any entry in the matrix is +/-INF

function XMMatrixIsInfinite(M: TXMMATRIX): boolean;
var
    i: size_t = 16;
    uTest: UINT32;
begin
    while i > 0 do
    begin
        // Fetch value into integer unit
        uTest := M.u[i];
        // Remove sign
        uTest := uTest and $7FFFFFFF;
        // INF is $7F800000
        if (uTest = $7F800000) then
            break;      // INF found
        Dec(i);// Next entry
    end;
    Result := (i <> 0);      // i == 0 if nothing matched
end;


// Return true if the XMMatrix is equal to identity
function XMMatrixIsIdentity(M: TXMMATRIX): boolean;
var
    uOne, uZero: UINT32;
begin
    // Use the integer pipeline to reduce branching to a minimum
    // Convert 1.0f to zero and or them together
    uOne := M.u[0] xor $3F800000;
    // Or all the 0.0f entries together
    uZero := M.u[1];
    uZero := uZero or M.u[2];
    uZero := uZero or M.u[3];
    // 2nd row
    uZero := uZero or M.u[4];
    uOne := uOne or (M.u[5] xor $3F800000);
    uZero := uZero or M.u[6];
    uZero := uZero or M.u[7];
    // 3rd row
    uZero := uZero or M.u[8];
    uZero := uZero or M.u[9];
    uOne := uOne or (M.u[10] xor $3F800000);
    uZero := uZero or M.u[11];
    // 4th row
    uZero := uZero or M.u[12];
    uZero := uZero or M.u[13];
    uZero := uZero or M.u[14];
    uOne := uOne or (M.u[15] xor $3F800000);
    // If all zero entries are zero, the uZero==0
    uZero := uZero and $7FFFFFFF;    // Allow -0.0f
    // If all 1.0f entries are 1.0f, then uOne==0
    uOne := uOne or uZero;
    Result := (uOne = 0);
end;




//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------

// Perform a 4x4 matrix multiply by a 4x4 matrix
function XMMatrixMultiply(M1: TXMMATRIX; M2: TXMMATRIX): TXMMATRIX;
var
    x, y, z, w: single;
begin
    // Cache the invariants in registers
    x := M1.m[0, 0];
    y := M1.m[0, 1];
    z := M1.m[0, 2];
    w := M1.m[0, 3];
    // Perform the operation on the first row
    Result.m[0, 0] := (M2.m[0, 0] * x) + (M2.m[1, 0] * y) + (M2.m[2, 0] * z) + (M2.m[3, 0] * w);
    Result.m[0, 1] := (M2.m[0, 1] * x) + (M2.m[1, 1] * y) + (M2.m[2, 1] * z) + (M2.m[3, 1] * w);
    Result.m[0, 2] := (M2.m[0, 2] * x) + (M2.m[1, 2] * y) + (M2.m[2, 2] * z) + (M2.m[3, 2] * w);
    Result.m[0, 3] := (M2.m[0, 3] * x) + (M2.m[1, 3] * y) + (M2.m[2, 3] * z) + (M2.m[3, 3] * w);
    // Repeat for all the other rows
    x := M1.m[1, 0];
    y := M1.m[1, 1];
    z := M1.m[1, 2];
    w := M1.m[1, 3];
    Result.m[1, 0] := (M2.m[0, 0] * x) + (M2.m[1, 0] * y) + (M2.m[2, 0] * z) + (M2.m[3, 0] * w);
    Result.m[1, 1] := (M2.m[0, 1] * x) + (M2.m[1, 1] * y) + (M2.m[2, 1] * z) + (M2.m[3, 1] * w);
    Result.m[1, 2] := (M2.m[0, 2] * x) + (M2.m[1, 2] * y) + (M2.m[2, 2] * z) + (M2.m[3, 2] * w);
    Result.m[1, 3] := (M2.m[0, 3] * x) + (M2.m[1, 3] * y) + (M2.m[2, 3] * z) + (M2.m[3, 3] * w);
    x := M1.m[2, 0];
    y := M1.m[2, 1];
    z := M1.m[2, 2];
    w := M1.m[2, 3];
    Result.m[2, 0] := (M2.m[0, 0] * x) + (M2.m[1, 0] * y) + (M2.m[2, 0] * z) + (M2.m[3, 0] * w);
    Result.m[2, 1] := (M2.m[0, 1] * x) + (M2.m[1, 1] * y) + (M2.m[2, 1] * z) + (M2.m[3, 1] * w);
    Result.m[2, 2] := (M2.m[0, 2] * x) + (M2.m[1, 2] * y) + (M2.m[2, 2] * z) + (M2.m[3, 2] * w);
    Result.m[2, 3] := (M2.m[0, 3] * x) + (M2.m[1, 3] * y) + (M2.m[2, 3] * z) + (M2.m[3, 3] * w);
    x := M1.m[3, 0];
    y := M1.m[3, 1];
    z := M1.m[3, 2];
    w := M1.m[3, 3];
    Result.m[3, 0] := (M2.m[0, 0] * x) + (M2.m[1, 0] * y) + (M2.m[2, 0] * z) + (M2.m[3, 0] * w);
    Result.m[3, 1] := (M2.m[0, 1] * x) + (M2.m[1, 1] * y) + (M2.m[2, 1] * z) + (M2.m[3, 1] * w);
    Result.m[3, 2] := (M2.m[0, 2] * x) + (M2.m[1, 2] * y) + (M2.m[2, 2] * z) + (M2.m[3, 2] * w);
    Result.m[3, 3] := (M2.m[0, 3] * x) + (M2.m[1, 3] * y) + (M2.m[2, 3] * z) + (M2.m[3, 3] * w);
end;



function XMMatrixMultiplyTranspose(M1: TXMMATRIX; M2: TXMMATRIX): TXMMATRIX;
var
    x, y, z, w: single;
begin
    // Cache the invariants in registers
    x := M2.m[0, 0];
    y := M2.m[1, 0];
    z := M2.m[2, 0];
    w := M2.m[3, 0];
    // Perform the operation on the first row
    Result.m[0, 0] := (M1.m[0, 0] * x) + (M1.m[0, 1] * y) + (M1.m[0, 2] * z) + (M1.m[0, 3] * w);
    Result.m[0, 1] := (M1.m[1, 0] * x) + (M1.m[1, 1] * y) + (M1.m[1, 2] * z) + (M1.m[1, 3] * w);
    Result.m[0, 2] := (M1.m[2, 0] * x) + (M1.m[2, 1] * y) + (M1.m[2, 2] * z) + (M1.m[2, 3] * w);
    Result.m[0, 3] := (M1.m[3, 0] * x) + (M1.m[3, 1] * y) + (M1.m[3, 2] * z) + (M1.m[3, 3] * w);
    // Repeat for all the other rows
    x := M2.m[0, 1];
    y := M2.m[1, 1];
    z := M2.m[2, 1];
    w := M2.m[3, 1];
    Result.m[1, 0] := (M1.m[0, 0] * x) + (M1.m[0, 1] * y) + (M1.m[0, 2] * z) + (M1.m[0, 3] * w);
    Result.m[1, 1] := (M1.m[1, 0] * x) + (M1.m[1, 1] * y) + (M1.m[1, 2] * z) + (M1.m[1, 3] * w);
    Result.m[1, 2] := (M1.m[2, 0] * x) + (M1.m[2, 1] * y) + (M1.m[2, 2] * z) + (M1.m[2, 3] * w);
    Result.m[1, 3] := (M1.m[3, 0] * x) + (M1.m[3, 1] * y) + (M1.m[3, 2] * z) + (M1.m[3, 3] * w);
    x := M2.m[0, 2];
    y := M2.m[1, 2];
    z := M2.m[2, 2];
    w := M2.m[3, 2];
    Result.m[2, 0] := (M1.m[0, 0] * x) + (M1.m[0, 1] * y) + (M1.m[0, 2] * z) + (M1.m[0, 3] * w);
    Result.m[2, 1] := (M1.m[1, 0] * x) + (M1.m[1, 1] * y) + (M1.m[1, 2] * z) + (M1.m[1, 3] * w);
    Result.m[2, 2] := (M1.m[2, 0] * x) + (M1.m[2, 1] * y) + (M1.m[2, 2] * z) + (M1.m[2, 3] * w);
    Result.m[2, 3] := (M1.m[3, 0] * x) + (M1.m[3, 1] * y) + (M1.m[3, 2] * z) + (M1.m[3, 3] * w);
    x := M2.m[0, 3];
    y := M2.m[1, 3];
    z := M2.m[2, 3];
    w := M2.m[3, 3];
    Result.m[3, 0] := (M1.m[0, 0] * x) + (M1.m[0, 1] * y) + (M1.m[0, 2] * z) + (M1.m[0, 3] * w);
    Result.m[3, 1] := (M1.m[1, 0] * x) + (M1.m[1, 1] * y) + (M1.m[1, 2] * z) + (M1.m[1, 3] * w);
    Result.m[3, 2] := (M1.m[2, 0] * x) + (M1.m[2, 1] * y) + (M1.m[2, 2] * z) + (M1.m[2, 3] * w);
    Result.m[3, 3] := (M1.m[3, 0] * x) + (M1.m[3, 1] * y) + (M1.m[3, 2] * z) + (M1.m[3, 3] * w);
end;



function XMMatrixTranspose(M: TXMMATRIX): TXMMATRIX;
var
    P: TXMMATRIX;
begin
    // Original matrix:

    //     m00m01m02m03
    //     m10m11m12m13
    //     m20m21m22m23
    //     m30m31m32m33


    P.r[0] := XMVectorMergeXY(M.r[0], M.r[2]); // m00m20m01m21
    P.r[1] := XMVectorMergeXY(M.r[1], M.r[3]); // m10m30m11m31
    P.r[2] := XMVectorMergeZW(M.r[0], M.r[2]); // m02m22m03m23
    P.r[3] := XMVectorMergeZW(M.r[1], M.r[3]); // m12m32m13m33

    Result.r[0] := XMVectorMergeXY(P.r[0], P.r[1]); // m00m10m20m30
    Result.r[1] := XMVectorMergeZW(P.r[0], P.r[1]); // m01m11m21m31
    Result.r[2] := XMVectorMergeXY(P.r[2], P.r[3]); // m02m12m22m32
    Result.r[3] := XMVectorMergeZW(P.r[2], P.r[3]); // m03m13m23m33
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
    Result.m[0, 0] := m00;
    Result.m[0, 1] := m01;
    Result.m[0, 2] := m02;
    Result.m[0, 3] := m03;
    Result.m[1, 0] := m10;
    Result.m[1, 1] := m11;
    Result.m[1, 2] := m12;
    Result.m[1, 3] := m13;
    Result.m[2, 0] := m20;
    Result.m[2, 1] := m21;
    Result.m[2, 2] := m22;
    Result.m[2, 3] := m23;
    Result.m[3, 0] := m30;
    Result.m[3, 1] := m31;
    Result.m[3, 2] := m32;
    Result.m[3, 3] := m33;
end;



function XMMatrixTranslation(OffsetX: single; OffsetY: single; OffsetZ: single): TXMMATRIX;
begin

    Result.m[0, 0] := 1.0;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := 1.0;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := 1.0;
    Result.m[2, 3] := 0.0;

    Result.m[3, 0] := OffsetX;
    Result.m[3, 1] := OffsetY;
    Result.m[3, 2] := OffsetZ;
    Result.m[3, 3] := 1.0;

end;



function XMMatrixTranslationFromVector(Offset: TXMVECTOR): TXMMATRIX;
begin

    Result.m[0, 0] := 1.0;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := 1.0;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := 1.0;
    Result.m[2, 3] := 0.0;

    Result.m[3, 0] := Offset.f32[0];
    Result.m[3, 1] := Offset.f32[1];
    Result.m[3, 2] := Offset.f32[2];
    Result.m[3, 3] := 1.0;
end;



function XMMatrixScaling(constref ScaleX, ScaleY, ScaleZ: single): TXMMATRIX;
begin
    Result.m[0, 0] := ScaleX;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := ScaleY;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := ScaleZ;
    Result.m[2, 3] := 0.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := 0.0;
    Result.m[3, 3] := 1.0;
end;



function XMMatrixScalingFromVector(constref Scale: TXMVECTOR): TXMMATRIX;
begin

    Result.m[0, 0] := Scale.f32[0];
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := Scale.f32[1];
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := Scale.f32[2];
    Result.m[2, 3] := 0.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := 0.0;
    Result.m[3, 3] := 1.0;
end;



function XMMatrixRotationX(Angle: single): TXMMATRIX;
var
    fSinAngle, fCosAngle: single;
begin
    XMScalarSinCos(fSinAngle, fCosAngle, Angle);

    Result.m[0, 0] := 1.0;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := fCosAngle;
    Result.m[1, 2] := fSinAngle;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := -fSinAngle;
    Result.m[2, 2] := fCosAngle;
    Result.m[2, 3] := 0.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := 0.0;
    Result.m[3, 3] := 1.0;
end;



function XMMatrixRotationY(Angle: single): TXMMATRIX;
var
    fSinAngle: single;
    fCosAngle: single;
begin

    XMScalarSinCos(fSinAngle, fCosAngle, Angle);


    Result.m[0, 0] := fCosAngle;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := -fSinAngle;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := 1.0;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := fSinAngle;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := fCosAngle;
    Result.m[2, 3] := 0.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := 0.0;
    Result.m[3, 3] := 1.0;
end;



function XMMatrixRotationZ(Angle: single): TXMMATRIX;
var
    fSinAngle: single;
    fCosAngle: single;
begin

    XMScalarSinCos(fSinAngle, fCosAngle, Angle);


    Result.m[0, 0] := fCosAngle;
    Result.m[0, 1] := fSinAngle;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := -fSinAngle;
    Result.m[1, 1] := fCosAngle;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := 1.0;
    Result.m[2, 3] := 0.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := 0.0;
    Result.m[3, 3] := 1.0;
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
var
    TwoNearZ, fRange: single;
begin
    assert((NearZ > 0.0) and (FarZ > 0.0));
    assert(not XMScalarNearEqual(ViewWidth, 0.0, 0.00001));
    assert(not XMScalarNearEqual(ViewHeight, 0.0, 0.00001));
    assert(not XMScalarNearEqual(FarZ, NearZ, 0.00001));
    TwoNearZ := NearZ + NearZ;
    fRange := FarZ / (FarZ - NearZ);

    Result.m[0, 0] := TwoNearZ / ViewWidth;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := TwoNearZ / ViewHeight;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := fRange;
    Result.m[2, 3] := 1.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := -fRange * NearZ;
    Result.m[3, 3] := 0.0;
end;



function XMMatrixPerspectiveRH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    TwoNearZ, fRange: single;
begin
    assert((NearZ > 0.0) and (FarZ > 0.0));
    assert(not XMScalarNearEqual(ViewWidth, 0.0, 0.00001));
    assert(not XMScalarNearEqual(ViewHeight, 0.0, 0.00001));
    assert(not XMScalarNearEqual(FarZ, NearZ, 0.00001));

    TwoNearZ := NearZ + NearZ;
    fRange := FarZ / (NearZ - FarZ);


    Result.m[0, 0] := TwoNearZ / ViewWidth;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := TwoNearZ / ViewHeight;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := fRange;
    Result.m[2, 3] := -1.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := fRange * NearZ;
    Result.m[3, 3] := 0.0;
end;



function XMMatrixPerspectiveFovLH(FovAngleY: single; AspectRatio: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    SinFov: single;
    CosFov: single;
    Height, Width, fRange: single;
begin
    assert((NearZ > 0.0) and (FarZ > 0.0));
    assert(not XMScalarNearEqual(FovAngleY, 0.0, 0.00001 * 2.0));
    assert(not XMScalarNearEqual(AspectRatio, 0.0, 0.00001));
    assert(not XMScalarNearEqual(FarZ, NearZ, 0.00001));

    XMScalarSinCos(SinFov, CosFov, 0.5 * FovAngleY);

    Height := CosFov / SinFov;
    Width := Height / AspectRatio;
    fRange := FarZ / (FarZ - NearZ);


    Result.m[0, 0] := Width;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := Height;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := fRange;
    Result.m[2, 3] := 1.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := -fRange * NearZ;
    Result.m[3, 3] := 0.0;
end;



function XMMatrixPerspectiveFovRH(FovAngleY: single; AspectRatio: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    SinFov: single;
    CosFov: single;
    Height, Width, fRange: single;
begin
    assert((NearZ > 0.0) and (FarZ > 0.0));
    assert(not XMScalarNearEqual(FovAngleY, 0.0, 0.00001 * 2.0));
    assert(not XMScalarNearEqual(AspectRatio, 0.0, 0.00001));
    assert(not XMScalarNearEqual(FarZ, NearZ, 0.00001));

    XMScalarSinCos(SinFov, CosFov, 0.5 * FovAngleY);

    Height := CosFov / SinFov;
    Width := Height / AspectRatio;
    fRange := FarZ / (NearZ - FarZ);


    Result.m[0, 0] := Width;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := Height;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := fRange;
    Result.m[2, 3] := -1.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := fRange * NearZ;
    Result.m[3, 3] := 0.0;
end;



function XMMatrixPerspectiveOffCenterLH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    TwoNearZ, ReciprocalWidth, ReciprocalHeight, fRange: single;
begin
    assert((NearZ > 0.0) and (FarZ > 0.0));
    assert(not XMScalarNearEqual(ViewRight, ViewLeft, 0.00001));
    assert(not XMScalarNearEqual(ViewTop, ViewBottom, 0.00001));
    assert(not XMScalarNearEqual(FarZ, NearZ, 0.00001));

    TwoNearZ := NearZ + NearZ;
    ReciprocalWidth := 1.0 / (ViewRight - ViewLeft);
    ReciprocalHeight := 1.0 / (ViewTop - ViewBottom);
    fRange := FarZ / (FarZ - NearZ);


    Result.m[0, 0] := TwoNearZ * ReciprocalWidth;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := TwoNearZ * ReciprocalHeight;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := -(ViewLeft + ViewRight) * ReciprocalWidth;
    Result.m[2, 1] := -(ViewTop + ViewBottom) * ReciprocalHeight;
    Result.m[2, 2] := fRange;
    Result.m[2, 3] := 1.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := -fRange * NearZ;
    Result.m[3, 3] := 0.0;
end;



function XMMatrixPerspectiveOffCenterRH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    TwoNearZ, ReciprocalWidth, ReciprocalHeight, fRange: single;
begin
    assert((NearZ > 0.0) and (FarZ > 0.0));
    assert(not XMScalarNearEqual(ViewRight, ViewLeft, 0.00001));
    assert(not XMScalarNearEqual(ViewTop, ViewBottom, 0.00001));
    assert(not XMScalarNearEqual(FarZ, NearZ, 0.00001));

    TwoNearZ := NearZ + NearZ;
    ReciprocalWidth := 1.0 / (ViewRight - ViewLeft);
    ReciprocalHeight := 1.0 / (ViewTop - ViewBottom);
    fRange := FarZ / (NearZ - FarZ);


    Result.m[0, 0] := TwoNearZ * ReciprocalWidth;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := TwoNearZ * ReciprocalHeight;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := (ViewLeft + ViewRight) * ReciprocalWidth;
    Result.m[2, 1] := (ViewTop + ViewBottom) * ReciprocalHeight;
    Result.m[2, 2] := fRange;
    Result.m[2, 3] := -1.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := fRange * NearZ;
    Result.m[3, 3] := 0.0;
end;



function XMMatrixOrthographicLH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    fRange: single;
begin
    assert(not XMScalarNearEqual(ViewWidth, 0.0, 0.00001));
    assert(not XMScalarNearEqual(ViewHeight, 0.0, 0.00001));
    assert(not XMScalarNearEqual(FarZ, NearZ, 0.00001));

    fRange := 1.0 / (FarZ - NearZ);


    Result.m[0, 0] := 2.0 / ViewWidth;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := 2.0 / ViewHeight;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := fRange;
    Result.m[2, 3] := 0.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := -fRange * NearZ;
    Result.m[3, 3] := 1.0;

end;



function XMMatrixOrthographicRH(ViewWidth: single; ViewHeight: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    fRange: single;
begin
    assert(not XMScalarNearEqual(ViewWidth, 0.0, 0.00001));
    assert(not XMScalarNearEqual(ViewHeight, 0.0, 0.00001));
    assert(not XMScalarNearEqual(FarZ, NearZ, 0.00001));

    fRange := 1.0 / (NearZ - FarZ);


    Result.m[0, 0] := 2.0 / ViewWidth;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := 2.0 / ViewHeight;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := fRange;
    Result.m[2, 3] := 0.0;

    Result.m[3, 0] := 0.0;
    Result.m[3, 1] := 0.0;
    Result.m[3, 2] := fRange * NearZ;
    Result.m[3, 3] := 1.0;
end;



function XMMatrixOrthographicOffCenterLH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    ReciprocalWidth, ReciprocalHeight, fRange: single;
begin
    assert(not XMScalarNearEqual(ViewRight, ViewLeft, 0.00001));
    assert(not XMScalarNearEqual(ViewTop, ViewBottom, 0.00001));
    assert(not XMScalarNearEqual(FarZ, NearZ, 0.00001));

    ReciprocalWidth := 1.0 / (ViewRight - ViewLeft);
    ReciprocalHeight := 1.0 / (ViewTop - ViewBottom);
    fRange := 1.0 / (FarZ - NearZ);


    Result.m[0, 0] := ReciprocalWidth + ReciprocalWidth;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := ReciprocalHeight + ReciprocalHeight;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := fRange;
    Result.m[2, 3] := 0.0;

    Result.m[3, 0] := -(ViewLeft + ViewRight) * ReciprocalWidth;
    Result.m[3, 1] := -(ViewTop + ViewBottom) * ReciprocalHeight;
    Result.m[3, 2] := -fRange * NearZ;
    Result.m[3, 3] := 1.0;
end;



function XMMatrixOrthographicOffCenterRH(ViewLeft: single; ViewRight: single; ViewBottom: single; ViewTop: single; NearZ: single; FarZ: single): TXMMATRIX;
var
    ReciprocalWidth, ReciprocalHeight, fRange: single;
begin
    assert(not XMScalarNearEqual(ViewRight, ViewLeft, 0.00001));
    assert(not XMScalarNearEqual(ViewTop, ViewBottom, 0.00001));
    assert(not XMScalarNearEqual(FarZ, NearZ, 0.00001));

    ReciprocalWidth := 1.0 / (ViewRight - ViewLeft);
    ReciprocalHeight := 1.0 / (ViewTop - ViewBottom);
    fRange := 1.0 / (NearZ - FarZ);


    Result.m[0, 0] := ReciprocalWidth + ReciprocalWidth;
    Result.m[0, 1] := 0.0;
    Result.m[0, 2] := 0.0;
    Result.m[0, 3] := 0.0;

    Result.m[1, 0] := 0.0;
    Result.m[1, 1] := ReciprocalHeight + ReciprocalHeight;
    Result.m[1, 2] := 0.0;
    Result.m[1, 3] := 0.0;

    Result.m[2, 0] := 0.0;
    Result.m[2, 1] := 0.0;
    Result.m[2, 2] := fRange;
    Result.m[2, 3] := 0.0;

    Result.r[3] := XMVectorSet(-(ViewLeft + ViewRight) * ReciprocalWidth, -(ViewTop + ViewBottom) * ReciprocalHeight, fRange * NearZ, 1.0);
end;


// Returns the product Q2*Q1 (which is the concatenation of a rotation Q1 followed by the rotation Q2)

// [ (Q2.w * Q1.x) + (Q2.x * Q1.w) + (Q2.y * Q1.z) - (Q2.z * Q1.y),
//   (Q2.w * Q1.y) - (Q2.x * Q1.z) + (Q2.y * Q1.w) + (Q2.z * Q1.x),
//   (Q2.w * Q1.z) + (Q2.x * Q1.y) - (Q2.y * Q1.x) + (Q2.z * Q1.w),
//   (Q2.w * Q1.w) - (Q2.x * Q1.x) - (Q2.y * Q1.y) - (Q2.z * Q1.z) ]
function XMQuaternionMultiply(Q1: TXMVECTOR; Q2: TXMVECTOR): TXMVECTOR; inline;
begin
    Result.f32[0] := (Q2.f32[3] * Q1.f32[0]) + (Q2.f32[0] * Q1.f32[3]) + (Q2.f32[1] * Q1.f32[2]) - (Q2.f32[2] * Q1.f32[1]);
    Result.f32[1] := (Q2.f32[3] * Q1.f32[1]) - (Q2.f32[0] * Q1.f32[2]) + (Q2.f32[1] * Q1.f32[3]) + (Q2.f32[2] * Q1.f32[0]);
    Result.f32[2] := (Q2.f32[3] * Q1.f32[2]) + (Q2.f32[0] * Q1.f32[1]) - (Q2.f32[1] * Q1.f32[0]) + (Q2.f32[2] * Q1.f32[3]);
    Result.f32[3] := (Q2.f32[3] * Q1.f32[3]) - (Q2.f32[0] * Q1.f32[0]) - (Q2.f32[1] * Q1.f32[1]) - (Q2.f32[2] * Q1.f32[2]);
end;



function XMQuaternionConjugate(Q: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := -Q.f32[0];
    Result.f32[1] := -Q.f32[1];
    Result.f32[2] := -Q.f32[2];
    Result.f32[3] := Q.f32[3];
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



function XMQuaternionRotationMatrix(M: TXMMATRIX): TXMVECTOR; inline;
var
    q: TXMVECTORF32;
    r22, dif10, omr22: single;
    fourXSqr, inv4x: single;
    fourYSqr, inv4y: single;
    fourZSqr, inv4z: single;
    sum10, opr22: single;
    fourWSqr, inv4w: single;
begin
    r22 := M.m[2, 2];
    if (r22 <= 0.0) then  // x^2 + y^2 >= z^2 + w^2
    begin
        dif10 := M.m[1, 1] - M.m[0, 0];
        omr22 := 1.0 - r22;
        if (dif10 <= 0.0) then  // x^2 >= y^2
        begin
            fourXSqr := omr22 - dif10;
            inv4x := 0.5 / sqrt(fourXSqr);
            q.f[0] := fourXSqr * inv4x;
            q.f[1] := (M.m[0, 1] + M.m[1, 0]) * inv4x;
            q.f[2] := (M.m[0, 2] + M.m[2, 0]) * inv4x;
            q.f[3] := (M.m[1, 2] - M.m[2, 1]) * inv4x;
        end
        else  // y^2 >= x^2
        begin
            fourYSqr := omr22 + dif10;
            inv4y := 0.5 / sqrt(fourYSqr);
            q.f[0] := (M.m[0, 1] + M.m[1, 0]) * inv4y;
            q.f[1] := fourYSqr * inv4y;
            q.f[2] := (M.m[1, 2] + M.m[2, 1]) * inv4y;
            q.f[3] := (M.m[2, 0] - M.m[0, 2]) * inv4y;
        end;
    end
    else  // z^2 + w^2 >= x^2 + y^2
    begin
        sum10 := M.m[1, 1] + M.m[0, 0];
        opr22 := 1.0 + r22;
        if (sum10 <= 0.0) then  // z^2 >= w^2
        begin
            fourZSqr := opr22 - sum10;
            inv4z := 0.5 / sqrt(fourZSqr);
            q.f[0] := (M.m[0, 2] + M.m[2, 0]) * inv4z;
            q.f[1] := (M.m[1, 2] + M.m[2, 1]) * inv4z;
            q.f[2] := fourZSqr * inv4z;
            q.f[3] := (M.m[0, 1] - M.m[1, 0]) * inv4z;
        end
        else  // w^2 >= z^2
        begin
            fourWSqr := opr22 + sum10;
            inv4w := 0.5 / sqrt(fourWSqr);
            q.f[0] := (M.m[1, 2] - M.m[2, 1]) * inv4w;
            q.f[1] := (M.m[2, 0] - M.m[0, 2]) * inv4w;
            q.f[2] := (M.m[0, 1] - M.m[1, 0]) * inv4w;
            q.f[3] := fourWSqr * inv4w;
        end;
    end;
    Result := q.v;
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
    fLengthSq: single;
begin
    fLengthSq := sqrt((P.f32[0] * P.f32[0]) + (P.f32[1] * P.f32[1]) + (P.f32[2] * P.f32[2]));
    // Prevent divide by zero
    if (fLengthSq <> 0) then
        fLengthSq := 1.0 / fLengthSq;

    Result.f32[0] := P.f32[0] * fLengthSq;
    Result.f32[0] := P.f32[1] * fLengthSq;
    Result.f32[0] := P.f32[2] * fLengthSq;
    Result.f32[0] := P.f32[3] * fLengthSq;
end;


//------------------------------------------------------------------------------
// Computation operations
//------------------------------------------------------------------------------

function XMColorNegative(constref vColor: TXMVECTOR): TXMVECTOR; inline;
begin
    Result.f32[0] := 1.0 - vColor.f32[0];
    Result.f32[1] := 1.0 - vColor.f32[1];
    Result.f32[2] := 1.0 - vColor.f32[2];
    Result.f32[3] := vColor.f32[3];
end;


// Luminance = 0.2125f * C[0] + 0.7154f * C[1] + 0.0721f * C[2];
// Result = (C - Luminance) * Saturation + Luminance;
function XMColorAdjustSaturation(constref vColor: TXMVECTOR; Saturation: single): TXMVECTOR; inline;
const
    gvLuminance: TXMVECTORF32 = (f: (0.2125, 0.7154, 0.0721, 0.0));
var
    fLuminance: single;
begin
    fLuminance := (vColor.f32[0] * gvLuminance.f[0]) + (vColor.f32[1] * gvLuminance.f[1]) + (vColor.f32[2] * gvLuminance.f[2]);
    Result.f32[0] := ((vColor.f32[0] - fLuminance) * Saturation) + fLuminance;
    Result.f32[1] := ((vColor.f32[1] - fLuminance) * Saturation) + fLuminance;
    Result.f32[2] := ((vColor.f32[2] - fLuminance) * Saturation) + fLuminance;
    Result.f32[3] := vColor.f32[3];
end;


// Result = (vColor - 0.5f) * fContrast + 0.5f;
function XMColorAdjustContrast(constref vColor: TXMVECTOR; constref Contrast: single): TXMVECTOR; inline;
begin
    Result.f32[0] := ((vColor.f32[0] - 0.5) * Contrast) + 0.5;
    Result.f32[1] := ((vColor.f32[1] - 0.5) * Contrast) + 0.5;
    Result.f32[2] := ((vColor.f32[2] - 0.5) * Contrast) + 0.5;
    Result.f32[3] := vColor.f32[3];        // Leave W untouched
end;


{****************************************************************************
 *
 * Miscellaneous
 *
 ****************************************************************************}

function XMVerifyCPUSupport(CPURequired: TCPUType): boolean;
begin
    // No intrinsics path always supported
    Result := True;
end;



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




class operator TXMMATRIX.Divide(M: TXMMATRIX; s: single): TXMMATRIX;
var
    vS: TXMVECTOR;
begin
    vS := XMVectorReplicate(S);
    Result.r[0] := XMVectorDivide(M.r[0], vS);
    Result.r[1] := XMVectorDivide(M.r[1], vS);
    Result.r[2] := XMVectorDivide(M.r[2], vS);
    Result.r[3] := XMVectorDivide(M.r[3], vS);
end;



function XMVectorNegate(constref V: TXMVECTOR): TXMVECTOR;
begin
    Result.f32[0] := -V.f32[0];
    Result.f32[1] := -V.f32[1];
    Result.f32[2] := -V.f32[2];
    Result.f32[3] := -V.f32[3];
end;

// end.
