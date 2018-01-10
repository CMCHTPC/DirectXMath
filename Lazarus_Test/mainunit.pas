unit MainUnit;

{$mode delphi}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ComCtrls,
    Windows,DirectX.Math, Types;

type

    { TForm1 }

    TForm1 = class(TForm)
        btnXMVector2EqualInt: TButton;
        btnXMVector2LessOrEqual: TButton;
        btnXMVector2InBounds: TButton;
        btnXMVector2Greater: TButton;
        btnXMVector2IsNaN: TButton;
        btnXMVector2IsInfinite: TButton;
        btnXMVector2Dot: TButton;
        btnXMVector2Cross: TButton;
        btnXMVector2LengthSq: TButton;
        btnXMVector2ReciprocalLengthEst: TButton;
        btnXMVector2ReciprocalLength: TButton;
        btnXMVector2EqualIntR: TButton;
        btnXMVector2LengthEst: TButton;
        btnXMVector2Length: TButton;
        btnXMVector2NormalizeEst: TButton;
        btnXMVector2Normalize: TButton;
        btnXMVector2ClampLength: TButton;
        btnXMVector2ClampLengthV: TButton;
        btnXMVector2Reflect: TButton;
        btnXMVector2Refract: TButton;
        btnXMVector2RefractV: TButton;
        btnXMVector2Orthogonal: TButton;
        btnXMVector2NearEqual: TButton;
        btnXMVector2Transform: TButton;
        btnXMVector2TransformStream: TButton;
        btnXMVector2TransformCoord: TButton;
        btnXMVector2TransformNormal: TButton;
        btnXMVector2NotEqual: TButton;
        btnXMVector2AngleBetweenNormalsEst: TButton;
        btnXMVector2AngleBetweenNormals: TButton;
        btnXMVector2AngleBetweenVectors: TButton;
        btnXMVector2LinePointDistance: TButton;
        btnXMVector2IntersectLine: TButton;
        btnXMVector2TransformCoordStream: TButton;
        btnXMVector2TransformNormalStream: TButton;
        btnXMVector3Equal: TButton;
        btnXMVector3EqualR: TButton;
        btnXMVector2NotEqualInt: TButton;
        btnXMVector3EqualInt: TButton;
        btnXMVector3EqualIntR: TButton;
        btnXMVector3NearEqual: TButton;
        btnXMVector3NotEqual: TButton;
        btnXMVector3NotEqualInt: TButton;
        btnXMVector3Greater: TButton;
        btnXMVector3GreaterR: TButton;
        btnXMVector3GreaterOrEqual: TButton;
        btnXMVector3GreaterOrEqualR: TButton;
        btnXMVector3Less: TButton;
        btnXMVector2GreaterR: TButton;
        btnXMVector3LessOrEqual: TButton;
        btnXMVector3InBounds: TButton;
        btnXMVector3IsNaN: TButton;
        btnXMVector3IsInfinite: TButton;
        btnXMVector3Dot: TButton;
        btnXMVector3Cross: TButton;
        btnXMVector3LengthSq: TButton;
        btnXMVector3ReciprocalLengthEst: TButton;
        btnXMVector3ReciprocalLength: TButton;
        btnXMVector3LengthEst: TButton;
        btnXMVector2GreaterOrEqual: TButton;
        btnXMVector3Length: TButton;
        btnXMVector3NormalizeEst: TButton;
        btnXMVector3Normalize: TButton;
        btnXMVector3ClampLength: TButton;
        btnXMVector3ClampLengthV: TButton;
        btnXMVector3Reflect: TButton;
        btnXMVector3Refract: TButton;
        btnXMVector3RefractV: TButton;
        btnXMVector3Orthogonal: TButton;
        btnXMVector3Rotate: TButton;
        btnXMVector2GreaterOrEqualR: TButton;
        btnXMVector3InverseRotate: TButton;
        btnXMVector3Transform: TButton;
        btnXMVector3TransformStream: TButton;
        btnXMVector3TransformCoord: TButton;
        btnXMVector3TransformNormal: TButton;
        btnXMVector3Project: TButton;
        btnXMVector3ProjectStream: TButton;
        btnXMVector3Unproject: TButton;
        btnXMVector3UnprojectStream: TButton;
        btnXMVector2Less: TButton;
        btnXMVector3AngleBetweenNormalsEst: TButton;
        btnXMVector3AngleBetweenNormals: TButton;
        btnXMVector3AngleBetweenVectors: TButton;
        btnXMVector3LinePointDistance: TButton;
        btnXMVector3ComponentsFromNormal: TButton;
        btnXMVector3TransformCoordStream: TButton;
        btnXMVector3TransformNormalStream: TButton;
        btnXMConvertToDegrees: TButton;
        btnXMComparisonAllTrue: TButton;
        btnXMComparisonAnyTrue: TButton;
        btnXMComparisonAllFalse: TButton;
        btnXMComparisonAnyFalse: TButton;
        btnXMComparisonMixed: TButton;
        btnXMComparisonAllInBounds: TButton;
        btnXMComparisonAnyOutOfBounds: TButton;
        btnXMScalarACos: TButton;
        btnXMScalarACosEst: TButton;
        btnXMMin: TButton;
        btnXMMax: TButton;
        btnXMConvertToRadians: TButton;
        btnXMVector2Equal: TButton;
        btnXMVerifyCPUSupport: TButton;
        btnXMFresnelTerm: TButton;
        btnXMScalarNearEqual: TButton;
        btnXMScalarModAngle: TButton;
        btnXMScalarSin: TButton;
        btnXMScalarSinEst: TButton;
        btnXMScalarCos: TButton;
        btnXMScalarCosEst: TButton;
        btnXMScalarSinCos: TButton;
        btnXMScalarSinCosEst: TButton;
        btnXMScalarASin: TButton;
        btnXMScalarASinEst: TButton;
        btnXMVectorModAngles: TButton;
        btnXMVectorSinH: TButton;
        btnXMVectorCosH: TButton;
        btnXMVectorTanH: TButton;
        btnXMVectorASin: TButton;
        btnXMVectorASinEst: TButton;
        btnXMVectorACos: TButton;
        btnXMVectorACosEst: TButton;
        btnXMVectorATan: TButton;
        btnXMVectorATanEst: TButton;
        btnXMVectorATan2: TButton;
        btnXMVectorSin: TButton;
        btnXMVectorATan2Est: TButton;
        btnXMVectorLerp: TButton;
        btnXMVectorLerpV: TButton;
        btnXMVectorHermite: TButton;
        btnXMVectorHermiteV: TButton;
        btnXMVectorCatmullRom: TButton;
        btnXMVectorCatmullRomV: TButton;
        btnXMVectorBaryCentric: TButton;
        btnXMVectorBaryCentricV: TButton;
        btnXMVectorSinEst: TButton;
        btnXMVectorCos: TButton;
        btnXMVectorCosEst: TButton;
        btnXMVectorSinCos: TButton;
        btnXMVectorSinCosEst: TButton;
        btnXMVectorTan: TButton;
        btnXMVectorTanEst: TButton;
        btnXMVectorSelect: TButton;
        btnXMVectorEqualInt: TButton;
        btnXMVectorEqualIntR: TButton;
        btnXMVectorNearEqual: TButton;
        btnXMVectorNotEqual: TButton;
        btnXMVectorGreater: TButton;
        btnXMVectorNotEqualInt: TButton;
        btnXMVectorGreaterR: TButton;
        btnXMVectorGreaterOrEqual: TButton;
        btnXMVectorGreaterOrEqualR: TButton;
        btnXMVectorLess: TButton;
        btnXMVectorMergeXY: TButton;
        btnXMVectorLessOrEqual: TButton;
        btnXMVectorInBoundsR: TButton;
        btnXMVectorInBounds: TButton;
        btnXMVectorIsNaN: TButton;
        btnXMVectorIsInfinite: TButton;
        btnXMVectorMin: TButton;
        btnXMVectorMax: TButton;
        btnXMVectorRound: TButton;
        btnXMVectorFloor: TButton;
        btnXMVectorMod: TButton;
        btnXMVectorTruncate: TButton;
        btnXMVectorMergeZW: TButton;
        btnXMVectorCeiling: TButton;
        btnXMVectorClamp: TButton;
        btnXMVectorSaturate: TButton;
        btnXMVectorAndInt: TButton;
        btnXMVectorAndCInt: TButton;
        btnXMVectorNorInt: TButton;
        btnXMVectorOrInt: TButton;
        btnXMVectorXorInt: TButton;
        btnXMVectorNegate: TButton;
        btnXMVectorAdd: TButton;
        btnXMVectorShiftLeft: TButton;
        btnXMVectorSum: TButton;
        btnXMVectorAddAngles: TButton;
        btnXMVectorSubtractAngles: TButton;
        btnXMVectorSubtract: TButton;
        btnXMVectorMultiply: TButton;
        btnXMVectorMultiplyAdd: TButton;
        btnXMVectorDivide: TButton;
        btnXMVectorNegativeMultiplySubtract: TButton;
        btnXMVectorScale: TButton;
        btnXMVectorReciprocal: TButton;
        btnXMVectorRotateLeft: TButton;
        btnXMVectorReciprocalEst: TButton;
        btnXMVectorSqrtEst: TButton;
        btnXMVectorSqrt: TButton;
        btnXMVectorReciprocalSqrtEst: TButton;
        btnXMVectorReciprocalSqrt: TButton;
        btnXMVectorExp2: TButton;
        btnXMVectorLogE: TButton;
        btnXMVectorExpE: TButton;
        btnXMVectorLog: TButton;
        btnXMVectorPow: TButton;
        btnXMVectorRotateRight: TButton;
        btnXMVectorLog2: TButton;
        btnXMVectorAbs: TButton;
        btnXMVectorExp: TButton;
        btnXMVectorEqual: TButton;
        btnXMVectorInsert: TButton;
        btnXMVectorEqualR: TButton;
        btnXMStoreFloat3A: TButton;
        btnXMStoreFloat4A: TButton;
        btnXMStoreFloat3: TButton;
        btnXMStoreFloat4: TButton;
        btnXMStoreInt3A: TButton;
        btnXMStoreInt4A: TButton;
        btnXMStoreInt4: TButton;
        btnXMStoreSInt3: TButton;
        btnXMStoreSInt4: TButton;
        btnXMStoreUInt3: TButton;
        btnXMStoreUInt4: TButton;
        btnXMVector4GreaterOrEqual: TButton;
        btnXMVector4Equal: TButton;
        btnXMConvertVectorFloatToInt: TButton;
        btnXMConvertVectorIntToFloat: TButton;
        btnXMConvertVectorUIntToFloat: TButton;
        btnXMConvertVectorFloatToUInt: TButton;
        btnXMVectorSelectControl: TButton;
        btnXMVectorReplicate: TButton;
        btnXMVectorSetBinaryConstant: TButton;
        btnXMStoreInt2: TButton;
        btnXMStoreInt3: TButton;
        btnXMStoreInt2A: TButton;
        btnXMVectorZero: TButton;
        btnXMStoreFloat3x3: TButton;
        btnXMStoreFloat4x3: TButton;
        btnXMLoadInt: TButton;
        btnXMVector2EqualR: TButton;
        Button10: TButton;
        Button11: TButton;
        Button12: TButton;
        Button13: TButton;
        Button14: TButton;
        Button15: TButton;
        Button16: TButton;
        Button17: TButton;
        Button18: TButton;
        Button19: TButton;
        btnXMVector4EqualR: TButton;
        btnXMVectorSplatConstant: TButton;
        btnXMStoreFloat2: TButton;
        btnXMVectorSet: TButton;
        btnXMStoreFloat4x3A: TButton;
        btnXMLoadFloat: TButton;
        Button20: TButton;
        Button21: TButton;
        Button22: TButton;
        Button23: TButton;
        Button24: TButton;
        Button25: TButton;
        Button26: TButton;
        Button27: TButton;
        Button28: TButton;
        Button29: TButton;
        btnXMVector4EqualInt: TButton;
        btnXMVectorSplatConstantInt: TButton;
        btnXMStoreFloat2A: TButton;
        btnXMVectorSetInt: TButton;
        btnXMStoreFloat4x4: TButton;
        btnXMLoadInt2: TButton;
        Button30: TButton;
        Button31: TButton;
        Button32: TButton;
        Button33: TButton;
        Button34: TButton;
        Button35: TButton;
        btnXMVector4EqualIntR: TButton;
        btnXMVector4NearEqual: TButton;
        btnXMStoreFloat: TButton;
        btnXMStoreSInt2: TButton;
        btnXMStoreInt: TButton;
        btnXMStoreUInt2: TButton;
        btnXMVectorReplicateInt: TButton;
        btnXMVectorReplicateIntPtr: TButton;
        btnXMVectorTrueInt: TButton;
        btnXMVectorFalseInt: TButton;
        btnXMVectorGetY: TButton;
        btnXMVectorGetZ: TButton;
        btnXMVectorGetW: TButton;
        btnXMVectorGetByIndexPtr: TButton;
        bntXMVectorGetByIndex: TButton;
        btnXMVectorSplatX: TButton;
        btnXMVectorSplatY: TButton;
        btnXMVectorSplatZ: TButton;
        btnXMVectorSplatW: TButton;
        btnXMVectorSplatOne: TButton;
        btnXMVectorSplatInfinity: TButton;
        btnXMVectorSplatQNaN: TButton;
        btnXMVectorSplatEpsilon: TButton;
        btnXMVectorSplatSignMask: TButton;
        btnXMVectorReplicatePtr: TButton;
        btnXMVectorGetXPtr: TButton;
        btnXMVectorGetYPtr: TButton;
        btnXMVectorGetZPtr: TButton;
        btnXMVectorGetWPtr: TButton;
        btnXMVectorGetIntByIndex: TButton;
        btnXMVectorGetIntX: TButton;
        btnXMVectorGetIntY: TButton;
        btnXMVectorGetIntZ: TButton;
        btnXMVectorGetIntW: TButton;
        btnXMVectorGetIntByIndexPtr: TButton;
        btnXMVectorGetX: TButton;
        btnXMVectorGetIntXPtr: TButton;
        btnXMVectorGetIntYPtr: TButton;
        btnXMVectorGetIntZPtr: TButton;
        btnXMVectorGetIntWPtr: TButton;
        btnXMVectorSetByIndex: TButton;
        btnXMVectorSetX: TButton;
        btnXMVectorSetY: TButton;
        btnXMVectorSetZ: TButton;
        btnXMVectorSetW: TButton;
        btnXMVectorSetByIndexPtr: TButton;
        btnXMStoreFloat4x4A: TButton;
        btnXMLoadFloat2A: TButton;
        btnXMLoadSInt2: TButton;
        btnXMLoadUInt2: TButton;
        btnXMLoadInt3: TButton;
        btnXMLoadInt2A: TButton;
        btnXMLoadInt3A: TButton;
        btnXMLoadFloat3: TButton;
        btnXMLoadFloat3_2: TButton;
        btnXMLoadFloat3A: TButton;
        btnXMLoadSInt3: TButton;
        btnXMLoadUInt3: TButton;
        btnXMLoadInt4: TButton;
        btnXMLoadInt4A: TButton;
        btnXMLoadFloat4: TButton;
        btnXMLoadFloat4_2: TButton;
        btnXMLoadFloat2: TButton;
        btnXMLoadFloat4A: TButton;
        btnXMLoadSInt4: TButton;
        btnXMLoadUInt4: TButton;
        btnXMLoadFloat3x3: TButton;
        btnXMLoadFloat4x3: TButton;
        btnXMLoadFloat4x3A: TButton;
        btnXMLoadFloat4x4: TButton;
        btnXMLoadFloat4x4A: TButton;
        Button58: TButton;
        Button6: TButton;
        btnXMVectorSetXPtr: TButton;
        btnXMVectorSetYPtr: TButton;
        btnXMVectorSetZPtr: TButton;
        btnXMVectorSetWPtr: TButton;
        btnXMVectorSetIntByIndex: TButton;
        btnXMVectorSetIntX: TButton;
        btnXMVectorSetIntY: TButton;
        btnXMVectorSetIntZ: TButton;
        btnXMVectorSetIntW: TButton;
        btnXMVectorSetIntByIndexPtr: TButton;
        Button7: TButton;
        btnXMVectorSetIntXPtr: TButton;
        btnXMVectorSetIntYPtr: TButton;
        btnXMVectorSetIntZPtr: TButton;
        btnXMVectorSetIntWPtr: TButton;
        btnXMVectorSwizzle: TButton;
        btnXMVectorPermute: TButton;
        Button8: TButton;
        Button9: TButton;
        Memo1: TMemo;
        PageControl1: TPageControl;
        TabSheet1: TTabSheet;
        TabSheet2: TTabSheet;
        TabSheet3: TTabSheet;
        TabSheet4: TTabSheet;
        TabSheet6: TTabSheet;
        TabSheet7: TTabSheet;
        TabSheet8: TTabSheet;
        TabSheet9: TTabSheet;
        procedure bntXMVectorGetByIndexClick(Sender: TObject);
        procedure btnXMVector2EqualClick(Sender: TObject);
        procedure btnXMVector3EqualClick(Sender: TObject);
        procedure btnXMVectorFalseIntClick(Sender: TObject);
        procedure btnXMVectorGetByIndexPtrClick(Sender: TObject);
        procedure btnXMVectorGetIntByIndexClick(Sender: TObject);
        procedure btnXMVectorGetIntByIndexPtrClick(Sender: TObject);
        procedure btnXMVectorGetIntWClick(Sender: TObject);
        procedure btnXMVectorGetIntWPtrClick(Sender: TObject);
        procedure btnXMVectorGetIntXClick(Sender: TObject);
        procedure btnXMVectorGetIntXPtrClick(Sender: TObject);
        procedure btnXMVectorGetIntYClick(Sender: TObject);
        procedure btnXMVectorGetIntYPtrClick(Sender: TObject);
        procedure btnXMVectorGetIntZClick(Sender: TObject);
        procedure btnXMVectorGetIntZPtrClick(Sender: TObject);
        procedure btnXMVectorGetWClick(Sender: TObject);
        procedure btnXMVectorGetWPtrClick(Sender: TObject);
        procedure btnXMVectorGetXClick(Sender: TObject);
        procedure btnXMVectorGetXPtrClick(Sender: TObject);
        procedure btnXMVectorGetYClick(Sender: TObject);
        procedure btnXMVectorGetYPtrClick(Sender: TObject);
        procedure btnXMVectorGetZClick(Sender: TObject);
        procedure btnXMVectorGetZPtrClick(Sender: TObject);
        procedure btnXMVectorReplicateIntClick(Sender: TObject);
        procedure btnXMVectorReplicateIntPtrClick(Sender: TObject);
        procedure btnXMVectorReplicatePtrClick(Sender: TObject);
        procedure btnXMVectorSetByIndexClick(Sender: TObject);
        procedure btnXMVectorSetByIndexPtrClick(Sender: TObject);
        procedure btnXMVectorSplatEpsilonClick(Sender: TObject);
        procedure btnXMVectorSplatInfinityClick(Sender: TObject);
        procedure btnXMVectorSplatOneClick(Sender: TObject);
        procedure btnXMVectorSplatQNaNClick(Sender: TObject);
        procedure btnXMVectorSplatSignMaskClick(Sender: TObject);
        procedure btnXMVectorSplatWClick(Sender: TObject);
        procedure btnXMVectorSplatXClick(Sender: TObject);
        procedure btnXMVectorSplatYClick(Sender: TObject);
        procedure btnXMVectorSplatZClick(Sender: TObject);
        procedure btnXMVectorTanEstClick(Sender: TObject);
        procedure btnXMConvertVectorFloatToIntClick(Sender: TObject);
        procedure btnXMConvertVectorFloatToUIntClick(Sender: TObject);
        procedure btnXMConvertVectorIntToFloatClick(Sender: TObject);
        procedure btnXMConvertVectorUIntToFloatClick(Sender: TObject);
        procedure btnXMStoreFloat4AClick(Sender: TObject);
        procedure btnXMStoreFloat2AClick(Sender: TObject);
        procedure btnXMStoreFloat2Click(Sender: TObject);
        procedure btnXMStoreFloat3AClick(Sender: TObject);
        procedure btnXMStoreFloat3Click(Sender: TObject);
        procedure btnXMStoreFloat4Click(Sender: TObject);
        procedure btnXMStoreFloatClick(Sender: TObject);
        procedure btnXMStoreInt2AClick(Sender: TObject);
        procedure btnXMStoreInt2Click(Sender: TObject);
        procedure btnXMStoreInt3AClick(Sender: TObject);
        procedure btnXMStoreInt3Click(Sender: TObject);
        procedure btnXMStoreInt4AClick(Sender: TObject);
        procedure btnXMStoreInt4Click(Sender: TObject);
        procedure btnXMStoreIntClick(Sender: TObject);
        procedure btnXMStoreSInt2Click(Sender: TObject);
        procedure btnXMStoreSInt3Click(Sender: TObject);
        procedure btnXMStoreSInt4Click(Sender: TObject);
        procedure btnXMStoreUInt2Click(Sender: TObject);
        procedure btnXMStoreUInt3Click(Sender: TObject);
        procedure btnXMStoreUInt4Click(Sender: TObject);
        procedure btnXMVector4EqualClick(Sender: TObject);
        procedure btnXMVector4EqualIntRClick(Sender: TObject);
        procedure btnXMVector4EqualRClick(Sender: TObject);
        procedure btnXMVector4GreaterOrEqualClick(Sender: TObject);
        procedure btnXMVector4NearEqualClick(Sender: TObject);
        procedure btnXMVectorReplicateClick(Sender: TObject);
        procedure btnXMVectorSetBinaryConstantClick(Sender: TObject);
        procedure btnXMVectorSetClick(Sender: TObject);
        procedure btnXMVectorSetIntClick(Sender: TObject);
        procedure btnXMVectorSplatConstantClick(Sender: TObject);
        procedure btnXMVectorSplatConstantIntClick(Sender: TObject);
        procedure btnXMVectorTrueIntClick(Sender: TObject);
        procedure btnXMVectorZeroClick(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure btnXMVector4EqualIntClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure TabSheet6ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
        procedure TabSheet7ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
    private
        FCheckPerformance: boolean;
        FMSecondsPerCount: double;
    public

    end;



var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function DebugXMVectorFloat(constref v: TXMVector): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.f32[0]) + ', Y: ' + formatfloat('0.000000000E+00', V.f32[1]) +
        ', Z: ' + formatfloat('0.000000000E+00', V.f32[2]) + ', W: ' + formatfloat('0.000000000E+00', V.f32[3]);
end;



function DebugXMFloat2(constref v: TXMFLOAT2): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.x) + ', Y: ' + formatfloat('0.000000000E+00', V.y);
end;



function DebugXMFloat3(constref v: TXMFLOAT3): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.x) + ', Y: ' + formatfloat('0.000000000E+00', V.y) + ', Z: ' + formatfloat('0.000000000E+00', V.z);
end;



function DebugXMFloat4(constref v: TXMFLOAT4): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.x) + ', Y: ' + formatfloat('0.000000000E+00', V.y) + ', Z: ' +
        formatfloat('0.000000000E+00', V.z) + ', W: ' + formatfloat('0.000000000E+00', V.w);
end;



 function DebugXMVectorIntHex(constref v: TXMVector): ansistring;
begin
    Result := 'X: ' + IntToHex( V.u32[0], 8) + ', Y: ' + IntToHex( V.u32[1], 8) +
        ', Z: ' + IntToHex( V.u32[2], 8) + ', W: ' + IntToHex( V.u32[3], 8);
end;

function DebugXMVectorInt(constref v: TXMVector): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.u32[0]) + ', Y: ' + formatfloat('0.000000000E+00', V.u32[1]) +
        ', Z: ' + formatfloat('0.000000000E+00', V.u32[2]) + ', W: ' + formatfloat('0.000000000E+00', V.u32[3]);
end;



function DebugXMINT2(constref v: TXMINT2): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.x) + ', Y: ' + formatfloat('0.000000000E+00', V.y);
end;



function DebugXMINT3(constref v: TXMINT3): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.x) + ', Y: ' + formatfloat('0.000000000E+00', V.y) + ', Z: ' + formatfloat('0.000000000E+00', V.z);
end;



function DebugXMINT4(constref v: TXMINT4): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.x) + ', Y: ' + formatfloat('0.000000000E+00', V.y) + ', Z: ' +
        formatfloat('0.000000000E+00', V.z) + ', W: ' + formatfloat('0.000000000E+00', V.w);
end;



function DebugXMUINT2(constref v: TXMUINT2): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.x) + ', Y: ' + formatfloat('0.000000000E+00', V.y);
end;



function DebugXMUINT3(constref v: TXMUINT3): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.x) + ', Y: ' + formatfloat('0.000000000E+00', V.y) + ', Z: ' + formatfloat('0.000000000E+00', V.z);
end;



function DebugXMUINT4(constref v: TXMUINT4): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.x) + ', Y: ' + formatfloat('0.000000000E+00', V.y) + ', Z: ' +
        formatfloat('0.000000000E+00', V.z) + ', W: ' + formatfloat('0.000000000E+00', V.w);
end;



procedure TForm1.btnXMVectorReplicateClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Float: ' + formatfloat('0.000000000E+00', f));
    Memo1.Lines.Add('Resulting TXMVector:');
    f1 := XMVectorReplicate(f);
    Memo1.Lines.Add(DebugXMVectorFloat(f1));
end;



procedure TForm1.btnXMVectorSetBinaryConstantClick(Sender: TObject);
var
    u1, f: TXMVector;
    u2: PXMVector;
    p0, p1, p2, p3: uint32;
begin
    p0 := 1;
    p1 := 3;
    p2 := 4;
    p3 := 32;
    Memo1.Clear;
    u1.u32[0] := 0;
    u1 := XMVectorSetBinaryConstant(p0, p1, p2, p3);
    // u1:=u2^;
    Memo1.Lines.Add('C0: ' + IntToStr(p0) + ', C1: ' + IntToStr(p1) + ', C2: ' + IntToStr(p2) + ', C3: ' + IntToStr(p3));
    Memo1.Lines.Add('TXMVector U1 : ' + DebugXMVectorFloat(u1));
end;



procedure TForm1.btnXMVectorSetClick(Sender: TObject);
var
    x, y, z, w: single;
    f: TXMVector;
    i: uint32;
    lStartTime: TLargeInteger;
    lCurrTime: TLargeInteger;
begin
    x := 1.23;
    y := 2.55;
    z := 3.44;
    w := 4.89;

    f := XMVectorSet(x, y, z, w);
    Memo1.Clear;
    Memo1.Lines.Add('X: ' + formatfloat('0.000000000E+00', x) + ', Y: ' + formatfloat('0.000000000E+00', y) + ', Z: ' +
        formatfloat('0.000000000E+00', z) + ', W: ' + formatfloat('0.000000000E+00', w));
    Memo1.Lines.Add('TXMVector F: ' + DebugXMVectorFloat(f));

    if FCheckPerformance then
    begin
        QueryPerformanceCounter(lStartTime);
        for i:=1 to 1000000 do
        begin
             f := XMVectorSet(x, y, z, w);
        end;
        QueryPerformanceCounter(lCurrTime);
        Memo1.Lines.Add('Performance: '+ formatfloat('#######.000000000',((lCurrTime-lStartTime)*FMSecondsPerCount))+' µs for 1.000.000 operations');
    end;
end;



procedure TForm1.btnXMVectorSetIntClick(Sender: TObject);
var
    x, y, z, w: uint32;
    f: TXMVector;
begin
    x := 123;
    y := 255;
    z := 344;
    w := 489;
    f := XMVectorSetInt(x, y, z, w);
    Memo1.Clear;
    Memo1.Lines.Add('X: ' + formatfloat('0.000000000E+00', x) + ', Y: ' + formatfloat('0.000000000E+00', y) + ', Z: ' +
        formatfloat('0.000000000E+00', z) + ', W: ' + formatfloat('0.000000000E+00', w));
    Memo1.Lines.Add('TXMVector F: ' + DebugXMVectorInt(f));
end;



procedure TForm1.btnXMVectorSplatConstantClick(Sender: TObject);
var
    f: TXMVector;
    p1, p2: uint32;
begin
    p1 := 233;
    p2 := 4;
    f := XMVectorSplatConstant(p1, p2);
    Memo1.Clear;
    Memo1.Lines.Add('P1: ' + IntToStr(p1) + ', DivExp: ' + IntToStr(p2));
    Memo1.Lines.Add('TXMVector F: ' + DebugXMVectorFloat(f));
end;



procedure TForm1.btnXMVectorSplatConstantIntClick(Sender: TObject);
var
    f: TXMVector;
    p1: uint32;
begin
    p1 := 233;
    f := XMVectorSplatConstantInt(p1);
    Memo1.Clear;
    Memo1.Lines.Add('P1: ' + IntToStr(p1));
    Memo1.Lines.Add('TXMVector F: ' + DebugXMVectorFloat(f));
end;

procedure TForm1.btnXMVectorTrueIntClick(Sender: TObject);
var
    f1: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorTrueInt');
    f1 := XMVectorTrueInt();
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorIntHex(f1));
end;



procedure TForm1.btnXMVectorZeroClick(Sender: TObject);
var
    f: TXMVector;
    i: uint32;
    lStartTime: TLargeInteger;
    lCurrTime: TLargeInteger;
begin
    f := XMVectorZero();
    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F: ' + DebugXMVectorFloat(f));

    if FCheckPerformance then
    begin
        QueryPerformanceCounter(lStartTime);
        for i:=1 to 1000000 do
        begin
             f := XMVectorZero();
        end;
        QueryPerformanceCounter(lCurrTime);
        Memo1.Lines.Add('Performance: '+ formatfloat('#######.000000000',((lCurrTime-lStartTime)*FMSecondsPerCount))+' µs for 1.000.000 operations');
    end;
end;



procedure TForm1.Button1Click(Sender: TObject);
begin

end;



procedure TForm1.btnXMVector4EqualIntClick(Sender: TObject);
var
    f1: TXMVector = (u32: (1, 2, 3, 4));
    f2: TXMVector = (u32: (1, 2, 3, 4));
    f3: TXMVector = (u32: (2, 1, 3, 4));
    f4: TXMVector = (u32: (2, 5, 7, 8));
begin
    Memo1.Clear;
    // Test F1 and F2 = equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorInt(f1));
    Memo1.Lines.Add('TXMVector F2: ' + DebugXMVectorInt(f2));
    if XMVector4Equal(f1, f2) then
        Memo1.Lines.Add('OK:  F1 is equal F2')
    else
        Memo1.Lines.Add('NOK: F1 is not equal F2');
    if XMVector4Equal(f2, f1) then
        Memo1.Lines.Add('OK:  F2 is equal F1')
    else
        Memo1.Lines.Add('NOK: F2 is not equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F3 = not equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorInt(f1));
    Memo1.Lines.Add('TXMVector F3: ' + DebugXMVectorInt(f3));

    if XMVector4Equal(f1, f3) then
        Memo1.Lines.Add('NOK: F1 is equal F3')
    else
        Memo1.Lines.Add('OK:  F1 is not equal F3');
    if XMVector4Equal(f3, f1) then
        Memo1.Lines.Add('NOK: F3 is  equal F1')
    else
        Memo1.Lines.Add('OK:  F3 is not equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F4  = not equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorInt(f1));
    Memo1.Lines.Add('TXMVector F4: ' + DebugXMVectorInt(f4));
    if XMVector4Equal(f1, f4) then
        Memo1.Lines.Add('NOK: F1 is equal F4')
    else
        Memo1.Lines.Add('OK:  F1 is not equal F4');
    if XMVector4Equal(f4, f1) then
        Memo1.Lines.Add('NOK: F4 is equal F1')
    else
        Memo1.Lines.Add('OK:  F4 is not equal F1');

end;



procedure TForm1.btnXMVector4GreaterOrEqualClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f2: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f3: TXMVector = (f32: (2.0, 1.0, 3.0, 4.0));
    f4: TXMVector = (f32: (2.0, 5.0, 7.0, 8.0));
begin
    Memo1.Clear;
    // Test F1 and F2 = equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F2: ' + DebugXMVectorFloat(f2));
    if XMVector4GreaterOrEqual(f1, f2) then
        Memo1.Lines.Add('OK:  F1 is greater or equal F2')
    else
        Memo1.Lines.Add('NOK: F1 is not greater or equal F2');
    if XMVector4GreaterOrEqual(f2, f1) then
        Memo1.Lines.Add('OK:  F2 is greater or equal F1')
    else
        Memo1.Lines.Add('NOK: F2 is not greater or equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F3 = not greater or equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F3: ' + DebugXMVectorFloat(f3));

    if XMVector4GreaterOrEqual(f1, f3) then
        Memo1.Lines.Add('NOK: F1 is greater or equal F3')
    else
        Memo1.Lines.Add('OK:  F1 is not greater or equal F3');
    if XMVector4GreaterOrEqual(f3, f1) then
        Memo1.Lines.Add('NOK: F3 is greater or equal F1')
    else
        Memo1.Lines.Add('OK:  F3 is not greater or equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F4  F4>F1
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F4: ' + DebugXMVectorFloat(f4));
    if XMVector4GreaterOrEqual(f1, f4) then
        Memo1.Lines.Add('NOK: F1 is greater or equal F4')
    else
        Memo1.Lines.Add('OK:  F1 is not greater or equal F4');
    if XMVector4GreaterOrEqual(f4, f1) then
        Memo1.Lines.Add('OK:  F4 is greater or equal F1')
    else
        Memo1.Lines.Add('NOK: F4 is not greater or equal F1');

end;



procedure TForm1.btnXMVector4NearEqualClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f2: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f3: TXMVector = (f32: (1.2, 1.7, 3.4, 4.5));
    f4: TXMVector = (f32: (1.0, 2.0, 2.4, 4.6));
    Epsilon: TXMVector = (f32: (0.5, 0.5, 0.5, 0.5));
begin
    Memo1.Clear;
    // Test F1 and F2 = equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F2: ' + DebugXMVectorFloat(f2));
    Memo1.Lines.Add('TXMVector E : ' + DebugXMVectorFloat(Epsilon));
    if XMVector4NearEqual(f1, f2, Epsilon) then
        Memo1.Lines.Add('OK:  F1 is near equal F2')
    else
        Memo1.Lines.Add('NOK: F1 is not near equal F2');
    if XMVector4NearEqual(f2, f1, Epsilon) then
        Memo1.Lines.Add('OK:  F2 is near equal F1')
    else
        Memo1.Lines.Add('NOK: F2 is not near equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F3 = near equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F3: ' + DebugXMVectorFloat(f3));
    Memo1.Lines.Add('TXMVector E : ' + DebugXMVectorFloat(Epsilon));

    if XMVector4NearEqual(f1, f3, Epsilon) then
        Memo1.Lines.Add('OK:  F1 is near equal F3')
    else
        Memo1.Lines.Add('NOK: F1 is not near equal F3');
    if XMVector4NearEqual(f3, f1, Epsilon) then
        Memo1.Lines.Add('OK:  F3 is near equal F1')
    else
        Memo1.Lines.Add('NOK: F3 is not near equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F4  = not equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F4: ' + DebugXMVectorFloat(f4));
    Memo1.Lines.Add('TXMVector E : ' + DebugXMVectorFloat(Epsilon));
    if XMVector4NearEqual(f1, f4, Epsilon) then
        Memo1.Lines.Add('NOK: F1 is near equal F4')
    else
        Memo1.Lines.Add('OK:  F1 is not near equal F4');
    if XMVector4NearEqual(f4, f1, Epsilon) then
        Memo1.Lines.Add('NOK: F4 is near equal F1')
    else
        Memo1.Lines.Add('OK:  F4 is not near equal F1');
end;



procedure TForm1.btnXMVector4EqualClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f2: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f3: TXMVector = (f32: (2.0, 1.0, 3.0, 4.0));
    f4: TXMVector = (f32: (2.0, 5.0, 7.0, 8.0));
begin
    Memo1.Clear;
    // Test F1 and F2 = equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F2: ' + DebugXMVectorFloat(f2));
    if XMVector4Equal(f1, f2) then
        Memo1.Lines.Add('OK:  F1 is equal F2')
    else
        Memo1.Lines.Add('NOK: F1 is not equal F2');
    if XMVector4Equal(f2, f1) then
        Memo1.Lines.Add('OK:  F2 is equal F1')
    else
        Memo1.Lines.Add('NOK: F2 is not equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F3 = not equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F3: ' + DebugXMVectorFloat(f3));

    if XMVector4Equal(f1, f3) then
        Memo1.Lines.Add('NOK: F1 is equal F3')
    else
        Memo1.Lines.Add('OK:  F1 is not equal F3');
    if XMVector4Equal(f3, f1) then
        Memo1.Lines.Add('NOK: F3 is  equal F1')
    else
        Memo1.Lines.Add('OK:  F3 is not equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F4  = not equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F4: ' + DebugXMVectorFloat(f4));
    if XMVector4Equal(f1, f4) then
        Memo1.Lines.Add('NOK: F1 is equal F4')
    else
        Memo1.Lines.Add('OK:  F1 is not equal F4');
    if XMVector4Equal(f4, f1) then
        Memo1.Lines.Add('NOK: F4 is equal F1')
    else
        Memo1.Lines.Add('OK:  F4 is not equal F1');
end;



procedure TForm1.btnXMConvertVectorFloatToIntClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
    u2, u3, u4: TXMVector;
    p: uint32;
    i: uint32;
    lStartTime: TLargeInteger;
    lCurrTime: TLargeInteger;
begin
    Memo1.Clear;
    // Convert F1 to U2
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    p := 10;
    u2 := XMConvertVectorFloatToInt(f1, p);
    Memo1.Lines.Add('TXMVector U2: ' + DebugXMVectorInt(u2) + ', MulExponent: ' + IntToStr(p));
    Memo1.Lines.Add('');
    // Convert F1 to U3
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    p := 2;
    u3 := XMConvertVectorFloatToInt(f1, p);
    Memo1.Lines.Add('TXMVector U2: ' + DebugXMVectorInt(u3) + ', MulExponent: ' + IntToStr(p));
    Memo1.Lines.Add('');
    // Convert F1 to U3
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    p := 0;
    u4 := XMConvertVectorFloatToInt(f1, p);
    Memo1.Lines.Add('TXMVector U4: ' + DebugXMVectorInt(u4) + ', MulExponent: ' + IntToStr(p));

    if FCheckPerformance then
    begin
        QueryPerformanceCounter(lStartTime);
        for i:=1 to 1000000 do
        begin
             u2 := XMConvertVectorFloatToInt(f1, p);
        end;
        QueryPerformanceCounter(lCurrTime);
        Memo1.Lines.Add('Performance: '+ formatfloat('#######.000000000',((lCurrTime-lStartTime)*FMSecondsPerCount))+' µs for 1.000.000 operations');
    end;

end;



procedure TForm1.btnXMVectorTanEstClick(Sender: TObject);
begin

end;



procedure TForm1.btnXMVector2EqualClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f2: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f3: TXMVector = (f32: (2.0, 1.0, 3.0, 4.0));
    f4: TXMVector = (f32: (2.0, 5.0, 7.0, 8.0));
begin
    Memo1.Clear;
    // Test F1 and F2 = equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F2: ' + DebugXMVectorFloat(f2));
    if XMVector2Equal(f1, f2) then
        Memo1.Lines.Add('OK:  F1 is equal F2')
    else
        Memo1.Lines.Add('NOK: F1 is not equal F2');
    if XMVector2Equal(f2, f1) then
        Memo1.Lines.Add('OK:  F2 is equal F1')
    else
        Memo1.Lines.Add('NOK: F2 is not equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F3 = not equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F3: ' + DebugXMVectorFloat(f3));

    if XMVector2Equal(f1, f3) then
        Memo1.Lines.Add('NOK: F1 is equal F3')
    else
        Memo1.Lines.Add('OK:  F1 is not equal F3');
    if XMVector2Equal(f3, f1) then
        Memo1.Lines.Add('NOK: F3 is  equal F1')
    else
        Memo1.Lines.Add('OK:  F3 is not equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F4  = not equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F4: ' + DebugXMVectorFloat(f4));
    if XMVector2Equal(f1, f4) then
        Memo1.Lines.Add('NOK: F1 is equal F4')
    else
        Memo1.Lines.Add('OK:  F1 is not equal F4');
    if XMVector2Equal(f4, f1) then
        Memo1.Lines.Add('NOK: F4 is equal F1')
    else
        Memo1.Lines.Add('OK:  F4 is not equal F1');
end;

procedure TForm1.bntXMVectorGetByIndexClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetByIndex');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   f := XMVectorGetByIndex(f1,0);
   Memo1.Lines.Add('Resulting Float x: ' + formatfloat('0.000000000E+00', f));
   f := XMVectorGetByIndex(f1,1);
   Memo1.Lines.Add('Resulting Float y: ' + formatfloat('0.000000000E+00', f));
   f := XMVectorGetByIndex(f1,2);
   Memo1.Lines.Add('Resulting Float z: ' + formatfloat('0.000000000E+00', f));
   f := XMVectorGetByIndex(f1,3);
   Memo1.Lines.Add('Resulting Float w: ' + formatfloat('0.000000000E+00', f));
end;



procedure TForm1.btnXMVector3EqualClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f2: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f3: TXMVector = (f32: (2.0, 1.0, 3.0, 4.0));
    f4: TXMVector = (f32: (2.0, 5.0, 7.0, 8.0));
begin
    Memo1.Clear;
    // Test F1 and F2 = equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F2: ' + DebugXMVectorFloat(f2));
    if XMVector3Equal(f1, f2) then
        Memo1.Lines.Add('OK:  F1 is equal F2')
    else
        Memo1.Lines.Add('NOK: F1 is not equal F2');
    if XMVector3Equal(f2, f1) then
        Memo1.Lines.Add('OK:  F2 is equal F1')
    else
        Memo1.Lines.Add('NOK: F2 is not equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F3 = not equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F3: ' + DebugXMVectorFloat(f3));

    if XMVector3Equal(f1, f3) then
        Memo1.Lines.Add('NOK: F1 is equal F3')
    else
        Memo1.Lines.Add('OK:  F1 is not equal F3');
    if XMVector3Equal(f3, f1) then
        Memo1.Lines.Add('NOK: F3 is  equal F1')
    else
        Memo1.Lines.Add('OK:  F3 is not equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F4  = not equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F4: ' + DebugXMVectorFloat(f4));
    if XMVector3Equal(f1, f4) then
        Memo1.Lines.Add('NOK: F1 is equal F4')
    else
        Memo1.Lines.Add('OK:  F1 is not equal F4');
    if XMVector3Equal(f4, f1) then
        Memo1.Lines.Add('NOK: F4 is equal F1')
    else
        Memo1.Lines.Add('OK:  F4 is not equal F1');
end;

procedure TForm1.btnXMVectorFalseIntClick(Sender: TObject);
var
    f1: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorFalseInt');
    f1 := XMVectorFalseInt();
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorIntHex(f1));

end;

procedure TForm1.btnXMVectorGetByIndexPtrClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetByIndexPtr');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   XMVectorGetByIndexPtr(f,f1,0);
   Memo1.Lines.Add('Resulting Float x: ' + formatfloat('0.000000000E+00', f));
   XMVectorGetByIndexPtr(f,f1,1);
   Memo1.Lines.Add('Resulting Float y: ' + formatfloat('0.000000000E+00', f));
   XMVectorGetByIndexPtr(f,f1,2);
   Memo1.Lines.Add('Resulting Float z: ' + formatfloat('0.000000000E+00', f));
   XMVectorGetByIndexPtr(f,f1,3);
   Memo1.Lines.Add('Resulting Float w: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetIntByIndexClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (u32: (123, 256, 377, 445));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetIntByIndex');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorInt(f1));

   f:=XMVectorGetIntByIndex(f1,0);
   Memo1.Lines.Add('Resulting Int x: ' + formatfloat('0.000000000E+00', f));
   f:=XMVectorGetIntByIndex(f1,1);
   Memo1.Lines.Add('Resulting Int y: ' + formatfloat('0.000000000E+00', f));
   f:=XMVectorGetIntByIndex(f1,2);
   Memo1.Lines.Add('Resulting Int z: ' + formatfloat('0.000000000E+00', f));
   f:=XMVectorGetIntByIndex(f1,3);
   Memo1.Lines.Add('Resulting Int w: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetIntByIndexPtrClick(Sender: TObject);
var
    f: uint32;
    f1: TXMVector = (u32: (123, 256, 377, 445));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetIntByIndexPtr');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorInt(f1));

   XMVectorGetIntByIndexPtr(f,f1,0);
   Memo1.Lines.Add('Resulting Int x: ' + formatfloat('0.000000000E+00', f));
  XMVectorGetIntByIndexPtr(f,f1,1);
   Memo1.Lines.Add('Resulting Int y: ' + formatfloat('0.000000000E+00', f));
   XMVectorGetIntByIndexPtr(f,f1,2);
   Memo1.Lines.Add('Resulting Int z: ' + formatfloat('0.000000000E+00', f));
   XMVectorGetIntByIndexPtr(f,f1,3);
   Memo1.Lines.Add('Resulting Int w: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetIntWClick(Sender: TObject);
var
    f: uint32;
    f1: TXMVector = (u32: (123, 256, 377, 445));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetIntW');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorInt(f1));

   f := XMVectorGetIntW(f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetIntWPtrClick(Sender: TObject);
var
    f: uint32;
    f1: TXMVector = (u32: (123, 256, 377, 445));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetIntWPtr');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorInt(f1));

   XMVectorGetIntWPtr(f,f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetIntXClick(Sender: TObject);
var
    f: uint32;
    f1: TXMVector = (u32: (123, 256, 377, 445));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetIntX');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorInt(f1));

   f := XMVectorGetIntX(f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetIntXPtrClick(Sender: TObject);
var
    f: uint32;
    f1: TXMVector = (u32: (123, 256, 377, 445));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetIntXPtr');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorInt(f1));

   XMVectorGetIntXPtr(f,f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetIntYClick(Sender: TObject);
var
    f: uint32;
    f1: TXMVector = (u32: (123, 256, 377, 445));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetIntY');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorInt(f1));

   f := XMVectorGetIntY(f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetIntYPtrClick(Sender: TObject);
var
    f: uint32;
    f1: TXMVector = (u32: (123, 256, 377, 445));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetIntYPtr');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorInt(f1));

   XMVectorGetIntYPtr(f,f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetIntZClick(Sender: TObject);
var
    f: uint32;
    f1: TXMVector = (u32: (123, 256, 377, 445));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetIntZ');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorInt(f1));

   f := XMVectorGetIntZ(f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetIntZPtrClick(Sender: TObject);
var
    f: uint32;
    f1: TXMVector = (u32: (123, 256, 377, 445));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetIntZPtr');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorInt(f1));

   XMVectorGetIntZPtr(f,f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetWClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetW');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   f := XMVectorGetW(f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetWPtrClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetWPtr');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   XMVectorGetWPtr(f,f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetXClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetX');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   f := XMVectorGetX(f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetXPtrClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetXPtr');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   XMVectorGetXPtr(f,f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetYClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetY');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   f := XMVectorGetY(f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetYPtrClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetYPtr');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   XMVectorGetYPtr(f,f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetZClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetZ');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   f := XMVectorGetZ(f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;

procedure TForm1.btnXMVectorGetZPtrClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorGetZPtr');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   XMVectorGetZPtr(f,f1);
   Memo1.Lines.Add('Resulting Float: ' + formatfloat('0.000000000E+00', f));
end;



procedure TForm1.btnXMVectorReplicateIntClick(Sender: TObject);
var
    f: uint32 = 532;
    f1: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Float: ' + formatfloat('0.000000000E+00', f));
    Memo1.Lines.Add('Resulting TXMVector:');
    f1 := XMVectorReplicateInt(f);
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorInt(f1));

end;

procedure TForm1.btnXMVectorReplicateIntPtrClick(Sender: TObject);
var
    f: uint32 = 532;
    f1: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorReplicateIntPtr');
    Memo1.Lines.Add('Float: ' + formatfloat('0.000000000E+00', f));
    Memo1.Lines.Add('Resulting TXMVector:');
    f1 := XMVectorReplicateIntPtr(@f);
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorInt(f1));

end;



procedure TForm1.btnXMVectorReplicatePtrClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorReplicatePtr');
    Memo1.Lines.Add('Float: ' + formatfloat('0.000000000E+00', f));
    Memo1.Lines.Add('Resulting TXMVector:');
    f1 := XMVectorReplicatePtr(@f);
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorFloat(f1));
end;

procedure TForm1.btnXMVectorSetByIndexClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
    f2: TXMVector ;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorSetByIndex');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   f2 := XMVectorSetByIndex(f1,f,0);
   Memo1.Lines.Add('Setting Float x: ' + formatfloat('0.000000000E+00', f));
   Memo1.Lines.Add('Resulting TXMVector:');
   Memo1.Lines.Add(DebugXMVectorFloat(f2));

   f2 := XMVectorSetByIndex(f1,f,1);
   Memo1.Lines.Add('Setting Float y: ' + formatfloat('0.000000000E+00', f));
    Memo1.Lines.Add('Resulting TXMVector:');
   Memo1.Lines.Add(DebugXMVectorFloat(f2));

   f2 := XMVectorSetByIndex(f1,f,2);
   Memo1.Lines.Add('Setting Float z: ' + formatfloat('0.000000000E+00', f));
    Memo1.Lines.Add('Resulting TXMVector:');
   Memo1.Lines.Add(DebugXMVectorFloat(f2));

   f2 := XMVectorSetByIndex(f1,f,3);
   Memo1.Lines.Add('Setting Float w: ' + formatfloat('0.000000000E+00', f));
    Memo1.Lines.Add('Resulting TXMVector:');
   Memo1.Lines.Add(DebugXMVectorFloat(f2));
end;

procedure TForm1.btnXMVectorSetByIndexPtrClick(Sender: TObject);
var
    f: single = 5.32;
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
    f2: TXMVector ;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorSetByIndexPtr');

    Memo1.Lines.Add('Starting TXMVector:');
    Memo1.Lines.Add(DebugXMVectorFloat(f1));

   f2 := XMVectorSetByIndexPtr(f1,@f,0);
   Memo1.Lines.Add('Setting Float x: ' + formatfloat('0.000000000E+00', f));
   Memo1.Lines.Add('Resulting TXMVector:');
   Memo1.Lines.Add(DebugXMVectorFloat(f2));

   f2 := XMVectorSetByIndexPtr(f1,@f,1);
   Memo1.Lines.Add('Setting Float y: ' + formatfloat('0.000000000E+00', f));
    Memo1.Lines.Add('Resulting TXMVector:');
   Memo1.Lines.Add(DebugXMVectorFloat(f2));

   f2 := XMVectorSetByIndexPtr(f1,@f,2);
   Memo1.Lines.Add('Setting Float z: ' + formatfloat('0.000000000E+00', f));
    Memo1.Lines.Add('Resulting TXMVector:');
   Memo1.Lines.Add(DebugXMVectorFloat(f2));

   f2 := XMVectorSetByIndexPtr(f1,@f,3);
   Memo1.Lines.Add('Setting Float w: ' + formatfloat('0.000000000E+00', f));
    Memo1.Lines.Add('Resulting TXMVector:');
   Memo1.Lines.Add(DebugXMVectorFloat(f2));
end;

procedure TForm1.btnXMVectorSplatEpsilonClick(Sender: TObject);
var
    f1: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorSplatEpsilon');
    Memo1.Lines.Add('Result TXMVector:');
    f1 := XMVectorSplatEpsilon();
    Memo1.Lines.Add('TXMVector F1 (Int)   : ' +DebugXMVectorInt(f1));
    Memo1.Lines.Add('TXMVector F1 (Single): ' +DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F1 (Hex)   : ' +DebugXMVectorIntHex(f1));

end;

procedure TForm1.btnXMVectorSplatInfinityClick(Sender: TObject);
var
    f1: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorSplatInfinity');
    Memo1.Lines.Add('Result TXMVector:');
    f1 := XMVectorSplatInfinity();
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorIntHex(f1));
end;

procedure TForm1.btnXMVectorSplatOneClick(Sender: TObject);
var
    f1: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorSplatOne');
    Memo1.Lines.Add('Result TXMVector:');
    f1 := XMVectorSplatOne();
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorFloat(f1));
end;

procedure TForm1.btnXMVectorSplatQNaNClick(Sender: TObject);
var
    f1: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorSplatQNaN');
    Memo1.Lines.Add('Result TXMVector:');
    f1 := XMVectorSplatQNaN();
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorIntHex(f1));
end;

procedure TForm1.btnXMVectorSplatSignMaskClick(Sender: TObject);
var
    f1: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorSplatSignMask');
    Memo1.Lines.Add('Result TXMVector:');
    f1 := XMVectorSplatSignMask();
    Memo1.Lines.Add('TXMVector F1 (INT)   : ' +DebugXMVectorInt(f1));
    Memo1.Lines.Add('TXMVector F1 (Single): ' +DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F1 (Hex)   : ' +DebugXMVectorIntHex(f1));
end;

procedure TForm1.btnXMVectorSplatWClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
    f2: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorSplatW');
    Memo1.Lines.Add('Start TXMVector:');
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorFloat(f1));
    Memo1.Lines.Add('Result TXMVector:');
    f2 := XMVectorSplatW(f1);
    Memo1.Lines.Add('TXMVector F2: ' +DebugXMVectorFloat(f2));

end;

procedure TForm1.btnXMVectorSplatXClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
    f2: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorSplatX');
    Memo1.Lines.Add('Start TXMVector:');
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorFloat(f1));
    Memo1.Lines.Add('Result TXMVector:');
    f2 := XMVectorSplatX(f1);
    Memo1.Lines.Add('TXMVector F2: ' +DebugXMVectorFloat(f2));
end;

procedure TForm1.btnXMVectorSplatYClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
    f2: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorSplatY');
    Memo1.Lines.Add('Start TXMVector:');
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorFloat(f1));
    Memo1.Lines.Add('Result TXMVector:');
    f2 := XMVectorSplatY(f1);
    Memo1.Lines.Add('TXMVector F2: ' +DebugXMVectorFloat(f2));
end;

procedure TForm1.btnXMVectorSplatZClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
    f2: TXMVector;
begin
    Memo1.Clear;
    Memo1.Lines.Add('Testing XMVectorSplatZ');
    Memo1.Lines.Add('Start TXMVector:');
    Memo1.Lines.Add('TXMVector F1: ' +DebugXMVectorFloat(f1));
    Memo1.Lines.Add('Result TXMVector:');
    f2 := XMVectorSplatZ(f1);
    Memo1.Lines.Add('TXMVector F2: ' +DebugXMVectorFloat(f2));
end;



procedure TForm1.btnXMConvertVectorFloatToUIntClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
    u2, u3, u4: TXMVector;
    p: uint32;
begin
    Memo1.Clear;
    // Convert F1 to U2
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    p := 10;
    u2 := XMConvertVectorFloatToUInt(f1, p);
    Memo1.Lines.Add('TXMVector U2: ' + DebugXMVectorInt(u2) + ', MulExponent: ' + IntToStr(p));
    Memo1.Lines.Add('');
    // Convert F1 to U3
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    p := 2;
    u3 := XMConvertVectorFloatToUInt(f1, p);
    Memo1.Lines.Add('TXMVector U2: ' + DebugXMVectorInt(u3) + ', MulExponent: ' + IntToStr(p));
    Memo1.Lines.Add('');
    // Convert F1 to U3
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    p := 0;
    u4 := XMConvertVectorFloatToUInt(f1, p);
    Memo1.Lines.Add('TXMVector U4: ' + DebugXMVectorInt(u4) + ', MulExponent: ' + IntToStr(p));

end;



procedure TForm1.btnXMConvertVectorIntToFloatClick(Sender: TObject);
var
    u1: TXMVector = (u32: (1, 4, 7, 32));
    f2, f3, f4: TXMVector;
    p: uint32;
begin
    Memo1.Clear;
    // Convert U1 to F2
    Memo1.Lines.Add('TXMVector U1: ' + DebugXMVectorInt(u1));
    p := 10;
    f2 := XMConvertVectorIntToFloat(u1, p);
    Memo1.Lines.Add('TXMVector F2: ' + DebugXMVectorFloat(f2) + ', DivExponent: ' + IntToStr(p));
    Memo1.Lines.Add('');
    // Convert U1 to F3
    Memo1.Lines.Add('TXMVector U1: ' + DebugXMVectorInt(u1));
    p := 2;
    f3 := XMConvertVectorIntToFloat(u1, p);
    Memo1.Lines.Add('TXMVector F3: ' + DebugXMVectorFloat(f3) + ', DivExponent: ' + IntToStr(p));
    Memo1.Lines.Add('');
    // Convert U1 to F4
    Memo1.Lines.Add('TXMVector U1: ' + DebugXMVectorInt(u1));
    p := 0;
    f4 := XMConvertVectorIntToFloat(u1, p);
    Memo1.Lines.Add('TXMVector F4: ' + DebugXMVectorFloat(f4) + ', DivExponent: ' + IntToStr(p));

end;



procedure TForm1.btnXMConvertVectorUIntToFloatClick(Sender: TObject);
var
    u1: TXMVector = (u32: (1, 4, 7, 32));
    f2, f3, f4: TXMVector;
    p: uint32;
begin
    Memo1.Clear;
    // Convert U1 to F2
    Memo1.Lines.Add('TXMVector U1: ' + DebugXMVectorInt(u1));
    p := 10;
    f2 := XMConvertVectorUIntToFloat(u1, p);
    Memo1.Lines.Add('TXMVector F2: ' + DebugXMVectorFloat(f2) + ', DivExponent: ' + IntToStr(p));
    Memo1.Lines.Add('');
    // Convert U1 to F3
    Memo1.Lines.Add('TXMVector U1: ' + DebugXMVectorInt(u1));
    p := 2;
    f3 := XMConvertVectorUIntToFloat(u1, p);
    Memo1.Lines.Add('TXMVector F3: ' + DebugXMVectorFloat(f3) + ', DivExponent: ' + IntToStr(p));
    Memo1.Lines.Add('');
    // Convert U1 to F4
    Memo1.Lines.Add('TXMVector U1: ' + DebugXMVectorInt(u1));
    p := 0;
    f4 := XMConvertVectorUIntToFloat(u1, p);
    Memo1.Lines.Add('TXMVector F4: ' + DebugXMVectorFloat(f4) + ', DivExponent: ' + IntToStr(p));
end;



procedure TForm1.btnXMStoreFloat4AClick(Sender: TObject);
begin
    Memo1.Clear;
    {$HINT ToDo}
end;



procedure TForm1.btnXMStoreFloat2AClick(Sender: TObject);
begin
    Memo1.Clear;
    {$HINT ToDo}
end;



procedure TForm1.btnXMStoreFloat2Click(Sender: TObject);
var
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
    u1: TXMFLOAT2;
begin
    XMStoreFloat2(u1, f1);
    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMFLOAT2 U1: ' + DebugXMFLOAT2(u1));
end;



procedure TForm1.btnXMStoreFloat3AClick(Sender: TObject);
begin
    Memo1.Clear;
    {$HINT ToDo}
end;



procedure TForm1.btnXMStoreFloat3Click(Sender: TObject);
var
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
    u1: TXMFLOAT3;
begin
    XMStoreFloat3(u1, f1);
    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMFLOAT2 U1: ' + DebugXMFLOAT3(u1));
end;



procedure TForm1.btnXMStoreFloat4Click(Sender: TObject);
var
    f1: TXMVector = (f32: (1.23, 2.56, 3.77, 4.45));
    u1: TXMFLOAT4;
begin
    XMStoreFloat4(u1, f1);
    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMFLOAT2 U1: ' + DebugXMFLOAT4(u1));
end;



procedure TForm1.btnXMStoreFloatClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f: single;
begin
    XMStoreFloat(f, f1);
    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('f: ' + formatfloat('0.000000000E+00', f));
end;



procedure TForm1.btnXMStoreInt2AClick(Sender: TObject);
var
    u: TUINT32A_Array2;
    f1: TXMVector = (u32: (1, 2, 3, 4));
begin
    XMStoreInt2A(u, f1);
    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorInt(f1));
    Memo1.Lines.Add('u[0]: ' + IntToStr(u[0]) + ', u[1]: ' + IntToStr(u[1]));
    {$HINT ToDo}
end;



procedure TForm1.btnXMStoreInt2Click(Sender: TObject);
var
    f1: TXMVector = (u32: (1, 2, 3, 4));
    u: array[0..1] of UINT32;
begin
    XMStoreInt2(u, f1);

    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorInt(f1));
    Memo1.Lines.Add('u[0]: ' + IntToStr(u[0]) + ', u[1]: ' + IntToStr(u[1]));
end;



procedure TForm1.btnXMStoreInt3AClick(Sender: TObject);
begin
    Memo1.Clear;
    {$HINT ToDo}
end;



procedure TForm1.btnXMStoreInt3Click(Sender: TObject);
var
    f1: TXMVector = (u32: (1, 2, 3, 4));
    u: array[0..2] of UINT32;
begin
    XMStoreInt3(u, f1);

    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorInt(f1));
    Memo1.Lines.Add('u[0]: ' + IntToStr(u[0]) + ', u[1]: ' + IntToStr(u[1]) + ', u[2]: ' + IntToStr(u[2]));

end;



procedure TForm1.btnXMStoreInt4AClick(Sender: TObject);
begin
    Memo1.Clear;
    {$HINT ToDo}
end;



procedure TForm1.btnXMStoreInt4Click(Sender: TObject);
var
    f1: TXMVector = (u32: (1, 2, 3, 4));
    u: array[0..3] of UINT32;
begin
    XMStoreInt4(u, f1);

    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorInt(f1));
    Memo1.Lines.Add('u[0]: ' + IntToStr(u[0]) + ', u[1]: ' + IntToStr(u[1]) + ', u[2]: ' + IntToStr(u[2]) + ', u[3]: ' + IntToStr(u[3]));

end;



procedure TForm1.btnXMStoreIntClick(Sender: TObject);
var
    f1: TXMVector = (u32: (1, 2, 3, 4));
    u: uint32;
begin
    XMStoreInt(u, f1);

    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorInt(f1));
    Memo1.Lines.Add('u[0]: ' + IntToStr(u));
end;



procedure TForm1.btnXMStoreSInt2Click(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    u1: TXMINT2;
begin
    XMStoreSInt2(u1, f1);

    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMINT2   U1: ' + DebugXMINT2(u1));
end;



procedure TForm1.btnXMStoreSInt3Click(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    u1: TXMINT3;
begin
    XMStoreSInt3(u1, f1);

    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMINT3   U1: ' + DebugXMINT3(u1));
end;



procedure TForm1.btnXMStoreSInt4Click(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    u1: TXMINT4;
begin
    XMStoreSInt4(u1, f1);

    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMINT4   U1: ' + DebugXMINT4(u1));

end;



procedure TForm1.btnXMStoreUInt2Click(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    u1: TXMUINT2;
begin
    XMStoreUInt2(u1, f1);

    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMUINT2  U1: ' + DebugXMUINT2(u1));
end;



procedure TForm1.btnXMStoreUInt3Click(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    u1: TXMUINT3;
begin
    XMStoreUInt3(u1, f1);

    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMUINT3  U1: ' + DebugXMUINT3(u1));
end;



procedure TForm1.btnXMStoreUInt4Click(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    u1: TXMUINT4;
begin
    XMStoreUInt4(u1, f1);

    Memo1.Clear;
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMUINT4  U1: ' + DebugXMUINT4(u1));
end;



procedure TForm1.btnXMVector4EqualIntRClick(Sender: TObject);
var
    f1: TXMVector = (u32: (1, 2, 3, 4));
    f2: TXMVector = (u32: (1, 2, 3, 4));
    f3: TXMVector = (u32: (2, 1, 3, 4));
    f4: TXMVector = (u32: (2, 5, 7, 8));
    u: UINT32;
begin
    Memo1.Clear;
    // Test F1 and F2 = equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorInt(f1));
    Memo1.Lines.Add('TXMVector F2: ' + DebugXMVectorInt(f2));
    u := XMVector4EqualIntR(f1, f2);
    if u = XM_CRMASK_CR6TRUE then
        Memo1.Lines.Add('OK:  all elements of F1 are equal F2')
    else if u = XM_CRMASK_CR6FALSE then
        Memo1.Lines.Add('NOK:  no elements of F1 are equal F2')
    else
        Memo1.Lines.Add('NOK:  some elements of F1 are equal F2');
    Memo1.Lines.Add('');

    // Test F1 and F3 = some equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorInt(f1));
    Memo1.Lines.Add('TXMVector F3: ' + DebugXMVectorInt(f3));
    u := XMVector4EqualIntR(f1, f3);
    if u = XM_CRMASK_CR6TRUE then
        Memo1.Lines.Add('NOK: F1 is equal F3')
    else if u = XM_CRMASK_CR6FALSE then
        Memo1.Lines.Add('NOK: no elements of F1 are equal F3')
    else
        Memo1.Lines.Add('OK:  some elements of F1 are equal F3');
    Memo1.Lines.Add('');

    // Test F1 and F4  = no equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorInt(f1));
    Memo1.Lines.Add('TXMVector F4: ' + DebugXMVectorInt(f4));
    u := XMVector4EqualIntR(f1, f4);
    if u = XM_CRMASK_CR6TRUE then
        Memo1.Lines.Add('NOK: F1 is equal F4')
    else if u = XM_CRMASK_CR6FALSE then
        Memo1.Lines.Add('OK:  no elements of F1 are equal F4')
    else
        Memo1.Lines.Add('NOK: some elements of F1 are equal F4');

end;



procedure TForm1.btnXMVector4EqualRClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f2: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f3: TXMVector = (f32: (2.0, 1.0, 3.0, 4.0));
    f4: TXMVector = (f32: (2.0, 5.0, 7.0, 8.0));
    u: UINT32;
begin
    Memo1.Clear;
    // Test F1 and F2 = equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F2: ' + DebugXMVectorFloat(f2));
    u := XMVector4EqualR(f1, f2);
    if u = XM_CRMASK_CR6TRUE then
        Memo1.Lines.Add('OK:  all elements of F1 are equal F2')
    else if u = XM_CRMASK_CR6FALSE then
        Memo1.Lines.Add('NOK:  no elements of F1 are equal F2')
    else
        Memo1.Lines.Add('NOK:  some elements of F1 are equal F2');
    Memo1.Lines.Add('');

    // Test F1 and F3 = some equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F3: ' + DebugXMVectorFloat(f3));
    u := XMVector4EqualR(f1, f3);
    if u = XM_CRMASK_CR6TRUE then
        Memo1.Lines.Add('NOK: F1 is equal F3')
    else if u = XM_CRMASK_CR6FALSE then
        Memo1.Lines.Add('NOK: no elements of F1 are equal F3')
    else
        Memo1.Lines.Add('OK:  some elements of F1 are equal F3');
    Memo1.Lines.Add('');

    // Test F1 and F4  = no equal
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F4: ' + DebugXMVectorFloat(f4));
    u := XMVector4EqualR(f1, f4);
    if u = XM_CRMASK_CR6TRUE then
        Memo1.Lines.Add('NOK: F1 is equal F4')
    else if u = XM_CRMASK_CR6FALSE then
        Memo1.Lines.Add('OK: no elements of F1 are equal F4')
    else
        Memo1.Lines.Add('NOK: some elements of F1 are equal F4');
end;



procedure TForm1.FormCreate(Sender: TObject);
var
    lCountsPerSec: TLargeInteger;
begin
    Memo1.Clear;
    QueryPerformanceFrequency(lCountsPerSec);
    FMSecondsPerCount := 1000000.0 / lCountsPerSec;
    FCheckPerformance:=TRUE;
end;



procedure TForm1.TabSheet6ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
begin

end;



procedure TForm1.TabSheet7ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
begin

end;


end.
