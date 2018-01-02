unit MainUnit;

{$mode delphi}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ComCtrls,
    DirectX.Math;

type

    { TForm1 }

    TForm1 = class(TForm)
        btnXMVectorReplicate: TButton;
        btnXMVector4GreaterOrEqual: TButton;
        Memo1: TMemo;
        PageControl1: TPageControl;
        TabSheet1: TTabSheet;
        procedure btnXMVector4GreaterOrEqualClick(Sender: TObject);
        procedure btnXMVectorReplicateClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private

    public

    end;



var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function DebugXMVectorFloat(constref v: TXMVector): ansistring;
begin
    Result := 'X: ' + formatfloat('0.000000000E+00', V.f32[0]) + ', Y: ' + formatfloat('0.000000000E+00', V.f32[1]) + ', Z: ' + formatfloat('0.000000000E+00', V.f32[2]) + ', W: ' + formatfloat('0.000000000E+00', V.f32[3]);
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

procedure TForm1.btnXMVector4GreaterOrEqualClick(Sender: TObject);
var
    f1: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f2: TXMVector = (f32: (1.0, 2.0, 3.0, 4.0));
    f3: TXMVector = (f32: (2.0, 1.0, 3.0, 4.0));
    f4: TXMVector = (f32: (2.0, 5.0, 7.0, 8.0));
begin
    Memo1.Clear;
    // Test F1 and F2
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F2: ' + DebugXMVectorFloat(f2));
    if XMVector4GreaterOrEqual(f1, f2) then
        Memo1.Lines.Add('F1 is greater or equal F2')
    else
        Memo1.Lines.Add('F1 is not greater or equal F2');
    if XMVector4GreaterOrEqual(f2, f1) then
        Memo1.Lines.Add('F2 is greater or equal F1')
    else
        Memo1.Lines.Add('F2 is not greater or equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F3
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F3: ' + DebugXMVectorFloat(f3));

    if XMVector4GreaterOrEqual(f1, f3) then
        Memo1.Lines.Add('F1 is greater or equal F3')
    else
        Memo1.Lines.Add('F1 is not greater or equal F3');
    if XMVector4GreaterOrEqual(f3, f1) then
        Memo1.Lines.Add('F3 is greater or equal F1')
    else
        Memo1.Lines.Add('F3 is not greater or equal F1');
    Memo1.Lines.Add('');

    // Test F1 and F4
    Memo1.Lines.Add('TXMVector F1: ' + DebugXMVectorFloat(f1));
    Memo1.Lines.Add('TXMVector F4: ' + DebugXMVectorFloat(f4));
    if XMVector4GreaterOrEqual(f1, f4) then
        Memo1.Lines.Add('F1 is greater or equal F4')
    else
        Memo1.Lines.Add('F1 is not greater or equal F4');
    if XMVector4GreaterOrEqual(f4, f1) then
        Memo1.Lines.Add('F4 is greater or equal F1')
    else
        Memo1.Lines.Add('F4 is not greater or equal F1');

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    Memo1.Clear;
end;

end.


