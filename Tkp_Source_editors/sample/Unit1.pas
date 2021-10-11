unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, kpMemo, ComCtrls, kpUserScr, kpSyntMemo, ExtCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    kpMemo1: TkpMemo;
    kpUserScreen1: TkpUserScreen;
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    Label1: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label2: TLabel;
    Label3: TLabel;
    kpSyntaxMemo1: TkpSyntaxMemo;
    Button6: TButton;
    Panel1: TPanel;
    Button7: TButton;
    Button8: TButton;
    Panel2: TPanel;
    UndoBtn: TButton;
    RedoBtn: TButton;
    CheckBox1: TCheckBox;
    InsertBtn: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure kpSyntaxMemo1GutterClick(Sender: TObject; LineNo: Integer);
    procedure kpSyntaxMemo1GutterDraw(Sender: TObject; ACanvas: TCanvas;
      LineNo: Integer; rct: TRect);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure kpSyntaxMemo1UndoChange(Sender: TObject; CanUndo,
      CanRedo: Boolean);
    procedure UndoBtnClick(Sender: TObject);
    procedure RedoBtnClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
  private
    StopInput: Boolean;     
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  BreakPointStyle: integer;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
     kpUserScreen1.AddLine(Edit1.Text);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    with kpUserScreen1 do begin
      HiddenCaret := not HiddenCaret;
      if HiddenCaret then
           Button2.Caption := 'Show caret'
      else Button2.Caption := 'Hide caret';
    end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
   StopInput := True;  
end;

procedure TForm1.Button3Click(Sender: TObject);
var i: integer;
begin
     StopInput := False;
     Label1.Caption := '';
     if kpUserScreen1.InputInteger(i, @StopInput) then
        Label1.Caption := IntToStr(i)
     else
     if i = 0 then
          Label1.Caption := 'terminated'
     else Label1.Caption := 'incorrect number';
end;

procedure TForm1.Button4Click(Sender: TObject);
var d: double;
begin
     StopInput := False;
     Label2.Caption := '';
     if kpUserScreen1.InputFloat(d, @StopInput) then
          Label2.Caption := FloatToStr(d)
     else Label2.Caption := 'terminated';
end;

procedure TForm1.Button5Click(Sender: TObject);
var s: string;
begin
     StopInput := False;
     Label3.Caption := '';
     if kpUserScreen1.InputText(s, @StopInput) then
          Label3.Caption := s
     else Label3.Caption := 'terminated';
end;

procedure TForm1.FormCreate(Sender: TObject);
var i, No: integer;
begin
    with kpMemo1 do begin
       No := Styles.Add(clWhite,clRed,[]);
       for i:=0 to Lines.Count-1 do
         if (i mod 5 = 1) then LineStyle[i] := No;
    end;
    with kpSyntaxMemo1 do begin
       No := Styles.Add(clBlack,clWhite,[fsBold]);
       AddWord(No, ['program', 'var', 'writeln', 'begin', 'end', 'uses',
                    'for', 'to', 'do']);
       No := Styles.Add(clGreen,clWhite,[]);
       AddBrackets(No,['''''', '""']);
       BreakPointStyle := Styles.Add(clWhite,clRed,[]);
       LineStyle[5] := BreakPointStyle;
    end;
end;

procedure TForm1.kpSyntaxMemo1GutterClick(Sender: TObject;
  LineNo: Integer);
begin
    with kpSyntaxMemo1 do begin
       LineStyle[LineNo] := BreakPointStyle - LineStyle[LineNo];
    end;
end;

procedure TForm1.kpSyntaxMemo1GutterDraw(Sender: TObject; ACanvas: TCanvas;
  LineNo: Integer; rct: TRect);
var XC, YC: integer;
begin
   if kpSyntaxMemo1.LineStyle[LineNo] = BreakPointStyle then
      with rct,ACanvas do begin
        XC := (Left + Right) div 2;
        YC := (Top + Bottom) div 2;
        Pen.Color := clGray;
        Brush.Color := clRed;
        Ellipse(XC-4,YC-4,XC+4,YC+4);
      end;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
   kpMemo1.Clear;
end;

{$J+}
procedure TForm1.Button8Click(Sender: TObject);
const k: integer = 0;
begin
   with kpMemo1 do begin
     if k = 0 then begin
          SelectChar(CurY,CurX,1);
          Button8.Caption := 'Unselect char';
     end
     else begin
          UnselectChar;
          Button8.Caption := 'Select char';
     end;
     k := 1 - k;
   end;
end;
{$J-}

procedure TForm1.kpSyntaxMemo1UndoChange(Sender: TObject; CanUndo,
  CanRedo: Boolean);
begin
    UndoBtn.Enabled := CanUndo;
    RedoBtn.Enabled := CanRedo;
end;

procedure TForm1.UndoBtnClick(Sender: TObject);
begin
   with kpSyntaxMemo1 do begin
        Undo;
        SetFocus;
   end;
end;

procedure TForm1.RedoBtnClick(Sender: TObject);
begin
   with kpSyntaxMemo1 do begin
        Redo;
        SetFocus;
   end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  kpSyntaxMemo1.CaseSensitive := CheckBox1.Checked;
  kpSyntaxMemo1.Invalidate;
end;

procedure TForm1.InsertBtnClick(Sender: TObject);
begin
     // In this line #7 denotes the cursor position
  kpMemo1.InsertTemplate('begin'#13'  '#7#13'end;');
end;

end.
