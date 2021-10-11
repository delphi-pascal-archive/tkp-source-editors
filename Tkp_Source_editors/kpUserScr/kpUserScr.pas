unit kpUserScr;
//--------------------------------------------------------------
//  TkpUserScreen - component allowing one to embed a console
//                  window in a form
//
//  version 1.55 (freeware, sources)
//  (C) K. Polyakov, 2000-2005
//
//--------------------------------------------------------------
//  Properties:
//    SysStyle   - style for computer messages
//    UserStyle  - style for user messages (input data)
//--------------------------------------------------------------
//  Methods:
//    function InputInteger(var Target: integer; ManualTerminate: PBoolean): Boolean;
//             input an integer into Target, terminate when ManualTerminate
//             is set to True
//    function   InputFloat(var Target: double; ManualTerminate: PBoolean): Boolean;
//             input a float number into Target, terminate when ManualTerminate
//             is set to True
//    function   InputText(var Target: string; ManualTerminate: PBoolean): Boolean;
//             input a text into Target, terminate when ManualTerminate
//             is set to True
//    procedure  AddLine(s: string);
//             add a line with system attributes
//--------------------------------------------------------------
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, kpMemo;

type
  TInputMode = (imNone, imInteger, imFloat, imText, imAdd);
  PBoolean = ^Boolean;
//--------------------------------------------------------------
//        USER SCREEN - declaration
//--------------------------------------------------------------
  TkpUserScreen = class(TkpCustomMemo)
  private
    { Private declarations }
    FOldCaretState: Boolean;
    FInputMode:   TInputMode;
    FInputReady:  Boolean;
    FCount:       integer;
    FWasPoint:    Boolean;             
    FSysStyle:    TCharStyle;
    FUserStyle:   TCharStyle;
    FUserStyleNo: integer;
    FTimer:       TTimer;
    procedure  SetStyle(Index: integer; Value: TCharStyle);
    procedure  RestoreFocus(Sender: TObject);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMChar(var Msg: TMessage); message WM_CHAR;
    function  GetInputString: string;
    property  InputMode: TInputMode read FInputMode write FInputMode;
    procedure StartInput(Mode: TInputMode);
    procedure StopInput;
    procedure DoExit; override;
    property  InputReady: Boolean read FInputReady write FInputReady;
    property  InputString: string read GetInputString;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
                                Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
                                override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function   InputInteger(var Target: integer; ManualTerminate: PBoolean): Boolean;
    function   InputFloat(var Target: double; ManualTerminate: PBoolean): Boolean;
    function   InputText(var Target: string; ManualTerminate: PBoolean): Boolean;
    procedure  AddLine(s: string);
    procedure  AppendLine(Index: integer; s: string);
  published
    { Published declarations }
    property SysStyle: TCharStyle index 0 read FSysStyle write SetStyle;
    property UserStyle: TCharStyle index 1 read FUserStyle write SetStyle;
        {TControl}
    property PopupMenu;
        {TCustomControl}
    property    Align;
    property    Enabled;
    property    ShowHint;
    property    TabOrder;
    property    TabStop;
    property    Visible;
    property    ReadOnly;
        {TkpCustomMemo}
    property    GutterColor;
    property    GutterWidth;
    property    ScrollBars;
    property    Font;
    property    BkColor;
    property    SelColor;
    property    SelBkColor;
    property    Lines;
    property    HiddenCaret;
    property    TabSize;
    {inherited events}
    property    OnClick;
    property    OnDblClick;
    property    OnDragDrop;
    property    OnDragOver;
    property    OnEndDrag;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnStartDrag;
    property    OnEnter;
    property    OnExit;
    property    OnKeyDown;
    property    OnKeyPress;
    property    OnKeyUp;
        {events}
    property    OnGutterDraw;
    property    OnChange;
    property    OnMoveCursor;
    property    OnSelectionChange;
    property    OnStatusChange;
  end;

procedure Register;

implementation

//uses MSDebug;

//--------------------------------------------------------------
//        USER SCREEN - RESTORE FOCUS
//--------------------------------------------------------------
procedure TkpUserScreen.RestoreFocus(Sender: TObject);
begin
  FTimer.Enabled := False;   
  SetFocus;   
end;
//--------------------------------------------------------------
//        USER SCREEN - CREATE
//--------------------------------------------------------------
constructor TkpUserScreen.Create(AOwner: TComponent);
begin
     inherited;
     InputMode := imNone;
     InputReady := False;
     ScrollMode := smStrict;
     FSysStyle := TCharStyle.Create;
     FUserStyle := TCharStyle.Create;
     FTimer := TTimer.Create(Self);
     AutoIndent := False;
     with FTimer do begin
       Enabled := False;
       Interval := 200;
       OnTimer := RestoreFocus;   
     end;
     FUserStyleNo := Styles.Add(clBlack,clWhite,[fsItalic]);
     with FSysStyle do begin
        TextColor := clBlack;
        BkCOlor := clWhite;
        Style := [];
     end;
     with FUserStyle do begin
        TextColor := clBlack;
        BkCOlor := clWhite;
        Style := [fsItalic];
     end;
end;

//--------------------------------------------------------------
//        USER SCREEN - DESTROY
//--------------------------------------------------------------
destructor TkpUserScreen.Destroy;
begin
    FSysStyle.Free;
    FUserStyle.Free;
    FTimer.Free;
    inherited;
end;

//--------------------------------------------------------------
//        USER SCREEN - PAINT
//--------------------------------------------------------------
procedure TkpUserScreen.Paint;
begin
  with SysStyle do
    Styles.Change(0, TextColor, BkCOlor, Style);
  with UserStyle do
    Styles.Change(FUserStyleNo, TextColor, BkCOlor, Style);
  inherited;
end;

//--------------------------------------------------------------
//        USER SCREEN - SET STYLE
//--------------------------------------------------------------
procedure TkpUserScreen.SetStyle(Index: integer; Value: TCharStyle);
var S: TCharStyle;
    No: integer;
begin
  S := nil; No := 0;
  case Index of
    0: begin S := FSysStyle; No := 0; end;
    1: begin S := FUserStyle; No := FUserStyleNo; end;
  end;
  with S do begin
    TextColor := Value.TextColor;
    BkColor := Value.BkColor;
    Style := Value.Style;
    Styles.Change(No, TextColor, BkCOlor, Style);
  end;
end;

//--------------------------------------------------------------
//        USER SCREEN - DO EXIT
//--------------------------------------------------------------
procedure TkpUserScreen.DoExit;
begin
   Application.ProcessMessages;  
   if InputMode in [imInteger, imFloat, imText] then
      FTimer.Enabled := True; //SetFocus;
end;

//--------------------------------------------------------------
//        USER SCREEN - ADD LINE
//--------------------------------------------------------------
procedure TkpUserScreen.AddLine(s: string);
var Ind: integer;
begin
     if (Lines.Count = 1)  and  (Lines[0] = '') then begin
          Ind := 0;
          Lines[0] := S;
     end
     else Ind := Lines.Add(S);
     LineStyle[Ind] := 0;
     CurY := Lines.Count-1;
     Invalidate;
end;

//--------------------------------------------------------------
//        USER SCREEN - APPEND LINE
//--------------------------------------------------------------
procedure TkpUserScreen.AppendLine(Index: integer; s: string);
begin
   if (Index < 0) or (Index >= Lines.Count) then
      raise EListError('Index ' + IntToStr(Index) + ' is out of bounds');
   Lines[Index] := Lines[Index] + s;
   Invalidate;
end;

//--------------------------------------------------------------
//        USER SCREEN - INPUT INTEGER
//--------------------------------------------------------------
function TkpUserScreen.InputInteger(var Target: integer;
         ManualTerminate: PBoolean): Boolean;
begin
     StartInput(imInteger);
     ManualTerminate^ := False;

     while not InputReady and not ManualTerminate^
            do Application.ProcessMessages;

     Result := True;
     if ManualTerminate^ then begin
          StopInput;
          Target := 0;
          Result := False;
     end
     else
       try
          Target := StrToInt(Trim(InputString));
       except
          Target := MaxInt;
          Result := False;
          HiddenCaret := FOldCaretState;
       end;
end;

//--------------------------------------------------------------
//        USER SCREEN - INPUT FLOAT
//--------------------------------------------------------------
function TkpUserScreen.InputFloat(var Target: double; ManualTerminate: PBoolean): Boolean;
begin

     StartInput(imFloat);
     ManualTerminate^ := False;

     while not InputReady and not ManualTerminate^
            do Application.ProcessMessages;

     Result := True;
     if ManualTerminate^ then begin
          StopInput;
          Target := 0;
          Result := False;
     end
     else
       try
          Target := StrToFloat(Trim(InputString));
       except
          Target := MaxInt;
          Result := False;
       end;
end;

//--------------------------------------------------------------
//        USER SCREEN - INPUT TEXT
//--------------------------------------------------------------
function TkpUserScreen.InputText(var Target: string; ManualTerminate: PBoolean): Boolean;
begin
     StartInput(imText);
     ManualTerminate^ := False;

     while not InputReady and not ManualTerminate^
            do Application.ProcessMessages;

     Result := True;
     if ManualTerminate^ then begin
          StopInput;
          Target := '';
          Result := False;
     end
     else Target := InputString;
end;

//--------------------------------------------------------------
//        USER SCREEN - GET INPUT STRING
//--------------------------------------------------------------
function TkpUserScreen.GetInputString: string;
begin
    Result := Lines[CurY];
end;

//--------------------------------------------------------------
//        USER SCREEN - START INPUT
//--------------------------------------------------------------
procedure TkpUserScreen.StartInput(Mode: TInputMode);
var Len: integer;
begin
     if InputMode <> imNone then Exit;

     InputMode := Mode;
     InputReady := False;

     FCount := 0;
     FWasPoint := False;

     Len := Length(Lines[Lines.Count-1]);
     CurY := Lines.Count-1;
     CurX := Len;
     if (Len > 0) then NewLine;
     CurX := 0;
     LineStyle[CurY] := FUserStyleNo;

     FOldCaretState := HiddenCaret;
     HiddenCaret := False;

     SetFocus;
end;

//--------------------------------------------------------------
//        USER SCREEN - STOP INPUT
//--------------------------------------------------------------
procedure TkpUserScreen.StopInput;
begin
    HiddenCaret :=FOldCaretState;
    InputMode := imNone;
end;

//--------------------------------------------------------------
//        USER SCREEN - WM_KEYDOWN
//--------------------------------------------------------------
procedure TkpUserScreen.WMKeyDown(var Msg: TMessage);
var Key: Integer;
begin
   Key:= Msg.WParamLo;
   if (Key = VK_BACK) and (CurX = 0) then Key := 0;
   if (InputMode <> imNone) then begin
      if Key in [VK_HOME,VK_END,VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN] then Key := 0;
      if Key <> 0 then inherited;
   end;
end;

//--------------------------------------------------------------
//        USER SCREEN - WM_CHAR
//--------------------------------------------------------------
procedure TkpUserScreen.WMChar(var Msg: TMessage);
var Key: Integer;
    charDeleted: char;
begin
  if InputMode = imAdd then begin
     inherited;
     Exit;
  end;

  Key:= Msg.WParamLo;

  charDeleted := #0;
  if Key = VK_BACK then begin
     if (CurX = 0) then Key := 0
     else charDeleted := Lines[CurY][CurX];
     Dec(FCount);
  end;

  case InputMode of
    imNone:      Key := 0;
    imInteger:   if not (Key in [$30..$39,Ord('-'),VK_BACK,VK_RETURN]) then
                    Key := 0;
    imFloat:     if not (Key in [$30..$39,Ord('-'),Ord(DecimalSeparator),VK_BACK,VK_RETURN]) then
                    Key := 0;
  end;

  if Key = VK_RETURN then begin
     StopInput;
     Key := 0;
     InputReady := True;
  end;

  if (InputMode in [imInteger,imFloat]) then
     if (Key = Ord('-')) and (FCount > 0) then Key := 0;

  if (InputMode = imFloat) then begin
     if (Key = Ord(DecimalSeparator)) and FWasPoint then Key := 0
     else
     if (Key = VK_BACK) and (charDeleted = DecimalSeparator) then
        FWasPoint := False;
  end;

  if Key <> 0 then begin
     inherited;
     if (Key <> VK_BACK) then Inc(FCount);
     if (InputMode = imFloat) and (Key = Ord(DecimalSeparator)) then
        FWasPoint := True;
  end;

end;

//--------------------------------------------------------------
//        USER SCREEN - MOUSE DOWN
//--------------------------------------------------------------
procedure TkpUserScreen.MouseDown(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: integer);
begin
  if not (InputMode in [imNone,imAdd]) then Exit;
  inherited;
end;
//--------------------------------------------------------------
//        USER SCREEN - MOUSE MOVE
//--------------------------------------------------------------
procedure TkpUserScreen.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not (InputMode in [imNone,imAdd]) then Exit;
  inherited;
end;
//--------------------------------------------------------------
//        USER SCREEN - MOUSE UP
//--------------------------------------------------------------
procedure TkpUserScreen.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (InputMode in [imNone,imAdd]) then Exit;
  inherited;
end;
//--------------------------------------------------------------
//        REGISTER
//--------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('KP', [TkpUserScreen]);
end;

end.
