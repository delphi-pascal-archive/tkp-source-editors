unit kpSyntProp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Grids, DsgnIntf, kpSyntMemo;

type

//--------------------------------------------------------------
//        KP TOKENS EDIT DIALOG - declaration
//--------------------------------------------------------------
  TkpTokensEditDialog = class(TForm)
    StringGrid1: TStringGrid;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ColorDialog1: TColorDialog;
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; Col, Row: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
//--------------------------------------------------------------
//        KP TOKENS PROPERTY - declaration
//--------------------------------------------------------------
    TkpTokensProperty = class( TPropertyEditor )
      function GetAttributes : TPropertyAttributes; override;
      function GetValue: string; override;
      procedure Edit; override;
    end;

procedure Register;

implementation

{$R *.DFM}

{--------------------------------------------------------------
          TOKENS EDIT DIALOG - CREATE
--------------------------------------------------------------}
procedure TkpTokensEditDialog.FormCreate(Sender: TObject);
begin
     with StringGrid1 do begin
          Cells[0,0] := 'команда';
          Cells[1,0] := 'цвет';
     end;
end;

{--------------------------------------------------------------
          TOKENS EDIT DIALOG - DRAW CELL
--------------------------------------------------------------}
procedure TkpTokensEditDialog.StringGrid1DrawCell(Sender: TObject; Col,
  Row: Integer; Rect: TRect; State: TGridDrawState);
var ARow, ACol: integer;
    S: string;
begin
     ARow := Row;
     ACol := Col;

     if ARow = 0 then begin
        with StringGrid1 do begin
            S := Cells[ACol,ARow];
            Brush.Color := $C0C0C0;
            Canvas.FillRect(Rect);
            DrawText(Canvas.Handle, PChar(S), Length(S), Rect,
                     DT_CENTER or DT_VCENTER );
        end;
        Exit;
     end;

     if (ACol = 1) and (ARow <> 0) then begin
        with StringGrid1, Canvas do begin
             if Trim(Cells[0,ARow]) <> '' then begin
                Brush.Color := TColor(Objects[ACol,ARow]);
                FillRect(Rect);
             end;
        end;
     end;

     if (ACol = 0) and (ARow > 0) then
        with StringGrid1, Canvas do begin
             if Trim(Cells[0,ARow]) = '' then
                Objects[1,ARow] := nil;
             if Cells[1,ARow] = '' then
                  Cells[1,ARow] := ' '
             else Cells[1,ARow] := '';
        end;
end;

{--------------------------------------------------------------
          TOKENS EDIT DIALOG - KEY PRESS
--------------------------------------------------------------}
procedure TkpTokensEditDialog.StringGrid1KeyPress(Sender: TObject;
  var Key: Char);
var i, n, k: integer;
begin
     with StringGrid1 do begin

          if Col > 0 then Key := #0;
          if Key = ' ' then Key := #0;

          if Key = #13 then
             if Cells[Col,Row] = '' then begin
                i := Row+1;
                while (i < RowCount) and (Cells[0,i] = '') do Inc(i);
                if (i < RowCount) then begin
                   i := i - Row;
                   for k:=Row to RowCount-1-i do begin
                       Cells[0,k] := Cells[0,k+i];
                       Objects[1,k] := Objects[1,k+i];
                   end;
                   for k:=1 to i do begin
                       Cells[0,RowCount-k] := '';
                       Objects[1,RowCount-k] := nil;
                   end;
                end;
             end
             else begin
                  i := Row - 1;
                  while (i > 0) and (Cells[0,i] = '') do Dec(i);
                  if (i > 0) and (i <> Row-1) then begin
                     Inc(i);
                     n := Row - i;
                     for k:=i to RowCount-1-n do begin
                         Cells[0,k] := Cells[0,k+n];
                         Objects[1,k] := Objects[1,k+n];
                     end;
                     for k:=1 to n do begin
                         Cells[0,RowCount-n] := '';
                         Objects[1,RowCount-n] := nil;
                     end;
                  end;
             end;
     end;
end;

{--------------------------------------------------------------
          TOKENS EDIT DIALOG - STRINGGRID CLICK
--------------------------------------------------------------}
procedure TkpTokensEditDialog.StringGrid1Click(Sender: TObject);
begin
     with StringGrid1 do begin
        if (Col > 0) and (Row > 0) and
           (Cells[0,Row] <> '') then
           with ColorDialog1 do begin
                Color := TColor(Objects[Col,Row]);
                if Execute then begin
                   Objects[Col,Row] := TObject(Color);
                   Invalidate;
                end;
           end;
     end;
end;

{--------------------------------------------------------------
          TOKENS EDIT DIALOG - KEY DOWN
--------------------------------------------------------------}
procedure TkpTokensEditDialog.StringGrid1KeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
     with StringGrid1 do begin
          if (Col = 0) and (Row > 0) then
               Options := Options + [goEditing]
          else Options := Options - [goEditing];
     end;
end;

{--------------------------------------------------------------
          TOKENS EDIT DIALOG - MOUSE DOWN
--------------------------------------------------------------}
procedure TkpTokensEditDialog.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     with StringGrid1 do begin
          if (Col = 0) and (Row > 0) then
               Options := Options + [goEditing]
          else Options := Options - [goEditing];
     end;
end;

{--------------------------------------------------------------
          TOKENS PROPERTY - GET ATTRIBUTES
--------------------------------------------------------------}
function TkpTokensProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

{--------------------------------------------------------------
          TOKENS PROPERTY - GET VALUE
--------------------------------------------------------------}
function TkpTokensProperty.GetValue : string;
begin
  FmtStr( Result, '(%s)', [ GetPropType^.Name ] );
end;

{--------------------------------------------------------------
          TOKENS PROPERTY - EDIT
--------------------------------------------------------------}
procedure TkpTokensProperty.Edit;
var EditStrings: TStringList;
    Dlg: TkpTokensEditDialog;
    i: integer;
begin
   EditStrings := TStringList.Create;
   Dlg := TkpTokensEditDialog.Create( Application );
   with Dlg do begin
      Caption := TComponent(GetComponent(0)).Owner.Name + '.' +
              TComponent(GetComponent(0)).Name + '.' + Name;  
      try
        EditStrings.Assign( TStringList( GetOrdValue ) );
        with StringGrid1 do begin
             if EditStrings.Count > 8 then
                RowCount := EditStrings.Count + 2
             else RowCount := 10;
             for i:=0 to EditStrings.Count-1 do begin
                 Cells[0,i+1] := Trim(EditStrings[i]);
                 Objects[1,i+1] := EditStrings.Objects[i];
             end;
        end;
        if ShowModal = mrOK then begin
           with StringGrid1 do begin
             EditStrings.Clear;
             for i:=1 to RowCount-2 do
                 EditStrings.AddObject(Cells[0,i], Objects[1,i]);
           end;
          SetOrdValue( Longint( EditStrings ) );
        end;
      finally
        Free;
      end;
   end;
   EditStrings.Free;
end;

procedure Register;
begin
    RegisterPropertyEditor( TypeInfo( TkpStringList ),
                            nil, '', TkpTokensProperty );
end;

end.
