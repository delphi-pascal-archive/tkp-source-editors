unit kpSyntMemo;
//--------------------------------------------------------------
//  TkpSyntaxMemo - editor with syntax highlighting
//
//  version 1.54 (freeware, sources)
//  (C) K. Polyakov, 2000-2004
//
//--------------------------------------------------------------
// Properties:
//    LineComment       - string that starts a commentary till the end of line
//    MultiCommentLeft  - string that starts a multiline commentary
//    MultiCommentRight - string that ends a multiline commentary
//    DelimiterStyle    - style for drawing delimiters
//    CommentStyle      - style for drawing commentaries
//    NumberStyle       - style for drawing numbers
//    Delimiters        - set of char that are regarded as delimiters (public)
//    CaseSensitive     - whether syntax highlighting is case sensitive
//--------------------------------------------------------------
// Methods:
//    procedure AddWord(StyleNo: integer; ArrS: array of string);
//              add a list of words that should be highlighted with style StyleNo
//    procedure AddSpecial(StyleNo: integer; ArrS: array of string);
//              add a list of special lines that should be highlighted with style StyleNo
//    procedure AddBrackets(StyleNo: integer; ArrS: array of string);
//              add a list of brackets lines that should be highlighted with style StyleNo
//--------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, kpMemo;

type
//--------------------------------------------------------------
//        KP STRING LIST - сохраняет в потоке и данные из Objects
//--------------------------------------------------------------
  TkpStringList = class(TStringList)
  private
    procedure ReadStrings(Reader: TReader);
    procedure WriteStrings(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
  end;

  TDelimiters = set of char;
  TTokenType = (ttWord, ttBracket, ttSpecial, ttDelimiter, ttSpace,
                ttEOL, ttInteger, ttFloat, ttComment, ttOther,
                ttWrongNumber);

//--------------------------------------------------------------
//        SYNTAX MEMO - declaration
//--------------------------------------------------------------
  TkpSyntaxMemo = class(TkpCustomMemo)
  private
    { Private declarations }
    FIsPainting:   Boolean;
    FInComment: Boolean;

    FWordList:     TkpStringList;
    FSpecialList:  TkpStringList;
    FBracketList:  TkpStringList;
    FDelimiters:   TDelimiters;
    FInBrackets:   integer;
    FLineComment:  string;
    FMultiCommentLeft: string;
    FMultiCommentRight: string;
    FDelimiterStyle: TCharStyle;
    FCommentStyle: TCharStyle;
    FNumberStyle:  TCharStyle;
    FDelimiterStyleNo,
    FCommentStyleNo,
    FNumberStyleNo: integer;
    FCaseSensitive: Boolean;
    function    GetToken(S: string; var From: integer;
                         var TokenType: TTokenType; var StyleNo: integer): string;
    procedure   SetWordList(Value: TkpStringList);
    procedure   SetSpecialList(Value: TkpStringList);
    procedure   SetBracketList(Value: TkpStringList);
    procedure   FindLineAttrs(Sender: TObject; LineNo: integer; var Attrs: string);
    procedure   SetStyle(Index: integer; Value: TCharStyle);
    procedure   SetCaseSensitive(Value: Boolean);
  protected
    { Protected declarations }
    procedure   Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AddWord(StyleNo: integer; ArrS: array of string);
    procedure   AddSpecial(StyleNo: integer; ArrS: array of string);
    procedure   AddBrackets(StyleNo: integer; ArrS: array of string);

    property    Delimiters: TDelimiters read FDelimiters write FDelimiters;
  published
       { Published declarations }
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
    property    AutoIndent;
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
    property    ScrollMode;
    property    UndoLimit;
    property    DelErase;
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
    property    OnGutterClick;
    property    OnGutterDraw;
    property    OnChange;
    property    OnMoveCursor;
    property    OnSelectionChange;
    property    OnStatusChange;
    property    OnUndoChange;
        {TkpSyntaxMemo}
    property    LineComment: string read FLineComment write FLineComment;
    property    MultiCommentLeft: string read FMultiCommentLeft write FMultiCommentLeft;
    property    MultiCommentRight: string read FMultiCommentRight write FMultiCommentRight;

    property    WordList: TkpStringList read FWordList write SetWordList;
    property    SpecialList: TkpStringList read FSpecialList write SetSpecialList;
    property    BracketList: TkpStringList read FBracketList write SetBracketList;

    property    DelimiterStyle: TCharStyle index 0 read FDelimiterStyle write SetStyle;
    property    CommentStyle: TCharStyle index 1 read FCommentStyle write SetStyle;
    property    NumberStyle: TCharStyle index 2 read FNumberStyle write SetStyle;
    property    CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
  end;

procedure Register;

implementation

//uses MSDebug;

//--------------------------------------------------------------
//        SYNTAX MEMO - PAINT
//--------------------------------------------------------------
procedure TkpSyntaxMemo.Paint;
begin
  FIsPainting := True;
try
  DelimiterStyle := FDelimiterStyle;
  CommentStyle := FCommentStyle;
  NumberStyle := FNumberStyle;
  inherited;
finally
  FIsPainting := False;
end;
end;

//--------------------------------------------------------------
//        SYNTAX MEMO - SET STYLE
//--------------------------------------------------------------
procedure TkpSyntaxMemo.SetStyle(Index: integer; Value: TCharStyle);
var No: integer;
    eRect: TRect;
begin
  No := - 1;
  case Index of
    0: No := FDelimiterStyleNo;
    1: No := FCommentStyleNo;
    2: No := FNumberStyleNo;
  end;
  with Value do Styles.Change(No, TextColor, BkColor, Style);
  if not FIsPainting then begin
     eRect := EditorRect;
     InvalidateRect(Handle, @eRect, True);
  end;
end;

//--------------------------------------------------------------
//        SYNTAX MEMO - SET WORD LIST
//--------------------------------------------------------------
procedure TkpSyntaxMemo.SetWordList(Value: TkpStringList);
begin
     FWordList.Assign(Value);
end;

procedure TkpSyntaxMemo.SetSpecialList(Value: TkpStringList);
begin
     FSpecialList.Assign(Value);
end;

procedure TkpSyntaxMemo.SetBracketList(Value: TkpStringList);
begin
     FBracketList.Assign(Value);
end;

//--------------------------------------------------------------
//        SYNTAX MEMO - SET CASE SENSITIVE
//--------------------------------------------------------------
procedure TkpSyntaxMemo.SetCaseSensitive(Value: Boolean);
var LineNo: integer;
begin
  if Value <> FCaseSensitive then begin
     FCaseSensitive := Value;
     for LineNo:=0 to Lines.Count-1 do
        ValidAttrs[LineNo] := False;
     Invalidate;
  end;
end;
                      
//--------------------------------------------------------------
//        SYNTAX MEMO - GET TOKEN
//--------------------------------------------------------------
function TkpSyntaxMemo.GetToken(S: string; var From: integer;
                  var TokenType: TTokenType; var StyleNo: integer): string;
var i, toStart, toEnd, Len, LenSpec: integer;
    Done: Boolean;
    Brackets: string;
    IntPart, FracPart{, NFrac}: integer;
    WasPoint: Boolean;
    //-------------------------------------------------------------
    function StartsFrom(S: string; Pos: integer; S0: string): Boolean;
    begin
      Result := (StrLComp(PChar(S)+Pos-1,PChar(S0),Length(S0)) = 0);
    end;
    //-------------------------------------------------------------
    function Equal(s1, s2: string): Boolean;
    begin
      if FCaseSensitive then
           Result := s1 = s2
      else Result := AnsiLowerCase(s1) = AnsiLowerCase(s2);
    end;
begin
     toStart := From;
     toEnd := From;
     TokenType := ttOther;
     StyleNo := 0;
     Len := Length(S);
             // --- конец строки
     if From > Len then begin
        From := - 1;
        Result := '';
        TokenType := ttEOL;
        StyleNo := 0;
        Exit;
     end;
             // --- начало многострочного комментария
     if (MultiCommentLeft <> '') and (MultiCommentRight <> '') and
        StartsFrom(S, From, MultiCommentLeft) then begin
        Result := MultiCommentLeft;
        FInComment := True;
        TokenType := ttComment;
        StyleNo := FCommentStyleNo;
        Inc(From, Length(MultiCommentLeft));
        Exit;
     end;
             // --- внутри многострочного комментария
     if FInComment then begin
        toEnd := toStart;
        while (toEnd <= Len) and (not StartsFrom(S,toEnd,MultiCommentRight)) do
                     Inc(toEnd);
        if toEnd > Len then begin
           Result := Copy(S,From,toEnd-From);
           From := toEnd;
        end
        else begin
           FInComment := False;
           toEnd := toEnd + Length(MultiCommentRight);
           Result := Copy(S,From,toEnd-From);
           From := toEnd;
        end;
        TokenType := ttComment;
        StyleNo := FCommentStyleNo;
        Exit;
     end;

             // --- внутри скобок
     if FInBrackets >= 0 then begin
        Brackets := FBracketList[FInBrackets];
        toEnd := toStart + 1;
        while (toEnd <= Len) and (S[toEnd] <> Brackets[2]) do
                     Inc(toEnd);
        StyleNo := integer(FBracketList.Objects[FInBrackets]);
        if toEnd <= Len then begin
           FInBrackets := - 1;
           From := toEnd + 1;
        end
        else From := toEnd;
        Result := Copy(S,toStart,toEnd-toStart+1);
        TokenType := ttBracket;
        Exit;
     end;
             // --- пробелы
     while (toStart <= Len) and (S[toStart] = ' ') do
           Inc(toStart);
     if toStart > From then begin
        Result := Copy(S,From,toStart-From);
        From := toStart;
        TokenType := ttSpace;
        StyleNo := 0;
        Exit;
     end;
             // --- комментарий строки
     if (FLineComment <> '') and StartsFrom(S,From,FLineComment) then begin
        Result := Copy(S,From,Len);
        From := Len + 1;
        TokenType := ttComment;
        StyleNo := FCommentStyleNo;
        Exit;
     end;

         // --- специальные сочетания символов
     Done := False;
     for i:=0 to FSpecialList.Count-1 do begin
        LenSpec := Length(FSpecialList[i]);
        if StrLComp(PChar(S)+toStart-1,
                    PChar(FSpecialList[i]),LenSpec) = 0 then begin
           toEnd   := toStart + LenSpec - 1;
           StyleNo := integer(FSpecialList.Objects[i]);
           TokenType := ttSpecial;
           From := toEnd + 1;
           Done := True;
           break;
        end;
     end;
         // --- скобки
     if not Done then begin
        for i:=0 to FBracketList.Count-1 do begin
            Brackets := FBracketList[i];
            if S[toStart] = Brackets[1] then begin
               FInBrackets := i;
               toEnd := toStart + 1;
               while (toEnd <= Len) and (S[toEnd] <> Brackets[2]) do
                     Inc(toEnd);
               if toEnd <= Len then
                    FInBrackets := - 1
               else Dec(toEnd);
               StyleNo := integer(FBracketList.Objects[i]);
               TokenType := ttBracket;
               Done := True;
               break;
            end;
        end;
     end;
         // --- разделители
     if not Done and (S[toStart] in Delimiters) then begin
        toEnd := toStart;
        StyleNo := FDelimiterStyleNo;
        TokenType := ttDelimiter;
        Done := True;
     end;
         // --- целое или вещественое число
     if not Done and (S[toStart] in ['0'..'9','.']) then begin
        IntPart := 0;
        FracPart := 0;
//        NFrac := 0;
        WasPoint := False;
        toEnd := toStart;
        Done := True;
        TokenType := ttInteger;
        StyleNo := FNumberStyleNo;
        while (toEnd <= Len) and (S[toEnd] in ['0'..'9','.']) do begin
          if S[toEnd] = '.' then begin
            if not WasPoint then begin
               WasPoint := True;
               TokenType := ttFloat;
            end
            else begin
               TokenType := ttWrongNumber;
               Color := clRed;
               //break;         //error ( "Две точки в числе", NULL );
            end;
          end
          else
            if not WasPoint then
                 try
                   IntPart := IntPart*10 + Ord(S[toEnd]) - Ord('0');
                 except
                   IntPart := MaxInt;
                 end
            else begin
//                 Inc(NFrac);
//                 FracPart := FracPart*10 + Ord(S[toEnd]) - Ord('0');
            end;
          Inc(toEnd);
        end;
        Dec(toEnd);
     end;
         // --- выделяем слово
     if not Done then begin
        toEnd := toStart;
        while (toEnd <= Len) and not (S[toEnd] in Delimiters) do
           Inc(toEnd);
        Dec(toEnd);
     end;
         // --- ищем в словаре
     Result := Copy(S,toStart,toEnd-toStart+1);
     for i:=0 to FWordList.Count-1 do
        if Equal(Result,FWordList[i]) then begin
           StyleNo := integer(FWordList.Objects[i]);
           TokenType := ttWord;
//           Done := True;
           break;
        end;
     From := toEnd + 1;
end;

//--------------------------------------------------------------
//        SYNTAX MEMO - FIND LINE ATTRS
//--------------------------------------------------------------
procedure TkpSyntaxMemo.FindLineAttrs(Sender: TObject; LineNo: integer;
                                     var Attrs: string);
var i, From, TokenLen: integer;
    S, Token: string;
    TokenType: TTokenType;
    StyleNo, OldInBrackets: integer;
    OldInComment: Boolean;
begin
  S := Lines[LineNo];
  SetLength(Attrs, Length(S));
  FInComment := InComment[LineNo];
  FInBrackets := InBrackets[LineNo];
  From := 1;
  while True do begin
     Token := GetToken(S, From, TokenType, StyleNo);
     if TokenType = ttEOL then break;
     TokenLen := Length(Token);
     for i:=From-TokenLen to From-1 do Attrs[i] := Char(StyleNo);
  end;
  if LineNo < Lines.Count-1 then begin
     OldInComment := InComment[LineNo+1];
     OldInBrackets := InBrackets[LineNo+1];
     if OldInComment <> FInComment then begin
        InComment[LineNo+1] := FInComment;
        ValidAttrs[LineNo+1] := False;
     end;
     if OldInBrackets <> FInBrackets then begin
        InBrackets[LineNo+1] := FInBrackets;
        ValidAttrs[LineNo+1] := False;
     end;
  end;
end;

//--------------------------------------------------------------
//        SYNTAX MEMO - ADD WORD
//--------------------------------------------------------------
procedure TkpSyntaxMemo.AddWord(StyleNo: integer; ArrS: array of string);
var i: integer;
begin
    for i:=Low(ArrS) to high(ArrS) do
      FWordList.AddObject(ArrS[i], TObject(StyleNo));
end;

//--------------------------------------------------------------
//        SYNTAX MEMO - ADD SPECIAL
//--------------------------------------------------------------
procedure TkpSyntaxMemo.AddSpecial(StyleNo: integer; ArrS: array of string);
var i: integer;
begin
    for i:=Low(ArrS) to high(ArrS) do
      FSpecialList.AddObject(ArrS[i], TObject(StyleNo));
end;

//--------------------------------------------------------------
//        SYNTAX MEMO - ADD BRACKETS
//--------------------------------------------------------------
procedure TkpSyntaxMemo.AddBrackets(StyleNo: integer; ArrS: array of string);
var i: integer;
begin
    for i:=Low(ArrS) to high(ArrS) do
      FBracketList.AddObject(ArrS[i], TObject(StyleNo));
end;

//--------------------------------------------------------------
//        SYNTAX MEMO - CREATE
//--------------------------------------------------------------
constructor TkpSyntaxMemo.Create(AOwner: TComponent);
begin
    inherited;
    FInBrackets := -1;
    FIsPainting := False;
    FInComment := False; 
    FWordList := TkpStringList.Create;
    FSpecialList := TkpStringList.Create;
    FBracketList := TkpStringList.Create;

    FDelimiterStyle := TCharStyle.Create;
    with FDelimiterStyle do begin
         TextColor := clBlue; BkColor := clWhite; Style := [];
    end;
    FCommentStyle := TCharStyle.Create;
    with FCommentStyle do begin
         TextColor := clSilver; BkColor := clWhite; Style := [fsItalic];
    end;
    FNumberStyle := TCharStyle.Create;
    with FNumberStyle do begin
         TextColor := clNavy; BkColor := clWhite; Style := [fsBold];
    end;

    FDelimiterStyleNo := Styles.Add(clBlue,clWhite,[]);
    FCommentStyleNo := Styles.Add(clSilver,clWhite,[fsItalic]);
    FNumberStyleNo := Styles.Add(clNavy,clWhite,[fsBold]);

    OnGetLineAttrs := FindLineAttrs;
    Delimiters := [' ', ',', ';', ':', '.', '(', ')', '{', '}', '[', ']',
                   '=', '+', '-', '*', '/', '^', '%', '<', '>',
                      '"', '''', #13, #10];
end;

//--------------------------------------------------------------
//        SYNTAX MEMO - DESTROY
//--------------------------------------------------------------
destructor TkpSyntaxMemo.Destroy;
begin
     FWordList.Free;
     FSpecialList.Free;
     FBracketList.Free;
     FDelimiterStyle.Free;
     FCommentStyle.Free;
     FNumberStyle.Free;
     inherited;
end;

//--------------------------------------------------------------
//        KP STRING LIST - READ STRINGS
//--------------------------------------------------------------
procedure TkpStringList.ReadStrings(Reader: TReader);
var i: Integer;
begin
  try
    Reader.ReadListBegin;
    Clear;
    while not Reader.EndOfList do begin
      i := Add(Reader.ReadString);
      Objects[i] := TObject(Reader.ReadInteger);
    end;
    Reader.ReadListEnd;
  finally
  end;
end;

//--------------------------------------------------------------
//        KP STRING LIST - WRITE STRINGS
//--------------------------------------------------------------
procedure TkpStringList.WriteStrings(Writer: TWriter);
var i: Integer;
begin
  with Writer do begin
    WriteListBegin;
    for i:=0 to Count-1 do begin
      WriteString(Strings[i]);
      WriteInteger(Integer(Objects[i]));
    end;
    WriteListEnd;
  end;
end;

//--------------------------------------------------------------
//        KP STRING LIST - DEFINE PROPERTIES
//--------------------------------------------------------------
procedure TkpStringList.DefineProperties(Filer: TFiler);
begin
//  inherited DefineProperties(Filer);
  Filer.DefineProperty('Strings', ReadStrings, WriteStrings, Count > 0 );
end;

//--------------------------------------------------------------
//        REGISTER
//--------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('KP', [TkpSyntaxMemo]);
end;

end.
