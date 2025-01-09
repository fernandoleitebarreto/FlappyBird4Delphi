unit uGame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Ani, FMX.Controls.Presentation, FMX.StdCtrls, Math, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    ImgBackGround: TImage;
    RectBird: TRectangle;
    RectBottomPipe: TRectangle;
    RectTopPipe: TRectangle;
    RectTopPipe2: TRectangle;
    RectBottomPipe2: TRectangle;
    RectTopPipe3: TRectangle;
    RectBottomPipe3: TRectangle;
    MemoLog: TMemo;
    procedure ImgBackGroundClick(Sender: TObject);
    procedure FloatAnimationFinish(Sender: TObject);
    procedure FloatAnimationJumpBirdFinish(Sender: TObject);
    procedure FloatAnimationJumpBackwardsBirdFinish(Sender: TObject);

    procedure FloatAnimationBirdFinish(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    RandomHeight, idx: Integer;
    TopAjusted, BottomAjusted: Boolean;
    FFloatAnimationBird: TFloatAnimation;
    function GetFloatAnimationBird: TFloatAnimation;
    procedure LoadBackgroundAnimation(Rect: TRectangle);
    procedure LoadBirdAnimation;
    procedure CalculateDuration(var FloatAnimation: TFloatAnimation);
    procedure JumpBird;
    procedure StopAllAnimations;
    function GetCurrentAnimation(Sender: TObject): TFloatAnimation;
    function GetCurrentRectangle(AFloatAnimation: TFloatAnimation): TRectangle;
    function IsTopRectangle(Rect: TRectangle): Boolean;
    procedure ReloadRectangle(FloatAnimation: TFloatAnimation);
    function GetTempCurrentLabel(Rect: TRectangle): TLabel;
    property FloatAnimationBird: TFloatAnimation read GetFloatAnimationBird;
  public
    { Public declarations }
  end;

const
  cTIME_DUMP = 0.3;
  cHEIGHT_DUMP = 30;
  cDEFAULT_RECT_HEIGHT = 200;
  CDEFAULT_RECT_POSITION_Y_TOP = 6;
  CDEFAULT_RECT_POSITION_Y_BOTTOM = 270;
  cSPACE =  ' ';
var
  Form1: TForm1;

implementation

uses
  Winapi.Windows, System.StrUtils;

{$R *.fmx}

procedure TForm1.FloatAnimationBirdFinish(Sender: TObject);
begin
  StopAllAnimations;
  ShowMessage('Game over');

end;

procedure TForm1.StopAllAnimations;
var
  i: Integer;
  Component: TComponent;
begin
  for i := 0 to Self.ComponentCount - 1 do
  begin
    Component := Self.Components[i];
    if Component is TFloatAnimation then
    begin
      if TFloatAnimation(Component).Running then
        TFloatAnimation(Component).Pause := True;
    end;
  end;
end;

function TForm1.GetCurrentAnimation(Sender: TObject): TFloatAnimation;
begin
  if Sender is TFloatAnimation
  then Result := TFloatAnimation(Sender)
  else raise Exception.Create('Object is not a TFloatAnimation');
end;

function TForm1.GetCurrentRectangle(AFloatAnimation: TFloatAnimation): TRectangle;
begin
  if AFloatAnimation.Parent is TRectangle
  then Result := TRectangle(AFloatAnimation.Parent)
  else raise Exception.Create('Parent object is not a TRectangle');
end;

procedure TForm1.FloatAnimationFinish(Sender: TObject);
var
  FloatAnimation: TFloatAnimation;
begin
  FloatAnimation := GetCurrentAnimation(Sender);
  ReloadRectangle(FloatAnimation);
  FloatAnimation.StartValue := ImgBackGround.Width;
  CalculateDuration(FloatAnimation);
  FloatAnimation.Start;
end;

function TForm1.GetFloatAnimationBird: TFloatAnimation;
begin
  if not Assigned(FFloatAnimationBird) then
  begin
    FFloatAnimationBird := TFloatAnimation.Create(Self);
    FFloatAnimationBird.Parent := RectBird;
    FFloatAnimationBird.PropertyName := 'Position.Y';
    FFloatAnimationBird.OnFinish := FloatAnimationBirdFinish;
    FFloatAnimationBird.StartValue := RectBird.Position.Y;
    FFloatAnimationBird.StopValue := ImgBackGround.Height - RectBird.Height + 10;
    FFloatAnimationBird.Duration := 5;
  end;

  Result := FFloatAnimationBird;
end;

procedure TForm1.ImgBackGroundClick(Sender: TObject);
begin
  LoadBackgroundAnimation(RectTopPipe);
  LoadBackgroundAnimation(RectBottomPipe);
  LoadBackgroundAnimation(RectTopPipe2);
  LoadBackgroundAnimation(RectBottomPipe2);
  LoadBackgroundAnimation(RectTopPipe3);
  LoadBackgroundAnimation(RectBottomPipe3);


  LoadBirdAnimation;
end;

function TForm1.IsTopRectangle(Rect: TRectangle): Boolean;
begin
  Result := Rect.Tag = 0;
  
end;

procedure TForm1.JumpBird;
var
  FloatAnimation: TFloatAnimation;
begin
  FloatAnimation := TFloatAnimation.Create(Self);
  FloatAnimation.Parent := RectBird;
  FloatAnimation.PropertyName := 'Position.Y';
  FloatAnimation.OnFinish := FloatAnimationJumpBirdFinish;
  FloatAnimation.StartValue := RectBird.Position.Y;
  FloatAnimation.StopValue := RectBird.Position.Y - cHEIGHT_DUMP - cHEIGHT_DUMP;
  FloatAnimation.Duration := cTIME_DUMP;
  FloatAnimationBird.Pause := True;
  FloatAnimation.Start;

end;

procedure TForm1.FloatAnimationJumpBirdFinish(Sender: TObject);
var
  FloatAnimation: TFloatAnimation;
begin
  FloatAnimation := TFloatAnimation.Create(Self);
  FloatAnimation.Parent := RectBird;
  FloatAnimation.PropertyName := 'Position.Y';
  FloatAnimation.OnFinish := FloatAnimationJumpBackwardsBirdFinish;
  FloatAnimation.StartValue := RectBird.Position.Y;
  FloatAnimation.StopValue := RectBird.Position.Y + cHEIGHT_DUMP;
  FloatAnimation.Duration := cTIME_DUMP;
  FloatAnimation.Start;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MemoLog.Visible := {$IFDEF DEBUG} True {$ELSE} False {$ENDIF};
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if KeyChar = cSPACE then
    JumpBird;
end;

procedure TForm1.FloatAnimationJumpBackwardsBirdFinish(Sender: TObject);
begin
  if FloatAnimationBird.Pause then
  begin
    FreeAndNil(FFloatAnimationBird);
    FloatAnimationBird.Start;
  end;
end;


procedure TForm1.LoadBackgroundAnimation(Rect: TRectangle);
var
  FloatAnimation: TFloatAnimation;
begin
  FloatAnimation := TFloatAnimation.Create(Self);
  FloatAnimation.Parent := Rect;
  FloatAnimation.OnFinish := FloatAnimationFinish;
  FloatAnimation.PropertyName := 'Position.X';
  FloatAnimation.StartValue := Rect.Position.X;
  FloatAnimation.StopValue := Rect.Width * (-1);
  CalculateDuration(FloatAnimation);

  FloatAnimation.Start;
end;

procedure TForm1.LoadBirdAnimation;
begin
  FloatAnimationBird.Start;
end;

procedure TForm1.CalculateDuration(var FloatAnimation: TFloatAnimation);
begin
  FloatAnimation.Duration := (0.0205 * FloatAnimation.StartValue) + 2.5;
end;

{procedure TForm1.ReloadRectangle(FloatAnimation: TFloatAnimation);
var
  Rect: TRectangle;
begin
  Rect := GetCurrentRectangle(FloatAnimation);
  if IsTopRectangle(Rect) then
  begin
    Memo1.Lines.Add('Rect Top');
    if not BottomAjusted then
      RandomHeight := RandomRange(30, 60);

    Rect.Height := cDEFAULT_RECT_HEIGHT - RandomHeight;
    Rect.Position.Y := CDEFAULT_RECT_POSITION_Y_TOP;
    TopAjusted := True;
    BottomAjusted := False;

  end
  else
  begin
    Memo1.Lines.Add('Rect Bottom');
    if not TopAjusted then
      RandomHeight := RandomRange(30, 60);

    Rect.Height := cDEFAULT_RECT_HEIGHT + RandomHeight;
    Rect.Position.Y := CDEFAULT_RECT_POSITION_Y_BOTTOM - RandomHeight;
    BottomAjusted := True;
    TopAjusted := False;
  end;
  Memo1.Lines.Add('Rect.Height: ' + Rect.Height.ToString + sLineBreak +
                  'Rect.Position.Y: ' + Rect.Position.Y.ToString);
  Memo1.Lines.Add('');
end;}

procedure TForm1.ReloadRectangle(FloatAnimation: TFloatAnimation);
var
  Rect: TRectangle;
  IsTop: Boolean;
  AdjustHeight: Integer;
const
  RandomRangeTemp: array of Integer = [50, -50, 0];
begin
  Rect := GetCurrentRectangle(FloatAnimation);
  IsTop := IsTopRectangle(Rect);

  if (IsTop and not BottomAjusted) or (not IsTop and not TopAjusted) then
  begin
    if idx > 2
    then idx := 0;

    RandomHeight := RandomRangeTemp[idx];
    Inc(idx);

    TopAjusted := IsTop;
    BottomAjusted := not IsTop;
  end
  else
    begin
     if IsTop then
       BottomAjusted := False
     else
       TopAjusted := False;
    end;

  AdjustHeight := IfThen(IsTop, -RandomHeight, +RandomHeight);
  Rect.Height := cDEFAULT_RECT_HEIGHT + AdjustHeight;
  Rect.Position.Y := IfThen(IsTop, CDEFAULT_RECT_POSITION_Y_TOP,
    CDEFAULT_RECT_POSITION_Y_BOTTOM - RandomHeight);

  MemoLog.Lines.Add(IfThen(IsTop, 'Rect Top', 'Rect Bottom'));
  MemoLog.Lines.Add('Rect.Height: ' + Rect.Height.ToString + sLineBreak +
                  'Rect.Position.Y: ' + Rect.Position.Y.ToString + sLineBreak +
                  'RandomHeight: ' + RandomHeight.ToString);
  MemoLog.Lines.Add('');
end;

function TForm1.GetTempCurrentLabel(Rect: TRectangle): TLabel;
var
  i: Integer;
  Component: TComponent;
begin
  for i := 0 to Self.ComponentCount - 1 do
  begin
    Component := Self.Components[i];
    if Component is TLabel and (TLabel(Component).Parent = Rect) then
    begin
      Result := TLabel(Component);
      Break;
    end;
  end;
end;

end.
