unit uGame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Ani, FMX.Controls.Presentation, FMX.StdCtrls;

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
    procedure ImgBackGroundClick(Sender: TObject);
    procedure FloatAnimationFinish(Sender: TObject);
    procedure FloatAnimationJumpBirdFinish(Sender: TObject);
    procedure FloatAnimationJumpBackwardsBirdFinish(Sender: TObject);

    procedure FloatAnimationBirdFinish(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FFloatAnimationBird: TFloatAnimation;
    function GetFloatAnimationBird: TFloatAnimation;
    procedure LoadBackgroundAnimation(Rect: TRectangle);
    procedure LoadBirdAnimation;
    procedure CalculateDuration(var FloatAnimation: TFloatAnimation);
    procedure JumpBird;
    procedure StopAllAnimations;
    function GetCurrentAnimation(Sender: TObject): TFloatAnimation;

    property FloatAnimationBird: TFloatAnimation read GetFloatAnimationBird;
  public
    { Public declarations }
  end;

const
  cTIME_DUMP = 0.3;
  cHEIGHT_DUMP = 30;
var
  Form1: TForm1;

implementation

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

procedure TForm1.FloatAnimationFinish(Sender: TObject);
var
  FloatAnimation: TFloatAnimation;
begin
  FloatAnimation := GetCurrentAnimation(Sender);
  FloatAnimation.StartValue := ImgBackGround.Width;
  CalculateDuration(FloatAnimation);
  FloatAnimation.Start;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if KeyChar = ' ' then
    JumpBird;

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

procedure TForm1.JumpBird;
var
  FloatAnimation: TFloatAnimation;
begin
  FloatAnimation := TFloatAnimation.Create(Self);
  FloatAnimation.Parent := RectBird;
  FloatAnimation.PropertyName := 'Position.Y';
  FloatAnimation.OnFinish := FloatAnimationJumpBirdFinish;
  FloatAnimation.StartValue := RectBird.Position.Y;
  FloatAnimation.StopValue := RectBird.Position.Y - cHEIGHT_DUMP;
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

end.
