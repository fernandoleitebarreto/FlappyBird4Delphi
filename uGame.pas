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
    procedure FloatAnimationBirdFinish(Sender: TObject);
  private
    procedure LoadAnimation(Rect: TRectangle);
    procedure LoadBirdAnimation(Rect: TRectangle);
    procedure CalculateDuration(var FloatAnimation: TFloatAnimation);
    procedure StopAllAnimations;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FloatAnimationBirdFinish(Sender: TObject);
begin
  StopAllAnimations;
  ShowMessage('End of the game');

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


procedure TForm1.FloatAnimationFinish(Sender: TObject);
var
  FloatAnimation: TFloatAnimation;
begin
  if Sender is TFloatAnimation
  then
    begin
      FloatAnimation := TFloatAnimation(Sender);
      FloatAnimation.StartValue := ImgBackGround.Width;
      CalculateDuration(FloatAnimation);
      FloatAnimation.Start;
    end;
end;

procedure TForm1.ImgBackGroundClick(Sender: TObject);
begin
  LoadAnimation(RectTopPipe);
  LoadAnimation(RectBottomPipe);
  LoadAnimation(RectTopPipe2);
  LoadAnimation(RectBottomPipe2);
  LoadAnimation(RectTopPipe3);
  LoadAnimation(RectBottomPipe3);


  LoadBirdAnimation(RectBird);
end;

procedure TForm1.LoadAnimation(Rect: TRectangle);
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

procedure TForm1.LoadBirdAnimation(Rect: TRectangle);
var
  FloatAnimationBird: TFloatAnimation;
begin
  FloatAnimationBird := TFloatAnimation.Create(Self);
  FloatAnimationBird.Parent := Rect;
  FloatAnimationBird.PropertyName := 'Position.Y';
  FloatAnimationBird.OnFinish := FloatAnimationBirdFinish;
  FloatAnimationBird.StartValue := Rect.Position.Y;
  FloatAnimationBird.StopValue := ImgBackGround.Height - Rect.Height + 10;
  FloatAnimationBird.Duration := 5;
  FloatAnimationBird.Start;
end;

procedure TForm1.CalculateDuration(var FloatAnimation: TFloatAnimation);
begin
  FloatAnimation.Duration := (0.0205 * FloatAnimation.StartValue) + 2.5;
end;

end.
