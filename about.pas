unit about;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

	{ TAboutForm }

 TAboutForm = class(TForm)
		CloseBtn: TButton;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		LicenseMemo: TMemo;
		procedure CloseBtnClick(Sender: TObject);
		procedure FormKeyPress(Sender: TObject; var Key: char);
	private

	public

	end;

var
	AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.CloseBtnClick(Sender: TObject);
begin
  AboutForm.Close;
end;

procedure TAboutForm.FormKeyPress(Sender: TObject; var Key: char);
begin
	if Key = #27 then
  begin
    AboutForm.Close;
	end;
  LicenseMemo.Lines.Add (IntToStr(Ord(Key)));
end;

end.

