unit main;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
	Menus, ComCtrls, StdCtrls, about, pngtext;

type

	{ TMainForm }

 TMainForm = class(TForm)
		MainMenu: TMainMenu;
		FileMenuItem: TMenuItem;
		HelpMenuItem: TMenuItem;
		AboutMenuItem: TMenuItem;
		ReadMenuItem: TMenuItem;
		OpenDialog: TOpenDialog;
		QuitMenuItem: TMenuItem;
		SaveDialog: TSaveDialog;
		SaveMenuItem: TMenuItem;
		NewMenuItem: TMenuItem;
		OpenMenuItem: TMenuItem;
		StatusBar: TStatusBar;
		SynEdit: TSynEdit;
		procedure AboutMenuItemClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
  	procedure NewMenuItemClick(Sender: TObject);
  	procedure OpenMenuItemClick(Sender: TObject);
		procedure QuitMenuItemClick(Sender: TObject);
		procedure ReadMenuItemClick(Sender: TObject);
		procedure SaveMenuItemClick(Sender: TObject);
	private
		Lib : TPngText;
    /// updates status bar according to values from Lib
    procedure UpdateStatusBar();
    /// updates the Status of file in StatusBar
    procedure SetStatusBarStatus(Status : String);
    /// updates the Quality of image in StatusBar
    procedure SetStatusBarQuality(Quality : String);
    /// updates the Max Length of image in StatusBar
    procedure SetStatusBarMaxLength(MaxLength : String);
	public

	end;

var
	MainForm: TMainForm;

procedure ShowError(Message : String);

implementation

{$R *.lfm}

{ TMainForm }

procedure ShowError(Message : String);
begin
  MessageDlg('pngtext - Error', Message, mtError, [mbOK],0);
end;

procedure TMainForm.UpdateStatusBar();
begin
  if Lib.IsSaved then
  	StatusBar.Panels[1].Text:='Saved'
  else
  	StatusBar.Panels[1].Text:='Not Saved';

	if Lib.Quality <= 1 then
    StatusBar.Panels[3].Text:='Highest'
  else if (Lib.Quality >= 2) and (Lib.Quality <= 3.5) then
    StatusBar.Panels[3].Text:='High'
  else if (Lib.Quality > 3.5) and (Lib.Quality <= 4.5) then
    StatusBar.Panels[3].Text:='Medium'
	else if (Lib.Quality > 4.5) and (Lib.Quality <= 6) then
    StatusBar.Panels[3].Text:='Low'
  else if Lib.Quality > 7 then
    StatusBar.Panels[3].Text:='Saturated';

  StatusBar.Panels[5].Text:=IntToStr(Lib.SaturatedBytesCount);
end;

procedure TMainForm.SetStatusBarStatus(Status : String);
begin
	StatusBar.Panels[1].Text:=Status;
end;

procedure TMainForm.SetStatusBarQuality(Quality : String);
begin
  StatusBar.Panels[3].Text:=Quality;
end;

procedure TMainForm.SetStatusBarMaxLength(MaxLength : String);
begin
  StatusBar.Panels[5].Text:=MaxLength;
end;

procedure TMainForm.OpenMenuItemClick(Sender: TObject);
begin
	if OpenDialog.Execute then
  begin
    if FileExists(OpenDialog.FileName) then
    begin
    	Lib.ContainerImage:=OpenDialog.FileName;
			// read stuff
      Lib.Refresh();
      // put stuff in status bar
			UpdateStatusBar();
		end else
      ShowError('Selected container image does not exist.');
	end;
end;

procedure TMainForm.QuitMenuItemClick(Sender: TObject);
begin
	MainForm.Close;
end;

procedure TMainForm.ReadMenuItemClick(Sender: TObject);
begin
	// read text into the memo
  SynEdit.Lines.Clear;
  SynEdit.Lines.AddText(AnsiString(Lib.Data));
  UpdateStatusBar();
end;

procedure TMainForm.SaveMenuItemClick(Sender: TObject);
begin
	// make sure that Container Image is set
  if not FileExists(Lib.ContainerImage) then
  	showError ('Invalid container image selected. Use Ctrl+O to select one.')
  else if Lib.OutputImage = '' then
  	if SaveDialog.Execute then
    begin
      Lib.OutputImage:=SaveDialog.FileName;
      Lib.Data:=ByteArray(SynEdit.Lines.Text);
      Lib.write();
      if not Lib.IsSaved then
				ShowError('Failed to save to image.');
		end;
	UpdateStatusBar();
end;

procedure TMainForm.NewMenuItemClick(Sender: TObject);
begin
	Lib.Destroy;
  Lib := TPngText.Create();
  UpdateStatusBar();
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
	AboutForm.Show;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
	Lib:=TPngText.Create();
  if not Lib.Loaded then
  	Application.Terminate;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
	//FreeAndNil(Lib); // Uncommenting this crashes the program right after it closes, idk why
end;

end.

