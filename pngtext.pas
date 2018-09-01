unit pngtext;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dynlibs;

const
  LIBNAME = '/home/nafees/projects/pngtext/libpngtext.so';

type
  ByteArray = array of Byte;
  ByteArrayPtr = ^ByteArray;
  BytePtr = ^Byte;
 	TCardinalPtr = ^Cardinal;
  TReadFromPngFunction = function (filename : PChar; Length : TCardinalPtr) : BytePtr; cdecl;
  TWriteToPngFunction = procedure (filename, oFilename : PChar; data : BytePtr; Length : Cardinal); cdecl;
  TPngCapacityFunction = function (filename : PChar; density : Byte) : Cardinal; cdecl;
  TGetQualityFunction = function (filename : PChar; dataLength : Cardinal) : Single; cdecl;
  TInitTermFunction = procedure (); cdecl;
  TPngText = Class
    private
      /// Filename of the original image
			_ContainerPng : String;
      /// Filename of the resulting image containing Text
      _OutputPng : String;
      /// The text which was read or written to OutputPng
      _Text : String;
      /// the quality of the image (0 - 8), 0 is highest, 8 is saturated
      _Quality : Single;
      /// Stores the max number of bytes that can be stored when the image is saturated
      _SaturatedBytesCount : Cardinal;
      /// stores whether the image was saved since last modified
      _IsSaved : Boolean;
      /// stores if everything was loaded properly
      _Loaded : Boolean;
      /// The handle to the library
      LibHandle : TLibHandle;
      // loaded functions from the library
			LibReadFromPng : TReadFromPngFunction;
      LibWriteToPng : TWriteToPngFunction;
      LibPngCapacity : TPngCapacityFunction;
      LibGetQuality : TGetQualityFunction;
      LibInit : TInitTermFunction;
      LibTerm : TInitTermFunction;
      /// write property for ContainerPng
      procedure WriteContainerPng(newVal : String);
      /// write property for OutputPng
      procedure WriteOutputPng(newVal : String);
      /// write property for Text
      procedure WriteText(newVal : String);
    public
      /// Constructor
      Constructor Create;
      /// Destructor
      Destructor Destroy; override;
			/// the filename of Container Image
      property ContainerImage : String read _ContainerPng write WriteContainerPng;
      /// the filename of Output Image
      property OutputImage : String read _OutputPng write WriteOutputPng;
      /// the text to be read or written
      property Text : String read _Text write WriteText;
      /// the quality of the image (0 - 8), 0 is highest, 8 is saturated
      property Quality : Single read _Quality;
      /// the number of bytes that can be stored with the whole image saturated
      property SaturatedBytesCount : Cardinal read _SaturatedBytesCount;
      /// if the image is saved, and there were no more modifications
			property IsSaved : Boolean read _IsSaved;
      /// if the library & its functions were loaded successfully
      property Loaded : Boolean read _Loaded;
			/// recalls all the value-getting library functions
      procedure Refresh();
      /// writes the Text to Container Image, and writes the resulting image to Output Image
      procedure Write();
	end;

implementation

uses main, math;

Constructor TPngText.Create;
begin
	// set all vars to their default value
  _ContainerPng:='';
  _OutputPng:='';
  _Text:='';
  _isSaved:=False;
  _SaturatedBytesCount:=0;
  LibHandle:=NilHandle;
  // now load the library
  LibHandle:=LoadLibrary(LIBNAME);
  if LibHandle = NilHandle then
  begin
		ShowError('Failed to load library: '+LIBNAME+':'#10+GetLoadErrorStr);
    _Loaded:=False;
	end else
  begin
    // load the functions
    LibReadFromPng:=TReadFromPngFunction(GetProcedureAddress(LibHandle, 'readFromPng'));
    LibWriteToPng:=TWriteToPngFunction(GetProcedureAddress(LibHandle, 'writeToPng'));
    LibPngCapacity:=TPngCapacityFunction(GetProcedureAddress(LibHandle, 'pngCapacity'));
    LibGetQuality:=TGetQualityFunction(GetProcedureAddress(LibHandle, 'getQuality'));
    LibInit:=TInitTermFunction(GetProcedureAddress(LibHandle, 'init'));
    LibTerm:=TInitTermFunction(GetProcedureAddress(LibHandle, 'term'));
		// init it
		LibInit();
		_Loaded:=True;
	end;
end;

Destructor TPngText.Destroy;
begin
  // try to unload it
  LibTerm();
  if LibHandle <> NilHandle then
  	if FreeLibrary(LibHandle) then
    	LibHandle:=NilHandle;
end;

procedure TPngText.WriteContainerPng(newVal : String);
begin
  _ContainerPng:=newVal;
  _isSaved:=False;
end;

procedure TPngText.WriteOutputPng(newVal : String);
begin
  _OutputPng:=newVal;
  _isSaved:=False;
end;

procedure TPngText.WriteText(newVal : String);
begin
	_Text:=newVal;
  _isSaved:=False;
end;

procedure TPngText.Refresh();
var
  ContainerPngPChar : PChar;
  Length : Cardinal;
  ArrayPtr : BytePtr;
  I : Cardinal;
begin
	ContainerPngPChar:=PChar(_ContainerPng);
  _SaturatedBytesCount:=LibPngCapacity(ContainerPngPChar, 8);
  _Quality:=LibGetQuality(ContainerPngPChar, _Text.Length);
  ArrayPtr:=LibReadFromPng(ContainerPngPChar, @Length);
  ShowError('Length is '+IntToStr(Length));
  SetLength(_Text, Length);
  if Length > 0 then
  	for I:=0 to Length-1 do
    	_Text[I]:=Char(Byte(ArrayPtr+I));
  // since everything is same as it was in image, no need to save
  _isSaved:=True;
end;

procedure TPngText.Write();
begin
	// check if the text will fit
  if (_Text.Length > _SaturatedBytesCount) or (_Text.Length > power(2, 24)) then
  begin
  	ShowError('Container Image can not hold that much data. Try using a larger image.');
    _IsSaved:=False;
	end
	else begin
    LibWriteToPng(PChar(_ContainerPng),PChar(_OutputPng),BytePtr(_Text), _Text.Length);
    _isSaved:=True;
	end;
end;

end.

