unit pngtext;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dynlibs;

const
  LIBNAME = 'libpngtext.so.0.1.0';

type
  ByteArray = array of Byte;
  BytePtr = ^Byte;
 	TCardinalPtr = ^Cardinal;
  TReadFromPngFunction = function (filename : PChar; Length : TCardinalPtr) : BytePtr; cdecl;
  TWriteToPngFunction = function (filename, oFilename : PChar; data : BytePtr; Length : Cardinal) : Boolean; cdecl;
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
      _Data : array of Byte;
      /// the quality of the image (0 - 8), 0 is highest, 8 is saturated
      _Quality : Single;
      /// Stores the max number of bytes that can be stored when the image is saturated
      _SaturatedBytesCount : Cardinal;
      /// stores if everything was loaded properly
      _Loaded : Boolean;
      /// The handle to the library
      LibH : TLibHandle;
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
      procedure WriteData(newVal : ByteArray);
    public
      /// stores whether the image was saved since last modified
      IsSaved : Boolean;
      /// Constructor
      Constructor Create;
      /// Destructor
      Destructor Destroy; override;
			/// the filename of Container Image
      property ContainerImage : String read _ContainerPng write WriteContainerPng;
      /// the filename of Output Image
      property OutputImage : String read _OutputPng write WriteOutputPng;
      /// the text to be read or written
      property Data : ByteArray read _Data write WriteData;
      /// the quality of the image (0 - 8), 0 is highest, 8 is saturated
      property Quality : Single read _Quality;
      /// the number of bytes that can be stored with the whole image saturated
      property SaturatedBytesCount : Cardinal read _SaturatedBytesCount;
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
  IsSaved:=False;
  _SaturatedBytesCount:=0;
  LibH:=NilHandle;
  // now load the library
  LibH:=LoadLibrary(LIBNAME);
  if LibH = NilHandle then
  begin
		ShowError('Failed to load library: '+LIBNAME+':'#10+GetLoadErrorStr);
    _Loaded:=False;
	end else
  begin
    // load the functions
    LibReadFromPng:=TReadFromPngFunction(GetProcedureAddress(LibH, 'readFromPng'));
    LibWriteToPng:=TWriteToPngFunction(GetProcedureAddress(LibH, 'writeToPng'));
    LibPngCapacity:=TPngCapacityFunction(GetProcedureAddress(LibH, 'pngCapacity'));
    LibGetQuality:=TGetQualityFunction(GetProcedureAddress(LibH, 'getQuality'));
    LibInit:=TInitTermFunction(GetProcedureAddress(LibH, 'init'));
    LibTerm:=TInitTermFunction(GetProcedureAddress(LibH, 'term'));
		// init it
		LibInit();
		_Loaded:=True;
	end;
end;

Destructor TPngText.Destroy;
begin
  // try to unload it
  LibTerm();
  if (LibH <> NilHandle) and (_Loaded) then
  	if FreeLibrary(LibH) then
    begin
    	LibH:=NilHandle;
      _Loaded:=False;
		end;
end;

procedure TPngText.WriteContainerPng(newVal : String);
begin
  _ContainerPng:=Copy(newVal,0, Length(newVal));
  IsSaved:=False;
end;

procedure TPngText.WriteOutputPng(newVal : String);
begin
  _OutputPng:=Copy(newVal,0, Length(newVal));
  IsSaved:=False;
end;

procedure TPngText.WriteData(newVal : ByteArray);
begin
	_Data:=Copy(newVal,0, Length(newVal));
  IsSaved:=False;
end;

procedure TPngText.Refresh();
var
  ContainerPngPChar : PChar;
  Len : Cardinal;
  ArrayPtr : BytePtr;
  I : Cardinal;
begin
  if FileExists(_ContainerPng) then
  begin
		ContainerPngPChar:=PChar(Copy(_ContainerPng,0,Length(_ContainerPng)));
	  _SaturatedBytesCount:=LibPngCapacity(ContainerPngPChar, 8);
	  _Quality:=LibGetQuality(ContainerPngPChar, Length(_Data));
	  ArrayPtr:=LibReadFromPng(ContainerPngPChar, @Len);
	  SetLength(_Data, Len);
	  if Len > 0 then
	  	for I:=0 to Len-1 do
			begin
				_Data[I]:= Byte(ArrayPtr^);
	      ArrayPtr:=ArrayPtr+1;
			end;
		// since everything is same as it was in image, no need to save
	  IsSaved:=True;
	end;
end;

procedure TPngText.Write();
var
  ContainerPngPChar, OutputPngChar : PChar;
  DataToPass : ByteArray;
begin
	// check if the text will fit
  if (Length(_Data) > _SaturatedBytesCount) or (Length(_Data)> power(2, 24)) then
  begin
  	ShowError('Container Image can not hold that much data. Try using a larger image.');
    IsSaved:=False;
	end
	else begin
    ContainerPngPChar:=PChar(Copy(_ContainerPng,0,Length(_ContainerPng)));
    OutputPngChar:=PChar(Copy(_OutputPng,0,Length(_OutputPng)));
    DataToPass:=Copy(_Data,0,Length(_Data));
    IsSaved:=LibWriteToPng(ContainerPngPChar,OutputPngChar,@DataToPass[0], Length(DataToPass));
	end;
end;

end.

