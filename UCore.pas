unit UCore;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, ActnList, Forms, Dialogs, ExtCtrls,
  ULastOpenedList, UApplicationInfo, UPersistentForm, UScaleDPI, UCommon,
  UTranslator, UDataFile, Menus, URegistry, UTheme, UAboutDialog, Registry;

type

  { TCore }

  TCore = class(TDataModule)
    AAbout: TAction;
    AboutDialog1: TAboutDialog;
    AViewSource: TAction;
    ATest: TAction;
    AFind: TAction;
    AFileSplit: TAction;
    AGenerate: TAction;
    AFindDuplicate: TAction;
    AFileCombine: TAction;
    ASettings: TAction;
    AFileOpenRecent: TAction;
    AHomePage: TAction;
    AFileClose: TAction;
    AFileSaveAs: TAction;
    AFileSave: TAction;
    AFileOpen: TAction;
    AFileNew: TAction;
    AExit: TAction;
    ActionList1: TActionList;
    ApplicationInfo1: TApplicationInfo;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Translator: TTranslator;
    ImageList1: TImageList;
    LastOpenedList1: TLastOpenedList;
    OpenDialog1: TOpenDialog;
    PersistentForm1: TPersistentForm;
    SaveDialog1: TSaveDialog;
    ScaleDPI1: TScaleDPI;
    ThemeManager1: TThemeManager;
    procedure AAboutExecute(Sender: TObject);
    procedure AExitExecute(Sender: TObject);
    procedure AFileCombineExecute(Sender: TObject);
    procedure AFileNewExecute(Sender: TObject);
    procedure AFileOpenExecute(Sender: TObject);
    procedure AFileOpenRecentExecute(Sender: TObject);
    procedure AFileSaveExecute(Sender: TObject);
    procedure AFileSaveAsExecute(Sender: TObject);
    procedure AFileCloseExecute(Sender: TObject);
    procedure AFileSplitExecute(Sender: TObject);
    procedure AFindDuplicateExecute(Sender: TObject);
    procedure AFindExecute(Sender: TObject);
    procedure AGenerateExecute(Sender: TObject);
    procedure AHomePageExecute(Sender: TObject);
    procedure ASettingsExecute(Sender: TObject);
    procedure ATestExecute(Sender: TObject);
    procedure AViewSourceExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure LastOpenedList1Change(Sender: TObject);
  private
    InitializeStarted: Boolean;
    InitializeFinished: Boolean;
    LoadErrors: string;
    ProfileImage: TImage;
    LastSplitDir: string;
    ProfilePhotoFileName: string;
    RecentFileRegistryContext: TRegistryContext;
    procedure FileModified(Sender: TObject);
    function FindFirstNonOption: string;
    procedure UpdateFile;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure DoError(Text: string; Line: Integer);
    procedure AddItemToLastOpenedList(FileName: string);
  public
    DefaultDataFileClass: TDataFileClass;
    DataFile: TDataFile;
    FileClosed: Boolean;
    ReopenLastFileOnStart: Boolean;
    LastContactTabIndex: Integer;
    LastContactFileName: string;
    LastPhotoFileName: string;
    LastPropertyValueFileName: string;
    MapUrl: string;
    GenerateCount: Integer;
    ToolbarVisible: Boolean;
    DefaultVcardVersion: string;
    function GetProfileImage: TImage;
    procedure FileNew;
    procedure FileOpen(FileName: string);
    procedure FileClose;
    procedure Initialize;
    procedure UpdateInterface;
  end;

var
  Core: TCore;


implementation

{$R *.lfm}

uses
  UFormMain, UFormSettings, UContact, UFormContacts, UFormFindDuplicity,
  UFormGenerate, UFormError, UFormFind, UFormTest, UFormSource;

resourcestring
  SAppExit = 'Application exit';
  SAppExitQuery = 'File was modified. Do you want to save it before exit?';
  SFileSplit = 'Contacts split';
  SFileSplitFinishedOpenDirectory = 'Total %d contact files saved. Do you want to open the directory %s?';
  SFileNotFound = 'File ''%s'' not found.';
  SCombinedContacts = 'Combined %d contact files.';
  SLine = 'Line %d: %s';

{ TCore }

procedure TCore.AExitExecute(Sender: TObject);
begin
  FormMain.Close;
end;

procedure TCore.AFileCombineExecute(Sender: TObject);
var
  TempFile: TDataFile;
  I: Integer;
  LoadedFiles: Integer;
begin
  TempFile := DefaultDataFileClass.Create;
  try
    OpenDialog1.Filter := TempFile.GetFileFilter;
  finally
    TempFile.Free;
  end;
  OpenDialog1.DefaultExt := '';
  if Assigned(DataFile) then begin
    OpenDialog1.InitialDir := ExtractFileDir(DataFile.FileName);
    OpenDialog1.FileName := ExtractFileName(DataFile.FileName);
  end;
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
  if OpenDialog1.Execute then begin
    LoadedFiles := 0;
    for I := 0 to OpenDialog1.Files.Count - 1 do begin
      if FileExists(OpenDialog1.Files[I]) then begin
        TempFile := TContactsFile.Create;
        try
          TempFile.LoadFromFile(OpenDialog1.Files[I]);
          TContactsFile(DataFile).Contacts.AddContacts(TContactsFile(TempFile).Contacts);
          Inc(LoadedFiles);
        finally
          TempFile.Free;
        end;
      end;
    end;
    if LoadedFiles > 0 then TContactsFile(DataFile).Modified := True;
    ShowMessage(Format(SCombinedContacts, [LoadedFiles]));
    UpdateFile;
  end;
end;

procedure TCore.AAboutExecute(Sender: TObject);
begin
  AboutDialog1.Show;
end;

procedure TCore.AFileCloseExecute(Sender: TObject);
begin
  FileClose;
  UpdateFile;
end;

procedure TCore.AFileSplitExecute(Sender: TObject);
var
  I: Integer;
  C: Integer;
  FileName: string;
  ModalResult: TModalResult;
begin
  C := 0;
  SelectDirectoryDialog1.FileName := LastSplitDir;
  if SelectDirectoryDialog1.Execute then begin
    LastSplitDir := SelectDirectoryDialog1.FileName;
    with TContactsFile(DataFile).Contacts do
    for I := 0 to Count - 1 do begin
      if Items[I].Fields[cfFullName] <> '' then begin
        FileName := SelectDirectoryDialog1.FileName + DirectorySeparator +
          Items[I].FullNameToFileName + VCardFileExt;
        Items[I].SaveToFile(FileName);
        Inc(C);
      end;
    end;
    ModalResult := MessageDlg(SFileSplit,
      Format(SFileSplitFinishedOpenDirectory, [C,
      SelectDirectoryDialog1.FileName]), mtConfirmation, [mbYes, mbNo], 0);
    if ModalResult = mrYes then begin
      {$IFDEF WINDOWS}
      ExecuteProgram('explorer.exe', ['"' + SelectDirectoryDialog1.FileName + '"']);
      {$ENDIF}
      {$IFDEF UNIX}
      ExecuteProgram('/usr/bin/xdg-open', [SelectDirectoryDialog1.FileName]);
      {$ENDIF}
    end;
  end;
end;

procedure TCore.AFindDuplicateExecute(Sender: TObject);
begin
  with TFormFindDuplicity.Create(nil) do
  try
    Contacts := TContactsFile(DataFile).Contacts;
    ShowModal;
    FormContacts.ReloadList;
    FormMain.UpdateInterface;
  finally
    Free;
  end;
end;

procedure TCore.AFindExecute(Sender: TObject);
begin
  with TFormFind.Create(nil) do
  try
    Contacts := TContactsFile(DataFile).Contacts;
    ShowModal;
    FormContacts.ReloadList;
    FormMain.UpdateInterface;
  finally
    Free;
  end;
end;

procedure TCore.AGenerateExecute(Sender: TObject);
begin
  with TFormGenerate.Create(nil) do
  try
    Contacts := TContactsFile(DataFile).Contacts;
    if ShowModal = mrOk then begin
      FormContacts.ReloadList;
      FormContacts.UpdateInterface;
      DataFile.Modified := True;
      FormMain.UpdateInterface;
    end;
  finally
    Free;
  end;
end;

procedure TCore.AHomePageExecute(Sender: TObject);
begin
  OpenWebPage(ApplicationInfo1.HomePage);
end;

procedure TCore.ASettingsExecute(Sender: TObject);
begin
  with TFormSettings.Create(nil) do
  try
    LoadData;
    if ShowModal = mrOK then begin
      SaveData;
      ThemeManager1.UseTheme(FormMain);
      ThemeManager1.UseTheme(FormContacts);
    end;
  finally
    Free;
  end;
end;

procedure TCore.ATestExecute(Sender: TObject);
begin
  with TFormTest.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TCore.AViewSourceExecute(Sender: TObject);
begin
  with TFormSource.Create(nil) do
  try
    Source := TContactsFile(DataFile).AsString;
    if ShowModal = mrOk then begin
      TContactsFile(DataFile).AsString := Source;
      UpdateFile;
    end;
  finally
    Free;
  end;
end;

procedure TCore.AFileNewExecute(Sender: TObject);
begin
  FileNew;
  UpdateFile;
end;

procedure TCore.AFileOpenExecute(Sender: TObject);
var
  TempFile: TDataFile;
begin
  TempFile := DefaultDataFileClass.Create;
  try
    OpenDialog1.Filter := TempFile.GetFileFilter;
  finally
    TempFile.Free;
  end;
  OpenDialog1.DefaultExt := '';
  if Assigned(DataFile) then begin
    OpenDialog1.InitialDir := ExtractFileDir(DataFile.FileName);
    OpenDialog1.FileName := ExtractFileName(DataFile.FileName);
  end;
  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
  if OpenDialog1.Execute then begin
    FileOpen(OpenDialog1.FileName);
    UpdateFile;
  end;
end;

procedure TCore.AFileOpenRecentExecute(Sender: TObject);
begin
  FileOpen(TMenuItem(Sender).Caption);
  UpdateFile;
end;

procedure TCore.AFileSaveAsExecute(Sender: TObject);
begin
  SaveDialog1.DefaultExt := DataFile.GetFileExt;
  SaveDialog1.Filter := DataFile.GetFileFilter;
  SaveDialog1.InitialDir := ExtractFileDir(DataFile.FileName);
  SaveDialog1.FileName := ExtractFileName(DataFile.FileName);
  if SaveDialog1.Execute then begin
    DataFile.SaveToFile(SaveDialog1.FileName);
    AddItemToLastOpenedList(SaveDialog1.FileName);
    UpdateFile;
  end;
end;

procedure TCore.AFileSaveExecute(Sender: TObject);
begin
  if FileExists(DataFile.FileName) then begin
    DataFile.SaveToFile(DataFile.FileName);
    AddItemToLastOpenedList(DataFile.FileName);
    UpdateFile;
  end else AFileSaveAs.Execute;
end;

procedure TCore.DataModuleCreate(Sender: TObject);
{$IFDEF UNIX}
const
  LinuxDataFilesDir = '/usr/share/vCardStudio';
  LinuxLanguagesDir = LinuxDataFilesDir + '/Languages';
  LinuxImagesDir = LinuxDataFilesDir + '/Images';
{$ENDIF}
begin
  ProfilePhotoFileName := 'Images/Profile.png';
  {$IFDEF UNIX}
  // If installed in Linux system then use installation directory for po files
  if not DirectoryExists(Translator.POFilesFolder) and DirectoryExists(LinuxLanguagesDir) then begin
    Translator.POFilesFolder := LinuxLanguagesDir;
  end;
  // If installed in Linux system then use installation directory for images files
  if not DirectoryExists('Images') and DirectoryExists(LinuxImagesDir) then begin
    ProfilePhotoFileName := LinuxImagesDir + DirectorySeparator + 'Profile.png';
  end;
  {$ENDIF}

  DataFile := nil;
  DefaultDataFileClass := TContactsFile;
  FileClosed := True;
  RecentFileRegistryContext := TRegistryContext.Create(ApplicationInfo1.RegistryRoot,
    ApplicationInfo1.RegistryKey + '\RecentFiles');
end;

procedure TCore.DataModuleDestroy(Sender: TObject);
begin
  FileClose;
  SaveConfig;
  if Assigned(ProfileImage) then
    FreeAndNil(ProfileImage);
end;

procedure TCore.LastOpenedList1Change(Sender: TObject);
begin
  LastOpenedList1.LoadToMenuItem(FormMain.MenuItemFileOpenRecent, AFileOpenRecentExecute);
  LastOpenedList1.LoadToMenuItem(FormMain.PopupMenuOpenRecent.Items, AFileOpenRecentExecute);
end;

procedure TCore.FileModified(Sender: TObject);
begin
  UpdateFile;
end;

procedure TCore.FileOpen(FileName: string);
begin
  if FileExists(FileName) then begin
    FileClose;
    if FileClosed then begin
      FileNew;
      LoadErrors := '';
      DataFile.LoadFromFile(FileName);
      AddItemToLastOpenedList(FileName);
      if LoadErrors <> '' then begin
        FormError := TFormError.Create(nil);
        FormError.MemoErrors.Text := LoadErrors;
        FormError.ShowModal;
        FreeAndNil(FormError);
      end;
    end;
  end else ShowMessage(Format(SFileNotFound, [FileName]));
end;

procedure TCore.FileClose;
var
  ModalResult: TModalResult;
  DoClose: Boolean;
begin
  DoClose := False;
  if Assigned(DataFile) then begin
    if DataFile.Modified then begin
       ModalResult := MessageDlg(SAppExit, SAppExitQuery,
       mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if ModalResult = mrYes then begin
        AFileSave.Execute;
        DoClose := True;
      end
      else if ModalResult = mrNo then begin
        DoClose := True;
      end else FileClosed := False;
    end else DoClose := True;
  end else DoClose := True;
  if DoClose then begin
    if Assigned(DataFile) then FreeAndNil(DataFile);
    FileClosed := True;
    UpdateFile;
  end;
end;

procedure TCore.FileNew;
begin
  FileClose;
  if FileClosed then begin
    DataFile := DefaultDataFileClass.Create;
    DataFile.OnModify := FileModified;
    TContactsFile(DataFile).OnError := DoError;
  end;
end;

procedure TCore.UpdateFile;
begin
  UpdateInterface;
  FormMain.UpdateInterface;
  if Assigned(FormContacts) then begin
    if Assigned(DataFile) then
      FormContacts.Contacts := TContactsFile(DataFile).Contacts
      else FormContacts.Contacts := nil;
    FormContacts.ReloadList;
    FormContacts.UpdateInterface;
  end;
end;

procedure TCore.LoadConfig;
begin
  PersistentForm1.RegistryContext := ApplicationInfo1.GetRegistryContext;
  LastOpenedList1.LoadFromRegistry(RecentFileRegistryContext);

  with TRegistryEx.Create do
  try
    CurrentContext := ApplicationInfo1.GetRegistryContext;
    if ValueExists('LanguageCode') then
      Translator.Language := Translator.Languages.SearchByCode(ReadStringWithDefault('LanguageCode', ''))
      else Translator.Language := Translator.Languages.SearchByCode('');
    if ValueExists('Theme') then
      ThemeManager1.Theme := ThemeManager1.Themes.FindByName(ReadStringWithDefault('Theme', 'System'))
      else ThemeManager1.Theme := ThemeManager1.Themes.FindByName('System');
    FormMain.MenuItemToolbar.Checked := ReadBoolWithDefault('ToolBarVisible', True);
    ReopenLastFileOnStart := ReadBoolWithDefault('ReopenLastFileOnStart', True);
    LastContactTabIndex := ReadIntegerWithDefault('LastContactTabIndex', 0);
    LastContactFileName := ReadStringWithDefault('LastContactFileName', '');
    LastSplitDir := ReadStringWithDefault('LastSplitDir', '');
    LastPropertyValueFileName := ReadStringWithDefault('LastPropertyValueFileName', '');
    GenerateCount := ReadIntegerWithDefault('GenerateCount', 1);
    DefaultVcardVersion := ReadStringWithDefault('DefaultVcardVersion', '2.1');
    MapUrl := ReadStringWithDefault('MapUrl', 'https://www.openstreetmap.org/search?query=');
    LastPhotoFileName := ReadStringWithDefault('LastPhotoFileName', '');
  finally
    Free;
  end;
end;

procedure TCore.SaveConfig;
begin
  with TRegistryEx.Create do
  try
    CurrentContext := ApplicationInfo1.GetRegistryContext;
    if Assigned(Translator.Language) and (Translator.Language.Code <> '') then
      WriteString('LanguageCode', Translator.Language.Code)
      else DeleteValue('LanguageCode');
    if Assigned(ThemeManager1.Theme) and (ThemeManager1.Theme.Name <> '') then
      WriteString('Theme', ThemeManager1.Theme.Name)
      else DeleteValue('Theme');
    WriteBool('ToolBarVisible', FormMain.MenuItemToolbar.Checked);
    WriteBool('ReopenLastFileOnStart', ReopenLastFileOnStart);
    WriteInteger('LastContactTabIndex', LastContactTabIndex);
    WriteString('LastContactFileName', LastContactFileName);
    WriteString('LastSplitDir', LastSplitDir);
    WriteString('LastPropertyValueFileName', LastPropertyValueFileName);
    WriteInteger('GenerateCount', GenerateCount);
    WriteString('DefaultVcardVersion', DefaultVcardVersion);
    WriteString('MapUrl', MapUrl);
    WriteString('LastPhotoFileName', LastPhotoFileName);
  finally
    Free;
  end;
end;

procedure TCore.DoError(Text: string; Line: Integer);
begin
  LoadErrors := LoadErrors + Format(SLine, [Line, Text]) + LineEnding;
end;

procedure TCore.AddItemToLastOpenedList(FileName: string);
begin
  with LastOpenedList1 do begin
    LoadFromRegistry(RecentFileRegistryContext);
    AddItem(FileName);
    SaveToRegistry(RecentFileRegistryContext);
  end;
end;

function TCore.GetProfileImage: TImage;
begin
  if not Assigned(ProfileImage) then begin
    ProfileImage := TImage.Create(nil);
    if FileExists(ProfilePhotoFileName) then
      ProfileImage.Picture.LoadFromFile(ProfilePhotoFileName);
  end;
  Result := ProfileImage;
end;

procedure TCore.UpdateInterface;
begin
  AFileSave.Enabled := Assigned(DataFile) and DataFile.Modified;
  AFileSaveAs.Enabled := Assigned(DataFile);
  AFileClose.Enabled := Assigned(DataFile);
  AFileSplit.Enabled := Assigned(DataFile);
  AFileCombine.Enabled := Assigned(DataFile);
  AFind.Enabled := Assigned(DataFile);
  AFindDuplicate.Enabled := Assigned(DataFile);
  AGenerate.Enabled := Assigned(DataFile);
  {$IFOPT D+}
  ATest.Enabled := True;
  {$ENDIF}
  {$IFOPT D-}
  ATest.Enabled := False;
  {$ENDIF}
  ATest.Visible := ATest.Enabled;
end;

procedure TCore.Initialize;
var
  FileNameOption: string;
begin
  if not InitializeStarted then begin
    InitializeStarted := True;
    LoadConfig;

    FileNameOption := FindFirstNonOption;
    if FileNameOption <> '' then begin
      // Open file specified as command line parameter
      AFileNew.Execute;
      DataFile.LoadFromFile(FileNameOption);
      AddItemToLastOpenedList(FileNameOption);
    end else
    if ReopenLastFileOnStart and (LastOpenedList1.Items.Count > 0) and FileExists(LastOpenedList1.Items[0]) then begin
      // Open last opened file
      AFileNew.Execute;
      DataFile.LoadFromFile(LastOpenedList1.Items[0])
    end else AFileNew.Execute;

    UpdateFile;
    InitializeFinished := True;
  end;
end;

function TCore.FindFirstNonOption: string;
var
  S: string;
  I: Integer;
begin
  Result := '';
  for I := 1 to Application.ParamCount do begin
    S := Application.Params[I];
    if S[1] = Application.OptionChar then Continue;
    Result := S;
    Break;
  end;
end;

end.

