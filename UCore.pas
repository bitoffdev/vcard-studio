unit UCore;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, ActnList, Forms, Dialogs,
  ULastOpenedList, UApplicationInfo, UPersistentForm, UScaleDPI, UCommon,
  UTranslator, UDataFile, Menus, URegistry, UTheme, UAboutDialog, Registry;

type

  { TMergeResult }

  TMergeResult = record
    Loaded: Integer;
    New: Integer;
    Updated: Integer;
    procedure Clear;
    class operator Add(const A, B: TMergeResult): TMergeResult;
  end;

  { TCore }

  TCore = class(TDataModule)
    AAbout: TAction;
    AboutDialog1: TAboutDialog;
    AGenerate: TAction;
    AFindDuplicate: TAction;
    AFileMerge: TAction;
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
    procedure AFileMergeExecute(Sender: TObject);
    procedure AFileNewExecute(Sender: TObject);
    procedure AFileOpenExecute(Sender: TObject);
    procedure AFileOpenRecentExecute(Sender: TObject);
    procedure AFileSaveExecute(Sender: TObject);
    procedure AFileSaveAsExecute(Sender: TObject);
    procedure AFileCloseExecute(Sender: TObject);
    procedure AFindDuplicateExecute(Sender: TObject);
    procedure AGenerateExecute(Sender: TObject);
    procedure AHomePageExecute(Sender: TObject);
    procedure ASettingsExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure LastOpenedList1Change(Sender: TObject);
  private
    InitializeStarted: Boolean;
    InitializeFinished: Boolean;
    procedure FileModified(Sender: TObject);
    function FindFirstNonOption: string;
    procedure UpdateFile;
    procedure LoadConfig;
    procedure SaveConfig;
  public
    DefaultDataFileClass: TDataFileClass;
    DataFile: TDataFile;
    FileClosed: Boolean;
    ReopenLastFileOnStart: Boolean;
    ToolbarVisible: Boolean;
    procedure FileNew;
    procedure FileOpen(FileName: string);
    procedure FileClose;
    function FileMerge(FileName: string): TMergeResult;
    procedure Initialize;
    procedure UpdateInterface;
  end;

var
  Core: TCore;


implementation

{$R *.lfm}

uses
  UFormMain, UFormAbout, UFormSettings, UContact, UFormContacts, UFormFindDuplicity,
  UFormGenerate;

resourcestring
  SAppExit = 'Application exit';
  SAppExitQuery = 'File was modified. Do you want to save it before exit?';
  SFileNotFound = 'File ''%s'' not found.';
  SMergedContacts = 'Contacts merged. Loaded: %d, New: %d, Updated: %d';

{ TMergeResult }

procedure TMergeResult.Clear;
begin
  Loaded := 0;
  New := 0;
  Updated := 0;
end;

class operator TMergeResult.Add(const A, B: TMergeResult): TMergeResult;
begin
  Result.Loaded := A.Loaded + B.Loaded;
  Result.New := A.New + B.New;
  Result.Updated := A.Updated + B.Updated;
end;

{ TCore }

procedure TCore.AExitExecute(Sender: TObject);
begin
  FormMain.Close;
end;

procedure TCore.AFileMergeExecute(Sender: TObject);
var
  TempFile: TDataFile;
  I: Integer;
  MergeResult: TMergeResult;
  TotalMergeResult: TMergeResult;
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
    TotalMergeResult.Clear;
    for I := 0 to OpenDialog1.Files.Count - 1 do begin
      MergeResult := FileMerge(OpenDialog1.Files[I]);
      TotalMergeResult := TotalMergeResult + MergeResult;
    end;
    ShowMessage(Format(SMergedContacts, [TotalMergeResult.Loaded,
      TotalMergeResult.New, TotalMergeResult.Updated]));
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

procedure TCore.AFindDuplicateExecute(Sender: TObject);
begin
  FormFindDuplicity := TFormFindDuplicity.Create(nil);
  with FormFindDuplicity do begin
    Contacts := TContactsFile(DataFile).Contacts;
    ShowModal;
    FormContacts.ReloadList;
    FormMain.UpdateInterface;
    Free;
  end;
end;

procedure TCore.AGenerateExecute(Sender: TObject);
begin
  FormGenerate := TFormGenerate.Create(nil);
  with FormGenerate do begin
    Contacts := TContactsFile(DataFile).Contacts;
    ShowModal;
    FormContacts.ReloadList;
    FormContacts.UpdateInterface;
    DataFile.Modified := True;
    FormMain.UpdateInterface;
    Free;
  end;
end;

procedure TCore.AHomePageExecute(Sender: TObject);
begin
  OpenWebPage(ApplicationInfo1.HomePage);
end;

procedure TCore.ASettingsExecute(Sender: TObject);
begin
  FormSettings := TFormSettings.Create(nil);
  try
    FormSettings.LoadData;
    if FormSettings.ShowModal = mrOK then begin
      FormSettings.SaveData;
      ThemeManager1.UseTheme(FormMain);
      ThemeManager1.UseTheme(FormContacts);
    end;
  finally
    FormSettings.Free;
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
    LastOpenedList1.AddItem(SaveDialog1.FileName);
    UpdateFile;
  end;
end;

procedure TCore.AFileSaveExecute(Sender: TObject);
begin
  if FileExists(DataFile.FileName) then begin
    DataFile.SaveToFile(DataFile.FileName);
    LastOpenedList1.AddItem(DataFile.FileName);
    UpdateFile;
  end else AFileSaveAs.Execute;
end;

procedure TCore.DataModuleCreate(Sender: TObject);
const
  LinuxLanguagesDir = '/usr/share/vCardStudio/Languages';
begin
  {$IFDEF Linux}
  // If installed in Linux system then use installation directory for po files
  if not DirectoryExists(Translator.POFilesFolder) and DirectoryExists(LinuxLanguagesDir) then
    Translator.POFilesFolder := LinuxLanguagesDir;
  {$ENDIF}

  DataFile := nil;
  DefaultDataFileClass := TContactsFile;
  FileClosed := True;
end;

procedure TCore.DataModuleDestroy(Sender: TObject);
begin
  FileClose;
  SaveConfig;
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
      DataFile.LoadFromFile(FileName);
      LastOpenedList1.AddItem(FileName);
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
  end;
end;

function TCore.FileMerge(FileName: string): TMergeResult;
var
  TempFile: TContactsFile;
  NewContact: TContact;
  I: Integer;
begin
  Result.Clear;
  if FileExists(FileName) then begin
    TempFile := TContactsFile.Create;
    try
      TempFile.LoadFromFile(FileName);
      Result.Loaded := TempFile.Contacts.Count;
      for I := 0 to TempFile.Contacts.Count - 1 do begin
        NewContact := TContactsFile(DataFile).Contacts.Search(TContact(TempFile.Contacts[I]).FullName);
        if not Assigned(NewContact) then begin
          NewContact := TContact.Create;
          NewContact.Parent := TContactsFile(DataFile);
          NewContact.Assign(TContact(TempFile.Contacts[I]));
          TContactsFile(DataFile).Contacts.Add(NewContact);
          Inc(Result.New);
        end else begin
          if NewContact.UpdateFrom(TContact(TempFile.Contacts[I])) then
            Inc(Result.Updated);
        end;
      end;
      TContactsFile(DataFile).Modified := True;
    finally
      TempFile.Free;
    end;
  end else ShowMessage(Format(SFileNotFound, [FileName]));
end;

procedure TCore.FileNew;
begin
  FileClose;
  if FileClosed then begin
    DataFile := DefaultDataFileClass.Create;
    DataFile.OnModify := FileModified;
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
  end;
end;

procedure TCore.LoadConfig;
begin
  PersistentForm1.RegistryContext := ApplicationInfo1.GetRegistryContext;
  LastOpenedList1.LoadFromRegistry(TRegistryContext.Create(ApplicationInfo1.RegistryRoot,
    ApplicationInfo1.RegistryKey + '\RecentFiles'));

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
  finally
    Free;
  end;
end;

procedure TCore.SaveConfig;
begin
  LastOpenedList1.SaveToRegistry(TRegistryContext.Create(ApplicationInfo1.RegistryRoot,
    ApplicationInfo1.RegistryKey + '\RecentFiles'));

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
  finally
    Free;
  end;
end;

procedure TCore.UpdateInterface;
begin
  AFileSave.Enabled := Assigned(DataFile) and DataFile.Modified;
  AFileSaveAs.Enabled := Assigned(DataFile);
  AFileClose.Enabled := Assigned(DataFile);
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
      LastOpenedList1.AddItem(FileNameOption);
    end else
    if (LastOpenedList1.Items.Count > 0) and FileExists(LastOpenedList1.Items[0]) then begin
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

