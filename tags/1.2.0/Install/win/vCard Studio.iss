; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "vCard Studio"
#define MyAppNameShort "vCardStudio"
#define MyAppVersion "1.2.0"
;#define MyAppVersionSuffix "alfa"
#define MyAppPublisher "Chronosoft"
#define MyAppPublisherShort "Chronosoft"
#define MyAppURL "https://app.zdechov.net/vcard-studio"
#define MyAppExeName "vCardStudio.exe"
#define MyAppDebugName "vCardStudio.dbg"
#define MyAppSubDir "../.."

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{20E1F71F-1511-425C-BE67-1825A215647D}
AppName={#MyAppName}
#ifdef MyAppVersionSuffix
AppVersion={#MyAppVersion}-{#MyAppVersionSuffix}
AppVerName={#MyAppName} {#MyAppVersion}-{#MyAppVersionSuffix}
OutputBaseFilename=Install-{#MyAppNameShort}-{#MyAppVersion}-{#MyAppVersionSuffix}
#else
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
OutputBaseFilename=Install-{#MyAppNameShort}-{#MyAppVersion}
#endif
UninstallDisplayName={#MyAppName}
UninstallDisplayIcon="{app}\{#MyAppExeName}"
VersionInfoVersion={#MyAppVersion}
VersionInfoCompany={#MyAppPublisher}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={commonpf}\{#MyAppPublisherShort}\{#MyAppName}
DefaultGroupName={#MyAppPublisherShort}\{#MyAppName}
AllowNoIcons=yes
OutputDir=.
Compression=lzma
SolidCompression=yes
ChangesAssociations=yes
; "ArchitecturesInstallIn64BitMode=x64" requests that the install be
; done in "64-bit mode" on x64, meaning it should use the native
; 64-bit Program Files directory and the 64-bit view of the registry.
; On all other architectures it will install in "32-bit mode".
ArchitecturesInstallIn64BitMode=x64
; Note: We don't set ProcessorsAllowed because we want this
; installation to run on all architectures (including Itanium,
; since it's capable of running 32-bit code too).
WizardStyle=modern

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "czech"; MessagesFile: "compiler:Languages\Czech.isl"
;Name: "slovak"; MessagesFile: "compiler:Languages\Slovak.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags:

[Registry]
Root: HKLM; Subkey: "Software\Author\Application"; Flags: uninsdeletekey

#define FileTypeName "vCard file"
Root: HKCR; Subkey: ".vcf"; ValueType: string; ValueName: ""; ValueData: "{#FileTypeName}"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "{#FileTypeName}"; ValueType: string; ValueName: ""; ValueData: "{#FileTypeName}"; Flags: uninsdeletekey
Root: HKCR; Subkey: "{#FileTypeName}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKCR; Subkey: "{#FileTypeName}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""

#define FileTypeName "vCard file"
Root: HKCR; Subkey: ".vcard"; ValueType: string; ValueName: ""; ValueData: "{#FileTypeName}"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "{#FileTypeName}"; ValueType: string; ValueName: ""; ValueData: "{#FileTypeName}"; Flags: uninsdeletekey
Root: HKCR; Subkey: "{#FileTypeName}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKCR; Subkey: "{#FileTypeName}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""

[Files]
Source: "{#MyAppSubDir}\lib\x86_64-win64-Release\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Check: Is64BitInstallMode
Source: "{#MyAppSubDir}\lib\i386-win32-Release\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion; Check: not Is64BitInstallMode
Source: "{#MyAppSubDir}\Languages\*.po"; DestDir: "{app}\Languages"; Flags: ignoreversion
Source: "{#MyAppSubDir}\Images\Profile.png"; DestDir: "{app}\Images"; Flags: ignoreversion

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, "&", "&&")}}"; Flags: nowait postinstall skipifsilent

