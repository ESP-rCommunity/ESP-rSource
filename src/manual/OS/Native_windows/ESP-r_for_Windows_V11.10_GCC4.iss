[Setup]
OutputDir=C:\Installer_for_ESP-r\Installer
SourceDir=C:\Installer_for_ESP-r\Source
OutputBaseFilename=ESP-r_setup_native_windows_gtk_txt_v11_10_dev6394
VersionInfoVersion=11.10.6394
VersionInfoCompany=ESRU and others
VersionInfoDescription=version 11.10.6394 GCC4 XML GTK text for native windows
VersionInfoCopyright=ESRU and others 2001-2010
Compression=zip
MinVersion=0,5.0.2195
AppCopyright=Copywrite ESRU and Natural Resources Canada 2001-2010
AppName=ESP-r for Windows
AppVerName=ESP-r for Windows V11.10.6394
InfoAfterFile=C:\Installer_for_ESP-r\Source\Esru\README_ESP-r_after_installer.txt
InfoBeforeFile=C:\Installer_for_ESP-r\Source\Esru\README_ESP-r_before_installer.txt
LicenseFile=C:\Installer_for_ESP-r\Source\Esru\GPL.txt
PrivilegesRequired=poweruser
ChangesEnvironment=true
DefaultDirName=C:\Esru
EnableDirDoesntExistWarning=true
DirExistsWarning=yes
AppendDefaultDirName=false
DisableStartupPrompt=false
FlatComponentsList=false
AlwaysShowDirOnReadyPage=true
ShowLanguageDialog=no
UsePreviousGroup=false
AppPublisher=Energy System Research Unit, Glasgow (and others)
AppPublisherURL=http://www.esru.strath.ac.uk
AppVersion=ESP-r Version 11.10.6394
UninstallDisplayName=ESP-r
MergeDuplicateFiles=false
DisableDirPage=true
AllowNoIcons=true
DefaultGroupName=ESP
WizardSmallImageFile=C:\Installer_for_ESP-r\Source\Esru\esplogosmall.bmp
AppID={{657A736E-6F01-474E-8BBB-3E5EE35F97E0}
UninstallDisplayIcon={app}\esplogosmall.bmp
[Files]
Source: Esru\GPL.txt; DestDir: {app}; Flags: comparetimestamp
Source: Esru\Documents\*.*; DestDir: {app}\Documents; Flags: comparetimestamp
Source: ..\Installer\EditPath.exe; DestDir: {app}\EditPath
Source: Esru\*.*; DestDir: {app}; Flags: comparetimestamp recursesubdirs ignoreversion
[Dirs]
Name: {app}\esp-r
Name: {app}\Models
Name: {app}\Documents
Name: {app}\esp-r\bin
Name: {app}\esp-r\bin_graphic
Name: {app}\esp-r\bin_text
Name: {app}\esp-r\climate
Name: {app}\esp-r\databases
Name: {app}\esp-r\electric_loads
Name: {app}\esp-r\manual
Name: {app}\esp-r\etc
Name: {app}\esp-r\lib
Name: {app}\esp-r\training
Name: {app}\esp-r\validation
Name: {app}\esp-r\xsl
Name: {app}\EditPath
Name: {app}\UKNCMbin
[Icons]
Name: {commondesktop}\ESP-r; Filename: {app}\esp-r\bin\esp-r.cmd; WorkingDir: {app}\Models; Comment: prj starup script; Flags: createonlyiffileexists; IconFilename: {app}\esplogosmall.bmp
[Run]
Filename: {app}\EditPath\EditPath.exe; Parameters: "-q -a ""C:\Esru\esp-r\bin"""
[UninstallRun]
Filename: {app}\EditPath\EditPath.exe; Parameters: "-q -r ""C:\Esru\esp-r\bin"""
[Registry]
Root: HKCR; SubKey: .cfg; ValueType: string; ValueData: ESP-r model; Flags: uninsdeletekey
Root: HKCR; SubKey: ESP-r model; ValueType: string; ValueData: ESP-r configuration file; Flags: uninsdeletekey
Root: HKCR; SubKey: ESP-r model\Shell\Open\Command; ValueType: string; ValueData: """{app}\Models\esp-r.cmd"" ""%1"""; Flags: uninsdeletevalue
Root: HKCR; Subkey: ESP-r model\DefaultIcon; ValueType: string; ValueData: {app}\esplogosmall.bmp,-1; Flags: uninsdeletevalue
