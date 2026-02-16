#define KiwiMSLogFile "{localappdata}\KiwiMS\kiwims_setup.log"

[Setup]
AppName=KiwiMS
AppId=KiwiMS
AppVersion=0.3.1
AppPublisher=Marian Freisleben
DefaultDirName={autopf}\KiwiMS
DefaultGroupName=KiwiMS
Compression=lzma2
SolidCompression=yes
OutputDir=.
OutputBaseFilename=KiwiMS-Windows-x86_64
SetupIconFile=setup\favicon.ico
WizardImageFile=setup\kiwims_banner.bmp
WizardSmallImageFile=setup\kiwims_small.bmp
PrivilegesRequired=none
PrivilegesRequiredOverridesAllowed=commandline
WizardStyle=modern
SetupLogging=yes

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"

[Files]
Source: "setup\functions.ps1"; DestDir: "{app}";
Source: "setup\config.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\miniconda_installer.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\conda_env.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\rtools_setup.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\install_renv.R"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\renv_install.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\setup_renv.R"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\renv_setup.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\quarto_install.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\check_deps.ps1"; Flags: dontcopy
Source: "KiwiMS_App\KiwiMS.exe"; DestDir: "{app}";
Source: "KiwiMS_App\update.exe"; DestDir: "{app}";
Source: "KiwiMS_App\app.R"; DestDir: "{app}";
Source: "KiwiMS_App\config.yml"; DestDir: "{app}";
Source: "KiwiMS_App\renv.lock"; DestDir: "{app}";
Source: "KiwiMS_App\renv\activate.R"; DestDir: "{app}\renv";
Source: "KiwiMS_App\rhino.yml"; DestDir: "{app}";
Source: "KiwiMS_App\app\*"; DestDir: "{app}\app"; Flags: recursesubdirs createallsubdirs;
Source: "KiwiMS_App\dev\*"; DestDir: "{app}\dev"; Flags: recursesubdirs createallsubdirs;
Source: "KiwiMS_App\resources\*"; DestDir: "{app}\resources"; Flags: recursesubdirs createallsubdirs;
Source: "setup\favicon.ico"; DestDir: "{app}"; Flags: ignoreversion

[CustomMessages]
; Installation steps
StatusMsg_Configuring=Configuring setup...
StatusMsg_InstallMiniforge=Installing Miniforge...
StatusMsg_SetupCondaEnv=Setting up Conda Environment...
StatusMsg_SetupRtools=Setting up rtools45...
StatusMsg_InstallRenv=Installing renv (1/2)...
StatusMsg_RestoreRenv=Restoring R packages (2/2)...
StatusMsg_InstallQuarto=Installing Quarto...

de.StatusMsg_Configuring=Setup wird konfiguriert...
de.StatusMsg_InstallMiniforge=Miniforgewird installiert...
de.StatusMsg_SetupCondaEnv=Conda Umgebung wird eingerichtet...
de.StatusMsg_SetupRtools=Installiere rtools45...
de.StatusMsg_InstallRenv=renv wird installiert (1/2)...
de.StatusMsg_RestoreRenv=R-Pakete werden wiederhergestellt (2/2)...
de.StatusMsg_InstallQuarto=Quarto wird installiert...

; Dialogue
Description_Launch=Launch KiwiMS
ScopeTitle=Select Installation Type
ScopeSub=Who should this application be installed for?
ScopeDesc=Choose how you want to install KiwiMS.
ScopeAllUsers=System-wide for all users (requires admin)
ScopeCurrUser=Current user only

de.Description_Launch=KiwiMS starten
de.ScopeTitle=Installationstyp auswählen
de.ScopeSub=Für wen soll diese Anwendung installiert werden?
de.ScopeDesc=Wählen Sie aus, wie Sie KiwiMS installieren möchten.
de.ScopeAllUsers=Systemweit für alle Benutzer (erfordert Admin)
de.ScopeCurrUser=Nur für den aktuellen Benutzer

; --- Dependency Page ---
DepTitle=Third-Party Dependencies
de.DepTitle=Drittanbieter-Abhängigkeiten
DepSub=Install additional tools required for KiwiMS.
de.DepSub=Zusätzliche Tools für KiwiMS installieren.
DepDesc=The following necessary components will also be installed. Please note that these are third-party tools with their own license agreements.
de.DepDesc=Folgende notwendige Komponenten werden zusätzlich installiert. Bitte beachten Sie, dass dies Drittanbieter-Tools mit eigenen Lizenzvereinbarungen sind.

DepMiniforge=Install Miniforge3 (Python environment manager)
de.DepMiniforge=Miniforge3 installieren (Erforderlich für Python-Umgebungen)
DepRtools=Install RTools 4.5 (Required for R package compilation)
de.DepRtools=RTools 4.5 installieren (Erforderlich zum Kompilieren von R-Paketen)
DepQuarto=Install Quarto (Required for document rendering)
de.DepQuarto=Quarto installieren (Erforderlich für HTML Report)

; --- Detected States ---
DepDetected=Already installed
de.DepDetected=Bereits installiert
DepMiniforge_Detected=Miniforge3 (Detected)
de.DepMiniforge_Detected=Miniforge3 (bereits installiert)
DepRtools_Detected=RTools 4.5 (Detected)
de.DepRtools_Detected=RTools 4.5 (bereits installiert)
DepQuarto_Detected=Quarto (Detected)
de.DepQuarto_Detected=Quarto (bereits installiert)

; --- Validation & Logic ---
DepRequiredError=The following components are required to install KiwiMS:%n%n%1%n%nPlease select them to continue or exit the installer.
de.DepRequiredError=Die folgenden Komponenten sind für die Installation von KiwiMS erforderlich:%n%n%1%n%nBitte wählen Sie sie aus, um fortzufahren oder beenden sie die Installation.

; --- Legal & Links ---
LegalNote=By checking the boxes, you acknowledge the respective licenses:
de.LegalNote=Durch das Aktivieren der obigen Kontrollkästchen akzeptieren Sie die jeweiligen Lizenzen:

LinkMiniforge=Miniforge / Conda-Forge License (BSD 3-Clause)
de.LinkMiniforge=Miniforge / Conda-Forge Lizenzbedingungen
UrlMiniforge=https://github.com/conda-forge/miniforge?tab=readme-ov-file

LinkRtools=RTools / GNU Toolchain License (GPL)
de.LinkRtools=RTools / GNU Toolchain Lizenz
UrlRtools=https://cran.r-project.org/bin/windows/Rtools/

LinkQuarto=Quarto CLI License (MIT Licence)
de.LinkQuarto=Quarto CLI Lizenz (MIT Licence)
UrlQuarto=https://quarto.org/license.html

[Run]
Filename: "{app}\KiwiMS.exe"; Description: "{cm:Description_Launch}"; Flags: postinstall skipifsilent shellexec;

[Icons]
Name: "{group}\KiwiMS"; Filename: "{app}\KiwiMS.exe"; WorkingDir: "{app}"; IconFilename: "{app}\favicon.ico"
Name: "{userdesktop}\KiwiMS"; Filename: "{app}\KiwiMS.exe"; WorkingDir: "{app}"; IconFilename: "{app}\favicon.ico"

[Code]
var
  InstallScopePage: TInputOptionWizardPage;
  DependenciesPage: TInputOptionWizardPage;
  SelectedScope: string; 
  InstallationFailed: Boolean;
  DetectedMask: Integer;

{ --- Browser Helpers --- }
procedure OpenBrowser(Url: string);
var ErrorCode: Integer;
begin
  ShellExec('open', Url, '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

procedure MiniforgeLinkClick(Sender: TObject); begin OpenBrowser(CustomMessage('UrlMiniforge')); end;
procedure RtoolsLinkClick(Sender: TObject);    begin OpenBrowser(CustomMessage('UrlRtools')); end;
procedure QuartoLinkClick(Sender: TObject);    begin OpenBrowser(CustomMessage('UrlQuarto')); end;

{ --- Detection Logic --- }
function GetDetectedBitmask(ScopeParam: string): Integer;
var
  ResultCode: Integer;
  ScriptPath: string;
  BasePath: string;
begin
  // 1. Visual Feedback: Disable buttons and set busy cursor
  WizardForm.NextButton.Enabled := False;
  WizardForm.BackButton.Enabled := False;
  WizardForm.Cursor := crHourGlass;
  
  // Force the UI to repaint so the user sees the changes immediately
  WizardForm.Refresh;

  try
    // Extract files (standard procedure)
    ExtractTemporaryFile('functions.ps1');
    ExtractTemporaryFile('check_deps.ps1');
    
    ScriptPath := ExpandConstant('{tmp}\check_deps.ps1');
    BasePath := ExpandConstant('{tmp}');
    
    // 2. Execute the check
    if Exec('powershell.exe', 
       '-ExecutionPolicy Bypass -File "' + ScriptPath + '" -basePath "' + BasePath + '" -installScope "' + ScopeParam + '"', 
       '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
      Result := ResultCode
    else
      Result := 0;

  finally
    // 3. Restore UI: Re-enable buttons and reset cursor
    WizardForm.Cursor := crDefault;
    WizardForm.NextButton.Enabled := True;
    WizardForm.BackButton.Enabled := True;
  end;
end;

{ --- UI Refresh Logic --- }
procedure RefreshDependenciesPage(Mask: Integer);
begin
  // Update Captions based on detection and scope compatibility
  if (Mask and 1) = 1 then 
    DependenciesPage.CheckListBox.ItemCaption[0] := CustomMessage('DepMiniforge_Detected') 
  else 
    DependenciesPage.CheckListBox.ItemCaption[0] := CustomMessage('DepMiniforge');

  if (Mask and 2) = 2 then 
    DependenciesPage.CheckListBox.ItemCaption[1] := CustomMessage('DepRtools_Detected') 
  else 
    DependenciesPage.CheckListBox.ItemCaption[1] := CustomMessage('DepRtools');

  if (Mask and 4) = 4 then 
    DependenciesPage.CheckListBox.ItemCaption[2] := CustomMessage('DepQuarto_Detected') 
  else 
    DependenciesPage.CheckListBox.ItemCaption[2] := CustomMessage('DepQuarto');

  // Set Values: Uncheck and Disable if detected in compatible scope
  DependenciesPage.Values[0] := not ((Mask and 1) = 1);
  DependenciesPage.CheckListBox.ItemEnabled[0] := not ((Mask and 1) = 1);
  
  DependenciesPage.Values[1] := not ((Mask and 2) = 2);
  DependenciesPage.CheckListBox.ItemEnabled[1] := not ((Mask and 2) = 2);
  
  DependenciesPage.Values[2] := not ((Mask and 4) = 4);
  DependenciesPage.CheckListBox.ItemEnabled[2] := not ((Mask and 4) = 4);
end;

{ --- Wizard Events --- }
procedure InitializeWizard;
var
  LegalLabel, LinkMF, LinkRT, LinkQ: TNewStaticText;
begin
  SelectedScope := 'currentuser';
  
  if not WizardSilent then begin
    // 1. Create Scope Page
    InstallScopePage := CreateInputOptionPage(wpWelcome, CustomMessage('ScopeTitle'), CustomMessage('ScopeSub'), CustomMessage('ScopeDesc'), True, False);
    InstallScopePage.Add(CustomMessage('ScopeAllUsers'));
    InstallScopePage.Add(CustomMessage('ScopeCurrUser'));
    InstallScopePage.Values[1] := True; // Default to Current User

    // 2. Create Dependencies Page
    DependenciesPage := CreateInputOptionPage(InstallScopePage.ID, CustomMessage('DepTitle'), CustomMessage('DepSub'), CustomMessage('DepDesc'), False, False);
    DependenciesPage.Add(''); // Placeholder 0
    DependenciesPage.Add(''); // Placeholder 1
    DependenciesPage.Add(''); // Placeholder 2

    // 3. Legal Info
    LegalLabel := TNewStaticText.Create(WizardForm);
    LegalLabel.Parent := DependenciesPage.Surface;
    LegalLabel.Top := DependenciesPage.CheckListBox.Top + DependenciesPage.CheckListBox.Height + ScaleY(8);
    LegalLabel.Caption := CustomMessage('LegalNote');
    LegalLabel.Font.Style := [fsBold];

    LinkMF := TNewStaticText.Create(WizardForm);
    LinkMF.Parent := DependenciesPage.Surface;
    LinkMF.Top := LegalLabel.Top + LegalLabel.Height + ScaleY(4);
    LinkMF.Left := ScaleX(10);
    LinkMF.Caption := '• ' + CustomMessage('LinkMiniforge');
    LinkMF.Font.Color := clBlue; LinkMF.Font.Style := [fsUnderline]; LinkMF.Cursor := crHand;
    LinkMF.OnClick := @MiniforgeLinkClick;

    LinkRT := TNewStaticText.Create(WizardForm);
    LinkRT.Parent := DependenciesPage.Surface;
    LinkRT.Top := LinkMF.Top + LinkMF.Height + ScaleY(2);
    LinkRT.Left := ScaleX(10);
    LinkRT.Caption := '• ' + CustomMessage('LinkRtools');
    LinkRT.Font.Color := clBlue; LinkRT.Font.Style := [fsUnderline]; LinkRT.Cursor := crHand;
    LinkRT.OnClick := @RtoolsLinkClick;

    LinkQ := TNewStaticText.Create(WizardForm);
    LinkQ.Parent := DependenciesPage.Surface;
    LinkQ.Top := LinkRT.Top + LinkRT.Height + ScaleY(2);
    LinkQ.Left := ScaleX(10);
    LinkQ.Caption := '• ' + CustomMessage('LinkQuarto');
    LinkQ.Font.Color := clBlue; LinkQ.Font.Style := [fsUnderline]; LinkQ.Cursor := crHand;
    LinkQ.OnClick := @QuartoLinkClick;
  end;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
var
  Missing: string;
begin
  Result := True;

  // Logic when user leaves the Scope Page
  if (not WizardSilent) and (InstallScopePage <> nil) and (CurPageID = InstallScopePage.ID) then begin
    if InstallScopePage.Values[0] then begin
      // ALL USERS selected
      SelectedScope := 'allusers';
      // Force the directory to Program Files
      WizardForm.DirEdit.Text := ExpandConstant('{pf}\KiwiMS');
    end else begin
      // CURRENT USER selected
      SelectedScope := 'currentuser';
      // Force the directory to Local AppData
      WizardForm.DirEdit.Text := ExpandConstant('{localappdata}\KiwiMS');
    end;
    
    // Perform scope-aware detection
    DetectedMask := GetDetectedBitmask(SelectedScope);
    
    // Refresh the next page UI
    RefreshDependenciesPage(DetectedMask);
  end;

  // Mandatory Validation Logic on Dependencies Page
  if (not WizardSilent) and (DependenciesPage <> nil) and (CurPageID = DependenciesPage.ID) then begin
    Missing := '';
    if DependenciesPage.CheckListBox.ItemEnabled[0] and (not DependenciesPage.Values[0]) then Missing := Missing + '• ' + CustomMessage('DepMiniforge') + #13#10;
    if DependenciesPage.CheckListBox.ItemEnabled[1] and (not DependenciesPage.Values[1]) then Missing := Missing + '• ' + CustomMessage('DepRtools') + #13#10;
    if DependenciesPage.CheckListBox.ItemEnabled[2] and (not DependenciesPage.Values[2]) then Missing := Missing + '• ' + CustomMessage('DepQuarto') + #13#10;
    
    if Missing <> '' then begin
      MsgBox(FmtMessage(CustomMessage('DepRequiredError'), [Missing]), mbCriticalError, MB_OK);
      Result := False;
    end;
  end;
end;

procedure RunStep(CaptionMsg: string; ScriptName: string; ProgressPos: Integer);
var
  ResultCode: Integer;
  PSArgs: string;
begin
  if InstallationFailed then Exit;
  WizardForm.StatusLabel.Caption := CaptionMsg;
  WizardForm.ProgressGauge.Position := ProgressPos * WizardForm.ProgressGauge.Max div 100;
  
  PSArgs := Format('-ExecutionPolicy Bypass -File "%s" -basePath "%s" -userDataPath "%s" -envName "kiwims" -logFile "%s" -installScope "%s"', [
    ExpandConstant('{app}\') + ScriptName, ExpandConstant('{app}'), ExpandConstant('{localappdata}\KiwiMS'), ExpandConstant('{#KiwiMSLogFile}'), SelectedScope
  ]);

  if Exec('powershell.exe', PSArgs, '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then begin
    if ResultCode <> 0 then InstallationFailed := True;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then begin
    RunStep(CustomMessage('StatusMsg_Configuring'), 'config.ps1', 10);
    
    if (WizardSilent) or (DependenciesPage.Values[0]) then begin
      RunStep(CustomMessage('StatusMsg_InstallMiniforge'), 'miniconda_installer.ps1', 20);
      RunStep(CustomMessage('StatusMsg_SetupCondaEnv'),    'conda_env.ps1', 40);
    end;

    if (WizardSilent) or (DependenciesPage.Values[1]) then
      RunStep(CustomMessage('StatusMsg_SetupRtools'), 'rtools_setup.ps1', 55);

    RunStep(CustomMessage('StatusMsg_InstallRenv'), 'renv_install.ps1', 60);
    RunStep(CustomMessage('StatusMsg_RestoreRenv'), 'renv_setup.ps1', 85);

    if (WizardSilent) or (DependenciesPage.Values[2]) then
      RunStep(CustomMessage('StatusMsg_InstallQuarto'), 'quarto_install.ps1', 95);

    if InstallationFailed then Abort;
  end;
end;