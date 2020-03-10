{===============================================================================

  Arduino based AVR High Voltage Programmer 2
  [https://github.com/dilshan/avr-hv2]

  Copyright (c) 2020 Dilshan R Jayakody [jayakody2000lk at gmail dot com]

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
===============================================================================}

unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, LazSerial, khexeditor, Inifiles, uintelhex, Math,
  LazSynaSer, fileinfo, LCLIntf;

type
  TAppState = (APIdle, APConnecting, APDisconnect, APFlashRead, APFlashWrite,
  APEPROMRead, APEPROMWrite, APVerifyFlash, APVerifyEEPROM);

  TDevCommand = (CMDNone, CMDHandshake, CMDVersion, CMDSignature, CMDGetFuse,
  CMDGetCalibrate, CMDFlashRead, CMDEnd, CMDErase, CMDFlashWrite,
  CMDFlashPageWrite, CMDEEPROMRead, CMDEEPROMWrite, CMDEEPROMPageWrite,
  CMDSetFuse);

  TMemConfig = record
    CurrentAddr: Word;
    CurrentPageAddr: Word;
    MaxAddr: DWord;
    PageLimit: Word;
    PageSize: Word;
  end;

  { TfmMain }

  TfmMain = class(TForm)
    btnConnect: TButton;
    btnEEPROMLoad: TButton;
    btnCancel: TButton;
    btnSetFuseConfig: TButton;
    btnLoadBrowse: TButton;
    btnLoadEEPROMBrowse: TButton;
    btnProfileLoad: TButton;
    btnProfileSave: TButton;
    btnLoadFile: TButton;
    btnReadEEPROM: TButton;
    btnReadFlash: TButton;
    btnSaveBrowse: TButton;
    btnSaveEEPROM: TButton;
    btnSaveEEPROMBrowse: TButton;
    btnVerifyEEPROM: TButton;
    btnVerifyFlash: TButton;
    btnWriteEEPROM: TButton;
    btnWriteFile: TButton;
    btnDisconnect: TButton;
    btnGetCalibration: TButton;
    btnGetFuseConfig: TButton;
    btnEraseChip: TButton;
    btnSignature: TButton;
    btnVersion: TButton;
    btnWriteFlash: TButton;
    chkLow: TCheckBox;
    chkHigh: TCheckBox;
    chkExt: TCheckBox;
    chkLock: TCheckBox;
    cmbChipProfile: TComboBox;
    grpEEPROM: TGroupBox;
    grpFlash: TGroupBox;
    hexE2PROM: TKHexEditor;
    hexFlash: TKHexEditor;
    lblAppName: TLabel;
    lblVersionStatic: TLabel;
    lblStaticProjectLink: TLabel;
    lblStaticDocLink: TLabel;
    lblHomepage: TLabel;
    lblDocLink: TLabel;
    lblFuse: TLabel;
    lblFuseLow: TLabel;
    lblFuseHigh: TLabel;
    lblFuseExt: TLabel;
    lblFuseLock: TLabel;
    lblFlashPageSize: TLabel;
    lblFlashPageCount: TLabel;
    lblE2PROMPageSize: TLabel;
    lblE2PROMPageCount: TLabel;
    dlgFileOpen: TOpenDialog;
    lblOpenEEPROM: TLabel;
    lblOpenFile: TLabel;
    lblSaveEEPROM: TLabel;
    lblSaveFile: TLabel;
    lblChipProfile: TLabel;
    lblVersion: TLabel;
    txtLicense: TMemo;
    pnlProjectInfo: TPanel;
    pnlVersionInfoBasic: TPanel;
    pnlE2PROM: TPanel;
    pnlFlash: TPanel;
    dlgFileSave: TSaveDialog;
    tabVersion: TTabSheet;
    txtLoadEEPROM: TEdit;
    txtLoadPath: TEdit;
    txtSaveEEPROM: TEdit;
    txtSavePath: TEdit;
    txtSignature: TEdit;
    txtChipId: TEdit;
    grpActions: TGroupBox;
    grpDevice: TGroupBox;
    lblSignature: TLabel;
    lblDevPath: TLabel;
    pgMain: TPageControl;
    pnlSideMain: TPanel;
    pnlStatus: TPanel;
    progMain: TProgressBar;
    tabLog: TTabSheet;
    tabFlash: TTabSheet;
    tabE2PROM: TTabSheet;
    tabDevConfig: TTabSheet;
    tmrTimeout: TTimer;
    tmrHandshake: TTimer;
    serMain: TLazSerial;
    txtDevPath: TEdit;
    txtLog: TMemo;
    txtFuseLow: TEdit;
    txtFuseHigh: TEdit;
    txtFuseExt: TEdit;
    txtFuseLock: TEdit;
    txtFlashPageSize: TEdit;
    txtFlashPageCount: TEdit;
    txtE2PROMPageSize: TEdit;
    txtE2PROMPageCount: TEdit;

    procedure btnCancelClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnEEPROMLoadClick(Sender: TObject);
    procedure btnEraseChipClick(Sender: TObject);
    procedure btnGetCalibrationClick(Sender: TObject);
    procedure btnGetFuseConfigClick(Sender: TObject);
    procedure btnLoadBrowseClick(Sender: TObject);
    procedure btnLoadEEPROMBrowseClick(Sender: TObject);
    procedure btnLoadFileClick(Sender: TObject);
    procedure btnProfileLoadClick(Sender: TObject);
    procedure btnProfileSaveClick(Sender: TObject);
    procedure btnReadEEPROMClick(Sender: TObject);
    procedure btnReadFlashClick(Sender: TObject);
    procedure btnSaveBrowseClick(Sender: TObject);
    procedure btnSaveEEPROMBrowseClick(Sender: TObject);
    procedure btnSaveEEPROMClick(Sender: TObject);
    procedure btnSetFuseConfigClick(Sender: TObject);
    procedure btnSignatureClick(Sender: TObject);
    procedure btnVerifyEEPROMClick(Sender: TObject);
    procedure btnVerifyFlashClick(Sender: TObject);
    procedure btnVersionClick(Sender: TObject);
    procedure btnWriteEEPROMClick(Sender: TObject);
    procedure btnWriteFileClick(Sender: TObject);
    procedure btnWriteFlashClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblHomepageClick(Sender: TObject);
    procedure serMainRxData(Sender: TObject);
    procedure tmrHandshakeTimer(Sender: TObject);
    procedure tmrTimeoutTimer(Sender: TObject);
  private
    responseStr: string;
    responseLen: byte;
    responseLenReq: byte;
    appState: TAppState;
    devCommand: TDevCommand;
    handshakeCount: Byte;
    flashMemConfig: TMemConfig;
    eepromConfig: TMemConfig;
    flashMem: TMemoryStream;
    eeprom: TMemoryStream;
    verifyMem: TMemoryStream;
    isSessionActive: boolean;
    isCancelOp: boolean;
    fuseIndex: Byte;
    fuseBitValue: array [0..4] of byte;

    function GetChipNameFromSignature(Signature: string): string;
    function BuildCommand(Cmd: Char; Len: Int8): string;
    procedure ResetCurrentCommand();
    function VerifyBuffer(refBuffer: TMemoryStream; cmpBuffer: TMemoryStream) : boolean;
    procedure AddLogLine(log: string);
    function SaveDialog() : boolean;
    function LoadDialog() : boolean;
    function SaveEEPROMDialog() : boolean;
    function LoadEEPROMDialog() : boolean;
    function ReadWord(stream: TMemoryStream) : Word;

    procedure ReadFlashAddress(addr: Word);
    procedure WriteFlashMemory(addr: byte; data: Word);
    procedure WriteFlashPage(addr: byte);
    procedure ReadEEPROMAddress(addr: Word);
    procedure WriteEEPROM(addr: Word; data: byte);
    procedure WriteEEPROMPage();
    procedure StopOperation();

    procedure OnEndOperation(IsSuccess: boolean);
    procedure OnCancelOperation();
  public
    property State: TAppState read appState write appState;
    property CurrentCommand: TDevCommand read devCommand write devCommand;

    procedure UpdateUiControlStates(updateDeviceSection: boolean; isCntEnabled: boolean);
    procedure ResetUIControls();
    procedure UpdateCancelStatus();
    procedure LoadChipProfiles();

    function GetApplicationVersion() : string;

    procedure DeviceHandshake();
    procedure GetDeviceVersion();
    procedure GetChipSignature();
    procedure GetFuseConfiguration();
    procedure GetCalibrationData();
    procedure EraseChip();
    procedure UpdateFuseByte();
    procedure MoveToNextFuseByte();

    procedure OnHandshakeCompleted(IsAvailable: Boolean);
    procedure OnVersionReceived(IsSuccess: boolean; versionInfo: string);
    procedure OnSignatureReceived(IsSuccess: boolean; chipSignature: string);
    procedure OnFuseConfigReceived(IsSuccess: boolean; fuseData: string);
    procedure OnCalibrationDataReceived(IsSuccess: boolean; calibData: string);
    procedure OnReadFlashData(IsSuccess: boolean; dataWord: string);
    procedure OnChipErase(IsSuccess: boolean);
    procedure OnFlashWriteCompleted(IsSuccess: boolean);
    procedure OnFlashPageWriteCompleted(IsSuccess: boolean);
    procedure OnReadEEPROMData(IsSuccess: boolean; dataByte: string);
    procedure OnEEPROMWriteCompleted(IsSuccess: boolean);
    procedure OnEEPROMPageWriteCompleted(IsSuccess: boolean);
    procedure OnFuseConfigurationUpdated(IsSuccess: boolean);
  end;

var
  fmMain: TfmMain;

const
  // Maximum number of handshake request attempts.
  MAX_HANDSHAKE_COUNT = 5;

  // Command IDs.
  CMD_ID_HANDSHAKE = 'H';
  CMD_ID_VERSION = 'V';
  CMD_ID_SIGNATURE = 'S';
  CMD_ID_GET_FUSE = 'A';
  CMD_ID_GET_CALIBRATE = 'L';
  CMD_ID_READ_FLASH = 'R';
  CMD_ID_END = 'E';
  CMD_ID_ERASE = 'C';
  CMD_ID_WRITE_FLASH = 'W';
  CMD_ID_WRITE_PAGE = 'P';
  CMD_ID_READ_EEPROM = 'T';
  CMD_ID_WRITE_EEPROM = 'J';
  CMD_ID_WRITE_PAGE_EEPROM = 'X';
  CMD_ID_SET_LOW_FUSE = 'B';
  CMD_ID_SET_HIGH_FUSE = 'I';
  CMD_ID_SET_EXT_FUSE = 'K';
  CMD_ID_SET_LOCK_FUSE = 'O';

  // Response string headers.
  HANDSHAKE_RESPONSE = '@' + CMD_ID_HANDSHAKE + '00';
  VERSION_RESPONSE = '@' + CMD_ID_VERSION + '0';
  SIGNATURE_RESPONSE = '@' + CMD_ID_SIGNATURE + '0';
  GET_FUSE_RESPONSE = '@' + CMD_ID_GET_FUSE + '0';
  GET_CALIBRATION_RESPONSE = '@' + CMD_ID_GET_CALIBRATE + '0';
  FLASH_READ_RESPONSE = '@' + CMD_ID_READ_FLASH + '0';
  END_OPERATION_RESPONSE = '@' + CMD_ID_END + '00';
  ERASE_RESPONSE = '@' + CMD_ID_ERASE + '0';
  FLASH_WRITE_RESPONSE = '@' + CMD_ID_WRITE_FLASH + '00';
  FLASH_PAGE_WRITE_RESPONSE = '@' + CMD_ID_WRITE_PAGE + '0';
  EEPROM_READ_RESPONSE = '@' + CMD_ID_READ_EEPROM + '0';
  EEPROM_WRITE_RESPONSE = '@' + CMD_ID_WRITE_EEPROM + '00';
  EEPROM_PAGE_WRITE_RESPONSE = '@' + CMD_ID_WRITE_PAGE_EEPROM + '0';
  SET_FUSE_LOW_RESPONSE = '@' + CMD_ID_SET_LOW_FUSE + '0';
  SET_FUSE_HIGH_RESPONSE = '@' + CMD_ID_SET_HIGH_FUSE + '0';
  SET_FUSE_EXT_RESPONSE = '@' + CMD_ID_SET_EXT_FUSE + '0';
  SET_FUSE_LOCK_RESPONSE =  '@' + CMD_ID_SET_LOCK_FUSE + '0';

resourcestring
  STR_DEV_PATH_ERROR = 'Device path is not specified';
  STR_DEV_CONNECT = 'Connecting with the AVR High Voltage Programmer...';
  STR_DEV_INIT_ERROR = 'Unable to initiate communication session with programmer';
  STR_DEV_UNSUPPORT = 'Programmer is not connected or unsupported device!';
  STR_DEV_DISCONNECT = 'Disconnected from AVR High Voltage Programmer';
  STR_DEV_DISCONNECT_FAIL = 'Unable to continue with High Voltage Programmer disconnecting';
  STR_DEV_ACK = 'Received acknowledgement from the programmer';
  STR_GET_CALIBRATE_FAIL = 'Calibration byte request is failed!';
  STR_GET_CALIBRATE = 'Calibration byte: ';
  STR_GET_FUSE_FAIL = 'Fuse configuration request is failed!';
  STR_GET_FUSE = 'Fuse and lock bytes configuration:';
  STR_FUSE_LOW = ' - Low: ';
  STR_FUSE_HIGH = ' - High: ';
  STR_FUSE_EXTENDED = ' - Extended: ';
  STR_FUSE_LOCK = ' - Lock: ';
  STR_GET_SIGNATURE_FAIL = 'Chip signature request is failed!';
  STR_GET_SIGNATURE = 'Chip signature: ';
  STR_UNKNOWN_CHIP = 'Unknown Chip';
  STR_GET_VERSION_FAIL = 'Version request is failed!';
  STR_GET_VERSION = 'Version: ';
  STR_FLASH_MEM_CONFIG_ERROR = 'Invalid flash memory configuration. Update flash memory configuration and retry';
  STR_START_FLASH_READ = 'Start reading flash memory...';
  STR_FALSH_READ_SUCCESS = 'Finished reading flash memory';
  STR_FLASH_READ_FAIL = 'Fail to read flash memory';
  STR_ERASE_CHIP = 'Start to erase flash memory of the chip...';
  STR_ERASE_SUCCESS = 'Chip erase completed';
  STR_ERASE_FAIL = 'Chip erase fail';
  STR_NO_DATA_ERROR = 'Flash memory buffer is empty. Read flash memory first!';
  STR_WRITE_FINISH = 'Flash memory content is written to ';
  STR_WRITE_FAIL = 'Unable to write flash memory content to the file!';
  STR_READ_FINISH = 'Flash memory content is loaded from ';
  STR_READ_FAIL = 'Unable to load flash memory content from the file!';
  STR_FLASH_BUFFER_EMPTY = 'Flash memory buffer is empty. Load hex file and retry';
  STR_EEPROM_BUFFER_EMPTY = 'EEPROM buffer is empty. Load hex file and retry';
  STR_FLASH_WRITE = 'Writing to flash memory...';
  STR_EEPROM_WRITE = 'Writing to EEPROM...';
  STR_LARGE_FILE = 'Content is too large for flash memory';
  STR_LARGE_EEPROM_FILE = 'Content is too large for EEPROM';
  STR_FLASH_WRITE_FAIL = 'Flash memory write fail';
  STR_EEPROM_WRITE_FAIL = 'EEPROM write fail';
  STR_FLASH_WRITE_SUCCESS = 'Flash memory is updated successfully';
  STR_EEPROM_MEM_CONFIG_ERROR = 'Invalid EEPROM configuration. Update EEPROM configuration and retry';
  STR_EEPROM_READ_FAIL = 'Fail to read EEPROM';
  STR_EEPROM_READ_SUCCESS = 'Finished reading EEPROM';
  STR_START_EEPROM_READ = 'Start reading EEPROM...';
  STR_EEPROM_FILE_READ_FAIL = 'Unable to load EEPROM content from the file!';
  STR_EEPROM_READ_FINISH = 'EEPROM content is loaded from ';
  STR_EEPROM_WRITE_SUCCESS = 'EEPROM is updated successfully';
  STR_EEPROM_FILE_WRITE_FINISH = 'EEPROM content is written to ';
  STR_EEPROM_FILE_WRITE_FAIL = 'Unable to write EEPROM content to the file!';
  STR_NO_EEPROM_DATA_ERROR = 'EEPROM buffer is empty. Read EEPROM first!';
  STR_START_FLASH_VERIFY = 'Start verfying flash memory...';
  STR_VERIFY_FAIL_INFO = ' - Verify fail at 0x%s, expected: 0x%s : returned: 0x%s';
  STR_VERIFY_START = 'Start verifying memory content...';
  STR_VERIFY_SUCCESS = 'Verification successful';
  STR_VERIFY_FAIL = 'Verification failed';
  STR_START_EEPROM_VERIFY = 'Start verfying EEPROM...';
  STR_CANCEL = 'User terminated the current action!';
  STR_SET_FUSE_LOW = 'Programming the fuse low bits...';
  STR_SET_FUSE_HIGH = 'Programming the fuse high bits...';
  STR_SET_FUSE_EXT = 'Programming the extended fuse bits...';
  STR_SET_FUSE_LOCK = 'Programming the lock fuse bits...';
  STR_FUSE_UPDATE_FINISH = 'Selected fuse bits are programmed successfully';
  STR_FUSE_UPDATE_FAIL = 'Fuse bits update fail';
  STR_FUSE_VALUE_NOT_FOUND = 'Fuse bit values are not specified';
  STR_FUSE_INVALID_VALUE = 'Specified fuse bit value(s) are invalid';

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  lblVersion.Caption := GetApplicationVersion();

  // Set global variables to known state.
  CurrentCommand := TDevCommand.CMDNone;
  State := TAppState.APIdle;
  handshakeCount := 0;
  isSessionActive := false;
  isCancelOp := false;

  flashMem := TMemoryStream.Create;
  eeprom := TMemoryStream.Create;
  verifyMem := TMemoryStream.Create;

  // Initialize UI elements.
  UpdateUiControlStates(true, false);
  btnCancel.Enabled := false;
  LoadChipProfiles();
  ResetUIControls();
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(flashMem);
  FreeAndNil(eeprom);
  FreeAndNil(verifyMem);
end;

procedure TfmMain.lblHomepageClick(Sender: TObject);
begin
  OpenURL(TLabel(Sender).Caption);
end;

function TfmMain.GetApplicationVersion() : string;
var
  versionStr : string;
  versionInfo : TVersionInfo;
begin
  try
    versionInfo := TVersionInfo.Create;
    versionInfo.Load(HINSTANCE);
    versionStr := IntToStr(versionInfo.FixedInfo.FileVersion[0]);
    versionStr := versionStr + '.' + IntToStr(versionInfo.FixedInfo.FileVersion[1]);
    versionStr := versionStr + '.' + IntToStr(versionInfo.FixedInfo.FileVersion[2]);
    versionStr := versionStr + '.' + IntToStr(versionInfo.FixedInfo.FileVersion[3]);
    FreeAndNil(versionInfo);
  finally
    result := versionStr;
  end;
end;

procedure TfmMain.AddLogLine(log: string);
begin
  txtLog.Lines.Add(log);
  // Move currsor to end of the log.
  txtLog.CaretPos.SetLocation(0, txtLog.Lines.Count - 1);
end;

procedure TfmMain.ResetCurrentCommand();
begin
  CurrentCommand := TDevCommand.CMDNone;
  tmrTimeout.Enabled := false;
  tmrTimeout.Interval := 1000;
end;

procedure TfmMain.ResetUIControls();
begin
  pgMain.ActivePageIndex := 0;

  // Set  fuse settings text fields with default values.
  txtFuseLock.Text := 'FF';
  txtFuseExt.Text := 'FF';
  txtFuseHigh.Text := 'FF';
  txtFuseLow.Text := 'FF';

  // Empty signature related text fields.
  txtChipId.Text := '';
  txtSignature.Text := '';

  // Setup memory related text fields.
  txtFlashPageCount.Text := '0';
  txtFlashPageSize.Text := '0';
  txtE2PROMPageCount.Text := '0';
  txtE2PROMPageSize.Text := '0';

  // Setup file related text fields.
  txtSavePath.Text := '';
  txtLoadPath.Text := '';
  txtSaveEEPROM.Text := '';
  txtLoadEEPROM.Text := '';

  // Reset profile configurations.
  cmbChipProfile.ItemIndex := cmbChipProfile.Items.IndexOf('');
end;

procedure TfmMain.tmrHandshakeTimer(Sender: TObject);
var
  tempData: string;
  dataPos: byte;
begin
  // Issue handshake command to the HV programmer.
  tmrHandshake.Enabled := false;

  // Clear receiving buffer of the HV programmer.
  dataPos := 0;
  while(dataPos < 64) do
  begin
    tempData := tempData + '~';
    inc(dataPos);
  end;

  serMain.WriteData(tempData);
  DeviceHandshake();
end;

procedure TfmMain.serMainRxData(Sender: TObject);
begin
  responseStr := responseStr + serMain.ReadData;

  case CurrentCommand of
    CMDHandshake:
      begin
        // Process handshake related serial data streams.
        if Length(Trim(responseStr)) = Length(HANDSHAKE_RESPONSE) then
        begin
          ResetCurrentCommand();
          OnHandshakeCompleted(responseStr = HANDSHAKE_RESPONSE);
        end;
      end;
    CMDVersion:
      begin
        // Process version related serial data streams.
        if Length(Trim(responseStr)) >= (Length(VERSION_RESPONSE) + 1) then
        begin
          responseLen := StrToIntDef(responseStr[4], 0);
          // Verify whole version info packet is received from the device.
          if Length(Trim(responseStr)) = (Length(VERSION_RESPONSE) + 1 + responseLen) then
          begin
            ResetCurrentCommand();
            OnVersionReceived(true, Copy(responseStr, 5, responseLen));
          end;
        end;
      end;
    CMDSignature:
      begin
        // Process chip signature related serial data streams.
        if Length(Trim(responseStr)) >= (Length(SIGNATURE_RESPONSE) + 1) then
        begin
          responseLen := StrToIntDef(responseStr[4], 0);
          // Verify whole version info packet is received from the device.
          if Length(Trim(responseStr)) = (Length(SIGNATURE_RESPONSE) + 1 + responseLen) then
          begin
            ResetCurrentCommand();
            OnSignatureReceived(true, Copy(responseStr, 5, responseLen));
          end;
        end;
      end;
    CMDGetFuse:
      begin
        // Process fuse configuration related serial data streams.
        if Length(Trim(responseStr)) >= (Length(GET_FUSE_RESPONSE) + 1) then
        begin
          responseLen := StrToIntDef(responseStr[4], 0);
          // Verify whole fuse configuration packet is received from the device.
          if Length(Trim(responseStr)) = (Length(GET_FUSE_RESPONSE) + 1 + responseLen) then
          begin
            ResetCurrentCommand();
            OnFuseConfigReceived(true, Copy(responseStr, 5, responseLen));
          end;
        end;
      end;
    CMDGetCalibrate:
      begin
        // Process calibration byte related serial data streams.
        if Length(Trim(responseStr)) >= (Length(GET_CALIBRATION_RESPONSE) + 1) then
        begin
          responseLen := StrToIntDef(responseStr[4], 0);
          // Verify calibration byte is received from the device.
          if Length(Trim(responseStr)) = (Length(GET_CALIBRATION_RESPONSE) + 1 + responseLen) then
          begin
            ResetCurrentCommand();
            OnCalibrationDataReceived(true, Copy(responseStr, 5, responseLen));
          end;
        end;
      end;
    CMDFlashRead:
      begin
        // Process flash memory read related serial data stream.
        if Length(Trim(responseStr)) >= (Length(FLASH_READ_RESPONSE) + 1) then
        begin
          responseLen := StrToIntDef(responseStr[4], 0);
          if (responseLen = 0) then
          begin
            // Read operation is fail.
            ResetCurrentCommand();
            OnReadFlashData(false, '');
          end
          else
          begin
            // Data is available for current read operation.
            if Length(Trim(responseStr)) = (Length(FLASH_READ_RESPONSE) + 1 + responseLen) then
            begin
              ResetCurrentCommand();
              OnReadFlashData(true, Copy(responseStr, 5, responseLen));
            end;
          end;
        end;
      end;
    CMDEEPROMRead:
      begin
        // Process EEPROM read related serial data streams.
        if Length(Trim(responseStr)) >= (Length(EEPROM_READ_RESPONSE) + 1) then
        begin
          responseLen := StrToIntDef(responseStr[4], 0);
          if (responseLen = 0) then
          begin
            // Read operation is fail.
            ResetCurrentCommand();
            OnReadEEPROMData(false, '');
          end
          else
          begin
            // Data is available for current read operation.
            if Length(Trim(responseStr)) = (Length(EEPROM_READ_RESPONSE) + 1 + responseLen) then
            begin
              ResetCurrentCommand();
              OnReadEEPROMData(true, Copy(responseStr, 5, responseLen));
            end;
          end;
        end;
      end;
    CMDEnd:
      begin
        // Process end of operation related serial data stream.
        if Length(Trim(responseStr)) = Length(END_OPERATION_RESPONSE) then
        begin
          ResetCurrentCommand();
          OnEndOperation(true);
        end;
      end;
    CMDErase:
      begin
        // Process chip erase related serial data streams.
        if Length(Trim(responseStr)) >= (Length(ERASE_RESPONSE) + 1) then
        begin
          responseLen := StrToIntDef(responseStr[4], 0);
          // Verify erase status byte is received from the programmer.
          if Length(Trim(responseStr)) = (Length(ERASE_RESPONSE) + 1 + responseLen) then
          begin
            ResetCurrentCommand();
            OnChipErase((responseStr[5] = '0'));
          end;
        end;
      end;
    CMDFlashWrite:
      begin
        // Process flash write related serial data streams.
        if Length(Trim(responseStr)) = Length(FLASH_WRITE_RESPONSE) then
        begin
          ResetCurrentCommand();
          OnFlashWriteCompleted(true);
        end;
      end;
    CMDFlashPageWrite:
      begin
        // Process flash page write related serial data streams.
        if Length(Trim(responseStr)) >= (Length(FLASH_PAGE_WRITE_RESPONSE) + 1) then
        begin
          responseLen := StrToIntDef(responseStr[4], 0);
          // Verify flash page write status byte is received from the programmer.
          if Length(Trim(responseStr)) = (Length(FLASH_PAGE_WRITE_RESPONSE) + 1 + responseLen) then
          begin
            ResetCurrentCommand();
            OnFlashPageWriteCompleted((responseStr[5] = '0'));
          end;
        end;
      end;
    CMDEEPROMWrite:
      begin
        // Process EEPROM write related serial data streams.
        if Length(Trim(responseStr)) = Length(EEPROM_WRITE_RESPONSE) then
        begin
          ResetCurrentCommand();
          OnEEPROMWriteCompleted(true);
        end;
      end;
    CMDEEPROMPageWrite:
      begin
        // Process EEPROM page write related serial data streams.
        if Length(Trim(responseStr)) >= (Length(EEPROM_PAGE_WRITE_RESPONSE) + 1) then
        begin
          responseLen := StrToIntDef(responseStr[4], 0);
          // Verify EEPROM page write status byte is received from the programmer.
          if Length(Trim(responseStr)) = (Length(EEPROM_PAGE_WRITE_RESPONSE) + 1 + responseLen) then
          begin
            ResetCurrentCommand();
            OnEEPROMPageWriteCompleted((responseStr[5] = '0'));
          end;
        end;
      end;
    CMDSetFuse:
      begin
        // Process fuse configuration related serial data streams.
        if Length(Trim(responseStr)) >= (responseLenReq + 1) then
        begin
          responseLen := StrToIntDef(responseStr[4], 0);
          // Verify update status byte is received from the programmer.
          if Length(Trim(responseStr)) = (responseLenReq + 1 + responseLen) then
          begin
            ResetCurrentCommand();
            OnFuseConfigurationUpdated((responseStr[5] = '0'));
          end;
        end;
      end;
  end;
end;

procedure TfmMain.tmrTimeoutTimer(Sender: TObject);
var
  tmpState: TDevCommand;
begin
  // Reset current state to idle.
  tmpState := CurrentCommand;
  ResetCurrentCommand();

  case tmpState of
    CMDHandshake:
      begin
        // Process handshake related timeouts.
        OnHandshakeCompleted(false);
      end;
    CMDVersion:
      begin
        // Process version related timeouts.
        OnVersionReceived(false, '');
      end;
    CMDSignature:
      begin
        // Process signature related timeouts.
        OnSignatureReceived(false, '');
      end;
    CMDGetFuse:
      begin
        // Process get fuse configuration related timeouts.
        OnFuseConfigReceived(false, '');
      end;
    CMDGetCalibrate:
      begin
        // Process calibration byte related timeouts.
        OnCalibrationDataReceived(false, '');
      end;
    CMDFlashRead:
      begin
        // Process flash read timeouts.
        OnReadFlashData(false, '');
      end;
    CMDEEPROMRead:
      begin
        // Process EEPROM read timeouts.
        OnReadEEPROMData(false, '');
      end;
    CMDEnd:
      begin
        // Process end operation timeouts.
        OnEndOperation(false);
      end;
    CMDErase:
      begin
        // Process chip erase timeouts.
        OnChipErase(false);
      end;
    CMDFlashWrite:
      begin
        // Process flash write timeouts.
        OnFlashWriteCompleted(false);
      end;
    CMDFlashPageWrite:
      begin
        // Process flash page write timeouts.
        OnFlashPageWriteCompleted(false);
      end;
    CMDEEPROMWrite:
      begin
        // Process EEPROM write timeouts.
        OnEEPROMWriteCompleted(false);
      end;
    CMDEEPROMPageWrite:
      begin
        // Process EEPROM page write timeouts.
        OnEEPROMPageWriteCompleted(false);
      end;
    CMDSetFuse:
      begin
        // Process fuse programming timeouts.
        OnFuseConfigurationUpdated(false);
      end;
  end;
end;

procedure TfmMain.btnConnectClick(Sender: TObject);
begin
  if (Trim(txtDevPath.Text) = '') then
  begin
    MessageDlg(Application.Title, STR_DEV_PATH_ERROR, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
    txtDevPath.SetFocus;
  end;

  try
    // Reset states.
    ResetUIControls();
    handshakeCount := 0;
    State := TAppState.APConnecting;
    txtLog.Lines.Clear;
    AddLogLine(STR_DEV_CONNECT);

    // Configure serial interface.
    serMain.Device := Trim(txtDevPath.Text);
    serMain.Active := true;
    serMain.SynSer.Purge;

    // Activate handshake timer.
    tmrHandshake.Enabled := true;
    progMain.Position := 0;
    progMain.Style := TProgressBarStyle.pbstMarquee;

    isSessionActive := serMain.Active;
  except on E : Exception do
    begin
      // Handle serial communication related exceptions.
      State := TAppState.APIdle;
      AddLogLine(STR_DEV_INIT_ERROR);
      AddLogLine(E.Message);

      progMain.Style := TProgressBarStyle.pbstNormal;
      isSessionActive := false;
    end;
  end;
end;

procedure TfmMain.btnDisconnectClick(Sender: TObject);
begin
  // Reset all pending commands.
  ResetCurrentCommand();

  // Release flash memory buffer.
  flashMem.Clear;

  try
    // Disconnection serial session.
    serMain.Active := false;
    serMain.SynSer.Purge;

    // Update UI and switch to initial state.
    UpdateUiControlStates(true, false);
    AddLogLine(STR_DEV_DISCONNECT);

    State := TAppState.APDisconnect;
    isSessionActive := false;
    isCancelOp := false;
  except on E : Exception do
    begin
      // Handle serial communication related exceptions.
      State := TAppState.APIdle;
      AddLogLine(STR_DEV_DISCONNECT_FAIL);
      AddLogLine(E.Message);
    end;
  end;
end;

procedure TfmMain.btnEEPROMLoadClick(Sender: TObject);
var
  hexFile: TIntelHex;
  binBuffer: TByteArray;
  bufferPos: integer;
begin
  if Trim(txtLoadEEPROM.Text) = '' then
  begin
    if not LoadEEPROMDialog() then
    begin
      txtLoadEEPROM.SetFocus;
      exit;
    end;
  end;

  // Load specified hex file into EEPROM buffer.
  try
    hexFile := TIntelHex.Create;
    eeprom.Clear;
    hexE2PROM.Clear;

    binBuffer := hexFile.LoadIntelHexFile(Trim(txtLoadEEPROM.Text));

    if Length(binBuffer) > 0 then
    begin
      // Load binary content into EEPROM buffer and view.
      for bufferPos := 0 to Length(binBuffer) - 1 do
      begin
        eeprom.WriteByte(binBuffer[bufferPos]);
      end;

      eeprom.Position := 0;
      hexE2PROM.LoadFromStream(eeprom);
      AddLogLine(STR_EEPROM_READ_FINISH + ExtractFileName(txtLoadEEPROM.Text));
    end
    else
    begin
      // Binary buffer is empty, may be specified file is invalid?
      AddLogLine(STR_EEPROM_FILE_READ_FAIL);
    end;

  finally
    // Release all allocated resources.
    FreeAndNil(hexFile);
    SetLength(binBuffer, 0);
  end;
end;

procedure TfmMain.btnEraseChipClick(Sender: TObject);
begin
  isCancelOp := false;
  AddLogLine(STR_ERASE_CHIP);
  EraseChip();
end;

procedure TfmMain.btnGetCalibrationClick(Sender: TObject);
begin
  isCancelOp := false;
  GetCalibrationData();
end;

procedure TfmMain.btnGetFuseConfigClick(Sender: TObject);
begin
  isCancelOp := false;
  GetFuseConfiguration();
end;

procedure TfmMain.btnLoadBrowseClick(Sender: TObject);
begin
  LoadDialog();
end;

procedure TfmMain.btnLoadEEPROMBrowseClick(Sender: TObject);
begin
  LoadEEPROMDialog();
end;

procedure TfmMain.btnLoadFileClick(Sender: TObject);
var
  hexFile: TIntelHex;
  binBuffer: TByteArray;
  bufferPos: integer;
begin
  if Trim(txtLoadPath.Text) = '' then
  begin
    if not LoadDialog() then
    begin
      txtLoadPath.SetFocus;
      exit;
    end;
  end;

  // Load specified hex file into flash memory buffer.
  try
    hexFile := TIntelHex.Create;
    flashMem.Clear;
    hexFlash.Clear;

    binBuffer := hexFile.LoadIntelHexFile(Trim(txtLoadPath.Text));

    if Length(binBuffer) > 0 then
    begin
      // Load binary content into flash memory buffer and view.
      for bufferPos := 0 to Length(binBuffer) - 1 do
      begin
        flashMem.WriteByte(binBuffer[bufferPos]);
      end;

      // Format buffer with 16bit content.
      if((Length(binBuffer) mod 2) = 1) then
      begin
        flashMem.WriteByte(0);
      end;

      flashMem.Position := 0;
      hexFlash.LoadFromStream(flashMem);
      AddLogLine(STR_READ_FINISH + ExtractFileName(txtLoadPath.Text));
    end
    else
    begin
      // Binary buffer is empty, may be specified file is invalid?
      AddLogLine(STR_READ_FAIL);
    end;

  finally
    // Release all allocated resources.
    FreeAndNil(hexFile);
    SetLength(binBuffer, 0);
  end;

end;

procedure TfmMain.btnReadEEPROMClick(Sender: TObject);
begin
  isCancelOp := false;

  eepromConfig.PageSize := StrToIntDef(Trim(txtE2PROMPageSize.Text), 0);
  eepromConfig.PageLimit := StrToIntDef(Trim(txtE2PROMPageCount.Text), 0);
  eepromConfig.MaxAddr := eepromConfig.PageSize * eepromConfig.PageLimit;

  if (eepromConfig.MaxAddr = 0) then
  begin
    // Specified page size or E2PROM memory sizes are invalid.
    pgMain.ActivePageIndex := 3;
    MessageDlg(Application.Title, STR_EEPROM_MEM_CONFIG_ERROR, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
  end
  else
  begin
    // Setup memory buffer to record captured bytes.
    eepromConfig.CurrentAddr := 0;
    eeprom.Clear;
    hexE2PROM.Clear;

    // Initialize UI controls which related with EEPROM reading.
    progMain.Max := eepromConfig.MaxAddr;
    progMain.Position := 0;
    progMain.Style := TProgressBarStyle.pbstNormal;
    AddLogLine(STR_START_EEPROM_READ);

    // Start EEPROM reading operation.
    State := TAppState.APEPROMRead;
    UpdateCancelStatus();
    ReadEEPROMAddress(eepromConfig.CurrentAddr);
  end;
end;

procedure TfmMain.btnReadFlashClick(Sender: TObject);
begin
  isCancelOp := false;

  flashMemConfig.PageSize := StrToIntDef(Trim(txtFlashPageSize.Text), 0);
  flashMemConfig.PageLimit := StrToIntDef(Trim(txtFlashPageCount.Text), 0);
  flashMemConfig.MaxAddr := flashMemConfig.PageSize * flashMemConfig.PageLimit;

  if (flashMemConfig.MaxAddr = 0) then
  begin
    // Specified page size or flash memory sizes are invalid.
    pgMain.ActivePageIndex := 3;
    MessageDlg(Application.Title, STR_FLASH_MEM_CONFIG_ERROR, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
  end
  else
  begin
    // Setup memory buffer to record captured bytes.
    flashMemConfig.CurrentAddr := 0;
    flashMem.Clear;
    hexFlash.Clear;

    // Initialize UI controls which related with flash memory reading.
    progMain.Max := flashMemConfig.MaxAddr;
    progMain.Position := 0;
    progMain.Style := TProgressBarStyle.pbstNormal;
    AddLogLine(STR_START_FLASH_READ);

    // Start flash memory reading operation.
    State := TAppState.APFlashRead;
    UpdateCancelStatus();
    ReadFlashAddress(flashMemConfig.CurrentAddr);
  end;
end;

function TfmMain.SaveDialog() : boolean;
begin
  dlgFileSave.FileName := Trim(txtSavePath.Text);

  if dlgFileSave.Execute then
  begin
    txtSavePath.Text := dlgFileSave.FileName;
  end;

  result := (Trim(txtSavePath.Text) <> '');
end;

function TfmMain.LoadDialog() : boolean;
begin
  dlgFileOpen.FileName := Trim(txtLoadPath.Text);

  if dlgFileOpen.Execute then
  begin
    txtLoadPath.Text := dlgFileOpen.FileName;
  end;

  result := (Trim(txtLoadPath.Text) <> '');
end;

procedure TfmMain.btnSaveBrowseClick(Sender: TObject);
begin
  SaveDialog();
end;

procedure TfmMain.btnSaveEEPROMBrowseClick(Sender: TObject);
begin
  SaveEEPROMDialog();
end;

procedure TfmMain.btnSaveEEPROMClick(Sender: TObject);
var
  hexFile: TIntelHex;
  dataBuffer: TByteArray;
  tempPos: integer;
begin
  if eeprom.Size > 0 then
  begin
    // Memory buffer is not empty.
    if Trim(txtSaveEEPROM.Text) = '' then
    begin
      if not SaveEEPROMDialog() then
      begin
        // Save file name is not specified by the user.
        pgMain.ActivePageIndex := 0;
        txtSaveEEPROM.SetFocus;
        exit;
      end;
    end;

    // Start to write hex file based on memory buffer content.
    try
      hexFile := TIntelHex.Create;
      SetLength(dataBuffer, eeprom.Size);
      eeprom.Position := 0;

      for tempPos := 0 to eeprom.Size - 1 do
      begin
        dataBuffer[tempPos] := eeprom.ReadByte;
      end;

      if hexFile.SaveIntelHexFile(Trim(txtSaveEEPROM.Text), dataBuffer) then
      begin
        // Write operation is successful.
        AddLogLine(STR_EEPROM_FILE_WRITE_FINISH + ExtractFileName(txtSaveEEPROM.Text));
      end
      else
      begin
        // Write operation is fail.
        AddLogLine(STR_EEPROM_FILE_WRITE_FAIL);
      end;
    finally
      // Release all allocated resources.
      eeprom.Position := 0;
      SetLength(dataBuffer, 0);
      FreeAndNil(hexFile);
    end;

  end
  else
  begin
    // Memory buffer is empty and stop file write operation.
    AddLogLine(STR_NO_EEPROM_DATA_ERROR);
  end;
end;

procedure TfmMain.btnSetFuseConfigClick(Sender: TObject);

  function ValidateFuseBitValue(txtField: TEdit) : Boolean;
  var
    tempConvVal : Integer;
  begin
    result := true;

    if(Trim(txtField.Text) = '') then
    begin
      // Fuse bit values are not specified.
      MessageDlg(Application.Title, STR_FUSE_VALUE_NOT_FOUND, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
      pgMain.ActivePageIndex := 3;
      txtField.SelectAll;
      txtField.Focused;
      result := false;
    end;

    tempConvVal := StrToIntDef('$' + Trim(txtField.Text), 256);
    if((tempConvVal < 0) or (tempConvVal > 255)) then
    begin
      // Fuse byte value is overflow!
      MessageDlg(Application.Title, STR_FUSE_INVALID_VALUE, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
      pgMain.ActivePageIndex := 3;
      txtField.Focused;
      result := false;
    end;
  end;

begin
  isCancelOp := false;
  fuseIndex := 0;

  // Atleast one fuse setting must selected by the user to continue.
  if((chkLow.Checked) or (chkLock.Checked) or (chkHigh.Checked) or (chkExt.Checked)) then
  begin

    // Validate and convert fuse low bit values.
    if(chkLow.Checked) then
    begin
      if(ValidateFuseBitValue(txtFuseLow)) then
      begin
        fuseBitValue[0] := StrToInt('$' + Trim(txtFuseLow.Text));
      end
      else
      begin
        // Fuse low bit values are invalid or not specified!
        exit;
      end;
    end;

    // Validate and convert fuse high bit values.
    if(chkHigh.Checked) then
    begin
      if(ValidateFuseBitValue(txtFuseHigh)) then
      begin
        fuseBitValue[1] := StrToInt('$' + Trim(txtFuseHigh.Text));
      end
      else
      begin
        // Fuse high bit values are invalid or not specified!
        exit;
      end;
    end;

    // Validate and convert extended fuse bit values.
    if(chkExt.Checked) then
    begin
      if(ValidateFuseBitValue(txtFuseExt)) then
      begin
        fuseBitValue[2] := StrToInt('$' + Trim(txtFuseExt.Text));
      end
      else
      begin
        // Extended fuse values are invalid or not specified!
        exit;
      end;
    end;

    // Validate and convert lock fuse bit values.
    if(chkLock.Checked) then
    begin
      if(ValidateFuseBitValue(txtFuseLock)) then
      begin
        fuseBitValue[3] := StrToInt('$' + Trim(txtFuseLock.Text));
      end
      else
      begin
        // Lock fuse values are invalid or not specified!
        exit;
      end;
    end;

    UpdateFuseByte();
  end;
end;

procedure TfmMain.btnSignatureClick(Sender: TObject);
begin
  isCancelOp := false;
  GetChipSignature();
end;

procedure TfmMain.btnVerifyEEPROMClick(Sender: TObject);
var
  hexFile: TIntelHex;
  binBuffer: TByteArray;
  bufferPos: integer;
begin
  isCancelOp := false;

  eepromConfig.PageSize := StrToIntDef(Trim(txtE2PROMPageSize.Text), 0);
  eepromConfig.PageLimit := StrToIntDef(Trim(txtE2PROMPageCount.Text), 0);
  eepromConfig.MaxAddr := eepromConfig.PageSize * eepromConfig.PageLimit;

  if Trim(txtLoadEEPROM.Text) = '' then
  begin
    if not LoadEEPROMDialog() then
    begin
      txtLoadEEPROM.SetFocus;
      exit;
    end;
  end;

  try
    // Load specified hex file into memory verify buffer.
    hexFile := TIntelHex.Create;
    verifyMem.Clear;

    binBuffer := hexFile.LoadIntelHexFile(Trim(txtLoadEEPROM.Text));

    if Length(binBuffer) > 0 then
    begin
      // Load binary content into verify buffer.
      for bufferPos := 0 to Length(binBuffer) - 1 do
      begin
        verifyMem.WriteByte(binBuffer[bufferPos]);
      end;

      verifyMem.Position := 0;

      // Setting up EEPROM memory reading from target device.
      if (eepromConfig.MaxAddr = 0) then
      begin
        // Specified page size or E2PROM memory sizes are invalid.
        pgMain.ActivePageIndex := 3;
        MessageDlg(Application.Title, STR_EEPROM_MEM_CONFIG_ERROR, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
      end
      else
      begin
        // Setup memory buffer to record captured bytes.
        eepromConfig.CurrentAddr := 0;
        eeprom.Clear;
        hexE2PROM.Clear;

        // Initialize UI controls which related with EEPROM reading.
        progMain.Max := eepromConfig.MaxAddr;
        progMain.Position := 0;
        progMain.Style := TProgressBarStyle.pbstNormal;
        AddLogLine(STR_START_EEPROM_VERIFY);

        // Start EEPROM reading operation.
        State := TAppState.APVerifyEEPROM;
        UpdateCancelStatus();
        ReadEEPROMAddress(eepromConfig.CurrentAddr);
      end;
    end
    else
    begin
      // Binary buffer is empty, may be specified file is invalid?
      AddLogLine(STR_READ_FAIL);
    end
  finally
    // Release all allocated resources.
    FreeAndNil(hexFile);
    SetLength(binBuffer, 0);
  end;
end;

procedure TfmMain.btnVerifyFlashClick(Sender: TObject);
var
  hexFile: TIntelHex;
  binBuffer: TByteArray;
  bufferPos: integer;
begin
  isCancelOp := false;

  flashMemConfig.PageSize := StrToIntDef(Trim(txtFlashPageSize.Text), 0);
  flashMemConfig.PageLimit := StrToIntDef(Trim(txtFlashPageCount.Text), 0);
  flashMemConfig.MaxAddr := flashMemConfig.PageSize * flashMemConfig.PageLimit;

  if Trim(txtLoadPath.Text) = '' then
  begin
    if not LoadDialog() then
    begin
      txtLoadPath.SetFocus;
      exit;
    end;
  end;

  try
    // Load specified hex file into memory verify buffer.
    hexFile := TIntelHex.Create;
    verifyMem.Clear;

    binBuffer := hexFile.LoadIntelHexFile(Trim(txtLoadPath.Text));

    if Length(binBuffer) > 0 then
    begin
      // Load binary content into verify buffer.
      for bufferPos := 0 to Length(binBuffer) - 1 do
      begin
        verifyMem.WriteByte(binBuffer[bufferPos]);
      end;

      // Format buffer with 16bit content.
      if((Length(binBuffer) mod 2) = 1) then
      begin
        verifyMem.WriteByte(0);
      end;

      verifyMem.Position := 0;

      // Setting up flash memory reading from target device.
      if (flashMemConfig.MaxAddr = 0) then
      begin
        // Specified page size or flash memory sizes are invalid.
        pgMain.ActivePageIndex := 3;
        MessageDlg(Application.Title, STR_FLASH_MEM_CONFIG_ERROR, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
      end
      else
      begin
        // Setup memory buffer to record captured bytes.
        flashMemConfig.CurrentAddr := 0;
        flashMem.Clear;
        hexFlash.Clear;

        // Initialize UI controls which related with flash memory reading.
        progMain.Max := flashMemConfig.MaxAddr;
        progMain.Position := 0;
        progMain.Style := TProgressBarStyle.pbstNormal;
        AddLogLine(STR_START_FLASH_VERIFY);

        // Start flash memory reading operation.
        State := TAppState.APVerifyFlash;
        UpdateCancelStatus();
        ReadFlashAddress(flashMemConfig.CurrentAddr);
      end;
    end
    else
    begin
      // Binary buffer is empty, may be specified file is invalid?
      AddLogLine(STR_READ_FAIL);
    end
  finally
    // Release all allocated resources.
    FreeAndNil(hexFile);
    SetLength(binBuffer, 0);
  end;
end;

procedure TfmMain.btnVersionClick(Sender: TObject);
begin
  isCancelOp := false;
  GetDeviceVersion();
end;

procedure TfmMain.btnWriteEEPROMClick(Sender: TObject);
begin
  isCancelOp := false;

  eepromConfig.PageSize := StrToIntDef(Trim(txtE2PROMPageSize.Text), 0);
  eepromConfig.PageLimit := StrToIntDef(Trim(txtE2PROMPageCount.Text), 0);
  eepromConfig.MaxAddr := eepromConfig.PageSize * eepromConfig.PageLimit;

  if (eepromConfig.MaxAddr = 0) then
  begin
    // Specified page size or E2PROM memory sizes are invalid.
    pgMain.ActivePageIndex := 3;
    MessageDlg(Application.Title, STR_EEPROM_MEM_CONFIG_ERROR, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
  end
  else if(eeprom.Size = 0) then
  begin
    // EEPROM buffer is empty.
    pgMain.ActivePageIndex := 2;
    MessageDlg(Application.Title, STR_EEPROM_BUFFER_EMPTY, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
  end
  else if(eeprom.Size > eepromConfig.MaxAddr) then
  begin
    // Specified data file is not fit into EEPROM.
    pgMain.ActivePageIndex := 2;
    MessageDlg(Application.Title, STR_LARGE_EEPROM_FILE, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
  end
  else
  begin
    eepromConfig.CurrentAddr := 0;
    eepromConfig.CurrentPageAddr := 0;
    eeprom.Position := 0;

    // Initialize UI controls which related with EEPROM writing.
    progMain.Max := eepromConfig.MaxAddr;
    progMain.Position := 0;
    progMain.Style := TProgressBarStyle.pbstNormal;
    AddLogLine(STR_EEPROM_WRITE);

    // Start EEPROM writing operation.
    State := TAppState.APEPROMWrite;
    UpdateCancelStatus();
    WriteEEPROM(eepromConfig.CurrentAddr, eeprom.ReadByte);
  end
end;

procedure TfmMain.btnWriteFileClick(Sender: TObject);
var
  hexFile: TIntelHex;
  dataBuffer: TByteArray;
  tempPos: integer;
begin
  if flashMem.Size > 0 then
  begin
    // Memory buffer is not empty.
    if Trim(txtSavePath.Text) = '' then
    begin
      if not SaveDialog() then
      begin
        // Save file name is not specified by the user.
        pgMain.ActivePageIndex := 0;
        txtSavePath.SetFocus;
        exit;
      end;
    end;

    // Start to write hex file based on memory buffer content.
    try
      hexFile := TIntelHex.Create;
      SetLength(dataBuffer, flashMem.Size);
      flashMem.Position := 0;

      for tempPos := 0 to flashMem.Size - 1 do
      begin
        dataBuffer[tempPos] := flashMem.ReadByte;
      end;

      if hexFile.SaveIntelHexFile(Trim(txtSavePath.Text), dataBuffer) then
      begin
        // Write operation is successful.
        AddLogLine(STR_WRITE_FINISH + ExtractFileName(txtSavePath.Text));
      end
      else
      begin
        // Write operation is fail.
        AddLogLine(STR_WRITE_FAIL);
      end;
    finally
      // Release all allocated resources.
      flashMem.Position := 0;
      SetLength(dataBuffer, 0);
      FreeAndNil(hexFile);
    end;

  end
  else
  begin
    // Memory buffer is empty and stop file write operation.
    AddLogLine(STR_NO_DATA_ERROR);
  end;
end;

function TfmMain.BuildCommand(Cmd: Char; Len: Int8): string;
begin
  result := '~' + Cmd + IntToStr(Len) + '^';
end;

procedure TfmMain.DeviceHandshake();
begin
  responseStr := '';
  responseLen := 0;
  CurrentCommand := TDevCommand.CMDHandshake;
  serMain.WriteData(BuildCommand(CMD_ID_HANDSHAKE, 0));
  tmrTimeout.Interval := 1000;
  tmrTimeout.Enabled := true;
end;

procedure TfmMain.OnHandshakeCompleted(IsAvailable: Boolean);
begin
  if(IsAvailable) then
  begin
    // Handshake request is successful.
    AddLogLine(STR_DEV_ACK);
    progMain.Style := TProgressBarStyle.pbstNormal;
    UpdateUiControlStates(true, true);
  end
  else
  begin
    // Handshake request is failed.
    if(handshakeCount < MAX_HANDSHAKE_COUNT) then
    begin
      // Try another handshake attempt.
      Inc(handshakeCount);
      tmrHandshake.Enabled := true;
    end
    else
    begin
      // All handshake attempts are failed and raise fail message.
      AddLogLine(STR_DEV_UNSUPPORT);
      progMain.Style := TProgressBarStyle.pbstNormal;
    end;
  end;
end;

procedure TfmMain.GetDeviceVersion();
begin
  responseStr := '';
  responseLen := 0;
  CurrentCommand := TDevCommand.CMDVersion;
  serMain.WriteData(BuildCommand(CMD_ID_VERSION, 0));
  tmrTimeout.Interval := 1000;
  tmrTimeout.Enabled := true;
end;

procedure TfmMain.OnVersionReceived(IsSuccess: boolean; versionInfo: string);
begin
  if(IsSuccess) then
  begin
    AddLogLine(STR_GET_VERSION + versionInfo);
  end
  else
  begin
    AddLogLine(STR_GET_VERSION_FAIL);
  end;
end;

procedure TfmMain.GetChipSignature();
begin
  responseStr := '';
  responseLen := 0;
  CurrentCommand := TDevCommand.CMDSignature;
  serMain.WriteData(BuildCommand(CMD_ID_SIGNATURE, 0));
  tmrTimeout.Interval := 2000;
  tmrTimeout.Enabled := true;
end;

function TfmMain.GetChipNameFromSignature(Signature: string): string;
var
  sigConf : TIniFile;
  chipName: string;
begin
  chipName := '';
  try
    sigConf := TIniFile.Create('avr-signatures.conf');
    chipName := sigConf.ReadString('atmega', Signature, STR_UNKNOWN_CHIP);
    FreeAndNil(sigConf);
  finally
    result := chipName;
  end;
end;

procedure TfmMain.OnSignatureReceived(IsSuccess: boolean; chipSignature: string);
var
  chipId: string;
  profileIndex: integer;
begin
  if(IsSuccess) then
  begin
    chipId := GetChipNameFromSignature(chipSignature);
    AddLogLine(STR_GET_SIGNATURE + chipSignature + ' - ' + chipId);

    // Update configuration page with received signature.
    txtSignature.Text := UpperCase(chipSignature);
    txtChipId.Text := chipId;

    // Try to load profile from received signature.
    profileIndex := cmbChipProfile.Items.IndexOf(lowercase(chipId));
    if profileIndex >= 0 then
    begin;
      cmbChipProfile.ItemIndex := profileIndex;
      btnProfileLoadClick(nil);
    end
    else
    begin
      // Chip profile is not available in configuration.
      cmbChipProfile.ItemIndex := cmbChipProfile.Items.IndexOf('');
    end;
  end
  else
  begin
    AddLogLine(STR_GET_SIGNATURE_FAIL);
  end;
end;

procedure TfmMain.GetFuseConfiguration();
begin
  responseStr := '';
  responseLen := 0;
  CurrentCommand := TDevCommand.CMDGetFuse;
  serMain.WriteData(BuildCommand(CMD_ID_GET_FUSE, 0));
  tmrTimeout.Interval := 2000;
  tmrTimeout.Enabled := true;
end;

procedure TfmMain.OnFuseConfigReceived(IsSuccess: boolean; fuseData: string);
var
  fuseVal: string;
begin
  if(IsSuccess) then
  begin
    AddLogLine(STR_GET_FUSE);

    // Extract low fuse setting.
    fuseVal := Copy(fuseData, 1, 2);
    AddLogLine(STR_FUSE_LOW + fuseVal);
    txtFuseLow.Text := UpperCase(fuseVal);

    // Extract high fuse setting.
    fuseVal := Copy(fuseData, 3, 2);
    AddLogLine(STR_FUSE_HIGH + fuseVal);
    txtFuseHigh.Text := UpperCase(fuseVal);

    // Extract extended fuse setting.
    fuseVal := Copy(fuseData, 5, 2);
    AddLogLine(STR_FUSE_EXTENDED + fuseVal);
    txtFuseExt.Text := UpperCase(fuseVal);

    // Extract lock fuse setting.
    fuseVal := Copy(fuseData, 7, 2);
    AddLogLine(STR_FUSE_LOCK + fuseVal);
    txtFuseLock.Text := UpperCase(fuseVal);
  end
  else
  begin
    AddLogLine(STR_GET_FUSE_FAIL);
  end;
end;

procedure TfmMain.GetCalibrationData();
begin
  responseStr := '';
  responseLen := 0;
  CurrentCommand := TDevCommand.CMDGetCalibrate;
  serMain.WriteData(BuildCommand(CMD_ID_GET_CALIBRATE, 0));
  tmrTimeout.Interval := 1500;
  tmrTimeout.Enabled := true;
end;

procedure TfmMain.OnCalibrationDataReceived(IsSuccess: boolean; calibData: string);
begin
  if(IsSuccess) then
  begin
    AddLogLine(STR_GET_CALIBRATE + calibData);
  end
  else
  begin
    AddLogLine(STR_GET_CALIBRATE_FAIL);
  end;
end;

procedure TfmMain.ReadEEPROMAddress(addr: Word);
begin
  if (isCancelOp) then
  begin
    // Operation cancel is requested.
    isCancelOp := false;
    OnCancelOperation();
  end
  else
  begin
    responseStr := '';
    responseLen := 0;
    CurrentCommand := TDevCommand.CMDEEPROMRead;
    serMain.WriteData(BuildCommand(CMD_ID_READ_EEPROM, 0));
    serMain.WriteData(Lowercase(IntToHex(addr, 4)));
    tmrTimeout.Interval := 1500;
    tmrTimeout.Enabled := true;
  end;
end;

procedure TfmMain.OnReadEEPROMData(IsSuccess: boolean; dataByte: string);
begin
  if(IsSuccess) and (Length(dataByte) = 2) then
  begin
    // EEPROM read operation is successful for current address.
    eeprom.WriteByte(StrToInt('$'+Copy(dataByte, 1, 2)));
    eepromConfig.CurrentAddr := eepromConfig.CurrentAddr + 1;
    progMain.Position := eepromConfig.CurrentAddr;

    if(eepromConfig.CurrentAddr >=  eepromConfig.MaxAddr) then
    begin
      // Reach end of EEPROM and stop reading operation.
      StopOperation();
      eeprom.Position := 0;
      hexE2PROM.LoadFromStream(eeprom);
      AddLogLine(STR_EEPROM_READ_SUCCESS);
    end
    else
    begin
      // Reading next memory address.
      ReadEEPROMAddress(eepromConfig.CurrentAddr);
    end;
  end
  else
  begin
    // EEPROM read operation fail. Stop reading operation.
    AddLogLine(STR_EEPROM_READ_FAIL);
    StopOperation();
  end;
end;

procedure TfmMain.ReadFlashAddress(addr: Word);
begin
  if (isCancelOp) then
  begin
    // Operation cancel is requested.
    isCancelOp := false;
    OnCancelOperation();
  end
  else
  begin
    responseStr := '';
    responseLen := 0;
    CurrentCommand := TDevCommand.CMDFlashRead;
    serMain.WriteData(BuildCommand(CMD_ID_READ_FLASH, 0));
    serMain.WriteData(Lowercase(IntToHex(addr, 4)));
    tmrTimeout.Interval := 1500;
    tmrTimeout.Enabled := true;
  end;
end;

procedure TfmMain.OnReadFlashData(IsSuccess: boolean; dataWord: string);
begin
  if(IsSuccess) and (Length(dataWord) = 4) then
  begin
    // Flash memory read operation is successful for current address.
    flashMem.WriteByte(StrToInt('$'+Copy(dataWord, 1, 2)));
    flashMem.WriteByte(StrToInt('$'+Copy(dataWord, 3, 2)));
    flashMemConfig.CurrentAddr := flashMemConfig.CurrentAddr + 1;
    progMain.Position := flashMemConfig.CurrentAddr;

    if(flashMemConfig.CurrentAddr >=  flashMemConfig.MaxAddr) then
    begin
      // Reach end of flash memory and stop reading operation.
      StopOperation();
      flashMem.Position := 0;
      hexFlash.LoadFromStream(flashMem);
      AddLogLine(STR_FALSH_READ_SUCCESS);
    end
    else
    begin
      // Reading next memory address.
      ReadFlashAddress(flashMemConfig.CurrentAddr);
    end;
  end
  else
  begin
    // Flash memory read operation fail. Stop reading operation.
    AddLogLine(STR_FLASH_READ_FAIL);
    StopOperation();
  end;
end;

procedure TfmMain.StopOperation();
begin
  responseStr := '';
  responseLen := 0;
  CurrentCommand := TDevCommand.CMDEnd;
  serMain.WriteData(BuildCommand(CMD_ID_END, 0));
  tmrTimeout.Interval := 1000;
  tmrTimeout.Enabled := true;
end;

procedure TfmMain.OnEndOperation(IsSuccess: boolean);

  procedure VerifyMemoryOp(targetBuffer: TMemoryStream);
  begin
    AddLogLine(STR_VERIFY_START);
    if VerifyBuffer(verifyMem, targetBuffer) then
    begin
      // Memory verification is successful.
      AddLogLine(STR_VERIFY_SUCCESS);
    end
    else
    begin
      // Memory verification is failed!
      AddLogLine(STR_VERIFY_FAIL);
    end;
  end;

begin
  case State of
    TAppState.APVerifyFlash:
      begin
        VerifyMemoryOp(flashMem);
      end;
    TAppState.APVerifyEEPROM:
      begin
        VerifyMemoryOp(eeprom);
      end;
  end;

  progMain.Position := 0;
  State := TAppState.APIdle;
  UpdateCancelStatus();
  isCancelOp := false;
end;

procedure TfmMain.EraseChip();
begin
  responseStr := '';
  responseLen := 0;
  CurrentCommand := TDevCommand.CMDErase;
  serMain.WriteData(BuildCommand(CMD_ID_ERASE, 0));
  tmrTimeout.Interval := 7000;
  tmrTimeout.Enabled := true;
end;

procedure TfmMain.MoveToNextFuseByte();
  begin
    inc(fuseIndex);
    UpdateFuseByte();
  end;

procedure TfmMain.UpdateFuseByte();

  procedure UpdateCurrentFuseByte(Cmd: Char; fuseVal: byte);
  begin
    responseStr := '';
    responseLen := 0;
    CurrentCommand := TDevCommand.CMDSetFuse;
    serMain.WriteData(BuildCommand(Cmd, 0));
    serMain.WriteData(Lowercase(IntToHex(fuseVal, 2)));
    tmrTimeout.Interval := 1500;
    tmrTimeout.Enabled := true;
  end;

begin
  case fuseIndex of
    0: // Update fuse low byte.
      begin
        if(chkLow.Checked) then
        begin
          AddLogLine(STR_SET_FUSE_LOW);
          UpdateCurrentFuseByte(CMD_ID_SET_LOW_FUSE, fuseBitValue[0]);
          responseLenReq := Length(SET_FUSE_LOW_RESPONSE);
        end
        else
        begin
          // Low fuse byte is not selected.
          MoveToNextFuseByte();
          exit;
        end;
      end;
    1: // Update fuse high byte.
      begin
        if(chkHigh.Checked) then
        begin
          AddLogLine(STR_SET_FUSE_HIGH);
          UpdateCurrentFuseByte(CMD_ID_SET_HIGH_FUSE, fuseBitValue[1]);
          responseLenReq := Length(SET_FUSE_HIGH_RESPONSE);
        end
        else
        begin
          // High fuse byte is not selected.
          MoveToNextFuseByte();
          exit;
        end;
      end;
    2: // Update extended fuse byte.
      begin
        if(chkExt.Checked) then
        begin
          AddLogLine(STR_SET_FUSE_EXT);
          UpdateCurrentFuseByte(CMD_ID_SET_EXT_FUSE, fuseBitValue[2]);
          responseLenReq := Length(SET_FUSE_EXT_RESPONSE);
        end
        else
        begin
          // Extended fuse byte is not selected.
          MoveToNextFuseByte();
          exit;
        end;
      end;
    3: // Update lock fuse byte.
      begin
        if(chkLock.Checked) then
        begin
          AddLogLine(STR_SET_FUSE_LOCK);
          UpdateCurrentFuseByte(CMD_ID_SET_LOCK_FUSE, fuseBitValue[3]);
          responseLenReq := Length(SET_FUSE_LOCK_RESPONSE);
        end
        else
        begin
          // Lock fuse byte is not selected.
          MoveToNextFuseByte();
          exit;
        end;
      end;
  end;

  if (fuseIndex >= 4) then
  begin
    // End of fuse update loop.
    AddLogLine(STR_FUSE_UPDATE_FINISH);
    fuseIndex := 0;
  end;
end;

procedure TfmMain.OnFuseConfigurationUpdated(IsSuccess: boolean);
begin
  if(IsSuccess) then
  begin
    // Current fuse bit update is successful.
    MoveToNextFuseByte();
  end
  else
  begin
    // Fuse bit update is failed.
    AddLogLine(STR_FUSE_UPDATE_FAIL);
  end;
end;

procedure TfmMain.OnChipErase(IsSuccess: boolean);
begin
  if(IsSuccess)then
  begin
    // Erase operation is successful.
    AddLogLine(STR_ERASE_SUCCESS);
  end
  else
  begin
    // Erase operation is failed.
    AddLogLine(STR_ERASE_FAIL);
  end;
end;

procedure TfmMain.WriteEEPROM(addr: Word; data: byte);
var
  outData : string;
begin
  if (isCancelOp) then
  begin
    // Operation cancel is requested.
    isCancelOp := false;
    OnCancelOperation();
  end
  else
  begin
    responseStr := '';
    responseLen := 0;
    CurrentCommand := TDevCommand.CMDEEPROMWrite;
    // Construct data packet with lower address byte and value word.
    outData := Lowercase(IntToHex(addr, 4));
    outData := outData + Lowercase(IntToHex(data, 2));
    serMain.WriteData(BuildCommand(CMD_ID_WRITE_EEPROM, 0));
    serMain.WriteData(outData);
    tmrTimeout.Interval := 1500;
    tmrTimeout.Enabled := true;
  end;
end;

procedure TfmMain.WriteFlashMemory(addr: byte; data: Word);
var
  outData : string;
begin
  if (isCancelOp) then
  begin
    // Operation cancel is requested.
    isCancelOp := false;
    OnCancelOperation();
  end
  else
  begin
    responseStr := '';
    responseLen := 0;
    CurrentCommand := TDevCommand.CMDFlashWrite;
    // Construct data packet with lower address byte and value word.
    outData := Lowercase(IntToHex(data, 4));
    outData := Lowercase(IntToHex(addr, 2)) + outData;
    serMain.WriteData(BuildCommand(CMD_ID_WRITE_FLASH, 0));
    serMain.WriteData(outData);
    tmrTimeout.Interval := 1500;
    tmrTimeout.Enabled := true;
  end;
end;

function TfmMain.readWord(stream: TMemoryStream) : Word;
begin
  if stream.Position = (stream.Size - 1) then
  begin
    // End of stream is already reached.
    result := 0;
  end
  else
  begin
    // Read current byte from the stream.
    result := stream.ReadByte;
  end;

  // Looking availability of next byte.
  if stream.Position <> (stream.Size - 1) then
  begin
    // Read current byte and append it to the word.
    result := result or (stream.ReadByte shl 8);
  end;
end;

procedure TfmMain.OnEEPROMWriteCompleted(IsSuccess: boolean);
begin
  if IsSuccess then
  begin
    // Advance current address and check page memory limit is reached.
    if (eepromConfig.CurrentAddr mod eepromConfig.PageSize) = (eepromConfig.PageSize - 1) then
    begin
      // Page data buffer is full. Request programmer to write the current page.
      WriteEEPROMPage();
    end
    else
    begin
      progMain.Position := eepromConfig.CurrentAddr;
      if eeprom.Position < eeprom.Size then
      begin
        // Page buffer is not filled, continue to push data into page buffer.
        Inc(eepromConfig.CurrentAddr);
        WriteEEPROM(eepromConfig.CurrentAddr, eeprom.ReadByte);
      end
      else
      begin
        // End of buffer is reached. Stop programming operation.
        WriteEEPROMPage();
      end;
    end;
  end
  else
  begin
    // EEPROM write operation is failed.
    StopOperation();
    AddLogLine(STR_EEPROM_WRITE_FAIL);
  end;
end;

procedure TfmMain.OnFlashWriteCompleted(IsSuccess: boolean);
begin
  if IsSuccess then
  begin
    // Advance current address and check page memory limit is reached.
    if (flashMemConfig.CurrentAddr mod flashMemConfig.PageSize) = (flashMemConfig.PageSize - 1) then
    begin
      // Page data buffer is full. Request programmer to write the current page.
      WriteFlashPage(Hi(flashMemConfig.CurrentAddr));
    end
    else
    begin
      progMain.Position := flashMemConfig.CurrentAddr;
      if flashMem.Position < (flashMem.Size - 1) then
      begin
        // Page buffer is not filled, continue to push data into page buffer.
        Inc(flashMemConfig.CurrentAddr);
        WriteFlashMemory(Lo(flashMemConfig.CurrentAddr), readWord(flashMem));
      end
      else
      begin
        // End of buffer is reached. Stop programming operation.
        WriteFlashPage(Hi(flashMemConfig.CurrentAddr));
      end;
    end;
  end
  else
  begin
    // Flash write operation is failed.
    StopOperation();
    AddLogLine(STR_FLASH_WRITE_FAIL);
  end;
end;

procedure TfmMain.WriteFlashPage(addr: byte);
begin
  responseStr := '';
  responseLen := 0;
  CurrentCommand := TDevCommand.CMDFlashPageWrite;
  serMain.WriteData(BuildCommand(CMD_ID_WRITE_PAGE, 0));
  serMain.WriteData(Lowercase(IntToHex(addr, 2)));
  tmrTimeout.Interval := 1500;
  tmrTimeout.Enabled := true;
end;

procedure TfmMain.WriteEEPROMPage();
begin
  responseStr := '';
  responseLen := 0;
  CurrentCommand := TDevCommand.CMDEEPROMPageWrite;
  serMain.WriteData(BuildCommand(CMD_ID_WRITE_PAGE_EEPROM, 0));
  tmrTimeout.Interval := 1500;
  tmrTimeout.Enabled := true;
end;

procedure TfmMain.OnFlashPageWriteCompleted(IsSuccess: boolean);
begin
  if IsSuccess then
  begin
    Inc(flashMemConfig.CurrentPageAddr);
    if flashMem.Position < (flashMem.Size - 1) then
    begin
      // Start writing flash from next memory address.
      Inc(flashMemConfig.CurrentAddr);
      progMain.Position := flashMemConfig.CurrentAddr;
      WriteFlashMemory(Lo(flashMemConfig.CurrentAddr), readWord(flashMem));
    end
    else
    begin
      // Finishing flash memory write operation.
      StopOperation();
      AddLogLine(STR_FLASH_WRITE_SUCCESS);
    end;
  end
  else
  begin
    // Flash page write operation is failed.
    StopOperation();
    AddLogLine(STR_FLASH_WRITE_FAIL);
  end;
end;

procedure TfmMain.OnEEPROMPageWriteCompleted(IsSuccess: boolean);
begin
  if IsSuccess then
  begin
    Inc(eepromConfig.CurrentPageAddr);
    if eeprom.Position < (eeprom.Size - 1) then
    begin
      // Start writing EEPROM from next memory address.
      Inc(eepromConfig.CurrentAddr);
      progMain.Position := eepromConfig.CurrentAddr;
      WriteEEPROM(eepromConfig.CurrentAddr, eeprom.ReadByte);
    end
    else
    begin
      // Finishing EEPROM write operation.
      StopOperation();
      AddLogLine(STR_EEPROM_WRITE_SUCCESS);
    end;
  end
  else
  begin
    // Flash page write operation is failed.
    StopOperation();
    AddLogLine(STR_EEPROM_WRITE_FAIL);
  end;
end;

procedure TfmMain.btnWriteFlashClick(Sender: TObject);
begin
  isCancelOp := false;

  flashMemConfig.PageSize := StrToIntDef(Trim(txtFlashPageSize.Text), 0);
  flashMemConfig.PageLimit := StrToIntDef(Trim(txtFlashPageCount.Text), 0);
  flashMemConfig.MaxAddr := flashMemConfig.PageSize * flashMemConfig.PageLimit;

  if (flashMemConfig.MaxAddr = 0) then
  begin
    // Specified page size or flash memory sizes are invalid.
    pgMain.ActivePageIndex := 3;
    MessageDlg(Application.Title, STR_FLASH_MEM_CONFIG_ERROR, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
  end
  else if(flashMem.Size = 0) then
  begin
    // Flash memory buffer is empty.
    pgMain.ActivePageIndex := 1;
    MessageDlg(Application.Title, STR_FLASH_BUFFER_EMPTY, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
  end
  else if(flashMem.Size > (flashMemConfig.MaxAddr * 2)) then
  begin
    // Specified data file is not fit into flash memory.
    pgMain.ActivePageIndex := 1;
    MessageDlg(Application.Title, STR_LARGE_FILE, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], '');
  end
  else
  begin
    flashMemConfig.CurrentAddr := 0;
    flashMemConfig.CurrentPageAddr := 0;
    flashMem.Position := 0;

    // Initialize UI controls which related with flash memory writing.
    progMain.Max := flashMemConfig.MaxAddr;
    progMain.Position := 0;
    progMain.Style := TProgressBarStyle.pbstNormal;
    AddLogLine(STR_FLASH_WRITE);

    // Start flash memory writing operation.
    State := TAppState.APFlashWrite;
    UpdateCancelStatus();
    WriteFlashMemory(Lo(flashMemConfig.CurrentAddr), readWord(flashMem));
  end;
end;

function TfmMain.SaveEEPROMDialog() : boolean;
begin
  dlgFileSave.FileName := Trim(txtSaveEEPROM.Text);

  if dlgFileSave.Execute then
  begin
    txtSaveEEPROM.Text := dlgFileSave.FileName;
  end;

  result := (Trim(txtSaveEEPROM.Text) <> '');
end;

function TfmMain.LoadEEPROMDialog() : boolean;
begin
  dlgFileOpen.FileName := Trim(txtLoadEEPROM.Text);

  if dlgFileOpen.Execute then
  begin
    txtLoadEEPROM.Text := dlgFileOpen.FileName;
  end;

  result := (Trim(txtLoadEEPROM.Text) <> '');
end;

function TfmMain.VerifyBuffer(refBuffer: TMemoryStream; cmpBuffer: TMemoryStream) : boolean;
var
  memPos, memLimit: integer;
  failCount, dataRef, dataCmp : byte;
begin
  refBuffer.Position := 0;
  cmpBuffer.Position := 0;
  failCount := 0;
  memLimit := Min(refBuffer.Size, cmpBuffer.Size);
  memPos := 0;

  // Start verify specified buffer with referenced memory buffer.
  while(memPos < memLimit) do
  begin
    dataRef := refBuffer.ReadByte;
    dataCmp := cmpBuffer.ReadByte;
    if (dataRef <> dataCmp) then
    begin
      // Verify fail.
      AddLogLine(Format(STR_VERIFY_FAIL_INFO, [IntToHex(memPos, 4), IntToHex(dataRef, 2), IntToHex(dataCmp, 2)]));

      Inc(failCount);
      if failCount > 9 then
      begin
        // Maximum verify fail rate if reached.
        break;
      end;
    end;
    inc(memPos);
  end;

  refBuffer.Position := 0;
  cmpBuffer.Position := 0;
  result := (failCount = 0);
end;

procedure TfmMain.UpdateCancelStatus();
var
  cancelState : boolean;
begin
  if (appState = TAppState.APConnecting) then
  begin
    // Application is establishing connection with programmer.
    cancelState := false;
  end
  else
  begin
    cancelState := (appState <> TAppState.APIdle) and isSessionActive;
  end;

  btnCancel.Enabled := cancelState;
  cancelState := not cancelState;
  UpdateUiControlStates(false, cancelState);
end;

procedure TfmMain.UpdateUiControlStates(updateDeviceSection: boolean; isCntEnabled: boolean);
begin
  // Update UI state of the side controls.

  if(updateDeviceSection) then
  begin
    btnConnect.Enabled := not isCntEnabled;
    btnDisconnect.Enabled := isCntEnabled;
  end;

  btnVersion.Enabled := isCntEnabled;
  btnSignature.Enabled := isCntEnabled;
  btnGetFuseConfig.Enabled := isCntEnabled;
  btnGetCalibration.Enabled := isCntEnabled;
  btnSetFuseConfig.Enabled := isCntEnabled;
  btnEraseChip.Enabled := isCntEnabled;
  btnReadFlash.Enabled := isCntEnabled;
  txtSavePath.Enabled := isCntEnabled;
  btnSaveBrowse.Enabled := isCntEnabled;
  lblSaveFile.Enabled := isCntEnabled;
  btnWriteFile.Enabled := isCntEnabled;
  txtLoadPath.Enabled := isCntEnabled;
  btnLoadBrowse.Enabled := isCntEnabled;
  lblOpenFile.Enabled := isCntEnabled;
  btnWriteFlash.Enabled := isCntEnabled;
  btnLoadFile.Enabled := isCntEnabled;
  btnReadEEPROM.Enabled := isCntEnabled;
  btnSaveEEPROM.Enabled := isCntEnabled;
  btnWriteEEPROM.Enabled := isCntEnabled;
  btnEEPROMLoad.Enabled := isCntEnabled;
  btnSaveEEPROMBrowse.Enabled := isCntEnabled;
  btnLoadEEPROMBrowse.Enabled := isCntEnabled;
  lblSaveEEPROM.Enabled := isCntEnabled;
  lblOpenEEPROM.Enabled := isCntEnabled;
  txtSaveEEPROM.Enabled := isCntEnabled;
  txtLoadEEPROM.Enabled := isCntEnabled;
  btnVerifyEEPROM.Enabled := isCntEnabled;
  btnVerifyFlash.Enabled := isCntEnabled;
  chkExt.Enabled := isCntEnabled;
  chkHigh.Enabled := isCntEnabled;
  chkLock.Enabled := isCntEnabled;
  chkLow.Enabled := isCntEnabled;

  // Update UI state of the configuration page.
  txtE2PROMPageSize.Enabled := isCntEnabled;
  txtE2PROMPageCount.Enabled := isCntEnabled;
  txtFlashPageSize.Enabled := isCntEnabled;
  txtFlashPageCount.Enabled := isCntEnabled;
  txtChipId.Enabled := isCntEnabled;
  txtSignature.Enabled := isCntEnabled;
  txtFuseExt.Enabled := isCntEnabled;
  txtFuseHigh.Enabled := isCntEnabled;
  txtFuseLock.Enabled := isCntEnabled;
  txtFuseLow.Enabled := isCntEnabled;
  btnProfileLoad.Enabled := isCntEnabled;
  btnProfileSave.Enabled := isCntEnabled;
  cmbChipProfile.Enabled := isCntEnabled;
end;

procedure TfmMain.btnCancelClick(Sender: TObject);
begin
  if appState <> TAppState.APIdle then
  begin
    // Notify cancel to pending operations.
    isCancelOp := true;
  end;
end;

procedure TfmMain.OnCancelOperation();
begin
  AddLogLine(STR_CANCEL);
  StopOperation();
end;

procedure TfmMain.LoadChipProfiles();
var
  profile: TIniFile;
  chipProfiles: TStringList;
begin
  cmbChipProfile.Items.Clear;
  cmbChipProfile.Items.Add('');
  // Load availabe chip profiles from configuration file.
  try
    chipProfiles := TStringList.Create;
    profile := TIniFile.Create('avr-profile.conf');
    profile.ReadSections(chipProfiles);
    // Process chip list.
    chipProfiles.Sort;
    cmbChipProfile.Items.AddStrings(chipProfiles);
    // Release allocated resources.
    FreeAndNil(profile);
    FreeAndNil(chipProfiles);
  finally
    cmbChipProfile.ItemIndex := cmbChipProfile.Items.IndexOf('');
  end;
end;

procedure TfmMain.btnProfileLoadClick(Sender: TObject);
var
  profile: TIniFile;
  selectedProfile: string;
begin
  selectedProfile := cmbChipProfile.Items.ValueFromIndex[cmbChipProfile.ItemIndex];
  if selectedProfile <> '' then
  begin
    try
      profile := TIniFile.Create('avr-profile.conf');
      txtFlashPageSize.Text := IntToStr(profile.ReadInteger(selectedProfile, 'flash-page-size', 0));
      txtFlashPageCount.Text := IntToStr(profile.ReadInteger(selectedProfile, 'flash-page-count', 0));
      txtE2PROMPageSize.Text := IntToStr(profile.ReadInteger(selectedProfile, 'eeprom-page-size', 0));
      txtE2PROMPageCount.Text := IntToStr(profile.ReadInteger(selectedProfile, 'eeprom-page-count', 0));
    finally
      FreeAndNil(profile);
    end;
  end;
end;

procedure TfmMain.btnProfileSaveClick(Sender: TObject);
var
  profile: TIniFile;
  chipProfile: string;
begin
  chipProfile := txtChipId.Text;
  // Avoid to write unidentified chips!
  if chipProfile <> STR_UNKNOWN_CHIP then
  begin
    try
      chipProfile := Lowercase(chipProfile);

      // Create or update profile entry.
      profile := TIniFile.Create('avr-profile.conf');
      profile.WriteInteger(chipProfile, 'flash-page-size', StrToIntDef(Trim(txtFlashPageSize.Text), 0));
      profile.WriteInteger(chipProfile, 'flash-page-count', StrToIntDef(Trim(txtFlashPageCount.Text), 0));
      profile.WriteInteger(chipProfile, 'eeprom-page-size', StrToIntDef(Trim(txtE2PROMPageSize.Text), 0));
      profile.WriteInteger(chipProfile, 'eeprom-page-count', StrToIntDef(Trim(txtE2PROMPageCount.Text), 0));

      // Load profile into UI.
      LoadChipProfiles();
      cmbChipProfile.ItemIndex := cmbChipProfile.Items.IndexOf(chipProfile);
    finally
      FreeAndNil(profile);
    end;
  end;
end;

end.

