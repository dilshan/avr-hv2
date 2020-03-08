/*==============================================================================

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
==============================================================================*/

#define RX_BUFFER_SIZE  4

#define CMD_HANDSHAKE       'H'
#define CMD_VERSION         'V'
#define CMD_CHIP_SIGNATURE  'S'
#define CMD_GET_FUSE        'A'
#define CMD_GET_CALIBRATE   'L'
#define CMD_READ_FLASH      'R'
#define CMD_END             'E'
#define CMD_ERASE           'C'
#define CMD_WRITE_FLASH     'W'
#define CMD_WRITE_PAGE      'P'
#define CMD_READ_EEPROM     'T'
#define CMD_WRITE_EEPROM    'J'
#define CMD_WRITE_E2PAGE    'X'

#define PIN_VCC             22
#define PIN_12V             23

#define PIN_RDY             24
#define PIN_OE              25
#define PIN_WR              26
#define PIN_BS1             27
#define PIN_BS2             28
#define PIN_XTAL1           29
#define PIN_XA0             30
#define PIN_XA1             31
#define PIN_PAGEL           32

#define PIN_D0              2
#define PIN_D1              3
#define PIN_D2              4
#define PIN_D3              5
#define PIN_D4              6
#define PIN_D5              7
#define PIN_D6              8
#define PIN_D7              9

#define PROG_ENABLE0        PIN_BS1
#define PROG_ENABLE1        PIN_XA0
#define PROG_ENABLE2        PIN_XA1
#define PROG_ENABLE3        PIN_PAGEL

#define loadAddressLow(a) loadAddress(LOW, a)
#define loadAddressHigh(a) loadAddress(HIGH, a)

#define loadDataLow(d) loadData(LOW, d)
#define loadDataHigh(d) loadData(HIGH, d)

const unsigned char charMap[16] = {0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66};

char commandBuffer[RX_BUFFER_SIZE];
unsigned char commandPos;
unsigned char lastCommand = ' ';

void resetBuffer()
{
  // Clear command buffer.
  for(unsigned char tmpPos = 0; tmpPos < RX_BUFFER_SIZE; tmpPos++)
  {
    commandBuffer[tmpPos] = 0;    
  }

  commandPos = 0;
}

void sendCommandResponse(char cmd, char error, char dataLen)
{
  char responseData[] = {'@', cmd, error, dataLen};
  Serial.write(responseData, 4);  
}

void byteToChr(unsigned char inByte, unsigned char *out)
{
  out[0] = charMap[(inByte >> 4) & 0x0F];
  out[1] = charMap[inByte & 0x0F];
}

void chrToByte(unsigned char *chrBuffer, unsigned char* byteData)
{
  byteData[0] = (((chrBuffer[0] < 58) ? (chrBuffer[0] - 48) : (chrBuffer[0]) - 87) << 4) 
  | ((chrBuffer[1] < 58) ? (chrBuffer[1] - 48) : (chrBuffer[1] - 87));
}

void chrToWord(unsigned char *chrBuffer, unsigned char* wordData)
{
  wordData[0] = (((chrBuffer[0] < 58) ? (chrBuffer[0] - 48) : (chrBuffer[0]) - 87) << 4) 
  | ((chrBuffer[1] < 58) ? (chrBuffer[1] - 48) : (chrBuffer[1] - 87));
  wordData[1] = (((chrBuffer[2] < 58) ? (chrBuffer[2] - 48) : (chrBuffer[2]) - 87) << 4) 
  | ((chrBuffer[3] < 58) ? (chrBuffer[3] - 48) : (chrBuffer[3] - 87));  
}

void chrToWriteWord(unsigned char *chrBuffer, unsigned char* wordData)
{
  wordData[0] = (((chrBuffer[0] < 58) ? (chrBuffer[0] - 48) : (chrBuffer[0]) - 87) << 4) 
  | ((chrBuffer[1] < 58) ? (chrBuffer[1] - 48) : (chrBuffer[1] - 87));
  wordData[1] = (((chrBuffer[2] < 58) ? (chrBuffer[2] - 48) : (chrBuffer[2]) - 87) << 4) 
  | ((chrBuffer[3] < 58) ? (chrBuffer[3] - 48) : (chrBuffer[3] - 87));  
  wordData[2] = (((chrBuffer[4] < 58) ? (chrBuffer[4] - 48) : (chrBuffer[4]) - 87) << 4) 
  | ((chrBuffer[5] < 58) ? (chrBuffer[5] - 48) : (chrBuffer[5] - 87));
}

void setDataPinMode(unsigned char value)
{
  pinMode(PIN_D0, value);
  pinMode(PIN_D1, value);
  pinMode(PIN_D2, value);
  pinMode(PIN_D3, value);
  pinMode(PIN_D4, value);
  pinMode(PIN_D5, value);
  pinMode(PIN_D6, value);
  pinMode(PIN_D7, value);
  
  delayMicroseconds(5);
}

void initProgramMode()
{
  setDataPinMode(INPUT);
  
  // Set Prog_enable pins to 0000.
  digitalWrite(PROG_ENABLE0, LOW);
  digitalWrite(PROG_ENABLE1, LOW);
  digitalWrite(PROG_ENABLE2, LOW);
  digitalWrite(PROG_ENABLE3, LOW);

  // Shutdown VCC and +12V
  digitalWrite(PIN_VCC, LOW);
  digitalWrite(PIN_12V, LOW);

  delayMicroseconds(25);

  // Apply 5V to target device.
  digitalWrite(PIN_VCC, HIGH);
  delayMicroseconds(50);

  // Apply +12V to target device.
  digitalWrite(PIN_12V, HIGH);

  // Wait before giving any parallel programming commands.
  delayMicroseconds(500);

  // Enable output and write modes.
  digitalWrite(PIN_OE, HIGH);
  digitalWrite(PIN_WR, HIGH);
  delayMicroseconds(50);
}

void resetProgramMode()
{
  // Power the device down by setting VCC and +12V to low.
  digitalWrite(PIN_VCC, LOW);
  digitalWrite(PIN_12V, LOW);

  // Reset command bits.
  digitalWrite(PIN_RDY, HIGH);
  digitalWrite(PIN_OE, LOW);
  digitalWrite(PIN_WR, LOW);
  digitalWrite(PIN_BS1, LOW);
  digitalWrite(PIN_BS2, LOW);
  digitalWrite(PIN_XTAL1, LOW);
  digitalWrite(PIN_XA0, LOW);
  digitalWrite(PIN_XA1, LOW);
  digitalWrite(PIN_PAGEL, LOW);
}

void loadCommand(unsigned char cmd)
{
  // Set XA1, XA0 to 1, 0.
  digitalWrite(PIN_XA0, LOW);
  digitalWrite(PIN_XA1, HIGH);

  // Set BS1 to 0.
  digitalWrite(PIN_BS1, LOW);

  // Set DATA with specified command.
  setDataPinMode(OUTPUT);  
  digitalWrite(PIN_D0, (cmd >> 0) & 0x01);
  digitalWrite(PIN_D1, (cmd >> 1) & 0x01);
  digitalWrite(PIN_D2, (cmd >> 2) & 0x01);
  digitalWrite(PIN_D3, (cmd >> 3) & 0x01);
  digitalWrite(PIN_D4, (cmd >> 4) & 0x01);
  digitalWrite(PIN_D5, (cmd >> 5) & 0x01);
  digitalWrite(PIN_D6, (cmd >> 6) & 0x01);
  digitalWrite(PIN_D7, (cmd >> 7) & 0x01);

  // Pulse XTAL1.
  delayMicroseconds(5);
  digitalWrite(PIN_XTAL1, HIGH);
  delayMicroseconds(5);
  digitalWrite(PIN_XTAL1, LOW);
  delayMicroseconds(5);
}

void loadAddress(unsigned char seg, unsigned char addr)
{
  // Set XA1, XA0 to 0, 0.
  digitalWrite(PIN_XA0, LOW);
  digitalWrite(PIN_XA1, LOW);

  // Set BS1 based on address HIGH or LOW.
  digitalWrite(PIN_BS1, seg);

  // Set DATA with specified address.
  setDataPinMode(OUTPUT);  
  digitalWrite(PIN_D0, (addr >> 0) & 0x01);
  digitalWrite(PIN_D1, (addr >> 1) & 0x01);
  digitalWrite(PIN_D2, (addr >> 2) & 0x01);
  digitalWrite(PIN_D3, (addr >> 3) & 0x01);
  digitalWrite(PIN_D4, (addr >> 4) & 0x01);
  digitalWrite(PIN_D5, (addr >> 5) & 0x01);
  digitalWrite(PIN_D6, (addr >> 6) & 0x01);
  digitalWrite(PIN_D7, (addr >> 7) & 0x01);

  // Pulse XTAL1.
  delayMicroseconds(5);
  digitalWrite(PIN_XTAL1, HIGH);
  delayMicroseconds(5);
  digitalWrite(PIN_XTAL1, LOW);
  delayMicroseconds(5);  
}

void loadData(unsigned char seg, unsigned char data)
{
  // Set BS1 based on data HIGH or LOW.
  digitalWrite(PIN_BS1, seg);
  
  // Set XA1, XA0 to 0, 1.
  digitalWrite(PIN_XA0, HIGH);  
  digitalWrite(PIN_XA1, LOW);

  // Set DATA.
  setDataPinMode(OUTPUT); 
  digitalWrite(PIN_D0, (data >> 0) & 0x01);
  digitalWrite(PIN_D1, (data >> 1) & 0x01);
  digitalWrite(PIN_D2, (data >> 2) & 0x01);
  digitalWrite(PIN_D3, (data >> 3) & 0x01);
  digitalWrite(PIN_D4, (data >> 4) & 0x01);
  digitalWrite(PIN_D5, (data >> 5) & 0x01);
  digitalWrite(PIN_D6, (data >> 6) & 0x01);
  digitalWrite(PIN_D7, (data >> 7) & 0x01);

  // Pulse XTAL1.
  delayMicroseconds(5);
  digitalWrite(PIN_XTAL1, HIGH);
  delayMicroseconds(5);
  digitalWrite(PIN_XTAL1, LOW);
  delayMicroseconds(5);  
}

void latchData()
{
  // Set BS1 to 1.
  digitalWrite(PIN_BS1, HIGH);

  // Pulse PAGEL.
  delayMicroseconds(5);
  digitalWrite(PIN_PAGEL, HIGH);
  delayMicroseconds(5);
  digitalWrite(PIN_PAGEL, LOW);
  delayMicroseconds(5);
}

unsigned char programPage()
{
  unsigned char writeTimeout = 0;
  
  // Pulse WR.
  delayMicroseconds(5);
  digitalWrite(PIN_WR, LOW);
  delayMicroseconds(5);
  digitalWrite(PIN_WR, HIGH);

  // Wait until RDY/BSY goes high.
  while(writeTimeout < 250)
  {
    delay(10);
    writeTimeout++;

    if(digitalRead(PIN_RDY) == HIGH)
    {
      writeTimeout = 0;
      break;      
    }
  }

  return (writeTimeout == 0) ? 0x30 : 0x31;
}

unsigned char readData()
{
  unsigned char retData;
  
  setDataPinMode(INPUT);
  retData = (digitalRead(PIN_D0) << 0) & 0x01;
  retData |= (digitalRead(PIN_D1) << 1) & 0x02;
  retData |= (digitalRead(PIN_D2) << 2) & 0x04;
  retData |= (digitalRead(PIN_D3) << 3) & 0x08;
  retData |= (digitalRead(PIN_D4) << 4) & 0x10;
  retData |= (digitalRead(PIN_D5) << 5) & 0x20;
  retData |= (digitalRead(PIN_D6) << 6) & 0x40;
  retData |= (digitalRead(PIN_D7) << 7) & 0x80;

  return retData;
}

void setup() 
{
  unsigned char clearPos, tempBuffer;
  
  // Set power pin modes.
  pinMode(PIN_VCC, OUTPUT);   // VCC/+5V output control.
  pinMode(PIN_12V, OUTPUT);   // +12V output control.

  // Set initial state of the power related I/O pins.
  digitalWrite(PIN_VCC, LOW);
  digitalWrite(PIN_12V, LOW);

  // Initialize serial interface.
  Serial.begin(115200);
  delayMicroseconds(55);

  // Sep pin modes and initial values.
  pinMode(PIN_RDY, INPUT);
  pinMode(PIN_OE, OUTPUT);
  pinMode(PIN_WR, OUTPUT);
  pinMode(PIN_BS1, OUTPUT);
  pinMode(PIN_BS2, OUTPUT);
  pinMode(PIN_XTAL1, OUTPUT);
  pinMode(PIN_XA0, OUTPUT);
  pinMode(PIN_XA1, OUTPUT);
  pinMode(PIN_PAGEL, OUTPUT);

  setDataPinMode(OUTPUT);
  resetProgramMode();

  // Clear data in serial RX buffer.
  while(Serial.available() > 0)
  {
    tempBuffer = Serial.read();
    if((++clearPos) > 250)
    {
      break;
    }
  }

  resetBuffer();
}

void readSignature(unsigned char* signature)
{
  unsigned char addrOffset = 0;
  
  // Enter device into programming mode.
  initProgramMode();  

  // Load Command 0000 1000.
  loadCommand(0x08);

  while(addrOffset < 3)
  {
    // Load Address Low Byte for signature 0.
    loadAddressLow(addrOffset);
  
    // Set OE to 0, and BS1 to 0.
    digitalWrite(PIN_OE, LOW);
    digitalWrite(PIN_BS1, LOW);
  
    // Read signature byte from DATA.
    signature[addrOffset] = readData();
  
    // Set OE to 1.
    digitalWrite(PIN_OE, HIGH);
    addrOffset++;
  }

  resetProgramMode();
}

void readFuseBytes(unsigned char* fuseConfig)
{
  // Enter device into programming mode.
  initProgramMode();

  // Load Command 0000 0100.
  loadCommand(0x04);

  // Reading Fuse Low byte. Set OE to 0, BS2 to 0 and BS1 to 0.
  digitalWrite(PIN_OE, LOW);
  digitalWrite(PIN_BS1, LOW);
  digitalWrite(PIN_BS2, LOW);
  fuseConfig[0] = readData();

  // Reading Fuse High byte. Set OE to 0, BS2 to 1 and BS1 to 1.
  digitalWrite(PIN_OE, LOW);
  digitalWrite(PIN_BS1, HIGH);
  digitalWrite(PIN_BS2, HIGH);
  fuseConfig[1] = readData();

  // Reading Fuse Extended byte. Set OE to 0, BS2 to 1, and BS1 to 0.
  digitalWrite(PIN_OE, LOW);
  digitalWrite(PIN_BS1, LOW);
  digitalWrite(PIN_BS2, HIGH);
  fuseConfig[2] = readData();

  // Reading Lock byte. Set OE to 0, BS2 to 0 and BS1 to 1.
  digitalWrite(PIN_OE, LOW);
  digitalWrite(PIN_BS1, HIGH);
  digitalWrite(PIN_BS2, LOW);
  fuseConfig[3] = readData();

  // Set OE to 1.
  digitalWrite(PIN_OE, HIGH);
  resetProgramMode();
}

unsigned char readCalibrationByte()
{
  unsigned char tempData;
  
  // Enter device into programming mode.
  initProgramMode();

  // Load Command 0000 1000.
  loadCommand(0x08);

  // Load Address Low Byte 0x00.
  loadAddressLow(0x00);

  // Set OE to 0, and BS1 to 1.
  digitalWrite(PIN_OE, LOW);
  digitalWrite(PIN_BS1, HIGH);

  // Read calibration byte from DATA.
  tempData = readData();

  // Set OE to 1.
  digitalWrite(PIN_OE, HIGH);
  resetProgramMode();

  return tempData;  
}

unsigned char readCmdParam(unsigned char dataLen, unsigned char* dataBuffer)
{
  unsigned char readAttempt = 0;
  unsigned char tmpParamData;
  unsigned char readPos = 0;

  while(readAttempt < 250)
  {
    readAttempt++;
    
    // Check for any available data in serial buffer.
    if(Serial.available() > 0)
    {
      tmpParamData = Serial.read();      
            
      // Verify received data is in valid range.
      if(((tmpParamData >= 0x30) && (tmpParamData <= 0x39)) || ((tmpParamData >= 0x61) && (tmpParamData <= 0x66)))
      {
        dataBuffer[readPos] = tmpParamData;
        readPos++;
        readAttempt = 0;
      }

      // Check for data limit to terminate the service loop.
      if(readPos >= dataLen)
      {
        break;
      }
  
      delayMicroseconds(5);      
    }
    else
    {
      delayMicroseconds(10);
    }
  }

  return (readAttempt == 0) ? 0 : 1;
}

void writeEEPROM(unsigned char* addr, unsigned char data)
{
  // Set write mode if last command is not CMD_WRITE_EEPROM or CMD_WRITE_E2PAGE.
  if(!((lastCommand == CMD_WRITE_EEPROM) || (lastCommand == CMD_WRITE_E2PAGE)))
  {
      // Enter device into programming mode.
      initProgramMode();

      // Load Command 0001 0001.
      loadCommand(0x11);
  }

  // Load Address High Byte.
  loadAddressHigh(addr[0]);

  // Load Address Low Byte.
  loadAddressLow(addr[1]);

  // Load Data.
  loadDataLow(data);

  // Latch Data.
  latchData();
}

void writeFlash(unsigned char addr, unsigned char *data)
{
  // Set write mode if last command is not CMD_WRITE_FLASH or CMD_WRITE_PAGE.
  if(!((lastCommand == CMD_WRITE_FLASH) || (lastCommand == CMD_WRITE_PAGE)))
  {
      // Enter device into programming mode.
      initProgramMode();

      // Load Command 0001 0000.
      loadCommand(0x10);
  }

  // Load Address Low byte.
  loadAddressLow(addr);

  // Load Data Low Byte.
  loadDataLow(data[1]);

  // Load Data High Byte.
  loadDataHigh(data[0]);

  // Latch Data.
  latchData();
}

unsigned char writeE2PROMPage()
{
  // Set write mode if last command is not CMD_WRITE_EEPROM or CMD_WRITE_E2PAGE.
  if(!((lastCommand == CMD_WRITE_EEPROM) || (lastCommand == CMD_WRITE_E2PAGE)))
  {
      // Enter device into programming mode.
      initProgramMode();

      // Load Command 0001 0001.
      loadCommand(0x11);
  }

  // Set BS1 to 0.
  digitalWrite(PIN_BS1, LOW);

  // Program Page.
  return programPage();
}

unsigned char writePage(unsigned char addr)
{
  // Set write mode if last command is not CMD_WRITE_FLASH or CMD_WRITE_PAGE.
  if(!((lastCommand == CMD_WRITE_FLASH) || (lastCommand == CMD_WRITE_PAGE)))
  {
      // Enter device into programming mode.
      initProgramMode();

      // Load Command 0001 0000.
      loadCommand(0x10);
  }

  // Load Address High byte.
  loadAddressHigh(addr);

  // Program Page.
  return programPage();
}

unsigned char readEEPROM(unsigned char* addr)
{
  unsigned char data;
  
  // Set read mode if last command is not CMD_READ_EEPROM.
  if(lastCommand != CMD_READ_EEPROM)
  {
      // Enter device into programming mode.
      initProgramMode();

      // Load Command 0000 0011.
      loadCommand(0x03);
  }  

  // Load Address High Byte.
  loadAddressHigh(addr[0]);

  // Load Address Low Byte.
  loadAddressLow(addr[1]);

  // Set OE to 0, and BS1 to 1.
  digitalWrite(PIN_OE, LOW);
  digitalWrite(PIN_BS1, LOW);

  // Read EEPROM content.
  data = readData();

  // Set OE to 1.
  digitalWrite(PIN_OE, HIGH);
    
  return data;
}

void readFlash(unsigned char* addr, unsigned char *data)
{
  // Set read mode if last command is not CMD_READ_FLASH.
  if(lastCommand != CMD_READ_FLASH)
  {
      // Enter device into programming mode.
      initProgramMode();

      // Load Command 0000 0010.
      loadCommand(0x02);
  }

  // Load Address High Byte.
  loadAddressHigh(addr[0]);

  // Load Address Low Byte.
  loadAddressLow(addr[1]);

  // Set OE to 0, and BS1 to 0.
  digitalWrite(PIN_OE, LOW);
  digitalWrite(PIN_BS1, LOW);

  // Read flash word low byte.
  data[0] = readData();

  // Set BS1 to 1.
  digitalWrite(PIN_BS1, HIGH);

  // Read flash word high byte.
  data[1] = readData();

  // Set OE to 1.
  digitalWrite(PIN_OE, HIGH);
}

unsigned char eraseChip()
{
  unsigned char eraseTimeout = 0;
  
  // Enter device into programming mode.
  initProgramMode();

  // Load Command 1000 0000.
  loadCommand(0x80);

  // Give WR a negative pulse.
  delayMicroseconds(5);
  digitalWrite(PIN_WR, LOW);
  delayMicroseconds(5);
  digitalWrite(PIN_WR, HIGH);

  // Wait until RDY/BSY goes high.
  while(eraseTimeout < 250)
  {
    delay(20);
    eraseTimeout++;

    if(digitalRead(PIN_RDY) == HIGH)
    {
      eraseTimeout = 0;
      break;      
    }
  }

  resetProgramMode();
  return (eraseTimeout == 0) ? 0x30 : 0x31;
}

void loop() 
{
  if(Serial.available() > 0)
  {
    char tempData= Serial.read();

    // Clear data buffer at the beginning of the next command.
    if(tempData == '~')
    {
      resetBuffer();
    }

    // Fill command buffer with incoming bytes.
    if(commandPos < 4)
    {
      commandBuffer[commandPos] = tempData;
      commandPos++; 
    }

    // Looking for end of command.
    if(tempData == '^')
    {
      delayMicroseconds(10);

      unsigned char tmpSignature[3] = {0,0,0};
      unsigned char sigData[6];

      unsigned char tmpFuse[4] = {0,0,0,0};
      unsigned char fuseData[8];

      unsigned char calibrationData[2];

      unsigned char inAddr[4];
      unsigned char inAddrWord[2];

      unsigned char outData[4];
      unsigned char outDataWord[2];

      unsigned char inWriteAddr[6];
      unsigned char inwriteData[3];

      unsigned char inWriteHighAddr[2];
      unsigned char inAddrByte[0];

      unsigned char writeStatus;
      
      unsigned char eraseStatus;

      // Execute specified action based on the received command.
      switch(commandBuffer[1])
      {
        case CMD_HANDSHAKE:
          sendCommandResponse(CMD_HANDSHAKE, '0', '0');
          lastCommand = CMD_HANDSHAKE;
          break;
        case CMD_VERSION:
          sendCommandResponse(CMD_VERSION, '0', '8');
          // Submit version info at the end of the header.
          Serial.write("01.00.00");
          lastCommand = CMD_VERSION;
          break;
        case CMD_CHIP_SIGNATURE:
          // Read chip 3 byte chip signature.
          readSignature(tmpSignature); 

          // Convert byte to char.
          byteToChr(tmpSignature[0], sigData);
          byteToChr(tmpSignature[1], sigData + 2);
          byteToChr(tmpSignature[2], sigData + 4);

          // Submit chip signature at the end of the header.
          sendCommandResponse(CMD_CHIP_SIGNATURE, '0', '6');
          Serial.write(sigData, 6);
          lastCommand = CMD_CHIP_SIGNATURE;
          break;
        case CMD_GET_FUSE:
          // Read low, high, extended and lock fuse bytes.
          readFuseBytes(tmpFuse);

          // Convert byte to char
          byteToChr(tmpFuse[0], fuseData);
          byteToChr(tmpFuse[1], fuseData + 2);
          byteToChr(tmpFuse[2], fuseData + 4);
          byteToChr(tmpFuse[3], fuseData + 6);

          // Submit fuse configuration at the end of the header.
          sendCommandResponse(CMD_GET_FUSE, '0', '8');
          Serial.write(fuseData, 8);
          lastCommand = CMD_GET_FUSE;
          break;
        case CMD_GET_CALIBRATE:
          // Get calibration byte and convert to char.
          byteToChr(readCalibrationByte(), calibrationData);

          sendCommandResponse(CMD_GET_CALIBRATE, '0', '2');
          Serial.write(calibrationData, 2);
          lastCommand = CMD_GET_CALIBRATE;
          break;
        case CMD_READ_FLASH:
          // Read specified address of the flash memory.
          if(readCmdParam(4, inAddr) == 0)
          {
            // Address parameters are loaded successfully.
            chrToWord(inAddr, inAddrWord);
            readFlash(inAddrWord, outDataWord);

            // Convert byte to char
            byteToChr(outDataWord[0], outData);
            byteToChr(outDataWord[1], outData + 2);

            // Submit read data at the end of the header.
            sendCommandResponse(CMD_READ_FLASH, '0', '4');
            Serial.write(outData, 4);
          }
          else
          {
            // Address parameter loading is fail.
            sendCommandResponse(CMD_READ_FLASH, '0', '0');
          } 
           
          lastCommand = CMD_READ_FLASH;  
          break;   
        case CMD_END:
          // End current operation.
          digitalWrite(PIN_OE, HIGH);
          resetProgramMode();

          sendCommandResponse(CMD_END, '0', '0');
          lastCommand = CMD_END;
          break;   
        case CMD_ERASE:
          // Perform chip erase.
          eraseStatus = eraseChip();

          // Submit erase status at the end of the header.
          sendCommandResponse(CMD_ERASE, '0', '1');
          Serial.write(eraseStatus);
          lastCommand = CMD_ERASE;
          break;
        case CMD_WRITE_FLASH:
          // Write specified data into specified flash memory address.
          if(readCmdParam(6, inWriteAddr) == 0)
          {
            // Extract address low byte and data word. [addr][d1][d0]
            chrToWriteWord(inWriteAddr, inwriteData);
            writeFlash(inwriteData[0], inwriteData + 1);
          }
          
          // Send response of the write command.
          sendCommandResponse(CMD_WRITE_FLASH, '0', '0');          
          lastCommand = CMD_WRITE_FLASH;
          break;
        case CMD_WRITE_PAGE:  
          // Write flash page contents.       
          if(readCmdParam(2, inWriteHighAddr) == 0)
          {
            chrToByte(inWriteHighAddr, inAddrByte); 
            writeStatus = writePage(inAddrByte[0]);  

            // Send write status at the end of the header.
            sendCommandResponse(CMD_WRITE_PAGE, '0', '1');
            Serial.write(writeStatus);  
          }
          else
          {
            // Send fail response of the write page command.
            sendCommandResponse(CMD_WRITE_PAGE, '0', '0');   
          }
                 
          lastCommand = CMD_WRITE_PAGE;
          break;
        case CMD_READ_EEPROM:
          // Read specified address of the EEPROM.
          if(readCmdParam(4, inAddr) == 0)
          {
            // Address parameters are loaded successfully.
            chrToWord(inAddr, inAddrWord);
            outDataWord[0] = readEEPROM(inAddrWord);

            // Convert byte to char
            byteToChr(outDataWord[0], outData);

            // Submit EEPROM data at the end of the header.
            sendCommandResponse(CMD_READ_EEPROM, '0', '2');
            Serial.write(outData, 2);
          }
          else
          {
            // Address parameter loading is fail.
            sendCommandResponse(CMD_READ_EEPROM, '0', '0');
          }
          
          lastCommand = CMD_READ_EEPROM;
          break;
        case CMD_WRITE_EEPROM:
          // Write specified data into specified EEPROM address.
          if(readCmdParam(6, inWriteAddr) == 0)
          {
            // Extract address word and data byte. [d0][addr1][addr0]
            chrToWriteWord(inWriteAddr, inwriteData);
            writeEEPROM(inwriteData, inwriteData[2]);
          }
          
          // Send response of the write command.
          sendCommandResponse(CMD_WRITE_EEPROM, '0', '0');          
          lastCommand = CMD_WRITE_EEPROM;
          break;
        case CMD_WRITE_E2PAGE:
          // Perform EEPROM page write.
          writeStatus = writeE2PROMPage();

          // Submit write status at the end of the header.
          sendCommandResponse(CMD_WRITE_E2PAGE, '0', '1');
          Serial.write(writeStatus);
          lastCommand = CMD_WRITE_E2PAGE;
          break;
      }
    }
    
  }

}
