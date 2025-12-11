# Communicating with Raspberry Pi

**Part of:** [Chapter 26: Hardware Interfacing](./README.md)

---

## Introduction

UART communication allows your computer to exchange data with other devices like Raspberry Pi. This section teaches you how to use UART for serial communication and bridge with MicroPython.

**Key concepts:**
- **UART** — Universal Asynchronous Receiver-Transmitter
- **Serial communication** — Sending data byte by byte
- **Protocol design** — Structuring data exchange
- **MicroPython bridging** — Connecting with Python code

---

## Understanding UART

### What is UART?

**UART is a serial communication protocol:**

- **Asynchronous** — No shared clock
- **Two wires** — TX (transmit), RX (receive)
- **Byte-by-byte** — Sends one byte at a time
- **Configurable** — Baud rate, data bits, parity

**UART connection:**
```
Zeal Computer          Raspberry Pi
    TX ───────────────> RX
    RX <─────────────── TX
   GND ──────────────── GND
```

**Common baud rates:**
- 9600 — Slow, reliable
- 19200 — Medium speed
- 38400 — Fast
- 115200 — Very fast

### UART Hardware

**UART on Zeal (via PIO):**

```pascal
const
  UART_TX_BIT = 4;  // PIO bit for transmit
  UART_RX_BIT = 3;  // PIO bit for receive
  UART_BAUD_9600 = 104;  // Timer value for 9600 baud
```

---

## UART Functions

### Basic UART Operations

**Send and receive bytes:**

```pascal
procedure UART_Init(baudRate: word);
begin
  // Configure PIO for UART
  PortOut(IO_PIO_CTRL_A, $CF);  // Bit mode
  PortOut(IO_PIO_CTRL_A, $10);  // Pin 4 = output (TX), pin 3 = input (RX)
  
  // Set initial state (idle = high)
  var portValue := PortIn(GPIO_PORT);
  portValue := portValue or (1 shl UART_TX_BIT);
  PortOut(GPIO_PORT, portValue);
end;

procedure UART_SendByte(data: byte);
var
  i: byte;
  bitTime: word;
begin
  bitTime := UART_BAUD_9600;
  
  // Start bit (low)
  SetGPIO(UART_TX_BIT, false);
  DelayMicroseconds(bitTime);
  
  // Data bits (LSB first)
  for i := 0 to 7 do
  begin
    SetGPIO(UART_TX_BIT, (data and (1 shl i)) <> 0);
    DelayMicroseconds(bitTime);
  end;
  
  // Stop bit (high)
  SetGPIO(UART_TX_BIT, true);
  DelayMicroseconds(bitTime);
end;

function UART_ReceiveByte: byte;
var
  i: byte;
  data: byte;
  bitTime: word;
begin
  bitTime := UART_BAUD_9600;
  
  // Wait for start bit (low)
  while ReadGPIO(UART_RX_BIT) do
    ;  // Wait
  
  // Wait half bit time (sample in middle)
  DelayMicroseconds(bitTime div 2);
  
  // Read data bits (LSB first)
  data := 0;
  for i := 0 to 7 do
  begin
    DelayMicroseconds(bitTime);
    if ReadGPIO(UART_RX_BIT) then
      data := data or (1 shl i);
  end;
  
  // Wait for stop bit
  DelayMicroseconds(bitTime);
  
  UART_ReceiveByte := data;
end;
```

### Sending Strings

**Send text messages:**

```pascal
procedure UART_SendString(s: string);
var
  i: byte;
begin
  for i := 1 to Length(s) do
    UART_SendByte(Ord(s[i]));
end;

procedure UART_SendLine(s: string);
begin
  UART_SendString(s);
  UART_SendByte(13);  // Carriage return
  UART_SendByte(10);  // Line feed
end;
```

### Receiving Strings

**Receive text messages:**

```pascal
function UART_ReceiveString(maxLength: byte): string;
var
  s: string;
  ch: char;
  i: byte;
begin
  s := '';
  i := 0;
  
  while i < maxLength do
  begin
    ch := Chr(UART_ReceiveByte);
    
    if ch = #13 then  // Carriage return
      Break;
    
    s := s + ch;
    i := i + 1;
  end;
  
  UART_ReceiveString := s;
end;
```

---

## Protocol Design

### Simple Protocol

**Basic command protocol:**

```pascal
type
  TCommand = (cmdPing, cmdGetTemp, cmdSetLED, cmdGetStatus);
  
  TMessage = record
    Command: TCommand;
    Data: array[0..15] of byte;
    DataLength: byte;
  end;

procedure SendMessage(var msg: TMessage);
begin
  // Send command byte
  UART_SendByte(Ord(msg.Command));
  
  // Send data length
  UART_SendByte(msg.DataLength);
  
  // Send data
  var i: byte;
  for i := 0 to msg.DataLength - 1 do
    UART_SendByte(msg.Data[i]);
end;

function ReceiveMessage(var msg: TMessage): boolean;
var
  cmdByte: byte;
  i: byte;
begin
  // Receive command
  cmdByte := UART_ReceiveByte;
  if cmdByte > Ord(High(TCommand)) then
  begin
    ReceiveMessage := false;
    Exit;
  end;
  msg.Command := TCommand(cmdByte);
  
  // Receive data length
  msg.DataLength := UART_ReceiveByte;
  if msg.DataLength > 16 then
  begin
    ReceiveMessage := false;
    Exit;
  end;
  
  // Receive data
  for i := 0 to msg.DataLength - 1 do
    msg.Data[i] := UART_ReceiveByte;
  
  ReceiveMessage := true;
end;
```

### Command Handling

**Process received commands:**

```pascal
procedure HandleCommand(var msg: TMessage);
var
  response: TMessage;
begin
  case msg.Command of
    cmdPing:
      begin
        // Respond with pong
        response.Command := cmdPing;
        response.Data[0] := $FF;  // Pong indicator
        response.DataLength := 1;
        SendMessage(response);
      end;
      
    cmdGetTemp:
      begin
        // Read temperature and send
        var temp := ReadTemperature;
        response.Command := cmdGetTemp;
        Move(temp, response.Data[0], SizeOf(Q8.8));
        response.DataLength := SizeOf(Q8.8);
        SendMessage(response);
      end;
      
    cmdSetLED:
      begin
        // Set LED state
        SetLED(msg.Data[0], msg.Data[1] <> 0);
        
        // Acknowledge
        response.Command := cmdSetLED;
        response.Data[0] := 1;  // Success
        response.DataLength := 1;
        SendMessage(response);
      end;
      
    cmdGetStatus:
      begin
        // Send status
        response.Command := cmdGetStatus;
        response.Data[0] := GetSystemStatus;
        response.DataLength := 1;
        SendMessage(response);
      end;
  end;
end;
```

---

## Raspberry Pi Communication

### Python Side (MicroPython)

**Raspberry Pi code:**

```python
# Raspberry Pi side (MicroPython)
import serial
import time

# Open serial port
ser = serial.Serial('/dev/ttyUSB0', 9600, timeout=1)

def send_command(cmd, data=None):
    # Send command byte
    ser.write(bytes([cmd]))
    
    # Send data length
    if data:
        ser.write(bytes([len(data)]))
        ser.write(data)
    else:
        ser.write(bytes([0]))

def receive_response():
    # Receive command
    cmd = ser.read(1)[0]
    
    # Receive data length
    length = ser.read(1)[0]
    
    # Receive data
    data = ser.read(length)
    
    return cmd, data

# Example: Get temperature
send_command(1)  # cmdGetTemp
cmd, data = receive_response()
if cmd == 1:
    # Parse temperature (Q8.8 format)
    temp = int.from_bytes(data, 'little') / 256.0
    print(f"Temperature: {temp}°C")
```

### SuperPascal Side

**Zeal computer code:**

```pascal
program RaspberryPiComm;

const
  CMD_PING = 0;
  CMD_GET_TEMP = 1;
  CMD_SET_LED = 2;
  CMD_GET_STATUS = 3;

procedure ProcessCommands;
var
  msg: TMessage;
begin
  while true do
  begin
    if ReceiveMessage(msg) then
      HandleCommand(msg);
    
    Delay(10);  // Small delay
  end;
end;

begin
  InitGPIO;
  UART_Init(9600);
  
  // Main loop
  while true do
  begin
    ProcessCommands;
    UpdateSystem;
    WaitVBlank;
  end;
end.
```

---

## Data Exchange Examples

### Temperature Monitoring

**Send temperature to Raspberry Pi:**

```pascal
procedure SendTemperature;
var
  temp: Q8.8;
  msg: TMessage;
begin
  temp := ReadTemperature;
  
  msg.Command := cmdGetTemp;
  Move(temp, msg.Data[0], SizeOf(Q8.8));
  msg.DataLength := SizeOf(Q8.8);
  
  SendMessage(msg);
end;

// On Raspberry Pi
def monitor_temperature():
    while True:
        send_command(CMD_GET_TEMP)
        cmd, data = receive_response()
        if cmd == CMD_GET_TEMP:
            temp = int.from_bytes(data, 'little') / 256.0
            print(f"Temp: {temp:.2f}°C")
        time.sleep(1)
```

### Remote LED Control

**Control LEDs from Raspberry Pi:**

```pascal
procedure HandleSetLED(var msg: TMessage);
begin
  // msg.Data[0] = LED number
  // msg.Data[1] = state (0/1)
  SetLED(msg.Data[0], msg.Data[1] <> 0);
end;

// On Raspberry Pi
def set_led(led_num, state):
    send_command(CMD_SET_LED, bytes([led_num, state]))
    cmd, data = receive_response()
    if data[0] == 1:
        print("LED set successfully")
```

### Bidirectional Communication

**Exchange data both ways:**

```pascal
procedure ExchangeData;
var
  msg: TMessage;
  response: TMessage;
begin
  // Receive command from Pi
  if ReceiveMessage(msg) then
  begin
    // Process command
    case msg.Command of
      cmdGetTemp:
        begin
          response.Command := cmdGetTemp;
          var temp := ReadTemperature;
          Move(temp, response.Data[0], SizeOf(Q8.8));
          response.DataLength := SizeOf(Q8.8);
          SendMessage(response);
        end;
      
      cmdSetLED:
        begin
          SetLED(msg.Data[0], msg.Data[1] <> 0);
          response.Command := cmdSetLED;
          response.Data[0] := 1;  // Success
          response.DataLength := 1;
          SendMessage(response);
        end;
    end;
  end;
end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program RaspberryPiBridge;

const
  CMD_PING = 0;
  CMD_GET_TEMP = 1;
  CMD_SET_LED = 2;

type
  TCommand = (cmdPing, cmdGetTemp, cmdSetLED);
  
  TMessage = record
    Command: byte;
    Data: array[0..15] of byte;
    DataLength: byte;
  end;

var
  Temperature: Q8.8;

procedure UART_Init;
begin
  // Initialize UART (simplified)
end;

procedure UART_SendByte(data: byte);
begin
  // Send byte (simplified)
end;

function UART_ReceiveByte: byte;
begin
  // Receive byte (simplified)
  UART_ReceiveByte := 0;
end;

procedure SendMessage(var msg: TMessage);
var
  i: byte;
begin
  UART_SendByte(msg.Command);
  UART_SendByte(msg.DataLength);
  for i := 0 to msg.DataLength - 1 do
    UART_SendByte(msg.Data[i]);
end;

function ReceiveMessage(var msg: TMessage): boolean;
var
  i: byte;
begin
  msg.Command := UART_ReceiveByte;
  msg.DataLength := UART_ReceiveByte;
  
  if msg.DataLength > 16 then
  begin
    ReceiveMessage := false;
    Exit;
  end;
  
  for i := 0 to msg.DataLength - 1 do
    msg.Data[i] := UART_ReceiveByte;
  
  ReceiveMessage := true;
end;

procedure HandleCommand(var msg: TMessage);
var
  response: TMessage;
begin
  case TCommand(msg.Command) of
    cmdPing:
      begin
        response.Command := CMD_PING;
        response.Data[0] := $FF;
        response.DataLength := 1;
        SendMessage(response);
      end;
      
    cmdGetTemp:
      begin
        Temperature := ReadTemperature;
        response.Command := CMD_GET_TEMP;
        Move(Temperature, response.Data[0], SizeOf(Q8.8));
        response.DataLength := SizeOf(Q8.8);
        SendMessage(response);
      end;
      
    cmdSetLED:
      begin
        SetLED(msg.Data[0], msg.Data[1] <> 0);
        response.Command := CMD_SET_LED;
        response.Data[0] := 1;
        response.DataLength := 1;
        SendMessage(response);
      end;
  end;
end;

begin
  InitGPIO;
  InitI2C;
  UART_Init;
  
  while true do
  begin
    var msg: TMessage;
    if ReceiveMessage(msg) then
      HandleCommand(msg);
    
    Delay(10);
  end;
end.
```

---

## Best Practices

### 1. Use Consistent Protocol

**Standardize message format:**

```pascal
// ✅ GOOD: Consistent format
TMessage = record
  Command: byte;
  DataLength: byte;
  Data: array[0..15] of byte;
end;

// ❌ BAD: Inconsistent format
// Different formats for different commands
```

### 2. Validate Received Data

**Check data integrity:**

```pascal
// ✅ GOOD: Validate
if msg.DataLength > 16 then
  Exit;  // Invalid

// ❌ BAD: No validation
// May overflow buffer
```

### 3. Handle Timeouts

**Don't wait forever:**

```pascal
// ✅ GOOD: Timeout
if not ReceiveMessageWithTimeout(msg, 1000) then
  WriteLn('Timeout');

// ❌ BAD: Wait forever
ReceiveMessage(msg);  // May hang
```

### 4. Use Checksums

**Verify data integrity:**

```pascal
// ✅ GOOD: Checksum
msg.Checksum := CalculateChecksum(msg);
// ... send ...
if VerifyChecksum(receivedMsg) then
  ProcessMessage(receivedMsg);

// ❌ BAD: No checksum
// Can't detect corruption
```

### 5. Error Handling

**Handle communication errors:**

```pascal
// ✅ GOOD: Error handling
if not SendMessage(msg) then
  WriteLn('Send failed');

// ❌ BAD: No error handling
SendMessage(msg);  // May fail silently
```

---

## Exercises

### Exercise 1: Basic UART

Write a program that:
1. Initializes UART
2. Sends bytes
3. Receives bytes
4. Demonstrates serial communication

### Exercise 2: Protocol Design

Write a program that:
1. Defines message protocol
2. Sends structured messages
3. Receives and parses messages
4. Handles different command types

### Exercise 3: Raspberry Pi Bridge

Write a program that:
1. Communicates with Raspberry Pi
2. Exchanges data
3. Handles commands
4. Sends responses

### Exercise 4: Complete System

Write a program that:
1. Implements complete communication system
2. Handles multiple commands
3. Processes sensor data
4. Integrates with hardware

---

**Previous Section:** [Reading Sensors](./02_ReadingSensors.md)  
**Next Chapter:** [Chapter 27: Advanced Topics](../25_AdvancedTopics/README.md)  
**Language Specification:** See [Memory and I/O](../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX

