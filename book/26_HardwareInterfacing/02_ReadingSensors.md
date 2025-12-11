# Reading Sensors

**Part of:** [Chapter 26: Hardware Interfacing](./README.md)

---

## Introduction

Sensors allow your computer to sense the physical world. This section teaches you how to read digital and analog sensors, process sensor data, and use I²C sensors.

**Key concepts:**
- **Digital sensors** — On/off sensors (buttons, switches)
- **Analog sensors** — Continuous value sensors (temperature, light)
- **I²C sensors** — Sensors using I²C protocol
- **Sensor data processing** — Filtering and calibration

---

## Understanding Sensors

### What is a Sensor?

**A sensor converts physical phenomena to electrical signals:**

- **Digital sensors** — Output high/low (1/0)
- **Analog sensors** — Output continuous voltage
- **I²C sensors** — Digital sensors with I²C interface
- **Common types** — Temperature, light, motion, buttons

**Sensor categories:**
- **Input sensors** — Buttons, switches (digital)
- **Environmental** — Temperature, humidity, light (analog/I²C)
- **Motion** — Accelerometer, gyroscope (I²C)
- **Distance** — Ultrasonic, infrared (analog/digital)

---

## Digital Sensors

### Reading Digital Input

**Read button or switch:**

```pascal
const
  BUTTON_PIN = 1;
  GPIO_PORT = $D0;

procedure InitGPIO;
begin
  // Configure pin as input
  PortOut(IO_PIO_CTRL_A, $CF);  // Bit mode
  PortOut(IO_PIO_CTRL_A, $02);  // Pin 1 = input (bit 1 = 1)
end;

function ReadButton: boolean;
var
  portValue: byte;
begin
  portValue := PortIn(GPIO_PORT);
  // Button pressed = low (0), not pressed = high (1)
  // Assuming pull-up resistor
  ReadButton := (portValue and (1 shl BUTTON_PIN)) = 0;
end;
```

### Button Debouncing

**Handle button bounce:**

```pascal
type
  TButtonState = record
    CurrentState: boolean;
  LastState: boolean;
    LastChangeTime: word;
    DebounceTime: word;
  end;

var
  Button: TButtonState;

procedure InitButton;
begin
  Button.CurrentState := false;
  Button.LastState := false;
  Button.LastChangeTime := 0;
  Button.DebounceTime := 50;  // 50ms debounce
end;

function ReadButtonDebounced: boolean;
var
  current: boolean;
  now: word;
begin
  current := ReadButton;
  now := GetTimerValue;
  
  // If state changed, record time
  if current <> Button.LastState then
  begin
    Button.LastChangeTime := now;
    Button.LastState := current;
  end;
  
  // If state stable for debounce time, update
  if (now - Button.LastChangeTime) >= Button.DebounceTime then
    Button.CurrentState := current;
  
  ReadButtonDebounced := Button.CurrentState;
end;
```

### Multiple Buttons

**Read multiple buttons:**

```pascal
const
  BUTTON_UP = 0;
  BUTTON_DOWN = 1;
  BUTTON_LEFT = 2;
  BUTTON_RIGHT = 3;
  BUTTON_A = 4;
  BUTTON_B = 5;

function ReadButtons: word;
var
  portValue: byte;
  buttons: word;
begin
  portValue := PortIn(GPIO_PORT);
  
  // Convert to button bits
  buttons := 0;
  if (portValue and (1 shl BUTTON_UP)) = 0 then
    buttons := buttons or (1 shl BUTTON_UP);
  if (portValue and (1 shl BUTTON_DOWN)) = 0 then
    buttons := buttons or (1 shl BUTTON_DOWN);
  if (portValue and (1 shl BUTTON_LEFT)) = 0 then
    buttons := buttons or (1 shl BUTTON_LEFT);
  if (portValue and (1 shl BUTTON_RIGHT)) = 0 then
    buttons := buttons or (1 shl BUTTON_RIGHT);
  if (portValue and (1 shl BUTTON_A)) = 0 then
    buttons := buttons or (1 shl BUTTON_A);
  if (portValue and (1 shl BUTTON_B)) = 0 then
    buttons := buttons or (1 shl BUTTON_B);
  
  ReadButtons := buttons;
end;
```

---

## Analog Sensors

### Reading Analog Values

**Read analog sensor (via ADC):**

```pascal
const
  ADC_PORT = $E0;  // ADC data port (example)
  ADC_CTRL_PORT = $E1;  // ADC control port

function ReadAnalog(channel: byte): word;
begin
  // Select channel
  PortOut(ADC_CTRL_PORT, channel);
  
  // Wait for conversion
  Delay(1);  // Small delay for ADC
  
  // Read value (16-bit)
  ReadAnalog := PortInW(ADC_PORT);
end;
```

### Temperature Sensor

**Read temperature sensor:**

```pascal
const
  TEMP_SENSOR_CHANNEL = 0;

function ReadTemperature: Q8.8;
var
  rawValue: word;
  voltage: Q8.8;
  temperature: Q8.8;
begin
  // Read ADC value (0-1023 for 10-bit ADC)
  rawValue := ReadAnalog(TEMP_SENSOR_CHANNEL);
  
  // Convert to voltage (assuming 0-5V range, 10-bit ADC)
  voltage := Q8_8(rawValue) * Q8_8(5.0) / Q8_8(1023.0);
  
  // Convert voltage to temperature (example: LM35, 10mV/°C)
  temperature := voltage * Q8_8(100.0);
  
  ReadTemperature := temperature;
end;
```

### Light Sensor

**Read light sensor:**

```pascal
const
  LIGHT_SENSOR_CHANNEL = 1;

function ReadLight: byte;
var
  rawValue: word;
  lightLevel: byte;
begin
  // Read ADC value
  rawValue := ReadAnalog(LIGHT_SENSOR_CHANNEL);
  
  // Convert to 0-255 range
  lightLevel := (rawValue * 255) div 1023;
  
  ReadLight := lightLevel;
end;
```

---

## I²C Sensors

### Understanding I²C

**I²C (Inter-Integrated Circuit) protocol:**

- **2 wires** — SDA (data), SCL (clock)
- **Addressable** — Each device has address
- **Master-slave** — Computer is master
- **Multiple devices** — Can connect many sensors

**I²C operations:**
- **Write** — Send data to sensor
- **Read** — Read data from sensor
- **Register access** — Read/write sensor registers

### I²C Functions

**Basic I²C operations:**

```pascal
const
  I2C_SDA_IN = 2;   // PIO bit for SDA input
  I2C_SDA_OUT = 0;  // PIO bit for SDA output
  I2C_SCL = 1;      // PIO bit for SCL

procedure I2C_Start;
begin
  // Set SDA and SCL high
  SetGPIO(I2C_SDA_OUT, true);
  SetGPIO(I2C_SCL, true);
  Delay(1);
  
  // Pull SDA low (start condition)
  SetGPIO(I2C_SDA_OUT, false);
  Delay(1);
  
  // Pull SCL low
  SetGPIO(I2C_SCL, false);
end;

procedure I2C_Stop;
begin
  // SCL low, SDA low
  SetGPIO(I2C_SCL, false);
  SetGPIO(I2C_SDA_OUT, false);
  Delay(1);
  
  // SCL high
  SetGPIO(I2C_SCL, true);
  Delay(1);
  
  // SDA high (stop condition)
  SetGPIO(I2C_SDA_OUT, true);
  Delay(1);
end;

function I2C_WriteByte(data: byte): boolean;
var
  i: byte;
  ack: boolean;
begin
  // Send 8 bits
  for i := 0 to 7 do
  begin
    SetGPIO(I2C_SDA_OUT, (data and $80) <> 0);
    Delay(1);
    SetGPIO(I2C_SCL, true);
    Delay(1);
    SetGPIO(I2C_SCL, false);
    data := data shl 1;
  end;
  
  // Read ACK
  SetGPIO(I2C_SDA_OUT, true);  // Release SDA
  Delay(1);
  SetGPIO(I2C_SCL, true);
  Delay(1);
  ack := not ReadGPIO(I2C_SDA_IN);  // ACK is low
  SetGPIO(I2C_SCL, false);
  
  I2C_WriteByte := ack;
end;

function I2C_ReadByte(ack: boolean): byte;
var
  i: byte;
  data: byte;
begin
  data := 0;
  SetGPIO(I2C_SDA_OUT, true);  // Release SDA
  
  // Read 8 bits
  for i := 0 to 7 do
  begin
    SetGPIO(I2C_SCL, true);
    Delay(1);
    if ReadGPIO(I2C_SDA_IN) then
      data := data or (1 shl (7 - i));
    SetGPIO(I2C_SCL, false);
    Delay(1);
  end;
  
  // Send ACK/NACK
  SetGPIO(I2C_SDA_OUT, not ack);
  Delay(1);
  SetGPIO(I2C_SCL, true);
  Delay(1);
  SetGPIO(I2C_SCL, false);
  
  I2C_ReadByte := data;
end;
```

### Reading I²C Sensor

**Read from I²C sensor:**

```pascal
const
  TEMP_SENSOR_ADDR = $48;  // TMP102 address (example)

function ReadTemperatureSensor: Q8.8;
var
  tempHigh, tempLow: byte;
  temperature: word;
begin
  I2C_Start;
  
  // Write sensor address + write bit
  if not I2C_WriteByte((TEMP_SENSOR_ADDR shl 1) or 0) then
  begin
    I2C_Stop;
    ReadTemperatureSensor := Q8_8(0.0);
    Exit;
  end;
  
  // Write register address (temperature register = 0)
  I2C_WriteByte(0);
  
  // Restart for read
  I2C_Start;
  
  // Write sensor address + read bit
  I2C_WriteByte((TEMP_SENSOR_ADDR shl 1) or 1);
  
  // Read temperature (16-bit, high byte first)
  tempHigh := I2C_ReadByte(true);   // ACK
  tempLow := I2C_ReadByte(false);   // NACK
  
  I2C_Stop;
  
  // Combine bytes
  temperature := (word(tempHigh) shl 8) or tempLow;
  
  // Convert to temperature (TMP102: 12-bit, 0.0625°C per LSB)
  ReadTemperatureSensor := Q8_8(temperature) * Q8_8(0.0625);
end;
```

---

## Sensor Data Processing

### Filtering

**Smooth sensor readings:**

```pascal
type
  TSensorFilter = record
    Values: array[0..7] of word;
    Index: byte;
    Sum: word;
  end;

var
  TempFilter: TSensorFilter;

procedure InitFilter(var filter: TSensorFilter);
begin
  FillChar(filter.Values, SizeOf(filter.Values), 0);
  filter.Index := 0;
  filter.Sum := 0;
end;

function FilterReading(var filter: TSensorFilter; value: word): word;
begin
  // Remove oldest value
  filter.Sum := filter.Sum - filter.Values[filter.Index];
  
  // Add new value
  filter.Values[filter.Index] := value;
  filter.Sum := filter.Sum + value;
  
  // Move index
  filter.Index := (filter.Index + 1) mod 8;
  
  // Return average
  FilterReading := filter.Sum div 8;
end;
```

### Calibration

**Calibrate sensor readings:**

```pascal
type
  TSensorCalibration = record
    Offset: word;
    Scale: Q8.8;
  end;

var
  TempCalibration: TSensorCalibration;

procedure CalibrateSensor(var cal: TSensorCalibration; 
                          knownValue: Q8.8; rawReading: word);
var
  measured: Q8.8;
begin
  // Measure at known value
  measured := Q8_8(rawReading);
  
  // Calculate scale (if needed)
  // cal.Scale := knownValue / measured;
  
  // Calculate offset
  cal.Offset := rawReading - Round(knownValue);
end;

function ApplyCalibration(var cal: TSensorCalibration; raw: word): Q8.8;
begin
  ApplyCalibration := Q8_8(raw - cal.Offset) * cal.Scale;
end;
```

---

## Practical Examples

### Temperature Monitor

**Monitor temperature with LED:**

```pascal
program TemperatureMonitor;

const
  TEMP_SENSOR_ADDR = $48;
  LED_COLD = 0;    // Blue
  LED_NORMAL = 1;  // Green
  LED_HOT = 2;     // Red

var
  temperature: Q8.8;

function ReadTemperature: Q8.8;
begin
  // Read from I²C sensor (simplified)
  ReadTemperature := ReadTemperatureSensor;
end;

procedure UpdateLEDs(temp: Q8.8);
begin
  AllLEDsOff;
  
  if temp < Q8_8(20.0) then
    SetLED(LED_COLD, true)      // Cold: blue
  else if temp < Q8_8(30.0) then
    SetLED(LED_NORMAL, true)    // Normal: green
  else
    SetLED(LED_HOT, true);      // Hot: red
end;

begin
  InitGPIO;
  InitI2C;
  
  while true do
  begin
    temperature := ReadTemperature;
    UpdateLEDs(temperature);
    Delay(1000);  // Read every second
  end;
end.
```

### Light-Activated System

**Respond to light level:**

```pascal
program LightSensor;

const
  LIGHT_THRESHOLD = 128;  // 0-255

var
  lightLevel: byte;

function ReadLight: byte;
begin
  ReadLight := ReadAnalog(LIGHT_SENSOR_CHANNEL) div 4;  // 10-bit to 8-bit
end;

procedure HandleLightLevel(level: byte);
begin
  if level < LIGHT_THRESHOLD then
  begin
    // Dark: turn on LED
    SetLED(0, true);
  end
  else
  begin
    // Bright: turn off LED
    SetLED(0, false);
  end;
end;

begin
  InitGPIO;
  InitADC;
  
  while true do
  begin
    lightLevel := ReadLight;
    HandleLightLevel(lightLevel);
    Delay(100);
  end;
end.
```

---

## Complete Example

**Putting it all together:**

```pascal
program SensorDemo;

const
  BUTTON_PIN = 1;
  TEMP_SENSOR_ADDR = $48;
  LED_PIN = 0;

var
  buttonPressed: boolean;
  temperature: Q8.8;

procedure InitHardware;
begin
  InitGPIO;
  InitI2C;
  InitADC;
end;

function ReadButton: boolean;
var
  portValue: byte;
begin
  portValue := PortIn(GPIO_PORT);
  ReadButton := (portValue and (1 shl BUTTON_PIN)) = 0;
end;

function ReadTemperature: Q8.8;
begin
  // Read from I²C sensor
  ReadTemperature := ReadTemperatureSensor;
end;

begin
  InitHardware;
  
  while true do
  begin
    // Read button
    buttonPressed := ReadButton;
    
    if buttonPressed then
    begin
      // Read temperature when button pressed
      temperature := ReadTemperature;
      
      // Blink LED to show reading
      SetLED(LED_PIN, true);
      Delay(100);
      SetLED(LED_PIN, false);
    end;
    
    Delay(50);
  end;
end.
```

---

## Best Practices

### 1. Debounce Digital Inputs

**Handle switch bounce:**

```pascal
// ✅ GOOD: Debounce buttons
function ReadButtonDebounced: boolean;
// ... debounce logic ...

// ❌ BAD: Read directly
function ReadButton: boolean;
// May get false readings from bounce
```

### 2. Filter Analog Readings

**Smooth noisy sensors:**

```pascal
// ✅ GOOD: Filter readings
var filtered := FilterReading(TempFilter, rawValue);

// ❌ BAD: Use raw readings
var temp := rawValue;  // May be noisy
```

### 3. Calibrate Sensors

**Account for sensor errors:**

```pascal
// ✅ GOOD: Calibrate
CalibrateSensor(cal, knownTemp, rawReading);
var temp := ApplyCalibration(cal, rawReading);

// ❌ BAD: Use raw values
var temp := rawReading;  // May be inaccurate
```

### 4. Handle I²C Errors

**Check for communication errors:**

```pascal
// ✅ GOOD: Check ACK
if not I2C_WriteByte(addr) then
  WriteLn('I²C error');

// ❌ BAD: Ignore errors
I2C_WriteByte(addr);  // May fail silently
```

### 5. Add Delays

**Allow time for operations:**

```pascal
// ✅ GOOD: Appropriate delays
I2C_Start;
Delay(1);  // Allow setup time

// ❌ BAD: No delays
I2C_Start;  // May be too fast
```

---

## Exercises

### Exercise 1: Digital Sensor

Write a program that:
1. Reads digital sensor (button)
2. Debounces input
3. Responds to sensor state
4. Demonstrates digital input

### Exercise 2: Analog Sensor

Write a program that:
1. Reads analog sensor
2. Converts to meaningful value
3. Filters readings
4. Displays sensor data

### Exercise 3: I²C Sensor

Write a program that:
1. Communicates via I²C
2. Reads sensor data
3. Processes readings
4. Uses sensor information

### Exercise 4: Complete System

Write a program that:
1. Reads multiple sensors
2. Processes all data
3. Responds to sensor changes
4. Integrates sensors with program

---

**Previous Section:** [Breadboards and LEDs](./01_BreadboardsAndLEDs.md)  
**Next Section:** [Communicating with Raspberry Pi](./03_CommunicatingWithRaspberryPi.md)  
**Language Specification:** See [Memory and I/O](../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX

