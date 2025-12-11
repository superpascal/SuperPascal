# Breadboards and LEDs

**Part of:** [Chapter 26: Hardware Interfacing](./README.md)

---

## Introduction

Breadboards and LEDs are the foundation of hardware interfacing. This section teaches you how to use breadboards, control LEDs, and work with GPIO pins.

**Key concepts:**
- **Breadboards** — Prototyping boards
- **LEDs** — Light-emitting diodes
- **GPIO pins** — General Purpose I/O
- **Digital output** — Controlling on/off states

---

## Understanding Breadboards

### What is a Breadboard?

**A breadboard is a prototyping board:**

- **No soldering** — Components plug in
- **Reusable** — Easy to change circuits
- **Connected rows** — Horizontal rows are connected
- **Power rails** — Vertical columns for power/ground

**Breadboard layout:**
```
Power rails (vertical):
  + + + + +     - - - - -
  + + + + +     - - - - -
  
Component area (horizontal rows):
  a b c d e     f g h i j
  a b c d e     f g h i j
```

**Connections:**
- Same letter row (a-a, b-b) = connected horizontally
- Power rails = connected vertically
- Middle gap = not connected

### Basic Circuit

**Simple LED circuit:**

```
LED Circuit:
  GPIO Pin → Resistor (220Ω) → LED → Ground
```

**Components:**
- **GPIO pin** — Output pin from computer
- **Resistor** — Limits current (protects LED)
- **LED** — Light-emitting diode
- **Ground** — Common ground connection

---

## GPIO Control

### Understanding GPIO

**GPIO (General Purpose I/O) pins:**

- **Output mode** — Drive signals (control LEDs)
- **Input mode** — Read signals (read buttons)
- **High/Low** — 1 (high, 3.3V/5V) or 0 (low, 0V)
- **Bit manipulation** — Control individual pins

### Setting GPIO Pins

**Control GPIO via I/O ports:**

```pascal
const
  GPIO_PORT = $D0;  // PIO Port A (example)
  GPIO_PIN_LED = 0;  // Pin 0 for LED

procedure SetGPIO(pin: byte; value: boolean);
var
  portValue: byte;
begin
  // Read current port value
  portValue := PortIn(GPIO_PORT);
  
  if value then
    // Set bit (turn on)
    portValue := portValue or (1 shl pin)
  else
    // Clear bit (turn off)
    portValue := portValue and not (1 shl pin);
  
  // Write back to port
  PortOut(GPIO_PORT, portValue);
end;
```

### GPIO Initialization

**Configure GPIO pins:**

```pascal
procedure InitGPIO;
begin
  // Configure PIO Port A as output
  PortOut(IO_PIO_CTRL_A, $CF);  // Set bit mode
  PortOut(IO_PIO_CTRL_A, $00);  // All pins output (0 = output)
  PortOut(IO_PIO_DATA_A, $00);  // All pins low initially
end;
```

---

## Controlling LEDs

### Basic LED Control

**Turn LED on/off:**

```pascal
const
  LED_PIN = 0;  // GPIO pin 0

procedure LEDOn;
begin
  SetGPIO(LED_PIN, true);
end;

procedure LEDOff;
begin
  SetGPIO(LED_PIN, false);
end;

procedure LEDToggle;
var
  current: boolean;
begin
  current := (PortIn(GPIO_PORT) and (1 shl LED_PIN)) <> 0;
  SetGPIO(LED_PIN, not current);
end;
```

### Multiple LEDs

**Control multiple LEDs:**

```pascal
const
  LED_RED = 0;
  LED_GREEN = 1;
  LED_BLUE = 2;

procedure SetLED(led: byte; on: boolean);
begin
  SetGPIO(led, on);
end;

procedure AllLEDsOff;
begin
  PortOut(GPIO_PORT, $00);  // All pins low
end;

procedure AllLEDsOn;
begin
  PortOut(GPIO_PORT, $07);  // Pins 0, 1, 2 high (binary: 00000111)
end;
```

### LED Patterns

**Create LED patterns:**

```pascal
procedure LEDPattern;
var
  pattern: byte;
  i: byte;
begin
  // Pattern: 00000001, 00000010, 00000100, etc.
  pattern := 1;
  
  for i := 0 to 7 do
  begin
    PortOut(GPIO_PORT, pattern);
    pattern := pattern shl 1;  // Shift left
    Delay(100);  // Wait 100ms
  end;
end;

procedure LEDKnightRider;
var
  i: byte;
  direction: integer;
  position: byte;
begin
  position := 0;
  direction := 1;
  
  while true do
  begin
    // Turn on LED at position
    PortOut(GPIO_PORT, 1 shl position);
    Delay(50);
    
    // Move position
    position := position + direction;
    
    // Reverse direction at ends
    if position = 7 then
      direction := -1
    else if position = 0 then
      direction := 1;
  end;
end;
```

---

## Practical Examples

### Blinking LED

**Simple blink program:**

```pascal
program BlinkLED;

const
  LED_PIN = 0;
  GPIO_PORT = $D0;

procedure InitGPIO;
begin
  PortOut(IO_PIO_CTRL_A, $CF);  // Bit mode
  PortOut(IO_PIO_CTRL_A, $00);  // All output
  PortOut(GPIO_PORT, $00);       // All low
end;

procedure SetLED(on: boolean);
var
  value: byte;
begin
  value := PortIn(GPIO_PORT);
  if on then
    value := value or (1 shl LED_PIN)
  else
    value := value and not (1 shl LED_PIN);
  PortOut(GPIO_PORT, value);
end;

begin
  InitGPIO;
  
  while true do
  begin
    SetLED(true);   // LED on
    Delay(500);     // Wait 500ms
    SetLED(false);  // LED off
    Delay(500);     // Wait 500ms
  end;
end.
```

### LED Binary Counter

**Count in binary with LEDs:**

```pascal
program BinaryCounter;

const
  GPIO_PORT = $D0;
  LED_COUNT = 4;

var
  counter: byte;

procedure InitGPIO;
begin
  PortOut(IO_PIO_CTRL_A, $CF);
  PortOut(IO_PIO_CTRL_A, $00);
  PortOut(GPIO_PORT, $00);
end;

procedure DisplayBinary(value: byte);
begin
  // Display value as binary on LEDs
  PortOut(GPIO_PORT, value and $0F);  // Lower 4 bits
end;

begin
  InitGPIO;
  counter := 0;
  
  while true do
  begin
    DisplayBinary(counter);
    counter := counter + 1;
    if counter > 15 then
      counter := 0;
    Delay(500);
  end;
end.
```

### LED Status Indicator

**Use LEDs as status indicators:**

```pascal
type
  TStatusLED = (ledReady, ledError, ledWorking);

procedure SetStatusLED(status: TStatusLED; on: boolean);
const
  LED_PINS: array[TStatusLED] of byte = (0, 1, 2);
begin
  SetGPIO(LED_PINS[status], on);
end;

procedure ShowStatus(status: TStatusLED);
begin
  // Turn off all
  AllLEDsOff;
  
  // Turn on status LED
  SetStatusLED(status, true);
end;

// Usage
begin
  ShowStatus(ledReady);   // Green LED on
  // ... do work ...
  ShowStatus(ledWorking); // Yellow LED on
  // ... work done ...
  ShowStatus(ledReady);   // Green LED on
end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program LEDDemo;

const
  GPIO_PORT = $D0;
  LED_RED = 0;
  LED_GREEN = 1;
  LED_BLUE = 2;

type
  TLEDState = (lsOff, lsRed, lsGreen, lsBlue, lsAll);

var
  CurrentState: TLEDState;

procedure InitGPIO;
begin
  PortOut(IO_PIO_CTRL_A, $CF);
  PortOut(IO_PIO_CTRL_A, $00);
  PortOut(GPIO_PORT, $00);
end;

procedure SetLEDState(state: TLEDState);
begin
  case state of
    lsOff: PortOut(GPIO_PORT, $00);
    lsRed: PortOut(GPIO_PORT, 1 shl LED_RED);
    lsGreen: PortOut(GPIO_PORT, 1 shl LED_GREEN);
    lsBlue: PortOut(GPIO_PORT, 1 shl LED_BLUE);
    lsAll: PortOut(GPIO_PORT, $07);
  end;
end;

procedure CycleLEDs;
begin
  case CurrentState of
    lsOff: CurrentState := lsRed;
    lsRed: CurrentState := lsGreen;
    lsGreen: CurrentState := lsBlue;
    lsBlue: CurrentState := lsAll;
    lsAll: CurrentState := lsOff;
  end;
  
  SetLEDState(CurrentState);
end;

begin
  InitGPIO;
  CurrentState := lsOff;
  
  while true do
  begin
    CycleLEDs;
    Delay(1000);  // 1 second per state
  end;
end.
```

---

## Best Practices

### 1. Always Use Resistors

**Protect LEDs and GPIO pins:**

```pascal
// ✅ GOOD: Use resistor (220Ω typical)
// GPIO → Resistor → LED → Ground

// ❌ BAD: No resistor
// GPIO → LED → Ground (may damage LED or pin)
```

### 2. Initialize GPIO First

**Configure pins before use:**

```pascal
// ✅ GOOD: Initialize first
InitGPIO;
SetLED(true);

// ❌ BAD: Use before init
SetLED(true);  // Pin may not be configured
```

### 3. Use Constants for Pins

**Make pin assignments clear:**

```pascal
// ✅ GOOD: Named constants
const
  LED_PIN = 0;
  BUTTON_PIN = 1;

// ❌ BAD: Magic numbers
SetGPIO(0, true);  // What is pin 0?
```

### 4. Handle GPIO Safely

**Check pin state before changing:**

```pascal
// ✅ GOOD: Read-modify-write
var value := PortIn(GPIO_PORT);
value := value or (1 shl pin);
PortOut(GPIO_PORT, value);

// ❌ BAD: Overwrite all pins
PortOut(GPIO_PORT, 1 shl pin);  // Clears other pins!
```

### 5. Add Delays for Visibility

**Make LED changes visible:**

```pascal
// ✅ GOOD: Visible delay
SetLED(true);
Delay(500);  // Visible blink

// ❌ BAD: Too fast
SetLED(true);
SetLED(false);  // Too fast to see
```

---

## Exercises

### Exercise 1: Basic LED

Write a program that:
1. Initializes GPIO
2. Controls single LED
3. Blinks LED on/off
4. Demonstrates basic control

### Exercise 2: Multiple LEDs

Write a program that:
1. Controls multiple LEDs
2. Creates LED patterns
3. Cycles through patterns
4. Demonstrates bit manipulation

### Exercise 3: LED Patterns

Write a program that:
1. Creates various LED patterns
2. Implements knight rider effect
3. Creates binary counter
4. Demonstrates creative patterns

### Exercise 4: Status Indicators

Write a program that:
1. Uses LEDs as status indicators
2. Shows different states
3. Responds to program events
4. Demonstrates practical use

---

**Previous Chapter:** [Chapter 25: File I/O, Save Systems, and Packaging](../25_FileIOSaveSystemsAndPackaging/README.md)  
**Next Section:** [Reading Sensors](./02_ReadingSensors.md)  
**Language Specification:** See [Memory and I/O](../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX

