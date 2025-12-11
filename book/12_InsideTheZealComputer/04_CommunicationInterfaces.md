# Communication Interfaces

**Part of:** [Chapter 12: Inside the Zeal Computer](./README.md)

---

> **For GCSE students:**  
> Communication interfaces let your computer talk to other devices. GPIO is like switches you can turn on/off, I²C is like a conversation between devices, and UART is like sending messages one letter at a time.
>
> **For A-Level students:**  
> GPIO provides direct bit-level I/O control. I²C is a serial bus protocol for connecting multiple devices with just two wires. UART enables asynchronous serial communication for data exchange. Understanding these interfaces is essential for hardware interfacing and embedded systems programming.
>
> **For University students:**  
> GPIO implements memory-mapped I/O with configurable input/output modes and interrupt capabilities. I²C uses a master-slave architecture with clock synchronization and addressing. UART provides full-duplex asynchronous communication with configurable baud rates and framing. *Detailed protocol specifications and electrical characteristics are covered in Part IV: Computers in Depth.*

---

## Introduction

The Zeal computer needs to communicate with other devices: sensors, displays, storage, other computers, etc. **Communication interfaces** provide the means for this communication. Understanding these interfaces helps you connect your Zeal computer to the outside world.

**What you'll learn:**
- What GPIO is and how to use it
- What I²C is and why it's useful
- What UART is and how it enables serial communication
- When to use each interface

---

## GPIO (General-Purpose Input/Output)

### What is GPIO?

**GPIO provides direct hardware control:**
- **General-Purpose** — Can be used for many different things
- **Input/Output** — Can read or write individual bits
- **Direct control** — Low-level hardware access
- **Flexible** — Configure each pin independently

**Think of GPIO like switches:**
- **Output pins** — Turn things on/off (LEDs, motors, relays)
- **Input pins** — Read sensor states (buttons, switches, sensors)
- **Configurable** — Each pin can be input or output

### GPIO Uses

**Common GPIO applications:**
- **LEDs** — Turn lights on/off
- **Buttons** — Read button presses
- **Sensors** — Read sensor values (temperature, light, etc.)
- **Motors** — Control motor speed/direction
- **Relays** — Switch external devices on/off

**Example:**
```pascal
// Configure pin 5 as output
GPIO_SetMode(5, GPIO_OUTPUT);

// Turn LED on
GPIO_Write(5, true);

// Turn LED off
GPIO_Write(5, false);

// Configure pin 3 as input
GPIO_SetMode(3, GPIO_INPUT);

// Read button state
if GPIO_Read(3) then
  WriteLn('Button pressed!');
```

### GPIO Characteristics

**GPIO features:**
- **Bit-level** — Control individual bits (0 or 1)
- **Fast** — Direct hardware access (very fast)
- **Simple** — Easy to use
- **Limited** — Only on/off, no complex protocols

**When to use GPIO:**
- ✅ Simple on/off control
- ✅ Reading digital sensors
- ✅ Controlling LEDs, relays
- ❌ Complex data communication
- ❌ Analog signals (need ADC)

---

## I²C (Inter-Integrated Circuit)

### What is I²C?

**I²C is a serial communication protocol:**
- **Two-wire** — Only needs 2 wires (SDA, SCL)
- **Multiple devices** — Can connect many devices on same bus
- **Master-Slave** — One master controls communication
- **Addressable** — Each device has an address

**I²C connection:**
```
Zeal Computer (Master)
    SDA ────────────────┬─────────────── Device 1 (Slave)
    SCL ────────────────┼─────────────── Device 2 (Slave)
                        └─────────────── Device 3 (Slave)
```

**Why I²C?**
- **Simple wiring** — Only 2 wires for many devices
- **Standard protocol** — Many devices support I²C
- **Addressable** — Can talk to specific devices
- **Widely used** — Common in sensors, displays, EEPROMs

### I²C Communication

**How I²C works:**
1. **Master starts** — Initiates communication
2. **Master sends address** — Identifies which device to talk to
3. **Device responds** — Slave device acknowledges
4. **Data transfer** — Master and slave exchange data
5. **Master stops** — Ends communication

**I²C data format:**
```
Start → Address → R/W → ACK → Data → ACK → ... → Stop
```

**Example:**
```pascal
// Read temperature from I²C sensor (address 0x48)
var temp: byte;
I2C_Start;
I2C_Write(0x48 shl 1);  // Device address + read bit
temp := I2C_Read;
I2C_Stop;
WriteLn('Temperature: ', temp);
```

### I²C Characteristics

**I²C features:**
- **Serial** — Sends data one bit at a time
- **Synchronous** — Uses clock signal (SCL)
- **Addressable** — Each device has unique address
- **Multi-device** — Can connect many devices

**When to use I²C:**
- ✅ Connecting multiple sensors
- ✅ Simple data exchange
- ✅ Standard devices (displays, EEPROMs)
- ❌ High-speed communication
- ❌ Long distances

---

## UART (Universal Asynchronous Receiver-Transmitter)

### What is UART?

**UART enables serial communication:**
- **Asynchronous** — No shared clock signal
- **Full-duplex** — Can send and receive simultaneously
- **Byte-by-byte** — Sends one byte at a time
- **Configurable** — Baud rate, data bits, parity

**UART connection:**
```
Zeal Computer          Other Device
    TX ───────────────> RX
    RX <─────────────── TX
   GND ──────────────── GND
```

**Why UART?**
- **Simple** — Easy to implement
- **Universal** — Works with many devices
- **Reliable** — Error detection (parity)
- **Common** — Used in many systems

### UART Communication

**How UART works:**
1. **Idle state** — Line is high (1)
2. **Start bit** — Line goes low (0) to signal start
3. **Data bits** — 5-9 bits of data (LSB first)
4. **Parity bit** — Optional error detection
5. **Stop bit** — Line goes high (1) to signal end

**UART data format:**
```
Idle → Start → D0 → D1 → D2 → D3 → D4 → D5 → D6 → D7 → Stop → Idle
 1      0      1    0    1    1    0    0    1    1     1      1
```

**Example:**
```pascal
// Initialize UART at 9600 baud
UART_Init(9600);

// Send message
UART_SendString('Hello, World!');

// Receive byte
var data: byte;
data := UART_Receive;
WriteLn('Received: ', data);
```

### UART Characteristics

**UART features:**
- **Asynchronous** — No clock signal needed
- **Full-duplex** — Send and receive at same time
- **Configurable** — Baud rate, data bits, parity
- **Simple** — Easy to use

**When to use UART:**
- ✅ Communication with computers
- ✅ Debugging (serial console)
- ✅ Simple data exchange
- ✅ Long distances (with proper hardware)
- ❌ High-speed communication
- ❌ Multiple devices (use I²C instead)

---

## Comparing Interfaces

### When to Use Each

**GPIO:**
- ✅ Simple on/off control
- ✅ Reading digital inputs
- ✅ Controlling LEDs, relays
- ❌ Complex data communication

**I²C:**
- ✅ Multiple devices on same bus
- ✅ Sensors, displays, EEPROMs
- ✅ Standard devices
- ❌ High-speed communication
- ❌ Long distances

**UART:**
- ✅ Computer communication
- ✅ Debugging, serial console
- ✅ Simple data exchange
- ✅ Long distances
- ❌ Multiple devices (need separate UARTs)

### Interface Summary

| Interface | Wires | Speed | Devices | Use Case |
|-----------|-------|-------|---------|----------|
| **GPIO** | 1+ | Very Fast | 1 per pin | Simple I/O |
| **I²C** | 2 | Medium | Many | Sensors, displays |
| **UART** | 2-3 | Medium | 1 per UART | Computer communication |

---

## Summary

**Key Concepts:**
- **GPIO** — Direct bit-level I/O control (switches, LEDs, sensors)
- **I²C** — Serial bus for multiple devices (sensors, displays)
- **UART** — Serial communication for data exchange (computers, debugging)
- **Each interface** has different strengths and use cases
- **Understanding interfaces** helps you connect Zeal to other devices

**What's Next:**
- Learn how **programs run on Zeal** in the next chapter
- Understand the **compilation and execution pipeline**
- See how all components work together to run your programs

**For Deeper Study:**
- Part VI: Hardware Interfacing covers GPIO, I²C, UART programming in detail
- Part IV: Computers in Depth covers protocol specifications and electrical characteristics

---

**Next Chapter:** [Chapter 13: How Programs Run on Zeal](../13_HowProgramsRunOnZeal/README.md)

