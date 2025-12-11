# Chapter 26: Hardware Interfacing

**Learning Objectives:**
- Understand GPIO (General Purpose I/O) pins
- Learn to control LEDs and read sensors
- Master I²C and UART communication
- Interface with external hardware
- Connect to Raspberry Pi

**SuperPascal Features Covered:**
- I/O port access (`PortIn`, `PortOut`)
- GPIO control
- I²C communication
- UART serial communication
- Hardware abstraction

**Prerequisites:**
- Chapter 16 (Memory and I/O) - Understanding Peek/Poke and port access
- Chapter 16 (Input, State, and Interaction) - Understanding input handling
- Basic electronics knowledge (helpful but not required)

**Estimated Time:** 3-4 hours

**Chapter Size:** 3 H2 sections, ~35-40 pages total

**Code Examples:**
- LED control
- Sensor reading
- I²C communication
- UART communication
- Raspberry Pi interfacing

**Exercises:**
- Control LEDs
- Read sensors
- Communicate via I²C
- Interface with Raspberry Pi

---

## Chapter Structure

- **01_BreadboardsAndLEDs.md** - Breadboards, LEDs, GPIO control, digital output, basic circuits
- **02_ReadingSensors.md** - Sensor types, analog/digital sensors, I²C sensors, sensor data processing
- **03_CommunicatingWithRaspberryPi.md** - UART communication, protocol design, data exchange, MicroPython bridging

---

## Learning Path

- **Before:** Chapter 27 (File I/O, Save Systems, and Packaging) - Data persistence
- **After:** Chapter 27 (Advanced Topics) - Advanced algorithms and optimization
- **After:** Chapter 28 (Capstone Project Guide) - Complete project development

---

## Notes

**Hardware Interfacing Overview:**
- **GPIO pins** — General Purpose Input/Output pins
- **Digital I/O** — On/off signals (LEDs, buttons)
- **Analog I/O** — Continuous values (sensors)
- **Communication protocols** — I²C, UART, SPI

**Hardware Components:**
- **Breadboards** — Prototyping boards
- **LEDs** — Light-emitting diodes
- **Sensors** — Temperature, light, motion, etc.
- **Raspberry Pi** — External computer for advanced processing

**I/O Port Access:**
- **PortIn** — Read from I/O port
- **PortOut** — Write to I/O port
- **PortInW/PortOutW** — Word-sized I/O
- **Bit manipulation** — Set/clear individual bits

**Communication Protocols:**
- **I²C** — Inter-Integrated Circuit (2-wire)
- **UART** — Universal Asynchronous Receiver-Transmitter (serial)
- **SPI** — Serial Peripheral Interface (4+ wires)

**Educational Approach:**
- **Practical focus** — Hardware interfacing applied to real projects
- **Code examples** — Control hardware in SuperPascal
- **Safety** — Proper handling of electronics
- **Progressive complexity** — Build from simple to advanced

---

**Language Specification:** See [Memory and I/O](../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX

