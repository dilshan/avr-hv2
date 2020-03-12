AVR High Voltage Programmer 2
==========================

![AVR-HV2 Prototype with Arduino MEGA board](https://raw.githubusercontent.com/dilshan/avr-hv2/master/resources/hv2-mid-08032020B.png)

*AVR-HV2* is *Arduino* based high voltage parallel programmer for *[AVR](https://www.microchip.com/design-centers/8-bit/avr-mcus)* microcontrollers. This programmer can read, write, and erase both flash memory and EEPROM. Also, this can use to set fuse bits of *AVR* MCUs. 

*AVR-HV2* programmer is designed as an *[Arduino Mega](https://www.arduino.cc/en/Main/ArduinoBoardMega2560)* shield. Dimensions of the *AVR-HV2* are similar to the *Arduino Mega* board. It can be powered using a power source connected to the *Arduino Mega* board. The suggested power source for this programmer is a 12V 1A DC power adapter. 

The control software of this programmer is design to work on both *Windows* and *Linux* operating systems. It supports the import and export of memory data in the Intel hex file format. The communication link between the programmer and the control software is established through the USB port. 

The current firmware and control software of this programmer supports most of the generally available *AVR* MCUs which including *ATmega8*, *ATmega328*, *ATmega32*, *ATmega16*, etc. The *AVR* MCUs with extended addressing modes are currently not supported. 

The PCB board used in this project is sponsored by the *[PCBWay](https://www.pcbway.com/)*. PCB of this project can directly order from *PCBWay* through this link.

This is an open-source hardware project. All the design files, firmware, [documentation](https://github.com/dilshan/avr-hv2/wiki) and control software source codes are available at this repository.

This is an open-source hardware project. All the design files, documentation, firmware source code are available to [download](https://github.com/dilshan/avr-hv2/releases) at the project source repository. All the content of this project are distributed under the terms of the following license:

- Hardware License: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
- Software License: [MIT](https://github.com/dilshan/avr-hv2/blob/master/LICENSE)
- Documentation License: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
