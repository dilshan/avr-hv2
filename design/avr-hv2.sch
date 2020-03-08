EESchema Schematic File Version 4
LIBS:avr-hv2-cache
EELAYER 29 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title "AVR High Voltage Programmer 2"
Date "2020-03-08"
Rev "1.0.0"
Comp "Dilshan R Jayakody"
Comment1 "http://jayakody2000lk.blogspot.com"
Comment2 "https://github.com/dilshan/avr-hv2"
Comment3 "jayakody2000lk@gmail.com"
Comment4 ""
$EndDescr
$Comp
L arduino:Arduino_Mega2560_Shield XA1
U 1 1 5DE3CBBE
P 2300 3300
F 0 "XA1" H 2300 919 60  0000 C CNN
F 1 "Arduino Mega2560" H 2300 813 60  0000 C CNN
F 2 "avr-hv2:MEGA2560" H 3000 6050 60  0001 C CNN
F 3 "https://store.arduino.cc/arduino-mega-2560-rev3" H 3000 6050 60  0001 C CNN
	1    2300 3300
	1    0    0    -1  
$EndComp
Text GLabel 850  4950 0    50   Input ~ 0
GND
Text GLabel 850  5350 0    50   Input ~ 0
5V
Text GLabel 850  5450 0    50   Input ~ 0
12V
Wire Wire Line
	850  5350 900  5350
Wire Wire Line
	1000 5450 850  5450
Text GLabel 3800 2450 2    50   Input ~ 0
12V-CNT
Text GLabel 3800 2350 2    50   Input ~ 0
VCC-CNT
Text GLabel 3800 2550 2    50   Input ~ 0
RDY
Text GLabel 3800 2650 2    50   Input ~ 0
OE
Text GLabel 3800 2750 2    50   Input ~ 0
WR
Text GLabel 3800 2850 2    50   Input ~ 0
BS1
Text GLabel 3800 2950 2    50   Input ~ 0
BS2
Text GLabel 3800 3050 2    50   Input ~ 0
XTAL1
Text GLabel 3800 3150 2    50   Input ~ 0
XA0
Text GLabel 3800 3250 2    50   Input ~ 0
XA1
Text GLabel 3800 3350 2    50   Input ~ 0
PAGEL
Wire Wire Line
	3600 2350 3800 2350
Wire Wire Line
	3800 2450 3600 2450
Wire Wire Line
	3600 2550 3800 2550
Wire Wire Line
	3800 2650 3600 2650
Wire Wire Line
	3600 2750 3800 2750
Wire Wire Line
	3800 2850 3600 2850
Wire Wire Line
	3600 2950 3800 2950
Wire Wire Line
	3800 3050 3600 3050
Wire Wire Line
	3600 3150 3800 3150
Wire Wire Line
	3800 3250 3600 3250
Wire Wire Line
	3600 3350 3800 3350
Text GLabel 3800 1150 2    50   Input ~ 0
D0
Text GLabel 3800 1250 2    50   Input ~ 0
D1
Text GLabel 3800 1350 2    50   Input ~ 0
D2
Text GLabel 3800 1450 2    50   Input ~ 0
D3
Text GLabel 3800 1550 2    50   Input ~ 0
D4
Text GLabel 3800 1650 2    50   Input ~ 0
D5
Text GLabel 3800 1750 2    50   Input ~ 0
D6
Text GLabel 3800 1850 2    50   Input ~ 0
D7
Wire Wire Line
	3600 1150 3800 1150
Wire Wire Line
	3800 1250 3600 1250
Wire Wire Line
	3600 1350 3800 1350
Wire Wire Line
	3800 1450 3600 1450
Wire Wire Line
	3600 1550 3800 1550
Wire Wire Line
	3800 1650 3600 1650
Wire Wire Line
	3600 1750 3800 1750
Wire Wire Line
	3800 1850 3600 1850
NoConn ~ 3600 1950
NoConn ~ 3600 2050
NoConn ~ 3600 2150
NoConn ~ 3600 2250
NoConn ~ 3600 3450
NoConn ~ 3600 3550
NoConn ~ 3600 3650
NoConn ~ 3600 3750
NoConn ~ 3600 3850
NoConn ~ 3600 3950
NoConn ~ 3600 4050
NoConn ~ 3600 4150
NoConn ~ 3600 4250
NoConn ~ 3600 4350
NoConn ~ 3600 4450
NoConn ~ 3600 4550
NoConn ~ 3600 4650
NoConn ~ 3600 4750
NoConn ~ 3600 4850
NoConn ~ 3600 4950
NoConn ~ 3600 5050
NoConn ~ 3600 5150
NoConn ~ 3600 5250
NoConn ~ 3600 5350
NoConn ~ 3600 5450
Text GLabel 850  4650 0    50   Input ~ 0
GND
NoConn ~ 2050 700 
NoConn ~ 2150 700 
NoConn ~ 2250 700 
NoConn ~ 2350 700 
NoConn ~ 2450 700 
NoConn ~ 2550 700 
NoConn ~ 1000 1150
NoConn ~ 1000 1250
NoConn ~ 1000 1350
NoConn ~ 1000 1450
NoConn ~ 1000 1550
NoConn ~ 1000 1650
NoConn ~ 1000 1750
NoConn ~ 1000 1850
NoConn ~ 1000 1950
NoConn ~ 1000 2050
NoConn ~ 1000 2150
NoConn ~ 1000 2250
NoConn ~ 1000 2450
NoConn ~ 1000 2550
NoConn ~ 1000 2650
NoConn ~ 1000 2750
NoConn ~ 1000 2850
NoConn ~ 1000 2950
NoConn ~ 1000 3050
NoConn ~ 1000 3150
NoConn ~ 1000 3250
NoConn ~ 1000 3350
NoConn ~ 1000 3450
NoConn ~ 1000 3550
NoConn ~ 1000 3650
NoConn ~ 1000 3750
NoConn ~ 1000 3850
NoConn ~ 1000 3950
NoConn ~ 1000 4050
NoConn ~ 1000 4250
NoConn ~ 1000 4350
NoConn ~ 1000 5050
Text GLabel 950  7150 0    50   Input ~ 0
GND
$Comp
L power:GND #PWR0101
U 1 1 5DE74D81
P 1050 7300
F 0 "#PWR0101" H 1050 7050 50  0001 C CNN
F 1 "GND" H 1055 7127 50  0000 C CNN
F 2 "" H 1050 7300 50  0001 C CNN
F 3 "" H 1050 7300 50  0001 C CNN
	1    1050 7300
	1    0    0    -1  
$EndComp
Wire Wire Line
	950  7150 1050 7150
Wire Wire Line
	1050 7150 1050 7300
Text GLabel 950  6950 0    50   Input ~ 0
12V
$Comp
L power:+12V #PWR0102
U 1 1 5DE759B3
P 1050 6750
F 0 "#PWR0102" H 1050 6600 50  0001 C CNN
F 1 "+12V" H 1065 6923 50  0000 C CNN
F 2 "" H 1050 6750 50  0001 C CNN
F 3 "" H 1050 6750 50  0001 C CNN
	1    1050 6750
	1    0    0    -1  
$EndComp
Wire Wire Line
	950  6950 1050 6950
Wire Wire Line
	1050 6950 1050 6750
Text GLabel 1650 6950 0    50   Input ~ 0
5V
$Comp
L power:+5V #PWR0103
U 1 1 5DE77212
P 1750 6750
F 0 "#PWR0103" H 1750 6600 50  0001 C CNN
F 1 "+5V" H 1765 6923 50  0000 C CNN
F 2 "" H 1750 6750 50  0001 C CNN
F 3 "" H 1750 6750 50  0001 C CNN
	1    1750 6750
	1    0    0    -1  
$EndComp
Wire Wire Line
	1650 6950 1750 6950
Wire Wire Line
	1750 6950 1750 6750
$Comp
L Device:Q_NPN_ECB Q1
U 1 1 5DE7CECC
P 9500 4450
F 0 "Q1" H 9690 4496 50  0000 L CNN
F 1 "2SC945" H 9690 4405 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_HandSolder" H 9700 4550 50  0001 C CNN
F 3 "~" H 9500 4450 50  0001 C CNN
	1    9500 4450
	1    0    0    -1  
$EndComp
$Comp
L Device:R R1
U 1 1 5DE7DC47
P 9150 4450
F 0 "R1" V 8943 4450 50  0000 C CNN
F 1 "10K" V 9034 4450 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0204_L3.6mm_D1.6mm_P7.62mm_Horizontal" V 9080 4450 50  0001 C CNN
F 3 "~" H 9150 4450 50  0001 C CNN
	1    9150 4450
	0    1    1    0   
$EndComp
$Comp
L power:GND #PWR0104
U 1 1 5DE7FCAD
P 9600 4850
F 0 "#PWR0104" H 9600 4600 50  0001 C CNN
F 1 "GND" H 9605 4677 50  0000 C CNN
F 2 "" H 9600 4850 50  0001 C CNN
F 3 "" H 9600 4850 50  0001 C CNN
	1    9600 4850
	1    0    0    -1  
$EndComp
$Comp
L Device:R R2
U 1 1 5DE8196D
P 9850 4150
F 0 "R2" V 9643 4150 50  0000 C CNN
F 1 "220R" V 9734 4150 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0204_L3.6mm_D1.6mm_P7.62mm_Horizontal" V 9780 4150 50  0001 C CNN
F 3 "~" H 9850 4150 50  0001 C CNN
	1    9850 4150
	0    1    1    0   
$EndComp
Wire Wire Line
	9600 4150 9600 4250
$Comp
L Device:Q_PNP_ECB Q2
U 1 1 5DE82DB8
P 10200 4150
F 0 "Q2" H 10391 4196 50  0000 L CNN
F 1 "2SA733" H 10391 4105 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_HandSolder" H 10400 4250 50  0001 C CNN
F 3 "~" H 10200 4150 50  0001 C CNN
	1    10200 4150
	1    0    0    -1  
$EndComp
Text GLabel 10500 4500 2    50   Input ~ 0
5V
Wire Wire Line
	10500 4500 10300 4500
Wire Wire Line
	10300 4500 10300 4350
Text GLabel 10500 3750 2    50   Input ~ 0
VCC
Wire Wire Line
	10500 3750 10300 3750
Wire Wire Line
	10300 3750 10300 3950
Text GLabel 8950 4450 0    50   Input ~ 0
VCC-CNT
$Comp
L Device:Q_NPN_ECB Q3
U 1 1 5DE8BB53
P 9500 5800
F 0 "Q3" H 9690 5846 50  0000 L CNN
F 1 "2SC945" H 9690 5755 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_HandSolder" H 9700 5900 50  0001 C CNN
F 3 "~" H 9500 5800 50  0001 C CNN
	1    9500 5800
	1    0    0    -1  
$EndComp
$Comp
L Device:R R3
U 1 1 5DE8BB5D
P 9150 5800
F 0 "R3" V 8943 5800 50  0000 C CNN
F 1 "10K" V 9034 5800 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0204_L3.6mm_D1.6mm_P7.62mm_Horizontal" V 9080 5800 50  0001 C CNN
F 3 "~" H 9150 5800 50  0001 C CNN
	1    9150 5800
	0    1    1    0   
$EndComp
$Comp
L power:GND #PWR0105
U 1 1 5DE8BB68
P 9600 6200
F 0 "#PWR0105" H 9600 5950 50  0001 C CNN
F 1 "GND" H 9605 6027 50  0000 C CNN
F 2 "" H 9600 6200 50  0001 C CNN
F 3 "" H 9600 6200 50  0001 C CNN
	1    9600 6200
	1    0    0    -1  
$EndComp
Wire Wire Line
	9600 6200 9600 6000
$Comp
L Device:R R4
U 1 1 5DE8BB73
P 9850 5500
F 0 "R4" V 9643 5500 50  0000 C CNN
F 1 "220R" V 9734 5500 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0204_L3.6mm_D1.6mm_P7.62mm_Horizontal" V 9780 5500 50  0001 C CNN
F 3 "~" H 9850 5500 50  0001 C CNN
	1    9850 5500
	0    1    1    0   
$EndComp
Wire Wire Line
	9600 5500 9600 5600
$Comp
L Device:Q_PNP_ECB Q4
U 1 1 5DE8BB7F
P 10200 5500
F 0 "Q4" H 10391 5546 50  0000 L CNN
F 1 "2SA733" H 10391 5455 50  0000 L CNN
F 2 "Package_TO_SOT_THT:TO-92_HandSolder" H 10400 5600 50  0001 C CNN
F 3 "~" H 10200 5500 50  0001 C CNN
	1    10200 5500
	1    0    0    -1  
$EndComp
Wire Wire Line
	10500 5850 10300 5850
Wire Wire Line
	10300 5850 10300 5700
Wire Wire Line
	10500 5100 10300 5100
Wire Wire Line
	10300 5100 10300 5300
Text GLabel 8950 5800 0    50   Input ~ 0
12V-CNT
Text GLabel 10500 5850 2    50   Input ~ 0
12V
Text GLabel 10500 5100 2    50   Input ~ 0
RESET
Text GLabel 2550 6800 0    50   Input ~ 0
RESET
$Comp
L Device:R R5
U 1 1 5DEA03A6
P 2900 6800
F 0 "R5" V 2693 6800 50  0000 C CNN
F 1 "4.7K" V 2784 6800 50  0000 C CNN
F 2 "Resistor_THT:R_Axial_DIN0204_L3.6mm_D1.6mm_P7.62mm_Horizontal" V 2830 6800 50  0001 C CNN
F 3 "~" H 2900 6800 50  0001 C CNN
	1    2900 6800
	0    1    1    0   
$EndComp
$Comp
L Device:LED D1
U 1 1 5DEA0CE1
P 3400 6800
F 0 "D1" H 3393 6545 50  0000 C CNN
F 1 "LED (RED)" H 3393 6636 50  0000 C CNN
F 2 "LED_THT:LED_D3.0mm" H 3400 6800 50  0001 C CNN
F 3 "~" H 3400 6800 50  0001 C CNN
	1    3400 6800
	-1   0    0    1   
$EndComp
Wire Wire Line
	2550 6800 2750 6800
Wire Wire Line
	3050 6800 3250 6800
$Comp
L power:GND #PWR0106
U 1 1 5DEA64F0
P 3700 6900
F 0 "#PWR0106" H 3700 6650 50  0001 C CNN
F 1 "GND" H 3705 6727 50  0000 C CNN
F 2 "" H 3700 6900 50  0001 C CNN
F 3 "" H 3700 6900 50  0001 C CNN
	1    3700 6900
	1    0    0    -1  
$EndComp
Wire Wire Line
	3550 6800 3700 6800
Wire Wire Line
	3700 6800 3700 6900
$Comp
L MCU_Microchip_ATmega:ATmega328-PU ZIF1
U 1 1 5DEC0DE1
P 8000 2400
F 0 "ZIF1" H 7356 2446 50  0000 R CNN
F 1 "MCU-28" H 7356 2355 50  0000 R CNN
F 2 "Socket:DIP_Socket-28_W6.9_W7.62_W10.16_W12.7_W13.5_3M_228-4817-00-0602J" H 8000 2400 50  0001 C CIN
F 3 "http://ww1.microchip.com/downloads/en/DeviceDoc/ATmega328_P%20AVR%20MCU%20with%20picoPower%20Technology%20Data%20Sheet%2040001984A.pdf" H 8000 2400 50  0001 C CNN
	1    8000 2400
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0107
U 1 1 5DEC4A8D
P 8000 4050
F 0 "#PWR0107" H 8000 3800 50  0001 C CNN
F 1 "GND" H 8005 3877 50  0000 C CNN
F 2 "" H 8000 4050 50  0001 C CNN
F 3 "" H 8000 4050 50  0001 C CNN
	1    8000 4050
	1    0    0    -1  
$EndComp
Wire Wire Line
	8000 4050 8000 3900
Text GLabel 7850 700  0    50   Input ~ 0
VCC
Wire Wire Line
	7850 700  8000 700 
Wire Wire Line
	8100 700  8100 900 
Wire Wire Line
	8000 900  8000 700 
Connection ~ 8000 700 
Wire Wire Line
	8000 700  8100 700 
Text GLabel 8800 1200 2    50   Input ~ 0
D0
Text GLabel 8800 1300 2    50   Input ~ 0
D1
Text GLabel 8800 1400 2    50   Input ~ 0
D2
Text GLabel 8800 1500 2    50   Input ~ 0
D3
Text GLabel 8800 1600 2    50   Input ~ 0
D4
Text GLabel 8800 1700 2    50   Input ~ 0
D5
Wire Wire Line
	8600 1200 8800 1200
Wire Wire Line
	8800 1300 8600 1300
Wire Wire Line
	8600 1400 8800 1400
Wire Wire Line
	8800 1500 8600 1500
Wire Wire Line
	8600 1600 8800 1600
Wire Wire Line
	8800 1700 8600 1700
NoConn ~ 7400 1200
Text GLabel 8800 3000 2    50   Input ~ 0
RDY
Text GLabel 8800 3100 2    50   Input ~ 0
OE
Text GLabel 8800 3200 2    50   Input ~ 0
WR
Text GLabel 8800 3300 2    50   Input ~ 0
BS1
Text GLabel 8800 1800 2    50   Input ~ 0
XTAL1
Text GLabel 8800 3400 2    50   Input ~ 0
XA0
Text GLabel 8800 3500 2    50   Input ~ 0
XA1
Text GLabel 8800 3600 2    50   Input ~ 0
PAGEL
Wire Wire Line
	8600 3000 8800 3000
Wire Wire Line
	8800 3100 8600 3100
Wire Wire Line
	8600 3200 8800 3200
Wire Wire Line
	8800 3300 8600 3300
Wire Wire Line
	8800 1800 8600 1800
Wire Wire Line
	8600 3400 8800 3400
Wire Wire Line
	8800 3500 8600 3500
Wire Wire Line
	8600 3600 8800 3600
NoConn ~ 8600 1900
NoConn ~ 8600 2400
NoConn ~ 8600 2500
NoConn ~ 8600 2600
NoConn ~ 8600 2900
$Comp
L Connector_Generic:Conn_01x08 J1
U 1 1 5DE3CB10
P 10150 1400
F 0 "J1" H 10068 1917 50  0000 C CNN
F 1 "PROG-DATA1" H 10068 1826 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_1x08_P2.54mm_Vertical" H 10150 1400 50  0001 C CNN
F 3 "~" H 10150 1400 50  0001 C CNN
	1    10150 1400
	-1   0    0    -1  
$EndComp
Text GLabel 10550 1100 2    50   Input ~ 0
D0
Text GLabel 10550 1200 2    50   Input ~ 0
D1
Text GLabel 10550 1300 2    50   Input ~ 0
D2
Text GLabel 10550 1400 2    50   Input ~ 0
D3
Text GLabel 10550 1500 2    50   Input ~ 0
D4
Text GLabel 10550 1600 2    50   Input ~ 0
D5
Text GLabel 10550 1700 2    50   Input ~ 0
D6
Text GLabel 10550 1800 2    50   Input ~ 0
D7
Wire Wire Line
	10350 1100 10550 1100
Wire Wire Line
	10550 1200 10350 1200
Wire Wire Line
	10350 1300 10550 1300
Wire Wire Line
	10550 1400 10350 1400
Wire Wire Line
	10350 1500 10550 1500
Wire Wire Line
	10550 1600 10350 1600
Wire Wire Line
	10350 1700 10550 1700
Wire Wire Line
	10550 1800 10350 1800
$Comp
L Connector_Generic:Conn_02x06_Odd_Even J2
U 1 1 5DE6B996
P 5600 6500
F 0 "J2" H 5650 6917 50  0000 C CNN
F 1 "PROG-DATA2" H 5650 6826 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_2x06_P2.54mm_Vertical" H 5600 6500 50  0001 C CNN
F 3 "~" H 5600 6500 50  0001 C CNN
	1    5600 6500
	1    0    0    -1  
$EndComp
Text GLabel 5200 6300 0    50   Input ~ 0
VCC
Text GLabel 8800 2700 2    50   Input ~ 0
RESET
Wire Wire Line
	8600 2700 8800 2700
Wire Wire Line
	5400 6300 5200 6300
Text GLabel 6100 6400 2    50   Input ~ 0
RESET
Wire Wire Line
	6100 6400 5900 6400
Text GLabel 6100 6300 2    50   Input ~ 0
GND
Wire Wire Line
	5900 6300 6100 6300
Text GLabel 5200 6400 0    50   Input ~ 0
RDY
Text GLabel 5200 6500 0    50   Input ~ 0
OE
Text GLabel 5200 6600 0    50   Input ~ 0
WR
Text GLabel 5200 6700 0    50   Input ~ 0
BS1
Text GLabel 5200 6800 0    50   Input ~ 0
BS2
Text GLabel 6100 6500 2    50   Input ~ 0
XTAL1
Text GLabel 6100 6600 2    50   Input ~ 0
XA0
Text GLabel 6100 6700 2    50   Input ~ 0
XA1
Text GLabel 6100 6800 2    50   Input ~ 0
PAGEL
Wire Wire Line
	5400 6400 5200 6400
Wire Wire Line
	5200 6500 5400 6500
Wire Wire Line
	5400 6600 5200 6600
Wire Wire Line
	5200 6700 5400 6700
Wire Wire Line
	5400 6800 5200 6800
Wire Wire Line
	6100 6500 5900 6500
Wire Wire Line
	5900 6600 6100 6600
Wire Wire Line
	6100 6700 5900 6700
Wire Wire Line
	5900 6800 6100 6800
Wire Wire Line
	9600 4650 9600 4850
$Comp
L MCU_Microchip_ATmega:ATmega32-16PU ZIF2
U 1 1 5DE4194B
P 5650 2900
F 0 "ZIF2" H 4900 3450 50  0000 C CNN
F 1 "MCU-40" H 4850 3350 50  0000 C CNN
F 2 "Socket:DIP_Socket-40_W11.9_W12.7_W15.24_W17.78_W18.5_3M_240-1280-00-0602J" H 5650 2900 50  0001 C CIN
F 3 "http://ww1.microchip.com/downloads/en/DeviceDoc/doc2503.pdf" H 5650 2900 50  0001 C CNN
	1    5650 2900
	1    0    0    -1  
$EndComp
Text GLabel 5500 700  0    50   Input ~ 0
VCC
Wire Wire Line
	5500 700  5650 700 
Wire Wire Line
	5750 700  5750 900 
Wire Wire Line
	5650 900  5650 700 
Connection ~ 5650 700 
Wire Wire Line
	5650 700  5750 700 
$Comp
L power:GND #PWR0108
U 1 1 5DEE6AA5
P 5650 5050
F 0 "#PWR0108" H 5650 4800 50  0001 C CNN
F 1 "GND" H 5655 4877 50  0000 C CNN
F 2 "" H 5650 5050 50  0001 C CNN
F 3 "" H 5650 5050 50  0001 C CNN
	1    5650 5050
	1    0    0    -1  
$EndComp
Wire Wire Line
	5650 5050 5650 4900
Text GLabel 6450 2100 2    50   Input ~ 0
D0
Text GLabel 6450 2200 2    50   Input ~ 0
D1
Text GLabel 6450 2300 2    50   Input ~ 0
D2
Text GLabel 6450 2400 2    50   Input ~ 0
D3
Text GLabel 6450 2500 2    50   Input ~ 0
D4
Text GLabel 6450 2600 2    50   Input ~ 0
D5
Wire Wire Line
	6250 2100 6450 2100
Wire Wire Line
	6450 2200 6250 2200
Wire Wire Line
	6250 2300 6450 2300
Wire Wire Line
	6450 2400 6250 2400
Wire Wire Line
	6250 2500 6450 2500
Wire Wire Line
	6450 2600 6250 2600
Wire Wire Line
	8600 2300 8800 2300
Text GLabel 8800 2300 2    50   Input ~ 0
BS2
Wire Wire Line
	8800 2200 8600 2200
Wire Wire Line
	8600 2100 8800 2100
Text GLabel 8800 2200 2    50   Input ~ 0
D7
Text GLabel 8800 2100 2    50   Input ~ 0
D6
Wire Wire Line
	6450 2800 6250 2800
Wire Wire Line
	6250 2700 6450 2700
Text GLabel 6450 2800 2    50   Input ~ 0
D7
Text GLabel 6450 2700 2    50   Input ~ 0
D6
Text GLabel 4850 1200 0    50   Input ~ 0
RESET
Wire Wire Line
	5050 1200 4850 1200
Text GLabel 4850 1400 0    50   Input ~ 0
XTAL1
Wire Wire Line
	4850 1400 5050 1400
Text GLabel 6450 4000 2    50   Input ~ 0
RDY
Text GLabel 6450 4100 2    50   Input ~ 0
OE
Text GLabel 6450 4200 2    50   Input ~ 0
WR
Text GLabel 6450 4300 2    50   Input ~ 0
BS1
Text GLabel 6450 4400 2    50   Input ~ 0
XA0
Text GLabel 6450 4500 2    50   Input ~ 0
XA1
Wire Wire Line
	6250 4000 6450 4000
Wire Wire Line
	6450 4100 6250 4100
Wire Wire Line
	6250 4200 6450 4200
Wire Wire Line
	6450 4300 6250 4300
Wire Wire Line
	6250 4400 6450 4400
Wire Wire Line
	6450 4500 6250 4500
Text GLabel 6450 4600 2    50   Input ~ 0
PAGEL
Wire Wire Line
	6250 4600 6450 4600
Wire Wire Line
	6250 1200 6450 1200
Text GLabel 6450 1200 2    50   Input ~ 0
BS2
NoConn ~ 6250 1300
NoConn ~ 6250 1400
NoConn ~ 6250 1500
NoConn ~ 6250 1600
NoConn ~ 6250 1700
NoConn ~ 6250 1800
NoConn ~ 6250 1900
NoConn ~ 5050 1600
NoConn ~ 5050 1800
NoConn ~ 6250 3000
NoConn ~ 6250 3100
NoConn ~ 6250 3200
NoConn ~ 6250 3300
NoConn ~ 6250 3400
NoConn ~ 6250 3500
NoConn ~ 6250 3600
NoConn ~ 6250 3700
Wire Wire Line
	9700 4150 9600 4150
Wire Wire Line
	9000 4450 8950 4450
Wire Wire Line
	8950 5800 9000 5800
Wire Wire Line
	9600 5500 9700 5500
Wire Wire Line
	1000 4950 900  4950
Wire Wire Line
	850  4650 900  4650
Wire Wire Line
	1000 4550 900  4550
Wire Wire Line
	900  4550 900  4650
Connection ~ 900  4650
Wire Wire Line
	900  4650 1000 4650
Wire Wire Line
	1000 4850 900  4850
Wire Wire Line
	900  4850 900  4950
Connection ~ 900  4950
Wire Wire Line
	900  4950 850  4950
Wire Wire Line
	1000 5250 900  5250
Wire Wire Line
	900  5250 900  5350
Connection ~ 900  5350
Wire Wire Line
	900  5350 1000 5350
Wire Wire Line
	1000 5150 900  5150
Wire Wire Line
	900  5150 900  5250
Connection ~ 900  5250
Wire Wire Line
	1000 4750 900  4750
Wire Wire Line
	900  4750 900  4650
$EndSCHEMATC
