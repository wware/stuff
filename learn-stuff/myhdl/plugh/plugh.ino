// This is an Arduino sketch for the Arduino board that drives the
// parameter nibble bus to feed parameters into the FPGA.

#define AACK_HIGH()    PORTB |= 1
#define AACK_LOW()     PORTB &= ~1
#define PCLK_HIGH()    PORTD |= 0x40
#define PCLK_LOW()     PORTD &= ~0x40
#define PDATA_SET(n)   PORTD = ((PORTD & 0xC3) | (n & 0xF) << 2)

int areq = 7;
int ledPin = 13;

byte buf[] = {
  0x55, 0xAA, 0x55, 0xAA, 0x55, 0xAA, 0x55, 0xAA,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0
};

void setup() {
  DDRD = DDRD | 0x7C;
  DDRB = DDRB | 0x01;
  pinMode(areq, INPUT);
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  AACK_LOW();
  PCLK_LOW();
}

void loop() {
  int i;

  if (1 || digitalRead(areq)) {
    digitalWrite(ledPin, HIGH);
    for (i = 0; i < 32; i++) {
      PDATA_SET(buf[i]);
      PCLK_HIGH(); PCLK_LOW();
      PDATA_SET(buf[i] >> 4);
      PCLK_HIGH(); PCLK_LOW();
      AACK_HIGH(); AACK_LOW();
    }
    digitalWrite(ledPin, LOW);
    for (i = 0; i < 32; i++) {
      PDATA_SET(buf[i]);
      PCLK_HIGH(); PCLK_LOW();
      PDATA_SET(buf[i] >> 4);
      PCLK_HIGH(); PCLK_LOW();
      AACK_HIGH(); AACK_LOW();
    }
  }
}
