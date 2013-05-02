// This is an Arduino sketch for the Arduino board that drives the
// parameter nibble bus to feed parameters into the FPGA.

#define AACK_HIGH()    PORTB |= 1
#define AACK_LOW()     PORTB &= ~1
#define PCLK_HIGH()    PORTD |= 0x40
#define PCLK_LOW()     PORTD &= ~0x40
#define PDATA_SET(n)   PORTD |= (n & 0xF) << 2

int areq = 7;
int ledPin = 13;

byte buf[32];
byte *wptr;
byte ready_to_send;

void setup() {
  DDRD = DDRD | 0x7C;
  DDRB = DDRB | 0x01;
  pinMode(areq, INPUT);
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  wptr = &buf[0];
  ready_to_send = 0;
  AACK_LOW();
  PCLK_LOW();
}

void loop() {
  int i, _areq = digitalRead(areq);

  if (!ready_to_send && Serial.available()) {
    digitalWrite(ledPin, HIGH);
    *wptr++ = Serial.read();
    if (wptr == &buf[32]) {
      ready_to_send = 1;
    }
  }

  if (_areq && ready_to_send) {
    digitalWrite(ledPin, LOW);
    for (i = 0; i < 32; i++) {
      PDATA_SET(buf[i]);
      PCLK_HIGH(); PCLK_LOW();
      PDATA_SET(buf[i] >> 4);
      PCLK_HIGH(); PCLK_LOW();
      wptr = &buf[0];
      ready_to_send = 0;
      AACK_HIGH(); AACK_LOW();
    }
  }
}
