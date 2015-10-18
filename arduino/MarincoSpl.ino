// -*- c -*-
const int transmit_pin = 13;
const int pulse_length = 50;  // micro seconds
const int word_pulses = 440;  // total number of pulses / word

#define DELAY(t) delayMicroseconds((t))

#define RIGHT   'r'
#define LEFT    'l'
#define UP      'u'
#define DOWN    'd'
#define SOS     'x'
#define ONOFF   '1'
#define SWEEP   'w'
#define SPEED   'p'
#define RELEASE 'z'

int is_pressed;
int last_char;
int count;

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
  pinMode(transmit_pin, OUTPUT);
  digitalWrite(transmit_pin, LOW);
  last_char = 0;
  is_pressed = 0;
  count = 10;
}

void write_pulse(int high, int low)
{
    int i;
    digitalWrite(transmit_pin, HIGH);
    for (i = 0; i < high; i++)
      DELAY(pulse_length);
    digitalWrite(transmit_pin, LOW);
    for (i = 0; i < low; i++)
      DELAY(pulse_length);
}

// write msb first  bit 15 .. bit 0
void write_word(unsigned word)
{
  int i;
  int pulses = word_pulses;
    
  for (i = 0; i < 16; i++) {
    if ((word >> 15) & 1) {
      write_pulse(1,3);
      pulses -= 4;
    }
    else {
      write_pulse(3,1);
      pulses -= 4;
    }
    word <<= 1;
  }
  for (i = 0; i < pulses; i++)
    DELAY(pulse_length);
}

#define CODE_SWEEP   0b0000000110000000
#define CODE_ONOFF   0b0000001001000000
#define CODE_RIGHT   0b0000010000100000
#define CODE_DOWN    0b0000100000010000
#define CODE_SOS     0b0001000000001000
#define CODE_UP      0b0010000000000100
#define CODE_LEFT    0b0100000000000010
#define CODE_SPEED   0b1000000000000001
#define CODE_RELEASE 0b0000000000000000

void write_code(int code)
{
    switch(code) {
    case RIGHT: write_word(CODE_RIGHT); break;
    case LEFT: write_word(CODE_LEFT); break;
    case UP: write_word(CODE_UP); break;
    case DOWN: write_word(CODE_DOWN); break;
    case SOS: write_word(CODE_SOS); break;         // toggle on/off
    case ONOFF: write_word(CODE_ONOFF); break;     // toggle on/off
    case SWEEP: write_word(CODE_SWEEP); break;     // toggle on/off
    case SPEED: write_word(CODE_SPEED); break;     // toggle slow/fast
    case RELEASE: write_word(CODE_RELEASE); break; // release button
    }
}

void loop() 
{
  if (is_pressed) {
    write_code(last_char);
  }
  else if (count) {
    write_word(CODE_RELEASE);
    count--;
  }
  
  if (Serial.available() > 0) {
    last_char = Serial.read();
    write_code(last_char);
    Serial.print("char=");
    Serial.println(last_char, DEC);
    is_pressed = (last_char != RELEASE);
    count = 3;
  }
}

