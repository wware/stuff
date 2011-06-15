"""
Here's what it would look like in C, for a low-endian machine.

union {
    unsigned int whole;  /* 4 bytes */
    struct
    {
        unsigned char data;        /* 1 byte */
        unsigned short remainder;  /* 2 bytes */
        unsigned char head;        /* 1 byte */
    } part;
} crc_buffer;

static void putCRC(unsigned char b)
{
    unsigned char i;
    crc_buffer.part.data = b;
    for (i = 0; i < 8; i++)
    {
        crc_buffer.whole <<= 1;
        if (crc_buffer.part.head & 1)
            crc_buffer.part.remainder ^= POLYNOMIAL;
    };
}

unsigned short CRC (unsigned char *data, unsigned int length)
{
    crc_buffer.part.remainder = 0;
    while (length-- > 0)
        putCRC(*data++);
    putCRC(0);
    putCRC(0);
    return crc_buffer.part.remainder;
}
"""

import array

def crc16(bytes):
    """
    >>> a = array.array('B')
    >>> a.fromlist([3,1,4,1,5,9])
    >>> crc16(a)
    18009
    >>> crc16(a) == 0x4659
    True
    >>> crc16([1,2,3])
    Traceback (most recent call last):
        ...
    AssertionError
    >>> a = array.array('b')
    >>> a.fromlist([3,1,4,1,5,9])
    >>> crc16(a)
    Traceback (most recent call last):
        ...
    AssertionError
    """
    POLYNOMIAL = 0x8005
    class CRCBuffer:
        def __init__(self):
            self.whole = 0
        def leftshift(self):
            self.whole <<= 1
        def applyPolynomial(self):
            self.whole ^= POLYNOMIAL << 8
        def highBit(self):
            return self.whole & 0x1000000
        def setData(self, x):
            self.whole = ((self.whole & 0xffff00)
                          | (x & 0xff))
        def getResult(self):
            return int((self.whole >> 8) & 0xffff)
    buf = CRCBuffer()
    def putByte(b):
        buf.setData(b)
        for i in range(8):
            buf.leftshift()
            if buf.highBit():
                buf.applyPolynomial()
    assert type(bytes) is array.ArrayType
    assert bytes.typecode is 'B'
    for x in bytes:
        putByte(x)
    putByte(0)
    putByte(0)
    return buf.getResult()

if __name__ == '__main__':
    import doctest
    doctest.testmod()
