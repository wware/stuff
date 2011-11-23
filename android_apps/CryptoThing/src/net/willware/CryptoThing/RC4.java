package net.willware.CryptoThing;

import java.util.Random;
import java.util.Formatter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.zip.Inflater;
import java.util.zip.Deflater;
import java.util.zip.DataFormatException;

public class RC4 {

    // handy to make this an option for certain kinds of debugging
    private static final boolean USE_SALT = true;

    public static final int DEFAULT_SETUP_LOOPS = 20;

    // Zlib compression only pays off if plaintexts are really long.
    public static final int COMPRESSION_LENGTH_THRESHOLD = 100;

    // Bytes in Java are eight-bit SIGNED integers. Hence, lots of gymnastics
    // with "[int] & 255" when a byte needs to serve as an array index.
    private byte[] S;
    private int i, j;

    public RC4(String key) {
        try {
            init(key.getBytes("ASCII"), DEFAULT_SETUP_LOOPS);
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    public RC4(byte[] key) {
        init(key, DEFAULT_SETUP_LOOPS);
    }

    private void init(byte[] key, int setupLoops) {
        int m;
        int n = key.length;
        S = new byte[256];
        for (i = 0; i < 256; i++)
            S[i] = (byte) i;
        i = j = 0;
        for (m = 0; m < setupLoops; m++) {
            for (i = 0; i < 256; i++) {
                byte tmp;
                j = (j + S[i] + key[i % n]) & 255;
                tmp = S[i];
                S[i] = S[j];
                S[j] = tmp;
            }
        }
        i = j = 0;
    }

    private static byte[] prependSalt(byte[] salt, String key) {
        byte[] keybytes;
        try {
            keybytes = key.getBytes("ASCII");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
        return prependSalt(salt, keybytes);
    }

    private static byte[] prependSalt(byte[] salt, byte[] key) {
        byte[] extendedKey = new byte[salt.length + key.length];
        for (int i = 0; i < salt.length + key.length; i++) {
            if (i < salt.length)
                extendedKey[i] = salt[i];
            else
                extendedKey[i] = key[i - salt.length];
        }
        return extendedKey;
    }

    public byte sequence() {
        byte tmp;
        i = (i + 1) & 255;
        j = (j + S[i]) & 255;
        tmp = S[i];
        S[i] = S[j];
        S[j] = tmp;
        return S[(S[i] + S[j]) & 255];
    }

    public byte[] encrypt(byte[] input) {
        byte[] output = new byte[input.length];
        for (int i = 0; i < input.length; i++)
            output[i] = (byte) (input[i] ^ sequence());
        return output;
    }

    public String encrypt(String str)
        throws UnsupportedEncodingException {
        return new String(encrypt(str.getBytes("ASCII")), "ASCII");
    }

    public static String cipherSaberEncrypt(String key, String plaintext) {
        // convert key and plaintext to byte arrays
        byte[] textbytes;
        byte[] b;
        int n;
        try {
            textbytes = plaintext.getBytes("ASCII");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }

        // Zlib-compress the plaintext, producing a byte array, but do this
        // only if the plaintext is long enough to make it worthwhile. Prepend
        // a byte which is 1 if we compressed, 0 if we didn't.
        if (plaintext.length() > COMPRESSION_LENGTH_THRESHOLD) {
            Deflater compressor = new Deflater();
            compressor.setInput(textbytes);
            compressor.finish();
            b = new byte[textbytes.length + 100];  // longer than necessary
            n = compressor.deflate(b) + 1;
            textbytes = new byte[n];
            textbytes[0] = 1;   // yes, we compressed
            for (int i = 1; i < n; i++)
                textbytes[i] = b[i - 1];
        } else {
            n = textbytes.length + 1;
            b = new byte[n];
            b[0] = 0;    // nope, didn't compress
            for (int i = 1; i < n; i++)
                b[i] = textbytes[i - 1];
            textbytes = b;
        }
        // Now textbytes is the bytes to be encrypted and n = len(textbytes)

        // create a 10-byte random salt
        byte[] salt = new byte[10];
        if (USE_SALT) {
            new Random().nextBytes(salt);
        } else {
            for (int i = 0; i < 10; i++)
                salt[i] = 0;
        }

        // setup loop for Cipher Saber
        RC4 rc4 = new RC4(prependSalt(salt, key));

        // perform the encryption
        byte[] cipherbytes = rc4.encrypt(textbytes);

        // prepend the salt to the cipherbytes
        b = new byte[10 + n];
        for (int i = 0; i < 10 + n; i++) {
            if (i < 10)
                b[i] = salt[i];
            else
                b[i] = cipherbytes[i - 10];
        }

        return Base64.encodeBytes(b, 0, 10 + n);
    }

    public static String cipherSaberDecrypt(String key, String ciphertext) {
        // convert key and plaintext to byte arrays
        byte[] textbytes;
        int n;
        try {
            textbytes = Base64.decode(ciphertext);
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        // extract the 10-byte salt from the remaining ciphertext
        byte[] salt = new byte[10];
        byte[] cipherbytes = new byte[textbytes.length - 10];
        for (int i = 0; i < textbytes.length; i++) {
            if (i < 10)
                salt[i] = textbytes[i];
            else
                cipherbytes[i - 10] = textbytes[i];
        }

        // setup loop for Cipher Saber
        RC4 rc4 = new RC4(prependSalt(salt, key));

        // perform the decryption
        byte[] plainbytes = rc4.encrypt(cipherbytes);
        byte[] b;
        // the first byte tells us whether we need to uncompress
        if (plainbytes[0] != 0) {
            Inflater decompressor = new Inflater();
            decompressor.setInput(plainbytes, 1, plainbytes.length - 1);
            // how big is big enough??
            b = new byte[10 * plainbytes.length + 100];
            try {
                n = decompressor.inflate(b);
            }
            catch (DataFormatException e) {
                return "-- BAD DECRYPTION --";
            }
            decompressor.end();
            plainbytes = new byte[n];
            for (int i = 0; i < n; i++)
                plainbytes[i] = b[i];
        } else {
            n = plainbytes.length - 1;
            b = new byte[n];
            for (int i = 0; i < n; i++)
                b[i] = plainbytes[i + 1];
            plainbytes = b;
        }

        try {
            return new String(plainbytes, "ASCII");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    private static void print_bytes(byte[] ary, int len) {
        if (len > ary.length)
            len = ary.length;
        for (int i = 0; i < len; i++) {
            Formatter fmt = new Formatter();
            fmt.format("%02X ", ary[i]);
            System.err.print(fmt);
        }
        System.err.println();
    }

    private static void print_bytes(byte[] ary) {
        print_bytes(ary, ary.length);
    }

    private static void help_info() {
        // too lazy right now
    }

    private static String read_all_stdin() {
        StringBuilder sb = new StringBuilder();
        try {
            int c;
            while ((c = System.in.read()) != -1) {
                sb.append((char)c);
            }
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
        return sb.toString();
    }

    public static void main(String[] args) {
        String cmd = null, key = null, arg2 = null;
        if (args.length > 0)
            cmd = args[0];
        if (args.length > 1)
            key = args[1];
        if (args.length > 2)
            arg2 = args[2];
        // first arg is a command, second is a crypto key
        if (cmd == null || key == null) {
            help_info();
            return;
        }

        // create a 10-byte random salt
        byte[] salt = new byte[10];
        if (USE_SALT) {
            new Random().nextBytes(salt);
        } else {
            for (int i = 0; i < 10; i++)
                salt[i] = 0;
        }

        // setup loop for Cipher Saber
        //print_bytes(prependSalt(salt, key));
        RC4 rc4 = new RC4(prependSalt(salt, key));

        if ("sequence".equals(cmd)) {
            int n = 20;
            byte[] ary = new byte[n];
            for (int i = 0; i < n; i++) {
                ary[i] = rc4.sequence();
            }
            print_bytes(ary);
        }

        else if ("test".equals(cmd)) {
            String original = (arg2 != null) ? arg2 : "ABCDEFGH";
            System.out.println(original);
            String cipher = cipherSaberEncrypt(key, original);
            System.out.println(cipher);
            String decrypted = cipherSaberDecrypt(key, cipher);
            System.out.println(decrypted);
        }

        else if ("encrypt".equals(cmd)) {
            if (args.length < 3)
                return;
            System.out.println(cipherSaberEncrypt(key, arg2));
        }

        else if ("decrypt".equals(cmd)) {
            if (args.length < 3)
                return;
            System.out.println(cipherSaberDecrypt(key, arg2));
        }

        else if ("encryptstream".equals(cmd)) {
            System.out.println(cipherSaberEncrypt(key, read_all_stdin()));
        }

        else if ("decryptstream".equals(cmd)) {
            System.out.println(cipherSaberDecrypt(key, read_all_stdin()));
        }
    }
}
