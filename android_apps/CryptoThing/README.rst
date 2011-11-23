Android crypt app stuff
=======================

* Write some documentation, help text, etc.
* Make future plans, password safe, SMS connection, etc.
* If a decryption fails, it shouldn't throw an exception, which makes the app
  die violently. It should maybe do a pop-up, or just a "sorry" message in the
  output window. The only reason a decryption will fail in this way is if the
  zlib headers are broken, so investigate with "adb logcat".

I think it would make a lot of sense to create a cloud-backed store for
encrypted notes. The notes are pulled down and decrypted on the client only.
If the user edits them, they are re-encrypted before being sent back to the
server. This escapes any need for HTTPS, assuming the user is willing to use a
strong password.

There should be a very strong password on the device which is unlocked by a
weak key, like a bank card PIN. The weak key should be four characters,
including mixed-case alphanumerics and punctuation.

What if you have two devices, or a device and a laptop? Do you also have a PIN
on a laptop? You could, if it's long enough and if you eat a lot of time if
the user gets the PIN wrong three times in a row.

The PIN is hashed and compared to a stored hash value for confirmation. The
PIN is then used to decrypt the strong key from a byte sequence taken from
Android's SQLite DB. See
http://developer.android.com/guide/topics/data/data-storage.html#db for details
about SQLite on Android.

Rather than write a separate hash function, it's sufficient to simply use the
PIN (without salt) as an RC4 key with 40 setup loops, and generate a sequence,
and the first 10 bytes of the sequence are stored in SQLite to confirm the PIN.
Then the PIN is used to decrypt the strong key from a stored byte sequence that
includes a salt as usual. It will be necessary to store unprintable byte
sequences in SQLite::

 # here's the SQLite schema
 CREATE TABLE keyInfo(_id INTEGER PRIMARY KEY, pinHash BLOB, strongKey BLOB)

 # here's how we store a couple of byte arrays in the database
 # first user supplies the PIN and the strong key
 byte[] pinhash = make_pin_sequence(pin);
 byte[] cryptkey = RC4.cipherSaberEncrypt(pin, strongkey);
 getContentResolver().insert(CONTENT_URI, pinhash, cryptkey);

 # here's how we'll retrieve them and use them
 byte[] strongkey = null;
 byte[] pinhash = cursor.getBlob(cursor.getColumnIndex(1));
 for (tries = 0; tries < 3; tries++)
     if (java.util.Arrays.equals(pinhash, make_pin_sequence(received_pin))) {
         byte[] cryptkey = cursor.getBlob(cursor.getColumnIndex(2));
         strongkey = RC4.cipherSaberDecrypt(received_pin, cryptkey);
         break;
     }
 if (strongkey == null) {
     // bad pin after three tries, kick the user out of the app
 }
 // strongkey is OK, go ahead and work with password safe or cloud-stored
 // secure docs or whatever

