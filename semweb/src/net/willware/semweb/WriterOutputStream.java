package net.willware.semweb;

import java.io.OutputStream;
import java.io.IOException;
import java.io.Writer;

/**  
 * Adapter for a Writer to behave like an OutputStream.   
 * 
 * Bytes are converted to chars using the platform default encoding. 
 * If this encoding is not a single-byte encoding, some data may be lost. 
 */  
public class WriterOutputStream extends OutputStream {  
   
    private final Writer writer;  
   
    public WriterOutputStream(Writer writer) {  
        this.writer = writer;  
    }  
   
    public void write(int b) throws IOException {  
        // It's tempting to use writer.write((char) b), but that may get the encoding wrong  
        // This is inefficient, but it works  
        write(new byte[] {(byte) b}, 0, 1);  
    }  
   
    public void write(byte b[], int off, int len) throws IOException {  
        writer.write(new String(b, off, len));  
    }  
   
    public void flush() throws IOException {  
        writer.flush();  
    }  
   
    public void close() throws IOException {  
        writer.close();  
    }  
}
