package net.yeeyaa.eight.core.util;

import java.io.OutputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;

public class BASE64Decoder {
    protected final static byte pem_array[] = {

                'A','B','C','D','E','F','G','H', 
                'I','J','K','L','M','N','O','P', 
                'Q','R','S','T','U','V','W','X', 
                'Y','Z','a','b','c','d','e','f', 
                'g','h','i','j','k','l','m','n', 
                'o','p','q','r','s','t','u','v', 
                'w','x','y','z','0','1','2','3', 
                '4','5','6','7','8','9'  
	        };
    
    protected final static byte spec_array[][] = {{'+','/','='},{'+','/'},{'-','_','='},{'-','_'},{'.','_','-'},{'.','-'},{'_','-'},{'_',':'},{'.','_'},{'!','-'},{'~','-'}};
    
    protected final byte pem_convert_array[] = new byte[256];
	protected final byte terminal;

    public BASE64Decoder(Integer index) {
    	if (index == null || index >= spec_array.length || index < 0) index = 0;
        for (int i = 0; i < 255; i++) pem_convert_array[i] = -1;
        for (int i = 0; i < 62; i++) pem_convert_array[pem_array[i]] = (byte) i;
        pem_convert_array[spec_array[index][0]] = 62;
        pem_convert_array[spec_array[index][1]] = 63;
        if (spec_array[index].length > 2) terminal = spec_array[index][2];
        else terminal = 0;
	}

    public BASE64Decoder(){
    	this(0);
    }   

    protected void decodeAtom(OutputStream outStream, int rem, byte[] buffer) throws IOException {
        byte    a = -1, b = -1, c = -1, d = -1;
        if (rem > 3 && (terminal == buffer[3])) rem = 3;
        if (rem > 2 && (terminal == buffer[2])) rem = 2;
        switch (rem) {
        case 4: d = pem_convert_array[buffer[3] & 0xff];
        if (d == -1) throw new IOException("BASE64Decoder: Error data format.");
        case 3: c = pem_convert_array[buffer[2] & 0xff];
        if (c == -1) throw new IOException("BASE64Decoder: Error data format.");
        case 2: b = pem_convert_array[buffer[1] & 0xff];
            a = pem_convert_array[buffer[0] & 0xff];
            if (a == -1 || b == -1) throw new IOException("BASE64Decoder: Error data format.");
            break;
        }
        switch (rem) {
        case 2: outStream.write( (byte)(((a << 2) & 0xfc) | ((b >>> 4) & 3)) );
            break;
        case 3: outStream.write( (byte) (((a << 2) & 0xfc) | ((b >>> 4) & 3)) );
            outStream.write( (byte) (((b << 4) & 0xf0) | ((c >>> 2) & 0xf)) );
            break;
        case 4: outStream.write( (byte) (((a << 2) & 0xfc) | ((b >>> 4) & 3)) );
            outStream.write( (byte) (((b << 4) & 0xf0) | ((c >>> 2) & 0xf)) );
            outStream.write( (byte) (((c << 6) & 0xc0) | (d  & 0x3f)) );
            break;
        }
    }

    public void decode(InputStream inStream, OutputStream outStream) throws IOException {
        byte[] buffer = new byte[4];
        int rem = 0;
        int i;
        while (true) {
            do {
                i = inStream.read();
                if (i == -1) {
                	if (rem > 1) decodeAtom(outStream, rem, buffer);
                	else if (rem == 1) throw new IOException("BASE64Decoder: Not enough bytes for an atom");
                	return;
                }
            } while (i == '\n' || i == '\r');
            buffer[rem++] = (byte) i;
            if (rem > 3) {
            	decodeAtom(outStream, 4, buffer);
            	rem = 0;
            }
        }
    }

    public byte[] decode(String inputString) {
        ByteArrayInputStream inStream;
        ByteArrayOutputStream outStream;
        byte[] bytes = inputString.getBytes();
        inStream = new ByteArrayInputStream(bytes);
        outStream = new ByteArrayOutputStream(bytes.length * 3 / 4 + 1);
        try {
	        decode(inStream, outStream);
	        return (outStream.toByteArray());
	    } catch (Exception IOException) {
	        throw new Error("CharacterEncoder.encode internal error");
	    }
    }

    public byte[] decode(InputStream in) {
        try {
        	int size = in.available();
        	ByteArrayOutputStream outStream = size == 0 ? new ByteArrayOutputStream() : new ByteArrayOutputStream(size);
	        decode(in, outStream);
	        return (outStream.toByteArray());
        } catch (Exception IOException) {
            throw new Error("CharacterEncoder.encode internal error");
        }
    }

    public ByteBuffer decodeBuffer(String inputString) {
        return ByteBuffer.wrap(decode(inputString));
    }

    public ByteBuffer decodeBuffer(InputStream in){
        return ByteBuffer.wrap(decode(in));
    }
}
