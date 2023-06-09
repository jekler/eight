package net.yeeyaa.eight.core.util;

import java.io.InputStream;
import java.io.ByteArrayInputStream;
import java.io.OutputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.IOException;
import java.nio.ByteBuffer;

public class BASE64Encoder {
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
	
    protected final byte pem_convert_array[] = new byte[64];
	protected final byte terminal;
	protected final int perLine;
	
    public BASE64Encoder(Integer index, Integer perLine) {
    	if (index == null || index < 0 || index >= spec_array.length) index = 0;
    	if (perLine == null) perLine = 76;
    	for (int i = 0; i < 62; i++) pem_convert_array[i] = pem_array[i];
    	if (index > spec_array.length || index < 0) index = 0;
        pem_convert_array[62] = spec_array[index][0];
        pem_convert_array[63] = spec_array[index][1];
        if (spec_array[index].length > 2) terminal = spec_array[index][2];
        else terminal = 0;
        this.perLine = perLine;
	}

    public BASE64Encoder(){
    	this(0, 76);
    }  

    protected int bytesPerLine() {
        return perLine > 3 ? (perLine / 4) * 3 : 57;
    }
    
    protected void encodeAtom(OutputStream outStream, byte data[], int offset, int len) throws IOException {
        byte a, b, c;
        if (len == 1) {
            a = data[offset];
            b = 0;
            c = 0;
            outStream.write(pem_convert_array[(a >>> 2) & 0x3F]);
            outStream.write(pem_convert_array[((a << 4) & 0x30) + ((b >>> 4) & 0xf)]);
            if (terminal != 0) {
	            outStream.write(terminal);
	            outStream.write(terminal);
            }
        } else if (len == 2) {
            a = data[offset];
            b = data[offset+1];
            c = 0;
            outStream.write(pem_convert_array[(a >>> 2) & 0x3F]);
            outStream.write(pem_convert_array[((a << 4) & 0x30) + ((b >>> 4) & 0xf)]);
            outStream.write(pem_convert_array[((b << 2) & 0x3c) + ((c >>> 6) & 0x3)]);
            if (terminal != 0) outStream.write(terminal);
        } else {
            a = data[offset];
            b = data[offset+1];
            c = data[offset+2];
            outStream.write(pem_convert_array[(a >>> 2) & 0x3F]);
            outStream.write(pem_convert_array[((a << 4) & 0x30) + ((b >>> 4) & 0xf)]);
            outStream.write(pem_convert_array[((b << 2) & 0x3c) + ((c >>> 6) & 0x3)]);
            outStream.write(pem_convert_array[c & 0x3F]);
        }
    }

    protected int readFully(InputStream in, byte buffer[]) throws IOException {
        for (int i = 0; i < buffer.length; i++) {
            int q = in.read();
            if (q == -1) return i;
            buffer[i] = (byte)q;
        }
        return buffer.length;
    }

    public void encode(InputStream inStream, OutputStream outStream) throws IOException {
        int j;
        int numBytes;
        byte tmpbuffer[] = new byte[bytesPerLine()];
        PrintStream pStream = new PrintStream(outStream);
        while (true) {
            numBytes = readFully(inStream, tmpbuffer);
            if (numBytes == 0)  break;
            for (j = 0; j < numBytes; j += 3) if ((j + 3) <= numBytes) encodeAtom(outStream, tmpbuffer, j, 3);
            else encodeAtom(outStream, tmpbuffer, j, (numBytes)- j);
            if (numBytes < tmpbuffer.length) break;
            else if (perLine > 0)  pStream.println();
        }
    }
    
    public void encode(byte aBuffer[], OutputStream aStream) throws IOException {
        ByteArrayInputStream inStream = new ByteArrayInputStream(aBuffer);
        encode(inStream, aStream);
    }

    public String encode(byte aBuffer[]) {
        ByteArrayOutputStream   outStream = new ByteArrayOutputStream(aBuffer.length * 4 / 3 + 1);
        ByteArrayInputStream    inStream = new ByteArrayInputStream(aBuffer);
        String retVal = null;
        try {
            encode(inStream, outStream);
            retVal = outStream.toString("8859_1");
        } catch (Exception IOException) {
            throw new Error("CharacterEncoder.encode internal error");
        }
        return retVal;
    }

    protected byte [] getBytes(ByteBuffer bb) {
        byte [] buf = null;
        if (bb.hasArray()) {
            byte [] tmp = bb.array();
            if ((tmp.length == bb.capacity()) && (tmp.length == bb.remaining())) {
                buf = tmp;
                bb.position(bb.limit());
            }
        }
        if (buf == null) {
            buf = new byte[bb.remaining()];
            bb.get(buf);
        }
        return buf;
    }

    public void encode(ByteBuffer aBuffer, OutputStream aStream) throws IOException {
        byte [] buf = getBytes(aBuffer);
        encode(buf, aStream);
    }

    public String encode(ByteBuffer aBuffer) {
        byte [] buf = getBytes(aBuffer);
        return encode(buf);
    }
}
