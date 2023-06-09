package net.yeeyaa.eight.core.util;

import java.io.IOException;
import java.io.Writer;
import java.util.LinkedList;


public class FragmentStringWriter extends Writer {
	private int cursor;
	private int current;
	private final LinkedList<char[]> buffer = new LinkedList<char[]>();
	private final int bufferSize;

	public FragmentStringWriter() {
		this(8192);
	}

	public FragmentStringWriter(final int bufferSize) {
		this.bufferSize = bufferSize;
		buffer.add(new char[bufferSize]);
	}

	public synchronized void write(final char[] cbuf, final int off, final int len) throws IOException {
		if (len < 0) throw new IllegalArgumentException();
		if (cbuf == null) throw new NullPointerException();
		if (off < 0 || (len + off) > cbuf.length) throw new IndexOutOfBoundsException();
		char[] b = buffer.getLast();
		int length = len + current - b.length;
		if (length > 0) {
			char[] a = new char[length > bufferSize ? length : bufferSize];
			if (current < b.length) System.arraycopy(cbuf, off, b, current, b.length - current);
			 System.arraycopy(cbuf, off + b.length - current, a, 0, length);
			 current = length;
			 buffer.add(a);
		} else {
			System.arraycopy(cbuf, off, b, current, len);
			current += len;
		}
		cursor += len;
	}

	public void flush() throws IOException {}

	public void close() throws IOException {}

	public int getCursor() {
		return cursor;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder(cursor);
		int count = 0;
		for (char[] b : buffer) {
			count += b.length;
			sb.append(b, 0, count <= cursor ? b.length : b.length + cursor - count);
			if (count > cursor) break;
		}
		return sb.toString();
	}
}
