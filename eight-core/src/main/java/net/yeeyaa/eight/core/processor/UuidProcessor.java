package net.yeeyaa.eight.core.processor;

import java.lang.management.ManagementFactory;
import java.lang.management.MemoryUsage;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;

import net.yeeyaa.eight.IProcessor;


public class UuidProcessor implements IProcessor<Object, UUID> {
	protected java.util.Random random;
	protected long special;
	protected Entry<IProcessor<Object, Object>, Object>[] generator;
	
	public class Random implements IProcessor<Object, byte[]> {
		protected Integer length = 1;
		
		public void setLength(Integer length) {
			if (length != null && length > 0) this.length = length;
		}

		@Override
		public byte[] process(Object instance) {
			byte[] bytes = new byte[length];
			random.nextBytes(bytes);
			return bytes;
		}
	}
	
	public void setSpecial(Object special) {
		if (special != null) {
			long value;
			if (special instanceof Number) value = ((Number) special).longValue();
			else {
				byte[] bytes = special.toString().getBytes();
				value = 0;
		        for (int i = 0; i < bytes.length; i ++) value = bytes[i] & 0xff ^ (value << 8 | value >>> 56);
			}
			this.special = value & 0xffffffffffffL ^ value >>> 48;
		}
	}

	public void setGenerator(Map<IProcessor<Object, Object>, Object> generator) {
		if (generator != null) {
			for (Object key : generator.keySet().toArray()) if (!(key instanceof IProcessor) || generator.get(key) == null) generator.remove(key);
			this.generator = generator.entrySet().toArray(new Entry[generator.size()]);
		}
	}

	public UuidProcessor() {
		MemoryUsage bean = ManagementFactory.getMemoryMXBean().getHeapMemoryUsage();
		random = new java.util.Random(System.currentTimeMillis() << 24 ^ bean.getUsed());
		long special = System.nanoTime() << 24 ^ bean.getCommitted();
		this.special = special & 0xffffffffffffL ^ special >>> 48;
	}

	@Override
	public UUID process(Object instance) {
		if (generator == null) {
			long r = random.nextInt();
			return new UUID(r >>> 16 & 0xffff | System.currentTimeMillis() << 16, r << 48 | special);
		} else {
			int index = 0;
			byte[] bs = new byte[16];
			for (Entry<IProcessor<Object, Object>, Object> entry : generator) {
				Object ret = entry.getKey().process(instance);
				Integer length = 0;
				if (ret != null) {
					byte[] bytes;
					if (ret instanceof byte[]) bytes = (byte[]) ret;
					else if (ret instanceof Integer) {
						long l = (Integer) ret;
						bytes = new byte[4];
					    for (int i = 3; i >= 0; l >>= 8) bytes[i--] = (byte)(l & 0xFF);
					} else if (ret instanceof Long) {
						long l = (Long) ret;
						bytes = new byte[8];
					    for (int i = 7; i >= 0; l >>= 8) bytes[i--] = (byte)(l & 0xFF);
					} else if (ret instanceof Short) {
						short l = (Short) ret;
						bytes = new byte[2];
					    for (int i = 1; i >= 0; l >>= 8) bytes[i--] = (byte)(l & 0xFF);
					} else if (ret instanceof Byte) bytes = new byte[]{(Byte)ret};
					else if (ret instanceof Double) {
						long l = Double.doubleToRawLongBits((Double) ret);
						bytes = new byte[8];
					    for (int i = 7; i >= 0; l >>= 8) bytes[i--] = (byte)(l & 0xFF);
					} else if (ret instanceof Float) {
						long l = Float.floatToRawIntBits((Float) ret);
						bytes = new byte[4];
					    for (int i = 3; i >= 0; l >>= 8) bytes[i--] = (byte)(l & 0xFF);
					} else bytes = ret.toString().getBytes();
					Object value = entry.getValue();
					if (value instanceof Integer) {
						length = (Integer) value;
						if (length == 0) length = bytes.length;
						if (length < 0) {
							length = -length;
							for (int i = bytes.length - 1, min = (length > bytes.length ? 0 : bytes.length - length), j = 0; i >= min;  i--, j++) if (index + j > 15) break; 
							else bs[index + j] = bytes[i];
						} else if (length > 0) for (int i = length > bytes.length ? 0 : bytes.length - length, j = 0; i < bytes.length;  i++, j++) if (index + j > 15) break; 
						else bs[index + j] = bytes[i];
					} else if (value instanceof Object[]) {
						length = ((Object[])value).length;
						for (int i = 0; i < length; i++) if (index + i > 15) break;
						else if (((Object[])value)[i] instanceof Integer && (Integer)((Object[])value)[i] >= 0 && (Integer)((Object[])value)[i] < bytes.length) bs[index + i] = bytes[(Integer)((Object[])value)[i]];
					}
				}
				index += length;
				if (index > 15) break;
			}
			while (index < 16) for (int r = random.nextInt(), count = index + 4 > 15 ? 16 : index + 4; index < count;  r >>= 8) bs[index++] = (byte)(r & 0xFF);
			long high = 0;
			long low = 0;
	        for (int i = 0; i < 8; i++) high = bs[i] & 0xff | high << 8;
	        for (int i = 8; i < 16; i++) low = bs[i] & 0xff | low << 8;
			return new UUID(high, low);
		}
	}	
}
