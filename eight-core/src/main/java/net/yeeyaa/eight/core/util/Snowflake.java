package net.yeeyaa.eight.core.util;

import net.yeeyaa.eight.IProcessor;


public class Snowflake implements IProcessor<Object, Long> {
    protected static final Long twepoch = 1420041600000L;
    protected static final Long maxWorkerId = 31L;
    protected static final Long maxDatacenterId = 31L;
    protected static final Long workerIdShift = 12L;
    protected static final Long datacenterIdShift = 17L;
    protected static final Long timestampLeftShift = 22L;
    protected static final Long sequenceMask = 4095L;
    protected final Long workerId;
    protected final Long datacenterId;
    protected Long sequence = 0L;
    protected Long lastTimestamp = -1L;

    public Snowflake() {
        this.workerId = 0L;
        this.datacenterId = 0L;
    }

	public Snowflake(Long workerId, Long datacenterId) {
        if (workerId <= maxWorkerId && workerId >= 0) {
            if (datacenterId <= maxDatacenterId && datacenterId >= 0) {
                this.workerId = workerId;
                this.datacenterId = datacenterId;
            } else throw new IllegalArgumentException(String.format("datacenter Id can't be greater than %d or less than 0", maxDatacenterId));
        } else throw new IllegalArgumentException(String.format("worker Id can't be greater than %d or less than 0", maxWorkerId));
    }
	
	@Override
	public synchronized Long process(Object object) {
        Long timestamp = System.currentTimeMillis();
        if (timestamp < this.lastTimestamp) throw new RuntimeException(String.format("Clock moved backwards.  Refusing to generate id for %d milliseconds", this.lastTimestamp - timestamp));
        else {
            if (this.lastTimestamp == timestamp) {
                this.sequence = this.sequence + 1 & sequenceMask;
                if (this.sequence == 0L) {
                    timestamp = System.currentTimeMillis();
                    while (timestamp <= lastTimestamp) timestamp = System.currentTimeMillis();
                }
            } else this.sequence = 0L;
            this.lastTimestamp = timestamp;
            return timestamp - twepoch << timestampLeftShift | this.datacenterId << datacenterIdShift | this.workerId << workerIdShift | this.sequence;
        }
	}
}
