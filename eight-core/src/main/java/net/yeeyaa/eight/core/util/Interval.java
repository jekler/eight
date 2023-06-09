package net.yeeyaa.eight.core.util;

import java.util.StringTokenizer;
import java.util.concurrent.TimeUnit;

import net.yeeyaa.eight.IProcessor;


public class Interval implements IProcessor<Object, Interval>{
    protected int years;
    protected int months;
    protected int weeks;
    protected int days;
    protected int hours;
    protected int minutes;
    protected int seconds;
    protected int milliseconds;
    protected int microseconds;
    protected int nanoseconds;
    
	public Interval(int years, int months, int weeks, int days, int hours,
			int minutes, int seconds, int milliseconds, int microseconds,
			int nanoseconds) {
		setValue(years, months, weeks, days, hours, minutes, seconds, milliseconds, microseconds, nanoseconds);
	}

	public Interval(String interval){
		setValue(interval);
	}

	public Interval(long time, TimeUnit timeUnit){
		if(timeUnit == null) throw new NullPointerException();
		if(time != 0) {
			long days = TimeUnit.DAYS.convert(time, timeUnit);
			long weeks = days / 7;
			if(weeks > Integer.MAX_VALUE || weeks < Integer.MIN_VALUE) throw new RuntimeException("Interval: the time number is overflow.");
			time -= timeUnit.convert(days, TimeUnit.DAYS);
			this.weeks = (int) weeks;
			this.days = (int) (days - 7 * weeks);
			if(time != 0) {
				this.hours = (int)TimeUnit.HOURS.convert(time, timeUnit);
				time -= timeUnit.convert(this.hours, TimeUnit.HOURS);
				if(time != 0){
					this.minutes = (int)TimeUnit.MINUTES.convert(time, timeUnit);
					time -= timeUnit.convert(this.minutes, TimeUnit.MINUTES);
					if(time != 0){
						this.seconds = (int)TimeUnit.SECONDS.convert(time, timeUnit);
						time -= timeUnit.convert(this.seconds, TimeUnit.SECONDS);
						if(time != 0){
							this.milliseconds = (int)TimeUnit.MILLISECONDS.convert(time, timeUnit);
							time -= timeUnit.convert(this.milliseconds, TimeUnit.MILLISECONDS);
							if(time != 0){
								this.microseconds = (int)TimeUnit.MICROSECONDS.convert(time, timeUnit);
								time -= timeUnit.convert(this.microseconds, TimeUnit.MICROSECONDS);
								if(time != 0) this.nanoseconds = (int)TimeUnit.NANOSECONDS.convert(time, timeUnit);
							}
						}
					}
				}
			}
		}
	}
	
	public Interval(long time){
		this(time, TimeUnit.MILLISECONDS);
	}
	
	public Interval(){}
	
	public Integer getYears() {
		return years;
	}
	
	public void setYears(int years) {
		this.years = years;
	}
	
	public Integer getMonths() {
		return months;
	}
	
	public void setMonths(int months) {
		this.months = months;
	}
	
	public Integer getDays() {
		return days;
	}
	
	public void setDays(int days) {
		this.days = days;
	}
	
	public Integer getHours() {
		return hours;
	}
	
	public void setHours(int hours) {
		this.hours = hours;
	}
	
	public Integer getMinutes() {
		return minutes;
	}
	
	public Integer getWeeks() {
		return weeks;
	}

	public void setWeeks(int weeks) {
		this.weeks = weeks;
	}

	public void setMinutes(int minutes) {
		this.minutes = minutes;
	}
	
	public Integer getSeconds() {
		return seconds;
	}

	public void setSeconds(int seconds) {
		this.seconds = seconds;
	}

	public Integer getMilliseconds() {
		return milliseconds;
	}

	public void setMilliseconds(int milliseconds) {
		this.milliseconds = milliseconds;
	}

	public Integer getMicroseconds() {
		return microseconds;
	}

	public void setMicroseconds(int microseconds) {
		this.microseconds = microseconds;
	}

	public Integer getNanoseconds() {
		return nanoseconds;
	}

	public void setNanoseconds(int nanoseconds) {
		this.nanoseconds = nanoseconds;
	}

	public Long getTime(TimeUnit timeUnit){
		if(timeUnit == null) throw new NullPointerException();
		long second = seconds + minutes * 60 + hours * 3600 + days * 86400 + (long)weeks * 604800 + months * monthToSec(months) + years * yearToSec(years);
		long ret = timeUnit.convert(second, TimeUnit.SECONDS);
		long nanosecond = milliseconds * 1000000  + microseconds * 1000 + nanoseconds;
		ret += timeUnit.convert(nanosecond, TimeUnit.NANOSECONDS);
		return ret;
	}
	
	protected Long yearToSec(int years){
		return 31536000L * years;
	}
	
	protected Long monthToSec(int months){
		return 2592000L * months;
	}
	
	public static Interval getInstance(long time, TimeUnit timeUnit){
		return new Interval(time, timeUnit);
	}
    
	public static Interval getInstance(long time){
		return getInstance(time, TimeUnit.MILLISECONDS);
	}
	
    public static Interval getInstance(String time){
    	Interval interval = new Interval();
    	interval.setValue(time);
    	return interval;
    }
    
    public void setValue(String value){
        int    years   = 0;
        int    months  = 0;
        int    weeks  = 0;
        int    days    = 0;
        int    hours   = 0;
        int    minutes = 0;
        int    seconds = 0;
        int    milliseconds = 0;
        int    microseconds = 0;
        int    nanoseconds = 0;
        String valueToken = null;
        value = value.replace('+', ' ');
        final StringTokenizer st = new StringTokenizer(value);
        for (int i = 1; st.hasMoreTokens(); i++) {
            String token = st.nextToken();
            if ((i & 1) == 1) {
                int endHours = token.indexOf(':'); 
                if (endHours == -1) {
                    valueToken = token;
                    continue;
                }
                int offset = (token.charAt(0) == '-') ? 1 : 0;
                hours    = new Integer(token.substring(offset+0, endHours));
                minutes  = new Integer(token.substring(endHours+1, endHours+3));
                int endMinutes = token.indexOf(':', endHours+1);
                if (endMinutes != -1) seconds  = new Integer(token.substring(endMinutes+1));
                if (offset == 1){
                    hours   = -hours;
                    minutes = -minutes;
                    seconds = -seconds;
                }
                valueToken = null;
            } else {
                if (token.startsWith("year")) years = new Integer(valueToken);
                else if (token.startsWith("mon")) months = new Integer(valueToken);
                else if (token.startsWith("week")) weeks = new Integer(valueToken);
                else if (token.startsWith("day")) days = new Integer(valueToken);
                else if (token.startsWith("hour")) hours = new Integer(valueToken);
                else if (token.startsWith("min")) minutes = new Integer(valueToken);
                else if (token.startsWith("sec")) seconds = new Integer(valueToken);
                else if (token.startsWith("millisec")) milliseconds = new Integer(valueToken);
                else if (token.startsWith("microsec")) microseconds = new Integer(valueToken);
                else if (token.startsWith("nanosec")) nanoseconds = new Integer(valueToken);
            }
        }
        if (value.endsWith("ago")) setValue(-years, -months, -weeks, -days, -hours, -minutes, -seconds, -milliseconds, -microseconds, -nanoseconds);
        else setValue(years, months, weeks, days, hours, minutes, seconds, milliseconds, microseconds, nanoseconds);
    }

    public void setValue(int years, int months, int weeks, int days, int hours, int minutes, int seconds, int milliseconds, int microseconds, int nanoseconds){
        setYears(years);
        setMonths(months);
        setWeeks(weeks);
        setDays(days);
        setHours(hours);
        setMinutes(minutes);
        setSeconds(seconds);
        setMilliseconds(milliseconds);
        setMicroseconds(microseconds);
        setNanoseconds(nanoseconds);
    }
    
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append(years).append(" year ").append(months).append(" mon ").append(weeks).append(" week ").append(days).append(" day ").append(hours).append(" hour ").append(minutes)
		.append(" min ").append(seconds).append(" sec ").append(milliseconds).append(" millisec ").append(microseconds).append(" microsec ").append(nanoseconds).append(" nanosec");
		return sb.toString();
	}
	
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (!Interval.class.isInstance(obj)) return false;
        final Interval interval = (Interval)obj;
        return interval.years        == years &&
            interval.months       == months &&
            interval.weeks        == weeks &&
            interval.days         == days &&
            interval.hours        == hours &&
            interval.minutes      == minutes &&
            interval.seconds      == seconds &&
            interval.milliseconds      == milliseconds &&
            interval.microseconds      == microseconds &&
            interval.nanoseconds      == nanoseconds;
    }

    public int hashCode(){ 
    	return (((((((((nanoseconds * 31 +  microseconds) * 31 +  milliseconds) * 31 + seconds) * 31 +
                minutes) * 31 + hours) * 31 +  days) * 31 +  weeks) * 31 +  months) * 31 +  years) * 31;
    }

	@Override
	public Interval process(Object instance) {
		if (instance instanceof Interval) return (Interval) instance;
		else if (instance instanceof Long) return getInstance((Long) instance);
		else if (instance instanceof String) return getInstance((String)instance);
		else return null;
	}
	
	public static class Parser implements IProcessor<Interval, Object> {
		protected TimeUnit unit;
		
		public void setUnit(TimeUnit unit) {
			if (unit != null) this.unit = unit;
		}

		@Override
		public Object process(Interval instance) {
			if (instance != null) if (unit == null) return instance.toString();
			else return instance.getTime(unit);
			else return null;
		}
	}
}
