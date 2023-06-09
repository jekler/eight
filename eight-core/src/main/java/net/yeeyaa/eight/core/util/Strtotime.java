package net.yeeyaa.eight.core.util;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

public class Strtotime {  
    protected final List<Matcher> matchers = new LinkedList<Matcher>();  

    public void registerMatcher(Matcher matcher) { 
        matchers.add(matcher); 
    } 
 
    public static interface Matcher { 
        public Calendar tryConvert(String input); 
    } 
 
    protected static class DateFormatMatcher implements Matcher { 
        protected final DateFormat dateFormat; 
 
        public DateFormatMatcher(DateFormat dateFormat) { 
            this.dateFormat = dateFormat; 
        } 
 
        public Calendar tryConvert(String input) { 
            try { 
                dateFormat.format(dateFormat.parse(input)); 
                return dateFormat.getCalendar();
            } catch (Exception ex) { 
                return null; 
            } 
        } 
    } 
 
    protected static class NowMatcher implements Matcher { 
        protected final Pattern now = Pattern.compile("now"); 
 
        public Calendar tryConvert(String input) { 
            if (now.matcher(input).matches()) { 
                return Calendar.getInstance(); 
            } else { 
                return null; 
            } 
        } 
    } 
 
    protected static class TomorrowMatcher implements Matcher { 
        protected final Pattern tomorrow = Pattern.compile("tomorrow"); 
 
        public Calendar tryConvert(String input) { 
            if (tomorrow.matcher(input).matches()) { 
                Calendar calendar = Calendar.getInstance(); 
                calendar.add(Calendar.DAY_OF_YEAR, +1); 
                return calendar; 
            } else { 
                return null; 
            } 
        } 
    } 
 
    public Calendar getTime(String input) { 
        for (Matcher matcher : matchers) { 
        	Calendar date = matcher.tryConvert(input); 
            if (date != null) { 
                return date; 
            } 
        } 
        return null; 
    } 
    
    public Strtotime() { 
        matchers.add(new NowMatcher()); 
        matchers.add(new TomorrowMatcher()); 
        matchers.add(new DateFormatMatcher(new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US)));
        matchers.add(new DateFormatMatcher(new SimpleDateFormat("EEE, MMM dd yyyy HH:mm:ss Z", Locale.US)));
        matchers.add(new DateFormatMatcher(new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy"))); 
        matchers.add(new DateFormatMatcher(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"))); 
        matchers.add(new DateFormatMatcher(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")));
        matchers.add(new DateFormatMatcher(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss zzzz"))); 
        matchers.add(new DateFormatMatcher(new SimpleDateFormat("yyyy年MM月dd日 HH时mm分ss秒")));
        matchers.add(new DateFormatMatcher(new SimpleDateFormat("yyyy-MM-dd")));
    } 
} 
