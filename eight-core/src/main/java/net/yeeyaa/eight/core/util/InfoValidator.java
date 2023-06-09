package net.yeeyaa.eight.core.util;
import java.util.regex.*;   

public final class InfoValidator   
{   
    private InfoValidator() {
	}

    protected static final Pattern email = Pattern.compile("^([\\w-\\.]+)@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.)|(([\\w-]+\\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\\]?)$");
    public static boolean isEmail(String str)   
    {   
        return email.matcher(str).matches();    
    }   

    protected static final Pattern ip = Pattern.compile("^(25[0-5]|2[0-4]\\d|[0-1]\\d{2}|[1-9]?\\d)\\.(25[0-5]|2[0-4]\\d|[0-1]\\d{2}|[1-9]?\\d)\\.(25[0-5]|2[0-4]\\d|[0-1]\\d{2}|[1-9]?\\d)\\.(25[0-5]|2[0-4]\\d|[0-1]\\d{2}|[1-9]?\\d)$");
    public static boolean isIP(String str)   
    {   
        return ip.matcher(str).matches();  
    }   
 
    protected static final Pattern url = Pattern.compile("http(s)?://([\\w-]+\\.)+[\\w-]+(/[\\w- ./?%&=]*)?");
    public static boolean IsUrl(String str)   
    {   
        return url.matcher(str).matches(); 
    }   

    protected static final Pattern telephone = Pattern.compile("^(\\d{3,4}-)?\\d{6,8}$");
    public static boolean IsTelephone(String str)   
    {   
        return telephone.matcher(str).matches(); 
    }   
 
    protected static final Pattern password = Pattern.compile("[A-Za-z]+[0-9]");
    public static boolean IsPassword(String str)   
    {   
        return password.matcher(str).matches();    
    }   
 
    protected static final Pattern passwlength = Pattern.compile("^\\d{6,18}$");
    public static boolean IsPasswLength(String str)   
    {   
        return passwlength.matcher(str).matches();      
    }   
  
    protected static final Pattern postalcode = Pattern.compile("^\\d{6}$");
    public static boolean IsPostalcode(String str)   
    {   
        return postalcode.matcher(str).matches();    
    }   

    protected static final Pattern handset = Pattern.compile("^[1]+[3,5]+\\d{9}$");
    public static boolean IsHandset(String str)   
    {   
        return handset.matcher(str).matches();  
    }   
 
    protected static final Pattern idcard = Pattern.compile("(^\\d{18}$)|(^\\d{15}$)");
    public static boolean IsIDcard(String str)   
    {   
        return idcard.matcher(str).matches(); 
    }   

    protected static final Pattern decimal = Pattern.compile("^[0-9]+(.[0-9]{2})?$");
    public static boolean IsDecimal(String str)   
    {   
        return decimal.matcher(str).matches(); 
    }    
  
    protected static final Pattern month = Pattern.compile("^(0?[[1-9]|1[0-2])$");
    public static boolean IsMonth(String str)   
    {   
        return month.matcher(str).matches();   
    }   
 
    protected static final Pattern day = Pattern.compile("^((0?[1-9])|((1|2)[0-9])|30|31)$");
    public static boolean IsDay(String str)   
    {   
        return day.matcher(str).matches();   
    }   
 
    protected static final Pattern date = Pattern.compile("^((((1[6-9]|[2-9]\\d)\\d{2})-(0?[13578]|1[02])-(0?[1-9]|[12]\\d|3[01]))|(((1[6-9]|[2-9]\\d)\\d{2})-(0?[13456789]|1[012])-(0?[1-9]|[12]\\d|30))|(((1[6-9]|[2-9]\\d)\\d{2})-0?2-(0?[1-9]|1\\d|2[0-8]))|(((1[6-9]|[2-9]\\d)(0[48]|[2468][048]|[13579][26])|((16|[2468][048]|[3579][26])00))-0?2-29-)) (20|21|22|23|[0-1]?\\d):[0-5]?\\d:[0-5]?\\d$");
    public static boolean isDate(String str)   
    {    
        return date.matcher(str).matches();  
    }   

    protected static final Pattern number = Pattern.compile("^[0-9]*$");
    public static boolean IsNumber(String str)   
    {   
        return number.matcher(str).matches(); 
    }   

    protected static final Pattern intnumber = Pattern.compile("^\\+?[1-9][0-9]*$");
    public static boolean IsIntNumber(String str)   
    {   
        return intnumber.matcher(str).matches();   
    }   
 
    protected static final Pattern upchar = Pattern.compile("^[A-Z]+$");
    public static boolean IsUpChar(String str)   
    {   
        return upchar.matcher(str).matches();  
    }   
  
    protected static final Pattern lowchar = Pattern.compile("^[a-z]+$");
    public static boolean IsLowChar(String str)   
    {   
        return lowchar.matcher(str).matches();   
    }   

    protected static final Pattern letter = Pattern.compile("^[A-Za-z]+$");
    public static boolean IsLetter(String str)   
    {   
        return letter.matcher(str).matches(); 
    }   

    protected static final Pattern chinese = Pattern.compile("^[\u4e00-\u9fa5],{0,}$");
    public static boolean IsChinese(String str)   
    {   
        return chinese.matcher(str).matches();  
    }   
 
    protected static final Pattern length = Pattern.compile("^.{8,}$");
    public static boolean IsLength(String str)   
    {   
        return length.matcher(str).matches();   
    }    
}
