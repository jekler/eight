package net.yeeyaa.eight.annotation.tag;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.ElementType;

@Documented
@Retention(RetentionPolicy.RUNTIME)  
@Target(ElementType.METHOD) 
public @interface ServiceMethod {
	String name() default "";
	String filter() default "";
}
