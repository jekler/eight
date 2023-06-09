package net.yeeyaa.eight.annotation.tag;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.ElementType;

@Documented
@Retention(RetentionPolicy.RUNTIME)  
@Target(ElementType.TYPE) 
public @interface ServiceClass {
	String name() default "";
	String filter() default "";
	ServiceScope scope() default ServiceScope.SINGLETON;
}
