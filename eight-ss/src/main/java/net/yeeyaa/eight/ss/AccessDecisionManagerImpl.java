package net.yeeyaa.eight.ss;

import java.util.Collection;

import net.yeeyaa.eight.ss.InvocationSecurityMetadataSourceServiceImpl.SecurityConfig;

import org.springframework.security.access.AccessDecisionManager;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.authentication.InsufficientAuthenticationException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;


public class AccessDecisionManagerImpl implements AccessDecisionManager {
	 public void decide(Authentication authentication, Object object, Collection<ConfigAttribute> configAttributes)
	            throws AccessDeniedException, InsufficientAuthenticationException {
        if(configAttributes != null) for(ConfigAttribute ca : configAttributes)
            for(GrantedAuthority ga:authentication.getAuthorities())
                if(((SecurityConfig)ca).getAttribute().equals(ga.getAuthority())) if (((SecurityConfig)ca).permit) return;
                else throw new AccessDeniedException("no right");
        throw new AccessDeniedException("no right");
	}

    public boolean supports(ConfigAttribute attribute) {
        return true;
    }

    public boolean supports(Class<?> clazz) {
        return true;
    }
}
