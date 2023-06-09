package net.yeeyaa.eight.ss;

import java.util.HashSet;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.AuthenticationUserDetailsService;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;

public class UserDetailServiceImpl implements AuthenticationUserDetailsService<Authentication> {
	protected static final Logger log = LoggerFactory.getLogger(UserDetailServiceImpl.class);
	protected IProcessor<String, String[]> userProcessor;
	protected IResource<String, UserInfo> cache;
	protected Integer refreshInterval = 0;

	protected static class UserInfo{
		protected UserDetails userDetail;
		protected Long timestamp;
	}

	public void setUserProcessor(IProcessor<String, String[]> userProcessor) {
		this.userProcessor = userProcessor;
	}

	public void setRefreshInterval(Integer refreshInterval) {
		if(refreshInterval != null && refreshInterval > 0) this.refreshInterval = refreshInterval * 1000;
	}
	
	public void setCache(IResource<String, UserInfo> cache) {
		this.cache = cache;
	}

	protected User loadUserAuth(Authentication token){
		Set<GrantedAuthority> auths = new HashSet<GrantedAuthority>();
		try{
			String[] roles = userProcessor.process(token.getName());
			if(roles != null && roles.length > 0) for(String role : roles) auths.add(new SimpleGrantedAuthority(role));
		}catch(Exception e){
			log.error("UserDetailServiceImpl: Cannot load user auth info: " + token.getName(), e);
		}
		return new User(token.getName(), "", true, true, true, true, auths);
	}
	
	public UserDetails loadUserDetails(Authentication token) throws UsernameNotFoundException {
		if(token != null && token.getName() != null && token.getName().trim().length() > 0) if(cache == null || refreshInterval == 0) return loadUserAuth(token);
		else {
			UserInfo userInfo = cache.find(token.getName());
			long time = System.currentTimeMillis();
			if(userInfo == null || userInfo.timestamp + refreshInterval < time){
				userInfo = new UserInfo();
				userInfo.userDetail = loadUserAuth(token);
				userInfo.timestamp = time;
				cache.store(userInfo, token.getName());
			}
			return userInfo.userDetail;
		}
		else return null;
	}
}
