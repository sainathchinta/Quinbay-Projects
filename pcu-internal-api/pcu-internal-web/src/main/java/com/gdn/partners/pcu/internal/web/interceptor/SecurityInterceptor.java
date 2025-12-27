package com.gdn.partners.pcu.internal.web.interceptor;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import jakarta.servlet.http.Cookie;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.gdn.partners.core.security.model.Accessibility;
import com.gdn.partners.core.security.model.Mode;
import com.gdn.partners.core.security.model.Session;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.CacheNames;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.ClientParameterProperties;
import com.google.common.collect.ImmutableSet;

/**
 * @author Pradeep Reddy
 */
public class SecurityInterceptor implements HandlerInterceptor {

  private static final Set<String> EXTERNAL_USER_TYPES_SET =
      new HashSet<>(ImmutableSet.of(Constants.EXTERNAL_USER, Constants.EXTERNAL_USER_MODE));

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private StringRedisTemplate redisTemplate;

  @Autowired
  private ClientParameterProperties clientParameterProperties;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Override
  public boolean preHandle(HttpServletRequest httpServletRequest,
      HttpServletResponse httpServletResponse, Object handler) throws Exception {
    boolean isAuthorized = true;
    try {
      String signature = getSignature(httpServletRequest);

      String raw = redisTemplate.opsForValue().get(CacheNames.SESSION + ":" + signature);
      if (StringUtils.isEmpty(raw)) {
        throw new UnauthorizedException();
      }

      Session session = objectMapper.readValue(raw, Session.class);
      Credential.setUsername(session.getUsername());
      Credential.setRoles(session.getRoles().toArray(new String[session.getRoles().size()]));
      Credential.setAccessibilities(session.getAccessibilities().stream()
          .map(Accessibility::getCode).toArray(String[]::new));
      clientParameterHelper.set(Constants.CHANNEL_ID, clientParameterProperties.getChannelId());
      clientParameterHelper.set(Constants.STORE_ID, clientParameterProperties.getStoreId());
      clientParameterHelper.set(Constants.CLIENT_ID, clientParameterProperties.getClientId());
      clientParameterHelper.set(Constants.REQUEST_ID, session.getUsername() + "-" + UUID.randomUUID().toString());
      clientParameterHelper.set(Constants.USER_NAME, session.getUsername());
      if (isRequestFromApp(httpServletRequest.getParameter(Constants.CHANNEL_ID))) {
        clientParameterHelper.set(Constants.CLIENT_ID, Constants.MTA_APP);
      } else if (isRequestFromApi(httpServletRequest.getParameter(Constants.CHANNEL_ID))) {
        clientParameterHelper.set(Constants.CLIENT_ID, Constants.API_CLIENT_ID);
        session.setMode(new Mode());
        session.getMode().setType(Constants.EXTERNAL_USER_MODE);
      }
      Credential.setMode(session.getMode().getType());
      if (EXTERNAL_USER_TYPES_SET.contains(session.getMode().getType())) {
        clientParameterHelper.set(Constants.BUSINESS_PARTNER_CODE, session.getMode().getCode());
      } else if(Constants.VENDOR_USER.equals(session.getMode().getType())) {
        clientParameterHelper.set(Constants.VENDOR_CODE, session.getMode().getCode());
      }
      clientParameterHelper.set(Constants.USER_TYPE, session.getMode().getType());
      httpServletRequest.getSession().setAttribute(Constants.SESSION, session);
    } catch (Exception e) {
      httpServletResponse.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
      isAuthorized = false;
    }
    return isAuthorized;
  }

  private String getSignature(HttpServletRequest httpServletRequest) {
    if (isRequestFromApp(httpServletRequest.getParameter(Constants.CHANNEL_ID))) {
      return getSignatureFromHeader(httpServletRequest);
    } else {
      return getSignatureFromCookies(httpServletRequest);
    }
  }

  private boolean isRequestFromApp(String channelId) {
    return Constants.APP_CHANNEL_ID.equals(channelId);
  }

  private boolean isRequestFromApi(String channelId) {
    return Constants.API_CHANNEL_ID.equals(channelId);
  }

  private String getSignatureFromHeader(HttpServletRequest httpServletRequest) {
    return Optional.ofNullable(httpServletRequest.getHeader(Constants.SIGNATURE))
        .orElseThrow(UnauthorizedException::new);
  }

  private String getSignatureFromCookies(HttpServletRequest httpServletRequest) {
    Cookie[] cookies = Optional.of(httpServletRequest.getCookies()).orElseThrow(UnauthorizedException::new);
    String signature = Stream.of(cookies).filter(cookie -> Constants.SIGNATURE.equals(cookie.getName())).findFirst()
        .map(Cookie::getValue).orElseThrow(UnauthorizedException::new);
    return signature;
  }

  @Override
  public void postHandle(HttpServletRequest httpServletRequest,
      HttpServletResponse httpServletResponse, Object o, ModelAndView modelAndView) throws Exception {
  }

  @Override
  public void afterCompletion(HttpServletRequest httpServletRequest,
      HttpServletResponse httpServletResponse, Object o, Exception e) throws Exception {
  }

}
