package com.gdn.partners.pcu.master.web.interceptor;

import java.util.Optional;
import java.util.stream.Stream;


import com.gdn.partners.pcu.master.model.ClientParameterConstants;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.gdn.partners.core.security.model.Accessibility;
import com.gdn.partners.core.security.model.Session;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.CacheNames;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.properties.ClientParameterProperties;

/**
 * @author Pradeep Reddy
 */
@Slf4j
public class SecurityInterceptor implements HandlerInterceptor {

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
      Credential.setMode(session.getMode().getType());
      Credential.setRoles(session.getRoles().toArray(new String[session.getRoles().size()]));
      Credential.setAccessibilities(
          session.getAccessibilities().stream().map(Accessibility::getCode).toArray(String[]::new));
      clientParameterHelper.set(Constants.CHANNEL_ID, clientParameterProperties.getChannelId());
      clientParameterHelper.set(Constants.STORE_ID, clientParameterProperties.getStoreId());
      clientParameterHelper.set(Constants.CLIENT_ID, clientParameterProperties.getClientId());
      clientParameterHelper.set(Constants.REQUEST_ID,
          session.getUsername() + Constants.HYPHEN + clientParameterHelper.getRequestId());
      clientParameterHelper.set(Constants.USER_NAME, session.getUsername());
      if (Constants.EXTERNAL_USER_MODE.equals(session.getMode().getType())) {
        clientParameterHelper.set(Constants.IS_EXTERNAL, String.valueOf(Boolean.TRUE));
      }
      if (isRequestFromApi(httpServletRequest.getParameter(Constants.CHANNEL_ID))) {
        String businessPartnerCode = httpServletRequest.getParameter(Constants.STORE_CODE);
        if (session.getStores().contains(businessPartnerCode)) {
          clientParameterHelper.set(ClientParameterConstants.BUSINESS_PARTNER_CODE, businessPartnerCode);
        } else {
          throw new UnauthorizedException();
        }
      } else {
        clientParameterHelper.set(ClientParameterConstants.BUSINESS_PARTNER_CODE,
            StringUtils.isNotBlank(session.getMode().getCode()) ?
                session.getMode().getCode() :
                clientParameterHelper.getBusinessPartnerCode());
      }
      httpServletRequest.getSession().setAttribute(Constants.SESSION, session);
    } catch (Exception e) {
      log.error("error on pre-handler, error- ", e);
      httpServletResponse.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
      isAuthorized = false;
    }
    return isAuthorized;
  }

  private boolean isRequestFromApi(String channelId) {
    return Constants.API_CHANNEL_ID.equals(channelId);
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