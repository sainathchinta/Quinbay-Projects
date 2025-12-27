package com.gdn.partners.pcu.external.web.interceptor;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.gdn.partners.core.security.model.Accessibility;
import com.gdn.partners.core.security.model.Mode;
import com.gdn.partners.core.security.model.Session;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.CacheNames;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.MandatoryParameterConstants;
import com.gdn.partners.pcu.external.properties.ClientParameterProperties;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

/**
 * @author Pradeep Reddy
 */
public class SecurityInterceptor implements HandlerInterceptor {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private StringRedisTemplate redisTemplate;

  @Autowired
  private ClientParameterProperties clientParameterProperties;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Value("${set.client.type}")
  private boolean setClientType;

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
      mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.CHANNEL_ID,
          StringUtils.isNotBlank(httpServletRequest.getParameter(Constants.CHANNEL_ID)) ?
              httpServletRequest.getParameter(Constants.CHANNEL_ID) :
              clientParameterProperties.getChannelId());
      mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.APP_TYPE,
          StringUtils.isNotBlank(httpServletRequest.getParameter(Constants.CLIENT_ID)) ?
              httpServletRequest.getParameter(Constants.CLIENT_ID) :
              clientParameterProperties.getClientId());
      mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.APP_VERSION,
          StringUtils.isNotBlank(httpServletRequest.getParameter(MandatoryParameterConstants.APP_VERSION)) ?
              httpServletRequest.getParameter(MandatoryParameterConstants.APP_VERSION) :
              String.valueOf(0));
      mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.STORE_ID, clientParameterProperties.getStoreId());
      mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.CLIENT_ID, clientParameterProperties.getClientId());
      String clientId = Optional.ofNullable(httpServletRequest.getParameter(Constants.CLIENT_ID))
          .orElse(mandatoryParameterHelper.getClientId());
      mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.CLIENT_ID, clientId);
      if (isRequestFromApp(httpServletRequest.getParameter(Constants.CHANNEL_ID))) {
        mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.CLIENT_ID, Constants.APP_CLIENT_ID);
      } else if (isRequestFromApi(httpServletRequest.getParameter(Constants.CHANNEL_ID))) {
        mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.CLIENT_ID, Constants.API_CLIENT_ID);
        session.setMode(new Mode());
        session.getMode().setType(Constants.EXTERNAL_USER_MODE);
      }
      Credential.setMode(session.getMode().getType());
      String requestId = Optional.ofNullable(httpServletRequest.getParameter(Constants.REQUEST_ID))
          .orElse(session.getUsername() + "-" + UUID.randomUUID().toString());
      mandatoryParameterHelper.validateAndSet(Constants.REQUEST_ID, requestId);
      mandatoryParameterHelper.validateAndSet(Constants.USER_NAME, session.getUsername());
      if (Constants.EXTERNAL_USER_MODE.equals(session.getMode().getType())) {
        mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.IS_EXTERNAL, String.valueOf(Boolean.TRUE));
        if (isRequestFromApi(httpServletRequest.getParameter(Constants.CHANNEL_ID))) {
          String businessPartnerCode = httpServletRequest.getParameter(Constants.STORE_CODE);
          if (session.getStores().contains(businessPartnerCode)) {
            mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.BUSINESS_PARTNER_CODE, businessPartnerCode);
            mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.BUSINESS_PARTNER_NAME, "-");
          } else {
            throw new UnauthorizedException();
          }
        } else {
          mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.BUSINESS_PARTNER_CODE, session.getMode().getCode());
          mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.BUSINESS_PARTNER_NAME, session.getMode().getName());
        }

        Object linkedPartnerCode = session.getCustomFields().get(Constants.LINKED_BUSINESS_PARTNER_CODE);
        if (Objects.nonNull(linkedPartnerCode)) {
          mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.LINKED_BUSINESS_PARTNER_CODE, String.valueOf(linkedPartnerCode));
        }

        Object isProductVideoActivated =
            session.getCustomFields().get(MandatoryParameterConstants.IS_PRODUCT_VIDEO_ACTIVATED);
        if (Objects.nonNull(isProductVideoActivated)) {
          mandatoryParameterHelper.validateAndSet(
              MandatoryParameterConstants.IS_PRODUCT_VIDEO_ACTIVATED,
              isProductVideoActivated.toString());
        }
      }
      mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.IS_EXTERNAL_ONLY,
          String.valueOf(session.getModes().size() == 1 && session.getModes()
              .contains(Constants.EXTERNAL_USER_MODE)));
      httpServletRequest.getSession().setAttribute(Constants.SESSION, session);
      mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.PICKUP_POINTS,
          StringUtils.join(session.getPickupPoints(), Constants.COMMA_DELIMITER_NO_SPACE));
    } catch (Exception e) {
      httpServletResponse.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
      isAuthorized = false;
    }
    return isAuthorized;
  }

  private String getSignature(HttpServletRequest httpServletRequest) {
    if (isRequestFromApp(httpServletRequest.getParameter(Constants.CHANNEL_ID))) {
      setClientType(Constants.CLIENT_TYPE_APP);
      return getSignatureFromHeader(httpServletRequest);
    } else if (isRequestFromApi(httpServletRequest.getParameter(Constants.CHANNEL_ID))) {
        setClientType(Constants.CLIENT_TYPE_SELLER_API);
      return getSignatureFromHeader(httpServletRequest);
    } else {
        setClientType(Constants.CLIENT_TYPE_WEB);
      return getSignatureFromCookies(httpServletRequest);
    }
  }

  private void setClientType(String clientType) {
    if (setClientType) {
      mandatoryParameterHelper.validateAndSet(MandatoryParameterConstants.CLIENT_TYPE, clientType);
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
