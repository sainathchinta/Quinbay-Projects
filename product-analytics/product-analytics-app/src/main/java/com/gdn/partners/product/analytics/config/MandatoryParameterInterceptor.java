package com.gdn.partners.product.analytics.config;


import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.HandlerInterceptor;

import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.product.analytics.client.constants.ClientParameterConstants;

@Slf4j
@Configuration
public class MandatoryParameterInterceptor implements HandlerInterceptor {

  @Override
  public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        StringUtils.isBlank(request.getHeader(ClientParameterConstants.STORE_ID)) ?
            request.getParameter(ClientParameterConstants.STORE_ID) :
            request.getHeader(ClientParameterConstants.STORE_ID));
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        StringUtils.isBlank(request.getHeader(ClientParameterConstants.REQUEST_ID)) ?
            request.getParameter(ClientParameterConstants.REQUEST_ID) :
            request.getHeader(ClientParameterConstants.REQUEST_ID));
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        StringUtils.isBlank(request.getHeader(ClientParameterConstants.USERNAME)) ?
            request.getParameter(ClientParameterConstants.USERNAME) :
            request.getHeader(ClientParameterConstants.USERNAME));
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        StringUtils.isBlank(request.getHeader(ClientParameterConstants.CLIENT_ID)) ?
            request.getParameter(ClientParameterConstants.CLIENT_ID) :
            request.getHeader(ClientParameterConstants.CLIENT_ID));
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        StringUtils.isBlank(request.getHeader(ClientParameterConstants.CHANNEL_ID)) ?
            request.getParameter(ClientParameterConstants.CHANNEL_ID) :
            request.getHeader(ClientParameterConstants.CHANNEL_ID));
    return true;
  }
}
