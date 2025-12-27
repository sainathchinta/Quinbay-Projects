package com.gdn.x.mta.distributiontask.config;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.MDC;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@AllArgsConstructor
public class MandatoryParameterInterceptor implements WebMvcConfigurer, HandlerInterceptor {

  private MandatoryParameterHelper mandatoryParameterHelper;
  private SecondaryDataSourceApis secondaryDataSourceApis;

  /**
   * Get mandatory parameter from servlet request
   *
   * @param request servlet request
   * @return mandatory parameter
   */
  private MandatoryParameter getMandatoryParameter(HttpServletRequest request) {
    log.debug("Start Request = {} - {} ", request.getMethod(), request.getRequestURI());

    String STORE_ID = StringUtils.isBlank(request.getHeader(MandatoryParameterConstants.STORE_ID)) ?
        request.getParameter(MandatoryParameterConstants.STORE_ID) :
        request.getHeader(MandatoryParameterConstants.STORE_ID);
    String REQUEST_ID = StringUtils.isBlank(request.getHeader(MandatoryParameterConstants.REQUEST_ID)) ?
        request.getParameter(MandatoryParameterConstants.REQUEST_ID) :
        request.getHeader(MandatoryParameterConstants.REQUEST_ID);
    String CLIENT_ID = StringUtils.isBlank(request.getHeader(MandatoryParameterConstants.CLIENT_ID)) ?
        request.getParameter(MandatoryParameterConstants.CLIENT_ID) :
        request.getHeader(MandatoryParameterConstants.CLIENT_ID);
    String CHANNEL_ID = StringUtils.isBlank(request.getHeader(MandatoryParameterConstants.CHANNEL_ID)) ?
        request.getParameter(MandatoryParameterConstants.CHANNEL_ID) :
        request.getHeader(MandatoryParameterConstants.CHANNEL_ID);
    String USERNAME = StringUtils.isBlank(request.getHeader(MandatoryParameterConstants.USERNAME)) ?
        request.getParameter(MandatoryParameterConstants.USERNAME) :
        request.getHeader(MandatoryParameterConstants.USERNAME);

    return MandatoryParameter.builder().storeId(STORE_ID).requestId(REQUEST_ID).username(USERNAME).clientId(CLIENT_ID)
        .channelId(CHANNEL_ID).build();
  }

  @Override
  public boolean preHandle(HttpServletRequest request, HttpServletResponse response,
      Object handler) {
    MandatoryParameter mandatoryParameter = this.getMandatoryParameter(request);

    String servletPath = request.getServletPath();
    if (servletPath.startsWith("/api")) {
      mandatoryParameterHelper.validateAndSet(mandatoryParameter);
      request.setAttribute(MandatoryParameterConstants.MANDATORY_PARAMETER, mandatoryParameter);
    }
    mandatoryParameterHelper.validateAndSetDataSource(getDataSourceBasedOnServletPath(servletPath));
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        mandatoryParameter.getStoreId());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, mandatoryParameter.getRequestId());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, mandatoryParameter.getUsername());
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, mandatoryParameter.getClientId());
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, mandatoryParameter.getChannelId());
    return true;
  }

  private DataSourceType getDataSourceBasedOnServletPath(String servletPath) {
    List<String> apiPatterns =
        Optional.ofNullable(secondaryDataSourceApis.getApis()).orElseGet(ArrayList::new);
    for (String apiPattern : apiPatterns) {
      Pattern pattern = Pattern.compile(apiPattern);
      Matcher matcher = pattern.matcher(servletPath);
      if (matcher.find()) {
        return DataSourceType.SECONDARY;
      }
    }
    return DataSourceType.PRIMARY;
  }

}
