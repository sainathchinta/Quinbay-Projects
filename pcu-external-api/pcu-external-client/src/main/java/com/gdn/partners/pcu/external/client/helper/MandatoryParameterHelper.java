package com.gdn.partners.pcu.external.client.helper;

import com.gdn.partners.pcu.external.client.config.TraceHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.MandatoryParameterConstants;
import io.micrometer.tracing.Tracer;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Objects;
import java.util.Set;

@Slf4j
@Component
public class MandatoryParameterHelper {

  @Autowired
  private Tracer tracer;

  @Autowired
  private TraceHelper traceHelper;


  public String getStoreId() {
    return getValueFromTracer(MandatoryParameterConstants.STORE_ID);
  }

  public String getChannelId() {
    return getValueFromTracer(MandatoryParameterConstants.CHANNEL_ID);
  }

  public String getClientId() {
    return getValueFromTracer(MandatoryParameterConstants.CLIENT_ID);
  }

  public String getRequestId() {
    return getValueFromTracer(MandatoryParameterConstants.REQUEST_ID);
  }

  public String getUsername() {
    return getValueFromTracer(MandatoryParameterConstants.USER_NAME);
  }

  public String getBusinessPartnerCode() {
    return getValueFromTracer(MandatoryParameterConstants.BUSINESS_PARTNER_CODE);
  }

  public String getLinkedBusinessPartnerCode() {
    return getValueFromTracer(MandatoryParameterConstants.LINKED_BUSINESS_PARTNER_CODE);
  }

  public String isExternal() {
    return getValueFromTracer(MandatoryParameterConstants.IS_EXTERNAL);
  }

  public String getClientType() {
    return getValueFromTracer(MandatoryParameterConstants.CLIENT_TYPE);
  }

  public String isExternalOnly() {
    return getValueFromTracer(MandatoryParameterConstants.IS_EXTERNAL_ONLY);
  }

  public String getBusinessPartnerName() {
    return getValueFromTracer(MandatoryParameterConstants.BUSINESS_PARTNER_NAME);
  }

  public boolean getIsProductVideoActivated() {
    return Boolean.parseBoolean(getValueFromTracer(MandatoryParameterConstants.IS_PRODUCT_VIDEO_ACTIVATED));
  }

  public int getAppVersion() {
    int appVersion = 0;
    if (Objects.nonNull(getValueFromTracer(MandatoryParameterConstants.APP_VERSION))) {
      try {
        appVersion = Integer.parseInt(getValueFromTracer(MandatoryParameterConstants.APP_VERSION));
      } catch (Exception e) {
        log.info("Error when trying to get app version ", e);
      }
    }
    return appVersion;
  }

  public String getAppType() {
    return getValueFromTracer(MandatoryParameterConstants.APP_TYPE);
  }

  public Set<String> getPickupPoints() {
    return Set.of(StringUtils.split(getValueFromTracer(MandatoryParameterConstants.PICKUP_POINTS),
        Constants.COMMA_DELIMITER_NO_SPACE));
  }

  private String getValueFromTracer(String key) {
    return traceHelper.getBaggage(key, tracer);
  }

  public void validateAndSet(String key, String value) {
    traceHelper.setBaggage(key, value, tracer);
  }
}