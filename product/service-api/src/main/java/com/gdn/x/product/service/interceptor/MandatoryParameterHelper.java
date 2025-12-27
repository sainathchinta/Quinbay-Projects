package com.gdn.x.product.service.interceptor;

import java.util.UUID;

import brave.Tracer;
import brave.propagation.TraceContext;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import com.gdn.x.product.enums.TracerFieldKey;
import com.gdn.x.product.property.MandatoryParameterConstants;
import com.gdn.x.product.service.properties.MandatoryParameter;
import com.gdn.x.product.service.properties.MandatoryParameterDefaultProperties;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Configuration
public class MandatoryParameterHelper {

  String DEFAULT_USERNAME = "username";
  String SYSTEM = "system";

  @Autowired
  private Tracer tracer;

  @Autowired
  private TracerHelperImpl traceHelper;

  @Autowired
  private MandatoryParameterDefaultProperties mandatoryParameterDefaultProperties;

  @Value("${default.mongo.read.preference}")
  private String defaultMongoReadPreference;

  public MandatoryParameter get() {
    return MandatoryParameter.builder().storeId(getStoreId()).requestId(getRequestId()).channelId(getChannelId())
        .clientId(getClientId()).username(getUsername()).build();
  }

  public String getStoreId() {
    return getValueFromTracer(MandatoryParameterConstants.STORE_ID, mandatoryParameterDefaultProperties.getStoreId());
  }

  public String getChannelId() {
    return getValueFromTracer(MandatoryParameterConstants.CHANNEL_ID,
        mandatoryParameterDefaultProperties.getChannelId());
  }

  public String getClientId() {
    return getValueFromTracer(MandatoryParameterConstants.CLIENT_ID, mandatoryParameterDefaultProperties.getClientId());
  }

  public String getRequestId() {
    String defaultRequestId = UUID.randomUUID().toString();
    return getValueFromTracer(MandatoryParameterConstants.REQUEST_ID, defaultRequestId);
  }

  public String getReadPreference() {
    return getValueFromTracer(TracerFieldKey.READ_PREFERENCE.getKey(), defaultMongoReadPreference);
  }

  public String getUsername() {
    return getValueFromTracer(MandatoryParameterConstants.USERNAME, DEFAULT_USERNAME);
  }

  private String getValueFromTracer(String key, String defaultValue) {
    if (tracer == null || tracer.currentSpan() == null) {
      return defaultValue;
    }
    return StringUtils.defaultIfBlank(
      traceHelper.getBaggage(key, tracer.currentSpan().context()), defaultValue);
  }

  public void setReadPreference(String readPreference) {
    TraceContext context = tracer.currentSpan().context();
    traceHelper.setBaggage(TracerFieldKey.READ_PREFERENCE.getKey(), readPreference, context);
  }

  public void validateAndSet(MandatoryParameter mandatoryParameter) {
    TraceContext context = tracer.currentSpan().context();
    traceHelper.setBaggage(TracerFieldKey.CLIENT_ID.getKey(), StringUtils.isNotBlank(mandatoryParameter.getClientId()) ?
        mandatoryParameter.getClientId() :
        mandatoryParameterDefaultProperties.getClientId(), context);
    traceHelper.setBaggage(TracerFieldKey.STORE_ID.getKey(), StringUtils.isNotBlank(mandatoryParameter.getStoreId()) ?
        mandatoryParameter.getStoreId() :
        mandatoryParameterDefaultProperties.getStoreId(), context);
    traceHelper.setBaggage(TracerFieldKey.CLIENT_ID.getKey(), StringUtils.isNotBlank(mandatoryParameter.getClientId()) ?
        mandatoryParameter.getClientId() :
        mandatoryParameterDefaultProperties.getClientId(), context);
    traceHelper.setBaggage(TracerFieldKey.REQUEST_ID.getKey(),
        StringUtils.isNotBlank(mandatoryParameter.getRequestId()) ?
            mandatoryParameter.getRequestId() :
            mandatoryParameterDefaultProperties.getUsername() + "-" + context.traceIdString(), context);
    if (mandatoryParameter.getUsername() != null) {
      traceHelper.setBaggage(TracerFieldKey.USER_NAME.getKey(),
          StringUtils.isNotBlank(mandatoryParameter.getUsername()) ?
              mandatoryParameter.getUsername() :
              mandatoryParameterDefaultProperties.getUsername(), context);
    }
  }
}
