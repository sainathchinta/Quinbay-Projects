package com.gdn.pbp.property;

import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.mta.product.enums.TracerFieldKey;
import com.gdn.mta.product.service.config.MandatoryParameter;
import com.gdn.mta.product.service.config.TraceHelper;
import brave.Tracer;
import brave.propagation.TraceContext;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@AllArgsConstructor
@Component
public class MandatoryParameterHelper {

  String DEFAULT_USERNAME = "username";

  @Autowired
  private Tracer tracer;

  @Autowired
  private TraceHelper traceHelper;

  @Autowired
  private MandatoryParameterDefaultProperties mandatoryParameterDefaultProperties;

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


  public void validateAndSetDataSource(DataSourceType dataSource) {
    TraceContext context = tracer.currentSpan().context();
    traceHelper.setBaggage(TracerFieldKey.DATA_SOURCE.getKey(), dataSource.name(), context);
  }

  public String getDataSource() {
    return getValueFromTracer(TracerFieldKey.DATA_SOURCE.getKey(), DataSourceType.PRIMARY.name());
  }

  public void validateAndSet(MandatoryParameter mandatoryParameter) {
    TraceContext context = tracer.currentSpan().context();
    traceHelper.setBaggage(MandatoryParameterConstants.CLIENT_ID,
        StringUtils.isNotBlank(mandatoryParameter.getClientId()) ?
            mandatoryParameter.getClientId() :
            mandatoryParameterDefaultProperties.getClientId(), context);
    traceHelper.setBaggage(MandatoryParameterConstants.STORE_ID,
        StringUtils.isNotBlank(mandatoryParameter.getStoreId()) ?
            mandatoryParameter.getStoreId() :
            mandatoryParameterDefaultProperties.getStoreId(), context);
    traceHelper.setBaggage(MandatoryParameterConstants.CLIENT_ID,
        StringUtils.isNotBlank(mandatoryParameter.getClientId()) ?
            mandatoryParameter.getClientId() :
            mandatoryParameterDefaultProperties.getClientId(), context);
    traceHelper.setBaggage(MandatoryParameterConstants.REQUEST_ID,
        StringUtils.isNotBlank(mandatoryParameter.getRequestId()) ?
            mandatoryParameter.getRequestId() :
            mandatoryParameterDefaultProperties.getUsername() + "-" + context.traceIdString(), context);
    if (mandatoryParameter.getUsername() != null) {
      traceHelper.setBaggage(MandatoryParameterConstants.USERNAME,
          StringUtils.isNotBlank(mandatoryParameter.getUsername()) ?
              mandatoryParameter.getUsername() :
              mandatoryParameterDefaultProperties.getUsername(), context);
    }
  }

  public void setParameter(String parameter, String value) {
    TraceContext context = tracer.currentSpan().context();
    traceHelper.setBaggage(parameter, value, context);
  }

}