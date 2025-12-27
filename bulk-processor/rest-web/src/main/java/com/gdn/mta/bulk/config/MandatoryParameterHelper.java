package com.gdn.mta.bulk.config;

import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.gdn.mta.bulk.property.MandatoryParameterDefaultProperties;

import brave.Tracer;
import brave.propagation.TraceContext;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@AllArgsConstructor
public class MandatoryParameterHelper {

  private String system;

  @Autowired
  private Tracer tracer;

  @Autowired
  private TracerHelper tracerHelper;

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
    return getValueFromTracer(MandatoryParameterConstants.USERNAME, system);
  }

  private String getValueFromTracer(String key, String defaultValue) {
    if (tracer == null || tracer.currentSpan() == null) {
      return defaultValue;
    }
    return StringUtils.defaultIfBlank(
      tracerHelper.getBaggage(key, tracer.currentSpan().context()), defaultValue);
  }

  public void validateAndSet(MandatoryParameter mandatoryParameter) {
    TraceContext context = tracer.currentSpan().context();
    tracerHelper.setBaggage(TracerFieldKey.CHANNEL_ID.getKey(),
      StringUtils.isNotBlank(mandatoryParameter.getChannelId()) ?
        mandatoryParameter.getChannelId() :
        mandatoryParameterDefaultProperties.getChannelId(), context);
    tracerHelper.setBaggage(TracerFieldKey.STORE_ID.getKey(),
      StringUtils.isNotBlank(mandatoryParameter.getStoreId()) ?
        mandatoryParameter.getStoreId() :
        mandatoryParameterDefaultProperties.getStoreId(), context);
    tracerHelper.setBaggage(TracerFieldKey.CLIENT_ID.getKey(),
      StringUtils.isNotBlank(mandatoryParameter.getClientId()) ?
        mandatoryParameter.getClientId() :
        mandatoryParameterDefaultProperties.getClientId(), context);
    tracerHelper.setBaggage(TracerFieldKey.REQUEST_ID.getKey(),
      StringUtils.isNotBlank(mandatoryParameter.getRequestId()) ?
        mandatoryParameter.getRequestId() :
        mandatoryParameterDefaultProperties.getUsername() + "-" + context.traceIdString(), context);
    if (mandatoryParameter.getUsername() != null) {
      tracerHelper.setBaggage(TracerFieldKey.USER_NAME.getKey(),
        StringUtils.isNotBlank(mandatoryParameter.getUsername()) ?
          mandatoryParameter.getUsername() :
        mandatoryParameterDefaultProperties.getUsername(), context);
    }
  }

  public void validateAndSetDataSource(DataSourceType dataSource) {
    TraceContext context = tracer.currentSpan().context();
    tracerHelper.setBaggage(TracerFieldKey.DATA_SOURCE.getKey(), dataSource.name(), context);
  }

  public String getDataSource() {
    return getValueFromTracer(TracerFieldKey.DATA_SOURCE.getKey(), DataSourceType.PRIMARY.name());
  }
}
