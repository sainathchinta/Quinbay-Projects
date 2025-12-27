package com.gdn.partners.pcu.internal.client.helper;


import java.util.Objects;
import java.util.UUID;

import io.micrometer.tracing.TraceContext;
import io.micrometer.tracing.Tracer;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

import com.gdn.partners.pcu.internal.client.constants.ClientParameter;
import com.gdn.partners.pcu.internal.client.constants.ClientParameterConstants;
import com.gdn.partners.pcu.internal.enums.TracerFieldKey;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.ClientParameterProperties;
import lombok.Builder;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Builder
@Configuration
public class ClientParameterHelper {

  @Autowired
  Tracer tracer;

  @Autowired
  TracerHelper tracerHelper;

  @Autowired
  ClientParameterProperties clientParameterProperties;

  public ClientParameter get() {
    return ClientParameter.builder().storeId(getStoreId()).requestId(getRequestId()).channelId(getChannelId())
        .clientId(getClientId()).username(getUsername()).build();
  }

  public String getStoreId() {
    return getValueFromTracer(ClientParameterConstants.STORE_ID, clientParameterProperties.getStoreId());
  }

  public String getChannelId() {
    return getValueFromTracer(ClientParameterConstants.CHANNEL_ID, clientParameterProperties.getChannelId());
  }

  public String getClientId() {
    return getValueFromTracer(ClientParameterConstants.CLIENT_ID, clientParameterProperties.getClientId());
  }

  public String getRequestId() {
    String defaultRequestId = UUID.randomUUID().toString();
    return getValueFromTracer(ClientParameterConstants.REQUEST_ID, defaultRequestId);
  }

  public String getUserType() {
    return getValueFromTracer(ClientParameterConstants.USER_TYPE, clientParameterProperties.getUserType());
  }

  public String getBusinessPartnerCode() {
    return getValueFromTracer(ClientParameterConstants.BUSINESSPARTNER_CODE,
        clientParameterProperties.getBusinessPartnerCode());
  }

  public String getVendorCode() {
    return getValueFromTracer(ClientParameterConstants.VENDOR_CODE, clientParameterProperties.getVendorCode());
  }

  public String getUsername() {
    return getValueFromTracer(ClientParameterConstants.USERNAME, clientParameterProperties.getUsername());
  }

  private String getValueFromTracer(String key, String defaultValue) {
    if (Objects.isNull(tracer) || Objects.isNull(tracer.currentSpan())) {
      return defaultValue;
    }
    return StringUtils.defaultIfBlank(
        tracerHelper.getBaggage(key, tracer.currentSpan().context(), tracer), defaultValue);
  }

  public void set(String key, String value) {
    if (Objects.nonNull(value)) {
      TraceContext context = tracer.currentSpan().context();
      tracerHelper.setBaggage(key, value, context, tracer);
    }
  }

  public void validateAndSet(ClientParameter clientParameter) {
    TraceContext context = tracer.currentSpan().context();
    tracerHelper.setBaggage(TracerFieldKey.CHANNEL_ID.getKey(), StringUtils.isNotBlank(clientParameter.getChannelId()) ?
        clientParameter.getChannelId() :
        clientParameterProperties.getChannelId(), context, tracer);
    tracerHelper.setBaggage(TracerFieldKey.STORE_ID.getKey(), StringUtils.isNotBlank(clientParameter.getStoreId()) ?
        clientParameter.getStoreId() :
        clientParameterProperties.getStoreId(), context, tracer);
    tracerHelper.setBaggage(TracerFieldKey.CLIENT_ID.getKey(), StringUtils.isNotBlank(clientParameter.getClientId()) ?
        clientParameter.getClientId() :
        clientParameterProperties.getClientId(), context, tracer);
    tracerHelper.setBaggage(TracerFieldKey.REQUEST_ID.getKey(), StringUtils.isNotBlank(clientParameter.getRequestId()) ?
        clientParameter.getRequestId() :
        clientParameterProperties.getUsername() + Constants.HYPHEN + context.traceId(), context, tracer);
    if (clientParameter.getUsername() != null) {
      tracerHelper.setBaggage(TracerFieldKey.USER_NAME.getKey(), StringUtils.isNotBlank(clientParameter.getUsername()) ?
          clientParameter.getUsername() :
          clientParameterProperties.getUsername(), context, tracer);
    }

    tracerHelper.setBaggage(TracerFieldKey.USER_TYPE.getKey(), StringUtils.isNotBlank(clientParameter.getUserType()) ?
        clientParameter.getUserType() :
        clientParameterProperties.getUserType(), context, tracer);
    tracerHelper.setBaggage(TracerFieldKey.BUSINESSPARTNER_CODE.getKey(),
        StringUtils.isNotBlank(clientParameter.getBusinessPartnerCode()) ?
            clientParameter.getBusinessPartnerCode() :
            clientParameterProperties.getBusinessPartnerCode(), context, tracer);
    tracerHelper.setBaggage(TracerFieldKey.VENDOR_CODE.getKey(),
        StringUtils.isNotBlank(clientParameter.getVendorCode()) ?
            clientParameter.getVendorCode() :
            clientParameterProperties.getVendorCode(), context, tracer);
  }

}
