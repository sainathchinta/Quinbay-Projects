package com.gdn.partners.pcu.master.client.helper;

import java.util.Objects;
import java.util.UUID;

import io.micrometer.tracing.TraceContext;
import io.micrometer.tracing.Tracer;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

import com.gdn.partners.pcu.master.model.ClientParameter;
import com.gdn.partners.pcu.master.model.ClientParameterConstants;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.TracerFieldKey;
import com.gdn.partners.pcu.master.properties.ClientParameterProperties;
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

  public String getUsername() {
    return getValueFromTracer(ClientParameterConstants.USERNAME, clientParameterProperties.getUsername());
  }

  public String isExternal() {
    return getValueFromTracer(Constants.IS_EXTERNAL, String.valueOf(false));
  }

  public String getBusinessPartnerCode() {
    return getValueFromTracer(ClientParameterConstants.BUSINESS_PARTNER_CODE,
        clientParameterProperties.getBusinessPartnerCode());
  }

  private String getValueFromTracer(String key, String defaultValue) {
    if (tracer == null || tracer.currentSpan() == null) {
      return defaultValue;
    }
    return StringUtils.defaultIfBlank(tracerHelper.getBaggage(key, tracer.currentSpan().context(), tracer), defaultValue);
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
  }

}
