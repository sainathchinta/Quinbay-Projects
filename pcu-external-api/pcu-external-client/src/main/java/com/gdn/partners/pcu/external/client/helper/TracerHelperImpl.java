package com.gdn.partners.pcu.external.client.helper;

import com.gdn.partners.pcu.external.client.config.TraceHelper;
import io.micrometer.tracing.Tracer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class TracerHelperImpl implements TraceHelper {

  @Autowired
  Tracer tracer;

  @Override
  public String getBaggage(String key, Tracer tracer) {
    return tracer.getBaggage(key).get();
  }

  @Override
  public void setBaggage(String key, String value, Tracer tracer) {
    tracer.createBaggage(key).set(value);
  }
}