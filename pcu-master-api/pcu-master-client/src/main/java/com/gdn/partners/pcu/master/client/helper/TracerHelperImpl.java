package com.gdn.partners.pcu.master.client.helper;

import io.micrometer.tracing.Tracer;
import org.springframework.stereotype.Service;
import io.micrometer.tracing.TraceContext;

@Service
public class TracerHelperImpl implements TracerHelper {

  @Override
  public String getBaggage(String key, TraceContext context, Tracer tracer){
    return tracer.getBaggage(context, key).get();
  }

  @Override
  public void setBaggage(String key, String value, TraceContext context, Tracer tracer) {
    tracer.createBaggageInScope(context,key, value);
  }

}
