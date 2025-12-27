package com.gdn.partners.pcu.internal.client.helper;


import io.micrometer.tracing.TraceContext;
import io.micrometer.tracing.Tracer;

public interface TracerHelper {
  String getBaggage(String key, TraceContext context, Tracer tracer);

  void setBaggage(String key, String value, TraceContext context, Tracer tracer);
}
