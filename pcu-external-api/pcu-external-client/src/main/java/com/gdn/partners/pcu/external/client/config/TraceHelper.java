package com.gdn.partners.pcu.external.client.config;

import io.micrometer.tracing.Tracer;

public interface TraceHelper {

  String getBaggage(String key, Tracer tracer);

  void setBaggage(String key, String value, Tracer tracer);
}