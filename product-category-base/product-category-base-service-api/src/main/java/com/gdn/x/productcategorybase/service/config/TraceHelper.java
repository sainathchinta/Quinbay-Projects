package com.gdn.x.productcategorybase.service.config;

import brave.propagation.TraceContext;

public interface TraceHelper {

  String getBaggage(String key, TraceContext context);

  void setBaggage(String key, String value, TraceContext context);
}