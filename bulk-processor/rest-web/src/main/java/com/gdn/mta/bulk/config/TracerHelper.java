package com.gdn.mta.bulk.config;

import org.springframework.stereotype.Component;

import brave.baggage.BaggageField;
import brave.propagation.TraceContext;

@Component
public class TracerHelper {

  public String getBaggage(String key, TraceContext context){
    return BaggageField.getByName(context, key).getValue(context);
  }

  public void setBaggage(String key, String value, TraceContext context) {
    BaggageField baggageField = BaggageField.getByName(key);
    baggageField.updateValue(value);
  }
}


