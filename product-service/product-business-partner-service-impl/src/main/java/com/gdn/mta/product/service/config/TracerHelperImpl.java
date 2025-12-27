package com.gdn.mta.product.service.config;

import org.springframework.stereotype.Service;

import brave.baggage.BaggageField;
import brave.propagation.TraceContext;

@Service
public class TracerHelperImpl implements TraceHelper {

  @Override
  public String getBaggage(String key, TraceContext context){
    return BaggageField.getByName(context, key).getValue(context);
  }

  @Override
  public void setBaggage(String key, String value, TraceContext context) {
    BaggageField baggageField = BaggageField.getByName(key);
    baggageField.updateValue(value);
  }

}
