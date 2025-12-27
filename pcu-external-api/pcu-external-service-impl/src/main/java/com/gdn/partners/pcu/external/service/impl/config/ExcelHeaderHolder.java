package com.gdn.partners.pcu.external.service.impl.config;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ExcelHeaderHolder {

  private String headerValue;
  private boolean mandatory;
  private String cellType;
  private String headerInfo;


  public ExcelHeaderHolder(String headerValue, boolean mandatory) {
    super();
    this.headerValue = headerValue;
    this.mandatory = mandatory;
  }

  public ExcelHeaderHolder(String headerValue, boolean mandatory, String headerInfo) {
    super();
    this.headerValue = headerValue;
    this.mandatory = mandatory;
    this.headerInfo = headerInfo;
  }
}