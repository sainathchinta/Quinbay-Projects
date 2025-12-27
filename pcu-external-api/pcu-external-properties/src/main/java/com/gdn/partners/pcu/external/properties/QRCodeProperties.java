package com.gdn.partners.pcu.external.properties;


import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties(value = "qr-code")
public class QRCodeProperties {

  private String basePath;
  private String templatePath;
  private String prefixUrlMerchant;
  private String prefixUrlProduct;
  private String blibliPinBig;
  private String blibliPinSmall;
  private String templateDarkA1;
  private String templateDarkA5;
  private String templateDarkA1CnC;
  private String templateDarkA5CnC;
  private String templateLightA1;
  private String templateLightA5;
  private String templateLightA1Cnc;
  private String templateLightA5Cnc;
  private String templateDarkBY46;
  private String templateDarkBY712;
  private String templateLightBY46;
  private String templateLightBY712;
}
