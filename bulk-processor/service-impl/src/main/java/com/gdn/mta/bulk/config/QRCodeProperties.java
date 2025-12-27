package com.gdn.mta.bulk.config;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

@Data
@Configuration
@ConfigurationProperties(value = "qr-code")
public class QRCodeProperties {
  private String prefixUrlMerchant;
  private String prefixUrlProduct;
  private String blibliPinBig;
  private String blibliPinSmall;
  private String localTempDirectory = StringUtils.EMPTY;
  private Map<String, String> qrConfigToTemplateName = new HashMap<>();
  private String prefixUrlAddToBag;
}
