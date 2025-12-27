package com.gdn.partners.pcu.external.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

/**
 * Created by govind on 12/12/2018 AD.
 */
@Data
@ConfigurationProperties("system.parameter")
public class SystemParameterProperties {

  private String defaultMerchantInternationalOriginId;
  private String directoryBrandlogoSource;
  private String directoryProfilebannerSource;
  private String directoryStockAlertsExcel;
  private String directoryBrandLogoFinal;
  private String directoryProfileBannerFinal;
  private String directoryVatFilepath;
  private String gcsBulkBasePath;
  private String fileStoreBulkBasePath;
  private String archieveTemplate;
  private String instoreTemplate;
  private String l5DeleteTemplate;
  private String vatTemplate;
  private String logisticsTemplate;
}
