package com.gdn.mta.product.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import lombok.Data;

@Configuration
@Data
public class ApplicationProperties {

  @Value("${application.product.sku.digits}")
  private String skuDigits;

  @Value("${application.max.stock.alert.attempt}")
  private String maxStockAlertAttempt;

  @Value("${application.mta.url}")
  private String mtaUrl;

  @Value("${application.batch_update.size}")
  private String batchUpdateSize;

  @Value("${application.merchant_product_mv_full_indexing.batch_size}")
  private int batchFullIndexingSize;

  @Value("${application.merchant_product_mv_partial_indexing.batch_size}")
  private int batchPartialIndexingSize;

  @Value("${application.xproduct.gdn_sku_request_max_size}")
  private String gdnSkuRequestMaxSize;

  @Value("${application.product_solr_category_code_search_rows}")
  private String rowsCategoryCodeSolrSearch;

  @Value("${application.product_solr_product_code_search_rows}")
  private String rowsProductCodeSolrSearch;

  @Value("${application.product_reject.max_days}")
  private String productRejectMaxDays;

  @Value("${application.product.url}")
  private String QRCodeUrlSource;

  @Value("${application.product.detail.page.url.prefix:http://blibli.com/product-detail}")
  private String productDetailPageUrlPrefix;

  @Value("${application.product.is_catalog_code_required}")
  private boolean catalogCodeRequired;

  @Value("${application.directory_stock_alerts_excel}")
  private String directoryStockAlertsExcel;

  @Value("${application.days_to_check_oos_for}")
  private int daysToCheckOOSDateFor;

  @Value("${wn.kafka.create-notification-topic}")
  private String webNotifNotificationCreateTopic;

  @Value("${product.rejection.alert.email.subject}")
  private String productRejectionAlertEmailSubject;

  @Value("${product.event.alert.email.from}")
  private String productEventAlertEmailFrom;

  @Value("${inventory.api.batch.size}")
  private int inventoryApiBatchSize;

  private boolean production = true;
}