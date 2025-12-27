package com.gdn.mta.bulk.config;

import java.util.HashMap;
import java.util.Map;

import com.gdn.mta.bulk.service.download.BulkDownloadAutoApprovedProductsServiceBean;
import com.gdn.mta.bulk.service.download.BulkDownloadIprProductsServiceBean;
import com.gdn.mta.bulk.service.download.BulkDownloadMasterSkuInReviewServiceBean;
import com.gdn.mta.bulk.service.download.BulkProductUPCDataServiceBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.gdn.mta.bulk.factory.BulkProcessDataFactory;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.service.download.BulkBrandAuthorizationDataServiceBean;
import com.gdn.mta.bulk.service.download.BulkCampaignProductDataServiceBean;
import com.gdn.mta.bulk.service.download.BulkConfigurationCategorySummaryServiceBean;
import com.gdn.mta.bulk.service.download.BulkConfigurationMerchantSummaryServiceBean;
import com.gdn.mta.bulk.service.download.BulkDownloadMasterProductServiceBean;
import com.gdn.mta.bulk.service.download.BulkDownloadMasterSkuReviewItemsServiceBean;
import com.gdn.mta.bulk.service.download.BulkDownloadReviewProductServiceBean;
import com.gdn.mta.bulk.service.download.BulkDownloadTaggedProductsServiceBean;
import com.gdn.mta.bulk.service.download.BulkDownloadVendorSummaryServiceBean;
import com.gdn.mta.bulk.service.download.BulkInstantPickupProductDataServiceBean;
import com.gdn.mta.bulk.service.download.BulkInternalFailedProductsServiceBean;
import com.gdn.mta.bulk.service.download.BulkOrderDataServiceBean;
import com.gdn.mta.bulk.service.download.BulkPDTDataServiceBean;
import com.gdn.mta.bulk.service.download.BulkPriceRecommendationServiceBean;
import com.gdn.mta.bulk.service.download.BulkProcessDataService;
import com.gdn.mta.bulk.service.download.BulkProductBasicInfoDataServiceBean;
import com.gdn.mta.bulk.service.download.BulkProductDataServiceBean;
import com.gdn.mta.bulk.service.download.BulkRecatFailedProductsServiceBean;
import com.gdn.mta.bulk.service.download.BulkSelectedMasterProductsDownloadServiceBean;
import com.gdn.mta.bulk.service.download.BulkStoreCopyProductsServiceBean;
import com.gdn.mta.bulk.service.download.BulkStoreCopyUploadTemplateServiceBean;

@Configuration
public class BulkProcessDataFactoryConfiguration {

  @Autowired
  private BulkOrderDataServiceBean bulkOrderDataServiceBean;

  @Autowired
  private BulkProductDataServiceBean bulkProductDataServiceBean;

  @Autowired
  private BulkProductUPCDataServiceBean bulkProductUPCDataServiceBean;

  @Autowired
  private BulkPDTDataServiceBean bulkPDTDataServiceBean;

  @Autowired
  private BulkCampaignProductDataServiceBean bulkCampaignProductDataServiceBean;

  @Autowired
  private BulkDownloadMasterProductServiceBean bulkDownloadMasterProductServiceBean;

  @Autowired
  private BulkSelectedMasterProductsDownloadServiceBean bulkSelectedMasterProductsDownloadServiceBean;

  @Autowired
  private BulkDownloadReviewProductServiceBean bulkDownloadReviewProductServiceBean;

  @Autowired
  private BulkDownloadVendorSummaryServiceBean bulkDownloadVendorSummaryServiceBean;

  @Autowired
  private BulkConfigurationMerchantSummaryServiceBean bulkConfigurationMerchantSummaryServiceBean;

  @Autowired
  private BulkConfigurationCategorySummaryServiceBean bulkConfigurationCategorySummaryServiceBean;

  @Autowired
  private BulkInstantPickupProductDataServiceBean bulkInstantPickupProductDataServiceBean;

  @Autowired
  private BulkRecatFailedProductsServiceBean bulkRecatFailedProductsServiceBean;

  @Autowired
  private BulkInternalFailedProductsServiceBean bulkInternalFailedProductsServiceBean;

  @Autowired
  private BulkStoreCopyProductsServiceBean bulkStoreCopyProductsServiceBean;

  @Autowired
  private BulkStoreCopyUploadTemplateServiceBean bulkStoreCopyUploadTemplateServiceBean;

  @Autowired
  private BulkBrandAuthorizationDataServiceBean bulkBrandAuthorizationDataServiceBean;

  @Autowired
  private BulkDownloadMasterSkuReviewItemsServiceBean bulkDownloadMasterSkuReviewItemsServiceBean;

  @Autowired
  private BulkDownloadMasterSkuInReviewServiceBean bulkDownloadMasterSkuInReviewServiceBean;

  @Autowired
  private BulkDownloadAutoApprovedProductsServiceBean bulkDownloadAutoApprovedProductsServiceBean;

  @Autowired
  private BulkPriceRecommendationServiceBean bulkPriceRecommendationServiceBean;

  @Autowired
  private BulkDownloadTaggedProductsServiceBean bulkDownloadTaggedProductsServiceBean;

  @Autowired
  private BulkDownloadIprProductsServiceBean bulkDownloadIprProductsServiceBean;

  @Autowired
  private BulkProductBasicInfoDataServiceBean bulkProductBasicInfoDataServiceBean;

  @Bean
  public BulkProcessDataFactory bulkProcessDataFactory() throws Exception {
    BulkProcessDataFactory bulkProcessDataFactory = new BulkProcessDataFactory();
    Map<BulkProcessEntity, BulkProcessDataService> bulkDataService = new HashMap<>();
    bulkDataService.put(BulkProcessEntity.ORDER, bulkOrderDataServiceBean);
    bulkDataService.put(BulkProcessEntity.PRODUCT, bulkProductDataServiceBean);
    bulkDataService.put(BulkProcessEntity.PRODUCT_EAN,bulkProductUPCDataServiceBean);
    bulkDataService.put(BulkProcessEntity.PRODUCT_VENDOR, bulkPDTDataServiceBean);
    bulkDataService.put(BulkProcessEntity.CAMPAIGN_PRODUCT, bulkCampaignProductDataServiceBean);
    bulkDataService.put(BulkProcessEntity.MASTER_PRODUCT, bulkDownloadMasterProductServiceBean);
    bulkDataService.put(BulkProcessEntity.SELECTED_MASTER_PRODUCTS, bulkSelectedMasterProductsDownloadServiceBean);
    bulkDataService.put(BulkProcessEntity.MASTER_SKU_IN_REVIEW_DOWNLOAD,
      bulkDownloadMasterSkuInReviewServiceBean);
    bulkDataService.put(BulkProcessEntity.REVIEW_PRODUCTS, bulkDownloadReviewProductServiceBean);
    bulkDataService.put(BulkProcessEntity.VENDOR_FILTERED_PRODUCT, bulkDownloadVendorSummaryServiceBean);
    bulkDataService.put(BulkProcessEntity.CONFIGURATION_MERCHANT_SUMMARY, bulkConfigurationMerchantSummaryServiceBean);
    bulkDataService.put(BulkProcessEntity.CONFIGURATION_CATEGORY_SUMMARY, bulkConfigurationCategorySummaryServiceBean);
    bulkDataService.put(BulkProcessEntity.INSTANT_PICKUP_PRODUCT, bulkInstantPickupProductDataServiceBean);
    bulkDataService.put(BulkProcessEntity.RECAT_FAILED_PRODUCTS, bulkRecatFailedProductsServiceBean);
    bulkDataService.put(BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS, bulkInternalFailedProductsServiceBean);
    bulkDataService.put(BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS, bulkInternalFailedProductsServiceBean);
    bulkDataService.put(BulkProcessEntity.STORE_COPY_PRODUCTS, bulkStoreCopyProductsServiceBean);
    bulkDataService.put(BulkProcessEntity.STORE_COPY_UPLOAD_TEMPLATE, bulkStoreCopyUploadTemplateServiceBean);
    bulkDataService.put(BulkProcessEntity.BRAND_AUTHORIZATION_DOWNLOAD, bulkBrandAuthorizationDataServiceBean);
    bulkDataService.put(BulkProcessEntity.MASTER_SKU_ALL_ITEMS_DOWNLOAD, bulkDownloadMasterSkuReviewItemsServiceBean);
    bulkDataService.put(BulkProcessEntity.MASTER_SKU_SELECTED_ITEMS_DOWNLOAD,
        bulkDownloadMasterSkuReviewItemsServiceBean);
    bulkDataService.put(BulkProcessEntity.AUTO_APPROVED_PRODUCTS_ALL_DOWNLOAD,
        bulkDownloadAutoApprovedProductsServiceBean);
    bulkDataService.put(BulkProcessEntity.AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD,
        bulkDownloadAutoApprovedProductsServiceBean);
    bulkDataService.put(BulkProcessEntity.BULK_PRICE_RECOMMENDATION,bulkPriceRecommendationServiceBean);
    bulkDataService.put(BulkProcessEntity.BULK_DOWNLOAD_TAGGED_PRODUCTS, bulkDownloadTaggedProductsServiceBean);
    bulkDataService.put(BulkProcessEntity.IPR_PRODUCTS_DOWNLOAD_ALL, bulkDownloadIprProductsServiceBean);
    bulkDataService.put(BulkProcessEntity.PRODUCT_BASIC_INFO, bulkProductBasicInfoDataServiceBean);
    bulkProcessDataFactory.setBulkDataService(bulkDataService);
    return bulkProcessDataFactory;
  }
}
