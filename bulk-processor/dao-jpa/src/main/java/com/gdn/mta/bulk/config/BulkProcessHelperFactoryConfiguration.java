package com.gdn.mta.bulk.config;

import com.gdn.mta.bulk.factory.BulkProcessHelperFactory;
import com.gdn.mta.bulk.helper.AutoApprovedProductsDownloadProcessHelper;
import com.gdn.mta.bulk.helper.BulkBrandAuthDownloadProcessHelper;
import com.gdn.mta.bulk.helper.BulkCampaignProductProcessHelper;
import com.gdn.mta.bulk.helper.BulkConfigurationCategorySummaryProcessHelper;
import com.gdn.mta.bulk.helper.BulkConfigurationMerchantSummaryProcessHelper;
import com.gdn.mta.bulk.helper.BulkDownloadTaggedProductsHelper;
import com.gdn.mta.bulk.helper.BulkInstantPickupProductProcessHelper;
import com.gdn.mta.bulk.helper.BulkInternalFailedProductsProcessHelper;
import com.gdn.mta.bulk.helper.BulkMasterProductProcessHelper;
import com.gdn.mta.bulk.helper.BulkMasterSkuReviewItemsDownloadProcessHelper;
import com.gdn.mta.bulk.helper.BulkOrderProcessHelper;
import com.gdn.mta.bulk.helper.BulkPriceRecommendationProcessHelper;
import com.gdn.mta.bulk.helper.BulkProcessHelper;
import com.gdn.mta.bulk.helper.BulkProductBasicInfoProcessHelper;
import com.gdn.mta.bulk.helper.BulkProductProcessHelper;
import com.gdn.mta.bulk.helper.BulkProductUPCProcessHelper;
import com.gdn.mta.bulk.helper.BulkRecatFailedProductsProcessHelper;
import com.gdn.mta.bulk.helper.BulkReviewProductProcessHelper;
import com.gdn.mta.bulk.helper.BulkStoreCopyProcessHelper;
import com.gdn.mta.bulk.helper.BulkStoreCopyTemplateProcessHelper;
import com.gdn.mta.bulk.helper.BulkVendorProcessHelper;
import com.gdn.mta.bulk.helper.BulkVendorSummaryProcessHelper;
import com.gdn.mta.bulk.helper.DeleteUpdatePickupPointsProcessHelper;
import com.gdn.mta.bulk.helper.IprProductsDownloadProcessHelper;
import com.gdn.mta.bulk.helper.MasterSkuReviewDownloadProcessHelper;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

@Configuration
public class BulkProcessHelperFactoryConfiguration {

  @Autowired
  private BulkOrderProcessHelper bulkOrderProcessHelper;

  @Autowired
  private BulkProductProcessHelper bulkProductProcessHelper;

  @Autowired
  private BulkProductUPCProcessHelper bulkProductUPCProcessHelper;

  @Autowired
  private BulkVendorProcessHelper bulkVendorProcessHelper;

  @Autowired
  private BulkCampaignProductProcessHelper bulkCampaignProductProcessHelper;

  @Autowired
  private BulkMasterProductProcessHelper bulkMasterProductProcessHelper;

  @Autowired
  private BulkReviewProductProcessHelper bulkReviewProductProcessHelper;

  @Autowired
  private BulkVendorSummaryProcessHelper bulkVendorSummaryProcessHelper;

  @Autowired
  private BulkConfigurationMerchantSummaryProcessHelper
      bulkConfigurationMerchantSummaryProcessHelper;

  @Autowired
  private BulkConfigurationCategorySummaryProcessHelper
      bulkConfigurationCategorySummaryProcessHelper;

  @Autowired
  private BulkInstantPickupProductProcessHelper bulkInstantPickupProductProcessHelper;

  @Autowired
  private BulkRecatFailedProductsProcessHelper bulkRecatFailedProductsProcessHelper;

  @Autowired
  private BulkInternalFailedProductsProcessHelper bulkInternalFailedProductsProcessHelper;

  @Autowired
  private BulkStoreCopyProcessHelper bulkStoreCopyProcessHelper;

  @Autowired
  private BulkStoreCopyTemplateProcessHelper bulkStoreCopyTemplateProcessHelper;

  @Autowired
  private BulkBrandAuthDownloadProcessHelper bulkBrandAuthDownloadProcessHelper;

  @Autowired
  private DeleteUpdatePickupPointsProcessHelper deleteUpdatePickupPointsProcessHelper;

  @Autowired
  private BulkMasterSkuReviewItemsDownloadProcessHelper masterSkuReviewItemsDownloadProcessHelper;

  @Autowired
  private AutoApprovedProductsDownloadProcessHelper autoApprovedProductsDownloadProcessHelper;

  @Autowired
  private MasterSkuReviewDownloadProcessHelper masterSkuReviewDownloadProcessHelper;

  @Autowired
  private BulkPriceRecommendationProcessHelper bulkPriceRecommendationProcessHelper;


  @Autowired
  private BulkDownloadTaggedProductsHelper bulkDownloadTaggedProductsHelper;

  @Autowired
  private IprProductsDownloadProcessHelper iprProductsDownloadProcessHelper;

  @Autowired
  private BulkProductBasicInfoProcessHelper bulkProductBasicInfoProcessHelper;

  @Bean
  public BulkProcessHelperFactory bulkProcessHelperFactory() throws Exception {
    BulkProcessHelperFactory bulkProcessHelperFactory = new BulkProcessHelperFactory();
    Map<BulkProcessEntity, BulkProcessHelper> bulkProcessHelper = new HashMap<>();
    bulkProcessHelper.put(BulkProcessEntity.ORDER, bulkOrderProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.PRODUCT, bulkProductProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.PRODUCT_EAN,bulkProductUPCProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.PRODUCT_VENDOR, bulkVendorProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.CAMPAIGN_PRODUCT, bulkCampaignProductProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.MASTER_PRODUCT, bulkMasterProductProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.SELECTED_MASTER_PRODUCTS, bulkMasterProductProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.REVIEW_PRODUCTS, bulkReviewProductProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.VENDOR_FILTERED_PRODUCT, bulkVendorSummaryProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.CONFIGURATION_MERCHANT_SUMMARY,
        bulkConfigurationMerchantSummaryProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.CONFIGURATION_CATEGORY_SUMMARY,
        bulkConfigurationCategorySummaryProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.INSTANT_PICKUP_PRODUCT, bulkInstantPickupProductProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.RECAT_FAILED_PRODUCTS, bulkRecatFailedProductsProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS, bulkInternalFailedProductsProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS,
        bulkInternalFailedProductsProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.STORE_COPY_PRODUCTS, bulkStoreCopyProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.STORE_COPY_UPLOAD_TEMPLATE, bulkStoreCopyTemplateProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.BRAND_AUTHORIZATION_DOWNLOAD, bulkBrandAuthDownloadProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.DELETE_UPDATE_PICKUP_POINTS,deleteUpdatePickupPointsProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.MASTER_SKU_ALL_ITEMS_DOWNLOAD, masterSkuReviewItemsDownloadProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.MASTER_SKU_SELECTED_ITEMS_DOWNLOAD,
        masterSkuReviewItemsDownloadProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.MASTER_SKU_IN_REVIEW_DOWNLOAD,
      masterSkuReviewDownloadProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.AUTO_APPROVED_PRODUCTS_ALL_DOWNLOAD,
        autoApprovedProductsDownloadProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD,
        autoApprovedProductsDownloadProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.BULK_PRICE_RECOMMENDATION, bulkPriceRecommendationProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.BULK_DOWNLOAD_TAGGED_PRODUCTS, bulkDownloadTaggedProductsHelper);
    bulkProcessHelper.put(BulkProcessEntity.IPR_PRODUCTS_DOWNLOAD_ALL, iprProductsDownloadProcessHelper);
    bulkProcessHelper.put(BulkProcessEntity.PRODUCT_BASIC_INFO, bulkProductBasicInfoProcessHelper);
    bulkProcessHelperFactory.setBulkProcessHelper(bulkProcessHelper);
    return bulkProcessHelperFactory;
  }
}
