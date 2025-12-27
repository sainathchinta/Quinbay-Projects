package com.gdn.mta.bulk.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Data;

/**
 * Created by akashkumargautam on 16/01/24.
 */

@Data
@ConfigurationProperties(value = "configuration.kafka.topic")
@Component
public class KafkaTopicProperties {
  // bulk kafka events configurable topics
  private String bulkCreateEvent;
  private String bulkCreatePriorityEvent;
  private String bulkGenericCreateEvent;
  private String bulkGenericCreatePriorityEvent;
  private String createProductEvent;
  private String bulkApiCreateProductEvent;
  private String bulkApiCreateProductV2Event;
  private String bulkCreateDownloadImageEvent;
  private String bulkCreateDownloadImagePriority1;
  private String bulkCreateDownloadImagePriority2;
  private String bulkGenericCreateProductEvent;
  private String bulkGenericCreateProductForPriority1Event;
  private String bulkGenericCreateProductForPriority2Event;
  private String bulkCnCreateProductEvent;
  private String bulkCnCreateProductForPriority1Event;
  private String bulkCnCreateProductForPriority2Event;
  private String analyticBwaEventPublish;
  private String bulkProductDownloadEvent;
  private String bulkDownloadAllEvent;
  private String bulkUploadEvent;
  private String bulkUploadEANEvent;
  private String bulkUpdatePriority1Event;
  private String internalUserBulkUploadEvent;
  private String bulkUploadCampaignEvent;
  private String bulkUploadInstantPickupProductEvent;
  private String bulkUploadDeleteInstantPickupProductEvent;
  private String bulkProcessImageApiToMtaEvent;
  private String bulkProcessImageApiToMtaEventV2;
  private String updateProductDetailEvent;
  private String bulkUpdateProductDetailEvent;
  private String updateProductSummaryEvent;
  private String bulkUpdateProductSummaryEvent;
  private String bulkApiProcessImageV1Event;
  private String bulkApiProcessImageV2Event;
  private String bulkUpdateProductDetailResponseEvent;
  private String bulkProcessImageResponseEvent;
  private String bulkCreateProductResponseEvent;
  private String bulkCreateProductV2QueueFeedEvent;
  private String categoryToBusinessPartnerMappingEvent;
  private String categoryToProductCodeMappingEvent;
  private String categoryToProductSkuMappingEvent;
  private String productSkuToSalesCatalogMappingEvent;
  private String bulkArchiveItems;
  private String bulkArchiveProducts;
  private String bulkUpdateOff2On;
  private String bulkProductSuspension;
  private String bulkAssignVendorProduct;
  private String bulkConfigurationUpdate;
  private String bulkConfigurationUpdateRows;
  private String downloadUnmappedSkus;
  private String productRecatProcess;
  private String subjectToVatUploadEvent;
  private String dormantSellerProductDeactivateEvent;
  private String dormantSellerItemSkuViewConfigUpdate;
  private String bulkUploadUpdateItem;
  private String bulkUploadUpdateItemPriority1;
  private String bulkUploadUpdateItemPriority2;
  private String bulkUploadUpdateEANItem;
  private String bulkDeleteItemPickupPoint;
  private String bulkUploadCampaignItem;
  private String bulkUpsertInstantPickupItemEvent;
  private String bulkDeleteInstantPickupItemEvent;
  private String bulkUploadInstoreUpdate;
  private String storeCopyProductCreationDetails;

  // work order topics
  private String bulkWorkOrderUploadEvent;
  private String workOrderEvent;

  // update sales category topic
  private String updateSalesCategoryDetails;

  private String internalBulkUploadDetails;
  private String bulkArchiveProductRows;
  private String bulkProductSuspensionEvent;
  private String bulkVendorAssignmentEvent;
  private String bulkVatUpdateEvent;
  private String deleteBrandAuthorisation;
  private String deletePickupPointResponseEvent;
  private String updateFields;

  // fbb event topics
  private String fbbCreateConsignment;
  private String fbbCreateL5;
  private String fbbL5UpdateRows;
  private String fbbCreateConsignmentResult;

  // vendor Auto Assignment topic
  private String vendorAutoAssignmentEvent;

  // restricted keywords event topics
  private String restrictedKeywordBulkProcess;
  private String bulkRestrictedKeywordUploadEvent;

  // brand auth event topics
  private String bulkBrandAuthUploadEvent;
  private String brandAuthorisationBulkProcess;

  private String bulkReviewUploadEvent;
  private String bulkApprovalRejectionProcessEvent;

  // Generate QR codes event topics
  private String generateQrCodeStore;
  private String generateQrCodeProduct;
  private String generateQrCodeRow;
  private String generateQrCodeExcel;

  private String brandUpdateEvent;
  private String bulkDataDeletionEvent;

  // kafka topic for  master-sku review
  private String bulkAssigneeMasterSkuProcessEvent;
  private String bulkMasterSkuReviewProcessEvent;

  //Auto approved products topics
  private String autoApprovedProductsBulkAssignProcessEvent;

  // Bulk Price update
  private String bulkInternalPriceUpdateEvent;

  // Auto start up
  private boolean autoStartup;

  // Price Rebate
  private String bulkPriceRebateUpload;

  //Bulk Product Type tagging
  private String bulkProductTypeTaggingUpdateEvent;

  //Bulk IPR Product Add Review
  private String bulkIprPortalAddReviewProcessEvent;

  // SKU Level Rebate
  private String bulkSkuLevelRebateUpload;

  // Bulk New Price Update
  private String bulkPriceUpdateNewEvent;

  // Business partner update
  private String businessPartnerUpdateEvent;

  //Need revision Deletion Events
  private String needRevisionDeletionEvent;
  //Basic info update upload events
  private String bulkBasicInfoUploadPriority1Event;
  private String bulkBasicInfoUploadPriority2Event;

  //Basic info update event
  private String bulkBasicInfoUpdateEvent;
  private String bulkBasicInfoUpdatePriority1Event;
  private String bulkBasicInfoUpdatePriority2Event;

  private String bulkBasicInfoDownloadImageEvent;
  private String bulkBasicInfoDownloadImagePriority1;
  private String bulkBasicInfoDownloadImagePriority2;

  // To video service
  private String bulkBasicInfoDownloadVideoEvent;
  private String bulkBasicInfoDownloadVideoPriority1;
  private String bulkBasicInfoDownloadVideoPriority2;

  // From video service after downloading
  private String bulkBasicInfoDownloadVideoResponseEvent;

  // Business partner data change event
  private String businessPartnerChangeEvent;

  // Internal brand update
  private String internalBrandUpdateEvent;

  // Shopee Events
  private String bulkConvertedProductCreationEvent;
  private String bulkGenericCreateProductForConvertedUpload;
  private String bulkExternalCreateEvent;
  private String bulkExternalCreateDownloadImageEvent;
  private String externalUploadCategoryAndBrandPrediction;
  private String bulkGenericCreateProductForExternalUpload;
}
