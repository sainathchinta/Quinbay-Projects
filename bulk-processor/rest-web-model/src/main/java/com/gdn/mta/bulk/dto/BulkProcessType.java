package com.gdn.mta.bulk.dto;

import static com.gdn.mta.bulk.BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;


/**
 * Created by hardikbohra on 05/06/18.
 */
public enum BulkProcessType {
  PRODUCT_LEVEL_3("ProductLevel3"),
  PRODUCT_LEVEL_3_UPDATE_PRIORITY_1("ProductLevel3UpdatePriority1"),
  PRODUCT_LEVEL_3_UPDATE_PRIORITY_2("ProductLevel3UpdatePriority2"),
  PRODUCT_BASIC_INFO("ProductBasicInfo"),
  PRODUCT_BASIC_INFO_PRIORITY_1("ProductBasicInfoPriority1"),
  PRODUCT_BASIC_INFO_PRIORITY_2("ProductBasicInfoPriority2"),
  EAN_PRODUCT_LEVEL_4("EANProductLevel4"),
  VENDOR_BULK_ASSIGN("VENDOR-BULK-ASSIGN"),
  RECATEGORIZATION("Recategorization"),
  INSTANT_PICKUP_PRODUCT("InstantPickupProduct"),
  PRODUCT_LEVEL_3_GENERIC("ProductLevel3Generic"),
  CAMPAIGN("Campaign"),
  PRODUCT_CREATION_UPLOAD("ProductCreationUpload"),
  CONVERTED_PRODUCT_CREATION_UPLOAD("ConvertedProductCreationUpload"),
  EXTERNAL_CREATION_UPLOAD("ExternalProductCreationUpload"),
  SUBJECT_TO_VAT("SubjectToVat"),
  SUBJECT_TO_VAT_ERROR("SubjectToVatError"),
  INSTANT_PICKUP_PRODUCT_UPSERT("InstantPickupProductUpsert"),
  INSTANT_PICKUP_PRODUCT_DELETE("InstantPickupProductDelete"),
  IN_STORE("InStore"),
  ARCHIVE("Archive"),
  PICKUP_POINT_DELETE_ERROR("PickupPointDeleteError"),
  DOWNLOAD_FAILED_UPSERT_MPP_PRODUCT("FailedUpsertMppProduct"),
  PRODUCT_CREATION_FAILED_DIR("ProductCreationFailedDir"),
  PRODUCT_SUSPENSION("suspension"),
  CONFIGURATION("config"),
  CAMPAIGN_ERROR("CampaignError"),
  MASTER_PRODUCT("MasterProduct"),
  STORE_COPY("StoreCopy"),
  SUSPENSION_ERROR("SuspensionError"),
  CONFIGURATION_ERROR("ConfigurationError"),
  RECAT_ERROR("RecatError"),
  UNCATEGORISED_SKU("UncategorisedSku"),
  INTERNAL_UPLOAD("internal"),
  DELETE_PICKUP_POINT("DeletePickupPoint"),
  STORE_COPY_TEMPLATE("StoreCopyTemplate"),
  PRODUCT_CREATION_UPLOAD_PRIORITY("ProductCreationUploadPriority"),
  PRODUCT_CREATION_UPLOAD_PRIORITY_1("ProductCreationUploadPriority1"),
  PRODUCT_CREATION_UPLOAD_PRIORITY_2("ProductCreationUploadPriority2"),
  QR_GENERATION("QrGeneration"),
  ARCHIVE_ERROR("ArchiveError"),
  ASSEMBLY_REQUEST("ASSEMBLY_REQUEST"),
  DISASSEMBLY_REQUEST("DISASSEMBLY_REQUEST"),
  TRANSFER_REQUEST("TRANSFER_REQUEST"),
  WORK_ORDER_ERROR("WorkOrderError"),
  MASTER_SKU_REVIEW_ITEMS_DOWNLOAD("MasterSkuReviewItemsDownload"),
  MASTER_SKU_IN_REVIEW_DOWNLOAD("MasterSkuAnchorsDownload"),
  AUTO_APPROVED_PRODUCTS_DOWNLOAD("AutoApprovedProductsDownload"),

  BULK_PRICE_REBATE("BULK_PRICE_REBATE"),
  BULK_PRICE_PRODUCT_TYPE_TAGGING("BULK_PRICE_PRODUCT_TYPE_TAGGING"),
  IPR_PRODUCTS_DOWNLOAD_ALL("IPR_PRODUCTS_DOWNLOAD_ALL");

  private String value;

  private BulkProcessType(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public static BulkProcessType getBulkProcessType(String process) {
    return Arrays.stream(BulkProcessType.values())
        .filter(element -> StringUtils.equalsIgnoreCase(element.getValue(), process)).findAny().orElse(null);
  }
}
