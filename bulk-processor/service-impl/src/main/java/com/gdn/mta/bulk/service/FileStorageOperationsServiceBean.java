package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.dto.BulkProcessPath;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;

import static com.gdn.mta.bulk.dto.BulkProcessType.QR_GENERATION;
import static com.gdn.mta.bulk.dto.BulkProcessType.getBulkProcessType;

@Service("FileStorageOperationsService")
@Slf4j
public class FileStorageOperationsServiceBean implements FileStorageOperationsService {

  @Autowired
  private GCSService gcsService;

  @Value("${gcs.enabled}")
  private boolean gcsEnabled;

  @Value("${gcs.basePath}")
  private String gcsBasePath;

  @Value("${gcs.bucket.name}")
  private String gcsBulkBucketName;

  @Value("${gcs.update.path}")
  private String gcsUpdatePath;

  @Value("${gcs.upload.path}")
  private String gcsUploadPath;

  @Value("${gcs.campaign.upload.path}")
  private String gcsCampaignUploadPath;

  @Value("${gcs.master.product.download.path}")
  private String gcsMasterProductDownloadPath;

  @Value("${gcs.uncategorised.sku.download.path}")
  private String gcsUncategorisedSkuPath;

  @Value("${gcs.store.copy.download.path}")
  private String gcsStoreCopyDownloadPath;

  @Value("${gcs.suspension.product.download.path}")
  private String gcsSuspensionProductDownloadPath;

  @Value("${gcs.configuration.error.upload.path}")
  private String gcsConfigurationErrorUploadPath;

  @Value("${gcs.master.sku.items.download.path}")
  private String gcsMasterSkuItemsDownloadPath;

  @Value("${gcs.master.sku.anchors.download.path}")
  private String gcsMasterSkuAnchorsDownloadPath;

  @Value("${gcs.auto.approved.products.download.path}")
  private String gcsAutoApprovedProductsDownloadPath;

  @Value("${gcs.ipr.products.download.path}")
  private String gcsIprProductsDownloadPath;

  @Value("${gcs.blibli.mass.template.location}")
  private String gcsMassTemplateLocation;

  @Value("${gcs.upsert.upload.path}")
  private String gcsUpsertUploadPath;

  @Value("${gcs.delete.upsert.upload.path}")
  private String gcsDeleteUpsertUploadPath;

  @Value("${gcs.subject.to.vat.path}")
  private String gcsSubjectToVatPath;

  @Value("${gcs.subjectToVat.error.upload.path}")
  private String gcsSubjectToVatErrorPath;

  @Value("${gcs.archival.upload.path}")
  private String gcsArchivalUploadPath;

  @Value("${gcs.instore.upload.path}")
  private String gcsInstoreUploadPath;

  @Value("${gcs.failed.mpp.upload.path}")
  private String gcsFailedMppUploadPath;

  @Value("${gcs.download.failed.products.path}")
  private String gcsDownloadFailedProductsPath;

  @Value("${static.baseUrl}")
  private String staticBaseUrl;

  @Value("${blibli.mass.template.location}")
  private String unifiedTemplateDirectory;

  @Value("${gcs.campaign.error.upload.path}")
  private String gcsCampaignErrorUploadPath;

  @Value("${gcs.suspension.error.file.path}")
  private String gcsSuspensionErrorFilePath;

  @Value("${gcs.archive.error.upload.path}")
  private String gcsArchiveErrorUploadPath;

  @Value("${gcs.delete.pp.error.upload.path}")
  private String gcsDeletePpErrorUploadPath;

  @Value("${gcs.work.order.error.upload.path}")
  private String gcsWorkOrderErrorUploadPath;

  @Value("${storeCopy.template.path}")
  private String storeCopyUploadTemplatePath;

  public static final String BULK_PRICE_RECOMMENDATION_BASE_TEMPLATE = "BulkPriceRecommendationBaseTemplate.xlsx";

  @Override
  public String getFilePrefix(String bulkProcessType) {
    switch (getBulkProcessType(bulkProcessType)) {
      case MASTER_PRODUCT:
        return gcsEnabled ?
            gcsMasterProductDownloadPath :
            EmailConstants.MASTER_PRODUCT_DOWNLOAD_FILE_PREFIX;
      case UNCATEGORISED_SKU:
        return gcsEnabled ?
            gcsUncategorisedSkuPath :
            EmailConstants.UNCATEGORISED_SKU_DOWNLOAD_FILE_PREFIX;
      case STORE_COPY:
        return gcsEnabled ? gcsStoreCopyDownloadPath : EmailConstants.STORE_COPY_FILE_PREFIX;
      case PRODUCT_SUSPENSION:
        return gcsEnabled ?
            gcsSuspensionProductDownloadPath :
            EmailConstants.BULK_PRODUCT_SUSPENSION_FILE_PREFIX;
      case CONFIGURATION:
        return gcsEnabled ?
            gcsConfigurationErrorUploadPath :
            EmailConstants.BULK_CONFIGURATION_ERROR_FILE_PREFIX;
      case MASTER_SKU_REVIEW_ITEMS_DOWNLOAD:
        return gcsMasterSkuItemsDownloadPath;
      case MASTER_SKU_IN_REVIEW_DOWNLOAD:
        return gcsMasterSkuAnchorsDownloadPath;
      case AUTO_APPROVED_PRODUCTS_DOWNLOAD:
        return gcsAutoApprovedProductsDownloadPath;
      case IPR_PRODUCTS_DOWNLOAD_ALL:
        return gcsIprProductsDownloadPath;
      default:
        return StringUtils.EMPTY;
    }
  }

  @Override
  public String getEmailPrefix() {
    return gcsEnabled ? gcsBasePath : ProcessorUtils.X_BULK;
  }

  @Override
  public byte[] downloadBaseTemplateForPriceRecommendation() {
    String filePath = gcsMassTemplateLocation + Constant.SLASH + BULK_PRICE_RECOMMENDATION_BASE_TEMPLATE;
    return gcsService.downloadFile(gcsBulkBucketName, filePath);
  }

  @Override
  public byte[] downloadFile(BulkProcess bulkProcess, String excelFileType) throws IOException {
    String filePath = getBasePath(bulkProcess.getBulkProcessType()) + bulkProcess.getBulkProcessCode() + File.separator
        + bulkProcess.getBulkProcessCode() + excelFileType;
    return gcsService.downloadFile(gcsBulkBucketName, filePath);
  }

  public String getBasePath(String bulkProcessType) {
    switch (getBulkProcessType(bulkProcessType)) {
      case CAMPAIGN: {
        return gcsEnabled ? gcsCampaignUploadPath : ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR;
      }
      case PRODUCT_CREATION_UPLOAD:
      case PRODUCT_CREATION_UPLOAD_PRIORITY:
      case PRODUCT_CREATION_UPLOAD_PRIORITY_1:
      case PRODUCT_CREATION_UPLOAD_PRIORITY_2:
      case CONVERTED_PRODUCT_CREATION_UPLOAD:
      case ASSEMBLY_REQUEST:
      case DISASSEMBLY_REQUEST:
      case TRANSFER_REQUEST:{
        return gcsEnabled ? gcsUploadPath : ProcessorUtils.DATA_BASE_DIR;
      }
      case INSTANT_PICKUP_PRODUCT_UPSERT: {
        return gcsEnabled ? gcsUpsertUploadPath : ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR;
      }
      case INSTANT_PICKUP_PRODUCT_DELETE: {
        return gcsEnabled ? gcsDeleteUpsertUploadPath : ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR;
      }
      case SUBJECT_TO_VAT: {
        return gcsEnabled ? gcsSubjectToVatPath : ProcessorUtils.BULK_VAT_UPDATE_DIR;
      }
      case SUBJECT_TO_VAT_ERROR: {
        return gcsSubjectToVatErrorPath;
      }
      case ARCHIVE: {
        return gcsEnabled ? gcsArchivalUploadPath : ProcessorUtils.BULK_UPDATE_DIR;
      }
      case PRODUCT_LEVEL_3:
      case PRODUCT_LEVEL_3_UPDATE_PRIORITY_1:
      case PRODUCT_LEVEL_3_UPDATE_PRIORITY_2: {
        return gcsEnabled ? gcsUpdatePath : ProcessorUtils.BULK_UPDATE_DIR;
      }
      case IN_STORE: {
        return gcsEnabled ? gcsInstoreUploadPath : ProcessorUtils.BULK_UPDATE_DIR;
      }
      case DOWNLOAD_FAILED_UPSERT_MPP_PRODUCT: {
        return gcsEnabled ? gcsFailedMppUploadPath : ProcessorUtils.DOWNLOAD_FAILED_MPP_PRODUCT_DIR;
      }
      case PRODUCT_CREATION_FAILED_DIR: {
        return gcsEnabled ? gcsDownloadFailedProductsPath : ProcessorUtils.BULK_FAILED_PRODUCT_DIR;
      }
      case PRODUCT_SUSPENSION: {
        return gcsEnabled ?
            gcsUploadPath + BulkProcessPath.SUSPENSION_PATH.getValue() :
            ProcessorUtils.BULK_SUSPENSION_DIR;
      }
      case BULK_PRICE_REBATE: {
        return gcsUploadPath + BulkProcessPath.BULK_REBATE.getValue();
      }
      case BULK_PRICE_PRODUCT_TYPE_TAGGING:{
        return gcsUploadPath + BulkProcessPath.BULK_PRICE_PRODUCT_TYPE_TAGGING.getValue();
      }
      case INTERNAL_UPLOAD: {
        return gcsEnabled ? gcsUploadPath + BulkProcessPath.INTERNAL_UPLOAD.getValue() : ProcessorUtils.BULK_UPDATE_DIR;
      }
      case CONFIGURATION: {
        return gcsEnabled ?
            gcsUploadPath + BulkProcessPath.CONFIGURATION.getValue() :
            ProcessorUtils.BULK_CONFIGURATION_DIR;
      }
      case CAMPAIGN_ERROR: {
        return gcsEnabled ?
            gcsCampaignErrorUploadPath :
            ProcessorUtils.BULK_CAMPAIGN_ERROR_PRODUCT_CREATE_DIR;
      }
      case SUSPENSION_ERROR: {
        return gcsEnabled ?
            gcsSuspensionErrorFilePath :
            ProcessorUtils.BULK_SUSPENSION_DIR;
      }
      case CONFIGURATION_ERROR: {
        return gcsEnabled ?
            gcsConfigurationErrorUploadPath :
            EmailConstants.BULK_CONFIGURATION_ERROR_FILE_PREFIX;
      }
      case RECAT_ERROR: {
        return gcsEnabled ?
            gcsBasePath + Constant.SLASH + BulkProcessPath.RECAT.getValue() :
            ProcessorUtils.X_BULK + BulkProcessPath.CONFIGURATION.getValue();
      }
      case STORE_COPY_TEMPLATE:
        return gcsEnabled ? gcsMassTemplateLocation : storeCopyUploadTemplatePath;
      case QR_GENERATION:
        return gcsBasePath + File.separator + QR_GENERATION.getValue() + File.separator;
      case ARCHIVE_ERROR:
        return gcsArchiveErrorUploadPath;
      case PICKUP_POINT_DELETE_ERROR:
        return gcsDeletePpErrorUploadPath;
      case WORK_ORDER_ERROR:
        return gcsWorkOrderErrorUploadPath;
      default:
        return StringUtils.EMPTY;
    }
  }

  @Override
  public byte[] downloadBaseTemplateForBulkBasicInfoUpdate(String fileName) {
    String filePath = gcsMassTemplateLocation + Constant.SLASH + fileName;
    return gcsService.downloadFile(gcsBulkBucketName, filePath);
  }
}
