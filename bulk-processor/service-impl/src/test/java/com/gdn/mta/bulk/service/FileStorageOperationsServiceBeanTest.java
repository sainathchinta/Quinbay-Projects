package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.dto.BulkProcessPath;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.File;
import java.io.UnsupportedEncodingException;

import static com.gdn.mta.bulk.dto.BulkProcessType.QR_GENERATION;

public class FileStorageOperationsServiceBeanTest {

  @InjectMocks
  private FileStorageOperationsServiceBean fileStorageOperationsService;

  @Mock
  private GCSService gcsService;

  private BulkProcess bulkProcess;

  private static final String FILE_PATH = "file-path";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_REQUEST_ID = "123";
  private static final String CREATED_BY = "createdBy";
  private static final String UPDATED_BY = "updatedBy";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String BUCKET_NAME = "bucket";

  @BeforeEach
  public void init() throws UnsupportedEncodingException {
    MockitoAnnotations.initMocks(this);

    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_GENERIC.name());
    bulkProcess.setStoreId(DEFAULT_STORE_ID);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedBy(CREATED_BY);
    bulkProcess.setRequestId(DEFAULT_REQUEST_ID);
    bulkProcess.setUpdatedBy(UPDATED_BY);
  }

  @Test
  public void getFilePrefixMasterProductGCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsMasterProductDownloadPath", FILE_PATH);
    String result = fileStorageOperationsService.getFilePrefix(BulkProcessType.MASTER_PRODUCT.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixUncategorisedSkuGCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsUncategorisedSkuPath", FILE_PATH);
    String result = fileStorageOperationsService.getFilePrefix(BulkProcessType.UNCATEGORISED_SKU.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixMasterSkuAnchorsDownloadTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsMasterSkuAnchorsDownloadPath",
        FILE_PATH);
    String result = fileStorageOperationsService.getFilePrefix(
        BulkProcessType.MASTER_SKU_IN_REVIEW_DOWNLOAD.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixMasterSkuItemsDownloadTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsMasterSkuItemsDownloadPath",
        FILE_PATH);
    String result = fileStorageOperationsService.getFilePrefix(
        BulkProcessType.MASTER_SKU_REVIEW_ITEMS_DOWNLOAD.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixAutoApprovedProductsTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsAutoApprovedProductsDownloadPath",
        FILE_PATH);
    String result = fileStorageOperationsService.getFilePrefix(
        BulkProcessType.AUTO_APPROVED_PRODUCTS_DOWNLOAD.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixIprProductsTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsIprProductsDownloadPath",
        FILE_PATH);
    String result = fileStorageOperationsService.getFilePrefix(
        BulkProcessType.IPR_PRODUCTS_DOWNLOAD_ALL.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixCopyProductGCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsStoreCopyDownloadPath", FILE_PATH);
    String result = fileStorageOperationsService.getFilePrefix(BulkProcessType.STORE_COPY.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixSuspensionGCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsSuspensionProductDownloadPath", FILE_PATH);
    String result = fileStorageOperationsService.getFilePrefix(BulkProcessType.PRODUCT_SUSPENSION.getValue());
    Assertions.assertEquals(result, FILE_PATH);
  }

  @Test
  public void getFilePrefixMasterProductGCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getFilePrefix(BulkProcessType.MASTER_PRODUCT.getValue());
    Assertions.assertEquals(result, EmailConstants.MASTER_PRODUCT_DOWNLOAD_FILE_PREFIX);
  }

  @Test
  public void getFilePrefixUncategorisedSkuGCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getFilePrefix(BulkProcessType.UNCATEGORISED_SKU.getValue());
    Assertions.assertEquals(result, EmailConstants.UNCATEGORISED_SKU_DOWNLOAD_FILE_PREFIX);
  }

  @Test
  public void getFilePrefixCopyProductGCSTrueFalse() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getFilePrefix(BulkProcessType.STORE_COPY.getValue());
    Assertions.assertEquals(result, EmailConstants.STORE_COPY_FILE_PREFIX);
  }

  @Test
  public void getFilePrefixSuspensionGCSFalseest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getFilePrefix(BulkProcessType.PRODUCT_SUSPENSION.getValue());
    Assertions.assertEquals(result, EmailConstants.BULK_PRODUCT_SUSPENSION_FILE_PREFIX);
  }

  @Test
  public void filePreFixDefaultCaseTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getFilePrefix(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Assertions.assertEquals(result, StringUtils.EMPTY);
  }

  @Test
  public void getEmailPrefixTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsBasePath", "xbulk");
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    Assertions.assertEquals("xbulk", fileStorageOperationsService.getEmailPrefix());

    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    Assertions.assertEquals("x-bulk", fileStorageOperationsService.getEmailPrefix());
  }

  @Test
  public void downloadBaseTemplateForPriceRecommendationTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsMassTemplateLocation", "template");
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsBulkBucketName", "bucket");
    Mockito.when(gcsService.downloadFile("bucket", "template/BulkPriceRecommendationBaseTemplate.xlsx")).thenReturn(new byte[100]);
    fileStorageOperationsService.downloadBaseTemplateForPriceRecommendation();
    Mockito.verify(gcsService).downloadFile("bucket", "template/BulkPriceRecommendationBaseTemplate.xlsx");
  }

  @Test
  public void downloadFileFromGcsTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsBulkBucketName", BUCKET_NAME);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    bulkProcess.setBulkProcessType("ProductCreationUpload");
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn("Hello".getBytes("UTF-8"));
    byte[] data = fileStorageOperationsService.downloadFile(bulkProcess,"xlsx");
    Assertions.assertNotNull(data);
  }

  @Test
  public void downloadFileFromGcsArchivalTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsBulkBucketName", BUCKET_NAME);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    bulkProcess.setBulkProcessType("Archive");
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn("Hello".getBytes("UTF-8"));
    byte[] data = fileStorageOperationsService.downloadFile(bulkProcess,"xlsx");
    Assertions.assertNotNull(data);
  }

  @Test
  public void downloadFileCampaignGCSTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsBulkBucketName", BUCKET_NAME);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    bulkProcess.setBulkProcessType("Campaign");
    Mockito.when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.anyString())).thenReturn("Hello".getBytes("UTF-8"));
    byte[] data = fileStorageOperationsService.downloadFile(bulkProcess,"xlsx");
    Assertions.assertNotNull(data);
  }

  @Test
  public void downloadFileErrorCampaignGCSTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsBulkBucketName", BUCKET_NAME);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    bulkProcess.setBulkProcessType("CampaignError");
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn("Hello".getBytes("UTF-8"));
    byte[] data = fileStorageOperationsService.downloadFile(bulkProcess,"xlsx");
    Assertions.assertNotNull(data);
  }

  @Test
  public void getBasePath_Campaign_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsCampaignUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.CAMPAIGN.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_Campaign_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.CAMPAIGN.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR, result);
  }

  @Test
  public void getBasePath_ProductCreationUpload_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_ProductCreationUpload_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Assertions.assertEquals(ProcessorUtils.DATA_BASE_DIR, result);
  }

  @Test
  public void getBasePath_SubjectToVat_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsSubjectToVatPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.SUBJECT_TO_VAT.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_SubjectToVat_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.SUBJECT_TO_VAT.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_VAT_UPDATE_DIR, result);
  }

  @Test
  public void getBasePath_Archive_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsArchivalUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.ARCHIVE.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_Archive_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.ARCHIVE.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_UPDATE_DIR, result);
  }

  @Test
  public void getBasePath_ProductSuspension_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.PRODUCT_SUSPENSION.getValue());
    Assertions.assertEquals(FILE_PATH + BulkProcessPath.SUSPENSION_PATH.getValue(), result);
  }

  @Test
  public void getBasePath_ProductSuspension_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.PRODUCT_SUSPENSION.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_SUSPENSION_DIR, result);
  }

  @Test
  public void getBasePath_Configuration_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.CONFIGURATION.getValue());
    Assertions.assertEquals(FILE_PATH + BulkProcessPath.CONFIGURATION.getValue(), result);
  }

  @Test
  public void getBasePath_Configuration_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.CONFIGURATION.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_CONFIGURATION_DIR, result);
  }

  @Test
  public void getBasePath_QRGeneration_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsBasePath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(QR_GENERATION.getValue());
    Assertions.assertEquals(FILE_PATH + File.separator + QR_GENERATION.getValue() + File.separator, result);
  }

  @Test
  public void getBasePath_DefaultCaseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.PRODUCT_LEVEL_3_GENERIC.getValue());
    Assertions.assertEquals(StringUtils.EMPTY, result);
  }

  @Test
  public void getBasePath_InstantPickupProductUpsert_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsUpsertUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_InstantPickupProductUpsert_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR, result);
  }

  @Test
  public void getBasePath_InstantPickupProductDelete_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsDeleteUpsertUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_InstantPickupProductDelete_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR, result);
  }

  @Test
  public void getBasePath_SubjectToVatError_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsSubjectToVatErrorPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.SUBJECT_TO_VAT_ERROR.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_ProductLevel3_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsUpdatePath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_ProductLevel3_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.PRODUCT_LEVEL_3.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_UPDATE_DIR, result);
  }

  @Test
  public void getBasePath_InStore_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsInstoreUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.IN_STORE.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_InStore_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.IN_STORE.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_UPDATE_DIR, result);
  }

  @Test
  public void getBasePath_DownloadFailedUpsertMppProduct_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsFailedMppUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.DOWNLOAD_FAILED_UPSERT_MPP_PRODUCT.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_DownloadFailedUpsertMppProduct_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.DOWNLOAD_FAILED_UPSERT_MPP_PRODUCT.getValue());
    Assertions.assertEquals(ProcessorUtils.DOWNLOAD_FAILED_MPP_PRODUCT_DIR, result);
  }

  @Test
  public void getBasePath_ProductCreationFailedDir_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsDownloadFailedProductsPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.PRODUCT_CREATION_FAILED_DIR.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_ProductCreationFailedDir_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.PRODUCT_CREATION_FAILED_DIR.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_FAILED_PRODUCT_DIR, result);
  }

  @Test
  public void getBasePath_BulkPriceRebate_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.BULK_PRICE_REBATE.getValue());
    Assertions.assertEquals(FILE_PATH + BulkProcessPath.BULK_REBATE.getValue(), result);
  }

  @Test
  public void getBasePath_BulkPriceProductTypeTagging_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.getValue());
    Assertions.assertEquals(FILE_PATH + BulkProcessPath.BULK_PRICE_PRODUCT_TYPE_TAGGING.getValue(), result);
  }

  @Test
  public void getBasePath_InternalUpload_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.INTERNAL_UPLOAD.getValue());
    Assertions.assertEquals(FILE_PATH + BulkProcessPath.INTERNAL_UPLOAD.getValue(), result);
  }

  @Test
  public void getBasePath_InternalUpload_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.INTERNAL_UPLOAD.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_UPDATE_DIR, result);
  }

  @Test
  public void getBasePath_CampaignError_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsCampaignErrorUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.CAMPAIGN_ERROR.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_CampaignError_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.CAMPAIGN_ERROR.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_CAMPAIGN_ERROR_PRODUCT_CREATE_DIR, result);
  }

  @Test
  public void getBasePath_SuspensionError_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsSuspensionErrorFilePath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.SUSPENSION_ERROR.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_SuspensionError_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.SUSPENSION_ERROR.getValue());
    Assertions.assertEquals(ProcessorUtils.BULK_SUSPENSION_DIR, result);
  }

  @Test
  public void getBasePath_ConfigurationError_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsConfigurationErrorUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.CONFIGURATION_ERROR.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_ConfigurationError_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.CONFIGURATION_ERROR.getValue());
    Assertions.assertEquals(EmailConstants.BULK_CONFIGURATION_ERROR_FILE_PREFIX, result);
  }

  @Test
  public void getBasePath_RecatError_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsBasePath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.RECAT_ERROR.getValue());
    Assertions.assertEquals(FILE_PATH + Constant.SLASH + BulkProcessPath.RECAT.getValue(), result);
  }

  @Test
  public void getBasePath_RecatError_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.RECAT_ERROR.getValue());
    Assertions.assertEquals(ProcessorUtils.X_BULK + BulkProcessPath.CONFIGURATION.getValue(), result);
  }

  @Test
  public void getBasePath_StoreCopyTemplate_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsMassTemplateLocation", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.STORE_COPY_TEMPLATE.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_StoreCopyTemplate_GCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    ReflectionTestUtils.setField(fileStorageOperationsService, "storeCopyUploadTemplatePath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.STORE_COPY_TEMPLATE.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_ArchiveError_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsArchiveErrorUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.ARCHIVE_ERROR.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_PickupPointDeleteError_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsDeletePpErrorUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.PICKUP_POINT_DELETE_ERROR.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getBasePath_WorkOrderError_GCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsWorkOrderErrorUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getBasePath(BulkProcessType.WORK_ORDER_ERROR.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getFilePrefixConfigurationGCSTrueTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", true);
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsConfigurationErrorUploadPath", FILE_PATH);
    String result = fileStorageOperationsService.getFilePrefix(BulkProcessType.CONFIGURATION.getValue());
    Assertions.assertEquals(FILE_PATH, result);
  }

  @Test
  public void getFilePrefixConfigurationGCSFalseTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsEnabled", false);
    String result = fileStorageOperationsService.getFilePrefix(BulkProcessType.CONFIGURATION.getValue());
    Assertions.assertEquals(EmailConstants.BULK_CONFIGURATION_ERROR_FILE_PREFIX, result);
  }

  @Test
  public void downloadBaseTemplateForBulkBasicInfoUpdateTest() {
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsMassTemplateLocation", "template");
    ReflectionTestUtils.setField(fileStorageOperationsService, "gcsBulkBucketName", "bucket");
    Mockito.when(gcsService.downloadFile("bucket", "template/file-name"))
        .thenReturn(new byte[1]);
    fileStorageOperationsService.downloadBaseTemplateForBulkBasicInfoUpdate(FILE_PATH);
    Mockito.verify(gcsService).downloadFile("bucket", "template/" + FILE_PATH);
  }
}