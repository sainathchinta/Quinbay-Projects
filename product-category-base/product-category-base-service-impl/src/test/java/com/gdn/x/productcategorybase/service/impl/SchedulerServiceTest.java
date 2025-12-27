package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.dto.CategoryProductAttributeExtractedDTO;
import com.gdn.x.productcategorybase.dto.request.ProductMigrationRequest;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.ProductAttributeExtracted;

import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipServiceWrapper;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ProductMigrationStatus;
import com.gdn.x.productcategorybase.ProductMigrationType;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.entity.ExtractionStatus;
import com.gdn.x.productcategorybase.entity.ProductMigration;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.service.ProductArchivalService;
import com.gdn.x.productcategorybase.service.ProductAttributeExtractionService;
import com.gdn.x.productcategorybase.service.ProductDeletionWrapperService;
import com.gdn.x.productcategorybase.service.ProductMigrationService;
import com.gdn.x.productcategorybase.service.SystemParameterService;

public class SchedulerServiceTest {
  private static final String STORE_ID = "10001";
  private static final String USERNAME = "developer";
  private static final String PRODUCT_CODE = "product-code";
  private static final String PRODUCT_CODE_1 = "product-code-1";
  private static final String WRONG_MIGRATION_TYPE = "wrongMigrationType";

  @InjectMocks
  private SchedulerServiceImpl schedulerService;

  @Mock
  private ProductAttributeExtractionService productAttributeExtractionService;

  @Mock
  private ProductMigrationService productMigrationService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ProductArchivalService productArchivalService;

  @Mock
  private ProductDeletionWrapperService productDeletionWrapperService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private BrandAuthorisationWipService brandAuthorisationWipService;

  @Mock
  private BrandAuthorisationWipServiceWrapper brandAuthorisationWipServiceWrapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productAttributeExtractionService, kafkaTopicProperties);
  }

  @Test
  public void processPublishingProductAttributeExtractionBackFillingTest() {
    List<CategoryProductAttributeExtractedDTO> categoryProductAttributeExtractedList =
      new ArrayList<>();
    categoryProductAttributeExtractedList.add(CategoryProductAttributeExtractedDTO.builder().category(new Category()).productAttributeExtracted(new ProductAttributeExtracted()).build());
    Mockito.when(
      productAttributeExtractionService.publishProductsForBackfillingByStatus(STORE_ID, USERNAME,
        ExtractionStatus.PENDING.name())).thenReturn(categoryProductAttributeExtractedList);
    Mockito.doNothing().when(productAttributeExtractionService)
      .publishAttributeExtractionEvent(Mockito.anyString(), Mockito.anyString(), Mockito.any(),
        Mockito.any());
    schedulerService.processPublishingProductAttributeExtractionBackFilling(STORE_ID, USERNAME,
      ExtractionStatus.PENDING.name());
    Mockito.verify(productAttributeExtractionService)
      .publishProductsForBackfillingByStatus(STORE_ID, USERNAME, ExtractionStatus.PENDING.name());
    Mockito.verify(productAttributeExtractionService)
      .publishAttributeExtractionEvent(Mockito.anyString(), Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void processPublishingProductAttributeExtractionBackFillingWithNoEentsToPublishTest() {
    List<CategoryProductAttributeExtractedDTO> categoryProductAttributeExtractedList =
      new ArrayList<>();
    categoryProductAttributeExtractedList.add(CategoryProductAttributeExtractedDTO.builder().category(new Category()).productAttributeExtracted(new ProductAttributeExtracted()).build());
    Mockito.when(
      productAttributeExtractionService.publishProductsForBackfillingByStatus(STORE_ID, USERNAME,
        ExtractionStatus.PENDING.name())).thenReturn(new ArrayList<>());
    Mockito.doNothing().when(productAttributeExtractionService)
      .publishAttributeExtractionEvent(Mockito.anyString(), Mockito.anyString(), Mockito.any(),
        Mockito.any());
    schedulerService.processPublishingProductAttributeExtractionBackFilling(STORE_ID, USERNAME,
      ExtractionStatus.PENDING.name());
    Mockito.verify(productAttributeExtractionService)
      .publishProductsForBackfillingByStatus(STORE_ID, USERNAME, ExtractionStatus.PENDING.name());
  }

  @Test
  public void publishCommonImageMigrationRecordsTest() {
    SystemParameter maxNumberOfRecordsToFetch = new SystemParameter();
    maxNumberOfRecordsToFetch.setValue("2");
    SystemParameter batchSize = new SystemParameter();
    batchSize.setValue("1");
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    Mockito.when(
        productMigrationService.findProductMigrationByStoreIdAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyInt())).thenReturn(Arrays.asList(commonImageMigration));
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION)).thenReturn(maxNumberOfRecordsToFetch);
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS)).thenReturn(batchSize);
    schedulerService.publishPendingProductMigrationRecords(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),
        new ProductCodesRequest());
    Mockito.verify(productMigrationService, Mockito.times(2))
        .findProductMigrationByStoreIdAndStatus(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), 1);
    Mockito.verify(productMigrationService, Mockito.times(2)).saveProductMigration(commonImageMigration);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS);
  }

  @Test
  public void publishCommonImageMigrationRecordsDeleteMfdTrueEntriesTest() {
    SystemParameter maxNumberOfRecordsToFetch = new SystemParameter();
    maxNumberOfRecordsToFetch.setValue("2");
    SystemParameter batchSize = new SystemParameter();
    batchSize.setValue("1");
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    Mockito.when(productMigrationService.findProductMigrationByStoreIdAndStatus(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyInt())).thenReturn(Arrays.asList(commonImageMigration));
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
            Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES))
        .thenReturn(maxNumberOfRecordsToFetch);
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
            Constants.BATCH_SIZE_TO_PUBLISH_DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES))
        .thenReturn(batchSize);
    schedulerService.publishPendingProductMigrationRecords(STORE_ID,
        ProductMigrationType.DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES.name(),
        new ProductCodesRequest());
    Mockito.verify(productMigrationService, Mockito.times(2))
        .findProductMigrationByStoreIdAndStatus(STORE_ID,
            ProductMigrationType.DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES.name(), 1);
    Mockito.verify(productMigrationService, Mockito.times(2))
        .saveProductMigration(commonImageMigration);
    Mockito.verify(systemParameterService).findByStoreIdAndVariable(STORE_ID,
        Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES);
    Mockito.verify(systemParameterService).findByStoreIdAndVariable(STORE_ID,
        Constants.BATCH_SIZE_TO_PUBLISH_DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES);
  }

  @Test
  public void publishCommonGfsToGcsMigrationRecordsTest() {
    SystemParameter maxNumberOfRecordsToFetch = new SystemParameter();
    maxNumberOfRecordsToFetch.setValue("2");
    SystemParameter batchSize = new SystemParameter();
    batchSize.setValue("1");
    ProductMigration productMigration = new ProductMigration();
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    productMigration.setMigrationType(ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name());
    Mockito.when(
        productMigrationService.findProductMigrationByStoreIdAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyInt())).thenReturn(Arrays.asList(productMigration));
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_GFS_TO_GCS_MIGRATION)).thenReturn(maxNumberOfRecordsToFetch);
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.BATCH_SIZE_TO_PUBLISH_GFS_TO_GCS_MIGRATION_RECORDS)).thenReturn(batchSize);
    schedulerService.publishPendingProductMigrationRecords(STORE_ID, ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name(),
        new ProductCodesRequest());
    Mockito.verify(productMigrationService, Mockito.times(2))
        .findProductMigrationByStoreIdAndStatus(STORE_ID, ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name(), 1);
    Mockito.verify(productMigrationService, Mockito.times(2)).saveProductMigration(productMigration);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_GFS_TO_GCS_MIGRATION);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_PUBLISH_GFS_TO_GCS_MIGRATION_RECORDS);
  }

  @Test
  public void publishGfsToGcsFinalImageMigrationRecordsTest() {
    SystemParameter maxNumberOfRecordsToFetch = new SystemParameter();
    maxNumberOfRecordsToFetch.setValue("2");
    SystemParameter batchSize = new SystemParameter();
    batchSize.setValue("1");
    ProductMigration productMigration = new ProductMigration();
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    productMigration.setMigrationType(ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name());
    Mockito.when(
        productMigrationService.findProductMigrationByStoreIdAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyInt())).thenReturn(Arrays.asList(productMigration));
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_GFS_TO_GCS_FINAL_IMAGE_MIGRATION)).thenReturn(maxNumberOfRecordsToFetch);
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.BATCH_SIZE_TO_PUBLISH_GFS_TO_GCS_FINAL_IMAGE_MIGRATION_RECORDS)).thenReturn(batchSize);
    schedulerService.publishPendingProductMigrationRecords(STORE_ID, ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name(),
        new ProductCodesRequest());
    Mockito.verify(productMigrationService, Mockito.times(2))
        .findProductMigrationByStoreIdAndStatus(STORE_ID, ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name(), 1);
    Mockito.verify(productMigrationService, Mockito.times(2)).saveProductMigration(productMigration);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_GFS_TO_GCS_FINAL_IMAGE_MIGRATION);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_PUBLISH_GFS_TO_GCS_FINAL_IMAGE_MIGRATION_RECORDS);
  }

  @Test
  public void publishCommonImageMigrationRecordsEmptyProductsListTest() {
    SystemParameter maxNumberOfRecordsToFetch = new SystemParameter();
    maxNumberOfRecordsToFetch.setValue("2");
    SystemParameter batchSize = new SystemParameter();
    batchSize.setValue("1");
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    Mockito.when(
        productMigrationService.findProductMigrationByStoreIdAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyInt())).thenReturn(Arrays.asList(commonImageMigration));
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION)).thenReturn(maxNumberOfRecordsToFetch);
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        Constants.BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS)).thenReturn(batchSize);
    schedulerService.publishPendingProductMigrationRecords(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),
        new ProductCodesRequest());
    Mockito.verify(productMigrationService, Mockito.times(2))
        .findProductMigrationByStoreIdAndStatus(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), 1);
    Mockito.verify(productMigrationService, Mockito.times(2)).saveProductMigration(commonImageMigration);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS);
  }

  @Test
  public void publishCommonImageMigrationRecordsNonEmptyProductTest() {
    ProductCodesRequest productCodeListRequest = new ProductCodesRequest();
    productCodeListRequest.setProductCodes(Arrays.asList(PRODUCT_CODE));
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    Mockito.when(productMigrationService.findProductMigrationByProductCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList())).thenReturn(Arrays.asList(commonImageMigration));
    schedulerService.publishPendingProductMigrationRecords(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),
        productCodeListRequest);
    Mockito.verify(productMigrationService)
        .findProductMigrationByProductCodes(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),
            Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productMigrationService).saveProductMigration(commonImageMigration);
  }

  @Test
  public void publishCommonImageMigrationRecordsNonEmptyProductListTest() {
    ProductCodesRequest productCodeListRequest = new ProductCodesRequest();
    productCodeListRequest.setProductCodes(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    List<ProductMigration> productMigrationList = new ArrayList<>();
    productMigrationList.add(commonImageMigration);
    Mockito.when(productMigrationService.findProductMigrationByProductCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList())).thenReturn(productMigrationList);
    schedulerService.publishPendingProductMigrationRecords(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),
        productCodeListRequest);
    Mockito.verify(productMigrationService)
        .findProductMigrationByProductCodes(STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),
            Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
    Mockito.verify(productMigrationService).saveProductMigration(commonImageMigration);
  }

  @Test
  public void publishCommonImageMigrationRecordsNonEmptyProductListDeleteMfdTrueEntriesTest() {
    ProductCodesRequest productCodeListRequest = new ProductCodesRequest();
    productCodeListRequest.setProductCodes(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    List<ProductMigration> productMigrationList = new ArrayList<>();
    productMigrationList.add(commonImageMigration);
    Mockito.when(productMigrationService.findProductMigrationByProductCodes(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList())).thenReturn(productMigrationList);
    schedulerService.publishPendingProductMigrationRecords(STORE_ID,
        ProductMigrationType.DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES.name(), productCodeListRequest);
    Mockito.verify(productMigrationService).findProductMigrationByProductCodes(STORE_ID,
        ProductMigrationType.DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES.name(),
        Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
    Mockito.verify(productMigrationService).saveProductMigration(commonImageMigration);
    Mockito.verify(kafkaTopicProperties).getDeleteMfdTrueImageAndAttributeEvent();
  }

  @Test
  public void publishCommonImageMigrationRecordsNonEmptyProductListWrongMigrationTypeTest() {
    ProductCodesRequest productCodeListRequest = new ProductCodesRequest();
    productCodeListRequest.setProductCodes(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    List<ProductMigration> productMigrationList = new ArrayList<>();
    productMigrationList.add(commonImageMigration);
    Mockito.when(productMigrationService.findProductMigrationByProductCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList())).thenReturn(productMigrationList);
    schedulerService.publishPendingProductMigrationRecords(STORE_ID, WRONG_MIGRATION_TYPE, productCodeListRequest);
    Mockito.verify(productMigrationService).findProductMigrationByProductCodes(STORE_ID, WRONG_MIGRATION_TYPE,
        Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
  }

  @Test
  public void publishCommonImageMigrationRecordsEmptyProductsListWrongMigrationTypeTest() {
    schedulerService.publishPendingProductMigrationRecords(STORE_ID, WRONG_MIGRATION_TYPE, new ProductCodesRequest());
  }

  @Test
  public void deleteArchiveProductDataTest() throws Exception {
    schedulerService.deleteArchiveProductData(STORE_ID);
  }

  @Test
  public void publishRejectedProductForDeletionTest() {
    schedulerService.publishRejectedProductForDeletion(STORE_ID);
  }

  @Test
  void activateUpcomingBrandAuthorisationTest() {
    schedulerService.activateUpcomingBrandAuthorisation(STORE_ID, 1);
    Mockito.verify(brandAuthorisationWipService, Mockito.times(1))
      .publishUpcomingBrandAuthorisation(STORE_ID, 1);
  }

  @Test
  public void publishUpcMigrationRecordsTest() {
    SystemParameter maxNumberOfRecordsToFetch = new SystemParameter();
    maxNumberOfRecordsToFetch.setValue("2");
    SystemParameter batchSize = new SystemParameter();
    batchSize.setValue("1");
    ProductMigration productMigration = new ProductMigration();
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    productMigration.setMigrationType(ProductMigrationType.UPC_MIGRATION.name());
    Mockito.when(
      productMigrationService.findProductMigrationByStoreIdAndStatus(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt())).thenReturn(Arrays.asList(productMigration));
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
      Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_UPC_MIGRATION)).thenReturn(maxNumberOfRecordsToFetch);
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
      Constants.BATCH_SIZE_TO_PUBLISH_UPC_MIGRATION_RECORDS)).thenReturn(batchSize);
    schedulerService.publishPendingProductMigrationRecords(STORE_ID, ProductMigrationType.UPC_MIGRATION.name(),
      new ProductCodesRequest());
    Mockito.verify(productMigrationService, Mockito.times(2))
      .findProductMigrationByStoreIdAndStatus(STORE_ID, ProductMigrationType.UPC_MIGRATION.name(), 1);
    Mockito.verify(productMigrationService, Mockito.times(2)).saveProductMigration(productMigration);
    Mockito.verify(systemParameterService)
      .findByStoreIdAndVariable(STORE_ID, Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_UPC_MIGRATION);
    Mockito.verify(systemParameterService)
      .findByStoreIdAndVariable(STORE_ID, Constants.BATCH_SIZE_TO_PUBLISH_UPC_MIGRATION_RECORDS);
  }

  @Test
  public void publishProductAttributeMigrationRecordsTest() {
    SystemParameter maxNumberOfRecordsToFetch = new SystemParameter();
    maxNumberOfRecordsToFetch.setValue("2");
    SystemParameter batchSize = new SystemParameter();
    batchSize.setValue("1");
    ProductMigration productMigration = new ProductMigration();
    productMigration.setProductCode(PRODUCT_CODE);
    productMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    productMigration.setMigrationType(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name());
    Mockito.when(kafkaTopicProperties.getProductAttributeMigrationTopic()).thenReturn(
      DomainEventName.PRODUCT_ATTRIBUTE_BACKFILLING);
    Mockito.when(
      productMigrationService.findProductMigrationByStoreIdAndStatus(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt())).thenReturn(Arrays.asList(productMigration));
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
      Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_PRODUCT_ATTRIBUTE_MIGRATION)).thenReturn(maxNumberOfRecordsToFetch);
    Mockito.when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
      Constants.BATCHES_TO_FETCH_FOR_PRODUCT_ATTRIBUTE_MIGRATION)).thenReturn(batchSize);
    schedulerService.publishPendingProductMigrationRecords(STORE_ID,
      ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name(),
      new ProductCodesRequest());
    Mockito.verify(productMigrationService, Mockito.times(2))
      .findProductMigrationByStoreIdAndStatus(STORE_ID, ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name(), 1);
    Mockito.verify(productMigrationService, Mockito.times(2)).saveProductMigration(productMigration);
    Mockito.verify(systemParameterService)
      .findByStoreIdAndVariable(STORE_ID, Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_PRODUCT_ATTRIBUTE_MIGRATION);
    Mockito.verify(systemParameterService)
      .findByStoreIdAndVariable(STORE_ID, Constants.BATCHES_TO_FETCH_FOR_PRODUCT_ATTRIBUTE_MIGRATION);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getProductAttributeMigrationTopic();
  }

  @Test
  public void updateProductMigrationStatusWithValidRequestTest() {
    ProductMigrationRequest productMigrationRequest = new ProductMigrationRequest();
    productMigrationRequest.setProductCode(PRODUCT_CODE);
    productMigrationRequest.setUpdatedStatus(ProductMigrationStatus.FAILED.name());
    productMigrationRequest.setCurrentStatus(ProductMigrationStatus.IN_PROGRESS.name());
    productMigrationRequest.setMigrationType(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name());

    ProductMigration savedProductMigration = new ProductMigration();
    savedProductMigration.setProductCode(PRODUCT_CODE);
    savedProductMigration.setStatus(ProductMigrationStatus.IN_PROGRESS.name());
    savedProductMigration.setMigrationType(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name());

    Mockito.when(productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(
      PRODUCT_CODE, ProductMigrationStatus.IN_PROGRESS.name(),
      ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name())).thenReturn(savedProductMigration);

    schedulerService.updateProductMigrationStatus(STORE_ID, USERNAME, productMigrationRequest);

    Mockito.verify(productMigrationService).findProductMigrationByProductCodeAndStatusAndMigrationType(
      productMigrationRequest.getProductCode(), productMigrationRequest.getCurrentStatus(), productMigrationRequest.getMigrationType());
    Mockito.verify(productMigrationService).saveProductMigration(savedProductMigration);
  }

  @Test
  public void updateProductMigrationStatusWithInvalidRequestTest() {
    ProductMigrationRequest productMigrationRequest = new ProductMigrationRequest();
    productMigrationRequest.setProductCode(PRODUCT_CODE);
    productMigrationRequest.setCurrentStatus(ProductMigrationStatus.IN_PROGRESS.name());
    productMigrationRequest.setUpdatedStatus(ProductMigrationStatus.PUBLISHED.name());
    productMigrationRequest.setMigrationType(StringUtils.EMPTY);

    Exception exception = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      schedulerService.updateProductMigrationStatus(STORE_ID, USERNAME, productMigrationRequest);
    });
    Assertions.assertTrue(
      exception.getMessage().contains(ErrorMessage.MIGRATION_TYPE_MUST_NOT_BE_EMPTY.getMessage()));
    Mockito.verifyNoInteractions(productMigrationService);
  }

  @Test
  public void updateProductMigrationStatusWithNoMatchingRecordTest() {
    ProductMigrationRequest productMigrationRequest = new ProductMigrationRequest();
    productMigrationRequest.setProductCode(PRODUCT_CODE);
    productMigrationRequest.setCurrentStatus(ProductMigrationStatus.IN_PROGRESS.name());
    productMigrationRequest.setUpdatedStatus(ProductMigrationStatus.FAILED.name());
    productMigrationRequest.setMigrationType(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name());

    Mockito.when(productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(
      PRODUCT_CODE, ProductMigrationStatus.IN_PROGRESS.name(),
      ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name())).thenReturn(null);

    schedulerService.updateProductMigrationStatus(STORE_ID, USERNAME, productMigrationRequest);

    Mockito.verify(productMigrationService)
      .findProductMigrationByProductCodeAndStatusAndMigrationType(
        productMigrationRequest.getProductCode(), productMigrationRequest.getCurrentStatus(),
        productMigrationRequest.getMigrationType());
    Mockito.verifyNoMoreInteractions(productMigrationService);
  }

  @Test
  public void updateProductMigrationStatusWhenCurrentStatusIsMissingTest() {
    ProductMigrationRequest productMigrationRequest = new ProductMigrationRequest();
    productMigrationRequest.setProductCode(PRODUCT_CODE);
    productMigrationRequest.setUpdatedStatus(ProductMigrationStatus.FAILED.name());
    productMigrationRequest.setMigrationType(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name());

    ProductMigration savedProductMigration = new ProductMigration();
    savedProductMigration.setProductCode(PRODUCT_CODE);
    savedProductMigration.setStatus(ProductMigrationStatus.IN_PROGRESS.name());
    savedProductMigration.setMigrationType(ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name());

    Mockito.when(productMigrationService.findProductMigrationByProductCodes(STORE_ID,
        ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name(),
        Collections.singletonList(productMigrationRequest.getProductCode())))
      .thenReturn(Collections.singletonList(savedProductMigration));

    schedulerService.updateProductMigrationStatus(STORE_ID, USERNAME, productMigrationRequest);

    Mockito.verify(productMigrationService).findProductMigrationByProductCodes(
      STORE_ID, productMigrationRequest.getMigrationType(), Collections.singletonList(productMigrationRequest.getProductCode()));
    Mockito.verify(productMigrationService).saveProductMigration(savedProductMigration);
  }

  @Test
  public void sendNearExpiryMailsTest() {
    schedulerService.sendNearExpiryMails(STORE_ID);
    Mockito.verify(brandAuthorisationWipServiceWrapper).sendNearExpiryMailNotification(STORE_ID);
  }
}
