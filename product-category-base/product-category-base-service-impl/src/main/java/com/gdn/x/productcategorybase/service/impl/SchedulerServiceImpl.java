package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.ProductMigrationRequest;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipServiceWrapper;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ProductMigrationStatus;
import com.gdn.x.productcategorybase.ProductMigrationType;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.dto.CategoryProductAttributeExtractedDTO;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.entity.ProductMigration;
import com.gdn.x.productcategorybase.service.ProductArchivalService;
import com.gdn.x.productcategorybase.service.ProductAttributeExtractionService;
import com.gdn.x.productcategorybase.service.ProductDeletionWrapperService;
import com.gdn.x.productcategorybase.service.ProductMigrationService;
import com.gdn.x.productcategorybase.service.SchedulerService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class SchedulerServiceImpl implements SchedulerService {

  @Autowired
  private ProductAttributeExtractionService productAttributeExtractionService;

  @Autowired
  private ProductMigrationService productMigrationService;

  @Autowired
  private ProductArchivalService productArchivalService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ProductDeletionWrapperService productDeletionWrapperService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private BrandAuthorisationWipService brandAuthorisationWipService;

  @Autowired
  private BrandAuthorisationWipServiceWrapper brandAuthorisationWipServiceWrapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Async
  @Override
  public void processPublishingProductAttributeExtractionBackFilling(String storeId, String username, String status) {
    List<CategoryProductAttributeExtractedDTO> categoryProductAttributeExtractedList =
      productAttributeExtractionService.publishProductsForBackfillingByStatus(storeId, username,
        status);
    if (CollectionUtils.isNotEmpty(categoryProductAttributeExtractedList)) {
      categoryProductAttributeExtractedList.stream().forEach(
        categoryProductAttributeExtractedDTO -> productAttributeExtractionService.publishAttributeExtractionEvent(
          storeId, username, categoryProductAttributeExtractedDTO.getCategory(),
          categoryProductAttributeExtractedDTO.getProductAttributeExtracted()));
    }
  }

  @Async
  @Override
  public void publishPendingProductMigrationRecords(String storeId, String migrationType,
      ProductCodesRequest productCodesRequest) {
    if (CollectionUtils.isNotEmpty(productCodesRequest.getProductCodes())) {
      processListOfProductCodesForMigration(storeId, migrationType, productCodesRequest);
    } else {
      processBatchOfProductsForMigration(storeId, migrationType);
    }
  }

  @Async
  @Override
  public void deleteArchiveProductData(String storeId) throws Exception {
    productArchivalService.deleteDataFromArchivalTable(storeId);
  }

  @Async
  @Override
  public void publishRejectedProductForDeletion(String storeId) {
    productDeletionWrapperService.publishProductForDeletion(storeId);
  }

  @Async
  @Override
  public void activateUpcomingBrandAuthorisation(String storeId, int daysThreshold) {
    brandAuthorisationWipService.publishUpcomingBrandAuthorisation(storeId, daysThreshold);
  }

  @Override
  @Async
  public void sendNearExpiryMails(String storeId) {
    brandAuthorisationWipServiceWrapper.sendNearExpiryMailNotification(storeId);
  }

  @Override
  public void updateProductMigrationStatus(String storeId, String requestId,
    ProductMigrationRequest productMigrationRequest) {
    GdnPreconditions.checkArgument(
      org.apache.commons.lang.StringUtils.isNotEmpty(productMigrationRequest.getMigrationType()),
      ErrorMessage.MIGRATION_TYPE_MUST_NOT_BE_EMPTY.getMessage());
    GdnPreconditions.checkArgument(
      org.apache.commons.lang.StringUtils.isNotEmpty(productMigrationRequest.getUpdatedStatus()),
      ErrorMessage.STATUS_MUST_NOT_BE_EMPTY.getMessage());

    ProductMigration savedProductMigration =
      StringUtils.isNotBlank(productMigrationRequest.getCurrentStatus()) ?
        productMigrationService.findProductMigrationByProductCodeAndStatusAndMigrationType(
          productMigrationRequest.getProductCode(), productMigrationRequest.getCurrentStatus(),
          productMigrationRequest.getMigrationType()) :
        productMigrationService.findProductMigrationByProductCodes(storeId,
            productMigrationRequest.getMigrationType(),
            Collections.singletonList(productMigrationRequest.getProductCode())).stream().findFirst()
          .orElse(null);

    if (Objects.nonNull(savedProductMigration)) {
      log.info("Updating Product Migration for productCode : {} to {}",
        productMigrationRequest.getProductCode(), productMigrationRequest.getUpdatedStatus());
      CommonUtil.updateProductMigrationEntity(requestId, productMigrationRequest,
        savedProductMigration);
      productMigrationService.saveProductMigration(savedProductMigration);
    }
  }

  private void processListOfProductCodesForMigration(String storeId, String migrationType,
      ProductCodesRequest productCodesRequest) {
    List<ProductMigration> productMigrationList =
        fetchProductMigrationByMigrationType(storeId, migrationType, productCodesRequest);
    publishProductForMigrationEvent(storeId, productMigrationList);
  }

  private List<ProductMigration> fetchProductMigrationByMigrationType(String storeId, String migrationType,
      ProductCodesRequest productCodeListRequest) {
    List<ProductMigration> productMigrationList =
        productMigrationService.findProductMigrationByProductCodes(storeId, migrationType,
            productCodeListRequest.getProductCodes());
    List<String> productCodesFromDb =
        productMigrationList.stream().map(ProductMigration::getProductCode).toList();
    List<String> productCodesInRequest = new ArrayList<>(productCodeListRequest.getProductCodes());
    productCodesInRequest.removeAll(productCodesFromDb);
    for (String productCode : productCodesInRequest) {
      ProductMigration productMigration =
        ProductMigration.builder().productCode(productCode).migrationType(migrationType)
          .status(ProductMigrationStatus.PENDING.name()).build();
      productMigration.setStoreId(storeId);
      productMigrationList.add(productMigration);
    }
    return productMigrationList;
  }

  private void processBatchOfProductsForMigration(String storeId, String migrationType) {
    int limit = getMaxRecordFromDbByMigrationType(storeId, migrationType);
    int batch = getBatchLimitByMigrationType(storeId, migrationType);
    int page = 0;
    while (page * batch < limit) {
      List<ProductMigration> commonImageMigrationList =
          productMigrationService.findProductMigrationByStoreIdAndStatus(storeId, migrationType, batch);
      publishProductForMigrationEvent(storeId, commonImageMigrationList);
      page++;
    }
  }

  private void publishProductForMigrationEvent(String storeId, List<ProductMigration> productMigrationList) {
    for (ProductMigration productMigration : productMigrationList) {
      productMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
      productMigrationService.saveProductMigration(productMigration);
      String topic = getMigrationTopicName(productMigration.getMigrationType());
      log.info("Publishing event = {}  for productCode = {} ", topic, productMigration.getProductCode());
      kafkaPublisher.send(topic, productMigration.getProductCode(),
          CommonUtil.toCommonImageBackfillingEventModel(storeId, productMigration));
    }
  }

  private int getBatchLimitByMigrationType(String storeId, String migrationType) {
    if (ProductMigrationType.COMMON_IMAGE_MIGRATION.name().equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
          Constants.BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS).getValue());
    } else if (ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name().equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
          Constants.BATCH_SIZE_TO_PUBLISH_GFS_TO_GCS_MIGRATION_RECORDS).getValue());
    } else if (ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name().equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
          Constants.BATCH_SIZE_TO_PUBLISH_GFS_TO_GCS_FINAL_IMAGE_MIGRATION_RECORDS).getValue());
    } else if (ProductMigrationType.UPC_MIGRATION.name().equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
        Constants.BATCH_SIZE_TO_PUBLISH_UPC_MIGRATION_RECORDS).getValue());
    } else if (ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name().equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
        Constants.BATCHES_TO_FETCH_FOR_PRODUCT_ATTRIBUTE_MIGRATION).getValue());
    } else if (ProductMigrationType.DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES.name()
        .equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
          Constants.BATCH_SIZE_TO_PUBLISH_DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES).getValue());
    }
    return 0;
  }

  private int getMaxRecordFromDbByMigrationType(String storeId, String migrationType) {
    if (ProductMigrationType.COMMON_IMAGE_MIGRATION.name().equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
          Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION).getValue());
    } else if (ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name().equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
          Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_GFS_TO_GCS_MIGRATION).getValue());
    } else if (ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name().equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
          Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_GFS_TO_GCS_FINAL_IMAGE_MIGRATION).getValue());
    } else if (ProductMigrationType.UPC_MIGRATION.name().equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
        Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_UPC_MIGRATION).getValue());
    }
    else if (ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name().equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
        Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_PRODUCT_ATTRIBUTE_MIGRATION).getValue());
    } else if (ProductMigrationType.DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES.name()
        .equals(migrationType)) {
      return Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
              Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES)
          .getValue());
    }
    return 0;
  }

  private String getMigrationTopicName(String migrationType) {
    if (ProductMigrationType.COMMON_IMAGE_MIGRATION.name().equals(migrationType)) {
      return DomainEventName.PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING;
    } else if (ProductMigrationType.GFS_TO_GCS_SOURCE_IMAGE_MIGRATION.name()
      .equals(migrationType)) {
      return DomainEventName.PRODUCT_AND_ITEM_GFS_TO_GCS_MIGRATION;
    } else if (ProductMigrationType.GFS_TO_GCS_FINAL_IMAGE_MIGRATION.name().equals(migrationType)) {
      return DomainEventName.PRODUCT_AND_ITEM_GFS_TO_GCS_FINAL_IMAGE_MIGRATION;
    } else if (ProductMigrationType.UPC_MIGRATION.name().equals(migrationType)) {
      return DomainEventName.PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING;
    } else if (ProductMigrationType.PRODUCT_ATTRIBUTE_UPDATE.name().equals(migrationType)) {
      return kafkaTopicProperties.getProductAttributeMigrationTopic();
    } else if (ProductMigrationType.DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES.name()
        .equals(migrationType)) {
      return kafkaTopicProperties.getDeleteMfdTrueImageAndAttributeEvent();
    }
    return StringUtils.EMPTY;
  }

}
