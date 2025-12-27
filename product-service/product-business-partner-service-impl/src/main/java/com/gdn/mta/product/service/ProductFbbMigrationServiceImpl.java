package com.gdn.mta.product.service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ProductFbbMigrationEventModel;
import com.gdn.mta.product.entity.ProductFbbMigration;
import com.gdn.mta.product.enums.FbbMigrationConstants;
import com.gdn.mta.product.enums.MigrationStatus;
import com.gdn.mta.product.repository.ProductFbbMigrationRepository;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.x.productcategorybase.ProductMigrationStatus;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class ProductFbbMigrationServiceImpl implements ProductFbbMigrationService {

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  private ProductFbbMigrationRepository productFbbMigrationRepository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Async
  @Override
  @Trace(dispatcher = true)
  public void migrateFbbProducts(String storeId, String migrationType) {
    int fetchSize = getFetchSizeFromDbByMigrationType(storeId, migrationType);
    int batchSize = getBatchSizeForEachProcess(storeId, migrationType);
    int page = 0;
    while (page * batchSize < fetchSize) {
      PageRequest pageRequest = PageRequest.of(0, batchSize);
      List<ProductFbbMigration> productMigrationList =
        productFbbMigrationRepository.findByStoreIdAndStatusAndMigrationType(storeId,
            MigrationStatus.PENDING.getMigrationStatus(), pageRequest, migrationType).getContent();
      publishProductForMigrationEvent(storeId, productMigrationList);
      page++;
    }
  }

  private int getFetchSizeFromDbByMigrationType(String storeId, String migrationType) {
    if (FbbMigrationConstants.BACK_FILL_FBB_FLAG.name().equals(migrationType)) {
      return Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
        SystemParameterConstants.backFillFbbFlagFetchSize).getValue());
    } else {
      return Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
        SystemParameterConstants.pickupPointChangeFetchSize).getValue());
    }
  }

  private int getBatchSizeForEachProcess(String storeId, String migrationType) {
    if (FbbMigrationConstants.BACK_FILL_FBB_FLAG.name().equals(migrationType)) {
      return Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
        SystemParameterConstants.backFillFbbFlagBatchSize).getValue());
    } else {
      return Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
        SystemParameterConstants.pickupPointChangeBatchSize).getValue());
    }
  }

  private String getMigrationTopicName(String migrationType, String productState) {
    if (FbbMigrationConstants.BACK_FILL_FBB_FLAG.name().equalsIgnoreCase(migrationType)
      && FbbMigrationConstants.ACTIVE.name().equalsIgnoreCase(productState)) {
      return DomainEventName.ACTIVE_PRODUCT_BACK_FILL_FBB_FLAG;
    } else if (FbbMigrationConstants.BACK_FILL_FBB_FLAG.name().equalsIgnoreCase(migrationType)
      && FbbMigrationConstants.INACTIVE.name().equalsIgnoreCase(productState)) {
      return DomainEventName.INACTIVE_PRODUCT_BACK_FILL_FBB_FLAG;
    } else {
      return DomainEventName.FBB_PICKUP_POINT_MIGRATION;
    }
  }

  private void publishProductForMigrationEvent(String storeId,
    List<ProductFbbMigration> productMigrationList) {
    for (ProductFbbMigration productFbbMigration : productMigrationList) {
      productFbbMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
      String topic = getMigrationTopicName(productFbbMigration.getMigrationType(),
        productFbbMigration.getProductState());
      log.info("Publishing event = {}  for identifier = {} ", topic,
        productFbbMigration.getIdentifier());
      kafkaProducer.send(topic, productFbbMigration.getIdentifier(),
        ProductFbbMigrationEventModel.builder().identifier(productFbbMigration.getIdentifier())
          .pickupPointCode(productFbbMigration.getPickupPointCode()).storeId(storeId)
          .productState(productFbbMigration.getProductState()).build());
      productFbbMigration.setStatus(ProductMigrationStatus.SUCCESS.name());
      productFbbMigrationRepository.saveAndFlush(productFbbMigration);
    }
  }

}
