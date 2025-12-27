package com.gdn.mta.product.service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.product.entity.ProductFbbMigration;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.enums.FbbMigrationConstants;
import com.gdn.mta.product.enums.MigrationStatus;
import com.gdn.mta.product.repository.ProductFbbMigrationRepository;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import java.util.Collections;

import static org.mockito.Mockito.times;

public class ProductFbbMigrationServiceImplTest {

  private static final String STORE_ID = "store-id";
  private static final String PRODUCT_SKU = "product-sku";
  @InjectMocks
  private ProductFbbMigrationServiceImpl productFbbMigrationService;
  @Mock
  private ProductSystemParameterService productSystemParameterService;
  @Mock
  private ProductFbbMigrationRepository productFbbMigrationRepository;
  @Mock
  private KafkaPublisher kafkaProducer;
  private ProductSystemParameter backFillFbbFlagFetch;
  private ProductSystemParameter backFillFbbFlagBatch;
  private ProductFbbMigration productFbbMigration;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    Mockito.verifyNoMoreInteractions(productSystemParameterService);
    Mockito.verifyNoMoreInteractions(productFbbMigrationRepository);
    backFillFbbFlagFetch = new ProductSystemParameter();
    backFillFbbFlagFetch.setValue("2");

    backFillFbbFlagBatch = new ProductSystemParameter();
    backFillFbbFlagBatch.setValue("1");

    productFbbMigration = new ProductFbbMigration();
    productFbbMigration.setIdentifier(PRODUCT_SKU);
  }

  @Test
  public void backFillFbbFlagAtActiveProductTest() {
    productFbbMigration.setProductState(FbbMigrationConstants.ACTIVE.name());
    productFbbMigration.setMigrationType(FbbMigrationConstants.BACK_FILL_FBB_FLAG.name());
    PageRequest pageRequest = PageRequest.of(0, 1);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.backFillFbbFlagFetchSize)).thenReturn(backFillFbbFlagFetch);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.backFillFbbFlagBatchSize)).thenReturn(backFillFbbFlagBatch);
    Mockito.when(
        productFbbMigrationRepository.findByStoreIdAndStatusAndMigrationType(STORE_ID,
            MigrationStatus.PENDING.getMigrationStatus(), pageRequest, FbbMigrationConstants.BACK_FILL_FBB_FLAG.name()))
      .thenReturn(new PageImpl<>(Collections.singletonList(productFbbMigration), pageRequest, 2));
    productFbbMigrationService.migrateFbbProducts(STORE_ID,
      FbbMigrationConstants.BACK_FILL_FBB_FLAG.name());
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.backFillFbbFlagFetchSize);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.backFillFbbFlagBatchSize);
    Mockito.verify(productFbbMigrationRepository, times(2))
      .findByStoreIdAndStatusAndMigrationType(STORE_ID, MigrationStatus.PENDING.getMigrationStatus(), pageRequest,
          FbbMigrationConstants.BACK_FILL_FBB_FLAG.name());
  }

  @Test
  public void backFillFbbFlagAtInActiveProductTest() {
    productFbbMigration.setProductState(FbbMigrationConstants.INACTIVE.name());
    productFbbMigration.setMigrationType(FbbMigrationConstants.BACK_FILL_FBB_FLAG.name());
    PageRequest pageRequest = PageRequest.of(0, 1);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.backFillFbbFlagFetchSize)).thenReturn(backFillFbbFlagFetch);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.backFillFbbFlagBatchSize)).thenReturn(backFillFbbFlagBatch);
    Mockito.when(
        productFbbMigrationRepository.findByStoreIdAndStatusAndMigrationType(STORE_ID,
            MigrationStatus.PENDING.getMigrationStatus(), pageRequest, FbbMigrationConstants.BACK_FILL_FBB_FLAG.name()))
      .thenReturn(new PageImpl<>(Collections.singletonList(productFbbMigration), pageRequest, 2));
    productFbbMigrationService.migrateFbbProducts(STORE_ID,
      FbbMigrationConstants.BACK_FILL_FBB_FLAG.name());
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.backFillFbbFlagFetchSize);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.backFillFbbFlagBatchSize);
    Mockito.verify(productFbbMigrationRepository, times(2))
      .findByStoreIdAndStatusAndMigrationType(STORE_ID, MigrationStatus.PENDING.getMigrationStatus(), pageRequest,
          FbbMigrationConstants.BACK_FILL_FBB_FLAG.name());
  }

  @Test
  public void pickupPointChangeMigrationTest() {
    productFbbMigration.setMigrationType(FbbMigrationConstants.PICKUP_POINT_CHANGE.name());
    PageRequest pageRequest = PageRequest.of(0, 1);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.pickupPointChangeFetchSize)).thenReturn(backFillFbbFlagFetch);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.pickupPointChangeBatchSize)).thenReturn(backFillFbbFlagBatch);
    Mockito.when(
        productFbbMigrationRepository.findByStoreIdAndStatusAndMigrationType(STORE_ID,
            MigrationStatus.PENDING.getMigrationStatus(), pageRequest, FbbMigrationConstants.PICKUP_POINT_CHANGE.name()))
      .thenReturn(new PageImpl<>(Collections.singletonList(productFbbMigration), pageRequest, 2));
    productFbbMigrationService.migrateFbbProducts(STORE_ID,
      FbbMigrationConstants.PICKUP_POINT_CHANGE.name());
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.pickupPointChangeFetchSize);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.pickupPointChangeBatchSize);
    Mockito.verify(productFbbMigrationRepository, times(2))
      .findByStoreIdAndStatusAndMigrationType(STORE_ID, MigrationStatus.PENDING.getMigrationStatus(), pageRequest,
          FbbMigrationConstants.PICKUP_POINT_CHANGE.name());
  }
}