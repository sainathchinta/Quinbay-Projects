package com.gdn.partners.pbp.service.productlevel3;

import static org.mockito.MockitoAnnotations.initMocks;

import java.math.BigInteger;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.orm.ObjectOptimisticLockingFailureException;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryCount;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorInventoryCriteria;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorState;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.repository.agregator.ProductLevel3AggregatorRepository;
import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;

public class ProductLevel3AggregatorServiceTest {

  private static final String BUSINESS_PARTNER_CODE = "BusinessPartnerCode";
  private static final String GDN_SKU = "GdnSku";
  private static final String STORE_ID = "10001";
  private static final String GDN_SKUS_CANNOT_EMPTY = "GdnSkus cannot be empty";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final Long COUNT = 1000000000L;
  private static final Long ZERO_COUNT = 0L;
  private static final Long ACTIVE_COUNT = 10L;

  @InjectMocks
  private ProductLevel3AggregatorServiceBean productLevel3AggregatorServiceBean;

  @Mock
  protected ProductLevel3AggregatorRepository productLevel3AggregatorRepository;

  @Captor
  private ArgumentCaptor<List<ProductLevel3Aggregator>> listArgumentCaptor;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private ProductLevel3Service productLevel3Service;
  private ProductLevel3Aggregator productLevel3Aggregator;
  private ProductLevel3Summary productLevel3Summary;
  private List<Object[]> summaryCounter;
  private List<Object[]> summaryCounterActiveZero;
  private Level2InventoryMinimumStockAlertEvent level2InventoryMinimumStockAlertEvent;
  private Level2InventoryOosEvent level2InventoryOosEvent;
  private Level2InventoryNonOosEvent level2InventoryNonOosEvent;
  private ProductCountResponse productCountResponse;

  @BeforeEach
  public void __initialize() throws Exception {
    initMocks(this);
    this.productLevel3Aggregator = new ProductLevel3Aggregator();
    this.productLevel3Aggregator.setGdnSku(GDN_SKU);
    this.productLevel3Aggregator.setMarkForDelete(Boolean.FALSE);
    this.productLevel3Summary = new ProductLevel3Summary();
    this.productLevel3Summary.setAvailableStockLevel2(2);
    this.productLevel3Summary.setMinimumStockLevel2(1);
    this.productLevel3Summary.setAvailableStockLevel1(2);
    this.productLevel3Summary.setIsArchived(false);
    this.productLevel3Summary.setSynchronizeStock(false);

    this.level2InventoryMinimumStockAlertEvent =
      new Level2InventoryMinimumStockAlertEvent(STORE_ID, GDN_SKU, 1, 2, BUSINESS_PARTNER_CODE,
        PICKUP_POINT_CODE);
    this.level2InventoryMinimumStockAlertEvent.setTimestamp(Instant.now().toEpochMilli());

    this.level2InventoryNonOosEvent =
      new Level2InventoryNonOosEvent(STORE_ID, GDN_SKU, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
        GDN_SKU, Boolean.FALSE);
    this.level2InventoryNonOosEvent.setTimestamp(Instant.now().toEpochMilli());

    this.level2InventoryOosEvent =
      new Level2InventoryOosEvent(STORE_ID, GDN_SKU, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
        GDN_SKU, Boolean.FALSE);
    this.level2InventoryOosEvent.setTimestamp(Instant.now().toEpochMilli());
    
    Page<ProductLevel3Aggregator> productLevel3Aggregators = new PageImpl<>(new ArrayList<>());
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndBusinessPartnerCodeAndStateAndOosAndMarkForDeleteFalse(
            Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductLevel3AggregatorState.class),
            Mockito.anyBoolean(), Mockito.any(Pageable.class))).thenReturn(productLevel3Aggregators);
    Mockito.when(
        this.productLevel3AggregatorRepository
            .findByStoreIdAndBusinessPartnerCodeAndStateAndMinimumStockAndOosAndMarkForDeleteFalse(Mockito.anyString(),
                Mockito.anyString(), Mockito.any(ProductLevel3AggregatorState.class), Mockito.anyBoolean(),
                Mockito.anyBoolean(), Mockito.any(Pageable.class))).thenReturn(productLevel3Aggregators);
    this.summaryCounter = new ArrayList<>();
    this.summaryCounterActiveZero = new ArrayList<>();
    MDC.put("storeId", STORE_ID);
    
    Object[] a = new Object[2];
    a[0] = "false";
    a[1] = 10L;
    Object[] b = new Object[2];
    b[0] = "true";
    b[1] = 10L;
    this.summaryCounter.add(a);
    this.summaryCounter.add(b);
    this.summaryCounterActiveZero.add(b);

    productCountResponse = new ProductCountResponse();
    productCountResponse.setOutOfStock(COUNT);
  }

  @AfterEach
  public void _finalize() {
    Mockito.verifyNoMoreInteractions(this.productLevel3AggregatorRepository);
    Mockito.verifyNoMoreInteractions(this.productLevel3Service);
    Mockito.verifyNoMoreInteractions(xProductOutbound);
  }


  @Test
  public void findByGdnSku() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(GDN_SKU))).thenReturn(productLevel3Aggregator);
    final ProductLevel3Aggregator result = this.productLevel3AggregatorServiceBean.findByGdnSku(GDN_SKU);
    Assertions.assertEquals(this.productLevel3Aggregator, result);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
  }

  @Test
  public void updateProductLevel3AggregatorOOS_createNewAvailable() throws Exception {
    Mockito.when(this.productLevel3AggregatorRepository.save(this.productLevel3Aggregator)).thenReturn(
        this.productLevel3Aggregator);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(
        this.productLevel3Summary);

    this.productLevel3AggregatorServiceBean.updateOOS(this.level2InventoryOosEvent, 0);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any(ProductLevel3Aggregator.class));
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU);
  }

  @Test
  public void updateProductLevel3AggregatorOOS_createNewOOS() throws Exception {
    productLevel3Summary.setIsArchived(true);
    productLevel3Summary.setAvailableStockLevel2(0);
    Mockito.when(this.productLevel3AggregatorRepository.save(this.productLevel3Aggregator)).thenReturn(
        this.productLevel3Aggregator);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(
        this.productLevel3Summary);

    this.productLevel3AggregatorServiceBean.updateOOS(this.level2InventoryOosEvent, 0);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any(ProductLevel3Aggregator.class));
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU);
  }

  @Test
  public void updateProductLevel3AggregatorOOS_update() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(GDN_SKU))).thenReturn(productLevel3Aggregator);
    Mockito.when(this.productLevel3AggregatorRepository.save(this.productLevel3Aggregator)).thenReturn(
        this.productLevel3Aggregator);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(
        this.productLevel3Summary);

    this.productLevel3AggregatorServiceBean.updateOOS(this.level2InventoryOosEvent, 0);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository).save(this.productLevel3Aggregator);
  }

  @Test
  public void updateProductLevel3AggregatorOOS_update_ObjectOptimisticLockingFailureException() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(GDN_SKU))).thenReturn(productLevel3Aggregator);
    Mockito.doThrow(ObjectOptimisticLockingFailureException.class)
        .when(this.productLevel3AggregatorRepository)
            .save(Mockito.any(ProductLevel3Aggregator.class));
    
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(
        this.productLevel3Summary);

    this.productLevel3AggregatorServiceBean.updateOOS(this.level2InventoryOosEvent, 0);

    Mockito.verify(this.productLevel3AggregatorRepository, Mockito.times(4)).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository, Mockito.times(3)).save(this.productLevel3Aggregator);
  }
  
  @Test
  public void updateProductLevel3AggregatorNonOOS_createNewAvailable() throws Exception {
    Mockito.when(this.productLevel3AggregatorRepository.save(this.productLevel3Aggregator)).thenReturn(
        this.productLevel3Aggregator);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(
        this.productLevel3Summary);

    this.productLevel3AggregatorServiceBean.updateNonOOS(this.level2InventoryNonOosEvent, 0);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any(ProductLevel3Aggregator.class));
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU);
  }

  @Test
  public void updateProductLevel3AggregatorNonOOS_createNewOOS() throws Exception {
    productLevel3Summary.setIsArchived(true);
    productLevel3Summary.setAvailableStockLevel2(0);
    Mockito.when(this.productLevel3AggregatorRepository.save(this.productLevel3Aggregator)).thenReturn(
        this.productLevel3Aggregator);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(
        this.productLevel3Summary);

    this.productLevel3AggregatorServiceBean.updateNonOOS(this.level2InventoryNonOosEvent, 0);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any(ProductLevel3Aggregator.class));
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU);
  }

  @Test
  public void updateProductLevel3AggregatorNonOOS_update() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(GDN_SKU))).thenReturn(productLevel3Aggregator);
    Mockito.when(this.productLevel3AggregatorRepository.save(this.productLevel3Aggregator)).thenReturn(
        this.productLevel3Aggregator);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(
        this.productLevel3Summary);

    this.productLevel3AggregatorServiceBean.updateNonOOS(this.level2InventoryNonOosEvent, 0);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository).save(this.productLevel3Aggregator);
  }

  @Test
  public void updateProductLevel3AggregatorNonOOS_update_ObjectOptimisticLockingFailureException() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(GDN_SKU))).thenReturn(productLevel3Aggregator);
    Mockito.doThrow(ObjectOptimisticLockingFailureException.class)
        .when(this.productLevel3AggregatorRepository)
            .save(Mockito.any(ProductLevel3Aggregator.class));
    
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(
        this.productLevel3Summary);

    this.productLevel3AggregatorServiceBean.updateNonOOS(this.level2InventoryNonOosEvent, 0);

    Mockito.verify(this.productLevel3AggregatorRepository, Mockito.times(4)).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository, Mockito.times(3)).save(this.productLevel3Aggregator);
  }
  
  @Test
  public void updateProductLevel3AggregatorMinimumStock_createNewAvailable() throws Exception {
    Mockito.when(this.productLevel3AggregatorRepository.save(this.productLevel3Aggregator)).thenReturn(
        this.productLevel3Aggregator);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(
        this.productLevel3Summary);

    this.productLevel3AggregatorServiceBean.updateMinimumStock(this.level2InventoryMinimumStockAlertEvent, true, 0);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any(ProductLevel3Aggregator.class));
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU);
  }

  @Test
  public void updateProductLevel3AggregatorMinimumStock_createNewOOS() throws Exception {
    productLevel3Summary.setIsArchived(true);
    productLevel3Summary.setAvailableStockLevel2(0);
    Mockito.when(this.productLevel3AggregatorRepository.save(this.productLevel3Aggregator)).thenReturn(
        this.productLevel3Aggregator);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(
        this.productLevel3Summary);

    this.productLevel3AggregatorServiceBean.updateMinimumStock(this.level2InventoryMinimumStockAlertEvent, true, 0);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any(ProductLevel3Aggregator.class));
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU);
  }

  @Test
  public void updateProductLevel3AggregatorMinimumStock_update() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(GDN_SKU))).thenReturn(productLevel3Aggregator);
    Mockito.when(this.productLevel3AggregatorRepository.save(this.productLevel3Aggregator)).thenReturn(
        this.productLevel3Aggregator);

    this.productLevel3AggregatorServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent, true, 0);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository).save(this.productLevel3Aggregator);
  }
  
  @Test
  public void updateProductLevel3AggregatorMinimumStock_update_ObjectOptimisticLockingFailureException() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(GDN_SKU))).thenReturn(productLevel3Aggregator);
    Mockito.doThrow(ObjectOptimisticLockingFailureException.class)
        .when(this.productLevel3AggregatorRepository)
        .save(Mockito.any(ProductLevel3Aggregator.class));

    this.productLevel3AggregatorServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent, true, 0);

    Mockito.verify(this.productLevel3AggregatorRepository, Mockito.times(4)).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository, Mockito.times(3)).save(this.productLevel3Aggregator);
  }


  @Test
  public void updateTest() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any())).thenReturn(this.productLevel3Aggregator);
    productLevel3Summary.setSynchronizeStock(true);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(Mockito.any(), Mockito.any())).thenReturn(
        this.productLevel3Summary);
    Mockito.when(this.productLevel3AggregatorRepository.save(Mockito.any())).thenReturn(
        null);
    this.productLevel3AggregatorServiceBean.update(null, null);
    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any());
  }
  
  @Test
  public void updateSyncTrueWithZeroStock() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(this.productLevel3Aggregator);
    productLevel3Summary.setSynchronizeStock(true);
    productLevel3Summary.setAvailableStockLevel1(0);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(Mockito.anyString(), Mockito.anyString())).thenReturn(
        this.productLevel3Summary);
    Mockito.when(this.productLevel3AggregatorRepository.save(Mockito.any(ProductLevel3Aggregator.class))).thenReturn(
        null);
    this.productLevel3AggregatorServiceBean.update(GDN_SKU, BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        STORE_ID, GDN_SKU);
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU);
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any(ProductLevel3Aggregator.class));
  }

  @Test
  public void updateWithArchivedIsTrueAndOOSIsTrueAndMinimumStockIsTrueTest() throws Exception {
    this.productLevel3Summary.setIsArchived(true);
    this.productLevel3Summary.setAvailableStockLevel2(0);
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any())).thenReturn(this.productLevel3Aggregator);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(Mockito.any(), Mockito.any())).thenReturn(
        this.productLevel3Summary);
    Mockito.when(this.productLevel3AggregatorRepository.save(Mockito.any(ProductLevel3Aggregator.class))).thenReturn(
        null);
    this.productLevel3AggregatorServiceBean.update(null, null);
    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any());
  }

  @Test
  public void updateWithProductLevel3AggregatorIsNullTest() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any())).thenReturn(null);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(Mockito.any(), Mockito.any())).thenReturn(
        this.productLevel3Summary);
    Mockito.when(this.productLevel3AggregatorRepository.save(Mockito.any())).thenReturn(
        null);
    this.productLevel3AggregatorServiceBean.update(null, null);
    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any());
  }
  
  @Test
  public void updateProductLevel3AggregatorArchived_createNewArchived() throws Exception {
    Mockito.when(this.productLevel3AggregatorRepository.save(this.productLevel3Aggregator)).thenReturn(
        this.productLevel3Aggregator);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(
        this.productLevel3Summary);

    this.productLevel3AggregatorServiceBean.updateState(GDN_SKU, BUSINESS_PARTNER_CODE, ProductLevel3AggregatorState.ARCHIVED);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any(ProductLevel3Aggregator.class));
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU);
  }
  
  @Test
  public void updateProductLevel3AggregatorArchived_updateUnarchived() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.findByStoreIdAndGdnSkuAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(GDN_SKU))).thenReturn(productLevel3Aggregator);
    Mockito.when(this.productLevel3AggregatorRepository.save(this.productLevel3Aggregator)).thenReturn(
        this.productLevel3Aggregator);

    this.productLevel3AggregatorServiceBean.updateState(GDN_SKU, BUSINESS_PARTNER_CODE, ProductLevel3AggregatorState.ACTIVE);

    Mockito.verify(this.productLevel3AggregatorRepository).findByStoreIdAndGdnSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(GDN_SKU));
    Mockito.verify(this.productLevel3AggregatorRepository).save(this.productLevel3Aggregator);
  }

  @Test
  public void findByBusinessPartnerCodeAndInventoryFilterAndStateWithInventoryCriteriaIsNONETest() throws Exception {
    this.productLevel3AggregatorServiceBean.findByBusinessPartnerCodeAndInventoryFilterAndState(null,
        ProductLevel3AggregatorInventoryCriteria.NONE, null, PageRequest.of(0, 10));
  }

  @Test
  public void findByBusinessPartnerCodeAndInventoryFilterAndStateWithInventoryCriteriaIsAVAILABLETest()
      throws Exception {
    this.productLevel3AggregatorServiceBean.findByBusinessPartnerCodeAndInventoryFilterAndState(null,
        ProductLevel3AggregatorInventoryCriteria.AVAILABLE, null, null);
    Mockito.verify(this.productLevel3AggregatorRepository)
        .findByStoreIdAndBusinessPartnerCodeAndStateAndOosAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.any(), Mockito.any(), Mockito.anyBoolean(), Mockito.any());
  }

  @Test
  public void findByBusinessPartnerCodeAndInventoryFilterAndStateWithInventoryCriteriaIsOOSTest() throws Exception {
    this.productLevel3AggregatorServiceBean.findByBusinessPartnerCodeAndInventoryFilterAndState(null,
        ProductLevel3AggregatorInventoryCriteria.OOS, null, null);
    Mockito.verify(this.productLevel3AggregatorRepository)
        .findByStoreIdAndBusinessPartnerCodeAndStateAndOosAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString(),
            Mockito.any(), Mockito.any(), Mockito.anyBoolean(), Mockito.any());
  }

  @Test
  public void findByBusinessPartnerCodeAndInventoryFilterAndStateWithInventoryCriteriaIsMINIMUM_STOCKTest()
      throws Exception {
    this.productLevel3AggregatorServiceBean.findByBusinessPartnerCodeAndInventoryFilterAndState(null,
        ProductLevel3AggregatorInventoryCriteria.MINIMUM_STOCK, null, null);
    Mockito.verify(this.productLevel3AggregatorRepository)
        .findByStoreIdAndBusinessPartnerCodeAndStateAndMinimumStockAndOosAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.any(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any());
  }

  @Test
  public void countSummaryTest() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.countSummaryOosByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(
            Mockito.anyString(), Mockito.anyString())).thenReturn(summaryCounter);
    Mockito.when(
        this.productLevel3AggregatorRepository.countSummaryMinimumStockByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(
            Mockito.anyString(), Mockito.anyString())).thenReturn(10L);
    Mockito.when(xProductOutbound.getProductCountByType(BUSINESS_PARTNER_CODE, Constants.TYPE_OF_PRODUCT))
        .thenReturn(productCountResponse);
    ProductLevel3SummaryCount productLevel3SummaryCount =
        this.productLevel3AggregatorServiceBean.countSummary(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3AggregatorRepository)
        .countSummaryOosByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.productLevel3AggregatorRepository)
        .countSummaryMinimumStockByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(xProductOutbound).getProductCountByType(BUSINESS_PARTNER_CODE, Constants.TYPE_OF_PRODUCT);
    Assertions.assertEquals(productLevel3SummaryCount.getStockConditionCounts().get(ProductLevel3InventoryCriteria.OOS),
        COUNT);
    Assertions.assertEquals(productLevel3SummaryCount.getStockConditionCounts().get(ProductLevel3InventoryCriteria.AVAILABLE),
        ACTIVE_COUNT);
  }

  @Test
  public void countSummary121Test() throws Exception {
    Mockito.when(
        this.productLevel3AggregatorRepository.countSummaryOosByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(
            Mockito.anyString(), Mockito.anyString())).thenReturn(summaryCounterActiveZero);
    Mockito.when(
        this.productLevel3AggregatorRepository.countSummaryMinimumStockByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(
            Mockito.anyString(), Mockito.anyString())).thenReturn(10L);
    Mockito.when(xProductOutbound.getProductCountByType(BUSINESS_PARTNER_CODE, Constants.TYPE_OF_PRODUCT))
        .thenReturn(productCountResponse);
    ProductLevel3SummaryCount productLevel3SummaryCount =
        this.productLevel3AggregatorServiceBean.countSummary(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3AggregatorRepository)
        .countSummaryOosByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.productLevel3AggregatorRepository)
        .countSummaryMinimumStockByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(xProductOutbound).getProductCountByType(BUSINESS_PARTNER_CODE, Constants.TYPE_OF_PRODUCT);
    Assertions.assertEquals(productLevel3SummaryCount.getStockConditionCounts().get(ProductLevel3InventoryCriteria.OOS),
        COUNT);
    Assertions.assertEquals(productLevel3SummaryCount.getStockConditionCounts().get(ProductLevel3InventoryCriteria.AVAILABLE),
        ZERO_COUNT);
  }
  
  @Test
  public void countSummaryNullTest() throws Exception {
    Mockito.when(this.productLevel3AggregatorRepository.countSummaryOosByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString())).thenReturn(null);
    this.productLevel3AggregatorServiceBean.countSummary(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3AggregatorRepository).countSummaryOosByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void createTest() throws Exception {
    this.productLevel3AggregatorServiceBean.create(new ProductLevel3Aggregator());
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any(ProductLevel3Aggregator.class));
  }

  @Test
  public void createTest1() throws Exception {
    this.productLevel3Summary.setIsArchived(false);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(productLevel3Summary);
    this.productLevel3AggregatorServiceBean.create(GDN_SKU, BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU);
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any(ProductLevel3Aggregator.class));
  }

  @Test
  public void createTest2() throws Exception {
    this.productLevel3Summary.setIsArchived(true);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU)).thenReturn(productLevel3Summary);
    this.productLevel3AggregatorServiceBean.create(GDN_SKU, BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_SKU);
    Mockito.verify(this.productLevel3AggregatorRepository).save(Mockito.any(ProductLevel3Aggregator.class));
  }

  @Test
  public void deleteNullListTest() throws Exception {
    Exception exception = new Exception();
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3AggregatorServiceBean.delete(STORE_ID, Collections.EMPTY_LIST);
      });
    } catch (Exception e) {
      exception = e;
      Assertions.assertTrue(exception.getMessage().contains(GDN_SKUS_CANNOT_EMPTY));
    } finally {
    }
  }

  @Test
  public void deleteTest() throws Exception {
    Mockito.when(this.productLevel3AggregatorRepository
        .findByStoreIdAndGdnSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(GDN_SKU)))
        .thenReturn(Collections.singletonList(productLevel3Aggregator));
    this.productLevel3AggregatorServiceBean.delete(STORE_ID, Collections.singletonList(GDN_SKU));
    Mockito.verify(productLevel3AggregatorRepository)
        .findByStoreIdAndGdnSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(GDN_SKU));
    Mockito.verify(productLevel3AggregatorRepository).saveAll(listArgumentCaptor.capture());
    Assertions.assertEquals(GDN_SKU, listArgumentCaptor.getValue().get(0).getGdnSku());
    Assertions.assertTrue(listArgumentCaptor.getValue().get(0).isMarkForDelete());
  }
}
