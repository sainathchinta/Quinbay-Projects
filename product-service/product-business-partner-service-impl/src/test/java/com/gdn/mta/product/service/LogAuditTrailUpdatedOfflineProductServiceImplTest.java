package com.gdn.mta.product.service;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.mta.product.entity.LogAuditTrailUpdatedOfflineProduct;
import com.gdn.mta.product.repository.LogAuditTrailUpdatedOfflineProductRepository;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.Collections;
import java.util.List;

public class LogAuditTrailUpdatedOfflineProductServiceImplTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_ITEM_SKU = "item-sku";
  private static final String DEFAULT_PICKUP_POINT_CODE = "pickup-point-code";

  @InjectMocks
  private LogAuditTrailUpdatedOfflineProductServiceImpl service;

  @Mock
  private LogAuditTrailUpdatedOfflineProductRepository repository;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getOfflineProductAuditLogsByItemSkuAndPickupPointCodeTest() {
    Pageable pageable = PageRequest.of(0, 10);
    this.service.getOfflineProductAuditLogsByItemSkuAndPickupPointCode(DEFAULT_STORE_ID,
        DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE, pageable);
    Mockito.verify(this.repository).findByItemSkuAndPickupPointCodeAndStoreIdOrderByAccessTimeDesc(
        DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE, DEFAULT_STORE_ID, pageable);
  }

  @Test
  public void getOfflineProductAuditLogsByItemSkuAndPickupPointCodeEmptyItemSkuTest() {
    Pageable pageable = PageRequest.of(0, 10);
    Page<LogAuditTrailUpdatedOfflineProduct> result= null;
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.service.getOfflineProductAuditLogsByItemSkuAndPickupPointCode(DEFAULT_STORE_ID, StringUtils.EMPTY,
            DEFAULT_PICKUP_POINT_CODE, pageable);
      });
    } finally {

    }
  }

  @Test
  public void getOfflineProductAuditLogsByItemSkuAndPickupPointCodeEmptyPickupPointCodeTest() {
    Pageable pageable = PageRequest.of(0, 10);
    Page<LogAuditTrailUpdatedOfflineProduct> result= null;
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.service.getOfflineProductAuditLogsByItemSkuAndPickupPointCode(DEFAULT_STORE_ID,
            DEFAULT_ITEM_SKU, StringUtils.EMPTY, pageable);
      });
    } finally {
      Assertions.assertNull(result);
    }
  }
  
  @Test
  public void saveTest() {
    LogAuditTrailUpdatedOfflineProduct logAuditTrailUpdatedOfflineProduct =
        new LogAuditTrailUpdatedOfflineProduct();
    this.service.save(logAuditTrailUpdatedOfflineProduct);
    Mockito.verify(this.repository).save(logAuditTrailUpdatedOfflineProduct);
  }

  @Test
  public void saveTest_list() {
    List<LogAuditTrailUpdatedOfflineProduct> logAudits =
        Collections.singletonList(new LogAuditTrailUpdatedOfflineProduct());
    this.service.save(logAudits);
    Mockito.verify(this.repository).saveAll(Mockito.eq(logAudits));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.repository);
  }
}
