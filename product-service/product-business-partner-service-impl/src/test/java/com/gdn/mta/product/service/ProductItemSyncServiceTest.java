package com.gdn.mta.product.service;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

import com.gdn.mta.product.entity.ProductItemSyncProcessSummary;
import com.gdn.mta.product.entity.ProductItemSyncStatus;
import com.gdn.mta.product.enums.ProductSyncStatus;
import com.gdn.mta.product.repository.ProductItemSyncStatusRepository;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

/**
 * @author anand
 * @since Sep 2019
 */
public class ProductItemSyncServiceTest {

  private static final String STORE_ID = "STORE_ID";

  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";

  private static final String GDN_ITEM_SKU = "GDN_ITEM_SKU";

  private static final String GDN_PRODUCT_SKU = "GDN_PRODUCT_SKU";

  private static final String PROCESS_ID = "PROCESS_ID";

  private static final String USERNAME = "USERNAME";

  private static final String LINKED_BUSINESS_PARTNER_CODE = "SOA-21412";

  private static final String DEFAULT_PICKUP_POINT = "pickupPoint";

  @InjectMocks
  private ProductItemSyncServiceImpl service;

  @Mock
  private ProductItemSyncStatusRepository syncStatusRepository;

  @Mock
  private ProductStatusPublisherService productStatusPublisherService;

  @Captor
  private ArgumentCaptor<ProductItemSyncStatus> syncStatusCaptor;

  @Captor
  private ArgumentCaptor<Date> dateCaptor;

  @BeforeEach
  public void init(){
    MockitoAnnotations.initMocks(this);

  }

  @AfterEach
  public void cleanup(){
    Mockito.verifyNoMoreInteractions(syncStatusRepository);
    Mockito.verifyNoMoreInteractions(productStatusPublisherService);
  }

  @Test
  public void save() {
    ProductItemSyncStatus itemSyncStatus = new ProductItemSyncStatus();

    service.save(itemSyncStatus);

    Mockito.verify(syncStatusRepository).save(itemSyncStatus);
  }

  @Test
  public void findItemsAlreadyInSyncProcessTest() {
    Mockito.when(syncStatusRepository
      .findByStoreIdAndBusinessPartnerCodeAndLinkedBusinessPartnerCodeAndProductSyncStatus(STORE_ID,
        BUSINESS_PARTNER_CODE, LINKED_BUSINESS_PARTNER_CODE, ProductSyncStatus.IN_PROGRESS))
      .thenReturn(Arrays.asList(ProductItemSyncStatus.builder().gdnItemSku(GDN_ITEM_SKU).build()));

    List<String> response = service
      .findItemsAlreadyInSyncProcess(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_BUSINESS_PARTNER_CODE);

    Assertions.assertEquals(Arrays.asList(GDN_ITEM_SKU), response);
    
    Mockito.verify(syncStatusRepository)
      .findByStoreIdAndBusinessPartnerCodeAndLinkedBusinessPartnerCodeAndProductSyncStatus(STORE_ID,
        BUSINESS_PARTNER_CODE, LINKED_BUSINESS_PARTNER_CODE, ProductSyncStatus.IN_PROGRESS);
  }

  @Test
  public void findByItemSkuAndBusinessPartnerCodeTest() {
    ProductItemSyncStatus itemSyncStatus = new ProductItemSyncStatus();

    Mockito.when(syncStatusRepository
      .findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(STORE_ID, BUSINESS_PARTNER_CODE, GDN_ITEM_SKU))
    .thenReturn(itemSyncStatus);

    ProductItemSyncStatus response = service
      .findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE);

    Assertions.assertEquals(itemSyncStatus, response);

    Mockito.verify(syncStatusRepository)
      .findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(STORE_ID, BUSINESS_PARTNER_CODE, GDN_ITEM_SKU);
  }

  @Test
  public void productCopyStatusForProcessIDTest() {
    ProductItemSyncProcessSummary summary = new ProductItemSyncProcessSummary();

    Mockito.when(syncStatusRepository.getCountByProcessIdGroupByProductSyncStatus(STORE_ID, PROCESS_ID))
      .thenReturn(Arrays.asList(summary));

    List<ProductItemSyncProcessSummary> summaryList = service.productCopyStatusForProcessID(STORE_ID, PROCESS_ID);

    Assertions.assertEquals(summary, summaryList.get(0));

    Mockito.verify(syncStatusRepository).getCountByProcessIdGroupByProductSyncStatus(STORE_ID, PROCESS_ID);
  }

  @Test
  public void copyProductsTest_whenStatusInProgress() {
    Mockito.when(syncStatusRepository
      .findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(STORE_ID, BUSINESS_PARTNER_CODE, GDN_ITEM_SKU))
      .thenReturn(ProductItemSyncStatus.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .gdnItemSku(GDN_ITEM_SKU)
        .processId("process-id-existing")
        .productSyncStatus(ProductSyncStatus.IN_PROGRESS)
        .linkedBusinessPartnerCode(LINKED_BUSINESS_PARTNER_CODE)
        .build()
      );

    service.copy(STORE_ID, USERNAME, BUSINESS_PARTNER_CODE, DEFAULT_PICKUP_POINT, false,
      "process-id", Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU)), LINKED_BUSINESS_PARTNER_CODE);

    Mockito
      .verify(syncStatusRepository)
      .findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(STORE_ID, BUSINESS_PARTNER_CODE, GDN_ITEM_SKU);
  }

  @Test
  public void copyProductsTest_whenItemAlreadyCopied() {
    Mockito.when(syncStatusRepository
      .findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(STORE_ID, BUSINESS_PARTNER_CODE, GDN_ITEM_SKU))
      .thenReturn(ProductItemSyncStatus.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .gdnItemSku(GDN_ITEM_SKU)
        .processId("process-id-existing")
        .productSyncStatus(ProductSyncStatus.SUCCESS)
        .linkedBusinessPartnerCode(LINKED_BUSINESS_PARTNER_CODE)
        .build()
      );

    service.copy(STORE_ID, USERNAME, BUSINESS_PARTNER_CODE, DEFAULT_PICKUP_POINT, false,
      "process-id", Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU)), LINKED_BUSINESS_PARTNER_CODE);

    Mockito
      .verify(syncStatusRepository)
      .findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(STORE_ID, BUSINESS_PARTNER_CODE, GDN_ITEM_SKU);
  }

  @Test
  public void copyProductsTest_WhenRetryTrueAndStatusInProgress() {
    Mockito
      .when(syncStatusRepository
        .findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(STORE_ID, BUSINESS_PARTNER_CODE, GDN_ITEM_SKU))
      .thenReturn(ProductItemSyncStatus.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .gdnItemSku(GDN_ITEM_SKU)
        .processId("process-id-existing")
        .productSyncStatus(ProductSyncStatus.IN_PROGRESS)
        .linkedBusinessPartnerCode(LINKED_BUSINESS_PARTNER_CODE)
        .attempt(2)
        .build()
      );

    service
      .copy(STORE_ID, USERNAME, BUSINESS_PARTNER_CODE, DEFAULT_PICKUP_POINT, true, PROCESS_ID,
        Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU)), LINKED_BUSINESS_PARTNER_CODE);

    Mockito
      .verify(syncStatusRepository)
      .findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(STORE_ID, BUSINESS_PARTNER_CODE, GDN_ITEM_SKU);

    Mockito
      .verify(productStatusPublisherService)
      .publishCreateProductSyncEvent(STORE_ID, USERNAME, Arrays.asList(GDN_ITEM_SKU), BUSINESS_PARTNER_CODE,
        DEFAULT_PICKUP_POINT);

    Mockito.verify(syncStatusRepository).save(syncStatusCaptor.capture());

    Assertions.assertNotNull(syncStatusCaptor.getValue().getProcessId());
    Assertions.assertEquals(PROCESS_ID, syncStatusCaptor.getValue().getProcessId());
    Assertions.assertEquals(3, syncStatusCaptor.getValue().getAttempt());
    Assertions.assertEquals(ProductSyncStatus.IN_PROGRESS, syncStatusCaptor.getValue().getProductSyncStatus());
  }

  @Test
  public void copyProductsTest_WhenStatusInFail() {
    Mockito
      .when(syncStatusRepository
        .findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(STORE_ID, BUSINESS_PARTNER_CODE, GDN_ITEM_SKU))
      .thenReturn(ProductItemSyncStatus.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .gdnItemSku(GDN_ITEM_SKU)
        .processId("process-id-existing")
        .productSyncStatus(ProductSyncStatus.FAIL)
        .linkedBusinessPartnerCode(LINKED_BUSINESS_PARTNER_CODE)
        .attempt(2)
        .build()
      );

    service
      .copy(STORE_ID, USERNAME, BUSINESS_PARTNER_CODE, DEFAULT_PICKUP_POINT, false, PROCESS_ID,
        Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU)), LINKED_BUSINESS_PARTNER_CODE);

    Mockito
      .verify(syncStatusRepository)
      .findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(STORE_ID, BUSINESS_PARTNER_CODE, GDN_ITEM_SKU);

    Mockito
      .verify(productStatusPublisherService)
      .publishCreateProductSyncEvent(STORE_ID, USERNAME, Arrays.asList(GDN_ITEM_SKU), BUSINESS_PARTNER_CODE,
        DEFAULT_PICKUP_POINT);

    Mockito.verify(syncStatusRepository).save(syncStatusCaptor.capture());
    Assertions.assertNotNull(syncStatusCaptor.getValue().getProcessId());

    Assertions.assertEquals(ProductSyncStatus.IN_PROGRESS, syncStatusCaptor.getValue().getProductSyncStatus());
    Assertions.assertEquals(PROCESS_ID, syncStatusCaptor.getValue().getProcessId());
    Assertions.assertEquals(3, syncStatusCaptor.getValue().getAttempt());
  }

  @Test
  public void copyProductsTest_WhenNoProcessExists() {
    service
      .copy(STORE_ID, USERNAME, BUSINESS_PARTNER_CODE, DEFAULT_PICKUP_POINT, false, PROCESS_ID,
        Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU)), LINKED_BUSINESS_PARTNER_CODE);

    Mockito
      .verify(syncStatusRepository)
      .findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(STORE_ID, BUSINESS_PARTNER_CODE, GDN_ITEM_SKU);

    Mockito
      .verify(productStatusPublisherService)
      .publishCreateProductSyncEvent(STORE_ID, USERNAME, Arrays.asList(GDN_ITEM_SKU), BUSINESS_PARTNER_CODE,
        DEFAULT_PICKUP_POINT);

    Mockito.verify(syncStatusRepository).save(syncStatusCaptor.capture());
    Assertions.assertNotNull(syncStatusCaptor.getValue().getProcessId());
    Assertions.assertEquals(1, syncStatusCaptor.getValue().getAttempt());
    Assertions.assertEquals(PROCESS_ID, syncStatusCaptor.getValue().getProcessId());
    Assertions.assertEquals(ProductSyncStatus.IN_PROGRESS, syncStatusCaptor.getValue().getProductSyncStatus());
  }

  @Test
  public void updateProductItemSyncStatusTest() {
    service.updateProductItemSyncStatus(STORE_ID, 24L);
    Mockito.verify(syncStatusRepository)
      .updateSyncStatusForSyncRetry(Mockito.eq(STORE_ID), dateCaptor.capture(), Mockito.eq(ProductSyncStatus.IN_PROGRESS), Mockito.eq(ProductSyncStatus.FAIL));
    Date syncRetryDate = new Date(System.currentTimeMillis() - TimeUnit.HOURS.toMillis(24L));
    Assertions.assertEquals( syncRetryDate.getDate(), dateCaptor.getValue().getDate());
  }
}
