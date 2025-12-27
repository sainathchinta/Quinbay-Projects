package com.gdn.x.product.service.impl;


import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.dao.solr.api.ProductSolrRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.config.KafkaPublisher;

public class ProductL3SolrServiceImplTest {

  @InjectMocks
  private ProductL3SolrServiceImpl productL3SolrService;

  @Mock
  private ProductSolrRepository productSolrRepository;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Captor
  private ArgumentCaptor<List<Item>> listArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductAndItemEventModel> productAndItemEventModel;


  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";
  private static final String MERCHANT_CODE = "merchantCode";

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productSolrRepository);
    Mockito.verifyNoMoreInteractions(itemPickupPointService);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
  }

  @Test
  public void updateStockStatusInL3Solr() {
    productL3SolrService.updateStockStatusInL3Solr(SolrFieldNames.PRODUCT_SKU,
      Constants.AVAILABLE, MERCHANT_CODE);
    Mockito.verify(productSolrRepository).updateStockStatus(SolrFieldNames.PRODUCT_SKU, true,
      MERCHANT_CODE);
  }

  @Test
  public void updateStockStatusInL3SolrStatusFalse() {
    productL3SolrService.updateStockStatusInL3Solr(SolrFieldNames.PRODUCT_SKU,
      SolrFieldNames.PRODUCT_SKU, MERCHANT_CODE);
    Mockito.verify(productSolrRepository).updateStockStatus(SolrFieldNames.PRODUCT_SKU, false,
      MERCHANT_CODE);
  }

  @Test
  public void updatePromoOrWholesaleItemSkus() {
    Item item = new Item();
    productL3SolrService.updatePromoOrWholesaleItemSkus(Arrays.asList(item), false);
    Mockito.verify(productSolrRepository)
        .updatePromoOrWholesaleItemSkus(listArgumentCaptor.capture(), Mockito.eq(false));
  }

  @Test
  public void updatePromoOrWholesaleItemSkusByItemPickupPoint() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(
            itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(),
                Mockito.anyString()))
        .thenReturn(Arrays.asList(itemPickupPoint));
    productL3SolrService.updatePromoOrWholesaleItemSkusByItemPickupPoint(itemPickupPoint);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(Mockito.anyList());
  }

  @Test
  public void updatePromoOrWholesaleItemSkusByItemPickupPointViaEventTest() {
    ReflectionTestUtils.setField(productL3SolrService, "eventBasedSolrUpdateEnable", true);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString()))
        .thenReturn(Arrays.asList(itemPickupPoint));
    productL3SolrService.updatePromoOrWholesaleItemSkusByItemPickupPoint(itemPickupPoint);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModel.capture());
    Assertions.assertEquals(productAndItemEventModel.getValue().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void updatePromoOrWholesaleItemSkusByItemPickupPointsViaEventTest() {
    ReflectionTestUtils.setField(productL3SolrService, "eventBasedSolrUpdateEnable", true);
    String id = UUID.randomUUID().toString();
    ItemPickupPoint itemPickupPoint1 =
        ItemPickupPoint.builder().merchantCode(MERCHANT_CODE).productSku(PRODUCT_SKU).price(new HashSet<>()).build();
    itemPickupPoint1.setId(id);
    ItemPickupPoint itemPickupPoint2 =
        ItemPickupPoint.builder().merchantCode(MERCHANT_CODE).productSku(PRODUCT_SKU).build();
    itemPickupPoint2.setId(id);
    ItemPickupPoint itemPickupPoint3 =
        ItemPickupPoint.builder().merchantCode(MERCHANT_CODE).productSku(PRODUCT_SKU).price(new HashSet<>()).build();
    itemPickupPoint3.setId(UUID.randomUUID().toString());
    Mockito.when(itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(itemPickupPoint2, itemPickupPoint3));
    Mockito.doNothing().when(productSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    productL3SolrService.updatePromoOrWholesaleItemSkusByItemPickupPoint(Arrays.asList(itemPickupPoint1));
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR), eq(PRODUCT_SKU),
      productAndItemEventModel.capture());
    Assertions.assertEquals(productAndItemEventModel.getValue().getProductSku(), PRODUCT_SKU);
  }


  @Test
  public void updatePromoOrWholesaleItemSkusByItemPickupPointEmptyTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    Mockito.when(
            itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(),
                Mockito.anyString()))
        .thenReturn(new ArrayList<>());
    productL3SolrService.updatePromoOrWholesaleItemSkusByItemPickupPoint(ItemPickupPoint.builder().pickupPointCode(PICKUP_POINT_CODE).build());
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.isNull());
  }

  @Test
  public void updatePromoOrWholesaleItemSkusByItemPickupPointTest() {
    String id = UUID.randomUUID().toString();
    ItemPickupPoint itemPickupPoint1 = ItemPickupPoint.builder().productSku(PRODUCT_SKU).price(new HashSet<>()).build();
    itemPickupPoint1.setId(id);
    ItemPickupPoint itemPickupPoint2 = ItemPickupPoint.builder().productSku(PRODUCT_SKU).build();
    itemPickupPoint2.setId(id);
    ItemPickupPoint itemPickupPoint3 = ItemPickupPoint.builder().productSku(PRODUCT_SKU).price(new HashSet<>()).build();
    itemPickupPoint3.setId(UUID.randomUUID().toString());

    Mockito.when(itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(itemPickupPoint2, itemPickupPoint3));
    Mockito.doNothing().when(productSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());

    productL3SolrService.updatePromoOrWholesaleItemSkusByItemPickupPoint(Arrays.asList(itemPickupPoint1));

    Mockito.verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU);
    Mockito.verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(Mockito.anyList());
  }

  @Test
  public void updatePromoOrWholesaleItemSkusByItemPickupPointEmptyDataTest() {
    String id = UUID.randomUUID().toString();
    ItemPickupPoint itemPickupPoint1 = ItemPickupPoint.builder().productSku(PRODUCT_SKU).price(new HashSet<>()).build();
    itemPickupPoint1.setId(id);
    ItemPickupPoint itemPickupPoint2 = ItemPickupPoint.builder().productSku(PRODUCT_SKU).build();
    itemPickupPoint2.setId(id);
    ItemPickupPoint itemPickupPoint3 = ItemPickupPoint.builder().productSku(PRODUCT_SKU).price(new HashSet<>()).build();
    itemPickupPoint3.setId(UUID.randomUUID().toString());

    Mockito.when(itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        PRODUCT_SKU)).thenReturn(new ArrayList<>());

    productL3SolrService.updatePromoOrWholesaleItemSkusByItemPickupPoint(Arrays.asList(itemPickupPoint1));

    Mockito.verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU);
  }
}