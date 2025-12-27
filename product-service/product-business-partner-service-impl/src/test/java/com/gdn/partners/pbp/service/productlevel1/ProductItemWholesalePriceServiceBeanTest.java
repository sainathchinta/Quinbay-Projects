package com.gdn.partners.pbp.service.productlevel1;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
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
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.ItemPickupPointDto;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.repository.ProductItemWholesalePriceCustomRepository;
import com.gdn.mta.product.repository.ProductItemWholesalePriceRepository;
import com.gdn.partners.pbp.service.productlevel3.ProductItemWholesalePriceServiceBean;

public class ProductItemWholesalePriceServiceBeanTest {

  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_SKU1 = "itemSku1";
  private static final String ITEM_SKU2 = "itemSku2";
  private static final String STORE_ID = "storeId";
  private static final String ITEM_ID = "itemId";
  private static final String PICKUP_POINT_CODE = "ppCode";
  private static final String WHOLESALE_RULE_1 = "[{\"quantity\":3,\"wholesaleDiscount\":30.0]";

  @Mock
  private ProductItemWholesalePriceRepository productItemWholesalePriceRepository;

  @Mock
  private ProductItemWholesalePriceCustomRepository productItemWholesalePriceCustomRepository;

  @InjectMocks
  private ProductItemWholesalePriceServiceBean productItemWholesalePriceService;

  @Captor
  private ArgumentCaptor<List<ProductItemWholesalePrice>> listArgumentCaptor;

  private List<ProductItemWholesalePrice> productItemWholesalePrices;
  private List<String> itemSkus;
  private List<ProductItemWholesalePrice> wholesalePriceList;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    itemSkus = new ArrayList<>();
    itemSkus.add(ITEM_SKU);
    itemSkus.add(ITEM_SKU1);
    itemSkus.add(ITEM_SKU2);
    ProductItemWholesalePrice productItemWholesalePrice = new ProductItemWholesalePrice();
    productItemWholesalePrice.setItemSku(ITEM_SKU);
    productItemWholesalePrice.setWholesaleRules(WHOLESALE_RULE_1);
    wholesalePriceList = Arrays.asList(productItemWholesalePrice);
    productItemWholesalePrices = new ArrayList<>();
    productItemWholesalePrices.add(new ProductItemWholesalePrice());
    productItemWholesalePrices.add(new ProductItemWholesalePrice());

    ReflectionTestUtils.setField(productItemWholesalePriceService, "itemWholesalePriceBatchSize", 10);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productItemWholesalePriceRepository);
  }

  @Test
  public void saveWholesalePrice() {
    this.productItemWholesalePriceService.saveWholesalePrice(productItemWholesalePrices);
    Mockito.verify(productItemWholesalePriceRepository).saveAll(listArgumentCaptor.capture());
  }

  @Test
  public void saveWholesalePriceNew() {
    this.productItemWholesalePriceService.saveWholesalePriceNew(productItemWholesalePrices);
    Mockito.verify(productItemWholesalePriceRepository).saveAll(listArgumentCaptor.capture());
  }

  @Test
  public void findByStoreIdAndGdnSkus() throws IOException {
    Mockito.when(this.productItemWholesalePriceRepository.findByStoreIdAndItemSkuIn(STORE_ID, itemSkus))
        .thenReturn(wholesalePriceList);
    List<ProductItemWholesalePrice> productItemWholesalePrices =
        this.productItemWholesalePriceService.findByStoreIdAndItemSkus(STORE_ID, itemSkus);
    Mockito.verify(productItemWholesalePriceRepository).findByStoreIdAndItemSkuIn(STORE_ID, itemSkus);
    Assertions.assertNotNull(productItemWholesalePrices);
    Assertions.assertEquals(productItemWholesalePrices.get(0).getItemSku(), ITEM_SKU);
  }

  @Test
  public void findByStoreIdAndProductItemIdTest() {
    Mockito.when(this.productItemWholesalePriceRepository.findByStoreIdAndProductItemIdIn(STORE_ID, Arrays.asList(ITEM_ID)))
        .thenReturn(wholesalePriceList);
    List<ProductItemWholesalePrice> productItemWholesalePrices =
        this.productItemWholesalePriceService.findByStoreIdAndProductItemId(STORE_ID, Arrays.asList(ITEM_ID));
    Mockito.verify(productItemWholesalePriceRepository).findByStoreIdAndProductItemIdIn(STORE_ID, Arrays.asList(ITEM_ID));
    Assertions.assertNotNull(productItemWholesalePrices);
    Assertions.assertEquals(productItemWholesalePrices.get(0).getItemSku(), ITEM_SKU);
  }

  @Test
  public void findByStoreIdAndProductItemIdNoResponseTest() {
    Mockito.when(this.productItemWholesalePriceRepository.findByStoreIdAndProductItemIdIn(STORE_ID, Arrays.asList(ITEM_ID)))
        .thenReturn(new ArrayList<>());
    List<ProductItemWholesalePrice> productItemWholesalePrices =
        this.productItemWholesalePriceService.findByStoreIdAndProductItemId(STORE_ID, Arrays.asList(ITEM_ID));
    Mockito.verify(productItemWholesalePriceRepository).findByStoreIdAndProductItemIdIn(STORE_ID, Arrays.asList(ITEM_ID));
    Assertions.assertTrue(productItemWholesalePrices.isEmpty());
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCodeTest() {
    ItemPickupPointDto itemPickupPointDto = new ItemPickupPointDto();
    Mockito.when(productItemWholesalePriceCustomRepository.findProductItemWholesalePriceByItemSkuAndPickupPointCode(STORE_ID,
        Arrays.asList(itemPickupPointDto))).thenReturn(new ArrayList<>());
    productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID,
        Arrays.asList(itemPickupPointDto));
    Mockito.verify(productItemWholesalePriceCustomRepository)
        .findProductItemWholesalePriceByItemSkuAndPickupPointCode(STORE_ID, Arrays.asList(itemPickupPointDto));
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCodeEmptyListTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, new ArrayList<>());
    });
  }

  @Test
  public void findOneByStoreIdAndItemSkuAndPickupPointCodeTest() {
    productItemWholesalePriceService.findOneByItemSkuAndPickupPointCode(ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(this.productItemWholesalePriceRepository)
      .findFirstByItemSkuAndPickupPointCode(ITEM_SKU, PICKUP_POINT_CODE);
  }

  @Test
  public void findOneByStoreIdAndItemSkuAndPickupPointCodeTest1() {
    productItemWholesalePriceService.findOneByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.productItemWholesalePriceRepository)
        .findFirstByStoreIdAndItemSkuAndPickupPointCode(STORE_ID,ITEM_SKU, PICKUP_POINT_CODE);
  }
}