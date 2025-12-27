package com.gdn.x.product.service.event.listener;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.domain.event.model.ItemEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.domain.event.model.ProductEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.google.common.collect.ImmutableSet;

public class SolrUpdateProductAndItemToSolrListenerTest {

  private static final String MESSAGE = "message";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String MERCHANT_CODE_1 = "merchantCode1";
  private static final String MERCHANT_CODE_2 = "merchantCode2";
  private static final String MERCHANT_CODE_3 = "merchantCode3";


  @InjectMocks
  private SolrUpdateProductAndItemToSolrListener solrAddProductToSolrListener;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ProductService productService;


  @Captor
  private ArgumentCaptor<ProductAndItemsVO> productAndItemsVOArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<Item> itemArgumentCaptor;

  private ProductAndItemEventModel productAndItemEventModel;
  private Map<String, Object> fieldsAndValues = new HashMap<>();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    productAndItemEventModel = new ProductAndItemEventModel();
    productAndItemEventModel.setProduct(new ProductEventModel());
    productAndItemEventModel.setItems(new ArrayList<>());
    fieldsAndValues.put(PRODUCT_SKU, PRODUCT_SKU);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productAndItemSolrIndexerService);
    Mockito.verifyNoMoreInteractions(objectConverterService);
  }

  @Test
  public void onDomainEventConsumedProductAndItemTest() throws Exception {
    ReflectionTestUtils.setField(solrAddProductToSolrListener, "blackListSellersForL3Reindex",
      "");
    productAndItemEventModel.getItems().add(new ItemEventModel());
    productAndItemEventModel.setMerchantCode(MERCHANT_CODE_1);
    Mockito.when(objectConverterService.convertToProduct(productAndItemEventModel.getProduct()))
        .thenReturn(new Product());
    Mockito.when(objectConverterService.convertToListItem(productAndItemEventModel.getItems(), null, false))
        .thenReturn(Arrays.asList(new Item()));
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class))
        .thenReturn(productAndItemEventModel);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .applyProductAndItems(Mockito.any(ProductAndItemsVO.class), Mockito.anyBoolean());
    this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.productAndItemSolrIndexerService)
        .applyProductAndItems(productAndItemsVOArgumentCaptor.capture(), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).convertToProduct(productAndItemEventModel.getProduct());
    Mockito.verify(objectConverterService).convertToListItem(productAndItemEventModel.getItems(), null, false);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
  }

  @Test
  public void onDomainEventConsumedProductTest() throws Exception {
    ReflectionTestUtils.setField(solrAddProductToSolrListener, "blackListSellersForL3Reindex",
      "");
    productAndItemEventModel.setMerchantCode(MERCHANT_CODE_1);
    Mockito.when(objectConverterService.convertToProduct(productAndItemEventModel.getProduct()))
        .thenReturn(new Product());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class))
        .thenReturn(productAndItemEventModel);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).applyProduct(Mockito.any(Product.class), Mockito.anyBoolean());
    this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.productAndItemSolrIndexerService).applyProduct(productArgumentCaptor.capture(), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).convertToProduct(productAndItemEventModel.getProduct());
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
  }

  @Test
  public void onDomainEventConsumedProductCallInvTest() throws Exception {
    ReflectionTestUtils.setField(solrAddProductToSolrListener, "blackListSellersForL3Reindex", "");
    ReflectionTestUtils.setField(solrAddProductToSolrListener, "l3AtomicUpdateEnabled", true);
    productAndItemEventModel.setMerchantCode(MERCHANT_CODE_1);
    productAndItemEventModel.setSkipInventoryCallForAtomicUpdate(false);
    Mockito.when(objectConverterService.convertToProduct(productAndItemEventModel.getProduct()))
        .thenReturn(new Product());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class))
        .thenReturn(productAndItemEventModel);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).applyProduct(Mockito.any(Product.class), Mockito.anyBoolean());
    this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.productAndItemSolrIndexerService).applyProduct(productArgumentCaptor.capture(), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).convertToProduct(productAndItemEventModel.getProduct());
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
  }

  @Test
  public void onDomainEventConsumedProductCallInvNewTest() throws Exception {
    ReflectionTestUtils.setField(solrAddProductToSolrListener, "blackListSellersForL3Reindex", "");
    ReflectionTestUtils.setField(solrAddProductToSolrListener, "l3AtomicUpdateEnabled", true);
    productAndItemEventModel.setMerchantCode(MERCHANT_CODE_1);
    productAndItemEventModel.setSkipInventoryCallForAtomicUpdate(true);
    Mockito.when(objectConverterService.convertToProduct(productAndItemEventModel.getProduct()))
        .thenReturn(new Product());
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class))
        .thenReturn(productAndItemEventModel);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).applyProduct(Mockito.any(Product.class), Mockito.anyBoolean());
    this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.productAndItemSolrIndexerService).applyProduct(productArgumentCaptor.capture(), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).convertToProduct(productAndItemEventModel.getProduct());
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
  }

  @Test
  public void onDomainEventConsumedItemListTest() throws Exception {
    productAndItemEventModel.setProduct(null);
    productAndItemEventModel.getItems().add(new ItemEventModel());
    productAndItemEventModel.setMerchantCode(MERCHANT_CODE_1);
    Item item = new Item();
    item.setStoreId(STORE_ID);
    Mockito.when(objectConverterService.convertToListItem(productAndItemEventModel.getItems(), null, false))
        .thenReturn(Arrays.asList(item));
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class))
        .thenReturn(productAndItemEventModel);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.productAndItemSolrIndexerService)
        .applyProductAndItems(productAndItemsVOArgumentCaptor.capture(), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).convertToListItem(productAndItemEventModel.getItems(), null, false);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
  }

  @Test
  public void onDomainEventConsumedItemListEventBasedAtomicUpdateTest() throws Exception {
    ReflectionTestUtils.setField(solrAddProductToSolrListener, "blackListSellersForL3Reindex",
      "");
    productAndItemEventModel = new ProductAndItemEventModel(PRODUCT_SKU, fieldsAndValues,
      MERCHANT_CODE_1);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class)).thenReturn(productAndItemEventModel);
    this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(productAndItemSolrIndexerService).eventBasedAtomicUpdateToSolr(PRODUCT_SKU,
        MERCHANT_CODE_1, fieldsAndValues);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
  }

  @Test
  public void onDomainEventConsumedItemListEmptyTest() throws Exception {
    productAndItemEventModel.setProduct(null);
    productAndItemEventModel.setItems(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class)).thenReturn(productAndItemEventModel);
    this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
  }

  @Test
  public void onDomainEventConsumedNullTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class)).thenReturn(null);
    this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class))
        .thenThrow(ApplicationRuntimeException.class);
    try {
      this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    } finally {
      Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
    }
  }


  @Test
  public void onDomainEventConsumedProductAndItemRejectionTest() throws Exception {
    productAndItemEventModel.setRejected(true);
    productAndItemEventModel.setProductSku(PRODUCT_SKU);
    ItemEventModel itemEventModel = new ItemEventModel();
    itemEventModel.setItemSku(ITEM_SKU);
    productAndItemEventModel.getItems().add(itemEventModel);

    Mockito.when(objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class)).thenReturn(productAndItemEventModel);
    Mockito.doNothing().when(productAndItemSolrIndexerService)
        .deleteItemsFromSolrAfterPostLiveRejection(Mockito.anyList());
    Mockito.doNothing().when(productAndItemSolrIndexerService)
        .deleteProductsFromSolrAfterPostLiveRejection(Mockito.anySet());
    this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(productAndItemSolrIndexerService)
        .deleteItemsFromSolrAfterPostLiveRejection(Arrays.asList(ITEM_SKU));
    Mockito.verify(productAndItemSolrIndexerService)
        .deleteProductsFromSolrAfterPostLiveRejection(ImmutableSet.of(PRODUCT_SKU));
    Mockito.verify(objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
  }

  @Test
  public void onDomainEventConsumedUpdateToSolrForBlacklistedSellersTest() throws Exception {
    ReflectionTestUtils.setField(solrAddProductToSolrListener, "blackListSellersForL3Reindex",
      MERCHANT_CODE_1.concat(Constants.COMMA).concat(MERCHANT_CODE_2));
    productAndItemEventModel.setProductSku(PRODUCT_SKU);
    ItemEventModel itemEventModel = new ItemEventModel();
    itemEventModel.setItemSku(ITEM_SKU);
    productAndItemEventModel.getItems().add(itemEventModel);

    Mockito.when(objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class)).thenReturn(productAndItemEventModel);
    Mockito.doNothing().when(productAndItemSolrIndexerService)
      .deleteItemsFromSolrAfterPostLiveRejection(Mockito.anyList());
    Mockito.doNothing().when(productAndItemSolrIndexerService)
      .deleteProductsFromSolrAfterPostLiveRejection(Mockito.anySet());
    this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
    Mockito.verify(this.productAndItemSolrIndexerService)
      .applyProductAndItems(productAndItemsVOArgumentCaptor.capture(), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).convertToProduct(productAndItemEventModel.getProduct());
    Mockito.verify(objectConverterService).convertToListItem(productAndItemEventModel.getItems(),
      PRODUCT_SKU, false);
  }

  @Test
  public void onDomainEventConsumedUpdateToSolrForNonBlacklistedSellersTest() throws Exception {
    ReflectionTestUtils.setField(solrAddProductToSolrListener, "blackListSellersForL3Reindex",
      MERCHANT_CODE_1.concat(Constants.COMMA).concat(MERCHANT_CODE_3));
    productAndItemEventModel.setMerchantCode(MERCHANT_CODE_2);
    productAndItemEventModel.setProductSku(PRODUCT_SKU);
    ItemEventModel itemEventModel = new ItemEventModel();
    itemEventModel.setItemSku(ITEM_SKU);
    productAndItemEventModel.getItems().add(itemEventModel);

    Mockito.when(objectMapper.readValue(MESSAGE, ProductAndItemEventModel.class)).thenReturn(productAndItemEventModel);
    Mockito.doNothing().when(productAndItemSolrIndexerService)
      .deleteItemsFromSolrAfterPostLiveRejection(Mockito.anyList());
    Mockito.doNothing().when(productAndItemSolrIndexerService)
      .deleteProductsFromSolrAfterPostLiveRejection(Mockito.anySet());
    this.solrAddProductToSolrListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ProductAndItemEventModel.class);
    Mockito.verify(this.productAndItemSolrIndexerService)
      .applyProductAndItems(productAndItemsVOArgumentCaptor.capture(), Mockito.anyBoolean());
    Mockito.verify(objectConverterService).convertToProduct(productAndItemEventModel.getProduct());
    Mockito.verify(objectConverterService).convertToListItem(productAndItemEventModel.getItems(),
      PRODUCT_SKU, false);
  }

}
