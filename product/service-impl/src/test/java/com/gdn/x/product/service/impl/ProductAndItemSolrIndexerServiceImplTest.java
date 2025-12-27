package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.dao.solr.api.ProductSolrRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ExternalSearchReindexToSolrEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.domain.event.model.ProductEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductL3SolrReindexStatus;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.rest.web.model.FieldValueObject;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.ProductAndItemSolrConstructorService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductL3SolrReindexStatusService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.google.common.collect.ImmutableSet;

public class ProductAndItemSolrIndexerServiceImplTest {

  private static final String COMMIT_WITHIN_VALUE = "1000";
  private static final String PRODUCT_CODE = "product-code";
  private static final String CODE = "code";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String PRISTINE_ID = "pristine-id";
  private static final String STORE_ID = "10001";
  private static final String NEW_BRAND = "new_brand";
  private static final String OLD_BRAND = "old_brand";
  private static final String ITEM_SKU = "item-sku";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final ProductAndItemSolr PRODUCT_AND_ITEM_SOLR = new ProductAndItemSolr();
  private static final double OFFER_PRICE = 1000D;
  private static final double DISCOUNT_PRICE = 800D;
  private static final String PICKUP_POINT_CODE = "ppCode";
  private static final String CHANNEL_1 = "CHANNEL_1";
  private static final String CHANNEL_2 = "CHANNEL_2";
  private static final String CHANNEL_3 = "CHANNEL_3";
  private static final int L5_COUNT = 10;
  private SystemParameter systemParameter;

  @InjectMocks
  private ProductAndItemSolrIndexerServiceImpl productAndItemSolrIndexer;

  @Mock
  private ProductService productService;

  @Mock
  private ProductAndItemSolrRepository productItemSolrRepository;

  @Mock
  private ProductSolrRepository productSolrRepository;

  @Mock
  private CloudSolrClient cloudSolrClient;

  @Mock
  private CloudSolrClient cloudSolrClientL3;

  @Mock
  private ProductAndItemSolrConstructorService productAndItemConstructorService;

  @Mock
  private ProductSolrConstructorServiceImpl productSolrConstructorService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemService itemService;

  @Mock
  private OfflineItemService offlineItemService;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> listArgumentCaptor;

  @Captor
  private ArgumentCaptor<SolrInputDocument> solrInputDocumentArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> solrInputDocumentsArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductAndItemEventModel> productAndItemEventModelArgumentCaptor;

  @Mock
  private ProductL3SolrReindexStatusService productL3SolrReindexStatusService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ObjectConverterService objectConverter;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ObjectConverterService objectConverterService;

  @Captor
  private ArgumentCaptor<ProductAndItemsVO> productAndItemsVoArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  private Item item;
  private PristineDataItem pristineDataItem;
  private Product product;
  private Product productSaved = new Product();
  private ProductAndItemsVO productAndItems;
  private OfflineItem offlineItem;
  private ProductAndItemSolr productAndItemSolr;
  private MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo =
      new MasterDataDetailWithProductAndItemsResponseVo();
  private ProductSolr productSolr = new ProductSolr();
  private Map<String, Double> productAndTotalScoreMap = new HashMap<>();
  private InventoryStockInfoDTO inventoryStockInfoDTO = new InventoryStockInfoDTO();
  private ItemPickupPoint itemPickupPoint;
  private Set<ItemViewConfig> itemViewConfigs;
  private ProductEventModel productEventModel;
  private ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
  private Map<String, Object> fieldAndValuesMap = new HashMap<>();


  @SuppressWarnings("unchecked")
  @Test
  public void applyItemWithFindOneException() throws Exception {
    when(this.productItemSolrRepository.findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU,
      ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenThrow(ApplicationRuntimeException.class);
    try {
      this.productAndItemSolrIndexer.applyItem(this.item);
    } catch (final Exception e) {
      verify(this.productItemSolrRepository)
          .findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    }
  }

  @Test
  public void applyItemsWithFindOneException() throws Exception {
    when(this.productItemSolrRepository.findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU,
      ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenThrow(ApplicationRuntimeException.class);
    try {
      this.productAndItemSolrIndexer.applyItems(Arrays.asList(item));
    } catch (final Exception e) {
      verify(this.productItemSolrRepository)
          .findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU,
            ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    }
  }

  @Test
  public void applyItemsEmptyList() throws Exception {
    this.productAndItemSolrIndexer.applyItems(new ArrayList<>());
  }

  @Test
  public void applyItemWithItemExistsInSolr() throws Exception {
    when(this.productItemSolrRepository.findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU,
      ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenReturn(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    when(this.cloudSolrClient.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.applyItem(this.item);
    verify(this.productItemSolrRepository)
        .findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR, this.item, true);
    verify(this.cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void applyItemsWithItemExistsInSolr() throws Exception {
    when(this.productItemSolrRepository.findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU,
      ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenReturn(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    when(this.cloudSolrClient.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.applyItems(Arrays.asList(this.item));
    verify(this.productItemSolrRepository)
        .findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR, this.item, true);
    verify(this.cloudSolrClient).add(solrInputDocumentsArgumentCaptor.capture());
  }

  @Test
  public void updateInSolrByProductCodeTest() throws Exception {
    when(productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(Arrays.asList(product));
    when(itemService.findItemsByStoreIdAndProductSku(STORE_ID, product.getProductSku())).thenReturn(Arrays.asList(item));
    when(objectConverter.convertToProductEventModel(product)).thenReturn(productEventModel);
    this.productAndItemSolrIndexer.updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(itemService).findItemsByStoreIdAndProductSku(STORE_ID, product.getProductSku());
    verify(objectConverter).convertToProductEventModel(product);
    verify(objectConverter).convertToListItemEventModel(Arrays.asList(item));
    verify(kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR), eq(PRODUCT_SKU),
        productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void updateInSolrByProductCodeAndSkipInventoryTrueTest() throws Exception {
    when(productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(Arrays.asList(product));
    when(itemService.findItemsByStoreIdAndProductSku(STORE_ID, product.getProductSku())).thenReturn(Arrays.asList(item));
    when(objectConverter.convertToProductEventModel(product)).thenReturn(productEventModel);
    this.productAndItemSolrIndexer.updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    verify(itemService).findItemsByStoreIdAndProductSku(STORE_ID, product.getProductSku());
    verify(objectConverter).convertToProductEventModel(product);
    verify(objectConverter).convertToListItemEventModel(Arrays.asList(item));
    verify(kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR), eq(PRODUCT_SKU),
        productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void applyItemWithItemNotExistsInSolr() throws Exception {
    when(this.productItemSolrRepository.findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU,
      ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenReturn(null);
    when(this.productService.getProductDeletedOrUndeleted(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(this.product);
    this.productAndItemSolrIndexer.applyItem(this.item);
    verify(this.productItemSolrRepository)
        .findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productService).getProductDeletedOrUndeleted(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    verify(this.productAndItemConstructorService).constructProduct(new ProductAndItemSolr(),
        this.product, true);
    verify(this.productAndItemConstructorService).constructItem(new ProductAndItemSolr(), this.item,
        true);
    verify(this.cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void applyItemWithItemNotExistsInSolrAndGetProductThrowException() throws Exception {
    when(this.productItemSolrRepository.findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU,
      ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenReturn(null);
    doThrow(ApplicationRuntimeException.class).when(this.productService).getProductDeletedOrUndeleted(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    this.productAndItemSolrIndexer.applyItem(this.item);
    verify(this.productItemSolrRepository)
        .findOne(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productService).getProductDeletedOrUndeleted(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    verify(this.productAndItemConstructorService).constructItem(new ProductAndItemSolr(), this.item,
        true);
    verify(this.cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void applyMasterDataChanges() throws Exception {
    final ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    productDomainEventModel.setProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE);
    final ProductAndItemSolr solrObject = new ProductAndItemSolr();
    final List<ProductAndItemSolr> solrList = Arrays.asList(solrObject);
    Map<String, FieldValueObject> fieldValueObjectMap = new HashMap<>();
    fieldValueObjectMap
        .put(SolrFieldNames.BRAND, FieldValueObject.builder().oldValue(OLD_BRAND).newValue(NEW_BRAND).build());
    when(this.productItemSolrRepository.findByProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE))
        .thenReturn(solrList);
    when(this.productAndItemConstructorService.constructByMasterDataChangeModel(solrObject, productDomainEventModel,
        productAndTotalScoreMap))
        .thenReturn(fieldValueObjectMap);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.productSolrRepository.findByProductCode(PRODUCT_CODE, null)).thenReturn(Arrays.asList(productSolr));
    when(this.productSolrConstructorService.constructProductFromMasterDataChanges(productSolr, productDomainEventModel,
        productAndTotalScoreMap))
        .thenReturn(fieldValueObjectMap);
    when(this.cloudSolrClientL3.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.applyMasterDataChanges(productDomainEventModel, productAndTotalScoreMap);
    verify(this.productItemSolrRepository).findByProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE);
    verify(this.productAndItemConstructorService).constructByMasterDataChangeModel(solrObject, productDomainEventModel,
        productAndTotalScoreMap);
    verify(this.cloudSolrClient).add(Mockito.anyList());
    verify(this.productSolrRepository).findByProductCode(PRODUCT_CODE, null);
    verify(this.productSolrConstructorService)
        .constructProductFromMasterDataChanges(productSolr, productDomainEventModel, productAndTotalScoreMap);
    verify(this.cloudSolrClientL3).add(Mockito.anyList());
  }

  @Test
  public void applyMasterDataChangesWithEmptyResult() throws Exception {
    final ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    productDomainEventModel.setProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE);
    final List<ProductAndItemSolr> solrList = new ArrayList<>();
    when(this.productItemSolrRepository
        .findByProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE))
            .thenReturn(solrList);
    when(this.productSolrRepository.findByProductCode(PRODUCT_CODE, null)).thenReturn(new ArrayList<>());
    this.productAndItemSolrIndexer.applyMasterDataChanges(productDomainEventModel, productAndTotalScoreMap);
    verify(this.productItemSolrRepository)
        .findByProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE);
    verify(this.productSolrRepository).findByProductCode(PRODUCT_CODE, null);
  }

  @Test
  public void applyMasterDataChangesWithEmptyResult1() throws Exception {
    ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    productDomainEventModel.setProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE);
    List<ProductAndItemSolr> solrList = Arrays.asList(new ProductAndItemSolr());
    ProductAndItemSolr solrObject = new ProductAndItemSolr();
    Map<String, FieldValueObject> fieldValueObjectMap = new HashMap<>();
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.productAndItemConstructorService
        .constructByMasterDataChangeModel(solrObject, productDomainEventModel, productAndTotalScoreMap))
        .thenReturn(fieldValueObjectMap);
    when(this.productItemSolrRepository.findByProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE))
        .thenReturn(solrList);
    when(this.productSolrRepository.findByProductCode(PRODUCT_CODE, null)).thenReturn(new ArrayList<>());
    this.productAndItemSolrIndexer.applyMasterDataChanges(productDomainEventModel, productAndTotalScoreMap);
    verify(this.productItemSolrRepository).findByProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE);
    verify(this.cloudSolrClient).add(Mockito.anyList());
    verify(this.productAndItemConstructorService)
        .constructByMasterDataChangeModel(solrObject, productDomainEventModel, productAndTotalScoreMap);
    verify(this.productSolrRepository).findByProductCode(PRODUCT_CODE, null);

  }

  @SuppressWarnings("unchecked")
  @Test
  public void applyMasterDataChangesWithFindException() throws Exception {
    final ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    productDomainEventModel.setProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE);
    when(this.productItemSolrRepository
        .findByProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE))
            .thenThrow(ApplicationRuntimeException.class);
    try {
      this.productAndItemSolrIndexer.applyMasterDataChanges(productDomainEventModel, productAndTotalScoreMap);
    } catch (final Exception e) {
      verify(this.productItemSolrRepository)
          .findByProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE);
    }
  }

  @SuppressWarnings("unchecked")
  @Test
  @Disabled
  public void applyProductAndItemsItemWithException() throws Exception {
    final ProductAndItemSolr existingProductAndItemSolr = new ProductAndItemSolr();
    existingProductAndItemSolr.setId(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU);
    existingProductAndItemSolr.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU,
      ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenThrow(Exception.class);

    try {
      this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    } catch (final Exception e) {
      verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
          ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
          ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    }
  }

  @Test
  public void applyProductAndItemsItemWithItemExistsInSolrPreOrderOff() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", false);
    final ProductAndItemSolr existingProductAndItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    offlineItems.add(offlineItem);
    existingProductAndItemSolr.setId(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU);
    existingProductAndItemSolr.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenReturn(Arrays.asList(existingProductAndItemSolr));
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    verify(this.productAndItemConstructorService).constructItem(existingProductAndItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(existingProductAndItemSolr, this.product, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyProductAndItemsItemWithItemExistsInSolrPreOrderOn() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    final ProductAndItemSolr existingProductAndItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    offlineItems.add(offlineItem);
    existingProductAndItemSolr.setId(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU);
    existingProductAndItemSolr.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenReturn(Arrays.asList(existingProductAndItemSolr));
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    verify(this.productAndItemConstructorService).constructItem(existingProductAndItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(existingProductAndItemSolr, this.product, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyProductAndItemsItemWithItemExistsInSolr() throws Exception {
    final ProductAndItemSolr existingProductAndItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    offlineItems.add(offlineItem);
    existingProductAndItemSolr.setId(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU);
    existingProductAndItemSolr.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
            .thenReturn(Arrays.asList(existingProductAndItemSolr));
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    verify(this.productAndItemConstructorService).constructItem(existingProductAndItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(existingProductAndItemSolr, this.product, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyProductAndItemsItemWithItemExistsFailedToAddInL4SolrTest() throws Exception {
    final ProductAndItemSolr existingProductAndItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    offlineItems.add(offlineItem);
    existingProductAndItemSolr.setId(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU);
    existingProductAndItemSolr.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenReturn(Arrays.asList(existingProductAndItemSolr));
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenThrow(ApplicationRuntimeException.class);
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    verify(this.productAndItemConstructorService).constructItem(existingProductAndItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(existingProductAndItemSolr, this.product, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyProductAndItemsItemWithExceptionOnL3Collection() throws Exception {
    final ProductAndItemSolr existingProductAndItemSolr = new ProductAndItemSolr();
    existingProductAndItemSolr.setId(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU);
    existingProductAndItemSolr.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(Arrays.asList(existingProductAndItemSolr));
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(Arrays.asList(offlineItem));
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenThrow(ApplicationRuntimeException.class);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    try {
      Assertions.assertThrows(Exception.class, () -> this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false));
    } finally {
      verify(this.productAndItemConstructorService).constructItem(existingProductAndItemSolr, this.item, true);
      verify(this.productAndItemConstructorService).constructProduct(existingProductAndItemSolr, this.product, true);
      verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
          ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
          ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
      verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
      verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
      verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
      verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku());
      verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
      verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
      verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
    }
  }


  @Test
  public void applyProductAndItemsItemWithItemNotExistsInSolr() throws Exception {
    final ProductAndItemSolr productItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    offlineItems.add(offlineItem);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(null);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(productItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(productItemSolr, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyProductAndItems_L4_Reindex_Enabled_For_Specific_Sellers() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eligibleSellersForL4Reindex",
        Set.of(MERCHANT_CODE));
    final ProductAndItemSolr productItemSolr = new ProductAndItemSolr();
    productItemSolr.setId(ITEM_SKU);
    productItemSolr.setItemSku(ITEM_SKU);
    productItemSolr.setMerchantCode(MERCHANT_CODE);
    List<OfflineItem> offlineItems = new ArrayList<>();
    offlineItems.add(offlineItem);
    productAndItemSolr.setMerchantCode(MERCHANT_CODE);
    productAndItemSolr.setId(ITEM_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU,
        ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(
        List.of(productAndItemSolr));
    when(this.offlineItemService.findByMerchantCodeAndItemSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
        ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.productService.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService)
        .constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(
        new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU,
        ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(productItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(productItemSolr, this.product,
        true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    verify(this.productSolrConstructorService).constructProduct(productSolr,
        productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(
        productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(),
        Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyProductAndItems_L4_Reindex_Enabled_Not_Eligible_Test() throws Exception {
    productAndItems.getProduct().setMerchantCode("MERCHANT-CODE");
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eligibleSellersForL4Reindex",
        Set.of(MERCHANT_CODE));
    final ProductAndItemSolr productItemSolr = new ProductAndItemSolr();
    productItemSolr.setId(ITEM_SKU);
    productItemSolr.setItemSku(ITEM_SKU);
    productItemSolr.setMerchantCode("MERCHANT_CODE");
    List<OfflineItem> offlineItems = new ArrayList<>();
    offlineItems.add(offlineItem);
    productAndItemSolr.setMerchantCode("MERCHANT_CODE");
    productAndItemSolr.setId(ITEM_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU,
        ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(
        List.of(productAndItemSolr));
    when(this.offlineItemService.findByMerchantCodeAndItemSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
        ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.productService.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService)
        .constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(
        new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    verify(this.productSolrConstructorService).constructProduct(productSolr,
        productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(
        productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(),
        Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyProductAndItemsItemOverideSolrResponse() throws Exception {
    productAndItems.getProduct().setMarkForDelete(false);
    final ProductAndItemSolr productItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    productSaved.setMarkForDelete(true);
    offlineItems.add(offlineItem);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(null);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(productSaved);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProductAndItems(productAndItems, false);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(productItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(productItemSolr, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.MARK_FOR_DELETE), true);
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyProductAndItemsItemOverideSolrResponseProductSolrNotNull() throws Exception {
    productAndItems.getProduct().setMarkForDelete(false);
    productSolr.setProductSku(PRODUCT_SKU);
    final ProductAndItemSolr productItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    productSaved.setMarkForDelete(true);
    offlineItems.add(offlineItem);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(null);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(productSaved);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
       this.productAndItemSolrIndexer.applyProductAndItems(productAndItems, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(productItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(productItemSolr, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.MARK_FOR_DELETE),
        Map.of(SolrConstants.SET_CLAUSE, true));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void applyProductAndItemsItemOverideSolrResponseProductSolrNotNullPreOrderSwitchOff() throws Exception {
    productAndItems.getProduct().setMarkForDelete(false);
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", false);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    productSolr.setProductSku(PRODUCT_SKU);
    final ProductAndItemSolr productItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    productSaved.setMarkForDelete(true);
    offlineItems.add(offlineItem);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(null);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(productSaved);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.applyProductAndItems(productAndItems, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(productItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(productItemSolr, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.MARK_FOR_DELETE),
        Map.of(SolrConstants.SET_CLAUSE, true));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void applyProductAndItemsItemOverideSolrResponseProductSolrNotNullPreOrderNullObj() throws Exception {
    productAndItems.getProduct().setMarkForDelete(false);
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    productSolr.setProductSku(PRODUCT_SKU);
    final ProductAndItemSolr productItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    productSaved.setMarkForDelete(true);
    offlineItems.add(offlineItem);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(null);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(productSaved);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.applyProductAndItems(productAndItems, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(productItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(productItemSolr, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.MARK_FOR_DELETE),
        Map.of(SolrConstants.SET_CLAUSE, true));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void applyProductAndItemsItemOverideSolrResponseProductSolrNotNullPreOrderNullDate() throws Exception {
    productAndItems.getProduct().setMarkForDelete(false);
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    productAndItems.getProduct().setPreOrder(preOrder);
    productAndItems.getProduct().getPreOrder().setPreOrderDate(calendar.getTime());
    productSolr.setProductSku(PRODUCT_SKU);
    final ProductAndItemSolr productItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    productSaved.setMarkForDelete(true);
    offlineItems.add(offlineItem);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(null);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(productSaved);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.applyProductAndItems(productAndItems, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(productItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(productItemSolr, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.MARK_FOR_DELETE),
        Map.of(SolrConstants.SET_CLAUSE, true));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void applyProductAndItemsItemOverideSolrResponseProductSolrNotNullPreOrderPastDate() throws Exception {
    productAndItems.getProduct().setMarkForDelete(false);
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    preOrder.setPreOrderDate(calendar.getTime());
    productAndItems.getProduct().setPreOrder(preOrder);
    productAndItems.getProduct().getPreOrder().setPreOrderDate(calendar.getTime());
    productSolr.setProductSku(PRODUCT_SKU);
    final ProductAndItemSolr productItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    productSaved.setMarkForDelete(true);
    offlineItems.add(offlineItem);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(null);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(productSaved);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.applyProductAndItems(productAndItems, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(productItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(productItemSolr, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.MARK_FOR_DELETE),
        Map.of(SolrConstants.SET_CLAUSE, true));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void applyProductAndItemsItemOverideSolrResponseProductSolrNotNullPreOrderFutureDate() throws Exception {
    productAndItems.getProduct().setMarkForDelete(false);
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    preOrder.setPreOrderDate(calendar.getTime());
    productAndItems.getProduct().setPreOrder(preOrder);
    productAndItems.getProduct().getPreOrder().setPreOrderDate(calendar.getTime());
    productSolr.setProductSku(PRODUCT_SKU);
    final ProductAndItemSolr productItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    productSaved.setMarkForDelete(true);
    offlineItems.add(offlineItem);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(null);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(productSaved);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.applyProductAndItems(productAndItems, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(productItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(productItemSolr, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.MARK_FOR_DELETE),
        Map.of(SolrConstants.SET_CLAUSE, true));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(),
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void applyProductAndItemsItemOverideSolrResponseProductSolrNull() throws Exception {
    productAndItems.getProduct().setMarkForDelete(false);
    final ProductAndItemSolr productItemSolr = new ProductAndItemSolr();
    List<OfflineItem> offlineItems = new ArrayList<>();
    productSaved.setMarkForDelete(true);
    offlineItems.add(offlineItem);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(null);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(offlineItems);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(productSaved);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku())).thenReturn(new ProductSolr());
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProductAndItems(productAndItems, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService).constructItem(productItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(productItemSolr, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.MARK_FOR_DELETE), true);
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyProductSuccess() throws Exception {
    final List<ProductAndItemSolr> productAndItems = new ArrayList<>();
    productAndItems.add(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    when(this.productItemSolrRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(productAndItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>()))
        .thenReturn(Arrays.asList(item));
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(new ProductSolr());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProduct(this.product, false);
    verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService)
        .constructProduct(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
  }

  @Test
  public void applyProductSuccessSwitchOffPreOrder() throws Exception {
    final List<ProductAndItemSolr> productAndItems = new ArrayList<>();
    productAndItems.add(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", false);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    this.product.setPreOrder(preOrder);
    when(this.productItemSolrRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(productAndItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>()))
        .thenReturn(Arrays.asList(item));
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(new ProductSolr());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProduct(this.product, false);
    verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService)
        .constructProduct(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
  }

  @Test
  public void applyProductSuccessPreOrderNullObj() throws Exception {
    final List<ProductAndItemSolr> productAndItems = new ArrayList<>();
    productAndItems.add(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    when(this.productItemSolrRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(productAndItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>()))
        .thenReturn(Arrays.asList(item));
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(new ProductSolr());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProduct(this.product, false);
    verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService)
        .constructProduct(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
  }

  @Test
  public void applyProductSuccessPreOrder() throws Exception {
    final List<ProductAndItemSolr> productAndItems = new ArrayList<>();
    productAndItems.add(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    this.product.setPreOrder(preOrder);
    when(this.productItemSolrRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(productAndItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>()))
        .thenReturn(Arrays.asList(item));
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(new ProductSolr());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProduct(this.product, false);
    verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService)
        .constructProduct(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
  }

  @Test
  public void applyProduct_Success_And_L4_Addition_True() throws Exception {
    final List<ProductAndItemSolr> productAndItems = new ArrayList<>();
    ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR.setMerchantCode(MERCHANT_CODE);
    productAndItems.add(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eligibleSellersForL4Reindex",
        Set.of("MERCHANT_CODE"));
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    this.product.setPreOrder(preOrder);
    this.product.setMerchantCode(MERCHANT_CODE);
    when(this.productItemSolrRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU,
        ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(productAndItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(
        new UpdateResponse());
    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU,
        new ArrayList<>())).thenReturn(Arrays.asList(item));
    doNothing().when(this.productSolrConstructorService)
        .constructProduct(productSolr, product, true);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(),
        product.getProductSku())).thenReturn(new ProductSolr());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProduct(this.product, false);
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU,
        new ArrayList<>());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(),
        product.getProductSku());
  }

  @Test
  public void applyProductSuccessProductSolrNull() throws Exception {
    final List<ProductAndItemSolr> productAndItems = new ArrayList<>();
    productAndItems.add(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    when(this.productItemSolrRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(productAndItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>()))
        .thenReturn(Arrays.asList(item));
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(),
        product.getProductSku())).thenReturn(new ProductSolr());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    this.productAndItemSolrIndexer.applyProduct(this.product, true);
    verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService)
        .constructProduct(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
  }

  @Test
  public void applyProductSuccessProductSolrNotNull() throws Exception {
    final List<ProductAndItemSolr> productAndItems = new ArrayList<>();
    productAndItems.add(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    productSolr.setProductSku(PRODUCT_SKU);
    when(this.productItemSolrRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(productAndItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>()))
        .thenReturn(Arrays.asList(item));
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(productSolr);
    this.productAndItemSolrIndexer.applyProduct(this.product, true);
    verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService)
        .constructProduct(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
  }

  @Test
  public void applyProductSkipInventoryTest() throws Exception {
    final List<ProductAndItemSolr> productAndItems = new ArrayList<>();
    productAndItems.add(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    when(this.productItemSolrRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(productAndItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>()))
        .thenReturn(Arrays.asList(item));
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(new ProductSolr());
    this.productAndItemSolrIndexer.applyProduct(this.product, false);
    verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService)
        .constructProduct(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
  }

  @Test
  public void applyProductOosTest() throws Exception {
    final List<ProductAndItemSolr> productAndItems = new ArrayList<>();
    productAndItems.add(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    when(this.productItemSolrRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU,
            ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(productAndItems);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>()))
        .thenReturn(Arrays.asList(item));
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(new ProductSolr());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    this.productAndItemSolrIndexer.applyProduct(this.product, false);
    verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU,
            ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productAndItemConstructorService)
        .constructProduct(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR, this.product, true);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>());
    verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
  }

  @Test
  public void applyProductWithAnEmptyL4Exception() throws Exception {
    final List<ProductAndItemSolr> productAndItems = new ArrayList<>();
    productAndItems.add(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    when(this.productItemSolrRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
            ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenReturn(new ArrayList<>());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(new ProductSolr());
    this.productAndItemSolrIndexer.applyProduct(this.product, false);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
  }

  @Test
  public void applyProductWithAnException() throws Exception {
    final List<ProductAndItemSolr> productAndItems = new ArrayList<>();
    productAndItems.add(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_AND_ITEM_SOLR);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    when(this.productItemSolrRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID, ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU,
        ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE)).thenThrow(ApplicationRuntimeException.class);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(new ProductSolr());
    this.productAndItemSolrIndexer.applyProduct(this.product, false);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID, ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU,
        ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, new ArrayList<>());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
  }

  @Test
  public void deleteOfflineItemByMerchantCode_valid_success() throws Exception {
    Mockito.when(this.productItemSolrRepository
        .findAllIdsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE))
        .thenReturn(Arrays.asList(productAndItemSolr));
    this.productAndItemSolrIndexer.deleteOfflineItemByMerchantCode(STORE_ID, MERCHANT_CODE);
    Mockito.verify(this.productItemSolrRepository).deleteOfflineItemByItemIds(Mockito.anyList());
    Mockito.verify(this.productItemSolrRepository).findAllIdsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE);
  }

  @Test
  public void deleteOfflineItemByMerchantCode_error_throwException() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.productItemSolrRepository)
        .deleteOfflineItemByItemIds(Mockito.anyList());
    Mockito.when(this.productItemSolrRepository
        .findAllIdsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE))
        .thenReturn(Arrays.asList(productAndItemSolr));
    try {
      this.productAndItemSolrIndexer.deleteOfflineItemByMerchantCode(STORE_ID, null);
    } catch (Exception e) {
      Mockito.verify(this.productItemSolrRepository)
          .deleteOfflineItemByItemIds(Mockito.anyList());
      Mockito.verify(this.productItemSolrRepository).findAllIdsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID, null);
    }
  }

  @BeforeEach
  public void init() throws Exception {
    openMocks(this);
    this.item = new Item();
    this.item.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    this.item.setStoreId(ProductAndItemSolrIndexerServiceImplTest.STORE_ID);
    this.item.setItemSku(ITEM_SKU);
    this.item.setMerchantPromoDiscount(true);
    this.item.setUpdatedDate(new Date());
    this.item.setMerchantCode(MERCHANT_CODE);
    this.product = new Product();
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(CODE);
    Category category = new Category();
    category.setCategoryCode(CODE);
    masterCatalog.setCategory(category);
    product.setMasterCatalog(masterCatalog);
    SalesCatalog salesCatalog = new SalesCatalog();
    salesCatalog.setCatalogCode(CODE);
    salesCatalog.setListOfCategories(Arrays.asList(category));
    product.setSalesCatalogs(Arrays.asList(salesCatalog));
    this.product.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    this.product.setStoreId(ProductAndItemSolrIndexerServiceImplTest.STORE_ID);
    this.item.setItemSku(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU);
    this.productAndItems = new ProductAndItemsVO(this.product, Arrays.asList(this.item));
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_COMMIT_WITHIN))
            .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.SOLR_COMMIT_WITHIN,
                COMMIT_WITHIN_VALUE, SystemParameterNames.SOLR_COMMIT_WITHIN));
    Mockito.when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());

    this.offlineItem = new OfflineItem();
    this.offlineItem.setItemSku(ITEM_SKU);

    this.productAndItemSolr = new ProductAndItemSolr();
    this.productAndItemSolr.setItemSku(ITEM_SKU);

    productAndItems.getProduct().setProductCode(PRODUCT_CODE);
    masterDataDetailWithProductAndItemsResponseVo.setProductAndItems(Collections.singletonList(productAndItems));
    masterDataDetailWithProductAndItemsResponseVo.setMasterDataProducts(new HashedMap());
    masterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts().put(PRODUCT_CODE, new MasterDataProduct());
    masterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts().get(PRODUCT_CODE).setMasterCatalog(masterCatalog);

    productAndTotalScoreMap.put(PRODUCT_SKU, 10.0);
    pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);

    List<Item> items = new ArrayList<>();
    Price price = new Price();
    price.setOfferPrice(1000.0);
    price.setListPrice(1000.0);
    price.setChannel(CHANNEL_1);
    Price price1 = new Price();
    price1.setOfferPrice(100.0);
    price1.setListPrice(100.0);
    price1.setChannel(CHANNEL_2);
    Price price2 = new Price();
    price2.setOfferPrice(1000.0);
    price2.setListPrice(1000.0);
    price2.setChannel(CHANNEL_3);
    Item item2 = new Item();
    Set<Price> prices = Stream.of(price, price1, price2).collect(Collectors.toSet());
    item2.setPrice(prices);
    items.add(item2);

    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(ITEM_SKU)))
        .thenReturn(items);

    inventoryStockInfoDTO.setWebProductSku(PRODUCT_SKU);
    inventoryStockInfoDTO.setWebTotalAvailableStock(0);

    this.itemViewConfigs = new HashSet<>();
    this.itemViewConfigs.add(new ItemViewConfig());
    itemPickupPoint = ItemPickupPoint.builder().itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).price(prices).itemViewConfig(itemViewConfigs).build();
   itemPickupPoint.setStoreId(STORE_ID);
    Mockito.when(itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(Arrays.asList(itemPickupPoint));

    productEventModel = new ProductEventModel();
    productAndItemEventModel.setProductSku(PRODUCT_SKU);

    productAndItems.getProduct().setMerchantCode(MERCHANT_CODE);
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eligibleSellersForL4Reindex",
        new HashSet<>());
  }

  @Test
  public void applyMasterDataDetailWithProductAndItemsTest() throws Exception {
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    Mockito.when(this.productSolrRepository.findByProductSku(product.getMerchantCode(), PRODUCT_SKU)).thenReturn(productSolr);
    Mockito.doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(false);
    productAndItemSolrIndexer.applyMasterDataDetailWithProductAndItems(masterDataDetailWithProductAndItemsResponseVo, false);
    Mockito.verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSku(STORE_ID, productAndItems.getProduct().getProductSku(),
          MERCHANT_CODE);
    Mockito.verify(this.productAndItemConstructorService)
        .constructItem(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productAndItemConstructorService)
        .constructProduct(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.cloudSolrClient).add(Mockito.anyCollection());
    Mockito.verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    Mockito.verify(this.productSolrRepository).findByProductSku(product.getMerchantCode(), PRODUCT_SKU);
    Assertions.assertFalse((boolean) solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.IS_IN_STOCK));
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyMasterDataDetailWithProductAndItems_inStockTest() throws Exception {
    inventoryStockInfoDTO.setWebTotalAvailableStock(1);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    Mockito.when(this.productSolrRepository.findByProductSku(product.getMerchantCode(), PRODUCT_SKU)).thenReturn(productSolr);
    Mockito.doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    productAndItemSolrIndexer.applyMasterDataDetailWithProductAndItems(masterDataDetailWithProductAndItemsResponseVo, false);
    Mockito.verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSku(STORE_ID, productAndItems.getProduct().getProductSku(),
          MERCHANT_CODE);
    Mockito.verify(this.productAndItemConstructorService)
        .constructItem(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productAndItemConstructorService)
        .constructProduct(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.cloudSolrClient).add(Mockito.anyCollection());
    Mockito.verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    Mockito.verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    Mockito.verify(this.productSolrRepository).findByProductSku(product.getMerchantCode(), PRODUCT_SKU);
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    assertTrue((boolean)solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.IS_IN_STOCK));
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyMasterDataDetailWithProductAndItemsWithSyncTrueTest() throws Exception {
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    Mockito.when(this.productSolrRepository.findByProductSku(product.getMerchantCode(), PRODUCT_SKU)).thenReturn(productSolr);
    Mockito.doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    masterDataDetailWithProductAndItemsResponseVo.getProductAndItems().get(0).getProduct().setSynchronized(true);
    masterDataDetailWithProductAndItemsResponseVo.getProductAndItems().get(0).getItems().get(0).setSynchronized(true);
    masterDataDetailWithProductAndItemsResponseVo.getProductAndItems().get(0).getItems().get(0).setItemCode(ITEM_SKU);
    productAndItemSolrIndexer.applyMasterDataDetailWithProductAndItems(masterDataDetailWithProductAndItemsResponseVo, false);
    Mockito.verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSku(STORE_ID, productAndItems.getProduct().getProductSku(),
          MERCHANT_CODE);
    Mockito.verify(this.productAndItemConstructorService)
        .constructItem(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productAndItemConstructorService)
        .constructProduct(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.cloudSolrClient).add(Mockito.anyCollection());
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    Mockito.verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    Mockito.verify(this.productSolrRepository).findByProductSku(product.getMerchantCode(), PRODUCT_SKU);
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyMasterDataDetailWithProductAndItemsWithMasterCatalogNotNullTest() throws Exception {
    Mockito.when(this.productSolrRepository.findByProductSku(product.getMerchantCode(), PRODUCT_SKU)).thenReturn(productSolr);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    Mockito.doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    masterDataDetailWithProductAndItemsResponseVo.getProductAndItems().get(0).getProduct().setMasterCatalog(null);
    masterDataDetailWithProductAndItemsResponseVo.getProductAndItems().get(0).getItems().get(0).setSynchronized(true);
    masterDataDetailWithProductAndItemsResponseVo.getProductAndItems().get(0).getItems().get(0).setItemCode(ITEM_SKU);
    productAndItemSolrIndexer.applyMasterDataDetailWithProductAndItems(masterDataDetailWithProductAndItemsResponseVo, false);
    Mockito.verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSku(STORE_ID, productAndItems.getProduct().getProductSku(),
          MERCHANT_CODE);
    Mockito.verify(this.productAndItemConstructorService)
        .constructItem(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productAndItemConstructorService)
        .constructProduct(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.cloudSolrClient).add(Mockito.anyCollection());
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    Mockito.verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    Mockito.verify(this.productSolrRepository).findByProductSku(product.getMerchantCode(), PRODUCT_SKU);
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyMasterDataDetailWithProductAndItemsTest_updateL3Exception() throws Exception {
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    Mockito.when(this.productSolrRepository.findByProductSku(product.getMerchantCode(), PRODUCT_SKU)).thenReturn(productSolr);
    Mockito.doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    Mockito.when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenThrow(ApplicationRuntimeException.class);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    productAndItemSolrIndexer.applyMasterDataDetailWithProductAndItems(masterDataDetailWithProductAndItemsResponseVo, false);
    Mockito.verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSku(STORE_ID, productAndItems.getProduct().getProductSku(),
          MERCHANT_CODE);
    Mockito.verify(this.productAndItemConstructorService)
        .constructItem(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productAndItemConstructorService)
        .constructProduct(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.cloudSolrClient).add(Mockito.anyCollection());
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    Mockito.verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    Mockito.verify(this.productSolrRepository).findByProductSku(product.getMerchantCode(), PRODUCT_SKU);
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyMasterDataDetailWithProductAndItemsWithItemsNullTest() throws Exception {
    Mockito.when(this.productSolrRepository.findByProductSku(product.getMerchantCode(), PRODUCT_SKU))
        .thenReturn(productSolr);
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    Mockito.when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(ITEM_SKU)))
        .thenReturn(new ArrayList<>());
    Mockito.doNothing().when(this.productSolrConstructorService)
        .constructProduct(productSolr, productAndItems.getProduct(), true);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    masterDataDetailWithProductAndItemsResponseVo.getProductAndItems().get(0).setItems(null);
    productAndItemSolrIndexer.applyMasterDataDetailWithProductAndItems(masterDataDetailWithProductAndItemsResponseVo, false);
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    Mockito.verify(this.productSolrConstructorService)
        .constructProduct(productSolr, productAndItems.getProduct(), true);
    Mockito.verify(this.productSolrRepository).findByProductSku(product.getMerchantCode(), PRODUCT_SKU);
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyMasterDataDetailWithProductAndItemsExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.cloudSolrClient).add(Mockito.anyList());
    Mockito.when(this.productSolrRepository.findByProductSku(null, PRODUCT_SKU))
        .thenReturn(productSolr);
    Mockito.doNothing().when(this.productSolrConstructorService)
        .constructProduct(productSolr, productAndItems.getProduct(), true);
    Mockito.when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenThrow(ApplicationRuntimeException.class);
    try {
      Assertions.assertThrows(Exception.class, () -> productAndItemSolrIndexer.applyMasterDataDetailWithProductAndItems(masterDataDetailWithProductAndItemsResponseVo, false));
    } finally {
      Mockito.verify(this.productItemSolrRepository)
          .findByStoreIdAndProductSku(STORE_ID, productAndItems.getProduct().getProductSku(),
            MERCHANT_CODE);
      Mockito.verify(this.productAndItemConstructorService)
          .constructItem(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
      Mockito.verify(this.productAndItemConstructorService)
          .constructProduct(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
      Mockito.verify(this.cloudSolrClient).add(Mockito.anyCollection());
    }
  }

  @Test
  public void updateItemsInSolrTest() throws Exception {
    Map<String, Boolean> itemsMap = new HashMap<String, Boolean>();
    itemsMap.put(ITEM_SKU, true);
    productSolr.setProductSku(PRODUCT_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU,
      StringUtils.EMPTY))
        .thenReturn(Arrays.asList(productAndItemSolr));
    when(this.productSolrRepository.findByProductSku(null, PRODUCT_SKU)).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.updateItemsInSolr(STORE_ID, PRODUCT_SKU, true, itemsMap, true, new ArrayList<>(),
      StringUtils.EMPTY);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, StringUtils.EMPTY);
    verify(this.productSolrRepository).findByProductSku(null, PRODUCT_SKU);
    verify(this.cloudSolrClient).add(Mockito.any((SolrInputDocument.class)));
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateItemsInSolrAddDeleteTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "addDeleteVariantSwitch", true);
    Map<String, Boolean> itemsMap = new HashMap<String, Boolean>();
    itemsMap.put(ITEM_SKU, true);
    productSolr.setProductSku(PRODUCT_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU,
      StringUtils.EMPTY))
        .thenReturn(Arrays.asList(productAndItemSolr));
    when(this.productSolrRepository.findByProductSku(null, PRODUCT_SKU)).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.updateItemsInSolr(STORE_ID, PRODUCT_SKU, true, itemsMap, true, new ArrayList<>(),
      StringUtils.EMPTY);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, StringUtils.EMPTY);
    verify(this.productSolrRepository).findByProductSku(null, PRODUCT_SKU);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateItemsInSolrAddDeleteSuspendedFalseTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "addDeleteVariantSwitch", true);
    Map<String, Boolean> itemsMap = new HashMap<String, Boolean>();
    itemsMap.put(ITEM_SKU, true);
    productSolr.setProductSku(PRODUCT_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU,
      StringUtils.EMPTY))
        .thenReturn(Arrays.asList(productAndItemSolr));
    when(this.productSolrRepository.findByProductSku(null, PRODUCT_SKU)).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.updateItemsInSolr(STORE_ID, PRODUCT_SKU, false, itemsMap, true, new ArrayList<>(),
      StringUtils.EMPTY);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, StringUtils.EMPTY);
    verify(this.productSolrRepository).findByProductSku(null, PRODUCT_SKU);
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateItemsInSolrAddDeleteSuspendedFalseItemPresentTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "addDeleteVariantSwitch", true);
    Map<String, Boolean> itemsMap = new HashMap<String, Boolean>();
    itemsMap.put(ITEM_SKU, true);
    productSolr.setProductSku(PRODUCT_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU,
      StringUtils.EMPTY))
        .thenReturn(Arrays.asList(productAndItemSolr));
    when(this.productSolrRepository.findByProductSku(null, PRODUCT_SKU)).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.updateItemsInSolr(STORE_ID, PRODUCT_SKU, false, itemsMap, true,
        Collections.singletonList(item), StringUtils.EMPTY);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, StringUtils.EMPTY);
    verify(this.productSolrRepository).findByProductSku(null, PRODUCT_SKU);
    verify(this.cloudSolrClient).add(Mockito.any(SolrInputDocument.class));
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateItemsInSolrViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    Map<String, Boolean> itemsMap = new HashMap<String, Boolean>();
    itemsMap.put(ITEM_SKU, true);
    productSolr.setProductSku(PRODUCT_SKU);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU,
      StringUtils.EMPTY))
        .thenReturn(Arrays.asList(productAndItemSolr));
    when(this.productSolrRepository.findByProductSku(null, PRODUCT_SKU)).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.updateItemsInSolr(STORE_ID, PRODUCT_SKU, true, itemsMap, true, new ArrayList<>(),
      StringUtils.EMPTY);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, StringUtils.EMPTY);
    verify(this.cloudSolrClient).add(Mockito.any((SolrInputDocument.class)));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR), eq(PRODUCT_SKU),
      productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void updateItemsInSolrTestExceptionTest() throws Exception {
    Map<String, Boolean> itemsMap = new HashMap<String, Boolean>();
    itemsMap.put(ITEM_SKU, true);
    productSolr.setProductSku(PRODUCT_SKU);
    Mockito.doThrow(SolrServerException.class).when(cloudSolrClient).add(Mockito.any((SolrInputDocument.class)));
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU,
      StringUtils.EMPTY))
        .thenReturn(Arrays.asList(productAndItemSolr));
    try {
      this.productAndItemSolrIndexer.updateItemsInSolr(STORE_ID, PRODUCT_SKU, true, itemsMap, true, new ArrayList<>(),
        StringUtils.EMPTY);
    } catch (Exception e) {
      verify(this.productItemSolrRepository)
          .findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
              ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, StringUtils.EMPTY);
      Mockito.verify(this.cloudSolrClient).add(Mockito.any((SolrInputDocument.class)));
    }
  }

  @Test
  public void updateItemsInSolr_updateProductSolrExceptionTest() throws Exception {
    Map<String, Boolean> itemsMap = new HashMap<String, Boolean>();
    itemsMap.put(ITEM_SKU, true);
    productSolr.setProductSku(PRODUCT_SKU);
    when(cloudSolrClient.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    when(this.productSolrRepository.findByProductSku(null, PRODUCT_SKU)).thenReturn(productSolr);
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU,
      StringUtils.EMPTY))
        .thenReturn(Arrays.asList(productAndItemSolr));
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenThrow(SolrServerException.class);
    try {
      this.productAndItemSolrIndexer.updateItemsInSolr(STORE_ID, PRODUCT_SKU, true, itemsMap, true, new ArrayList<>(),
        StringUtils.EMPTY);
    } catch (Exception e) {
      verify(this.productSolrRepository).findByProductSku(null, PRODUCT_SKU);
      verify(this.productItemSolrRepository)
          .findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
              ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, StringUtils.EMPTY);
      verify(this.cloudSolrClient).add(Mockito.any(SolrInputDocument.class));
      verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    }
  }

  @Test
  public void indexMerchantPromoDiscountItem_whenSkuStateChangeTrueTest() throws Exception {
    this.productAndItemSolrIndexer.indexMerchantPromoDiscountItem(item, true);
    verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.ID));
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.MERCHANT_PROMO_DISCOUNT));
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.UPDATED_DATE));
  }

  @Test
  public void indexMerchantPromoDiscountItemPickupPoint() throws Exception {
    this.productAndItemSolrIndexer.indexMerchantPromoDiscountItemPickupPoint(itemPickupPoint, true);
    Mockito.verify(productItemSolrRepository)
        .executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED));
  }

  @Test
  public void indexMerchantPromoDiscountItemPickupPointActivatedFalse() throws Exception {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(1))));
    itemPickupPoint.getPrice().stream().findFirst().get().setMerchantPromoDiscountPrice(discountPrice);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(itemPickupPoint));
    this.productAndItemSolrIndexer.indexMerchantPromoDiscountItemPickupPoint(itemPickupPoint, false);
    Mockito.verify(productItemSolrRepository)
        .executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED));
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void indexMerchantPromoDiscountItemPickupPointActivatedNullTest() throws Exception {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(1))));
    itemPickupPoint.getPrice().stream().findFirst().get().setMerchantPromoDiscountPrice(discountPrice);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new ArrayList<>());
    this.productAndItemSolrIndexer.indexMerchantPromoDiscountItemPickupPoint(itemPickupPoint, false);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void indexMerchantPromoDiscountItem_whenSkuStateChangeFalseAndActivationTest() throws Exception {
    item.setMerchantPromoDiscountActive(true);
    Price price = new Price();
    price.setOfferPrice(OFFER_PRICE);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(DISCOUNT_PRICE);
    price.setMerchantPromoDiscountPrice(discountPrice);
    item.setPrice(Collections.singleton(price));
    this.productAndItemSolrIndexer.indexMerchantPromoDiscountItem(item, false);
    verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.ID));
    assertTrue(
        solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED));
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.OFFER_PRICE));
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.UPDATED_DATE));
  }

  @Test
  public void indexMerchantPromoDiscountItem_whenSkuStateChangeFalseAndDeactivationTest() throws Exception {
    item.setMerchantPromoDiscountActive(false);
    Price price = new Price();
    price.setOfferPrice(OFFER_PRICE);
    price.setMerchantPromoDiscountPrice(null);
    item.setPrice(Collections.singleton(price));
    this.productAndItemSolrIndexer.indexMerchantPromoDiscountItem(item, false);
    verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.ID));
    assertTrue(
        solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED));
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.OFFER_PRICE));
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.UPDATED_DATE));
  }

  @Test
  public void indexMerchantPromoDiscountItem_whenExceptionTest() throws Exception {
    doThrow(SolrServerException.class).when(productItemSolrRepository)
        .executeSolrDocumentAtomicUpdate(any(SolrInputDocument.class));
    try {
      Assertions.assertThrows(SolrServerException.class, () -> this.productAndItemSolrIndexer.indexMerchantPromoDiscountItem(item, true));
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
      assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.ID));
      assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.MERCHANT_PROMO_DISCOUNT));
      assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.UPDATED_DATE));
    }
  }

  @Test
  public void updateItemSyncStatusForFulfillmentByBlibliTest() throws Exception {
    this.productAndItemSolrIndexer.updateItemSyncStatusForFulfillmentByBlibli(STORE_ID, ITEM_SKU, MERCHANT_CODE);

    verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.ID));
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.STORE_ID));
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.LINKED_PARTNERS));

    Assertions.assertEquals(ITEM_SKU, solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.ID).getValue());
    Assertions.assertEquals(STORE_ID, solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.STORE_ID).getValue());
    Assertions.assertEquals(Collections.singletonMap("add", MERCHANT_CODE),
      solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.LINKED_PARTNERS).getFirstValue());
  }

  @Test
  public void updateItemSyncStatusForFulfillmentByBlibliTest_whenUpdateFailed() throws Exception {
    doThrow(SolrServerException.class).when(productItemSolrRepository)
      .executeSolrDocumentAtomicUpdate(any(SolrInputDocument.class));

    try {
      this.productAndItemSolrIndexer.updateItemSyncStatusForFulfillmentByBlibli(STORE_ID, ITEM_SKU, MERCHANT_CODE);
    } catch(SolrServerException e) {
      verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
      assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.ID));
      assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.STORE_ID));
      assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.LINKED_PARTNERS));

      Assertions.assertEquals(ITEM_SKU, solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.ID).getValue());
      Assertions.assertEquals(STORE_ID, solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.STORE_ID).getValue());
      Assertions.assertEquals(Collections.singletonMap("add", MERCHANT_CODE),
        solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.LINKED_PARTNERS).getFirstValue());
    }
  }

  @Test
  public void updateItemSyncStatusForFulfillmentByBlibliTest_withMissingStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productAndItemSolrIndexer.updateItemSyncStatusForFulfillmentByBlibli(null, ITEM_SKU, MERCHANT_CODE));
  }

  @Test
  public void updateItemSyncStatusForFulfillmentByBlibliTest_withMissingItemSku() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productAndItemSolrIndexer.updateItemSyncStatusForFulfillmentByBlibli(STORE_ID, null, MERCHANT_CODE));
  }

  @Test
  public void updateItemSyncStatusForFulfillmentByBlibliTest_withMissingLinkedPartnerCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productAndItemSolrIndexer.updateItemSyncStatusForFulfillmentByBlibli(STORE_ID, ITEM_SKU, null));
  }

  @Test
  public void updateSolrOnArchivalAction() throws Exception {
    this.productAndItemSolrIndexer.updateSolrOnToggleArchiveItemAction(item);
    verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void deleteItemsFromSolrAfterPostLiveRejectionTest() throws IOException, SolrServerException {
    productAndItemSolrIndexer.deleteItemsFromSolrAfterPostLiveRejection(Collections.singletonList(ITEM_SKU));
    verify(cloudSolrClient).deleteById(Collections.singletonList(ITEM_SKU));
  }

  @Test
  public void deleteItemsFromSolrAfterPostLiveRejectionEmptyListTest() {
    productAndItemSolrIndexer.deleteItemsFromSolrAfterPostLiveRejection(Collections.EMPTY_LIST);
  }

  @Test
  public void updateSolrOnArchivalActionExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.productItemSolrRepository)
        .executeSolrDocumentAtomicUpdate(Mockito.any(SolrInputDocument.class));
    try {
      this.productAndItemSolrIndexer.updateSolrOnToggleArchiveItemAction(item);
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    }
  }


  @Test
  public void updateSolrOnSyncUnsyncAction() throws Exception {
    this.productAndItemSolrIndexer.updateSolrOnSyncUnsyncAction(productAndItems);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    verify(cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateSolrOnSyncUnsyncActionViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    this.productAndItemSolrIndexer.updateSolrOnSyncUnsyncAction(productAndItems);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    verify(this.kafkaProducer)
      .send(eq(ProductDomainEventName.UPDATE_TO_SOLR), eq(PRODUCT_SKU),
        productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void updateSolrOnSyncUnsyncActionExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    try {
      this.productAndItemSolrIndexer.updateSolrOnSyncUnsyncAction(productAndItems);
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    }
  }

  @Test
  public void updateSolrOnPriceChange() throws Exception {
    this.productAndItemSolrIndexer.updateSolrOnPriceChange(Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
  }

  @Test
  public void updateSolrOnPriceChangeExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    try {
      this.productAndItemSolrIndexer.updateSolrOnPriceChange(Arrays.asList(item));
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    }
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChange() throws Exception {
    this.productAndItemSolrIndexer.updateSolrOnPromoBundlingFlagChange(Arrays.asList(item), false);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusTrueViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, true,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap);
    verify(this.kafkaProducer)
      .send(eq(ProductDomainEventName.UPDATE_TO_SOLR), eq(PRODUCT_SKU),
        productAndItemEventModelArgumentCaptor.capture());
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 2);
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getProductSku(), PRODUCT_SKU);
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getFieldsAndValues().size(), 1);
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusFalseViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    itemPickupPoint.setItemSku(ITEM_SKU);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(this.kafkaProducer)
      .send(eq(ProductDomainEventName.UPDATE_TO_SOLR), eq(PRODUCT_SKU),
       productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getFieldsAndValues().size(), 1);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusFalseEmptyViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(new ArrayList<>());
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusTrueTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, true,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 2);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
  }

  @Test
  public void updateSolrOnNoFieldNameFlagChangeByItemSkusTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, true,
        null, productSkuAndMerchantCodeMap);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
  }

  @Test
  public void updateSolrOnMPDFlagChangeByItemSkusTrueTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, true,
        SolrFieldNames.MERCHANT_PROMO_DISCOUNT, productSkuAndMerchantCodeMap);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 2);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusFalseTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusMFDTrueFalseTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setMarkForDelete(true);
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusMapNullTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusL5TrueFalseTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPromoBundling(true);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeFalseAndNullTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPromoBundling(true);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        null, productSkuAndMerchantCodeMap);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnMPDFlagChangeByItemSkusFalseTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.MERCHANT_PROMO_DISCOUNT, productSkuAndMerchantCodeMap);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnMPDFlagChangeByItemSkusMFDTrueFalseTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setMarkForDelete(true);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.MERCHANT_PROMO_DISCOUNT, productSkuAndMerchantCodeMap);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnMPDFlagChangeByItemSkusFalseFieldNameDiffTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setMarkForDelete(true);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.PRODUCT_NAME, productSkuAndMerchantCodeMap);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnMPDFlagChangeByItemSkusL5TrueTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setMerchantPromoDiscount(true);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.MERCHANT_PROMO_DISCOUNT, productSkuAndMerchantCodeMap);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(productSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    Assertions.assertEquals(solrInputDocumentsArgumentCaptor.getValue().size(), 1);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusFalseEmptyTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenReturn(new ArrayList<>());
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusExceptionEmptyTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    itemPickupPoint.setItemSku(ITEM_SKU);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList())).thenThrow(ApplicationRuntimeException.class);
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, false,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap);
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
        Mockito.anyList());
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusEmptyTrueTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Collections.EMPTY_LIST);
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(productSkusMap, true,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap);
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeByItemSkusEmptyMapTest() throws Exception {
    Map<String, List<String>> productSkusMap = new HashMap<>();
    productSkusMap.put(PRODUCT_SKU, Arrays.asList(ITEM_SKU, PRODUCT_SKU));
    Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();
    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
    Assertions.assertThrows(Exception.class, () -> this.productAndItemSolrIndexer.updateSolrOnPromoFlagChangeByItemSkus(new HashMap<>(), true,
        SolrFieldNames.PROMO_BUNDLING, productSkuAndMerchantCodeMap));
  }

  @Test
  public void updateSolrOnPromoBundlingFlagChangeExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    try {
      this.productAndItemSolrIndexer.updateSolrOnPromoBundlingFlagChange(Arrays.asList(item), false);
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    }
  }

  @Test
  public void updateSolrOnMasterCatalogChanges() throws Exception {
    this.productAndItemSolrIndexer.updateSolrOnMasterCatalogChanges(product, Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateSolrOnMasterCatalogChangesViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    this.productAndItemSolrIndexer.updateSolrOnMasterCatalogChanges(product, Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void updateSolrOnMasterCatalogNullTest() throws Exception {
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setMasterCatalog(product.getMasterCatalog());
    product.setMasterCatalog(null);
    product.setMasterDataProduct(masterDataProduct);
    this.productAndItemSolrIndexer.updateSolrOnMasterCatalogChanges(product, Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateSolrOnMasterCatalogChangesExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    try {
      this.productAndItemSolrIndexer.updateSolrOnMasterCatalogChanges(product, Arrays.asList(item));
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    }
  }

  @Test
  public void takeDownL3Test() throws Exception {
    ProductSolr productSolr = new ProductSolr();
    productSolr.setProductSku(PRODUCT_SKU);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(productSolr);
    this.productAndItemSolrIndexer.takeDownL3(product);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
    verify(cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void takeDownL3SolrViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    ProductSolr productSolr = new ProductSolr();
    productSolr.setProductSku(PRODUCT_SKU);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(productSolr);
    this.productAndItemSolrIndexer.takeDownL3(product);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void takeDownL3ProductSolrEmptyTest() throws Exception {
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(new ProductSolr());
    this.productAndItemSolrIndexer.takeDownL3(product);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
    verify(productL3SolrReindexStatusService)
        .insertProductSkuToReindexStatusCollection(Mockito.any(ProductL3SolrReindexStatus.class));
  }

  @Test
  public void takeDownL3ProductSolrNullest() throws Exception {
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku())).thenReturn(null);
    this.productAndItemSolrIndexer.takeDownL3(product);
    verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
    verify(productL3SolrReindexStatusService)
        .insertProductSkuToReindexStatusCollection(Mockito.any(ProductL3SolrReindexStatus.class));
  }

  @Test
  public void takeDownL3ExceptionTest() throws Exception {
    productSolr.setProductSku(PRODUCT_SKU);
    when(productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku()))
        .thenReturn(productSolr);
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClientL3)
        .add(Mockito.anyList());
    try {
      this.productAndItemSolrIndexer.takeDownL3(product);
    } finally {
      verify(productSolrRepository).findByProductSku(product.getMerchantCode(), product.getProductSku());
      verify(cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    }
  }

  @Test
  public void updateSolrOnSalesCatalogChanges() throws Exception {
    this.productAndItemSolrIndexer.updateSolrOnSalesCatalogChanges(product, Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateSolrOnSalesCatalogChangesViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    this.productAndItemSolrIndexer.updateSolrOnSalesCatalogChanges(product, Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void updateSolrOnSalesCatalogChangesExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    try {
      this.productAndItemSolrIndexer.updateSolrOnSalesCatalogChanges(product, Arrays.asList(item));
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    }
  }

  @Test
  public void updateSolrOnSalesCatalogChangesForProductListTest() throws Exception {
    this.productAndItemSolrIndexer
        .updateSolrOnSalesCatalogChangesForProductList(Arrays.asList(product), Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(solrInputDocumentsArgumentCaptor.capture());
  }

  @Test
  public void updateSolrOnSalesCatalogChangesForProductListViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    this.productAndItemSolrIndexer
        .updateSolrOnSalesCatalogChangesForProductList(Arrays.asList(product), Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void updateSolrOnSalesCatalogChangesForProductListExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    try {
      this.productAndItemSolrIndexer
          .updateSolrOnSalesCatalogChangesForProductList(Arrays.asList(product), Arrays.asList(item));
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    }
  }

  @Test
  public void updateSolrOnItemViewConfigChanges() throws Exception {
    this.productAndItemSolrIndexer.updateSolrOnItemViewConfigChanges(Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
  }

  @Test
  public void updateSolrOnItemViewConfigChangesExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    try {
      this.productAndItemSolrIndexer.updateSolrOnItemViewConfigChanges(Arrays.asList(item));
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    }
  }

  @Test
  public void updateSolrOnContentChange() throws Exception {
    item.setPristineDataItem(null);
    this.productAndItemSolrIndexer.updateSolrOnContentChange(Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
  }

  @Test
  public void updateSolrOnContentNonNullPIChange() throws Exception {
    item.setPristineDataItem(new PristineDataItem());
    this.productAndItemSolrIndexer.updateSolrOnContentChange(Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
  }

  @Test
  public void updateSolrOnContentChangeExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    try {
      this.productAndItemSolrIndexer.updateSolrOnContentChange(Arrays.asList(item));
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    }
  }

  @Test
  public void deleteProductsFromSolrAfterPostLiveRejectionTest() throws Exception {
    Mockito.doNothing().when(this.productSolrRepository)
        .deleteSolrDocumentsByListOfProductSku(Collections.singleton(PRODUCT_SKU));
    this.productAndItemSolrIndexer.deleteProductsFromSolrAfterPostLiveRejection(Collections.singleton(PRODUCT_SKU));
    Mockito.verify(this.productSolrRepository).deleteSolrDocumentsByListOfProductSku(Collections.singleton(PRODUCT_SKU));
  }

  @Test
  public void deleteProductsFromSolrAfterPostLiveRejection_ExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productSolrRepository)
        .deleteSolrDocumentsByListOfProductSku(Collections.singleton(PRODUCT_SKU));
    this.productAndItemSolrIndexer.deleteProductsFromSolrAfterPostLiveRejection(Collections.singleton(PRODUCT_SKU));
    Mockito.verify(this.productSolrRepository).deleteSolrDocumentsByListOfProductSku(Collections.singleton(PRODUCT_SKU));
  }

  @Test
  public void applyProductToL3CollectionSuccess() throws Exception {
    ProductSolr productSolr =
        ProductSolr.builder().productCode(PRODUCT_CODE).productSku(PRODUCT_SKU).markForDelete(false).build();
    when(this.productSolrRepository.findByProductSku(product.getMerchantCode(), PRODUCT_SKU)).thenReturn(productSolr);
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.reindexProductToL3Collection(this.product, Arrays.asList(item), true);
    verify(this.productSolrConstructorService).constructProduct(productSolr, product, true);
    verify(this.productSolrRepository).findByProductSku(product.getMerchantCode(), PRODUCT_SKU);
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Assertions.assertEquals(PRODUCT_SKU,
        solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.PRODUCT_SKU).getValue());
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.PRODUCT_CODE).getValue());
  }

  @Test
  public void applyProductToL3CollectionWithAnException() {
    when(this.productSolrRepository.findByProductSku(product.getMerchantCode(), PRODUCT_SKU)).thenThrow(ApplicationRuntimeException.class);
    this.productAndItemSolrIndexer.reindexProductToL3Collection(this.product, Arrays.asList(item),
        true);
    verify(this.productSolrRepository).findByProductSku(product.getMerchantCode(), PRODUCT_SKU);
  }

  @Test
  public void updateWholesalePriceActivatedFlag() throws Exception {
    this.productAndItemSolrIndexer.updateWholesalePriceActivatedFlag(ITEM_SKU, true);
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void updateWholesalePriceActivatedFlagSolrException() throws Exception {
    Mockito.doThrow(SolrException.class).when(cloudSolrClient)
        .add(Mockito.any(SolrInputDocument.class));
    this.productAndItemSolrIndexer.updateWholesalePriceActivatedFlag(ITEM_SKU, true);
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void updateWholesalePriceActivatedFlagSolrServerException() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(cloudSolrClient)
        .add(Mockito.any(SolrInputDocument.class));
    this.productAndItemSolrIndexer.updateWholesalePriceActivatedFlag(ITEM_SKU, true);
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void updateWholesalePriceActivatedFlagIOException() throws Exception {
    Mockito.doThrow(IOException.class).when(cloudSolrClient)
        .add(Mockito.any(SolrInputDocument.class));
    this.productAndItemSolrIndexer.updateWholesalePriceActivatedFlag(ITEM_SKU, true);
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void updateSolrOnPriceAndWholesalePriceFlagChange() throws Exception {
    productSolr.setVariantCount(1);
    List<Item> itemList = new ArrayList();
    itemList.add(item);
    Mockito.when(productHelperService.getCachedItemsByProductSkuWithoutOverridingL5Data(STORE_ID, PRODUCT_SKU, Arrays.asList(item.getItemSku())))
        .thenReturn(itemList);
    this.productAndItemSolrIndexer.updateSolrOnPriceAndWholesalePriceFlagChange(item, true);
    verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    verify(this.productHelperService).getCachedItemsByProductSkuWithoutOverridingL5Data(STORE_ID, PRODUCT_SKU, Arrays.asList(item.getItemSku()));
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(this.objectConverter).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void updateSolrOnPriceAndWholesalePriceFlagChangeViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    productSolr.setVariantCount(1);
    List<Item> itemList = new ArrayList();
    itemList.add(item);
    Mockito.when(productHelperService.getCachedItemsByProductSkuWithoutOverridingL5Data(STORE_ID, PRODUCT_SKU, Arrays.asList(item.getItemSku())))
        .thenReturn(itemList);
    this.productAndItemSolrIndexer.updateSolrOnPriceAndWholesalePriceFlagChange(item, true);
    verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    verify(this.productHelperService).getCachedItemsByProductSkuWithoutOverridingL5Data(STORE_ID, PRODUCT_SKU, Arrays.asList(item.getItemSku()));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getProductSku(), PRODUCT_SKU);
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(this.objectConverter).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void updateSolrOnPriceAndWholesalePriceFlagChange_wholesaleFlagNull() throws Exception {
    productSolr.setVariantCount(1);
    List<Item> itemList = new ArrayList();
    itemList.add(item);
    Mockito.when(this.productSolrRepository.findByProductSku(null, PRODUCT_SKU))
        .thenReturn(productSolr);
    Mockito.when(productHelperService.getCachedItemsByProductSkuWithoutOverridingL5Data(STORE_ID, PRODUCT_SKU, Arrays.asList(item.getItemSku())))
        .thenReturn(itemList);
    this.productAndItemSolrIndexer.updateSolrOnPriceAndWholesalePriceFlagChange(item, null);
    verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    verify(this.productHelperService).getCachedItemsByProductSkuWithoutOverridingL5Data(STORE_ID, PRODUCT_SKU, Arrays.asList(item.getItemSku()));
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(this.objectConverter).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void updateSolrOnPriceAndWholesalePriceFlagChange_exceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(productItemSolrRepository)
        .executeSolrDocumentAtomicUpdate(Mockito.any(SolrInputDocument.class));
    try {
      this.productAndItemSolrIndexer.updateSolrOnPriceAndWholesalePriceFlagChange(item, true);
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    }
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.productItemSolrRepository);
    verifyNoMoreInteractions(this.productAndItemConstructorService);
    verifyNoMoreInteractions(this.systemParameterService);
    verifyNoMoreInteractions(this.offlineItemService);
    verifyNoMoreInteractions(this.cloudSolrClient);
    verifyNoMoreInteractions(this.cloudSolrClientL3);
    verifyNoMoreInteractions(this.productSolrConstructorService);
    verifyNoMoreInteractions(this.productSolrRepository);
    verifyNoMoreInteractions(this.inventoryOutbound);
    verifyNoMoreInteractions(this.productL3SolrReindexStatusService, itemPickupPointService);
  }

  @Test
  public void applyProductAndItemsItemWithExceptionTest() throws Exception {
    masterDataDetailWithProductAndItemsResponseVo.getProductAndItems().get(0).getProduct()
        .setMasterDataProduct(new MasterDataProduct());
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    Mockito.when(this.productSolrRepository.findByProductSku(product.getMerchantCode(), PRODUCT_SKU)).thenReturn(productSolr);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productItemSolrRepository)
        .findByStoreIdAndProductSku(STORE_ID, productAndItems.getProduct().getProductSku(),
          StringUtils.EMPTY);
    Mockito.doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    productAndItemSolrIndexer.applyMasterDataDetailWithProductAndItems(masterDataDetailWithProductAndItemsResponseVo, false);
    Mockito.verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSku(STORE_ID, productAndItems.getProduct().getProductSku(),
          MERCHANT_CODE);
    Mockito.verify(this.productAndItemConstructorService)
        .constructItem(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productAndItemConstructorService)
        .constructProduct(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.cloudSolrClient).add(Mockito.anyCollection());
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    Mockito.verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    Mockito.verify(this.productSolrRepository).findByProductSku(product.getMerchantCode(), PRODUCT_SKU);
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyMasterDataDetailWithProductAndItems_emptyMasterDataTest() throws Exception {
    masterDataDetailWithProductAndItemsResponseVo.getProductAndItems().get(0).getProduct()
        .setMasterDataProduct(new MasterDataProduct());
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    Mockito.when(this.productSolrRepository.findByProductSku(product.getMerchantCode(), PRODUCT_SKU)).thenReturn(productSolr);
    Mockito.doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    productAndItemSolrIndexer.applyMasterDataDetailWithProductAndItems(masterDataDetailWithProductAndItemsResponseVo, false);
    Mockito.verify(this.productItemSolrRepository)
        .findByStoreIdAndProductSku(STORE_ID, productAndItems.getProduct().getProductSku(),
          MERCHANT_CODE);
    Mockito.verify(this.productAndItemConstructorService)
        .constructItem(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productAndItemConstructorService)
        .constructProduct(Mockito.any(), Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.cloudSolrClient).add(Mockito.anyCollection());
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    Mockito.verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    Mockito.verify(this.productSolrRepository).findByProductSku(product.getMerchantCode(), PRODUCT_SKU);
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void getProductSolrByProductSkuTest() throws Exception{
    when(productSolrRepository.findByProductSku(null, PRODUCT_SKU)).thenReturn(productSolr);
    this.productAndItemSolrIndexer.getProductSolrByProductSku(PRODUCT_SKU);
    verify(productSolrRepository).findByProductSku(null, PRODUCT_SKU);
  }

  @Test
  public void applyMasterDataChangesEmptyListTest() throws Exception {
    final ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    productDomainEventModel.setProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE);
    final ProductAndItemSolr solrObject = new ProductAndItemSolr();
    final List<ProductAndItemSolr> solrList = Arrays.asList(solrObject);
    Map<String, FieldValueObject> fieldValueObjectMap = new HashMap<>();
    fieldValueObjectMap
        .put(SolrFieldNames.BRAND, FieldValueObject.builder().oldValue(OLD_BRAND).newValue(NEW_BRAND).build());
    when(this.productItemSolrRepository.findByProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE))
        .thenReturn(solrList);
    when(this.productAndItemConstructorService.constructByMasterDataChangeModel(solrObject, productDomainEventModel,
        productAndTotalScoreMap))
        .thenReturn(null);
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    when(this.productSolrRepository.findByProductCode(PRODUCT_CODE, null)).thenReturn(Arrays.asList(productSolr));
    when(this.productSolrConstructorService.constructProductFromMasterDataChanges(productSolr, productDomainEventModel,
        productAndTotalScoreMap))
        .thenReturn(null);
    when(this.cloudSolrClientL3.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    this.productAndItemSolrIndexer.applyMasterDataChanges(productDomainEventModel, productAndTotalScoreMap);
    verify(this.productItemSolrRepository).findByProductCode(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_CODE);
    verify(this.productAndItemConstructorService).constructByMasterDataChangeModel(solrObject, productDomainEventModel,
        productAndTotalScoreMap);
    verify(this.productSolrRepository).findByProductCode(PRODUCT_CODE, null);
    verify(this.productSolrConstructorService)
        .constructProductFromMasterDataChanges(productSolr, productDomainEventModel, productAndTotalScoreMap);
  }

  @Test
  public void updateSolrOnPristineChangesValidationExceptionTest() {
    Assertions.assertThrows(Exception.class, () -> this.productAndItemSolrIndexer.updateSolrOnPristineChanges(Collections.emptyList()));
  }

  @Test
  public void updateSolrOnPristineChangesTest () throws Exception {
    item.setPristineDataItem(pristineDataItem);
    this.productAndItemSolrIndexer.updateSolrOnPristineChanges(Collections.singletonList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
  }

  @Test
  public void updateSolrOnPristineChanges_DeleteMapTest () throws Exception {
    item.setPristineDataItem(pristineDataItem);
    this.productAndItemSolrIndexer.updateSolrOnPristineChanges(Collections.singletonList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
  }

  @Test
  public void updateSolrOnPristineChangesTest_Exception() throws Exception {
    item.setPristineDataItem(pristineDataItem);
    Mockito.doThrow(Exception.class).when(this.productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    try {
      this.productAndItemSolrIndexer.updateSolrOnPristineChanges(Collections.singletonList(item));
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    }
  }

  @Test
  public void getPickupPointCodesByProductSkuTest() throws Exception {
    productSolr.setPickupPointCodes(Arrays.asList(PICKUP_POINT_CODE));
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito
        .when(this.productSolrRepository.findOneByProductSkuAndMarkForDeleteFalse(PRODUCT_SKU))
        .thenReturn(productSolr);
    ProductPickupPointListResponse productPickupPointListResponse =
        productAndItemSolrIndexer.getPickupPointCodesByProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.productSolrRepository)
        .findOneByProductSkuAndMarkForDeleteFalse(PRODUCT_SKU);
    assertTrue(
        productPickupPointListResponse.getPickupPointCodes().contains(PICKUP_POINT_CODE));
  }

  @Test
  public void getPickupPointCodesByProductSku_emptySolrResponseTest() throws Exception {
    Mockito
        .when(this.productSolrRepository.findOneByProductSkuAndMarkForDeleteFalse(PRODUCT_SKU))
        .thenReturn(new ProductSolr());
    try {
      Assertions.assertThrows(Exception.class, () -> productAndItemSolrIndexer.getPickupPointCodesByProductSku(STORE_ID, PRODUCT_SKU));
    } finally {
      Mockito.verify(this.productSolrRepository)
          .findOneByProductSkuAndMarkForDeleteFalse(PRODUCT_SKU);
    }
  }

  @Test
  public void productListingAtomicUpdateTest() throws Exception {
    Price price = new Price();
    price.setListPrice(1000.0);
    price.setOfferPrice(1000.0);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    item.setPrice(Stream.of(price).collect(Collectors.toSet()));
    item.setItemViewConfigs(Stream.of(itemViewConfig).collect(Collectors.toSet()));
    item.setActivePromoBundlings(ImmutableSet.of(Constants.WHOLESALE_PRICE));
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);
    productAndItemSolr.setWholesalePriceActivated(true);
    productSolr.setVariantCount(2);
    productSolr.setMinimumListPrice(10.0);
    productSolr.setMaximumListPrice(100.0);
    productSolr.setMaximumSellingPrice(100.0);
    productSolr.setMinimumSellingPrice(10.0);
    product.setProductType(ProductType.REGULAR);
    List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
    productAndItemSolrs.add(productAndItemSolr);
    doNothing().when(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(item.getItemSku()))).thenReturn(new ArrayList<>());
    when(cloudSolrClientL3.add(solrInputDocumentArgumentCaptor.capture())).thenReturn(new UpdateResponse());
    productAndItemSolrIndexer.productListingAtomicUpdate(product, Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.getValue());
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(item.getItemSku()));
    verify(cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.getValue());
  }

  @Test
  public void productListingAtomicUpdateViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    Price price = new Price();
    price.setListPrice(1000.0);
    price.setOfferPrice(1000.0);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    item.setPrice(Stream.of(price).collect(Collectors.toSet()));
    item.setItemViewConfigs(Stream.of(itemViewConfig).collect(Collectors.toSet()));
    item.setActivePromoBundlings(ImmutableSet.of(Constants.WHOLESALE_PRICE));
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);
    productAndItemSolr.setWholesalePriceActivated(true);
    productSolr.setVariantCount(2);
    productSolr.setMinimumListPrice(10.0);
    productSolr.setMaximumListPrice(100.0);
    productSolr.setMaximumSellingPrice(100.0);
    productSolr.setMinimumSellingPrice(10.0);
    product.setProductType(ProductType.REGULAR);
    List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
    productAndItemSolrs.add(productAndItemSolr);
    doNothing().when(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    when(productHelperService.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(item.getItemSku()))).thenReturn(new ArrayList<>());
    when(cloudSolrClientL3.add(solrInputDocumentArgumentCaptor.capture())).thenReturn(new UpdateResponse());
    productAndItemSolrIndexer.productListingAtomicUpdate(product, Arrays.asList(item));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.getValue());
    verify(productHelperService).getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(item.getItemSku()));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void pickupPointCodesAtomicUpdateTest() throws Exception {
    item.setPickupPointCode(PICKUP_POINT_CODE);
    doNothing().when(productItemSolrRepository)
        .executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    productAndItemSolrIndexer.pickupPointCodesAtomicUpdate(item);
    verify(productItemSolrRepository)
        .executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.getValue());
  }

  @Test
  public void pickupPointCodesAtomicUpdateExceptionTest() throws Exception {
    doThrow(SolrServerException.class).when(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.capture());
    productAndItemSolrIndexer.pickupPointCodesAtomicUpdate(item);
    verify(productItemSolrRepository).executeSolrDocumentAtomicUpdate(solrInputDocumentArgumentCaptor.getValue());
  }

  @Test
  public void pickupPointCodesAtomicUpdateL3DifferentLocationTest() {
    item.setPickupPointCode(PICKUP_POINT_CODE);
    item.setPermanentDelete(true);
    Item item2 = new Item();
    item2.setPickupPointCode(PRODUCT_CODE);
    when(productHelperService
        .getCachedItemsByProductSku(item.getStoreId(), item.getProductSku(), Arrays.asList(item.getItemSku())))
        .thenReturn(new ArrayList<>(Arrays.asList(item, item2)));
    when(productService.findByStoreIdAndProductSku(any(), any())).thenReturn(new Product());
    when(productService.saveProductWithoutUpdatingSolr(Mockito.any(), Mockito.anyList(), eq(StringUtils.EMPTY))).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(Mockito.any()))
        .thenReturn(productAndItemEventModel);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(productAndItems))
        .thenReturn(productAndItemEventModel);
    productAndItemSolrIndexer.pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), true);
    verify(productHelperService)
        .getCachedItemsByProductSku(item.getStoreId(), item.getProductSku(), Arrays.asList(item.getItemSku()));
    verify(productService).findByStoreIdAndProductSku(any(), any());
    verify(productService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY));
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(productAndItemsVoArgumentCaptor.capture());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getPickupPointCodes().size(), 0);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getPickupPointCodes().iterator().next());
  }

  @Test
  public void pickupPointCodesAtomicUpdateL3SameLocationTest() {
    item.setPickupPointCode(PICKUP_POINT_CODE);
    when(productService.findByStoreIdAndProductSku(any(), any())).thenReturn(new Product());
    when(productService.saveProductWithoutUpdatingSolr(Mockito.any(), Mockito.anyList(), eq(StringUtils.EMPTY))).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(Mockito.any()))
        .thenReturn(productAndItemEventModel);
    productAndItemSolrIndexer.pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), false);
    verify(productService).findByStoreIdAndProductSku(any(), any());
    verify(productService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(StringUtils.EMPTY));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(productAndItemsVoArgumentCaptor.capture());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
  }

  @Test
  public void pickupPointCodesAtomicUpdateL3ExceptionTest() throws Exception {
    item.setPickupPointCode(PICKUP_POINT_CODE);
    when(cloudSolrClientL3.add(solrInputDocumentArgumentCaptor.capture())).thenThrow(SolrServerException.class);
    when(productService.findByStoreIdAndProductSku(any(), any())).thenReturn(new Product());
    when(productService.saveProductWithoutUpdatingSolr(Mockito.any(), Mockito.anyList(), eq(StringUtils.EMPTY))).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(productAndItems))
        .thenReturn(productAndItemEventModel);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(Mockito.any()))
        .thenReturn(productAndItemEventModel);
    productAndItemSolrIndexer.pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), false);
    verify(productService).findByStoreIdAndProductSku(any(), any());
    verify(productService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(StringUtils.EMPTY));
  }

  @Test
  public void offlineItemPriceAtomicUpdateTest() throws IOException, SolrServerException {
    when(cloudSolrClient.add(solrInputDocumentArgumentCaptor.capture())).thenReturn(new UpdateResponse());
    productAndItemSolrIndexer.offlineItemPriceAtomicUpdate(ITEM_SKU, Arrays.asList(offlineItem));
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.getValue());
  }

  @Test
  public void offlineItemPriceAtomicUpdateExceptionTest() throws IOException, SolrServerException {
    when(cloudSolrClient.add(solrInputDocumentArgumentCaptor.capture())).thenThrow(SolrServerException.class);
    productAndItemSolrIndexer.offlineItemPriceAtomicUpdate(ITEM_SKU, Arrays.asList(offlineItem));
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.getValue());
  }

  @Test
  public void reindexOfflineItemAndCncActivatedFlagEmptyTest() throws Exception {
    when(productAndItemConstructorService.getOfflinePricesByStoreIdAndItemSku(ITEM_SKU, item.getItemSku())).thenReturn(
        new ArrayList<>());
    when(cloudSolrClient.add(solrInputDocumentArgumentCaptor.capture())).thenReturn(new UpdateResponse());
    productAndItemSolrIndexer.reindexOfflineItemAndCncActivatedFlag(ITEM_SKU, new ArrayList<>());
    Assertions.assertTrue(StringUtils.isNotBlank(ITEM_SKU));
  }

  @Test
  public void reindexOfflineItemAndCncActivatedFlagTest() throws Exception {
    when(productAndItemConstructorService.getOfflinePricesByStoreIdAndItemSku(ITEM_SKU, ITEM_SKU)).thenReturn(
        new ArrayList<>());
    productAndItemSolrIndexer.reindexOfflineItemAndCncActivatedFlag(ITEM_SKU, Arrays.asList(item));
    verify(productAndItemConstructorService).getOfflinePricesByStoreIdAndItemSku(ITEM_SKU, ITEM_SKU);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
  }

    @Test
  public void reindexOfflineItemAndCncActivatedFlagExceptionTest() throws Exception {
    when(productAndItemConstructorService.getOfflinePricesByStoreIdAndItemSku(ITEM_SKU, ITEM_SKU)).thenReturn(new ArrayList<>());
    doThrow(SolrServerException.class).when(productItemSolrRepository).executeSolrDocumentAtomicUpdate(Mockito.any());
    productAndItemSolrIndexer.reindexOfflineItemAndCncActivatedFlag(ITEM_SKU, Arrays.asList(item));
    verify(productAndItemConstructorService).getOfflinePricesByStoreIdAndItemSku(ITEM_SKU, ITEM_SKU);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
  }

  @Test
  public void applyPendingProductReindexTest() throws IOException, SolrServerException {
    systemParameter = new SystemParameter();
    systemParameter.setValue(String.valueOf(10));
    when(productHelperService
      .getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(ITEM_SKU)))
      .thenReturn(Arrays.asList(item));
    when(this.cloudSolrClientL3.add(Mockito.anyList()))
      .thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    productAndItemSolrIndexer
      .applyPendingProductReindex(Arrays.asList(product), Arrays.asList(PRODUCT_SKU));
    verify(this.productSolrConstructorService)
      .constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.cloudSolrClientL3).add(Mockito.anyCollection());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }
  @Test
  public void applyPendingProductReindexPreOrderSwitchOffTest() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", false);
    systemParameter = new SystemParameter();
    systemParameter.setValue(String.valueOf(10));
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    product.setPreOrder(preOrder);
    when(productHelperService
        .getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    when(this.cloudSolrClientL3.add(Mockito.anyList()))
        .thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    productAndItemSolrIndexer
        .applyPendingProductReindex(Arrays.asList(product), Arrays.asList(PRODUCT_SKU));
    verify(this.productSolrConstructorService)
        .constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.cloudSolrClientL3).add(Mockito.anyCollection());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyPendingProductReindexPreOrderObjNullTest() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    systemParameter = new SystemParameter();
    systemParameter.setValue(String.valueOf(10));
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    when(productHelperService
        .getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    when(this.cloudSolrClientL3.add(Mockito.anyList()))
        .thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    productAndItemSolrIndexer
        .applyPendingProductReindex(Arrays.asList(product), Arrays.asList(PRODUCT_SKU));
    verify(this.productSolrConstructorService)
        .constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.cloudSolrClientL3).add(Mockito.anyCollection());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyPendingProductReindexPreOrderNullDateTest() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    systemParameter = new SystemParameter();
    systemParameter.setValue(String.valueOf(10));
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    product.setPreOrder(preOrder);
    when(productHelperService
        .getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    when(this.cloudSolrClientL3.add(Mockito.anyList()))
        .thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    productAndItemSolrIndexer
        .applyPendingProductReindex(Arrays.asList(product), Arrays.asList(PRODUCT_SKU));
    verify(this.productSolrConstructorService)
        .constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.cloudSolrClientL3).add(Mockito.anyCollection());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyPendingProductReindexPreOrderPastTest() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    systemParameter = new SystemParameter();
    systemParameter.setValue(String.valueOf(10));
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    preOrder.setPreOrderDate(calendar.getTime());
    product.setPreOrder(preOrder);
    when(productHelperService
        .getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    when(this.cloudSolrClientL3.add(Mockito.anyList()))
        .thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    productAndItemSolrIndexer
        .applyPendingProductReindex(Arrays.asList(product), Arrays.asList(PRODUCT_SKU));
    verify(this.productSolrConstructorService)
        .constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.cloudSolrClientL3).add(Mockito.anyCollection());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyPendingProductReindexPreOrderFutureTest() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    systemParameter = new SystemParameter();
    systemParameter.setValue(String.valueOf(10));
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    preOrder.setPreOrderDate(calendar.getTime());
    product.setPreOrder(preOrder);
    when(productHelperService
        .getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    when(this.cloudSolrClientL3.add(Mockito.anyList()))
        .thenReturn(new UpdateResponse());
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, true)).thenReturn(true);
    productAndItemSolrIndexer
        .applyPendingProductReindex(Arrays.asList(product), Arrays.asList(PRODUCT_SKU));
    verify(this.productSolrConstructorService)
        .constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.cloudSolrClientL3).add(Mockito.anyCollection());
    verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, true);
  }

  @Test
  public void updateSolrOnProductTypeChange() throws Exception {
    Item item1= new Item();
    item1.setPristineDataItem(pristineDataItem);
    this.productAndItemSolrIndexer.updateSolrOnProductTypeChange(Arrays.asList(item, item1), ProductType.BIG_PRODUCT);
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
  }

  @Test
  public void updateSolrOnProductTypeChangeExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    try {
      this.productAndItemSolrIndexer.updateSolrOnProductTypeChange(Arrays.asList(item), ProductType.BIG_PRODUCT);
    } finally {
      verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(solrInputDocumentsArgumentCaptor.capture());
    }
  }

  @Test
  public void updateSolrOnItemViewConfigChangesByItemPickupPointTest() throws Exception {
    doNothing().when(productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    productAndItemSolrIndexer.updateSolrOnItemViewConfigChangesByItemPickupPoint(Arrays.asList(new ItemPickupPoint()));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(Mockito.anyList());
  }

  @Test
  public void updateSolrOnItemViewConfigChangesByItemPickupPointExceptionTest() throws Exception {
    doThrow(SolrServerException.class).when(productItemSolrRepository)
        .executeSolrDocumentsAtomicUpdate(Mockito.anyList());
    productAndItemSolrIndexer.updateSolrOnItemViewConfigChangesByItemPickupPoint(Arrays.asList(new ItemPickupPoint()));
    verify(productItemSolrRepository).executeSolrDocumentsAtomicUpdate(Mockito.anyList());
  }

  @Test
  public void updateSolrOnItemViewConfigChangesByItemPickupPointEmptyTest() {
    Assertions.assertThrows(Exception.class, () -> productAndItemSolrIndexer.updateSolrOnItemViewConfigChangesByItemPickupPoint(new ArrayList<>()));
  }

  @Test
  public void updatePickUpPointAndVariantCountAndCncActivationTest() throws SolrServerException, IOException {
    itemPickupPoint.setCncActive(true);
    productAndItemSolrIndexer.updatePickUpPointAndVariantCountAndCncActivation(productAndItems,
        Arrays.asList(itemPickupPoint));
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updatePickUpPointAndVariantCountAndCncActivationViaEventTest() throws SolrServerException, IOException {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    itemPickupPoint.setCncActive(true);
    productAndItemSolrIndexer.updatePickUpPointAndVariantCountAndCncActivation(productAndItems,
        Arrays.asList(itemPickupPoint));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(productAndItemEventModelArgumentCaptor.getValue().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void updatePickUpPointAndVariantCountAndCncActivationExceptionTest() throws SolrServerException, IOException {
    itemPickupPoint.setCncActive(true);
    Mockito.doThrow(ApplicationRuntimeException.class).when(cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    try {
      Assertions.assertThrows(Exception.class, () -> productAndItemSolrIndexer.updatePickUpPointAndVariantCountAndCncActivation(productAndItems,
          Arrays.asList(itemPickupPoint)));
    } finally {
      verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    }
  }

  @Test
  public void updateProductDetailsInSolrTest() {
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(objectConverterService.convertToProductEventModel(product)).thenReturn(productEventModel);
    this.productAndItemSolrIndexer.updateProductDetailsInSolr(Arrays.asList(product));
    Mockito.verify(objectConverterService).convertToProductEventModel(product);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
  }

  @Test
  public void updateProductAndItemDetailsInSolrTest() {
    ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
    productAndItemEventModel.setProductSku(PRODUCT_SKU);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(productAndItems))
      .thenReturn(productAndItemEventModel);
    this.productAndItemSolrIndexer.updateProductAndItemDetailsInSolr(product, Arrays.asList(item), false);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(productAndItems);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
  }

  @Test
  public void updatePickUpPointsInProductSolrTest() throws SolrServerException, IOException {
    productAndItemSolrIndexer.updateDistinctPickupPointCodesAndL5Count(PRODUCT_SKU,
      ImmutableSet.of(PRODUCT_SKU), L5_COUNT, false, product.getMerchantCode());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU,
        solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.PRODUCT_SKU));
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, ImmutableSet.of(PRODUCT_SKU)),
        solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.PICKUP_POINT_CODES));
    Assertions.assertEquals(L5_COUNT,
        solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.L5_COUNT));
  }

  @Test
  public void updatePickUpPointsInProductSolrViaEventTest() throws SolrServerException, IOException {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "eventBasedSolrUpdateEnable", true);
    productAndItemSolrIndexer.updateDistinctPickupPointCodesAndL5Count(PRODUCT_SKU,
      ImmutableSet.of(PRODUCT_SKU), L5_COUNT, true, product.getMerchantCode());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, ImmutableSet.of(PRODUCT_SKU)),
        productAndItemEventModelArgumentCaptor.getValue().getFieldsAndValues().get(SolrFieldNames.PICKUP_POINT_CODES));
    Assertions.assertEquals(L5_COUNT, productAndItemEventModelArgumentCaptor.getValue().getFieldsAndValues().get(SolrFieldNames.L5_COUNT));
    Assertions.assertEquals(true,
      productAndItemEventModelArgumentCaptor.getValue().getFieldsAndValues().get(SolrFieldNames.FBB_ACTIVATED));
  }

  @Test
  public void updatePickUpPointsInProductSolrExceptionTest() throws SolrServerException, IOException {
    Mockito.when(cloudSolrClientL3.add(solrInputDocumentArgumentCaptor.capture())).thenThrow(SolrServerException.class);
    productAndItemSolrIndexer.updateDistinctPickupPointCodesAndL5Count(PRODUCT_SKU,
      ImmutableSet.of(PRODUCT_SKU), L5_COUNT, false, product.getMerchantCode());
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.getValue());
  }

  @Test
  public void atomicUpdateToSolrUsingEventForDateFieldTest() throws SolrServerException, IOException {
    fieldAndValuesMap.put(SolrFieldNames.L5_COUNT, L5_COUNT);
    fieldAndValuesMap.put(SolrFieldNames.PICKUP_POINT_CODES, Collections.singletonMap(SolrConstants.SET_CLAUSE, ImmutableSet.of(PICKUP_POINT_CODE)));
    fieldAndValuesMap.put(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE, null);
    productAndItemSolrIndexer.eventBasedAtomicUpdateToSolr(PRODUCT_SKU, MERCHANT_CODE, fieldAndValuesMap);
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU,
        solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.PRODUCT_SKU));
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, ImmutableSet.of(PICKUP_POINT_CODE)),
        solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.PICKUP_POINT_CODES));
    Assertions.assertEquals(L5_COUNT,
        solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.L5_COUNT));
    Assertions.assertEquals(null,
        ((Map)solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE))
            .get(SolrConstants.SET_CLAUSE));
  }

  @Test
  public void atomicUpdateToSolrUsingEventForDateFieldMerchantCodeTest() throws SolrServerException, IOException {
    fieldAndValuesMap.put(SolrFieldNames.L5_COUNT, L5_COUNT);
    fieldAndValuesMap.put(SolrFieldNames.PICKUP_POINT_CODES, Collections.singletonMap(SolrConstants.SET_CLAUSE, ImmutableSet.of(PICKUP_POINT_CODE)));
    fieldAndValuesMap.put(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE, null);
    product.setMerchantCode(MERCHANT_CODE);
    Mockito.when(productService.getProduct(Constants.DEFAULT_STORE_ID, PRODUCT_SKU)).thenReturn(product);
    productAndItemSolrIndexer.eventBasedAtomicUpdateToSolr(PRODUCT_SKU, "", fieldAndValuesMap);
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.capture());
    verify(productService).getProduct(Constants.DEFAULT_STORE_ID, PRODUCT_SKU);
    Assertions.assertEquals(PRODUCT_SKU,
        solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.PRODUCT_SKU));
    Assertions.assertEquals(MERCHANT_CODE,
        solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.MERCHANT_CODE));
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, ImmutableSet.of(PICKUP_POINT_CODE)),
        solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.PICKUP_POINT_CODES));
    Assertions.assertEquals(L5_COUNT,
        solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.L5_COUNT));
    Assertions.assertEquals(null,
        ((Map)solrInputDocumentArgumentCaptor.getValue().getFieldValue(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE))
            .get(SolrConstants.SET_CLAUSE));
  }

  @Test
  public void atomicUpdateToSolrUsingEventExceptionTest() throws SolrServerException, IOException {
    fieldAndValuesMap.put(SolrFieldNames.L5_COUNT, L5_COUNT);
    fieldAndValuesMap.put(SolrFieldNames.PICKUP_POINT_CODES, Collections.singletonMap(SolrConstants.SET_CLAUSE, ImmutableSet.of(PICKUP_POINT_CODE)));
    Mockito.when(cloudSolrClientL3.add(solrInputDocumentArgumentCaptor.capture())).thenThrow(SolrServerException.class);
    productAndItemSolrIndexer.eventBasedAtomicUpdateToSolr(PRODUCT_SKU, MERCHANT_CODE, fieldAndValuesMap);
    verify(this.cloudSolrClientL3).add(solrInputDocumentArgumentCaptor.getValue());
  }

  @Test
  public void deleteL3AndL4DocumentsToReduceLoadTest() throws SolrServerException, IOException {
    List<ProductSolr> productSolrList = new ArrayList<>();
    ProductSolr productSolr1 = new ProductSolr();
    productSolr1.setProductCode("dddd");
    productSolr1.setProductSku("dddd");
    productSolrList.add(productSolr1);
    ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    productDomainEventModel.setProductCode(PRODUCT_CODE);
    when(productSolrRepository.findByProductCode(Mockito.anyString(), Mockito.anyBoolean())).thenReturn(
        productSolrList);
    productAndItemSolrIndexer.deleteL3AndL4Documents(productDomainEventModel);
    verify(productSolrRepository).findByProductCode(Mockito.any(), Mockito.any());
    verify(productSolrRepository).deleteSolrDocumentsByListOfProductSku(Mockito.any());
    verify(cloudSolrClient).deleteByQuery(Mockito.any());
  }

  @Test
  public void deleteL3AndL4DocumentsToReduceLoadTestProductSolrListNull() throws SolrServerException, IOException {
    List<ProductSolr> productSolrList = new ArrayList<>();
    ProductSolr productSolr1 = new ProductSolr();
    productSolr1.setProductCode("dddd");
    productSolr1.setProductSku("dddd");
    productSolrList.add(productSolr1);
    when(productSolrRepository.findByProductCode(Mockito.anyString(), Mockito.anyBoolean())).thenReturn(null);
    productAndItemSolrIndexer.deleteL3AndL4Documents(new ProductDomainEventModel());
    verify(productSolrRepository).findByProductCode(Mockito.any(), Mockito.any());
  }

  @Test
  public void deleteL3AndL4DocumentsToReduceLoadTestProductSkyNull() throws SolrServerException, IOException {
    List<ProductSolr> productSolrList = new ArrayList<>();
    ProductSolr productSolr1 = new ProductSolr();
    productSolr1.setProductCode("dddd");
    productSolrList.add(productSolr1);
    when(productSolrRepository.findByProductCode(Mockito.anyString(), Mockito.anyBoolean())).thenReturn(
        productSolrList);
    productAndItemSolrIndexer.deleteL3AndL4Documents(new ProductDomainEventModel());
    verify(productSolrRepository).findByProductCode(Mockito.any(), Mockito.any());
  }

  @Test
  public void deleteSolrDocumentsByProductSkuL4Solr() throws SolrServerException, IOException {
    try {
      Mockito.when(cloudSolrClient.deleteByQuery(Mockito.anyString())).thenThrow(SolrServerException.class);
      Assertions.assertThrows(Exception.class, () -> productAndItemSolrIndexer.deleteSolrDocumentByProductSkuInL4Solr(PRODUCT_SKU));
    } finally {
      verify(cloudSolrClient).deleteByQuery(Mockito.any());
    }
  }

  @Test
  public void deleteSolrDocumentsByProductSkuL4SolrForProductSkuEmpty()
  {
    productAndItemSolrIndexer.deleteSolrDocumentByProductSkuInL4Solr(StringUtils.EMPTY);
  }

  @Test
  public void reindexOnExternalSearchTest() {
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    productAndItemSolrIndexer.reindexOnExternalSearch(
        ExternalSearchReindexToSolrEventModel.builder().productSku(PRODUCT_SKU).storeId(STORE_ID).build());
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(kafkaProducer).send(Mockito.eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.eq(PRODUCT_SKU),
        Mockito.any(ProductAndItemEventModel.class));
  }

  @Test
  public void reindexOnExternalSearchMfdTrueProductTest() {
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(null);
    productAndItemSolrIndexer.reindexOnExternalSearch(
        ExternalSearchReindexToSolrEventModel.builder().productSku(PRODUCT_SKU).storeId(STORE_ID).build());
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void reindexOnExternalSearchMfdTrueProductAutoHealTest() {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "autoHealOosProductsWithStock", true);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    productAndItemSolrIndexer.reindexOnExternalSearch(
        ExternalSearchReindexToSolrEventModel.builder().productSku(PRODUCT_SKU).storeId(STORE_ID).build());
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void applyProductAndItemsWithPreOrderActiveTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    // Setup pre-order with future date (active pre-order)
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    
    // Set pre-order date to future (tomorrow)
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    preOrder.setPreOrderDate(calendar.getTime());
    
    // Set pre-order on product
    product.setPreOrder(preOrder);
    
    final ProductAndItemSolr existingProductAndItemSolr = new ProductAndItemSolr();
    existingProductAndItemSolr.setId(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU);
    existingProductAndItemSolr.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenReturn(Arrays.asList(existingProductAndItemSolr));
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(Arrays.asList(offlineItem));
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    
    // Mock isProductInStock with true parameter (pre-order active)
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, true)).thenReturn(true);
    
    this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    
    verify(this.productAndItemConstructorService).constructItem(existingProductAndItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(existingProductAndItemSolr, this.product, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    
    // Verify isProductInStock was called with true parameter (pre-order active)
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, true);
  }

    @Test
  public void applyProductAndItemsWithPreOrderInactiveTest() throws Exception {
    // Setup pre-order with past date (inactive pre-order)
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    
    // Set pre-order date to past (yesterday)
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    preOrder.setPreOrderDate(calendar.getTime());
    
    // Set pre-order on product
    product.setPreOrder(preOrder);
    
    final ProductAndItemSolr existingProductAndItemSolr = new ProductAndItemSolr();
    existingProductAndItemSolr.setId(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU);
    existingProductAndItemSolr.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenReturn(Arrays.asList(existingProductAndItemSolr));
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(Arrays.asList(offlineItem));
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    
    // Mock isProductInStock with false parameter (pre-order inactive)
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    
    this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    
    verify(this.productAndItemConstructorService).constructItem(existingProductAndItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(existingProductAndItemSolr, this.product, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    
    // Verify isProductInStock was called with false parameter (pre-order inactive)
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyProductAndItemsWithPreOrderNullTest() throws Exception {
    // Setup product with null pre-order
    product.setPreOrder(null);
    
    final ProductAndItemSolr existingProductAndItemSolr = new ProductAndItemSolr();
    existingProductAndItemSolr.setId(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU);
    existingProductAndItemSolr.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenReturn(Arrays.asList(existingProductAndItemSolr));
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(Arrays.asList(offlineItem));
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    
    // Mock isProductInStock with false parameter (no pre-order)
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    
    this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    
    verify(this.productAndItemConstructorService).constructItem(existingProductAndItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(existingProductAndItemSolr, this.product, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    
    // Verify isProductInStock was called with false parameter (no pre-order)
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }

  @Test
  public void applyProductAndItemsWithProductNullTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrIndexer, "preOrderQuotaSwitch", true);
    
    final ProductAndItemSolr existingProductAndItemSolr = new ProductAndItemSolr();
    existingProductAndItemSolr.setId(ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU);
    existingProductAndItemSolr.setProductSku(ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU);
    
    when(this.productItemSolrRepository.findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE))
        .thenReturn(Arrays.asList(existingProductAndItemSolr));
    when(this.offlineItemService
        .findByMerchantCodeAndItemSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID, item.getMerchantCode(),
            ProductAndItemSolrIndexerServiceImplTest.ITEM_SKU)).thenReturn(Arrays.asList(offlineItem));
    when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    doNothing().when(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    when(this.productSolrRepository.findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku())).thenReturn(productSolr);
    when(this.cloudSolrClientL3.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    
    // Mock productService to return null - this is the key test case
    when(this.productService.findByStoreIdAndProductSku(ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU)).thenReturn(null);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU, false)).thenReturn(true);
    when(itemPickupPointService.findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new ArrayList<>());
    
    // This should not throw NullPointerException anymore
    this.productAndItemSolrIndexer.applyProductAndItems(this.productAndItems, false);
    
    verify(this.productAndItemConstructorService).constructItem(existingProductAndItemSolr, this.item, true);
    verify(this.productAndItemConstructorService).constructProduct(existingProductAndItemSolr, this.product, true);
    verify(this.productItemSolrRepository).findByStoreIdAndProductSku(
        ProductAndItemSolrIndexerServiceImplTest.STORE_ID,
        ProductAndItemSolrIndexerServiceImplTest.PRODUCT_SKU, ProductAndItemSolrIndexerServiceImplTest.MERCHANT_CODE);
    verify(this.cloudSolrClient).add(listArgumentCaptor.capture());
    verify(this.cloudSolrClientL3).add(Mockito.any(SolrInputDocument.class));
    verify(this.productSolrConstructorService).constructProduct(productSolr, productAndItems.getProduct(), true);
    verify(this.productSolrRepository).findByProductSku(productAndItems.getProduct().getMerchantCode(), 
        productAndItems.getProduct().getProductSku());
    verify(productService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    verify(itemPickupPointService).findByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString());
    
    // Verify isProductInStock was called with false parameter (no pre-order since product is null)
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU, false);
  }
}
