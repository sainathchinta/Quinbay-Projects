package com.gdn.x.product.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.gdn.x.product.domain.event.model.VideoCompressionEventModel;
import com.gdn.x.product.model.entity.Video;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.dozer.DozerBeanMapper;
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
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.base.mapper.impl.DozerMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.api.ItemPickupPointRepository;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.ItemEventModel;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.domain.event.model.ProductEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.model.entity.DeferredSolrReindexItem;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.DeferredSolrReindexItemService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductL3SolrReindexStatusService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SolrIndexService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.google.common.collect.ImmutableMap;

public class SaveOperationServiceImplTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_SKU1 = "productSku1";
  private static final String PRODUCT_NAME = "productName";

  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_SKU1 = "itemSku1";

  private static final String STORE_ID = "storeId";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String PROMO_BUNDLING_TYPE = "promoBundlingType";
  private static final String PREORDER_TYPE = "preOrderType";
  private static final Integer PREORDER_VALUE = 10;
  private static final String CLIENT_ID = "clientId";
  private static final String VIDEO_URL_CLIENT_ID = "videoUrlClientId";
  private static final String VIDEO_URL = "videoUrl";
  private static final String USERNAME = "username";
  private static final String PICKUP_POINT = "pickupPoint";
  private static final String PICKUP_POINT_NEW = "pickupPointNew";

  @InjectMocks
  private SaveOperationServiceImpl saveOperationServiceImpl;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private SolrIndexService solrIndexService;

  @Mock
  private DeferredSolrReindexItemService deferredSolrReindexItemService;

  @Mock
  private ProductL3SolrReindexStatusService productL3SolrReindexStatusService;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private MasterDataCacheService masterDataCacheService;

  @Mock
  private ProductService productService;

  @Mock
  private ItemPickupPointRepository itemPickupPointRepository;

  @Captor
  private ArgumentCaptor<List<DeferredSolrReindexItem>> deferredSolrReindexItemsArgumentCaptor;

  @Captor
  private ArgumentCaptor<DeferredSolrReindexItem> deferredSolrReindexItemArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<Item>> itemListArgumentCaptor;

  @Captor
  private ArgumentCaptor<Item> itemArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Mock
  private ObjectConverterServiceImpl objectConverterService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Captor
  private ArgumentCaptor<ProductAndItemEventModel> productAndItemEventModelArgumentCaptor;

  @Mock
  private GdnMapper gdnMapper;

  @Mock
  private ProductCacheableService productCacheableService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ItemPriceService itemPriceService;

  @Captor
  private ArgumentCaptor<List<ProductAndItemEventModel>> productAndItemListCaptor̉;

  @Captor
  private ArgumentCaptor<List<ItemPickupPoint>> itemPickupPointListCaptor;

  private GdnMapper mapper = new DozerMapper(new DozerBeanMapper());

  private final Item item = new Item();
  private final Item item1 = new Item();
  private Product product = new Product();
  private PreOrder preOrder;
  private ProductDetailResponse productDetailResponse;
  private MasterDataProductAndItemsVO masterDataProductAndItemsVO;
  private ProductEventModel productEventModel = new ProductEventModel();
  private ItemEventModel itemEventModel = new ItemEventModel();
  private List<ItemEventModel> itemEventModels = new ArrayList<>();
  private ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
  private List<ItemPickupPoint> itemPickupPoints;
  private Map<String, Set<String>> productSkuAndPickupPointCodeMap = new HashMap<>();

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
    item.setMasterDataItem(new MasterDataItem());
    item.setProductSku(PRODUCT_SKU);
    product.setProductSku(PRODUCT_SKU);
    product.setProductCode(PRODUCT_CODE);

    item1.setMasterDataItem(new MasterDataItem());
    item1.setProductSku(PRODUCT_SKU1);

    preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(PREORDER_TYPE);
    preOrder.setPreOrderValue(PREORDER_VALUE);

    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setName(PRODUCT_NAME);

    masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    masterDataProductAndItemsVO.setMasterDataProduct(masterDataProduct);

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    HashSet<ItemViewConfig> set = new HashSet();
    set.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(set);
    itemPickupPoints = Arrays.asList(itemPickupPoint);

    productSkuAndPickupPointCodeMap = new HashMap<>();
    productSkuAndPickupPointCodeMap.putIfAbsent(PRODUCT_SKU, Collections.singleton(PICKUP_POINT));
    ReflectionTestUtils.setField(this.saveOperationServiceImpl, "distributionSellerList", Set.of(MERCHANT_CODE));
  }

  @Test
  public void addActivePromoBundling() {
    Mockito.when(this.saveAndPublishService.addActivePromoBundling(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU,
        SaveOperationServiceImplTest.PROMO_BUNDLING_TYPE)).thenReturn(this.item);

    this.saveOperationServiceImpl.addActivePromoBundling(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, SaveOperationServiceImplTest.PROMO_BUNDLING_TYPE);

    Mockito.verify(this.saveAndPublishService).addActivePromoBundling(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, SaveOperationServiceImplTest.PROMO_BUNDLING_TYPE);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(SaveOperationServiceImplTest.STORE_ID,
        this.item);
  }

  @Test
  public void updateActivePromoBundling() {
    item.setActivePromoBundlings(new HashSet<>());
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false)).thenReturn(item);
    this.saveOperationServiceImpl
        .updateActivePromoBundling(SaveOperationServiceImplTest.STORE_ID, SaveOperationServiceImplTest.ITEM_SKU,
            Constants.WHOLESALE);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false);
    Mockito.verify(itemRepository).save(itemArgumentCaptor.capture());
    Mockito.verify(this.cacheEvictHelperService).evictItemData(eq(SaveOperationServiceImplTest.STORE_ID),
        Mockito.isNull());
    Assertions.assertFalse(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    Assertions.assertTrue(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE));
  }

  @Test
  public void updateActivePromoBundlingInitiallyNullPB() {
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false)).thenReturn(item);
    this.saveOperationServiceImpl
        .updateActivePromoBundling(SaveOperationServiceImplTest.STORE_ID, SaveOperationServiceImplTest.ITEM_SKU,
            Constants.WHOLESALE);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false);
    Mockito.verify(itemRepository).save(itemArgumentCaptor.capture());
    Mockito.verify(this.cacheEvictHelperService).evictItemData(eq(SaveOperationServiceImplTest.STORE_ID),
        Mockito.isNull());
    Assertions.assertFalse(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    Assertions.assertTrue(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE));
  }

  @Test
  public void updateActivePromoBundlingWHPrice() {
    item.setActivePromoBundlings(new HashSet<>());
    item.getActivePromoBundlings().add(Constants.WHOLESALE);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false)).thenReturn(item);
    this.saveOperationServiceImpl
        .updateActivePromoBundling(SaveOperationServiceImplTest.STORE_ID, SaveOperationServiceImplTest.ITEM_SKU,
            Constants.WHOLESALE_PRICE);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false);
    Mockito.verify(itemRepository).save(itemArgumentCaptor.capture());
    Mockito.verify(this.cacheEvictHelperService).evictItemData(eq(SaveOperationServiceImplTest.STORE_ID),
        Mockito.isNull());
    Assertions.assertTrue(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    Assertions.assertFalse(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE));
  }

  @Test
  public void updateActivePromoBundlingInitiallyNullPBWHPrice() {
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false)).thenReturn(item);
    this.saveOperationServiceImpl
        .updateActivePromoBundling(SaveOperationServiceImplTest.STORE_ID, SaveOperationServiceImplTest.ITEM_SKU,
            Constants.WHOLESALE_PRICE);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false);
    Mockito.verify(itemRepository).save(itemArgumentCaptor.capture());
    Mockito.verify(this.cacheEvictHelperService).evictItemData(eq(SaveOperationServiceImplTest.STORE_ID),
        Mockito.isNull());
    Assertions.assertTrue(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    Assertions.assertFalse(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE));
  }

  @Test
  public void updateActivePromoBundlingWHPriceAndComboTest() {
    item.setActivePromoBundlings(new HashSet<>());
    item.getActivePromoBundlings().add(Constants.WHOLESALE);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false)).thenReturn(item);
    this.saveOperationServiceImpl
        .updateActivePromoBundling(SaveOperationServiceImplTest.STORE_ID, SaveOperationServiceImplTest.ITEM_SKU,
            Constants.COMBO);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false);
    Mockito.verify(itemRepository).save(itemArgumentCaptor.capture());
    Mockito.verify(this.cacheEvictHelperService).evictItemData(eq(SaveOperationServiceImplTest.STORE_ID),
        Mockito.isNull());
    Assertions.assertFalse(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    Assertions.assertTrue(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE));
    Assertions.assertTrue(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.COMBO));
  }

  @Test
  public void updateActivePromoBundlingWithWHPriceAndComboViceVersaTest() {
    item.setActivePromoBundlings(new HashSet<>());
    item.getActivePromoBundlings().add(Constants.COMBO);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false)).thenReturn(item);
    this.saveOperationServiceImpl
        .updateActivePromoBundling(SaveOperationServiceImplTest.STORE_ID, SaveOperationServiceImplTest.ITEM_SKU,
            Constants.WHOLESALE);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, false);
    Mockito.verify(itemRepository).save(itemArgumentCaptor.capture());
    Mockito.verify(this.cacheEvictHelperService).evictItemData(eq(SaveOperationServiceImplTest.STORE_ID),
        Mockito.isNull());
    Assertions.assertFalse(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    Assertions.assertTrue(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE));
    Assertions.assertTrue(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.COMBO));
  }


  @Test
  public void inserItemTest() {
    Map<String, Object> fieldsAndValues =
        CommonUtil.getFieldsAndValuesL3ForPromoAndWholesaleUpdate((itemPickupPoints), false, false);
    Mockito.when(this.saveAndPublishService.insertItem(this.item)).thenReturn(this.item);
    Mockito.when(objectConverterService.convertToItemEventModel(item)).thenReturn(new ItemEventModel());
    this.saveOperationServiceImpl.insertItem(this.item);
    Mockito.verify(this.saveAndPublishService).insertItem(this.item);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(objectConverterService).convertToItemEventModel(item);
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
  }

  @Test
  public void inserItemTestNullItem() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.saveOperationServiceImpl.insertItem(null));
  }

  @Test
  public void insertItemsTest() {
    List<Item> items = Arrays.asList(this.item);
    this.saveOperationServiceImpl.insertItems(items);
    Mockito.verify(this.saveAndPublishService).insertItems(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
  }

  @Test
  public void insertItemsTestEmptyItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.saveOperationServiceImpl.insertItems(new ArrayList<Item>()));
  }


  @Test
  public void insertItemsTestNullItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.saveOperationServiceImpl.insertItems(null));
  }

  @Test
  public void removeActivePromoBundling() {
    Mockito.when(this.saveAndPublishService.removeActivePromoBundling(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU,
        SaveOperationServiceImplTest.PROMO_BUNDLING_TYPE)).thenReturn(this.item);

    this.saveOperationServiceImpl.removeActivePromoBundling(SaveOperationServiceImplTest.STORE_ID,
        SaveOperationServiceImplTest.ITEM_SKU, SaveOperationServiceImplTest.PROMO_BUNDLING_TYPE);

    Mockito.verify(this.saveAndPublishService).removeActivePromoBundling(
        SaveOperationServiceImplTest.STORE_ID, SaveOperationServiceImplTest.ITEM_SKU,
        SaveOperationServiceImplTest.PROMO_BUNDLING_TYPE);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(SaveOperationServiceImplTest.STORE_ID,
        this.item);
  }

  @Test
  public void saveItemsTest() {
    List<Item> items = Collections.singletonList(this.item);
    Mockito.when(objectConverterService.convertToListItemEventModel(items)).thenReturn(new ArrayList<>());
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    this.saveOperationServiceImpl.saveItems(items, null);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.isNull(),
        Mockito.anyList(), anyBoolean());
    Mockito.verify(objectConverterService).convertToListItemEventModel(items);
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveItemsTestEmptyItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.saveOperationServiceImpl.saveItems(new ArrayList<Item>(), null));
  }

  @Test
  public void saveItemsTestNullItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.saveOperationServiceImpl.saveItems(null, null));
  }

  @Test
  public void saveNewlyAddedItemsTest() {
    List<Item> items = Collections.singletonList(this.item);
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    this.saveOperationServiceImpl.saveNewlyAddedItems(items);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService)
        .publishItemDataChangeEvent(itemListArgumentCaptor.capture());
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR), eq(PRODUCT_SKU),
        productAndItemEventModelArgumentCaptor.capture());
    Mockito.verify(objectConverterService).convertToListItemEventModel(items);
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveItemsWithoutUpdatingSolrTest() {
    List<Item> items = Collections.singletonList(this.item);
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    this.saveOperationServiceImpl.saveItemsWithoutUpdatingSolr(items);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveItemsWithoutUpdatingSolrTestEmptyItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.saveOperationServiceImpl.saveItemsWithoutUpdatingSolr(new ArrayList<Item>()));
  }

  @Test
  public void saveItemsWithoutUpdatingSolrTestNullItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.saveOperationServiceImpl.saveItemsWithoutUpdatingSolr(null));
  }


  @Test
  public void saveItemsForSuspensionTest() {
    List<Item> items = Arrays.asList(this.item);
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    this.saveOperationServiceImpl.saveItemsAndClearCacheWithoutUpdatingSolr(items, null, StringUtils.EMPTY);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.isNull(),
        Mockito.anyList(), anyString(), anyBoolean(), eq(Collections.EMPTY_MAP));
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveItemsForSuspensionNonNullTest() {
    List<Item> items = Arrays.asList(this.item);
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    this.saveOperationServiceImpl.saveItemsAndClearCacheWithoutUpdatingSolr(items, itemPickupPoints, StringUtils.EMPTY);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(),
        Mockito.anyList(), anyString(), anyBoolean(), eq(Collections.EMPTY_MAP));
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveItemsForSuspensionTestEmptyItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.saveOperationServiceImpl.saveItemsAndClearCacheWithoutUpdatingSolr(new ArrayList<Item>(), null,StringUtils.EMPTY));
  }

  @Test
  public void saveItemsForSuspensionTestNullItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.saveOperationServiceImpl.saveItemsAndClearCacheWithoutUpdatingSolr(null, null, StringUtils.EMPTY));
  }

  @Test
  public void saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing() {
    List<Item> items = Arrays.asList(this.item);
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    this.saveOperationServiceImpl.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(items);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
  }

  @Test
  public void saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishingException() {
    List<Item> items = Arrays.asList(this.item);
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.saveOperationServiceImpl.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(null));
  }

  @Test
  public void saveItemTest() {
    Mockito.when(this.itemRepository.save(this.item)).thenReturn(this.item);
    Mockito.when(objectConverterService.convertToItemEventModel(item)).thenReturn(new ItemEventModel());
    this.saveOperationServiceImpl.saveItem(this.item, null, new ArrayList<>());
    Mockito.verify(cacheItemHelperService).setItemCacheByStoreIdAndItemSku(item.getStoreId(), item.getItemSku(), item);
    Mockito.verify(this.itemRepository).save(this.item);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.isNull(),
        Mockito.anyList(), anyBoolean());
    Mockito.verify(objectConverterService).convertToItemEventModel(item);
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveItemTestWithNullItem() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.saveOperationServiceImpl.saveItem(null, null, new ArrayList<>()));
  }


  @Test
  public void saveProductAndItemsTest() {
    List<Item> items = Collections.singletonList(this.item);
    Product product = new Product();
    product.setPreOrder(preOrder);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(itemPickupPointRepository.saveAll(itemPickupPoints)).thenReturn(itemPickupPoints);
    this.saveOperationServiceImpl.saveProductAndItems(productAndItems, new ArrayList<>());
    Mockito.verify(this.saveAndPublishService).saveProduct(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertTrue(productArgumentCaptor.getValue().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(PREORDER_TYPE, productArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    Assertions.assertEquals(PREORDER_VALUE, productArgumentCaptor.getValue().getPreOrder().getPreOrderValue());
  }

  @Test
  public void saveProductAndItemsTestWithEmptyItems() {
    List<Item> items = Arrays.asList();
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    this.saveOperationServiceImpl.saveProductAndItems(productAndItems, new ArrayList<>());
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void saveProductAndItemsTestWithNullItems() {
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, null);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    this.saveOperationServiceImpl.saveProductAndItems(productAndItems, new ArrayList<>());
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void saveProductAndItemsTestWithNullProduct() {
    List<Item> items = Collections.singletonList(item);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(null, items);
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    this.saveOperationServiceImpl.saveProductAndItems(productAndItems, new ArrayList<>());
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveProductAndItemsTestWithNullProductAndItem() {
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(null, null);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    this.saveOperationServiceImpl.saveProductAndItems(productAndItems, new ArrayList<>());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }


  @Test
  public void saveProductAndItemsWithoutPublishingEventTest() {
    List<Item> items = Collections.singletonList(this.item);
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(this.itemRepository.saveAll(itemListArgumentCaptor.capture())).thenReturn(items);
    Mockito.when(this.productRepository.save(product)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingEvent(productAndItems);
    Mockito.verify(this.productRepository).save(product);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveProductAndItemsWithoutPublishingEventTestWithEmptyItems() {
    List<Item> items = Arrays.asList();
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(this.productRepository.save(product)).thenReturn(product);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingEvent(productAndItems);
    Mockito.verify(this.productRepository).save(product);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void UPDATE_TO_SOLR() {
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, null);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(this.productRepository.save(product)).thenReturn(product);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingEvent(productAndItems);
    Mockito.verify(this.productRepository).save(product);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
        Mockito.isNull() ,any(ProductAndItemEventModel.class));
  }

  @Test
  public void saveProductAndItemsWithoutPublishingEventTestWithNullProduct() {
    List<Item> items = Collections.singletonList(item);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(null, items);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(itemRepository.saveAll(itemListArgumentCaptor.capture())).thenReturn(items);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingEvent(productAndItems);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void saveProductAndItemsWithoutPublishingEventWithNullProductAndItem() {
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(null, null);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingEvent(productAndItems);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void saveProductAndItemsWithoutPublishingEventNoSolrUpdateTest() {
    List<Item> items = Collections.singletonList(this.item);
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(this.itemRepository.saveAll(itemListArgumentCaptor.capture())).thenReturn(items);
    Mockito.when(this.productRepository.save(product)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingEventAndWithoutReindexing(productAndItems);
    Mockito.verify(this.productRepository).save(product);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveProductAndItemsWithoutPublishingEventNoSolrUpdateTestWithEmptyItems() {
    List<Item> items = Arrays.asList();
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(this.productRepository.save(product)).thenReturn(product);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingEventAndWithoutReindexing(productAndItems);
    Mockito.verify(this.productRepository).save(product);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
  }

  @Test
  public void saveProductAndItemsWithoutPublishingEventNoSolrUpdateTestWithNullItems() {
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, null);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(this.productRepository.save(product)).thenReturn(product);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingEventAndWithoutReindexing(productAndItems);
    Mockito.verify(this.productRepository).save(product);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
  }

  @Test
  public void saveProductAndItemsWithoutPublishingEventNoSolrUpdateTestWithNullProduct() {
    List<Item> items = Collections.singletonList(item);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(null, items);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(itemRepository.saveAll(itemListArgumentCaptor.capture())).thenReturn(items);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingEventAndWithoutReindexing(productAndItems);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveProductAndItemsWithoutPublishingEventNoSolrUpdateWithNullProductAndItem() {
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(null, null);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingEventAndWithoutReindexing(productAndItems);
    Mockito.verify(cacheEvictHelperService, Mockito.times(0)).evictItemCache(anyString(),
      any(Item.class));
  }

  @Test
  public void saveProductTest() {
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    product.setProductCode(SaveOperationServiceImplTest.PRODUCT_CODE);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductEventModel(product)).thenReturn(productEventModel);
    this.saveOperationServiceImpl.saveProduct(product);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(objectConverterService).convertToProductEventModel(product);
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
  }

  @Test
  public void saveProductTestWithNullProduct() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.saveOperationServiceImpl.saveProduct(null));
  }


  @Test
  public void saveProductForSuspensionTest() {
    Product product = new Product();
    product.setProductCode(SaveOperationServiceImplTest.PRODUCT_CODE);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    this.saveOperationServiceImpl.saveProductWithoutUpdatingSolr(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
  }

  @Test
  public void saveProductForSuspensionTestWithNullProduct() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.saveOperationServiceImpl.saveProductWithoutUpdatingSolr(null, new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.saveAndPublishService);
    Mockito.verifyNoMoreInteractions(this.cacheEvictHelperService);
    Mockito.verifyNoMoreInteractions(this.cacheItemHelperService);
    Mockito.verifyNoMoreInteractions(this.itemRepository);
    Mockito.verifyNoMoreInteractions(this.solrIndexService);
    Mockito.verifyNoMoreInteractions(this.deferredSolrReindexItemService);
    Mockito.verifyNoMoreInteractions(this.productL3SolrReindexStatusService);
    Mockito.verifyNoMoreInteractions(this.kafkaProducer);
    Mockito.verifyNoMoreInteractions(this.objectConverterService);
  }

  @Test
  public void updateItemFieldByItemSkuTest() {
    String fieldValue = "field-value";
    String fieldName = "field-name";
    String itemSku = "item-sku";
    String storeId = "store-id";
    Item savedItem = new Item();
    savedItem.setStoreId(storeId);
    Mockito.when(this.itemRepository.updateFieldByItemSku(storeId, itemSku, fieldName, fieldValue))
        .thenReturn(savedItem);
    this.saveOperationServiceImpl.updateItemFieldByItemSku(storeId, itemSku, fieldName, fieldValue);
    Mockito.verify(this.itemRepository).updateFieldByItemSku(storeId, itemSku, fieldName,
        fieldValue);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(storeId, savedItem);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture());
    Assertions.assertEquals(savedItem.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void updateItemFieldByItemSkuAndPickupPoint() {
    String fieldValue = "field-value";
    String fieldName = "field-name";
    String itemSku = "item-sku";
    String storeId = "store-id";
    Item savedItem = new Item();
    savedItem.setStoreId(storeId);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setStoreId(storeId);
    Mockito.when(this.itemPickupPointService.updateFieldByItemSku(storeId, itemSku, fieldName, fieldValue))
        .thenReturn(itemPickupPoint);
    this.saveOperationServiceImpl.updateItemFieldByItemSkuAndPickupPoint(storeId, itemSku, fieldName, fieldValue, savedItem);
    Mockito.verify(this.itemPickupPointService).updateFieldByItemSku(storeId, itemSku, fieldName, fieldValue);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(storeId, savedItem);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(),
       Mockito.anyList(), anyBoolean());
    Assertions.assertEquals(savedItem.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void updateItemPickupPointFieldsByItemSkuTest() {
    String fieldValue = "field-value";
    String fieldName = "field-name";
    String itemSku = "item-sku";
    String storeId = "store-id";
    Item savedItem = new Item();
    savedItem.setStoreId(storeId);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setStoreId(storeId);
    Mockito.when(this.itemPickupPointService.updateFieldsByItemSku(storeId, itemSku, fieldName, fieldValue))
        .thenReturn(Arrays.asList(itemPickupPoint));
    this.saveOperationServiceImpl.updateItemPickupPointFieldsByItemSku(storeId, itemSku, fieldName, fieldValue, savedItem);
    Mockito.verify(this.itemPickupPointService).updateFieldsByItemSku(storeId, itemSku, fieldName, fieldValue);
    Mockito.verify(this.cacheEvictHelperService).evictItemPickupPointCache(storeId, Arrays.asList(savedItem), Arrays.asList(itemPickupPoint));
    Mockito.verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(Collections.singletonList(itemPickupPoint),
      new ArrayList<>(), Collections.EMPTY_MAP);
  }

  @Test
  public void updateItemPickupPointFieldsByItemSkuBuyableScheduleTest() {
    String fieldName = ProductFieldNames.ITEM_PICKUP_POINT_DISPLAYABLE_SCHEDULES;
    Item savedItem = new Item();
    savedItem.setStoreId(STORE_ID);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setStoreId(STORE_ID);
    Mockito.when(this.itemPickupPointService.updateFieldsByItemSku(STORE_ID, ITEM_SKU, fieldName,
        true))
      .thenReturn(Arrays.asList(itemPickupPoint));
    this.saveOperationServiceImpl.updateItemPickupPointFieldsByItemSku(STORE_ID, ITEM_SKU, fieldName,
      true, savedItem);
    Mockito.verify(this.itemPickupPointService).updateFieldsByItemSku(STORE_ID, ITEM_SKU, fieldName,
      true);
    Mockito.verify(this.cacheEvictHelperService).evictItemPickupPointCache(STORE_ID,
      Arrays.asList(savedItem), Arrays.asList(itemPickupPoint));
    Mockito.verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(Collections.singletonList(itemPickupPoint),
      Collections.singletonList(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE),
      Collections.EMPTY_MAP);
  }

  @Test
  public void updateItemPickupPointFieldsByItemSkuDiscoverableScheduleTest() {
    String fieldName = ProductFieldNames.ITEM_PICKUP_POINT_DISCOVERABLE_SCHEDULES;
    Item savedItem = new Item();
    savedItem.setStoreId(STORE_ID);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setStoreId(STORE_ID);
    Mockito.when(this.itemPickupPointService.updateFieldsByItemSku(STORE_ID, ITEM_SKU, fieldName,
        true))
      .thenReturn(Arrays.asList(itemPickupPoint));
    this.saveOperationServiceImpl.updateItemPickupPointFieldsByItemSku(STORE_ID, ITEM_SKU, fieldName,
      true, savedItem);
    Mockito.verify(this.itemPickupPointService).updateFieldsByItemSku(STORE_ID, ITEM_SKU, fieldName,
      true);
    Mockito.verify(this.cacheEvictHelperService).evictItemPickupPointCache(STORE_ID,
      Arrays.asList(savedItem), Arrays.asList(itemPickupPoint));
    Mockito.verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(Collections.singletonList(itemPickupPoint),
      Collections.singletonList(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE),
      Collections.EMPTY_MAP);
  }

  @Test
  public void updateMerchantSkuForItemPickupPointsTest() {
    Mockito.when(itemPickupPointService
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(ITEM_SKU)))
        .thenReturn(itemPickupPoints);
    Mockito.when(itemPickupPointService.saveItemPickupPoint(itemPickupPoints)).thenReturn(itemPickupPoints);
    saveOperationServiceImpl.updateMerchantSkuForItemPickupPoints(STORE_ID, ITEM_SKU, item, MERCHANT_SKU);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(ITEM_SKU));
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(itemPickupPoints);
    Mockito.verify(this.cacheEvictHelperService)
        .evictItemPickupPointCache(STORE_ID, Collections.singletonList(item), itemPickupPoints);
    Mockito.verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(itemPickupPoints, new ArrayList<>(),
      Collections.EMPTY_MAP);
  }

  @Test
  public void updateItemFieldByItemSkusTest() {
    String fieldName = "field-name";
    String fieldValue = "field-value";

    Item item = new Item();
    item.setStoreId(STORE_ID);

    Mockito.when(
        this.itemRepository.updateFieldByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU), fieldName,
            fieldValue)).thenReturn(Collections.singletonList(item));

    this.saveOperationServiceImpl.updateItemFieldByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU), fieldName,
        fieldValue);

    Mockito.verify(this.itemRepository)
        .updateFieldByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU), fieldName, fieldValue);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(STORE_ID, item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());

  }

  @Test
  public void saveAndEvictItemsTest() {
    List<Item> items = Collections.singletonList(item);
    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(objectConverterService.convertToListItemEventModel(items)).thenReturn(new ArrayList<>());
    this.saveOperationServiceImpl.saveAndEvictItems(items);
    Mockito.verify(objectConverterService).convertToListItemEventModel(items);
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
  }

  @Test
  public void saveAndEvictItemTest() {
    Mockito.when(this.itemRepository.save(item)).thenReturn(item);
    Mockito.when(objectConverterService.convertToItemEventModel(item)).thenReturn(new ItemEventModel());
    this.saveOperationServiceImpl.saveAndEvictItem(item);
    Mockito.verify(this.itemRepository).save(item);
    Mockito.verify(this.cacheEvictHelperService).evictItemCache(this.item.getStoreId(), this.item);
    Mockito.verify(objectConverterService).convertToItemEventModel(item);
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
  }

  @Test
  public void saveAndEvictItemsTest_WhenNullItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.saveOperationServiceImpl.saveAndEvictItems(null));
  }

  @Test
  public void saveAndEvictItemTest_WhenNullItem() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.saveOperationServiceImpl.saveAndEvictItem(null));
  }

  @Test
  public void updateAndEvictOff2OnItemCountByProductSkuTest() {
    Product product = new Product();
    Mockito.when(saveAndPublishService.updateOff2OnItemCountIncrement(STORE_ID,
        PRODUCT_SKU, true, 10)).thenReturn(product);
    saveOperationServiceImpl.updateAndEvictOff2OnItemCountByProductSku(STORE_ID, PRODUCT_SKU, true, 10);
    Mockito.verify(saveAndPublishService).updateOff2OnItemCountIncrement(STORE_ID,
        PRODUCT_SKU, true, 10);
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
  }

  @Test
  public void updateItemDGLevelTest() {
    Set<String> itemSkus  = Collections.singleton(ITEM_SKU);
    Mockito.when(itemRepository.updateDangerousGoodsByItemSku(STORE_ID, itemSkus, 1))
        .thenReturn(Collections.singletonList(item));
    saveOperationServiceImpl.updateItemDGLevel(STORE_ID, itemSkus, 1);
    Mockito.verify(itemRepository).updateDangerousGoodsByItemSku(STORE_ID, itemSkus, 1);
    Mockito.verify(solrIndexService).updateSolrAndClearCache(STORE_ID, item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void updateOff2OnChannelActiveByItemSkuTest() {
    Mockito.when(saveAndPublishService.updateItemOff2OnChannelActiveByItemSku(STORE_ID, ITEM_SKU, true))
        .thenReturn(item);
    Mockito.when(saveAndPublishService.updateOff2OnItemCountIncrement(STORE_ID, PRODUCT_SKU, true, 1))
        .thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    saveOperationServiceImpl.updateOff2OnChannelActiveByItemSku(STORE_ID, ITEM_SKU, true);
    Mockito.verify(saveAndPublishService).updateItemOff2OnChannelActiveByItemSku(STORE_ID, ITEM_SKU, true);
    Mockito.verify(saveAndPublishService).updateOff2OnItemCountIncrement(STORE_ID, PRODUCT_SKU, true, 1);
    Mockito.verify(cacheEvictHelperService).evictItemData(STORE_ID, item);
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void updateOff2OnChannelActiveByItemSkuTestWithNullItem() {
    Mockito.when(
            saveAndPublishService.updateItemOff2OnChannelActiveByItemSku(STORE_ID, ITEM_SKU, true))
        .thenReturn(null);
    saveOperationServiceImpl.updateOff2OnChannelActiveByItemSku(STORE_ID, ITEM_SKU, true);
    Mockito.verify(saveAndPublishService)
        .updateItemOff2OnChannelActiveByItemSku(STORE_ID, ITEM_SKU, true);
  }

  @Test
  public void updateOff2OnChannelActiveByProductSkuTest() {
    List<Item> items = Collections.singletonList(item);
    Mockito.when(saveAndPublishService.updateItemOff2OnChannelActiveByProductSku(STORE_ID, PRODUCT_SKU, true))
        .thenReturn(items);
    Mockito.when(saveAndPublishService.updateOff2OnItemCountIncrement(STORE_ID, PRODUCT_SKU, true, 1))
        .thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    saveOperationServiceImpl.updateOff2OnChannelActiveByProductSku(STORE_ID, PRODUCT_SKU, true);
    Mockito.verify(saveAndPublishService).updateItemOff2OnChannelActiveByProductSku(STORE_ID, PRODUCT_SKU, true);
    Mockito.verify(saveAndPublishService).updateOff2OnItemCountIncrement(STORE_ID, PRODUCT_SKU, true, 1);
    Mockito.verify(cacheEvictHelperService).evictItemData(STORE_ID, item);
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void saveItemsForBulkTest() {
    Mockito.when(
            deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(),
                Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    List<Item> items = Collections.singletonList(this.item);
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    this.saveOperationServiceImpl.saveItemsWithDeferredReindexing(items, null);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.isNull(),
        Mockito.anyList());
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.isNull(),
        Mockito.anyList(), anyBoolean());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
    Mockito.verify(deferredSolrReindexItemService)
        .save(deferredSolrReindexItemsArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(),
        deferredSolrReindexItemsArgumentCaptor.getValue().get(0).getItemSku());
    Mockito.verify(productL3SolrReindexStatusService).insertProductSkusToReindexStatusCollection(Mockito.isNull(),
        Mockito.anyList());

  }

  @Test
  public void testDeferredReindexOfflineItems() {
    DeferredSolrReindexItem deferredSolrReindexItem = new DeferredSolrReindexItem();
    deferredSolrReindexItem.setItemSku(ITEM_SKU);
    Item item1 = new Item();
    item1.setStoreId(STORE_ID);
    item1.setItemSku(ITEM_SKU);
    Mockito.when(
            deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(),
                Mockito.anyList()))
        .thenReturn(new ArrayList<>(Arrays.asList(deferredSolrReindexItem)));
    this.saveOperationServiceImpl.deferredReindexItems(item.getStoreId(), Arrays.asList(item, item1), false,
        ReindexType.OFFLINE_ITEM_REINDEX, false);
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.isNull(),
        Mockito.anyList());
    Mockito.verify(deferredSolrReindexItemService)
        .save(deferredSolrReindexItemsArgumentCaptor.capture());
    Mockito.verify(cacheEvictHelperService).evictItemData(Mockito.anyString(), any(Item.class));
    Mockito.verify(cacheEvictHelperService).evictItemData(item.getStoreId(), item);
    Assertions.assertEquals(item.getItemSku(),
        deferredSolrReindexItemsArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(ReindexType.OFFLINE_ITEM_REINDEX.getDescription(),
        deferredSolrReindexItemsArgumentCaptor.getValue().get(0).getReindexType());
    Assertions.assertEquals(item1.getItemSku(),
        deferredSolrReindexItemsArgumentCaptor.getValue().get(1).getItemSku());
  }

  @Test
  public void testDeferredReindexOffline_withUpdateL3Items() {
    DeferredSolrReindexItem deferredSolrReindexItem = new DeferredSolrReindexItem();
    deferredSolrReindexItem.setItemSku(ITEM_SKU);
    Item item1 = new Item();
    item1.setStoreId(STORE_ID);
    item1.setItemSku(ITEM_SKU);
    Mockito.when(
        deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(),
          Mockito.anyList()))
      .thenReturn(new ArrayList<>(Arrays.asList(deferredSolrReindexItem)));
    this.saveOperationServiceImpl.deferredReindexItems(item.getStoreId(), Arrays.asList(item, item1), false,
      ReindexType.OFFLINE_ITEM_REINDEX, true);
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.isNull(),
      Mockito.anyList());
    Mockito.verify(deferredSolrReindexItemService)
      .save(deferredSolrReindexItemsArgumentCaptor.capture());
    Mockito.verify(cacheEvictHelperService).evictItemData(Mockito.anyString(), any(Item.class));
    Mockito.verify(productL3SolrReindexStatusService)
      .insertProductSkusToReindexStatusCollection(Mockito.isNull(),
        Mockito.anyList());
    Mockito.verify(cacheEvictHelperService).evictItemData(item.getStoreId(), item);
    Assertions.assertEquals(item.getItemSku(),
      deferredSolrReindexItemsArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(ReindexType.OFFLINE_ITEM_REINDEX.getDescription(),
      deferredSolrReindexItemsArgumentCaptor.getValue().get(0).getReindexType());
    Assertions.assertEquals(item1.getItemSku(),
      deferredSolrReindexItemsArgumentCaptor.getValue().get(1).getItemSku());
  }

  @Test
  public void testDeferredReindexOffline_withoutUpdateL3Items() {
    DeferredSolrReindexItem deferredSolrReindexItem = new DeferredSolrReindexItem();
    deferredSolrReindexItem.setItemSku(ITEM_SKU);
    Item item1 = new Item();
    item1.setStoreId(STORE_ID);
    item1.setItemSku(ITEM_SKU);
    Mockito.when(
        deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(),
          Mockito.anyList()))
      .thenReturn(new ArrayList<>(Arrays.asList(deferredSolrReindexItem)));
    this.saveOperationServiceImpl.deferredReindexItems(item.getStoreId(), Arrays.asList(item, item1), false,
      ReindexType.ITEM_REINDEX, false);
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.isNull(),
      Mockito.anyList());
    Mockito.verify(deferredSolrReindexItemService)
      .save(deferredSolrReindexItemsArgumentCaptor.capture());
    Mockito.verify(cacheEvictHelperService).evictItemData(Mockito.anyString(), any(Item.class));
    Mockito.verify(productL3SolrReindexStatusService)
      .insertProductSkusToReindexStatusCollection(Mockito.isNull(),
        Mockito.anyList());
    Mockito.verify(cacheEvictHelperService).evictItemData(item.getStoreId(), item);
    Assertions.assertEquals(item.getItemSku(),
      deferredSolrReindexItemsArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(ReindexType.ITEM_REINDEX.getDescription(),
      deferredSolrReindexItemsArgumentCaptor.getValue().get(0).getReindexType());
    Assertions.assertEquals(item1.getItemSku(),
      deferredSolrReindexItemsArgumentCaptor.getValue().get(1).getItemSku());
  }

  @Test
  public void testDeferredReindexOfflineItemsEmptyList() {
    DeferredSolrReindexItem deferredSolrReindexItem = new DeferredSolrReindexItem();
    deferredSolrReindexItem.setItemSku(ITEM_SKU);
    Item item1 = new Item();
    item1.setStoreId(STORE_ID);
    item1.setItemSku(ITEM_SKU);
    Mockito.when(
            deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(),
                Mockito.anyList()))
        .thenReturn(new ArrayList<>(Arrays.asList(deferredSolrReindexItem)));
    this.saveOperationServiceImpl.deferredReindexItems(item.getStoreId(), new ArrayList<>(), false,
        ReindexType.OFFLINE_ITEM_REINDEX, false);
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.isNull(),
        Mockito.anyList());
  }

  @Test
  public void saveProductAndItemsWithoutUpdatingSolrTest() {
    List<Item> items = Collections.singletonList(this.item);
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(objectConverterService.convertToListItemEventModel(items)).thenReturn(new ArrayList<>());
    this.saveOperationServiceImpl.saveItems(items, null);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.isNull(),
       Mockito.anyList(), anyBoolean());
    Mockito.verify(objectConverterService).convertToListItemEventModel(items);
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveProductAndItemsWithoutUpdatingSolrTestWithEmptyItems() {
    List<Item> items = Arrays.asList();
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutUpdatingSolr(productAndItems);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
  }

  @Test
  public void saveProductAndItemsWithoutUpdatingSolrTest_withItems() {
    List<Item> items = Arrays.asList(item);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.doNothing().when(this.cacheEvictHelperService).evictItemCache(this.item.getStoreId(), item);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutUpdatingSolr(productAndItems);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PRODUCT_CODE, itemListArgumentCaptor.getValue().get(0).getMasterDataItem().getProductCode());
  }

  @Test
  public void saveProductAndItemsWithoutUpdatingSolrMasterDataItemNullTest() {
    item.setMasterDataItem(null);
    List<Item> items = Arrays.asList(item);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.doNothing().when(this.cacheEvictHelperService).evictItemCache(this.item.getStoreId(), item);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutUpdatingSolr(productAndItems);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveProductAndItemsWithoutUpdatingSolrTestWithNullItems() {
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, null);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutUpdatingSolr(productAndItems);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
  }

  @Test
  public void saveProductAndItemsDeferredReindexTestWithEmptyItems() {
    List<Item> items = Arrays.asList();
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    this.saveOperationServiceImpl.saveProductAndItemsWithDeferredReindex(productAndItems);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
  }

  @Test
  public void saveProductAndItemsDeferredReindexTest_withItems() {
    List<Item> items = Arrays.asList(item);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.doNothing().when(this.cacheEvictHelperService).evictItemCache(this.item.getStoreId(), item);
    Mockito.when(
            deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(),
                Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    this.saveOperationServiceImpl.saveProductAndItemsWithDeferredReindex(productAndItems);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PRODUCT_CODE, itemListArgumentCaptor.getValue().get(0).getMasterDataItem().getProductCode());
    Mockito.verify(productL3SolrReindexStatusService).insertProductSkusToReindexStatusCollection(Mockito.isNull(),
        Mockito.anyList());
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.isNull(),
        Mockito.anyList());
    Mockito.verify(deferredSolrReindexItemService)
        .save(deferredSolrReindexItemsArgumentCaptor.capture());
    Assertions.assertEquals(item.getItemSku(),
        deferredSolrReindexItemsArgumentCaptor.getValue().get(0).getItemSku());

  }

  @Test
  public void saveProductAndItemsDeferredReindexTestWithNullItems() {
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, null);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    this.saveOperationServiceImpl.saveProductAndItemsWithDeferredReindex(productAndItems);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
  }

  @Test
  public void saveItemsAndClearCacheWithoutUpdatingSolr() {
    List<Item> items = Collections.singletonList(this.item);
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(objectConverterService.convertToListItemEventModel(items)).thenReturn(new ArrayList<>());
    this.saveOperationServiceImpl.saveItems(items, null);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(objectConverterService).convertToListItemEventModel(items);
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModelArgumentCaptor.capture());
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.isNull(),
        Mockito.anyList(), anyBoolean());
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveItemsAndClearCacheWithoutUpdatingSolrTestEmptyItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.saveOperationServiceImpl.saveItemsAndClearCacheWithoutUpdatingSolr(new ArrayList<>(), null, StringUtils.EMPTY));
  }

  @Test
  public void saveItemsAndClearCacheWithoutUpdatingSolrNullItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.saveOperationServiceImpl.saveItemsAndClearCacheWithoutUpdatingSolr(null, null, StringUtils.EMPTY));
  }

  @Test
  public void saveItemWithoutUpdatingSolrTest() {
    Mockito.when(this.itemRepository.save(this.item)).thenReturn(this.item);
    this.saveOperationServiceImpl.saveItemWithoutUpdatingSolr(this.item, null, false,StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.itemRepository).save(this.item);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.isNull(),
       Mockito.anyList(), anyString(), anyBoolean(), eq(Collections.EMPTY_MAP));
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }
  @Test
  public void saveItemWithoutUpdatingSolrRejectedTest() {
    Mockito.when(this.itemRepository.save(this.item)).thenReturn(this.item);
    this.saveOperationServiceImpl.saveItemWithoutUpdatingSolr(this.item, null, true,StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.itemRepository).save(this.item);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.isNull(),
       Mockito.anyList(),anyString(), anyBoolean(), eq(Collections.EMPTY_MAP));
    Assertions.assertEquals(item.getItemSku(), itemListArgumentCaptor.getValue().get(0).getItemSku());
  }

  @Test
  public void saveProductAndItemsWithoutPublishingItemChangeTest() {
    List<Item> items = Collections.singletonList(this.item);
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingItemChange(productAndItems);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void saveProductAndItemsWithoutPublishingItemChangeTestWithEmptyItems() {
    List<Item> items = Arrays.asList();
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingItemChange(productAndItems);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void saveProductAndItemsWithoutPublishingItemChangeTestWithNullItems() {
    Product product = new Product();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, null);
    Mockito.when(this.saveAndPublishService.saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingItemChange(productAndItems);
    Mockito.verify(this.saveAndPublishService).saveProduct(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void saveProductAndItemsWithoutPublishingItemChangeTestWithNullProduct() {
    List<Item> items = Collections.singletonList(item);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(null, items);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(itemRepository.saveAll(items)).thenReturn(items);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingItemChange(productAndItems);
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void saveProductAndItemsWithoutPublishingItemChangeTestWithNullProductAndItem() {
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(null, null);
    this.saveOperationServiceImpl.saveProductAndItemsWithoutPublishingItemChange(productAndItems);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkusTest() throws Exception{
    Mockito.when(saveAndPublishService
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, true,
            Constants.DEFAULT_UPDATED_BY)).thenReturn(Collections.singletonList(item));
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    product.setMasterDataProduct(masterDataProduct);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(saveAndPublishService
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 1, Constants.DEFAULT_UPDATED_BY))
        .thenReturn(product);
    Mockito.when(productService
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    ProductAndItemsVO productAndItemsVO = saveOperationServiceImpl
        .changeOff2OnChannelActiveByProductSkus(STORE_ID, PRODUCT_SKU, true, Constants.DEFAULT_UPDATED_BY,
            new ArrayList<>(), Constants.DEFAULT_REQUEST_ID);
    Mockito.verify(saveAndPublishService)
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, true,
            Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(saveAndPublishService)
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 1, Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(cacheEvictHelperService).evictItemData(STORE_ID, item);
    Mockito.verify(saveAndPublishService).publishProduct(product, new ArrayList<>());
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveAndPublishService).publishListOfItemsWithItemChangeType(Collections.singletonList(item),
        Collections.singletonList(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE));
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(product, productAndItemsVO.getProduct());
    Assertions.assertEquals(item, productAndItemsVO.getItems().get(0));
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkusFalseTest() throws Exception{
    Mockito.when(saveAndPublishService
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, false,
            Constants.DEFAULT_UPDATED_BY)).thenReturn(Collections.singletonList(item));
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    product.setMasterDataProduct(masterDataProduct);
    Mockito.when(saveAndPublishService
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, false, 1, Constants.DEFAULT_UPDATED_BY))
        .thenReturn(product);
    product.setOff2OnChannelActive(true);
    Mockito.when(productService
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    ProductAndItemsVO productAndItemsVO = saveOperationServiceImpl
        .changeOff2OnChannelActiveByProductSkus(STORE_ID, PRODUCT_SKU, false, Constants.DEFAULT_UPDATED_BY,
            new ArrayList<>(), Constants.DEFAULT_REQUEST_ID);
    Mockito.verify(saveAndPublishService)
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, false,
            Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(saveAndPublishService)
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, false, 1, Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(cacheEvictHelperService).evictItemData(STORE_ID, item);
    Mockito.verify(saveAndPublishService).publishProduct(product, new ArrayList<>());
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveAndPublishService).publishListOfItemsWithItemChangeType(Collections.singletonList(item),
        Collections.singletonList(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE));
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(product, productAndItemsVO.getProduct());
    Assertions.assertEquals(item, productAndItemsVO.getItems().get(0));
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkusWithMasterDataNullProductNameTest() throws Exception{
    Mockito.when(saveAndPublishService
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, true,
            Constants.DEFAULT_UPDATED_BY)).thenReturn(Collections.singletonList(item));
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    product.setMasterDataProduct(masterDataProduct);
    Mockito.when(saveAndPublishService
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 1, Constants.DEFAULT_UPDATED_BY))
        .thenReturn(product);
    Mockito.when(productService
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(masterDataCacheService.getMasterDataProductAndItems(Constants.DEFAULT_UPDATED_BY,
        Constants.DEFAULT_REQUEST_ID, PRODUCT_CODE, Boolean.FALSE)).thenReturn(masterDataProductAndItemsVO);
    ProductAndItemsVO productAndItemsVO = saveOperationServiceImpl
        .changeOff2OnChannelActiveByProductSkus(STORE_ID, PRODUCT_SKU, true, Constants.DEFAULT_UPDATED_BY,
            new ArrayList<>(), Constants.DEFAULT_REQUEST_ID);
    Mockito.verify(saveAndPublishService)
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, true,
            Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(masterDataCacheService).getMasterDataProductAndItems(Constants.DEFAULT_UPDATED_BY,
        Constants.DEFAULT_REQUEST_ID, PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(saveAndPublishService)
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 1, Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU);
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(cacheEvictHelperService).evictItemData(STORE_ID, item);
    Mockito.verify(saveAndPublishService).publishProduct(product, new ArrayList<>());
    Mockito.verify(saveAndPublishService).publishListOfItemsWithItemChangeType(Collections.singletonList(item),
        Collections.singletonList(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE));
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(product, productAndItemsVO.getProduct());
    Assertions.assertEquals(item, productAndItemsVO.getItems().get(0));
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkusWithMasterDataNullTest() throws Exception{
    Mockito.when(saveAndPublishService
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, true,
            Constants.DEFAULT_UPDATED_BY)).thenReturn(Collections.singletonList(item));
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    product.setMasterDataProduct(null);
    Mockito.when(saveAndPublishService
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 1, Constants.DEFAULT_UPDATED_BY))
        .thenReturn(product);
    Mockito.when(productService
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(masterDataCacheService.getMasterDataProductAndItems(Constants.DEFAULT_UPDATED_BY,
        Constants.DEFAULT_REQUEST_ID, PRODUCT_CODE, Boolean.FALSE)).thenReturn(masterDataProductAndItemsVO);
    ProductAndItemsVO productAndItemsVO = saveOperationServiceImpl
        .changeOff2OnChannelActiveByProductSkus(STORE_ID, PRODUCT_SKU, true, Constants.DEFAULT_UPDATED_BY,
            new ArrayList<>(), Constants.DEFAULT_REQUEST_ID);
    Mockito.verify(saveAndPublishService)
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, true,
            Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(masterDataCacheService).getMasterDataProductAndItems(Constants.DEFAULT_UPDATED_BY,
        Constants.DEFAULT_REQUEST_ID, PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(saveAndPublishService)
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 1, Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU);
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(cacheEvictHelperService).evictItemData(STORE_ID, item);
    Mockito.verify(saveAndPublishService).publishProduct(product, new ArrayList<>());
    Mockito.verify(saveAndPublishService).publishListOfItemsWithItemChangeType(Collections.singletonList(item),
        Collections.singletonList(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE));
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(product, productAndItemsVO.getProduct());
    Assertions.assertEquals(item, productAndItemsVO.getItems().get(0));
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkusMasterDateNullTest() throws Exception{
    this.item.setMasterDataItem(null);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(saveAndPublishService
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, true,
            Constants.DEFAULT_UPDATED_BY)).thenReturn(Collections.singletonList(item));
    Mockito.when(saveAndPublishService
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 1, Constants.DEFAULT_UPDATED_BY))
        .thenReturn(product);
    Mockito.when(productService
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    product.setMasterDataProduct(masterDataProduct);
    Mockito.when(masterDataCacheService.getMasterDataProductAndItems(Constants.DEFAULT_UPDATED_BY,
        Constants.DEFAULT_REQUEST_ID, PRODUCT_CODE, Boolean.FALSE)).thenReturn(masterDataProductAndItemsVO);
    ProductAndItemsVO productAndItemsVO = saveOperationServiceImpl
        .changeOff2OnChannelActiveByProductSkus(STORE_ID, PRODUCT_SKU, true, Constants.DEFAULT_UPDATED_BY,
            new ArrayList<>(), Constants.DEFAULT_REQUEST_ID);
    Mockito.verify(saveAndPublishService)
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, true,
            Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(saveAndPublishService)
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 1, Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU);
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(cacheEvictHelperService).evictItemData(STORE_ID, item);
    Mockito.verify(saveAndPublishService).publishProduct(product, new ArrayList<>());
    Mockito.verify(saveAndPublishService).publishListOfItemsWithItemChangeType(Collections.singletonList(item),
        Collections.singletonList(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE));
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(product, productAndItemsVO.getProduct());
    Assertions.assertEquals(item, productAndItemsVO.getItems().get(0));
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkusO2ofalse() throws Exception{
    product.setOff2OnChannelActive(false);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(saveAndPublishService
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, true,
            Constants.DEFAULT_UPDATED_BY)).thenReturn(Collections.singletonList(item));
    Mockito.when(saveAndPublishService
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 1, Constants.DEFAULT_UPDATED_BY))
        .thenReturn(product);
    Mockito.when(productService
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    product.setMasterDataProduct(masterDataProduct);
    Mockito.when(masterDataCacheService.getMasterDataProductAndItems(Constants.DEFAULT_UPDATED_BY,
        Constants.DEFAULT_REQUEST_ID, PRODUCT_CODE, Boolean.FALSE)).thenReturn(masterDataProductAndItemsVO);
    ProductAndItemsVO productAndItemsVO = saveOperationServiceImpl
        .changeOff2OnChannelActiveByProductSkus(STORE_ID, PRODUCT_SKU, true, Constants.DEFAULT_UPDATED_BY,
            new ArrayList<>(), Constants.DEFAULT_REQUEST_ID);
    Mockito.verify(saveAndPublishService)
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, true,
            Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(saveAndPublishService)
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 1, Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU);
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(cacheEvictHelperService).evictItemData(STORE_ID, item);
    Mockito.verify(saveAndPublishService).publishProduct(product, new ArrayList<>());
    Mockito.verify(saveAndPublishService).publishListOfItemsWithItemChangeType(Collections.singletonList(item),
        Collections.singletonList(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE));
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(product, productAndItemsVO.getProduct());
    Assertions.assertEquals(item, productAndItemsVO.getItems().get(0));
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkusItemEmptyTest() throws Exception{
    Mockito.when(saveAndPublishService
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 0, Constants.DEFAULT_UPDATED_BY))
        .thenReturn(product);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    product.setMasterDataProduct(masterDataProduct);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(productService
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU))
        .thenReturn(new Product());
    ProductAndItemsVO productAndItemsVO = saveOperationServiceImpl
        .changeOff2OnChannelActiveByProductSkus(STORE_ID, PRODUCT_SKU, true, Constants.DEFAULT_UPDATED_BY,
            new ArrayList<>(), Constants.DEFAULT_REQUEST_ID);
    Mockito.verify(saveAndPublishService)
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, true,
            Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(saveAndPublishService)
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 0, Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_SKU);
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(saveAndPublishService).publishProduct(product, new ArrayList<>());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(null),productAndItemEventModelArgumentCaptor.capture());
    Assertions.assertEquals(product, productAndItemsVO.getProduct());
    Assertions.assertTrue(CollectionUtils.isEmpty(productAndItemsVO.getItems()));
  }

  @Test
  public void saveProductAndItemsAndPickupPointMultiPickupPointEnabledTest() {
    ReflectionTestUtils.setField(saveOperationServiceImpl, "populateNewDataFlagInItemChangeEventForCreation", true);
    ReflectionTestUtils.setField(saveOperationServiceImpl, "clientIdForVideoCompression", VIDEO_URL_CLIENT_ID);
    ReflectionTestUtils.setField(saveOperationServiceImpl, "distributionSellerList", Set.of(MERCHANT_CODE));
    List<Item> items = Collections.singletonList(this.item);
    itemPickupPoints.get(0).setPickupPointCode(PICKUP_POINT);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    product.setPreOrder(preOrder);
    Video video = new Video();
    video.setSourceUrl(VIDEO_URL);
    video.setVideoId(VIDEO_URL);
    product.setVideo(video);
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    productAndItemEventModel.setProductSku(PRODUCT_SKU);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemVo.class), eq(Item.class))).thenReturn(items.get(0));
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ProductVo.class), eq(Product.class))).thenReturn(product);
    itemPickupPoints.get(0).setForceReview(true);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemPickupPointVo.class), eq(ItemPickupPoint.class))).thenReturn(itemPickupPoints.get(0));
    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(this.saveAndPublishService.saveProductAndSKipPublishForMfdTrueProducts(product)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(itemPickupPointRepository.saveAll(itemPickupPoints)).thenReturn(itemPickupPoints);
    Mockito.doNothing().when(saveAndPublishService)
        .publishItemPickupPointDataChangeEvent((Mockito.anyList()), Mockito.anyList(),
          eq(Collections.EMPTY_MAP));
    Mockito.doNothing().when(saveAndPublishService)
        .publishItemDataChangeEvent((Mockito.anyList()));
    Mockito.doReturn(new ItemPickupPointDataChangeEventModel()).when(saveAndPublishService)
        .publishItemPickupPointDataChangeEventForAGP(Mockito.any(ItemPickupPoint.class));
    Mockito.doNothing().when(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());
    productItemsVo.getItemVoList().get(0).getItemPickupPointVoList().get(0).setForceReview(true);
    productItemsVo.getItemVoList().get(0).getItemPickupPointVoList().get(0).setDistribution(true);
    this.saveOperationServiceImpl.saveProductAndItemsAndPickupPoint(productItemsVo.getProductVo(), productItemsVo.getItemVoList());

    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ItemVo.class), eq(Item.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ProductVo.class), eq(Product.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ItemPickupPointVo.class), eq(ItemPickupPoint.class));
    Mockito.verify(this.saveAndPublishService).saveProductAndSKipPublishForMfdTrueProducts(productArgumentCaptor.capture());
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(this.cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(objectConverterService)
        .convertToProductAndItemEventModel(any(ProductAndItemsVO.class), Mockito.anyList());
    Mockito.verify(this.kafkaProducer)
      .send(eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.isNull(), Mockito.isNull());
    Mockito.verify(itemPickupPointRepository).saveAll(itemPickupPointListCaptor.capture());
    Mockito.verify(
        saveAndPublishService).publishItemPickupPointDataChangeEvent((Mockito.anyList()),
        Mockito.anyList(),eq( Collections.EMPTY_MAP));
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent(itemListArgumentCaptor.capture());
    Mockito.verify(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());
    Mockito.verify(saveAndPublishService).publishProductBundleOneToOneMappingEvent(Mockito.anyCollection());
    Mockito.verify(saveAndPublishService).publishVideoCompressionEvent(
        VideoCompressionEventModel.builder().videoId(VIDEO_URL).clientId(VIDEO_URL_CLIENT_ID).ownerId(MERCHANT_CODE)
            .additionalFields(Map.of(Constants.PRODUCT_CODE, PRODUCT_CODE, Constants.PRODUCT_SKU, PRODUCT_SKU))
            .build());
    Assertions.assertTrue(productArgumentCaptor.getValue().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(PICKUP_POINT, productArgumentCaptor.getValue().getPickupPointCodes().stream().findFirst().get());
    Assertions.assertEquals(PREORDER_TYPE, productArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    Assertions.assertEquals(PREORDER_VALUE, productArgumentCaptor.getValue().getPreOrder().getPreOrderValue());
    Assertions.assertTrue(itemPickupPointListCaptor.getAllValues().get(0).get(0).isForceReview());
    Assertions.assertTrue(itemListArgumentCaptor.getValue().stream().allMatch(Item::isNewData));
  }

  @Test
  public void saveProductAndItemsAndPickupPointMultiPickupPointEnabledTestwithSkipEventForTakeDownTrue() {
    ReflectionTestUtils.setField(saveOperationServiceImpl, "skipEventPublishForTakeDownProduct", true);
    List<Item> items = Collections.singletonList(this.item);
    itemPickupPoints.get(0).setPickupPointCode(PICKUP_POINT);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    product.setPreOrder(preOrder);
    product.setProductSku(PRODUCT_SKU);
    product.setMarkForDelete(true);
    productAndItemEventModel.setProductSku(PRODUCT_SKU);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));

    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemVo.class), eq(Item.class))).thenReturn(items.get(0));
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ProductVo.class), eq(Product.class))).thenReturn(product);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemPickupPointVo.class), eq(ItemPickupPoint.class))).thenReturn(itemPickupPoints.get(0));

    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(this.saveAndPublishService.saveProductAndSKipPublishForMfdTrueProducts(product)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(itemPickupPointRepository.saveAll(itemPickupPoints)).thenReturn(itemPickupPoints);
    Mockito.doNothing().when(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());

    this.saveOperationServiceImpl.saveProductAndItemsAndPickupPoint(productItemsVo.getProductVo(),
        productItemsVo.getItemVoList());

    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ItemVo.class), eq(Item.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ProductVo.class), eq(Product.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ItemPickupPointVo.class), eq(ItemPickupPoint.class));
    Mockito.verify(this.saveAndPublishService).saveProductAndSKipPublishForMfdTrueProducts(productArgumentCaptor.capture());
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(this.cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(objectConverterService)
        .convertToProductAndItemEventModel(any(ProductAndItemsVO.class), Mockito.anyList());
    Mockito.verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.isNull(), Mockito.isNull());
    Mockito.verify(itemPickupPointRepository).saveAll(itemPickupPoints);
    Mockito.verify(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());
    Mockito.verify(saveAndPublishService).publishProductBundleOneToOneMappingEvent(Mockito.anyCollection());
    Assertions.assertTrue(productArgumentCaptor.getValue().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(PICKUP_POINT, productArgumentCaptor.getValue().getPickupPointCodes().stream().findFirst().get());
    Assertions.assertEquals(PREORDER_TYPE, productArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    Assertions.assertEquals(PREORDER_VALUE, productArgumentCaptor.getValue().getPreOrder().getPreOrderValue());

  }

  @Test
  public void saveProductAndItemsAndPickupPointMultiPickupPointEnabledTestwithSkipEventForTakeDownTrueAndMfdFalse() {
    ReflectionTestUtils.setField(saveOperationServiceImpl, "skipEventPublishForTakeDownProduct", true);
    List<Item> items = Collections.singletonList(this.item);
    itemPickupPoints.get(0).setPickupPointCode(PICKUP_POINT);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    product.setPreOrder(preOrder);
    product.setProductSku(PRODUCT_SKU);
    product.setMarkForDelete(false);
    productAndItemEventModel.setProductSku(PRODUCT_SKU);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));

    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemVo.class), eq(Item.class))).thenReturn(items.get(0));
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ProductVo.class), eq(Product.class))).thenReturn(product);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemPickupPointVo.class), eq(ItemPickupPoint.class))).thenReturn(itemPickupPoints.get(0));

    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(this.saveAndPublishService.saveProductAndSKipPublishForMfdTrueProducts(product)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(itemPickupPointRepository.saveAll(itemPickupPoints)).thenReturn(itemPickupPoints);
    Mockito.doNothing().when(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());

    this.saveOperationServiceImpl.saveProductAndItemsAndPickupPoint(productItemsVo.getProductVo(),
        productItemsVo.getItemVoList());

    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ItemVo.class), eq(Item.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ProductVo.class), eq(Product.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ItemPickupPointVo.class), eq(ItemPickupPoint.class));
    Mockito.verify(this.saveAndPublishService).saveProductAndSKipPublishForMfdTrueProducts(productArgumentCaptor.capture());
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(this.cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(objectConverterService)
        .convertToProductAndItemEventModel(any(ProductAndItemsVO.class), Mockito.anyList());
    Mockito.verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.isNull(), Mockito.isNull());
    Mockito.verify(itemPickupPointRepository).saveAll(itemPickupPoints);
    Mockito.verify(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());
    Mockito.verify(
        saveAndPublishService).publishItemPickupPointDataChangeEvent((Mockito.anyList()),
        Mockito.anyList(),eq( Collections.EMPTY_MAP));
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent((Mockito.anyList()));
    Mockito.verify(saveAndPublishService).publishProductBundleOneToOneMappingEvent(Mockito.anyCollection());
    Assertions.assertTrue(productArgumentCaptor.getValue().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(PICKUP_POINT, productArgumentCaptor.getValue().getPickupPointCodes().stream().findFirst().get());
    Assertions.assertEquals(PREORDER_TYPE, productArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    Assertions.assertEquals(PREORDER_VALUE, productArgumentCaptor.getValue().getPreOrder().getPreOrderValue());

  }

  @Test
  public void saveProductAndItemsAndPickupPointMultiPickupPointEnabledTestwithSkipEventForTakeDownFalse() {
    ReflectionTestUtils.setField(saveOperationServiceImpl, "skipEventPublishForTakeDownProduct", false);
    List<Item> items = Collections.singletonList(this.item);
    itemPickupPoints.get(0).setPickupPointCode(PICKUP_POINT);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    product.setPreOrder(preOrder);
    product.setProductSku(PRODUCT_SKU);
    product.setMarkForDelete(true);
    productAndItemEventModel.setProductSku(PRODUCT_SKU);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));

    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemVo.class), eq(Item.class))).thenReturn(items.get(0));
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ProductVo.class), eq(Product.class))).thenReturn(product);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemPickupPointVo.class), eq(ItemPickupPoint.class))).thenReturn(itemPickupPoints.get(0));

    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(this.saveAndPublishService.saveProductAndSKipPublishForMfdTrueProducts(product)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(itemPickupPointRepository.saveAll(itemPickupPoints)).thenReturn(itemPickupPoints);
    Mockito.doNothing().when(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());

    this.saveOperationServiceImpl.saveProductAndItemsAndPickupPoint(productItemsVo.getProductVo(),
        productItemsVo.getItemVoList());

    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ItemVo.class), eq(Item.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ProductVo.class), eq(Product.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ItemPickupPointVo.class), eq(ItemPickupPoint.class));
    Mockito.verify(this.saveAndPublishService).saveProductAndSKipPublishForMfdTrueProducts(productArgumentCaptor.capture());
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(this.cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(objectConverterService)
        .convertToProductAndItemEventModel(any(ProductAndItemsVO.class), Mockito.anyList());
    Mockito.verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.isNull(), Mockito.isNull());
    Mockito.verify(itemPickupPointRepository).saveAll(itemPickupPoints);
    Mockito.verify(
        saveAndPublishService).publishItemPickupPointDataChangeEvent((Mockito.anyList()),
        Mockito.anyList(),eq( Collections.EMPTY_MAP));
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent((Mockito.anyList()));
    Mockito.verify(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());
    Mockito.verify(saveAndPublishService).publishProductBundleOneToOneMappingEvent(Mockito.anyCollection());
    Assertions.assertTrue(productArgumentCaptor.getValue().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(PICKUP_POINT, productArgumentCaptor.getValue().getPickupPointCodes().stream().findFirst().get());
    Assertions.assertEquals(PREORDER_TYPE, productArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    Assertions.assertEquals(PREORDER_VALUE, productArgumentCaptor.getValue().getPreOrder().getPreOrderValue());

  }

  @Test
  public void saveProductAndItemsAndPickupPointMultiPickupPointEnabledTestwithSkipEventForTakeDownFalseAndMfdFalse() {
    ReflectionTestUtils.setField(saveOperationServiceImpl, "skipEventPublishForTakeDownProduct", false);
    List<Item> items = Collections.singletonList(this.item);
    itemPickupPoints.get(0).setPickupPointCode(PICKUP_POINT);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    product.setPreOrder(preOrder);
    product.setProductSku(PRODUCT_SKU);
    product.setMarkForDelete(false);
    productAndItemEventModel.setProductSku(PRODUCT_SKU);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));

    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemVo.class), eq(Item.class))).thenReturn(items.get(0));
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ProductVo.class), eq(Product.class))).thenReturn(product);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemPickupPointVo.class), eq(ItemPickupPoint.class))).thenReturn(itemPickupPoints.get(0));

    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(this.saveAndPublishService.saveProductAndSKipPublishForMfdTrueProducts(product)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(itemPickupPointRepository.saveAll(itemPickupPoints)).thenReturn(itemPickupPoints);
    Mockito.doNothing().when(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());

    this.saveOperationServiceImpl.saveProductAndItemsAndPickupPoint(productItemsVo.getProductVo(),
        productItemsVo.getItemVoList());

    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ItemVo.class), eq(Item.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ProductVo.class), eq(Product.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ItemPickupPointVo.class), eq(ItemPickupPoint.class));
    Mockito.verify(this.saveAndPublishService).saveProductAndSKipPublishForMfdTrueProducts(productArgumentCaptor.capture());
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(this.cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(objectConverterService)
        .convertToProductAndItemEventModel(any(ProductAndItemsVO.class), Mockito.anyList());
    Mockito.verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.isNull(), Mockito.isNull());
    Mockito.verify(itemPickupPointRepository).saveAll(itemPickupPoints);
    Mockito.verify(
        saveAndPublishService).publishItemPickupPointDataChangeEvent((Mockito.anyList()),
        Mockito.anyList(),eq( Collections.EMPTY_MAP));
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent((Mockito.anyList()));
    Mockito.verify(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());
    Mockito.verify(saveAndPublishService).publishProductBundleOneToOneMappingEvent(Mockito.anyCollection());
    Assertions.assertTrue(productArgumentCaptor.getValue().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(PICKUP_POINT, productArgumentCaptor.getValue().getPickupPointCodes().stream().findFirst().get());
    Assertions.assertEquals(PREORDER_TYPE, productArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    Assertions.assertEquals(PREORDER_VALUE, productArgumentCaptor.getValue().getPreOrder().getPreOrderValue());

  }

  @Test
  public void saveProductAndItemsAndPickupPointMultiPickupPointEnabled_No_ItempPickupPointTest() {
    List<Item> items = Collections.singletonList(this.item);
    itemPickupPoints.get(0).setPickupPointCode(PICKUP_POINT);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    product.setPreOrder(preOrder);
    product.setProductSku(PRODUCT_SKU);
    productAndItemEventModel.setProductSku(PRODUCT_SKU);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    productItemsVo.getItemVoList().get(0).setItemPickupPointVoList(new ArrayList<>());
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemVo.class), eq(Item.class))).thenReturn(items.get(0));
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ProductVo.class), eq(Product.class))).thenReturn(product);

    Mockito.when(this.itemRepository.saveAll(items)).thenReturn(items);
    Mockito.when(this.saveAndPublishService.saveProductAndSKipPublishForMfdTrueProducts(product)).thenReturn(product);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class)))
        .thenReturn(productAndItemEventModel);
    Mockito.when(itemPickupPointRepository.saveAll(itemPickupPoints)).thenReturn(itemPickupPoints);
    Mockito.doNothing().when(saveAndPublishService)
        .publishItemPickupPointDataChangeEvent((Mockito.anyList()), Mockito.anyList(),
          eq(Collections.EMPTY_MAP));
    Mockito.doNothing().when(saveAndPublishService)
        .publishItemDataChangeEvent((Mockito.anyList()));
    Mockito.doReturn(new ItemPickupPointDataChangeEventModel()).when(saveAndPublishService)
        .publishItemPickupPointDataChangeEventForAGP(Mockito.any(ItemPickupPoint.class));
    Mockito.doNothing().when(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());

    this.saveOperationServiceImpl.saveProductAndItemsAndPickupPoint(productItemsVo.getProductVo(),
        productItemsVo.getItemVoList());

    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ItemVo.class), eq(Item.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(ProductVo.class), eq(Product.class));
    Mockito.verify(this.saveAndPublishService).saveProductAndSKipPublishForMfdTrueProducts(productArgumentCaptor.capture());
    Mockito.verify(this.itemRepository).saveAll(items);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(product.getStoreId(), product);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(this.item.getStoreId(), this.item);
    Mockito.verify(objectConverterService)
        .convertToProductAndItemEventModel(any(ProductAndItemsVO.class), Mockito.anyList());
    Mockito.verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.isNull(), Mockito.isNull());
    Mockito.verify(
        saveAndPublishService).publishItemPickupPointDataChangeEvent((Mockito.anyList()),
        Mockito.anyList(),eq( Collections.EMPTY_MAP));
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent((Mockito.anyList()));
    Mockito.verify(saveAndPublishService)
        .publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.any(Product.class), Mockito.anyList(),
            Mockito.anyMap());
    Mockito.verify(saveAndPublishService).publishProductBundleOneToOneMappingEvent(Mockito.anyCollection());
    Assertions.assertTrue(productArgumentCaptor.getValue().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(PREORDER_TYPE, productArgumentCaptor.getValue().getPreOrder().getPreOrderType());
    Assertions.assertEquals(PREORDER_VALUE, productArgumentCaptor.getValue().getPreOrder().getPreOrderValue());

  }

  @Test
  public void updateCncActivatedFlagAtProducAndtItemAndItemPickupPointOnlineTest() {
    product.setPickupPointCodes(new HashSet<>(Arrays.asList(PRODUCT_CODE)));
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, item);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setCncActive(false);
    itemPickupPoint.setNewData(Boolean.TRUE);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    HashSet<ItemViewConfig> set = new HashSet();
    set.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(set);

    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setCncActive(false);
    itemPickupPoint1.setNewData(Boolean.TRUE);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT + "1");
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    HashSet<ItemViewConfig> set1 = new HashSet();
    set1.add(itemViewConfig1);
    itemPickupPoint1.setItemViewConfig(set1);

    Mockito.doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(product);
    Mockito.when(productRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(product));
    Mockito.doNothing().when(cacheEvictHelperService).evictProductData(eq(STORE_ID), Mockito.any(Product.class));
    Mockito.when(itemRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(cacheEvictHelperService).evictItemData(eq(STORE_ID), Mockito.any(Item.class));
    Mockito.when(itemPickupPointRepository.saveAll(Mockito.anyList()))
        .thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    Mockito.doNothing().when(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.when(itemPickupPointService.pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet())).thenReturn(new HashSet<>());
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    map.put(itemPickupPoints.get(0).getItemSku(), Arrays.asList(itemPickupPoint, itemPickupPoint1));
    productSkuAndPickupPointCodeMap.replace(PRODUCT_SKU, Collections.singleton(PICKUP_POINT_NEW));
    saveOperationServiceImpl.updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(STORE_ID, CLIENT_ID, map, itemMap,
        new HashSet<>(), new ArrayList<>(), productSkuAndPickupPointCodeMap, new ArrayList<>(), new ArrayList<>());

    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productRepository).saveAll(Mockito.anyCollection());
    Mockito.verify(cacheEvictHelperService).evictProductData(eq(STORE_ID), productArgumentCaptor.capture());
    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.verify(cacheEvictHelperService, Mockito.times(2))
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(productAndItemListCaptor̉.capture());
    Mockito.verify(objectConverterService).convertToProductEventModel(product);
    Mockito.verify(itemPickupPointService).pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet());
  }


  @Test
  public void updateCncActivatedFlagAtProducAndtItemAndItemPickupPointOnlineFbbUpdateTest() {
    product.setPickupPointCodes(new HashSet<>(Arrays.asList(PRODUCT_CODE)));
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, item);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setCncActive(false);
    itemPickupPoint.setNewData(Boolean.TRUE);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    HashSet<ItemViewConfig> set = new HashSet();
    set.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(set);

    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setCncActive(false);
    itemPickupPoint1.setNewData(Boolean.TRUE);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT + "1");
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    HashSet<ItemViewConfig> set1 = new HashSet();
    set1.add(itemViewConfig1);
    itemPickupPoint1.setItemViewConfig(set1);

    Mockito.doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(product);
    Mockito.when(productRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(product));
    Mockito.doNothing().when(cacheEvictHelperService).evictProductData(eq(STORE_ID), Mockito.any(Product.class));
    Mockito.when(itemRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(cacheEvictHelperService).evictItemData(eq(STORE_ID), Mockito.any(Item.class));
    Mockito.when(itemPickupPointRepository.saveAll(Mockito.anyList()))
        .thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    Mockito.when(itemPickupPointService.updateFbbFlag(Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, false)).thenReturn(
        ItemPickupPointDataChangeEventModel.builder().build());
    Mockito.doNothing().when(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.when(itemPickupPointService.pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet())).thenReturn(new HashSet<>());

    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    map.put(itemPickupPoints.get(0).getItemSku(), Arrays.asList(itemPickupPoint, itemPickupPoint1));
    productSkuAndPickupPointCodeMap.replace(PRODUCT_SKU, Collections.singleton(PICKUP_POINT_NEW));
    saveOperationServiceImpl.updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(STORE_ID, CLIENT_ID, map, itemMap,
        new HashSet<>(), new ArrayList<>(), productSkuAndPickupPointCodeMap, new ArrayList<>(), new ArrayList<>());

    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productRepository).saveAll(Mockito.anyCollection());
    Mockito.verify(cacheEvictHelperService).evictProductData(eq(STORE_ID), productArgumentCaptor.capture());
    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.verify(cacheEvictHelperService, Mockito.times(2))
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(productAndItemListCaptor̉.capture());
    Mockito.verify(objectConverterService).convertToProductEventModel(product);
    Mockito.verify(itemPickupPointService).updateFbbFlag(Mockito.anyList());
    Mockito.verify(objectConverterService).convertToItemPickupPointChangeEventModel(itemPickupPoint, false);
    Mockito.verify(itemPickupPointService).pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet());
  }

  @Test
  public void updateCncActivatedFlagAtProducAndtItemAndItemPickupPointOnlineAndCncChangeTest() {
    item.setCncActivated(false);
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, item);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setNewData(Boolean.TRUE);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    HashSet<ItemViewConfig> set = new HashSet();
    set.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(set);

    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setCncActive(false);
    itemPickupPoint1.setNewData(Boolean.TRUE);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT + "1");
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    HashSet<ItemViewConfig> set1 = new HashSet();
    set1.add(itemViewConfig1);
    itemPickupPoint1.setItemViewConfig(set1);

    Mockito.doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(product);
    Mockito.when(productRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(product));
    Mockito.doNothing().when(cacheEvictHelperService).evictProductData(eq(STORE_ID), Mockito.any(Product.class));
    Mockito.when(itemRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(cacheEvictHelperService).evictItemData(eq(STORE_ID), Mockito.any(Item.class));
    Mockito.when(itemPickupPointRepository.saveAll(Mockito.anyList()))
        .thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    Mockito.doNothing().when(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.when(itemPickupPointService.pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet())).thenReturn(new HashSet<>());
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    map.put(itemPickupPoints.get(0).getItemSku(), Arrays.asList(itemPickupPoint, itemPickupPoint1));

    saveOperationServiceImpl.updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(STORE_ID, CLIENT_ID, map, itemMap,
        new HashSet<>(), new ArrayList<>(), productSkuAndPickupPointCodeMap, new ArrayList<>(), new ArrayList<>());

    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productRepository).saveAll(Mockito.anyCollection());
    Mockito.verify(cacheEvictHelperService).evictProductData(eq(STORE_ID), productArgumentCaptor.capture());
    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.verify(cacheEvictHelperService, Mockito.times(2))
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(itemRepository).saveAll(Mockito.any());
    Mockito.verify(cacheEvictHelperService).evictItemData(eq(STORE_ID), itemArgumentCaptor.capture());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(productAndItemListCaptor̉.capture());
    Mockito.verify(objectConverterService).convertToProductEventModel(product);
    Mockito.verify(itemPickupPointService).pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet());
  }

  @Test
  public void updateCncActivatedFlagAtProducAndtItemAndItemPickupPointOnlineAndCncChange1Test() {
    item.setCncActivated(true);
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, item);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setNewData(Boolean.TRUE);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    HashSet<ItemViewConfig> set = new HashSet();
    set.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(set);

    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setCncActive(false);
    itemPickupPoint1.setNewData(Boolean.TRUE);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT + "1");
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    HashSet<ItemViewConfig> set1 = new HashSet();
    set1.add(itemViewConfig1);
    itemPickupPoint1.setItemViewConfig(set1);

    Mockito.doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(product);
    Mockito.when(productRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(product));
    Mockito.doNothing().when(cacheEvictHelperService).evictProductData(eq(STORE_ID), Mockito.any(Product.class));
    Mockito.when(itemRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(cacheEvictHelperService).evictItemData(eq(STORE_ID), Mockito.any(Item.class));
    Mockito.when(itemPickupPointRepository.saveAll(Mockito.anyList()))
        .thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    Mockito.doNothing().when(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.when(itemPickupPointService.pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet())).thenReturn(new HashSet<>());
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    map.put(itemPickupPoints.get(0).getItemSku(), Arrays.asList(itemPickupPoint, itemPickupPoint1));

    saveOperationServiceImpl.updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(STORE_ID, CLIENT_ID, map, itemMap,
        new HashSet<>(), new ArrayList<>(), productSkuAndPickupPointCodeMap, new ArrayList<>(), new ArrayList<>());

    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productRepository).saveAll(Mockito.anyCollection());
    Mockito.verify(cacheEvictHelperService).evictProductData(eq(STORE_ID), productArgumentCaptor.capture());
    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.verify(cacheEvictHelperService, Mockito.times(2))
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(productAndItemListCaptor̉.capture());
    Mockito.verify(objectConverterService).convertToProductEventModel(product);
    Mockito.verify(itemPickupPointService).pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet());
  }

  @Test
  public void updateCncActivatedFlagAtProducAndtItemAndItemPickupPointOnlineAndCncChange2Test() {
    item.setCncActivated(false);
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, item);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setCncActive(false);
    itemPickupPoint.setNewData(Boolean.TRUE);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    HashSet<ItemViewConfig> set = new HashSet();
    set.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(set);

    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setCncActive(false);
    itemPickupPoint1.setNewData(Boolean.TRUE);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT + "1");
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    HashSet<ItemViewConfig> set1 = new HashSet();
    set1.add(itemViewConfig1);
    itemPickupPoint1.setItemViewConfig(set1);

    Mockito.doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(product);
    Mockito.when(productRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(product));
    Mockito.doNothing().when(cacheEvictHelperService).evictProductData(eq(STORE_ID), Mockito.any(Product.class));
    Mockito.when(itemRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(cacheEvictHelperService).evictItemData(eq(STORE_ID), Mockito.any(Item.class));
    Mockito.when(itemPickupPointRepository.saveAll(Mockito.anyList()))
        .thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    Mockito.doNothing().when(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.when(itemPickupPointService.pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet())).thenReturn(new HashSet<>());
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    map.put(itemPickupPoints.get(0).getItemSku(), Arrays.asList(itemPickupPoint, itemPickupPoint1));

    saveOperationServiceImpl.updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(STORE_ID, CLIENT_ID, map, itemMap,
        new HashSet<>(), new ArrayList<>(), productSkuAndPickupPointCodeMap, new ArrayList<>(), new ArrayList<>());

    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productRepository).saveAll(Mockito.anyCollection());
    Mockito.verify(cacheEvictHelperService).evictProductData(eq(STORE_ID), productArgumentCaptor.capture());
    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.verify(cacheEvictHelperService, Mockito.times(2))
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(productAndItemListCaptor̉.capture());
    Mockito.verify(objectConverterService).convertToProductEventModel(product);
    Mockito.verify(itemPickupPointService).pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet());
  }

  @Test
  public void updateCncActivatedFlagAtProducAndtItemAndItemPickupPointOnlineTest1() {
    Map<String, Item> itemMap = new HashMap<>();
    itemMap.put(ITEM_SKU, item);
    item1.setProductSku(PRODUCT_SKU);
    itemMap.put(ITEM_SKU1, item1);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setCncActive(false);
    itemPickupPoint.setNewData(Boolean.TRUE);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setBuyable(false);
    itemViewConfig.setDiscoverable(true);
    HashSet<ItemViewConfig> set = new HashSet();
    set.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(set);

    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setCncActive(false);
    itemPickupPoint1.setNewData(Boolean.FALSE);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT + "1");
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(false);
    HashSet<ItemViewConfig> set1 = new HashSet();
    set1.add(itemViewConfig1);
    itemPickupPoint1.setItemViewConfig(set1);

    Mockito.doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(product);
    Mockito.when(productRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(product));
    Mockito.doNothing().when(cacheEvictHelperService).evictProductData(eq(STORE_ID), Mockito.any(Product.class));
    Mockito.when(itemRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(cacheEvictHelperService).evictItemData(eq(STORE_ID), Mockito.any(Item.class));
    Mockito.when(itemPickupPointRepository.saveAll(Mockito.anyList()))
        .thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    Mockito.doNothing().when(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.when(itemPickupPointService.pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet())).thenReturn(new HashSet<>());
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    map.put(itemPickupPoints.get(0).getItemSku(), Arrays.asList(itemPickupPoint, itemPickupPoint1));
    itemPickupPoint1.setItemSku(ITEM_SKU1);
    map.put(ITEM_SKU1, Arrays.asList(itemPickupPoint1));
    saveOperationServiceImpl.updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(STORE_ID, CLIENT_ID, map, itemMap,
        new HashSet<>(), new ArrayList<>(), productSkuAndPickupPointCodeMap, Arrays.asList(ITEM_SKU), Arrays.asList(PRODUCT_SKU));

    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.verify(cacheEvictHelperService, Mockito.times(2))
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(productAndItemListCaptor̉.capture());
    Mockito.verify(cacheEvictHelperService).evictItemData(eq(STORE_ID), itemArgumentCaptor.capture());
    Mockito.verify(itemRepository).saveAll(Mockito.anyCollection());
    Mockito.verify(cacheEvictHelperService).evictProductData(eq(STORE_ID), productArgumentCaptor.capture());
    Mockito.verify(objectConverterService).convertToProductEventModel(product);
    Mockito.verify(itemPickupPointService).pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet());
  }

  @Test
  public void updateCncActivatedFlagAtProducAndtItemAndItemPickupPointOnlineTest4() {
    Map<String, Item> itemMap = new HashMap<>();
    itemMap.put(ITEM_SKU, item);
    item1.setProductSku(PRODUCT_SKU);
    itemMap.put(ITEM_SKU1, item1);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setCncActive(false);
    itemPickupPoint.setNewData(Boolean.TRUE);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setBuyable(false);
    itemViewConfig.setDiscoverable(true);
    HashSet<ItemViewConfig> set = new HashSet();
    set.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(set);

    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setCncActive(false);
    itemPickupPoint1.setNewData(Boolean.FALSE);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT + "1");
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(false);
    HashSet<ItemViewConfig> set1 = new HashSet();
    set1.add(itemViewConfig1);
    itemPickupPoint1.setItemViewConfig(set1);

    Mockito.doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(product);
    Mockito.when(productRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(product));
    Mockito.doNothing().when(cacheEvictHelperService).evictProductData(eq(STORE_ID), Mockito.any(Product.class));
    Mockito.when(itemRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(cacheEvictHelperService).evictItemData(eq(STORE_ID), Mockito.any(Item.class));
    Mockito.when(itemPickupPointRepository.saveAll(Mockito.anyList()))
        .thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    Mockito.doNothing().when(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.when(itemPickupPointService.pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet())).thenReturn(new HashSet<>());
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    map.put(itemPickupPoints.get(0).getItemSku(), Arrays.asList(itemPickupPoint, itemPickupPoint1));
    itemPickupPoint1.setItemSku(ITEM_SKU1);
    map.put(ITEM_SKU1, Arrays.asList(itemPickupPoint1));
    saveOperationServiceImpl.updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(STORE_ID, CLIENT_ID, map, itemMap,
        new HashSet<>(), new ArrayList<>(), productSkuAndPickupPointCodeMap, Arrays.asList(ITEM_SKU), new ArrayList<>());

    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.verify(cacheEvictHelperService, Mockito.times(2))
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(productAndItemListCaptor̉.capture());
    Mockito.verify(cacheEvictHelperService).evictItemData(eq(STORE_ID), itemArgumentCaptor.capture());
    Mockito.verify(itemRepository).saveAll(Mockito.anyCollection());
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(objectConverterService).convertToProductEventModel(product);
    Mockito.verify(itemPickupPointService).pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet());
  }

  @Test
  public void updateCncActivatedFlagAtProductAndItemAndItemPickupPointDifferentProductSKUMppOnTest() {

    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, item);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setNewData(Boolean.TRUE);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setDelivery(true);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    HashSet<ItemViewConfig> set = new HashSet();
    set.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(set);

    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU1);
    itemPickupPoint1.setCncActive(false);
    itemPickupPoint1.setNewData(Boolean.TRUE);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT + "1");
    itemPickupPoint1.setProductSku(PRODUCT_SKU1);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    HashSet<ItemViewConfig> set1 = new HashSet();
    set1.add(itemViewConfig1);
    itemPickupPoint1.setItemViewConfig(set1);

    Mockito.doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(product);
    Mockito.when(productRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(product));
    Mockito.doNothing().when(cacheEvictHelperService).evictProductData(eq(STORE_ID), Mockito.any(Product.class));
    Mockito.when(itemRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(cacheEvictHelperService).evictItemData(eq(STORE_ID), Mockito.any(Item.class));
    Mockito.when(itemPickupPointRepository.saveAll(Mockito.anyList()))
        .thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    Mockito.doNothing().when(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.when(itemPickupPointService.pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet())).thenReturn(new HashSet<>());
    Mockito.when(itemPickupPointService.pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet())).thenReturn(new HashSet<>());
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    map.put(itemPickupPoints.get(0).getItemSku(), Arrays.asList(itemPickupPoint, itemPickupPoint1));

    saveOperationServiceImpl.updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(STORE_ID, CLIENT_ID, map, itemMap,
        new HashSet<>(), new ArrayList<>(), productSkuAndPickupPointCodeMap, new ArrayList<>(), new ArrayList<>());

    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productRepository).saveAll(Mockito.anyCollection());
    Mockito.verify(itemRepository).saveAll(Mockito.anyCollection());
    Mockito.verify(cacheEvictHelperService).evictProductData(eq(STORE_ID), productArgumentCaptor.capture());
    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.verify(cacheEvictHelperService, Mockito.times(2))
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(cacheEvictHelperService).evictItemData(eq(STORE_ID), itemArgumentCaptor.capture());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(productAndItemListCaptor̉.capture());
    Mockito.verify(objectConverterService).convertToProductEventModel(product);
    Mockito.verify(itemPickupPointService).pickupPointsAddedToOtherVariants(Mockito.anyString(), Mockito.anySet());
  }


  @Test
  public void updateCncActivatedFlagAtProductAndItemAndItemPickupPointNoChangeMPPTrueTest() {
    product.setCncActivated(true);
    item.setCncActivated(true);
    Set<String> pickupPointCodes = new HashSet<>();
    pickupPointCodes.add(PICKUP_POINT);
    product.setPickupPointCodes(pickupPointCodes);
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, item);

    Mockito.doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.anyString(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU)).thenReturn(product);
    Mockito.when(productRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(product));
    Mockito.doNothing().when(cacheEvictHelperService).evictProductData(eq(STORE_ID), Mockito.any(Product.class));
    Mockito.when(itemRepository.saveAll(Mockito.anyList())).thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(cacheEvictHelperService).evictItemData(eq(STORE_ID), Mockito.any(Item.class));
    Mockito.when(itemPickupPointRepository.saveAll(Mockito.anyList()))
        .thenReturn(itemPickupPoints);
    Mockito.doNothing().when(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    map.put(itemPickupPoints.get(0).getItemSku(), itemPickupPoints);

    saveOperationServiceImpl.updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(STORE_ID, CLIENT_ID, map, itemMap,
        new HashSet<>(), new ArrayList<>(), productSkuAndPickupPointCodeMap, new ArrayList<>(), new ArrayList<>());

    Mockito.verify(productCacheableService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            eq(CLIENT_ID),Mockito.anyList());
    Mockito.verify(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.isNull(), Mockito.any(ItemPickupPoint.class), Mockito.anyString());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(productAndItemListCaptor̉.capture());
  }

  @Test
  public void updateCncActivatedFlagAtProductAndItemAndItemPickupPointEmptyTest() {

    saveOperationServiceImpl.updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(STORE_ID, CLIENT_ID,
        new HashMap<>(), new HashMap<>(), new HashSet<>(), new ArrayList<>(), productSkuAndPickupPointCodeMap,
        new ArrayList<>(), new ArrayList<>());
  }

  @Test
  public void test(){
    Mockito.when(itemPickupPointRepository.saveAll(itemPickupPoints)).thenReturn(itemPickupPoints);

    saveOperationServiceImpl.saveItemPickupPointsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemPickupPoints);
    Mockito.verify(itemPickupPointRepository).saveAll(itemPickupPoints);
    Mockito.verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.any(),Mockito.any(),
      Mockito.any());
  }

  private ProductItemsVo generateProductItemVo(Product product, Item item, ItemPickupPoint itemPickupPoint) {
    ProductItemsVo productItemsVo = new ProductItemsVo();
    productItemsVo.setProductVo(Objects.nonNull(product) ? mapper.deepCopy(product, ProductVo.class) : null);
    productItemsVo.setItemVoList(Objects.nonNull(item) ? Arrays.asList(mapper.deepCopy(item, ItemVo.class)) : null);
    if (CollectionUtils.isNotEmpty(productItemsVo.getItemVoList())) {
      productItemsVo.getItemVoList().get(0).setItemPickupPointVoList(new ArrayList<>());
      productItemsVo.getItemVoList().get(0).setItemPickupPointVoList(Objects.nonNull(itemPickupPoint) ?
          Arrays.asList(mapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class)) :
          new ArrayList<>());
    }
    return productItemsVo;
  }
}
