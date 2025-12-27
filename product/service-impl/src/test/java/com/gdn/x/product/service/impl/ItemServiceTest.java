package com.gdn.x.product.service.impl;

import static com.gdn.x.product.enums.Constants.WAREHOUSE_USERNAME;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.base.mapper.impl.DozerMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.ItemBasicL4Response;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.MasterDataAttributeService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.FormulaUtil;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.google.common.collect.ImmutableSet;

public class ItemServiceTest {

  private static final List<MasterDataItemAttributeValue> MASTER_DATA_ITEM_ATTRIBUTES = Arrays
      .asList(new MasterDataItemAttributeValue());

  private static final HashSet<ProductItemResponse> ITEM_RESPONSES =
      new HashSet<ProductItemResponse>();

  private static final String ITEM_SKU_2 = "item-sku-2";

  private static final String PRODUCT_SKU = "productSku";

  private static final String MERCHANT_CODE = "merchantCode";

  private static final String MERCHANT_SKU = "merchantSku";

  private static final String REQUEST_ID = "requestId";

  private static final String ITEM_NOT_FOUND = "Item not found";

  private static final String BLANK = "";

  private static final String ITEM_SKU = "itemSku";

  private static final List<String> ITEM_SKUS = Arrays.asList(ITEM_SKU);

  private static final String USERNAME = "username";

  private static final int OFFER_PRICE = 1;

  private static final int LIST_PRICE = 2;

  private static final String CHANNEL = "channel";

  private static final String LAST_UPDATED_BY = "lastUpdatedBy";

  private static final Date LAST_UPDATED_DATE = new Date();

  private static final String CURRENCY = "currency";

  private static final double SHIPPING_WT = 1.0;
  private static final int DANGEROUS_LEVEL = 1;
  private static final String STORE_ID = "storeId";
  private static final String ITEM_CODE = "itemCode";
  private static final String ITEM_CODE_1 = "itemCode1";
  private static final String SOURCE_ITEM_CODE = "sourceItemCode";
  private static final String PREORDER_TYPE = "preOrderType";
  private static final String ITEM_NAME = "itemName";
  private static final Integer PREORDER_VALUE = 10;
  private static final String PRODUCT_CATEGORY_CODE = "53187";
  private static final String UPC_CODE = "upcCode";
  private static final String UPC_CODE_NEW = "upcCodeNew";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String DEFAULT = "DEFAULT";
  private static final String DEFAULT_USERNAME = "system";
  private static final String DEFAULT_CLIENT_ID = "pbp";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String B2C_SELLER_CHANNEL = "BLIBLI";
  private static final String B2B_SELLER_CHANNEL = "BLIBLI FOR BUSINESS";
  private static final double HEIGHT = 10.0;
  private static final double WEIGHT = 10.0;
  private static final double WIDTH = 10.0;
  private static final double LENGTH = 10.0;
  private static final double INVALID_DIMENSION = 0.0;
  private static final String MASTER_SKU = "masterSku";
  private static final String CNC = "cnc";

  @InjectMocks
  private ItemServiceImpl itemServiceImpl;

  @Mock
  private FormulaUtil formulaUtil;



  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ProductService productService;

  @Mock
  private ItemCacheableService itemCacheableService;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private SaveOperationService saveOperationService;

  @Mock
  private ChannelService channelService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemPriceService itemPriceService;

  @Mock
  private ItemHelperService itemHelperServiceImpl;

  @Mock
  private SkuValidator skuValidator;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  private GdnMapper gdnMapper = new DozerMapper(new DozerBeanMapper());

  private Item item;

  private List<Item> items;

  private Price price;

  private Product product;

  private List<Product> products;

  private ProductDetailResponse productDetailResponse;

  private PreOrder preOrder;

  private List<ItemPickupPoint> itemPickupPoints;

  private ProfileResponse profileResponse = new ProfileResponse();
  private BusinessPartner businessPartner = new BusinessPartner();

  @Mock
  private MasterDataAttributeService masterDataAttributeService;

  @Mock
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Captor
  private ArgumentCaptor<ProductAndItemsVO> productAndItemsVOArgumentCaptor;

  @Captor
  private ArgumentCaptor<ItemVo> itemVoArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ItemVo>> itemVoListArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<Item>> itemsCaptor;

  @Captor
  private ArgumentCaptor<AuditTrailListResponse> auditTrailListResponseArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Test
  public void addItemPriceAlreadyHasPriceTest() {
    this.item.getPrice().add(this.price);
    when(
        this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
            ItemServiceTest.STORE_ID, ItemServiceTest.ITEM_SKU, false)).thenReturn(this.item);
    try {
      this.itemServiceImpl.addItemPrice(ItemServiceTest.STORE_ID, this.price,
          ItemServiceTest.ITEM_SKU, ItemServiceTest.USERNAME);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
    }
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServiceTest.STORE_ID, ItemServiceTest.ITEM_SKU, false);
  }


  @Test
  public void addItemPriceBlankItemSkuTest() {
    assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.addItemPrice(ItemServiceTest.STORE_ID, this.price, ItemServiceTest.BLANK,
        ItemServiceTest.USERNAME));
  }

  @Test
  public void addItemPriceBlankStoreIdTest() {
    assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemPrice(ItemServiceTest.BLANK, this.price, ItemServiceTest.ITEM_SKU,
        ItemServiceTest.USERNAME));
  }

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeBlankMerchantCodeTest() {
    assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemPriceByMerchantSkuAndMerchantCode(ItemServiceTest.STORE_ID,
        ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME, ItemServiceTest.MERCHANT_SKU,
        ItemServiceTest.BLANK, this.price));
  }

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeBlankStoreIdTest() {
    assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemPriceByMerchantSkuAndMerchantCode(ItemServiceTest.BLANK,
        ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME, ItemServiceTest.MERCHANT_SKU,
        ItemServiceTest.MERCHANT_CODE, this.price));
  }

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeNullPriceTest() {
    assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemPriceByMerchantSkuAndMerchantCode(ItemServiceTest.STORE_ID,
        ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME, ItemServiceTest.MERCHANT_SKU,
        ItemServiceTest.MERCHANT_CODE, null));
  }

  @Test
  public void addItemPriceByMerchantSkuAndMerchantCodeTest() {
    this.item.setPrice(new HashSet<Price>());
    when(
        this.itemRepository.findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(
            ItemServiceTest.STORE_ID, ItemServiceTest.MERCHANT_SKU)).thenReturn(this.items);
    when(
        this.productService.getProductsByMerchantCode(ItemServiceTest.STORE_ID,
            ItemServiceTest.MERCHANT_CODE)).thenReturn(this.products);
    this.itemServiceImpl.addItemPriceByMerchantSkuAndMerchantCode(ItemServiceTest.STORE_ID,
        ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME, ItemServiceTest.MERCHANT_SKU,
        ItemServiceTest.MERCHANT_CODE, this.price);
    verify(this.itemRepository).findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(
        ItemServiceTest.STORE_ID, ItemServiceTest.MERCHANT_SKU);
    verify(this.productService).getProductsByMerchantCode(ItemServiceTest.STORE_ID,
        ItemServiceTest.MERCHANT_CODE);
    verify(this.saveOperationService).saveItems(this.items, null);
    assertTrue(this.items.get(0).getPrice().contains(this.price));
  }

  @Test
  public void addItemPriceNotFoundItemSkuTest() {
    when(
        this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
            ItemServiceTest.STORE_ID, ItemServiceTest.ITEM_SKU, false)).thenReturn(null);
    try {
      this.itemServiceImpl.addItemPrice(ItemServiceTest.STORE_ID, this.price,
          ItemServiceTest.ITEM_SKU, ItemServiceTest.USERNAME);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      assertTrue(e.getMessage().contains(ItemServiceTest.ITEM_NOT_FOUND));
    }
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServiceTest.STORE_ID, ItemServiceTest.ITEM_SKU, false);
  }

  @Test
  public void addItemPriceNullPriceTest() {
    assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.addItemPrice(ItemServiceTest.STORE_ID, null, ItemServiceTest.ITEM_SKU,
        ItemServiceTest.USERNAME));
  }

  @Test
  public void addItemPriceTest() {
    this.item.getPrice().clear();
    when(
        this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
            ItemServiceTest.STORE_ID, ItemServiceTest.ITEM_SKU, false)).thenReturn(this.item);
    this.itemServiceImpl.addItemPrice(ItemServiceTest.STORE_ID, this.price,
        ItemServiceTest.ITEM_SKU, ItemServiceTest.USERNAME);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServiceTest.STORE_ID, ItemServiceTest.ITEM_SKU, false);
    verify(this.saveOperationService).saveItemWithoutUpdatingSolr(this.item, null, false, StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(this.productAndItemSolrIndexerService).updateSolrOnPriceChange(Arrays.asList(item));
  }

  @Test
  public void addItems() throws Exception {
    product.setPreOrder(preOrder);
    itemPickupPoints.get(0).setPrice(ImmutableSet.of(price));
    HashMap<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    masterDataItemMap.put(ModelHelper.ITEM_CODE, masterDataItem);
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    Page<Item> itemPage = new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    itemPickupPoints.get(0).setCncActive(true);
    itemPickupPoints.get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemPickupPoints.get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    when(this.channelService.getCncChannel()).thenReturn(CNC);
    when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(eq(this.STORE_ID),
        anyString(), any(Pageable.class))).thenReturn(itemPage);
    when(this.objectConverterService.convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
        ModelHelper.PRODUCT_CODE)).thenReturn(masterDataItemMap);
    when(this.productHelperService.addItemAttributeToProductAttribute(this.product, ItemServiceTest.ITEM_SKU,
        ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES)).thenReturn(this.product);
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    Mockito.when(this.productHelperService.setItemDetail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.any(ItemVo.class))).thenReturn(itemVo);
    List<ItemVo> result =
        this.itemServiceImpl.addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            ItemServiceTest.PRODUCT_SKU, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
            businessPartner);
    verify(this.channelService).getDefaultChannel();
    verify(this.channelService).getCncChannel();
    Mockito.verify(productHelperService, Mockito.times(this.items.size())).setItemDetail(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.any(ItemVo.class));
    verify(this.objectConverterService).convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
        ModelHelper.PRODUCT_CODE);
    verify(this.productHelperService).addItemAttributeToProductAttribute(this.product, ItemServiceTest.ITEM_SKU,
        ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    assertNotNull(result);
    Assertions.assertTrue(result.get(0).isContentChanged());
    Assertions.assertTrue(result.get(0).isInitialContentChanged());
    assertEquals(SOURCE_ITEM_CODE, result.get(0).getSourceItemCode());
    Assertions.assertNull(result.get(0).getPristineDataItem());
  }

  @Test
  public void addItemsNoCncItems() throws Exception {
    product.setPreOrder(preOrder);
    itemPickupPoints.get(0).setPrice(ImmutableSet.of(price));
    HashMap<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    masterDataItemMap.put(ModelHelper.ITEM_CODE, masterDataItem);
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    Page<Item> itemPage = new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    itemPickupPoints.get(0).getItemViewConfig().iterator().next().setBuyable(false);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    productItemsVo.getItemVoList().forEach(itemVo -> itemVo.getItemPickupPointVoList()
        .forEach(itemPickupPointVo -> itemPickupPointVo.setCncActive(false)));
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    productItemsVo.getItemVoList().get(0).getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next()
        .setBuyable(false);
    when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(eq(this.STORE_ID),
        anyString(), any(Pageable.class))).thenReturn(itemPage);
    when(this.objectConverterService.convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
        ModelHelper.PRODUCT_CODE)).thenReturn(masterDataItemMap);
    when(this.productHelperService.addItemAttributeToProductAttribute(this.product, ItemServiceTest.ITEM_SKU,
        ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES)).thenReturn(this.product);
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    Mockito.when(this.productHelperService.setItemDetail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.any(ItemVo.class))).thenReturn(itemVo);
    List<ItemVo> result =
        this.itemServiceImpl.addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            ItemServiceTest.PRODUCT_SKU, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
            businessPartner);
    verify(this.channelService).getDefaultChannel();
    Mockito.verify(productHelperService, Mockito.times(this.items.size())).setItemDetail(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.any(ItemVo.class));
    verify(this.objectConverterService).convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
        ModelHelper.PRODUCT_CODE);
    verify(this.productHelperService).addItemAttributeToProductAttribute(this.product, ItemServiceTest.ITEM_SKU,
        ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    assertNotNull(result);
    Assertions.assertTrue(result.get(0).isContentChanged());
    Assertions.assertTrue(result.get(0).isInitialContentChanged());
    assertEquals(SOURCE_ITEM_CODE, result.get(0).getSourceItemCode());
    Assertions.assertNull(result.get(0).getPristineDataItem());
  }

  @Test
  public void addItemsWithWholesalePriceExistsTrue() throws Exception {
    items.get(0).setWholesalePriceExists(true);
    itemPickupPoints.get(0).setPrice(ImmutableSet.of(price));
    itemPickupPoints.get(0).setWholesalePriceExists(true);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    productDetailResponse.getProductItemResponses().forEach(item -> item.setContentChanged(false));
    HashMap<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    masterDataItemMap.put(ModelHelper.ITEM_CODE, masterDataItem);
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    item.setMasterSku(MASTER_SKU);
    Page<Item> itemPage1 = new PageImpl<>(Arrays.asList(existingItem), PageRequest.of(0, 1), 2);
    Page<Item> itemPage2 = new PageImpl<>(Arrays.asList(item), PageRequest.of(0, 10), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    productItemsVo.getItemVoList().get(0).getItemPickupPointVoList().get(0).setWholesalePriceActivated(true);
    Mockito.when(this.productHelperService.setItemDetail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.any(ItemVo.class))).thenReturn(productItemsVo.getItemVoList().get(0));
    when(itemRepository
        .findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(isNull(), anyString(),
            any(Pageable.class))).thenReturn(itemPage1).thenReturn(itemPage2);
    when(
        this.objectConverterService.convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
            ModelHelper.PRODUCT_CODE)).thenReturn(masterDataItemMap);
    when(
        this.productHelperService.addItemAttributeToProductAttribute(this.product,
            ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES)).thenReturn(
        this.product);

    List<ItemVo> result = this.itemServiceImpl
        .addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            ItemServiceTest.PRODUCT_SKU, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
            businessPartner);

    verify(this.channelService).getDefaultChannel();
    Mockito.verify(productHelperService, Mockito.times(this.items.size())).setItemDetail(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.any(ItemVo.class));
    verify(this.objectConverterService).convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
        ModelHelper.PRODUCT_CODE);
    verify(this.productHelperService).addItemAttributeToProductAttribute(this.product,
        ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    Mockito.verify(itemRepository, times(2))
        .findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(isNull(), anyString(),
            any(Pageable.class));
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    assertNotNull(result);
    Assertions.assertNull(result.get(0).getPristineDataItem());
    assertEquals(true, result.get(0).isWholesalePriceExists());
    assertEquals(com.gdn.x.product.enums.Constants.WHOLESALE_PRICE,
        result.get(0).getActivePromoBundlings().stream().findFirst().get());
    assertEquals(MASTER_SKU, result.get(0).getMasterSku());
  }

  @Test
  public void addItemsWithWholesalePriceActivatedFalse() throws Exception {
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.getItemPickupPointVoList().get(0).setCncActive(true);
    itemVo.setItemCode(ITEM_CODE);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setBuyable(true);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
    itemViewConfigSet.add(itemViewConfig);
    itemVo.getItemPickupPointVoList().get(0).setItemViewConfig(itemViewConfigSet);
    itemVo.getItemPickupPointVoList().forEach(itemPickupPoint -> itemPickupPoint.setPrice(ImmutableSet.of(price)));
    itemVo.getItemPickupPointVoList()
        .forEach(itemPickupPointVo -> itemPickupPointVo.setCncActive(true));
    itemVo.getItemPickupPointVoList()
        .forEach(itemPickupPointVo -> itemPickupPointVo.setPickupPointCode(PICKUP_POINT_CODE));
    itemVo.setWholesalePriceActivated(false);
    Mockito.when(this.productHelperService.setItemDetail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.any(ItemVo.class))).thenReturn(itemVo);
    HashMap<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    masterDataItemMap.put(ModelHelper.ITEM_CODE, masterDataItem);
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    when(itemRepository
        .findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(anyString(), anyString(),
            any(Pageable.class))).thenReturn(itemPage);
    when(
        this.objectConverterService.convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
            ModelHelper.PRODUCT_CODE)).thenReturn(masterDataItemMap);
    when(
        this.productHelperService.addItemAttributeToProductAttribute(this.product,
            ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES)).thenReturn(
        this.product);
    when(this.objectConverterService.toProductAttributeDetail(
        Mockito.any(ProductItemAttributeValueResponse.class))).thenReturn(new ProductAttributeDetail());
    when(this.channelService.getCncChannel()).thenReturn(CNC);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(ITEM_CODE_1);
    productItemResponse.setSourceItemCode(SOURCE_ITEM_CODE);
    productItemResponse.setContentChanged(true);
    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    ProductItemAttributeValueResponse productItemAttributeValueResponse1 = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse1.setVariantCreation(true);
    productItemAttributeValueResponse1.setAttributeResponse(attributeResponse1);
    ProductItemAttributeValueResponse productItemAttributeValueResponse2 = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productItemAttributeValueResponse2.setAttributeResponse(attributeResponse2);
    productItemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse, productItemAttributeValueResponse1, productItemAttributeValueResponse2));
    ITEM_RESPONSES.add(productItemResponse);
    this.productDetailResponse.setProductItemResponses(ITEM_RESPONSES);


    List<ItemVo> result = this.itemServiceImpl
        .addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            ItemServiceTest.PRODUCT_SKU, Arrays.asList(itemVo), this.product, this.productDetailResponse,
            businessPartner);
    verify(this.channelService).getCncChannel();
    verify(this.channelService).getDefaultChannel();
    Mockito.verify(productHelperService, Mockito.times(this.items.size())).setItemDetail(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.any(ItemVo.class));
    verify(this.objectConverterService).convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
        ModelHelper.PRODUCT_CODE);
    verify(this.productHelperService).addItemAttributeToProductAttribute(this.product,
        ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        Mockito.anyList());
    assertNotNull(result);
    Assertions.assertNull(result.get(0).getPristineDataItem());
    assertEquals(true, result.get(0).isWholesalePriceExists());
    Assertions.assertNull(result.get(0).getActivePromoBundlings());
  }

  @Test
  public void addItemsWithWholesalePriceExistsFalse() throws Exception {
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().forEach(itemPickupPoint ->  itemPickupPoint.setWholesalePriceExists(false));
    itemVo.getItemPickupPointVoList().forEach(itemPickupPoint -> itemPickupPoint.setPrice(ImmutableSet.of(price)));
    itemVo.setWholesalePriceActivated(false); itemVo.getItemPickupPointVoList()
        .forEach(itemPickupPointVo -> itemPickupPointVo.setCncActive(true));
    itemVo.getItemPickupPointVoList()
        .forEach(itemPickupPointVo -> itemPickupPointVo.setPickupPointCode(PICKUP_POINT_CODE));
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Mockito.when(this.productHelperService.setItemDetail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.any(ItemVo.class))).thenReturn(itemVo);
    HashMap<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    masterDataItemMap.put(ModelHelper.ITEM_CODE, masterDataItem);
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    when(itemRepository
        .findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(anyString(), anyString(),
            any(Pageable.class))).thenReturn(itemPage);
    when(
        this.objectConverterService.convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
            ModelHelper.PRODUCT_CODE)).thenReturn(masterDataItemMap);
    when(
        this.productHelperService.addItemAttributeToProductAttribute(this.product,
            ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES)).thenReturn(
        this.product);
    when(this.channelService.getCncChannel()).thenReturn(CNC);
    List<ItemVo> result = this.itemServiceImpl
        .addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            ItemServiceTest.PRODUCT_SKU, Arrays.asList(itemVo), this.product, this.productDetailResponse,
            businessPartner);
    verify(this.channelService).getCncChannel();
    verify(this.channelService).getDefaultChannel();
    Mockito.verify(productHelperService, Mockito.times(this.items.size())).setItemDetail(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.any(ItemVo.class));
    verify(this.objectConverterService).convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
        ModelHelper.PRODUCT_CODE);
    verify(this.productHelperService).addItemAttributeToProductAttribute(this.product,
        ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
            Mockito.anyList());
    assertNotNull(result);
    Assertions.assertNull(result.get(0).getPristineDataItem());
    assertEquals(false, result.get(0).isWholesalePriceExists());
    assertEquals(null, result.get(0).getActivePromoBundlings());
  }

  @Test
  public void addItemsContainingPriceWithoutDefaultChannel() throws Exception {
    Item itemWithoutDefaultChannel =
        new Item(ItemServiceTest.ITEM_SKU_2, ItemServiceTest.PRODUCT_SKU);
    Price price =
        new Price(ItemServiceTest.CURRENCY, ItemServiceTest.OFFER_PRICE,
            ItemServiceTest.LIST_PRICE, "", ItemServiceTest.LAST_UPDATED_BY,
            ItemServiceTest.LAST_UPDATED_DATE);
    price.setChannel("");
    itemWithoutDefaultChannel.setItemCode(ModelHelper.ITEM_CODE);
    itemWithoutDefaultChannel.setPrice(new HashSet<Price>());
    itemWithoutDefaultChannel.getPrice().add(price);
    this.items.clear();
    this.items.add(itemWithoutDefaultChannel);
    itemPickupPoints.get(0).setPrice(ImmutableSet.of(price));
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    try {
      List<ItemVo> result = this.itemServiceImpl
          .addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
              ItemServiceTest.PRODUCT_SKU, productItemsVo.getItemVoList(), this.product,
              this.productDetailResponse, businessPartner);
    } catch (ApplicationRuntimeException e) {
      verify(this.channelService).getDefaultChannel();
    }
  }

  @Test
  public void addItemsWithBlankProductSku() throws Exception {
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl
        .addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            "", productItemsVo.getItemVoList(), this.product, this.productDetailResponse, businessPartner));
  }

  @Test
  public void addItemsWithBlankStoreId() throws Exception {
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl
        .addItems("", ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME, ItemServiceTest.PRODUCT_SKU,
            productItemsVo.getItemVoList(), this.product, this.productDetailResponse, businessPartner));
  }

  @Test
  public void addItemsWithEmptyListOfItemRequest() throws Exception {
    ProductItemsVo productItemsVo = generateProductItemVo(product, null, itemPickupPoints.get(0));
    assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl
        .addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            ItemServiceTest.PRODUCT_SKU, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
            businessPartner));
  }

  @Test
  public void addItemsWithNullListOfItemRequest() throws Exception {
    ProductItemsVo productItemsVo = generateProductItemVo(product, null, itemPickupPoints.get(0));
    assertThrows(ApplicationRuntimeException.class, () ->   this.itemServiceImpl
        .addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            ItemServiceTest.PRODUCT_SKU, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
            businessPartner));
  }

  @Test
  public void addItemsWithNullProduct() throws Exception {
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl
        .addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            ItemServiceTest.PRODUCT_SKU, productItemsVo.getItemVoList(), null, this.productDetailResponse,
            businessPartner));
  }

  @Test
  public void getItemSkuByItemNameKeywordTest() {
    when(itemRepository.getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_NAME)).thenReturn(Arrays.asList(item));
    itemServiceImpl.getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_NAME);
    verify(itemRepository).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_NAME);
  }

  @Test
  public void findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivatedTest() {
    itemServiceImpl.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(STORE_ID, PRODUCT_SKU, true);
    Mockito.verify(itemRepository)
        .countByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(STORE_ID, PRODUCT_SKU, true, false);
  }

  @Test
  public void getItemSkuByMerchantCodeAndItemCodeAndItemNameKeywordTest() {
    when(itemRepository.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, ImmutableSet.of(ITEM_CODE),
        ITEM_NAME)).thenReturn(Arrays.asList(item));
    itemServiceImpl.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, ImmutableSet.of(ITEM_CODE), ITEM_NAME);
    verify(itemRepository).getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, ImmutableSet.of(ITEM_CODE),
        ITEM_NAME);
  }

  @Test
  public void getItemSkuByMerchantCodeAndItemCodeAndItemNameKeywordOnlyNameTest() {
    when(itemRepository.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, new HashSet<>(),
        ITEM_NAME)).thenReturn(Arrays.asList(item));
    itemServiceImpl.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, new HashSet<>(), ITEM_NAME);
    verify(itemRepository).getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, new HashSet<>(),
        ITEM_NAME);
  }

  @Test
  public void getItemSkuByMerchantCodeAndItemCodeAndItemNameKeywordStoreIdNullTest() {
    assertThrows(ApplicationRuntimeException.class, () ->  itemServiceImpl.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(StringUtils.EMPTY, MERCHANT_CODE, ImmutableSet.of(ITEM_CODE), ITEM_NAME));
  }

  @Test
  public void getItemSkuByMerchantCodeAndItemCodeAndItemNameKeywordMerchantCodrNullTest() {
    assertThrows(ApplicationRuntimeException.class, () ->  itemServiceImpl.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, StringUtils.EMPTY, ImmutableSet.of(ITEM_CODE), ITEM_NAME));
  }

  @Test
  public void getItemSkuByMerchantCodeAndItemCodeAndItemNameKeywordCodeAndKeywordNullTest() {
    assertThrows(ApplicationRuntimeException.class, () ->  itemServiceImpl.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, new HashSet<>(), StringUtils.EMPTY));
  }

  @Test
  public void publishUpdateToSolrEventTest() {
    itemServiceImpl.publishUpdateToSolrEvent(product, items);
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(new ProductAndItemsVO(product, items));
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(Mockito.anyList());
  }

  @Test
  public void updateItemDimensionsAndUpcCodeTest() {
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setSynchronized(true);
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.LENGTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WIDTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.HEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(
        new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.SHIPPING_WEIGHT_CHANGE,
            Double.toString(20.0), Double.toString(20.0), null, item.getProductSku(),
            item.getGeneratedItemName(), item.getPickupPointCode(), true));
    AuditTrailListResponse auditTrailListResponse =
        new AuditTrailListResponse(auditTrailDtoList, DEFAULT, DEFAULT_USERNAME, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID,
            true);
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, 10.0, 10.0, 10.0, 10.0, new ArrayList<>());
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService)
        .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    Mockito.verify(kafkaProducer).send(Mockito.any(),
        auditTrailListResponseArgumentCaptor.capture());
    AuditTrailListResponse capturedResponse = auditTrailListResponseArgumentCaptor.getValue();
    assertEquals(WAREHOUSE_USERNAME, capturedResponse.getChangedBy());
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(20.0, itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void updateItemDimensionsAndUpcCodeSwitchOnTest() {
    ReflectionTestUtils.setField(itemServiceImpl, "updateProductInDimensionEvent", Boolean.TRUE);
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setSynchronized(true);
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.LENGTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WIDTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.HEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(
        new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.SHIPPING_WEIGHT_CHANGE,
            Double.toString(20.0), Double.toString(20.0), null, item.getProductSku(),
            item.getGeneratedItemName(), item.getPickupPointCode(), true));
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku()))
        .thenReturn(product);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, 10.0, 10.0, 10.0, 10.0, new ArrayList<>());
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku());
    Mockito.verify(productService)
        .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    Mockito.verify(kafkaProducer).send(Mockito.any(),
        auditTrailListResponseArgumentCaptor.capture());
    AuditTrailListResponse capturedResponse = auditTrailListResponseArgumentCaptor.getValue();
    assertEquals(WAREHOUSE_USERNAME, capturedResponse.getChangedBy());
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(20.0, itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void updateItemDimensionsAndUpcCodeSwitchOnNullProductTest() {
    ReflectionTestUtils.setField(itemServiceImpl, "updateProductInDimensionEvent", Boolean.TRUE);
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setSynchronized(true);
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.LENGTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WIDTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.HEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(
        new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.SHIPPING_WEIGHT_CHANGE,
            Double.toString(20.0), Double.toString(20.0), null, item.getProductSku(),
            item.getGeneratedItemName(), item.getPickupPointCode(), true));
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, 10.0, 10.0, 10.0, 10.0, new ArrayList<>());
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku());
    Mockito.verify(productService)
        .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    Mockito.verify(kafkaProducer).send(Mockito.any(),
        auditTrailListResponseArgumentCaptor.capture());
    AuditTrailListResponse capturedResponse = auditTrailListResponseArgumentCaptor.getValue();
    assertEquals(WAREHOUSE_USERNAME, capturedResponse.getChangedBy());
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(20.0, itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void updateItemDimensionsAndUpcCodeSwitchOnDimensionMissingTrueTest() {
    ReflectionTestUtils.setField(itemServiceImpl, "updateProductInDimensionEvent", Boolean.TRUE);
    product.setDimensionsMissing(Boolean.TRUE);
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setSynchronized(true);
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.LENGTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WIDTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.HEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(
        new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.SHIPPING_WEIGHT_CHANGE,
            Double.toString(20.0), Double.toString(20.0), null, item.getProductSku(),
            item.getGeneratedItemName(), item.getPickupPointCode(), true));
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku()))
        .thenReturn(product);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, 10.0, 10.0, 10.0, 10.0, new ArrayList<>());
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku());
    Mockito.verify(productService)
        .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    Mockito.verify(kafkaProducer).send(Mockito.any(),
        auditTrailListResponseArgumentCaptor.capture());
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), Mockito.anyList(),
        eq(StringUtils.EMPTY));
    AuditTrailListResponse capturedResponse = auditTrailListResponseArgumentCaptor.getValue();
    assertEquals(WAREHOUSE_USERNAME, capturedResponse.getChangedBy());
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(20.0, itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void updateItemDimensionsAndUpcCodeSwitchOnDimensionMissingFieldsTest() {
    ReflectionTestUtils.setField(itemServiceImpl, "updateProductInDimensionEvent", Boolean.TRUE);
    product.setMissingFields(Collections.singleton(com.gdn.x.product.enums.Constants.DIMENSIONS_MISSING));
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setSynchronized(true);
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.LENGTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WIDTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.HEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(
        new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.SHIPPING_WEIGHT_CHANGE,
            Double.toString(20.0), Double.toString(20.0), null, item.getProductSku(),
            item.getGeneratedItemName(), item.getPickupPointCode(), true));
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku()))
        .thenReturn(product);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, 10.0, 10.0, 10.0, 10.0, new ArrayList<>());
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku());
    Mockito.verify(productService)
        .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    Mockito.verify(kafkaProducer).send(Mockito.any(),
        auditTrailListResponseArgumentCaptor.capture());
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), Mockito.anyList(),
        eq(StringUtils.EMPTY));
    AuditTrailListResponse capturedResponse = auditTrailListResponseArgumentCaptor.getValue();
    assertEquals(WAREHOUSE_USERNAME, capturedResponse.getChangedBy());
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(20.0, itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void updateItemDimensionsAndUpcCodeSwitchOnDimensionMissingFields2Test() {
    ReflectionTestUtils.setField(itemServiceImpl, "updateProductInDimensionEvent", Boolean.TRUE);
    product.setMissingFields(Collections.singleton(com.gdn.x.product.enums.Constants.DESCRIPTION_MISSING));
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setSynchronized(true);
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.LENGTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WIDTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.HEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(
        new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.SHIPPING_WEIGHT_CHANGE,
            Double.toString(20.0), Double.toString(20.0), null, item.getProductSku(),
            item.getGeneratedItemName(), item.getPickupPointCode(), true));
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku()))
        .thenReturn(product);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, 10.0, 10.0, 10.0, 10.0, new ArrayList<>());
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku());
    Mockito.verify(productService)
        .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    Mockito.verify(kafkaProducer).send(Mockito.any(),
        auditTrailListResponseArgumentCaptor.capture());
    AuditTrailListResponse capturedResponse = auditTrailListResponseArgumentCaptor.getValue();
    assertEquals(WAREHOUSE_USERNAME, capturedResponse.getChangedBy());
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(20.0, itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void updateItemDimensionsAndUpcCodeSwitchOnDimensionMissingFieldsNullTest() {
    ReflectionTestUtils.setField(itemServiceImpl, "updateProductInDimensionEvent", Boolean.TRUE);
    product.setDimensionsMissing(false);
    product.setMissingFields(null);
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setUpcCode(UPC_CODE);
    item.setSynchronized(true);
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.LENGTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WIDTH_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.HEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WEIGHT_CHANGE,
        Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(
        new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.SHIPPING_WEIGHT_CHANGE,
            Double.toString(20.0), Double.toString(20.0), null, item.getProductSku(),
            item.getGeneratedItemName(), item.getPickupPointCode(), true));
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku()))
        .thenReturn(product);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, 10.0, 10.0, 10.0, 10.0, List.of(UPC_CODE_NEW, USERNAME));
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku());
    Mockito.verify(productService)
        .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    Assertions.assertEquals(UPC_CODE_NEW, itemsCaptor.getValue().getFirst().getUpcCode());
    Mockito.verify(kafkaProducer).send(Mockito.any(),
        auditTrailListResponseArgumentCaptor.capture());
    AuditTrailListResponse capturedResponse = auditTrailListResponseArgumentCaptor.getValue();
    assertEquals(WAREHOUSE_USERNAME, capturedResponse.getChangedBy());
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(20.0, itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void updateItemDimensionsAndUpcCodeSwitchNoDimensionMissingFieldsTest() {
    ReflectionTestUtils.setField(itemServiceImpl, "updateProductInDimensionEvent", Boolean.TRUE);
    product.setMissingFields(Collections.singleton(com.gdn.x.product.enums.Constants.DIMENSIONS_MISSING));
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setUpcCode(UPC_CODE);
    item.setSynchronized(true);
    item.setLength(10.0);
    item.setWidth(10.0);
    item.setWeight(10.0);
    item.setHeight(10.0);
    item.setShippingWeight(20.0);
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku()))
        .thenReturn(product);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, 10.0, 10.0, 10.0, 10.0, List.of(UPC_CODE));
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService)
        .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(20.0, itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void updateItemDimensionsAndUpcCodeSwitchNoDesceriptionMissingFieldsTest() {
    ReflectionTestUtils.setField(itemServiceImpl, "updateProductInDimensionEvent", Boolean.TRUE);
    product.setMissingFields(Collections.singleton(com.gdn.x.product.enums.Constants.DESCRIPTION_MISSING));
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setSynchronized(true);
    item.setLength(10.0);
    item.setWidth(10.0);
    item.setWeight(10.0);
    item.setHeight(10.0);
    item.setShippingWeight(20.0);
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, item.getProductSku()))
        .thenReturn(product);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, 10.0, 10.0, 10.0, 10.0, new ArrayList<>());
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService)
        .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(20.0, itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void updateItemDimensionsAndUpcCodeInvalidTest() {
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setSynchronized(true);
    item.setLength(-1.0);
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    auditTrailDtoList.add(
        new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.LENGTH_CHANGE,
            Double.toString(-1.0), Double.toString(10.0), null, item.getProductSku(),
            item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(
        new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WIDTH_CHANGE,
            Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(), item.getGeneratedItemName(),
            item.getPickupPointCode(), true));
    auditTrailDtoList.add(
        new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.HEIGHT_CHANGE,
            Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(), item.getGeneratedItemName(),
            item.getPickupPointCode(), true));
    auditTrailDtoList.add(
        new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WEIGHT_CHANGE,
            Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(), item.getGeneratedItemName(),
            item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(),
        com.gdn.x.product.enums.Constants.SHIPPING_WEIGHT_CHANGE, Double.toString(20.0), Double.toString(20.0), null,
        item.getProductSku(), item.getGeneratedItemName(), item.getPickupPointCode(), true));
    AuditTrailListResponse auditTrailListResponse =
        new AuditTrailListResponse(auditTrailDtoList, DEFAULT, DEFAULT_USERNAME, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID,
            true);
    Mockito.when(
            this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, -1.0, 10.0, 10.0, 10.0, new ArrayList<>());
    Mockito.verify(this.itemRepository)
        .findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService)
        .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    verify(kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
        any(AuditTrailListResponse.class));
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(20.0, itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void updateItemDimensionsAndUpcCodeFalseTest() {
    List<Item> itemList = new ArrayList<>();
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setSynchronized(true);
    item.setLength(item.getLength());
    item.setWeight(item.getWeight());
    item.setWidth(item.getWidth());
    item.setHeight(item.getHeight());
    item.setShippingWeight(item.getShippingWeight());
    itemList.add(item);
    Mockito.when(
            this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
            Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble()))
        .thenReturn(item.getShippingWeight());
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, LENGTH, WEIGHT, WIDTH, HEIGHT, new ArrayList<>());
    Mockito.verify(this.itemRepository)
        .findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService)
        .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(item.getShippingWeight(), itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void updateItemDimensionsAndUpcCodeUnsyncTest() {
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, 10.0, 10.0, 10.0, 10.0, new ArrayList<>());
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    ModelHelper modelHelper = new ModelHelper();
    this.item = modelHelper.setupItem();
    this.items = new ArrayList<Item>(Arrays.asList(this.item));
    item.setPickupPointCode(PICKUP_POINT_CODE);
    this.product = modelHelper.setupProduct();
    this.products = Arrays.asList(this.product);

    this.price =
        new Price(ItemServiceTest.CURRENCY, ItemServiceTest.OFFER_PRICE,
            ItemServiceTest.LIST_PRICE, ItemServiceTest.CHANNEL, ItemServiceTest.LAST_UPDATED_BY,
            ItemServiceTest.LAST_UPDATED_DATE);
    this.item.getPrice().add(this.price);
    this.productDetailResponse = new ProductDetailResponse();
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(ITEM_CODE);
    productItemResponse.setSourceItemCode(SOURCE_ITEM_CODE);
    productItemResponse.setContentChanged(true);
    productItemResponse.setProductItemAttributeValueResponses(new ArrayList<>());
    ITEM_RESPONSES.add(productItemResponse);
    this.productDetailResponse.setProductItemResponses(ItemServiceTest.ITEM_RESPONSES);
    this.productDetailResponse.setProductCode(ModelHelper.PRODUCT_CODE);
    this.productDetailResponse.setLength(10.0);
    this.productDetailResponse.setWeight(10.0);
    this.productDetailResponse.setWidth(10.0);
    this.productDetailResponse.setHeight(10.0);
    this.productDetailResponse.setShippingWeight(10.0);
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(PRODUCT_CATEGORY_CODE);
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setCategory(categoryResponse);
    this.productDetailResponse.setProductCategoryResponses(Arrays.asList(productCategoryResponse));
    when(this.channelService.getDefaultChannel()).thenReturn(ItemServiceTest.CHANNEL);

    preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(PREORDER_TYPE);
    preOrder.setPreOrderValue(PREORDER_VALUE);

    itemPickupPoints = Arrays.asList(new ItemPickupPoint());
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setBuyable(true);
    itemPickupPoints.get(0).setItemViewConfig(Collections.singleton(itemViewConfig));


    when(this.saveOperationService.saveProductAndItemsAndPickupPoint(Mockito.any(Product.class),
            Mockito.anyList())).thenReturn(new ProductAndItemsVO());

    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
            Mockito.anyList())).thenReturn(Arrays.asList(businessPartnerPickupPoint));

    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    Mockito.when(this.productHelperService.setItemDetail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.any(ItemVo.class))).thenReturn(itemVo);
    profileResponse.setCompany(new CompanyDTO());
    List<String> salesChannel = new ArrayList<>();
    salesChannel.add(B2C_SELLER_CHANNEL);
    salesChannel.add(B2B_SELLER_CHANNEL);
    businessPartner.setSalesChannel(salesChannel);
    ReflectionTestUtils.setField(itemServiceImpl, "validateDimensionSwitch", false);
  }

  private ProductItemsVo generateProductItemVo(Product product, Item item, ItemPickupPoint itemPickupPoint) {
    ProductItemsVo productItemsVo = new ProductItemsVo();
    productItemsVo.setProductVo(Objects.nonNull(product) ? gdnMapper.deepCopy(product, ProductVo.class) : null);
    productItemsVo.setItemVoList(Objects.nonNull(item) ? Arrays.asList(gdnMapper.deepCopy(item, ItemVo.class)) : null);
    if (CollectionUtils.isNotEmpty(productItemsVo.getItemVoList())) {
      productItemsVo.getItemVoList().get(0).setItemPickupPointVoList(new ArrayList<>());
      productItemsVo.getItemVoList().get(0).setItemPickupPointVoList(Objects.nonNull(itemPickupPoint) ?
          Arrays.asList(gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class)) :
          null);
    }
    return productItemsVo;
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.itemCacheableService);
    verifyNoMoreInteractions(this.productHelperService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.saveOperationService);
    verifyNoMoreInteractions(this.channelService);
    verifyNoMoreInteractions(this.itemPriceService);
    verifyNoMoreInteractions(this.formulaUtil);
    verifyNoMoreInteractions(this.cacheEvictHelperService);
    verifyNoMoreInteractions(this.itemRepository);
    verifyNoMoreInteractions(this.itemHelperServiceImpl);
    verifyNoMoreInteractions(this.skuValidator);
    verifyNoMoreInteractions(this.itemRepository);
    verifyNoMoreInteractions(this.saveAndPublishService);
  }

  @Test
  public void getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalseTest() {
    Mockito.when(this.itemRepository.findFirstItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(STORE_ID,
        MERCHANT_CODE, MERCHANT_SKU)).thenReturn(item);
    itemServiceImpl.getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE, MERCHANT_SKU);
    Mockito.verify(this.itemRepository)
        .findFirstItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE, MERCHANT_SKU);
  }

  @Test
  public void getOneItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSkuTest() {
    Mockito.when(this.itemRepository.findFirstItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(STORE_ID,
        MERCHANT_CODE, Arrays.asList(PRODUCT_SKU), MERCHANT_SKU)).thenReturn(item);
    itemServiceImpl.getOneItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(STORE_ID,
        MERCHANT_CODE, Arrays.asList(PRODUCT_SKU), MERCHANT_SKU);
    Mockito.verify(this.itemRepository)
        .findFirstItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(STORE_ID,
            MERCHANT_CODE,Arrays.asList(PRODUCT_SKU), MERCHANT_SKU);
  }

  @Test
  public void getbasicItemDetailsL5NullTest() {
    Mockito.when(this.itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(null);
    try {
      assertThrows(Exception.class, () ->  itemServiceImpl.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE));
    } finally {
      Mockito.verify(this.itemPickupPointService).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
          ITEM_SKU, PICKUP_POINT_CODE);
    }
  }

  @Test
  public void getbasicItemDetailsL4NullTest() {
    Mockito.when(this.itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(new ItemPickupPoint());
    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU)).thenReturn(null);
    try {
      assertThrows(Exception.class, () ->  itemServiceImpl.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE));
    } finally {
      Mockito.verify(this.itemPickupPointService).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
          ITEM_SKU, PICKUP_POINT_CODE);
      Mockito.verify(this.cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void getBasicItemDetailsTest() {
    item.setShippingWeight(SHIPPING_WT);
    item.setDangerousLevel(DANGEROUS_LEVEL);
    Mockito.when(this.itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(itemPickupPoints.get(0));
    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU)).thenReturn(item);
    BasicItemDTO basicItemDTO = itemServiceImpl.getbasicItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointService).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(this.cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    assertEquals(ITEM_SKU, basicItemDTO.getItemSku());
  }

  @Test
  public void updateItemDimensionsAndUpcCodeTestLengthZero() {
    ReflectionTestUtils.setField(itemServiceImpl, "validateDimensionSwitch", true);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, INVALID_DIMENSION, WIDTH, WEIGHT, HEIGHT, new ArrayList<>());
  }

  @Test
  public void updateItemDimensionsAndUpcCodeTestWidthZero() {
    ReflectionTestUtils.setField(itemServiceImpl, "validateDimensionSwitch", true);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, LENGTH, INVALID_DIMENSION, WEIGHT, HEIGHT, new ArrayList<>());
  }

  @Test
  public void updateItemDimensionsAndUpcCodeTestWeightZero() {
    ReflectionTestUtils.setField(itemServiceImpl, "validateDimensionSwitch", true);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, LENGTH, WIDTH, INVALID_DIMENSION, HEIGHT, new ArrayList<>());
  }

  @Test
  public void updateItemDimensionsAndUpcCodeTestHeightZero() {
    ReflectionTestUtils.setField(itemServiceImpl, "validateDimensionSwitch", true);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, LENGTH, WIDTH, WEIGHT, INVALID_DIMENSION, new ArrayList<>());
  }

  @Test
  public void updateItemDimensionsAndUpcCodeTestDecimalWeight() {
    ReflectionTestUtils.setField(itemServiceImpl, "validateDimensionSwitch", true);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, LENGTH, WIDTH, 0.00005, HEIGHT, new ArrayList<>());
  }

  @Test
  public void updateItemDimensionsAndUpcCodeTestDecimalHeight() {
    ReflectionTestUtils.setField(itemServiceImpl, "validateDimensionSwitch", true);
    item.setCategoryCode(PRODUCT_CATEGORY_CODE);
    item.setSynchronized(true);
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.LENGTH_CHANGE,
      Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
      item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WIDTH_CHANGE,
      Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
      item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.HEIGHT_CHANGE,
      Double.toString(10.0), Double.toString(0.0006), null, item.getProductSku(),
      item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.WEIGHT_CHANGE,
      Double.toString(10.0), Double.toString(10.0), null, item.getProductSku(),
      item.getGeneratedItemName(), item.getPickupPointCode(), true));
    auditTrailDtoList.add(
      new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), com.gdn.x.product.enums.Constants.SHIPPING_WEIGHT_CHANGE,
        Double.toString(20.0), Double.toString(20.0), null, item.getProductSku(),
        item.getGeneratedItemName(), item.getPickupPointCode(), true));
    AuditTrailListResponse auditTrailListResponse =
      new AuditTrailListResponse(auditTrailDtoList, DEFAULT, DEFAULT_USERNAME, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID,
        true);
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE))
      .thenReturn(Arrays.asList(item));
    Mockito.when(productService.generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE),
      Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble())).thenReturn(20.0);
    itemServiceImpl.updateItemDimensionsAndUpcCode(ITEM_CODE, 10.0, 10.0, 10.0, 0.0006, new ArrayList<>());
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, ITEM_CODE);
    Mockito.verify(productService)
      .generateShippingWeight(eq(Constants.DEFAULT_STORE_ID), eq(PRODUCT_CATEGORY_CODE), Mockito.anyDouble(), Mockito.anyDouble(),
        Mockito.anyDouble(), Mockito.anyDouble());
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    verify(kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
      Mockito.any(AuditTrailListResponse.class));
    assertEquals(PRODUCT_CATEGORY_CODE, itemsCaptor.getValue().get(0).getCategoryCode());
    assertEquals(20.0, itemsCaptor.getValue().get(0).getShippingWeight(), 0);
  }

  @Test
  public void addItemCreateMasterSkuTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "categoriesToGenerateMasterSkuInFlow2ProductCreation", PRODUCT_CATEGORY_CODE);
    items.get(0).setWholesalePriceExists(true);
    itemPickupPoints.get(0).setPrice(ImmutableSet.of(price));
    itemPickupPoints.get(0).setWholesalePriceExists(true);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    productDetailResponse.getProductItemResponses().forEach(item -> item.setContentChanged(false));
    HashMap<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    masterDataItemMap.put(ModelHelper.ITEM_CODE, masterDataItem);
    Item existingItem = new Item();
    existingItem.setItemSku(MASTER_SKU);
    existingItem.setPristineDataItem(new PristineDataItem());
    Page<Item> itemPage1 = new PageImpl<>(Arrays.asList(existingItem), PageRequest.of(0, 1), 2);
    Page<Item> itemPage2 = new PageImpl<>(Arrays.asList(item), PageRequest.of(0, 10), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    productItemsVo.getItemVoList().get(0).getItemPickupPointVoList().get(0).setWholesalePriceActivated(true);
    productDetailResponse.setCategoryCodes(Arrays.asList(PRODUCT_CATEGORY_CODE));

    Mockito.when(this.productHelperService.setItemDetail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.any(ItemVo.class))).thenReturn(productItemsVo.getItemVoList().get(0));
    when(itemRepository
        .findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(isNull(), anyString(),
            any(Pageable.class))).thenReturn(itemPage1).thenReturn(itemPage2);
    when(
        this.objectConverterService.convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
            ModelHelper.PRODUCT_CODE)).thenReturn(masterDataItemMap);
    when(
        this.productHelperService.addItemAttributeToProductAttribute(this.product,
            ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES)).thenReturn(
        this.product);

    List<ItemVo> result = this.itemServiceImpl
        .addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            ItemServiceTest.PRODUCT_SKU, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
            businessPartner);

    verify(this.channelService).getDefaultChannel();
    Mockito.verify(productHelperService, Mockito.times(this.items.size())).setItemDetail(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.any(ItemVo.class));
    verify(this.objectConverterService).convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
        ModelHelper.PRODUCT_CODE);
    verify(this.productHelperService).addItemAttributeToProductAttribute(this.product,
        ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    Mockito.verify(itemRepository, times(2))
        .findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(isNull(), anyString(),
            any(Pageable.class));
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    verify(saveOperationService).saveItemsWithoutUpdatingSolr(itemsCaptor.capture());
    assertNotNull(result);
    Assertions.assertNull(result.get(0).getPristineDataItem());
    assertEquals(true, result.get(0).isWholesalePriceExists());
    assertEquals(com.gdn.x.product.enums.Constants.WHOLESALE_PRICE,
        result.get(0).getActivePromoBundlings().stream().findFirst().get());
    assertEquals(MASTER_SKU, itemsCaptor.getValue().get(0).getMasterSku());
    assertEquals(MASTER_SKU, itemsCaptor.getValue().get(1).getMasterSku());
    assertEquals(ItemChangeEventType.MASTER_SKU_UPDATE,
        itemsCaptor.getValue().get(1).getItemChangeEventTypes().get(0));
  }

  @Test
  public void addItemCreateMasterSkuGenerateMasterSkuFalseTest() throws Exception {
    items.get(0).setWholesalePriceExists(true);
    itemPickupPoints.get(0).setPrice(ImmutableSet.of(price));
    itemPickupPoints.get(0).setWholesalePriceExists(true);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    productDetailResponse.getProductItemResponses().forEach(item -> item.setContentChanged(false));
    HashMap<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    masterDataItemMap.put(ModelHelper.ITEM_CODE, masterDataItem);
    Item existingItem = new Item();
    existingItem.setItemSku(MASTER_SKU);
    existingItem.setPristineDataItem(new PristineDataItem());
    Page<Item> itemPage1 = new PageImpl<>(Arrays.asList(existingItem), PageRequest.of(0, 1), 2);
    Page<Item> itemPage2 = new PageImpl<>(Arrays.asList(item), PageRequest.of(0, 10), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    productItemsVo.getItemVoList().get(0).getItemPickupPointVoList().get(0).setWholesalePriceActivated(true);
    productDetailResponse.setCategoryCodes(Arrays.asList(PRODUCT_CATEGORY_CODE));

    Mockito.when(this.productHelperService.setItemDetail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.any(ItemVo.class))).thenReturn(productItemsVo.getItemVoList().get(0));
    when(itemRepository
        .findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(isNull(), anyString(),
            any(Pageable.class))).thenReturn(itemPage1).thenReturn(itemPage2);
    when(
        this.objectConverterService.convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
            ModelHelper.PRODUCT_CODE)).thenReturn(masterDataItemMap);
    when(
        this.productHelperService.addItemAttributeToProductAttribute(this.product,
            ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES)).thenReturn(
        this.product);

    List<ItemVo> result = this.itemServiceImpl
        .addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            ItemServiceTest.PRODUCT_SKU, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
            businessPartner);

    verify(this.channelService).getDefaultChannel();
    Mockito.verify(productHelperService, Mockito.times(this.items.size())).setItemDetail(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.any(ItemVo.class));
    verify(this.objectConverterService).convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
        ModelHelper.PRODUCT_CODE);
    verify(this.productHelperService).addItemAttributeToProductAttribute(this.product,
        ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    Mockito.verify(itemRepository, times(2))
        .findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(isNull(), anyString(),
            any(Pageable.class));
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    assertNotNull(result);
    Assertions.assertNull(result.get(0).getPristineDataItem());
    assertEquals(true, result.get(0).isWholesalePriceExists());
    assertEquals(com.gdn.x.product.enums.Constants.WHOLESALE_PRICE,
        result.get(0).getActivePromoBundlings().stream().findFirst().get());
    Assertions.assertNull(itemVoListArgumentCaptor.getValue().get(0).getMasterSku());
  }

  @Test
  public void addItemCreateMasterSkuEmptySkusTest() throws Exception {
    items.get(0).setWholesalePriceExists(true);
    itemPickupPoints.get(0).setPrice(ImmutableSet.of(price));
    itemPickupPoints.get(0).setWholesalePriceExists(true);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    productDetailResponse.getProductItemResponses().forEach(item -> item.setContentChanged(false));
    HashMap<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    masterDataItemMap.put(ModelHelper.ITEM_CODE, masterDataItem);
    Item existingItem = new Item();
    existingItem.setItemSku(MASTER_SKU);
    existingItem.setPristineDataItem(new PristineDataItem());
    Page<Item> itemPage1 = new PageImpl<>(Arrays.asList(), PageRequest.of(0, 1), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoints.get(0));
    productItemsVo.getItemVoList().get(0).getItemPickupPointVoList().get(0).setWholesalePriceActivated(true);
    productDetailResponse.setCategoryCodes(Arrays.asList(PRODUCT_CATEGORY_CODE));

    Mockito.when(this.productHelperService.setItemDetail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.any(ItemVo.class))).thenReturn(productItemsVo.getItemVoList().get(0));
    when(itemRepository
        .findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(isNull(), anyString(),
            any(Pageable.class))).thenReturn(itemPage1);
    when(
        this.objectConverterService.convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
            ModelHelper.PRODUCT_CODE)).thenReturn(masterDataItemMap);
    when(
        this.productHelperService.addItemAttributeToProductAttribute(this.product,
            ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES)).thenReturn(
        this.product);

    List<ItemVo> result = this.itemServiceImpl
        .addItems(ItemServiceTest.STORE_ID, ItemServiceTest.REQUEST_ID, ItemServiceTest.USERNAME,
            ItemServiceTest.PRODUCT_SKU, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
            businessPartner);

    verify(this.channelService).getDefaultChannel();
    Mockito.verify(productHelperService, Mockito.times(this.items.size())).setItemDetail(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.any(ItemVo.class));
    verify(this.objectConverterService).convertToMasterDataItems(ItemServiceTest.ITEM_RESPONSES,
        ModelHelper.PRODUCT_CODE);
    verify(this.productHelperService).addItemAttributeToProductAttribute(this.product,
        ItemServiceTest.ITEM_SKU, ItemServiceTest.MASTER_DATA_ITEM_ATTRIBUTES);
    Mockito.verify(itemRepository)
        .findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(isNull(), anyString(),
            any(Pageable.class));
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    assertNotNull(result);
    Assertions.assertNull(result.get(0).getPristineDataItem());
    assertEquals(true, result.get(0).isWholesalePriceExists());
    assertEquals(com.gdn.x.product.enums.Constants.WHOLESALE_PRICE,
        result.get(0).getActivePromoBundlings().stream().findFirst().get());
    Assertions.assertNull(itemVoListArgumentCaptor.getValue().get(0).getMasterSku());
  }

  @Test
  void getL4ItemListByProductSkuLite_shouldReturnMappedPage() {
    // given
    String storeId = "STORE_1";
    int page = 0;
    int size = 10;
    Set<String> productSkus = Set.of("PROD_1", "PROD_2");
    Pageable pageable = PageRequest.of(page, size);

    Item item = new Item();
    item.setGeneratedItemName("Item Name");
    item.setItemSku("ITEM_SKU_1");
    item.setProductSku("PROD_1");
    item.setUpcCode("UPC123");

    Page<Item> itemPage = new PageImpl<>(List.of(item), pageable, 1);

    when(itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, productSkus, pageable))
        .thenReturn(itemPage);
    when(systemParameterService.findValueByStoreIdAndVariable(storeId,
            SystemParameterNames.PRODUCT_SKU_LIST_LIMIT_FOR_BASIC_DETAILS))
        .thenReturn(new SystemParameter("10001","","100",""));

    // when
    Page<ItemBasicL4Response> result =
        itemServiceImpl.getL4ItemListByProductSkuLite(productSkus, storeId, page, size);

    // then
    assertNotNull(result);
    assertEquals(1, result.getTotalElements());
    assertEquals(page, result.getNumber());
    assertEquals(size, result.getSize());

    List<ItemBasicL4Response> content = result.getContent();
    assertEquals(1, content.size());

    ItemBasicL4Response response = content.get(0);
    assertEquals("Item Name", response.getItemName());
    assertEquals("ITEM_SKU_1", response.getItemSku());
    assertEquals("PROD_1", response.getProductSku());
    assertEquals("UPC123", response.getUpcCode());

    verify(itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, productSkus, pageable);
    verifyNoMoreInteractions(itemRepository);
  }

  @Test
  void getL4ItemListByProductSkuLite_shouldThrowWhenProductSkusNull() {
    assertThrows(ApplicationRuntimeException.class,
        () -> itemServiceImpl.getL4ItemListByProductSkuLite(null, "STORE_1", 0, 10));
  }

  @Test
  void getL4ItemListByProductSkuLite_shouldReturnMappedPage_more_sku() {
    // given
    String storeId = "STORE_1";
    int page = 0;
    int size = 10;
    Set<String> productSkus = Set.of("PROD_1", "PROD_2");
    Pageable pageable = PageRequest.of(page, size);

    Item item = new Item();
    item.setGeneratedItemName("Item Name");
    item.setItemSku("ITEM_SKU_1");
    item.setProductSku("PROD_1");
    item.setUpcCode("UPC123");

    Page<Item> itemPage = new PageImpl<>(List.of(item), pageable, 1);

    when(itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, productSkus, pageable))
        .thenReturn(itemPage);
    when(systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.PRODUCT_SKU_LIST_LIMIT_FOR_BASIC_DETAILS))
        .thenReturn(new SystemParameter("10001","","1",""));

    // when
    assertThrows(ApplicationRuntimeException.class,()->
        itemServiceImpl.getL4ItemListByProductSkuLite(productSkus, storeId, page, size));


  }

}
