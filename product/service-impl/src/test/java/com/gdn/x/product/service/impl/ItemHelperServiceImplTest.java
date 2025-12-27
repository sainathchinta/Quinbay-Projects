package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.util.ReflectionTestUtils.setField;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PriceHistory;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.AddVariantRequestVo;
import com.gdn.x.product.model.vo.OfflineItemPriceVO;
import com.gdn.x.product.model.vo.ProductAttributeDetailVo;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PriceHistoryService;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

/**
 * This unit test class template is the work of maria.o.a.sunaryo and felix.w.wijaya as
 * participation in Blibli Hackathon #1. Generated on 24 Jun 2016 22:36:27
 */
public class ItemHelperServiceImplTest {

  private static final String NOT_FOUND_CHANNEL = "not-found-channel";
  private static final String OTHER_CHANNEL = "other-channel";
  private static final ItemViewConfig OTHER_CHANNEL_OBJ = new ItemViewConfig(true, true,
      ItemHelperServiceImplTest.OTHER_CHANNEL, null, null);
  private static final String DEFAULT_CHANNEL = "default-channel";
  private static final ItemViewConfig DEFAULT_CHANNEL_OBJ = new ItemViewConfig(true, true,
      ItemHelperServiceImplTest.DEFAULT_CHANNEL, null, null);
  private static final String USERNAME = "username";
  private static final String ITEM_SKU = "item-sku";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String DELIMITER = "#_#";
  private static final String CATEGORY_NAME = "category-name";
  private static final String CATEGORY_CODE = "category-code";
  private static final String PRODUCT_CODE = "product-code";
  private static final String REQUEST_ID = "request-id";
  private static final String STORE_ID = "storeId";
  private static final String PRISTINE_ID = "pristineId";

  @InjectMocks
  private ItemHelperServiceImpl itemHelperServiceImpl;

  @Mock
  private ChannelService channelService;

  @Mock
  private PriceHistoryService priceHistoryService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private CachedService cachedService;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  private Set<ItemViewConfig> itemViewConfigs;
  private Set<Price> prices;
  private CategoryResponse categoryResponse;
  private PristineDataItem pristineDataItem;

  @Test
  public void getItemViewConfigsTestWithBlankChannel() throws ApplicationRuntimeException {
    when(this.channelService.getDefaultChannel()).thenReturn(
        ItemHelperServiceImplTest.DEFAULT_CHANNEL);
    ItemViewConfig result =
        this.itemHelperServiceImpl.getItemViewConfigs(this.itemViewConfigs, StringUtils.EMPTY);
    assertEquals(result, ItemHelperServiceImplTest.DEFAULT_CHANNEL_OBJ);
    verify(this.channelService).getDefaultChannel();
  }

  @Test
  public void getItemViewConfigsTestWithFoundChannel() throws ApplicationRuntimeException {
    ItemViewConfig result =
        this.itemHelperServiceImpl.getItemViewConfigs(this.itemViewConfigs,
            ItemHelperServiceImplTest.OTHER_CHANNEL);
    assertEquals(result, ItemHelperServiceImplTest.OTHER_CHANNEL_OBJ);
  }

  @Test
  public void getItemViewConfigsTestWithNotFoundChannel() throws ApplicationRuntimeException {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemHelperServiceImpl.getItemViewConfigs(this.itemViewConfigs,
        ItemHelperServiceImplTest.NOT_FOUND_CHANNEL));
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.itemViewConfigs = new HashSet<ItemViewConfig>();
    this.itemViewConfigs.add(ItemHelperServiceImplTest.DEFAULT_CHANNEL_OBJ);
    this.itemViewConfigs.add(ItemHelperServiceImplTest.OTHER_CHANNEL_OBJ);
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(800);
    price.setListPrice(1000);
    price.setChannel(ItemHelperServiceImplTest.DEFAULT_CHANNEL);
    prices.add(price);

    setField(this.itemHelperServiceImpl, "solrStringDelimiter", DELIMITER);

    categoryResponse = new CategoryResponse();
    categoryResponse.setName(CATEGORY_NAME);
    categoryResponse.setCategoryCode(CATEGORY_CODE);

    pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.channelService);
    verifyNoMoreInteractions(this.priceHistoryService);
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.itemRepository, this.itemPickupPointService);

  }

  @Test
  public void getDiscountPriceTest() {
    Date date = new Date();

    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(10);
    discountPrice.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice.setEndDateTime(new Date(date.getTime() + 10000));
    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));

    double price1 = itemHelperServiceImpl.getDiscountPrice(price);
    assertEquals(10,price1,0);
  }

  @Test
  public void getDiscountPriceTest_ExpiredDate() {
    Date date = new Date();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(10);
    discountPrice.setStartDateTime(new Date(date.getTime() + 10000));
    discountPrice.setEndDateTime(new Date(date.getTime() - 10000));
    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    double finalDiscountPrice = itemHelperServiceImpl.getDiscountPrice(price);
    assertEquals(0,finalDiscountPrice,0);
  }

  @Test
  public void setItemPriceByChannel(){
    Item item = new Item();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(900);
    price.setChannel(ItemHelperServiceImplTest.DEFAULT_CHANNEL);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(100);
    price.setListOfDiscountPrices(Collections.singletonList(discountPrice));
    itemPrices.add(price);
    item.setPrice(itemPrices);
    Set<Price> updatedPrices = new HashSet<>();
    Price updatedPrice = new Price();
    updatedPrice.setChannel(ItemHelperServiceImplTest.DEFAULT_CHANNEL);
    updatedPrice.setListPrice(800);
    updatedPrices.add(updatedPrice);
    this.itemHelperServiceImpl
        .setItemPriceByChannel(item, updatedPrices, ItemHelperServiceImplTest.USERNAME);
    Mockito.verify(this.priceHistoryService).savePriceHistory(Mockito.isNull());
    Mockito.verify(this.objectConverterService).convertToPriceHistory(price, item.getItemSku());
    assertEquals(discountPrice.getDiscountPrice(),
        updatedPrice.getListOfDiscountPrices().get(0).getDiscountPrice(), 0.0);
  }

  @Test
  public void convertOfflineItemPrices() {

    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ITEM_SKU);
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);
    productAndItemSolr.setOfflinePrices(
        Arrays.asList("PP-3001143#_#700000.99#_#650000.99", "PP-1001234#_#15000.00#_#10000.00"));

    final List<OfflineItemPriceVO> results =
        this.itemHelperServiceImpl.convertOfflineItemPrices(null, productAndItemSolr);
    final List<OfflineItemPriceVO> results2 =
        this.itemHelperServiceImpl.convertOfflineItemPrices(null, null, productAndItemSolr);

    OfflineItemPriceVO offlineItemPriceVO1 = new OfflineItemPriceVO();
    offlineItemPriceVO1.setPickupPointCode("PP-3001143");
    offlineItemPriceVO1.setListPrice(700000.99);
    offlineItemPriceVO1.setOfferPrice(650000.99);

    OfflineItemPriceVO offlineItemPriceVO2 = new OfflineItemPriceVO();
    offlineItemPriceVO2.setPickupPointCode("PP-1001234");
    offlineItemPriceVO2.setListPrice(15000.00);
    offlineItemPriceVO2.setOfferPrice(10000.00);

    assertNotNull(results);
    assertEquals(2, results.size());
    assertTrue(results.contains(offlineItemPriceVO1));
    assertTrue(results.contains(offlineItemPriceVO2));
    assertNotNull(results2);
    assertEquals(2, results2.size());
    assertTrue(results2.contains(offlineItemPriceVO1));
    assertTrue(results2.contains(offlineItemPriceVO2));
  }

  @Test
  public void convertOfflineItemPrices_pickupPointCodeFilterUsed() {

    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ITEM_SKU);
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);
    productAndItemSolr.setOfflinePrices(
        Arrays.asList("PP-3001143#_#700000.99#_#650000.99", "PP-1001234#_#15000.00#_#10000.00"));

    final List<OfflineItemPriceVO> results =
        this.itemHelperServiceImpl.convertOfflineItemPrices("PP-1001234", productAndItemSolr);

    OfflineItemPriceVO offlineItemPriceVO2 = new OfflineItemPriceVO();
    offlineItemPriceVO2.setPickupPointCode("PP-1001234");
    offlineItemPriceVO2.setListPrice(15000.00);
    offlineItemPriceVO2.setOfferPrice(10000.00);

    assertNotNull(results);
    assertEquals(1, results.size());
    assertTrue(results.contains(offlineItemPriceVO2));
  }

  @Test
  public void convertOfflineItemPrices_pickupPointCodesFilterUsed() {

    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ITEM_SKU);
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);
    productAndItemSolr.setOfflinePrices(
        Arrays.asList("PP-3001143#_#700000.99#_#650000.99", "PP-1001234#_#15000.00#_#10000.00"));

    final List<OfflineItemPriceVO> results = this.itemHelperServiceImpl
        .convertOfflineItemPrices(null, Collections.singletonList("PP-1001234"),
            productAndItemSolr);
    final List<OfflineItemPriceVO> results1 = this.itemHelperServiceImpl
        .convertOfflineItemPrices("PP-1001234,PP-1001234", Collections.singletonList("PP-1001234"),
            productAndItemSolr);

    OfflineItemPriceVO offlineItemPriceVO2 = new OfflineItemPriceVO();
    offlineItemPriceVO2.setPickupPointCode("PP-1001234");
    offlineItemPriceVO2.setListPrice(15000.00);
    offlineItemPriceVO2.setOfferPrice(10000.00);

    assertNotNull(results);
    assertEquals(1, results.size());
    assertTrue(results.contains(offlineItemPriceVO2));
  }

  @Test
  public void convertOfflineItemPrices_pickupPointCodeFilterAndpickupPointCodesFilterUsed() {

    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ITEM_SKU);
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);
    productAndItemSolr.setOfflinePrices(Arrays
        .asList("PP-3001143#_#700000.99#_#650000.99", "PP-1001234#_#15000.00#_#10000.00",
            "PP-9923115#_#20000.00#_#15000.00"));

    final List<OfflineItemPriceVO> results = this.itemHelperServiceImpl
        .convertOfflineItemPrices("PP-3001143", Collections.singletonList("PP-1001234"),
            productAndItemSolr);

    OfflineItemPriceVO offlineItemPriceVO1 = new OfflineItemPriceVO();
    offlineItemPriceVO1.setPickupPointCode("PP-3001143");
    offlineItemPriceVO1.setListPrice(700000.99);
    offlineItemPriceVO1.setOfferPrice(650000.99);

    OfflineItemPriceVO offlineItemPriceVO2 = new OfflineItemPriceVO();
    offlineItemPriceVO2.setPickupPointCode("PP-1001234");
    offlineItemPriceVO2.setListPrice(15000.00);
    offlineItemPriceVO2.setOfferPrice(10000.00);

    assertNotNull(results);
    assertEquals(2, results.size());
    assertTrue(results.contains(offlineItemPriceVO1));
    assertTrue(results.contains(offlineItemPriceVO2));
  }

  @Test
  public void convertOfflineItemPrices_withoutListPrice() {
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ITEM_SKU);
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);
    productAndItemSolr.setOfflinePrices(
      Arrays.asList("PP-3001143#_#650000.99", "PP-1001234#_#10000.00"));

    final List<OfflineItemPriceVO> results =
      this.itemHelperServiceImpl.convertOfflineItemPrices("PP-1001234,PP-3001143", productAndItemSolr);

    OfflineItemPriceVO offlineItemPriceVO2 = new OfflineItemPriceVO();
    offlineItemPriceVO2.setPickupPointCode("PP-1001234");
    offlineItemPriceVO2.setListPrice(10000.00);
    offlineItemPriceVO2.setOfferPrice(10000.00);

    assertNotNull(results);
    assertEquals(2, results.size());
    assertTrue(results.contains(offlineItemPriceVO2));
  }

  @Test
  public void getCategoryNameByCategoryCodeTest() {
    when(this.cachedService.getParentCategoriesFromMasterData(REQUEST_ID, USERNAME, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryResponse));
    String categoryName = itemHelperServiceImpl
        .getCategoryNameByCategoryCode(REQUEST_ID, USERNAME, CATEGORY_CODE);
    verify(this.cachedService).getParentCategoriesFromMasterData(REQUEST_ID, USERNAME, CATEGORY_CODE);
    assertEquals(CATEGORY_NAME, categoryName);
  }

  @Test
  public void getCategoryNameByCategoryCode_whenIncorrectCategoryCodeTest() {
    when(this.cachedService.getParentCategoriesFromMasterData(REQUEST_ID, USERNAME, CATEGORY_CODE + 1))
        .thenReturn(Collections.singletonList(categoryResponse));
    String categoryName = itemHelperServiceImpl
        .getCategoryNameByCategoryCode(REQUEST_ID, USERNAME, CATEGORY_CODE + 1);
    verify(this.cachedService).getParentCategoriesFromMasterData(REQUEST_ID, USERNAME, CATEGORY_CODE + 1);
    assertEquals(Constants.HYPHEN, categoryName);
  }

  @Test
  public void processDiscountPricesByPriorityEmptyTest() {
    DiscountPrice discountPrice = itemHelperServiceImpl.processDiscountPricesByPriority(new ArrayList<>());
    Assertions.assertNull(discountPrice);
  }

  @Test
  public void processDiscountPricesByPriorityStartDateAfterTwoDaysTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(20.0);
    discountPrice.setPriority(1);
    discountPrice.setStartDateTime(Date.from(Instant.now().plus(Duration.ofDays(1))));
    discountPrice.setStartDateTime(Date.from(Instant.now().plus(Duration.ofDays(2))));
    discountPrice =
        itemHelperServiceImpl.processDiscountPricesByPriority(new ArrayList<>(Arrays.asList(discountPrice)));
    Assertions.assertNull(discountPrice);
  }

  @Test
  public void getDiscountPriceEmptyTest() {
    double discountPrice = itemHelperServiceImpl.getDiscountPrice(new Price());
    assertEquals(0.0, discountPrice, 0);
  }

  @Test
  public void getCategoryNameByCategoryCode_whenNullCategoryCodeResponseTest() {
    categoryResponse.setCategoryCode(null);
    when(this.cachedService.getParentCategoriesFromMasterData(REQUEST_ID, USERNAME, CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryResponse));
    String categoryName = itemHelperServiceImpl
        .getCategoryNameByCategoryCode(REQUEST_ID, USERNAME, CATEGORY_CODE);
    verify(this.cachedService).getParentCategoriesFromMasterData(REQUEST_ID, USERNAME, CATEGORY_CODE);
    assertEquals(Constants.HYPHEN, categoryName);
  }

  @Test
  public void getCategoryNameByCategoryCode_whenCategoryCodeNullTest() {
    String categoryName = itemHelperServiceImpl
        .getCategoryNameByCategoryCode(REQUEST_ID, USERNAME, null);
    assertEquals(Constants.HYPHEN, categoryName);
  }

  @Test
  public void getCategoryNameByCategoryCode_whenExceptionTest() {
    when(this.cachedService.getParentCategoriesFromMasterData(REQUEST_ID, USERNAME, CATEGORY_CODE))
        .thenThrow(new RuntimeException());
    String categoryName = itemHelperServiceImpl.getCategoryNameByCategoryCode(REQUEST_ID, USERNAME, CATEGORY_CODE);
    verify(this.cachedService).getParentCategoriesFromMasterData(REQUEST_ID, USERNAME, CATEGORY_CODE);
    assertEquals(Constants.HYPHEN, categoryName);
  }

  @Test
  public void getDiscountPrice2Test(){
    Price price = new Price();
    Date date = new Date();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(20.0);
    discountPrice.setPriority(1);
    discountPrice.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice.setEndDateTime(new Date(date.getTime() + 10000));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setDiscountPrice(40.0);
    discountPrice1.setPriority(3);discountPrice1.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice1.setEndDateTime(new Date(date.getTime() + 10000));
    DiscountPrice discountPrice3 = new DiscountPrice();
    discountPrice3.setDiscountPrice(400.0);
    discountPrice3.setPriority(4);
    discountPrice3.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice3.setEndDateTime(new Date(date.getTime() + 10000));
    List<DiscountPrice> discountPrices =
      Arrays.asList(discountPrice1, discountPrice3, discountPrice);
    price.setListOfDiscountPrices(discountPrices);
    double lowestPriority = itemHelperServiceImpl.getDiscountPrice(price);
    assertEquals(20.0,lowestPriority,0);
  }

  @Test
  public void getDiscountPriceV2WithDateTest(){
    Price price = new Price();
    Date date = new Date();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(20.0);
    discountPrice.setPriority(1);
    discountPrice.setStartDateTime(new Date(date.getTime() - 20000));
    discountPrice.setEndDateTime(new Date(date.getTime() - 10000));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setDiscountPrice(40.0);
    discountPrice1.setPriority(3);discountPrice1.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice1.setEndDateTime(new Date(date.getTime() + 10000));
    DiscountPrice discountPrice3 = new DiscountPrice();
    discountPrice3.setDiscountPrice(400.0);
    discountPrice3.setPriority(4);
    discountPrice3.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice3.setEndDateTime(new Date(date.getTime() + 10000));
    List<DiscountPrice> discountPrices =
      Arrays.asList(discountPrice1, discountPrice3, discountPrice);
    price.setListOfDiscountPrices(discountPrices);
    double lowestPriority = itemHelperServiceImpl.getDiscountPrice(price);
    assertEquals(40.0,lowestPriority,0);
  }

  @Test
  public void getDiscountPriceV2WithSamePriority(){
    Price price = new Price();
    Date date = new Date();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(20.0);
    discountPrice.setPriority(1);
    discountPrice.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice.setEndDateTime(new Date(date.getTime() + 10000));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setDiscountPrice(40.0);
    discountPrice1.setPriority(3);discountPrice1.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice1.setEndDateTime(new Date(date.getTime() + 10000));
    DiscountPrice discountPrice3 = new DiscountPrice();
    discountPrice3.setDiscountPrice(400.0);
    discountPrice3.setPriority(1);
    discountPrice3.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice3.setEndDateTime(new Date(date.getTime() + 10000));
    List<DiscountPrice> discountPrices =
      Arrays.asList(discountPrice1, discountPrice3, discountPrice);
    price.setListOfDiscountPrices(discountPrices);
    double lowestPriority = itemHelperServiceImpl.getDiscountPrice(price);
    assertEquals(420.0,lowestPriority,0);
  }

  @Test
  public void getDiscountPriceV2WithEmptyPriority(){
    Price price = new Price();
    Date date = new Date();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(20.0);
    discountPrice.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice.setEndDateTime(new Date(date.getTime() + 10000));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setDiscountPrice(40.0);
    discountPrice1.setPriority(3);
    discountPrice1.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice1.setEndDateTime(new Date(date.getTime() + 10000));
    DiscountPrice discountPrice3 = new DiscountPrice();
    discountPrice3.setDiscountPrice(400.0);
    discountPrice3.setPriority(1);
    discountPrice3.setStartDateTime(new Date(date.getTime() - 10000));
    discountPrice3.setEndDateTime(new Date(date.getTime() + 10000));
    List<DiscountPrice> discountPrices =
      Arrays.asList(discountPrice1, discountPrice3, discountPrice);
    price.setListOfDiscountPrices(discountPrices);
    double lowestPriority = itemHelperServiceImpl.getDiscountPrice(price);
    assertEquals(20.0,lowestPriority,0);
  }

  @Test
  public void getDiscountPriceV2Test_ExpiredDate() {
    Date date = new Date();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(10);
    discountPrice.setStartDateTime(new Date(date.getTime() + 10000));
    discountPrice.setEndDateTime(new Date(date.getTime() - 10000));
    discountPrice.setPriority(1);
    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    itemHelperServiceImpl.getDiscountPrice(price);
  }

  @Test
  public void getNewVarintTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(REQUEST_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID);
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    addVariantRequestVo.setItemSku(ITEM_SKU);
    ProductAttributeDetailVo productAttributeDetailVo = new ProductAttributeDetailVo();
    addVariantRequestVo.setDefiningAttributes(Collections.singletonList(productAttributeDetailVo));
    Item item = itemHelperServiceImpl.convertToItem(addVariantRequestVo, new Product(), mandatoryRequestParam);
    assertEquals(ITEM_SKU, item.getItemSku());
  }

  @Test
  public void findActivePromoBundlingsByStoreIdAndPristineAndPickupPointCodeTest() {
    when(itemRepository.findItemSkusByPristine(STORE_ID, pristineDataItem)).thenReturn(Set.of(ITEM_SKU));
    when(itemPickupPointService
        .getActivePromoBundlingByItemSkusAndPickupPointCode(STORE_ID, Arrays.asList(ITEM_SKU), PICKUP_POINT_CODE))
        .thenReturn(Set.of(ITEM_SKU));
    Set<String> result = itemHelperServiceImpl
        .findActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(STORE_ID, pristineDataItem, PICKUP_POINT_CODE);
    Mockito.verify(itemRepository).findItemSkusByPristine(STORE_ID, pristineDataItem);
    Mockito.verify(itemPickupPointService)
        .getActivePromoBundlingByItemSkusAndPickupPointCode(STORE_ID, Arrays.asList(ITEM_SKU), PICKUP_POINT_CODE);
  }

  @Test
  public void findActivePromoBundlingsByStoreIdAndPristineAndPickupPointCodeWithPristineDatItemNullTest() {
    PristineDataItem pristineDataItem = null;
    when(itemRepository.findItemSkusByPristine(STORE_ID, pristineDataItem)).thenReturn(Set.of(ITEM_SKU));
    when(itemPickupPointService
        .getActivePromoBundlingByItemSkusAndPickupPointCode(STORE_ID, Arrays.asList(ITEM_SKU), PICKUP_POINT_CODE))
        .thenReturn(Set.of(ITEM_SKU));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemHelperServiceImpl
        .findActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(STORE_ID, pristineDataItem, PICKUP_POINT_CODE));
  }

  @Test
  public void findActivePromoBundlingsByStoreIdAndPristineAndPickupPointCodeAndEmptyItemSkusTest() {
    when(itemRepository.findItemSkusByPristine(STORE_ID, pristineDataItem)).thenReturn(Set.of());
    Set<String> result = itemHelperServiceImpl
        .findActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(STORE_ID, pristineDataItem, PICKUP_POINT_CODE);
    Mockito.verify(itemRepository).findItemSkusByPristine(STORE_ID, pristineDataItem);
  }

  @Test
  public void findActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCodeTest() {
    when(itemRepository.findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_SKU)).thenReturn(Set.of(ITEM_SKU));
    when(itemPickupPointService
        .getActivePromoBundlingByItemSkusAndPickupPointCode(STORE_ID, Arrays.asList(ITEM_SKU), PICKUP_POINT_CODE))
        .thenReturn(Set.of(ITEM_SKU));
    Set<String> result = itemHelperServiceImpl
        .findActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(itemRepository).findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointService)
        .getActivePromoBundlingByItemSkusAndPickupPointCode(STORE_ID, Arrays.asList(ITEM_SKU), PICKUP_POINT_CODE);
  }

  @Test
  public void findActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCodeAndEmptyItemSKusTest() {
    when(itemRepository.findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_SKU)).thenReturn(Set.of());
    Set<String> result = itemHelperServiceImpl
        .findActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(itemRepository).findItemSkusByStoreIdAndItemCode(STORE_ID, ITEM_SKU);
  }
}
