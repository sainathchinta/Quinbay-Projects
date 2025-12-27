package com.gdn.x.product.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemPriceChangeEventModel;
import com.gdn.x.product.domain.event.model.ParentCategory;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.AdjustmentType;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.response.AdjustmentProductResponse;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.outbound.api.PromotionOutbound;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.google.common.collect.ImmutableMap;

public class ItemPriceServiceImplTest {
  private static final String ITEM_SKU_4 = "item-sku-4";

  private static final long DISCOUNT_PRICE = 10000;

  private static final Date START_DATE_TIME = new Date(1231321);

  private static final Date END_DATE_TIME = new Date(123123123);

  private static final String ADJUSTMENT_NAME = "adjustmentName";

  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "request-id";

  private static final String STORE_ID = "store-id";

  private static final String ITEM_SKU2 = "item-sku2";

  private static final String ITEM_SKU3 = "item-sku3";

  private static final String ITEM_SKU1 = "item-sku1";

  private static final String PRODUCT_SKU_1 = "product-sku-1";

  private static final String PRODUCT_SKU_2 = "product-sku-2";

  private static final String PRODUCT_CODE = "MTA-00001";

  private static final String PARENT_CATEGORY_NAME = "Handphone & Tablet";

  private static final String PARENT_CATEGORY_CODE = "10580";

  private static final String PRISTINE_ID = "PRI-67634-00";

  private static final String CHILD_CATEGORY_NAME = "Android";

  private static final String CHILD_CATEGORY_CODE = "10582";

  private static final String X_PROMO_API_SWITCH = "system_parameter_x_promo_api_switch";
  private static final String STORE_ID_DEFAULT = "10001";
  private static final double OFFER_PRICE = 10000;
  private static final double DISCOUNT_PERCENTAGE = 10;

  private static final double MINIMUM_PRICE = 1.0;

  private static final double FINAL_PRICE = 8000.0;

  private static final Double DISCOUNT = 1000.0;

  private static final List<String> ITEM_SKUS = Arrays.asList(ItemPriceServiceImplTest.ITEM_SKU1,
      ItemPriceServiceImplTest.ITEM_SKU2, ItemPriceServiceImplTest.ITEM_SKU3);

  private static final List<String> PRODUCT_SKUS = Arrays.asList(
      ItemPriceServiceImplTest.PRODUCT_SKU_1, ItemPriceServiceImplTest.PRODUCT_SKU_2);

  @InjectMocks
  private ItemPriceServiceImpl itemPriceServiceImpl;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private PromotionOutbound promotionOutbound;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private CachedService cachedService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemCacheableService itemCacheableService;

  @Mock
  private ItemHelperService itemHelperService;

  @Captor
  private ArgumentCaptor<List<String>> listArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<String>> itemSkuCaptor;

  private List<Item> items;

  private Set<Price> prices;

  private List<AdjustmentProductResponse> adjusmentResponses;

  private List<List<CategoryResponse>> categoryResponses;

  private Product product;

  private ItemPriceChangeEventModel itemPriceChangeEventModel;

  private Price price = new Price();

  /*@Test
  public void getPriceWithDiscountTest() {
    ItemDiscountPriceVO itemDiscountPriceVO = new ItemDiscountPriceVO();
    List<DiscountPrice> discountPriceList = new ArrayList<>();
    discountPriceList.add(new DiscountPrice(200, new Date(), new Date(), StringUtils.EMPTY, AdjustmentType.MERCHANT));
    itemDiscountPriceVO.setDiscountPrice(discountPriceList.get(0));
    itemDiscountPriceVO.setMerchantPromoDiscount(true);
    items.get(0).setMerchantPromoDiscount(true);
    items.get(0).getPrice().iterator().next().setMerchantPromoDiscountPrice(
        new DiscountPrice(600, new Date(), new Date(), "AdjustmentName", AdjustmentType.MERCHANT));
    items.get(2).getPrice().stream().iterator().next().setListOfDiscountPrices(new ArrayList<>());
    when(
        this.itemRepository.getPriceAndViewConfigs(ItemPriceServiceImplTest.STORE_ID,
            ItemPriceServiceImplTest.ITEM_SKUS)).thenReturn(this.items);
    Map<String, Set<Price>> prices =
        this.itemPriceServiceImpl.getPriceWithDiscount(ItemPriceServiceImplTest.STORE_ID,
            ItemPriceServiceImplTest.USERNAME, ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.ITEM_SKUS);
    verify(this.itemRepository).getPriceAndViewConfigs(ItemPriceServiceImplTest.STORE_ID,
        ItemPriceServiceImplTest.ITEM_SKUS);
    assertThat(prices.get(ItemPriceServiceImplTest.ITEM_SKU1).iterator().next()
       .getListPrice(), equalTo(700d));
    assertThat(prices.get(ItemPriceServiceImplTest.ITEM_SKU1).iterator().next()
        .getOfferPrice(), equalTo(600d));
  }

  @Test
  public void getPriceWithMPDFalseDiscountTest() {
    ItemDiscountPriceVO itemDiscountPriceVO = new ItemDiscountPriceVO();
    List<DiscountPrice> discountPriceList = new ArrayList<>();
    discountPriceList.add(new DiscountPrice(200, new Date(), new Date(), StringUtils.EMPTY, AdjustmentType.MERCHANT));
    itemDiscountPriceVO.setDiscountPrice(discountPriceList.get(0));
    itemDiscountPriceVO.setMerchantPromoDiscount(true);
    items.get(0).setMerchantPromoDiscount(false);
    items.get(0).getPrice().stream().findFirst().get().setOfferPrice(700d);
    items.get(0).getPrice().iterator().next().setMerchantPromoDiscountPrice(
        new DiscountPrice(600D, new Date(), new Date(), "AdjustmentName", AdjustmentType.MERCHANT));
    items.get(2).getPrice().stream().iterator().next().setListOfDiscountPrices(new ArrayList<>());
    when(
        this.itemRepository.getPriceAndViewConfigs(ItemPriceServiceImplTest.STORE_ID,
            ItemPriceServiceImplTest.ITEM_SKUS)).thenReturn(this.items);
    Map<String, Set<Price>> prices =
        this.itemPriceServiceImpl.getPriceWithDiscount(ItemPriceServiceImplTest.STORE_ID,
            ItemPriceServiceImplTest.USERNAME, ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.ITEM_SKUS);
    verify(this.itemRepository).getPriceAndViewConfigs(ItemPriceServiceImplTest.STORE_ID,
        ItemPriceServiceImplTest.ITEM_SKUS);
    assertThat(prices.get(ItemPriceServiceImplTest.ITEM_SKU1).iterator().next()
        .getListPrice(), equalTo(700d));
    assertThat(prices.get(ItemPriceServiceImplTest.ITEM_SKU1).iterator().next()
        .getOfferPrice(), equalTo(700d));
  }

  @Test
  public void getPriceWithDiscountWithXPromoSwitchOffTest_2() {
    List<DiscountPrice> discountPriceList = new ArrayList<>();
    ItemDiscountPriceVO itemDiscountPriceVO = new ItemDiscountPriceVO();
    discountPriceList.add(new DiscountPrice(200, new Date(), new Date(), StringUtils.EMPTY, AdjustmentType.MERCHANT));
    itemDiscountPriceVO.setDiscountPrice(discountPriceList.get(0));
    itemDiscountPriceVO.setMerchantPromoDiscount(true);
    Map<String, Set<Price>> prices = this.itemPriceServiceImpl.getPriceWithDiscount(ItemPriceServiceImplTest.STORE_ID,
        ItemPriceServiceImplTest.USERNAME, ItemPriceServiceImplTest.REQUEST_ID, ItemPriceServiceImplTest.ITEM_SKUS);
    verify(this.itemRepository).getPriceAndViewConfigs(ItemPriceServiceImplTest.STORE_ID,
        ItemPriceServiceImplTest.ITEM_SKUS);
    assertThat(prices.get(ItemPriceServiceImplTest.ITEM_SKU1).iterator().next().getListOfDiscountPrices().size(),
        equalTo(0));
    assertThat(prices.get(ItemPriceServiceImplTest.ITEM_SKU2).iterator().next().getListOfDiscountPrices().size(),
        equalTo(0));
  }*/

  @Test
  public void getAndSetDiscountPrice() {
    adjusmentResponses.get(0).setActivated(Boolean.TRUE);
    adjusmentResponses.get(0).setStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    adjusmentResponses.get(0).setEndDate(Date.from(Instant.now().plus(Duration.ofDays(2))));
    when(
        this.promotionOutbound.getAdjustmentProduct(eq(REQUEST_ID),
            eq(USERNAME), listArgumentCaptor.capture())).thenReturn(
        this.adjusmentResponses);
    when(itemHelperService.processDiscountPricesByPriority(Mockito.anyList())).thenReturn(new DiscountPrice());
    Assertions.assertEquals(1,
      items.get(0).getPrice().iterator().next().getListOfDiscountPrices().size());
    this.itemPriceServiceImpl
        .getAndSetPromotionPrice(ItemPriceServiceImplTest.REQUEST_ID, ItemPriceServiceImplTest.USERNAME, items);
    verify(this.promotionOutbound).getAdjustmentProduct(eq(REQUEST_ID),
        eq(USERNAME), Mockito.anyList());
    verify(itemHelperService).processDiscountPricesByPriority(Mockito.anyList());
    Assertions.assertEquals(1,
      items.get(0).getPrice().iterator().next().getListOfDiscountPrices().size());
  }

  @Test
  public void getAndSetDiscountPriceNullResponse() {
    adjusmentResponses.get(0).setActivated(Boolean.TRUE);
    adjusmentResponses.get(0).setStartDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    adjusmentResponses.get(0).setEndDate(Date.from(Instant.now().plus(Duration.ofDays(2))));
    when(
        this.promotionOutbound.getAdjustmentProduct(eq(REQUEST_ID),
            eq(USERNAME), listArgumentCaptor.capture())).thenReturn(
        this.adjusmentResponses);
    when(itemHelperService.processDiscountPricesByPriority(Mockito.anyList())).thenReturn(null);
    Assertions.assertEquals(1,
        items.get(0).getPrice().iterator().next().getListOfDiscountPrices().size());
    this.itemPriceServiceImpl
        .getAndSetPromotionPrice(ItemPriceServiceImplTest.REQUEST_ID, ItemPriceServiceImplTest.USERNAME, items);
    verify(this.promotionOutbound).getAdjustmentProduct(eq(REQUEST_ID),
        eq(USERNAME), Mockito.anyList());
    verify(itemHelperService).processDiscountPricesByPriority(Mockito.anyList());
    Assertions.assertEquals(1,
        items.get(0).getPrice().iterator().next().getListOfDiscountPrices().size());
  }

  @Test
  public void getDiscountItemPickupPoint() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU1);
    itemPickupPoint.setPrice(new HashSet<>());
    itemPickupPoint.getPrice().add(new Price());
    itemPriceServiceImpl.getDiscountItemPickupPoint(Arrays.asList(itemPickupPoint));
    Mockito.verify(itemHelperService).processDiscountPricesByPriority(Mockito.anyList());
  }

  @BeforeEach
  public void init() throws Exception{
    openMocks(this);
    Price price = new Price();
    price.setListPrice(700);
    price.setOfferPrice(600);
    price.setChannel("DEFAULT");
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(DISCOUNT_PRICE);
    discountPrice.setPriority(0);
    discountPrice.setStartDateTime(new Date());
    discountPrice.setAdjustmentName(ADJUSTMENT_NAME);
    discountPrice.setAdjustmentType(AdjustmentType.BLIBLI);
    discountPrice.setEndDateTime(new Date());
    this.items = new ArrayList<Item>();
    this.prices = new HashSet<Price>();
    price.setListOfDiscountPrices(Collections.singletonList(discountPrice));
    price.setMerchantPromoDiscountPrice(new DiscountPrice());
    this.prices.add(price);
    Item item1 = new Item();
    item1.setItemSku(ItemPriceServiceImplTest.ITEM_SKU1);
    item1.setProductSku(ItemPriceServiceImplTest.PRODUCT_SKU_1);
    item1.setPrice(this.prices);
    item1.setFlashSaleActive(false);

    Item item2 = new Item();
    item2.setItemSku(ItemPriceServiceImplTest.ITEM_SKU2);
    item2.setProductSku(ItemPriceServiceImplTest.PRODUCT_SKU_2);
    price = new Price();
    price.setListPrice(700);
    price.setOfferPrice(600);
    price.setChannel("DEFAULT");Set<Price> prices = new HashSet<>();
    prices.add(price);
    item2.setPrice(prices);
    Item item3 = new Item();
    item3.setItemSku(ItemPriceServiceImplTest.ITEM_SKU3);
    item3.setProductSku(ItemPriceServiceImplTest.PRODUCT_SKU_2);
    item3.setPrice(this.prices);
    item3.setFlashSaleActive(true);

    this.items.add(item1);
    this.items.add(item2);
    this.items.add(item3);

    this.adjusmentResponses = new ArrayList<AdjustmentProductResponse>();
    AdjustmentProductResponse adjustmentResponse = new AdjustmentProductResponse();
    adjustmentResponse.setProductSku(ItemPriceServiceImplTest.ITEM_SKU1);
    adjustmentResponse.setAdjustmentName(ItemPriceServiceImplTest.ADJUSTMENT_NAME);
    adjustmentResponse.setStartDate(ItemPriceServiceImplTest.START_DATE_TIME);
    adjustmentResponse.setEndDate(ItemPriceServiceImplTest.END_DATE_TIME);
    adjustmentResponse.setValue(ItemPriceServiceImplTest.DISCOUNT_PRICE);
    adjustmentResponse.setActivated(Boolean.TRUE);

    AdjustmentProductResponse adjustmentResponse2 = new AdjustmentProductResponse();
    adjustmentResponse2.setProductSku(ItemPriceServiceImplTest.ITEM_SKU_4);
    adjustmentResponse2.setAdjustmentName(ItemPriceServiceImplTest.ADJUSTMENT_NAME);
    adjustmentResponse2.setStartDate(ItemPriceServiceImplTest.START_DATE_TIME);
    adjustmentResponse2.setEndDate(ItemPriceServiceImplTest.END_DATE_TIME);
    adjustmentResponse2.setValue(ItemPriceServiceImplTest.DISCOUNT_PRICE);
    adjustmentResponse.setActivated(Boolean.TRUE);

    this.adjusmentResponses.add(adjustmentResponse);
    this.adjusmentResponses.add(adjustmentResponse2);

    product = new Product();
    product.setProductCode(PRODUCT_CODE);

    MasterCatalog masterCatalog = new MasterCatalog();
    Category category = new Category(CHILD_CATEGORY_CODE,CHILD_CATEGORY_CODE);
    masterCatalog.setCategory(category);
    product.setMasterCatalog(masterCatalog);

    categoryResponses = new ArrayList<>();
    List<CategoryResponse> categoryList = new ArrayList<>();
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setCategoryCode(PARENT_CATEGORY_CODE);
    categoryResponse1.setName(PARENT_CATEGORY_NAME);

    CategoryResponse categoryResponse2 = new CategoryResponse();
    categoryResponse2.setCategoryCode(CHILD_CATEGORY_CODE);
    categoryResponse2.setName(CHILD_CATEGORY_NAME);
    categoryList.add(categoryResponse2);
    categoryList.add(categoryResponse1);
    categoryResponses.add(categoryList);

    itemPriceChangeEventModel = new ItemPriceChangeEventModel();
    ParentCategory parentCategory = new ParentCategory();
    parentCategory.setCategoryCode(CHILD_CATEGORY_CODE);
    parentCategory.setName(PARENT_CATEGORY_NAME);
    itemPriceChangeEventModel.setParentCategory(parentCategory);
    itemPriceChangeEventModel.setItemSku(ITEM_SKU1);
    itemPriceChangeEventModel.setProductCode(PRODUCT_CODE);
    itemPriceChangeEventModel.setArchived(false);
    itemPriceChangeEventModel.setMarkForDelete(false);
    when(
        this.itemRepository.getPriceAndViewConfigs(ItemPriceServiceImplTest.STORE_ID,
            ItemPriceServiceImplTest.ITEM_SKUS)).thenReturn(this.items);

    when(
        this.promotionOutbound.getAdjustmentProduct(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME, ItemPriceServiceImplTest.ITEM_SKUS)).thenReturn(
        this.adjusmentResponses);
  }

  @Test
  public void publishItemPriceChangeEventByItemPickupPointTest(){
    ItemPickupPointVo itemPickupPoint = new ItemPickupPointVo();
    itemPickupPoint.setItemSku(ITEM_SKU1);
    itemPickupPoint.setPrice(prices);
    ItemVo itemVo = new ItemVo();
    itemVo.setItemPickupPointVoList(Arrays.asList(itemPickupPoint));
    Mockito.when(productHelperService
            .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
                ItemPriceServiceImplTest.USERNAME,
                Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode())))
        .thenReturn(categoryResponses);
    this.itemPriceServiceImpl
        .publishItemPriceChangeEvent(ItemPriceServiceImplTest.USERNAME,
            ItemPriceServiceImplTest.REQUEST_ID, product, itemVo);
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME),
      eq(ITEM_SKU1), any(ItemPriceChangeEventModel.class));
  }

  @Test
  public void publishItemPriceChangeEventTest(){
    Mockito.when(productHelperService
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode())))
        .thenReturn(categoryResponses);
    this.itemPriceServiceImpl
        .publishItemPriceChangeEvent(ItemPriceServiceImplTest.USERNAME,
            ItemPriceServiceImplTest.REQUEST_ID, product, items.get(0));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME),
      eq(ITEM_SKU1), any(ItemPriceChangeEventModel.class));
  }

  @Test
  public void publishItemPriceChangeEvent_whenPristineDataItemNotNullTest(){

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    items.get(0).setPristineDataItem(pristineDataItem);
    Mockito.when(productHelperService
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode())))
        .thenReturn(categoryResponses);
    this.itemPriceServiceImpl
        .publishItemPriceChangeEvent(ItemPriceServiceImplTest.USERNAME,
            ItemPriceServiceImplTest.REQUEST_ID, product, items.get(0));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME),
      eq(ITEM_SKU1), any(ItemPriceChangeEventModel.class));
  }

  @Test
  public void publishItemPriceChangeEvent_whenMasterCatalogAndProdCodeNullTest(){
    product.setMasterCatalog(null);
    product.setProductCode(null);
    this.itemPriceServiceImpl
        .publishItemPriceChangeEvent(ItemPriceServiceImplTest.USERNAME,
            ItemPriceServiceImplTest.REQUEST_ID, product, items.get(0));
  }

  @Test
  public void publishItemPriceChangeEvent_whenMasterCatalogNullAndProdCodeNotNullTest(){
    product.setMasterCatalog(null);
    Mockito.when(cachedService
        .getMasterParentCategoryResponseByProductCode(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME, product.getProductCode()))
        .thenReturn(categoryResponses.get(0));
    this.itemPriceServiceImpl
        .publishItemPriceChangeEvent(ItemPriceServiceImplTest.USERNAME,
            ItemPriceServiceImplTest.REQUEST_ID, product, items.get(0));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME),
      eq(ITEM_SKU1), any(ItemPriceChangeEventModel.class));
    Mockito.verify(cachedService)
        .getMasterParentCategoryResponseByProductCode(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME, product.getProductCode());
  }

  @Test
  public void publishItemPriceChangeEvent_whenMasterCatalogCategoryNullAndProdCodeNotNullTest(){
    product.getMasterCatalog().setCategory(null);
    Mockito.when(cachedService
        .getMasterParentCategoryResponseByProductCode(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME, product.getProductCode()))
        .thenReturn(categoryResponses.get(0));
    this.itemPriceServiceImpl
        .publishItemPriceChangeEvent(ItemPriceServiceImplTest.USERNAME,
            ItemPriceServiceImplTest.REQUEST_ID, product, items.get(0));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME),
      eq(ITEM_SKU1), any(ItemPriceChangeEventModel.class));
    Mockito.verify(cachedService)
        .getMasterParentCategoryResponseByProductCode(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME, product.getProductCode());
  }

  @Test
  public void publishItemPriceChangeEvent_whenCategoryResponseEmptyNullTest(){
    Mockito.when(productHelperService
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode())))
        .thenReturn(null);
    this.itemPriceServiceImpl
        .publishItemPriceChangeEvent(ItemPriceServiceImplTest.USERNAME,
            ItemPriceServiceImplTest.REQUEST_ID, product, items.get(0));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));

  }

  @Test
  public void publishItemPriceChangeEvent_whenCategoryNullTest(){
    List<List<CategoryResponse>> categoryResponseList = new ArrayList<>();
    List<CategoryResponse> categoryList = new ArrayList<>();
    categoryList.add(null);
    categoryResponseList.add(categoryList);
    Mockito.when(productHelperService
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode())))
        .thenReturn(categoryResponseList);
    this.itemPriceServiceImpl
        .publishItemPriceChangeEvent(ItemPriceServiceImplTest.USERNAME,
            ItemPriceServiceImplTest.REQUEST_ID, product, items.get(0));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));

  }

  @Test
  public void publishItemPriceChangeEvent_whenResponseEmptyListTest(){
    Mockito.when(productHelperService
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode())))
        .thenReturn(new ArrayList<List<CategoryResponse>>());
    this.itemPriceServiceImpl
        .publishItemPriceChangeEvent(ItemPriceServiceImplTest.USERNAME,
            ItemPriceServiceImplTest.REQUEST_ID, product, items.get(0));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));

  }

  @Test
  public void publishItemPriceChangeEventExceptionTest() {
    Mockito.doThrow(new RuntimeException()).when(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));
    this.itemPriceServiceImpl.publishItemPriceChangeEvent(ItemPriceServiceImplTest.USERNAME,
        ItemPriceServiceImplTest.REQUEST_ID, product, items.get(0));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
            ItemPriceServiceImplTest.USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));
  }

  @Test
  public void publishItemPriceChangeEventWithEmptyDiscountTest(){
    this.items.get(1).getPrice().forEach(price1 -> price1.setListOfDiscountPrices(Collections.emptyList()));
    Mockito.when(productHelperService
        .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
          ItemPriceServiceImplTest.USERNAME,
          Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode())))
      .thenReturn(categoryResponses);
    this.itemPriceServiceImpl
      .publishItemPriceChangeEvent(ItemPriceServiceImplTest.USERNAME,
        ItemPriceServiceImplTest.REQUEST_ID, product, items.get(1));
    Mockito.verify(productHelperService)
      .getCategoryResponseListByCategoryCodesForProducts(ItemPriceServiceImplTest.REQUEST_ID,
        ItemPriceServiceImplTest.USERNAME,
        Collections.singletonList(product.getMasterCatalog().getCategory().getCategoryCode()));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME),
      eq(ITEM_SKU2), any(ItemPriceChangeEventModel.class));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.itemRepository, this.cachedService);
    verifyNoMoreInteractions(this.promotionOutbound, this.productHelperService,
        this.kafkaProducer);
    verifyNoMoreInteractions(this.promotionOutbound, systemParameterService);
  }

  @Test
  public void getFinalPriceTest() {
    Price price = new Price();
    price.setOfferPrice(10000);
    Mockito.when(itemHelperService.getDiscountPrice(price)).thenReturn(1000.0);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue("1");
    Mockito.when(
        systemParameterService.findValueByStoreIdAndVariable(STORE_ID_DEFAULT, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(systemParameter);
    double result = this.itemPriceServiceImpl.getFinalPrice(price, 10);
    verify(this.itemHelperService).getDiscountPrice(price);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID_DEFAULT, SystemParameterNames.MINIMUM_PRICE);
  }

  @Test
  public void getFinalPriceWithParameterTest() {
    price.setOfferPrice(OFFER_PRICE);
    Mockito.when(itemHelperService.getDiscountPrice(price)).thenReturn(DISCOUNT);
    double result =
      this.itemPriceServiceImpl.getFinalPriceWithMinimumPriceParameter(price, DISCOUNT_PERCENTAGE,
        MINIMUM_PRICE);
    verify(this.itemHelperService).getDiscountPrice(price);
    Assertions.assertEquals(result, FINAL_PRICE, 0.0);
  }

  @Test
  public void publishItemPriceChangeEventItemPickupPointTest() {
    ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder().itemSku(items.get(0).getItemSku()).price(prices).build();
    Map<String, Item> itemMap = ImmutableMap.of(items.get(0).getItemSku(), items.get(0));

    Mockito.when(productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
            Constants.DEFAULT_USERNAME, Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode())))
        .thenReturn(categoryResponses);

    itemPriceServiceImpl.publishItemPriceChangeEvent(product, Arrays.asList(itemPickupPoint), itemMap);

    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME),
      eq(ITEM_SKU1), any(ItemPriceChangeEventModel.class));
  }

  @Test
   void publishItemPriceChangeEventItemPickupPointWithNoCategoryResponseTest() {
    ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder().itemSku(items.get(0).getItemSku()).price(prices).build();
    Map<String, Item> itemMap = ImmutableMap.of(items.get(0).getItemSku(), items.get(0));

    Mockito.when(productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode())))
      .thenReturn(null);

    itemPriceServiceImpl.publishItemPriceChangeEvent(product, Arrays.asList(itemPickupPoint), itemMap);

    Mockito.verify(productHelperService)
      .getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));
  }

  @Test
   void publishItemPriceChangeEventItemPickupPointWithCategoryResponseTest() {
    ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder().itemSku(items.get(0).getItemSku()).price(prices).build();
    Map<String, Item> itemMap = ImmutableMap.of(items.get(0).getItemSku(), items.get(0));
    List<CategoryResponse> categoryResponses1 = new ArrayList<>();
    Mockito.when(productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode())))
      .thenReturn(Collections.singletonList(categoryResponses1));

    itemPriceServiceImpl.publishItemPriceChangeEvent(product, Arrays.asList(itemPickupPoint), itemMap);

    Mockito.verify(productHelperService)
      .getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));
  }

  @Test
  void publishItemPriceChangeEventItemPickupPointWithCategoryResponseEmptyTest() {
    ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder().itemSku(items.get(0).getItemSku()).price(prices).build();
    Map<String, Item> itemMap = ImmutableMap.of(items.get(0).getItemSku(), items.get(0));
    List<CategoryResponse> categoryResponses1 = new ArrayList<>();
    Mockito.when(productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode())))
      .thenReturn(Collections.emptyList());

    itemPriceServiceImpl.publishItemPriceChangeEvent(product, Arrays.asList(itemPickupPoint), itemMap);

    Mockito.verify(productHelperService)
      .getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));
  }


  @Test
  public void validateAndSetDiscountPriceTest(){
    DiscountPrice discountPrice =
      new DiscountPrice(DISCOUNT_PRICE, START_DATE_TIME, END_DATE_TIME, ADJUSTMENT_NAME,
        AdjustmentType.BLIBLI, 2);
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    items.get(0).setPrice(Collections.singleton(price));
    when(itemHelperService.processDiscountPricesByPriority(Collections.singletonList(discountPrice))).thenReturn(discountPrice);
    itemPriceServiceImpl.validateAndSetDiscountPrice(items);
    Mockito.verify(itemHelperService).processDiscountPricesByPriority(Collections.singletonList(discountPrice));
    Assertions.assertEquals(discountPrice,
      items.get(0).getPrice().stream().findFirst().get().getListOfDiscountPrices().get(0));
  }

  @Test
  public void validateAndSetDiscountPriceEmptyDiscountTest(){
    price.setListOfDiscountPrices(new ArrayList<>());
    items.get(0).setPrice(Collections.singleton(price));
    when(itemHelperService.processDiscountPricesByPriority(new ArrayList<>())).thenReturn(null);
    itemPriceServiceImpl.validateAndSetDiscountPrice(items);
    Mockito.verify(itemHelperService, Mockito.times(2)).processDiscountPricesByPriority(new ArrayList<>());
  }

  @Test
  public void validateAndSetDiscountPriceWithMultipleDiscountTest(){
    DiscountPrice discountPrice1 =
      new DiscountPrice(DISCOUNT_PRICE, START_DATE_TIME, END_DATE_TIME, ADJUSTMENT_NAME,
        AdjustmentType.BLIBLI, 2);
    DiscountPrice discountPrice2 =
      new DiscountPrice(2 * DISCOUNT_PRICE, START_DATE_TIME, END_DATE_TIME, ADJUSTMENT_NAME,
        AdjustmentType.MERCHANT, 1);
    List<DiscountPrice> discountPriceList = new ArrayList<>();
    discountPriceList.add(discountPrice1);
    discountPriceList.add(discountPrice2);
    price.setListOfDiscountPrices(discountPriceList);
    items.get(0).setPrice(Collections.singleton(price));
    when(itemHelperService.processDiscountPricesByPriority(discountPriceList)).thenReturn(discountPrice1);
    itemPriceServiceImpl.validateAndSetDiscountPrice(items);
    Mockito.verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(discountPrice1,
      items.get(0).getPrice().stream().findFirst().get().getListOfDiscountPrices().get(0));
  }

  @Test
  public void getDiscountItemPickupPoints() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU1);
    itemPickupPoint.setPrice(new HashSet<>());
    itemPickupPoint.getPrice().add(new Price());
    itemPriceServiceImpl.getDiscountItemPickupPoints(Arrays.asList(itemPickupPoint));
    Mockito.verify(itemHelperService).processDiscountPricesByPriority(Mockito.anyList());
  }

  @Test
  public void getOriginalSellingPriceMap() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU1);
    itemPickupPoint.setPrice(new HashSet<>());
    itemPickupPoint.getPrice().add(new Price());
    Map<String, Double> originalPriceMap =
        itemPriceServiceImpl.getOriginalSellingPrice(Arrays.asList(itemPickupPoint,
            itemPickupPoint));
    Assertions.assertEquals(originalPriceMap.size(), 1);
  }

  @Test
  public void getOriginalSellingPriceEmptyMap() {
    Map<String, Double> originalPriceMap = itemPriceServiceImpl.getOriginalSellingPrice(null);
    Assertions.assertEquals(originalPriceMap, new HashMap<>());
  }

  @Test
  public void validMerchantPromoDiscountPriceTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setStartDateTime(new Date(1640995200000L));
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(new Date());
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date futureDate = calendar.getTime();
    discountPrice.setEndDateTime(futureDate);
    Assertions.assertTrue(itemPriceServiceImpl.validMerchantPromoDiscountPrice(discountPrice));
  }

  @Test
  public void validMerchantPromoDiscountPriceTestFuture() {
    DiscountPrice discountPrice = new DiscountPrice();
    Date date = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date futureDate = calendar.getTime();
    discountPrice.setStartDateTime(futureDate);
    Assertions.assertFalse(itemPriceServiceImpl.validMerchantPromoDiscountPrice(discountPrice));
  }

  @Test
  public void getOriginalSellingPriceTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU1);
    itemPickupPoint.setPrice(new HashSet<>());
    Price price1 = new Price();
    price1.setOfferPrice(200);
    itemPickupPoint.getPrice().add(price1);
    Map<String, Set<Price>> result = itemPriceServiceImpl.getDiscountItemPickupPoints(Arrays.asList(itemPickupPoint));
    Mockito.verify(itemHelperService).processDiscountPricesByPriority(Mockito.anyList());
    Assertions.assertEquals(200,result.get("item-sku1-null").stream().findFirst().get().getOriginalSellingPrice());
  }

}