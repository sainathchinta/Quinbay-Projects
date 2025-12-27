package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.BeanUtils;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponse;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponseDetail;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.ItemSkuVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductForTransactionVO;
import com.gdn.x.product.model.vo.ProductItemDetailVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.outbound.api.PickupPointOutbound;
import com.gdn.x.product.outbound.api.PromotionOutbound;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.promotion.enums.PromoBundlingType;

public class ProductForTransactionServiceImplTest {

  private static final boolean NEED_MASTER_DATA_DETAIL = true;
  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "request-id";
  private static final String ITEM_SKU = "item-sku";
  private static final String ITEM_CODE = "item-code";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String MERCHANT_ID = "merchant-id";
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final boolean IS_SYNCHRONIZED = true;
  private static final String REQUEST_ID_INVALID_WHITESPACE = " ";
  private static final String ITEM_SKU_INVALID_WHITESPACE = " ";
  private static final String MERCHANT_ID_NOT_EXIST = "merchant-id-not-exist";
  private static final String PRODUCT_SKU_WITH_NULL_MERCHANT_PROFILE_RESPONSE =
      "product-sku-with-null-merchant-profile-response";
  private static final String ITEM_SKU_WITH_NULL_RESPONSE = "item-sku-with-null-response";
  private static final String ITEM_SKU_WITH_NULL_MERCHANT_PROFILE_RESPONSE =
      "item-sku-with-null-merchant-profile-response";
  private static final String ITEM_SKU_WITH_NULL_PRODUCT_RESPONSE =
      "item-sku-with-null-product-response";
  private static final String PRODUCT_SKU_WITH_NULL_PRODUCT_RESPONSE =
      "product-sku-with-null-product-response";
  private static final String ITEM_SKU_WITH_NULL_PRODUCT_ITEM_DETAIL_RESPONSE =
      "item-sku-with-null-product-item-detail-response";
  private static final String PRODUCT_SKU_WITH_NULL_PRODUCT_ITEM_DETAIL_RESPONSE =
      "product-sku-with-null-product-item-detail-response";
  private static final String ITEM_SKU_UNSYNCHRONIZED = "item-sku-unsynchronized";
  private static final String PRODUCT_SKU_UNSYNCHRONIZED = "product-sku-unsynchronized";
  private static final String BLANK = "";
  private static final String CHANNEL = ChannelName.MOBILE_WEB.toString();
  private static final String USERNAME = "username";
  private static final double RELEVANT_ITEM_PRICE = 10000;
  private static final String ITEM_SKU_NOT_FOUND = "item-sku-not-found";
  private static final boolean USING_OFFLINE_STORE_DATA_TRUE = true;
  private static final boolean USING_OFFLINE_STORE_DATA_FALSE = false;
  private static final String BUSINESS_PARTNER_CODE= "business-partner-code";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String PICKUP_POINT_CODE_2 = "pickup-point-code-2";
  private static final String EXTERNAL_PICKUP_POINT_CODE = "external-pickup-point-code";
  private static final double LIST_PRICE = 20000;
  private static final double OFFER_PRICE = 10000;
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String SALES_CATEGORY_CODE = "salesCategoryCode";
  private static final String CATALOG_CODE = "12051";
  private static final Double FINAL_PRICE = 10.0;
  private static final Double MIN_PRICE = 1.0;

  @InjectMocks
  private ProductForTransactionServiceImpl productPulsaServiceImpl;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductService productService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private ChannelService channelService;

  @Mock
  private CatalogService catalogService;

  @Mock
  private OfflineItemService offlineItemService;

  @Mock
  private PickupPointOutbound pickupPointOutbound;

  @Mock
  private MasterDataConstructorService masterDataConstructorService;

  @Mock
  private ItemPriceService itemPriceService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private PromotionOutbound promotionOutbound;

  private Item item;
  private Product product;
  private ProfileResponse merchantProfile;
  private GdnRestSingleResponse<ProfileResponse> merchantProfileResponse;
  private List<ItemCatalogVO> itemCatalogs;
  private List<List<CategoryResponse>> listOfCategoriesList;
  private ProductForTransactionVO productVO;
  private Product productWithNullMerchantProfileResponse;
  private Item itemWithNullMerchantProfileResponse;
  private Item itemWithNullItemResponse;
  private Item itemWithNullProductResponse;
  private Product productWithNullProductResponse;
  private Item itemWithNullProductItemDetailResponse;
  private Product productWithNullProductItemDetailResponse;
  private Product productWithUnsynchronized;
  private ProductForTransactionVO productVOWithUnsynchronized;
  private List<List<CategoryResponse>> listOfCategoriesListWithUnsynchronized;
  private Item itemWithUnsynchronized;
  private List<String> itemSkuList;
  private List<Item> itemsGetPriceForPulsa;
  private ArrayList<String> itemSkuListNotFound;
  private Price price;
  private List<ItemSkuVO> itemSkuVOList;
  private ItemSkuVO offlineItemSkuVO;
  private ItemSkuVO onlineItemSkuVO;
  private OfflineItem offlineItem;
  private List<String> pickupPointExternalList;
  private ExternalPickupPointCodeResponse externalPickupPointCodeResponse;
  private ExternalPickupPointCodeResponseDetail externalPickupPointCodeResponseDetail;
  private List<ExternalPickupPointCodeResponseDetail> externalPickupPointCodeResponseDetailList;
  private Item offlineItem2;
  private ProductItemDetailVO productItemDetailVO = new ProductItemDetailVO();
  private SystemParameter systemParameter = new SystemParameter();
  private SystemParameter minimumPriceSystemParameter = new SystemParameter();
  private DiscountPrice discountPrice = new DiscountPrice();
  private Map<String, List<ItemCatalogVO>> itemCatalogMap = new HashMap<>();
  private ProductMasterDataResponse productMasterDataResponse = new ProductMasterDataResponse();
  private ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
  private ActivePromoBundlingResponseVO activePromoBundlingResponseVO = new ActivePromoBundlingResponseVO();
  private PromoBundlingDetailResponseVO promoBundlingDetailResponseVO = new PromoBundlingDetailResponseVO();
  private WholesaleRuleVO wholesaleRuleVO = new WholesaleRuleVO();
  private SystemParameter masterDataControlSwitch = new SystemParameter();
  private ItemPickupPoint itemPickupPoint;
  Set<ItemViewConfig> itemViewConfigList;

  @Test
  public void findProductForPulsaTestWithInvalidWhitespaceItemSku() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productPulsaServiceImpl.findProductForTransactionForAllItems(
        ProductForTransactionServiceImplTest.STORE_ID,
        ProductForTransactionServiceImplTest.REQUEST_ID,
        ProductForTransactionServiceImplTest.USERNAME,
        ProductForTransactionServiceImplTest.ITEM_SKU_INVALID_WHITESPACE));
  }

  @Test
  public void findProductForPulsaTestWithInvalidWhitespaceRequestId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productPulsaServiceImpl.findProductForTransactionForAllItems(
        ProductForTransactionServiceImplTest.STORE_ID,
        ProductForTransactionServiceImplTest.REQUEST_ID_INVALID_WHITESPACE,
        ProductForTransactionServiceImplTest.USERNAME, this.item.getItemSku()));
  }

  @Test
  public void findProductForTransactionForAllItemsTest() throws Exception {
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(this.product, Arrays.asList(this.item));
    when(
        this.productService.getProductDetailAndSingleItemByItemSku(
            ProductForTransactionServiceImplTest.STORE_ID,
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME,
            ProductForTransactionServiceImplTest.ITEM_SKU, false, false, null))
        .thenReturn(productAndItemsVO);

    ProductForTransactionVO result =
        this.productPulsaServiceImpl.findProductForTransactionForAllItems(
            ProductForTransactionServiceImplTest.STORE_ID,
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME,
            ProductForTransactionServiceImplTest.ITEM_SKU);

    verify(this.productService).getProductDetailAndSingleItemByItemSku(
        ProductForTransactionServiceImplTest.STORE_ID,
        ProductForTransactionServiceImplTest.REQUEST_ID,
        ProductForTransactionServiceImplTest.USERNAME,
        ProductForTransactionServiceImplTest.ITEM_SKU,
        false, false, null);

    verify(this.catalogService).getItemCatalogsWithCategoryHierarchy(
        ProductForTransactionServiceImplTest.USERNAME,
        ProductForTransactionServiceImplTest.REQUEST_ID, this.product);

    verify(this.objectConverterService).convertToProductForTransaction(this.product, this.item,
        this.itemCatalogs);

    assertNotNull(result);
    assertEquals(result, (this.productVO));
  }

  @Test
  public void getProductPriceForPulsaTest() throws Exception {
    List<ItemPriceVO> result =
        this.productPulsaServiceImpl.getProductPriceForTransaction(
            ProductForTransactionServiceImplTest.STORE_ID, this.itemSkuList,
            ProductForTransactionServiceImplTest.CHANNEL);

    verify(this.itemService).getItemPriceAndViewConfigs(
        ProductForTransactionServiceImplTest.STORE_ID, this.itemSkuList);

    for (Item item : this.itemsGetPriceForPulsa) {
      verify(this.productHelperService).getCurrentBuyableStatusForItem(itemPickupPoint.getItemViewConfig(),
          ProductForTransactionServiceImplTest.CHANNEL, item.isArchived());
      verify(this.productHelperService).getCurrentDiscoverableStatusForItem(itemViewConfigList,
          ProductForTransactionServiceImplTest.CHANNEL, item.isArchived());
      verify(this.productHelperService).getRelevantItemPrice(item.getPrice(),
          ProductForTransactionServiceImplTest.CHANNEL);
    }

    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void getProductPriceForPulsaTestWithBlankStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productPulsaServiceImpl.getProductPriceForTransaction(
        ProductForTransactionServiceImplTest.BLANK, this.itemSkuList,
        ProductForTransactionServiceImplTest.CHANNEL));
  }

  @Test
  public void getProductPriceForPulsaTestWithEmptyItemSkuList() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productPulsaServiceImpl.getProductPriceForTransaction(
        ProductForTransactionServiceImplTest.STORE_ID, new ArrayList<String>(),
        ProductForTransactionServiceImplTest.CHANNEL));
  }

  @Test
  public void getProductPriceForPulsaTestWithNoItemsFound() throws Exception {
    List<ItemPriceVO> result =
        this.productPulsaServiceImpl.getProductPriceForTransaction(
            ProductForTransactionServiceImplTest.STORE_ID, this.itemSkuListNotFound,
            ProductForTransactionServiceImplTest.CHANNEL);

    verify(this.itemService).getItemPriceAndViewConfigs(
        ProductForTransactionServiceImplTest.STORE_ID, this.itemSkuListNotFound);

    assertNotNull(result);
    assertEquals(0, result.size());
  }

  @Test
  public void getProductPriceForPulsaTestWithNullChannel() throws Exception {
    when(
        this.productHelperService.getRelevantItemPrice(this.item.getPrice(),
            ChannelName.DEFAULT.toString())).thenReturn(this.price);
    this.productPulsaServiceImpl.getProductPriceForTransaction(
        ProductForTransactionServiceImplTest.STORE_ID, this.itemSkuList, null);
    verify(this.itemService).getItemPriceAndViewConfigs(
        ProductForTransactionServiceImplTest.STORE_ID, this.itemSkuList);

    for (Item item : this.itemsGetPriceForPulsa) {
      verify(this.productHelperService)
          .getCurrentBuyableStatusForItem(itemViewConfigList, ChannelName.DEFAULT.toString(), item.isArchived());
      verify(this.productHelperService)
          .getCurrentDiscoverableStatusForItem(itemViewConfigList, ChannelName.DEFAULT.toString(), item.isArchived());
      verify(this.productHelperService).getRelevantItemPrice(item.getPrice(), ChannelName.DEFAULT.toString());
      verify(this.channelService).getDefaultChannel();
    }
  }

  @Test
  public void getProductPriceForPulsaTestWithNullItemSkuList() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productPulsaServiceImpl.getProductPriceForTransaction(
        ProductForTransactionServiceImplTest.STORE_ID, null,
        ProductForTransactionServiceImplTest.CHANNEL));
  }

  @Test
  public void getProductPriceForOnlineAndOfflineTransactionTest() throws Exception {
    this.offlineItemSkuVO = new ItemSkuVO(ProductForTransactionServiceImplTest.ITEM_SKU_NOT_FOUND,
        ProductForTransactionServiceImplTest.USING_OFFLINE_STORE_DATA_TRUE,
        ProductForTransactionServiceImplTest.BUSINESS_PARTNER_CODE,
        ProductForTransactionServiceImplTest.PICKUP_POINT_CODE_2);

    itemSkuVOList.add(this.offlineItemSkuVO);

    Item item1 = new Item();
    item1.setItemSku(ITEM_SKU);
    when(this.itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU, ITEM_SKU_NOT_FOUND)))
        .thenReturn(Arrays.asList(item1));

    when(this.offlineItemService
        .findByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(ProductForTransactionServiceImplTest.STORE_ID,
            ProductForTransactionServiceImplTest.ITEM_SKU, ProductForTransactionServiceImplTest.PICKUP_POINT_CODE))
        .thenReturn(this.offlineItem);

    when(this.offlineItemService
        .findByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(ProductForTransactionServiceImplTest.STORE_ID,
            ProductForTransactionServiceImplTest.ITEM_SKU_NOT_FOUND,
            ProductForTransactionServiceImplTest.PICKUP_POINT_CODE_2)).thenReturn(null);

    List<ItemPriceVO> result = this.productPulsaServiceImpl
        .findProductPriceForOnlineAndOfflineTransaction(ProductForTransactionServiceImplTest.STORE_ID,
            this.itemSkuVOList, ProductForTransactionServiceImplTest.CHANNEL,
            ProductForTransactionServiceImplTest.REQUEST_ID, ProductForTransactionServiceImplTest.USERNAME);

    verify(this.itemService, times(2)).getItemPriceAndViewConfigs(Mockito.eq(STORE_ID), Mockito.anyList());

    for (Item item : this.itemsGetPriceForPulsa) {
      verify(this.productHelperService)
          .getCurrentBuyableStatusForItem(itemViewConfigList, ProductForTransactionServiceImplTest.CHANNEL,
              item.isArchived());
      verify(this.productHelperService)
          .getRelevantItemPrice(item.getPrice(), ProductForTransactionServiceImplTest.CHANNEL);
    }
    assertNotNull(result);
    assertTrue(result.get(0).isBuyable());
  }

  @Test
  public void getProductPriceForOnlineAndOfflineTransactionTest_nullOfflineItem() throws Exception {
    when(this.offlineItemService.findByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        ProductForTransactionServiceImplTest.STORE_ID,
        ProductForTransactionServiceImplTest.ITEM_SKU,
        ProductForTransactionServiceImplTest.PICKUP_POINT_CODE)).thenReturn(null);

    List<ItemPriceVO> result =
        this.productPulsaServiceImpl.findProductPriceForOnlineAndOfflineTransaction(
            ProductForTransactionServiceImplTest.STORE_ID, this.itemSkuVOList,
            ProductForTransactionServiceImplTest.CHANNEL, ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME);


    verify(this.itemService, times(2)).getItemPriceAndViewConfigs(
        ProductForTransactionServiceImplTest.STORE_ID, this.itemSkuList);

    for (Item item : this.itemsGetPriceForPulsa) {
      verify(this.productHelperService)
          .getCurrentBuyableStatusForItem(itemViewConfigList, ProductForTransactionServiceImplTest.CHANNEL,
              item.isArchived());
      verify(this.productHelperService).getRelevantItemPrice(item.getPrice(),
          ProductForTransactionServiceImplTest.CHANNEL);
    }
    assertNotNull(result);
  }

  @Test
  public void getProductPriceForOnlineAndOfflineTransactionTest_nullOfflineItemSku()
      throws Exception {
    this.itemSkuVOList = new ArrayList<>();
    this.itemSkuVOList.add(this.offlineItemSkuVO);

    Item item1 = new Item();
    item1.setItemSku(ITEM_SKU);
    Mockito.when(itemService.getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(itemsGetPriceForPulsa);

    List<ItemPriceVO> result =
        this.productPulsaServiceImpl.findProductPriceForOnlineAndOfflineTransaction(
            ProductForTransactionServiceImplTest.STORE_ID, this.itemSkuVOList,
            ProductForTransactionServiceImplTest.CHANNEL, ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME);

    verify(this.itemService).getItemPriceAndViewConfigs(
        ProductForTransactionServiceImplTest.STORE_ID, this.itemSkuList);

    for (Item item : this.itemsGetPriceForPulsa) {
      verify(this.productHelperService).getCurrentBuyableStatusForItem(itemPickupPoint.getItemViewConfig(),
          ProductForTransactionServiceImplTest.CHANNEL, item.isArchived());
      verify(this.productHelperService).getRelevantItemPrice(item.getPrice(),
          ProductForTransactionServiceImplTest.CHANNEL);
    }
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Arrays.asList(ITEM_SKU));
    assertNotNull(result);
  }

  @Test
  public void getProductPriceForOnlineAndOfflineTransactionWithBlankStoreIdTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productPulsaServiceImpl.findProductPriceForOnlineAndOfflineTransaction(
        ProductForTransactionServiceImplTest.BLANK, this.itemSkuVOList,
        ProductForTransactionServiceImplTest.CHANNEL, ProductForTransactionServiceImplTest.REQUEST_ID,
        ProductForTransactionServiceImplTest.USERNAME));
  }

  @Test
  public void getProductPriceForOnlineAndOfflineTransactionWithEmptyRequestIdTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productPulsaServiceImpl.findProductPriceForOnlineAndOfflineTransaction(
        ProductForTransactionServiceImplTest.STORE_ID, this.itemSkuVOList,
        ProductForTransactionServiceImplTest.CHANNEL, ProductForTransactionServiceImplTest.BLANK,
        ProductForTransactionServiceImplTest.USERNAME));
  }

  @Test
  public void getProductPriceForOnlineAndOfflineTransactionWithEmptyItemSkuListTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productPulsaServiceImpl.findProductPriceForOnlineAndOfflineTransaction(
        ProductForTransactionServiceImplTest.STORE_ID, null,
        ProductForTransactionServiceImplTest.CHANNEL, ProductForTransactionServiceImplTest.REQUEST_ID,
        ProductForTransactionServiceImplTest.USERNAME));
  }

  @Test
  public void findProductForTransactionByItemSkusTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    List<Item> itemList = Arrays.asList(item);
    Mockito.when(this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
      .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
      .thenReturn(Arrays.asList(product));
    Mockito.when(
        this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
      .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));

    Mockito
        .when(this.catalogService
            .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(itemCatalogMap);
    List<ProductForTransactionVO> response = this.productPulsaServiceImpl
        .findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
            Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService)
        .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.masterDataConstructorService)
        .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.catalogService)
        .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
      Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_withMerchantPromoDiscountTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    discountPrice.setDiscountPrice(2000);
    item.getPrice().stream().findFirst().get().setOfferPrice(1000);
    item.getPrice().stream().findFirst().get().setMerchantPromoDiscountPrice(discountPrice);
    Mockito
        .when(this.catalogService
            .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(itemCatalogMap);
    List<Item> itemList = Arrays.asList(item);
    Mockito.when(
        this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
        .thenReturn(itemList);
    Mockito.when(
        this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.masterDataConstructorService
        .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
        .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));
    List<ProductForTransactionVO> response = this.productPulsaServiceImpl
        .findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
            Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService)
        .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.masterDataConstructorService)
        .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
        Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.catalogService)
        .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
      Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_nullMasterCatalogTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    product.setMasterCatalog(null);
    item.setCategoryCode(CATEGORY_CODE);
    product.setMasterDataProduct(new MasterDataProduct());
    product.getMasterDataProduct()
        .setMasterCatalog(
            new MasterCatalog(CATALOG_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    List<Item> itemList = Arrays.asList(item);
    Mockito.when(
        this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
        .thenReturn(itemList);
    Mockito.when(
        this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.masterDataConstructorService
        .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
      .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(this.objectConverterService.convertToProductForTransactionNewMasterData(product, item, itemCatalogs,
      productMasterDataResponse)).thenReturn(productVO);
    Mockito
        .when(this.catalogService
            .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(itemCatalogMap);
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));
    List<ProductForTransactionVO> response = this.productPulsaServiceImpl
        .findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
            Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
        Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService)
        .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.masterDataConstructorService)
        .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.catalogService)
        .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_withSalesCatalogTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    product.setMasterCatalog(null);
    item.setCategoryCode(CATEGORY_CODE);
    product.setMasterDataProduct(new MasterDataProduct());
    product.getMasterDataProduct()
        .setMasterCatalog(
            new MasterCatalog(CATALOG_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    product.setSalesCatalogs(Arrays
        .asList(new SalesCatalog(CATALOG_CODE,
            Arrays.asList(new Category(SALES_CATEGORY_CODE, SALES_CATEGORY_CODE)))));
    itemCatalogMap.put(SALES_CATEGORY_CODE, itemCatalogs);
    List<Item> itemList = Arrays.asList(item);
    Mockito.when(
        this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
        .thenReturn(itemList);
    Mockito.when(
        this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(
        this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
      .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));

    Mockito.when(this.catalogService
        .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID,
            Arrays.asList(CATEGORY_CODE, SALES_CATEGORY_CODE)))
        .thenReturn(itemCatalogMap);
    List<ProductForTransactionVO> response = this.productPulsaServiceImpl
        .findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
            Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService)
        .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.masterDataConstructorService)
        .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
        Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.catalogService)
        .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID,
            Arrays.asList(CATEGORY_CODE, SALES_CATEGORY_CODE));
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
      Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_withNullPriceTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    item.setPrice(Collections.EMPTY_SET);
    List<Item> itemList = Arrays.asList(item);
    Mockito.when(
        this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
        .thenReturn(itemList);
    Mockito.when(
        this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(
        this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
      .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));
    Mockito
        .when(this.catalogService
            .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(itemCatalogMap);
    List<ProductForTransactionVO> response = this.productPulsaServiceImpl
        .findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
            Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService)
        .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.masterDataConstructorService)
        .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(this.catalogService)
        .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
        Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
      Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_emptyProductListTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    List<Item> itemList = Arrays.asList(item);
    Mockito.when(
        this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
        .thenReturn(itemList);
    Mockito.when(
        this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Collections.emptyList());
    List<ProductForTransactionVO> response = this.productPulsaServiceImpl
        .findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
            Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService)
        .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
      Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    assertTrue(CollectionUtils.isEmpty(response));
  }

  @Test
  public void findProductForTransactionByItemSkus_itemCountLimitCrossedTest() throws Exception {
    ReflectionTestUtils.setField(productPulsaServiceImpl, "maxItemsForTransaction", 0);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productPulsaServiceImpl.findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
      Arrays.asList(ITEM_SKU)));
  }

  @Test
  public void findProductForTransactionByItemSkus_findByOfflineItemTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    offlineItem.setItemSku(ITEM_SKU);
    List<Item> itemList = Arrays.asList(item);
    Mockito.when(
        this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
        .thenReturn(Collections.emptyList());
    Mockito.when(this.itemService.getItemsByOfflineItemIds(STORE_ID,
        Arrays.asList(ITEM_SKU), false)).thenReturn(Arrays.asList(item));
    Mockito.when(
        this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(
        this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
      .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));
    Mockito
        .when(this.catalogService
            .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(itemCatalogMap);
    List<ProductForTransactionVO> response = this.productPulsaServiceImpl
        .findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
            Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService)
        .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.itemService).getItemsByOfflineItemIds(STORE_ID, Arrays.asList(ITEM_SKU),
      false);
    Mockito.verify(this.masterDataConstructorService)
        .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
        Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    Mockito.verify(this.catalogService)
        .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_withWholesaleTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    item.setActivePromoBundlings(Collections.singleton(Constants.WHOLESALE));
    itemPickupPoint.setActivePromoBundlings(item.getActivePromoBundlings());
    List<Item> itemList = Arrays.asList(item);
    MandatoryRequestParam mandatoryRequestParam =
      MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, null);
    Mockito.when(this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
      .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
      .thenReturn(Arrays.asList(product));
    Mockito.when(
        this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
      .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));
    Mockito.when(this.catalogService.getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
      .thenReturn(itemCatalogMap);
    Mockito.when(
      promotionOutbound.getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        Collections.singleton(ITEM_SKU))).thenReturn(activePromoBundlingResponseVO);
    Mockito.when(this.itemPriceService.getFinalPriceWithMinimumPriceParameter(
      item.getPrice().stream().findFirst().orElse(new Price()),
      wholesaleRuleVO.getDiscountPercentage(), MIN_PRICE)).thenReturn(FINAL_PRICE);
    List<ProductForTransactionVO> response =
      this.productPulsaServiceImpl.findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.masterDataConstructorService)
      .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
        Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.catalogService).getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(promotionOutbound)
      .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        Collections.singleton(ITEM_SKU));
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterNames.MINIMUM_PRICE);
    Mockito.verify(this.itemPriceService).getFinalPriceWithMinimumPriceParameter(
      item.getPrice().stream().findFirst().orElse(new Price()),
      wholesaleRuleVO.getDiscountPercentage(), MIN_PRICE);
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
      Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_activePromoBundlingForDifferentSkuTest() throws Exception {
    activePromoBundlingResponseVO.getPromoBundlingDetailResponseVOList().get(0).setItemSku(StringUtils.EMPTY);
    product.setProductSku(PRODUCT_SKU);
    item.setActivePromoBundlings(Collections.singleton(Constants.WHOLESALE));
    itemPickupPoint.setActivePromoBundlings(item.getActivePromoBundlings());
    List<Item> itemList = Arrays.asList(item);
    MandatoryRequestParam mandatoryRequestParam =
      MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, null);
    Mockito.when(this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
      .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
      .thenReturn(Arrays.asList(product));
    Mockito.when(
        this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
      .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));
    Mockito.when(this.catalogService.getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
      .thenReturn(itemCatalogMap);
    Mockito.when(
      promotionOutbound.getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        Collections.singleton(ITEM_SKU))).thenReturn(activePromoBundlingResponseVO);
    List<ProductForTransactionVO> response =
      this.productPulsaServiceImpl.findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.masterDataConstructorService)
      .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.catalogService).getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(promotionOutbound)
      .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        Collections.singleton(ITEM_SKU));
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterNames.MINIMUM_PRICE);
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
      Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_activePromoBundlingForDuplicateSkuTest() throws Exception {
    PromoBundlingDetailResponseVO promoBundlingDetailResponseVO1 = new PromoBundlingDetailResponseVO();
    BeanUtils.copyProperties(activePromoBundlingResponseVO.getPromoBundlingDetailResponseVOList().get(0),
      promoBundlingDetailResponseVO1);
    activePromoBundlingResponseVO.setPromoBundlingDetailResponseVOList(Arrays.asList(promoBundlingDetailResponseVO1,
      promoBundlingDetailResponseVO1));
    product.setProductSku(PRODUCT_SKU);
    item.setActivePromoBundlings(Collections.singleton(Constants.WHOLESALE));
    itemPickupPoint.setActivePromoBundlings(item.getActivePromoBundlings());
    List<Item> itemList = Arrays.asList(item);
    MandatoryRequestParam mandatoryRequestParam =
      MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, null);
    Mockito.when(this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
      .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
      .thenReturn(Arrays.asList(product));
    Mockito.when(
        this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
      .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));
    Mockito.when(this.catalogService.getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
      .thenReturn(itemCatalogMap);
    Mockito.when(
      promotionOutbound.getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        Collections.singleton(ITEM_SKU))).thenReturn(activePromoBundlingResponseVO);
    Mockito.when(this.itemPriceService.getFinalPriceWithMinimumPriceParameter(
      item.getPrice().stream().findFirst().orElse(new Price()),
      wholesaleRuleVO.getDiscountPercentage(), MIN_PRICE)).thenReturn(FINAL_PRICE);
    List<ProductForTransactionVO> response =
      this.productPulsaServiceImpl.findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.masterDataConstructorService)
      .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
        Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.catalogService).getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(promotionOutbound)
      .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        Collections.singleton(ITEM_SKU));
    Mockito.verify(this.itemPriceService).getFinalPriceWithMinimumPriceParameter(
      item.getPrice().stream().findFirst().orElse(new Price()),
      wholesaleRuleVO.getDiscountPercentage(), MIN_PRICE);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterNames.MINIMUM_PRICE);
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
      Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_withWholesalePriceTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    item.setActivePromoBundlings(Collections.singleton(Constants.WHOLESALE_PRICE));
    itemPickupPoint.setActivePromoBundlings(item.getActivePromoBundlings());
    List<Item> itemList = Arrays.asList(item);
    MandatoryRequestParam mandatoryRequestParam =
      MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, null);
    Mockito.when(this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
      .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
      .thenReturn(Arrays.asList(product));
    Mockito.when(
        this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
      .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));
    Mockito.when(this.catalogService.getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
      .thenReturn(itemCatalogMap);
    Mockito.when(
      promotionOutbound.getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        Collections.singleton(ITEM_SKU))).thenReturn(activePromoBundlingResponseVO);
    Mockito.when(this.itemPriceService.getFinalPriceWithMinimumPriceParameter(
      item.getPrice().stream().findFirst().orElse(new Price()),
      wholesaleRuleVO.getDiscountPercentage(), MIN_PRICE)).thenReturn(FINAL_PRICE);
    List<ProductForTransactionVO> response =
      this.productPulsaServiceImpl.findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.masterDataConstructorService)
      .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
        Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.catalogService).getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(promotionOutbound)
      .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        Collections.singleton(ITEM_SKU));
    Mockito.verify(this.itemPriceService).getFinalPriceWithMinimumPriceParameter(
      item.getPrice().stream().findFirst().orElse(new Price()),
      wholesaleRuleVO.getDiscountPercentage(), MIN_PRICE);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterNames.MINIMUM_PRICE);
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
      Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_withDifferenPromoTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    item.setActivePromoBundlings(Collections.singleton(Constants.PROMO_SCHEDULER));
    List<Item> itemList = Arrays.asList(item);
    Mockito.when(this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
      .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
      .thenReturn(Arrays.asList(product));
    Mockito.when(
        this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
      .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));
    Mockito.when(this.catalogService.getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
      .thenReturn(itemCatalogMap);
    List<ProductForTransactionVO> response =
      this.productPulsaServiceImpl.findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.masterDataConstructorService)
      .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
        Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.catalogService).getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
      Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_usePCBMasterDataOffTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    List<Item> itemList = Arrays.asList(item);
    ReflectionTestUtils.setField(productPulsaServiceImpl, "fetchPcbMasterData", false);
    Mockito.when(this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
        .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(
            this.masterDataConstructorService.constructProductsAndItemsWithMasterData(STORE_ID, USERNAME, REQUEST_ID,
                Collections.singletonMap(PRODUCT_SKU, product), itemList, true))
        .thenReturn(Arrays.asList(new ProductAndItemsVO(product, itemList)));
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));
    Mockito.when(
            this.catalogService.getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(itemCatalogMap);
    List<ProductForTransactionVO> response =
        this.productPulsaServiceImpl.findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
            Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
        Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.catalogService)
        .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(STORE_ID, Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService)
        .getCurrentBuyableStatusForItem(Mockito.anySet(), eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService)
        .getCurrentDiscoverableStatusForItem(Mockito.anySet(), eq(Constants.DEFAULT), eq(false));
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @Test
  public void findProductForTransactionByItemSkus_exceptionOnBuyableFetchTest() throws Exception {
    product.setProductSku(PRODUCT_SKU);
    List<Item> itemList = Arrays.asList(item);
    Mockito.when(this.productHelperService.getCurrentBuyableStatusForItem(item.getItemViewConfigs(),
      ChannelName.DEFAULT.toString(), item.isArchived())).thenThrow(Exception.class);
    Mockito.when(this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU)))
      .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
      .thenReturn(Arrays.asList(product));
    Mockito.when(
        this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE)))
      .thenReturn(Collections.singletonMap(ITEM_CODE, productMasterDataResponse));
    Mockito.when(objectConverterService.generateProductForTransactionVO(Mockito.anySet(),
            Mockito.anyList(), Mockito.anyMap()))
        .thenReturn(Arrays.asList(productVO));
    Mockito
      .when(this.catalogService
        .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE)))
      .thenReturn(itemCatalogMap);
    List<ProductForTransactionVO> response = this.productPulsaServiceImpl
      .findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        Arrays.asList(ITEM_SKU));
    Mockito.verify(this.itemService)
      .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(this.productService)
      .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.masterDataConstructorService)
      .fetchMasterDataMapForTransaction(STORE_ID, Collections.singletonList(ITEM_CODE));
    Mockito.verify(objectConverterService).generateProductForTransactionVO(Mockito.anySet(),
        Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(this.catalogService)
      .getCategoryCodeToItemCatalogsMap(USERNAME, REQUEST_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(this.itemPickupPointService).findByItemSkusAndDelivery(STORE_ID,
      Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(this.productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      eq(Constants.DEFAULT), eq(false));
    Mockito.verify(objectConverterService).overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), itemList);
    assertEquals(ITEM_CODE, response.get(0).getItemCode());
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(itemCatalogs, response.get(0).getItemDetail().getItemCatalogs());
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    when(this.channelService.getDefaultChannel()).thenReturn(ChannelName.DEFAULT.toString());
    // setup for findProductForPulsa
    this.item = new Item();
    this.item.setProductSku(ProductForTransactionServiceImplTest.PRODUCT_SKU);
    item.setItemSku(ITEM_SKU);
    item.setItemViewConfigs(Collections.singleton(new ItemViewConfig()));

    this.product = new Product();
    this.product.setMerchantCode(ProductForTransactionServiceImplTest.MERCHANT_ID);
    this.product.setSynchronized(ProductForTransactionServiceImplTest.IS_SYNCHRONIZED);
    this.product.setMasterCatalog(new MasterCatalog(CATEGORY_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    this.product.setSalesCatalogs(Collections.emptyList());

    this.itemCatalogMap.put(CATEGORY_CODE, itemCatalogs);

    this.merchantProfile = new ProfileResponse();
    this.price = new Price();
    this.price.setOfferPrice(ProductForTransactionServiceImplTest.RELEVANT_ITEM_PRICE);

    Set<Price> prices = new HashSet<Price>();
    prices.add(this.price);
    this.item.setPrice(prices);
    this.merchantProfileResponse =
        new GdnRestSingleResponse<ProfileResponse>(this.merchantProfile,
            ProductForTransactionServiceImplTest.REQUEST_ID);

    when(
        this.itemService.getItem(ProductForTransactionServiceImplTest.STORE_ID,
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME,
            ProductForTransactionServiceImplTest.ITEM_SKU,
            ProductForTransactionServiceImplTest.NEED_MASTER_DATA_DETAIL, false, true, false, null, false, false)).thenReturn(this.item);

    when(
        this.productService.getProductForView(ProductForTransactionServiceImplTest.STORE_ID,
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME, this.item.getProductSku(),
            ProductForTransactionServiceImplTest.NEED_MASTER_DATA_DETAIL, false)).thenReturn(
        this.product);

    this.listOfCategoriesList = new ArrayList<List<CategoryResponse>>();

    when(
        this.productHelperService.constructListOfCategoriesListOfProduct(
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME, new ArrayList<String>())).thenReturn(
        this.listOfCategoriesList);

    this.itemCatalogs = new ArrayList<ItemCatalogVO>();

    when(this.objectConverterService.convertToListOfItemCatalog(this.listOfCategoriesList))
        .thenReturn(this.itemCatalogs);

    this.productVO = new ProductForTransactionVO();

    // null merchant profile response
    this.itemWithNullMerchantProfileResponse = new Item();
    this.itemWithNullMerchantProfileResponse
        .setItemSku(ProductForTransactionServiceImplTest.ITEM_SKU_WITH_NULL_MERCHANT_PROFILE_RESPONSE);
    this.itemWithNullMerchantProfileResponse
        .setProductSku(ProductForTransactionServiceImplTest.PRODUCT_SKU_WITH_NULL_MERCHANT_PROFILE_RESPONSE);
    this.itemWithNullMerchantProfileResponse
        .setStoreId(ProductForTransactionServiceImplTest.STORE_ID);

    when(
        this.itemService.getItem(this.itemWithNullMerchantProfileResponse.getStoreId(),
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME,
            this.itemWithNullMerchantProfileResponse.getItemSku(),
            ProductForTransactionServiceImplTest.NEED_MASTER_DATA_DETAIL, false, true, false, null, false, false)).thenReturn(
        this.itemWithNullMerchantProfileResponse);

    this.productWithNullMerchantProfileResponse = new Product();
    this.productWithNullMerchantProfileResponse
        .setMerchantCode(ProductForTransactionServiceImplTest.MERCHANT_ID_NOT_EXIST);
    this.productWithNullMerchantProfileResponse
        .setStoreId(ProductForTransactionServiceImplTest.STORE_ID);

    when(
        this.productService.getProductForView(
            this.productWithNullMerchantProfileResponse.getStoreId(),
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME,
            this.itemWithNullMerchantProfileResponse.getProductSku(),
            ProductForTransactionServiceImplTest.NEED_MASTER_DATA_DETAIL, false)).thenReturn(
        this.productWithNullMerchantProfileResponse);

    // null item response
    this.itemWithNullItemResponse = new Item();
    this.itemWithNullItemResponse
        .setItemSku(ProductForTransactionServiceImplTest.ITEM_SKU_WITH_NULL_RESPONSE);
    this.itemWithNullItemResponse.setStoreId(ProductForTransactionServiceImplTest.STORE_ID);

    doThrow(new ApplicationRuntimeException()).when(this.itemService).getItem(
        this.itemWithNullItemResponse.getStoreId(),
        ProductForTransactionServiceImplTest.REQUEST_ID,
        ProductForTransactionServiceImplTest.USERNAME, this.itemWithNullItemResponse.getItemSku(),
        ProductForTransactionServiceImplTest.NEED_MASTER_DATA_DETAIL, false, true, false, null, false, false);

    // null product response
    this.itemWithNullProductResponse = new Item();
    this.itemWithNullProductResponse
        .setItemSku(ProductForTransactionServiceImplTest.ITEM_SKU_WITH_NULL_PRODUCT_RESPONSE);
    this.itemWithNullProductResponse
        .setProductSku(ProductForTransactionServiceImplTest.PRODUCT_SKU_WITH_NULL_PRODUCT_RESPONSE);
    this.itemWithNullProductResponse.setStoreId(ProductForTransactionServiceImplTest.STORE_ID);

    when(
        this.itemService.getItem(this.itemWithNullProductResponse.getStoreId(),
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME,
            this.itemWithNullProductResponse.getItemSku(),
            ProductForTransactionServiceImplTest.NEED_MASTER_DATA_DETAIL, false, true, false, null, false, false)).thenReturn(
        this.itemWithNullProductResponse);

    this.productWithNullProductResponse = new Product();
    this.productWithNullProductResponse
        .setProductSku(ProductForTransactionServiceImplTest.PRODUCT_SKU_WITH_NULL_PRODUCT_RESPONSE);
    this.productWithNullProductResponse.setStoreId(ProductForTransactionServiceImplTest.STORE_ID);

    doThrow(new ApplicationRuntimeException()).when(this.productService).getProductForView(
        this.productWithNullProductResponse.getStoreId(),
        ProductForTransactionServiceImplTest.REQUEST_ID,
        ProductForTransactionServiceImplTest.USERNAME,
        this.itemWithNullProductResponse.getProductSku(),
        ProductForTransactionServiceImplTest.NEED_MASTER_DATA_DETAIL, false);

    // null product item detail response
    this.itemWithNullProductItemDetailResponse = new Item();
    this.itemWithNullProductItemDetailResponse
        .setItemSku(ProductForTransactionServiceImplTest.ITEM_SKU_WITH_NULL_PRODUCT_ITEM_DETAIL_RESPONSE);
    this.itemWithNullProductItemDetailResponse
        .setProductSku(ProductForTransactionServiceImplTest.PRODUCT_SKU_WITH_NULL_PRODUCT_ITEM_DETAIL_RESPONSE);
    this.itemWithNullProductItemDetailResponse
        .setStoreId(ProductForTransactionServiceImplTest.STORE_ID);

    when(
        this.itemService.getItem(this.itemWithNullProductItemDetailResponse.getStoreId(),
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME,
            this.itemWithNullProductItemDetailResponse.getItemSku(),
            ProductForTransactionServiceImplTest.NEED_MASTER_DATA_DETAIL, false, true, false, null, false, false)).thenReturn(
        this.itemWithNullProductItemDetailResponse);

    this.productWithNullProductItemDetailResponse = new Product();
    this.productWithNullProductItemDetailResponse
        .setMerchantCode(ProductForTransactionServiceImplTest.MERCHANT_ID);
    this.productWithNullProductItemDetailResponse
        .setProductSku(ProductForTransactionServiceImplTest.PRODUCT_SKU_WITH_NULL_PRODUCT_ITEM_DETAIL_RESPONSE);
    this.productWithNullProductItemDetailResponse
        .setSynchronized(ProductForTransactionServiceImplTest.IS_SYNCHRONIZED);
    this.productWithNullProductItemDetailResponse
        .setStoreId(ProductForTransactionServiceImplTest.STORE_ID);

    when(
        this.productService.getProductForView(
            this.productWithNullProductItemDetailResponse.getStoreId(),
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME,
            this.itemWithNullProductItemDetailResponse.getProductSku(),
            ProductForTransactionServiceImplTest.NEED_MASTER_DATA_DETAIL, false)).thenReturn(
        this.productWithNullProductItemDetailResponse);

    when(
        this.productHelperService.constructListOfCategoriesListOfProduct(
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME, new ArrayList<String>())).thenReturn(
        this.listOfCategoriesList);

    // Unsynchronized
    this.itemWithUnsynchronized = new Item();
    this.itemWithUnsynchronized
        .setProductSku(ProductForTransactionServiceImplTest.PRODUCT_SKU_UNSYNCHRONIZED);

    when(
        this.itemService.getItem(ProductForTransactionServiceImplTest.STORE_ID,
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME,
            ProductForTransactionServiceImplTest.ITEM_SKU_UNSYNCHRONIZED,
            ProductForTransactionServiceImplTest.NEED_MASTER_DATA_DETAIL, false, true, false, null, false, false)).thenReturn(
        this.itemWithUnsynchronized);

    this.productWithUnsynchronized = new Product();
    this.productWithUnsynchronized
        .setMerchantCode(ProductForTransactionServiceImplTest.MERCHANT_ID);
    this.productWithUnsynchronized
        .setSynchronized(!(ProductForTransactionServiceImplTest.IS_SYNCHRONIZED));

    when(
        this.productService.getProductForView(ProductForTransactionServiceImplTest.STORE_ID,
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME,
            ProductForTransactionServiceImplTest.PRODUCT_SKU_UNSYNCHRONIZED,
            ProductForTransactionServiceImplTest.NEED_MASTER_DATA_DETAIL, false)).thenReturn(
        this.productWithUnsynchronized);

    this.listOfCategoriesListWithUnsynchronized = new ArrayList<List<CategoryResponse>>();

    when(
        this.productHelperService.constructListOfCategoriesListOfProduct(
            ProductForTransactionServiceImplTest.REQUEST_ID,
            ProductForTransactionServiceImplTest.USERNAME, new ArrayList<String>())).thenReturn(
        this.listOfCategoriesListWithUnsynchronized);

    this.productVOWithUnsynchronized = new ProductForTransactionVO();

    when(
        this.objectConverterService.convertToProductForTransaction(this.productWithUnsynchronized,
            this.itemWithUnsynchronized, this.itemCatalogs)).thenReturn(
        this.productVOWithUnsynchronized);

    when(
        this.objectConverterService.convertToProductForTransaction(this.product, this.item,
            this.itemCatalogs)).thenReturn(this.productVO);

    this.itemsGetPriceForPulsa = new ArrayList<Item>();
    this.itemsGetPriceForPulsa.add(this.item);
    this.itemSkuList = new ArrayList<String>();
    this.itemSkuList.add(ProductForTransactionServiceImplTest.ITEM_SKU);
    this.itemSkuListNotFound = new ArrayList<String>();
    this.itemSkuListNotFound.add(ProductForTransactionServiceImplTest.ITEM_SKU_NOT_FOUND);
    when(
        this.itemService.getItemPriceAndViewConfigs(ProductForTransactionServiceImplTest.STORE_ID,
            this.itemSkuList)).thenReturn(this.itemsGetPriceForPulsa);
    when(
        this.productHelperService.getCurrentBuyableStatusForItem(this.item.getItemViewConfigs(),
            ProductForTransactionServiceImplTest.CHANNEL, item.isArchived())).thenReturn(true);
    when(
        this.productHelperService.getRelevantItemPrice(this.item.getPrice(),
            ProductForTransactionServiceImplTest.CHANNEL)).thenReturn(this.price);
    when(
        this.itemService.getItemPriceAndViewConfigs(ProductForTransactionServiceImplTest.STORE_ID,
            this.itemSkuListNotFound)).thenReturn(new ArrayList<Item>());

    this.offlineItemSkuVO = new ItemSkuVO(ProductForTransactionServiceImplTest.ITEM_SKU,
        ProductForTransactionServiceImplTest.USING_OFFLINE_STORE_DATA_FALSE, ProductForTransactionServiceImplTest.BUSINESS_PARTNER_CODE,
        ProductForTransactionServiceImplTest.PICKUP_POINT_CODE);
    this.onlineItemSkuVO = new ItemSkuVO(ProductForTransactionServiceImplTest.ITEM_SKU,
        ProductForTransactionServiceImplTest.USING_OFFLINE_STORE_DATA_TRUE, ProductForTransactionServiceImplTest.BUSINESS_PARTNER_CODE,
        ProductForTransactionServiceImplTest.PICKUP_POINT_CODE);
    this.itemSkuVOList = new ArrayList<ItemSkuVO>();
    this.itemSkuVOList.add(offlineItemSkuVO);
    this.itemSkuVOList.add(onlineItemSkuVO);

    this.offlineItem = new OfflineItem();
    this.offlineItem.setItemSku(ProductForTransactionServiceImplTest.ITEM_SKU);
    this.offlineItem.setMerchantSku(ProductForTransactionServiceImplTest.MERCHANT_SKU);
    this.offlineItem.setPickupPointCode(ProductForTransactionServiceImplTest.PICKUP_POINT_CODE);
    this.offlineItem.setExternalPickupPointCode(ProductForTransactionServiceImplTest.EXTERNAL_PICKUP_POINT_CODE);
    this.offlineItem.setListPrice(ProductForTransactionServiceImplTest.LIST_PRICE);
    this.offlineItem.setOfferPrice(ProductForTransactionServiceImplTest.OFFER_PRICE);

    this.pickupPointExternalList = new ArrayList<>();
    this.pickupPointExternalList.add(ProductForTransactionServiceImplTest.EXTERNAL_PICKUP_POINT_CODE);

    this.externalPickupPointCodeResponseDetail = new ExternalPickupPointCodeResponseDetail();
    this.externalPickupPointCodeResponseDetail.setCode(ProductForTransactionServiceImplTest.PICKUP_POINT_CODE);
    this.externalPickupPointCodeResponseDetail.setExternalPickupPointCode(ProductForTransactionServiceImplTest.EXTERNAL_PICKUP_POINT_CODE);
    this.externalPickupPointCodeResponseDetailList = new ArrayList<>();
    this.externalPickupPointCodeResponseDetailList.add(this.externalPickupPointCodeResponseDetail);

    this.externalPickupPointCodeResponse = new ExternalPickupPointCodeResponse();
    this.externalPickupPointCodeResponse.setExternalPickupPointCodeResponseDetails(externalPickupPointCodeResponseDetailList);

    this.offlineItem2 = new Item();
    this.offlineItem2.setItemSku(ProductForTransactionServiceImplTest.ITEM_SKU);
    this.offlineItem2.setMerchantSku(ProductForTransactionServiceImplTest.MERCHANT_SKU);
    productVO.setItemSku(ITEM_SKU);
    productVO.setItemCode(ITEM_CODE);
    productItemDetailVO.setItemCatalogs(itemCatalogs);
    productVO.setItemDetail(productItemDetailVO);
    item.setItemCode(ITEM_CODE);
    productCategoryResponse.setCategory(new CategoryResponse());
    productCategoryResponse.getCategory().setCategoryCode(CATEGORY_CODE);
    productCategoryResponse.getCategory().setCatalog(new CatalogResponse(CATALOG_CODE, CATALOG_CODE, CATALOG_CODE));
    productMasterDataResponse.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    wholesaleRuleVO.setDiscountPercentage(10.0);
    promoBundlingDetailResponseVO.setWholesaleRules(Arrays.asList(wholesaleRuleVO));
    promoBundlingDetailResponseVO.setItemSku(ITEM_SKU);
    activePromoBundlingResponseVO.setPromoBundlingDetailResponseVOList(Arrays.asList(promoBundlingDetailResponseVO));


    ReflectionTestUtils.setField(productPulsaServiceImpl, "maxItemsForTransaction", 90);
    ReflectionTestUtils.setField(productPulsaServiceImpl, "fetchPcbMasterData", true);
    minimumPriceSystemParameter.setValue(String.valueOf(MIN_PRICE));
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemViewConfigList = new HashSet<>(Arrays
        .asList(new ItemViewConfig(true, false, "DEFAULT", new ItemDiscoverableSchedule(), new ItemBuyableSchedule())));
    itemPickupPoint.setItemViewConfig(itemViewConfigList);
    itemPickupPoint.setPrice(prices);
    when(itemPickupPointService.findByItemSkuInAndDelivery(anyString(), anyList(), anyBoolean()))
        .thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(anyString(), anyString(), anyString()))
        .thenReturn(itemPickupPoint);
    Mockito.when(this.itemPickupPointService.findByItemSkusAndDelivery(STORE_ID,
        Collections.singletonList(ITEM_SKU), true))
      .thenReturn(Collections.singletonList(itemPickupPoint));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.productHelperService);
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.channelService);
    verifyNoMoreInteractions(this.catalogService);
    verifyNoMoreInteractions(masterDataConstructorService);
    verifyNoMoreInteractions(itemPriceService);
    verifyNoMoreInteractions(systemParameterService);
    verifyNoMoreInteractions(promotionOutbound);
  }

}
