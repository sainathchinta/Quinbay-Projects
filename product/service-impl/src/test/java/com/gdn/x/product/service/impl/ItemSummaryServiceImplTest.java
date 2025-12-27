package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.util.ReflectionTestUtils.setField;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
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
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.api.ItemRepositoryCustom;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.AdjustmentType;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.BusinessPartnerPromo;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.BulkItemSummaryRequestVo;
import com.gdn.x.product.model.vo.CampaignItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.ItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemSummaryResponseVO;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.ItemsSummaryDetailRequestVo;
import com.gdn.x.product.model.vo.OfflineItemPriceVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.model.vo.UpdateItemSummaryRequestVo;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.enums.ProductStatus;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemImagesListResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.BusinessPartnerPromoService;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ItemViewConfigService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.ProductAndItemSolrConstructorService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.ItemSummaryUtil;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class ItemSummaryServiceImplTest {

  private static final String RANDOM = "random";

  private static final String PRODUCT_NAME = "product-name";

  private static final String ATTRIBUTE_VALUE = "ATTRIBUTE_VALUE";

  private static final String ATTRIBUTE_NAME = "ATTRIBUTE_NAME";

  private static final String ATTRIBUTE_CODE = "ATTRIBUTE_CODE";

  private static final boolean NEED_MASTER_DATA_DETAIL = false;

  private static final String PRODUCT_SKU = "PRODUCT-SKU";

  private static final String PRODUCT_SKU2 = "PRODUCT-SKU2";

  private static final String PRODUCT_SKU3 = "PRODUCT-SKU3";

  private static final String PRODUCT_CODE = "MTA-100001";

  private static final boolean IS_LATE_FULFILLMENT = true;

  private static final String PICKUP_POINT_CODE = "pickup-point-code";

  private static final String DEFAULT_CHANNEL = "default channel";

  private static final String MERCHANT_SKU = "merchant-sku";

  private static final String NEW_MERCHANT_SKU = "new-merchant-sku";

  private static final String MERCHANT_CODE = "merchant-code";

  private static final String ITEM_NAME = "item-name";

  private static final String ITEM_NAME_1 = "item-name-1";

  private static final String ITEM_SKU = "ITEM-SKU";

  private static final String IMAGE_URL = "/x/y/z.jpg";

  private static final String ITEM_SKU_1 = "ITEM_SKU-1";

  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "request-id";

  private static final String STORE_ID = "store-id";

  private static final String SORT_BY = "sortBy";

  private static final String ORDER_BY = "orderBy";

  private static final String DELIMITER = "delimiter";

  private static final String PRISTINE_ID = "pristineId";

  private static final String PICKUP_POINT_NAME = "pickupPointName";

  private static final String MASTER_CATALOG = "catalog" + ItemSummaryServiceImplTest.DELIMITER
      + "categoryCode";

  private static final List<String> SALES_CATALOG = Arrays.asList("salesCatalog1"
      + ItemSummaryServiceImplTest.DELIMITER + "salesCategoryCode1", "salesCatalog1"
      + ItemSummaryServiceImplTest.DELIMITER + "salesCategoryCode2", "salesCatalog2"
      + ItemSummaryServiceImplTest.DELIMITER + "salesCategoryCode3", "salesCatalog2"
      + ItemSummaryServiceImplTest.DELIMITER + "salesCategoryCode4");

  private static final List<String> DISCOVERABLE = Arrays.asList(ChannelName.DEFAULT
      + ItemSummaryServiceImplTest.DELIMITER + "true", ChannelName.ANDROID
      + ItemSummaryServiceImplTest.DELIMITER + "false");

  private static final List<String> BUYABLE = Arrays.asList(ChannelName.DEFAULT
      + ItemSummaryServiceImplTest.DELIMITER + "true", ChannelName.ANDROID
      + ItemSummaryServiceImplTest.DELIMITER + "false");

  private static final List<String> ITEM_IMAGES = Arrays.asList("true"
      + ItemSummaryServiceImplTest.DELIMITER + "imagepath1" + ItemSummaryServiceImplTest.DELIMITER
      + "0", "false" + ItemSummaryServiceImplTest.DELIMITER + "imagepath2"
      + ItemSummaryServiceImplTest.DELIMITER + "1");

  private static final List<String> OFFLINE_PRICES = Arrays.asList(
      ItemSummaryServiceImplTest.PICKUP_POINT_CODE + ItemSummaryServiceImplTest.DELIMITER + "10000",
      "pp-02" + ItemSummaryServiceImplTest.DELIMITER + "20000",
      "pp-03" + ItemSummaryServiceImplTest.DELIMITER + "WRONG_PRICE");

  private static final Pageable PAGE = PageRequest.of(0, 1);

  private static final PageRequest PAGE_REQUEST = PageRequest.of(0, 1);

  private static final String ITEMS_NOT_FOUND =
      "No matching item data found for the requested parameters";

  private static final String ITEM_CODE = "item-code";

  private static final String SELLER_PROMO_BUNDLINGS_SWITCH = "sellerPromoBundlingsSwitch";

  private static final String  TRUE = "true";

  private static final String FALSE = "false";

  private static final String ACTIVE_PROMO_BUNDLING_TEST = "ACTIVE_PROMO_BUNDLING_TEST";

  private static final Long SIZE = 1l;
  private static final int QUANTITY = 12;

  private static final String CATEGORY_CODE = "category-code";
  private static final String BRAND = "brand";

  @InjectMocks
  private ItemSummaryServiceImpl itemSummaryFilterServiceImpl;

  @Mock
  private ProductAndItemSolrRepository productAndItemSolrRepository;

  @Mock
  private ItemPriceService itemPriceService;

  @Mock
  private ItemService itemService;

  @Mock
  private CachedService cachedService;

  @Mock
  private BusinessPartnerPromoService businessPartnerPromoService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private ProductService productService;

  @Mock
  private SaveOperationService saveOperationService;

  @Mock
  private Page<ProductAndItemSolr> solrResult;

  @Mock
  private ProductAndItemSolrConstructorService solrDataConstructor;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private OfflineItemService offlineItemService;

  @Mock
  private ItemHelperService itemHelperService;

  @Mock
  private ItemViewConfigService itemViewConfigService;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private ItemSummaryUtil itemSummaryUtil;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private MasterDataService masterDataService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private ChannelService channelService;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Mock
  private ItemRepositoryCustom itemRepositoryCustom;

  @Mock
  private ProductCacheableService productCacheableService;

  @Mock
  private GdnMapper gdnMapper;

  @Captor
  private ArgumentCaptor<Item> itemArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<Item>> itemListArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<Map<String, Set<ItemViewConfig>>> itemViewConfigMapArgumentCaptor;

  @Captor
  private ArgumentCaptor<Map<String, Set<Price>>> priceMapMapArgumentCaptor;

  private List<ProductAndItemSolr> solrContentResult;

  private UpdateItemSummaryRequestVo updateItemSummaryRequestVo;

  private ItemSummaryRequestVO itemSummaryRequestVo;
  private ItemSummaryRequestVO itemSummaryRequestVo1;

  private BulkItemSummaryRequestVo bulkItemSummaryRequestVo;

  private CampaignItemSummaryRequestVO campaignItemSummaryRequestVO;
  private ProductAndItemSolr solrResultElement;
  private ItemPickupPoint itemPickupPoint;
  private Set<Price> prices;
  private Set<ItemViewConfig> itemViewConfigs;
  private ProductAndItemsVO productAndItems;
  private SystemParameter enableDeferredSolrReindexParameter;
  private ProductItemsVo productItemsVo;
  private SystemParameter singleItemSummarySolrSwitch;

  private Item item;
  private Item item1 = new Item();

  private Product product;

  private List<OfflineItem> offlineItemList;

  private OfflineItem offlineItem;
  private List<OfflineItemPriceVO> offlineItems;
  private Map<String, Set<Price>> mapOfPrices;
  private ItemsSummaryDetailRequestVo itemsSummaryDetailRequestVo;
  private Map<String, String> itemCodeAndSkuMap = new HashMap<>();
  private Set<ItemViewConfig> itemViewConfigList;
  private Set<Price> prices1;
  private List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
  private SystemParameter fetchPCBMasterDataParameter;
  private BundleRecipe bundleRecipe;
  private BundleRecipe bundleRecipe2;

  @Captor
  private ArgumentCaptor<Map<String, Double>> mapOfOriginalPrice;
  private Map<String, MasterDataItem> masterDataItemMap;

  @Test
  public void getArchivedItemSummaryByFilterTestWithNullFilter() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl.getItemSummaryByArchivedFilter(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.REQUEST_ID, null, ItemSummaryServiceImplTest.PAGE_REQUEST));
  }

  @Test
  public void getItemNameByListOfItemSkuTest() {
    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(ItemSummaryServiceImplTest.ITEM_SKU);
    String[] includeFields = {ProductFieldNames.ITEM_SKU, ProductFieldNames.GENERATED_ITEM_NAME,
        ProductFieldNames.GENERATED_ITEM_NAME_IN_MASTER_DATA_ITEM, ProductFieldNames.IS_SYNCHRONIZED};
    this.item.setItemSku(ItemSummaryServiceImplTest.ITEM_SKU);
    this.item.setGeneratedItemName(ItemSummaryServiceImplTest.ITEM_NAME);
    this.item.setSynchronized(true);
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    when(
        this.itemService.getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields)).thenReturn(
        itemList);

    Map<String, String> result =
        this.itemSummaryFilterServiceImpl.getItemNameByItemSkus(ItemSummaryServiceImplTest.STORE_ID,
            Arrays.asList(ItemSummaryServiceImplTest.ITEM_SKU), true);

    verify(this.itemService).getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields);
    assertEquals(ItemSummaryServiceImplTest.ITEM_NAME, result.get(ItemSummaryServiceImplTest.ITEM_SKU));
  }

  @Test
  public void getItemNameByListOfItemSkuIncludeMarkForDeleteTest() {
    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(ItemSummaryServiceImplTest.ITEM_SKU);
    String[] includeFields = {ProductFieldNames.ITEM_SKU, ProductFieldNames.GENERATED_ITEM_NAME,
        ProductFieldNames.GENERATED_ITEM_NAME_IN_MASTER_DATA_ITEM, ProductFieldNames.IS_SYNCHRONIZED};
    this.item.setItemSku(ItemSummaryServiceImplTest.ITEM_SKU);
    this.item.setGeneratedItemName(ItemSummaryServiceImplTest.ITEM_NAME);
    this.item.setSynchronized(true);
    this.item1.setItemSku(ItemSummaryServiceImplTest.ITEM_SKU_1);
    this.item1.setGeneratedItemName(ItemSummaryServiceImplTest.ITEM_NAME_1);
    this.item1.setSynchronized(true);
    this.item1.setMarkForDelete(true);
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    itemList.add(item1);
    when(
        this.itemService.getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields)).thenReturn(
        itemList);

    Map<String, String> result =
        this.itemSummaryFilterServiceImpl.getItemNameByItemSkus(ItemSummaryServiceImplTest.STORE_ID,
            Arrays.asList(ItemSummaryServiceImplTest.ITEM_SKU), false);

    verify(this.itemService).getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields);
  }

  @Test
  public void getItemNameByListOfItemSkuSynchronizedFalseTest() {
    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(ItemSummaryServiceImplTest.ITEM_SKU);
    String[] includeFields = {ProductFieldNames.ITEM_SKU, ProductFieldNames.GENERATED_ITEM_NAME,
        ProductFieldNames.GENERATED_ITEM_NAME_IN_MASTER_DATA_ITEM, ProductFieldNames.IS_SYNCHRONIZED};
    this.item.setItemSku(ItemSummaryServiceImplTest.ITEM_SKU);
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setGeneratedItemName(ItemSummaryServiceImplTest.ITEM_NAME);
    this.item.setMasterDataItem(masterDataItem);
    this.item.setSynchronized(false);
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    when(
        this.itemService.getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields)).thenReturn(
        itemList);

    Map<String, String> result =
        this.itemSummaryFilterServiceImpl.getItemNameByItemSkus(ItemSummaryServiceImplTest.STORE_ID,
            Arrays.asList(ItemSummaryServiceImplTest.ITEM_SKU), false);

    verify(this.itemService).getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields);
    assertEquals(ItemSummaryServiceImplTest.ITEM_NAME, result.get(ItemSummaryServiceImplTest.ITEM_SKU));
  }

  @Test
  public void getItemNameByListOfItemSkuTestWithEmptyItemSkus() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl.getItemNameByItemSkus(ItemSummaryServiceImplTest.STORE_ID,
        new ArrayList<String>(), false));
  }

  @Test
  public void getItemNameByListOfItemSkuTestWithNullItemSkus() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl.getItemNameByItemSkus(ItemSummaryServiceImplTest.STORE_ID,
        null, false));
  }

  @Test
  public void getItemSummaryByArchivedFilterTest() {
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, null, null,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyList())).thenReturn(
        new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.getItemSummaryByArchivedFilter(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.REQUEST_ID, this.itemSummaryRequestVo,
        ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, null, null, ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyList());
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
  }

  @Test
  public void getItemSummaryByArchivedFilterTestWithUnexpectedProductType() {
    itemPickupPoint.setPromoBundling(item.isPromoBundling());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    this.solrResultElement.setProductType(ItemSummaryServiceImplTest.RANDOM);
    when(this.solrResult.getContent()).thenReturn(this.solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyList())).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    ItemSummaryPageResponseVo response = this.itemSummaryFilterServiceImpl
        .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
            ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
            this.itemSummaryRequestVo, ORDER_BY, SORT_BY, ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY, ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyList());
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    assertTrue(response.getItemSummaryResponses().get(0).isMerchantPromoDiscountActivated());
    assertTrue(response.getItemSummaryResponses().get(0).isPromoBundling());
  }

  @Test
  public void getItemSummaryPristineIdAndPriceNull() {
    solrResultElement.setPristineId(null);
    itemPickupPoint.setMerchantPromoDiscount(item.isMerchantPromoDiscount());
    itemPickupPoint.setPromoBundling(item.isPromoBundling());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    this.solrResultElement.setProductType(ItemSummaryServiceImplTest.RANDOM);
    when(this.solrResult.getContent()).thenReturn(this.solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    ItemSummaryPageResponseVo response = this.itemSummaryFilterServiceImpl
        .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
            ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
            this.itemSummaryRequestVo, ORDER_BY, SORT_BY, ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY, ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    assertTrue(response.getItemSummaryResponses().get(0).isMerchantPromoDiscountActivated());
    assertTrue(response.getItemSummaryResponses().get(0).isPromoBundling());
  }

  @Test
  public void getItemSummaryByFilterTest() {
    solrResultElement.setMerchantPromoDiscountActivated(Boolean.FALSE);
    solrResultElement.setWholesalePriceActivated(Boolean.TRUE);
    solrResultElement.setProductScoreTotal(90.0);
    itemPickupPoint.setActivePromoBundlings(item.getActivePromoBundlings());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    ItemSummaryPageResponseVo response = this.itemSummaryFilterServiceImpl
        .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
            ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
            this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(),isNull(), any(ProductAndItemSolr.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    assertFalse(response.getItemSummaryResponses().get(0).isMerchantPromoDiscountActivated());
    Assertions.assertEquals(Collections.emptySet(), response.getItemSummaryResponses().get(0).getPriceEditDisabledReasons());
    assertTrue(response.getItemSummaryResponses().get(0).getWholesalePriceActivated());
    assertTrue(response.getItemSummaryResponses().get(0).getActivePromoBundlings()
        .contains(Constants.WHOLESALE_PRICE));
  }

  @Test
  public void getItemSummaryByFilterWithPreOrderStatusTest() {
    solrResultElement.setMerchantPromoDiscountActivated(Boolean.FALSE);
    solrResultElement.setWholesalePriceActivated(Boolean.TRUE);
    solrResultElement.setProductScoreTotal(90.0);
    itemPickupPoint.setActivePromoBundlings(item.getActivePromoBundlings());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    Map<String, PreOrder> map = new HashMap<>();
    map.put(PRODUCT_SKU, null);
    when(productService.getPreOrderStatusByProductSkus(Mockito.anyList())).thenReturn(map);
    itemSummaryRequestVo.setPreOrderStatus(true);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    ItemSummaryPageResponseVo response = this.itemSummaryFilterServiceImpl
        .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
            ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
            this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(this.productService).getPreOrderStatusByProductSkus(Mockito.anyList());
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    assertFalse(response.getItemSummaryResponses().get(0).isMerchantPromoDiscountActivated());
    Assertions.assertEquals(Collections.emptySet(), response.getItemSummaryResponses().get(0).getPriceEditDisabledReasons());
    assertTrue(response.getItemSummaryResponses().get(0).getWholesalePriceActivated());
    assertTrue(response.getItemSummaryResponses().get(0).getActivePromoBundlings()
        .contains(Constants.WHOLESALE_PRICE));
    assertFalse(response.getItemSummaryResponses().get(0).isPreOrder());
  }

  @Test
  public void getItemSummaryByFilterWithPreOrderStatusTrueTest() {
    solrResultElement.setMerchantPromoDiscountActivated(Boolean.FALSE);
    solrResultElement.setWholesalePriceActivated(Boolean.TRUE);
    solrResultElement.setProductScoreTotal(90.0);
    itemPickupPoint.setActivePromoBundlings(item.getActivePromoBundlings());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    Map<String, PreOrder> map = new HashMap<>();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    map.put(PRODUCT_SKU, preOrder);
    when(productService.getPreOrderStatusByProductSkus(Mockito.anyList())).thenReturn(map);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    itemSummaryRequestVo.setPreOrderStatus(true);
    ItemSummaryPageResponseVo response = this.itemSummaryFilterServiceImpl
        .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
            ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
            this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(this.productService).getPreOrderStatusByProductSkus(Mockito.anyList());
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    assertFalse(response.getItemSummaryResponses().get(0).isMerchantPromoDiscountActivated());
    Assertions.assertEquals(Collections.emptySet(), response.getItemSummaryResponses().get(0).getPriceEditDisabledReasons());
    assertTrue(response.getItemSummaryResponses().get(0).getWholesalePriceActivated());
    assertTrue(response.getItemSummaryResponses().get(0).getActivePromoBundlings()
        .contains(Constants.WHOLESALE_PRICE));
    assertTrue(response.getItemSummaryResponses().get(0).isPreOrder());
  }

  @Test
  public void getItemSummaryByFilterTest_forceReviewFlag() {
    item.setForceReview(true);
    solrResultElement.setMerchantPromoDiscountActivated(Boolean.FALSE);
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    ItemSummaryPageResponseVo response = this.itemSummaryFilterServiceImpl
        .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
            ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
            this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    assertFalse(response.getItemSummaryResponses().get(0).isMerchantPromoDiscountActivated());
    //    Assertions.assertTrue(response.getItemSummaryResponses().get(0).isForceReview());
  }

  @Test
  public void getItemSummaryByFilterTestOriginalSellingPriceTest() {
    item.getPrice().stream().findFirst().get().setMerchantPromoDiscountPrice(
        new DiscountPrice(100, new Date(), new Date(), item.getItemSku(), AdjustmentType.MERCHANT));
    item.getPrice().stream().findFirst().get().setOfferPrice(1000);
    item.getPrice().stream().findFirst().get().setListPrice(1000);
    Map<String, Set<Price>> itemMap = new HashMap<>();
    Item item1 = new Item();
    Set<Price> prices1 = new HashSet<>();
    prices1.add(new Price("IDR", 900, 1000, "DEFAULT", null, new Date()));
    BeanUtils.copyProperties(item, item1);
    item1.setPrice(prices1);
    itemMap.put(item1.getItemSku(), item1.getPrice());
    itemPickupPoint.setPrice(item.getPrice());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(itemMap);
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    ItemSummaryPageResponseVo response = this.itemSummaryFilterServiceImpl
        .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
            ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
            this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    Assertions.assertNotEquals(response.getItemSummaryResponses().get(0).getOriginalSellingPrice(),
        response.getItemSummaryResponses().get(0).getPrice().stream().findFirst().get().getOfferPrice());
  }

  @Test
  public void getItemSummaryByFilterTestOriginalSellingPriceEqaulsToofferPriceTestTest() {
    item.getPrice().stream().findFirst().get().setMerchantPromoDiscountPrice(
        new DiscountPrice(100, new Date(), new Date(), item.getItemSku(), AdjustmentType.MERCHANT));
    item.getPrice().stream().findFirst().get().setOfferPrice(1000);
    item.getPrice().stream().findFirst().get().setListPrice(1000);
    Map<String, Set<Price>> itemMap = new HashMap<>();
    itemMap.put(item.getItemSku(), item.getPrice());
    itemPickupPoint.setPrice(item.getPrice());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(itemMap);
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    ItemSummaryPageResponseVo response = this.itemSummaryFilterServiceImpl
        .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
            ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
            this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    Assertions.assertEquals(response.getItemSummaryResponses().get(0).getOriginalSellingPrice(),
        response.getItemSummaryResponses().get(0).getPrice().stream().findFirst().get().getOfferPrice(), 0);
    verify(itemService).isPriceEditDisabled(itemPickupPoint);
  }

  @Test
  public void getBulkItemSummaryByFilterTest() {
    when(
      this.productAndItemSolrRepository.getBulkProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.bulkItemSummaryRequestVo,
        ItemSummaryServiceImplTest.PAGE_REQUEST, SORT_BY, ORDER_BY)).thenReturn(this.solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyList())).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.productAndItemSolrRepository.getBulkProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID,
        this.bulkItemSummaryRequestVo, ItemSummaryServiceImplTest.PAGE_REQUEST, SORT_BY, ORDER_BY);
    this.itemSummaryFilterServiceImpl.getBulkItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
        this.bulkItemSummaryRequestVo, ItemSummaryServiceImplTest.PAGE_REQUEST, SORT_BY, ORDER_BY);
    verify(this.productAndItemSolrRepository, times(2)).getBulkProductAndItemsByFilter(
      ItemSummaryServiceImplTest.STORE_ID, this.bulkItemSummaryRequestVo,
      ItemSummaryServiceImplTest.PAGE_REQUEST, SORT_BY, ORDER_BY );
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyList());
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), anyList(), any(ProductAndItemSolr.class));
  }

  @Test
  public void getBulkItemSummaryByFilterTest_null() {
    solrContentResult.get(0).setPristineId(null);
    when(this.solrResult.getContent()).thenReturn(this.solrContentResult);
    when(
        this.productAndItemSolrRepository.getBulkProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.bulkItemSummaryRequestVo,
            ItemSummaryServiceImplTest.PAGE_REQUEST, SORT_BY, ORDER_BY)).thenReturn(this.solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyList())).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.getBulkItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
        this.bulkItemSummaryRequestVo, ItemSummaryServiceImplTest.PAGE_REQUEST, SORT_BY, ORDER_BY);
    verify(this.productAndItemSolrRepository).getBulkProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.bulkItemSummaryRequestVo,
        ItemSummaryServiceImplTest.PAGE_REQUEST, SORT_BY, ORDER_BY );
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyList());
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), anyList(), any(ProductAndItemSolr.class));
  }

  // Test case for skipping generate item summary when solrDataConstructor throws error
  @Test
  public void getItemSummaryByFilter_WithSkippingGenerateData_Test() {
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID,
        this.itemSummaryRequestVo, ORDER_BY, SORT_BY, ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(
        this.solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyList())).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    when(this.solrDataConstructor.constructMasterDataItemMainImages(Mockito.anyList())).thenThrow(
        new RuntimeException("error generate test"));
    ItemSummaryPageResponseVo result = null;
    try {
      result =
          this.itemSummaryFilterServiceImpl.getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
              ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID, this.itemSummaryRequestVo,
              ORDER_BY, SORT_BY, ItemSummaryServiceImplTest.PAGE_REQUEST);
    } finally {
      verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID,
          this.itemSummaryRequestVo, ORDER_BY, SORT_BY, ItemSummaryServiceImplTest.PAGE_REQUEST);
      verify(itemService).getItemPriceAndViewConfigs(STORE_ID,
          Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
      verify(this.solrDataConstructor).constructMasterDataItemMainImages(ItemSummaryServiceImplTest.ITEM_IMAGES);
      verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
      verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyList());
      assertTrue(CollectionUtils.isEmpty(result.getItemSummaryResponses()));
    }
  }


  @Test
  public void getItemSummaryByFilterTestWithNullFilter() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl.getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID, null, ORDER_BY, SORT_BY,
        ItemSummaryServiceImplTest.PAGE_REQUEST));
  }

  @Test
  public void getItemSummaryByFilterTestWithUnexpectedProductType() {
    this.itemSummaryRequestVo.setPickupPointCode(PICKUP_POINT_CODE);
    this.solrResultElement.setProductType(ItemSummaryServiceImplTest.RANDOM);
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    when(this.solrResult.getContent()).thenReturn(this.solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    ItemSummaryPageResponseVo responseVO = this.itemSummaryFilterServiceImpl
        .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
            ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
            this.itemSummaryRequestVo, ORDER_BY, SORT_BY, ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY, ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(anyString(), isNull(), any(ProductAndItemSolr.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    assertEquals(ItemSummaryServiceImplTest.PRODUCT_NAME, responseVO.getItemSummaryResponses().get(0).getProductName());
    assertEquals(ItemSummaryServiceImplTest.PRODUCT_SKU, responseVO.getItemSummaryResponses().get(0).getProductSku());
    assertEquals(ItemSummaryServiceImplTest.ITEM_NAME, responseVO.getItemSummaryResponses().get(0).getGeneratedItemName());
    assertEquals(ItemSummaryServiceImplTest.ITEM_SKU, responseVO.getItemSummaryResponses().get(0).getItemSku());
    assertEquals("item-code", responseVO.getItemSummaryResponses().get(0).getItemCode());
    assertEquals("brand", responseVO.getItemSummaryResponses().get(0).getBrand());
    assertEquals(ItemSummaryServiceImplTest.MERCHANT_CODE, responseVO.getItemSummaryResponses().get(0).getMerchantCode());
  }

  @Test
  public void getItemSummaryByFilterTestPickupPointNull() {
    this.itemSummaryRequestVo.setPickupPointCode(PICKUP_POINT_CODE);
    this.solrResultElement.setPickupPointCode(PICKUP_POINT_CODE);
    this.solrResultElement.setProductType(ItemSummaryServiceImplTest.RANDOM);
    itemPickupPoint.setItemSku("ITEM");
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    when(itemPickupPointService.findByItemSkuInAndDelivery(anyString(), anyList(), anyBoolean()))
        .thenReturn(Arrays.asList(itemPickupPoint));
    when(this.solrResult.getContent()).thenReturn(this.solrContentResult);
    ItemSummaryPageResponseVo responseVO = this.itemSummaryFilterServiceImpl
        .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
            ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
            this.itemSummaryRequestVo, ORDER_BY, SORT_BY, ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY, ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.itemPriceService).getDiscountItemPickupPoint(new ArrayList<>());
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
  }

  @Test
  public void getItemSummaryByFilterTestNullSolrResult() {
    when(this.productAndItemSolrRepository
        .getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemSummaryFilterServiceImpl
        .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
            ItemSummaryServiceImplTest.REQUEST_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST));
    verify(this.productAndItemSolrRepository)
        .getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST);
  }

  @Test
  public void getPromoItemSummaryByFilterTest() {
    when(this.productAndItemSolrRepository
        .getPromoProductAndItemsByFilter(STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY, PAGE_REQUEST))
        .thenReturn(this.solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyList())).thenReturn(new HashMap<>());
    this.itemSummaryFilterServiceImpl
        .getPromoItemSummaryByFilter(STORE_ID, REQUEST_ID, USERNAME, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            PAGE_REQUEST);
    verify(this.productAndItemSolrRepository)
        .getPromoProductAndItemsByFilter(STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY, PAGE_REQUEST);
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyList());
    verify(itemSummaryUtil).constructItemSummaryResponseForPromoItems(eq(REQUEST_ID), eq(USERNAME), eq(this.solrResult),
        priceMapMapArgumentCaptor.capture(), mapOfOriginalPrice.capture(), itemViewConfigMapArgumentCaptor.capture());
  }

  @Test
  public void getPromoItemSummaryByFilterTest_whenMasterCatalogNull() {
    when(this.productAndItemSolrRepository
        .getPromoProductAndItemsByFilter(STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY, PAGE_REQUEST))
        .thenReturn(this.solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyList())).thenReturn(new HashMap<>());
    this.itemSummaryFilterServiceImpl
        .getPromoItemSummaryByFilter(STORE_ID, REQUEST_ID, USERNAME, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getPromoProductAndItemsByFilter(STORE_ID, this.itemSummaryRequestVo,
        ORDER_BY, SORT_BY, PAGE_REQUEST);
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyList());
    verify(itemSummaryUtil).constructItemSummaryResponseForPromoItems(eq(REQUEST_ID), eq(USERNAME), eq(this.solrResult),
        priceMapMapArgumentCaptor.capture(), mapOfOriginalPrice.capture(), itemViewConfigMapArgumentCaptor.capture());
  }

  @Test
  public void getPromoItemSummaryByFilter_whenStoreIdNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl
        .getPromoItemSummaryByFilter(null, REQUEST_ID, USERNAME, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
            PAGE_REQUEST));
  }

  @Test
  public void getPromoItemSummaryByFilter_whenNullFilterTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl
        .getPromoItemSummaryByFilter(STORE_ID, REQUEST_ID, USERNAME, null, ORDER_BY, SORT_BY, PAGE_REQUEST));
  }

  @Test
  public void getItemNamesByKeywordTest() {
    when(this.productAndItemSolrRepository
        .getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo1, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    this.solrResultElement.setProductType(ItemSummaryServiceImplTest.RANDOM);
    when(this.solrResult.getContent()).thenReturn(this.solrContentResult);
    ItemSummaryPageResponseVo response = this.itemSummaryFilterServiceImpl
        .getItemNamesByKeyword(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
            ItemSummaryServiceImplTest.REQUEST_ID, this.itemSummaryRequestVo1, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository)
        .getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo1, ORDER_BY, SORT_BY,
            ItemSummaryServiceImplTest.PAGE_REQUEST);
  }

  @Test
  public void getItemNamesByKeywordTestWithNullFilter() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl.getItemNamesByKeyword(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID, null, ORDER_BY, SORT_BY,
        ItemSummaryServiceImplTest.PAGE_REQUEST));
  }

  @Test
  public void getProductNameByListOfProductSkuTest() {
    when(
        this.productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(
            ItemSummaryServiceImplTest.STORE_ID,
            Arrays.asList(ItemSummaryServiceImplTest.PRODUCT_SKU))).thenReturn(
        this.solrContentResult);
    this.product.setProductSku(ItemSummaryServiceImplTest.PRODUCT_SKU);
    this.product.setProductName(ItemSummaryServiceImplTest.PRODUCT_NAME);
    this.product.setSynchronized(true);
    List<Product> productList = new ArrayList<>();
    productList.add(product);
    String[] includedField = {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_NAME,
        ProductFieldNames.PRODUCT_NAME_IN_MASTER_DATA_PRODUCT, ProductFieldNames.IS_SYNCHRONIZED};
    Set<String> itemSet = new HashSet<>();
    itemSet.add(ItemSummaryServiceImplTest.PRODUCT_SKU);
    when(this.productService.getProductsByProductSkus(ItemSummaryServiceImplTest.STORE_ID, itemSet, includedField, false))
        .thenReturn(productList);

    Map<String, String> result =
        this.itemSummaryFilterServiceImpl.getProductNameByProductSkus(
            ItemSummaryServiceImplTest.STORE_ID,
            Arrays.asList(ItemSummaryServiceImplTest.PRODUCT_SKU));

    verify(this.productService).getProductsByProductSkus(ItemSummaryServiceImplTest.STORE_ID, itemSet, includedField, false);
    Assertions.assertEquals(ItemSummaryServiceImplTest.PRODUCT_NAME, result.get(ItemSummaryServiceImplTest.PRODUCT_SKU));
  }

  @Test
  public void getProductNameByListOfProductSkuSynchronizedFalseTest() {
    when(this.productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, Arrays.asList(ItemSummaryServiceImplTest.PRODUCT_SKU))).thenReturn(
        this.solrContentResult);

    this.product.setProductSku(ItemSummaryServiceImplTest.PRODUCT_SKU);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(ItemSummaryServiceImplTest.PRODUCT_NAME);
    this.product.setMasterDataProduct(masterDataProduct);
    this.product.setSynchronized(false);
    List<Product> productList = new ArrayList<>();
    productList.add(product);
    String[] includedField = {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_NAME,
        ProductFieldNames.PRODUCT_NAME_IN_MASTER_DATA_PRODUCT, ProductFieldNames.IS_SYNCHRONIZED};
    Set<String> itemSet = new HashSet<>();
    itemSet.add(ItemSummaryServiceImplTest.PRODUCT_SKU);
    when(this.productService.getProductsByProductSkus(ItemSummaryServiceImplTest.STORE_ID, itemSet,
        includedField, false)).thenReturn(productList);

    Map<String, String> result =
        this.itemSummaryFilterServiceImpl.getProductNameByProductSkus(ItemSummaryServiceImplTest.STORE_ID,
            Arrays.asList(ItemSummaryServiceImplTest.PRODUCT_SKU));

    verify(this.productService).getProductsByProductSkus(ItemSummaryServiceImplTest.STORE_ID, itemSet, includedField, false);
    Assertions.assertEquals(ItemSummaryServiceImplTest.PRODUCT_NAME, result.get(ItemSummaryServiceImplTest.PRODUCT_SKU));
  }

  @Test
  public void getProductNameByListOfProductSkuTestWithEmptyList() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl.getProductNameByProductSkus(
        ItemSummaryServiceImplTest.STORE_ID, new ArrayList<String>()));
  }

  @Test
  public void getProductNameByListOfProductSkuTestWithNullList() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl.getProductNameByProductSkus(
        ItemSummaryServiceImplTest.STORE_ID, null));
  }

  @Test
  public void getProductNamesByProductCodesTest() {
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_SKU);
    Set<String> productCodeSet = new HashSet<>();
    productCodeSet.add(PRODUCT_SKU);
    List<ProductAndItemSolr> response = new ArrayList<>();
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    productAndItemSolr.setProductName(PRODUCT_NAME);
    response.add(productAndItemSolr);
    when(
        productAndItemSolrRepository.findByStoreIdAndProductCodeInAndMarkForDeleteFalse(STORE_ID,
            productCodeSet)).thenReturn(response);

    Map<String, String> mapResponse =
        itemSummaryFilterServiceImpl.getProductNamesByProductCodes(STORE_ID, productCodes);
    assertEquals(PRODUCT_NAME, mapResponse.get(PRODUCT_SKU));
  }

  @BeforeEach
  public void init() throws Exception {
    openMocks(this);
    ReflectionTestUtils.setField(this.itemSummaryFilterServiceImpl, "maxShippingWeight", 50);
    ReflectionTestUtils.setField(this.itemSummaryFilterServiceImpl, "itemSkuListSize", 50);
    ReflectionTestUtils.setField(this.itemSummaryFilterServiceImpl, "productSkuListSize", 50);
    ReflectionTestUtils.setField(this.itemSummaryFilterServiceImpl, "getItemBasicDetailApiItemSkuListSize", 50);
    this.itemSummaryRequestVo = new ItemSummaryRequestVO();
    this.campaignItemSummaryRequestVO = new CampaignItemSummaryRequestVO();
    campaignItemSummaryRequestVO.setMerchantCode(MERCHANT_CODE);
    campaignItemSummaryRequestVO.setKeyword("abc");
    campaignItemSummaryRequestVO.setItemSku(ITEM_SKU);
    campaignItemSummaryRequestVO.setBrands(Arrays.asList("br1", "br2"));
    campaignItemSummaryRequestVO.setCategories(Arrays.asList("cat-1", "cat2"));
    ArrayList<String> itemSkus = new ArrayList<>();
    itemSkus.add(ITEM_SKU);
    this.itemSummaryRequestVo.setItemSkus(itemSkus);
    this.solrResultElement = new ProductAndItemSolr();
    this.solrResultElement.setBrand("brand");
    this.solrResultElement.setBuyable(ItemSummaryServiceImplTest.BUYABLE);
    this.solrResultElement.setDiscoverable(ItemSummaryServiceImplTest.DISCOVERABLE);
    this.solrResultElement.setItemCode("item-code");
    this.solrResultElement.setItemImages(ItemSummaryServiceImplTest.ITEM_IMAGES);
    this.solrResultElement.setItemName(ItemSummaryServiceImplTest.ITEM_NAME);
    this.solrResultElement.setItemSku(ItemSummaryServiceImplTest.ITEM_SKU);
    this.solrResultElement.setLatefulfillment(false);
    this.solrResultElement.setMasterCatalog(ItemSummaryServiceImplTest.MASTER_CATALOG);
    this.solrResultElement.setMerchantCode(ItemSummaryServiceImplTest.MERCHANT_CODE);
    this.solrResultElement.setMerchantSku(ItemSummaryServiceImplTest.MERCHANT_SKU);
    this.solrResultElement.setPickupPointCode(ItemSummaryServiceImplTest.PICKUP_POINT_CODE);
    this.solrResultElement.setProductType(ProductType.REGULAR.toString());
    this.solrResultElement.setProductSku(ItemSummaryServiceImplTest.PRODUCT_SKU);
    this.solrResultElement.setProductName(ItemSummaryServiceImplTest.PRODUCT_NAME);
    this.solrResultElement.setSalesCatalog(ItemSummaryServiceImplTest.SALES_CATALOG);
    this.solrResultElement.setOfflinePrices(ItemSummaryServiceImplTest.OFFLINE_PRICES);
    this.solrResultElement.setMerchantPromoDiscount(Boolean.TRUE);
    this.solrResultElement.setMerchantPromoDiscountActivated(Boolean.TRUE);
    this.solrResultElement.setPristineId(PRISTINE_ID);
    this.solrContentResult = new ArrayList<>();
    this.solrContentResult.add(this.solrResultElement);
    when(this.solrResult.getContent()).thenReturn(this.solrContentResult);

    this.prices = new HashSet<>();
    this.prices.add(new Price());
    this.itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    this.itemViewConfigs.add(itemViewConfig);
    this.updateItemSummaryRequestVo =
        new UpdateItemSummaryRequestVo(ItemSummaryServiceImplTest.MERCHANT_SKU,
            ItemSummaryServiceImplTest.PICKUP_POINT_CODE,
            ItemSummaryServiceImplTest.IS_LATE_FULFILLMENT, this.prices, this.itemViewConfigs);
    this.updateItemSummaryRequestVo.setPickupPointCode(PICKUP_POINT_CODE);
    this.productAndItems = new ProductAndItemsVO();
    this.product = new Product();
    this.product.setProductCode(PRODUCT_CODE);
    this.product.setProductType(ProductType.REGULAR);
    this.product.setProductSku(ItemSummaryServiceImplTest.PRODUCT_SKU);
    this.product.setMerchantCode(ItemSummaryServiceImplTest.MERCHANT_CODE);
    this.product.setMasterDataProduct(generateMasterDataProduct());
    this.item = new Item();
    this.item.setSynchronized(false);
    this.item.setItemSku(ItemSummaryServiceImplTest.ITEM_SKU);
    this.item.setProductSku(PRODUCT_SKU);
    this.item.setPrice(this.prices);
    this.item.setItemViewConfigs(this.itemViewConfigs);
    this.item.setPromoBundling(true);
    this.item.setPickupPointCode(ItemSummaryServiceImplTest.PICKUP_POINT_CODE);
    this.item.setMerchantSku(ItemSummaryServiceImplTest.MERCHANT_SKU);
    this.item.setLateFulfillment(ItemSummaryServiceImplTest.IS_LATE_FULFILLMENT);
    this.item.setActivePromoBundlings(new HashSet<>());
    this.item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setUpcCode("ean/upc code");
    MasterDataItemImage itemImage = new MasterDataItemImage();
    itemImage.setLocationPath("/x/y/z.jpg");
    itemImage.setMainImage(true);
    itemImage.setSequence(1);
    masterDataItem.setMasterDataItemImages(Arrays.asList(itemImage));
    this.item.setMasterDataItem(masterDataItem);
    this.item.setFlashSaleActive(true);
    this.item.setWholesalePriceExists(true);
    masterDataItemMap = new HashMap<>();
    masterDataItemMap.put(ITEM_CODE, masterDataItem);
    mapOfPrices = new HashMap<>();
    mapOfPrices.put(ITEM_SKU, item.getPrice());
//    this.item.setForceReview(true);
    this.productAndItems = new ProductAndItemsVO();
    this.productAndItems.setProduct(this.product);
    this.productAndItems.setItems(Arrays.asList(this.item));
    this.productItemsVo = new ProductItemsVo();
    ProductVo productVo = new ProductVo();
    BeanUtils.copyProperties(product, productVo);
    this.productItemsVo.setProductVo(productVo);
    ItemVo itemVo = new ItemVo();
    BeanUtils.copyProperties(item, itemVo);
    this.productItemsVo.setItemVoList(Arrays.asList(itemVo));
    this.offlineItem = new OfflineItem();
    this.offlineItemList = new ArrayList<>();
    this.offlineItemList.add(offlineItem);
    when(
        this.productService
            .getProductAndSingleItemByItemSku(ItemSummaryServiceImplTest.STORE_ID,
                ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
                ItemSummaryServiceImplTest.ITEM_SKU,
                ItemSummaryServiceImplTest.NEED_MASTER_DATA_DETAIL, false, null, false)).thenReturn(
        this.productItemsVo);

    setField(this.itemSummaryFilterServiceImpl, "solrStringDelimiter", ItemSummaryServiceImplTest.DELIMITER);

    OfflineItemPriceVO offlineItem1 = new OfflineItemPriceVO();
    offlineItem1.setPickupPointCode(PICKUP_POINT_CODE);
    offlineItem1.setListPrice(15000D);
    offlineItem1.setOfferPrice(10000D);
    OfflineItemPriceVO offlineItem2 = new OfflineItemPriceVO();
    offlineItem2.setPickupPointCode("pp-02");
    offlineItem2.setListPrice(25000D);
    offlineItem2.setOfferPrice(20000D);

    offlineItems = new ArrayList<>();
    offlineItems.add(offlineItem1);
    offlineItems.add(offlineItem2);

    when(this.itemHelperService.convertOfflineItemPrices(anyString(), any(ProductAndItemSolr.class))).thenReturn(
        offlineItems);

    this.bulkItemSummaryRequestVo = new BulkItemSummaryRequestVo();
    this.bulkItemSummaryRequestVo.setItemSkus(itemSkus);

    enableDeferredSolrReindexParameter = new SystemParameter();
    enableDeferredSolrReindexParameter.setVariable(SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    enableDeferredSolrReindexParameter.setValue(String.valueOf(false));

    singleItemSummarySolrSwitch = new SystemParameter();
    singleItemSummarySolrSwitch.setVariable(SystemParameterNames.SINGLE_ITEM_SUMMARY_SOLR_SWITCH);
    singleItemSummarySolrSwitch.setValue(String.valueOf(false));

    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(enableDeferredSolrReindexParameter);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(),
        Mockito.any())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(itemService.getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU)))
        .thenReturn(Collections.singletonList(item));

    this.itemSummaryRequestVo1 = new ItemSummaryRequestVO();
    itemSummaryRequestVo1.setArchived(false);
    itemSummaryRequestVo1.setSearchKey(PRODUCT_NAME);
    itemSummaryRequestVo1.setMerchantCode(MERCHANT_CODE);

    Mockito.doNothing().when(productAndItemSolrIndexerService).updateWholesalePriceActivatedFlag(ITEM_SKU, false);

    when(itemService.getItemPriceAndViewConfigsAndPromoDetails(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU)))
        .thenReturn(Collections.singletonList(item));

    itemsSummaryDetailRequestVo = new ItemsSummaryDetailRequestVo();
    itemsSummaryDetailRequestVo.setItemSku(ITEM_SKU);
    itemsSummaryDetailRequestVo.setProductSku(PRODUCT_SKU);

    itemCodeAndSkuMap.put(ITEM_CODE, ITEM_SKU);

    itemPickupPoint = ItemPickupPoint.builder().itemSku(solrResultElement.getItemSku())
        .pickupPointCode(solrResultElement.getPickupPointCode()).price(prices).itemViewConfig(itemViewConfigs).build();

    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemViewConfigList = new HashSet<>(Arrays.asList(
        new ItemViewConfig(true, false, "DEFAULT", new ItemDiscoverableSchedule(), new ItemBuyableSchedule())));
    prices1 = new HashSet<>(Arrays.asList(new Price("$", 10, 10, "", "", new Date())));
    itemPickupPoint.setItemViewConfig(itemViewConfigList);
    itemPickupPoint.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    itemPickupPoint.setPromoBundling(true);
    itemPickupPoint.setPrice(prices1);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setOfflineItemId(ITEM_SKU+PICKUP_POINT_CODE);
    solrResultElement.setPickupPointCode(PICKUP_POINT_CODE);
    when(itemPickupPointService.findByItemSkuInAndDelivery(anyString(), anyList(), anyBoolean())).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkuInAndDelivery(anyString(), anyList(), anyBoolean()))
        .thenReturn(Arrays.asList(itemPickupPoint));
    itemPickupPointList.add(itemPickupPoint);
    when(itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(),Mockito.anyString())).thenReturn(itemPickupPoint);
    when(saveOperationService.saveProductAndItems(Mockito.any(ProductAndItemsVO.class), Mockito.any())).thenReturn(productAndItems);

    fetchPCBMasterDataParameter = new SystemParameter();
    fetchPCBMasterDataParameter.setValue("true");
    when(channelService.getDefaultChannel()).thenReturn(DEFAULT_CHANNEL);

    bundleRecipe = new BundleRecipe();
    bundleRecipe.setItemSku(ITEM_SKU);
    bundleRecipe.setQuantity(QUANTITY);

    bundleRecipe2 = new BundleRecipe();
    bundleRecipe2.setItemSku(ITEM_SKU_1);
    bundleRecipe2.setQuantity(QUANTITY);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.itemHelperService);
    verifyNoMoreInteractions(this.itemPriceService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.solrDataConstructor);
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.itemViewConfigService);
    verifyNoMoreInteractions(this.productHelperService);
    verifyNoMoreInteractions(this.systemParameterService);
    verifyNoMoreInteractions(this.saveAndPublishService);
    verifyNoMoreInteractions(this.businessPartnerPickupPointService);
    verifyNoMoreInteractions(this.productCategoryBaseOutbound);
    verifyNoMoreInteractions(this.productCacheableService, this.itemRepositoryCustom);
    verifyNoMoreInteractions(this.gdnMapper);
  }

  @Test
  public void updateItemSummarySuccess() throws Exception {
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    item.setOff2OnChannelActive(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID,
      SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX, "true", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveItemsWithDeferredReindexing(anyList(), anyList()))
        .thenReturn(Arrays.asList(item));
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)),
        null, null, PageRequest.of(0, 1))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(this.saveOperationService).saveItemsWithDeferredReindexing(Arrays.asList(item),
        new ArrayList<>(Arrays.asList(itemPickupPoint)));
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    assertFalse(item.isContentChanged());
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummaryIsBulkTrueSuccess() throws Exception {
    enableDeferredSolrReindexParameter.setValue(String.valueOf(true));
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    item.setOff2OnChannelActive(true);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)),
        null, null, PageRequest.of(0, 1))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(this.saveOperationService).saveItemsWithDeferredReindexing(Arrays.asList(item),
        new ArrayList<>(Arrays.asList(itemPickupPoint)));
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(),
      Mockito.anyList());
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummaryNoChangeTest() throws Exception {
    item.setActivePromoBundlings(null);
    enableDeferredSolrReindexParameter.setValue(null);
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    item.setOff2OnChannelActive(true);
    when(this.itemService
      .findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService
      .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
      updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(
      itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME))
      .thenReturn(false);
    when(this.productAndItemSolrRepository
      .getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID,
        new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)), null, null, PageRequest.of(0, 1)))
      .thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true))
      .thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList())))
      .thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl
      .updateItemSummary(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.REQUEST_ID,
        ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.ITEM_SKU,
        ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(this.productService)
      .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService)
      .findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
      updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
      updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService)
      .validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID,
      Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(this.solrDataConstructor)
      .constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor)
      .constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor)
      .constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(itemPickupPointService)
      .findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummaryPickupPointChangeTest() throws Exception {
    item.setPickupPointCode("pick-up-point-2");
    item.setActivePromoBundlings(null);
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    item.setOff2OnChannelActive(true);
    Item item2 = new Item();
    BeanUtils.copyProperties(item, item2);
    item2.setPickupPointCode("pick-up-point-2");
    when(itemService.getItemPriceAndViewConfigs(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(item2));
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(
        false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID,
        new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)), null, null, PageRequest.of(0, 1))).thenReturn(
        this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(
        new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    when(itemService.updatePickupPointInItemPickupPoints(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
        Mockito.any(Item.class), eq(new HashMap<>()), eq(false), Mockito.any(EditProductDetailDTO.class), eq(false))).thenReturn(
        getItemPickupPointMap());
    when(itemPickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        new ArrayList<>(getItemPickupPointMap().values()));

    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.ITEM_SKU,
        ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID,
        Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(itemHelperService).convertOfflineItemPrices(isNull(), isNull(),
        Mockito.any(ProductAndItemSolr.class));
    verify(itemService).updatePickupPointInItemPickupPoints(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
        Mockito.any(Item.class), Mockito.anyMap(), eq(false), isNull(), eq(false));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), Mockito.any());
    verify(solrDataConstructor).constructMasterDataItemMainImages(Mockito.anyList());
    verify(solrDataConstructor).constructMasterCatalog(Mockito.anyString());
    verify(solrDataConstructor).constructSalesCatalogs(Mockito.anyList());
  }


  @Test
  public void updateItemSummaryPickupPointChangeExceptionTest() throws Exception {
    item.setPickupPointCode("pick-up-point-2");
    item.setActivePromoBundlings(null);
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    item.setOff2OnChannelActive(true);
    itemPickupPoint.setOfflineItemId(itemPickupPoint.getItemSku() + itemPickupPoint.getPickupPointCode());
    Item item2 = new Item();
    BeanUtils.copyProperties(item, item2);
    item2.setPickupPointCode("pick-up-point-2");
    when(itemService.getItemPriceAndViewConfigs(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(item2));
    when(this.itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(new ArrayList<>(Arrays.asList(itemPickupPoint)));
    when(this.itemService
      .findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService
      .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
      updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(
      itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME))
      .thenReturn(false);
    when(this.productAndItemSolrRepository
      .getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID,
        new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)), null, null, PageRequest.of(0, 1)))
      .thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true))
      .thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList())))
      .thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    try {
      this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false);
    } finally {
      verify(itemPickupPointService).findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString());
      verify(saveOperationService).saveProductAndItems(Mockito.any(ProductAndItemsVO.class), Mockito.any());
      verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
      verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
      verify(this.productService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
          ItemSummaryServiceImplTest.PRODUCT_SKU);
      verify(this.itemService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
          ItemSummaryServiceImplTest.ITEM_SKU);
      verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
      verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
      verify(itemService)
        .validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
      verify(itemService).getItemPriceAndViewConfigs(STORE_ID,
        Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
      verify(itemPickupPointService)
        .findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
      verify(itemPriceService)
        .getDiscountItemPickupPoint((Mockito.anyList()));
      verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
      verify(itemHelperService).convertOfflineItemPrices(isNull(), isNull(),
          Mockito.any(ProductAndItemSolr.class));
      verify(itemService).updatePickupPointInItemPickupPoints(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
          Mockito.any(Item.class), Mockito.anyMap(), eq(false), isNull(), eq(false));
      verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
      verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
          Mockito.any(Item.class), isNull());
      verify(solrDataConstructor).constructMasterDataItemMainImages(Mockito.anyList());
      verify(solrDataConstructor).constructMasterCatalog(Mockito.anyString());
      verify(solrDataConstructor).constructSalesCatalogs(Mockito.anyList());
    }
  }

  @Test
  public void updateItemSummaryPickupPointChangeDeferredTest() throws Exception {
    item.setPickupPointCode("pick-up-point-2");
    item.setActivePromoBundlings(null);
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    solrContentResult.get(0).setPickupPointCode("pick-up-point-2");
    item.setOff2OnChannelActive(true);
    Item item2 = new Item();
    BeanUtils.copyProperties(item, item2);
    item2.setPickupPointCode("pick-up-point-2");
    when(itemService.getItemPriceAndViewConfigs(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(item2));
    itemPickupPoint.setOfflineItemId(itemPickupPoint.getItemSku() + itemPickupPoint.getPickupPointCode());
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "true", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(this.itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(new ArrayList<>(Arrays.asList(itemPickupPoint)));
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(
        false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(eq(ItemSummaryServiceImplTest.STORE_ID),
        Mockito.any(), eq(null), eq(null), eq(PageRequest.of(0, 1)))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(
        new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    when(saveOperationService.saveProductAndItemsWithDeferredReindex(Mockito.any(ProductAndItemsVO.class))).thenReturn(productAndItems);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.ITEM_SKU,
        ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(itemPickupPointService).findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(saveOperationService).saveProductAndItemsWithDeferredReindex(Mockito.any(ProductAndItemsVO.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID,
        Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemService).updatePickupPointInItemPickupPoints(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
        Mockito.any(Item.class), Mockito.anyMap(), eq(false), isNull(), eq(false));
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
  }

  private Map<String, ItemPickupPoint> getItemPickupPointMap() {
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setDelivery(false);

    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setMarkForDelete(true);

    itemPickupPoint.setDelivery(true);
    itemPickupPoint.setMarkForDelete(false);

    return ImmutableMap.of("1", itemPickupPoint1, "2", itemPickupPoint2, "3", itemPickupPoint);
  }

  @Test
  public void updateItemSummaryPickupPointChangeAndO2oChangeTest() throws Exception {
    updateItemSummaryRequestVo.setOff2OnChannelActive(false);
    itemPickupPoint.setPickupPointCode("pick-up-point-2");
    itemPickupPoint.setOfflineItemId(itemPickupPoint.getItemSku() + itemPickupPoint.getPickupPointCode());
    item.setOff2OnChannelActive(true);
    item.setPickupPointCode("pick-up-point-2");
    item.setActivePromoBundlings(null);
    Item item2 = new Item();
    BeanUtils.copyProperties(item, item2);
    item2.setPickupPointCode("pick-up-point-2");
    when(itemService.getItemPriceAndViewConfigs(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(item2));
    solrContentResult.get(0).setPickupPointCode("pick-up-point-2");
    when(this.itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(new ArrayList<>(Arrays.asList(itemPickupPoint)));
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(
        false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID,
        new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)), null, null, PageRequest.of(0, 1))).thenReturn(
        this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(
        new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.ITEM_SKU,
        ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(itemPickupPointService).findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(saveOperationService).saveProductAndItems(Mockito.any(ProductAndItemsVO.class), Mockito.any());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID,
        Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(itemService).updatePickupPointInItemPickupPoints(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
        Mockito.any(Item.class), Mockito.anyMap(), eq(false), isNull(), eq(false));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
  }

  @Test
  public void updateItemSummaryPickupPointChangeAndO2oChangeDeferredTest() throws Exception {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    itemPickupPoint.setPickupPointCode("pick-up-point-2");
    updateItemSummaryRequestVo.setOff2OnChannelActive(false);
    item.setOff2OnChannelActive(true);
    item.setPickupPointCode("pick-up-point-2");
    item.setActivePromoBundlings(null);
    solrContentResult.get(0).setPickupPointCode("pick-up-point-2");
    Item item2 = new Item();
    BeanUtils.copyProperties(item, item2);
    item2.setPickupPointCode("pick-up-point-2");
    when(itemService.getItemPriceAndViewConfigs(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(item2));
    itemPickupPoint.setOfflineItemId(itemPickupPoint.getItemSku() + itemPickupPoint.getPickupPointCode());
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "true", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(this.itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(new ArrayList<>(Arrays.asList(itemPickupPoint)));
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(
        false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID,
        new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)), null, null, PageRequest.of(0, 1))).thenReturn(
        this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(
        new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    when(saveOperationService.saveProductAndItemsWithDeferredReindex(Mockito.any(ProductAndItemsVO.class))).thenReturn(productAndItems);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.ITEM_SKU,
        ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(itemPickupPointService).findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(itemService).updatePickupPointInItemPickupPoints(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
        Mockito.any(Item.class), Mockito.anyMap(), eq(false), isNull(), eq(false));
    verify(saveOperationService).saveProductAndItemsWithDeferredReindex(Mockito.any(ProductAndItemsVO.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID,
        Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
  }

  @Test
  public void updateItemSummaryPickupPointChangeAndO2oChangeDeferredNotEmptyPPcodeTest() throws Exception {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    itemPickupPoint.setPickupPointCode("pick-up-point-2");
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setOfflineItemId("itemPickupPoint1");
    updateItemSummaryRequestVo.setOff2OnChannelActive(false);
    item.setOff2OnChannelActive(true);
    item.setPickupPointCode("pick-up-point-2");
    item.setActivePromoBundlings(null);
    solrContentResult.get(0).setPickupPointCode("pick-up-point-2");
    itemPickupPoint.setOfflineItemId(itemPickupPoint.getItemSku() + itemPickupPoint.getPickupPointCode());
    Item item2 = new Item();
    BeanUtils.copyProperties(item, item2);
    item2.setPickupPointCode("pick-up-point-2");
    when(itemService.getItemPriceAndViewConfigs(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(item2));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "true", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(this.itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(new ArrayList<>(Arrays.asList(itemPickupPoint1, itemPickupPoint)));
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(
        false);
    when(itemService.updatePickupPointInItemPickupPoints(anyString(), any(ItemPickupPoint.class), any(Item.class),
        eq(new HashMap<>()), eq(false), any(EditProductDetailDTO.class), eq(false))).thenReturn(getItemPickupPointMap());
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID,
        new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)), null, null, PageRequest.of(0, 1))).thenReturn(
        this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(
        new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    when(saveOperationService.saveProductAndItemsWithDeferredReindex(Mockito.any(ProductAndItemsVO.class))).thenReturn(productAndItems);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.ITEM_SKU,
        ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(itemPickupPointService).findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(saveOperationService).saveProductAndItemsWithDeferredReindex(Mockito.any(ProductAndItemsVO.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID,
        Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(itemService).updatePickupPointInItemPickupPoints(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
        Mockito.any(Item.class), Mockito.anyMap(), eq(false), isNull(), eq(false));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
  }

  @Test
  public void updateItemSummary_WithOff2OnFlagActive() throws Exception {
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    item.setOff2OnChannelActive(false);
    when(this.itemPickupPointService
      .findByItemSkuAndDelivery(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(itemPickupPoint);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(false);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)),
        null, null, PageRequest.of(0, 1))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(saveOperationService.updateAndEvictOff2OnItemCountByProductSku(STORE_ID, PRODUCT_SKU, true, 1))
        .thenReturn(product);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(this.saveOperationService).saveProductAndItems(Mockito.any(ProductAndItemsVO.class), Mockito.any());
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(),
      Mockito.anyList());
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummary_WithLateFullfillment() throws Exception {
    updateItemSummaryRequestVo.setIsLateFulfillment(true);
    item.setLateFulfillment(false);
    item.setLateFullfillmentUpdatedBy(USERNAME);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(false);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)),
        null, null, PageRequest.of(0, 1))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(saveOperationService.updateAndEvictOff2OnItemCountByProductSku(STORE_ID, PRODUCT_SKU, true, 1))
        .thenReturn(product);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    ItemSummaryResponseVO response = this.itemSummaryFilterServiceImpl
        .updateItemSummary(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.REQUEST_ID,
            ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.ITEM_SKU,
            ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(this.saveOperationService).saveItems(Arrays.asList(item), new ArrayList<>());
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummary_WithOff2OnFlagInActive() throws Exception {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    updateItemSummaryRequestVo.setOff2OnChannelActive(false);
    item.setOff2OnChannelActive(true);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(false);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)),
        null, null, PageRequest.of(0, 1))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(saveOperationService.updateAndEvictOff2OnItemCountByProductSku(STORE_ID, PRODUCT_SKU, false, 1))
        .thenReturn(product);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(this.saveOperationService).saveProductAndItems(Mockito.any(ProductAndItemsVO.class), Mockito.any());
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummary_ItemChangeInactiveTest() throws Exception {
    item.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.PROMO_SCHEDULER)));
    itemPickupPoint.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.PROMO_SCHEDULER)));
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
      ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
      ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(false);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
      updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
      ItemSummaryServiceImplTest.STORE_ID, new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)),
      null, null, PageRequest.of(0, 1))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(saveOperationService.updateAndEvictOff2OnItemCountByProductSku(STORE_ID, PRODUCT_SKU, false, 1))
      .thenReturn(product);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
      ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
      ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
      ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
      ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
      updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateArchivedExceptionTest() throws Exception {
    item.setArchived(true);
    item.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.PROMO_SCHEDULER)));
    itemPickupPoint
      .setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.PROMO_SCHEDULER)));
    when(this.itemService
      .findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService
      .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
      updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(false);
    when(this.productAndItemSolrRepository
      .getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID,
        new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)), null, null, PageRequest.of(0, 1)))
      .thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(saveOperationService
      .updateAndEvictOff2OnItemCountByProductSku(STORE_ID, PRODUCT_SKU, false, 1))
      .thenReturn(product);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true))
      .thenReturn(Arrays.asList(itemPickupPoint));
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false));
    } finally {
      verify(this.objectConverterService)
        .overrideL4DetailsFromL5(Arrays.asList(item), Arrays.asList(itemPickupPoint));
      verify(this.productService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
          ItemSummaryServiceImplTest.PRODUCT_SKU);
      verify(this.itemService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemSummaryServiceImplTest.STORE_ID,
          ItemSummaryServiceImplTest.ITEM_SKU);
    }
  }

  @Test
  public void updateItemSummarySuccessWithUnexpectedMerchantCode() throws Exception {
    boolean exceptionThrown = false;
    when(
        this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
            ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(
        new Item(ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.PRODUCT_SKU));
    when(
        this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
            ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU))
        .thenReturn(new Product());
    try {
      this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
          ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
          ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.RANDOM,
          this.updateItemSummaryRequestVo, false);
    } catch (Exception e) {
      exceptionThrown = true;
    }
    assertTrue(exceptionThrown);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(),
      Mockito.anyList());
  }

  @Test
  public void updateItemSummaryUpdateBlankItemSku() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME, "",
        ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false));
  }

  @Test
  public void updateItemSummaryUpdateBlankMerchantCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, "", this.updateItemSummaryRequestVo, false));
  }

  @Disabled
  @Test
  public void updateItemSummaryUpdateItemOnly() throws Exception {
    this.updateItemSummaryRequestVo = new UpdateItemSummaryRequestVo();
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(null, Arrays.asList(this.item));
    this.updateItemSummaryRequestVo
        .setPickupPointCode(ItemSummaryServiceImplTest.PICKUP_POINT_CODE);
    when(this.saveOperationService.saveProductAndItems(productAndItems, new ArrayList<>())).thenReturn(
        new ProductAndItemsVO());
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false);
    verify(this.productService).getProductAndSingleItemByItemSku(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.REQUEST_ID,
        ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.ITEM_SKU,
        ItemSummaryServiceImplTest.NEED_MASTER_DATA_DETAIL, false, null, false);
    verify(this.saveOperationService).saveProductAndItems(productAndItems, new ArrayList<>());
  }

  @Test
  public void updateItemSummaryUpdateNullUpdateRequest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, null, false));
  }

  @Test
  public void updateItemSummaryUpdateProductOnly() throws Exception {
    this.updateItemSummaryRequestVo = new UpdateItemSummaryRequestVo();
    when(this.saveOperationService.saveProduct(this.productAndItems.getProduct())).thenReturn(
        this.productAndItems.getProduct());
    ArrayList<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<>();
    itemSummaryResponses.add(new ItemSummaryResponseVO());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, null, null ,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(solrResult);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(
        item);
    when(
        productService
            .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void getCamapignItemSummaryByFilterTest() throws ApplicationException {
    when(
        this.productAndItemSolrRepository
            .getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalse(
                ItemSummaryServiceImplTest.STORE_ID,
                this.campaignItemSummaryRequestVO.getMerchantCode(),
                this.campaignItemSummaryRequestVO.getCategories(),
                this.campaignItemSummaryRequestVO.getBrands(),
                campaignItemSummaryRequestVO.getKeyword(),
                campaignItemSummaryRequestVO.getItemSku(), ItemSummaryServiceImplTest.PAGE_REQUEST))
        .thenReturn(this.solrResult);
    try {
      this.itemSummaryFilterServiceImpl.getCampaignItemSummaryByFilter(
          ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
          ItemSummaryServiceImplTest.REQUEST_ID, this.campaignItemSummaryRequestVO,
          ItemSummaryServiceImplTest.PAGE_REQUEST);
    } catch (ApplicationException e) {
      assertEquals(ErrorCategory.DATA_NOT_FOUND.getMessage()
          + ItemSummaryServiceImplTest.ITEMS_NOT_FOUND, e.getMessage());
      assertEquals(ErrorCategory.DATA_NOT_FOUND.getCode(), e.getErrorCodes().getCode());
    }
    verifyNoMoreInteractions(this.itemPriceService);
  }

  @Test
  public void getCamapignItemSummaryByFilterTestForSuccess() throws ApplicationException {
    this.solrResult = new PageImpl<ProductAndItemSolr>(this.solrContentResult);
    when(
        this.productAndItemSolrRepository
            .getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalse(
                Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.anyList(),
                Mockito.anyString(), Mockito.isNull(), Mockito.any())).thenReturn(
        this.solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyList())).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.getCampaignItemSummaryByFilter(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.REQUEST_ID, this.campaignItemSummaryRequestVO,
        ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository)
        .getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalse(
            Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.anyList(),
            Mockito.anyString(), Mockito.isNull(), Mockito.any());
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyList());
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
  }

  @Test
  public void updateItemSummaryRemoveOfflineItem() throws Exception {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.PROMO_SCHEDULER)));
    this.updateItemSummaryRequestVo = new UpdateItemSummaryRequestVo();
    when(this.saveOperationService.saveProduct(this.productAndItems.getProduct())).thenReturn(
        this.productAndItems.getProduct());
    item.setSynchronized(true);
    ArrayList<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<>();
    itemSummaryResponses.add(new ItemSummaryResponseVO());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, null, null ,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(solrResult);
    item.setMerchantSku(NEW_MERCHANT_SKU);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(
        item);
    when(
        productService
            .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(this.offlineItemService.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenReturn(offlineItemList);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
  }

  @Test
  public void updateItemSummaryRemoveOfflineItem_updateMerchantSku() throws Exception {
    this.updateItemSummaryRequestVo = new UpdateItemSummaryRequestVo();
    updateItemSummaryRequestVo.setMerchantSku(MERCHANT_SKU);

    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(MERCHANT_SKU);

    List<Item> items = new ArrayList<>();
    items.add(item);

    when(this.itemService
        .getItemsByMerchantCodeAndMerchantSkus(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
            merchantSkus)).thenReturn(items);

    when(this.saveOperationService.saveProduct(this.productAndItems.getProduct())).thenReturn(
        this.productAndItems.getProduct());
    ArrayList<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<>();
    itemSummaryResponses.add(new ItemSummaryResponseVO());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, null, null ,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(solrResult);
    item.setMerchantSku(NEW_MERCHANT_SKU);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(
        item);
    when(
        productService
            .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(this.offlineItemService.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenReturn(offlineItemList);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void testUpdateItemSummary_InconsistentValueBetweenDBAndSolr_offlineItemShouldNotBeDeleted()
      throws Exception {
    this.updateItemSummaryRequestVo = new UpdateItemSummaryRequestVo();
    updateItemSummaryRequestVo.setMerchantSku(StringUtils.EMPTY);

    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(StringUtils.EMPTY);

    List<Item> items = new ArrayList<>();
    items.add(item);

    when(this.itemService.getItemsByMerchantCodeAndMerchantSkus(STORE_ID, REQUEST_ID, USERNAME,
        MERCHANT_CODE, merchantSkus)).thenReturn(items);

    when(this.saveOperationService.saveProduct(this.productAndItems.getProduct()))
        .thenReturn(this.productAndItems.getProduct());

    this.solrResultElement.setMerchantSku(null);
    when(this.solrResult.getContent()).thenReturn(this.solrContentResult);

    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, null, null,
        ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(solrResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    item.setMerchantSku(StringUtils.EMPTY);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(product);

    itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID,
        Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor)
        .constructMasterCatalog(ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor)
        .constructMasterDataItemMainImages(ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor)
        .constructSalesCatalogs(ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(),
        any(ProductAndItemSolr.class));
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummaryRemoveOfflineItem_whenPriceChange() throws Exception {
    this.updateItemSummaryRequestVo = new UpdateItemSummaryRequestVo();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(900);
    price.setChannel("default-channel");
    itemPrices.add(price);
    Item item1 = new Item();
    item1.setPrice(itemPrices);
    updateItemSummaryRequestVo.setPrice(itemPrices);
    updateItemSummaryRequestVo.setMerchantSku(MERCHANT_SKU);

    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(MERCHANT_SKU);

    List<Item> items = new ArrayList<>();
    items.add(item);

    when(this.itemService
        .getItemsByMerchantCodeAndMerchantSkus(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
            merchantSkus)).thenReturn(items);

    when(this.saveOperationService.saveProduct(this.productAndItems.getProduct())).thenReturn(
        this.productAndItems.getProduct());
    when(itemService.validatePriceChangeAndSet(productAndItems.getItems().get(0), itemPrices, USERNAME))
        .thenReturn(true);
    when(this.itemHelperService
        .setItemPriceByChannel(productAndItems.getItems().get(0), itemPrices, USERNAME))
        .thenReturn(item1);

    ArrayList<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<>();
    itemSummaryResponses.add(new ItemSummaryResponseVO());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, null, null ,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(solrResult);
    item.setMerchantSku(NEW_MERCHANT_SKU);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(
        item);
    when(
        productService
            .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(this.offlineItemService.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenReturn(offlineItemList);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);

    itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    Mockito.verify(this.itemPriceService)
        .publishItemPriceChangeEvent(USERNAME, REQUEST_ID, this.productAndItems.getProduct(),
            this.productAndItems.getItems().get(0));
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    Assertions.assertEquals(
        ItemChangeEventType.ITEM_DATA_CHANGE, this.productAndItems.getItems().get(0).getItemChangeEventTypes().get(0));
    Assertions.assertEquals(
        ItemChangeEventType.ITEM_PRICE_CHANGE, this.productAndItems.getItems().get(0).getItemChangeEventTypes().get(1));
    verify(itemService).validatePriceChangeAndSet(productAndItems.getItems().get(0), itemPrices, USERNAME);
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummaryRemoveOfflineItem_whenPriceNotChange() throws Exception {
    this.updateItemSummaryRequestVo = new UpdateItemSummaryRequestVo();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(900);
    price.setChannel("default-channel");
    itemPrices.add(price);
    Item item1 = new Item();
    item1.setPrice(itemPrices);
    updateItemSummaryRequestVo.setPrice(itemPrices);
    updateItemSummaryRequestVo.setMerchantSku(MERCHANT_SKU);

    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(MERCHANT_SKU);

    List<Item> items = new ArrayList<>();
    items.add(item);

    when(this.itemService
        .getItemsByMerchantCodeAndMerchantSkus(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
            merchantSkus)).thenReturn(items);

    when(this.saveOperationService.saveProduct(this.productAndItems.getProduct())).thenReturn(
        this.productAndItems.getProduct());
    when(itemService.validatePriceChangeAndSet(productAndItems.getItems().get(0), itemPrices, USERNAME))
        .thenReturn(false);
    when(this.itemHelperService
        .setItemPriceByChannel(productAndItems.getItems().get(0), itemPrices, USERNAME))
        .thenReturn(item1);

    ArrayList<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<>();
    itemSummaryResponses.add(new ItemSummaryResponseVO());
    when(
        this.productAndItemSolrRepository.getProductAndItemsByFilter(
            ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, null, null ,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(solrResult);
    item.setMerchantSku(NEW_MERCHANT_SKU);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(
        item);
    when(
        productService
            .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(this.offlineItemService.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenReturn(offlineItemList);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);

    itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
        ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
        ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
        ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemService).validatePriceChangeAndSet(productAndItems.getItems().get(0), itemPrices, USERNAME);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(),
      Mockito.anyList());
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummaryTestIsSyncronizedFalse() throws Exception {
    this.item.setSynchronized(true);
    this.product.setMasterDataProduct(generateMasterDataProduct());

  }

  @Test
  public void updateItemSummaryTestIsSyncronizedFalseAndRegularAndWeightMoreThanMaxWeight()
      throws Exception {
    this.updateItemSummaryRequestVo = new UpdateItemSummaryRequestVo();
    when(this.saveOperationService.saveProduct(this.productAndItems.getProduct()))
        .thenReturn(this.productAndItems.getProduct());
    ArrayList<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<>();
    itemSummaryResponses.add(new ItemSummaryResponseVO());
    when(this.productAndItemSolrRepository
        .getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY ,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(solrResult);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    when(productService
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    product.getMasterDataProduct().setShippingWeight(51);
    try {
      itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
          ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
          ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE,
          this.updateItemSummaryRequestVo, false);
    } catch (ApplicationRuntimeException e) {
      verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
      verify(productService)
          .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
      verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
        Arrays.asList(itemPickupPoint));
    }
  }

  @Test
  public void updateItemSummaryTestIsSyncronizedFalseAndBigProductAndWeightMoreThanMaxWeight()
      throws Exception {
    product.setProductType(ProductType.BIG_PRODUCT);
    this.updateItemSummaryRequestVo = new UpdateItemSummaryRequestVo();
    when(this.saveOperationService.saveProduct(this.productAndItems.getProduct()))
        .thenReturn(this.productAndItems.getProduct());
    ArrayList<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<>();
    itemSummaryResponses.add(new ItemSummaryResponseVO());
    when(this.productAndItemSolrRepository
        .getProductAndItemsByFilter(ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, null, null ,
            ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(solrResult);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    when(productService
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE,
        this.updateItemSummaryRequestVo, false);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(productService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor)
        .constructMasterCatalog(ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor)
        .constructMasterDataItemMainImages(ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor)
        .constructSalesCatalogs(ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(),
      Mockito.anyList());
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  private MasterDataProduct generateMasterDataProduct() {
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setShippingWeight(34);
    return masterDataProduct;
  }

  @Test
  public void updateItemSummary_forceReviewTrue() throws Exception {
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    item.setForceReview(true);
    item.setOff2OnChannelActive(true);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl
          .updateItemSummary(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.REQUEST_ID,
              ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false));
    } finally {
      verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    }
  }

  @Test
  public void updateItemSummary_trueWholesaleFlag() throws Exception {
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    item.setOff2OnChannelActive(true);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)),
        null, null, PageRequest.of(0, 1))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, true);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(this.saveOperationService).saveItems(Arrays.asList(item), new ArrayList<>());
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummary_falseWholesaleFlag() throws Exception {
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    item.setOff2OnChannelActive(true);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)),
        null, null, PageRequest.of(0, 1))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(this.saveOperationService).saveItems(Arrays.asList(item), new ArrayList<>());
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummary_nullWholesaleFlag() throws Exception {
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    item.setOff2OnChannelActive(true);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)),
        null, null, PageRequest.of(0, 1))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, null);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(this.saveOperationService).saveItems(Arrays.asList(item), new ArrayList<>());
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }


  @Test
  public void updateItemSummary_nullActivePromoSet() throws Exception {
    updateItemSummaryRequestVo.setOff2OnChannelActive(true);
    item.setOff2OnChannelActive(true);
    item.setActivePromoBundlings(null);
    itemPickupPoint.setActivePromoBundlings(null);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)),
        null, null, PageRequest.of(0, 1))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, true);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(this.saveOperationService).saveItems(Arrays.asList(item), new ArrayList<>());
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(),
      Mockito.anyList());
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void updateItemSummary_emptyActivePromoBundling() throws Exception {
    updateItemSummaryRequestVo.setOff2OnChannelActive(false);
    item.setOff2OnChannelActive(true);
    item.setActivePromoBundlings(null);
    when(this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU)).thenReturn(item);
    when(this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU)).thenReturn(product);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs())).thenReturn(true);
    when(itemService.validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME)).thenReturn(false);
    when(this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, new ItemSummaryRequestVO(Arrays.asList(ITEM_SKU)),
        null, null, PageRequest.of(0, 1))).thenReturn(this.solrResult);
    when(solrResult.getContent()).thenReturn(solrContentResult);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    this.itemSummaryFilterServiceImpl.updateItemSummary(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.REQUEST_ID, ItemSummaryServiceImplTest.USERNAME,
        ItemSummaryServiceImplTest.ITEM_SKU, ItemSummaryServiceImplTest.MERCHANT_CODE, this.updateItemSummaryRequestVo, false);
    verify(this.productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.PRODUCT_SKU);
    verify(this.itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.ITEM_SKU);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(item,
        updateItemSummaryRequestVo.getItemViewConfigs());
    verify(itemService).validatePriceChangeAndSet(item, updateItemSummaryRequestVo.getPrice(), USERNAME);
    verify(this.saveOperationService).saveProductAndItems(Mockito.any(ProductAndItemsVO.class), Mockito.any());
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(itemHelperService).convertOfflineItemPrices(null, null, solrResult.getContent().get(0));
    verify(this.solrDataConstructor).constructMasterCatalog(solrResult.getContent().get(0).getMasterCatalog());
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(solrResult.getContent().get(0).getItemImages());
    verify(this.solrDataConstructor).constructSalesCatalogs(solrResult.getContent().get(0).getSalesCatalog());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    verify(itemService).updateItemPickupPointIfMerchantSkuChanged(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(Item.class), isNull());
  }

  @Test
  public void getItemsSummaryDetailByFilterMppTrueTest2() throws Exception {
    Page<ItemPickupPoint> itemPickupPoints = new PageImpl<>(itemPickupPointList);
    HashMap<String, Set<Price>> offlineItemIdToPriceMap = new HashMap<>();
    offlineItemIdToPriceMap.put(itemPickupPointList.get(0).getOfflineItemId(), itemPickupPointList.get(0).getPrice());
    item.setWholesalePriceActivated(true);
    item.setItemCode(ITEM_CODE);
    item.setMasterDataItem(null);
    Set<String> itemCodeList = Arrays.asList(item).stream().map(Item::getItemCode).collect(Collectors.toSet());
    when(this.masterDataService
        .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
            ItemSummaryServiceImplTest.REQUEST_ID, itemCodeList)).thenReturn(masterDataItemMap);
    when(itemService.findByStoreIdAndItemSkusNotCached(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.getProductDeletedOrUndeleted(STORE_ID, itemPickupPointList.get(0).getProductSku())).thenReturn(product);
    when(productService.getProductL3CountByProductCode(PRODUCT_CODE)).thenReturn((long) 2);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoints(Mockito.anyList())).thenReturn(offlineItemIdToPriceMap);
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    when(itemPickupPointService.findItemPickupPointByProductSkuOrItemSku(STORE_ID, itemsSummaryDetailRequestVo.getProductSku(),
        itemsSummaryDetailRequestVo.getItemSku(), PAGE.getPageNumber(), PAGE.getPageSize())).thenReturn(itemPickupPoints);
    this.itemSummaryFilterServiceImpl
        .getItemsSummaryDetailByFilter(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
            ItemSummaryServiceImplTest.REQUEST_ID, this.itemsSummaryDetailRequestVo, PAGE.getPageNumber(),
            PAGE.getPageSize());
    verify(productService).getProductDeletedOrUndeleted(STORE_ID, itemPickupPointList.get(0).getProductSku());
    verify(itemPriceService).getDiscountItemPickupPoints(Mockito.anyList());
    verify(itemPickupPointService).findItemPickupPointByProductSkuOrItemSku(STORE_ID, itemsSummaryDetailRequestVo.getProductSku(),
        itemsSummaryDetailRequestVo.getItemSku(), PAGE.getPageNumber(), PAGE.getPageSize());
    verify(itemPickupPointService).findItemPickupPointByProductSkuOrItemSku(STORE_ID, itemsSummaryDetailRequestVo.getProductSku(),
        itemsSummaryDetailRequestVo.getItemSku(), PAGE.getPageNumber(), PAGE.getPageSize());
    verify(itemService).findByStoreIdAndItemSkusNotCached(eq(STORE_ID), any());
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
  }

  @Test
  public void getItemsSummaryDetailByFilterMppTrueEmptyResponseTest() throws Exception {
    itemsSummaryDetailRequestVo.setResponseWithoutPickupPoint(true);
    Page<ItemPickupPoint> itemPickupPoints = new PageImpl<>(new ArrayList<>());
    when(itemPickupPointService.findItemPickupPointByProductSkuOrItemSku(STORE_ID, itemsSummaryDetailRequestVo.getProductSku(),
        itemsSummaryDetailRequestVo.getItemSku(), PAGE.getPageNumber(), PAGE.getPageSize())).thenReturn(itemPickupPoints);
    this.itemSummaryFilterServiceImpl
        .getItemsSummaryDetailByFilter(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
            ItemSummaryServiceImplTest.REQUEST_ID, this.itemsSummaryDetailRequestVo, PAGE.getPageNumber(),
            PAGE.getPageSize());
    verify(itemPickupPointService).findItemPickupPointByProductSkuOrItemSku(STORE_ID, itemsSummaryDetailRequestVo.getProductSku(),
        itemsSummaryDetailRequestVo.getItemSku(), PAGE.getPageNumber(), PAGE.getPageSize());
  }

  @Test
  public void getItemsSummaryDetailByFilterMppTrueTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "cncForWarehouseFeatureSwitch",
        Boolean.valueOf("true"));
    itemsSummaryDetailRequestVo.setResponseWithoutPickupPoint(true);
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPointList.add(itemPickupPoint);
    Page<ItemPickupPoint> itemPickupPoints = new PageImpl<>(itemPickupPointList);
    HashMap<String, Set<Price>> offlineItemIdToPriceMap = new HashMap<>();
    offlineItemIdToPriceMap.put(itemPickupPointList.get(0).getOfflineItemId(), itemPickupPointList.get(0).getPrice());
    item.setWholesalePriceActivated(true);
    item.setItemCode(ITEM_CODE);
    product.setSynchronized(true);
    item.setBundleRecipe(new HashSet<>(Arrays.asList(bundleRecipe)));
    Set<String> itemCodeList = Arrays.asList(item).stream().map(Item::getItemCode).collect(Collectors.toSet());
    when(this.masterDataService
        .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
            ItemSummaryServiceImplTest.REQUEST_ID, itemCodeList)).thenReturn(masterDataItemMap);
    when(itemService.findByStoreIdAndItemSkusNotCached(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.getProductDeletedOrUndeleted(STORE_ID, itemPickupPointList.get(0).getProductSku())).thenReturn(product);
    when(productService.getProductL3CountByProductCode(PRODUCT_CODE)).thenReturn((long) 2);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoints(Mockito.anyList())).thenReturn(offlineItemIdToPriceMap);
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    when(itemPickupPointService.findItemPickupPointByProductSkuOrItemSku(STORE_ID, itemsSummaryDetailRequestVo.getProductSku(),
        itemsSummaryDetailRequestVo.getItemSku(), PAGE.getPageNumber(), PAGE.getPageSize())).thenReturn(itemPickupPoints);
    ItemSummaryPageResponseVo itemSummaryPageResponseVo = this.itemSummaryFilterServiceImpl
        .getItemsSummaryDetailByFilter(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
            ItemSummaryServiceImplTest.REQUEST_ID, this.itemsSummaryDetailRequestVo, PAGE.getPageNumber(),
            PAGE.getPageSize());
    verify(productService).getProductDeletedOrUndeleted(STORE_ID, itemPickupPointList.get(0).getProductSku());
    verify(itemPriceService).getDiscountItemPickupPoints(Mockito.anyList());
    verify(itemPickupPointService).findItemPickupPointByProductSkuOrItemSku(STORE_ID, itemsSummaryDetailRequestVo.getProductSku(),
        itemsSummaryDetailRequestVo.getItemSku(), PAGE.getPageNumber(), PAGE.getPageSize());
    verify(itemPickupPointService).findItemPickupPointByProductSkuOrItemSku(STORE_ID, itemsSummaryDetailRequestVo.getProductSku(),
        itemsSummaryDetailRequestVo.getItemSku(), PAGE.getPageNumber(), PAGE.getPageSize());
    verify(itemService).findByStoreIdAndItemSkusNotCached(eq(STORE_ID), any());
    verify(itemService,Mockito.times(3)).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    Assertions.assertEquals(10.0,itemSummaryPageResponseVo.getItemSummaryResponses().get(0).getOriginalSellingPrice(),1);
    Assertions.assertEquals(bundleRecipe.getItemSku(), itemSummaryPageResponseVo.getItemSummaryResponses()
        .get(0).getBundleRecipe().iterator().next().getItemSku());
    Assertions.assertEquals(bundleRecipe.getQuantity(), itemSummaryPageResponseVo.getItemSummaryResponses()
        .get(0).getBundleRecipe().iterator().next().getQuantity());
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "cncForWarehouseFeatureSwitch",
        Boolean.valueOf("false"));
  }

  @Test
  public void getItemsSummaryDetailByFilterMppTrueUnSyncTest() throws Exception {
    itemsSummaryDetailRequestVo.setResponseWithoutPickupPoint(true);
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPointList.add(itemPickupPoint);
    Page<ItemPickupPoint> itemPickupPoints = new PageImpl<>(itemPickupPointList);
    HashMap<String, Set<Price>> offlineItemIdToPriceMap = new HashMap<>();
    offlineItemIdToPriceMap.put(itemPickupPointList.get(0).getOfflineItemId(), itemPickupPointList.get(0).getPrice());
    item.setWholesalePriceActivated(true);
    item.setItemCode(ITEM_CODE);
    item.setBundleRecipe(new HashSet<>(Arrays.asList(bundleRecipe)));
    Set<String> itemCodeList = Arrays.asList(item).stream().map(Item::getItemCode).collect(Collectors.toSet());
    when(this.masterDataService
        .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
            ItemSummaryServiceImplTest.REQUEST_ID, itemCodeList)).thenReturn(masterDataItemMap);
    when(itemService.findByStoreIdAndItemSkusNotCached(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.getProductDeletedOrUndeleted(STORE_ID, itemPickupPointList.get(0).getProductSku())).thenReturn(product);
    when(productService.getProductL3CountByProductCode(PRODUCT_CODE)).thenReturn((long) 2);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoints(Mockito.anyList())).thenReturn(offlineItemIdToPriceMap);
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    when(itemPickupPointService.findItemPickupPointByProductSkuOrItemSku(STORE_ID, itemsSummaryDetailRequestVo.getProductSku(),
        itemsSummaryDetailRequestVo.getItemSku(), PAGE.getPageNumber(), PAGE.getPageSize())).thenReturn(itemPickupPoints);
    ItemSummaryPageResponseVo itemSummaryPageResponseVo = this.itemSummaryFilterServiceImpl
        .getItemsSummaryDetailByFilter(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
            ItemSummaryServiceImplTest.REQUEST_ID, this.itemsSummaryDetailRequestVo, PAGE.getPageNumber(),
            PAGE.getPageSize());
    verify(productService).getProductDeletedOrUndeleted(STORE_ID, itemPickupPointList.get(0).getProductSku());
    verify(itemPriceService).getDiscountItemPickupPoints(Mockito.anyList());
    verify(itemPickupPointService).findItemPickupPointByProductSkuOrItemSku(STORE_ID, itemsSummaryDetailRequestVo.getProductSku(),
        itemsSummaryDetailRequestVo.getItemSku(), PAGE.getPageNumber(), PAGE.getPageSize());
    verify(itemPickupPointService).findItemPickupPointByProductSkuOrItemSku(STORE_ID, itemsSummaryDetailRequestVo.getProductSku(),
        itemsSummaryDetailRequestVo.getItemSku(), PAGE.getPageNumber(), PAGE.getPageSize());
    verify(itemService).findByStoreIdAndItemSkusNotCached(eq(STORE_ID), any());
    verify(itemService,Mockito.times(3)).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    Assertions.assertEquals(10.0,itemSummaryPageResponseVo.getItemSummaryResponses().get(0).getOriginalSellingPrice(),1);
    Assertions.assertEquals(bundleRecipe.getItemSku(), itemSummaryPageResponseVo.getItemSummaryResponses()
        .get(0).getBundleRecipe().iterator().next().getItemSku());
    Assertions.assertEquals(bundleRecipe.getQuantity(), itemSummaryPageResponseVo.getItemSummaryResponses()
        .get(0).getBundleRecipe().iterator().next().getQuantity());
  }

  @Test
  public void getItemsSummaryDetailByFilterTestWithNullFilter() {
    itemsSummaryDetailRequestVo.setItemSku(null);
    itemsSummaryDetailRequestVo.setProductSku(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemSummaryFilterServiceImpl
        .getItemsSummaryDetailByFilter(ItemSummaryServiceImplTest.STORE_ID, ItemSummaryServiceImplTest.USERNAME,
            ItemSummaryServiceImplTest.REQUEST_ID, itemsSummaryDetailRequestVo, PAGE.getPageNumber(),
            PAGE.getPageSize()));
  }

  @Test
  public void getItemsSummaryDetailByFilterStoreIdNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemSummaryFilterServiceImpl
        .getItemsSummaryDetailByFilter(null, ItemSummaryServiceImplTest.USERNAME,
            ItemSummaryServiceImplTest.REQUEST_ID, this.itemsSummaryDetailRequestVo, PAGE.getPageNumber(), PAGE.getPageSize()));
  }

  @Test
  public void updateItemListingTest() throws Exception {
    List<ProductChangeEventType> productChangeEventTypes = new ArrayList<>();
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setMerchantCode(MERCHANT_CODE);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setMerchantSku("merchantSku");
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode("pickup");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(true);
    when(itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(), eq(itemViewConfigs)))
        .thenReturn(true);
    when(offlineItemService.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenReturn(offlineItemList);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID,
        Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    doNothing().when(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), eq(itemViewConfigs));
    doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(product),
            itemArgumentCaptor.capture());
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.getAllValues().get(1), itemViewConfigs);
    verify(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.getAllValues().get(2), itemViewConfigs);
    verify(itemPriceService)
        .publishItemPriceChangeEvent(USERNAME, REQUEST_ID, product, itemArgumentCaptor.getAllValues().get(3));
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.getAllValues().get(0), new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(),
        Mockito.anyList(), anyBoolean());
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemService).updateItemPickupPointDeliveryFalse(anyString(), any(), anyList());
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
  }

  @Test
  public void updateItemListingTestcnc1pon() throws Exception {
    ReflectionTestUtils.setField(this.itemSummaryFilterServiceImpl, "cncForWarehouseFeatureSwitch", true);
    List<ProductChangeEventType> productChangeEventTypes = new ArrayList<>();
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setMerchantCode(MERCHANT_CODE);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setMerchantSku("merchantSku");
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode("pickup");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    itemListingUpdateRequestVo.getItemViewConfigs().add(ItemViewConfig.builder().isDiscoverable(true).channel(Constants.CNC).build());
    itemPickupPoint.getAllItemViewConfigs().add(ItemViewConfig.builder().isDiscoverable(false).channel(Constants.CNC).build());
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(true);
    when(itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(), eq(itemViewConfigs)))
        .thenReturn(true);
    when(offlineItemService.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenReturn(offlineItemList);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID,
        Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    doNothing().when(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), eq(itemViewConfigs));
    doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(product),
            itemArgumentCaptor.capture());
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.getAllValues().get(1), itemViewConfigs);
    verify(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.getAllValues().get(2), itemViewConfigs);
    verify(itemPriceService)
        .publishItemPriceChangeEvent(USERNAME, REQUEST_ID, product, itemArgumentCaptor.getAllValues().get(3));
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.getAllValues().get(0), new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(),
        Mockito.anyList(), anyBoolean());
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.any());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemService).updateItemPickupPointDeliveryFalse(anyString(), any(), anyList());
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
  }

  @Test
  public void updateItemListingPriceUpdateTestTest() throws Exception {
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setMerchantCode(MERCHANT_CODE);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setMerchantSku("merchantSku");
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode("pickup");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(true);
    when(itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(), eq(itemViewConfigs)))
        .thenReturn(true);
    when(offlineItemService.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenReturn(offlineItemList);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    itemPickupPoint.setCncActive(true);
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID,
        Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .productListingAtomicUpdate(productArgumentCaptor.capture(), itemListArgumentCaptor.capture());
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    doNothing().when(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), eq(itemViewConfigs));
    doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(product),
            itemArgumentCaptor.capture());
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.getAllValues().get(1), itemViewConfigs);
    verify(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.getAllValues().get(2), itemViewConfigs);
    verify(itemPriceService)
        .publishItemPriceChangeEvent(USERNAME, REQUEST_ID, product, itemArgumentCaptor.getAllValues().get(3));
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.getAllValues().get(0), new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(),
        Mockito.anyList(), anyBoolean());
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemService).updateItemPickupPointDeliveryFalse(anyString(), any(), anyList());
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
    verify(saveAndPublishService).publishOfflineItemChangeSellerEvent(Mockito.any(OfflineItemChange.class));
  }

  @Test
  public void updateItemListingNullItemAndPickupPointTest() throws Exception {
    ItemListingUpdateRequestVo itemListingUpdateRequestVo1 = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo1.setItemSku(PRODUCT_SKU);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo2 = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo2.setItemSku(item.getItemSku());
    itemPickupPoint.setItemSku("default-item-sku");

    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(
        product);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(
        Arrays.asList(itemPickupPoint));
    itemSummaryFilterServiceImpl.updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo1, itemListingUpdateRequestVo2));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(PRODUCT_SKU, ITEM_SKU));
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void updateItemListingEmptyItemListingUpdateRequestVoTest() throws Exception {
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, null, new ArrayList<>());
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void updateItemListingTestDifferedReindex() throws Exception {
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setMerchantCode(MERCHANT_CODE);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setMerchantSku("merchantSku");
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode("pickup");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(true);
    when(itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(), eq(itemViewConfigs)))
        .thenReturn(true);
    when(offlineItemService.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenReturn(offlineItemList);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "true", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
          eq(false));
    doNothing().when(productAndItemSolrIndexerService)
        .productListingAtomicUpdate(productArgumentCaptor.capture(), itemListArgumentCaptor.capture());
    doNothing().when(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), eq(itemViewConfigs));
    doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(product),
            itemArgumentCaptor.capture());
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.getAllValues().get(1), itemViewConfigs);
    verify(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.getAllValues().get(2), itemViewConfigs);
    verify(itemPriceService)
        .publishItemPriceChangeEvent(USERNAME, REQUEST_ID, product, itemArgumentCaptor.getAllValues().get(3));
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.getAllValues().get(0), new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(), Mockito.anyList(),
          anyBoolean());
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
          eq(true));
    verify(itemService).updateItemPickupPointDeliveryFalse(anyString(), any(), anyList());
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
  }

  @Test
  public void updateItemListingNoPriceAndViewConfigChangeTest() throws Exception {
    itemViewConfigs.forEach(itemViewConfig -> {
      itemViewConfig.setDiscoverable(true);
      itemViewConfig.setBuyable(false);
    });
    item.setMerchantCode(MERCHANT_CODE);
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    itemListingUpdateRequestVo.setMerchantSku("merchantSku");
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode("pickup");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(new HashSet<>(Collections.singletonList(new ItemViewConfig())));
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(item);
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(false);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(),
        eq(new HashSet<>(Collections.singletonList(new ItemViewConfig()))))).thenReturn(false);
    when(offlineItemService.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenReturn(offlineItemList);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(new ArrayList<>());
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(
        itemArgumentCaptor.capture(), eq(new HashSet<>(Collections.singletonList(new ItemViewConfig()))));
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.getAllValues().get(0), new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(),
        Mockito.anyList(), anyBoolean());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemService).updateItemPickupPointDeliveryFalse(anyString(), any(), anyList());
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void updateItemListingNoMerchantSkuChangeTest() throws Exception {
    itemViewConfigs.forEach(itemViewConfig -> {
      itemViewConfig.setDiscoverable(true);
      itemViewConfig.setBuyable(true);
    });
    item.setMerchantCode(MERCHANT_CODE);
    item.setArchived(true);
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setPickupPointCode("pickup");
    item.setOff2OnChannelActive(true);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode("pickup");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    doNothing().when(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), eq(itemViewConfigs));
   when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(false);
    when(itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(), eq(itemViewConfigs)))
        .thenReturn(true);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.getAllValues().get(1), itemViewConfigs);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(),
        Mockito.anyList(), anyBoolean());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.getAllValues().get(2), itemViewConfigs);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
  }

  @Test
  public void updateItemListingNoMerchantSkuChangeTestBuyableFalse() throws Exception {
    itemViewConfigs.forEach(itemViewConfig -> {
      itemViewConfig.setDiscoverable(false);
      itemViewConfig.setBuyable(false);
    });
    item.setMerchantCode(MERCHANT_CODE);
    item.setArchived(true);
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setPickupPointCode("pickup");
    item.setOff2OnChannelActive(true);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode("pickup");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    doNothing().when(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), eq(itemViewConfigs));
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(false);
    when(itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(), eq(itemViewConfigs)))
        .thenReturn(true);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.getAllValues().get(1), itemViewConfigs);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.getAllValues().get(0), new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(),
        Mockito.anyList(), anyBoolean());
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    verify(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.getAllValues().get(2), itemViewConfigs);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
  }

  @Test
  public void updateItemListingNoMerchantSkuChangeTestDiscoverableTrue() throws Exception {
    itemViewConfigs.forEach(itemViewConfig -> {
      itemViewConfig.setDiscoverable(true);
      itemViewConfig.setBuyable(false);
    });
    item.setMerchantCode(MERCHANT_CODE);
    item.setArchived(true);
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setPickupPointCode("pickup");
    item.setOff2OnChannelActive(true);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode("pickup");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    doNothing().when(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), eq(itemViewConfigs));
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(false);
    when(itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(), eq(itemViewConfigs)))
        .thenReturn(true);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.getAllValues().get(1), itemViewConfigs);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.getAllValues().get(0), new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(),
        Mockito.anyList(), anyBoolean());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.getAllValues().get(2), itemViewConfigs);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
  }

  @Test
  public void updateItemListingNoMerchantSkuChangeTestArchivedFalse() throws Exception {
    itemViewConfigs.forEach(itemViewConfig -> {
      itemViewConfig.setDiscoverable(true);
      itemViewConfig.setBuyable(true);
    });
    item.setMerchantCode(MERCHANT_CODE);
    item.setArchived(false);
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setPickupPointCode("pickup");
    item.setOff2OnChannelActive(true);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode("pickup");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    product.setB2cActivated(true);
    product.setOnline(true);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    doNothing().when(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), eq(itemViewConfigs));
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(false);
    when(itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(), eq(itemViewConfigs)))
        .thenReturn(true);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.getAllValues().get(1), itemViewConfigs);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.getAllValues().get(0), new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(), Mockito.anyList(),
          anyBoolean());
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    verify(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.getAllValues().get(2), itemViewConfigs);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
  }

  @Test
  public void updateItemListingOnlineFlagFalse() throws Exception {
    itemViewConfigs.forEach(itemViewConfig -> {
      itemViewConfig.setDiscoverable(true);
      itemViewConfig.setBuyable(true);
    });
    item.setMerchantCode(MERCHANT_CODE);
    item.setArchived(false);
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setPickupPointCode("pickup");
    item.setOff2OnChannelActive(true);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode("pickup");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    product.setB2cActivated(true);
    product.setOnline(false);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    doNothing().when(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), eq(itemViewConfigs));
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(false);
    when(itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(), eq(itemViewConfigs)))
        .thenReturn(true);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.getAllValues().get(1), itemViewConfigs);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.getAllValues().get(0), new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(), Mockito.anyList(),
          anyBoolean());
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    verify(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.getAllValues().get(2), itemViewConfigs);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
    Assertions.assertTrue(productArgumentCaptor.getValue().isOnline());
  }

  @Test
  public void updateItemListingB2CFlagFalse() throws Exception {
    itemViewConfigs.forEach(itemViewConfig -> {
      itemViewConfig.setDiscoverable(true);
      itemViewConfig.setBuyable(true);
    });
    item.setMerchantCode(MERCHANT_CODE);
    item.setArchived(false);
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setPickupPointCode("pickup");
    item.setOff2OnChannelActive(true);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode("pickup");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    product.setB2cActivated(false);
    product.setOnline(true);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    doNothing().when(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), eq(itemViewConfigs));
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(false);
    when(itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(), eq(itemViewConfigs)))
        .thenReturn(true);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.getAllValues().get(1), itemViewConfigs);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.getAllValues().get(0), new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(), Mockito.anyList(),
          anyBoolean());
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    verify(productHelperService)
        .updateItemViewConfigForExistingChannel(itemArgumentCaptor.getAllValues().get(2), itemViewConfigs);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
    Assertions.assertTrue(productArgumentCaptor.getValue().isOnline());
    Assertions.assertTrue(productArgumentCaptor.getValue().getB2cActivated());
  }

  @Test
  public void updateItemListingWholesaleUpdateTest() throws Exception {
    itemViewConfigs.forEach(itemViewConfig -> {
      itemViewConfig.setDiscoverable(false);
      itemViewConfig.setBuyable(true);
    });
    item.setActivePromoBundlings(new HashSet<>());
    item.setMerchantCode(MERCHANT_CODE);
    item.setMerchantSku("merchantSku");
    product.setOff2OnChannelActive(true);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemListingUpdateRequestVo.setMerchantSku("merchantSku");
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(true);
    when(itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(), eq(itemViewConfigs)))
        .thenReturn(false);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(product),
            itemArgumentCaptor.capture());
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.getAllValues().get(1), itemViewConfigs);
    verify(itemPriceService)
        .publishItemPriceChangeEvent(USERNAME, REQUEST_ID, product, itemArgumentCaptor.getAllValues().get(2));
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.getAllValues().get(0), new ArrayList<>(),
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(), Mockito.anyList(),
          anyBoolean());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void updateItemListingWholesaleUpdateTest2() throws Exception {
    item.setActivePromoBundlings(new HashSet<>());
    item.setMerchantCode(MERCHANT_CODE);
    item.setOff2OnChannelActive(true);
    product.setOff2OnChannelActive(false);
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemListingUpdateRequestVo.setMerchantSku("merchantSku");
    itemListingUpdateRequestVo.setOff2OnChannelActive(false);
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(new HashSet<>(Collections.singletonList(new ItemViewConfig())));
    when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    when(itemService.validatePriceChangeAndSet(itemArgumentCaptor.capture(), Mockito.eq(prices), Mockito.eq(USERNAME)))
        .thenReturn(false);
    when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(itemArgumentCaptor.capture(),
        eq(new HashSet<>(Collections.singletonList(new ItemViewConfig()))))).thenReturn(false);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito.any(), anyString()))
        .thenReturn(Arrays.asList(item));
    when(offlineItemService.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenReturn(new ArrayList<>());
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(productAndItemSolrIndexerService)
        .updateProductAndItemDetailsInSolr(productArgumentCaptor.capture(), itemListArgumentCaptor.capture(),
            Mockito.anyBoolean());
    doNothing().when(itemPriceService)
        .publishItemPriceChangeEvent(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(product),
            itemArgumentCaptor.capture());
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU))).thenReturn(Arrays.asList(itemPickupPoint));
    itemSummaryFilterServiceImpl
        .updateItemListing(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemService).validatePriceChangeAndSet(itemArgumentCaptor.getAllValues().get(0), prices, USERNAME);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.getAllValues().get(0));
    Mockito.verify(saveAndPublishService).publishListOfItems(itemListArgumentCaptor.capture(), Mockito.anyList(),
        Mockito.anyList(), anyBoolean());
    verify(itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(
        itemArgumentCaptor.getAllValues().get(1), new HashSet<>(Collections.singletonList(new ItemViewConfig())));
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(
        productArgumentCaptor.getAllValues().get(1), itemListArgumentCaptor.getAllValues().get(1), false);
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(itemService).updateItemPickupPointDeliveryFalse(anyString(), any(), anyList());
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, Arrays.asList(ITEM_SKU));
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void getItemPickupPointsAndItemNameByProductSkuMppTrueTest() {
    when(itemPickupPointService
        .findItemPickupPointByProductSkusOrItemSkus(STORE_ID, Collections.singletonList(PRODUCT_SKU), new ArrayList<>(),
            PAGE.getPageNumber(), PAGE.getPageSize())).thenReturn(new PageImpl<>(itemPickupPointList));
    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(item.getItemSku().toUpperCase());
    String[] includeFields = {ProductFieldNames.ITEM_SKU, ProductFieldNames.GENERATED_ITEM_NAME,
        ProductFieldNames.GENERATED_ITEM_NAME_IN_MASTER_DATA_ITEM, ProductFieldNames.IS_SYNCHRONIZED};
    this.item.setItemSku(ITEM_SKU);
    this.item.setGeneratedItemName(ITEM_NAME);
    this.item.setSynchronized(true);
    List<Item> itemList = new ArrayList<>();
    itemList.add(this.item);
    when(this.itemService.getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields))
        .thenReturn(itemList);
    when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        Collections.singletonList(PICKUP_POINT_CODE))).thenReturn(Collections
        .singletonList(BusinessPartnerPickupPoint.builder().name(PICKUP_POINT_NAME).code(PICKUP_POINT_CODE).build()));
    Page<ItemPickupPointVo> itemPickupPointVos =
        itemSummaryFilterServiceImpl.getItemPickupPointsAndItemNameByProductSku(STORE_ID,
            PRODUCT_SKU, PAGE.getPageNumber(), PAGE.getPageSize(), false);
    verify(itemPickupPointService).findItemPickupPointByProductSkusOrItemSkus(
        STORE_ID, Collections.singletonList(PRODUCT_SKU), new ArrayList<>(), PAGE.getPageNumber(), PAGE.getPageSize());
    verify(this.itemService).getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields);
    verify(this.businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,Collections.singletonList(PICKUP_POINT_CODE));
    assertEquals(ITEM_SKU, itemPickupPointVos.getContent().get(0).getItemSku());
    assertEquals(ITEM_NAME, itemPickupPointVos.getContent().get(0).getItemName());
    assertEquals(PICKUP_POINT_CODE, itemPickupPointVos.getContent().get(0).getPickupPointCode());
    assertEquals(PICKUP_POINT_NAME,itemPickupPointVos.getContent().get(0).getPickupPointName());
  }

  @Test
  public void getItemPickupPointsAndItemNameByProductSkuMppTrueFbbActivatedTest() {
    List<ItemPickupPoint> itemPickupPoints= new ArrayList<>();
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setFbbActivated(true);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoints.add(itemPickupPoint1);
    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(item.getItemSku().toUpperCase());
    String[] includeFields = {ProductFieldNames.ITEM_SKU, ProductFieldNames.GENERATED_ITEM_NAME,
        ProductFieldNames.GENERATED_ITEM_NAME_IN_MASTER_DATA_ITEM, ProductFieldNames.IS_SYNCHRONIZED};
    this.item.setItemSku(ITEM_SKU);
    this.item.setGeneratedItemName(ITEM_NAME);
    this.item.setSynchronized(true);
    List<Item> itemList = new ArrayList<>();
    itemList.add(this.item);
    when(this.itemService.getItemsByProductSkusPaginated(STORE_ID, PRODUCT_SKU,
        PageRequest.of(PAGE.getPageNumber(), PAGE.getPageSize(), Sort.by(Constants.ITEM_SKU))))
        .thenReturn(new PageImpl<>(itemList));
    when(this.itemPickupPointService
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(item.getItemSku())))
        .thenReturn(itemPickupPoints);
    when(this.itemService.getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields))
        .thenReturn(itemList);
    when(this.businessPartnerPickupPointService
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Collections.singletonList(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(
            BusinessPartnerPickupPoint.builder().name(PICKUP_POINT_NAME).code(PICKUP_POINT_CODE).build()));
    Page<ItemPickupPointVo> itemPickupPointVos = itemSummaryFilterServiceImpl
        .getItemPickupPointsAndItemNameByProductSku(STORE_ID, PRODUCT_SKU, PAGE.getPageNumber(), PAGE.getPageSize(),
            true);
    verify(this.itemService).getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields);
    verify(this.businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Collections.singletonList(PICKUP_POINT_CODE));
    verify(this.itemService).getItemsByProductSkusPaginated(STORE_ID, PRODUCT_SKU,
        PageRequest.of(PAGE.getPageNumber(), PAGE.getPageSize(), Sort.by(Constants.ITEM_SKU)));
    verify(this.itemPickupPointService)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(item.getItemSku()));
    assertEquals(ITEM_SKU, itemPickupPointVos.getContent().get(0).getItemSku());
    assertEquals(ITEM_NAME, itemPickupPointVos.getContent().get(0).getItemName());
    assertEquals(PICKUP_POINT_CODE, itemPickupPointVos.getContent().get(0).getPickupPointCode());
    assertEquals(PICKUP_POINT_NAME, itemPickupPointVos.getContent().get(0).getPickupPointName());
  }

  @Test
  public void getItemPickupPointsAndItemNameByProductSkuMppTrueFbbActivatedButNoFbbL5Test() {
    List<ItemPickupPoint> itemPickupPoints= new ArrayList<>();
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setFbbActivated(false);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoints.add(itemPickupPoint1);
    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(item.getItemSku().toUpperCase());
    String[] includeFields = {ProductFieldNames.ITEM_SKU, ProductFieldNames.GENERATED_ITEM_NAME,
        ProductFieldNames.GENERATED_ITEM_NAME_IN_MASTER_DATA_ITEM, ProductFieldNames.IS_SYNCHRONIZED};
    this.item.setItemSku(ITEM_SKU);
    this.item.setGeneratedItemName(ITEM_NAME);
    this.item.setSynchronized(true);
    List<Item> itemList = new ArrayList<>();
    itemList.add(this.item);
    when(this.itemService.getItemsByProductSkusPaginated(STORE_ID, PRODUCT_SKU,
        PageRequest.of(PAGE.getPageNumber(), PAGE.getPageSize(), Sort.by(Constants.ITEM_SKU))))
        .thenReturn(new PageImpl<>(itemList));
    when(this.itemPickupPointService
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(item.getItemSku())))
        .thenReturn(itemPickupPoints);
    when(this.itemService.getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields))
        .thenReturn(itemList);
    Page<ItemPickupPointVo> itemPickupPointVos = itemSummaryFilterServiceImpl
        .getItemPickupPointsAndItemNameByProductSku(STORE_ID, PRODUCT_SKU, PAGE.getPageNumber(), PAGE.getPageSize(),
            true);
    verify(this.itemService).getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields);
    verify(this.itemService).getItemsByProductSkusPaginated(STORE_ID, PRODUCT_SKU,
        PageRequest.of(PAGE.getPageNumber(), PAGE.getPageSize(), Sort.by(Constants.ITEM_SKU)));
    verify(this.itemPickupPointService)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(item.getItemSku()));
    assertEquals(ITEM_SKU, itemPickupPointVos.getContent().get(0).getItemSku());
    assertEquals(ITEM_NAME, itemPickupPointVos.getContent().get(0).getItemName());
    assertEquals(null, itemPickupPointVos.getContent().get(0).getPickupPointCode());
    assertEquals(null, itemPickupPointVos.getContent().get(0).getPickupPointName());
  }
  @Test
  public void getItemPickupPointsAndItemNameByProductSkuMppTrueFbbActivatedBut1FbbL5Test() {
    List<ItemPickupPoint> itemPickupPoints= new ArrayList<>();
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setFbbActivated(false);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE);
    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setItemSku(ITEM_SKU);
    itemPickupPoint2.setFbbActivated(true);
    itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoints.add(itemPickupPoint1);
    itemPickupPoints.add(itemPickupPoint2);
    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(item.getItemSku().toUpperCase());
    String[] includeFields = {ProductFieldNames.ITEM_SKU, ProductFieldNames.GENERATED_ITEM_NAME,
        ProductFieldNames.GENERATED_ITEM_NAME_IN_MASTER_DATA_ITEM, ProductFieldNames.IS_SYNCHRONIZED};
    this.item.setItemSku(ITEM_SKU);
    this.item.setGeneratedItemName(ITEM_NAME);
    this.item.setSynchronized(true);
    Item item1 = new Item();
    BeanUtils.copyProperties(item, item1);
    item1.setItemSku(ITEM_SKU_1);
    List<Item> itemList = new ArrayList<>();
    itemList.add(this.item);
    itemList.add(item1);
    itemSkuSet.add(item1.getItemSku().toUpperCase());
    when(this.itemService.getItemsByProductSkusPaginated(STORE_ID, PRODUCT_SKU,
        PageRequest.of(PAGE.getPageNumber(), PAGE.getPageSize(), Sort.by(Constants.ITEM_SKU))))
        .thenReturn(new PageImpl<>(itemList));
    when(this.itemPickupPointService
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(item.getItemSku())))
        .thenReturn(itemPickupPoints);
    when(this.businessPartnerPickupPointService
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Collections.singletonList(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(
            BusinessPartnerPickupPoint.builder().name(PICKUP_POINT_NAME).code(PICKUP_POINT_CODE).build()));
    when(this.itemPickupPointService
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(item1.getItemSku())))
        .thenReturn(Collections.singletonList(itemPickupPoint1));
    when(this.itemService.getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields))
        .thenReturn(itemList);
    Page<ItemPickupPointVo> itemPickupPointVos = itemSummaryFilterServiceImpl
        .getItemPickupPointsAndItemNameByProductSku(STORE_ID, PRODUCT_SKU, PAGE.getPageNumber(), PAGE.getPageSize(),
            true);
    verify(this.itemService).getItemsByItemSkus(ItemSummaryServiceImplTest.STORE_ID, itemSkuSet, includeFields);
    verify(this.itemService).getItemsByProductSkusPaginated(STORE_ID, PRODUCT_SKU,
        PageRequest.of(PAGE.getPageNumber(), PAGE.getPageSize(), Sort.by(Constants.ITEM_SKU)));
    verify(this.itemPickupPointService)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(item.getItemSku()));
    verify(this.itemPickupPointService)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(item1.getItemSku()));
    verify(this.businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Collections.singletonList(PICKUP_POINT_CODE));
    assertEquals(ITEM_SKU, itemPickupPointVos.getContent().get(0).getItemSku());
    assertEquals(ITEM_NAME, itemPickupPointVos.getContent().get(0).getItemName());
    assertEquals(null, itemPickupPointVos.getContent().get(1).getPickupPointCode());
    assertEquals(null, itemPickupPointVos.getContent().get(1).getPickupPointName());
    assertEquals(PICKUP_POINT_CODE, itemPickupPointVos.getContent().get(0).getPickupPointCode());
    assertEquals(PICKUP_POINT_NAME, itemPickupPointVos.getContent().get(0).getPickupPointName());
  }

  @Test
  public void getItemImagesListResponseTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "pcbGetMainImageDataDisabled", false);
    Mockito.when(itemService.findListOfItemCodesByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU)))
        .thenReturn(itemCodeAndSkuMap);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_SKU);
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(false);
    List<MasterDataItemImage> masterDataItemImages = masterDataItemMap.get(ITEM_CODE).getMasterDataItemImages();
    masterDataItemImages = new ArrayList<>(masterDataItemImages);
    masterDataItemImages.add(masterDataItemImage);
    masterDataItemMap.get(ITEM_CODE).setMasterDataItemImages(masterDataItemImages);
    when(this.masterDataService.getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE))).thenReturn(masterDataItemMap);
    List<ItemImagesListResponse> listResponse =
        itemSummaryFilterServiceImpl.getItemImagesListResponse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(itemService)
        .findListOfItemCodesByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(masterDataService)
        .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
            Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE));
    Assertions.assertEquals(1, listResponse.size());
    Assertions.assertEquals(1, listResponse.get(0).getImagesResponseList().size());
  }

  @Test
  public void getItemImagesListResponseDisabledPcbTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "pcbGetMainImageDataDisabled", true);
    item.setMainImageUrl(IMAGE_URL);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    List<ItemImagesListResponse> listResponse =
        itemSummaryFilterServiceImpl.getItemImagesListResponse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)));
    Assertions.assertEquals(ITEM_SKU, listResponse.get(0).getItemSku());
    Assertions.assertEquals(IMAGE_URL, listResponse.get(0).getImagesResponseList().get(0).getLocationPath());
    assertTrue(listResponse.get(0).getImagesResponseList().get(0).isMainImage());
  }

  @Test
  public void getItemImagesListResponseDisabledPcbImageUrlNotPresentInPrdItemTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "pcbGetMainImageDataDisabled", true);
    item.setMainImageUrl(null);
    item.setItemCode(ITEM_CODE);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_SKU);
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(false);
    List<MasterDataItemImage> masterDataItemImages = masterDataItemMap.get(ITEM_CODE).getMasterDataItemImages();
    masterDataItemImages = new ArrayList<>(masterDataItemImages);
    masterDataItemImages.add(masterDataItemImage);
    masterDataItemMap.get(ITEM_CODE).setMasterDataItemImages(masterDataItemImages);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_CODE);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    when(this.masterDataService.getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE))).thenReturn(masterDataItemMap);

    List<ItemImagesListResponse> listResponse =
        itemSummaryFilterServiceImpl.getItemImagesListResponse(STORE_ID, Collections.singleton(ITEM_SKU));

    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)));
    Mockito.verify(masterDataService)
        .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
            Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE));
    Assertions.assertEquals(ITEM_SKU, listResponse.get(0).getItemSku());
    Assertions.assertEquals(IMAGE_URL, listResponse.get(0).getImagesResponseList().get(0).getLocationPath());
    assertTrue(listResponse.get(0).getImagesResponseList().get(0).isMainImage());
  }

  @Test
  public void getItemImagesListResponseItemAttributeDetailsTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "pcbGetMainImageDataDisabled", true);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "populateItemAttributeDetail", true);
    item.setMainImageUrl(null);
    item.setItemCode(ITEM_CODE);
    product.setProductCode(PRODUCT_CODE);
    masterDataItemMap.values().forEach(masterDataItem -> masterDataItem.setProductCode(PRODUCT_CODE));
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_SKU);
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(false);
    List<MasterDataItemImage> masterDataItemImages = masterDataItemMap.get(ITEM_CODE).getMasterDataItemImages();
    masterDataItemImages = new ArrayList<>(masterDataItemImages);
    masterDataItemImages.add(masterDataItemImage);
    masterDataItemMap.get(ITEM_CODE).setMasterDataItemImages(masterDataItemImages);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_CODE);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Collections.singletonList(item));
    when(this.masterDataService.getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE))).thenReturn(masterDataItemMap);
    Mockito.when(productService.getProductDeletedOrUndeleted(STORE_ID, item.getProductSku())).thenReturn(product);

    List<ItemImagesListResponse> listResponse =
        itemSummaryFilterServiceImpl.getItemImagesListResponse(STORE_ID, Collections.singleton(ITEM_SKU));

    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)));
    Mockito.verify(masterDataService)
        .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
            Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE));
    Mockito.verify(productService).getProductDeletedOrUndeleted(STORE_ID,
      item.getProductSku());
    Mockito.verify(objectConverterService)
        .overrideDefiningAttributeDetailsFromL3ToL4(Collections.singletonList(product), Collections.singletonList(item));
    Assertions.assertEquals(ITEM_SKU, listResponse.get(0).getItemSku());
    Assertions.assertEquals(IMAGE_URL, listResponse.get(0).getImagesResponseList().get(0).getLocationPath());
    assertTrue(listResponse.get(0).getImagesResponseList().get(0).isMainImage());
  }

  @Test
  public void getItemImagesListResponseItemAttributeDetailsItemTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "pcbGetMainImageDataDisabled", false);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "populateItemAttributeDetail", true);
    item.setMainImageUrl(null);
    item.setItemCode(ITEM_CODE);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU2);
    item.setProductSku(PRODUCT_SKU3);
    item.setItemSku(ITEM_SKU_1);
    masterDataItemMap.values().forEach(masterDataItem -> masterDataItem.setProductCode(PRODUCT_CODE));
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_SKU);
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(false);
    List<MasterDataItemImage> masterDataItemImages = masterDataItemMap.get(ITEM_CODE).getMasterDataItemImages();
    masterDataItemImages = new ArrayList<>(masterDataItemImages);
    masterDataItemImages.add(masterDataItemImage);
    masterDataItemMap.get(ITEM_CODE).setMasterDataItemImages(masterDataItemImages);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_CODE);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
      .thenReturn(Collections.singletonList(item));
    when(this.masterDataService.getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
      Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE))).thenReturn(masterDataItemMap);
    Mockito.when(productService.getProductDeletedOrUndeleted(STORE_ID, item.getProductSku())).thenReturn(null);

    List<ItemImagesListResponse> listResponse =
      itemSummaryFilterServiceImpl.getItemImagesListResponse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(itemService)
      .findListOfItemCodesByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(masterDataService)
      .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_REQUEST_ID, Collections.emptySet());
  }

  @Test
  public void getItemImagesListResponseItemAttributeDetailsItempcbGetMainImageDataDisabledOffTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "pcbGetMainImageDataDisabled", false);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "populateItemAttributeDetail", true);
    item.setMainImageUrl(null);
    item.setItemCode(ITEM_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    item.setProductSku(PRODUCT_SKU);
    item.setItemSku(ITEM_SKU);
    masterDataItemMap.values().forEach(masterDataItem -> masterDataItem.setProductCode(PRODUCT_CODE));
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_SKU);
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(false);
    List<MasterDataItemImage> masterDataItemImages = masterDataItemMap.get(ITEM_CODE).getMasterDataItemImages();
    masterDataItemImages = new ArrayList<>(masterDataItemImages);
    masterDataItemImages.add(masterDataItemImage);
    masterDataItemMap.get(ITEM_CODE).setMasterDataItemImages(masterDataItemImages);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_CODE);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
      .thenReturn(Collections.singletonList(item));
    when(this.masterDataService.getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
      Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE))).thenReturn(masterDataItemMap);
    Mockito.when(itemService.findListOfItemCodesByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU)))
      .thenReturn(itemCodeAndSkuMap);
    Mockito.when(productService.getProductDeletedOrUndeleted(STORE_ID, item.getProductSku())).thenReturn(product);

    List<ItemImagesListResponse> listResponse =
      itemSummaryFilterServiceImpl.getItemImagesListResponse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(itemService)
      .findListOfItemCodesByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(masterDataService)
      .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE));
  }

  @Test
  public void getItemImagesListResponseItemAttributeDetailsWithEmptyMapTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "pcbGetMainImageDataDisabled", true);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "populateItemAttributeDetail", true);
    item.setMainImageUrl(null);
    item.setItemCode(ITEM_CODE);
    product.setProductCode(PRODUCT_CODE);
    item.setProductSku(PRODUCT_SKU2);
    masterDataItemMap.values().forEach(masterDataItem -> masterDataItem.setProductCode(PRODUCT_CODE));
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_SKU);
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(false);
    List<MasterDataItemImage> masterDataItemImages = masterDataItemMap.get(ITEM_CODE).getMasterDataItemImages();
    masterDataItemImages = new ArrayList<>(masterDataItemImages);
    masterDataItemImages.add(masterDataItemImage);
    masterDataItemMap.get(ITEM_CODE).setMasterDataItemImages(masterDataItemImages);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_CODE);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
      .thenReturn(Collections.singletonList(item));
    when(this.masterDataService.getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
      Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE))).thenReturn(masterDataItemMap);
    Mockito.when(productService.getProductDeletedOrUndeleted(STORE_ID, item.getProductSku())).thenReturn(product);

    List<ItemImagesListResponse> listResponse =
      itemSummaryFilterServiceImpl.getItemImagesListResponse(STORE_ID, Collections.singleton(ITEM_SKU));

    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)));
    Mockito.verify(masterDataService)
      .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE));
    Mockito.verify(productService).getProductDeletedOrUndeleted(STORE_ID,
      item.getProductSku());
    Mockito.verify(objectConverterService)
      .overrideDefiningAttributeDetailsFromL3ToL4(Collections.singletonList(product), Collections.singletonList(item));
    Assertions.assertEquals(ITEM_SKU, listResponse.get(0).getItemSku());
    Assertions.assertEquals(IMAGE_URL, listResponse.get(0).getImagesResponseList().get(0).getLocationPath());
    assertTrue(listResponse.get(0).getImagesResponseList().get(0).isMainImage());
  }

  @Test
  public void getItemImagesListResponseItemAttributeDetailsProductNullTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "pcbGetMainImageDataDisabled", true);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "populateItemAttributeDetail", true);
    item.setMainImageUrl(null);
    item.setItemCode(ITEM_CODE);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_SKU);
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(false);
    List<MasterDataItemImage> masterDataItemImages = masterDataItemMap.get(ITEM_CODE).getMasterDataItemImages();
    masterDataItemImages = new ArrayList<>(masterDataItemImages);
    masterDataItemImages.add(masterDataItemImage);
    masterDataItemMap.get(ITEM_CODE).setMasterDataItemImages(masterDataItemImages);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_CODE);
    item.setPermanentDelete(true);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU))))
        .thenReturn(Collections.singletonList(item));
    when(this.masterDataService.getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE))).thenReturn(masterDataItemMap);
    Mockito.when(productService.getProductDeletedOrUndeleted(STORE_ID, item.getProductSku())).thenReturn(null)
        .thenReturn(product);

    List<ItemImagesListResponse> listResponse =
        itemSummaryFilterServiceImpl.getItemImagesListResponse(STORE_ID, Collections.singleton(ITEM_SKU));

    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU)));
    Mockito.verify(masterDataService)
        .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
            Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE));
    Mockito.verify(productService).getProductDeletedOrUndeleted(STORE_ID, item.getProductSku());
    Assertions.assertEquals(ITEM_SKU, listResponse.get(0).getItemSku());
    Assertions.assertEquals(IMAGE_URL, listResponse.get(0).getImagesResponseList().get(0).getLocationPath());
    assertTrue(listResponse.get(0).getImagesResponseList().get(0).isMainImage());
  }

  @Test
  public void getItemImagesListResponseItemAttributeDetailsTest2() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "pcbGetMainImageDataDisabled", true);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "populateItemAttributeDetail", true);
    item.setMainImageUrl(null);
    item.setItemCode(ITEM_CODE);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_SKU);
    product.setProductCode(PRODUCT_CODE);
    masterDataItemMap.values().forEach(masterDataItem -> masterDataItem.setProductCode(PRODUCT_CODE));
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(false);
    List<MasterDataItemImage> masterDataItemImages = masterDataItemMap.get(ITEM_CODE).getMasterDataItemImages();
    masterDataItemImages = new ArrayList<>(masterDataItemImages);
    masterDataItemImages.add(masterDataItemImage);
    item.setProductSku(product.getProductSku());
    masterDataItemMap.get(ITEM_CODE).setMasterDataItemImages(masterDataItemImages);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_CODE);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(ITEM_CODE);
    item.setDefiningAttributes(Collections.singletonList(productAttributeDetail));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU))))
        .thenReturn(Collections.singletonList(item));
    when(this.masterDataService.getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE))).thenReturn(masterDataItemMap);
    when(productService.getProductDeletedOrUndeleted(STORE_ID, PRODUCT_SKU)).thenReturn(product);

    List<ItemImagesListResponse> listResponse =
        itemSummaryFilterServiceImpl.getItemImagesListResponse(STORE_ID, Collections.singleton(ITEM_SKU));

    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)));
    Mockito.verify(masterDataService)
        .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
            Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE));
    Mockito.verify(productService).getProductDeletedOrUndeleted(STORE_ID,
      PRODUCT_SKU);
    Mockito.verify(objectConverterService)
        .overrideDefiningAttributeDetailsFromL3ToL4(Collections.singletonList(product), Collections.singletonList(item));
    Assertions.assertEquals(ITEM_SKU, listResponse.get(0).getItemSku());
    Assertions.assertEquals(IMAGE_URL, listResponse.get(0).getImagesResponseList().get(0).getLocationPath());
    assertTrue(listResponse.get(0).getImagesResponseList().get(0).isMainImage());
  }

  @Test
  public void getItemImagesListResponseMultipleMainImageTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "pcbGetMainImageDataDisabled", false);
    Mockito.when(itemService.findListOfItemCodesByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU)))
        .thenReturn(itemCodeAndSkuMap);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_SKU);
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(true);
    List<MasterDataItemImage> masterDataItemImages = masterDataItemMap.get(ITEM_CODE).getMasterDataItemImages();
    masterDataItemImages = new ArrayList<>(masterDataItemImages);
    masterDataItemImages.add(masterDataItemImage);
    masterDataItemMap.get(ITEM_CODE).setMasterDataItemImages(masterDataItemImages);
    when(this.masterDataService.getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE))).thenReturn(masterDataItemMap);
    List<ItemImagesListResponse> listResponse =
        itemSummaryFilterServiceImpl.getItemImagesListResponse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(itemService)
        .findListOfItemCodesByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(masterDataService)
        .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
            Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE));
    Assertions.assertEquals(1, listResponse.size());
    Assertions.assertEquals(1, listResponse.get(0).getImagesResponseList().size());
  }

  @Test
  public void getItemImagesListResponseEmptyTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "pcbGetMainImageDataDisabled", false);
    Mockito.when(itemService.findListOfItemCodesByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU)))
        .thenReturn(itemCodeAndSkuMap);
    masterDataItemMap.get(ITEM_CODE).setSkuCode(ITEM_SKU);
    masterDataItemMap.get(ITEM_CODE).getMasterDataItemImages().get(0).setMainImage(false);
    when(this.masterDataService.getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
        Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE))).thenReturn(masterDataItemMap);
    List<ItemImagesListResponse> listResponse =
        itemSummaryFilterServiceImpl.getItemImagesListResponse(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(itemService)
        .findListOfItemCodesByItemSkus(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(masterDataService)
        .getMasterDataItems(ItemSummaryServiceImplTest.STORE_ID, Constants.DEFAULT_USERNAME,
            Constants.DEFAULT_REQUEST_ID, Collections.singleton(ITEM_CODE));
    Assertions.assertNull(listResponse.get(0).getImagesResponseList());
  }

  @Test
  public void getItemSummaryByItemSkuBusinessPartnerPromoEmptyTest() throws Exception {
    when(productService
        .getProductAndSingleItemByItemSku(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null,
            false, true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    when(itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(null);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(TRUE);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH))
        .thenReturn(systemParameter);
    BusinessPartnerPromo businessPartnerPromo = new BusinessPartnerPromo();
    Set<String> activePromoBundlings = new HashSet<>();
    businessPartnerPromo.setActivePromoBundlings(activePromoBundlings);
    when(businessPartnerPromoService.findByBusinessPartnerCode(product.getMerchantCode()))
        .thenReturn(businessPartnerPromo);
    ItemSummaryPageResponseVo response =
        this.itemSummaryFilterServiceImpl.getItemSummaryByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, null);
    verify(this.productService)
        .getProductAndSingleItemByItemSku(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null,
            false, true, false);
    verify(itemService).isPriceEditDisabled(item);
    verify(itemPickupPointService).findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH);
    verify(businessPartnerPromoService).findByBusinessPartnerCode(product.getMerchantCode());
    verify(productService).setPriceAndItemViewConfigFromItemPickupPointForItemSummary(Mockito.anyString(),
        Mockito.any(ProductAndItemsVO.class));
    Assertions.assertEquals(ITEM_SKU, response.getItemSummaryResponses().get(0).getItemSku());
    assertTrue(response.getItemSummaryResponses().get(0).isPromoBundling());
  }

  @Test
  public void getItemSummaryByItemSkuSystemParameterFalseTest() throws Exception {
    when(productService
        .getProductAndSingleItemByItemSku(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null,
            false, true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    when(itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(null);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(FALSE);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH))
        .thenReturn(systemParameter);
    ItemSummaryPageResponseVo response =
        this.itemSummaryFilterServiceImpl.getItemSummaryByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, null);
    verify(this.productService)
        .getProductAndSingleItemByItemSku(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null,
            false, true, false);
    verify(itemService).isPriceEditDisabled(item);
    verify(itemPickupPointService).findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH);
    verify(productService).setPriceAndItemViewConfigFromItemPickupPointForItemSummary(Mockito.anyString(),
        Mockito.any(ProductAndItemsVO.class));
    Assertions.assertEquals(ITEM_SKU, response.getItemSummaryResponses().get(0).getItemSku());
    assertTrue(response.getItemSummaryResponses().get(0).isPromoBundling());
  }

  @Test
  public void getItemSummaryByItemSkuSystemParameterFalse_cncSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "cncForWarehouseFeatureSwitch", true);
    when(productService
        .getProductAndSingleItemByItemSku(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null,
            false, true, false)).thenReturn(new ProductAndItemsVO(product, Arrays.asList(item)));
    when(itemPickupPointService.findByStoreIdAndItemSkuAndCncBuyableTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(null);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(FALSE);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH))
        .thenReturn(systemParameter);
    ItemSummaryPageResponseVo response =
        this.itemSummaryFilterServiceImpl.getItemSummaryByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, null);
    verify(this.productService)
        .getProductAndSingleItemByItemSku(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, true, false, false, null,
            false, true, false);
    verify(itemService).isPriceEditDisabled(item);
    verify(itemPickupPointService).findByStoreIdAndItemSkuAndCncBuyableTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH);
    verify(productService).setPriceAndItemViewConfigFromItemPickupPointForItemSummary(Mockito.anyString(),
        Mockito.any(ProductAndItemsVO.class));
    Assertions.assertEquals(ITEM_SKU, response.getItemSummaryResponses().get(0).getItemSku());
    Assertions.assertTrue(response.getItemSummaryResponses().get(0).isPromoBundling());
  }

  @Test
  public void getItemSummaryByItemSkusList() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "fetchPcbMasterData", true);
    when(itemService.findListOfItemCodesByItemSkus(eq(STORE_ID), any())).thenReturn(itemCodeAndSkuMap);
    when(masterDataService.getMasterDataItems(STORE_ID, Constants.DEFAULT_USERNAME, Constants.DEFAULT_REQUEST_ID,
        itemCodeAndSkuMap.keySet())).thenReturn(new HashMap<>());
    when(
        productAndItemSolrRepository.getProductAndItemsByFilter(eq(STORE_ID), any(ItemSummaryRequestVO.class), eq(null),
            eq(null), any())).thenReturn(this.solrResult);
    when(itemPickupPointService.findByItemSkuInAndDelivery(eq(STORE_ID), Mockito.anyList(),
        eq(true))).thenReturn(itemPickupPointList);
    this.itemSummaryFilterServiceImpl.getItemSummaryByItemSkusList(STORE_ID, Arrays.asList(ITEM_SKU));
    verify(itemService).findListOfItemCodesByItemSkus(eq(STORE_ID), any());
    verify(masterDataService).getMasterDataItems(STORE_ID, Constants.DEFAULT_USERNAME, Constants.DEFAULT_REQUEST_ID,
        itemCodeAndSkuMap.keySet());
    verify(productAndItemSolrRepository).getProductAndItemsByFilter(eq(STORE_ID), any(ItemSummaryRequestVO.class),
        eq(null), eq(null), any());
    verify(itemPickupPointService).findByItemSkusAndDelivery(eq(STORE_ID), Mockito.anyList(), eq(true));
    verify(objectConverterService).constructItemSummaryListResponse(eq(this.solrResult.getContent()), any(), any());
  }

  @Test
  public void getItemSummaryByItemSkusFetchPcbMasterDataFalseList() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "fetchPcbMasterData", false);
    when(itemService.findByStoreIdAndItemSkus(eq(STORE_ID), any())).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuIn(eq(STORE_ID), any())).thenReturn(Arrays.asList(product));
    when(itemPickupPointService.findByItemSkuInAndDelivery(eq(STORE_ID), Mockito.anyList(),
        eq(true))).thenReturn(itemPickupPointList);
    this.itemSummaryFilterServiceImpl.getItemSummaryByItemSkusList(STORE_ID, Arrays.asList(ITEM_SKU));
    verify(itemService).findByStoreIdAndItemSkus(eq(STORE_ID), any());
    verify(productService).findByStoreIdAndProductSkuIn(eq(STORE_ID), any());
    verify(itemPickupPointService).findByItemSkusAndDelivery(eq(STORE_ID), Mockito.anyList(), eq(true));
    verify(objectConverterService).constructItemSummaryListResponseUsingNewMasterData(eq(Arrays.asList(item)), any(), any());
  }

  @Test
  public void getItemSummaryByItemSkusFetchPcbMasterDataFalseNullProductList() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "fetchPcbMasterData", false);
    when(itemService.findByStoreIdAndItemSkus(eq(STORE_ID), any())).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuIn(eq(STORE_ID), any())).thenReturn(null);
    when(itemPickupPointService.findByItemSkuInAndDelivery(eq(STORE_ID), Mockito.anyList(),
        eq(true))).thenReturn(null);
    this.itemSummaryFilterServiceImpl.getItemSummaryByItemSkusList(STORE_ID, Arrays.asList(ITEM_SKU));
    verify(itemService).findByStoreIdAndItemSkus(eq(STORE_ID), any());
    verify(productService).findByStoreIdAndProductSkuIn(eq(STORE_ID), any());
    verify(itemPickupPointService).findByItemSkusAndDelivery(eq(STORE_ID), Mockito.anyList(), eq(true));
    verify(objectConverterService).constructItemSummaryListResponseUsingNewMasterData(eq(Arrays.asList(item)), any(), any());
  }

  @Test
  public void updateWholeSaleActivationFlagTest() {
    when(itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        new HashSet<>(Arrays.asList(ITEM_SKU)))).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    this.itemSummaryFilterServiceImpl.updateWholeSaleActivationFlag(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(itemService).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        new HashSet<>(Arrays.asList(ITEM_SKU)));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
        Arrays.asList(itemPickupPoint));
  }

  @Test
  public void updateWholeSaleActivationFlagFalseTest() {
    when(itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        new HashSet<>(Arrays.asList(ITEM_SKU)))).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    this.itemSummaryFilterServiceImpl.updateWholeSaleActivationFlag(STORE_ID, Arrays.asList(ITEM_SKU), false);
    verify(itemService).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        new HashSet<>(Arrays.asList(ITEM_SKU)));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
        Arrays.asList(itemPickupPoint));
  }

  @Test
  public void updateWholeSaleActivationFlagFalseEmptyPromoBundlingTest() {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    when(itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        new HashSet<>(Arrays.asList(ITEM_SKU)))).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true)).thenReturn(
        Arrays.asList(itemPickupPoint));
    this.itemSummaryFilterServiceImpl.updateWholeSaleActivationFlag(STORE_ID, Arrays.asList(ITEM_SKU), false);
    verify(itemService).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        new HashSet<>(Arrays.asList(ITEM_SKU)));
    verify(itemPickupPointService).findByItemSkusAndDelivery(STORE_ID, Arrays.asList(ITEM_SKU), true);
    verify(this.objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
        Arrays.asList(itemPickupPoint));
  }

  @Test
  public void getItemSummaryByItemSkusEmptyResultList() throws Exception {
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl, "fetchPcbMasterData", true);
    when(this.solrResult.getContent()).thenReturn(new ArrayList<>());
    when(itemService.findListOfItemCodesByItemSkus(eq(STORE_ID), any())).thenReturn(itemCodeAndSkuMap);
    when(masterDataService.getMasterDataItems(STORE_ID, Constants.DEFAULT_USERNAME, Constants.DEFAULT_REQUEST_ID,
        itemCodeAndSkuMap.keySet())).thenReturn(new HashMap<>());
    when(
        productAndItemSolrRepository.getProductAndItemsByFilter(eq(STORE_ID), any(ItemSummaryRequestVO.class), eq(null),
            eq(null), any())).thenReturn(this.solrResult);
    this.itemSummaryFilterServiceImpl.getItemSummaryByItemSkusList(STORE_ID, Arrays.asList(ITEM_SKU));
    verify(itemService).findListOfItemCodesByItemSkus(eq(STORE_ID), any());
    verify(masterDataService).getMasterDataItems(STORE_ID, Constants.DEFAULT_USERNAME, Constants.DEFAULT_REQUEST_ID,
        itemCodeAndSkuMap.keySet());
    verify(productAndItemSolrRepository).getProductAndItemsByFilter(eq(STORE_ID), any(ItemSummaryRequestVO.class),
        eq(null), eq(null), any());
  }

  @Test
  public void getItemSummaryByFilterFbbActivatedTest() {
    itemSummaryRequestVo.setFbbActivated(true);
    solrResultElement.setMerchantPromoDiscountActivated(Boolean.FALSE);
    solrResultElement.setWholesalePriceActivated(Boolean.TRUE);
    solrResultElement.setProductScoreTotal(90.0);
    itemPickupPoint.setActivePromoBundlings(item.getActivePromoBundlings());
    when(
      this.productAndItemSolrRepository.getProductAndItemsByFilter(
        ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY,
        ItemSummaryServiceImplTest.PAGE_REQUEST)).thenReturn(this.solrResult);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(STORE_ID,
      Collections.singleton(ITEM_SKU), true)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint((Mockito.anyList()))).thenReturn(new HashMap<>());
    when(itemService.isPriceEditDisabled(Mockito.any(ItemPickupPoint.class))).thenReturn(false);
    ItemSummaryPageResponseVo response = this.itemSummaryFilterServiceImpl
      .getItemSummaryByFilter(ItemSummaryServiceImplTest.STORE_ID,
        ItemSummaryServiceImplTest.USERNAME, ItemSummaryServiceImplTest.REQUEST_ID,
        this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(this.productAndItemSolrRepository).getProductAndItemsByFilter(
      ItemSummaryServiceImplTest.STORE_ID, this.itemSummaryRequestVo, ORDER_BY, SORT_BY , ItemSummaryServiceImplTest.PAGE_REQUEST);
    verify(itemService).getItemPriceAndViewConfigs(STORE_ID, Collections.singletonList(ItemSummaryServiceImplTest.ITEM_SKU));
    verify(this.solrDataConstructor).constructMasterCatalog(
      ItemSummaryServiceImplTest.MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(
      ItemSummaryServiceImplTest.ITEM_IMAGES);
    verify(this.solrDataConstructor).constructSalesCatalogs(
      ItemSummaryServiceImplTest.SALES_CATALOG);
    verify(this.itemHelperService).convertOfflineItemPrices(isNull(), isNull(), any(ProductAndItemSolr.class));
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(STORE_ID, Collections.singleton(ITEM_SKU), true);
    verify(itemPriceService).getDiscountItemPickupPoint((Mockito.anyList()));
    verify(itemService).isPriceEditDisabled(Mockito.any(ItemPickupPoint.class));
    assertFalse(response.getItemSummaryResponses().get(0).isMerchantPromoDiscountActivated());
    Assertions.assertEquals(Collections.emptySet(), response.getItemSummaryResponses().get(0).getPriceEditDisabledReasons());
    assertTrue(response.getItemSummaryResponses().get(0).getWholesalePriceActivated());
    assertTrue(response.getItemSummaryResponses().get(0).getActivePromoBundlings()
      .contains(Constants.WHOLESALE_PRICE));
  }

  @Test
  public void getItemBasicDetailsByProductSkuTest() throws Exception {
    item.setMainImageUrl(IMAGE_URL);
    item.setItemCode("MTA-1234-5678");
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(Collections.singletonList(item));
    Mockito.when(itemService.getProductCodeAndSharedProductMap(Collections.singletonList(item)))
        .thenReturn(Map.of(PRODUCT_SKU, true));
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).getProductCodeAndSharedProductMap(Collections.singletonList(item));
    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Assertions.assertEquals(response.get(0).getItemSku(),item.getItemSku());
    Assertions.assertEquals(response.get(0).getProductSku(),item.getProductSku());
    Assertions.assertEquals(response.get(0).getGeneratedItemName(),item.getGeneratedItemName());
    Assertions.assertEquals(response.get(0).getMerchantCode(),item.getMerchantCode());
    Assertions.assertEquals(response.get(0).getItemCode(),item.getItemCode());
    Assertions.assertEquals(response.get(0).getMainImageUrl(), item.getMainImageUrl());
  }

  @Test
  public void getItemBasicDetailsByProductSkuTest2() throws Exception {
    item.setMainImageUrl(IMAGE_URL);
    item.setItemCode("MTA-1234-5678");
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(new ArrayList<>());
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void getItemBasicDetailsByProductSkuWithBundleRecipeTest() throws Exception {
    item.setMainImageUrl(IMAGE_URL);
    item.setBundleRecipe(new HashSet<>(List.of(bundleRecipe, bundleRecipe2)));
    item1.setItemSku(ITEM_SKU_1);
    item1.setMarkForDelete(true);
    item1.setProductSku(PRODUCT_SKU);
    item.setItemCode("MTA-1234-5678");
    item.setProductSku(PRODUCT_SKU);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(Collections.singletonList(item));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU, ITEM_SKU_1))))
        .thenReturn(List.of(item, item1));
    Mockito.when(itemService.getProductCodeAndSharedProductMap(Collections.singletonList(item)))
        .thenReturn(Map.of(PRODUCT_SKU, true));
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).getProductCodeAndSharedProductMap(Collections.singletonList(item));
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU, ITEM_SKU_1)));
    Assertions.assertEquals(response.get(0).getItemSku(),item.getItemSku());
    Assertions.assertEquals(response.get(0).getProductSku(),item.getProductSku());
    Assertions.assertEquals(response.get(0).getGeneratedItemName(),item.getGeneratedItemName());
    Assertions.assertEquals(response.get(0).getMerchantCode(),item.getMerchantCode());
    Assertions.assertEquals(response.get(0).getItemCode(),item.getItemCode());
    Assertions.assertEquals(response.get(0).getMainImageUrl(), item.getMainImageUrl());
    Assertions.assertEquals(2, response.get(0).getBundleRecipeList().size());
    Assertions.assertEquals(ProductStatus.ACTIVE.name(), response.get(0).getBundleRecipeList().get(0).getProductStatus());
    Assertions.assertEquals(ProductStatus.INACTIVE.name(), response.get(0).getBundleRecipeList().get(1).getProductStatus());
  }

  @Test
  public void getItemBasicDetailsByProductSkuWithBundleRecipeItemArchivedTest() throws Exception {
    item.setMainImageUrl(IMAGE_URL);
    item.setBundleRecipe(new HashSet<>(List.of(bundleRecipe, bundleRecipe2)));
    item.setProductSku(PRODUCT_SKU);
    item1.setItemSku(ITEM_SKU_1);
    item1.setArchived(true);
    item1.setProductSku(PRODUCT_SKU);
    item.setItemCode("MTA-1234-5678");
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(Collections.singletonList(item));
    Mockito.when(itemService.getProductCodeAndSharedProductMap(Collections.singletonList(item)))
        .thenReturn(Map.of(PRODUCT_SKU, true));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU, ITEM_SKU_1))))
        .thenReturn(List.of(item, item1));
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(cacheItemHelperService)
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).getProductCodeAndSharedProductMap(Collections.singletonList(item));
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU, ITEM_SKU_1)));
    Assertions.assertEquals(response.get(0).getItemSku(),item.getItemSku());
    Assertions.assertEquals(response.get(0).getProductSku(),item.getProductSku());
    Assertions.assertEquals(response.get(0).getGeneratedItemName(),item.getGeneratedItemName());
    Assertions.assertEquals(response.get(0).getMerchantCode(),item.getMerchantCode());
    Assertions.assertEquals(response.get(0).getItemCode(),item.getItemCode());
    Assertions.assertEquals(response.get(0).getMainImageUrl(), item.getMainImageUrl());
    Assertions.assertEquals(2, response.get(0).getBundleRecipeList().size());
    Assertions.assertEquals(ProductStatus.ACTIVE.name(), response.get(0).getBundleRecipeList().get(0).getProductStatus());
    Assertions.assertEquals(ProductStatus.INACTIVE.name(), response.get(0).getBundleRecipeList().get(1).getProductStatus());
  }

  @Test
  public void getBulkItemDetailsByItemSkusTest() throws Exception {
    item.setMainImageUrl(IMAGE_URL);
    item.setBundleRecipe(new HashSet<>(List.of(bundleRecipe, bundleRecipe2)));
    item1.setItemSku(ITEM_SKU_1);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU, ITEM_SKU_1))))
        .thenReturn(List.of(item, item1));
    Mockito.when(itemService.getProductCodeAndSharedProductMap(Mockito.anyList())).thenReturn(new HashMap<>());
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getBulkItemDetailsByItemSkus(STORE_ID, false, List.of(ITEM_SKU, ITEM_SKU_1));
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU, ITEM_SKU_1)));
    Mockito.verify(itemService).getProductCodeAndSharedProductMap(Mockito.anyList());
    Assertions.assertEquals(response.get(0).getItemSku(), item.getItemSku());
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getGeneratedItemName(), item.getGeneratedItemName());
    Assertions.assertEquals(response.get(0).getMerchantCode(), item.getMerchantCode());
    Assertions.assertEquals(response.get(0).getItemCode(), item.getItemCode());
    Assertions.assertEquals(response.get(0).getMainImageUrl(), item.getMainImageUrl());
    Assertions.assertEquals(0, response.get(0).getBundleRecipeList().size());
  }

  @Test
  public void getBulkItemDetailsByItemSkusWhenFetchBundleRecipeTrueTest() throws Exception {
    item.setMainImageUrl(IMAGE_URL);
    item.setBundleRecipe(new HashSet<>(List.of(bundleRecipe, bundleRecipe2)));
    item1.setItemSku(ITEM_SKU_1);
    item1.setMarkForDelete(true);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU, ITEM_SKU_1))))
        .thenReturn(List.of(item, item1));
    Mockito.when(itemService.getProductCodeAndSharedProductMap(Mockito.anyList())).thenReturn(new HashMap<>());
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getBulkItemDetailsByItemSkus(STORE_ID, true, List.of(ITEM_SKU, ITEM_SKU_1));
    verify(this.itemService, times(2)).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU, ITEM_SKU_1)));
    verify(itemService).getProductCodeAndSharedProductMap(Mockito.anyList());
    Assertions.assertEquals(response.get(0).getItemSku(), item.getItemSku());
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getGeneratedItemName(), item.getGeneratedItemName());
    Assertions.assertEquals(response.get(0).getMerchantCode(), item.getMerchantCode());
    Assertions.assertEquals(response.get(0).getItemCode(), item.getItemCode());
    Assertions.assertEquals(response.get(0).getMainImageUrl(), item.getMainImageUrl());
    Assertions.assertEquals(2, response.get(0).getBundleRecipeList().size());
    Assertions.assertEquals(ProductStatus.ACTIVE.name(), response.get(0).getBundleRecipeList().get(0).getProductStatus());
    Assertions.assertEquals(ProductStatus.INACTIVE.name(), response.get(0).getBundleRecipeList().get(1).getProductStatus());
  }

  @Test
  public void getItemBasicDetailsByItemCodesTest() throws Exception {
    List<String> itemCodes = new ArrayList<>();
    itemCodes.add(ITEM_CODE);
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    Set<String> itemCodesSet= itemCodes.stream().collect(Collectors.toSet());
    Mockito.when(itemHelperService.getBasicItemDetailsByItemCodes(STORE_ID, itemCodesSet))
        .thenReturn(Collections.singletonList(item));
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemCodes(STORE_ID, itemCodes);
    Mockito.verify(itemHelperService)
        .getBasicItemDetailsByItemCodes(STORE_ID, itemCodesSet);
    Assertions.assertEquals(response.get(0).getItemSku(),item.getItemSku());
    Assertions.assertEquals(response.get(0).getProductSku(),item.getProductSku());
    Assertions.assertEquals(response.get(0).getGeneratedItemName(),item.getGeneratedItemName());
    Assertions.assertEquals(response.get(0).getMerchantCode(),item.getMerchantCode());
    Assertions.assertEquals(response.get(0).getItemCode(),item.getItemCode());
    Assertions.assertEquals(response.get(0).getMainImageUrl(), item.getMainImageUrl());
    Assertions.assertEquals(response.get(0).getCategoryCode(), CATEGORY_CODE);
  }

  @Test
  public void getItemBasicDetailsByItemCodesEmptyItemCodesTest() throws Exception {
    List<String> itemCodes = new ArrayList<>();
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemCodes(STORE_ID, itemCodes);
    Assertions.assertEquals(response, new ArrayList<>());
  }

  @Test
  public void getItemBasicDetailsByItemSkusTest() throws Exception {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    item.setBrand(BRAND);
    items.add(item);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(items);
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, true, false, false,
            new ArrayList<>(itemSkus), true, true);
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus);
    Assertions.assertEquals(response.get(0).getItemSku(),item.getItemSku());
    Assertions.assertEquals(response.get(0).getProductSku(),item.getProductSku());
    Assertions.assertEquals(response.get(0).getGeneratedItemName(),item.getGeneratedItemName());
    Assertions.assertEquals(response.get(0).getMerchantCode(),item.getMerchantCode());
    Assertions.assertEquals(response.get(0).getItemCode(),item.getItemCode());
    Assertions.assertEquals(response.get(0).getMainImageUrl(), item.getMainImageUrl());
    Assertions.assertEquals(response.get(0).getCategoryCode(), CATEGORY_CODE);
    Assertions.assertEquals(BRAND, response.get(0).getBrand());
  }

  @Test
  public void getItemBasicDetailsByItemSkus_AllProductsFalseTest() throws Exception {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    item.setItemSku(ITEM_SKU);
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, false, false, false,
            new ArrayList<String>(itemSkus), true, true);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);

  }

  @Test
  public void getItemBasicDetailsByItemSkus_NullItemTest() throws Exception {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    item.setItemSku(ITEM_SKU);
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(null);
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, false, false, false,
            new ArrayList<String>(itemSkus), true, true);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);

  }

  @Test
  public void getItemBasicDetailsByItemSkus_NeedAllProductDataTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    List<Product> productList = new ArrayList<>();
    product.setProductCode(PRODUCT_CODE);
    product.setProductName(PRODUCT_NAME);
    product.setProductType(ProductType.REGULAR);
    productList.add(product);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    items.add(item);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(items);
    Mockito.when(productService.getAllProducts(STORE_ID, productSkus, false)).thenReturn(productList);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse=  new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode("HHH");
    attribute.setAttributeType("PREDEFINED_ATTRIBUTE");
    productAttributeResponse.setAttribute(attribute);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("value");
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse));
    productAndAttributeDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    Mockito.when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, true))
        .thenReturn(productAndAttributeDetailResponse);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"halalAttributesValues", Arrays.asList("HHH"));
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"storageAttributesValues", Arrays.asList("HHH"));
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, true, true, false,
            new ArrayList<String>(itemSkus), true, true);
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus);
    Mockito.verify(productService).getAllProducts(STORE_ID, productSkus, false);
   Mockito.verify( productCategoryBaseOutbound,Mockito.times(2)).getProductAndAttributeDetails(PRODUCT_CODE, true);
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getProductData().getProductCode(), product.getProductCode());
    Assertions.assertEquals(response.get(0).getProductData().getHalal(),"value");
  }

  @Test
  public void getItemBasicDetailsByItemSkus_NeedAllProductDataNullTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    List<Product> productList = new ArrayList<>();
    product.setProductCode(PRODUCT_CODE);
    product.setProductName(PRODUCT_NAME);
    product.setProductType(ProductType.REGULAR);
    productList.add(product);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    items.add(item);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(items);
    Mockito.when(productService.getAllProducts(STORE_ID, productSkus, false)).thenReturn(productList);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse=  new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode("HHH");
    attribute.setAttributeType("PREDEFINED_ATTRIBUTE");
    productAttributeResponse.setAttribute(attribute);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeResponse.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse));
    productAndAttributeDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    Mockito.when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, true))
        .thenReturn(productAndAttributeDetailResponse);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"halalAttributesValues", Arrays.asList("HHH"));
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"storageAttributesValues", Arrays.asList("HHH"));
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, true, true, false,
            new ArrayList<String>(itemSkus), true, true);
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus);
    Mockito.verify(productService).getAllProducts(STORE_ID, productSkus, false);
    Mockito.verify( productCategoryBaseOutbound,Mockito.times(2)).getProductAndAttributeDetails(PRODUCT_CODE, true);
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getProductData().getProductCode(), product.getProductCode());
  }

  @Test
  public void getItemBasicDetailsByItemSkus_NeedAllProductData_AttributeTypeDescriptiveTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    List<Product> productList = new ArrayList<>();
    product.setProductCode(PRODUCT_CODE);
    product.setProductName(PRODUCT_NAME);
    product.setProductType(ProductType.REGULAR);
    productList.add(product);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    items.add(item);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(items);
    Mockito.when(productService.getAllProducts(STORE_ID, productSkus, false)).thenReturn(productList);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse=  new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode("HHH");
    attribute.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    productAttributeResponse.setAttribute(attribute);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("value");
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse.setDescriptiveAttributeValue("value");
    productAttributeResponse.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse));
    productAndAttributeDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    Mockito.when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, true))
        .thenReturn(productAndAttributeDetailResponse);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"halalAttributesValues", Arrays.asList("HHH"));
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"storageAttributesValues", Arrays.asList("HHH"));
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, true, true, false,
            new ArrayList<String>(itemSkus), true, true);
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus);
    Mockito.verify(productService).getAllProducts(STORE_ID, productSkus, false);
    Mockito.verify( productCategoryBaseOutbound,Mockito.times(2)).getProductAndAttributeDetails(PRODUCT_CODE, true);
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getProductData().getProductCode(), product.getProductCode());
    Assertions.assertEquals(response.get(0).getProductData().getHalal(),"value");
  }

  @Test
  public void getItemBasicDetailsByItemSkus_NeedAllProductData_AttributeNullTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    List<Product> productList = new ArrayList<>();
    product.setProductCode(PRODUCT_CODE);
    product.setProductName(PRODUCT_NAME);
    product.setProductType(ProductType.REGULAR);
    productList.add(product);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    items.add(item);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(items);
    Mockito.when(productService.getAllProducts(STORE_ID, productSkus, false)).thenReturn(productList);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse=  new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode("HHH");
    productAttributeResponse.setAttribute(attribute);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("value");
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse.setDescriptiveAttributeValue("value");
    productAttributeResponse.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse));
    productAndAttributeDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    Mockito.when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, true))
        .thenReturn(productAndAttributeDetailResponse);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"halalAttributesValues", Arrays.asList("HHH"));
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"storageAttributesValues", Arrays.asList("HHH"));
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, true, true, false,
            new ArrayList<String>(itemSkus), true, true);
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus);
    Mockito.verify(productService).getAllProducts(STORE_ID, productSkus, false);
    Mockito.verify( productCategoryBaseOutbound,Mockito.times(2)).getProductAndAttributeDetails(PRODUCT_CODE, true);
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getProductData().getProductCode(), product.getProductCode());
    Assertions.assertNull(response.get(0).getProductData().getHalal());
  }

  @Test
  public void getItemBasicDetailsByItemSkus_NeedAllProductData_AttributeTypeNullTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    List<Product> productList = new ArrayList<>();
    product.setProductCode(PRODUCT_CODE);
    product.setProductName(PRODUCT_NAME);
    product.setProductType(ProductType.REGULAR);
    productList.add(product);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    item.setItemCode("MTA-1234-5678");
    items.add(item);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(items);
    Mockito.when(productService.getAllProducts(STORE_ID, productSkus, false)).thenReturn(productList);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse=  new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode("HHH");
    productAttributeResponse.setAttribute(null);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("value");
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse.setDescriptiveAttributeValue("value");
    productAttributeResponse.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse));
    productAndAttributeDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    Mockito.when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, true))
        .thenReturn(productAndAttributeDetailResponse);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"halalAttributesValues", Arrays.asList("HHH"));
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"storageAttributesValues", Arrays.asList("HHH"));
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, true, true, false,
            new ArrayList<String>(itemSkus), true, true);
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus);
    Mockito.verify(productService).getAllProducts(STORE_ID, productSkus, false);
    Mockito.verify( productCategoryBaseOutbound,Mockito.times(2)).getProductAndAttributeDetails(PRODUCT_CODE, true);
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getProductData().getProductCode(), product.getProductCode());
    Assertions.assertNull(response.get(0).getProductData().getHalal());
  }

  @Test
  public void getItemBasicDetailsByItemSkus_NeedAllProductData_AttributeNotPresentTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    List<Product> productList = new ArrayList<>();
    product.setProductCode(PRODUCT_CODE);
    product.setProductName(PRODUCT_NAME);
    product.setProductType(ProductType.REGULAR);
    productList.add(product);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    items.add(item);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(items);
    Mockito.when(productService.getAllProducts(STORE_ID, productSkus, false)).thenReturn(productList);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse=  new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode("HHHH");
    productAttributeResponse.setAttribute(attribute);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("value");
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse.setDescriptiveAttributeValue("value");
    productAttributeResponse.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse));
    productAndAttributeDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    Mockito.when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, true))
        .thenReturn(productAndAttributeDetailResponse);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"halalAttributesValues", Arrays.asList("HHH"));
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"storageAttributesValues", Arrays.asList("HHH"));
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, true, true, false,
            new ArrayList<String>(itemSkus), true, true);
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus);
    Mockito.verify(productService).getAllProducts(STORE_ID, productSkus, false);
    Mockito.verify( productCategoryBaseOutbound,Mockito.times(2)).getProductAndAttributeDetails(PRODUCT_CODE, true);
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getProductData().getProductCode(), product.getProductCode());
    Assertions.assertNull(response.get(0).getProductData().getHalal());
  }

  @Test
  public void getItemBasicDetailsByItemSkus_NeedProductDataTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    List<Product> productList = new ArrayList<>();
    product.setProductCode(PRODUCT_CODE);
    product.setProductName(PRODUCT_NAME);
    product.setProductType(ProductType.REGULAR);
    productList.add(product);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    item.setItemCode("MTA-1234-5678");
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(item);
    Mockito.when(productService.getProducts(STORE_ID, productSkus)).thenReturn(productList);
    Mockito.when(productCategoryBaseOutbound.getProductAndAttributeDetails(PRODUCT_CODE, false))
        .thenReturn(new ProductAndAttributeDetailResponse());
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"halalAttributesValues", Arrays.asList("HHH"));
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, false, true, false,
            new ArrayList<String>(itemSkus), true, true);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productService).getProducts(STORE_ID, productSkus);
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getProductData().getProductCode(), product.getProductCode());
    assertFalse(response.get(0).getProductData().isSuspended());
    Mockito.verify( productCategoryBaseOutbound,times(2)).getProductAndAttributeDetails(PRODUCT_CODE, false);
  }

  @Test
  public void getItemBasicDetailsByItemSkusAttributeAndFbbFlagFalseTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    List<Product> productList = new ArrayList<>();
    product.setProductCode(PRODUCT_CODE);
    product.setProductName(PRODUCT_NAME);
    product.setProductType(ProductType.REGULAR);
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    productAttributeDTO.setItemSku(ITEM_SKU);
    productAttribute.setItemSku(ITEM_SKU);
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDetailDTO.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetailDTO.setAttributeName(ATTRIBUTE_NAME);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeName(ATTRIBUTE_NAME);
    productAttribute.setProductAttributeDetails(Collections.singletonList(productAttributeDetail));
    product.setDefiningAttributes(Collections.singletonList(productAttribute));
    productList.add(product);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    item.setItemCode("MTA-1234-5678");
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(item);
    Mockito.when(productService.getProducts(STORE_ID, productSkus)).thenReturn(productList);
    Mockito.when(gdnMapper.deepCopy(productAttribute, ProductAttributeDTO.class)).thenReturn(productAttributeDTO);
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, false, true, false,
            new ArrayList<>(itemSkus), false, false);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productService).getProducts(STORE_ID, productSkus);
    Mockito.verify(gdnMapper).deepCopy(productAttribute, ProductAttributeDTO.class);
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getProductData().getProductCode(), product.getProductCode());
    assertFalse(response.get(0).getProductData().isSuspended());
    Assertions.assertEquals(ITEM_SKU, response.get(0).getProductData().getDefiningAttributes().get(0).getItemSku());
    Assertions.assertEquals(ATTRIBUTE_NAME,
        response.get(0).getProductData().getDefiningAttributes().get(0).getProductAttributeDetails().get(0)
            .getAttributeName());
    Assertions.assertEquals(ATTRIBUTE_CODE,
        response.get(0).getProductData().getDefiningAttributes().get(0).getProductAttributeDetails().get(0)
            .getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_VALUE,
        response.get(0).getProductData().getDefiningAttributes().get(0).getProductAttributeDetails().get(0)
            .getAttributeValue());
  }

  @Test
  public void getItemBasicDetailsByItemSkus_NeedProductDataWithPCBFailureTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    List<Product> productList = new ArrayList<>();
    product.setProductCode(PRODUCT_CODE);
    product.setProductName(PRODUCT_NAME);
    product.setProductType(ProductType.REGULAR);
    productList.add(product);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    item.setItemCode("MTA-1234-5678");
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(item);
    Mockito.when(productService.getProducts(STORE_ID, productSkus)).thenReturn(productList);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productCategoryBaseOutbound).getProductAndAttributeDetails(PRODUCT_CODE, false);
    ReflectionTestUtils.setField(itemSummaryFilterServiceImpl,"halalAttributesValues", Arrays.asList("HHH"));
    List<ItemBasicDetailV2Response> response =
      itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, false, true, false,
        new ArrayList<String>(itemSkus), true, true);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(productService).getProducts(STORE_ID, productSkus);
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getProductData().getProductCode(), product.getProductCode());
    assertFalse(response.get(0).getProductData().isSuspended());
    Assertions.assertNull(response.get(0).getProductData().getHalal());
    Mockito.verify( productCategoryBaseOutbound,times(2)).getProductAndAttributeDetails(PRODUCT_CODE, false);
  }

  @Test
  public void getItemBasicDetailsByItemSkus_NeedCategoryDataTest() throws Exception {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    List<CategoryResponse> categoryResponseList = new ArrayList<>();
    categoryResponseList.add(categoryResponse);
    Map<String, List<CategoryResponse>> categoryHierarchyMap = new HashMap<>();
    categoryHierarchyMap.put(CATEGORY_CODE, categoryResponseList);
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(CATEGORY_CODE);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    item.setItemCode(ITEM_CODE);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(IMAGE_URL);
    items.add(item);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(items);
    Mockito.when(cachedService.getParentCategoriesFromDbAndCache(GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), categoryCodes)).thenReturn(categoryHierarchyMap);
    List<ItemBasicDetailV2Response> response =
        itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, true, false, true,
            new ArrayList<String>(itemSkus), true, true);
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkus);
    Mockito.verify(cachedService).getParentCategoriesFromDbAndCache(GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), categoryCodes);
    Assertions.assertEquals(response.get(0).getProductSku(), item.getProductSku());
    Assertions.assertEquals(response.get(0).getCategoryHierarchy().get(0).getCategoryCode(), CATEGORY_CODE);
  }

  @Test
  public void getItemBasicDetailsByItemSkus_ItemSkuSizeTest() throws Exception {
    ReflectionTestUtils.setField(this.itemSummaryFilterServiceImpl, "getItemBasicDetailApiItemSkuListSize", 1);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    itemSkus.add(ITEM_SKU_1);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  itemSummaryFilterServiceImpl.getItemBasicDetailsByItemSkus(STORE_ID, true, false, true,
        new ArrayList<String>(itemSkus), true, true));
  }

  @Test
  public void testFetchBasicItemDetailsByItemCodes_ValidInput() {
    List<Item> items = Collections.singletonList(item);
    Page<Item> itemPage = new PageImpl<>(items);
    Mockito.when(itemRepositoryCustom.getItemSummaryResponsesByItemCodes(STORE_ID, ITEM_CODE,
        ITEM_NAME, 0, 10, SORT_BY, ORDER_BY))
      .thenReturn(itemPage);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
      .thenReturn(product);
    Page<ItemCodeBasicDetailResponse> result = itemSummaryFilterServiceImpl.fetchBasicItemDetailsByItemCodes(
      STORE_ID, ITEM_CODE, ITEM_NAME, 0, 10, SORT_BY, ORDER_BY);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(1, result.getTotalElements());
    Mockito.verify(itemRepositoryCustom).getItemSummaryResponsesByItemCodes(STORE_ID, ITEM_CODE,
      ITEM_NAME, 0, 10, SORT_BY, ORDER_BY);
    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testFetchBasicItemDetailsByItemCodes_NoProductsFound() {
    List<Item> items = Collections.singletonList(item);
    Page<Item> itemPage = new PageImpl<>(items);
    Mockito.when(itemRepositoryCustom.getItemSummaryResponsesByItemCodes(STORE_ID, ITEM_CODE,
        ITEM_NAME, 0, 10, SORT_BY, ORDER_BY))
      .thenReturn(itemPage);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
      .thenReturn(null);

    Page<ItemCodeBasicDetailResponse> result = itemSummaryFilterServiceImpl.fetchBasicItemDetailsByItemCodes(
      STORE_ID, ITEM_CODE, ITEM_NAME, 0, 10, SORT_BY, ORDER_BY);

    Assertions.assertNotNull(result);
    Assertions.assertEquals(1, result.getTotalElements());
    Mockito.verify(itemRepositoryCustom).getItemSummaryResponsesByItemCodes(STORE_ID, ITEM_CODE,
      ITEM_NAME, 0, 10, SORT_BY, ORDER_BY);
    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testFetchBasicItemDetailsByItemCodes_BlankStoreId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  itemSummaryFilterServiceImpl.fetchBasicItemDetailsByItemCodes("", ITEM_CODE, ITEM_NAME, 0, 10, SORT_BY,
      ORDER_BY));
  }

  @Test
  public void testFetchBasicItemDetailsByItemCodes_BlankItemCode() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  itemSummaryFilterServiceImpl.fetchBasicItemDetailsByItemCodes(STORE_ID, "", ITEM_NAME, 0, 10, SORT_BY,
      ORDER_BY));
  }

  @Test
  public void testFetchBasicItemDetailsByItemCodes_NoItemsFound() {
    Page<Item> emptyItemPage = new PageImpl<>(Collections.emptyList());
    Mockito.when(
      itemRepositoryCustom.getItemSummaryResponsesByItemCodes(STORE_ID, ITEM_CODE, ITEM_NAME, 0, 10,
        SORT_BY, ORDER_BY)).thenReturn(emptyItemPage);
    Page<ItemCodeBasicDetailResponse> responsePage =
      itemSummaryFilterServiceImpl.fetchBasicItemDetailsByItemCodes(STORE_ID, ITEM_CODE, ITEM_NAME,
        0, 10, SORT_BY, ORDER_BY);
    Mockito.verify(itemRepositoryCustom)
      .getItemSummaryResponsesByItemCodes(STORE_ID, ITEM_CODE, ITEM_NAME, 0, 10, SORT_BY, ORDER_BY);
    Assertions.assertEquals(0 ,responsePage.getTotalElements());
  }
}
