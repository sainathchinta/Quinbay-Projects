package com.gdn.x.product.service.impl;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.joda.time.DateTime;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.dao.solr.api.ProductSolrRepository;
import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.BusinessPartnerPromo;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductImage;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.ActiveProductDetailVo;
import com.gdn.x.product.model.vo.ActiveProductsRequestVO;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.HalalDashboardFilterRequestVo;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCategoryVO;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.MasterDataWithProductItemsVo;
import com.gdn.x.product.model.vo.OfficialStoreRequestVO;
import com.gdn.x.product.model.vo.PristineItemAndSiblingsVO;
import com.gdn.x.product.model.vo.PristineItemVO;
import com.gdn.x.product.model.vo.PristineMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.PristineProductAndItemsResponseVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductCountResponseVo;
import com.gdn.x.product.model.vo.ProductDetailVo;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductSummaryRequestV2Vo;
import com.gdn.x.product.model.vo.ProductsToItemCatalogMapping;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.ReviewProductDetailVO;
import com.gdn.x.product.model.vo.SimplePristineProductRequestVo;
import com.gdn.x.product.model.vo.SolrGroupResultVO;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.outbound.api.PromotionOutbound;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.service.api.BusinessPartnerPromoService;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.PristineCacheableService;
import com.gdn.x.product.service.api.ProductAndItemSolrConstructorService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.FormulaUtil;
import com.gdn.x.product.service.util.ProductAttributesUtil;
import com.gdn.x.promotion.enums.PromoBundlingType;
import com.gdn.x.promotion.rest.web.model.dto.request.SimpleSetStringRequest;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class ProductSearchServiceImplTest {

  private static final String PRISTINE_MASTER_ID = "pristineMasterId";

  private static final String ITEM_SKU = "itemSku";

  private static final String ITEM_SKU2 = "itemSku2";

  private static final String ITEM_SKU_OFFLINE_ITEM = "item-sku-offline-item";

  private static final int DELTA = 0;

  private static final String CATEGORY_CODE = "CATEGORY_CODE";

  private static final String SALES_CATEGORY_CODE = "SALES_CATEGORY_CODE";

  private static final String MASTER_CATALOG = "10001#_#mastercatalog";

  private static final Pageable GENERATE_PAGEABLE = PageRequest.of(0, 10);

  private static final String PRODUCT_CODE = "product-code";

  private static final String PRODUCT_SKU = "product-sku";

  private static final String PRODUCT_SKU_2 = "product-sku-2";

  private static final String PRODUCT_SKU_3 = "product-sku-3";

  private static final String PRODUCT_SKU_4 = "product-sku-4";

  private static final String PRODUCT_SKU_5 = "product-sku-5";

  private static final String PRODUCT_SKU_6 = "product-sku-6";

  private static final String PRODUCT_SKU_7 = "product-sku-7";

  private static final String REQUESTID = "requestid";

  private static final String USERNAME = "username";

  private static final String STORE_ID = "store-id";

  private static final String CHANNEL_ID = "dummy-channel-id";

  private static final String CLIENT_ID = "dummy-client-id";

  private static final String PRODUCT_CATENTRY_ID = "product-catentry-id";

  private static final DateTime CREATED_DATE = new DateTime();

  private static final int YEAR = 0;

  private static final int MONTH = 1;

  private static final String PRISTINE_ID = "pristineId";

  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  private static final String PRISTINE_CATEGORY = "HANDPHONE";

  private static final String PROMO_BUNDLING_NAME = "promoBundlingTest";

  private static final String PROMO_BUNDLING_ID = "promoBundlingId";

  private static final String PROMO_BUNDLING_TYPE = "COMBO";

  private static final String PRODUCT_NAME = "product-name";

  private static final String MERCHANT_CODE = "dummy-merchant-code";

  private static final String BRAND = "dummy-brand";

  private static final String ITEM_IMAGE = "true#_#dummy-item-image";

  private static final String ITEM_IMAGE_1 = "false#_#dummy-item-image";

  private static final String ITEM_PRICE = "DEFAULT#_#23000";

  private static final String ITEM_PRICE_1 = "DEFAULT#_#25000";

  private static final String URL = "url";

  private static final String KEY = "key";

  private static final String STATUS = "All";

  private static final String DUMMY_NAME = "dummy-item-image";

  private static final String PRISTINE_LISTING_ATTRIBUTE =
      "{\"HANDPHONE\" : \"color,rom\", \"COMPUTER\" : \"color\"}";

  private static final String PRISTINE_SIMILAR_ATTRIBUTE =
      "{\"HANDPHONE\" : \"\", \"COMPUTER\" : \"screensize,processorname,ram,ssd/hdd\"}";

  private static final String PRISTINE_ATTRIBUTE_VALUE_TRANSLATION =
      "{\"color\":\"Warna\", \"rom\":\"Kapasitas Memori\"}";

  private static final String PRODUCT_ATTRIBUTE_VALUE = "dummy-product-attribute-value";

  private static final String IMAGE_LOCATION_PATH = "dummy-image-path";
  private static final String SOURCE_ITEM_CODE = "sourceItemCode";

  private static final String ITEM_CODE = "itemCode";

  private static final String SELLER_PROMO_BUNDLINGS_SWITCH = "sellerPromoBundlingsSwitch";

  private static final String  TRUE = "true";

  private static final String FALSE = "false";

  private static final String ACTIVE_PROMO_BUNDLING_TEST = "ACTIVE_PROMO_BUNDLING_TEST";

  Map<String, String> similarParameters = new HashMap<>();
  Map<String, String> listingParameters = new HashMap<>();
  Map<String, String> attributeNameTranslationMap = new HashMap<>();
  private SystemParameter systemParameter = new SystemParameter();
  private Set<String> itemSkuSet = Collections.singleton(ITEM_SKU);
  private List<String> productSkuSet = Collections.singletonList(PRODUCT_SKU);
  private ItemCatalogVO itemCatalogVO = new ItemCatalogVO();


  @InjectMocks
  private ProductSearchServiceImpl searchServiceImpl;

  @Mock
  private ProductService productService;

  @Mock
  private ProductAndItemSolrRepository solrRepository;

  @Mock
  private ProductSearchHelperService productSearchHelper;

  @Mock
  private ProductCacheableService productCacheHelperService;

  @Mock
  private Page<ProductAndItemSolr> solrPage;

  @Mock
  private ProductAndItemSolrConstructorService productAndItemSolrConstructorService;

  @Mock
  private CatalogService catalogService;

  @Mock
  private FormulaUtil formulaUtil;

  @Mock
  private ItemService itemService;

  @Mock
  private PromotionOutbound promotionOutbound;

  @Mock
  private GdnMapper gdnMapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private PristineItemRepository pristineItemRepository;

  @Mock
  private BusinessPartnerPromoService businessPartnerPromoService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ItemCacheableService itemCacheableService;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private PristineCacheableService pristineCacheableService;

  @Mock
  private ItemPriceService itemPriceService;

  @Mock
  private OfflineItemService offlineItemService;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private ProductAttributesUtil productAttributesUtil;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private MasterDataConstructorService masterDataConstructorService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ProductSolrRepository productSolrRepository;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  private PromoBundlingDetailResponseVO promoBundlingDetailResponse;

  private List<PromoBundlingDetailResponseVO> promoBundlingDetailResponseVOs;

  private ActivePromoBundlingResponseVO activePromoBundlingResponseVO;

  private WholesaleRuleVO wholesaleRuleVO;

  private List<WholesaleRuleVO> wholesaleRuleVOS;

  private OfflineItem offlineItem;
  private ItemPickupPoint itemPickupPoint;

  private MasterDataProductImage masterDataProductImage;

  private List<MasterDataProductImage> masterDataProductImages;

  private Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse;
  private OfficialStoreRequestVO officialStoreRequestVO = new OfficialStoreRequestVO();
  private ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
  private Item item;
  private MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo =
      new MasterDataDetailWithProductAndItemsResponseVo();
  private ProductAndItemsVO productAndItemsVO;
  private MasterDataProduct masterDataProduct = new MasterDataProduct();
  private Product product = new Product();

  @Test
  public void getAllProductAndItemsSortByProductCodeAscTest() throws Exception {
    List<ProductAndItemSolr> solrResult = Arrays.asList(new ProductAndItemSolr());
    when(this.solrPage.getContent()).thenReturn(solrResult);
    when(
        this.solrRepository.getListOfActiveProductSkusSortedByProductCode(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.GENERATE_PAGEABLE))
        .thenReturn(this.solrPage);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetailForReindexBySolrResult(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, solrResult)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getAllProductAndItemsSortByProductCodeAsc(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, true,
        ProductSearchServiceImplTest.GENERATE_PAGEABLE);
    verify(this.solrRepository).getListOfActiveProductSkusSortedByProductCode(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.GENERATE_PAGEABLE);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailForReindexBySolrResult(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, solrResult);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsInfoForActiveItemTest() throws Exception {
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(new Product(), Stream.of(new Item()).collect(toList()));
    Set<String> itemSkus = Stream.of(ITEM_SKU).collect(toSet());
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUESTID, USERNAME, USERNAME);
    when(productService.getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUESTID, USERNAME, itemSkus, true, true, false, true))
        .thenReturn(Stream.of(productAndItemsVO).collect(toList()));
    searchServiceImpl.getProductAndItemsInfoForActiveItem(mandatoryRequestParam,
        itemSkus, true, true, Boolean.FALSE, true, false);
    verify(productService).getProductAndItemsByItemSkusForActiveItems(STORE_ID, REQUESTID, USERNAME, itemSkus,
        true, true, false, true);
    Mockito.verify(this.productService).checkProductAndItemsForForceReview(
      Collections.singletonList(productAndItemsVO.getProduct()), productAndItemsVO.getItems());
    Mockito.verify(this.productService).setSellerPromoBundlings(eq(STORE_ID),
      Mockito.anyList());
  }

  @Test
  public void getProductAndItemsInfoForActiveItemNullBrandTest() throws Exception {
    Set<String> activePromoBundlingList = new HashSet<>();
    activePromoBundlingList.add("WHOLESALE");

    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributes = new LinkedHashMap<>();
    attributes.put("Warna", "BLUE");
    attributes.put("Kapasitas Memori", "8GB");


    Product product = new Product();
    Item item = new Item();
    Map<String, String> pristineListingAttributes = new LinkedHashMap<>();
    pristineListingAttributes.put("color", "BLUE");
    pristineListingAttributes.put("rom", "8GB");
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(ProductSearchServiceImplTest.PRISTINE_ID);
    pristineDataItem.setPristineMasterId(ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory(PRISTINE_CATEGORY);
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);
    pristineDataItem.setPristineBrand(BRAND);
    pristineDataItem.setPristineModel(PRISTINE_MASTER_ID);
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku(ITEM_SKU);
    item.setPromoBundling(Boolean.TRUE);
    item.setActivePromoBundlings(activePromoBundlingList);

    Calendar startDate = Calendar.getInstance();
    startDate.add(Calendar.DATE, -7);
    Calendar endDate = Calendar.getInstance();
    endDate.add(Calendar.DAY_OF_MONTH, 5);

    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(5000.0);

    List<DiscountPrice> discountPrices = new ArrayList<>();
    discountPrices.add(discountPrice);

    Price price = new Price();
    price.setChannel("web");
    price.setOfferPrice(100000.0);
    price.setListPrice(10000.0);
    price.setListOfDiscountPrices(discountPrices);
    price.setCurrency("currency");

    Set<Price> prices = new HashSet<>();
    prices.add(price);

    item.setPrice(prices);
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(product, Stream.of(item).collect(toList()));
    Set<String> itemSkus = Stream.of(ProductSearchServiceImplTest.ITEM_SKU).collect(toSet());
    ProductAttribute productAttribute = new ProductAttribute();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    pristineItemAndSiblingsVO.setSiblings(Arrays.asList(pristineDataItem));

    Map<String,String> expectedListingAttributes = new HashMap<>();
    expectedListingAttributes.put("Kapasitas Memori", "8GB");
    expectedListingAttributes.put("Warna", "BLUE");

    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUESTID, USERNAME, USERNAME);
    SimpleSetStringRequest simpleSetStringRequest = new SimpleSetStringRequest();
    simpleSetStringRequest.setValue(itemSkus);

    when(productAttributesUtil.getCategoryListingParameterKey(eq(item.getPristineDataItem())))
        .thenReturn(categoryListingParameters);
    when(productAttributesUtil.translatePristineListingAttributeName(
        eq(item.getPristineDataItem().getPristineListingAttributes()),
        Mockito.any(String[].class))).thenReturn(attributes);

    when(productService.getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true)).thenReturn(Stream.of(productAndItemsVO).collect(toList()));
    when(this.itemPriceService.getFinalPrice(price, 10.0)).thenReturn(5000.0);
    when(pristineCacheableService.
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    when(objectConverterService.convertPristineAttributeToProductAttribute(pristineDataItem))
        .thenReturn(productAttribute);
    when(promotionOutbound.getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        itemSkus)).thenReturn(activePromoBundlingResponseVO);
    Item item1 = new Item();
    item1.setPristineDataItem(pristineDataItem);

    List<ProductItemsVo> result =
      searchServiceImpl.getProductAndItemsInfoForActiveItem(mandatoryRequestParam,
        itemSkus, true, true, Boolean.TRUE, true, false);
    assertEquals(result.get(0).getItemVoList().get(0).getWholesaleRules().get(0), wholesaleRuleVO);
    assertEquals(result.get(0).getItemVoList().get(0).getPristineDataItem()
        .getPristineListingAttributes(), expectedListingAttributes);

    verify(productService).getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true);
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(item.getPristineDataItem()));
    verify(productAttributesUtil).translatePristineListingAttributeName(
        eq(pristineListingAttributes), Mockito.any(String[].class));
    verify(this.itemPriceService).getFinalPrice(price, 10.0);
    verify(pristineCacheableService).
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId());
    verify(objectConverterService).convertPristineAttributeToProductAttribute(pristineDataItem);
    verify(promotionOutbound)
        .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
            itemSkus);
    Mockito.verify(this.productService).checkProductAndItemsForForceReview(
      Collections.singletonList(productAndItemsVO.getProduct()), productAndItemsVO.getItems());
    Mockito.verify(this.productService).setSellerPromoBundlings(eq(STORE_ID),
            Mockito.anyList());
  }

  @Test
  public void getProductAndItemsInfoForActiveItemWithPristineTest() throws Exception {
    Set<String> activePromoBundlingList = new HashSet<>();
    activePromoBundlingList.add("WHOLESALE");

    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributes = new LinkedHashMap<>();
    attributes.put("Warna", "BLUE");
    attributes.put("Kapasitas Memori", "8GB");


    Product product = new Product();
    Item item = new Item();
    Map<String, String> pristineListingAttributes = new LinkedHashMap<>();
    pristineListingAttributes.put("color", "BLUE");
    pristineListingAttributes.put("rom", "8GB");
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(ProductSearchServiceImplTest.PRISTINE_ID);
    pristineDataItem.setPristineMasterId(ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory(PRISTINE_CATEGORY);
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku(ITEM_SKU);
    item.setPromoBundling(Boolean.TRUE);
    item.setActivePromoBundlings(activePromoBundlingList);

    Calendar startDate = Calendar.getInstance();
    startDate.add(Calendar.DATE, -7);
    Calendar endDate = Calendar.getInstance();
    endDate.add(Calendar.DAY_OF_MONTH, 5);

    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(5000.0);

    List<DiscountPrice> discountPrices = new ArrayList<>();
    discountPrices.add(discountPrice);

    Price price = new Price();
    price.setChannel("web");
    price.setOfferPrice(100000.0);
    price.setListPrice(10000.0);
    price.setListOfDiscountPrices(discountPrices);
    price.setCurrency("currency");

    Set<Price> prices = new HashSet<>();
    prices.add(price);

    item.setPrice(prices);
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(product, Stream.of(item).collect(toList()));
    Set<String> itemSkus = Stream.of(ProductSearchServiceImplTest.ITEM_SKU).collect(toSet());
    ProductAttribute productAttribute = new ProductAttribute();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    pristineItemAndSiblingsVO.setSiblings(Arrays.asList(pristineDataItem));

    Map<String,String> expectedListingAttributes = new HashMap<>();
    expectedListingAttributes.put("Kapasitas Memori", "8GB");
    expectedListingAttributes.put("Warna", "BLUE");

    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUESTID, USERNAME, USERNAME);
    SimpleSetStringRequest simpleSetStringRequest = new SimpleSetStringRequest();
    simpleSetStringRequest.setValue(itemSkus);

    when(productAttributesUtil.getCategoryListingParameterKey(eq(item.getPristineDataItem())))
        .thenReturn(categoryListingParameters);
    when(productAttributesUtil.translatePristineListingAttributeName(
        eq(item.getPristineDataItem().getPristineListingAttributes()),
        Mockito.any(String[].class))).thenReturn(attributes);

    when(productService.getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true)).thenReturn(Stream.of(productAndItemsVO).collect(toList()));
    when(this.itemPriceService.getFinalPrice(price, 10.0)).thenReturn(5000.0);
    when(pristineCacheableService.
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    when(objectConverterService.convertPristineAttributeToProductAttribute(pristineDataItem))
        .thenReturn(productAttribute);
    when(promotionOutbound.getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        itemSkus)).thenReturn(activePromoBundlingResponseVO);
    Item item1 = new Item();
    item1.setPristineDataItem(pristineDataItem);
    when(pristineCacheableService.findFirstItemByPristine(Mockito.any(PristineDataItem.class))).thenReturn(new Item());
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(Mockito.any(Item.class))).thenReturn(item1);

    List<ProductItemsVo> result =
      searchServiceImpl.getProductAndItemsInfoForActiveItem(mandatoryRequestParam,
        itemSkus, true, true, Boolean.TRUE, true, false);
    assertEquals(result.get(0).getItemVoList().get(0).getWholesaleRules().get(0), wholesaleRuleVO);
    assertEquals(result.get(0).getItemVoList().get(0).getPristineDataItem()
        .getPristineListingAttributes(), expectedListingAttributes);

    verify(productService).getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true);
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(item.getPristineDataItem()));
    verify(productAttributesUtil).translatePristineListingAttributeName(
        eq(pristineListingAttributes), Mockito.any(String[].class));
    verify(this.itemPriceService).getFinalPrice(price, 10.0);
    verify(pristineCacheableService).
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId());
    verify(objectConverterService).convertPristineAttributeToProductAttribute(pristineDataItem);
    verify(promotionOutbound)
        .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
            itemSkus);
    verify(pristineCacheableService).findFirstItemByPristine(Mockito.any(PristineDataItem.class));
    verify(masterDataConstructorService, Mockito.times(2)).constructPristineDataItemWithMasterData(Mockito.any(Item.class));
    Mockito.verify(this.productService).checkProductAndItemsForForceReview(
      Collections.singletonList(productAndItemsVO.getProduct()), productAndItemsVO.getItems());
    Mockito.verify(this.productService).setSellerPromoBundlings(eq(STORE_ID),
            Mockito.anyList());
  }

  @Test
  public void getProductAndItemsInfoForActiveItemWholesalePriceTest() throws Exception {
    Set<String> activePromoBundlingList = new HashSet<>();
    activePromoBundlingList.add(Constants.WHOLESALE_PRICE);

    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributes = new LinkedHashMap<>();
    attributes.put("Warna", "BLUE");
    attributes.put("Kapasitas Memori", "8GB");


    Product product = new Product();
    Item item = new Item();
    Map<String, String> pristineListingAttributes = new LinkedHashMap<>();
    pristineListingAttributes.put("color", "BLUE");
    pristineListingAttributes.put("rom", "8GB");
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(ProductSearchServiceImplTest.PRISTINE_ID);
    pristineDataItem.setPristineMasterId(ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory(PRISTINE_CATEGORY);
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku(ITEM_SKU);
    item.setPromoBundling(Boolean.TRUE);
    item.setActivePromoBundlings(activePromoBundlingList);

    Calendar startDate = Calendar.getInstance();
    startDate.add(Calendar.DATE, -7);
    Calendar endDate = Calendar.getInstance();
    endDate.add(Calendar.DAY_OF_MONTH, 5);

    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(5000.0);

    List<DiscountPrice> discountPrices = new ArrayList<>();
    discountPrices.add(discountPrice);

    Price price = new Price();
    price.setChannel("web");
    price.setOfferPrice(100000.0);
    price.setListPrice(10000.0);
    price.setListOfDiscountPrices(discountPrices);
    price.setCurrency("currency");

    Set<Price> prices = new HashSet<>();
    prices.add(price);

    item.setPrice(prices);
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(product, Stream.of(item).collect(toList()));
    Set<String> itemSkus = Stream.of(ProductSearchServiceImplTest.ITEM_SKU).collect(toSet());
    ProductAttribute productAttribute = new ProductAttribute();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    pristineDataItem.setPristineModel("Model");
    pristineDataItem.setPristineBrand("Brand");
    pristineItemAndSiblingsVO.setSiblings(Arrays.asList(pristineDataItem));

    Map<String,String> expectedListingAttributes = new HashMap<>();
    expectedListingAttributes.put("Kapasitas Memori", "8GB");
    expectedListingAttributes.put("Warna", "BLUE");

    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUESTID, USERNAME, USERNAME);
    SimpleSetStringRequest simpleSetStringRequest = new SimpleSetStringRequest();
    simpleSetStringRequest.setValue(itemSkus);

    when(productAttributesUtil.getCategoryListingParameterKey(eq(item.getPristineDataItem())))
        .thenReturn(categoryListingParameters);
    when(productAttributesUtil.translatePristineListingAttributeName(
        eq(item.getPristineDataItem().getPristineListingAttributes()),
        Mockito.any(String[].class))).thenReturn(attributes);

    when(productService.getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true)).thenReturn(Stream.of(productAndItemsVO).collect(toList()));
    when(this.itemPriceService.getFinalPrice(price, 10.0)).thenReturn(5000.0);
    when(pristineCacheableService.
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    when(objectConverterService.convertPristineAttributeToProductAttribute(pristineDataItem))
        .thenReturn(productAttribute);
    when(promotionOutbound.getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        itemSkus)).thenReturn(activePromoBundlingResponseVO);

    List<ProductItemsVo> result =
      searchServiceImpl.getProductAndItemsInfoForActiveItem(mandatoryRequestParam,
        itemSkus, true, true, Boolean.TRUE, true, false);
    assertEquals(result.get(0).getItemVoList().get(0).getWholesaleRules().get(0), wholesaleRuleVO);
    assertEquals(result.get(0).getItemVoList().get(0).getPristineDataItem()
        .getPristineListingAttributes(), expectedListingAttributes);

    verify(productService).getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true);
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(item.getPristineDataItem()));
    verify(productAttributesUtil).translatePristineListingAttributeName(
        eq(pristineListingAttributes), Mockito.any(String[].class));
    verify(this.itemPriceService).getFinalPrice(price, 10.0);
    verify(pristineCacheableService).
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId());
    verify(objectConverterService).convertPristineAttributeToProductAttribute(pristineDataItem);
    verify(promotionOutbound)
        .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
            itemSkus);
    Mockito.verify(this.productService).checkProductAndItemsForForceReview(
      Collections.singletonList(productAndItemsVO.getProduct()), productAndItemsVO.getItems());
    Mockito.verify(this.productService).setSellerPromoBundlings(eq(STORE_ID),
      Mockito.anyList());
  }

  @Test
  public void getProductAndItemsInfoForActiveItemComboTest() throws Exception {
    Set<String> activePromoBundlingList = new HashSet<>();
    activePromoBundlingList.add(Constants.COMBO);

    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributes = new LinkedHashMap<>();
    attributes.put("Warna", "BLUE");
    attributes.put("Kapasitas Memori", "8GB");


    Product product = new Product();
    Item item = new Item();
    Map<String, String> pristineListingAttributes = new LinkedHashMap<>();
    pristineListingAttributes.put("color", "BLUE");
    pristineListingAttributes.put("rom", "8GB");
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(ProductSearchServiceImplTest.PRISTINE_ID);
    pristineDataItem.setPristineMasterId(ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory(PRISTINE_CATEGORY);
    pristineDataItem.setPristineBrand("Brand");
    pristineDataItem.setPristineModel("Model");
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku(ITEM_SKU);
    item.setPromoBundling(Boolean.TRUE);
    item.setActivePromoBundlings(activePromoBundlingList);

    Calendar startDate = Calendar.getInstance();
    startDate.add(Calendar.DATE, -7);
    Calendar endDate = Calendar.getInstance();
    endDate.add(Calendar.DAY_OF_MONTH, 5);

    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(5000.0);

    List<DiscountPrice> discountPrices = new ArrayList<>();
    discountPrices.add(discountPrice);

    Price price = new Price();
    price.setChannel("web");
    price.setOfferPrice(100000.0);
    price.setListPrice(10000.0);
    price.setListOfDiscountPrices(discountPrices);
    price.setCurrency("currency");

    Set<Price> prices = new HashSet<>();
    prices.add(price);

    item.setPrice(prices);
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(product, Stream.of(item).collect(toList()));
    Set<String> itemSkus = Stream.of(ProductSearchServiceImplTest.ITEM_SKU).collect(toSet());
    ProductAttribute productAttribute = new ProductAttribute();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    pristineItemAndSiblingsVO.setSiblings(Arrays.asList(pristineDataItem));

    Map<String,String> expectedListingAttributes = new HashMap<>();
    expectedListingAttributes.put("Kapasitas Memori", "8GB");
    expectedListingAttributes.put("Warna", "BLUE");

    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUESTID, USERNAME, USERNAME);
    SimpleSetStringRequest simpleSetStringRequest = new SimpleSetStringRequest();
    simpleSetStringRequest.setValue(itemSkus);

    when(productAttributesUtil.getCategoryListingParameterKey(eq(item.getPristineDataItem())))
        .thenReturn(categoryListingParameters);
    when(productAttributesUtil.translatePristineListingAttributeName(
        eq(item.getPristineDataItem().getPristineListingAttributes()),
        Mockito.any(String[].class))).thenReturn(attributes);

    when(productService.getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true)).thenReturn(Stream.of(productAndItemsVO).collect(toList()));
    when(this.itemPriceService.getFinalPrice(price, 10.0)).thenReturn(5000.0);
    when(pristineCacheableService.
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    when(objectConverterService.convertPristineAttributeToProductAttribute(pristineDataItem))
        .thenReturn(productAttribute);
    List<ProductItemsVo> result =
      searchServiceImpl.getProductAndItemsInfoForActiveItem(mandatoryRequestParam,
        itemSkus, true, true, Boolean.TRUE, true, false);
    assertEquals(result.get(0).getItemVoList().get(0).getPristineDataItem()
        .getPristineListingAttributes(), expectedListingAttributes);

    verify(productService).getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true);
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(item.getPristineDataItem()));
    verify(productAttributesUtil).translatePristineListingAttributeName(
        eq(pristineListingAttributes), Mockito.any(String[].class));
    verify(pristineCacheableService).
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId());
    verify(objectConverterService).convertPristineAttributeToProductAttribute(pristineDataItem);
    Mockito.verify(this.productService).checkProductAndItemsForForceReview(
      Collections.singletonList(productAndItemsVO.getProduct()), productAndItemsVO.getItems());
    Mockito.verify(this.productService).setSellerPromoBundlings(eq(STORE_ID),
            Mockito.anyList());
  }

  @Test
  public void getProductAndItemsInfoForActiveItemWithPristine_WhenPristineDataItemNullTest() throws Exception {
    Set<String> activePromoBundlingList = new HashSet<>();
    Product product = new Product();
    Item item = new Item();
    item.setPristineDataItem(null);
    item.setActivePromoBundlings(activePromoBundlingList);

    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(product, Stream.of(item).collect(toList()));
    Set<String> itemSkus = Stream.of(ProductSearchServiceImplTest.ITEM_SKU).collect(toSet());
    when(productService.getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true)).thenReturn(Stream.of(productAndItemsVO).collect(toList()));

    searchServiceImpl.getProductAndItemsInfoForActiveItem(MandatoryRequestParam
            .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUESTID, USERNAME, USERNAME), itemSkus,
        true,true, true, true, false);

    verify(productService).getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true);
    Mockito.verify(this.productService).checkProductAndItemsForForceReview(
      Collections.singletonList(productAndItemsVO.getProduct()), productAndItemsVO.getItems());
    Mockito.verify(this.productService).setSellerPromoBundlings(eq(STORE_ID),
            Mockito.anyList());
  }


  @Test
  public void getProductAndItemsByProductCatentryIdBlankProductCatentryId() throws Exception {
    Assertions.assertThrows(Exception.class, () ->  this.searchServiceImpl.getProductAndItemsByProductCatentryId(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, ""));
  }

  @Test
  public void getProductAndItemsByProductCatentryIdBlankStoreId() throws Exception {
    Assertions.assertThrows(Exception.class, () -> this.searchServiceImpl.getProductAndItemsByProductCatentryId("",
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRODUCT_CATENTRY_ID));
  }

  @Test
  public void getProductAndItemsByProductCatentryIdsTest() throws Exception {
    HashSet<String> productCatentryIds = new HashSet<String>();
    productCatentryIds.add(ProductSearchServiceImplTest.PRODUCT_CATENTRY_ID);
    List<Product> productResult = Arrays.asList(new Product());
    when(
        this.productService.getProductsByProductCatentryIds(ProductSearchServiceImplTest.STORE_ID,
            productCatentryIds)).thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getProductAndItemsByProductCatentryIds(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productCatentryIds, true);
    verify(this.productSearchHelper).validateParameters(ProductSearchServiceImplTest.STORE_ID,
        productCatentryIds);
    verify(this.productService).getProductsByProductCatentryIds(
        ProductSearchServiceImplTest.STORE_ID, productCatentryIds);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsByProductCodesTest() throws Exception {
    HashSet<String> productCodes = new HashSet<String>();
    productCodes.add(ProductSearchServiceImplTest.PRODUCT_CODE);
    List<Product> productResult = Arrays.asList(new Product());
    when(
        this.productService.getProductsByProductCodes(ProductSearchServiceImplTest.STORE_ID,
            productCodes)).thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getProductAndItemsByProductCodes(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        productCodes, true);
    verify(this.productSearchHelper).validateParameters(ProductSearchServiceImplTest.STORE_ID,
        productCodes);
    verify(this.productService).getProductsByProductCodes(ProductSearchServiceImplTest.STORE_ID,
        productCodes);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsByProductCodes_PVSwitchTrueTest() throws Exception {
    HashSet<String> productCodes = new HashSet<String>();
    productCodes.add(ProductSearchServiceImplTest.PRODUCT_CODE);
    List<Product> productResult = Arrays.asList(new Product());
    ReflectionTestUtils.setField(searchServiceImpl, "isProductVisibilityEnabled", true);
    when(
        this.productService.getProductsByProductCodes(ProductSearchServiceImplTest.STORE_ID,
            productCodes)).thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetailByDefaultProduct(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getProductAndItemsByProductCodes(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        productCodes, true);
    verify(this.productSearchHelper).validateParameters(ProductSearchServiceImplTest.STORE_ID,
        productCodes);
    verify(this.productService).getProductsByProductCodes(ProductSearchServiceImplTest.STORE_ID,
        productCodes);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailByDefaultProduct(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsSyncByProductCodes_PVSwitchTrueTest() throws Exception {
    List<Product> productResult = Arrays.asList(new Product());
    ReflectionTestUtils.setField(searchServiceImpl, "isProductVisibilityEnabled", true);
    when(this.productCacheHelperService
        .findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetailByDefaultProduct(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, true, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID,
            SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED, "false", ""));
    this.searchServiceImpl.getProductAndItemsSyncByProductCodes(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRODUCT_CODE, true, true, false);
    verify(this.productCacheHelperService)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailByDefaultProduct(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, true, false);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsByProductCode_PVSwitchTrueTest() throws Exception {
    List<Product> productResult = Arrays.asList(new Product());
    ReflectionTestUtils.setField(searchServiceImpl, "isProductVisibilityEnabled", true);
    when(this.productCacheHelperService
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetailByDefaultProduct(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getProductAndItemsByProductCode(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRODUCT_CODE, true);
    verify(this.productCacheHelperService)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailByDefaultProduct(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsByProductCode_PVSwitchFalseTest() throws Exception {
    List<Product> productResult = Arrays.asList(new Product());
    when(this.productCacheHelperService
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getProductAndItemsByProductCode(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRODUCT_CODE, true);
    verify(this.productCacheHelperService)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsSyncByProductCodes_PVSwitchFalseTest() throws Exception {
    List<Product> productResult = Arrays.asList(new Product());
    when(this.productCacheHelperService
        .findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, true, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID,
            SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED, "false", ""));
    this.searchServiceImpl.getProductAndItemsSyncByProductCodes(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRODUCT_CODE, true, true, false);
    verify(this.productCacheHelperService)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, true, false);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsSyncByProductCodes_whenFetchAllProductsTrueTest() throws Exception {
    Product product = new Product();
    product.setSynchronized(true);
    List<Product> productResult = Collections.singletonList(product);
    when(this.productCacheHelperService
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    List<ProductAndItemsVO> productAndItems = Collections.singletonList(productAndItem);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, true, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID,
            SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED, "true", ""));
    this.searchServiceImpl.getProductAndItemsSyncByProductCodes(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRODUCT_CODE, true, true, false);
    verify(this.productCacheHelperService)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            ProductSearchServiceImplTest.PRODUCT_CODE);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, true, false);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getMasterDataAndProductAndItemData_PVSwitchTrueTest() throws Exception {
    List<Product> productResult = Collections.singletonList(new Product());
    Product product = new Product();
    ReflectionTestUtils.setField(searchServiceImpl, "isProductVisibilityEnabled", true);
    when(this.productCacheHelperService
        .findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE))
        .thenReturn(productResult);
    when(
        this.productSearchHelper.getProductAndItemWithMasterDataDetailByDefaultProduct(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, ITEM_SKU, true, false)).thenReturn(
        new MasterDataDetailWithProductAndItemResponseVo(null, null, product, new Item()));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID,
            SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED, "false", ""));
    this.searchServiceImpl.getMasterDataAndProductAndItemData(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRODUCT_CODE, ITEM_SKU, true, true, false);
    verify(this.productCacheHelperService)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED);
    verify(this.productSearchHelper).getProductAndItemWithMasterDataDetailByDefaultProduct(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, ITEM_SKU, true, false);
    verify(this.catalogService).getItemCatalogsWithCategoryHierarchy(
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, product);
  }

  @Test
  public void getMasterDataAndProductAndItemData_PVSwitchFalseTest() throws Exception {
    List<Product> productResult = Collections.singletonList(new Product());
    Product product = new Product();
    ReflectionTestUtils.setField(searchServiceImpl, "isProductVisibilityEnabled", false);
    when(this.productCacheHelperService
        .findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE))
        .thenReturn(productResult);
    when(
        this.productSearchHelper.getProductAndItemWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, ITEM_SKU, true, false)).thenReturn(
        new MasterDataDetailWithProductAndItemResponseVo(null, null, product, new Item()));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID,
            SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED, "false", ""));
    this.searchServiceImpl.getMasterDataAndProductAndItemData(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRODUCT_CODE, ITEM_SKU, true, true, false);
    verify(this.productCacheHelperService)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED);
    verify(this.productSearchHelper).getProductAndItemWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, ITEM_SKU, true, false);
    verify(this.catalogService).getItemCatalogsWithCategoryHierarchy(
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, product);
  }

  @Test
  public void getMasterDataAndProductAndItemData_PVSwitchFalseAndFetchAllTrueTest() throws Exception {
    Product resultProduct = new Product();
    resultProduct.setSynchronized(true);
    List<Product> productResult = Collections.singletonList(resultProduct);
    Product product = new Product();
    ReflectionTestUtils.setField(searchServiceImpl, "isProductVisibilityEnabled", false);
    when(this.productCacheHelperService
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE))
        .thenReturn(productResult);
    when(
        this.productSearchHelper.getProductAndItemWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, ITEM_SKU, true, false)).thenReturn(
        new MasterDataDetailWithProductAndItemResponseVo(null, null, product, new Item()));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID,
            SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED, "true", ""));
    this.searchServiceImpl.getMasterDataAndProductAndItemData(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRODUCT_CODE, ITEM_SKU, true, true, false);
    verify(this.productCacheHelperService)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.PRODUCT_CODE);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED);
    verify(this.productSearchHelper).getProductAndItemWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, ITEM_SKU, true, false);
    verify(this.catalogService).getItemCatalogsWithCategoryHierarchy(
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, product);
  }

  @Test
  public void getProductAndItemsByProductSkusTest() throws Exception {
    HashSet<String> productSkus = new HashSet<String>();
    productSkus.add(ProductSearchServiceImplTest.PRODUCT_SKU);
    List<Product> productResult = Arrays.asList(new Product());
    when(this.productService.getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    Item item = new Item();
    item.setMerchantCode(MERCHANT_CODE);
    item.setActivePromoBundlings(new HashSet<>());
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    productAndItem.setItems(itemList);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(TRUE);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH))
        .thenReturn(systemParameter);
    BusinessPartnerPromo businessPartnerPromo = new BusinessPartnerPromo();
    Set<String> activePromoBundlings = new HashSet<>();
    activePromoBundlings.add(ACTIVE_PROMO_BUNDLING_TEST);
    businessPartnerPromo.setActivePromoBundlings(activePromoBundlings);
    when(businessPartnerPromoService.findByBusinessPartnerCode(product.getMerchantCode()))
        .thenReturn(businessPartnerPromo);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));



    this.searchServiceImpl.getProductAndItemsByProductSkus(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, productSkus,
        true);
    verify(this.productSearchHelper).validateParameters(ProductSearchServiceImplTest.STORE_ID,
        productSkus);
    verify(this.productService).getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productService).setSellerPromoBundlings(eq(STORE_ID), Mockito.anyList());
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsByProductSkusBusinessPartnerPromoNullTest() throws Exception {
    HashSet<String> productSkus = new HashSet<String>();
    productSkus.add(ProductSearchServiceImplTest.PRODUCT_SKU);
    List<Product> productResult = Arrays.asList(new Product());
    when(this.productService.getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    Item item = new Item();
    item.setMerchantCode(MERCHANT_CODE);
    item.setActivePromoBundlings(new HashSet<>());
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    productAndItem.setItems(itemList);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(TRUE);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH))
        .thenReturn(systemParameter);
    when(businessPartnerPromoService.findByBusinessPartnerCode(product.getMerchantCode()))
        .thenReturn(null);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getProductAndItemsByProductSkus(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, productSkus,
        true);
    verify(this.productSearchHelper).validateParameters(ProductSearchServiceImplTest.STORE_ID,
        productSkus);
    verify(this.productService).getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productService).setSellerPromoBundlings(eq(STORE_ID), Mockito.anyList());
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsByProductSkusBusinessPartnerPromoEmptyTest() throws Exception {
    HashSet<String> productSkus = new HashSet<String>();
    productSkus.add(ProductSearchServiceImplTest.PRODUCT_SKU);
    List<Product> productResult = Arrays.asList(new Product());
    when(this.productService.getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    Item item = new Item();
    item.setMerchantCode(MERCHANT_CODE);
    item.setActivePromoBundlings(new HashSet<>());
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    productAndItem.setItems(itemList);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(TRUE);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH))
        .thenReturn(systemParameter);
    BusinessPartnerPromo businessPartnerPromo = new BusinessPartnerPromo();
    Set<String> activePromoBundlings = new HashSet<>();
    businessPartnerPromo.setActivePromoBundlings(activePromoBundlings);
    when(businessPartnerPromoService.findByBusinessPartnerCode(product.getMerchantCode()))
        .thenReturn(businessPartnerPromo);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getProductAndItemsByProductSkus(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, productSkus,
        true);
    verify(this.productSearchHelper).validateParameters(ProductSearchServiceImplTest.STORE_ID,
        productSkus);
    verify(this.productService).getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productService).setSellerPromoBundlings(eq(STORE_ID), Mockito.anyList());
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsByProductSkusActivePromoBundlingFalseTest() throws Exception {
    HashSet<String> productSkus = new HashSet<String>();
    productSkus.add(ProductSearchServiceImplTest.PRODUCT_SKU);
    List<Product> productResult = Arrays.asList(new Product());
    when(this.productService.getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    Item item = new Item();
    item.setMerchantCode(MERCHANT_CODE);
    item.setActivePromoBundlings(new HashSet<>());
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    productAndItem.setItems(itemList);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(TRUE);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH))
        .thenReturn(systemParameter);
    BusinessPartnerPromo businessPartnerPromo = new BusinessPartnerPromo();
    Set<String> activePromoBundlings = new HashSet<>();
    activePromoBundlings.add(ACTIVE_PROMO_BUNDLING_TEST);
    businessPartnerPromo.setActivePromoBundlings(activePromoBundlings);
    when(businessPartnerPromoService.findByBusinessPartnerCode(product.getMerchantCode()))
        .thenReturn(businessPartnerPromo);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getProductAndItemsByProductSkus(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, productSkus,
        true);
    verify(this.productSearchHelper).validateParameters(ProductSearchServiceImplTest.STORE_ID,
        productSkus);
    verify(this.productService).getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productService).setSellerPromoBundlings(eq(STORE_ID), Mockito.anyList());
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsByProductSkusSystemParameterFalseTest() throws Exception {
    HashSet<String> productSkus = new HashSet<String>();
    productSkus.add(ProductSearchServiceImplTest.PRODUCT_SKU);
    List<Product> productResult = Arrays.asList(new Product());
    when(this.productService.getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    Item item = new Item();
    item.setMerchantCode(MERCHANT_CODE);
    item.setActivePromoBundlings(new HashSet<>());
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    productAndItem.setItems(itemList);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(FALSE);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH))
        .thenReturn(systemParameter);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getProductAndItemsByProductSkus(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, productSkus,
        true);
    verify(this.productSearchHelper).validateParameters(ProductSearchServiceImplTest.STORE_ID,
        productSkus);
    verify(this.productService).getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productService).setSellerPromoBundlings(eq(STORE_ID), Mockito.anyList());
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsByProductSkusSystemParameterNullTest() throws Exception {
    HashSet<String> productSkus = new HashSet<String>();
    productSkus.add(ProductSearchServiceImplTest.PRODUCT_SKU);
    List<Product> productResult = Arrays.asList(new Product());
    when(this.productService.getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    Item item = new Item();
    item.setMerchantCode(MERCHANT_CODE);
    item.setActivePromoBundlings(new HashSet<>());
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    productAndItem.setItems(itemList);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(FALSE);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH))
        .thenReturn(systemParameter);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getProductAndItemsByProductSkus(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, productSkus,
        true);
    verify(this.productSearchHelper).validateParameters(ProductSearchServiceImplTest.STORE_ID,
        productSkus);
    verify(this.productService).getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productService).setSellerPromoBundlings(eq(STORE_ID), Mockito.anyList());
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsByProductSkusProductAndItemsNullTest() throws Exception {
    HashSet<String> productSkus = new HashSet<String>();
    productSkus.add(ProductSearchServiceImplTest.PRODUCT_SKU);
    List<Product> productResult = Arrays.asList(new Product());
    when(this.productService.getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus))
        .thenReturn(productResult);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    Item item = new Item();
    item.setMerchantCode(MERCHANT_CODE);
    item.setActivePromoBundlings(new HashSet<>());
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    productAndItem.setItems(itemList);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = new ArrayList<>();
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(FALSE);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH))
        .thenReturn(systemParameter);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetail(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    this.searchServiceImpl.getProductAndItemsByProductSkus(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, productSkus,
        true);
    verify(this.productSearchHelper).validateParameters(ProductSearchServiceImplTest.STORE_ID,
        productSkus);
    verify(this.productService).getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
  }

  @Test
  public void getProductAndItemsByProductSkus_PvSwitchTrueTest() throws Exception {
    HashSet<String> productSkus = new HashSet<String>();
    productSkus.add(ProductSearchServiceImplTest.PRODUCT_SKU);
    List<Product> productResult = Arrays.asList(new Product());
    ReflectionTestUtils.setField(searchServiceImpl, "isProductVisibilityEnabled", true);
    when(this.productService.getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus))
        .thenReturn(productResult);
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(FALSE);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SELLER_PROMO_BUNDLINGS_SWITCH))
        .thenReturn(systemParameter);
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    item.setSourceItemCode(SOURCE_ITEM_CODE);
    item.setContentChanged(true);
    productAndItem.setProduct(new Product());
    productAndItem.setItems(Arrays.asList(item));
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    when(
        this.productSearchHelper.getProductAndItemsWithMasterDataDetailByDefaultProduct(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productResult, false, false)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponseVo(null, null, productAndItems));

    MasterDataWithProductItemsVo response = this.searchServiceImpl
        .getProductAndItemsByProductSkus(ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, productSkus, true);
    verify(this.productSearchHelper).validateParameters(ProductSearchServiceImplTest.STORE_ID,
        productSkus);
    verify(this.productService).getProducts(ProductSearchServiceImplTest.STORE_ID, productSkus);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailByDefaultProduct(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productResult, false, false);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        productAndItems, null);
    verify(this.productService).setSellerPromoBundlings(eq(STORE_ID), Mockito.anyList());
    assertEquals(SOURCE_ITEM_CODE, response.getProductItemsVos().get(0).getItemVoList().get(0).getSourceItemCode());
    assertTrue(response.getProductItemsVos().get(0).getItemVoList().get(0).isContentChanged());
  }

  @Test
  public void getProductAndItemWithoutSalesCatalogTest() throws Exception {
    ProductAndItemSolr itemSolr = new ProductAndItemSolr();
    itemSolr.setMasterCatalog(ProductSearchServiceImplTest.MASTER_CATALOG);
    ProductAndItemSolr itemSolr2 = new ProductAndItemSolr();
    itemSolr2.setMasterCatalog(ProductSearchServiceImplTest.MASTER_CATALOG);
    Page<ProductAndItemSolr> response =
        new PageImpl<ProductAndItemSolr>(Arrays.asList(itemSolr, itemSolr2));
    Category category =
        new Category(ProductSearchServiceImplTest.CATEGORY_CODE,
            ProductSearchServiceImplTest.CATEGORY_CODE);
    ItemCatalogVO catalogVO =
        new ItemCatalogVO(ProductSearchServiceImplTest.MASTER_CATALOG,
            new ArrayList<ItemCategoryVO>());

    when(
        this.solrRepository.getProductsWithNullSalesCatalogAndMarkForDeleteFalse(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.CREATED_DATE,
            ProductSearchServiceImplTest.CREATED_DATE,
            ProductSearchServiceImplTest.GENERATE_PAGEABLE)).thenReturn(response);
    when(
        this.productAndItemSolrConstructorService
            .constructMasterCatalog(ProductSearchServiceImplTest.MASTER_CATALOG)).thenReturn(
        new MasterCatalog(ProductSearchServiceImplTest.MASTER_CATALOG, category));
    when(
        this.catalogService.getItemCatalogsWithCategoryHierarchy(
            ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
            Arrays.asList(ProductSearchServiceImplTest.CATEGORY_CODE))).thenReturn(
        Arrays.asList(catalogVO));
    when(
        this.formulaUtil.getStartDate(ProductSearchServiceImplTest.MONTH,
            ProductSearchServiceImplTest.YEAR)).thenReturn(
        ProductSearchServiceImplTest.CREATED_DATE);
    when(
        this.formulaUtil.getEndDate(ProductSearchServiceImplTest.MONTH,
            ProductSearchServiceImplTest.YEAR)).thenReturn(
        ProductSearchServiceImplTest.CREATED_DATE);
    Page<ProductsToItemCatalogMapping> result =
        this.searchServiceImpl.getProductAndItemWithoutSalesCatalog(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
            ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.MONTH,
            ProductSearchServiceImplTest.YEAR, ProductSearchServiceImplTest.GENERATE_PAGEABLE);

    verify(this.solrRepository).getProductsWithNullSalesCatalogAndMarkForDeleteFalse(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.CREATED_DATE,
        ProductSearchServiceImplTest.CREATED_DATE, ProductSearchServiceImplTest.GENERATE_PAGEABLE);
    verify(this.productAndItemSolrConstructorService).constructMasterCatalog(
        ProductSearchServiceImplTest.MASTER_CATALOG);
    verify(this.catalogService).getItemCatalogsWithCategoryHierarchy(
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        Arrays.asList(ProductSearchServiceImplTest.CATEGORY_CODE));
  }

  private void createProductAndItems(List<Product> productList, List<Item> itemList){
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    product.setProductCode(PRODUCT_CODE);
    Item item = new Item();
    item.setItemSku("ItemSku");
    item.setProductSku(PRODUCT_SKU);
    itemList.add(item);
    productList.add(product);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    item.setPristineDataItem(pristineDataItem);

  }

  @Test
  public void getProductAndItemsByPristineIdsTest() throws Exception {

    Set<String> pristineIds = new HashSet<>();
    List<Item> itemList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    pristineIds.add(PRISTINE_ID);
    createProductAndItems(productList, itemList);
    when(this.itemService.getItemsByPristineIds(Mockito.eq(STORE_ID), Mockito.eq(pristineIds)))
        .thenReturn(itemList);
    when(this.productService.getProducts(Mockito.eq(STORE_ID), Mockito.anySet()))
        .thenReturn(productList);
    MasterDataDetailWithProductAndItemsResponseVo result = this.searchServiceImpl
        .getProductAndItemsByPristineIds(STORE_ID, REQUESTID, USERNAME, pristineIds);

    verify(this.itemService).getItemsByPristineIds(Mockito.eq(STORE_ID), Mockito.eq(pristineIds));
    verify(this.productService).getProducts(Mockito.eq(STORE_ID), Mockito.anySet());
    Assertions.assertNotNull(result);
    assertEquals(result.getProductAndItems().size(), 1);
    assertEquals(result.getProductAndItems().get(0).getProduct().getProductSku(), PRODUCT_SKU);

  }

  @Test
  public void getProductAndItemsByPristineIds_WhenPristineIdNotExistTest() throws Exception {

    Set<String> pristineIds = new HashSet<>();
    List<Item> itemList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    pristineIds.add(PRISTINE_ID);
    when(this.itemService.getItemsByPristineIds(Mockito.eq(STORE_ID), Mockito.eq(pristineIds)))
        .thenReturn(itemList);
    MasterDataDetailWithProductAndItemsResponseVo result = this.searchServiceImpl
        .getProductAndItemsByPristineIds(STORE_ID, REQUESTID, USERNAME, pristineIds);
    verify(this.itemService).getItemsByPristineIds(Mockito.eq(STORE_ID), Mockito.eq(pristineIds));
    Assertions.assertNull(result);
  }

  @Test
  public void getProductAndItemsByProductCodesAndProductSkusTest() throws Exception {

    Set<String> productSkus = new HashSet<>();
    Set<String> productCodes = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    productCodes.add(PRODUCT_CODE);
    List<Item> itemList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    createProductAndItems(productList, itemList);
    when(this.productService.getProducts(Mockito.eq(STORE_ID), Mockito.eq(productSkus)))
        .thenReturn(productList);
    when(this.productService
        .getProductsByProductCodes(Mockito.eq(STORE_ID), Mockito.eq(productCodes)))
        .thenReturn(productList);
    when(this.itemService.getItemsByProductSkus(Mockito.eq(STORE_ID), Mockito.anySet()))
        .thenReturn(itemList);
    MasterDataDetailWithProductAndItemsResponseVo result = this.searchServiceImpl
        .getProductAndItemsByProductCodesAndProductSkus(STORE_ID, REQUESTID, USERNAME, productCodes,
            productSkus);

    verify(this.productService).getProducts(Mockito.eq(STORE_ID), Mockito.eq(productSkus));
    verify(this.productService)
        .getProductsByProductCodes(Mockito.eq(STORE_ID), Mockito.eq(productCodes));
    verify(this.itemService).getItemsByProductSkus(Mockito.eq(STORE_ID), Mockito.anySet());
    Assertions.assertNotNull(result);
    assertEquals(result.getProductAndItems().size(), 1);
    assertEquals(result.getProductAndItems().get(0).getProduct().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void getProductAndItemsByProductCodesAndProductSkusResultNullTest() throws Exception {

    Set<String> productSkus = new HashSet<>();
    Set<String> productCodes = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    productCodes.add(PRODUCT_CODE);
    List<Item> itemList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    createProductAndItems(productList, itemList);
    when(this.productService.getProducts(Mockito.eq(STORE_ID), Mockito.eq(productSkus)))
        .thenReturn(null);
    when(this.productService
        .getProductsByProductCodes(Mockito.eq(STORE_ID), Mockito.eq(productCodes)))
        .thenReturn(null);

    MasterDataDetailWithProductAndItemsResponseVo result = this.searchServiceImpl
        .getProductAndItemsByProductCodesAndProductSkus(STORE_ID, REQUESTID, USERNAME, productCodes,
            productSkus);

    verify(this.productService).getProducts(Mockito.eq(STORE_ID), Mockito.eq(productSkus));
    verify(this.productService)
        .getProductsByProductCodes(Mockito.eq(STORE_ID), Mockito.eq(productCodes));
    Assertions.assertNull(result);

  }


  private List<Item> createItemDataForComputerCategory(){
    Item item = new Item();
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId("pristineId");
    pristineDataItem.setPristineMasterId(ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory("COMPUTER");
    pristineDataItem.setDefaultProductCode(PRODUCT_CODE);
    item.setPristineDataItem(pristineDataItem);
    item.setProductSku(PRODUCT_SKU);
    item.setItemSku("itemSku");
    Map<String, String> listingAttributes = new HashMap<>();
    listingAttributes.put("color", "black");
    listingAttributes.put("screensize", "11.6 inch");
    listingAttributes.put("processorname", "PENTIUM");
    listingAttributes.put("ram", "8 gb");
    listingAttributes.put("ssd/hdd", "320 gb");
    pristineDataItem.setPristineListingAttributes(listingAttributes);
    Price price = new Price();
    price.setListPrice(100000);
    price.setOfferPrice(90000);
    price.setChannel("DEFAULT");
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    item.setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfig)));
    item.setMerchantCode(MERCHANT_CODE);
    List<Item> itemList = Arrays.asList(item);
    return itemList;
  }

  private List<Item> createItemDataForHandphoneCategory(){
    Item item = new Item();
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId("pristineId");
    pristineDataItem.setPristineMasterId(ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory("HANDPHONE");
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku("itemSku");
    item.setProductSku(PRODUCT_SKU);
    Map<String, String> listingAttributes = new HashMap<>();
    listingAttributes.put("color", "black");
    listingAttributes.put("rom", "32gb");
    pristineDataItem.setPristineListingAttributes(listingAttributes);
    List<Item> itemList = Arrays.asList(item);
    return itemList;
  }

  @Test
  public void getProductAndItemsSyncByPristineId_WhenPristineCategoryComputerTest() throws Exception {
    HashSet<String> productSkus = new HashSet<String>();
    productSkus.add(ProductSearchServiceImplTest.PRODUCT_SKU);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    List<ProductAttribute> attributes = Arrays.asList(new ProductAttribute());
    product.setDefiningAttributes(attributes);
    product.setProductSku(PRODUCT_SKU);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(product.getProductCode());
    List<Product> productResult = Arrays.asList(product);
    Map<String, Product> skuproductMap = new HashMap<>();
    skuproductMap.put(PRODUCT_SKU, product);
    Set<String> pristineMasterIds = new HashSet<>();
    pristineMasterIds.add(ProductSearchServiceImplTest.PRISTINE_MASTER_ID);

    List<Item> itemList = createItemDataForComputerCategory();

    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    productAndItem.setItems(itemList);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);

    PristineMasterDataDetailWithProductAndItemsResponseVo response =
        new PristineMasterDataDetailWithProductAndItemsResponseVo();
    response.setProductAndItems(productAndItems);
    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributesMap = new LinkedHashMap<>();
    attributesMap.put("Warna", "WHITE");
    attributesMap.put("Kapasitas Memori", "8 GB");

    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(categoryListingParameters);
    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);
    when(this.productSearchHelper.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(),
        productCodes)).thenReturn(response);
    when(this.itemCacheableService
        .findItemsByPristineId(STORE_ID, USERNAME, REQUESTID, "pristineId", false)).thenReturn(itemList);
    when(this.productCacheHelperService
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString())).thenReturn(product);
    when(this.productCacheHelperService.findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(
        ProductSearchServiceImplTest.STORE_ID, productSkus)).thenReturn(skuproductMap);
    this.searchServiceImpl.getProductAndItemsSyncByPristineId(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRISTINE_ID, true, "itemSku");
    verify(this.itemCacheableService).findItemsByPristineId(STORE_ID, USERNAME, REQUESTID, "pristineId", false);

    verify(this.productCacheHelperService).findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
        productSkus);
    verify(this.productCacheHelperService).findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString());
    verify(this.objectMapper).readValue(Mockito.anyString(),
        Mockito.any(TypeReference.class));
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(itemList.get(0).getPristineDataItem()));
    verify(productAttributesUtil, times(2)).getAttributeNameTranslationMap();
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(), productCodes);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        response.getProductAndItems(), response.getMasterDataProducts());
    assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineMasterId(), ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineId(), PRISTINE_ID);
  }

  @Test
  public void getProductAndItemsSyncByPristineId_WhenPristineCategoryHandphoneTest() throws Exception {
    HashSet<String> productSkus = new HashSet<String>();
    productSkus.add(ProductSearchServiceImplTest.PRODUCT_SKU);
    Product product = new Product();
    List<ProductAttribute> attributes = Arrays.asList(new ProductAttribute());
    product.setDefiningAttributes(attributes);
    product.setProductSku(PRODUCT_SKU);
    Map<String, Product> skuproductMap = new HashMap<>();
    skuproductMap.put(PRODUCT_SKU, product);
    List<Product> productResult = Arrays.asList(product);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(product.getProductCode());
    Set<String> pristineMasterIds = new HashSet<>();
    pristineMasterIds.add(ProductSearchServiceImplTest.PRISTINE_MASTER_ID);

    List<Item> itemList = createItemDataForHandphoneCategory();

    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    productAndItem.setItems(itemList);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    PristineMasterDataDetailWithProductAndItemsResponseVo response =
        new PristineMasterDataDetailWithProductAndItemsResponseVo();
    response.setProductAndItems(productAndItems);
    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributesMap = new LinkedHashMap<>();
    attributesMap.put("Warna", "black");
    attributesMap.put("Kapasitas Memori", "32GB");

    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(categoryListingParameters);
    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);

    when(this.itemCacheableService
        .findItemsByPristineId(STORE_ID, USERNAME, REQUESTID, "pristineId", false)).thenReturn(itemList);
    when(this.productCacheHelperService
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString())).thenReturn(product);
    when(this.productCacheHelperService.findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(
        ProductSearchServiceImplTest.STORE_ID, productSkus)).thenReturn(skuproductMap);
    when(this.productSearchHelper.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(), productCodes)).thenReturn(response);

    this.searchServiceImpl.getProductAndItemsSyncByPristineId(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRISTINE_ID, true, "");
    verify(this.itemCacheableService).findItemsByPristineId(STORE_ID, USERNAME, REQUESTID, "pristineId", false);

    verify(this.productCacheHelperService).findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
        productSkus);
    verify(this.productCacheHelperService).findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString());
    verify(this.objectMapper, times(1))
        .readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(), productCodes);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        response.getProductAndItems(), response.getMasterDataProducts());
    verify(productAttributesUtil).getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()));
    verify(productAttributesUtil, times(2)).getAttributeNameTranslationMap();
    assertEquals(
        productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineMasterId(),
        ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    assertEquals(
        productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineId(),
        PRISTINE_ID);
    assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem()
        .getProductNameBySimilarParameters(), StringUtils.EMPTY);

  }

  @Test
  public void getProductAndItemsSyncByPristineId_WhenProductResultNullTest() throws Exception {
    HashSet<String> productSkus = new HashSet<String>();
    productSkus.add(ProductSearchServiceImplTest.PRODUCT_SKU);
    Product product = new Product();
    List<ProductAttribute> attributes = Arrays.asList(new ProductAttribute());
    product.setDefiningAttributes(attributes);
    product.setProductSku(PRODUCT_SKU);
    Map<String, Product> skuproductMap = new HashMap<>();
    skuproductMap.put(PRODUCT_SKU, product);
    List<Product> productResult = Arrays.asList(product);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(product.getProductCode());
    Set<String> pristineMasterIds = new HashSet<>();
    pristineMasterIds.add(ProductSearchServiceImplTest.PRISTINE_MASTER_ID);

    List<Item> itemList = createItemDataForHandphoneCategory();

    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    productAndItem.setItems(itemList);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    PristineMasterDataDetailWithProductAndItemsResponseVo response =
        new PristineMasterDataDetailWithProductAndItemsResponseVo();
    response.setProductAndItems(productAndItems);

    when(this.itemCacheableService
        .findItemsByPristineId(STORE_ID, USERNAME, REQUESTID, "pristineId", false)).thenReturn(itemList);
    when(this.productCacheHelperService
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString())).thenReturn(product);
    when(this.productCacheHelperService.findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(
        ProductSearchServiceImplTest.STORE_ID, productSkus)).thenReturn(null);

    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.searchServiceImpl.getProductAndItemsSyncByPristineId(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRISTINE_ID, true, ""));

    verify(this.itemCacheableService).findItemsByPristineId(STORE_ID, USERNAME, REQUESTID, "pristineId", false);

    verify(this.productCacheHelperService).findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
        productSkus);
    verify(this.productCacheHelperService).findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString());
  }


  @Test
  public void getProductAndItemsSyncByPristineId_WhenPristineIdNullTest() throws Exception {
    when(this.itemCacheableService
        .findItemsByPristineId(STORE_ID, USERNAME, REQUESTID, "pristineId", false)).thenReturn(null);
    try {
      this.searchServiceImpl
          .getProductAndItemsSyncByPristineId(ProductSearchServiceImplTest.STORE_ID,
              ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
              ProductSearchServiceImplTest.PRISTINE_ID, true, "itemSku");
    } catch (ApplicationRuntimeException e) {
      verify(this.itemCacheableService).findItemsByPristineId(STORE_ID, USERNAME, REQUESTID, "pristineId", false);
    }
  }

  @Test
  public void getProductCodesAndSkusByPristineIdsTest() throws Exception {

    Set<String> pristineIds = new HashSet<>();
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    List<Item> itemList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    pristineIds.add(PRISTINE_ID);
    createProductAndItems(productList, itemList);

    when(this.itemService.getItemsByPristineIds(STORE_ID, pristineIds)).thenReturn(itemList);
    when(this.productService.getProducts(STORE_ID, productSkuSet)).thenReturn(productList);
    List<SimplePristineProductRequestVo> response = this.searchServiceImpl
        .getProductCodesAndSkusByPristineIds(STORE_ID, REQUESTID, USERNAME, pristineIds);
    verify(this.itemService).getItemsByPristineIds(STORE_ID, pristineIds);
    verify(this.productService).getProducts(STORE_ID, productSkuSet);
    Assertions.assertNotNull(response);
    assertEquals(response.size(), 1);
    assertEquals(response.get(0).getPristineId(), PRISTINE_ID);

  }

  @Test
  public void getProductCodesAndSkusByPristineIds_ProductListEmptyTest() throws Exception {

    Set<String> pristineIds = new HashSet<>();
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    List<Item> itemList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    pristineIds.add(PRISTINE_ID);
    createProductAndItems(productList, itemList);

    when(this.itemService.getItemsByPristineIds(STORE_ID, pristineIds)).thenReturn(itemList);
    when(this.productService.getProducts(STORE_ID, productSkuSet)).thenReturn(new ArrayList<Product>());
    List<SimplePristineProductRequestVo> response = this.searchServiceImpl
        .getProductCodesAndSkusByPristineIds(STORE_ID, REQUESTID, USERNAME, pristineIds);
    verify(this.itemService).getItemsByPristineIds(STORE_ID, pristineIds);
    verify(this.productService).getProducts(STORE_ID, productSkuSet);
    Assertions.assertNotNull(response);
    assertEquals(response.size(), 1);
    assertEquals(response.get(0).getPristineId(), PRISTINE_ID);
    assertEquals(response.get(0).getSimpleProductDTOList().size(),0);

  }

  @Test
  public void getProductCodesAndSkusByPristineIds_ItemListEmptyTest() throws Exception {

    Set<String> pristineIds = new HashSet<>();
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    List<Item> itemList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    pristineIds.add(PRISTINE_ID);
    createProductAndItems(productList, itemList);

    when(this.itemService.getItemsByPristineIds(STORE_ID, pristineIds)).thenReturn(new ArrayList
        <Item>());

    List<SimplePristineProductRequestVo> response = this.searchServiceImpl
        .getProductCodesAndSkusByPristineIds(STORE_ID, REQUESTID, USERNAME, pristineIds);
    verify(this.itemService).getItemsByPristineIds(STORE_ID, pristineIds);
    Assertions.assertNotNull(response);
    assertEquals(response.size(), 0);

  }

  @Test
  public void getProductMasterDataDetailByProductCodesAndSkusTest() throws Exception {

    Set<String> productSkus = new HashSet<>();
    Set<String> productCodes = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    productCodes.add(PRODUCT_CODE);
    List<Item> itemList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    createProductAndItems(productList, itemList);
    MasterDataDetailWithProductAndItemsResponseVo result =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<ProductAndItemsVO> productAndItemsVOList = new ArrayList<>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(productList.get(0));
    productAndItemsVO.setItems(itemList);
    productAndItemsVOList.add(productAndItemsVO);
    result.setProductAndItems(productAndItemsVOList);
    result.setMasterDataProducts(new HashMap<>());
    ItemCatalogVO catalogVO = new ItemCatalogVO(ProductSearchServiceImplTest.MASTER_CATALOG, new ArrayList<>());

    when(this.productService.getProducts(Mockito.eq(STORE_ID), Mockito.eq(productSkus)))
        .thenReturn(productList);
    when(this.productService
        .getProductsByProductCodes(Mockito.eq(STORE_ID), Mockito.eq(productCodes)))
        .thenReturn(productList);
    when(this.productSearchHelper
        .getProductAndItemsWithMasterDataDetail(Mockito.eq(STORE_ID), Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList(), Mockito.eq(false), Mockito.eq(false))).thenReturn(result);
    when(this.catalogService.getItemCatalogsWithCategoryHierarchy(ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, productList.get(0))).thenReturn(Arrays.asList(catalogVO));
    MasterDataDetailWithProductAndItemsResponseVo response = this.searchServiceImpl
        .getProductMasterDataDetailByProductCodesAndSkus(STORE_ID, USERNAME, REQUESTID,
            productCodes, productSkus);

    verify(this.productService).getProducts(Mockito.eq(STORE_ID), Mockito.eq(productSkus));
    verify(this.productService)
        .getProductsByProductCodes(Mockito.eq(STORE_ID), Mockito.eq(productCodes));
    verify(this.productSearchHelper)
        .getProductAndItemsWithMasterDataDetail(Mockito.eq(STORE_ID), Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList(), Mockito.eq(false), Mockito.eq(false));
    verify(this.productSearchHelper)
        .setItemCatalogs(Mockito.eq(STORE_ID), Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.eq(true), Mockito.eq(result.getProductAndItems()),
            Mockito.eq(result.getMasterDataProducts()));
    verify(this.catalogService).getItemCatalogsWithCategoryHierarchy(Mockito.eq(ProductSearchServiceImplTest.USERNAME),
        Mockito.eq(ProductSearchServiceImplTest.REQUESTID), Mockito.eq(productList.get(0)));
    verify(itemService).getSalesCategorySequenceListFromCategoryHierarchy(ArgumentMatchers.anyList());
    Assertions.assertNotNull(response);
    assertEquals(response.getProductAndItems().get(0).getProduct().getProductSku(),
        PRODUCT_SKU);

  }

  @Test
  public void getProductMasterDataDetailByProductCodesAndSkusResultNullTest()
      throws Exception {

    Set<String> productSkus = new HashSet<>();
    Set<String> productCodes = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    productCodes.add(PRODUCT_CODE);
    List<Item> itemList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    createProductAndItems(productList, itemList);
    MasterDataDetailWithProductAndItemsResponseVo result = null;
    try {
      when(this.productService.getProducts(Mockito.eq(STORE_ID), Mockito.eq(productSkus)))
          .thenReturn(null);
      when(this.productService
          .getProductsByProductCodes(Mockito.eq(STORE_ID), Mockito.eq(productCodes)))
          .thenReturn(null);

      result = this.searchServiceImpl
          .getProductMasterDataDetailByProductCodesAndSkus(STORE_ID, REQUESTID, USERNAME,
              productCodes, productSkus);
    } catch (Exception e) {
      verify(this.productService).getProducts(Mockito.eq(STORE_ID), Mockito.eq(productSkus));
      verify(this.productService)
          .getProductsByProductCodes(Mockito.eq(STORE_ID), Mockito.eq(productCodes));
      Assertions.assertNull(result);
      Assertions.assertTrue(e instanceof ApplicationRuntimeException);
    }

  }

  @BeforeEach
  public void init() throws IOException{
    openMocks(this);

    similarParameters.put("HANDPHONE", "");
    similarParameters.put("COMPUTER","screensize,processorname,ram,ssd/hdd");

    listingParameters.put("HANDPHONE", "color,rom");
    listingParameters.put("COMPUTER","color");

    attributeNameTranslationMap.put("color", "Warna");
    attributeNameTranslationMap.put("rom","Kapasitas Memori");

    when(this.objectMapper.readValue(Mockito.eq(PRISTINE_SIMILAR_ATTRIBUTE),
        Mockito.any(TypeReference.class)))
        .thenReturn(similarParameters);

    when(this.objectMapper.readValue(Mockito.eq(PRISTINE_LISTING_ATTRIBUTE),
        Mockito.any(TypeReference.class)))
        .thenReturn(listingParameters);

    when(this.objectMapper.readValue(Mockito.eq(PRISTINE_ATTRIBUTE_VALUE_TRANSLATION),
        Mockito.any(new TypeReference<Map<String, String>>() {}.getClass())))
        .thenReturn(attributeNameTranslationMap);

    ReflectionTestUtils.setField(searchServiceImpl, "pristineSimilarParameters",
        PRISTINE_SIMILAR_ATTRIBUTE);

    this.wholesaleRuleVO = new WholesaleRuleVO();
    this.wholesaleRuleVO.setMinQuantity(5);
    this.wholesaleRuleVO.setMaxQuantity(10);
    this.wholesaleRuleVO.setDiscountPercentage(10.0);

    this.wholesaleRuleVOS = new ArrayList<>();
    this.wholesaleRuleVOS.add(wholesaleRuleVO);

    this.promoBundlingDetailResponse = new PromoBundlingDetailResponseVO();
    this.promoBundlingDetailResponse.setPromoBundlingId(PROMO_BUNDLING_ID);
    this.promoBundlingDetailResponse.setPromoBundlingName(PROMO_BUNDLING_NAME);
    this.promoBundlingDetailResponse.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    this.promoBundlingDetailResponse.setItemSku(ITEM_SKU);
    this.promoBundlingDetailResponse.setWholesaleRules(wholesaleRuleVOS);

    this.promoBundlingDetailResponseVOs = new ArrayList<>();
    this.promoBundlingDetailResponseVOs.add(promoBundlingDetailResponse);

    this.activePromoBundlingResponseVO = new ActivePromoBundlingResponseVO();
    this.activePromoBundlingResponseVO.setPromoBundlingDetailResponseVOList(promoBundlingDetailResponseVOs);

    this.offlineItem = new OfflineItem();
    this.offlineItem.setItemSku(ITEM_SKU_OFFLINE_ITEM);

    this.itemPickupPoint = new ItemPickupPoint();
    this.itemPickupPoint.setItemSku(ITEM_SKU_OFFLINE_ITEM);
    this.itemPickupPoint.setPrice(new HashSet<>(Arrays.asList(new Price())));

    this.masterDataProductImage = new MasterDataProductImage();
    this.masterDataProductImage.setMainImage(Boolean.TRUE);
    this.masterDataProductImage.setLocationPath(IMAGE_LOCATION_PATH);

    this.masterDataProductImages = new ArrayList<>();
    this.masterDataProductImages.add(masterDataProductImage);

    masterDataProductDetailResponse = new HashMap<>();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    masterDataProduct.setUrl(URL);
    masterDataProduct.setMasterDataProductImages(masterDataProductImages);
    masterDataProductAndItemsVO.setMasterDataProduct(masterDataProduct);
    masterDataProductAndItemsVO.setMasterDataItems(Collections.emptyMap());
    masterDataProductDetailResponse.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    officialStoreRequestVO.setProductSku(PRODUCT_SKU);
    officialStoreRequestVO.setProductName(PRODUCT_NAME);
    officialStoreRequestVO.setBrands(Collections.singletonList(BRAND));
    officialStoreRequestVO.setMerchantCodes(Collections.singletonList(MERCHANT_CODE));

    item = new Item();
    item.setProductSku(PRODUCT_SKU);
    item.setItemCode(ITEM_SKU);
    item.setItemSku(ITEM_SKU);

    productAndItemsVO = new ProductAndItemsVO(new Product(), Arrays.asList(item));
    masterDataProduct.setProductName(PRODUCT_NAME);
    masterDataDetailWithProductAndItemsResponseVo.setMasterDataProducts(new HashMap<>());
    masterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts().put(PRODUCT_CODE, masterDataProduct);
    masterDataDetailWithProductAndItemsResponseVo.setProductAndItems(Arrays.asList(productAndItemsVO));
    systemParameter.setValue(String.valueOf(true));
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REVERT_TRANSACTION_API_SWITCH)).thenReturn(systemParameter);
    product.setProductSku(PRODUCT_SKU);
    product.setMasterDataProduct(masterDataProduct);
    product.setMasterCatalog(new MasterCatalog(CATEGORY_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    product.setSalesCatalogs(Collections.emptyList());
    item.setProductSku(PRODUCT_SKU);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.solrRepository);
    verifyNoMoreInteractions(this.productSearchHelper);
    verifyNoMoreInteractions(this.productAndItemSolrConstructorService);
    verifyNoMoreInteractions(this.catalogService);
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(pristineItemRepository);
    verifyNoMoreInteractions(objectConverterService);
    verifyNoMoreInteractions(this.itemPriceService);
    verifyNoMoreInteractions(this.offlineItemService);
    verifyNoMoreInteractions(this.productHelperService);
    verifyNoMoreInteractions(this.productAttributesUtil);
    verifyNoMoreInteractions(this.systemParameterService);
    verifyNoMoreInteractions(this.cacheItemHelperService);
    verifyNoMoreInteractions(this.masterDataConstructorService);
    verifyNoMoreInteractions(this.itemPickupPointService);
  }

  @Test
  public void getPristineProductAndItemsInfoByItemSku_WhenPristineCategoryComputerTest() throws Exception {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    List<ProductAttribute> attributes = Arrays.asList(new ProductAttribute());
    product.setDefiningAttributes(attributes);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(product.getProductCode());
    List<Product> productResult = Arrays.asList(product);
    List<Item> itemList = createItemDataForComputerCategory();

    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    productAndItem.setItems(itemList);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);

    MasterDataDetailWithProductAndItemsResponseVo response =
        new MasterDataDetailWithProductAndItemsResponseVo();
    response.setProductAndItems(productAndItems);
    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributesMap = new LinkedHashMap<>();
    attributesMap.put("Warna", "black");
    attributesMap.put("Kapasitas Memori", "32GB");

    when(masterDataConstructorService.constructPristineDataItemWithMasterData(Mockito.any(Item.class)))
        .thenReturn(itemList.get(0));
    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(categoryListingParameters);
    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);
    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false)).thenReturn(itemList.get(0));
    when(this.productCacheHelperService
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString())).thenReturn(product);
    when(this.productSearchHelper.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(),
        productCodes)).thenReturn(response);
    when(itemService.getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList))
        .thenReturn(Collections.singletonList(itemList.get(0)));
    this.searchServiceImpl.getPristineProductAndItemsInfoByItemSku(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.ITEM_SKU, true, true, false);
    verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false);
    verify(this.productCacheHelperService)
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString());
    verify(masterDataConstructorService).constructPristineDataItemWithMasterData(itemList.get(0));
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(itemList.get(0).getPristineDataItem()));
    verify(productAttributesUtil, times(2)).getAttributeNameTranslationMap();
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(), productCodes);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        response.getProductAndItems(), response.getMasterDataProducts());
    verify(this.productHelperService).findAndConstructOfflineItems(ProductSearchServiceImplTest.STORE_ID, itemList);
    verify(itemService).getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList);
    Assertions.assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineMasterId(), ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    Assertions.assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineId(), PRISTINE_ID);
  }

  @Test
  public void getPristineProductAndItemsInfoByItemSkuTest_usingOfflineItemSku_nullOfflineItem() throws Exception {
    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            ITEM_SKU, false, false, false)).thenReturn(null);
    when(this.itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.ITEM_SKU))
        .thenReturn(null);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.searchServiceImpl.getPristineProductAndItemsInfoByItemSku(ProductSearchServiceImplTest.STORE_ID,
          ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
          ProductSearchServiceImplTest.ITEM_SKU, true, false, false));
    } finally {
      verify(this.itemCacheableService)
          .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
              ProductSearchServiceImplTest.ITEM_SKU, false, false, false);
      verify(this.itemPickupPointService).findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
          ProductSearchServiceImplTest.ITEM_SKU);
    }
  }

  @Test
  public void getPristineProductAndItemsInfoByItemSkuTest_usingOfflineItemSku_nullItemFromOfflineItem() throws Exception {
    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            ITEM_SKU, false, false, false)).thenReturn(null);
    when(this.itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.ITEM_SKU))
        .thenReturn(this.itemPickupPoint);
    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            ITEM_SKU_OFFLINE_ITEM, false, false, false)).thenReturn(null);

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.searchServiceImpl.getPristineProductAndItemsInfoByItemSku(ProductSearchServiceImplTest.STORE_ID,
          ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
          ProductSearchServiceImplTest.ITEM_SKU, true, false, false));
    } finally {
      verify(this.itemCacheableService)
          .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
              ProductSearchServiceImplTest.ITEM_SKU, false, false, false);
      verify(this.itemPickupPointService).findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
          ProductSearchServiceImplTest.ITEM_SKU);
      verify(this.itemCacheableService)
          .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
              ProductSearchServiceImplTest.ITEM_SKU_OFFLINE_ITEM, false, false, false);
    }
  }

  @Test
  public void getPristineProductAndItemsInfoByItemSkuTest_usingOfflineItemSku_success() throws Exception {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    List<ProductAttribute> attributes = Arrays.asList(new ProductAttribute());
    product.setDefiningAttributes(attributes);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(product.getProductCode());
    List<Product> productResult = Arrays.asList(product);
    List<Item> itemList = createItemDataForComputerCategory();

    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    productAndItem.setItems(itemList);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);

    MasterDataDetailWithProductAndItemsResponseVo response =
        new MasterDataDetailWithProductAndItemsResponseVo();
    response.setProductAndItems(productAndItems);
    String[] categoryListingParameters = {"color"};
    Map<String, String> attributesMap = new LinkedHashMap<>();
    attributesMap.put("Warna", "WHITE");

    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(categoryListingParameters);
    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);
    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID, ITEM_SKU, false, false,
          false))
        .thenReturn(null);
    when(this.itemPickupPointService
        .findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.ITEM_SKU))
        .thenReturn(this.itemPickupPoint);
    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID, ITEM_SKU_OFFLINE_ITEM, false,
            false, false))
        .thenReturn(itemList.get(0));
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(Mockito.any(Item.class)))
        .thenReturn(itemList.get(0));
    when(this.productCacheHelperService
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString())).thenReturn(product);
    when(this.productSearchHelper.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(),
        productCodes)).thenReturn(response);
    when(itemService.getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList))
        .thenReturn(Collections.singletonList(itemList.get(0)));
    this.searchServiceImpl.getPristineProductAndItemsInfoByItemSku(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.ITEM_SKU, true, false, false);
    verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
            ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.ITEM_SKU, false, false,
          false);
    verify(this.itemPickupPointService)
        .findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.ITEM_SKU);
    verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            ProductSearchServiceImplTest.ITEM_SKU_OFFLINE_ITEM, false, false, false);
    verify(masterDataConstructorService).constructPristineDataItemWithMasterData(itemList.get(0));
    verify(this.productCacheHelperService)
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString());
    verify(productAttributesUtil).getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()));
    verify(productAttributesUtil).getAttributeNameTranslationMap();
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(), productCodes);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        response.getProductAndItems(), response.getMasterDataProducts());
    verify(itemService).getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID,
        Collections.singletonList(itemList.get(0)));
    verify(this.productHelperService).constructOfflineItem(itemList.get(0), CommonUtil.getOfflineItemByPickupPoint(this.itemPickupPoint, false, null));
    Assertions.assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineMasterId(), ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    Assertions.assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineId(), PRISTINE_ID);
  }

  @Test
  public void getPristineProductAndItemsInfoByItemSku_WhenPristineCategoryHandphoneTest() throws Exception {

    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    List<ProductAttribute> attributes = Arrays.asList(new ProductAttribute());
    product.setDefiningAttributes(attributes);
    List<Product> productResult = Arrays.asList(product);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(product.getProductCode());
    List<Item> itemList = createItemDataForHandphoneCategory();

    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    productAndItem.setItems(itemList);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    PristineMasterDataDetailWithProductAndItemsResponseVo response =
        new PristineMasterDataDetailWithProductAndItemsResponseVo();
    response.setProductAndItems(productAndItems);

    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributeMap = new LinkedHashMap<>();
    attributeMap.put("Warna", "WHITE");
    attributeMap.put("Kapasitas Memori", "8 GB");

    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(categoryListingParameters);

    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);

    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false)).thenReturn(itemList.get(0));
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(Mockito.any(Item.class)))
        .thenReturn(itemList.get(0));
    when(this.productCacheHelperService
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString())).thenReturn(product);
    when(this.productSearchHelper.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(),
        productCodes)).thenReturn(response);
    when(itemService.getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList))
        .thenReturn(Collections.singletonList(itemList.get(0)));
    this.searchServiceImpl
        .getPristineProductAndItemsInfoByItemSku(ProductSearchServiceImplTest.STORE_ID,
            ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
            ProductSearchServiceImplTest.ITEM_SKU, true, true, false);
    verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false);
    verify(this.productCacheHelperService)
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString());
    verify(masterDataConstructorService).constructPristineDataItemWithMasterData(itemList.get(0));
    verify(productAttributesUtil).getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()));
    verify(productAttributesUtil, times(2)).getAttributeNameTranslationMap();
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(), productCodes);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        response.getProductAndItems(), response.getMasterDataProducts());
    verify(this.productHelperService).findAndConstructOfflineItems(ProductSearchServiceImplTest.STORE_ID, itemList);
    verify(itemService).getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList);
    Assertions.assertEquals(
        productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineMasterId(),
        ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    Assertions.assertEquals(
        productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineId(),
        PRISTINE_ID);
  }

  @Test
  public void getPristineProductAndItemsInfoByItemSku_WhenProductNullTest() throws Exception {
    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    List<ProductAttribute> attributes = Arrays.asList(new ProductAttribute());
    product.setDefiningAttributes(attributes);
    List<Product> productResult = Arrays.asList(product);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(product.getProductCode());
    List<Item> itemList = createItemDataForHandphoneCategory();

    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    productAndItem.setItems(itemList);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);
    PristineMasterDataDetailWithProductAndItemsResponseVo response =
        new PristineMasterDataDetailWithProductAndItemsResponseVo();
    response.setProductAndItems(productAndItems);

    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false)).thenReturn(itemList.get(0));
    when(this.productCacheHelperService
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString())).thenReturn(null);
    when(itemService.getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList))
        .thenReturn(Collections.singletonList(itemList.get(0)));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.searchServiceImpl
          .getPristineProductAndItemsInfoByItemSku(ProductSearchServiceImplTest.STORE_ID,
              ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
              ProductSearchServiceImplTest.ITEM_SKU, true, true, false));
    } finally {
      verify(this.productHelperService).findAndConstructOfflineItems(
          ProductSearchServiceImplTest.STORE_ID, Arrays.asList(itemList.get(0)));
      verify(this.itemCacheableService)
          .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
              "itemSku", true, false, false);
      verify(this.productCacheHelperService)
          .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString());
      verify(itemService)
          .getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, Collections.singletonList(itemList.get(0)));
    }
  }


  @Test
  public void getPristineProductAndItemsInfoByItemSku_WhenItemSkuNotPristineMappedTest() throws Exception {
    List<Item> itemList = createItemDataForHandphoneCategory();
    itemList.get(0).setPristineDataItem(null);
    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false)).thenReturn(itemList.get(0));
    try {
      this.searchServiceImpl
          .getPristineProductAndItemsInfoByItemSku(ProductSearchServiceImplTest.STORE_ID,
              ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
              ProductSearchServiceImplTest.ITEM_SKU, true, true, false);
    } catch (ApplicationRuntimeException e) {
      verify(this.productHelperService).findAndConstructOfflineItems(ProductSearchServiceImplTest.STORE_ID, itemList);
      verify(this.itemCacheableService)
          .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
              "itemSku", true, false, false);
    }
  }

  @Test
  public void getPristineProductAndItemsInfoByItemSku_WhenItemSkuNullTest() throws Exception {
    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false)).thenReturn(null);
    when(this.itemPickupPointService
        .findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.ITEM_SKU))
        .thenReturn(null);
    try {
      this.searchServiceImpl
          .getPristineProductAndItemsInfoByItemSku(ProductSearchServiceImplTest.STORE_ID,
              ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
              ProductSearchServiceImplTest.ITEM_SKU, true, true, false);
    } catch (ApplicationRuntimeException e) {
      verify(this.itemCacheableService)
          .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
              "itemSku", true, false, false);
      verify(this.itemPickupPointService).findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(
          ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.ITEM_SKU);
    }
  }

  @Test
  public void getProductAndItemsResponseByPristineIdTest() throws Exception {
    List<Item> itemList = createItemDataForComputerCategory();
    PristineItemVO pristineItemVo = new PristineItemVO();
    pristineItemVo.setItemSku(itemList.get(0).getItemSku());
    PristineProductAndItemsResponseVO response =
        new PristineProductAndItemsResponseVO();
    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributes = new LinkedHashMap<>();
    attributes.put("Warna", "WHITE");
    attributes.put("Kapasitas Memori", "8 GB");

    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(categoryListingParameters);
    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);
    when(this.itemCacheableService
        .findItemsByPristineId(ProductSearchServiceImplTest.STORE_ID, USERNAME, REQUESTID,
            PRISTINE_ID, false)).thenReturn(itemList);
    when(objectConverterService.convertToPristineItem(itemList.get(0))).thenReturn(pristineItemVo);
    response = this.searchServiceImpl.getProductAndItemsResponseByPristineId(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRISTINE_ID, false);
    verify(this.itemCacheableService)
        .findItemsByPristineId(ProductSearchServiceImplTest.STORE_ID, USERNAME, REQUESTID,
            PRISTINE_ID, false);
    verify(this.productHelperService).findAndConstructOfflineItems(ProductSearchServiceImplTest.STORE_ID, itemList);
    verify(objectConverterService).convertToPristineItem(itemList.get(0));
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(itemList.get(0).getPristineDataItem()));
    verify(productAttributesUtil, times(2)).getAttributeNameTranslationMap();
    Assertions.assertEquals(1,response.getPristineItems().size());
    Assertions.assertEquals("itemSku", response.getPristineItems().get(0).getItemSku());
  }

  @Test
  public void getProductAndItemsResponseByPristineIdCategoryNullTest() throws Exception {
    List<Item> itemList = createItemDataForComputerCategory();
    PristineItemVO pristineItemVo = new PristineItemVO();
    pristineItemVo.setItemSku(itemList.get(0).getItemSku());
    PristineProductAndItemsResponseVO response =
        new PristineProductAndItemsResponseVO();
    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributes = new LinkedHashMap<>();
    attributes.put("Warna", "WHITE");
    attributes.put("Kapasitas Memori", "8 GB");

    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(new String[0]);
    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);
    when(this.itemCacheableService
        .findItemsByPristineId(ProductSearchServiceImplTest.STORE_ID, USERNAME, REQUESTID,
            PRISTINE_ID, false)).thenReturn(itemList);
    when(objectConverterService.convertToPristineItem(itemList.get(0))).thenReturn(pristineItemVo);
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(itemList.get(0))).thenReturn(itemList.get(0));
    response = this.searchServiceImpl.getProductAndItemsResponseByPristineId(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRISTINE_ID, false);
    verify(this.itemCacheableService)
        .findItemsByPristineId(ProductSearchServiceImplTest.STORE_ID, USERNAME, REQUESTID,
            PRISTINE_ID, false);
    verify(this.productHelperService).findAndConstructOfflineItems(ProductSearchServiceImplTest.STORE_ID, itemList);
    verify(objectConverterService).convertToPristineItem(itemList.get(0));
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(itemList.get(0).getPristineDataItem()));
    verify(masterDataConstructorService).constructPristineDataItemWithMasterData(itemList.get(0));
    Assertions.assertEquals(1,response.getPristineItems().size());
    Assertions.assertEquals("itemSku", response.getPristineItems().get(0).getItemSku());
  }

  @Test
  public void getProductAndItemsResponseByPristineIdForDummyPristineIdTest() throws Exception {
    List<Item> itemList = createItemDataForComputerCategory();
    PristineItemVO pristineItemVo = new PristineItemVO();
    pristineItemVo.setItemSku(itemList.get(0).getItemSku());
    PristineProductAndItemsResponseVO response = new PristineProductAndItemsResponseVO();
    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(null);
    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);
    when(this.itemCacheableService
        .findItemsByPristineId(ProductSearchServiceImplTest.STORE_ID, USERNAME, REQUESTID,
            PRISTINE_ID, false)).thenReturn(itemList);
    when(objectConverterService.convertToPristineItem(itemList.get(0))).thenReturn(pristineItemVo);
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(itemList.get(0))).thenReturn(itemList.get(0));
    response = this.searchServiceImpl.getProductAndItemsResponseByPristineId(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRISTINE_ID, false);
    verify(this.itemCacheableService)
        .findItemsByPristineId(ProductSearchServiceImplTest.STORE_ID, USERNAME, REQUESTID,
            PRISTINE_ID, false);
    verify(this.productHelperService).findAndConstructOfflineItems(ProductSearchServiceImplTest.STORE_ID, itemList);
    verify(objectConverterService).convertToPristineItem(itemList.get(0));
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(itemList.get(0).getPristineDataItem()));
    verify(masterDataConstructorService).constructPristineDataItemWithMasterData(itemList.get(0));
    Assertions.assertEquals(1,response.getPristineItems().size());
    Assertions.assertEquals("itemSku", response.getPristineItems().get(0).getItemSku());
  }

  @Test
  public void getProductAndItemsResponseByPristineId_whenItemListNullTest() throws Exception {

    when(this.itemCacheableService
        .findItemsByPristineId(ProductSearchServiceImplTest.STORE_ID, USERNAME, REQUESTID,
            PRISTINE_ID, false)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.searchServiceImpl.getProductAndItemsResponseByPristineId(ProductSearchServiceImplTest.STORE_ID,
          ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
          ProductSearchServiceImplTest.PRISTINE_ID, false));
    } finally {
      verify(this.itemCacheableService)
          .findItemsByPristineId(ProductSearchServiceImplTest.STORE_ID, USERNAME, REQUESTID,
              PRISTINE_ID, false);
      verify(this.productHelperService)
          .findAndConstructOfflineItems(ProductSearchServiceImplTest.STORE_ID, null);
    }
  }

  @Test
  public void getProductDetailForReviewByProductCode_WhenStoreIdNullTest() throws Exception {
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.searchServiceImpl
          .getProductDetailForReviewByProductCode(null, REQUESTID, USERNAME, PRODUCT_CODE, null));
    }catch(ApplicationRuntimeException e){
      throw e;
    }
  }

  @Test
  public void getProductDetailForReviewByProductCode_WhenProductCodeNullTest() throws Exception {
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.searchServiceImpl
          .getProductDetailForReviewByProductCode(STORE_ID, REQUESTID, USERNAME, null, null));
    }catch(ApplicationRuntimeException e){
      throw e;
    }
  }

  @Test
  public void getProductDetailForReviewByProductCodeTest() throws Exception {

    Mockito.when(productSearchHelper
            .getProductMasterDataDetailByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE))
        .thenReturn(masterDataProductDetailResponse);
    ReviewProductDetailVO reviewProductDetailVO = this.searchServiceImpl
        .getProductDetailForReviewByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE, null);
    Mockito.verify(productSearchHelper)
        .getProductMasterDataDetailByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, reviewProductDetailVO.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, reviewProductDetailVO.getProductName());
    Assertions.assertEquals(URL, reviewProductDetailVO.getProductUrl());
  }

  @Test
  public void getProductDetailForReviewByProductCode_WhenMasterDetailNotExistTest() throws Exception {

    Mockito.when(productSearchHelper
            .getProductMasterDataDetailByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE))
        .thenReturn(null);
    ReviewProductDetailVO reviewProductDetailVO = this.searchServiceImpl
        .getProductDetailForReviewByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE, null);
    Mockito.verify(productSearchHelper)
        .getProductMasterDataDetailByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE);
    Assertions.assertNull(reviewProductDetailVO.getProductCode());

  }

  @Test
  public void getProductDetailForReviewByProductSku_WhenStoreIdNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.searchServiceImpl
          .getProductDetailForReviewByProductSku(null, REQUESTID, USERNAME, PRODUCT_SKU, null));
  }

  @Test
  public void getProductDetailForReviewByProductSku_WhenProductCodeNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.searchServiceImpl
          .getProductDetailForReviewByProductSku(STORE_ID, REQUESTID, USERNAME, null, null));
  }

  @Test
  public void getProductDetailForReviewByProductSku_WhenProductNullTest() throws Exception {
    try {
      Mockito.when(
              this.productCacheHelperService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
          .thenReturn(null);
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.searchServiceImpl
          .getProductDetailForReviewByProductSku(STORE_ID, REQUESTID, USERNAME, PRODUCT_SKU, null));
    } finally {
      Mockito.verify(this.productCacheHelperService)
          .findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    }
  }

  @Test
  public void getProductDetailForReviewByProductSkuTest() throws Exception {

    Product product = new Product();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    masterDataProduct.setUrl(URL);
    product.setProductCode(PRODUCT_CODE);
    product.setSynchronized(true);
    product.setMasterDataProduct(masterDataProduct);
    Mockito.when(
            this.productCacheHelperService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(productSearchHelper
            .getProductMasterDataDetailByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE))
        .thenReturn(masterDataProductDetailResponse);
    ReviewProductDetailVO reviewProductDetailVO = this.searchServiceImpl
        .getProductDetailForReviewByProductSku(STORE_ID, USERNAME, REQUESTID, PRODUCT_SKU, null);

    Mockito.verify(this.productCacheHelperService)
        .findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productSearchHelper)
        .getProductMasterDataDetailByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, reviewProductDetailVO.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, reviewProductDetailVO.getProductName());
    Assertions.assertEquals(URL, reviewProductDetailVO.getProductUrl());
  }

  @Test
  public void getProductDetailForReviewByProductSku_WhenIsSynchronizedFalseTest() throws Exception {

    Product product = new Product();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    masterDataProduct.setUrl(URL);
    masterDataProduct.setMasterDataProductImages(masterDataProductImages);
    product.setProductCode(PRODUCT_CODE);
    product.setSynchronized(false);
    product.setMasterDataProduct(masterDataProduct);
    Mockito.when(
            this.productCacheHelperService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    ReviewProductDetailVO reviewProductDetailVO = this.searchServiceImpl
        .getProductDetailForReviewByProductSku(STORE_ID, USERNAME, REQUESTID, PRODUCT_SKU, null);

    Mockito.verify(this.productCacheHelperService)
        .findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Assertions.assertEquals(PRODUCT_NAME, reviewProductDetailVO.getProductName());
    Assertions.assertEquals(URL, reviewProductDetailVO.getProductUrl());
    Assertions.assertEquals(product.getProductCode(), reviewProductDetailVO.getProductCode());
    Assertions.assertEquals(masterDataProductImage.getLocationPath(), reviewProductDetailVO.getImageUrl());
  }

  @Test
  public void getProductDetailForReviewByProductSku_WhenIsSynchronizedFalseAndProductCodeNullTest() throws Exception {

    Product product = new Product();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    masterDataProduct.setUrl(URL);
    masterDataProduct.setMasterDataProductImages(masterDataProductImages);
    product.setSynchronized(false);
    product.setMasterDataProduct(masterDataProduct);
    Mockito.when(
            this.productCacheHelperService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    ReviewProductDetailVO reviewProductDetailVO = this.searchServiceImpl
        .getProductDetailForReviewByProductSku(STORE_ID, USERNAME, REQUESTID, PRODUCT_SKU, null);

    Mockito.verify(this.productCacheHelperService)
        .findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Assertions.assertEquals(PRODUCT_NAME, reviewProductDetailVO.getProductName());
    Assertions.assertEquals(URL, reviewProductDetailVO.getProductUrl());
    Assertions.assertEquals(StringUtils.EMPTY, reviewProductDetailVO.getProductCode());
    Assertions.assertEquals(masterDataProductImage.getLocationPath(), reviewProductDetailVO.getImageUrl());
  }

  @Test
  public void getProductDetailForReviewByProductSku_WhenIsSynchronizedFalseWithItemSkuTest() throws Exception {

    Product product = new Product();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    masterDataProduct.setUrl(URL);
    masterDataProduct.setMasterDataProductImages(masterDataProductImages);
    product.setSynchronized(false);
    product.setMasterDataProduct(masterDataProduct);

    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(PRODUCT_ATTRIBUTE_VALUE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(ITEM_SKU);
    productAttribute.setProductAttributeDetails(Collections.singletonList(productAttributeDetail));

    product.setDefiningAttributes(Collections.singletonList(productAttribute));
    Mockito.when(
            this.productCacheHelperService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    ReviewProductDetailVO reviewProductDetailVO = this.searchServiceImpl
        .getProductDetailForReviewByProductSku(STORE_ID, USERNAME, REQUESTID, PRODUCT_SKU, ITEM_SKU);

    Mockito.verify(this.productCacheHelperService)
        .findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Assertions.assertEquals(PRODUCT_NAME, reviewProductDetailVO.getProductName());
    Assertions.assertEquals(URL, reviewProductDetailVO.getProductUrl());
    Assertions.assertEquals(StringUtils.EMPTY, reviewProductDetailVO.getProductCode());
    Assertions.assertEquals(Collections.singletonList(PRODUCT_ATTRIBUTE_VALUE), reviewProductDetailVO.getAttributes());
    Assertions.assertEquals(masterDataProductImage.getLocationPath(), reviewProductDetailVO.getImageUrl());
  }

  @Test
  public void getProductDetailForReviewByProductCodeWithItemSkuTest() throws Exception {

    Mockito.when(productSearchHelper
            .getProductMasterDataDetailByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE))
        .thenReturn(masterDataProductDetailResponse);
    ReviewProductDetailVO reviewProductDetailVO = this.searchServiceImpl
        .getProductDetailForReviewByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE, ITEM_SKU);
    Mockito.verify(productSearchHelper)
        .getProductMasterDataDetailByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, reviewProductDetailVO.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, reviewProductDetailVO.getProductName());
    Assertions.assertEquals(URL, reviewProductDetailVO.getProductUrl());
    Assertions.assertEquals(Collections.emptyList(), reviewProductDetailVO.getAttributes());
  }

  @Test
  public void testGetProductsByMerchantCodeAndBrands() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    Pageable pageableItems = PageRequest.of(0, Integer.MAX_VALUE);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    officialStoreRequestVO.setMaxPrice(Double.MAX_VALUE);
    officialStoreRequestVO.setMinPrice(0.0);
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    when(solrRepository.getProductsForOfficialStore(STORE_ID, officialStoreRequestVO, pageable)).thenReturn(
        new SolrGroupResultVO<>(Collections.singletonList(productAndItemSolr), pageable, 10));

    Page<ProductDetailVo> result =
        searchServiceImpl.getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO, pageable);
    assertTrue(CollectionUtils.isEmpty(result.getContent()));

    verify(solrRepository).getProductsForOfficialStore(STORE_ID, officialStoreRequestVO, pageable);
  }

  @Test
  public void
  testGetActiveProductsListForMerchantMppTrueTest() throws Exception {
    ReflectionTestUtils.setField(searchServiceImpl, "solrStringDelimiter", Constants.DELIMETER);
    Pageable pageable = PageRequest.of(0, 10);
    Pageable pageableItems = PageRequest.of(0, Integer.MAX_VALUE);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setBuyable(true);
    activeProductsRequestVO.setDiscoverable(true);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    List<ItemPickupPoint> items = getItemPickupPoints();
    List<ProductSolr> productSolrs = getProductSolr();
    productSolrs.forEach(productSolr -> productSolr.setVariantCount(2));

    when(solrRepository.getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable))).thenReturn(new SolrGroupResultVO<>(productSolrs, pageable, 10));
    when(itemPickupPointService.getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList())).thenReturn(
        ImmutableMap.<String, List<ItemPickupPoint>>of(PRODUCT_SKU_3, Arrays.asList(items.get(0)), PRODUCT_SKU_4,
            Arrays.asList(items.get(1)), PRODUCT_SKU_5, Arrays.asList(items.get(2)), PRODUCT_SKU_6,
            Arrays.asList(items.get(3))));

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);

    verify(solrRepository).getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable));
    verify(itemPickupPointService).getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList());

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void testGetActiveProductsListForMerchantMppTrueAndTradingProductTrueTest() throws Exception {
    ReflectionTestUtils.setField(searchServiceImpl, "solrStringDelimiter", Constants.DELIMETER);
    activeProductsRequestVO.setTradingProduct(true);
    Pageable pageable = PageRequest.of(0, 10);
    Pageable pageableItems = PageRequest.of(0, Integer.MAX_VALUE);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setBuyable(true);
    activeProductsRequestVO.setDiscoverable(true);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    List<ItemPickupPoint> items = getItemPickupPoints();
    List<ProductSolr> productSolrs = getProductSolr();
    productSolrs.forEach(productSolr -> productSolr.setVariantCount(2));

    when(solrRepository.getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable))).thenReturn(new SolrGroupResultVO<>(productSolrs, pageable, 10));
    when(itemPickupPointService.getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList())).thenReturn(
        ImmutableMap.<String, List<ItemPickupPoint>>of(PRODUCT_SKU_3, Arrays.asList(items.get(0)), PRODUCT_SKU_4,
            Arrays.asList(items.get(1)), PRODUCT_SKU_5, Arrays.asList(items.get(2)), PRODUCT_SKU_6,
            Arrays.asList(items.get(3))));

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);

    verify(solrRepository).getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable));
    verify(itemPickupPointService).getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList());

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void testGetActiveProductsListForMerchantMppTrueAndEmptyMerchantCodeTest() throws Exception {
    ReflectionTestUtils.setField(searchServiceImpl, "solrStringDelimiter", Constants.DELIMETER);
    activeProductsRequestVO.setTradingProduct(true);
    Pageable pageable = PageRequest.of(0, 10);
    Pageable pageableItems = PageRequest.of(0, Integer.MAX_VALUE);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setBuyable(true);
    activeProductsRequestVO.setDiscoverable(true);
    activeProductsRequestVO.setMerchantCode(null);
    List<ItemPickupPoint> items = getItemPickupPoints();
    List<ProductSolr> productSolrs = getProductSolr();
    productSolrs.forEach(productSolr -> productSolr.setVariantCount(2));

    when(solrRepository.getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable))).thenReturn(new SolrGroupResultVO<>(productSolrs, pageable, 10));
    when(itemPickupPointService.getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList())).thenReturn(
        ImmutableMap.<String, List<ItemPickupPoint>>of(PRODUCT_SKU_3, Arrays.asList(items.get(0)), PRODUCT_SKU_4,
            Arrays.asList(items.get(1)), PRODUCT_SKU_5, Arrays.asList(items.get(2)), PRODUCT_SKU_6,
            Arrays.asList(items.get(3))));

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);

    verify(solrRepository).getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable));
    verify(itemPickupPointService).getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList());

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }


  @Test
  public void testGetActiveProductsListForMerchantMppTrueTradingProductFalseTest() throws Exception {
    ReflectionTestUtils.setField(searchServiceImpl, "solrStringDelimiter", Constants.DELIMETER);
    Pageable pageable = PageRequest.of(0, 10);
    Pageable pageableItems = PageRequest.of(0, Integer.MAX_VALUE);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setBuyable(true);
    activeProductsRequestVO.setDiscoverable(true);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setTradingProduct(false);
    List<ItemPickupPoint> items = getItemPickupPoints();
    List<ProductSolr> productSolrs = getProductSolr();
    productSolrs.forEach(productSolr -> productSolr.setVariantCount(2));

    when(solrRepository.getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable))).thenReturn(new SolrGroupResultVO<>(productSolrs, pageable, 10));
    when(itemPickupPointService.getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList())).thenReturn(
        ImmutableMap.<String, List<ItemPickupPoint>>of(PRODUCT_SKU_3, Arrays.asList(items.get(0)), PRODUCT_SKU_4,
            Arrays.asList(items.get(1)), PRODUCT_SKU_5, Arrays.asList(items.get(2)), PRODUCT_SKU_6,
            Arrays.asList(items.get(3))));

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);

    verify(solrRepository).getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable));
    verify(itemPickupPointService).getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList());

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }
  @Test
  public void testGetActiveProductsListForMerchantMppTrueTradingProductTrueTest() throws Exception {
    ReflectionTestUtils.setField(searchServiceImpl, "solrStringDelimiter", Constants.DELIMETER);
    Pageable pageable = PageRequest.of(0, 10);
    Pageable pageableItems = PageRequest.of(0, Integer.MAX_VALUE);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setBuyable(true);
    activeProductsRequestVO.setDiscoverable(true);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setTradingProduct(true);
    List<ItemPickupPoint> items = getItemPickupPoints();
    List<ProductSolr> productSolrs = getProductSolr();
    productSolrs.forEach(productSolr -> productSolr.setVariantCount(2));

    when(solrRepository.getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable))).thenReturn(new SolrGroupResultVO<>(productSolrs, pageable, 10));
    when(itemPickupPointService.getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList())).thenReturn(
        ImmutableMap.<String, List<ItemPickupPoint>>of(PRODUCT_SKU_3, Arrays.asList(items.get(0)), PRODUCT_SKU_4,
            Arrays.asList(items.get(1)), PRODUCT_SKU_5, Arrays.asList(items.get(2)), PRODUCT_SKU_6,
            Arrays.asList(items.get(3))));

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);

    verify(solrRepository).getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable));
    verify(itemPickupPointService).getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList());

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void testGetProductsByMerchantCodeAndBrandsMerchantIdBlankExpectedThrowRuntimeException()
      throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    officialStoreRequestVO.setMerchantCodes(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  searchServiceImpl.getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO,
        pageable));
  }

  @Test
  public void testGetProductsByMerchantCodeAndBrandsStoreIdBlankExpectedThrowRuntimeException()
      throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  searchServiceImpl.getProductsForOfficialStore(null, CLIENT_ID, officialStoreRequestVO, pageable));
  }

  @Test
  public void testGetProductsByMerchantCodeAndBrandsWithImage() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    Pageable pageableItems = PageRequest.of(0, Integer.MAX_VALUE);
    officialStoreRequestVO.setMinPrice(0.0);
    officialStoreRequestVO.setMaxPrice(26000d);

    List<String> prices = new ArrayList<>();
    prices.add(ITEM_PRICE);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    productAndItemSolr.setItemImages(Collections.singletonList(ITEM_IMAGE));
    productAndItemSolr.setOfferPrice(prices);

    ProductAndItemSolr productAndItemSolr1 = new ProductAndItemSolr();
    productAndItemSolr1.setProductSku(PRODUCT_SKU);
    productAndItemSolr1.setItemImages(Collections.singletonList(ITEM_IMAGE));
    List<String> prices1 = new ArrayList<>();
    prices1.add(ITEM_PRICE_1);
    productAndItemSolr1.setOfferPrice(prices1);
    List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
    productAndItemSolrs.add(productAndItemSolr);
    productAndItemSolrs.add(productAndItemSolr1);
    when(solrRepository.getProductsForOfficialStore(STORE_ID, officialStoreRequestVO, pageable)).thenReturn(
        new SolrGroupResultVO<>(productAndItemSolrs, pageable, 10));

    Page<ProductDetailVo> result =
        searchServiceImpl.getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO, pageable);
    verify(solrRepository).getProductsForOfficialStore(STORE_ID, officialStoreRequestVO, pageable);
    assertTrue(result.getContent().size() == 1);
    assertEquals(result.getContent().get(0).getProductSku(), PRODUCT_SKU);
    assertTrue(result.getContent().get(0).getMaxPrice() == 25000d);
    assertTrue(result.getContent().get(0).getMinPrice() == 23000d);

  }

  @Test
  public void testGetProductsByMerchantCodeWithoutProductFound() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    Pageable pageableItems = PageRequest.of(0, 10);

    when(solrRepository.getProductsForOfficialStore(STORE_ID, officialStoreRequestVO, pageable)).thenReturn(
        new SolrGroupResultVO<>(Collections.emptyList(), pageable, 10));

    Page<ProductDetailVo> result =
        searchServiceImpl.getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO, pageable);
    assertEquals(result.getContent(),Collections.emptyList());

    verify(solrRepository).getProductsForOfficialStore(STORE_ID, officialStoreRequestVO, pageable);
  }

  @Test
  public void publishAllItemSkusInBatchByMerchantCodeAndCreatedDateLessThanTest() {
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ITEM_SKU);
    Date voucherCreatedDate = new Date();
    PageRequest pageRequest = PageRequest.of(0, 100);
    when(solrRepository.getItemsByMerchantCode(
        eq(STORE_ID), eq(MERCHANT_CODE), eq(pageRequest)))
        .thenReturn(solrPage);
    searchServiceImpl.getItemsByMerchantCode(STORE_ID, MERCHANT_CODE,
        voucherCreatedDate, pageRequest);
    verify(solrRepository).getItemsByMerchantCode(eq(STORE_ID),
        eq(MERCHANT_CODE), eq(pageRequest));
  }

  @Test
  public void testGetActiveProductsListForSuspension() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setStatus(STATUS);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    Map<String, Long> productSku = new HashMap<>();
    productSku.put(PRODUCT_SKU, Long.valueOf(1));
    when(solrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(productAndItemSolr), pageable, 10));
    when(cacheItemHelperService.getCacheableItemsByProductSkusForSuspensionList(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(Arrays.asList(item));

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForSuspension(STORE_ID, activeProductsRequestVO, pageable);

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    verify(solrRepository)
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    verify(cacheItemHelperService).getCacheableItemsByProductSkusForSuspensionList(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void testGetActiveProductsListForSuspensionV2() throws Exception {
    ReflectionTestUtils.setField(searchServiceImpl, "suspensionListingV2QueryEnabled", true);
    Pageable pageable = PageRequest.of(0, 10);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setStatus(STATUS);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    Map<String, Long> productSku = new HashMap<>();
    productSku.put(PRODUCT_SKU, Long.valueOf(1));
    when(solrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatusV2(STORE_ID, activeProductsRequestVO, pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(productAndItemSolr), pageable, 10));
    when(cacheItemHelperService.getCacheableItemsByProductSkusForSuspensionList(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(Arrays.asList(item));

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForSuspension(STORE_ID, activeProductsRequestVO, pageable);

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    verify(solrRepository)
        .getProductsByMerchantCodeAndCategoryCodesAndStatusV2(STORE_ID, activeProductsRequestVO, pageable);
    verify(cacheItemHelperService).getCacheableItemsByProductSkusForSuspensionList(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void testGetActiveProductsListForSuspension_forActiveProduct() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setSuspended(true);
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setStatus(STATUS);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    Item item2 = new Item();
    item2.setProductSku(PRODUCT_SKU);
    item2.setItemCode(ITEM_SKU2);
    item2.setItemSku(ITEM_SKU2);
    itemList.add(item2);
    Map<String, Long> productSku = new HashMap<>();
    productSku.put(PRODUCT_SKU, Long.valueOf(1));
    when(solrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(productAndItemSolr), pageable, 10));
    when(cacheItemHelperService.getCacheableItemsByProductSkusForSuspensionList(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(itemList);

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForSuspension(STORE_ID, activeProductsRequestVO, pageable);

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    verify(solrRepository)
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    verify(cacheItemHelperService).getCacheableItemsByProductSkusForSuspensionList(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void testGetActiveProductsListForSuspension_WhenNoSkuInItemCache() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setSuspended(true);
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setStatus(STATUS);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    Item item2 = new Item();
    item2.setProductSku(PRODUCT_SKU);
    item2.setItemCode(ITEM_SKU2);
    item2.setItemSku(ITEM_SKU2);
    itemList.add(item2);
    Map<String, Integer> productSku = new HashMap<>();
    productSku.put(PRODUCT_SKU, 1);
    when(solrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(productAndItemSolr), pageable, 10));
    when(cacheItemHelperService.getCacheableItemsByProductSkusForSuspensionList(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(new ArrayList<>());
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(itemList);

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForSuspension(STORE_ID, activeProductsRequestVO, pageable);

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    verify(solrRepository)
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    verify(cacheItemHelperService).getCacheableItemsByProductSkusForSuspensionList(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_SKU)));
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testGetSuspendedItemList() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    activeProductsRequestVO.setNameKey(KEY);
    activeProductsRequestVO.setSortType(SolrQuery.ORDER.desc.toString());
    when(solrRepository.getItemsByMerchantCodeAndCategoryCodes(STORE_ID, activeProductsRequestVO, pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(productAndItemSolr), pageable, 10));

    Page<ItemInfoVO> result = searchServiceImpl.getSuspendedItemList(STORE_ID, activeProductsRequestVO, pageable);

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    verify(solrRepository).getItemsByMerchantCodeAndCategoryCodes(STORE_ID, activeProductsRequestVO, pageable);
  }

  @Test
  public void testGetSuspendedItemImage() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemImages(Arrays.asList(ITEM_IMAGE, ITEM_IMAGE_1));
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    activeProductsRequestVO.setNameKey(KEY);
    activeProductsRequestVO.setSortType(SolrQuery.ORDER.desc.toString());
    when(solrRepository.getItemsByMerchantCodeAndCategoryCodes(STORE_ID, activeProductsRequestVO, pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(productAndItemSolr), pageable, 10));

    Page<ItemInfoVO> result = searchServiceImpl.getSuspendedItemList(STORE_ID, activeProductsRequestVO, pageable);

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    assertEquals(result.getContent().get(0).getImageUrl(), DUMMY_NAME);
    verify(solrRepository).getItemsByMerchantCodeAndCategoryCodes(STORE_ID, activeProductsRequestVO, pageable);
  }

  @Test
  public void getProductAndItemsInfoForAllItemsTest() throws Exception {
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(new Product(), Stream.of(new Item()).collect(toList()));
    Set<String> itemSkus = Stream.of(ITEM_SKU).collect(toSet());
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUESTID, USERNAME, USERNAME);
    when(productService.getProductAndItemsByItemSkusForAllItems(STORE_ID, REQUESTID, USERNAME, itemSkus, true, true, false))
        .thenReturn(Stream.of(productAndItemsVO).collect(toList()));
    searchServiceImpl.getProductAndItemsInfoForAllItems(mandatoryRequestParam,
        itemSkus, true, true, Boolean.FALSE, true, false);
    verify(productService).getProductAndItemsByItemSkusForAllItems(STORE_ID, REQUESTID, USERNAME, itemSkus,
        true, true, false);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REVERT_TRANSACTION_API_SWITCH);
  }

  @Test
  public void getProductAndItemsInfoForAllItemsWithPristineTest() throws Exception {
    Set<String> activePromoBundlingList = new HashSet<>();
    activePromoBundlingList.add("WHOLESALE");

    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributes = new LinkedHashMap<>();
    attributes.put("Warna", "BLUE");
    attributes.put("Kapasitas Memori", "8GB");


    Product product = new Product();
    Item item = new Item();
    Map<String, String> pristineListingAttributes = new LinkedHashMap<>();
    pristineListingAttributes.put("color", "BLUE");
    pristineListingAttributes.put("rom", "8GB");
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(ProductSearchServiceImplTest.PRISTINE_ID);
    pristineDataItem.setPristineMasterId(ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory(PRISTINE_CATEGORY);
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku(ITEM_SKU);
    item.setPromoBundling(Boolean.TRUE);
    item.setActivePromoBundlings(activePromoBundlingList);

    Calendar startDate = Calendar.getInstance();
    startDate.add(Calendar.DATE, -7);
    Calendar endDate = Calendar.getInstance();
    endDate.add(Calendar.DAY_OF_MONTH, 5);

    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(5000.0);

    List<DiscountPrice> discountPrices = new ArrayList<>();
    discountPrices.add(discountPrice);

    Price price = new Price();
    price.setChannel("web");
    price.setOfferPrice(100000.0);
    price.setListPrice(10000.0);
    price.setListOfDiscountPrices(discountPrices);
    price.setCurrency("currency");

    Set<Price> prices = new HashSet<>();
    prices.add(price);

    item.setPrice(prices);
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(product, Stream.of(item).collect(toList()));
    Set<String> itemSkus = Stream.of(ProductSearchServiceImplTest.ITEM_SKU).collect(toSet());
    ProductAttribute productAttribute = new ProductAttribute();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    pristineItemAndSiblingsVO.setSiblings(Arrays.asList(pristineDataItem));

    Map<String,String> expectedListingAttributes = new HashMap<>();
    expectedListingAttributes.put("Kapasitas Memori", "8GB");
    expectedListingAttributes.put("Warna", "BLUE");

    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUESTID, USERNAME, USERNAME);
    SimpleSetStringRequest simpleSetStringRequest = new SimpleSetStringRequest();
    simpleSetStringRequest.setValue(itemSkus);

    when(productAttributesUtil.getCategoryListingParameterKey(eq(item.getPristineDataItem())))
        .thenReturn(categoryListingParameters);
    when(productAttributesUtil.translatePristineListingAttributeName(
        eq(item.getPristineDataItem().getPristineListingAttributes()),
        Mockito.any(String[].class))).thenReturn(attributes);

    when(productService.getProductAndItemsByItemSkusForAllItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false)).thenReturn(Stream.of(productAndItemsVO).collect(toList()));
    when(this.itemPriceService.getFinalPrice(price, 10.0)).thenReturn(5000.0);
    when(pristineCacheableService.
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    when(objectConverterService.convertPristineAttributeToProductAttribute(pristineDataItem))
        .thenReturn(productAttribute);
    when(promotionOutbound.getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        itemSkus)).thenReturn(activePromoBundlingResponseVO);
    Item item1 = new Item();
    item1.setPristineDataItem(pristineDataItem);
    when(pristineCacheableService.findFirstItemByPristine(Mockito.any(PristineDataItem.class))).thenReturn(new Item());
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(Mockito.any(Item.class))).thenReturn(item1);
    List<ProductAndItemsVO> result = searchServiceImpl.getProductAndItemsInfoForAllItems(mandatoryRequestParam,
        itemSkus, true, true, Boolean.TRUE, true, false);
    assertEquals(result.get(0).getItems().get(0).getWholesaleRules().get(0), wholesaleRuleVO);
    assertEquals(result.get(0).getItems().get(0).getPristineDataItem()
        .getPristineListingAttributes(), expectedListingAttributes);

    verify(productService).getProductAndItemsByItemSkusForAllItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false);
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(item.getPristineDataItem()));
    verify(productAttributesUtil).translatePristineListingAttributeName(
        eq(pristineListingAttributes), Mockito.any(String[].class));
    verify(this.itemPriceService).getFinalPrice(price, 10.0);
    verify(pristineCacheableService).
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId());
    verify(objectConverterService).convertPristineAttributeToProductAttribute(pristineDataItem);
    verify(promotionOutbound)
        .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
            itemSkus);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REVERT_TRANSACTION_API_SWITCH);
    verify(pristineCacheableService).findFirstItemByPristine(Mockito.any(PristineDataItem.class));
    verify(masterDataConstructorService, Mockito.times(2)).constructPristineDataItemWithMasterData(Mockito.any(Item.class));
  }

  @Test
  public void getProductAndItemsInfoForAllItemsWithPristine_WhenPristineDataItemNullTest() throws Exception {
    Set<String> activePromoBundlingList = new HashSet<>();
    Product product = new Product();
    Item item = new Item();
    item.setPristineDataItem(null);
    item.setActivePromoBundlings(activePromoBundlingList);

    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(product, Stream.of(item).collect(toList()));
    Set<String> itemSkus = Stream.of(ProductSearchServiceImplTest.ITEM_SKU).collect(toSet());
    when(productService.getProductAndItemsByItemSkusForAllItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false)).thenReturn(Stream.of(productAndItemsVO).collect(toList()));

    searchServiceImpl.getProductAndItemsInfoForAllItems(MandatoryRequestParam
            .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUESTID, USERNAME, USERNAME), itemSkus,
        true,true, true, true, false);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REVERT_TRANSACTION_API_SWITCH);
    verify(productService).getProductAndItemsByItemSkusForAllItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false);
  }

  @Test
  public void updateForceReviewForProductTest() {
    Product product = new Product();
    Mockito.when(this.productService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.doNothing().when(productAndItemSolrIndexerService).takeDownL3(product);
    when(productService.saveProductWithoutUpdatingSolr(product, new ArrayList<>(), StringUtils.EMPTY)).thenReturn(product);
    searchServiceImpl.updateForceReviewForProduct(STORE_ID, Collections.singletonList(item), false, false);
    Mockito.verify(this.productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.productService).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.FORCE_REVIEW_FLAG_CHANGE), StringUtils.EMPTY);
  }

  @Test
  public void updateForceReviewForProductArchiveTest() {
    Product product = new Product();
    Mockito.when(this.productService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.doNothing().when(productAndItemSolrIndexerService).takeDownL3(product);
    when(productService.saveProductWithoutUpdatingSolr(product, new ArrayList<>(), StringUtils.EMPTY)).thenReturn(product);
    searchServiceImpl.updateForceReviewForProduct(STORE_ID, Collections.singletonList(item), false, true);
    Mockito.verify(this.productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.productService).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.FORCE_REVIEW_FLAG_CHANGE), StringUtils.EMPTY);
  }

  @Test
  public void updateForceReviewForProductNullTest() {
    searchServiceImpl.updateForceReviewForProduct(STORE_ID, Collections.singletonList(item), false, false);
    Mockito.verify(this.productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testGetActiveProductsListForSuspension_duplicateSolrDocument() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    List<ProductAndItemSolr> productAndItemSolrList = new ArrayList<>();
    productAndItemSolrList.add(productAndItemSolr);
    productAndItemSolrList.add(productAndItemSolr);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setStatus(STATUS);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    Map<String, Long> productSku = new HashMap<>();
    productSku.put(PRODUCT_SKU, Long.valueOf(1));
    when(solrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable))
        .thenReturn(new PageImpl<>(productAndItemSolrList, pageable, 10));
    when(cacheItemHelperService.getCacheableItemsByProductSkusForSuspensionList(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(Arrays.asList(item));

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForSuspension(STORE_ID, activeProductsRequestVO, pageable);

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    verify(solrRepository)
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    verify(cacheItemHelperService).getCacheableItemsByProductSkusForSuspensionList(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void findMasterDataWithProductAndItemsInfoByItemCodeWithMppTrueTest() throws Exception {
    Mockito.when(this.itemService.getItemsByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(new Product()));
    Mockito.when(
            this.productSearchHelper.getProductAndItemsWithMasterDataDetailWithoutL5Details(STORE_ID, USERNAME, REQUESTID,
                Arrays.asList(new Product()), Arrays.asList(item)))
        .thenReturn(masterDataDetailWithProductAndItemsResponseVo);
    Mockito.doNothing().when(this.productSearchHelper).setItemCatalogs(STORE_ID, USERNAME, REQUESTID, true,
        masterDataDetailWithProductAndItemsResponseVo.getProductAndItems(),
        masterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts());
    MasterDataDetailWithProductAndItemsResponseVo result =
        this.searchServiceImpl.findMasterDataWithProductAndItemsInfoByItemCode(STORE_ID, USERNAME, REQUESTID, ITEM_CODE,
            true);
    Mockito.verify(this.itemService).getItemsByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.productSearchHelper)
        .getProductAndItemsWithMasterDataDetailWithoutL5Details(STORE_ID, USERNAME, REQUESTID,
            Arrays.asList(new Product()), Arrays.asList(item));
    Mockito.verify(this.productSearchHelper).setItemCatalogs(STORE_ID, USERNAME, REQUESTID, true,
        masterDataDetailWithProductAndItemsResponseVo.getProductAndItems(),
        masterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts());
    Assertions.assertEquals(PRODUCT_NAME, result.getMasterDataProducts().get(PRODUCT_CODE).getProductName());
    Assertions.assertEquals(PRODUCT_SKU, result.getProductAndItems().get(0).getItems().get(0).getProductSku());
  }

  @Test
  public void getPristineProductAndItemsInfoByItemSkuWhenPristineBrandNotNull() throws Exception {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    List<ProductAttribute> attributes = Arrays.asList(new ProductAttribute());
    product.setDefiningAttributes(attributes);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(product.getProductCode());
    List<Product> productResult = Arrays.asList(product);
    List<Item> itemList = createItemDataForComputerCategory();
    itemList.get(0).getPristineDataItem().setPristineBrand("Brand");
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    productAndItem.setItems(itemList);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);

    MasterDataDetailWithProductAndItemsResponseVo response =
        new MasterDataDetailWithProductAndItemsResponseVo();
    response.setProductAndItems(productAndItems);
    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributesMap = new LinkedHashMap<>();
    attributesMap.put("Warna", "black");
    attributesMap.put("Kapasitas Memori", "32GB");

    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(categoryListingParameters);
    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);
    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false)).thenReturn(itemList.get(0));
    when(this.productCacheHelperService
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString())).thenReturn(product);
    when(this.productSearchHelper.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(),
        productCodes)).thenReturn(response);
    when(itemService.getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList))
        .thenReturn(Collections.singletonList(itemList.get(0)));
    this.searchServiceImpl.getPristineProductAndItemsInfoByItemSku(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.ITEM_SKU, true, true, false);
    verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false);
    verify(itemService).getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList);
    verify(this.productCacheHelperService)
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString());
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(itemList.get(0).getPristineDataItem()));
    verify(productAttributesUtil, times(2)).getAttributeNameTranslationMap();
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(), productCodes);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        response.getProductAndItems(), response.getMasterDataProducts());
    verify(this.productHelperService).findAndConstructOfflineItems(ProductSearchServiceImplTest.STORE_ID, itemList);
    Assertions.assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineMasterId(), ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    Assertions.assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineId(), PRISTINE_ID);
  }

  @Test
  public void getPristineProductAndItemsInfoByItemSkuWhenPristineModlNotNull() throws Exception {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    List<ProductAttribute> attributes = Arrays.asList(new ProductAttribute());
    product.setDefiningAttributes(attributes);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(product.getProductCode());
    List<Product> productResult = Arrays.asList(product);
    List<Item> itemList = createItemDataForComputerCategory();
    itemList.get(0).getPristineDataItem().setPristineModel("model");
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    productAndItem.setItems(itemList);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);

    MasterDataDetailWithProductAndItemsResponseVo response =
        new MasterDataDetailWithProductAndItemsResponseVo();
    response.setProductAndItems(productAndItems);
    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributesMap = new LinkedHashMap<>();
    attributesMap.put("Warna", "black");
    attributesMap.put("Kapasitas Memori", "32GB");

    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(categoryListingParameters);
    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);
    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false)).thenReturn(itemList.get(0));
    when(this.productCacheHelperService
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString())).thenReturn(product);
    when(this.productSearchHelper.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(),
        productCodes)).thenReturn(response);
    when(itemService.getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList))
        .thenReturn(Collections.singletonList(itemList.get(0)));
    this.searchServiceImpl.getPristineProductAndItemsInfoByItemSku(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.ITEM_SKU, true, true, false);
    verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false);
    verify(itemService).getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList);
    verify(this.productCacheHelperService)
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString());
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(itemList.get(0).getPristineDataItem()));
    verify(productAttributesUtil, times(2)).getAttributeNameTranslationMap();
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(), productCodes);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        response.getProductAndItems(), response.getMasterDataProducts());
    verify(this.productHelperService).findAndConstructOfflineItems(ProductSearchServiceImplTest.STORE_ID, itemList);
    Assertions.assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineMasterId(), ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    Assertions.assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineId(), PRISTINE_ID);
  }

  @Test
  public void getPristineProductAndItemsInfoByItemSkuWithProperPristine() throws Exception {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    List<ProductAttribute> attributes = Arrays.asList(new ProductAttribute());
    product.setDefiningAttributes(attributes);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(product.getProductCode());
    List<Product> productResult = Arrays.asList(product);
    List<Item> itemList = createItemDataForComputerCategory();
    itemList.get(0).getPristineDataItem().setPristineModel("model");
    itemList.get(0).getPristineDataItem().setPristineBrand("Brand");
    ProductAndItemsVO productAndItem = new ProductAndItemsVO();
    productAndItem.setItems(itemList);
    productAndItem.setProduct(product);
    List<ProductAndItemsVO> productAndItems = Arrays.asList(productAndItem);

    MasterDataDetailWithProductAndItemsResponseVo response =
        new MasterDataDetailWithProductAndItemsResponseVo();
    response.setProductAndItems(productAndItems);
    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributesMap = new LinkedHashMap<>();
    attributesMap.put("Warna", "black");
    attributesMap.put("Kapasitas Memori", "32GB");

    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(categoryListingParameters);
    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);
    when(this.itemCacheableService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false)).thenReturn(itemList.get(0));
    when(this.productCacheHelperService
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString())).thenReturn(product);
    when(this.productSearchHelper.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(),
        productCodes)).thenReturn(response);
    when(itemService.getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList))
        .thenReturn(Collections.singletonList(itemList.get(0)));
    this.searchServiceImpl.getPristineProductAndItemsInfoByItemSku(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.ITEM_SKU, true, true, false);
    verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ProductSearchServiceImplTest.STORE_ID,
            "itemSku", true, false, false);
    verify(itemService).getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, itemList);
    verify(this.productCacheHelperService)
        .findProductByStoreIdAndProductSku(Mockito.eq(STORE_ID), anyString());
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(itemList.get(0).getPristineDataItem()));
    verify(productAttributesUtil, times(2)).getAttributeNameTranslationMap();
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchServiceImplTest.STORE_ID, ProductSearchServiceImplTest.USERNAME,
        ProductSearchServiceImplTest.REQUESTID, itemList, productResult, product.getProductCode(), productCodes);
    verify(this.productSearchHelper).setItemCatalogs(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID, true,
        response.getProductAndItems(), response.getMasterDataProducts());
    verify(this.productHelperService).findAndConstructOfflineItems(ProductSearchServiceImplTest.STORE_ID, itemList);
    Assertions.assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineMasterId(), ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    Assertions.assertEquals(productAndItems.get(0).getItems().get(0).getPristineDataItem().getPristineId(), PRISTINE_ID);
  }

  @Test
  public void getProductAndItemsInfoForActiveItemWithNullPristineTest() throws Exception {
    Set<String> activePromoBundlingList = new HashSet<>();
    activePromoBundlingList.add("WHOLESALE");

    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributes = new LinkedHashMap<>();
    attributes.put("Warna", "BLUE");
    attributes.put("Kapasitas Memori", "8GB");


    Product product = new Product();
    Item item = new Item();
    Map<String, String> pristineListingAttributes = new LinkedHashMap<>();
    pristineListingAttributes.put("color", "BLUE");
    pristineListingAttributes.put("rom", "8GB");
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(ProductSearchServiceImplTest.PRISTINE_ID);
    pristineDataItem.setPristineMasterId(ProductSearchServiceImplTest.PRISTINE_MASTER_ID);
    pristineDataItem.setPristineCategory(PRISTINE_CATEGORY);
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku(ITEM_SKU);
    item.setPromoBundling(Boolean.TRUE);
    item.setActivePromoBundlings(activePromoBundlingList);

    Calendar startDate = Calendar.getInstance();
    startDate.add(Calendar.DATE, -7);
    Calendar endDate = Calendar.getInstance();
    endDate.add(Calendar.DAY_OF_MONTH, 5);

    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(5000.0);

    List<DiscountPrice> discountPrices = new ArrayList<>();
    discountPrices.add(discountPrice);

    Price price = new Price();
    price.setChannel("web");
    price.setOfferPrice(100000.0);
    price.setListPrice(10000.0);
    price.setListOfDiscountPrices(discountPrices);
    price.setCurrency("currency");

    Set<Price> prices = new HashSet<>();
    prices.add(price);

    item.setPrice(prices);
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(product, Stream.of(item).collect(toList()));
    Set<String> itemSkus = Stream.of(ProductSearchServiceImplTest.ITEM_SKU).collect(toSet());
    ProductAttribute productAttribute = new ProductAttribute();
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    pristineItemAndSiblingsVO.setSiblings(Arrays.asList(pristineDataItem));

    Map<String,String> expectedListingAttributes = new HashMap<>();
    expectedListingAttributes.put("Kapasitas Memori", "8GB");
    expectedListingAttributes.put("Warna", "BLUE");

    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUESTID, USERNAME, USERNAME);
    SimpleSetStringRequest simpleSetStringRequest = new SimpleSetStringRequest();
    simpleSetStringRequest.setValue(itemSkus);

    when(productAttributesUtil.getCategoryListingParameterKey(eq(item.getPristineDataItem())))
        .thenReturn(categoryListingParameters);
    when(productAttributesUtil.translatePristineListingAttributeName(
        eq(item.getPristineDataItem().getPristineListingAttributes()),
        Mockito.any(String[].class))).thenReturn(attributes);

    when(productService.getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true)).thenReturn(Stream.of(productAndItemsVO).collect(toList()));
    when(this.itemPriceService.getFinalPrice(price, 10.0)).thenReturn(5000.0);
    when(pristineCacheableService.
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    when(objectConverterService.convertPristineAttributeToProductAttribute(pristineDataItem))
        .thenReturn(productAttribute);
    when(promotionOutbound.getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
        itemSkus)).thenReturn(activePromoBundlingResponseVO);
    Item item1 = new Item();
    item1.setPristineDataItem(pristineDataItem);
    when(pristineCacheableService.findFirstItemByPristine(Mockito.any(PristineDataItem.class))).thenReturn(null);
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(Mockito.any(Item.class))).thenReturn(item1);

    List<ProductItemsVo> result =
      searchServiceImpl.getProductAndItemsInfoForActiveItem(mandatoryRequestParam,
        itemSkus, true, true, Boolean.TRUE, true, false);
    assertEquals(result.get(0).getItemVoList().get(0).getWholesaleRules().get(0), wholesaleRuleVO);
    assertEquals(result.get(0).getItemVoList().get(0).getPristineDataItem()
        .getPristineListingAttributes(), expectedListingAttributes);

    verify(productService).getProductAndItemsByItemSkusForActiveItems(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.REQUESTID, ProductSearchServiceImplTest.USERNAME, itemSkus,
        true, true, false, true);
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(item.getPristineDataItem()));
    verify(productAttributesUtil).translatePristineListingAttributeName(
        eq(pristineListingAttributes), Mockito.any(String[].class));
    verify(this.itemPriceService).getFinalPrice(price, 10.0);
    verify(pristineCacheableService).
        findPristineItemAndItsSiblingsByPristineId(ProductSearchServiceImplTest.STORE_ID,
        item.getPristineDataItem().getPristineId());
    verify(promotionOutbound)
        .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE,
            itemSkus);
    verify(pristineCacheableService).findFirstItemByPristine(Mockito.any(PristineDataItem.class));
    verify(masterDataConstructorService).constructPristineDataItemWithMasterData(Mockito.any(Item.class));
    Mockito.verify(this.productService).checkProductAndItemsForForceReview(
      Collections.singletonList(productAndItemsVO.getProduct()), productAndItemsVO.getItems());
    Mockito.verify(this.productService).setSellerPromoBundlings(eq(STORE_ID),
            Mockito.anyList());
  }

  @Test
  public void getProductDetailForReviewByProductCodeNullTest() throws Exception {
    masterDataProductDetailResponse.put(PRODUCT_CODE, null);
    Mockito.when(productSearchHelper
            .getProductMasterDataDetailByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE))
        .thenReturn(masterDataProductDetailResponse);
    ReviewProductDetailVO reviewProductDetailVO = this.searchServiceImpl
        .getProductDetailForReviewByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE, null);
    Mockito.verify(productSearchHelper)
        .getProductMasterDataDetailByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE);
  }

  @Test
  public void getProductAndItemsResponseByPristineIdNullItemTest() throws Exception {
    List<Item> itemList = createItemDataForComputerCategory();
    PristineItemVO pristineItemVo = new PristineItemVO();
    pristineItemVo.setItemSku(itemList.get(0).getItemSku());
    PristineProductAndItemsResponseVO response =
        new PristineProductAndItemsResponseVO();
    Map<String, String> attributes = new LinkedHashMap<>();
    attributes.put("Warna", "WHITE");
    attributes.put("Kapasitas Memori", "8 GB");

    when(masterDataConstructorService.constructPristineDataItemWithMasterData(itemList.get(0))).thenReturn(itemList.get(0));
    when(productAttributesUtil.getCategoryListingParameterKey(
        eq(itemList.get(0).getPristineDataItem()))).thenReturn(null);
    when(productAttributesUtil.getAttributeNameTranslationMap()).thenReturn(attributeNameTranslationMap);
    when(this.itemCacheableService
        .findItemsByPristineId(ProductSearchServiceImplTest.STORE_ID, USERNAME, REQUESTID,
            PRISTINE_ID, false)).thenReturn(itemList);
    when(objectConverterService.convertToPristineItem(itemList.get(0))).thenReturn(pristineItemVo);
    response = this.searchServiceImpl.getProductAndItemsResponseByPristineId(ProductSearchServiceImplTest.STORE_ID,
        ProductSearchServiceImplTest.USERNAME, ProductSearchServiceImplTest.REQUESTID,
        ProductSearchServiceImplTest.PRISTINE_ID, false);
    verify(this.itemCacheableService)
        .findItemsByPristineId(ProductSearchServiceImplTest.STORE_ID, USERNAME, REQUESTID,
            PRISTINE_ID, false);
    verify(this.productHelperService).findAndConstructOfflineItems(ProductSearchServiceImplTest.STORE_ID, itemList);
    verify(objectConverterService).convertToPristineItem(itemList.get(0));
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(itemList.get(0).getPristineDataItem()));
    verify(masterDataConstructorService).constructPristineDataItemWithMasterData(itemList.get(0));
    Assertions.assertEquals(1,response.getPristineItems().size());
    Assertions.assertEquals("itemSku", response.getPristineItems().get(0).getItemSku());
  }

  @Test
  public void getItemPriceByPristineIdTest() throws IOException, SolrServerException {
    Set<String> itemSkus = new HashSet<>(Arrays.asList(ITEM_SKU));
    List<Item> items = createItemDataForComputerCategory();
    Map<String, Set<Price>> itemPrice = new HashMap<>();
    itemPrice.put(ITEM_SKU, items.get(0).getPrice());
    when(itemService.isItemBuyableAndDiscoverable(items.get(0))).thenReturn(true);
    when(itemService.getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, items)).thenReturn(items);
    when(itemService.getItemsByPristineId(STORE_ID, PRISTINE_ID)).thenReturn(items);

    List<ItemPriceVO> itemPriceVOS =
        searchServiceImpl.getItemPriceByPristineId(STORE_ID, USERNAME, REQUESTID, PRISTINE_ID);

    verify(itemService).getItemsByPristineId(STORE_ID, PRISTINE_ID);
    verify(itemService).isItemBuyableAndDiscoverable(items.get(0));
    verify(itemService).getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, items);
    Assertions.assertEquals(ITEM_SKU, itemPriceVOS.get(0).getItemSku());
    Assertions.assertEquals(MERCHANT_CODE, itemPriceVOS.get(0).getMerchantCode());
  }

  @Test
  public void getItemPriceByPristineIdPriceNullTest() throws IOException, SolrServerException {
    Set<String> itemSkus = new HashSet<>(Arrays.asList(ITEM_SKU));
    List<Item> items = createItemDataForComputerCategory();
    items.get(0).setPrice(new HashSet<>(Arrays.asList(new Price())));
    Map<String, Set<Price>> itemPrice = new HashMap<>();
    itemPrice.put(ITEM_SKU, items.get(0).getPrice());
    when(itemService.isItemBuyableAndDiscoverable(items.get(0))).thenReturn(true);
    when(itemService.getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, items)).thenReturn(items);
    when(itemService.getItemsByPristineId(STORE_ID, PRISTINE_ID)).thenReturn(items);

    List<ItemPriceVO> itemPriceVOS =
        searchServiceImpl.getItemPriceByPristineId(STORE_ID, USERNAME, REQUESTID, PRISTINE_ID);

    verify(itemService).getItemsByPristineId(STORE_ID, PRISTINE_ID);
    verify(itemService).isItemBuyableAndDiscoverable(items.get(0));
    verify(itemService).getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUESTID, items);
    Assertions.assertEquals(ITEM_SKU, itemPriceVOS.get(0).getItemSku());
    Assertions.assertEquals(MERCHANT_CODE, itemPriceVOS.get(0).getMerchantCode());
  }

  @Test
  public void getItemPriceByPristineIdNoItemSkusTest() throws IOException, SolrServerException {
    when(itemService.getItemsByPristineId(STORE_ID, PRISTINE_ID)).thenReturn(new ArrayList<>());
    List<ItemPriceVO> itemPriceVOS =
        searchServiceImpl.getItemPriceByPristineId(STORE_ID, USERNAME, REQUESTID, PRISTINE_ID);
    verify(itemService).getItemsByPristineId(STORE_ID, PRISTINE_ID);
    Assertions.assertTrue(itemPriceVOS.size() == 0);
  }

  @Test
  public void getItemPriceByPristineIdStoreIdNullTest() throws IOException, SolrServerException {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  searchServiceImpl.getItemPriceByPristineId(null, USERNAME, REQUESTID, PRISTINE_ID));
  }

  @Test
  public void getItemPriceByPristineIdNullTest() throws IOException, SolrServerException {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  searchServiceImpl.getItemPriceByPristineId(STORE_ID, USERNAME, REQUESTID, null));
  }

  @Test
  public void getProductCountByTypeActiveTest() {
    ProductCountResponseVo productCountResponseVo = new ProductCountResponseVo();
    productCountResponseVo.setActive(10L);
    productCountResponseVo.setOutOfStock(5L);
    when(productSolrRepository.getActiveAndOosProductCount(MERCHANT_CODE)).thenReturn(productCountResponseVo);
    searchServiceImpl.getProductCountByType(Constants.ACTIVE, MERCHANT_CODE, true);
    verify(productSolrRepository).getActiveAndOosProductCount(MERCHANT_CODE);
    Assertions.assertEquals(10L, productCountResponseVo.getActive().longValue());
    Assertions.assertEquals(5L, productCountResponseVo.getOutOfStock().longValue());
  }

  @Test
  public void getProductCountByTypeInActiveTest() {
    ProductCountResponseVo productCountResponseVo = new ProductCountResponseVo();
    productCountResponseVo.setActive(10L);
    productCountResponseVo.setOutOfStock(5L);
    productCountResponseVo.setArchived(2L);
    productCountResponseVo.setSuspended(0L);
    when(productSolrRepository.getActiveAndOosProductCount(MERCHANT_CODE)).thenReturn(productCountResponseVo);
    when(productSolrRepository.getSuspendedAndArchivedProductCount(MERCHANT_CODE, new ProductCountResponseVo()))
        .thenReturn(productCountResponseVo);
    searchServiceImpl.getProductCountByType("IN_ACTIVE", MERCHANT_CODE, true);
    verify(productSolrRepository).getActiveAndOosProductCount(MERCHANT_CODE);
    verify(productSolrRepository).getSuspendedAndArchivedProductCount(MERCHANT_CODE, productCountResponseVo);
    Assertions.assertEquals(10L, productCountResponseVo.getActive().longValue());
    Assertions.assertEquals(5L, productCountResponseVo.getOutOfStock().longValue());
    Assertions.assertEquals(2L, productCountResponseVo.getArchived().longValue());
    Assertions.assertEquals(0L, productCountResponseVo.getSuspended().longValue());
  }


  @Test
  public void getProductCountByTypeCacheableTest() {
    ProductCountResponseVo productCountResponseVo = new ProductCountResponseVo();
    productCountResponseVo.setActive(10L);
    productCountResponseVo.setOutOfStock(5L);
    when(productSolrRepository.getActiveAndOosProductCount(MERCHANT_CODE)).thenReturn(productCountResponseVo);
    searchServiceImpl.getProductCountByTypeCacheable(Constants.ACTIVE, MERCHANT_CODE, true);
    verify(productSolrRepository).getActiveAndOosProductCount(MERCHANT_CODE);
    Assertions.assertEquals(10L, productCountResponseVo.getActive().longValue());
    Assertions.assertEquals(5L, productCountResponseVo.getOutOfStock().longValue());
  }


  @Test
  public void getProductCountByTypeInActiveFalseTest() {
    ProductCountResponseVo productCountResponseVo = new ProductCountResponseVo();
    productCountResponseVo.setActive(null);
    productCountResponseVo.setOutOfStock(null);
    productCountResponseVo.setArchived(2L);
    productCountResponseVo.setSuspended(0L);
    when(productSolrRepository.getSuspendedAndArchivedProductCount(MERCHANT_CODE, new ProductCountResponseVo()))
        .thenReturn(productCountResponseVo);
    ProductCountResponseVo response = searchServiceImpl.getProductCountByType("IN_ACTIVE", MERCHANT_CODE, false);
    verify(productSolrRepository).getSuspendedAndArchivedProductCount(MERCHANT_CODE, new ProductCountResponseVo());
    Assertions.assertEquals(null, response.getActive());
    Assertions.assertEquals(null, response.getOutOfStock());
    Assertions.assertEquals(2L, productCountResponseVo.getArchived().longValue());
    Assertions.assertEquals(0L, productCountResponseVo.getSuspended().longValue());
  }

  @Test
  public void getProductAndItemsInfoForAllItems_switchOffTest() throws Exception {
    systemParameter.setValue(String.valueOf(false));
    productAndItemsVO.setProduct(product);
    List<Item> itemList = Arrays.asList(item);
    productAndItemsVO.setItems(itemList);
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUESTID, USERNAME,
            USERNAME);
    Mockito.when(this.systemParameterService
            .findValueByStoreIdAndVariable(STORE_ID,
                SystemParameterNames.REVERT_TRANSACTION_API_SWITCH))
        .thenReturn(systemParameter);
    Mockito.when(this.itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet))
        .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkuSet))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.catalogService
            .getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUESTID), Mockito.anyList()))
        .thenReturn(Collections.singletonMap(CATEGORY_CODE, Arrays.asList(itemCatalogVO)));
    Mockito.when(this.masterDataConstructorService
            .constructProductsAndItemsWithMasterData(STORE_ID, USERNAME, REQUESTID,
                Collections.singletonMap(PRODUCT_SKU, product), itemList, true))
        .thenReturn(Arrays.asList(productAndItemsVO));
    searchServiceImpl
        .getProductAndItemsInfoForAllItems(mandatoryRequestParam, itemSkuSet, true, true,
            Boolean.FALSE, true, false);
    Mockito.verify(this.itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet);
    Mockito.verify(this.productHelperService).findAndConstructOfflineItems(STORE_ID, itemList);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterNames.REVERT_TRANSACTION_API_SWITCH);
    verify(this.catalogService)
        .getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUESTID),
            eq(Arrays.asList(CATEGORY_CODE)));
    verify(this.masterDataConstructorService)
        .constructProductsAndItemsWithMasterData(STORE_ID, USERNAME, REQUESTID,
            Collections.singletonMap(PRODUCT_SKU, product), Arrays.asList(item), true);
    verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkuSet);
  }

  @Test
  public void getProductAndItemsInfoForAllItems_nullMasterCatalog_switchOffTest() throws Exception {
    systemParameter.setValue(String.valueOf(false));
    product.setMasterCatalog(null);
    item.setCategoryCode(CATEGORY_CODE);
    List<Item> itemList = Arrays.asList(item);
    product.getMasterDataProduct()
        .setMasterCatalog(
            new MasterCatalog(CATEGORY_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(Arrays.asList(item));
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUESTID, USERNAME,
            USERNAME);
    Mockito.when(this.systemParameterService
            .findValueByStoreIdAndVariable(STORE_ID,
                SystemParameterNames.REVERT_TRANSACTION_API_SWITCH))
        .thenReturn(systemParameter);
    Mockito.when(this.itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet))
        .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkuSet))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.catalogService
            .getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUESTID), Mockito.anyList()))
        .thenReturn(Collections.singletonMap(CATEGORY_CODE, Arrays.asList(itemCatalogVO)));
    Mockito.when(this.masterDataConstructorService
            .constructProductsAndItemsWithMasterData(STORE_ID, USERNAME, REQUESTID,
                Collections.singletonMap(PRODUCT_SKU, product), itemList, true))
        .thenReturn(Arrays.asList(productAndItemsVO));
    searchServiceImpl
        .getProductAndItemsInfoForAllItems(mandatoryRequestParam, itemSkuSet, true, true,
            Boolean.FALSE, true, false);
    Mockito.verify(this.itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterNames.REVERT_TRANSACTION_API_SWITCH);
    verify(this.catalogService)
        .getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUESTID),
            eq(Arrays.asList(CATEGORY_CODE)));
    Mockito.verify(this.productHelperService).findAndConstructOfflineItems(STORE_ID, itemList);
    verify(this.masterDataConstructorService)
        .constructProductsAndItemsWithMasterData(STORE_ID, USERNAME, REQUESTID,
            Collections.singletonMap(PRODUCT_SKU, product), Arrays.asList(item), true);
    verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkuSet);
  }

  @Test
  public void getProductAndItemsInfoForAllItems_salesCatalog_switchOffTest() throws Exception {
    systemParameter.setValue(String.valueOf(false));
    product.setSalesCatalogs(Arrays.asList(
        new SalesCatalog("12051",
            Arrays.asList(new Category(SALES_CATEGORY_CODE, SALES_CATEGORY_CODE)))));
    productAndItemsVO.setProduct(product);
    List<Item> itemList = Arrays.asList(item);
    productAndItemsVO.setItems(itemList);
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUESTID, USERNAME,
            USERNAME);
    Mockito.when(this.systemParameterService
            .findValueByStoreIdAndVariable(STORE_ID,
                SystemParameterNames.REVERT_TRANSACTION_API_SWITCH))
        .thenReturn(systemParameter);
    Mockito.when(this.itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet))
        .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkuSet))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.catalogService
            .getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUESTID), Mockito.anyList()))
        .thenReturn(
            Collections.singletonMap(CATEGORY_CODE, Arrays.asList(itemCatalogVO, itemCatalogVO)));
    Mockito.when(this.masterDataConstructorService
            .constructProductsAndItemsWithMasterData(STORE_ID, USERNAME, REQUESTID,
                Collections.singletonMap(PRODUCT_SKU, product), Arrays.asList(item), true))
        .thenReturn(Arrays.asList(productAndItemsVO));
    Mockito.when(this.offlineItemService.findByItemSkusAndMarkForDeleteFalse(STORE_ID, itemSkuSet))
        .thenReturn(Arrays.asList(offlineItem));
    searchServiceImpl
        .getProductAndItemsInfoForAllItems(mandatoryRequestParam, itemSkuSet, true, true,
            Boolean.FALSE, true, false);
    Mockito.verify(this.itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterNames.REVERT_TRANSACTION_API_SWITCH);
    verify(this.catalogService)
        .getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUESTID),
            eq(Arrays.asList(CATEGORY_CODE, SALES_CATEGORY_CODE)));
    verify(this.masterDataConstructorService)
        .constructProductsAndItemsWithMasterData(STORE_ID, USERNAME, REQUESTID,
            Collections.singletonMap(PRODUCT_SKU, product), Arrays.asList(item), true);
    verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkuSet);
    Mockito.verify(this.productHelperService)
        .findAndConstructOfflineItems(mandatoryRequestParam.getStoreId(), itemList);
  }

  @Test
  public void getProductAndItemsInfoForAllItems_emptyProduct_switchOffTest() throws Exception {
    systemParameter.setValue(String.valueOf(false));
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUESTID, USERNAME,
            USERNAME);
    Mockito.when(this.systemParameterService
            .findValueByStoreIdAndVariable(STORE_ID,
                SystemParameterNames.REVERT_TRANSACTION_API_SWITCH))
        .thenReturn(systemParameter);
    Mockito.when(this.itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet))
        .thenReturn(Collections.emptyList());
    Mockito.when(this.itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet))
        .thenReturn(Arrays.asList(item));
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkuSet))
        .thenReturn(Collections.emptyList());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  searchServiceImpl
          .getProductAndItemsInfoForAllItems(mandatoryRequestParam, itemSkuSet, true, true,
              Boolean.FALSE, true, false));
    } finally {
      Mockito.verify(this.itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet);
      Mockito.verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkuSet);
      Mockito.verify(this.productHelperService).findAndConstructOfflineItems(STORE_ID,
          Arrays.asList(item));
      verify(this.systemParameterService)
          .findValueByStoreIdAndVariable(STORE_ID,
              SystemParameterNames.REVERT_TRANSACTION_API_SWITCH);
    }
  }

  @Test
  public void getProductAndItemsInfoForAllItems_fetchOfflineItem_switchOffTest() throws Exception {
    systemParameter.setValue(String.valueOf(false));
    productAndItemsVO.setProduct(product);
    offlineItem.setItemSku(ITEM_SKU);
    List<Item> itemList = Arrays.asList(item);
    productAndItemsVO.setItems(itemList);
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUESTID, USERNAME,
            USERNAME);
    Mockito.when(this.systemParameterService
            .findValueByStoreIdAndVariable(STORE_ID,
                SystemParameterNames.REVERT_TRANSACTION_API_SWITCH))
        .thenReturn(systemParameter);
    Mockito.when(this.itemService.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet))
        .thenReturn(Collections.emptyList()).thenReturn(itemList);
    Mockito.when(itemService.getItemsByOfflineItemIds(STORE_ID, Arrays.asList(ITEM_SKU), true))
        .thenReturn(itemList);
    Mockito.when(this.productService.findByStoreIdAndProductSkuIn(STORE_ID, productSkuSet))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.catalogService
            .getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUESTID), Mockito.anyList()))
        .thenReturn(Collections.singletonMap(CATEGORY_CODE, Arrays.asList(itemCatalogVO)));
    Mockito.when(this.masterDataConstructorService
            .constructProductsAndItemsWithMasterData(STORE_ID, USERNAME, REQUESTID,
                Collections.singletonMap(PRODUCT_SKU, product), itemList, true))
        .thenReturn(Arrays.asList(productAndItemsVO));
    searchServiceImpl
        .getProductAndItemsInfoForAllItems(mandatoryRequestParam, itemSkuSet, true, true,
            Boolean.FALSE, true, false);
    Mockito.verify(this.itemService).getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet);
    Mockito.verify(itemService).getItemsByOfflineItemIds(STORE_ID, Arrays.asList(ITEM_SKU), true);
    Mockito.verify(this.productHelperService)
        .findAndConstructOfflineItems(STORE_ID, Collections.emptyList());
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterNames.REVERT_TRANSACTION_API_SWITCH);
    verify(this.catalogService)
        .getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUESTID),
            eq(Arrays.asList(CATEGORY_CODE)));
    verify(this.masterDataConstructorService)
        .constructProductsAndItemsWithMasterData(STORE_ID, USERNAME, REQUESTID,
            Collections.singletonMap(PRODUCT_SKU, product), Arrays.asList(item), true);
    verify(this.productService).findByStoreIdAndProductSkuIn(STORE_ID, productSkuSet);
  }

  @Test
  public void getProductsForOfficialStoreOxfordClientTest() throws Exception {
    ReflectionTestUtils.setField(searchServiceImpl, "solrStringDelimiter", "#_#");
    String oxfordClientId = "oxford";
    OfficialStoreRequestVO officialStoreRequestVO = new OfficialStoreRequestVO();
    officialStoreRequestVO.setBuyable(true);
    officialStoreRequestVO.setDiscoverable(true);
    officialStoreRequestVO.setMerchantCodes(Arrays.asList(MERCHANT_CODE));
    List<Item> items = getItems();
    Pageable pageable = PageRequest.of(0, 10);

    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setItemViewConfig(items.get(0).getItemViewConfigs());

    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setItemViewConfig(items.get(1).getItemViewConfigs());

    ItemPickupPoint itemPickupPoint3 = new ItemPickupPoint();
    itemPickupPoint3.setItemViewConfig(items.get(2).getItemViewConfigs());

    ItemPickupPoint itemPickupPoint4 = new ItemPickupPoint();
    itemPickupPoint4.setItemViewConfig(items.get(3).getItemViewConfigs());


    when(solrRepository.getProductsByCategoryAndMerchantCodeL3(eq(STORE_ID), Mockito.any(OfficialStoreRequestVO.class),
        eq(pageable))).thenReturn(new SolrGroupResultVO<>(getProductSolr(), pageable, 6));
    when(itemPickupPointService.getItemPickupPointByProductSkuListAndDeliveryTrue(eq(STORE_ID),
            Mockito.anyList())).thenReturn(
        ImmutableMap.<String, List<ItemPickupPoint>>of(PRODUCT_SKU_3, Arrays.asList(itemPickupPoint1), PRODUCT_SKU_4,
            Arrays.asList(itemPickupPoint2), PRODUCT_SKU_5, Arrays.asList(itemPickupPoint3), PRODUCT_SKU_6,
            Arrays.asList(itemPickupPoint4), PRODUCT_SKU_7, new ArrayList<>()));

    Page<ProductDetailVo> result = searchServiceImpl.getProductsForOfficialStore(STORE_ID, oxfordClientId, officialStoreRequestVO, pageable);

    verify(solrRepository).getProductsByCategoryAndMerchantCodeL3(eq(STORE_ID), Mockito.any(OfficialStoreRequestVO.class),
        eq(pageable));
    verify(itemPickupPointService).getItemPickupPointByProductSkuListAndDeliveryTrue(eq(STORE_ID),
            Mockito.anyList());

    Assertions.assertEquals(5, result.getContent().size());
    Assertions.assertEquals(ITEM_SKU, new ArrayList<>(result.getContent().get(0).getItemSkus()).get(0));
    Assertions.assertEquals(0, result.getContent().get(1).getItemSkus().size());
    Assertions.assertEquals(0, result.getContent().get(2).getItemSkus().size());
    Assertions.assertEquals(0, result.getContent().get(3).getItemSkus().size());
  }

  @Test
  public void getProductsForOfficialStoreOxfordClientNoRecordTest() throws Exception {
    ReflectionTestUtils.setField(searchServiceImpl, "solrStringDelimiter", "#_#");
    String oxfordClientId = "oxford";
    OfficialStoreRequestVO officialStoreRequestVO = new OfficialStoreRequestVO();
    officialStoreRequestVO.setBuyable(true);
    officialStoreRequestVO.setDiscoverable(true);
    officialStoreRequestVO.setMerchantCodes(Arrays.asList(MERCHANT_CODE));

    Pageable pageable = PageRequest.of(0, 10);
    when(solrRepository.getProductsByCategoryAndMerchantCodeL3(eq(STORE_ID), Mockito.any(OfficialStoreRequestVO.class),
        eq(pageable))).thenReturn(new SolrGroupResultVO<>(getProductSolr(), pageable, 0));

    Page<ProductDetailVo> result = searchServiceImpl.getProductsForOfficialStore(STORE_ID, oxfordClientId, officialStoreRequestVO, pageable);

    verify(solrRepository).getProductsByCategoryAndMerchantCodeL3(eq(STORE_ID), Mockito.any(OfficialStoreRequestVO.class),
        eq(pageable));
    Assertions.assertEquals(0, result.getContent().size());
  }

  private List<ProductSolr> getProductSolr() {
    ProductSolr productSolr1 = new ProductSolr();
    productSolr1.setMaximumSellingPrice(0d);
    productSolr1.setMinimumSellingPrice(1d);
    productSolr1.setProductSku(PRODUCT_SKU);

    ProductSolr productSolr2 = new ProductSolr();
    productSolr2.setMaximumSellingPrice(1d);
    productSolr2.setMinimumSellingPrice(0d);
    productSolr2.setProductSku(PRODUCT_SKU_2);

    ProductSolr productSolr3 = new ProductSolr();
    productSolr3.setMaximumSellingPrice(1d);
    productSolr3.setMinimumSellingPrice(1d);
    productSolr3.setProductSku(PRODUCT_SKU_3);
    productSolr3.setMasterCatalog(MASTER_CATALOG);

    ProductSolr productSolr4 = new ProductSolr();
    productSolr4.setMaximumSellingPrice(1d);
    productSolr4.setMinimumSellingPrice(1d);
    productSolr4.setProductSku(PRODUCT_SKU_4);
    productSolr4.setMasterCatalog(MASTER_CATALOG);

    ProductSolr productSolr5 = new ProductSolr();
    productSolr5.setMaximumSellingPrice(1d);
    productSolr5.setMinimumSellingPrice(1d);
    productSolr5.setProductSku(PRODUCT_SKU_5);
    productSolr5.setMasterCatalog(MASTER_CATALOG);

    ProductSolr productSolr6 = new ProductSolr();
    productSolr6.setMaximumSellingPrice(1d);
    productSolr6.setMinimumSellingPrice(1d);
    productSolr6.setProductSku(PRODUCT_SKU_6);
    productSolr6.setMasterCatalog(MASTER_CATALOG);
    productSolr6.setVariantCount(2);

    ProductSolr productSolr7 = new ProductSolr();
    productSolr7.setMaximumSellingPrice(1d);
    productSolr7.setMinimumSellingPrice(1d);
    productSolr7.setProductSku(PRODUCT_SKU_6);
    productSolr7.setMasterCatalog(MASTER_CATALOG);
    productSolr7.setProductSku(PRODUCT_SKU_7);
    productSolr7.setVariantCount(1);

    return Arrays.asList(productSolr1, productSolr2, productSolr3, productSolr4, productSolr5, productSolr6, productSolr7);
  }

  private List<Item> getItems() {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);

    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setChannel(Constants.DEFAULT);
    itemViewConfig2.setBuyable(false);
    itemViewConfig2.setDiscoverable(false);

    ItemViewConfig itemViewConfig3 = new ItemViewConfig();
    itemViewConfig3.setChannel(Constants.DEFAULT);
    itemViewConfig3.setBuyable(true);
    itemViewConfig3.setDiscoverable(false);

    ItemViewConfig itemViewConfig4 = new ItemViewConfig();
    itemViewConfig4.setChannel(Constants.DEFAULT);
    itemViewConfig4.setBuyable(false);
    itemViewConfig4.setDiscoverable(true);

    Item item1 = new Item();
    item1.setItemSku(ITEM_SKU);
    item1.setProductSku(PRODUCT_SKU_3);
    item1.setItemViewConfigs(ImmutableSet.of(itemViewConfig1));

    Item item2 = new Item();
    item2.setItemSku(ITEM_SKU);
    item2.setProductSku(PRODUCT_SKU_4);
    item2.setItemViewConfigs(ImmutableSet.of(itemViewConfig2));

    Item item3 = new Item();
    item3.setItemSku(ITEM_SKU);
    item3.setProductSku(PRODUCT_SKU_5);
    item3.setItemViewConfigs(ImmutableSet.of(itemViewConfig3));

    Item item4 = new Item();
    item4.setItemSku(ITEM_SKU);
    item4.setProductSku(PRODUCT_SKU_6);
    item4.setItemViewConfigs(ImmutableSet.of(itemViewConfig4));

    return Arrays.asList(item1, item2, item3, item4);
  }

  private List<ItemPickupPoint> getItemPickupPoints() {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);

    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setBuyable(false);
    itemViewConfig2.setDiscoverable(false);

    ItemViewConfig itemViewConfig3 = new ItemViewConfig();
    itemViewConfig3.setBuyable(true);
    itemViewConfig3.setDiscoverable(false);

    ItemViewConfig itemViewConfig4 = new ItemViewConfig();
    itemViewConfig3.setBuyable(false);
    itemViewConfig3.setDiscoverable(true);

    ItemPickupPoint item1 = new ItemPickupPoint();
    item1.setItemSku(ITEM_SKU);
    item1.setProductSku(PRODUCT_SKU_3);
    item1.setItemViewConfig(ImmutableSet.of(itemViewConfig1));

    ItemPickupPoint item2 = new ItemPickupPoint();
    item2.setItemSku(ITEM_SKU);
    item2.setProductSku(PRODUCT_SKU_4);
    item2.setItemViewConfig(ImmutableSet.of(itemViewConfig2));

    ItemPickupPoint item3 = new ItemPickupPoint();
    item3.setItemSku(ITEM_SKU);
    item3.setProductSku(PRODUCT_SKU_5);
    item3.setItemViewConfig(ImmutableSet.of(itemViewConfig3));

    ItemPickupPoint item4 = new ItemPickupPoint();
    item4.setItemSku(ITEM_SKU);
    item4.setProductSku(PRODUCT_SKU_6);
    item4.setItemViewConfig(ImmutableSet.of(itemViewConfig4));

    ItemPickupPoint item5 = new ItemPickupPoint();
    item1.setItemSku(ITEM_SKU_OFFLINE_ITEM);
    item1.setProductSku(PRODUCT_SKU_3);
    item1.setItemViewConfig(ImmutableSet.of(itemViewConfig1));

    return Arrays.asList(item1, item1, item2, item3, item4, item5);
  }

  @Test
  public void getMasterDataWithProductItemsVoTest() throws Exception {
    List<Product> productResult = Arrays.asList(new Product());
    ReflectionTestUtils.setField(searchServiceImpl, "isProductVisibilityEnabled", true);
    MasterDataItem masterDataItem = new MasterDataItem();
    when(
      this.productCacheHelperService.findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
        STORE_ID, PRODUCT_CODE)).thenReturn(productResult);
    when(this.productSearchHelper.getProductAndItemsWithMasterDataDetailByDefaultProduct(STORE_ID,
      USERNAME, REQUESTID, productResult, true, false)).thenReturn(
      new MasterDataDetailWithProductAndItemsResponseVo(
        Collections.singletonMap(PRODUCT_CODE, masterDataProduct),
        Collections.singletonMap(ITEM_CODE, masterDataItem),
        Collections.singletonList(productAndItemsVO)));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED)).thenReturn(
      new SystemParameter(STORE_ID, SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED,
        "false", ""));
    this.searchServiceImpl.getMasterDataWithProductItemsVo(STORE_ID, USERNAME, REQUESTID,
      PRODUCT_CODE, true, true, false);
    verify(this.productService).checkProductAndItemsForForceReview(
      Arrays.asList(productAndItemsVO.getProduct()), productAndItemsVO.getItems());
    verify(
      this.productCacheHelperService).findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
      STORE_ID, PRODUCT_CODE);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED);
    verify(this.productSearchHelper).getProductAndItemsWithMasterDataDetailByDefaultProduct(
      STORE_ID, USERNAME, REQUESTID, productResult, true, false);
    verify(this.productSearchHelper).setItemCatalogs(STORE_ID, USERNAME, REQUESTID, true,
      Collections.singletonList(productAndItemsVO),
      Collections.singletonMap(PRODUCT_CODE, masterDataProduct));
    verify(this.productService).setSellerPromoBundlings(eq(STORE_ID),
            Mockito.anyList());
  }

  @Test
  public void isPickupPointCodeUsedTest() {
    when(itemPickupPointService.findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    boolean pickupPointCodeUsed = searchServiceImpl.isPickupPointCodeUsed(STORE_ID, PICKUP_POINT_CODE);
    verify(itemPickupPointService).findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUP_POINT_CODE);
    Assertions.assertTrue(pickupPointCodeUsed);
  }

  @Test
  public void isPickupPointCodeUsedItemPickupPointNullTest() {
    when(itemPickupPointService.findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        PICKUP_POINT_CODE)).thenReturn(null);
    boolean pickupPointCodeUsed = searchServiceImpl.isPickupPointCodeUsed(STORE_ID, PICKUP_POINT_CODE);
    verify(itemPickupPointService).findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUP_POINT_CODE);
    Assertions.assertFalse(pickupPointCodeUsed);
  }

  @Test
  public void getProductSummaryResponseV2Test() {
    when(productSolrRepository.getProductSummaryV2(STORE_ID, 0, 1, new ProductSummaryRequestV2Vo())).thenReturn(
        new PageImpl<>(new ArrayList<>()));
    searchServiceImpl.getProductSummaryResponseV2(STORE_ID, 0, 1, new ProductSummaryRequestV2());
    verify(productSolrRepository).getProductSummaryV2(STORE_ID, 0, 1, new ProductSummaryRequestV2Vo());
  }

  @Test
  public void getgetHalalDashboardProductsTest() {
    when(productSolrRepository.getHalalDashboardProductsResponse(STORE_ID, 0, 1,
        new HalalDashboardFilterRequestVo())).thenReturn(new PageImpl<>(new ArrayList<>()));
    searchServiceImpl.getHalalDashboardProducts(STORE_ID, 0, 1, new HalalProductsFilterRequest());
    verify(productSolrRepository).getHalalDashboardProductsResponse(STORE_ID, 0, 1,
        new HalalDashboardFilterRequestVo());
  }

  @Test
  public void testGetActiveProductsListForMerchantSingleVariantTest() throws Exception {
    ReflectionTestUtils.setField(searchServiceImpl, "solrStringDelimiter", Constants.DELIMETER);
    Pageable pageable = PageRequest.of(0, 10);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setBuyable(true);
    activeProductsRequestVO.setDiscoverable(true);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    List<ItemPickupPoint> items = getItemPickupPoints();

    when(solrRepository.getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable))).thenReturn(new SolrGroupResultVO<>(getProductSolr(), pageable, 10));
    when(itemPickupPointService.getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList())).thenReturn(
        ImmutableMap.<String, List<ItemPickupPoint>>of(PRODUCT_SKU_3, Arrays.asList(items.get(0)), PRODUCT_SKU_4,
            Arrays.asList(items.get(1)), PRODUCT_SKU_5, Arrays.asList(items.get(2)), PRODUCT_SKU_6,
            Arrays.asList(items.get(3))));
    when(itemService.getItemsByProductSkus(STORE_ID, ImmutableSet.of(PRODUCT_SKU_7))).thenReturn(Arrays.asList(item));

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);

    verify(solrRepository).getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable));
    verify(itemPickupPointService).getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList());
    verify(itemService).getItemsByProductSkus(STORE_ID, ImmutableSet.of(PRODUCT_SKU_7));

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void testGetActiveProductsListForMerchantSingleVariantEmptyTest() throws Exception {
    ReflectionTestUtils.setField(searchServiceImpl, "solrStringDelimiter", Constants.DELIMETER);
    Pageable pageable = PageRequest.of(0, 10);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setBuyable(true);
    activeProductsRequestVO.setDiscoverable(true);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    List<ItemPickupPoint> items = getItemPickupPoints();

    when(solrRepository.getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable))).thenReturn(new SolrGroupResultVO<>(getProductSolr(), pageable, 10));
    when(itemPickupPointService.getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList())).thenReturn(
        ImmutableMap.<String, List<ItemPickupPoint>>of(PRODUCT_SKU_3, Arrays.asList(items.get(0)), PRODUCT_SKU_4,
            Arrays.asList(items.get(1)), PRODUCT_SKU_5, Arrays.asList(items.get(2)), PRODUCT_SKU_6,
            Arrays.asList(items.get(3))));
    when(itemService.getItemsByProductSkus(STORE_ID, ImmutableSet.of(PRODUCT_SKU_7))).thenReturn(new ArrayList<>());

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);

    verify(solrRepository).getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable));
    verify(itemPickupPointService).getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList());
    verify(itemService).getItemsByProductSkus(STORE_ID, ImmutableSet.of(PRODUCT_SKU_7));

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void testGetActiveProductsListForMerchantNoSingleVariantTest() throws Exception {
    ReflectionTestUtils.setField(searchServiceImpl, "solrStringDelimiter", Constants.DELIMETER);
    Pageable pageable = PageRequest.of(0, 10);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setBuyable(true);
    activeProductsRequestVO.setDiscoverable(true);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    List<ItemPickupPoint> items = getItemPickupPoints();
    List<ProductSolr> productSolrs = getProductSolr();
    productSolrs.forEach(productSolr -> productSolr.setVariantCount(2));

    when(solrRepository.getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable))).thenReturn(new SolrGroupResultVO<>(productSolrs, pageable, 10));
    when(itemPickupPointService.getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList())).thenReturn(
        ImmutableMap.<String, List<ItemPickupPoint>>of(PRODUCT_SKU_3, Arrays.asList(items.get(0)), PRODUCT_SKU_4,
            Arrays.asList(items.get(1)), PRODUCT_SKU_5, Arrays.asList(items.get(2)), PRODUCT_SKU_6,
            Arrays.asList(items.get(3))));

    Page<ActiveProductDetailVo> result =
        searchServiceImpl.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);

    verify(solrRepository).getActiveProductsListForMerchant(eq(STORE_ID), Mockito.any(ActiveProductsRequestVO.class),
        eq(pageable));
    verify(itemPickupPointService).getItemPickupPointByProductSkuList(eq(STORE_ID),
            Mockito.anyList());

    assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

}
