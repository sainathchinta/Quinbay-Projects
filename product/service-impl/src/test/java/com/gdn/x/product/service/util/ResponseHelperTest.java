package com.gdn.x.product.service.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartnerPromo;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.Video;
import com.gdn.x.product.model.vo.HalalDashboardProductsResponseVo;
import com.gdn.x.product.model.vo.ProductCollectionsVo;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.model.vo.SimpleMasterDataProductVO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.PredefinedAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.SortedDefiningAttributeDTO;
import com.gdn.x.product.rest.web.model.request.VideoAddEditRequest;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemL4SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.SwitchContext;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.time.DateUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.PromoType;
import com.gdn.x.product.model.entity.AdjustmentType;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Geolocation;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCategoryVO;
import com.gdn.x.product.model.vo.ItemPickupPointTransactionResponse;
import com.gdn.x.product.model.vo.ProductSummaryResponseV2Vo;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.Off2OnPriceResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponseV2;
import com.gdn.x.product.rest.web.model.response.SharedProductBundleRecipeResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class ResponseHelperTest {

  private static final String ITEM_SKU_1 = "itemSku1";
  private static final String ITEM_SKU_2 = "itemSku2-1";
  private static final String ITEM_SKU_3 = "itemSku3";
  private static final String PRODUCT_SKU_1 = "productSku1";
  private static final String PRODUCT_SKU_2 = "productSku2";
  private static final String PRODUCT_CODE = "productCode";
  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String DOCUMENT_TYPE = "documentType";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_CODE2 = "pickupPointCode2";
  private static final String PICKUP_POINT_NAME = "pickupPointName";
  private static final String IMAGE_URL = "imageUrl";
  private static final String PRISTINE_ID = "pristineId";
  private static final String PRISTINE_MODEL = "pristineModel";
  private static final String PRISTINE_BRAND = "pristineBrand";
  private static final String ATTRIBUTE_NAME = "attributeName";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String ATTRIBUTE_CODE_2 = "attributeCode2";
  private static final String IMEI_ATTRIBUTE_NAME = "IMEI";
  private static final String ATTRIBUTE_CODE_3 = "attributeCode3";
  private static final String ATTRIBUTE_VALUE = "attributeValue";
  private static final String CAMPAIGN_CODE = "campaignCode";
  private static final String DELIMITER = "#_#";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PLACE_ID = "placeId";
  private static final String STREET_ADDRESS = "streetAddress";
  private static final double LONGITUDE = 100.1;
  private static final double LATITUDE = 200.1;
  private static final String VALUE = "value";
  private static final String NAME = "name";
  private static final String CODE = "code";
  private static final double LIST_PRICE = 50.00;
  private static final double OFFER_PRICE = 30.00;
  private static final double SHIPPING_WT = 30.00;
  private static final String OFFLINE_ID = "itemSku2-1-pickupPointCode";
  private static final String OFFLINE_ID_2 = "itemSku-pickupPointCode";
  private static final String MAIN_IMAGE_URL = "MAIN_IMAGE_URL";
  private static final String SOURCE_URL = "SOURCE_URL";
  private static final double PRICE_1 = 1.2;
  private static final double PRICE_2 = 2.8;
  private static final double ORIGINAL_SELLING_PRICE_2 = 20.1;
  private static final Date TEN_DAYS_AGO = new Date(System.currentTimeMillis() - (10 * 24 * 60 * 60 * 1000));
  private static final Date TEN_DAYS_AFTER = new Date(System.currentTimeMillis() + (10 * 24 * 60 * 60 * 1000));
  private static final String ITEM_CODE_1 = "ITEM_CODE_1";
  private static final String ITEM_CODE_2 = "ITEM_CODE_2";
  private static final String ITEM_SKU = "itemSku";
  public static final String SIZE_CHART_CODE = "sizeChartCode";
  public static final String DIMENSION = "DIMENSION";
  public static final String DESCRIPTION = "DESCRIPTION";
  public static final String IMEI_ATTRIBUTE_CODE = "NO-2000051";

  private ItemPickupPoint itemPickupPoint1;
  private ItemPickupPoint itemPickupPoint2;
  private ItemPickupPoint itemPickupPoint3;
  private ItemPickupPoint itemPickupPoint4;
  private Item item;
  private Product product;
  private ProductSolr productSolr;
  private ProductSolr productSolr2;
  private ProductSummaryResponseV2Vo productSummaryResponseV2Vo;
  private MasterDataProduct masterDataProduct;
  private MasterDataItem masterDataItem;
  private List<ItemPickupPoint> itemPickupPointList;
  private List<ItemPickupPoint> itemPickupPointList1 = new ArrayList<>();
  private Set<ItemViewConfig> itemViewConfigSet2 = new HashSet<>();
  private Map<String, ItemViewConfig> offLineItemIdAndItemViewConfigOriginalMap = new HashMap<>();
  private Map<String, Item> itemMap;
  private Map<String, Product> productMap = new HashMap<>();
  private Map<String, List<CategoryResponse>> categoryHierarchyMap;
  private Map<String, BusinessPartnerPickupPoint> businessPartnerPickupPointMap;
  private BusinessPartnerPickupPoint businessPartnerPickupPoint;
  private Price price;
  private ItemViewConfig itemViewConfig = new ItemViewConfig();
  private B2bFields b2bFields;
  private DiscountPrice discountPrice1;
  private DiscountPrice discountPrice2;
  private ItemBuyableSchedule itemBuyableSchedule;
  private ItemDiscoverableSchedule itemDiscoverableSchedule;

  @BeforeEach
  public void setUp() {
    itemPickupPoint1 =
        ItemPickupPoint.builder().itemSku(ITEM_SKU_1).pickupPointCode(PICKUP_POINT_CODE).build();
    itemPickupPoint2 = ItemPickupPoint.builder().itemSku(ITEM_SKU_2).productSku(PRODUCT_SKU_1).build();
    itemPickupPoint3 =
        ItemPickupPoint.builder().itemSku(ITEM_SKU_2).productSku(PRODUCT_SKU_2).pickupPointCode(PICKUP_POINT_CODE)
            .build();
    itemPickupPoint4 = ItemPickupPoint.builder().itemSku(ITEM_SKU_2).productSku(PRODUCT_SKU_2).pickupPointCode(PICKUP_POINT_CODE)
        .build();

    Price price = new Price();
    price.setOfferPrice(10.0);
    price.setListPrice(10.0);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    ItemViewConfig itemViewConfigCnc = new ItemViewConfig();
    itemViewConfigCnc.setChannel(Constants.CNC);
    itemViewConfigCnc.setBuyable(true);
    itemViewConfigCnc.setDiscoverable(true);
    itemPickupPoint3.setPrice(ImmutableSet.of(price));
    itemPickupPoint4.setPrice(ImmutableSet.of(price));


    Price price2 = new Price();
    price.setOfferPrice(10.0);
    price.setListPrice(10.0);
    price2.setChannel("CHANNEL2");
    price2.setListOfDiscountPrices(Arrays.asList(new DiscountPrice()));
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setAdjustmentName("PROMO");
    price2.setMerchantPromoDiscountPrice(discountPrice);

    b2bFields = new B2bFields();
    b2bFields.setBasePrice(100.0);
    b2bFields.setManaged(true);
    itemPickupPoint3.setPrice(ImmutableSet.of(price, price2));
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
    itemViewConfigSet.add(itemViewConfig);
    itemPickupPoint3.setItemViewConfig(itemViewConfigSet);
    itemPickupPoint3.setB2bFields(b2bFields);
    itemPickupPoint4.setPrice(ImmutableSet.of(price, price2));
    Set<ItemViewConfig> itemViewConfigSet1 = new HashSet<>();
    itemViewConfigSet1.add(itemViewConfig);
    itemViewConfigSet1.add(itemViewConfigCnc);
    itemPickupPoint4.setItemViewConfig(itemViewConfigSet1);
    itemPickupPoint4.setB2bFields(b2bFields);

    item = new Item();
    item.setItemSku(ITEM_SKU_2);
    item.setOff2OnChannelActive(true);
    item.setCategoryCode(CATEGORY_CODE_2);
    item.setMainImageUrl(IMAGE_URL);
    item.setDangerousLevel(0);
    item.setUpcCode(PRISTINE_MODEL);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineModel(PRISTINE_MODEL);
    pristineDataItem.setPristineBrand(PRISTINE_BRAND);
    item.setPristineDataItem(pristineDataItem);

    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeName(ATTRIBUTE_NAME);
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    item.setDefiningAttributes(Arrays.asList(productAttributeDetail));

    product = new Product();
    product.setProductSku(PRODUCT_SKU_2);
    product.setProductType(ProductType.REGULAR);
    product.setProductCode(PRODUCT_CODE);
    product.setSynchronized(true);
    product.setCategoryCode(CATEGORY_CODE_2);
    ItemCategoryVO itemCategoryVO1 = new ItemCategoryVO();
    itemCategoryVO1.setProductCategoryCode(CATEGORY_CODE_1);
    ItemCategoryVO itemCategoryVO2 = new ItemCategoryVO();
    itemCategoryVO2.setProductCategoryCode(CATEGORY_CODE_2);
    itemCategoryVO2.setDocumentType(DOCUMENT_TYPE);
    ItemCatalogVO itemCatalogVO = new ItemCatalogVO();
    itemCatalogVO.setItemCategories(Arrays.asList(itemCategoryVO1, itemCategoryVO2));
    product.setItemCatalogs(Arrays.asList(itemCatalogVO));
    MasterCatalog masterCatalog = new MasterCatalog(CATEGORY_CODE_2, new Category(CATEGORY_CODE_2, CATEGORY_CODE_2));
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setMasterCatalog(masterCatalog);
    product.setMasterDataProduct(masterDataProduct);
    product.setB2bActivated(true);
    product.setB2cActivated(true);

    PreOrder preOrder = new PreOrder();
    preOrder.setPreOrderType(Constants.DATE);
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderDate(DateUtils.addDays(new Date(), -2));
    product.setPreOrder(preOrder);

    ProductScore productScore = new ProductScore();
    productScore.setTotalScore(10.0);
    product.setProductScore(productScore);

    ProductSpecialAttribute productSpecialAttribute1 = new ProductSpecialAttribute();
    productSpecialAttribute1.setAttributeCode(ATTRIBUTE_CODE);
    ProductSpecialAttribute productSpecialAttribute2 = new ProductSpecialAttribute();
    productSpecialAttribute2.setAttributeCode(ATTRIBUTE_CODE_2);
    productSpecialAttribute2.setAttributeValue(ATTRIBUTE_VALUE);
    productSpecialAttribute2.setAttributeName(Constants.GUARANTEE_ATTRIBUTE_NAME.get(0));
    productSpecialAttribute2.setAttributeValue(ATTRIBUTE_VALUE);
    ProductSpecialAttribute productSpecialAttribute3 = new ProductSpecialAttribute();
    productSpecialAttribute3.setAttributeCode(ATTRIBUTE_CODE_3);
    productSpecialAttribute3.setAttributeName(Constants.GUARANTEE_DURATION_ATTRIBUTE_NAME.get(0));
    productSpecialAttribute3.setAttributeValue(ATTRIBUTE_VALUE);
    product.setProductSpecialAttributes(
        Arrays.asList(null, productSpecialAttribute1, productSpecialAttribute2, productSpecialAttribute3));

    SalesCatalog salesCatalog = new SalesCatalog();
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_1);
    salesCatalog.setListOfCategories(Arrays.asList(category));
    product.setSalesCatalogs(Arrays.asList(salesCatalog));
    product.setCurationStatus(CurationStatus.APPROVED);

    masterDataProduct= new MasterDataProduct();
    masterDataItem = new MasterDataItem();
    MasterDataItemImage masterDataItemImage1 = new MasterDataItemImage();
    MasterDataItemImage masterDataItemImage2 = new MasterDataItemImage();
    masterDataItemImage1.setMainImage(true);
    masterDataItemImage1.setLocationPath(IMAGE_URL);
    masterDataItemImage2.setMainImage(false);
    masterDataItem.setMasterDataItemImages(Arrays.asList(masterDataItemImage1, masterDataItemImage2));

    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setName(CATEGORY_CODE_1);


    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);

    productSolr = ProductSolr.builder().productCode(PRODUCT_CODE).productSku(PRODUCT_SKU_2).isSynchronized(false)
        .masterCatalog(CATEGORY_CODE_2 + "#_#" + CATEGORY_CODE_2).isArchived(true).off2OnChannelActive(true)
        .inStock(true).minimumListPrice(100.0).minimumSellingPrice(100.0).maximumListPrice(100.0)
        .maximumSellingPrice(100.0).pickupPointCodes(Arrays.asList(PICKUP_POINT_CODE)).build();

    productSolr2 = ProductSolr.builder().productCode(PRODUCT_CODE).productSku(PRODUCT_SKU_2).build();

    productSummaryResponseV2Vo = new ProductSummaryResponseV2Vo();
    productSummaryResponseV2Vo.setCategoryCode(CATEGORY_CODE_1);


    itemPickupPointList = new ArrayList<>(Arrays.asList(itemPickupPoint1, itemPickupPoint2, itemPickupPoint3, itemPickupPoint4));
    itemPickupPointList.stream().forEach(itemPickupPoint -> itemPickupPoint.setB2bFields(b2bFields));
    offLineItemIdAndItemViewConfigOriginalMap.put(ITEM_SKU_1 + Constants.HYPHEN + PICKUP_POINT_CODE,
        new ItemViewConfig());
    offLineItemIdAndItemViewConfigOriginalMap.put(ITEM_SKU_2 + Constants.HYPHEN + PICKUP_POINT_CODE, new ItemViewConfig());
    offLineItemIdAndItemViewConfigOriginalMap.put(ITEM_SKU_2 + Constants.HYPHEN + PICKUP_POINT_CODE2,
        new ItemViewConfig());
    product.setMarkForDelete(true);
    product.setSuspended(true);
    productMap.putIfAbsent(PRODUCT_SKU_2, product);
    categoryHierarchyMap = ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(categoryResponse));
    businessPartnerPickupPointMap = ImmutableMap.of(PICKUP_POINT_CODE, businessPartnerPickupPoint);
    itemMap = ImmutableMap.of(ITEM_SKU_2, item);

    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    businessPartnerPickupPoint.setGeolocation(new Geolocation(PLACE_ID, LATITUDE, LONGITUDE, STREET_ADDRESS));

    this.price = new Price();
    this.price.setOfferPrice(10.0);
    this.price.setListPrice(10.0);

    itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.B2B);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setBuyable(true);

    discountPrice1 = new DiscountPrice();
    discountPrice2 = new DiscountPrice();
    discountPrice1.setDiscountPrice(PRICE_1);
    discountPrice1.setPriority(3);
    discountPrice2.setDiscountPrice(PRICE_2);
    discountPrice2.setPriority(1);

    itemBuyableSchedule = new ItemBuyableSchedule();
    itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemBuyableSchedule.setBuyable(true);
    itemBuyableSchedule.setStartDateTime(TEN_DAYS_AGO);
    itemBuyableSchedule.setEndDateTime(TEN_DAYS_AFTER);
    itemDiscoverableSchedule.setDiscoverable(true);
    itemDiscoverableSchedule.setStartDateTime(TEN_DAYS_AGO);
    itemDiscoverableSchedule.setEndDateTime(TEN_DAYS_AFTER);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    itemViewConfigSet2.add(itemViewConfig);
    ItemPickupPoint itemPickupPoint4 = new ItemPickupPoint();
    itemPickupPoint4.setItemViewConfig(itemViewConfigSet2);
    itemPickupPointList1.add(itemPickupPoint4);
  }

  @Test
  public void toOff2OnPriceResponseV2Test() {
    List<Off2OnPriceResponseV2> off2OnPriceResponseV2List =
        ResponseHelper.toOff2OnPriceResponseV2(Arrays.asList(itemPickupPoint3), itemMap, new HashMap<>());
    assertTrue(off2OnPriceResponseV2List.get(0).isOff2OnChannelActive());
  }

  @Test
  public void toOff2OnPriceResponseV2AllStorePromoTest() {
    HashMap<String, BusinessPartnerPromo> businessPartnerPickupPointMap = new HashMap<>();
    businessPartnerPickupPointMap.put(itemPickupPoint3.getMerchantCode(), BusinessPartnerPromo.builder()
        .activePromoBundlings(ImmutableSet.of(Constants.WHOLESALE, Constants.WHOLESALE_PRICE)).build());
    List<Off2OnPriceResponseV2> off2OnPriceResponseV2List =
        ResponseHelper.toOff2OnPriceResponseV2(Arrays.asList(itemPickupPoint3), itemMap, businessPartnerPickupPointMap);
    assertTrue(off2OnPriceResponseV2List.get(0).isOff2OnChannelActive());
  }

  @Test
  public void toItemResponseV2Test() {
    List<ItemResponseV2> itemResponseV2List =
        ResponseHelper.toItemResponseV2(itemPickupPointList, itemMap, productMap, businessPartnerPickupPointMap,
            categoryHierarchyMap, false, false, null);
    assertEquals(PRODUCT_SKU_2, itemResponseV2List.get(0).getProductSku());
    assertEquals(ITEM_SKU_2, itemResponseV2List.get(0).getItemSku());
    assertEquals(100.0, itemResponseV2List.get(0).getB2BResponse().getBasePrice(), 0.0);
    assertTrue(itemResponseV2List.get(0).getB2BResponse().isManaged());
    assertTrue(itemResponseV2List.get(0).isSuspended());
  }

  @Test
  public void toItemResponseV2UsePcbDataTrueTest() {
    List<ItemResponseV2> itemResponseV2List =
        ResponseHelper.toItemResponseV2(itemPickupPointList, itemMap, productMap, businessPartnerPickupPointMap,
            categoryHierarchyMap, true, false, null);
    assertEquals(PRODUCT_SKU_2, itemResponseV2List.get(0).getProductSku());
    assertEquals(ITEM_SKU_2, itemResponseV2List.get(0).getItemSku());
  }

  @Test
  public void toItemResponseV2ActivePromoBundlingTest() {
    product.setSynchronized(false);
    product.setMasterDataProduct(masterDataProduct);
    item.setMasterDataItem(masterDataItem);
    itemPickupPoint3.setActivePromoBundlings(ImmutableSet.of(Constants.WHOLESALE, Constants.WHOLESALE_PRICE));
    List<ItemResponseV2> itemResponseV2List =
        ResponseHelper.toItemResponseV2(itemPickupPointList, itemMap, productMap,
            businessPartnerPickupPointMap, categoryHierarchyMap, false, false, null);
    assertTrue(itemResponseV2List.get(0).getWholesalePriceActivated());
    assertTrue(itemResponseV2List.get(0).isWholesalePromoActivated());
  }

  @Test
  public void toItemResponseV2PriceAndViewConfigTest() {
    product.setSynchronized(false);
    List<ItemResponseV2> itemResponseV2List =
        ResponseHelper.toItemResponseV2(itemPickupPointList, itemMap, productMap,
            businessPartnerPickupPointMap, categoryHierarchyMap, false, false, null);
    assertEquals(2, itemResponseV2List.get(0).getPrices().size());
  }

  @Test
  public void toItemResponseV2CategoryResponseNullTest() {
    Product product = productMap.get(PRODUCT_SKU_2);
    product.setPreOrder(null);
    productMap.replace(PRODUCT_SKU_2, product);
    categoryHierarchyMap = ImmutableMap.of(CATEGORY_CODE_1, new ArrayList<>());
    List<ItemResponseV2> itemResponseV2List =
        ResponseHelper.toItemResponseV2(itemPickupPointList, itemMap, productMap,
            businessPartnerPickupPointMap, categoryHierarchyMap, false, false, null);
    assertNull(itemResponseV2List.get(0).getCategoryHierarchy());
    assertNull(itemResponseV2List.get(0).getPreOrder());
  }

  @Test
  public void toItemResponseV2PromoTypeTest() {
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.setPromoBundling(true);

    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(CAMPAIGN_CODE);
    discountPrice1.setStartDateTime(DateUtils.addDays(new Date(), 2));

    DiscountPrice discountPrice2 = new DiscountPrice();
    discountPrice1.setCampaignCode(CAMPAIGN_CODE);
    discountPrice1.setStartDateTime(DateUtils.addDays(new Date(), -2));
    discountPrice1.setEndDateTime(DateUtils.addDays(new Date(), -1));

    DiscountPrice discountPrice3 = new DiscountPrice();
    discountPrice1.setCampaignCode(CAMPAIGN_CODE);
    discountPrice1.setStartDateTime(DateUtils.addDays(new Date(), -2));
    discountPrice1.setEndDateTime(DateUtils.addDays(new Date(), 2));

    itemPickupPoint3.getPrice().stream().findFirst().get()
        .setListOfDiscountPrices(Arrays.asList(discountPrice1, discountPrice2, discountPrice3));

    List<ItemResponseV2> itemResponseV2List =
        ResponseHelper.toItemResponseV2(itemPickupPointList, itemMap, productMap, businessPartnerPickupPointMap,
            categoryHierarchyMap, false, false, null);

    assertTrue(itemResponseV2List.get(0).getPromoTypes()
        .containsAll(Arrays.asList(PromoType.PROMO_BUNDLING.name(), PromoType.PROMO_DISCOUNT.name(), PromoType.CAMPAIGN.name())));
  }

  @Test
  public void toHalalDashboardProductsResponseVoTest() {
    List<HalalDashboardProductsResponseVo> halalDashboardProductsResponseVoList =
        ResponseHelper.toHalalDashboardProductsResponseVo(Arrays.asList(productSolr), DELIMITER);
    assertEquals(CATEGORY_CODE_2, halalDashboardProductsResponseVoList.get(0).getCategoryCode());
    assertEquals(PRODUCT_CODE, halalDashboardProductsResponseVoList.get(0).getProductCode());
    assertEquals(PRODUCT_SKU_2, halalDashboardProductsResponseVoList.get(0).getProductSku());
  }

  @Test
  public void toHalalDashboardProductsResponseNullCategoryCodeVoTest() {
    List<HalalDashboardProductsResponseVo> halalDashboardProductsResponseVoList =
        ResponseHelper.toHalalDashboardProductsResponseVo(Arrays.asList(productSolr2), DELIMITER);
    assertEquals(PRODUCT_CODE, halalDashboardProductsResponseVoList.get(0).getProductCode());
    assertEquals(PRODUCT_SKU_2, halalDashboardProductsResponseVoList.get(0).getProductSku());
  }

  @Test
  public void toProductSummaryResponseV2VoTest() {
    List<ProductSummaryResponseV2Vo> productSummaryResponseV2VoList =
        ResponseHelper.toProductSummaryResponseV2Vo(Arrays.asList(productSolr), DELIMITER);
    assertTrue(productSummaryResponseV2VoList.get(0).isArchived());
    assertTrue(productSummaryResponseV2VoList.get(0).isInStock());
    assertTrue(productSummaryResponseV2VoList.get(0).isOff2OnActiveFlag());
    assertEquals(CATEGORY_CODE_2, productSummaryResponseV2VoList.get(0).getCategoryCode());
    assertEquals(100.0, productSummaryResponseV2VoList.get(0).getMaxNormalPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoList.get(0).getMaxSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoList.get(0).getMinSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoList.get(0).getMinNormalPrice(), 0);
  }

  @Test
  public void toProductSummaryResponseV2VoWithNullsTest() {
    List<ProductSummaryResponseV2Vo> productSummaryResponseV2VoList =
        ResponseHelper.toProductSummaryResponseV2Vo(Arrays.asList(productSolr2), DELIMITER);
    assertFalse(productSummaryResponseV2VoList.get(0).isArchived());
    assertFalse(productSummaryResponseV2VoList.get(0).isInStock());
    assertFalse(productSummaryResponseV2VoList.get(0).isOff2OnActiveFlag());
    assertNull(productSummaryResponseV2VoList.get(0).getCategoryCode());
    assertEquals(0.0, productSummaryResponseV2VoList.get(0).getMaxNormalPrice(), 0);
    assertEquals(0.0, productSummaryResponseV2VoList.get(0).getMaxSellingPrice(), 0);
    assertEquals(0.0, productSummaryResponseV2VoList.get(0).getMinSellingPrice(), 0);
    assertEquals(0.0, productSummaryResponseV2VoList.get(0).getMinNormalPrice(), 0);
  }

  @Test
  public void toProductSummaryResponseV2Test() {
    List<ProductSummaryResponseV2> productSummaryResponseV2List =
        ResponseHelper.toProductSummaryResponseV2(Arrays.asList(productSummaryResponseV2Vo), categoryHierarchyMap);
    assertEquals(CATEGORY_CODE_1, productSummaryResponseV2List.get(0).getCategoryName());
  }

  @Test
  public void toProductSummaryResponseV2NoCategoryTest() {
    productSummaryResponseV2Vo.setCategoryCode(CATEGORY_CODE_2);
    List<ProductSummaryResponseV2> productSummaryResponseV2List =
        ResponseHelper.toProductSummaryResponseV2(Arrays.asList(productSummaryResponseV2Vo), categoryHierarchyMap);
    assertNull(productSummaryResponseV2List.get(0).getCategoryName());
  }

  @Test
  public void toProductForTransactionVOTest() throws ParseException {
    item.setDangerousLevel(null);
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(),new HashSet<>(), false,
            false, true, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
    assertFalse(itemPickupPointTransactionResponseList.get(0).getItemDetail().isCncActive());
    assertTrue(itemPickupPointTransactionResponseList.get(1).getItemDetail().isCncActive());
  }

  @Test
  public void toProductForTransactionVOMasterDataAttributeTest() throws ParseException {
    item.setDangerousLevel(0);
    MasterDataItemAttributeValue masterDataItemAttributeValue1 = new MasterDataItemAttributeValue();
    MasterDataItemAttributeValue masterDataItemAttributeValue2 = new MasterDataItemAttributeValue();
    MasterDataItemAttributeValue masterDataItemAttributeValue3 = new MasterDataItemAttributeValue();
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setVariantCreation(true);
    masterDataItemAttributeValue1.setMarkForDelete(true);
    masterDataItemAttributeValue2.setMasterDataAttribute(new MasterDataAttribute());
    masterDataItemAttributeValue3.setMasterDataAttribute(masterDataAttribute);
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(Arrays.asList(masterDataItemAttributeValue1, masterDataItemAttributeValue2, masterDataItemAttributeValue3));
    item.setMasterDataItem(masterDataItem);
    product.getItemCatalogs().get(0).getItemCategories().forEach(itemCategoryVO -> itemCategoryVO.setDocumentType(null));
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(ITEM_SKU_2);
    productAttribute.setProductAttributeDetails(Collections.singletonList(new ProductAttributeDetail()));
    product.setDefiningAttributes(Collections.singletonList(productAttribute));


    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(),new HashSet<>(), false,
            false, true, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertNull(itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
  }


  @Test
  public void toProductForTransactionVOusePcbDataTrueTest() throws ParseException {
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(new ArrayList<>());
    item.setMasterDataItem(masterDataItem);
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), ImmutableSet.of(PRODUCT_SKU_2), false,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
  }

  @Test
  public void toProductForTransactionVOUnSyncTest() throws ParseException {
    product.setMasterDataProduct(masterDataProduct);
    item.setMasterDataItem(masterDataItem);
    product.setSynchronized(false);

    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), new HashSet<>(), false,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
  }

  @Test
  public void toProductForTransactionVOUnSyncNoMasterDataTest() throws ParseException {
    product.setSynchronized(false);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), new HashSet<>(), true,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl(), MAIN_IMAGE_URL);
  }

  @Test
  public void toProductForTransactionVOEqualPreOrderCurrentDateTest() throws ParseException {
    product.getPreOrder().setPreOrderDate(new Date());
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), new HashSet<>(), false,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
  }

  @Test
  public void toProductForTransactionVOLessPreOrderCurrentDateTest() throws ParseException {
    product.getPreOrder().setPreOrderDate(DateUtils.addDays(new Date(), -2));
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), new HashSet<>(), false,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
  }

  @Test
  public void toProductForTransactionVOEqualPreOrderDaysTest() throws ParseException {
    product.getPreOrder().setPreOrderType(Constants.DAYS);
    product.getPreOrder().setPreOrderValue(14);
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), new HashSet<>(), false,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
  }

  @Test
  public void toProductForTransactionVOPreOrderWeekTest() throws ParseException {
    product.getPreOrder().setPreOrderType(Constants.WEEK);
    product.getPreOrder().setPreOrderValue(14);
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), new HashSet<>(), false,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
  }

  @Test
  public void toProductForTransactionVOPreOrderNullTest() throws ParseException {
    product.setPreOrder(null);
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), new HashSet<>(), false,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
  }

  @Test
  public void toProductForTransactionVOPreOrderInactiveTest() throws ParseException {
    product.getPreOrder().setIsPreOrder(false);
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), new HashSet<>(), false,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
  }

  @Test
  public void toProductForTransactionVOPristineItemNullTest() throws ParseException {
    item.setPristineDataItem(null);
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), new HashSet<>(), false,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
  }

  @Test
  public void toProductForTransactionVOPristineNullTest() throws ParseException {
    item.getPristineDataItem().setPristineModel(null);
    item.getPristineDataItem().setPristineBrand(null);
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), new HashSet<>(), false,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
  }

  @Test
  public void toProductForTransactionVOOnlyGuaranteeAttributeNameSpecialTest() throws ParseException {
    product.setProductSpecialAttributes(new ArrayList<>());

    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(), new HashSet<>(), false,
            false, false, IMEI_ATTRIBUTE_CODE, new ArrayList<>());
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
  }

  @Test
  public void toProductForTransactionVONoSpecialAttributeTest() throws ParseException {
    ProductSpecialAttribute productSpecialAttribute2 = new ProductSpecialAttribute();
    productSpecialAttribute2.setAttributeCode(ATTRIBUTE_CODE_2);
    productSpecialAttribute2.setAttributeName(Constants.GUARANTEE_ATTRIBUTE_NAME.get(0));
    productSpecialAttribute2.setAttributeValue(ATTRIBUTE_VALUE);
    ProductSpecialAttribute productSpecialAttribute3 = new ProductSpecialAttribute();
    productSpecialAttribute3.setAttributeCode(IMEI_ATTRIBUTE_CODE);
    productSpecialAttribute3.setAttributeName(IMEI_ATTRIBUTE_NAME);
    productSpecialAttribute3.setAttributeValue(Constants.YES);
    product.setProductSpecialAttributes(new ArrayList<>());
    product.getProductSpecialAttributes().add(productSpecialAttribute2);
    product.getProductSpecialAttributes().add(productSpecialAttribute3);
    product.getProductSpecialAttributes().add(productSpecialAttribute3);
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList =
        ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, new HashMap<>(),
            new HashSet<>(), false, false, false, IMEI_ATTRIBUTE_CODE, Arrays.asList(Constants.YES));
    assertEquals(ITEM_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE,
        itemPickupPointTransactionResponseList.get(0).getItemDetail().getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemPickupPointTransactionResponseList.get(0).getItemDetail().getProductSku());
    assertEquals(Arrays.asList(DOCUMENT_TYPE), itemPickupPointTransactionResponseList.get(0).getItemDetail().getDocumentType());
    assertEquals(IMAGE_URL, itemPickupPointTransactionResponseList.get(0).getItemDetail().getImageUrl());
    assertTrue(itemPickupPointTransactionResponseList.get(0).getItemDetail().isImeiRequired());
  }

  @Test
  public void toItemSummaryListResponseTest() {
    SwitchContext switchContext = getSwitchContext(false);
    productMap.entrySet().iterator().next().getValue().setSizeChartCode(SIZE_CHART_CODE);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
        ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap, new HashMap<>(),
            offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(), new HashMap<>(),switchContext,
            StringUtils.EMPTY);
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemSummaryListResponseWithSkipActionForL4MissingTest() {
    SwitchContext switchContext = getSwitchContext(false);
    productMap.entrySet().iterator().next().getValue().setSizeChartCode(SIZE_CHART_CODE);
    itemMap = new HashMap<>();
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap, new HashMap<>(),
        offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(), new HashMap<>(),switchContext,
        StringUtils.EMPTY);
    assertTrue(CollectionUtils.isEmpty(itemSummaryListResponseList));
  }

  @Test
  public void toItemSummaryListResponseWithSkipActionForL3MissingTest() {
    SwitchContext switchContext = getSwitchContext(false);
    productMap.entrySet().iterator().next().getValue().setSizeChartCode(SIZE_CHART_CODE);
    productMap = new HashMap<>();
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap, new HashMap<>(),
        offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(), new HashMap<>(),switchContext,
        StringUtils.EMPTY);
    assertTrue(CollectionUtils.isEmpty(itemSummaryListResponseList));
  }

  @Test
  public void toItemSummaryListResponseWithSkipActionWithTakenDownProductTest() {
    SwitchContext switchContext = getSwitchContext(false);
    switchContext.setRemoveTakenDownL5FromResponse(true);
    productMap.entrySet().iterator().next().getValue().setSizeChartCode(SIZE_CHART_CODE);
    productMap.values().forEach(product1 -> product1.setForceReview(true));
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap, new HashMap<>(),
        offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(), new HashMap<>(),switchContext,
        StringUtils.EMPTY);
    assertTrue(CollectionUtils.isEmpty(itemSummaryListResponseList));
  }

  @Test
  public void toItemSummaryListResponseWithSkipActionWithTakenDownItemTest() {
    SwitchContext switchContext = getSwitchContext(false);
    switchContext.setRemoveTakenDownL5FromResponse(true);
    productMap.entrySet().iterator().next().getValue().setSizeChartCode(SIZE_CHART_CODE);
    productMap.values().forEach(product1 -> product1.setTakenDown(true));
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap, new HashMap<>(),
        offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(), new HashMap<>(),switchContext,
        StringUtils.EMPTY);
    assertTrue(CollectionUtils.isEmpty(itemSummaryListResponseList));
  }

  @Test
  public void toItemSummaryListResponseWithSkipActionWithForceREviewItemTest() {
    SwitchContext switchContext = getSwitchContext(false);
    switchContext.setRemoveTakenDownL5FromResponse(true);
    productMap.entrySet().iterator().next().getValue().setSizeChartCode(SIZE_CHART_CODE);
    itemMap.values().forEach(l4 -> l4.setForceReview(true));
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap, new HashMap<>(),
        offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(), new HashMap<>(),switchContext,
        StringUtils.EMPTY);
    assertTrue(CollectionUtils.isEmpty(itemSummaryListResponseList));
  }

  @Test
  public void toItemSummaryListResponseSwitchTrueRequestBlankTest() {
    SwitchContext switchContext = getSwitchContext(false);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, StringUtils.EMPTY);
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemSummaryListResponseSwitchTrueRequestAllTest() {
    SwitchContext switchContext = getSwitchContext(false);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, "ALL");
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void getItemViewConfigsOriginalDiscoverableAndOriginalBuyableTest() {
    Set<ItemViewConfigDTO> itemViewConfigSet1 = new HashSet<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList1) {
      itemViewConfigSet1 =
          (ResponseHelper.getItemViewConfigs("ALL", itemPickupPoint.getAllItemViewConfigs()));
    }
    List<ItemViewConfigDTO> itemViewConfigDTOs = new ArrayList<>();
    for (ItemViewConfigDTO config : itemViewConfigSet1) {
      itemViewConfigDTOs.add(config);
    }
    List<ItemViewConfig> itemViewConfigs = new ArrayList<>();
    for (ItemViewConfig config : itemViewConfigSet2) {
      itemViewConfigs.add(config);
    }
    assertEquals(itemViewConfigs.get(0).isDiscoverable(),
        itemViewConfigDTOs.get(0).isDiscoverableOriginal());
    assertEquals(itemViewConfigs.get(0).isBuyable(),
        itemViewConfigDTOs.get(0).isBuyableOriginal());
  }

  @Test
  public void getItemViewConfigsDiscoverableScheduleTest() {
    Set<ItemViewConfigDTO> itemViewConfigSet1 = new HashSet<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList1) {
      itemViewConfigSet1 =
          (ResponseHelper.getItemViewConfigs("ALL", itemPickupPoint.getAllItemViewConfigs()));
    }
    List<ItemViewConfigDTO> itemViewConfigDTOs = new ArrayList<>();
    for (ItemViewConfigDTO config : itemViewConfigSet1) {
      itemViewConfigDTOs.add(config);
    }
    List<ItemViewConfig> itemViewConfigs = new ArrayList<>();
    for (ItemViewConfig config : itemViewConfigSet2) {
      itemViewConfigs.add(config);
    }
    assertEquals(itemViewConfigs.get(0).getItemDiscoverableSchedules().isDiscoverable(),
        itemViewConfigDTOs.get(0).getItemDiscoverableSchedules().isDiscoverable());
    assertEquals(itemViewConfigs.get(0).getItemDiscoverableSchedules().getStartDateTime(),
        itemViewConfigDTOs.get(0).getItemDiscoverableSchedules().getStartDateTime());
    assertEquals(itemViewConfigs.get(0).getItemDiscoverableSchedules().getEndDateTime(),
        itemViewConfigDTOs.get(0).getItemDiscoverableSchedules().getEndDateTime());
  }

  @Test
  public void getItemViewConfigsBuyableScheduleTest() {
    Set<ItemViewConfigDTO> itemViewConfigSet1 = new HashSet<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList1) {
      itemViewConfigSet1 =
          (ResponseHelper.getItemViewConfigs("ALL", itemPickupPoint.getAllItemViewConfigs()));
    }
    List<ItemViewConfigDTO> itemViewConfigDTOs = new ArrayList<>();
    for (ItemViewConfigDTO config : itemViewConfigSet1) {
      itemViewConfigDTOs.add(config);
    }
    List<ItemViewConfig> itemViewConfigs = new ArrayList<>();
    for (ItemViewConfig config : itemViewConfigSet2) {
      itemViewConfigs.add(config);
    }
    assertEquals(itemViewConfigs.get(0).getItemBuyableSchedules().isBuyable(),
        itemViewConfigDTOs.get(0).getItemBuyableSchedules().isBuyable());
    assertEquals(itemViewConfigs.get(0).getItemBuyableSchedules().getStartDateTime(),
        itemViewConfigDTOs.get(0).getItemBuyableSchedules().getStartDateTime());
    assertEquals(itemViewConfigs.get(0).getItemBuyableSchedules().getEndDateTime(),
        itemViewConfigDTOs.get(0).getItemBuyableSchedules().getEndDateTime());
  }
  @Test
  public void toItemSummaryListResponseSwitchTrueMoreRequestTest() {
    SwitchContext switchContext = getSwitchContext(false);
    switchContext.setCncForWarehouseFeatureSwitch(true);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, "DEFAULT,CNC");
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemSummaryListResponseSwitchTrueMoreRequestEmptyConfigTest() {
    itemPickupPointList.get(0).setItemViewConfig(new HashSet<>());
    itemPickupPointList.get(1).setItemViewConfig(new HashSet<>());
    itemPickupPointList.get(2).setItemViewConfig(new HashSet<>());
    SwitchContext switchContext = getSwitchContext(false);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, "DEFAULT,CNC");
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
  }

  private static SwitchContext getSwitchContext(boolean overrideLateFulfillmentByProductType) {
    SwitchContext switchContext = new SwitchContext();
    switchContext.setCncForWarehouseFeatureSwitch(true);
    switchContext.setUsePcbMasterData(false);
    switchContext.setSubscriptionAtL5Flow(false);
    switchContext.setOverrideLateFulfillmentByProductType(overrideLateFulfillmentByProductType);
    switchContext.setRemoveTakenDownL5FromResponse(false);
    return switchContext;
  }

  @Test
  public void toItemSummaryListResponseSwitchTruCncRequestTest() {
    productMap.entrySet().iterator().next().getValue().setSizeChartCode(SIZE_CHART_CODE);
    SwitchContext switchContext = getSwitchContext(false);
    switchContext.setCncForWarehouseFeatureSwitch(true);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
        ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap, new HashMap<>(),
            offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(), new HashMap<>(),
          switchContext, "CNC");
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
    assertEquals(SIZE_CHART_CODE, itemSummaryListResponseList.get(0).getSizeChartCode());
  }

  @Test
  public void toItemSummaryListResponseSubscriptionL5LevelTest() {
    itemPickupPointList.forEach(itemPickupPoint -> itemPickupPoint.setSubscribable(true));
    itemMap.get(ITEM_SKU_2).setSubscribable(true);
    SwitchContext switchContext = getSwitchContext(false);
    switchContext.setSubscriptionAtL5Flow(true);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, StringUtils.EMPTY);
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
    assertTrue(itemSummaryListResponseList.get(0).isSubscribable());
    assertTrue(itemSummaryListResponseList.get(0).isSubscribableAtL5Level());
  }

  @Test
  public void toItemSummaryListResponseForFbbTest() {
    itemPickupPointList.get(2).setFbbActivated(true);
    SwitchContext switchContext = getSwitchContext(false);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, StringUtils.EMPTY);
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
    assertTrue(itemSummaryListResponseList.get(0).isFbbActivated());
  }
  @Test
  public void toItemSummaryListResponseWithEmptyDiscountTest() {
    itemPickupPointList.get(0).setPrice(Collections.singleton(new Price()));
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(ITEM_SKU_2);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(VALUE);
    productAttributeDetail.setAttributeName(NAME);
    productAttributeDetail.setAttributeCode(CODE);
    productAttribute.setProductAttributeDetails(Collections.singletonList(productAttributeDetail));
    product.setDefiningAttributes(Collections.singletonList(productAttribute));
    productMap.replace(PRODUCT_SKU_2, product);
    SwitchContext switchContext = getSwitchContext(false);
    List<ItemSummaryListResponse> itemSummaryListResponses =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, StringUtils.EMPTY);
    assertEquals(CODE, itemSummaryListResponses.get(0).getDefiningAttributes().get(0).getAttributeCode());
    assertEquals(NAME, itemSummaryListResponses.get(0).getDefiningAttributes().get(0).getAttributeName());
    assertEquals(VALUE, itemSummaryListResponses.get(0).getDefiningAttributes().get(0).getAttributeValue());
  }

  @Test
  public void toItemSummaryListResponseMasterDataItemAttributeTest() {
    MasterDataItemAttributeValue masterDataItemAttributeValue1 = new MasterDataItemAttributeValue();
    MasterDataItemAttributeValue masterDataItemAttributeValue2 = new MasterDataItemAttributeValue();
    MasterDataItemAttributeValue masterDataItemAttributeValue3 = new MasterDataItemAttributeValue();
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setVariantCreation(true);
    masterDataItemAttributeValue1.setMarkForDelete(true);
    masterDataItemAttributeValue2.setMasterDataAttribute(new MasterDataAttribute());
    masterDataItemAttributeValue3.setMasterDataAttribute(masterDataAttribute);
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(Arrays.asList(masterDataItemAttributeValue1, masterDataItemAttributeValue2, masterDataItemAttributeValue3));
    item.setMasterDataItem(masterDataItem);
    SwitchContext switchContext = getSwitchContext(false);
    switchContext.setUsePcbMasterData(true);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, StringUtils.EMPTY);
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
  }

  @Test
  public void toItemSummaryListResponseUsePcbDataTrueTest() {
    productMap.get(PRODUCT_SKU_2).setDimensionsMissing(true);
    SwitchContext switchContext = getSwitchContext(true);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, StringUtils.EMPTY);
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertTrue(itemSummaryListResponseList.get(0).getDimensionsMissing());
  }

  @Test
  public void toItemSummaryListResponseUnsyncTest() {
    product.setMasterDataProduct(masterDataProduct);
    item.setMasterDataItem(masterDataItem);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    product.setSynchronized(false);
    SwitchContext switchContext = getSwitchContext(false);
    switchContext.setMainImageFromMainImageUrlForUnsync(true);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, StringUtils.EMPTY);
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(MAIN_IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemSummaryListResponseUnsyncNoMasterDataTest() {
    product.setSynchronized(false);
    SwitchContext switchContext = getSwitchContext(false);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, StringUtils.EMPTY);
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
  }

  @Test
  public void toItemSummaryListResponseValidBuyableAndDiscoverableScheduleTest() {
    itemPickupPointList.get(2).getItemViewConfig()
        .forEach(itemViewConfig -> itemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule()));
    itemPickupPointList.get(2).getItemViewConfig()
        .forEach(itemViewConfig -> itemViewConfig.setItemDiscoverableSchedules(new ItemDiscoverableSchedule()));
    SwitchContext switchContext = getSwitchContext(false);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, StringUtils.EMPTY);
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemSummaryListResponseDiscountPriceTest() {
    Price price = new Price();
    price.setOfferPrice(10.0);
    price.setListPrice(10.0);
    DiscountPrice discountPrice1 = new DiscountPrice();
    DiscountPrice discountPrice2 = new DiscountPrice();
    discountPrice2.setAdjustmentType(AdjustmentType.BLIBLI);
    price.setListOfDiscountPrices(Arrays.asList(discountPrice1, discountPrice2));
    price.setMerchantPromoDiscountPrice(discountPrice1);
    itemPickupPoint3.setPrice(ImmutableSet.of(price));
    itemPickupPoint3.setB2bFields(null);
    ItemPickupPoint itemPickupPoint4 = new ItemPickupPoint();
    itemPickupPoint4.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint4.setItemSku(ITEM_SKU_1);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setChannel(Constants.B2B);
    itemViewConfig1.setDiscoverable(true);
    itemPickupPoint4.setItemViewConfig(ImmutableSet.of(itemViewConfig1));
    Map<String, ItemViewConfig> itemViewConfigB2bMap = new HashMap<>();
    itemViewConfigB2bMap.put(OFFLINE_ID, itemViewConfig1);
    itemPickupPointList.add(itemPickupPoint4);
    SwitchContext switchContext = getSwitchContext(false);
    Video video = new Video();
    video.setSourceUrl(SOURCE_URL);
    productMap.entrySet().iterator().next().getValue().setVideo(video);
    productMap.entrySet().iterator().next().getValue().setUrl(IMAGE_URL);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, itemViewConfigB2bMap,
        new HashMap<>(), switchContext, StringUtils.EMPTY);
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
    assertTrue(itemSummaryListResponseList.get(0).isMerchantPromoDiscountActivated());
    assertEquals(SOURCE_URL, itemSummaryListResponseList.get(0).getVideo().getUrl());
    assertEquals(StringUtils.EMPTY, itemSummaryListResponseList.get(0).getUrl());
  }

  @Test
  public void toB2bDtoNullTest() {
    ItemViewConfigDTO itemViewConfigDTO = ResponseHelper.toItemViewConfigDTO(null);
  }

  @Test
  public void toItemSummaryListResponseMerchantPromoDiscountPriceTest() {
    Price price = new Price();
    price.setOfferPrice(10.0);
    price.setListPrice(10.0);
    DiscountPrice discountPrice1 = new DiscountPrice();
    DiscountPrice discountPrice2 = new DiscountPrice();
    discountPrice2.setAdjustmentType(AdjustmentType.BLIBLI);
    price.setListOfDiscountPrices(Arrays.asList(discountPrice1, discountPrice2));
    price.setMerchantPromoDiscountPrice(null);
    itemPickupPoint3.setPrice(ImmutableSet.of(price));
    ItemPickupPoint itemPickupPoint5 = new ItemPickupPoint();
    SwitchContext switchContext = getSwitchContext(false);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        new HashMap<>(), offLineItemIdAndItemViewConfigOriginalMap, new HashMap<>(),
        new HashMap<>(), switchContext, StringUtils.EMPTY);
    assertEquals(ITEM_SKU_2, itemSummaryListResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSummaryListResponseList.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU_2, itemSummaryListResponseList.get(0).getProductSku());
    assertEquals(IMAGE_URL, itemSummaryListResponseList.get(0).getMainImageUrl());
    assertFalse(itemSummaryListResponseList.get(0).isMerchantPromoDiscountActivated());
    assertTrue(itemSummaryListResponseList.get(0).isB2bActivated());
    assertTrue(itemSummaryListResponseList.get(0).isB2cActivated());
  }

  @Test
  public void toBusinessPartnerPickupPointResponseTest() {
    List<BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponseList =
        ResponseHelper.toBusinessPartnerPickupPointResponse(Arrays.asList(businessPartnerPickupPoint));
    assertEquals(BUSINESS_PARTNER_CODE, businessPartnerPickupPointResponseList.get(0).getBusinessPartnerCode());
    assertEquals(PLACE_ID, businessPartnerPickupPointResponseList.get(0).getGeolocation().getPlaceId());
    assertEquals(STREET_ADDRESS,
        businessPartnerPickupPointResponseList.get(0).getGeolocation().getStreetAddress());
    assertEquals(LONGITUDE, businessPartnerPickupPointResponseList.get(0).getGeolocation().getLongitude(), 0);
    assertEquals(LATITUDE, businessPartnerPickupPointResponseList.get(0).getGeolocation().getLatitude(), 0);
  }

  @Test
  public void toBusinessPartnerPickupPointResponseGeolocationNullTest() {
    businessPartnerPickupPoint.setGeolocation(null);
    List<BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponseList =
        ResponseHelper.toBusinessPartnerPickupPointResponse(Arrays.asList(businessPartnerPickupPoint));
    assertEquals(BUSINESS_PARTNER_CODE, businessPartnerPickupPointResponseList.get(0).getBusinessPartnerCode());
    assertNull(businessPartnerPickupPointResponseList.get(0).getGeolocation());
  }

  @Test
  public void toItemPickupPointListingResponseWithoutL5Test() {
    product.setFbbActivated(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponseWithoutL5(product, item);
    assertTrue(itemPickupPointListingResponseList.get(0).isFbbActiveAtL3Level());
  }

  @Test
  public void toItemPickupPointListingResponseWithoutL5WithNoProductScoreTest() {
    product.setFbbActivated(true);
    product.setProductScore(null);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponseWithoutL5(product, item);
    assertTrue(itemPickupPointListingResponseList.get(0).isFbbActiveAtL3Level());
    assertEquals(0.0, itemPickupPointListingResponseList.get(0).getProductScore(),0);
  }

  @Test
  public void toItemPickupPointListingResponseTest() {
    item.setMainImageUrl(MAIN_IMAGE_URL);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
             businessPartnerPickupPointMap, false, new HashMap<>(), false, false, Constants.CNC, true, false);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertEquals(MAIN_IMAGE_URL, itemPickupPointListingResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemPickupPointListingResponseDefiningAttributesTest() {
    product.setDefiningAttributes(Collections.singletonList(new ProductAttribute()));
    productMap.putIfAbsent(PRODUCT_SKU_2, product);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, Constants.DEFAULT, true, false);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(MAIN_IMAGE_URL, itemPickupPointListingResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemPickupPointListingResponseWholesaleNullTest() {
    product.setDefiningAttributes(Collections.singletonList(new ProductAttribute()));
    productMap.putIfAbsent(PRODUCT_SKU_2, product);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, Constants.DEFAULT, true, true);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(MAIN_IMAGE_URL, itemPickupPointListingResponseList.get(0).getMainImageUrl());
    Assertions.assertNull(itemPickupPointListingResponseList.get(0).getWholesalePriceActivated());
  }

  @Test
  public void toItemPickupPointListingResponseWholesalePriceExistsTest() {
    product.setDefiningAttributes(Collections.singletonList(new ProductAttribute()));
    productMap.putIfAbsent(PRODUCT_SKU_2, product);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    itemPickupPointList.forEach(itemPickupPoint -> itemPickupPoint.setWholesalePriceExists(true));
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, Constants.DEFAULT, true, true);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(MAIN_IMAGE_URL, itemPickupPointListingResponseList.get(0).getMainImageUrl());
    Assertions.assertFalse(itemPickupPointListingResponseList.get(0).getWholesalePriceActivated());
  }

  @Test
  public void toItemPickupPointListingResponseWholesalePriceActivatedTest() {
    product.setDefiningAttributes(Collections.singletonList(new ProductAttribute()));
    productMap.putIfAbsent(PRODUCT_SKU_2, product);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    itemPickupPointList.forEach(itemPickupPoint -> itemPickupPoint.setWholesalePriceExists(true));
    itemPickupPointList.forEach(itemPickupPoint -> itemPickupPoint.setActivePromoBundlings(Collections.singleton(Constants.WHOLESALE_PRICE)));
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, Constants.DEFAULT, true, true);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(MAIN_IMAGE_URL, itemPickupPointListingResponseList.get(0).getMainImageUrl());
    Assertions.assertTrue(itemPickupPointListingResponseList.get(0).getWholesalePriceActivated());
  }

  @Test
  public void toItemPickupPointListingResponseDefiningAttributes2Test() {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(ITEM_SKU_2);
    product.setDefiningAttributes(Collections.singletonList(productAttribute));
    product.setMissingFields(Set.of(DIMENSION, DESCRIPTION));
    productMap.putIfAbsent(PRODUCT_SKU_2, product);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, null, false, false);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(MAIN_IMAGE_URL, itemPickupPointListingResponseList.get(0).getMainImageUrl());
    assertEquals(Set.of(DIMENSION, DESCRIPTION), itemPickupPointListingResponseList.get(0).getMissingFields());
  }

  @Test
  public void toItemPickupPointListingResponseDefiningAttributes3Test() {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(ITEM_SKU_2);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttribute.setProductAttributeDetails(Collections.singletonList(productAttributeDetail));
    product.setDefiningAttributes(Collections.singletonList(productAttribute));
    productMap.putIfAbsent(PRODUCT_SKU_2, product);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, null, false, false);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(MAIN_IMAGE_URL, itemPickupPointListingResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemPickupPointListingResponseDefiningAttributes6Test() {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(ITEM_SKU_1);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttribute.setProductAttributeDetails(Collections.singletonList(productAttributeDetail));
    product.setDefiningAttributes(Collections.singletonList(productAttribute));
    productMap.putIfAbsent(PRODUCT_SKU_2, product);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, null, false, false);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(MAIN_IMAGE_URL, itemPickupPointListingResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemPickupPointListingResponseDefiningAttributes4Test() {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(ITEM_SKU_2);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttribute.setProductAttributeDetails(Collections.singletonList(productAttributeDetail));
    product.setDefiningAttributes(Collections.singletonList(productAttribute));
    productMap.putIfAbsent(PRODUCT_SKU_2, product);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false , null, false, false);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(MAIN_IMAGE_URL, itemPickupPointListingResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemPickupPointListingResponseDefiningAttributes5Test() {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(ITEM_SKU_2);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttribute.setProductAttributeDetails(Collections.singletonList(productAttributeDetail));
    product.setDefiningAttributes(Collections.singletonList(productAttribute));
    productMap.putIfAbsent(PRODUCT_SKU_2, product);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, null, false, false);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(MAIN_IMAGE_URL, itemPickupPointListingResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemPickupPointListingResponseDefiningAttributes7Test() {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(ITEM_SKU_2);
    productAttribute.setProductAttributeDetails(new ArrayList<>());
    product.setDefiningAttributes(Collections.singletonList(productAttribute));
    productMap.putIfAbsent(PRODUCT_SKU_2, product);
    item.setMainImageUrl(MAIN_IMAGE_URL);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, null, false, false);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(MAIN_IMAGE_URL, itemPickupPointListingResponseList.get(0).getMainImageUrl());
  }

  @Test
  public void toItemPickupPointListingResponseB2BNotNullTest() {
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPoint3.setB2bFields(new B2bFields(true, 1000.0));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, true, new HashMap<>(), false, false, null, false, false);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertTrue(itemPickupPointListingResponseList.get(0).getB2bFields().isManaged());
    assertEquals(1000.0, itemPickupPointListingResponseList.get(0).getB2bFields().getBasePrice(), 0);
  }


  @Test
  public void toItemPickupPointListingResponseMerchantPromoDiscountFalseTest() {
    itemPickupPoint3.getPrice().iterator().next().setListOfDiscountPrices(new ArrayList<>());
    itemPickupPoint3.setMerchantPromoDiscount(false);
    itemPickupPoint3.getItemViewConfig().stream().findFirst()
        .orElse(new ItemViewConfig()).setItemBuyableSchedules(itemBuyableSchedule);
    itemPickupPoint3.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig())
        .setItemDiscoverableSchedules(itemDiscoverableSchedule);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), true, false, null, false, false);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertFalse(itemPickupPointListingResponseList.get(0).isPriceEditDisabled());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertFalse(itemPickupPointListingResponseList.get(0).isMerchantPromoDiscountActivated());
    assertEquals(TEN_DAYS_AGO, itemPickupPointListingResponseList
        .get(0).getViewConfigs().get(0).getBuyableScheduleResponse().getStartDateTime());
    assertEquals(TEN_DAYS_AFTER, itemPickupPointListingResponseList
        .get(0).getViewConfigs().get(0).getBuyableScheduleResponse().getEndDateTime());
    assertTrue(itemPickupPointListingResponseList
        .get(0).getViewConfigs().get(0).getBuyableScheduleResponse().isBuyable());
  }

  @Test
  public void toItemPickupPointListingResponseDiscountNullFalseTest() {
    itemPickupPoint3.getPrice().forEach(p -> p.setMerchantPromoDiscountPrice(null));
    itemPickupPoint3.setMerchantPromoDiscount(true);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, null, false, false);
    assertEquals(ITEM_SKU_2, itemPickupPointListingResponseList.get(0).getItemSku());
    assertEquals(1, itemPickupPointListingResponseList.get(0).getItemNumber());
    assertEquals(10.0, itemPickupPointListingResponseList.get(0).getProductScore(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
  }


  @Test
  public void toItemPickupPointListingResponsMerchantPromoDiscountTrueTest() {
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.setItemViewConfig(null);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(PRICE_2);
    itemPickupPoint3.getPrice().forEach(price1 -> price1.setMerchantPromoDiscountPrice(discountPrice));
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, true, new HashMap<>(), false, false, null, false, false);
    assertTrue(itemPickupPointListingResponseList.get(0).isPriceEditDisabled());
    assertFalse(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertFalse(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(itemPickupPointListingResponseList.get(0).getPrices().get(0).getSalePrice(), PRICE_2, 0);
  }

  @Test
  public void toItemPickupPointListingDiscountPercentageTest() {
    itemPickupPoint3.setMerchantPromoDiscount(true);
    Price price1 = new Price();
    price1.setListPrice(1000.00);
    price1.setOfferPrice(899.00);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(899.00);
    price1.setMerchantPromoDiscountPrice(discountPrice);
    itemPickupPoint3.getPrice().forEach(price -> price.setMerchantPromoDiscountPrice(discountPrice));
    itemPickupPoint3.setPrice(Collections.singleton(price1));
    itemPickupPoint3.setItemViewConfig(null);
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, true, new HashMap<>(), false, false, null, false, false);
    assertTrue(itemPickupPointListingResponseList.get(0).isPriceEditDisabled());
    assertFalse(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertFalse(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(itemPickupPointListingResponseList.get(0).getPrices().get(0).getSalePrice(), 899.00, 0);
    assertEquals(10.1,
        itemPickupPointListingResponseList.get(0).getPrices().get(0).getDiscountPercentage(), 0);
  }

  @Test
  public void toItemPickupPointListingResponsMerchantPromoDiscountTrueReplaceMerchantDiscountPriceFalseTest() {
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.setItemViewConfig(null);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(PRICE_2);
    discountPrice.setEndDateTime(TEN_DAYS_AGO);
    itemPickupPoint3.getPrice().forEach(price1 -> price1.setMerchantPromoDiscountPrice(discountPrice));
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(Arrays.asList(itemPickupPoint3), productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), true, false, null, false, false);
    assertFalse(itemPickupPointListingResponseList.get(0).isPriceEditDisabled());
    assertFalse(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertFalse(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(itemPickupPointListingResponseList.get(0).getPrices().get(0).getSalePrice(), price.getOfferPrice(), 0);
    assertFalse(itemPickupPointListingResponseList.get(0).isMerchantPromoDiscountActivated());
    assertFalse(itemPickupPointListingResponseList.get(0).isMerchantPromoDiscount());
  }

  @Test
  public void toItemPickupPointListingResponsMerchantPromoDiscountTrueReplaceMerchantDiscountPriceTureTest() {
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.setItemViewConfig(new HashSet<>());
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(PRICE_2);
    discountPrice.setEndDateTime(TEN_DAYS_AFTER);
    itemPickupPoint3.getPrice().forEach(price1 -> price1.setMerchantPromoDiscountPrice(discountPrice));
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(Arrays.asList(itemPickupPoint3), productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), true, false, Constants.CNC, true, false);
    assertTrue(itemPickupPointListingResponseList.get(0).isPriceEditDisabled());
    assertFalse(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isBuyable());
    assertFalse(itemPickupPointListingResponseList.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(itemPickupPointListingResponseList.get(0).getPrices().get(0).getSalePrice(), price.getOfferPrice(), 0);
    assertTrue(itemPickupPointListingResponseList.get(0).isMerchantPromoDiscountActivated());
    assertTrue(itemPickupPointListingResponseList.get(0).isMerchantPromoDiscount());
  }

  @Test
  public void toItemPickupPointListingResponsDiscountPricesTest() {
    Date now = new Date();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(100);
    discountPrice.setCampaignCode(CAMPAIGN_CODE);
    discountPrice.setStartDateTime(DateUtils.addDays(now, -1));
    discountPrice.setEndDateTime(DateUtils.addDays(now, 1));

    itemPickupPoint3.getPrice().iterator().next().setListOfDiscountPrices(Collections.singletonList(discountPrice));
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, null, false, false);
    assertTrue(itemPickupPointListingResponseList.get(0).isPriceEditDisabled());
    assertEquals(100, itemPickupPointListingResponseList.get(0).getPrices().get(0).getDiscountAmount(), 0);
    assertEquals(0, itemPickupPointListingResponseList.get(0).getOriginalSellingPrice(), 0);
  }

  @Test
  public void toItemPickupPointListingResponsDiscountPricesPromoEndedTest() {
    Date now = new Date();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(CAMPAIGN_CODE);
    discountPrice.setStartDateTime(DateUtils.addDays(now, -2));
    discountPrice.setEndDateTime(DateUtils.addDays(now, -1));

    itemPickupPoint3.getPrice().iterator().next().setListOfDiscountPrices(Arrays.asList(discountPrice));
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, null, false, false);
    assertFalse(itemPickupPointListingResponseList.get(0).isPriceEditDisabled());
  }

  @Test
  public void toItemPickupPointListingResponsDiscountPricesPromoUpcomingTest() {
    Date now = new Date();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(CAMPAIGN_CODE);
    discountPrice.setStartDateTime(DateUtils.addDays(now, 1));
    discountPrice.setEndDateTime(DateUtils.addDays(now, 2));

    itemPickupPoint3.getPrice().iterator().next().setListOfDiscountPrices(Arrays.asList(discountPrice));
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, new HashMap<>(), false, false, null, false, false);
    assertFalse(itemPickupPointListingResponseList.get(0).isPriceEditDisabled());
    assertEquals(itemPickupPointListingResponseList.get(0).getOriginalSellingPrice(), 0.0, 0.0);
  }

  @Test
  public void toItemPickupPointListingResponsCampaignCodeNullTest() {
    Date now = new Date();
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setStartDateTime(DateUtils.addDays(now, 1));
    discountPrice.setEndDateTime(DateUtils.addDays(now, 2));
    Map<String, Double> map = new HashMap<>();
    map.put(itemPickupPoint3.getItemSku() + Constants.HYPHEN + itemPickupPoint3.getPickupPointCode(), ORIGINAL_SELLING_PRICE_2);

    itemPickupPoint3.getPrice().iterator().next().setListOfDiscountPrices(Arrays.asList(discountPrice));
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(itemPickupPointList, productMap, itemMap,
            businessPartnerPickupPointMap, false, map, false, false, null, false, false);
    assertFalse(itemPickupPointListingResponseList.get(0).isPriceEditDisabled());
    assertEquals(itemPickupPointListingResponseList.get(0).getOriginalSellingPrice(), ORIGINAL_SELLING_PRICE_2, 0.0);
  }

  @Test
  public void toItemPriceResponseTest() {
    List<ItemPriceResponse> itemPriceResponseList =
        ResponseHelper.toItemPriceResponse(PICKUP_POINT_CODE, Arrays.asList(item),
            ImmutableMap.of(ITEM_SKU_2, itemPickupPoint3), ImmutableMap.of(ITEM_SKU_2, ImmutableSet.of(price)));
    assertEquals(ITEM_SKU_2, itemPriceResponseList.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemPriceResponseList.get(0).getPickupPointCode());
    assertEquals(10.0, itemPriceResponseList.get(0).getListPrice(), 0);
    assertEquals(10.0, itemPriceResponseList.get(0).getOfferPrice(), 0);
    assertTrue(itemPriceResponseList.get(0).isBuyable());
  }

  @Test
  public void toItemPriceResponseEmptyItemListTest() {
    List<ItemPriceResponse> itemPriceResponseList =
        ResponseHelper.toItemPriceResponse(PICKUP_POINT_CODE, new ArrayList<>(),
            ImmutableMap.of(ITEM_SKU_2, itemPickupPoint3), ImmutableMap.of(ITEM_SKU_2, ImmutableSet.of(price)));
    assertTrue(itemPriceResponseList.isEmpty());
  }

  @Test
  public void testToPriceDetailResponse() {
    Set<Price> priceSet = new HashSet<>();
    price.setListPrice(LIST_PRICE);
    price.setOfferPrice(OFFER_PRICE);
    priceSet.add(price);
    this.itemPickupPoint1.setPrice(priceSet);
    List<ItemPickupPointPriceResponse> itemPickupPointPriceResponses =
        ResponseHelper.toPriceDetailResponse(Collections.singletonList(itemPickupPoint1), false);
    assertEquals(ITEM_SKU_1, itemPickupPointPriceResponses.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemPickupPointPriceResponses.get(0).getPickupPointCode());
    assertEquals(LIST_PRICE, itemPickupPointPriceResponses.get(0).getListPrice(),0);
    assertEquals(OFFER_PRICE, itemPickupPointPriceResponses.get(0).getOfferPrice(),0);
  }

  @Test
  public void testToPriceDetailResponse3() {
    Set<Price> priceSet = new HashSet<>();
    price.setListPrice(LIST_PRICE);
    price.setOfferPrice(OFFER_PRICE);
    priceSet.add(price);
    this.itemPickupPoint1.setPrice(priceSet);
    List<ItemPickupPointPriceResponse> itemPickupPointPriceResponses =
        ResponseHelper.toPriceDetailResponse(Collections.EMPTY_LIST, false);
    assertTrue(CollectionUtils.isEmpty(itemPickupPointPriceResponses));
  }

  @Test
  public void testToPriceDetailResponse2() {
    Set<Price> priceSet = new HashSet<>();
    price.setListPrice(LIST_PRICE);
    price.setOfferPrice(OFFER_PRICE);
    priceSet.add(price);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setPriority(1);
    discountPrice.setDiscountPrice(LIST_PRICE-OFFER_PRICE);
    discountPrice.setCampaignCode(CAMPAIGN_CODE);
    this.itemPickupPoint3.getPrice().iterator().next().setListOfDiscountPrices(Collections.singletonList(discountPrice));
    this.itemPickupPoint3.getPrice().iterator().next().setOfferPrice(OFFER_PRICE);
    this.itemPickupPoint3.getPrice().iterator().next().setListPrice(LIST_PRICE);
    List<ItemPickupPointPriceResponse> itemPickupPointPriceResponses =
        ResponseHelper.toPriceDetailResponse(Collections.singletonList(itemPickupPoint3), false);
    assertEquals(ITEM_SKU_2, itemPickupPointPriceResponses.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemPickupPointPriceResponses.get(0).getPickupPointCode());
    assertEquals(LIST_PRICE, itemPickupPointPriceResponses.get(0).getListPrice(),0);
    assertEquals(OFFER_PRICE, itemPickupPointPriceResponses.get(0).getOfferPrice(),0);
  }

  @Test
  public void testToPriceDetailResponse_cnc1p() {
    Set<Price> priceSet = new HashSet<>();
    price.setListPrice(LIST_PRICE);
    price.setOfferPrice(OFFER_PRICE);
    priceSet.add(price);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setPriority(1);
    discountPrice.setDiscountPrice(LIST_PRICE-OFFER_PRICE);
    discountPrice.setCampaignCode(CAMPAIGN_CODE);
    this.itemPickupPoint3.getPrice().iterator().next().setListOfDiscountPrices(Collections.singletonList(discountPrice));
    this.itemPickupPoint3.getPrice().iterator().next().setOfferPrice(OFFER_PRICE);
    this.itemPickupPoint3.getPrice().iterator().next().setListPrice(LIST_PRICE);
    itemPickupPoint3.setCncActive(false);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.CNC);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(false);
    Set<ItemViewConfig> itemViewConfigTemp = new HashSet<>(itemPickupPoint3.getAllItemViewConfigs());
    itemViewConfigTemp.add(itemViewConfig);
    itemPickupPoint3.setItemViewConfig(itemViewConfigTemp);
    List<ItemPickupPointPriceResponse> itemPickupPointPriceResponses =
        ResponseHelper.toPriceDetailResponse(Collections.singletonList(itemPickupPoint3), true);
    assertEquals(ITEM_SKU_2, itemPickupPointPriceResponses.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemPickupPointPriceResponses.get(0).getPickupPointCode());
    assertEquals(LIST_PRICE, itemPickupPointPriceResponses.get(0).getListPrice(),0);
    assertEquals(OFFER_PRICE, itemPickupPointPriceResponses.get(0).getOfferPrice(),0);
    assertTrue(itemPickupPointPriceResponses.get(0).isCncActive());
    assertTrue(itemPickupPointPriceResponses.get(0).isCncBuyable());
    assertFalse(itemPickupPointPriceResponses.get(0).isCncDiscoverable());
  }

  @Test
  public void toBasicItemDTODangerousLevelNullTest() {
    Set<Price> prices = new HashSet<>();
    prices.add(price);
    item.setDangerousLevel(null);
    item.setSynchronized(true);
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPoint1.setPrice(prices);
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    BasicItemDTO basicItemDTO = ResponseHelper.toBasicItemDTO(item, itemPickupPoint1);
    assertEquals(basicItemDTO.getItemSku(), item.getItemSku());
    assertEquals(basicItemDTO.getMasterDataItem().getDangerousLevel(), 0 , 0);
  }

  @Test
  public void toBasicItemDTOTest() {
    Set<Price> prices = new HashSet<>();
    prices.add(price);
    item.setSynchronized(true);
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPoint1.setPrice(prices);
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    BasicItemDTO basicItemDTO = ResponseHelper.toBasicItemDTO(item, itemPickupPoint1);
    assertEquals(basicItemDTO.getItemSku(), item.getItemSku());
  }

  @Test
  public void toBasicItemDTODimensionsTest() {
    Set<Price> prices = new HashSet<>();
    prices.add(price);
    item.setSynchronized(true);
    item.setHeight(12.22);
    item.setWeight(12.33);
    item.setLength(23.33);
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPoint1.setPrice(prices);
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    BasicItemDTO basicItemDTO = ResponseHelper.toBasicItemDTO(item, itemPickupPoint1);
    assertEquals(basicItemDTO.getItemSku(), item.getItemSku());
    assertEquals(basicItemDTO.getMasterDataItem().getItemLength(), item.getLength());
    assertEquals(basicItemDTO.getMasterDataItem().getItemWeight(), item.getWeight());
    assertEquals(basicItemDTO.getMasterDataItem().getItemHeight(), item.getHeight());
  }

  @Test
  public void toBasicItemDTODangerousLevelNullSynchronizedFalseTest() {
    Set<Price> prices = new HashSet<>();
    prices.add(price);
    item.setDangerousLevel(null);
    item.setSynchronized(false);
    item.setMasterDataItem(new MasterDataItem());
    item.getMasterDataItem().setItemDeliveryWeight(SHIPPING_WT);
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPoint1.setPrice(prices);
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    BasicItemDTO basicItemDTO = ResponseHelper.toBasicItemDTO(item, itemPickupPoint1);
    assertEquals(basicItemDTO.getItemSku(), item.getItemSku());
    assertEquals(basicItemDTO.getMasterDataItem().getDangerousLevel(), 0 , 0);
  }

  @Test
  public void toBasicItemDTODangerousLevelNullSynchronizedFalseWeightNullTest() {
    Set<Price> prices = new HashSet<>();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice1, discountPrice2));
    prices.add(price);
    item.setDangerousLevel(null);
    item.setSynchronized(false);
    item.setMasterDataItem(new MasterDataItem());
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPoint1.setPrice(prices);
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    BasicItemDTO basicItemDTO = ResponseHelper.toBasicItemDTO(item, itemPickupPoint1);
    assertEquals(basicItemDTO.getItemSku(), item.getItemSku());
    assertEquals(basicItemDTO.getMasterDataItem().getDangerousLevel(), 0 , 0);
    assertEquals(basicItemDTO.getPrice().iterator()
        .next().getListOfDiscountPrices().get(0).getPriority(), 3 , 0);
    assertEquals(basicItemDTO.getPrice().iterator().next()
        .getListOfDiscountPrices().get(1).getPriority(), 1, 0);
  }

  @Test
  public void toBasicItemDTODangerousLevelMasterDataNullTest() {
    Set<Price> prices = new HashSet<>();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice1, discountPrice2));
    prices.add(price);
    item.setDangerousLevel(null);
    item.setSynchronized(false);
    item.setMasterDataItem(null);
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPoint1.setPrice(prices);
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    BasicItemDTO basicItemDTO = ResponseHelper.toBasicItemDTO(item, itemPickupPoint1);
    assertEquals(basicItemDTO.getItemSku(), item.getItemSku());
  }

  @Test
  public void toProductDetailL5ResponseTest() {
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.setMarkForDelete(true);
    product.setBrand(PRISTINE_BRAND);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ProductL5DetailResponse> productL5DetailResponses =
      ResponseHelper.toProductDetailL5Response(itemPickupPointList, productMap, itemMap,
          false, null);
    assertEquals(ITEM_SKU_2, productL5DetailResponses.get(0).getItemSku());
    assertTrue(productL5DetailResponses.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(productL5DetailResponses.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(productL5DetailResponses.get(0).getUpcCode(),PRISTINE_MODEL);
    assertEquals(productL5DetailResponses.get(0).getCategoryCode(),CATEGORY_CODE_2);
    assertTrue(productL5DetailResponses.get(0).isMarkForDelete());
    assertTrue(productL5DetailResponses.get(0).isMarkForDelete());
    assertTrue(productL5DetailResponses.get(0).isL5MarkForDelete());
    assertEquals(PRISTINE_BRAND, productL5DetailResponses.get(0).getBrand());
  }

  @Test
  public void toProductDetailL5ResponseSwitchOnAllInRequestTest() {
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.setMarkForDelete(true);
    product.setBrand(PRISTINE_BRAND);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ProductL5DetailResponse> productL5DetailResponses =
        ResponseHelper.toProductDetailL5Response(itemPickupPointList, productMap, itemMap,
            true, "ALL");
  }

  @Test
  public void toProductDetailL5ResponseSwitchOnEmptyInRequestTest() {
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.setMarkForDelete(true);
    product.setBrand(PRISTINE_BRAND);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ProductL5DetailResponse> productL5DetailResponses =
        ResponseHelper.toProductDetailL5Response(itemPickupPointList, productMap, itemMap,
            true, "");
    Assertions.assertEquals(ITEM_SKU_2, productL5DetailResponses.get(0).getItemSku());
    Assertions.assertTrue(productL5DetailResponses.get(0).getViewConfigs().get(0).isBuyable());
    Assertions.assertTrue(productL5DetailResponses.get(0).getViewConfigs().get(0).isDisplay());
    Assertions.assertEquals(productL5DetailResponses.get(0).getUpcCode(),PRISTINE_MODEL);
    Assertions.assertEquals(productL5DetailResponses.get(0).getCategoryCode(),CATEGORY_CODE_2);
    Assertions.assertTrue(productL5DetailResponses.get(0).isMarkForDelete());
    Assertions.assertTrue(productL5DetailResponses.get(0).isMarkForDelete());
    Assertions.assertTrue(productL5DetailResponses.get(0).isL5MarkForDelete());
    Assertions.assertEquals(PRISTINE_BRAND, productL5DetailResponses.get(0).getBrand());
  }

  @Test
  public void toProductDetailL5ResponseSwitchOnCncInRequestTest() {
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.setMarkForDelete(true);
    product.setBrand(PRISTINE_BRAND);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ProductL5DetailResponse> productL5DetailResponses =
        ResponseHelper.toProductDetailL5Response(itemPickupPointList, productMap, itemMap,
            true, "CNC");
  }

  @Test
  public void toProductDetailL5ResponseEmptyViewConfigTest() {
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.setMarkForDelete(true);
    product.setBrand(PRISTINE_BRAND);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    itemPickupPointList.get(0).setItemViewConfig(new HashSet<>());
    itemPickupPointList.get(2).setItemViewConfig(new HashSet<>());
    itemPickupPointList.get(1).setItemViewConfig(new HashSet<>());
    List<ProductL5DetailResponse> productL5DetailResponses =
        ResponseHelper.toProductDetailL5Response(itemPickupPointList, productMap, itemMap,
            false, null);
  }

  @Test
  public void toProductDetailL5ResponseEditEnabledFalseTest() {
    product.setForceReview(true);
    productMap.put(PRODUCT_SKU_2, product);
    itemPickupPoint3.setMerchantPromoDiscount(true);
    itemPickupPoint3.getPrice().forEach(p -> p.setListOfDiscountPrices(new ArrayList<>()));
    itemPickupPointList.get(0).setFbbActivated(true);
    List<ProductL5DetailResponse> productL5DetailResponses =
            ResponseHelper.toProductDetailL5Response(itemPickupPointList, productMap, itemMap,
                false, null);
    assertEquals(ITEM_SKU_2, productL5DetailResponses.get(0).getItemSku());
    assertTrue(productL5DetailResponses.get(0).getViewConfigs().get(0).isBuyable());
    assertTrue(productL5DetailResponses.get(0).getViewConfigs().get(0).isDisplay());
    assertEquals(productL5DetailResponses.get(0).getUpcCode(),PRISTINE_MODEL);
  }

  @Test
  public void setMainImageForProductsMainImageDontExists() {
    MasterDataProductImageDTO masterDataProductImageDTO = new MasterDataProductImageDTO();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductImages(Arrays.asList(masterDataProductImageDTO));
    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProductDTO);

    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    masterDataItemDTO.setMasterDataItemImages(Arrays.asList(masterDataItemImageDTO));
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setMasterDataItem(masterDataItemDTO);

    ResponseHelper.setMainImageForProducts(productResponse, Arrays.asList(itemResponse), true);

    assertTrue(productResponse.getMasterDataProduct().getMasterDataProductImages().get(0).isMainImage());
    assertTrue(itemResponse.getMasterDataItem().getMasterDataItemImages().get(0).isMainImage());
  }

  @Test
  public void setMainImageForProductsMainImageExists() {
    MasterDataProductImageDTO masterDataProductImageDTO = new MasterDataProductImageDTO();
    masterDataProductImageDTO.setMainImage(true);
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductImages(Arrays.asList(masterDataProductImageDTO));
    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProductDTO);

    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setMainImage(true);
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    masterDataItemDTO.setMasterDataItemImages(Arrays.asList(masterDataItemImageDTO));
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setMasterDataItem(masterDataItemDTO);

    ResponseHelper.setMainImageForProducts(productResponse, Arrays.asList(itemResponse), true);
  }

  @Test
  public void setMainImageForProductNoImageExists() {
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProductDTO);

    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setMasterDataItem(masterDataItemDTO);

    ResponseHelper.setMainImageForProducts(productResponse, Arrays.asList(itemResponse), true);
  }

  @Test
  public void setMainImageForProductsMainImageDontExistsaAndSwitchOff() {
    MasterDataProductImageDTO masterDataProductImageDTO = new MasterDataProductImageDTO();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductImages(Arrays.asList(masterDataProductImageDTO));
    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProductDTO);

    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    masterDataItemDTO.setMasterDataItemImages(Arrays.asList(masterDataItemImageDTO));
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setMasterDataItem(masterDataItemDTO);

    ResponseHelper.setMainImageForProducts(productResponse, Arrays.asList(itemResponse), false);

    assertFalse(productResponse.getMasterDataProduct().getMasterDataProductImages().get(0).isMainImage());
    assertFalse(itemResponse.getMasterDataItem().getMasterDataItemImages().get(0).isMainImage());
  }

  @Test
  public void updateMerchantPromoDiscountFlagsTest() {
    ItemL4SummaryResponse itemL4SummaryResponse = new ItemL4SummaryResponse();
    itemL4SummaryResponse.setMerchantPromoDiscount(true);
    ResponseHelper.updateMerchantPromoDiscountFlags(itemPickupPoint1, itemL4SummaryResponse, true, false);
    assertFalse(itemL4SummaryResponse.isMerchantPromoDiscountActivated());
  }

  @Test
  public void toItemPickupPointCodeResponsesTest() {
    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemSku(ITEM_SKU_1).pickupPointCode(PICKUP_POINT_CODE).build();
    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponses =
        ResponseHelper.toItemPickupPointCodeResponses(Arrays.asList(itemPickupPoint), null);
    Arrays.asList(ITEM_SKU_1, itemPickupPointCodeResponses.get(0).getItemSku());
    Arrays.asList(ITEM_SKU_2, itemPickupPointCodeResponses.get(0).getPickupPointCode());
  }

  @Test
  public void toSharedProductBundleRecipeResponseTest() {
    Item item1 = new Item();
    item1.setItemCode(ITEM_CODE_1);
    item1.setItemSku(ITEM_SKU_1);
    item1.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU_2, 1)));
    Item item2 = new Item();
    item2.setItemCode(ITEM_CODE_1);
    item2.setItemSku(ITEM_SKU_2);
    item2.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU_2, 1)));

    Item item3 = new Item();
    item3.setItemCode(ITEM_CODE_2);
    item3.setItemSku(ITEM_SKU_3);
    item3.setBundleRecipe(Set.of(new BundleRecipe("item-sku-3-1", 1)));

    List<SharedProductBundleRecipeResponse> sharedProductBundleRecipeResponses =
        ResponseHelper.toSharedProductBundleRecipeResponse(Arrays.asList(item1, item2, item3),
            Arrays.asList(item1, item2, item3));

    assertEquals(2, sharedProductBundleRecipeResponses.size());
    assertTrue(sharedProductBundleRecipeResponses.stream()
        .anyMatch(sharedProductBundleRecipeResponse -> sharedProductBundleRecipeResponse.isSharedProduct()));
    assertTrue(sharedProductBundleRecipeResponses.stream()
        .anyMatch(sharedProductBundleRecipeResponse -> !sharedProductBundleRecipeResponse.isSharedProduct()));
    assertTrue(sharedProductBundleRecipeResponses.stream().anyMatch(
        sharedProductBundleRecipeResponse -> sharedProductBundleRecipeResponse.getItemCode().equals(ITEM_CODE_1)));
    assertTrue(sharedProductBundleRecipeResponses.stream().anyMatch(
        sharedProductBundleRecipeResponse -> sharedProductBundleRecipeResponse.getItemCode().equals(ITEM_CODE_2)));
    assertTrue(sharedProductBundleRecipeResponses.stream()
        .anyMatch(sharedProductBundleRecipeResponse -> sharedProductBundleRecipeResponse.getBundleRecipe().isEmpty()));
    assertTrue(sharedProductBundleRecipeResponses.stream().anyMatch(
        sharedProductBundleRecipeResponse -> !sharedProductBundleRecipeResponse.getBundleRecipe().isEmpty()
            && sharedProductBundleRecipeResponse.getBundleRecipe().iterator().next().getItemCode()
            .equals(ITEM_CODE_1)));
  }

  @Test
  public void testToItemCodeBasicDetailResponse_EmptyList() {
    List<Item> emptyList = Collections.emptyList();
    Map<String, Product> productSkuAndProductMap = new HashMap<>();

    List<ItemCodeBasicDetailResponse> result = ResponseHelper.toItemCodeBasicDetailResponse(emptyList, productSkuAndProductMap);

    assertEquals(0, result.size());
  }

  @Test
  public void testToItemCodeBasicDetailResponse_ItemsWithCorrespondingProducts() {
    Item item2 = item;
    item.setProductSku(PRODUCT_SKU_1);
    item2.setItemSku(ITEM_SKU_2);
    item2.setProductSku(PRODUCT_SKU_2);
    List<Item> items = Arrays.asList(item2, item);
    Product product2 = product;
    product.setProductSku(PRODUCT_SKU_1);
    product2.setProductSku(PRODUCT_SKU_2);
    Map<String, Product> productSkuAndProductMap = new HashMap<>();
    productSkuAndProductMap.put(PRODUCT_SKU_1, product);
    productSkuAndProductMap.put(PRODUCT_SKU_2, product2);
    List<ItemCodeBasicDetailResponse> result = ResponseHelper.toItemCodeBasicDetailResponse(items, productSkuAndProductMap);
    assertEquals(2, result.size());

  }

  @Test
  public void testToItemCodeBasicDetailResponse_ItemsWithoutCorrespondingProducts() {
    List<Item> items = Collections.singletonList(item);
    Map<String, Product> productSkuAndProductMap = Collections.emptyMap();
    List<ItemCodeBasicDetailResponse> result = ResponseHelper.toItemCodeBasicDetailResponse(items, productSkuAndProductMap);
    assertEquals(1, result.size());
    assertFalse(result.get(0).isArchivedAtL3());
    assertFalse(result.get(0).isTdSeller());
    assertFalse(result.get(0).isSuspended());
  }

  @Test
  public void testToItemCodeBasicDetailResponse_MixedInput() {
    Item item2 = item;
    item.setArchived(true);
    product.setArchived(true);
    product.setTradingProduct(true);
    item2.setItemSku(ITEM_SKU_2);
    List<Item> items = Arrays.asList(item, item2);
    item2.setProductSku(PRODUCT_SKU_2);
    Map<String, Product> productSkuAndProductMap = new HashMap<>();
    productSkuAndProductMap.put(PRODUCT_SKU_1, product);
    List<ItemCodeBasicDetailResponse> result =
        ResponseHelper.toItemCodeBasicDetailResponse(items, productSkuAndProductMap);
    assertEquals(2, result.size());
  }

  @Test
  public void setBuyableScheduleRequestTest() {
    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    itemBuyableSchedule.setEndDateTime(null);
    ResponseHelper.setBuyableScheduleRequest(viewConfigResponse, itemBuyableSchedule);
    assertNull(viewConfigResponse.getBuyableScheduleResponse());
  }

  @Test
  public void setDiscoverableScheduleTest() {
    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    itemDiscoverableSchedule.setEndDateTime(null);
    ResponseHelper.setDiscoverableScheduleRequest(viewConfigResponse, itemDiscoverableSchedule);
    assertNull(viewConfigResponse.getBuyableScheduleResponse());
  }
  @Test
  public void testEmptyItemPickupPointList() {
    List<ItemPickupPoint> itemPickupPointList = Collections.emptyList();
    List<ItemPickupPointBasicResponse> result = ResponseHelper.toItemPickupPointBasicResponse(itemPickupPointList);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testItemPickupPointWithNullItemViewConfig() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setItemViewConfig(null);

    List<ItemPickupPointBasicResponse> result = ResponseHelper.toItemPickupPointBasicResponse(Arrays.asList(itemPickupPoint));

    assertEquals(1, result.size());
    assertEquals(ITEM_SKU, result.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, result.get(0).getPickupPointCode());
    assertNull(result.get(0).getViewConfigResponse());
  }

  @Test
  public void testItemPickupPointWithEmptyItemViewConfig() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setItemViewConfig(Collections.emptySet());

    List<ItemPickupPointBasicResponse> result = ResponseHelper.toItemPickupPointBasicResponse(Arrays.asList(itemPickupPoint));

    assertEquals(1, result.size());
    assertEquals(ITEM_SKU, result.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, result.get(0).getPickupPointCode());
    assertNull(result.get(0).getViewConfigResponse());
  }

  @Test
  public void testItemPickupPointWithNonNullItemViewConfigAndNullSchedules() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setItemDiscoverableSchedules(null);
    itemViewConfig.setItemBuyableSchedules(null);

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setItemViewConfig(Set.of(itemViewConfig));

    List<ItemPickupPointBasicResponse> result = ResponseHelper.toItemPickupPointBasicResponse(Arrays.asList(itemPickupPoint));

    assertEquals(1, result.size());
    ItemPickupPointBasicResponse response = result.get(0);
    assertEquals(ITEM_SKU, response.getItemSku());
    assertEquals(PICKUP_POINT_CODE, response.getPickupPointCode());
    Assertions.assertNotNull(response.getViewConfigResponse());
    assertNull(response.getViewConfigResponse().getDiscoverableScheduleResponse());
    assertNull(response.getViewConfigResponse().getBuyableScheduleResponse());
    assertTrue(response.getViewConfigResponse().isBuyable());
    assertTrue(response.getViewConfigResponse().isDisplay());
    assertEquals(Constants.DEFAULT, response.getViewConfigResponse().getChannelId());
  }

  @Test
  public void testItemPickupPointWithNonNullItemViewConfigAndNonNullItemDiscoverableSchedule() {
    ItemDiscoverableSchedule discoverableSchedule = new ItemDiscoverableSchedule();
    discoverableSchedule.setStartDateTime(new Date());
    discoverableSchedule.setEndDateTime(new Date());
    discoverableSchedule.setDiscoverable(true);

    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setItemDiscoverableSchedules(discoverableSchedule);
    itemViewConfig.setItemBuyableSchedules(null);

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setItemViewConfig(Set.of(itemViewConfig));

    List<ItemPickupPointBasicResponse> result =
        ResponseHelper.toItemPickupPointBasicResponse(Arrays.asList(itemPickupPoint));

    assertEquals(1, result.size());
    ItemPickupPointBasicResponse response = result.get(0);
    assertEquals(ITEM_SKU, response.getItemSku());
    assertEquals(PICKUP_POINT_CODE, response.getPickupPointCode());
    Assertions.assertNotNull(response.getViewConfigResponse());
    Assertions.assertNotNull(response.getViewConfigResponse().getDiscoverableScheduleResponse());
    assertNull(response.getViewConfigResponse().getBuyableScheduleResponse());
    assertTrue(response.getViewConfigResponse().isBuyable());
    assertTrue(response.getViewConfigResponse().isDisplay());
    assertEquals(Constants.DEFAULT, response.getViewConfigResponse().getChannelId());
  }

  @Test
  public void testItemPickupPointWithNonNullItemViewConfigAndNonNullItemBuyableSchedule() {
    ItemBuyableSchedule buyableSchedule = new ItemBuyableSchedule();
    buyableSchedule.setStartDateTime(new Date());
    buyableSchedule.setEndDateTime(new Date());
    buyableSchedule.setBuyable(true);

    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setItemDiscoverableSchedules(null);
    itemViewConfig.setItemBuyableSchedules(buyableSchedule);

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setItemViewConfig(Set.of(itemViewConfig));

    List<ItemPickupPointBasicResponse> result = ResponseHelper.toItemPickupPointBasicResponse(Arrays.asList(itemPickupPoint));

    assertEquals(1, result.size());
    ItemPickupPointBasicResponse response = result.get(0);
    assertEquals(ITEM_SKU, response.getItemSku());
    assertEquals(PICKUP_POINT_CODE, response.getPickupPointCode());
    Assertions.assertNotNull(response.getViewConfigResponse());
    assertNull(response.getViewConfigResponse().getDiscoverableScheduleResponse());
    Assertions.assertNotNull(response.getViewConfigResponse().getBuyableScheduleResponse());
    assertTrue(response.getViewConfigResponse().isBuyable());
    assertTrue(response.getViewConfigResponse().isDisplay());
    assertEquals(Constants.DEFAULT, response.getViewConfigResponse().getChannelId());
  }

  @Test
  public void testItemPickupPointWithNonNullItemViewConfigAndBothSchedules() {
    ItemDiscoverableSchedule discoverableSchedule = new ItemDiscoverableSchedule();
    discoverableSchedule.setStartDateTime(new Date());
    discoverableSchedule.setEndDateTime(new Date());
    discoverableSchedule.setDiscoverable(true);

    ItemBuyableSchedule buyableSchedule = new ItemBuyableSchedule();
    buyableSchedule.setStartDateTime(new Date());
    buyableSchedule.setEndDateTime(new Date());
    buyableSchedule.setBuyable(true);

    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setItemDiscoverableSchedules(discoverableSchedule);
    itemViewConfig.setItemBuyableSchedules(buyableSchedule);

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setItemViewConfig(Set.of(itemViewConfig));

    List<ItemPickupPointBasicResponse> result = ResponseHelper.toItemPickupPointBasicResponse(Arrays.asList(itemPickupPoint));

    assertEquals(1, result.size());
    ItemPickupPointBasicResponse response = result.get(0);
    assertEquals(ITEM_SKU, response.getItemSku());
    assertEquals(PICKUP_POINT_CODE, response.getPickupPointCode());
    Assertions.assertNotNull(response.getViewConfigResponse());
    Assertions.assertNotNull(response.getViewConfigResponse().getDiscoverableScheduleResponse());
    Assertions.assertNotNull(response.getViewConfigResponse().getBuyableScheduleResponse());
    assertTrue(response.getViewConfigResponse().isBuyable());
    assertTrue(response.getViewConfigResponse().isDisplay());
    assertEquals(Constants.DEFAULT, response.getViewConfigResponse().getChannelId());
  }

  @Test
  public void setViewConfigResponseForProductReindexWhenItemViewConfigEmpty() {
    Set<ItemViewConfig> result = ResponseHelper.setViewConfigResponseByRequestString(new HashSet<>(), null, true);
    assertEquals(0, result.size());
  }

    @Test
    public void setViewConfigResponseForProductReindex() {
      Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
      ItemViewConfig itemViewConfig = new ItemViewConfig();
      itemViewConfig.setBuyable(true);
      itemViewConfig.setDiscoverable(true);
      itemViewConfig.setChannel(Constants.DEFAULT);
      itemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule(true, new Date(), new Date()));
      itemViewConfig.setItemDiscoverableSchedules(new ItemDiscoverableSchedule(true, new Date(), new Date()));
      itemViewConfigSet.add(itemViewConfig);
      Set<ItemViewConfig> result = ResponseHelper.setViewConfigResponseByRequestString(itemViewConfigSet, Constants.DEFAULT, true);
      assertEquals(1, result.size());
      ItemViewConfig resultConfig = result.iterator().next();
      assertTrue(resultConfig.isBuyable());
      assertTrue(resultConfig.isDiscoverable());
      Assertions.assertNotNull(resultConfig.getItemBuyableSchedules());
      Assertions.assertNotNull(resultConfig.getItemDiscoverableSchedules());
      assertEquals(Constants.DEFAULT, resultConfig.getChannel());
    }

    @Test
    public void setViewConfigResponseForProductReindexSwicthOff() {
      Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
      ItemViewConfig itemViewConfig = new ItemViewConfig();
      itemViewConfig.setBuyable(true);
      itemViewConfig.setDiscoverable(true);
      itemViewConfig.setChannel(Constants.DEFAULT);
      itemViewConfig.setItemBuyableSchedules(null);
      itemViewConfigSet.add(itemViewConfig);
      Set<ItemViewConfig> result = ResponseHelper.setViewConfigResponseByRequestString(itemViewConfigSet, "DEFAULT,CNC", false);
      assertEquals(1, result.size());
      ItemViewConfig resultConfig = result.iterator().next();
      assertTrue(resultConfig.isBuyable());
      assertTrue(resultConfig.isDiscoverable());
      assertEquals(Constants.DEFAULT, resultConfig.getChannel());
    }

    @Test
    public void setViewConfigResponseForProductReindexMultipleChannels() {
      Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
      ItemViewConfig itemViewConfigForDefault = new ItemViewConfig();
      itemViewConfigForDefault.setBuyable(true);
      itemViewConfigForDefault.setChannel(Constants.DEFAULT);
      itemViewConfigSet.add(itemViewConfigForDefault);
      ItemViewConfig itemViewConfigForCnc = new ItemViewConfig();
      itemViewConfigForCnc.setBuyable(true);
      itemViewConfigForCnc.setChannel(Constants.CNC);
      itemViewConfigSet.add(itemViewConfigForCnc);
      ItemViewConfig itemViewConfigForB2B = new ItemViewConfig();
      itemViewConfigForB2B.setBuyable(true);
      itemViewConfigForB2B.setChannel(Constants.B2B);
      itemViewConfigSet.add(itemViewConfigForB2B);
      Set<ItemViewConfig> result = ResponseHelper.setViewConfigResponseByRequestString(itemViewConfigSet, "B2B,CNC", true);
      assertEquals(2, result.size());
    }

  @Test
  public void removeDuplicateDescriptiveProductAttributeTest() {
    MasterDataAttributeDTO masterDataAttributeDTO1 = new MasterDataAttributeDTO();
    masterDataAttributeDTO1.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttributeDTO1.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO1 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO1.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO2.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO1 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO1.setMasterDataAttribute(masterDataAttributeDTO1);
    masterDataProductAttributeDTO1.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO1, masterDataProductAttributeValueDTO2));

    MasterDataAttributeDTO masterDataAttributeDTO2 = new MasterDataAttributeDTO();
    masterDataAttributeDTO2.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttributeDTO2.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO3 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO3.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO4 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO4.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO2 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO2.setMasterDataAttribute(masterDataAttributeDTO2);
    masterDataProductAttributeDTO2.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO3, masterDataProductAttributeValueDTO4));

    ProductAttributeDetailDTO productAttributeDetailDTO1 = new ProductAttributeDetailDTO();
    productAttributeDetailDTO1.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetailDTO1.setAttributeValue(ATTRIBUTE_VALUE);

    ProductAttributeDetailDTO productAttributeDetailDTO2 = new ProductAttributeDetailDTO();
    productAttributeDetailDTO2.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetailDTO2.setAttributeValue(ATTRIBUTE_VALUE);

    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductAttributes(
        Arrays.asList(masterDataProductAttributeDTO1, masterDataProductAttributeDTO2));

    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProductDTO);
    productResponse.setDescriptiveAttributes(Arrays.asList(productAttributeDetailDTO1, productAttributeDetailDTO2));

    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse(productResponse, new ArrayList<>());

    ResponseHelper.removeDuplicateProductAttributeValue(productAndItemsResponse, true);

    Assertions.assertEquals(1, masterDataProductDTO.getMasterDataProductAttributes().size());
    Assertions.assertEquals(1,
        masterDataProductDTO.getMasterDataProductAttributes().getFirst().getMasterDataProductAttributeValues().size());
    Assertions.assertEquals(1, productResponse.getDescriptiveAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_VALUE, productResponse.getDescriptiveAttributes().getFirst().getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_VALUE,
        masterDataProductDTO.getMasterDataProductAttributes().getFirst().getMasterDataProductAttributeValues()
            .getFirst().getDescriptiveAttributeValue());
  }

  @Test
  public void removeDuplicateDescriptiveProductAttributeWithNullDescriptiveValueTest() {
    MasterDataAttributeDTO masterDataAttributeDTO1 = new MasterDataAttributeDTO();
    masterDataAttributeDTO1.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttributeDTO1.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO1 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO1.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO1 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO1.setMasterDataAttribute(masterDataAttributeDTO1);
    masterDataProductAttributeDTO1.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO1, masterDataProductAttributeValueDTO2));
    MasterDataAttributeDTO masterDataAttributeDTO2 = new MasterDataAttributeDTO();
    masterDataAttributeDTO2.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttributeDTO2.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO3 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO3.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO4 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO4.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO2 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO2.setMasterDataAttribute(masterDataAttributeDTO2);
    masterDataProductAttributeDTO2.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO3, masterDataProductAttributeValueDTO4));
    ProductAttributeDetailDTO productAttributeDetailDTO1 = new ProductAttributeDetailDTO();
    productAttributeDetailDTO1.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetailDTO1.setAttributeValue(ATTRIBUTE_VALUE);
    ProductAttributeDetailDTO productAttributeDetailDTO2 = new ProductAttributeDetailDTO();
    productAttributeDetailDTO2.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetailDTO2.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductAttributes(
        Arrays.asList(masterDataProductAttributeDTO1, masterDataProductAttributeDTO2));
    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProductDTO);
    productResponse.setDescriptiveAttributes(Arrays.asList(productAttributeDetailDTO1, productAttributeDetailDTO2));
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse(productResponse, new ArrayList<>());
    ResponseHelper.removeDuplicateProductAttributeValue(productAndItemsResponse, true);
    Assertions.assertEquals(1, masterDataProductDTO.getMasterDataProductAttributes().size());
    Assertions.assertEquals(1, productResponse.getDescriptiveAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_VALUE, productResponse.getDescriptiveAttributes().getFirst().getAttributeValue());
    Assertions.assertNull(
        masterDataProductDTO.getMasterDataProductAttributes().getFirst().getMasterDataProductAttributeValues()
            .getFirst().getDescriptiveAttributeValue());
  }

  @Test
  public void removeDuplicateDefiningProductAttributeTest() {
    MasterDataAttributeDTO masterDataAttributeDTO1 = new MasterDataAttributeDTO();
    masterDataAttributeDTO1.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttributeDTO1.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO1 = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO1.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO1 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO1.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO1);
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO2 = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO2.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO2.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO2);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO1 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO1.setMasterDataAttribute(masterDataAttributeDTO1);
    masterDataProductAttributeDTO1.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO1, masterDataProductAttributeValueDTO2));

    MasterDataAttributeDTO masterDataAttributeDTO2 = new MasterDataAttributeDTO();
    masterDataAttributeDTO2.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttributeDTO2.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO3 = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO3.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO3 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO3.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO3);
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO4 = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO4.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO4 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO4.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO4);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO2 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO2.setMasterDataAttribute(masterDataAttributeDTO2);
    masterDataProductAttributeDTO2.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO3, masterDataProductAttributeValueDTO4));

    SortedDefiningAttributeDTO sortedDefiningAttributeDTO1 = new SortedDefiningAttributeDTO();
    sortedDefiningAttributeDTO1.setDefiningAttributes(Arrays.asList(ATTRIBUTE_VALUE, ATTRIBUTE_VALUE));

    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductAttributes(
        Arrays.asList(masterDataProductAttributeDTO1, masterDataProductAttributeDTO2));
    masterDataProductDTO.setSortedDefiningAttributes(List.of(sortedDefiningAttributeDTO1));

    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProductDTO);

    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse(productResponse, new ArrayList<>());

    ResponseHelper.removeDuplicateProductAttributeValue(productAndItemsResponse, true);

    Assertions.assertEquals(1, masterDataProductDTO.getMasterDataProductAttributes().size());
    Assertions.assertEquals(1,
        masterDataProductDTO.getMasterDataProductAttributes().getFirst().getMasterDataProductAttributeValues().size());
    Assertions.assertEquals(1,
        masterDataProductDTO.getSortedDefiningAttributes().getFirst().getDefiningAttributes().size());
    Assertions.assertEquals(ATTRIBUTE_VALUE,
        masterDataProductDTO.getSortedDefiningAttributes().getFirst().getDefiningAttributes().getFirst());
    Assertions.assertEquals(ATTRIBUTE_VALUE,
        masterDataProductDTO.getMasterDataProductAttributes().getFirst().getMasterDataProductAttributeValues()
            .getFirst().getAllowedAttributeValue().getValue());
  }


  @Test
  public void removeDuplicatePredefinedProductAttributeTest() {
    MasterDataAttributeDTO masterDataAttributeDTO1 = new MasterDataAttributeDTO();
    masterDataAttributeDTO1.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttributeDTO1.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO1 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO1.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO1 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO1.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO1);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO2 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO2.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO2.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO2);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO1 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO1.setMasterDataAttribute(masterDataAttributeDTO1);
    masterDataProductAttributeDTO1.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO1, masterDataProductAttributeValueDTO2));

    MasterDataAttributeDTO masterDataAttributeDTO2 = new MasterDataAttributeDTO();
    masterDataAttributeDTO2.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttributeDTO2.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO3 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO3.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO3 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO3.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO3);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO4 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO4.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO4 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO4.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO4);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO2 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO2.setMasterDataAttribute(masterDataAttributeDTO2);
    masterDataProductAttributeDTO2.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO3, masterDataProductAttributeValueDTO4));

    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductAttributes(
        Arrays.asList(masterDataProductAttributeDTO1, masterDataProductAttributeDTO2));

    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProductDTO);

    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse(productResponse, new ArrayList<>());

    ResponseHelper.removeDuplicateProductAttributeValue(productAndItemsResponse, true);

    Assertions.assertEquals(1, masterDataProductDTO.getMasterDataProductAttributes().size());
    Assertions.assertEquals(1,
        masterDataProductDTO.getMasterDataProductAttributes().getFirst().getMasterDataProductAttributeValues().size());
    Assertions.assertEquals(ATTRIBUTE_VALUE,
        masterDataProductDTO.getMasterDataProductAttributes().getFirst().getMasterDataProductAttributeValues()
            .getFirst().getPredefinedAllowedAttributeValue().getValue());
  }

  @Test
  public void removeDuplicatePredefinedProductAttributeNullTest() {
    MasterDataAttributeDTO masterDataAttributeDTO1 = new MasterDataAttributeDTO();
    masterDataAttributeDTO1.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttributeDTO1.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO1 = new MasterDataProductAttributeValueDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO1 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO1.setMasterDataAttribute(masterDataAttributeDTO1);
    masterDataProductAttributeDTO1.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO1, masterDataProductAttributeValueDTO2));

    MasterDataAttributeDTO masterDataAttributeDTO2 = new MasterDataAttributeDTO();
    masterDataAttributeDTO2.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttributeDTO2.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO3 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO3.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO3 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO3.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO3);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO4 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO4.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO4 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO4.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO4);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO2 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO2.setMasterDataAttribute(masterDataAttributeDTO2);
    masterDataProductAttributeDTO2.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO3, masterDataProductAttributeValueDTO4));

    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductAttributes(
        Arrays.asList(masterDataProductAttributeDTO1, masterDataProductAttributeDTO2));

    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProductDTO);

    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse(productResponse, new ArrayList<>());

    ResponseHelper.removeDuplicateProductAttributeValue(productAndItemsResponse, true);

    Assertions.assertEquals(1, masterDataProductDTO.getMasterDataProductAttributes().size());
    Assertions.assertEquals(1,
        masterDataProductDTO.getMasterDataProductAttributes().getFirst().getMasterDataProductAttributeValues().size());
    Assertions.assertNull(
        masterDataProductDTO.getMasterDataProductAttributes().getFirst().getMasterDataProductAttributeValues()
            .getFirst().getPredefinedAllowedAttributeValue());
  }

  @Test
  public void removeDuplicateNoAttributeTypeAttributeTest() {
    MasterDataAttributeDTO masterDataAttributeDTO1 = new MasterDataAttributeDTO();
    masterDataAttributeDTO1.setAttributeCode(ATTRIBUTE_CODE);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO1 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO1.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO1 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO1.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO1);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO2 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO2.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO2.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO2);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO1 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO1.setMasterDataAttribute(masterDataAttributeDTO1);
    masterDataProductAttributeDTO1.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO1, masterDataProductAttributeValueDTO2));

    MasterDataAttributeDTO masterDataAttributeDTO2 = new MasterDataAttributeDTO();
    masterDataAttributeDTO2.setAttributeCode(ATTRIBUTE_CODE);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO3 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO3.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO3 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO3.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO3);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO4 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO4.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO4 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO4.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO4);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO2 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO2.setMasterDataAttribute(masterDataAttributeDTO2);
    masterDataProductAttributeDTO2.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO3, masterDataProductAttributeValueDTO4));

    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductAttributes(
        Arrays.asList(masterDataProductAttributeDTO1, masterDataProductAttributeDTO2));

    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProductDTO);

    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse(productResponse, new ArrayList<>());

    ResponseHelper.removeDuplicateProductAttributeValue(productAndItemsResponse, true);

    Assertions.assertEquals(1, masterDataProductDTO.getMasterDataProductAttributes().size());
    Assertions.assertEquals(2,
        masterDataProductDTO.getMasterDataProductAttributes().getFirst().getMasterDataProductAttributeValues().size());
  }

  @Test
  public void removeDuplicateSwitchOffAttributeTest() {
    MasterDataAttributeDTO masterDataAttributeDTO1 = new MasterDataAttributeDTO();
    masterDataAttributeDTO1.setAttributeCode(ATTRIBUTE_CODE);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO1 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO1.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO1 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO1.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO1);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO2 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO2.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO2.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO2);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO1 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO1.setMasterDataAttribute(masterDataAttributeDTO1);
    masterDataProductAttributeDTO1.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO1, masterDataProductAttributeValueDTO2));

    MasterDataAttributeDTO masterDataAttributeDTO2 = new MasterDataAttributeDTO();
    masterDataAttributeDTO2.setAttributeCode(ATTRIBUTE_CODE);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO3 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO3.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO3 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO3.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO3);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO4 = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO4.setValue(ATTRIBUTE_VALUE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO4 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO4.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO4);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO2 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO2.setMasterDataAttribute(masterDataAttributeDTO2);
    masterDataProductAttributeDTO2.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValueDTO3, masterDataProductAttributeValueDTO4));

    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductAttributes(
        Arrays.asList(masterDataProductAttributeDTO1, masterDataProductAttributeDTO2));

    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProductDTO);

    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse(productResponse, new ArrayList<>());

    ResponseHelper.removeDuplicateProductAttributeValue(productAndItemsResponse, false);

    Assertions.assertEquals(2, masterDataProductDTO.getMasterDataProductAttributes().size());
    Assertions.assertEquals(2,
        masterDataProductDTO.getMasterDataProductAttributes().getFirst().getMasterDataProductAttributeValues().size());
  }

  @Test
  public void populateMissingFieldsTest() {
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    product.setDimensionsMissing(false);
    product.setMissingFields(new HashSet<>());
    ResponseHelper.populateMissingFields(itemPickupPointListingResponse, product);
    Assertions.assertTrue(CollectionUtils.isEmpty(itemPickupPointListingResponse.getMissingFields()));
    product.setDimensionsMissing(true);
    itemPickupPointListingResponse.setDimensionsMissing(true);
    itemPickupPointListingResponse.setMissingFields(new HashSet<>());
    ResponseHelper.populateMissingFields(itemPickupPointListingResponse, product);
    Assertions.assertTrue(itemPickupPointListingResponse.getMissingFields().contains(Constants.DIMENSIONS_MISSING));
  }

  @Test
  public void getDateTest() {
    Assertions.assertNull(ResponseHelper.getDate(null, Constants.DATE_FORMAT));
    Assertions.assertNull(ResponseHelper.getDate(new Date(), PRODUCT_CODE));
    Assertions.assertNotNull(ResponseHelper.getDate(new Date(), Constants.DATE_FORMAT));
  }

  @Test
  public void checkPreOrderDateStatusTest() {
    Assertions.assertTrue(ResponseHelper.checkPreOrderDateStatus(new Date(), PRODUCT_CODE));
  }

  @Test
  public void setVideoUrlTest() {
    List<Product> products = new ArrayList<>();
    Product product1 = new Product();
    product1.setProductCode("PROD1");
    Video video1 = new Video();
    video1.setFinalUrl("final_url_1");
    video1.setSourceUrl("source_url_1");
    video1.setCoverImagePath("cover_path_1");
    product1.setVideo(video1);
    products.add(product1);
    Product product2 = new Product();
    product2.setProductCode("PROD2");
    Video video2 = new Video();
    video2.setFinalUrl("final_url_2");
    video2.setSourceUrl("source_url_2");
    video2.setCoverImagePath("cover_path_2");
    product2.setVideo(video2);
    products.add(product2);
    Map<String, SimpleMasterDataProductVO> masterDataProducts = new HashMap<>();
    SimpleMasterDataProductVO vo1 = new SimpleMasterDataProductVO();
    masterDataProducts.put("PROD1", vo1);
    SimpleMasterDataProductVO vo2 = new SimpleMasterDataProductVO();
    masterDataProducts.put("PROD2", vo2);
    ResponseHelper.setVideoUrl(products, masterDataProducts);
    assertEquals("final_url_1", vo1.getVideo().getUrl());
    assertEquals("cover_path_1", vo1.getVideo().getCoverImagePath());
    assertEquals("final_url_2", vo2.getVideo().getUrl());
    assertEquals("cover_path_2", vo2.getVideo().getCoverImagePath());
  }

  @Test
  public void setVideoUrlWithEmptyFinalUrlTest() {
    List<Product> products = new ArrayList<>();
    Product product = new Product();
    product.setProductCode("PROD1");
    Video video = new Video();
    video.setFinalUrl("");
    video.setSourceUrl("source_url");
    video.setCoverImagePath("cover_path");
    product.setVideo(video);
    products.add(product);

    Map<String, SimpleMasterDataProductVO> masterDataProducts = new HashMap<>();
    SimpleMasterDataProductVO vo = new SimpleMasterDataProductVO();
    masterDataProducts.put("PROD1", vo);

    ResponseHelper.setVideoUrl(products, masterDataProducts);

    assertEquals("source_url", vo.getVideo().getUrl());
    assertEquals("cover_path", vo.getVideo().getCoverImagePath());
  }

  @Test
  public void setVideoUrlAndYoutubeUrlTest() {
    ProductAndItemsResponse response = new ProductAndItemsResponse();
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    response.setProduct(new ProductResponse());
    response.getProduct().setMasterDataProduct(masterDataProduct);

    ProductVo productVo = new ProductVo();
    Video video = new Video();
    video.setFinalUrl("final_url");
    video.setSourceUrl("source_url");
    video.setCoverImagePath("cover_path");
    productVo.setVideo(video);
    productVo.setUrl("youtube_url");

    ResponseHelper.setVideoUrlAndYoutubeUrl(response, productVo);

    assertEquals("final_url", masterDataProduct.getVideo().getUrl());
    assertEquals(StringUtils.EMPTY, masterDataProduct.getUrl());
  }

  @Test
  public void setVideoUrlAndYoutubeUrlWithEmptyFinalUrlTest() {
    ProductAndItemsResponse response = new ProductAndItemsResponse();
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    response.setProduct(new ProductResponse());
    response.getProduct().setMasterDataProduct(masterDataProduct);

    ProductVo productVo = new ProductVo();
    Video video = new Video();
    video.setFinalUrl("");
    video.setSourceUrl("source_url");
    video.setCoverImagePath("cover_path");
    productVo.setVideo(video);
    productVo.setUrl("youtube_url");

    ResponseHelper.setVideoUrlAndYoutubeUrl(response, productVo);

    assertEquals("source_url", masterDataProduct.getVideo().getUrl());
    assertEquals(StringUtils.EMPTY, masterDataProduct.getUrl());
  }

  @Test
  public void setVideoUrlAndYoutubeUrlWithNullVideoTest() {
    ProductAndItemsResponse response = new ProductAndItemsResponse();
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    response.setProduct(new ProductResponse());
    response.getProduct().setMasterDataProduct(masterDataProduct);

    ProductVo productVo = new ProductVo();
    productVo.setUrl("youtube_url");

    ResponseHelper.setVideoUrlAndYoutubeUrl(response, productVo);
    assertNull(masterDataProduct.getVideo());
    assertEquals("youtube_url", masterDataProduct.getUrl());
  }

  @Test
  public void setVideoUrlWithEmptyProductsListTest() {
    List<Product> products = new ArrayList<>();
    Map<String, SimpleMasterDataProductVO> masterDataProducts = new HashMap<>();
    SimpleMasterDataProductVO vo = new SimpleMasterDataProductVO();
    masterDataProducts.put("PROD1", vo);
    ResponseHelper.setVideoUrl(products, masterDataProducts);
    assertNull(vo.getVideo());
  }

  @Test
  public void setVideoUrlWithNullProductTest() {
    List<Product> products = new ArrayList<>();
    products.add(null);
    Map<String, SimpleMasterDataProductVO> masterDataProducts = new HashMap<>();
    SimpleMasterDataProductVO vo = new SimpleMasterDataProductVO();
    masterDataProducts.put("PROD1", vo);
    ResponseHelper.setVideoUrl(products, masterDataProducts);
    assertNull(vo.getVideo());
  }

  @Test
  public void setVideoUrlWithNullVideoTest() {
    List<Product> products = new ArrayList<>();
    Product product = new Product();
    product.setProductCode("PROD1");
    product.setVideo(null);
    products.add(product);
    Map<String, SimpleMasterDataProductVO> masterDataProducts = new HashMap<>();
    SimpleMasterDataProductVO vo = new SimpleMasterDataProductVO();
    masterDataProducts.put("PROD1", vo);
    ResponseHelper.setVideoUrl(products, masterDataProducts);
    assertNull(vo.getVideo());
  }

  @Test
  public void setVideoUrlAndYoutubeUrlWithNullMasterDataProductTest() {
    ProductAndItemsResponse response = new ProductAndItemsResponse();
    response.setProduct(new ProductResponse());
    response.getProduct().setMasterDataProduct(null);

    ProductVo productVo = new ProductVo();
    Video video = new Video();
    video.setFinalUrl("final_url");
    video.setSourceUrl("source_url");
    video.setCoverImagePath("cover_path");
    productVo.setVideo(video);
    productVo.setUrl("youtube_url");
    ResponseHelper.setVideoUrlAndYoutubeUrl(response, productVo);
  }

  @Test
  public void testSetVideoUrlAndCoverImagePath() {
    VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
    videoAddEditRequest.setVideoUrl("https://example.com/video.mp4");
    videoAddEditRequest.setCoverImagePath("https://example.com/cover.jpg");
    ProductVo productVo = new ProductVo();
    productVo.setVideo(new Video());
    ResponseHelper.setVideoUrlAndCoverImagePath(videoAddEditRequest, productVo);
    assertEquals("https://example.com/video.mp4", productVo.getVideo().getSourceUrl());
    assertEquals("https://example.com/cover.jpg", productVo.getVideo().getCoverImagePath());
  }

  @Test
  public void setVideoUrlAndCoverImagePathNull(){
    ProductVo productVo = new ProductVo();
    ResponseHelper.setVideoUrlAndCoverImagePath(null, productVo);
    assertNull(productVo.getVideo());
  }

  @Test
  public void toItemPickupPointCodeResponsesWithNullProductCollectionsTest() {
    ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder()
        .itemSku(ITEM_SKU_1)
        .pickupPointCode(PICKUP_POINT_CODE)
        .productSku(PRODUCT_SKU_1)
        .build();
    
    List<ItemPickupPointCodeResponse> responses = 
        ResponseHelper.toItemPickupPointCodeResponses(Arrays.asList(itemPickupPoint), null);
    
    assertEquals(1, responses.size());
    assertEquals(ITEM_SKU_1, responses.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, responses.get(0).getPickupPointCode());
    assertNull(responses.get(0).getPreOrderDate());
  }

  @Test
  public void toItemPickupPointCodeResponsesWithEmptyItemPickupPointsTest() {
    ProductCollectionsVo productCollectionsVo = new ProductCollectionsVo(
        Arrays.asList(product), new ArrayList<>(), new ArrayList<>());
    
    List<ItemPickupPointCodeResponse> responses = 
        ResponseHelper.toItemPickupPointCodeResponses(Collections.emptyList(), productCollectionsVo);
    
    assertTrue(responses.isEmpty());
  }

  @Test
  public void toItemPickupPointCodeResponsesWithEmptyProductsTest() {
    ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder()
        .itemSku(ITEM_SKU_1)
        .pickupPointCode(PICKUP_POINT_CODE)
        .productSku(PRODUCT_SKU_1)
        .build();
    
    ProductCollectionsVo productCollectionsVo = new ProductCollectionsVo(
        Collections.emptyList(), new ArrayList<>(), new ArrayList<>());
    
    List<ItemPickupPointCodeResponse> responses = 
        ResponseHelper.toItemPickupPointCodeResponses(Arrays.asList(itemPickupPoint), productCollectionsVo);
    
    assertEquals(1, responses.size());
    assertEquals(ITEM_SKU_1, responses.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, responses.get(0).getPickupPointCode());
    assertNull(responses.get(0).getPreOrderDate());
  }

  @Test
  public void toItemPickupPointCodeResponsesWithMatchingPreOrderDateTest() {
    Date preOrderDate = new Date();
    PreOrder preOrder = new PreOrder();
    preOrder.setPreOrderDate(preOrderDate);
    
    Product productWithPreOrder = new Product();
    productWithPreOrder.setProductSku(PRODUCT_SKU_1);
    productWithPreOrder.setPreOrder(preOrder);
    
    ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder()
        .itemSku(ITEM_SKU_1)
        .pickupPointCode(PICKUP_POINT_CODE)
        .productSku(PRODUCT_SKU_1)
        .build();
    
    ProductCollectionsVo productCollectionsVo = new ProductCollectionsVo(
        Arrays.asList(productWithPreOrder), new ArrayList<>(), new ArrayList<>());
    
    List<ItemPickupPointCodeResponse> responses = 
        ResponseHelper.toItemPickupPointCodeResponses(Arrays.asList(itemPickupPoint), productCollectionsVo);
    
    assertEquals(1, responses.size());
    assertEquals(ITEM_SKU_1, responses.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, responses.get(0).getPickupPointCode());
    assertEquals(preOrderDate, responses.get(0).getPreOrderDate());
  }

  @Test
  public void toItemPickupPointCodeResponsesWithNonMatchingProductSkuTest() {
    Date preOrderDate = new Date();
    PreOrder preOrder = new PreOrder();
    preOrder.setPreOrderDate(preOrderDate);
    
    Product productWithPreOrder = new Product();
    productWithPreOrder.setProductSku(PRODUCT_SKU_2);
    productWithPreOrder.setPreOrder(preOrder);
    
    ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder()
        .itemSku(ITEM_SKU_1)
        .pickupPointCode(PICKUP_POINT_CODE)
        .productSku(PRODUCT_SKU_1)
        .build();
    
    ProductCollectionsVo productCollectionsVo = new ProductCollectionsVo(
        Arrays.asList(productWithPreOrder), new ArrayList<>(), new ArrayList<>());
    
    List<ItemPickupPointCodeResponse> responses = 
        ResponseHelper.toItemPickupPointCodeResponses(Arrays.asList(itemPickupPoint), productCollectionsVo);
    
    assertEquals(1, responses.size());
    assertEquals(ITEM_SKU_1, responses.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, responses.get(0).getPickupPointCode());
    assertNull(responses.get(0).getPreOrderDate()); // Should be null since SKU doesn't match
  }

  @Test
  public void toItemPickupPointCodeResponsesWithMultipleItemsTest() {
    Date preOrderDate1 = new Date();
    Date preOrderDate2 = new Date(System.currentTimeMillis() + 86400000);
    
    PreOrder preOrder1 = new PreOrder();
    preOrder1.setPreOrderDate(preOrderDate1);
    
    PreOrder preOrder2 = new PreOrder();
    preOrder2.setPreOrderDate(preOrderDate2);
    
    Product product1 = new Product();
    product1.setProductSku(PRODUCT_SKU_1);
    product1.setPreOrder(preOrder1);
    
    Product product2 = new Product();
    product2.setProductSku(PRODUCT_SKU_2);
    product2.setPreOrder(preOrder2);
    
    ItemPickupPoint itemPickupPoint1 = ItemPickupPoint.builder()
        .itemSku(ITEM_SKU_1)
        .pickupPointCode(PICKUP_POINT_CODE)
        .productSku(PRODUCT_SKU_1)
        .build();
    
    ItemPickupPoint itemPickupPoint2 = ItemPickupPoint.builder()
        .itemSku(ITEM_SKU_2)
        .pickupPointCode(PICKUP_POINT_CODE2)
        .productSku(PRODUCT_SKU_2)
        .build();
    
    ItemPickupPoint itemPickupPoint3 = ItemPickupPoint.builder()
        .itemSku(ITEM_SKU_3)
        .pickupPointCode(PICKUP_POINT_CODE)
        .productSku("nonExistentSku")
        .build();
    
    ProductCollectionsVo productCollectionsVo = new ProductCollectionsVo(
        Arrays.asList(product1, product2), new ArrayList<>(), new ArrayList<>());
    
    List<ItemPickupPointCodeResponse> responses = ResponseHelper.toItemPickupPointCodeResponses(
        Arrays.asList(itemPickupPoint1, itemPickupPoint2, itemPickupPoint3), productCollectionsVo);
    
    assertEquals(3, responses.size());

    assertEquals(ITEM_SKU_1, responses.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, responses.get(0).getPickupPointCode());
    assertEquals(preOrderDate1, responses.get(0).getPreOrderDate());

    assertEquals(ITEM_SKU_2, responses.get(1).getItemSku());
    assertEquals(PICKUP_POINT_CODE2, responses.get(1).getPickupPointCode());
    assertEquals(preOrderDate2, responses.get(1).getPreOrderDate());

    assertEquals(ITEM_SKU_3, responses.get(2).getItemSku());
    assertEquals(PICKUP_POINT_CODE, responses.get(2).getPickupPointCode());
    assertNull(responses.get(2).getPreOrderDate());
  }

  @Test
  public void toItemPickupPointCodeResponsesWithNullPreOrderTest() {
    Product productWithNullPreOrder = new Product();
    productWithNullPreOrder.setProductSku(PRODUCT_SKU_1);
    productWithNullPreOrder.setPreOrder(new PreOrder());
    
    ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder()
        .itemSku(ITEM_SKU_1)
        .pickupPointCode(PICKUP_POINT_CODE)
        .productSku(PRODUCT_SKU_1)
        .build();
    
    ProductCollectionsVo productCollectionsVo = new ProductCollectionsVo(
        Arrays.asList(productWithNullPreOrder), new ArrayList<>(), new ArrayList<>());
    
    List<ItemPickupPointCodeResponse> responses = 
        ResponseHelper.toItemPickupPointCodeResponses(Arrays.asList(itemPickupPoint), productCollectionsVo);
    
    assertEquals(1, responses.size());
    assertEquals(ITEM_SKU_1, responses.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, responses.get(0).getPickupPointCode());
    assertNull(responses.get(0).getPreOrderDate());
  }

  public void toItemPickupPointCodeResponsesWithNewPreOrderTest() {
    Product productWithNullPreOrder = new Product();
    productWithNullPreOrder.setProductSku(PRODUCT_SKU_1);
    productWithNullPreOrder.setPreOrder(new PreOrder());

    ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder()
        .itemSku(ITEM_SKU_1)
        .pickupPointCode(PICKUP_POINT_CODE)
        .productSku(PRODUCT_SKU_1)
        .build();

    ProductCollectionsVo productCollectionsVo = new ProductCollectionsVo(
        Arrays.asList(productWithNullPreOrder), new ArrayList<>(), new ArrayList<>());

    List<ItemPickupPointCodeResponse> responses =
        ResponseHelper.toItemPickupPointCodeResponses(Arrays.asList(itemPickupPoint), productCollectionsVo);

    assertEquals(1, responses.size());
    assertEquals(ITEM_SKU_1, responses.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, responses.get(0).getPickupPointCode());
    assertNull(responses.get(0).getPreOrderDate());
  }

  @Test
  public void toItemPickupPointCodeResponsesWithDuplicateProductSkuTest() {
    Date preOrderDate1 = new Date();
    Date preOrderDate2 = new Date(System.currentTimeMillis() + 86400000);
    
    PreOrder preOrder1 = new PreOrder();
    preOrder1.setPreOrderDate(preOrderDate1);
    
    PreOrder preOrder2 = new PreOrder();
    preOrder2.setPreOrderDate(preOrderDate2);

    Product product1 = new Product();
    product1.setProductSku(PRODUCT_SKU_1);
    product1.setPreOrder(preOrder1);
    
    Product product2 = new Product();
    product2.setProductSku(PRODUCT_SKU_1);
    product2.setPreOrder(preOrder2);
    
    ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder()
        .itemSku(ITEM_SKU_1)
        .pickupPointCode(PICKUP_POINT_CODE)
        .productSku(PRODUCT_SKU_1)
        .build();
    
    ProductCollectionsVo productCollectionsVo = new ProductCollectionsVo(
        Arrays.asList(product1, product2), new ArrayList<>(), new ArrayList<>());
    
    List<ItemPickupPointCodeResponse> responses = 
        ResponseHelper.toItemPickupPointCodeResponses(Arrays.asList(itemPickupPoint), productCollectionsVo);
    
    assertEquals(1, responses.size());
    assertEquals(ITEM_SKU_1, responses.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, responses.get(0).getPickupPointCode());
    assertEquals(preOrderDate1, responses.get(0).getPreOrderDate());
  }
}
