package com.gdn.x.product.service.impl;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.sql.Ref;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.businesspartner.dto.ProductCounterResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductAttributeValue;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.OfflineItemDetailVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.outbound.api.XbpOutbound;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.FormulaUtil;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.google.common.collect.ImmutableSet;

public class ProductHelperServiceImplTest {

  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_VALUE = "predefinedAllowedAttributeValue";

  private static final String DESCRIPTIVE_ATTRIBUTE_VALUE = "descriptiveAttributeValue";

  private static final String ATTRIBUTE_NAME = "attributeName";

  private static final String CATEGORY_ID = "category-id";

  private static final String USERNAME = "username";

  private static final long PAGE = 0;

  private static final long SIZE = 1;

  private static final String REQUEST_ID = "request-id";

  private static final String STORE_ID = "store-id";

  private static final String PRODUCT_SKU = "product-sku";

  private static final String ITEM_SKU = "item-sku";

  private static final String ITEM_SKU_1 = "item-sku-1";

  private static final String ATTRIBUTE_CODE = "attribute-code";

  private static final String ATTRIBUTE_VALUE = "attribute-value";

  private static final String ITEM_SKU_BLANK = "";

  private static final String CHANNEL_WEB = ChannelName.DESKTOP_WEB.toString();

  private static final String CHANNEL_DEFAULT = ChannelName.DEFAULT.toString();

  private static final String CHANNEL_MOBILE = ChannelName.MOBILE_WEB.toString();

  private static final String ITEM_CODE = "item-code";

  private static final String NO_ITEM_VIEW_CONFIG_AVAILABLE =
    "No item view config available for channel ";

  private static final String PRODUCT_CODE = "productCode";

  private static final ProductType PRODUCT_TYPE_REGULAR = ProductType.REGULAR;

  private static final String SETTLEMENT_TYPE = "settlementType";

  private static final String MERCHANT_CODE = "merchantCode";

  private static final String OLD_PRODUCT_NAME = "old";

  private static final String NEW_PRODUCT_NAME = "newProductName";

  private static final long AN_HOUR_IN_MILLISECOND = 3600000;

  private static final String GENERATED_ITEM_NAME = ProductHelperServiceImplTest.OLD_PRODUCT_NAME + "-generated-item-name";

  private static final Integer ORIGINAL_STOCK = 10;

  private static final Integer STOCK = 9;

  private static final long PRODUCT_COUNTER = 1;

  private static final String PICKUP_POINT_CODE = "PP-CODE";

  private static final Double OFFLINE_LIST_PRICE = 1000.0;

  private static final Double OFFLINE_OFFER_PRICE = 2000.0;

  @InjectMocks
  private ProductHelperServiceImpl productHelperServiceImpl;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private CachedService cachedService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseClient;

  @Mock
  private ChannelService channelService;

  @Mock
  private MasterDataService masterDataService;

  @Mock
  private FormulaUtil formulaUtil;

  @Mock
  private XbpOutbound xbpOutbound;

  @Mock
  private OfflineItemService offlineItemService;

  @Mock
  private GdnMapper gdnMapper;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  private Product product;

  private GdnRestListRequest listRequest;

  private List<MasterDataItemAttributeValue> itemAttributeValues;

  private MasterDataItemAttributeValue itemAttributeValue;

  private Product productUpdated;

  private ProductAttribute productAttribute;

  private ProductAttributeDetail productAttributeDetail;

  private ArrayList<ProductAttributeDetail> productAttributeDetails;

  private Date startDateTrue;

  private Date endDateTrue;

  private Date endDateFalse;

  private Date startDateFalse;

  private Item item;

  private Item item1;

  private ArrayList<ProductAttribute> listOfProductAttributes;

  private Set<ItemViewConfig> itemViewConfigs;

  private ItemViewConfig itemViewConfig;

  private ItemBuyableSchedule itemBuyableSchedule;

  private ItemDiscoverableSchedule itemDiscoverableSchedule;

  private Set<ItemViewConfig> currItemViewConfigs;

  private ItemViewConfig currItemViewConfig;

  private Product productRequestVO;

  private List<Item> listOfItems;

  private Date currDate;

  private MasterDataItem masterDataItem;

  private ProductDetailResponse productDetailResponse;

  private List<ProductCategoryResponse> productCategoryResponses;

  private MasterCatalog masterCatalog;

  private List<SalesCatalog> salesCatalogs;

  private Map<String, Item> mapOfItemsAndItemSkus;

  private CategoryDetailResponse categoryDetailResponse;

  private List<CategoryReferenceResponse> salesCategoryReferences;

  private OfflineItem offlineItem = new OfflineItem();

  private ItemPickupPoint itemPickupPoint = new ItemPickupPoint();

  private ProfileResponse profileResponse = new ProfileResponse();

  @Test
  public void addItemAttributeToProductAttributeTest() throws Exception {
    this.productHelperServiceImpl
      .addItemAttributeToProductAttribute(this.product, ProductHelperServiceImplTest.ITEM_SKU, this.itemAttributeValues);
  }

  @Test
  public void addItemAttributeToProductAttributeTestWithItemSkuBlank() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->this.productHelperServiceImpl
      .addItemAttributeToProductAttribute(this.product, ProductHelperServiceImplTest.ITEM_SKU_BLANK,
        this.itemAttributeValues));
  }

  @Test
  public void addItemAttributeToProductAttributeTestWithNullItemAttributeValues() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl
      .addItemAttributeToProductAttribute(this.product, ProductHelperServiceImplTest.ITEM_SKU, null));
  }

  @Test
  public void addItemAttributeToProductAttributeTestWithProductNull() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl
      .addItemAttributeToProductAttribute(null, ProductHelperServiceImplTest.ITEM_SKU, this.itemAttributeValues));
  }

  @Test
  public void addItemAttributeToProductAttributeVariantCreationTest() throws Exception {
    product.setDefiningAttributes(new ArrayList<>());
    Product product = this.productHelperServiceImpl
      .addItemAttributeToProductAttribute(this.product, ProductHelperServiceImplTest.ITEM_SKU, this.itemAttributeValues);
    assertEquals(1, product.getDefiningAttributes().get(0).getProductAttributeDetails().size());
    assertEquals("attribute-code1",
      product.getDefiningAttributes().get(0).getProductAttributeDetails().get(0).getAttributeCode());
  }

  @Test
  public void constructProductWithItemAndMasterDataWhenEmptyProductAvailable() {
    Map<String, List<ProductAndItemsVO>> result = productHelperServiceImpl.constructProductWithItemAndMasterData(null, null, null);
    assertTrue(result.isEmpty());
  }

  @Test
  public void constructProductWithItemAndMasterDataWhenEmptyItem() {
    Map<String, List<ProductAndItemsVO>> result = productHelperServiceImpl.constructProductWithItemAndMasterData(Stream.of(product).collect(toList()), null, null);
    assertTrue(result.isEmpty());
  }

  @Test
  public void constructProductWithItemAndMasterDataTest() {
    product.setSynchronized(true);
    List<Item> items = new ArrayList<>();
    items.add(item);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    masterDataProductAndItemsVO.setMasterDataProduct(new MasterDataProduct());
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setItemDeliveryWeight(1D);
    masterDataProductAndItemsVO.setMasterDataItems(
      Stream.of(masterDataItem).collect(toMap(e -> item.getItemCode(), e -> e)));

    Map<String, List<ProductAndItemsVO>> result = productHelperServiceImpl.constructProductWithItemAndMasterData(Stream.of(product).collect(toList()),
      Stream.of(items).collect(toMap(e -> product.getProductSku(), e -> items)),
      Stream.of(masterDataProductAndItemsVO).collect(toMap(e -> product.getProductCode(), Function.identity())));
    ProductAndItemsVO productAndItems = result.get(PRODUCT_CODE).get(0);
    assertEquals(1, productAndItems.getItems().size());
    assertEquals(0d, productAndItems.getProduct().getMasterDataProduct().getShippingWeight(), 0d);
    assertEquals(1d, productAndItems.getItems().get(0).getMasterDataItem().getItemDeliveryWeight(),
      0d);
  }

  @Test
  public void constructProductWithItemAndMasterDataWhenMasterDataIsNull() {
    product.setSynchronized(true);
    List<Item> items = new ArrayList<>();
    items.add(item);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    masterDataProductAndItemsVO.setMasterDataProduct(new MasterDataProduct());
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setItemDeliveryWeight(1D);
    masterDataProductAndItemsVO.setMasterDataItems(
      Stream.of(masterDataItem).collect(toMap(e -> item.getItemCode(), e -> e)));

    Map<String, List<ProductAndItemsVO>> result = productHelperServiceImpl
      .constructProductWithItemAndMasterData(Stream.of(product).collect(toList()),
        Stream.of(items).collect(toMap(e -> product.getProductSku(), e -> items)), null);
    assertNotNull(result);
  }

  @Test
  public void constructProductWithItemAndMasterDataWhenItemCandidatIsNull() {
    product.setSynchronized(true);
    List<Item> items = new ArrayList<>();
    items.add(item);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    masterDataProductAndItemsVO.setMasterDataProduct(new MasterDataProduct());
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setItemDeliveryWeight(1D);
    masterDataProductAndItemsVO.setMasterDataItems(
      Stream.of(masterDataItem).collect(toMap(e -> item.getItemCode(), e -> e)));

    Map<String, List<ProductAndItemsVO>> result = productHelperServiceImpl.constructProductWithItemAndMasterData(Stream.of(product).collect(toList()),
      Stream.of(items).collect(toMap(e -> product.getProductSku(), e -> new ArrayList<Item>())),
      Stream.of(masterDataProductAndItemsVO).collect(toMap(e -> product.getProductCode(), Function.identity())));
    assertNotNull(result);
  }

  @Test
  public void constructProductWithItemAndMasterDataWhenCandidatNotSync() {
    product.setSynchronized(false);
    List<Item> items = new ArrayList<>();
    items.add(item);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    masterDataProductAndItemsVO.setMasterDataProduct(new MasterDataProduct());
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setItemDeliveryWeight(1D);
    masterDataProductAndItemsVO.setMasterDataItems(
      Stream.of(masterDataItem).collect(toMap(e -> item.getItemCode(), e -> e)));

    Map<String, List<ProductAndItemsVO>> result = productHelperServiceImpl.constructProductWithItemAndMasterData(Stream.of(product).collect(toList()),
      Stream.of(items).collect(toMap(e -> product.getProductSku(), e -> items)),
      Stream.of(masterDataProductAndItemsVO).collect(toMap(e -> product.getProductCode(), Function.identity())));
    assertNotNull(result);
  }

  private MasterDataProductAttribute createMasterDataProductAttribute(MasterDataAttributeType attributeType) {
    MasterDataProductAttribute masterDataProductAttribute = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeType(attributeType);
    masterDataAttribute.setAttributeCode(ProductHelperServiceImplTest.ATTRIBUTE_CODE);
    masterDataProductAttribute.setMasterDataAttribute(masterDataAttribute);
    ArrayList<MasterDataProductAttributeValue> masterDataProductAttributeValues = new ArrayList<MasterDataProductAttributeValue>();
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setDescriptiveAttributeValue(ProductHelperServiceImplTest.DESCRIPTIVE_ATTRIBUTE_VALUE);
    masterDataProductAttributeValue.setPredefinedAllowedAttributeValue(
      new PredefinedAllowedAttributeValue(null, ProductHelperServiceImplTest.PREDEFINED_ALLOWED_ATTRIBUTE_VALUE, 0));
    masterDataProductAttributeValues.add(masterDataProductAttributeValue);
    masterDataProductAttribute.setMasterDataProductAttributeValues(masterDataProductAttributeValues);
    return masterDataProductAttribute;
  }

  @Test
  public void deleteItemAttributeFromProductAttributeTest() {
    Product product = this.productHelperServiceImpl.deleteItemAttributeFromProductAttribute(this.product, ProductHelperServiceImplTest.ITEM_SKU);
    assertEquals(0, product.getDefiningAttributes().size());
  }

  @Test
  public void deleteItemAttributeFromProductAttributeTestWithEmptyListProductAttributes() {
    this.product.getDefiningAttributes().clear();
    Product product = this.productHelperServiceImpl.deleteItemAttributeFromProductAttribute(this.product, ProductHelperServiceImplTest.ITEM_SKU);
    assertEquals(0, product.getDefiningAttributes().size());
  }

  @Test
  public void deleteItemAttributeFromProductAttributeTestWithItemSkuNotFound() {
    Product product = this.productHelperServiceImpl
      .deleteItemAttributeFromProductAttribute(this.product, ProductHelperServiceImplTest.ITEM_SKU_BLANK);
    assertEquals(1, product.getDefiningAttributes().size());
  }

  @Test
  public void generateSpecificationDetailTest() {
    Product gen = this.prepareProductMock();
    this.productHelperServiceImpl.generateSpecificationDetail(gen);
  }

  @Test
  public void generateSpecificationDetailWhenDefiningAttributeIsNull() {
    Product gen = this.prepareProductMock();
    gen.setDefiningAttributes(null);
    gen.getMasterDataProduct().setMasterDataProductAttributes(null);
    this.productHelperServiceImpl.generateSpecificationDetail(gen);
  }

  @Test
  public void generateSpecificationDetailWhenAttributeDetailIsNull() {
    Product gen = this.prepareProductMock();
    gen.getDefiningAttributes().get(0).setProductAttributeDetails(null);
    this.productHelperServiceImpl.generateSpecificationDetail(gen);
  }

  private Product prepareProductMock() {
    Product gen = new Product();
    ArrayList<ProductAttribute> definingAttributes = new ArrayList<ProductAttribute>();
    ArrayList<ProductAttributeDetail> attributeDetails = new ArrayList<ProductAttributeDetail>();
    attributeDetails.add(new ProductAttributeDetail(ProductHelperServiceImplTest.ATTRIBUTE_CODE,
      ProductHelperServiceImplTest.ATTRIBUTE_NAME, ProductHelperServiceImplTest.ATTRIBUTE_VALUE));
    definingAttributes
      .add(new ProductAttribute(ProductHelperServiceImplTest.ITEM_SKU, attributeDetails));
    gen.setDefiningAttributes(definingAttributes);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    ArrayList<MasterDataProductAttribute> masterDataProductAttributes = new ArrayList<MasterDataProductAttribute>();
    masterDataProductAttributes.add(this.createMasterDataProductAttribute(MasterDataAttributeType.DEFINING_ATTRIBUTE));
    masterDataProductAttributes.add(this.createMasterDataProductAttribute(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE));
    masterDataProductAttributes.add(this.createMasterDataProductAttribute(MasterDataAttributeType.PREDEFINED_ATTRIBUTE));
    masterDataProduct.setMasterDataProductAttributes(masterDataProductAttributes);
    gen.setMasterDataProduct(masterDataProduct);
    return gen;
  }


  @Test
  public void getCurrentBuyableStatusForItemTestWithBuyableScheduleActive() throws Exception {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel("DEFAULT");
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setBuyable(true);
    Date endDateTime = new Date(currDate.getTime() - 100000);
    itemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule(false, new Date(), endDateTime));
    boolean result = this.productHelperServiceImpl
      .getCurrentBuyableStatusForItem(Set.of(itemViewConfig), ProductHelperServiceImplTest.CHANNEL_DEFAULT, item1.isArchived());
    assertTrue(result);
  }

  @Test
  public void getCurrentBuyableStatusForItemTestWithBuyableScheduleActiveForArchivedproduct() throws Exception {
    item1.setArchived(true);
    boolean result = this.productHelperServiceImpl
      .getCurrentBuyableStatusForItem(this.item1.getItemViewConfigs(), ProductHelperServiceImplTest.CHANNEL_DEFAULT, item1.isArchived());
    assertFalse(result);
  }

  @Test
  public void getCurrentBuyableStatusForItemTestWithBuyableScheduleActiveForArchivedproduct1() throws Exception {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setBuyable(false);
    itemViewConfig1.setChannel("DEFAULT");
    ItemBuyableSchedule itemBuyableSchedule1 = new ItemBuyableSchedule();
    itemBuyableSchedule1.setBuyable(true);
    itemViewConfig1.setItemBuyableSchedules(itemBuyableSchedule1);
    item1.setItemViewConfigs(Set.of(itemViewConfig1));
    boolean result = this.productHelperServiceImpl.getCurrentBuyableStatusForItem(this.item1.getItemViewConfigs(),
        ProductHelperServiceImplTest.CHANNEL_DEFAULT, item1.isArchived());
    assertFalse(result);
  }

  @Test
  public void getCurrentBuyableStatusForItemTestWithBuyableScheduleActiveForArchivedproduct2() throws Exception {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setBuyable(false);
    itemViewConfig1.setChannel("DEFAULT");
    ItemBuyableSchedule itemBuyableSchedule1 = new ItemBuyableSchedule();
    itemBuyableSchedule1.setBuyable(true);
    itemBuyableSchedule1.setStartDateTime(new Date());
    itemViewConfig1.setItemBuyableSchedules(itemBuyableSchedule1);
    item1.setItemViewConfigs(Set.of(itemViewConfig1));
    boolean result = this.productHelperServiceImpl.getCurrentBuyableStatusForItem(this.item1.getItemViewConfigs(),
        ProductHelperServiceImplTest.CHANNEL_DEFAULT, item1.isArchived());
    assertFalse(result);
  }

  @Test
  public void getCurrentBuyableStatusForItemTestWithBuyableScheduleNotActive() throws Exception {
    this.itemBuyableSchedule.setStartDateTime(this.startDateFalse);
    this.itemBuyableSchedule.setEndDateTime(this.endDateFalse);
    boolean result = this.productHelperServiceImpl
      .getCurrentBuyableStatusForItem(this.item1.getItemViewConfigs(), ProductHelperServiceImplTest.CHANNEL_DEFAULT, item1.isArchived());
    assertTrue(result);
  }

  @Test
  public void getCurrentBuyableStatusForItemTestWithEmptyItemViewConfig() {
    this.itemViewConfigs.clear();
    try {
      this.productHelperServiceImpl.getCurrentBuyableStatusForItem(this.item1.getItemViewConfigs(),
        ProductHelperServiceImplTest.CHANNEL_WEB, item1.isArchived());
    } catch (Exception e) {
      assertTrue(e instanceof Exception);
      assertEquals(ProductHelperServiceImplTest.NO_ITEM_VIEW_CONFIG_AVAILABLE + ProductHelperServiceImplTest.CHANNEL_DEFAULT, e.getMessage());
    }
  }

  @Test
  public void getCurrentBuyableStatusForItemTestWithItemViewConfigNotFound() throws Exception {
    this.productHelperServiceImpl.getCurrentBuyableStatusForItem(this.item1.getItemViewConfigs(),
      ProductHelperServiceImplTest.CHANNEL_WEB, item1.isArchived());
  }

  @Test
  public void getCurrentBuyableStatusForItemTestWithItemViewConfigNotFound3() throws Exception {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(ProductHelperServiceImplTest.CHANNEL_DEFAULT);
    Date beforeDateTime = new Date(currDate.getTime() - 1000000000);
    Date endDateTime = new Date(currDate.getTime() - 100000000);
    itemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule(false, beforeDateTime, endDateTime));
    this.productHelperServiceImpl.getCurrentBuyableStatusForItem(Set.of(itemViewConfig),
        ProductHelperServiceImplTest.CHANNEL_DEFAULT, item1.isArchived());
  }

  @Test
  public void getCurrentBuyableStatusForItemTestWithItemViewConfigNotFound2() throws Exception {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel("DEFAULT");
    itemViewConfig.setItemDiscoverableSchedules(new ItemDiscoverableSchedule(false, new Date(), new Date()));
    itemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule(false, new Date(), new Date()));
    this.productHelperServiceImpl.getCurrentBuyableStatusForItem(Set.of(itemViewConfig),
        ProductHelperServiceImplTest.CHANNEL_WEB, item1.isArchived());
  }

  @Test
  public void getCurrentBuyableStatusForItemTestWithNoBuyableSchedule() throws Exception {
    this.itemViewConfig.setItemBuyableSchedules(null);
    boolean result = this.productHelperServiceImpl
      .getCurrentBuyableStatusForItem(this.item1.getItemViewConfigs(), ProductHelperServiceImplTest.CHANNEL_DEFAULT,
        item1.isArchived());
    assertTrue(result);
  }

  @Test
  public void getCurrentBuyableStatusForItemTestWithNullChannel() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl.getCurrentBuyableStatusForItem(this.item1.getItemViewConfigs(), null, item1.isArchived()));
  }

  @Test
  public void getCurrentBuyableStatusForItemTestWithNullItemViewConfig() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl
      .getCurrentBuyableStatusForItem(null, ProductHelperServiceImplTest.CHANNEL_DEFAULT, false));
  }

  @Test
  public void getCurrentDiscoverableStatusForItemTestWithDiscoverableScheduleActive() throws Exception {
    boolean result = this.productHelperServiceImpl.getCurrentDiscoverableStatusForItem(this.item1.getItemViewConfigs(),
      ProductHelperServiceImplTest.CHANNEL_DEFAULT, item1.isArchived());
    assertFalse(result);
  }

  @Test
  public void getCurrentDiscoverableStatusForItemTestWithDiscoverableScheduleActiveForArchivedProduct()
    throws Exception {
    item1.setArchived(true);
    boolean result = this.productHelperServiceImpl.getCurrentDiscoverableStatusForItem(this.item1.getItemViewConfigs(),
      ProductHelperServiceImplTest.CHANNEL_DEFAULT, item1.isArchived());
    assertFalse(result);
  }

  @Test
  public void getCurrentDiscoverableStatusForItemTestWithDiscoverableScheduleNotActive() throws Exception {
    this.itemDiscoverableSchedule.setStartDateTime(this.startDateFalse);
    this.itemDiscoverableSchedule.setEndDateTime(this.endDateFalse);
    boolean result = this.productHelperServiceImpl.getCurrentDiscoverableStatusForItem(this.item1.getItemViewConfigs(),
      ProductHelperServiceImplTest.CHANNEL_DEFAULT, item1.isArchived());
    assertTrue(result);
  }

  @Test
  public void getCurrentDiscoverableStatusForItemTestWithEmptyItemViewConfig() {
    this.itemViewConfigs.clear();
    try {
      this.productHelperServiceImpl.getCurrentDiscoverableStatusForItem(this.item1.getItemViewConfigs(),
          ProductHelperServiceImplTest.CHANNEL_WEB, item1.isArchived());
    } catch (Exception e) {
      assertTrue(e instanceof Exception);
      assertEquals(ProductHelperServiceImplTest.NO_ITEM_VIEW_CONFIG_AVAILABLE + ProductHelperServiceImplTest.CHANNEL_DEFAULT, e.getMessage());
    }
  }

  @Test
  public void getCurrentDiscoverableStatusForItemTestWithItemViewConfigNotFound() throws Exception {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel("DESKTOP_WEB");
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setBuyable(true);
    Date endDateTime = new Date(currDate.getTime() - 100000);
    itemViewConfig.setItemDiscoverableSchedules(new ItemDiscoverableSchedule(true, new Date(), endDateTime));
    this.productHelperServiceImpl.getCurrentDiscoverableStatusForItem(Set.of(itemViewConfig),
        ProductHelperServiceImplTest.CHANNEL_WEB, item1.isArchived());
  }

  @Test
  public void getCurrentDiscoverableStatusForItemTestWithItemViewConfigNotFound2() throws Exception {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    ItemDiscoverableSchedule discoverableSchedule = new ItemDiscoverableSchedule();
    itemViewConfig.setItemDiscoverableSchedules(discoverableSchedule);
    itemViewConfig.setChannel("DESKTOP_WEB");
    this.item1.setItemViewConfigs(Set.of(itemViewConfig));
    this.productHelperServiceImpl.getCurrentDiscoverableStatusForItem(this.item1.getItemViewConfigs(),
        ProductHelperServiceImplTest.CHANNEL_WEB, item1.isArchived());
  }

  @Test
  public void getCurrentDiscoverableStatusForItemTestWithItemViewConfigNotFound3() throws Exception {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    ItemDiscoverableSchedule discoverableSchedule = new ItemDiscoverableSchedule();
    discoverableSchedule.setStartDateTime(new Date());
    itemViewConfig.setItemDiscoverableSchedules(discoverableSchedule);
    itemViewConfig.setChannel("DESKTOP_WEB");
    this.item1.setItemViewConfigs(Set.of(itemViewConfig));
    this.productHelperServiceImpl.getCurrentDiscoverableStatusForItem(Set.of(itemViewConfig),
        ProductHelperServiceImplTest.CHANNEL_WEB, item1.isArchived());
  }

  @Test
  public void getDiscoverableStatusByChannelErrorTest() throws Exception {
    this.productHelperServiceImpl.getCurrentDiscoverableStatusForItem(this.item1.getItemViewConfigs(),
        ProductHelperServiceImplTest.CHANNEL_WEB, item1.isArchived());
  }

  @Test
  public void getCurrentDiscoverableStatusForItemTestWithNoDiscoverableSchedule() throws Exception {
    this.itemViewConfig.setItemDiscoverableSchedules(null);
    boolean result = this.productHelperServiceImpl.getCurrentDiscoverableStatusForItem(this.item1.getItemViewConfigs(),
      ProductHelperServiceImplTest.CHANNEL_DEFAULT, item1.isArchived());
    assertTrue(result);
  }

  @Test
  public void getCurrentDiscoverableStatusForItemTestWithNullChannel() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl
      .getCurrentDiscoverableStatusForItem(this.item1.getItemViewConfigs(), null, item1.isArchived()));
  }

  @Test
  public void getCurrentDiscoverableStatusForItemTestWithNullItemViewConfig() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl
      .getCurrentDiscoverableStatusForItem(null, ProductHelperServiceImplTest.CHANNEL_DEFAULT, false));
  }

  @Test
  public void getOriginalBuyableStatusForItemTest() throws Exception {
    boolean result = this.productHelperServiceImpl.getOriginalBuyableStatusForItem(this.item1.getItemViewConfigs(),
        item1.isArchived());
    assertTrue(result);
  }

  @Test
  public void getOriginalBuyableStatusForItemArchiveTest() throws Exception {
    boolean result = this.productHelperServiceImpl.getOriginalBuyableStatusForItem(this.item1.getItemViewConfigs(),
        true);
    assertFalse(result);
  }

  @Test
  public void getOriginalBuyableStatusForItemExceptionTest() throws Exception {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(CHANNEL_MOBILE);
    Assertions.assertThrows(Exception.class, () -> this.productHelperServiceImpl.getOriginalBuyableStatusForItem(ImmutableSet.of(itemViewConfig1), item1.isArchived()));
  }

  @Test
  public void getOriginalBuyableStatusForItemNullViewConfigExceptionTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl.getOriginalBuyableStatusForItem(null, item1.isArchived()));
  }

  @Test
  public void getOriginalDiscoverableStatusForItemTest() throws Exception {
    boolean result = this.productHelperServiceImpl.getOriginalDiscoverableStatusForItem(this.item1.getItemViewConfigs(), item1.isArchived());
    assertTrue(result);
  }

  @Test
  public void getOriginalDiscoverableStatusForItemArchiveTest() throws Exception {
    boolean result = this.productHelperServiceImpl.getOriginalDiscoverableStatusForItem(this.item1.getItemViewConfigs(), true);
    assertFalse(result);
  }

  @Test
  public void getOriginalDiscoverableStatusForItemExceptionTest() throws Exception {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(CHANNEL_MOBILE);
    Assertions.assertThrows(Exception.class, () ->this.productHelperServiceImpl.getOriginalDiscoverableStatusForItem(ImmutableSet.of(itemViewConfig1), item1.isArchived()));
  }

  @Test
  public void getOriginalDiscoverableStatusForItemNullViewConfigExceptionTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl.getOriginalDiscoverableStatusForItem(null, item1.isArchived()));
  }

  @Test
  public void getSettlementTypeTest() {
    productHelperServiceImpl.getSettlementType(product, item);
  }

  @Test
  public void modifyItemNamesTest() {
    this.productHelperServiceImpl.modifyItemNames(this.listOfItems, ProductHelperServiceImplTest.OLD_PRODUCT_NAME,
        ProductHelperServiceImplTest.NEW_PRODUCT_NAME);
  }

  @Test
  public void modifyItemNamesTestWithBlankNewName() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl
      .modifyItemNames(this.listOfItems, ProductHelperServiceImplTest.OLD_PRODUCT_NAME, null));
  }

  @Test
  public void modifyItemNamesTestWithBlankOldName() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl
      .modifyItemNames(this.listOfItems, null, ProductHelperServiceImplTest.NEW_PRODUCT_NAME));
  }

  @Test
  public void modifyItemNamesTestWithNullItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl.modifyItemNames(null, ProductHelperServiceImplTest.OLD_PRODUCT_NAME,
        ProductHelperServiceImplTest.NEW_PRODUCT_NAME));
  }

  @Test
  public void setMasterDataItemFromMasterDataTest() throws Exception {
    Set<String> itemCodes = new HashSet<String>();
    itemCodes.add(ProductHelperServiceImplTest.ITEM_CODE);
    Map<String, MasterDataItem> mapOfMasterDataItem = new HashMap<String, MasterDataItem>();
    mapOfMasterDataItem.put(ProductHelperServiceImplTest.ITEM_CODE, new MasterDataItem());
    when(this.masterDataService.getMasterDataItems(ProductHelperServiceImplTest.STORE_ID,
      ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME, itemCodes)).thenReturn(mapOfMasterDataItem);
    this.productHelperServiceImpl.setMasterDataItemFromMasterData(ProductHelperServiceImplTest.STORE_ID,
      ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME, this.item);
    verify(this.masterDataService).getMasterDataItems(ProductHelperServiceImplTest.STORE_ID,
      ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME, itemCodes);
  }

  @Test
  public void setMasterDataProductFromMasterDataTest() throws Exception {
    this.productHelperServiceImpl.setMasterDataProductFromMasterData(ProductHelperServiceImplTest.STORE_ID,
      ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME, this.product);
    Set<String> productCodes = new HashSet<String>();
    productCodes.add(this.product.getProductCode());
    verify(this.masterDataService).getMasterDataProducts(ProductHelperServiceImplTest.STORE_ID,
      ProductHelperServiceImplTest.USERNAME, ProductHelperServiceImplTest.REQUEST_ID, productCodes);
  }

  @Test
  public void setMultipleMasterDataItemsFromMasterDataTest() throws Exception {
    Set<String> itemCodes = new HashSet<String>();
    itemCodes.add(ProductHelperServiceImplTest.ITEM_CODE);
    Map<String, MasterDataItem> mapOfMasterDataItem = new HashMap<String, MasterDataItem>();
    mapOfMasterDataItem.put(ProductHelperServiceImplTest.ITEM_CODE, new MasterDataItem());
    when(this.masterDataService.getMasterDataItems(ProductHelperServiceImplTest.STORE_ID,
      ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME, itemCodes)).thenReturn(mapOfMasterDataItem);
    this.productHelperServiceImpl.setMultipleMasterDataItemsFromMasterData(ProductHelperServiceImplTest.STORE_ID,
      ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME, this.listOfItems);
    verify(this.masterDataService).getMasterDataItems(ProductHelperServiceImplTest.STORE_ID,
      ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME, itemCodes);
  }

  @Test
  public void setProductDetailTest() throws Exception {
    ProductCounterResponse productCounterResponseValue = new ProductCounterResponse();
    productCounterResponseValue.setBusinessPartnerCode(this.productRequestVO.getMerchantCode());
    productCounterResponseValue.setCounter(ProductHelperServiceImplTest.PRODUCT_COUNTER);
    categoryDetailResponse.setSalesCategoryReferences(Arrays.asList(new CategoryReferenceResponse()));
    String merchantCode = this.productRequestVO.getMerchantCode();
    when(this.xbpOutbound.productCounterIncrementAndGet(any(), any(), any(),eq(merchantCode))).thenReturn(productCounterResponseValue);
    when(this.formulaUtil
      .appendWithSerial(merchantCode.toUpperCase(), ProductHelperServiceImplTest.PRODUCT_COUNTER, 5)).thenReturn(ProductHelperServiceImplTest.PRODUCT_SKU);
    when(businessPartnerService.isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE)).thenReturn(true);
    Product result = this.productHelperServiceImpl
      .setProductDetail(ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME,
        ProductHelperServiceImplTest.STORE_ID, this.productRequestVO, this.productDetailResponse);
    verify(this.xbpOutbound).productCounterIncrementAndGet(any(), any(), any(),eq(merchantCode));
    verify(this.formulaUtil)
      .appendWithSerial(merchantCode.toUpperCase(), ProductHelperServiceImplTest.PRODUCT_COUNTER, 5);
    verify(this.productCategoryBaseClient)
      .getCategoryDetail(ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME,
        ProductHelperServiceImplTest.CATEGORY_ID);
    verify(this.objectConverterService).convertToSalesCatalogsFromDirectParentCategory(ProductHelperServiceImplTest.REQUEST_ID,
      ProductHelperServiceImplTest.USERNAME,
        this.categoryDetailResponse.getSalesCategoryReferences(), new ArrayList<>(), true);
    verify(businessPartnerService).isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE);
    Assertions.assertNotNull(result);
  }

  @Test
  public void setProductDetailWithThrowExceptionOnMissingCategoryIdTest() throws Exception {
    ReflectionTestUtils.setField(productHelperServiceImpl, "throwExceptionOnMissingCategoryId",
      true);
    ProductCounterResponse productCounterResponseValue = new ProductCounterResponse();
    productCounterResponseValue.setBusinessPartnerCode(this.productRequestVO.getMerchantCode());
    productCounterResponseValue.setCounter(ProductHelperServiceImplTest.PRODUCT_COUNTER);
    GdnRestSingleResponse<ProductCounterResponse> productCounterResponse = new GdnRestSingleResponse<ProductCounterResponse>(productCounterResponseValue,
      ProductHelperServiceImplTest.REQUEST_ID);
    categoryDetailResponse.setSalesCategoryReferences(Arrays.asList(new CategoryReferenceResponse()));
    String merchantCode = this.productRequestVO.getMerchantCode();
    when(this.xbpOutbound.productCounterIncrementAndGet(any(), any(), any(), eq(merchantCode))).thenReturn(productCounterResponseValue);
    when(this.formulaUtil
      .appendWithSerial(merchantCode.toUpperCase(), ProductHelperServiceImplTest.PRODUCT_COUNTER, 5)).thenReturn(ProductHelperServiceImplTest.PRODUCT_SKU);
    when(businessPartnerService.isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE)).thenReturn(true);
    Product result = this.productHelperServiceImpl
      .setProductDetail(ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME,
        ProductHelperServiceImplTest.STORE_ID, this.productRequestVO, this.productDetailResponse);
    verify(this.formulaUtil)
      .appendWithSerial(merchantCode.toUpperCase(), ProductHelperServiceImplTest.PRODUCT_COUNTER, 5);
    verify(this.productCategoryBaseClient)
      .getCategoryDetail(ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME,
        ProductHelperServiceImplTest.CATEGORY_ID);
    verify(this.objectConverterService).convertToSalesCatalogsFromDirectParentCategory(ProductHelperServiceImplTest.REQUEST_ID,
      ProductHelperServiceImplTest.USERNAME,
      this.categoryDetailResponse.getSalesCategoryReferences(), new ArrayList<>(), true);
    verify(businessPartnerService).isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE);
    verify(this.xbpOutbound).productCounterIncrementAndGet(any(), any(), any(), eq(merchantCode));
  }

  @Test
  public void setProductDetailWithThrowExceptionOnMissingCategoryIdTest_withException() throws Exception {
    ReflectionTestUtils.setField(productHelperServiceImpl, "throwExceptionOnMissingCategoryId",
      true);
    ProductCounterResponse productCounterResponseValue = new ProductCounterResponse();
    productCounterResponseValue.setBusinessPartnerCode(this.productRequestVO.getMerchantCode());
    productCounterResponseValue.setCounter(ProductHelperServiceImplTest.PRODUCT_COUNTER);
    this.productDetailResponse.getProductCategoryResponses().stream().findFirst().get().getCategory().setId(null);
    categoryDetailResponse.setSalesCategoryReferences(Arrays.asList(new CategoryReferenceResponse()));
    String merchantCode = this.productRequestVO.getMerchantCode();
    when(this.xbpOutbound.productCounterIncrementAndGet(any(), any(), any(), eq(merchantCode))).thenReturn(productCounterResponseValue);
    when(this.formulaUtil
      .appendWithSerial(merchantCode.toUpperCase(), ProductHelperServiceImplTest.PRODUCT_COUNTER, 5)).thenReturn(ProductHelperServiceImplTest.PRODUCT_SKU);
    when(businessPartnerService.isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE)).thenReturn(true);
    when(this.productCategoryBaseClient.getCategoryDetail(ProductHelperServiceImplTest.REQUEST_ID,
      ProductHelperServiceImplTest.USERNAME, null)).thenReturn(null);
    try {
      Assertions.assertThrows(
          ApplicationRuntimeException.class, () -> this.productHelperServiceImpl.setProductDetail(ProductHelperServiceImplTest.REQUEST_ID,
        ProductHelperServiceImplTest.USERNAME, ProductHelperServiceImplTest.STORE_ID, this.productRequestVO, this.productDetailResponse));
    }
    finally {
      verify(this.xbpOutbound).productCounterIncrementAndGet(any(), any(), any(), eq(merchantCode));
      verify(this.formulaUtil)
        .appendWithSerial(merchantCode.toUpperCase(), ProductHelperServiceImplTest.PRODUCT_COUNTER, 5);
    }
  }
  @Test
  public void setProductDetailWithNullCategoryDetailsFromPCBTest() throws Exception {
    ProductCounterResponse productCounterResponseValue = new ProductCounterResponse();
    productCounterResponseValue.setBusinessPartnerCode(this.productRequestVO.getMerchantCode());
    productCounterResponseValue.setCounter(ProductHelperServiceImplTest.PRODUCT_COUNTER);
    GdnRestSingleResponse<ProductCounterResponse> productCounterResponse = new GdnRestSingleResponse<ProductCounterResponse>(productCounterResponseValue,
      ProductHelperServiceImplTest.REQUEST_ID);
    when(this.productCategoryBaseClient.getCategoryDetail(ProductHelperServiceImplTest.REQUEST_ID,
      ProductHelperServiceImplTest.USERNAME, ProductHelperServiceImplTest.CATEGORY_ID)).thenReturn(null);
    categoryDetailResponse.setSalesCategoryReferences(Arrays.asList(new CategoryReferenceResponse()));
    String merchantCode = this.productRequestVO.getMerchantCode();
    when(this.xbpOutbound.productCounterIncrementAndGet(any(), any(), any(), eq(merchantCode))).thenReturn(productCounterResponseValue);
    when(this.formulaUtil
      .appendWithSerial(merchantCode.toUpperCase(), ProductHelperServiceImplTest.PRODUCT_COUNTER, 5)).thenReturn(ProductHelperServiceImplTest.PRODUCT_SKU);
    when(businessPartnerService.isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE)).thenReturn(true);
    Product result = this.productHelperServiceImpl
      .setProductDetail(ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME,
        ProductHelperServiceImplTest.STORE_ID, this.productRequestVO, this.productDetailResponse);
    verify(this.formulaUtil)
      .appendWithSerial(merchantCode.toUpperCase(), ProductHelperServiceImplTest.PRODUCT_COUNTER, 5);
    verify(this.productCategoryBaseClient)
      .getCategoryDetail(ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME,
        ProductHelperServiceImplTest.CATEGORY_ID);
    verify(this.objectConverterService).convertToSalesCatalogsFromDirectParentCategory(ProductHelperServiceImplTest.REQUEST_ID,
      ProductHelperServiceImplTest.USERNAME,
      new ArrayList<>(), new ArrayList<>(), true);
    verify(businessPartnerService).isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE);
    verify(this.xbpOutbound).productCounterIncrementAndGet(any(), any(), any(), eq(merchantCode));
    Assertions.assertNotNull(result);
  }

  @Test
  public void setProductDetailTestWithHalalcategory() throws Exception {
    ProductCounterResponse productCounterResponseValue = new ProductCounterResponse();
    productCounterResponseValue.setBusinessPartnerCode(this.productRequestVO.getMerchantCode());
    productCounterResponseValue.setCounter(ProductHelperServiceImplTest.PRODUCT_COUNTER);
    categoryDetailResponse.setSalesCategoryReferences(Arrays.asList(new CategoryReferenceResponse()));
    categoryDetailResponse.setHalalCategory(true);
    String merchantCode = this.productRequestVO.getMerchantCode();
    when(this.xbpOutbound.productCounterIncrementAndGet(any(), any(), any(),eq(merchantCode))).thenReturn(productCounterResponseValue);
    when(this.formulaUtil
        .appendWithSerial(merchantCode.toUpperCase(), ProductHelperServiceImplTest.PRODUCT_COUNTER, 5)).thenReturn(ProductHelperServiceImplTest.PRODUCT_SKU);
    when(businessPartnerService.isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE)).thenReturn(true);
    Product result = this.productHelperServiceImpl
        .setProductDetail(ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME,
            ProductHelperServiceImplTest.STORE_ID, this.productRequestVO, this.productDetailResponse);
    verify(this.xbpOutbound).productCounterIncrementAndGet(any(), any(), any(),eq(merchantCode));
    verify(this.formulaUtil)
        .appendWithSerial(merchantCode.toUpperCase(), ProductHelperServiceImplTest.PRODUCT_COUNTER, 5);
    verify(this.productCategoryBaseClient)
        .getCategoryDetail(ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME,
            ProductHelperServiceImplTest.CATEGORY_ID);
    verify(this.objectConverterService).convertToSalesCatalogsFromDirectParentCategory(ProductHelperServiceImplTest.REQUEST_ID,
        ProductHelperServiceImplTest.USERNAME,
        this.categoryDetailResponse.getSalesCategoryReferences(), new ArrayList<>(), true);
    verify(businessPartnerService).isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(true, result.getCurationStatus().equals(CurationStatus.NEED_CURATION));
  }


  @Test
  public void setProductDetailTestWithException() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl.setProductDetail(ProductHelperServiceImplTest.REQUEST_ID,
      ProductHelperServiceImplTest.USERNAME, ProductHelperServiceImplTest.STORE_ID, null,
        this.productDetailResponse));
  }

  @Test
  public void setProductDetailWithGivenSKUValueTest() throws Exception {
    categoryDetailResponse.setB2bSalesCategoryReferences(Arrays.asList(new CategoryReferenceResponse()));
    this.productRequestVO.setProductSku(ProductHelperServiceImplTest.PRODUCT_SKU);
    Product result = this.productHelperServiceImpl
      .setProductDetail(ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME,
        ProductHelperServiceImplTest.STORE_ID, this.productRequestVO, this.productDetailResponse);
    verify(this.productCategoryBaseClient)
      .getCategoryDetail(ProductHelperServiceImplTest.REQUEST_ID, ProductHelperServiceImplTest.USERNAME,
        ProductHelperServiceImplTest.CATEGORY_ID);
    verify(this.objectConverterService).convertToSalesCatalogsFromDirectParentCategory(ProductHelperServiceImplTest.REQUEST_ID,
      ProductHelperServiceImplTest.USERNAME,
        new ArrayList<>(), this.categoryDetailResponse.getB2bSalesCategoryReferences(), false);
    verify(businessPartnerService).isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE);
    Assertions.assertNotNull(result);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    this.categoryDetailResponse = new CategoryDetailResponse();
    this.categoryDetailResponse.setSalesCategoryReferences(this.salesCategoryReferences);

    when(this.channelService.getDefaultChannel()).thenReturn(ChannelName.DEFAULT.toString());

    when(this.productCategoryBaseClient.getCategoryDetail(ProductHelperServiceImplTest.REQUEST_ID,
      ProductHelperServiceImplTest.USERNAME, ProductHelperServiceImplTest.CATEGORY_ID)).thenReturn(this.categoryDetailResponse);

    this.productAttribute = new ProductAttribute();
    this.productAttribute.setItemSku(ProductHelperServiceImplTest.ITEM_SKU);
    this.listOfProductAttributes = new ArrayList<ProductAttribute>();
    this.listOfProductAttributes.add(this.productAttribute);
    this.product = new Product();
    this.product.setDefiningAttributes(this.listOfProductAttributes);
    product.setProductSku(PRODUCT_SKU);
    product.setProductCode(PRODUCT_CODE);


    this.listRequest =
      new GdnRestListRequest(ProductHelperServiceImplTest.PAGE, ProductHelperServiceImplTest.SIZE);
    this.listRequest.setRequestId(ProductHelperServiceImplTest.REQUEST_ID);

    this.itemAttributeValues = new ArrayList<MasterDataItemAttributeValue>();
    this.productAttributeDetails = new ArrayList<ProductAttributeDetail>();
    for (int i = 1; i <= 2; i++) {
      this.itemAttributeValue = new MasterDataItemAttributeValue();
      MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
      masterDataAttribute.setAttributeCode(ProductHelperServiceImplTest.ATTRIBUTE_CODE + i + "");
      masterDataAttribute.setVariantCreation(i == 1);
      this.itemAttributeValue.setMasterDataAttribute(masterDataAttribute);
      this.itemAttributeValue.setAttributeValue(ProductHelperServiceImplTest.ATTRIBUTE_VALUE + i + "");
      this.itemAttributeValues.add(this.itemAttributeValue);
      this.productAttributeDetail = new ProductAttributeDetail();
      this.productAttributeDetail.setAttributeName(ProductHelperServiceImplTest.ATTRIBUTE_CODE + i + "");
      this.productAttributeDetail.setAttributeValue(ProductHelperServiceImplTest.ATTRIBUTE_VALUE + i + "");
      this.productAttributeDetails.add(this.productAttributeDetail);
    }
    this.productAttribute = new ProductAttribute();
    this.productAttribute.setItemSku(ProductHelperServiceImplTest.ITEM_SKU);
    this.productAttribute.setProductAttributeDetails(this.productAttributeDetails);

    this.productUpdated = new Product();
    this.productUpdated.setStoreId(ProductHelperServiceImplTest.STORE_ID);
    this.productUpdated.setProductSku(ProductHelperServiceImplTest.PRODUCT_SKU);
    this.productUpdated.getDefiningAttributes().add(this.productAttribute);
    this.productUpdated.setCreatedBy("any");

    this.item = new Item();
    this.masterDataItem = new MasterDataItem();
    this.masterDataItem.setGeneratedItemName(ProductHelperServiceImplTest.GENERATED_ITEM_NAME);

    this.item.setItemCode(ProductHelperServiceImplTest.ITEM_CODE);
    this.item.setMasterDataItem(this.masterDataItem);

    this.currDate = new Date();

    this.startDateTrue = new Date(this.currDate.getTime() - ProductHelperServiceImplTest.AN_HOUR_IN_MILLISECOND);
    this.endDateTrue = new Date(this.currDate.getTime() + ProductHelperServiceImplTest.AN_HOUR_IN_MILLISECOND);

    this.startDateFalse = new Date(this.currDate.getTime() + ProductHelperServiceImplTest.AN_HOUR_IN_MILLISECOND);
    this.endDateFalse = new Date(this.currDate.getTime() - ProductHelperServiceImplTest.AN_HOUR_IN_MILLISECOND);

    this.itemBuyableSchedule = new ItemBuyableSchedule();
    this.itemBuyableSchedule.setBuyable(false);
    this.itemBuyableSchedule.setStartDateTime(this.startDateTrue);
    this.itemBuyableSchedule.setEndDateTime(this.endDateTrue);

    this.itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    this.itemDiscoverableSchedule.setDiscoverable(false);
    this.itemDiscoverableSchedule.setStartDateTime(this.startDateTrue);
    this.itemDiscoverableSchedule.setEndDateTime(this.endDateTrue);
    this.itemViewConfig = new ItemViewConfig();
    this.itemViewConfig.setBuyable(true);
    this.itemViewConfig.setDiscoverable(true);
    this.itemViewConfig.setChannel(ProductHelperServiceImplTest.CHANNEL_DEFAULT);
    this.itemViewConfig.setItemBuyableSchedules(this.itemBuyableSchedule);
    this.itemViewConfig.setItemDiscoverableSchedules(this.itemDiscoverableSchedule);
    this.itemViewConfigs = new HashSet<ItemViewConfig>();
    this.itemViewConfigs.add(this.itemViewConfig);
    this.item1 = new Item();
    item1.setItemViewConfigs(itemViewConfigs);


    this.currItemViewConfig = new ItemViewConfig();
    this.currItemViewConfig.setChannel(ProductHelperServiceImplTest.CHANNEL_DEFAULT);
    this.currItemViewConfig.setBuyable(false);
    this.currItemViewConfigs = new HashSet<ItemViewConfig>();
    this.currItemViewConfigs.add(this.currItemViewConfig);
    this.item.setItemViewConfigs(this.currItemViewConfigs);

    this.productRequestVO = new Product();
    this.productRequestVO.setProductCode(ProductHelperServiceImplTest.PRODUCT_CODE);
    this.productRequestVO.setProductType(ProductHelperServiceImplTest.PRODUCT_TYPE_REGULAR);
    this.productRequestVO.setSettlementType(ProductHelperServiceImplTest.SETTLEMENT_TYPE);
    this.productRequestVO.setMerchantCode(ProductHelperServiceImplTest.MERCHANT_CODE);

    this.listOfItems = new ArrayList<Item>();
    this.listOfItems.add(this.item);

    this.mapOfItemsAndItemSkus = new HashMap<String, Item>();
    this.mapOfItemsAndItemSkus.put(ProductHelperServiceImplTest.ITEM_SKU, this.item);

    this.productCategoryResponses = new ArrayList<ProductCategoryResponse>();
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setCategory(new CategoryResponse());
    productCategoryResponse.getCategory().setId(ProductHelperServiceImplTest.CATEGORY_ID);
    this.productCategoryResponses.add(productCategoryResponse);
    this.productDetailResponse = new ProductDetailResponse();
    this.productDetailResponse.setProductCategoryResponses(this.productCategoryResponses);

    this.masterCatalog = new MasterCatalog();
    this.salesCatalogs = new ArrayList<SalesCatalog>();

    when(this.objectConverterService.convertToMasterCatalog(this.productCategoryResponses)).thenReturn(this.masterCatalog);
    when(this.objectConverterService.convertToSalesCatalogs(this.productCategoryResponses)).thenReturn(this.salesCatalogs);

    offlineItem.setOfferPrice(OFFLINE_OFFER_PRICE);
    offlineItem.setListPrice(OFFLINE_LIST_PRICE);
    offlineItem.setPickupPointCode(PICKUP_POINT_CODE);

    Price price = new Price();
    price.setListPrice(OFFLINE_LIST_PRICE);
    price.setOfferPrice(OFFLINE_OFFER_PRICE);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setPrice(Collections.singleton(price));
    itemPickupPoint.setItemSku(ITEM_SKU);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.cachedService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.productCategoryBaseClient);
    verifyNoMoreInteractions(this.productRepository);
    verifyNoMoreInteractions(this.masterDataService);
    verifyNoMoreInteractions(this.formulaUtil);
    verifyNoMoreInteractions(this.xbpOutbound);
    verifyNoMoreInteractions(this.offlineItemService);
    verifyNoMoreInteractions(this.gdnMapper);
    verifyNoMoreInteractions(this.systemParameterService);
    verifyNoMoreInteractions(this.businessPartnerService);
  }

  @Test
  public void updateItemViewConfigForExistingChannelTestWithChannelFound() {
    Item result = this.productHelperServiceImpl
      .updateItemViewConfigForExistingChannel(this.item, this.itemViewConfig);
    assertNotNull(result);
    assertTrue(result.getItemViewConfigs().contains(this.itemViewConfig));
  }

  @Test
  public void updateItemViewConfigForExistingChannelTestWithChannelNotFound() {
    this.itemViewConfig.setChannel(ProductHelperServiceImplTest.CHANNEL_MOBILE);
    Item result = this.productHelperServiceImpl
      .updateItemViewConfigForExistingChannel(this.item, this.itemViewConfig);
    assertNull(result);
  }

  @Test
  public void updateItemViewConfigForExistingChannelTestWithEmptyItemViewConfig() {
    this.item.setItemViewConfigs(new HashSet<ItemViewConfig>());
    Item result = this.productHelperServiceImpl
      .updateItemViewConfigForExistingChannel(this.item, this.itemViewConfig);
    assertNull(result);
  }

  @Test
  public void updateItemViewConfigForExistingChannelTestWithNullItem() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl.updateItemViewConfigForExistingChannel(null, this.itemViewConfig));
  }

  @Test
  public void updateItemViewConfigForExistingChannelTestWithNullItemViewConfig() {
    Set<ItemViewConfig> viewConfigs = new HashSet<ItemViewConfig>();
    viewConfigs.add(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl.updateItemViewConfigForExistingChannel(this.item, viewConfigs));
  }

  @Test
  public void constructListOfCategoriesListOfProductExistsInPCBTest() {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setId(CATEGORY_ID);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    when(this.cachedService.getParentCategoriesFromMasterData(eq(REQUEST_ID), eq(USERNAME), eq(CATEGORY_ID)))
      .thenReturn(categoryResponses);
    when(this.cachedService.getParentCategoriesFromMasterData(eq(REQUEST_ID), eq(USERNAME), eq(PRODUCT_CODE)))
      .thenThrow(new ApplicationRuntimeException());
    List<List<CategoryResponse>> categoryListResponse = productHelperServiceImpl
      .getCategoryResponseListByCategoryCodesForProducts(REQUEST_ID, USERNAME, Arrays.asList(CATEGORY_ID, PRODUCT_CODE));
    verify(this.cachedService, times(2)).getParentCategoriesFromMasterData(eq(REQUEST_ID), eq(USERNAME), anyString());
    assertNotNull(categoryListResponse);
    Assertions.assertEquals(1, categoryListResponse.size());
    Assertions.assertEquals(1, categoryListResponse.get(0).size());
    Assertions.assertEquals(CATEGORY_ID, categoryListResponse.get(0).get(0).getId());
  }

  @Test
  public void constructListOfCategoriesListOfProductExistsInPCBWithDBAndCacheTest() {
    ReflectionTestUtils.setField(this.productHelperServiceImpl, "categoryHierarchyCacheSwitchEnabled", true);
    Set<String> categoryCodes = new HashSet<>();
    categoryCodes.add(CATEGORY_ID);
    categoryCodes.add(PRODUCT_CODE);
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setId(CATEGORY_ID);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    when(this.cachedService.getParentCategoriesFromMasterData(eq(REQUEST_ID), eq(USERNAME), eq(PRODUCT_CODE)))
            .thenThrow(new ApplicationRuntimeException());
    List<List<CategoryResponse>> categoryListResponse = productHelperServiceImpl
            .getCategoryResponseListByCategoryCodesForProducts(REQUEST_ID, USERNAME, Arrays.asList(CATEGORY_ID, PRODUCT_CODE));
    verify(this.cachedService).getParentCategoriesFromDbAndCache(eq(REQUEST_ID), eq(USERNAME), eq(categoryCodes));
  }
  @Test
  public void constructListOfCategoriesListOfProductExistsInPCBWithDBAndCacheCategoryListNonNullTest() {
    ReflectionTestUtils.setField(this.productHelperServiceImpl, "categoryHierarchyCacheSwitchEnabled", true);
    Set<String> categoryCodes = new HashSet<>();
    Map<String, List<CategoryResponse>> categoryHierarchyMap = new HashMap<>();
    categoryHierarchyMap.put(CATEGORY_ID, new ArrayList<>());
    categoryCodes.add(CATEGORY_ID);
    categoryCodes.add(PRODUCT_CODE);
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setId(CATEGORY_ID);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);

    when(this.cachedService.getParentCategoriesFromDbAndCache(eq(REQUEST_ID), eq(USERNAME),
        eq(categoryCodes))).thenReturn(categoryHierarchyMap);
    when(this.cachedService.getParentCategoriesFromMasterData(eq(REQUEST_ID), eq(USERNAME), eq(PRODUCT_CODE)))
        .thenThrow(new ApplicationRuntimeException());
    List<List<CategoryResponse>> categoryListResponse = productHelperServiceImpl
        .getCategoryResponseListByCategoryCodesForProducts(REQUEST_ID, USERNAME, Arrays.asList(CATEGORY_ID, PRODUCT_CODE));
    verify(this.cachedService).getParentCategoriesFromDbAndCache(eq(REQUEST_ID), eq(USERNAME), eq(categoryCodes));
  }


  @Test
  public void constructListOfCategoriesListOfProductExistsInPCBTest_withNullCategoryCodes() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> productHelperServiceImpl.getCategoryResponseListByCategoryCodesForProducts(REQUEST_ID, USERNAME, null));
  }

  @Test
  public void constructOfflineItem_nullItem() {
    OfflineItem offlineItem = new OfflineItem();
    productHelperServiceImpl.constructOfflineItem(null, offlineItem);
  }

  @Test
  public void constructOfflineItem_nullOfflineItem() {
    productHelperServiceImpl.constructOfflineItem(item, null);
    assertEquals(null, item.getOfflineItems());
  }

  @Test
  public void constructOfflineItem_success() {
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setOfflineItemId("id");
    offlineItem.setItemSku(ITEM_SKU);
    offlineItem.setPickupPointCode("pp-001");

    productHelperServiceImpl.constructOfflineItem(item, offlineItem);

    verify(gdnMapper).deepCopy(any(), eq(ItemViewConfig.class));

    assertEquals(1, item.getOfflineItems().size());
    assertEquals(offlineItem.getOfflineItemId(), item.getOfflineItems().get(0).getUniqueId());
    assertEquals(offlineItem.getItemSku(), item.getOfflineItems().get(0).getItemSku());
    assertEquals(offlineItem.getPickupPointCode(),
      item.getOfflineItems().get(0).getPickupPointCode());
  }

  @Test
  public void constructOfflineItems_nullItems() {
    productHelperServiceImpl.findAndConstructOfflineItems(STORE_ID, null);
  }

  @Test
  public void constructOfflineItems_noCncActivatedItem_success() {
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);

    Item item = listOfItems.get(0);
    item.setCncActivated(false);
    item.setItemSku(ITEM_SKU);

    List<Item> items = new ArrayList<>();
    items.add(item);

    productHelperServiceImpl.findAndConstructOfflineItems(STORE_ID, items);
    assertEquals(ITEM_SKU, items.get(0).getUniqueId());

    List<OfflineItemDetailVo> offlineItemDetailVo = items.get(0).getOfflineItems();
    assertNull(offlineItemDetailVo);
  }

  @Test
  public void constructOfflineItems_withLimit_success() {
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);

    Item item = listOfItems.get(0);
    item.setCncActivated(true);
    item.setItemSku(ITEM_SKU);

    List<Item> items = new ArrayList<>();
    items.add(item);

    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue("1");

    when(itemPickupPointService
      .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, itemSkus.iterator().next(),
        true, false, PageRequest.of(0, Integer.valueOf(systemParameter.getValue())))).thenReturn(
      new PageImpl<>(Collections.singletonList(itemPickupPoint), PageRequest.of(0, 1), 1));
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS))
      .thenReturn(systemParameter);

    productHelperServiceImpl.findAndConstructOfflineItems(STORE_ID, items);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS);
    verify(gdnMapper).deepCopy(any(), eq(ItemViewConfig.class));
    verify(itemPickupPointService)
      .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, itemSkus.iterator().next(),
        true, false, PageRequest.of(0, Integer.valueOf(systemParameter.getValue())));
    List<OfflineItemDetailVo> offlineItemDetailVo = items.get(0).getOfflineItems();
    assertEquals(offlineItem.getItemSku(), offlineItemDetailVo.get(0).getItemSku());
  }

  @Test
  public void constructOfflineItems_withoutLimit_success() {
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);

    Item item = listOfItems.get(0);
    item.setCncActivated(true);
    item.setItemSku(ITEM_SKU);

    List<Item> items = new ArrayList<>();
    items.add(item);

    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue("-1");

    when(itemPickupPointService
      .findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID, itemSkus,
        true)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS))
      .thenReturn(systemParameter);

    productHelperServiceImpl.findAndConstructOfflineItems(STORE_ID, items);

    verify(gdnMapper).deepCopy(any(), eq(ItemViewConfig.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS);
    verify(itemPickupPointService)
      .findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID, itemSkus,
        true);
    List<OfflineItemDetailVo> offlineItemDetailVo = items.get(0).getOfflineItems();
    assertEquals(offlineItem.getItemSku(), offlineItemDetailVo.get(0).getItemSku());
  }

  @Test
  public void overwriteItemPriceWithOfflinePrice_success() {
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setListPrice(20000.0);
    offlineItem.setOfferPrice(10000.0);
    offlineItem.setUpdatedBy("system");
    offlineItem.setUpdatedDate(new Date());

    Price price = new Price();
    price.setListPrice(0.0);
    price.setOfferPrice(0.0);
    price.setCurrency("USD");
    price.setLastUpdatedBy("someone");
    price.setLastUpdatedDate(new Date(10000000));
    Set<Price> prices = new HashSet<>(Arrays.asList(price));

    productHelperServiceImpl.overwriteItemPriceWithOfflinePrice(prices, offlineItem);

    Price result = prices.stream().findFirst().get();
    double delta = 0.0;
    assertEquals(offlineItem.getListPrice(), result.getListPrice(), delta);
    assertEquals(offlineItem.getOfferPrice(), result.getOfferPrice(), delta);
    assertEquals(offlineItem.getUpdatedBy(), result.getLastUpdatedBy());
    assertEquals(offlineItem.getUpdatedDate(), result.getLastUpdatedDate());
  }

  @Test
  public void overwriteItemViewConfigsForOfflineItem_success() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(Boolean.FALSE);
    itemViewConfig.setBuyable(Boolean.FALSE);

    Set<ItemViewConfig> itemViewConfigs = new HashSet<>(Arrays.asList(itemViewConfig));

    productHelperServiceImpl.overwriteItemViewConfigsForOfflineItem(itemViewConfigs);

    ItemViewConfig result = itemViewConfigs.stream().findFirst().get();
    assertEquals(Boolean.TRUE, result.isBuyable());
    assertEquals(Boolean.TRUE, result.isDiscoverable());
  }

  @Test
  public void findAndConstructOfflineItemsByPickupPointCodeTest() {
    listOfItems.forEach(item -> {
      item.setCncActivated(Boolean.TRUE);
      item.setItemSku(ITEM_SKU);
    });
    Set<String> itemSkus = listOfItems.stream().map(Item::getItemSku).collect(Collectors.toSet());
    when(this.itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndPickupPointCodeAndCncActivatedAndMarkForDeleteFalse(
      STORE_ID, itemSkus, true, PICKUP_POINT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    productHelperServiceImpl.findAndConstructOfflineItemsByPickupPointCode(STORE_ID, listOfItems, PICKUP_POINT_CODE);
    verify(gdnMapper).deepCopy(any(), eq(ItemViewConfig.class));
    verify(this.itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndPickupPointCodeAndCncActivatedAndMarkForDeleteFalse(
      STORE_ID, itemSkus, true, PICKUP_POINT_CODE);
  }

  @Test
  public void findAndConstructOfflineItemsByPickupPointCodeNotCnCTest() {
    listOfItems.forEach(item -> {
      item.setCncActivated(Boolean.FALSE);
      item.setItemSku(ITEM_SKU);
    });
    productHelperServiceImpl.findAndConstructOfflineItemsByPickupPointCode(STORE_ID, listOfItems, PICKUP_POINT_CODE);
  }

  @Test
  public void findAndConstructOfflineItemsByPickupPointCodeEmptyPickupPointTest() {
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);

    Item item = listOfItems.get(0);
    item.setCncActivated(true);
    item.setItemSku(ITEM_SKU);

    List<Item> items = new ArrayList<>();
    items.add(item);

    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue("1");

    when(itemPickupPointService
      .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, itemSkus.iterator().next(),
        true, false, PageRequest.of(0, Integer.valueOf(systemParameter.getValue())))).thenReturn(
      new PageImpl<>(Collections.singletonList(itemPickupPoint), PageRequest.of(0, 1), 1));
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS))
      .thenReturn(systemParameter);

    productHelperServiceImpl.findAndConstructOfflineItemsByPickupPointCode(STORE_ID, listOfItems, null);

    verify(itemPickupPointService)
      .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, itemSkus.iterator().next(),
        true, false, PageRequest.of(0, Integer.valueOf(systemParameter.getValue())));
    verify(gdnMapper).deepCopy(any(), eq(ItemViewConfig.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS);

    List<OfflineItemDetailVo> offlineItemDetailVo = items.get(0).getOfflineItems();
    assertEquals(offlineItem.getItemSku(), offlineItemDetailVo.get(0).getItemSku());
  }

  @Test
  public void findAndConstructOfflineItemsByPickupPointCodeMismatchOfflineItemTest() {
    listOfItems.forEach(item -> {
      item.setCncActivated(Boolean.TRUE);
      item.setItemSku(ITEM_SKU);
    });
    Set<String> itemSkus = listOfItems.stream().map(Item::getItemSku).collect(Collectors.toSet());

    when(this.itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndPickupPointCodeAndCncActivatedAndMarkForDeleteFalse(
      STORE_ID, itemSkus, true, PICKUP_POINT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    productHelperServiceImpl.findAndConstructOfflineItemsByPickupPointCode(STORE_ID, listOfItems, PICKUP_POINT_CODE);
    verify(gdnMapper).deepCopy(any(), eq(ItemViewConfig.class));
    verify(this.itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndPickupPointCodeAndCncActivatedAndMarkForDeleteFalse(
      STORE_ID, itemSkus, true, PICKUP_POINT_CODE);
  }

  @Test
  public void findAndConstructOfflineItemsByPickupPointCodeEmptyOfflineItemsTest() {
    listOfItems.forEach(item -> {
      item.setCncActivated(Boolean.TRUE);
      item.setItemSku(ITEM_SKU);
    });
    Set<String> itemSkus = listOfItems.stream().map(Item::getItemSku).collect(Collectors.toSet());

    when(this.itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndPickupPointCodeAndCncActivatedAndMarkForDeleteFalse(
      STORE_ID, itemSkus, true, PICKUP_POINT_CODE)).thenReturn(null);
    productHelperServiceImpl
      .findAndConstructOfflineItemsByPickupPointCode(STORE_ID, listOfItems, PICKUP_POINT_CODE);
    verify(this.itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndPickupPointCodeAndCncActivatedAndMarkForDeleteFalse(
      STORE_ID, itemSkus, true, PICKUP_POINT_CODE);
  }

  @Test
  public void getCategoryResponseByCategoryCodeTest() {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setId(CATEGORY_ID);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    when(this.cachedService.getParentCategoriesFromMasterData(eq(REQUEST_ID), eq(USERNAME), eq(CATEGORY_ID)))
      .thenReturn(categoryResponses);
    CategoryResponse categoryResponseByCategoryCode = productHelperServiceImpl.getCategoryResponseByCategoryCode(REQUEST_ID, USERNAME, CATEGORY_ID);
    verify(this.cachedService).getParentCategoriesFromMasterData(eq(REQUEST_ID), eq(USERNAME), anyString());
    Assertions.assertEquals(CATEGORY_ID, categoryResponseByCategoryCode.getId());
  }

  @Test
  public void getCategoryResponseByCategoryCodeResResponseNullTest() {
    when(this.cachedService.getParentCategoriesFromMasterData(eq(REQUEST_ID), eq(USERNAME), eq(CATEGORY_ID)))
      .thenReturn(null);
    CategoryResponse categoryResponseByCategoryCode = productHelperServiceImpl.getCategoryResponseByCategoryCode(REQUEST_ID, USERNAME, CATEGORY_ID);
    verify(this.cachedService).getParentCategoriesFromMasterData(eq(REQUEST_ID), eq(USERNAME), anyString());
    assertNull(categoryResponseByCategoryCode);
  }

  @Test
  public void getCategoryResponseByCategoryCodeNullTest() {
    CategoryResponse categoryResponseByCategoryCode = productHelperServiceImpl.getCategoryResponseByCategoryCode(REQUEST_ID, USERNAME, null);
    assertNull(categoryResponseByCategoryCode);
  }

  @Test
  public void getCachedItemsByProductSkuTest() {
    item.setItemSku(ITEM_SKU);
    item1.setItemSku(ITEM_SKU_1);
    List<Item> items = new ArrayList<>();
    items.add(item);
    items.add(item1);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(items);
    List<Item> result = productHelperServiceImpl
      .getCachedItemsByProductSkuWithoutOverridingL5Data(STORE_ID, PRODUCT_SKU, Arrays.asList(ITEM_SKU));
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(ITEM_SKU_1, result.get(0).getItemSku());
  }

  @Test
  public void getCachedItemsByProductSkuWithOverridingMppTest() {
    item.setItemSku(ITEM_SKU);
    item1.setItemSku(ITEM_SKU_1);
    List<Item> items = new ArrayList<>();
    items.add(item);
    items.add(item1);
    when(itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(items);
    List<Item> result =
        productHelperServiceImpl.getCachedItemsByProductSku(STORE_ID, PRODUCT_SKU, Arrays.asList(ITEM_SKU));
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(objectConverterService).overrideL4DetailsFromL5(Mockito.anyList(), Mockito.anyList());
    verify(itemPickupPointService).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList());
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(ITEM_SKU_1, result.get(0).getItemSku());
  }

  @Test
  public void constructOfflineItemForTransactionTest() {
    item.setPrice(new HashSet<>());
    item.getPrice().add(new Price());
    item.getItemViewConfigs().stream().findFirst().get().setDiscoverable(true);
    item.getItemViewConfigs().stream().findFirst().get().setBuyable(true);
    productHelperServiceImpl.constructOfflineItemForTransaction(item, itemPickupPoint);
    Assertions.assertEquals(
      item.getPrice().stream().findFirst().get().getListPrice(),OFFLINE_LIST_PRICE, 0.0);
    Assertions.assertEquals(
      item.getPrice().stream().findFirst().get().getOfferPrice(), OFFLINE_OFFER_PRICE, 0.0);
    Assertions.assertEquals(PICKUP_POINT_CODE, item.getPickupPointCode());
  }

  @Test
  public void constructOfflineItemForTransaction_mfdOfflineItemTest() {
    offlineItem.setMarkForDelete(true);
    item.setPrice(new HashSet<>());
    item.getPrice().add(new Price());
    item.getItemViewConfigs().stream().findFirst().get().setDiscoverable(true);
    item.getItemViewConfigs().stream().findFirst().get().setBuyable(true);
    productHelperServiceImpl.constructOfflineItemForTransaction(item, itemPickupPoint);
    Assertions.assertEquals(
      item.getPrice().stream().findFirst().get().getListPrice(),OFFLINE_LIST_PRICE, 0.0);
    Assertions.assertEquals(
      item.getPrice().stream().findFirst().get().getOfferPrice(),OFFLINE_OFFER_PRICE, 0.0);
    Assertions.assertEquals(OFFLINE_LIST_PRICE, item.getPrice().stream().findFirst().get().getListPrice(),
      0.0);
    Assertions.assertEquals(OFFLINE_OFFER_PRICE, item.getPrice().stream().findFirst().get().getOfferPrice(),
      0.0);
    Assertions.assertEquals(PICKUP_POINT_CODE, item.getPickupPointCode());
  }

  @Test
  public void constructOfflineItemForTransaction_nullItemTest() {
    productHelperServiceImpl.constructOfflineItemForTransaction(null, itemPickupPoint);
  }

  @Test
  public void constructOfflineItemForTransaction_nullOfflineItemTest() {
    productHelperServiceImpl.constructOfflineItemForTransaction(item, null);
  }

  @Test
  public void constructOfflineItemForTransaction_emptyPriceTest() {
    item.setPrice(new HashSet<>());
    item.getItemViewConfigs().stream().findFirst().get().setDiscoverable(true);
    item.getItemViewConfigs().stream().findFirst().get().setBuyable(true);
    productHelperServiceImpl.constructOfflineItemForTransaction(item, itemPickupPoint);
    Assertions.assertEquals(PICKUP_POINT_CODE, item.getPickupPointCode());
  }

  @Test
  public void constructOfflineItemForTransaction_nullViewConfigTest() {
    item.setPrice(new HashSet<>());
    item.setItemViewConfigs(Collections.emptySet());
    productHelperServiceImpl.constructOfflineItemForTransaction(item, itemPickupPoint);
    Assertions.assertEquals(PICKUP_POINT_CODE, item.getPickupPointCode());
  }

  @Test
  public void constructOfflineItemForTransaction_falseViewConfigTest() {
    item.setPrice(new HashSet<>());
    item.getItemViewConfigs().stream().findFirst().get().setDiscoverable(false);
    item.getItemViewConfigs().stream().findFirst().get().setBuyable(false);
    productHelperServiceImpl.constructOfflineItemForTransaction(item, itemPickupPoint);
    Assertions.assertEquals(PICKUP_POINT_CODE, item.getPickupPointCode());
  }

  @Test
  public void setItemDetailItemVoNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> productHelperServiceImpl.setItemDetail(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, 1, null));
  }

  @Test
  public void setItemDetailItemVoItemSkuBlankTest() {
    when(formulaUtil.appendWithSerial(PRODUCT_SKU, 2, 5)).thenReturn(ITEM_SKU);
    productHelperServiceImpl.setItemDetail(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, 1, new ItemVo());
    verify(formulaUtil).appendWithSerial(PRODUCT_SKU, 2, 5);
  }

  @Test
  public void setItemDetailItemVoTest() {
    ItemVo itemVo = new ItemVo();
    itemVo.setItemSku(ITEM_SKU);
    ItemVo itemVo1 = productHelperServiceImpl.setItemDetail(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, 1, itemVo);
    Assertions.assertEquals(MERCHANT_CODE, itemVo1.getMerchantCode());
  }

  @Test
  public void updateItemViewConfigForExistingChannelItemPickupPointTest() {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setChannel(CHANNEL_DEFAULT);
    ItemViewConfig itemViewConfig3 = new ItemViewConfig();
    itemViewConfig3.setChannel(CHANNEL_WEB);

    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemViewConfig(ImmutableSet.of(itemViewConfig2, itemViewConfig3)).build();

    productHelperServiceImpl.updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint, itemViewConfig1);
  }

  @Test
  public void updateItemViewConfigForExistingChannelItemPickupPointChannelTest() {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(CHANNEL_DEFAULT);
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setChannel(CHANNEL_DEFAULT);
    ItemViewConfig itemViewConfig3 = new ItemViewConfig();
    itemViewConfig3.setChannel(CHANNEL_WEB);

    ItemPickupPoint itemPickupPoint =
      ItemPickupPoint.builder().itemViewConfig(ImmutableSet.of(itemViewConfig2, itemViewConfig3)).build();

    productHelperServiceImpl.updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint, itemViewConfig1);
  }

  @Test
  public void updateItemViewConfigForExistingChannelItemPickupPointChannelTestForUpdatedViewConfigFalse() {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(CHANNEL_DEFAULT);
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setChannel(CHANNEL_MOBILE);
    ItemViewConfig itemViewConfig3 = new ItemViewConfig();
    itemViewConfig3.setChannel(CHANNEL_MOBILE);
    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemViewConfig(ImmutableSet.of(itemViewConfig2, itemViewConfig3)).build();
    productHelperServiceImpl.updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint, itemViewConfig1);
  }

  @Test
  public void updateItemViewConfigForExistingChannelItemPickupPointTestDiscoveryUpdate() {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setChannel(CHANNEL_DEFAULT);
    ItemViewConfig itemViewConfig3 = new ItemViewConfig();
    itemViewConfig3.setChannel(CHANNEL_WEB);
    itemViewConfig1.setDiscoverable(true);

    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemViewConfig(ImmutableSet.of(itemViewConfig2, itemViewConfig3)).build();

    productHelperServiceImpl.updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint, itemViewConfig1);
  }

  @Test
  public void updateItemViewConfigForExistingChannelItemPickupPointTestDiscoveryUpdateCnc1pswitchonDefaultChannel() {
    ReflectionTestUtils.setField(this.productHelperServiceImpl, "cncForWarehouseFeatureSwitch", true);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setChannel(CHANNEL_DEFAULT);
    ItemViewConfig itemViewConfig3 = new ItemViewConfig();
    itemViewConfig3.setChannel(CHANNEL_WEB);
    itemViewConfig1.setDiscoverable(true);

    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemViewConfig(ImmutableSet.of(itemViewConfig2, itemViewConfig3)).build();

    productHelperServiceImpl.updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint, itemViewConfig1);
  }

  @Test
  public void updateItemViewConfigForExistingChannelItemPickupPointTestDiscoveryUpdateCnc1pswitchonCncChannel() {
    ReflectionTestUtils.setField(this.productHelperServiceImpl, "cncForWarehouseFeatureSwitch", true);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.CNC);
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setChannel(Constants.CNC);
    ItemViewConfig itemViewConfig3 = new ItemViewConfig();
    itemViewConfig3.setChannel(CHANNEL_WEB);
    itemViewConfig1.setDiscoverable(true);

    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemViewConfig(ImmutableSet.of(itemViewConfig2, itemViewConfig3)).build();

    productHelperServiceImpl.updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint, itemViewConfig1);
  }


  @Test
  public void updateItemPickupPointViewConfigForExistingChannelTest() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(Boolean.FALSE);
    itemViewConfig.setBuyable(Boolean.FALSE);

    Set<ItemViewConfig> itemViewConfigs = new HashSet<>(Arrays.asList(itemViewConfig));
    itemViewConfigs.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    this.productHelperServiceImpl
      .updateItemPickupPointViewConfigForExistingChannel(this.itemPickupPoint, itemViewConfigs);
  }

  @Test
  public void updateItemPickupPointViewConfigForNewChannelTest() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(Boolean.FALSE);
    itemViewConfig.setBuyable(Boolean.FALSE);
    itemViewConfig.setChannel(Constants.CNC);

    Set<ItemViewConfig> itemViewConfigs = new HashSet<>(Arrays.asList(itemViewConfig));
    itemViewConfigs.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    this.productHelperServiceImpl
        .updateItemPickupPointViewConfigForExistingChannel(this.itemPickupPoint, itemViewConfigs);
  }

  @Test
  public void updateItemPickupPointViewConfigForEmptyChannelTest() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(Boolean.FALSE);
    itemViewConfig.setBuyable(Boolean.FALSE);
    itemViewConfig.setChannel(CHANNEL_DEFAULT);

    ItemViewConfig itemViewConfigChanged = new ItemViewConfig();
    itemViewConfigChanged.setDiscoverable(Boolean.FALSE);
    itemViewConfigChanged.setBuyable(Boolean.FALSE);
    itemViewConfigChanged.setChannel(CHANNEL_WEB);

    Set<ItemViewConfig> itemViewConfigs = new HashSet<>(Arrays.asList(itemViewConfig));
    itemViewConfigs.add(itemViewConfig);
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    ItemViewConfig result = this.productHelperServiceImpl
      .updateItemPickupPointViewConfigForExistingChannel(itemViewConfig, itemViewConfigChanged);
    assertNull(result);
  }

  @Test
  public void nullItemPickupPointTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.productHelperServiceImpl
      .updateItemPickupPointViewConfigForExistingChannel(null, itemViewConfigs));
  }
}
