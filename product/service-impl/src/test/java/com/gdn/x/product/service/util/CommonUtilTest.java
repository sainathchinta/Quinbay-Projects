package com.gdn.x.product.service.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.MockitoAnnotations.initMocks;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.x.product.enums.DistributionStatus;
import com.gdn.x.product.domain.event.enums.ItemChangeEventType;
import com.gdn.x.product.domain.event.model.ItemDataChange;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.model.entity.Video;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import com.gdn.x.product.model.vo.AddDeleteVariantRequestVo;
import com.gdn.x.product.model.vo.AddVariantRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointDeleteRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrInputDocument;
import org.joda.time.DateTime;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.BeanUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.ItemInfo;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemPickupPointViewConfigChangeEvent;
import com.gdn.x.product.domain.event.model.ItemPriceChangeEventModel;
import com.gdn.x.product.domain.event.model.ItemViewConfigWithArchivedChangeEvent;
import com.gdn.x.product.domain.event.model.MerchantPromoDiscountChangeEvent;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.domain.event.model.ParentCategory;
import com.gdn.x.product.domain.event.model.ProductChange;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.PromoType;
import com.gdn.x.product.enums.SalesChannel;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataAllowedAttributeValue;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductAttributeValue;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductCenterHistory;
import com.gdn.x.product.model.entity.ProductL3SolrReindexStatus;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.ActiveProductDetailVo;
import com.gdn.x.product.model.vo.B2bFieldsVo;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.model.vo.BuyableScheduleVo;
import com.gdn.x.product.model.vo.DeleteOfflineItemVO;
import com.gdn.x.product.model.vo.DiscoverableScheduleVo;
import com.gdn.x.product.model.vo.FieldUpdateRequestVo;
import com.gdn.x.product.model.vo.ImageBasicInfoResponse;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCategoryVO;
import com.gdn.x.product.model.vo.ItemNameSkuVO;
import com.gdn.x.product.model.vo.ItemPickupPointListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemSummaryResponseVO;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.OfflineItemHistoryDetailVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductBasicInfoResponse;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.product.rest.web.model.FieldValueObject;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.dto.MasterDataAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.ProductCenterHistoryResponse;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.SortedDefiningAttributeDTO;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
import com.gdn.x.product.rest.web.model.request.AttributeScoreRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemScoreRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest;
import com.gdn.x.product.rest.web.model.request.ProductBundleCreationRequest;
import com.gdn.x.product.rest.web.model.request.ProductScoreRequest;
import com.gdn.x.product.rest.web.model.response.EanUpcPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemL4SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuAndPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.PriceUpdatedInTimeRangeL5Response;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.enums.UpdatedFields;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class CommonUtilTest {

  private static final String PRODUCT_NAME = "product_name";
  private static final String URL = "url";
  private static final String VIDEO_ID = "videoId";
  private static final String FINAL_URL = "finalUrl";
  private static final String VIDEO_NAME = "videoName";
  private static final String COVER_IMAGE = "coverImage";
  private static final String SOURCE_IMAGE = "sourceImage";
  private static final String PRODUCT_NAME_2 = "product_name_2";
  private static final String PRODUCT_BRAND = "product_brand";
  private static final String PRODUCT_BRAND_2 = "product_brand_2";
  private static final String PRODUCT_CODE = "product_code";
  private static final String ATTRIBUTE_CODE = "attribute_code";
  private static final String ATTRIBUTE_NAME = "attribute_name";
  private static final String NEW_PRODUCT_NAME = "new product_name";
  private static final String ITEM_NAME = "item_name";
  private static final String NEW_ITEM_NAME = "new item_name";
  private static final String BRAND = "brand";
  private static final String NEW_BRAND = "new brand";
  private static final String MERCHANT_CODE = "merchant_code";
  private static final String MASTER_CATALOG = "masterCatalog";
  private static final String NEW_MASTER_CATALOG = "new masterCatalog";
  private static final String SALES_CATALOG = "12051";
  private static final String NEW_SALES_CATALOG = "new sales_catalog";
  private static final String PRODUCT_MAIN_IMAGE = "product_main_image";
  private static final String NEW_PRODUCT_MAIN_IMAGE = "new product_main_image";
  private static final String ITEM_SKU = "item_sku";
  private static final String ITEM_SKU_2 = "item_sku_2";
  private static final String PRODUCT_TYPE = "1";
  private static final String NEW_PRODUCT_TYPE = "2";
  private static final String DESCRIPTIVE_ATTRIBUTE_VALUE = "descriptiveValue";
  private static final String ATTRIBUTE_CODE_1 = "attributeCode1";
  private static final String PREDEFINED_ATTRIBUTE_VALUE = "predefinedAttributeValue";
  private static final String ATTRIBUTE_CODE_2 = "attributeCode2";
  private static final String ALLOWED_ATTRIBUTE_VALUE = "allowedAttributeValue";
  public static final String USERNAME = "username";
  public static final String X_PRODUCT_INTEGRATOR = "x-product-integrator";
  public static final String X_PRODUCT_INTEGRATOR_SYSTEM_USERNAME = "x-product-integrator,system,username";
  private static final String DATE_FORMAT = "dd-MM-yyyy";
  private static final String UPCOMING_DATE = "05-01-2030";
  private static String DESCRIPTION = "desc";
  private static String LONG_DESCRIPTION = "long-desc";
  private static String STRING_SOLR_DELIMITER = "#_#";
  private static String CODE = "Code";
  private static String PRODUCT_SKU = "PRODUCT_SKU";
  private static String PRODUCT_SKU_1 = "PRODUCT_SKU_1";
  private static String STORE_ID = "STORE_ID";
  private static final String CATEGORY_CODE = "CAT-01";
  private static final String ITEM_IMAGE = "true#_#image1";
  private static final String NEW_ITEM_IMAGE = "new true#_#image1";
  private static final String IMAGE = "image1";
  private static final String CATEGORY_CODE_2 = "CAT-02";
  private static final String UPC_CODE = "UPC_CODE";
  private static final String LOCATION_PATH = "locationPath";
  private static final String LOCATION_PATH_2 = "locationPath2";
  private static final String PRODUCT_CENTER_ACTIVITY = "activity";
  private static final String PRODUCT_CENTER_DESCRIPTION = "description";
  private static final String PRODUCT_CENTER_UPDATEDBY = "updatedBy";
  private static final String solrStringDelimiter = "#_#";
  private static final double OLD_PRODUCT_SCORE = 10.0;
  private static final double NEW_PRODUCT_SCORE = 20.0;
  private static final String OFFLINE_PRICE_ADD = "1000";
  private static final String OFFLINE_PRICE_REMOVE = "1000";
  private static final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";
  private static final String SIZE_CHART_CODE = "SIZE_CHART_CODE";
  private static final String PICKUP_POINT_CODE_1 = "PICKUP_POINT_CODE_1";
  private static final String CAMPAIGN_CODE = "CAMPAIGN_CODE";
  private static final String PRISTINE_ID = "PRISTINE_ID";
  private static final String PRODUCT_CENTER_UPDATED_DATE = "productCenterUpdatedDate";
  private static final String B2C_SELLER_CHANNEL = "BLIBLI";
  private static final String B2B_SELLER_CHANNEL = "BLIBLI FOR BUSINESS";
  private static final Date date = new Date();
  private static final String DEFAULT_CHANNEL = "default channel";
  private static final String OTHER_CHANNEL = "other channel";
  private static final String categoriesToGenerateMasterSku = "c1,c2,c4";
  private static final String categoriesToGenerateMasterSku1 = "c4,c5,c6";
  private static final String masterSku = "masterSku";
  private static final String ITEM_CODE_1 = "item_code_1";
  private static final String ITEM_CODE_2 = "item_code_2";
  private static final String DESCRIPTIVE_VALUE = "descriptive_value";
  private static final String PREDEFINED_VALUE = "predefined_value";
  private static final String ATTRIBUTE_CODE_3 = "attribute_code_3";
  private static final String ATTRIBUTE_CODE_4 = "attribute_code_4";
  private static final String OTHER_ATTRIBUTE_TYPE = "OTHER_TYPE";

  private static Item item;
  private static ProductAndItemsVO productAndItemsVO;
  private static ProductAndItemSolr productAndItemSolr;
  private SolrDocument solrDocumentL3 = new SolrDocument();
  private ProductSolr productSolr = new ProductSolr();
  private ProductSolr productSolr2;
  private Item item1;
  private CatalogResponse catalogResponse = new CatalogResponse();
  private CategoryResponse categoryResponse = new CategoryResponse();
  private ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
  private Product product = new Product();
  private Product product1 = new Product();
  private ProductDetailResponse productDetailResponse = new ProductDetailResponse();
  private ProductSpecialAttribute productSpecialAttribute = new ProductSpecialAttribute();
  private AttributeResponse attributeResponse = new AttributeResponse();
  private ProductItemResponse itemResponse = new ProductItemResponse();
  private Image image = new Image();
  private ProductScore productScore;
  private Map<String, FieldValueObject> fieldValueObjectMap = new HashMap<>();
  private MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
  private ProductCenterHistory productCenterHistory;
  private BasicInfoProductResponse basicInfoProductResponse;
  private Price price = new Price();
  private InventoryStockInfoDTO inventoryStockInfoDTO = new InventoryStockInfoDTO();
  private ItemCatalogVO itemCatalogVO = new ItemCatalogVO();
  private ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
  private Price itemPrice = new Price();
  private ItemChange itemChange = new ItemChange();
  private ProductAttributeDomainEventModel productAttributeDomainEventModel = new ProductAttributeDomainEventModel();
  private ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
  private static final Date PREVIOUS_DATE = new DateTime(new Date()).minusDays(1).toDate();
  private static final Date NEXT_DATE = new DateTime(new Date()).plusDays(1).toDate();
  private static final ItemVo itemVo = new ItemVo();
  private static final ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
  private static final ProfileResponse profileResponse = new ProfileResponse();
  private static final ItemViewConfig defaultItemViewConfig = new ItemViewConfig();
  private static final ItemViewConfig b2bItemViewConfig = new ItemViewConfig();
  BusinessPartner businessPartner = new BusinessPartner();
  private List<ItemPickupPoint> itemPickupPointList;
  private Set<Price> prices;
  private ItemPickupPointSummaryRequest itemPickupPointSummaryRequest;
  private EditProductDetailDTO editProductDetailDTO;
  private DeleteOfflineItemVO deleteOfflineItemVO;
  private DeleteOfflineItemVO deleteOfflineItemVO1;
  private ProductChange productChange;
  private List<String> categoryCodesHierarchy = Arrays.asList("c1", "c2", "c3");
  private ProductAndL5MigrationRequest productAndL5MigrationRequest;
  private static final double ZERO_DIMENSIONS = 0.0;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    item = new Item();
    item.setItemSku(SolrFieldNames.ITEM_SKU);
    item.setArchived(true);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel("DEFAULT");
    itemViewConfig.setBuyable(false);
    itemViewConfig.setDiscoverable(false);
    item.setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfig)));
    item.setUpdatedDate(new Date());
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setGeneratedItemName("Item name");
    masterDataItem.setUpcCode(UPC_CODE);
    item.setMasterDataItem(masterDataItem);
    Set<Price> priceSetL4 = new HashSet<>();
    priceSetL4.add(new Price());
    item.setPrice(priceSetL4);
    Product product = new Product();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName("Product name");
    product.setMasterDataProduct(masterDataProduct);
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(CODE);
    Category category = new Category();
    category.setCategoryCode(CODE);
    masterCatalog.setCategory(category);
    product.setMasterCatalog(masterCatalog);
    SalesCatalog salesCatalog = new SalesCatalog();
    salesCatalog.setCatalogCode(SALES_CATALOG);
    salesCatalog.setListOfCategories(Arrays.asList(category));
    product.setSalesCatalogs(Arrays.asList(salesCatalog));
    product.setProductSku(PRODUCT_SKU);
    product.setStoreId(STORE_ID);
    product.setProductCenterUpdatedDate((new SimpleDateFormat("yyyy/MM/dd").parse("2021/01/20")));
    productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(PRODUCT_SKU);
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    productAndItemSolr.setProductName(PRODUCT_NAME);
    productAndItemSolr.setItemName(ITEM_NAME);
    productAndItemSolr.setBrand(BRAND);
    productAndItemSolr.setProductType(PRODUCT_TYPE);
    productAndItemSolr.setMasterCatalog(MASTER_CATALOG);
    productAndItemSolr.setSalesCatalog(Arrays.asList(SALES_CATALOG));
    productAndItemSolr.setItemImages(Arrays.asList(ITEM_IMAGE));
    productAndL5MigrationRequest = new ProductAndL5MigrationRequest();

    solrDocumentL3.addField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocumentL3.addField(SolrFieldNames.ITEM_IMAGES, Arrays.asList(ITEM_IMAGE));
    solrDocumentL3.addField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    solrDocumentL3.addField(SolrFieldNames.MASTER_CATALOG, SolrFieldNames.MASTER_CATALOG);
    solrDocumentL3.setField(SolrFieldNames.IS_SYNCHRONIZED, Boolean.FALSE);
    solrDocumentL3.setField(SolrFieldNames.MARK_FOR_DELETE, Boolean.FALSE);
    solrDocumentL3.setField(SolrFieldNames.PRODUCT_CODE, SolrFieldNames.PRODUCT_CODE);
    solrDocumentL3.setField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    solrDocumentL3.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonList(SolrFieldNames.SALES_CATALOG));
    solrDocumentL3.setField(SolrFieldNames.IS_SUSPENDED, Boolean.FALSE);
    solrDocumentL3.setField(SolrFieldNames.MERCHANT_CODE, SolrFieldNames.MERCHANT_CODE);
    solrDocumentL3.setField(SolrFieldNames.STORE_ID, SolrFieldNames.STORE_ID);
    solrDocumentL3.setField(SolrFieldNames.CREATED_DATE, new Date());
    solrDocumentL3.setField(SolrFieldNames.UPDATED_DATE, new Date());
    solrDocumentL3.setField(SolrFieldNames.TOTAL_SCORE, 10.0);
    solrDocumentL3.setField(SolrFieldNames.PRODUCT_MAIN_IMAGE, PRODUCT_MAIN_IMAGE);
    solrDocumentL3.setField(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE, new Date());
    solrDocumentL3.setField(SolrFieldNames.PICKUP_POINT_CODES, new ArrayList<>());
    solrDocumentL3.setField(SolrFieldNames.IS_ARCHIVED, false);
    solrDocumentL3.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE, false);
    solrDocumentL3.setField(SolrFieldNames.PROMO_ITEM_SKUS, Arrays.asList(ITEM_SKU));
    solrDocumentL3.setField(SolrFieldNames.IS_IN_STOCK, true);
    solrDocumentL3.setField(SolrFieldNames.WHOLESALE_ITEM_SKUS, Arrays.asList(ITEM_SKU));
    solrDocumentL3.setField(SolrFieldNames.VARIANT_COUNT, 5);
    solrDocumentL3.setField(SolrFieldNames.MAXIMUM_SELLING_PRICE, 1000.0);
    solrDocumentL3.setField(SolrFieldNames.MINIMUM_SELLING_PRICE, 1000.0);
    solrDocumentL3.setField(SolrFieldNames.MAXIMUM_LIST_PRICE, 1000.0);
    solrDocumentL3.setField(SolrFieldNames.MINIMUM_LIST_PRICE, 1000.0);
    solrDocumentL3.setField(SolrFieldNames.IS_PRE_ORDER_ACTIVE, true);
    solrDocumentL3.setField(SolrFieldNames.L5_COUNT, 5);

    productSolr.setProductSku(PRODUCT_SKU);
    productSolr.setProductCode(CODE);
    productSolr.setProductName(PRODUCT_NAME);
    productSolr.setBrand(BRAND);
    productSolr.setMerchantCode(MERCHANT_CODE);
    productSolr.setStoreId(STORE_ID);
    productSolr.setMasterCatalog(MASTER_CATALOG);
    productSolr.setSalesCatalog(Arrays.asList(SALES_CATALOG));
    productSolr.setProductMainImage(PRODUCT_MAIN_IMAGE);
    productSolr.setProductScoreTotal(80.0);
    productSolr.setProductCenterUpdatedDate((new SimpleDateFormat("yyyy/MM/dd").parse("2021/01/20")));
    productSolr.setWholesaleItemSkus(Arrays.asList(ITEM_SKU));
    productSolr.setPromoItemSkus(Arrays.asList(ITEM_SKU));
    productSolr.setIsArchived(false);
    productSolr.setOff2OnChannelActive(false);
    productSolr.setInStock(true);
    productSolr.setIsPreOrderActive(false);
    productSolr.setVariantCount(1);

    productSolr2 = new ProductSolr();
    productSolr2.setProductSku(PRODUCT_SKU_1);
    productSolr2.setProductCode(CODE);
    productSolr2.setProductName(PRODUCT_NAME);
    productSolr2.setBrand(BRAND);
    productSolr2.setMerchantCode(MERCHANT_CODE);
    productSolr2.setStoreId(STORE_ID);
    productSolr2.setMasterCatalog(MASTER_CATALOG);
    productSolr2.setSalesCatalog(Arrays.asList(SALES_CATALOG));
    productSolr2.setProductMainImage(PRODUCT_MAIN_IMAGE);
    productSolr2.setProductScoreTotal(80.0);
    productSolr2.setProductCenterUpdatedDate((new SimpleDateFormat("yyyy/MM/dd").parse("2021/01/20")));
    productSolr2.setWholesaleItemSkus(Arrays.asList(ITEM_SKU));
    productSolr2.setPromoItemSkus(Arrays.asList(ITEM_SKU));
    productSolr2.setIsArchived(false);
    productSolr2.setOff2OnChannelActive(false);
    productSolr2.setInStock(true);
    productSolr2.setIsPreOrderActive(false);
    productSolr2.setVariantCount(1);

    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);

    itemPickupPoint.setPrice(new HashSet<>(Arrays.asList(new Price())));
    itemPickupPoint.setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig)));
    itemPickupPoint.setMerchantSku(MERCHANT_CODE);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setStoreId(STORE_ID);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setMarkForDelete(false);
    itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    item1 = new Item();
    item1.setItemSku(ITEM_SKU);
    item1.setMerchantCode(MERCHANT_CODE);
    item1.setProductSku(PRODUCT_SKU);
    item1.setMarkForDelete(false);
    item1.setStoreId(STORE_ID);
    Set<Price> priceSet = new HashSet<>();
    Price price = new Price();
    price.setListPrice(10.0);
    price.setOfferPrice(8.0);
    priceSet.add(price);
    item1.setPrice(priceSet);

    catalogResponse.setCatalogCode(MASTER_CATALOG);
    catalogResponse.setName(MASTER_CATALOG);
    catalogResponse.setCatalogType(MASTER_CATALOG);
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponse.setCatalog(catalogResponse);
    productCategoryResponse.setCategory(categoryResponse);

    productSpecialAttribute.setAttributeCode(ATTRIBUTE_CODE_2);
    productSpecialAttribute.setAttributeName(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productSpecialAttribute.setAttributeName(ATTRIBUTE_CODE_2);

    image.setLocationPath(LOCATION_PATH);
    image.setMainImages(Boolean.FALSE);
    image.setSequence(0);
    image.setMainImages(true);
    itemResponse.setImages(Arrays.asList(image));
    itemResponse.setUpcCode("ups");
    attributeResponse.setName("Name");
    attributeResponse.setBasicView(true);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setName("name2");
    attributeResponse.setBasicView(false);
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.setProductAttributeValues(new ArrayList<>());
    productAttributeResponse.getProductAttributeValues().add(
        new ProductAttributeValueResponse(new AllowedAttributeValueResponse("value", 1, STORE_ID), null,
            com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.NONE, null, STORE_ID));
    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.setProductAttributeValues(new ArrayList<>());
    productAttributeResponse1.getProductAttributeValues().add(new ProductAttributeValueResponse(null, "value",
        com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE, null, STORE_ID));
    productDetailResponse.setName(PRODUCT_NAME);
    productDetailResponse.setBrand(BRAND);
    productDetailResponse.setProductCode(PRODUCT_SKU);
    productDetailResponse.setDescription(DESCRIPTION.getBytes());
    productDetailResponse.setProductCategoryResponses(new ArrayList<>());
    productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);
    productDetailResponse.setProductItemResponses(new HashSet<>(Arrays.asList(itemResponse)));

    productScore = new ProductScore();
    productScore.setDescriptionScore(10.0);
    productScore.setEanUpcScore(5.0);
    productScore.setImageScore(10.0);
    productScore.setMandatoryAttributeScore(5.0);
    productScore.setProductTitleScore(12.0);
    productScore.setRemainingAttributeScore(10.0);
    productScore.setTotalScore(60.0);
    productScore.setVideoUrlScore(5.0);
    fieldValueObjectMap.put(SolrFieldNames.ITEM_NAME,
        FieldValueObject.builder().oldValue(ITEM_NAME).newValue(NEW_ITEM_NAME).build());
    fieldValueObjectMap.put(SolrFieldNames.PRODUCT_NAME,
        FieldValueObject.builder().oldValue(PRODUCT_NAME).newValue(NEW_PRODUCT_NAME).build());
    fieldValueObjectMap.put(SolrFieldNames.PRODUCT_TYPE,
        FieldValueObject.builder().oldValue(PRODUCT_TYPE).newValue(NEW_PRODUCT_TYPE).build());
    fieldValueObjectMap.put(SolrFieldNames.MASTER_CATALOG,
        FieldValueObject.builder().oldValue(MASTER_CATALOG).newValue(NEW_MASTER_CATALOG).build());
    fieldValueObjectMap.put(SolrFieldNames.SALES_CATALOG,
        FieldValueObject.builder().oldListValues(Arrays.asList(SALES_CATALOG))
            .newListValues(Arrays.asList(NEW_SALES_CATALOG)).build());
    fieldValueObjectMap.put(SolrFieldNames.ITEM_IMAGES,
        FieldValueObject.builder().oldListValues(Arrays.asList(ITEM_IMAGE)).newListValues(Arrays.asList(NEW_ITEM_IMAGE))
            .build());
    fieldValueObjectMap.put(SolrFieldNames.BRAND,
        FieldValueObject.builder().oldValue(BRAND).newValue(NEW_BRAND).build());
    fieldValueObjectMap.put(SolrFieldNames.PRODUCT_MAIN_IMAGE,
        FieldValueObject.builder().oldValue(PRODUCT_MAIN_IMAGE).newValue(NEW_PRODUCT_MAIN_IMAGE).build());
    fieldValueObjectMap.put(SolrFieldNames.TOTAL_SCORE,
        FieldValueObject.builder().oldValue(String.valueOf(10.0)).newValue(String.valueOf(20.0)).build());

    masterDataItemImage.setLocationPath(LOCATION_PATH);
    masterDataItemImage.setMainImage(true);
    masterDataItemImage.setSequence(0);

    productCenterHistory = ProductCenterHistory.builder().productSku(PRODUCT_SKU).activity(PRODUCT_CENTER_ACTIVITY)
        .description(PRODUCT_CENTER_DESCRIPTION).build();
    productCenterHistory.setUpdatedBy(PRODUCT_CENTER_UPDATEDBY);

    basicInfoProductResponse = new BasicInfoProductResponse();
    basicInfoProductResponse.setProductCode(PRODUCT_CODE);

    inventoryStockInfoDTO.setWebProductSku(PRODUCT_SKU);
    inventoryStockInfoDTO.setWebTotalAvailableStock(0);
    inventoryStockInfoDTO.setWarehouseTotalAvailableStock(0);
    this.product.setMasterCatalog(new MasterCatalog(CATEGORY_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    this.product.setMasterDataProduct(new MasterDataProduct());
    this.product.getMasterDataProduct()
        .setMasterCatalog(new MasterCatalog(CATEGORY_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    this.product.setSalesCatalogs(new ArrayList<>());
    this.product.setProductSku(PRODUCT_SKU);
    this.product1.setMasterCatalog(new MasterCatalog(CATEGORY_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    this.product1.setSalesCatalogs(
        Arrays.asList(new SalesCatalog(SALES_CATALOG, Arrays.asList(new Category(SALES_CATALOG, SALES_CATALOG)))));
    this.product1.setProductSku(PRODUCT_SKU_1);
    this.itemPickupPoint.setPrice(Collections.singleton(price));
    itemPrice.setListPrice(1);
    itemPrice.setOfferPrice(1);

    com.gdn.x.product.domain.event.model.Price priceEvent = new com.gdn.x.product.domain.event.model.Price();
    BeanUtils.copyProperties(itemPrice, priceEvent);
    com.gdn.x.product.domain.event.model.ItemViewConfig itemViewConfigEvent =
        new com.gdn.x.product.domain.event.model.ItemViewConfig();
    BeanUtils.copyProperties(itemViewConfig, itemViewConfigEvent);
    itemChange.setPrice(Collections.singleton(priceEvent));
    itemChange.setItemViewConfigs(Collections.singleton(itemViewConfigEvent));

    productAttributeDomainEventModel.setAttribute(new AttributeDomainEventModel());
    profileResponse.setCompany(new CompanyDTO());
    defaultItemViewConfig.setChannel(Constants.DEFAULT);
    b2bItemViewConfig.setChannel(Constants.B2B);
    businessPartner.setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));

    prices = new HashSet<>();
    price.setOfferPrice(800);
    price.setListPrice(1000);
    price.setChannel(DEFAULT_CHANNEL);
    prices.add(price);
    editProductDetailDTO = new EditProductDetailDTO();

    productChange = new ProductChange();
  }

  @Test
  public void toSolrInputDocument() {
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setId(SolrFieldNames.ID);
    productAndItemSolr.setBrand(SolrFieldNames.BRAND);
    productAndItemSolr.setBuyable(Collections.singletonList(SolrFieldNames.BUYABLE));
    productAndItemSolr.setCreatedDate(new Date());
    productAndItemSolr.setUpdatedDate(new Date());
    productAndItemSolr.setDiscoverable(Collections.singletonList(SolrFieldNames.DISCOVERABLE));
    productAndItemSolr.setItemCode(SolrFieldNames.ITEM_CODE);
    productAndItemSolr.setItemImages(Collections.singletonList(SolrFieldNames.ITEM_CODE));
    productAndItemSolr.setItemName(SolrFieldNames.ITEM_NAME);
    productAndItemSolr.setItemSku(SolrFieldNames.ITEM_SKU);
    productAndItemSolr.setLatefulfillment(false);
    productAndItemSolr.setListPrice(Collections.singletonList(SolrFieldNames.LIST_PRICE));
    productAndItemSolr.setMarkForDelete(false);
    productAndItemSolr.setMasterCatalog(SolrFieldNames.MASTER_CATALOG);
    productAndItemSolr.setMerchantCode(SolrFieldNames.MERCHANT_CODE);
    productAndItemSolr.setMerchantSku(SolrFieldNames.MERCHANT_SKU);
    productAndItemSolr.setOfferPrice(Collections.singletonList(SolrFieldNames.OFFER_PRICE));
    productAndItemSolr.setPickupPointCode(SolrFieldNames.PICKUP_POINT_CODE);
    productAndItemSolr.setProductCatentryId(SolrFieldNames.PRODUCT_CATENTRY_ID);
    productAndItemSolr.setProductCode(SolrFieldNames.PRODUCT_CODE);
    productAndItemSolr.setProductName(SolrFieldNames.PRODUCT_NAME);
    productAndItemSolr.setProductSku(SolrFieldNames.PRODUCT_SKU);
    productAndItemSolr.setProductType(SolrFieldNames.PRODUCT_TYPE);
    productAndItemSolr.setSalesCatalog(Collections.singletonList(SolrFieldNames.SALES_CATALOG));
    productAndItemSolr.setStoreId(SolrFieldNames.STORE_ID);
    productAndItemSolr.setSynchronized(false);
    productAndItemSolr.setTicketTemplateCode(SolrFieldNames.TICKET_TEMPLATE_CODE);
    productAndItemSolr.setOfflinePrices(Collections.singletonList(SolrFieldNames.OFFLINE_PRICES));
    productAndItemSolr.setWholesalePriceActivated(true);
    productAndItemSolr.setProductScoreTotal(80.0);
    productAndItemSolr.setPristineId(SolrFieldNames.PRISTINE_ID);

    SolrInputDocument solrInputFields = CommonUtil.toSolrInputDocument(productAndItemSolr);
    assertEquals(SolrFieldNames.TICKET_TEMPLATE_CODE,
        solrInputFields.getFieldValue(SolrFieldNames.TICKET_TEMPLATE_CODE));
    assertEquals(true, solrInputFields.getFieldValue(SolrFieldNames.WHOLESALE_PRICE_ACTIVATED));
    assertEquals(80.0, solrInputFields.getFieldValue(SolrFieldNames.TOTAL_SCORE));
  }

  @Test
  public void toSolrInputDocumentWithTotalScore() {
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setId(SolrFieldNames.ID);
    productAndItemSolr.setBrand(SolrFieldNames.BRAND);
    productAndItemSolr.setBuyable(Collections.singletonList(SolrFieldNames.BUYABLE));
    productAndItemSolr.setCreatedDate(new Date());
    productAndItemSolr.setUpdatedDate(new Date());
    productAndItemSolr.setDiscoverable(Collections.singletonList(SolrFieldNames.DISCOVERABLE));
    productAndItemSolr.setItemCode(SolrFieldNames.ITEM_CODE);
    productAndItemSolr.setItemImages(Collections.singletonList(SolrFieldNames.ITEM_CODE));
    productAndItemSolr.setItemName(SolrFieldNames.ITEM_NAME);
    productAndItemSolr.setItemSku(SolrFieldNames.ITEM_SKU);
    productAndItemSolr.setLatefulfillment(false);
    productAndItemSolr.setListPrice(Collections.singletonList(SolrFieldNames.LIST_PRICE));
    productAndItemSolr.setMarkForDelete(false);
    productAndItemSolr.setMasterCatalog(SolrFieldNames.MASTER_CATALOG);
    productAndItemSolr.setMerchantCode(SolrFieldNames.MERCHANT_CODE);
    productAndItemSolr.setMerchantSku(SolrFieldNames.MERCHANT_SKU);
    productAndItemSolr.setOfferPrice(Collections.singletonList(SolrFieldNames.OFFER_PRICE));
    productAndItemSolr.setPickupPointCode(SolrFieldNames.PICKUP_POINT_CODE);
    productAndItemSolr.setProductCatentryId(SolrFieldNames.PRODUCT_CATENTRY_ID);
    productAndItemSolr.setProductCode(SolrFieldNames.PRODUCT_CODE);
    productAndItemSolr.setProductName(SolrFieldNames.PRODUCT_NAME);
    productAndItemSolr.setProductSku(SolrFieldNames.PRODUCT_SKU);
    productAndItemSolr.setProductType(SolrFieldNames.PRODUCT_TYPE);
    productAndItemSolr.setSalesCatalog(Collections.singletonList(SolrFieldNames.SALES_CATALOG));
    productAndItemSolr.setStoreId(SolrFieldNames.STORE_ID);
    productAndItemSolr.setSynchronized(false);
    productAndItemSolr.setTicketTemplateCode(SolrFieldNames.TICKET_TEMPLATE_CODE);
    productAndItemSolr.setOfflinePrices(Collections.singletonList(SolrFieldNames.OFFLINE_PRICES));
    productAndItemSolr.setWholesalePriceActivated(true);
    productAndItemSolr.setProductScoreTotal(80.0);
    SolrInputDocument solrInputFields = CommonUtil.toSolrInputDocument(productAndItemSolr);
    assertEquals(80.0, solrInputFields.getFieldValue(SolrFieldNames.TOTAL_SCORE));
  }

  @Test
  public void toSolrInputDocumentWithWholesalePriceNullTest() {
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setId(SolrFieldNames.ID);
    productAndItemSolr.setBrand(SolrFieldNames.BRAND);
    productAndItemSolr.setBuyable(Collections.singletonList(SolrFieldNames.BUYABLE));
    productAndItemSolr.setCreatedDate(new Date());
    productAndItemSolr.setUpdatedDate(new Date());
    productAndItemSolr.setDiscoverable(Collections.singletonList(SolrFieldNames.DISCOVERABLE));
    productAndItemSolr.setItemCode(SolrFieldNames.ITEM_CODE);
    productAndItemSolr.setItemImages(Collections.singletonList(SolrFieldNames.ITEM_CODE));
    productAndItemSolr.setItemName(SolrFieldNames.ITEM_NAME);
    productAndItemSolr.setItemSku(SolrFieldNames.ITEM_SKU);
    productAndItemSolr.setLatefulfillment(false);
    productAndItemSolr.setListPrice(Collections.singletonList(SolrFieldNames.LIST_PRICE));
    productAndItemSolr.setMarkForDelete(false);
    productAndItemSolr.setMasterCatalog(SolrFieldNames.MASTER_CATALOG);
    productAndItemSolr.setMerchantCode(SolrFieldNames.MERCHANT_CODE);
    productAndItemSolr.setMerchantSku(SolrFieldNames.MERCHANT_SKU);
    productAndItemSolr.setOfferPrice(Collections.singletonList(SolrFieldNames.OFFER_PRICE));
    productAndItemSolr.setPickupPointCode(SolrFieldNames.PICKUP_POINT_CODE);
    productAndItemSolr.setProductCatentryId(SolrFieldNames.PRODUCT_CATENTRY_ID);
    productAndItemSolr.setProductCode(SolrFieldNames.PRODUCT_CODE);
    productAndItemSolr.setProductName(SolrFieldNames.PRODUCT_NAME);
    productAndItemSolr.setProductSku(SolrFieldNames.PRODUCT_SKU);
    productAndItemSolr.setProductType(SolrFieldNames.PRODUCT_TYPE);
    productAndItemSolr.setSalesCatalog(Collections.singletonList(SolrFieldNames.SALES_CATALOG));
    productAndItemSolr.setStoreId(SolrFieldNames.STORE_ID);
    productAndItemSolr.setSynchronized(false);
    productAndItemSolr.setTicketTemplateCode(SolrFieldNames.TICKET_TEMPLATE_CODE);
    productAndItemSolr.setOfflinePrices(Collections.singletonList(SolrFieldNames.OFFLINE_PRICES));

    SolrInputDocument solrInputFields = CommonUtil.toSolrInputDocument(productAndItemSolr);
    assertEquals(SolrFieldNames.TICKET_TEMPLATE_CODE,
        solrInputFields.getFieldValue(SolrFieldNames.TICKET_TEMPLATE_CODE));
    assertNull(solrInputFields.getFieldValue(SolrFieldNames.WHOLESALE_PRICE_ACTIVATED));
  }

  @Test
  public void toProductAndItemSolr() {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.setField(SolrFieldNames.ID, SolrFieldNames.ID);
    solrDocument.setField(SolrFieldNames.TICKET_TEMPLATE_CODE, SolrFieldNames.TICKET_TEMPLATE_CODE);
    solrDocument.setField(SolrFieldNames.CNC_ACTIVE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.OFFLINE_PRICES, Collections.singletonList(SolrFieldNames.OFFER_PRICE));
    solrDocument.setField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocument.setField(SolrFieldNames.ITEM_IMAGES, Collections.singletonList(SolrFieldNames.ITEM_IMAGES));
    solrDocument.setField(SolrFieldNames.IS_SYNCHRONIZED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MARK_FOR_DELETE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.IS_LATE_FULFILLMENT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.IS_ARCHIVED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.TRADING_PRODUCT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocument.setField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    solrDocument.setField(SolrFieldNames.PRODUCT_CODE, SolrFieldNames.PRODUCT_CODE);
    solrDocument.setField(SolrFieldNames.ITEM_NAME, SolrFieldNames.ITEM_NAME);
    solrDocument.setField(SolrFieldNames.ITEM_CODE, SolrFieldNames.ITEM_CODE);
    solrDocument.setField(SolrFieldNames.ITEM_SKU, SolrFieldNames.ITEM_SKU);
    solrDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.PROMO_BUNDLING, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MERCHANT_SKU, SolrFieldNames.MERCHANT_SKU);
    solrDocument.setField(SolrFieldNames.PRODUCT_TYPE, SolrFieldNames.PRODUCT_TYPE);
    solrDocument.setField(SolrFieldNames.MERCHANT_CODE, SolrFieldNames.MERCHANT_CODE);
    solrDocument.setField(SolrFieldNames.MASTER_CATALOG, SolrFieldNames.MASTER_CATALOG);
    solrDocument.setField(SolrFieldNames.PICKUP_POINT_CODE, SolrFieldNames.PICKUP_POINT_CODE);
    solrDocument.setField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    solrDocument.setField(SolrFieldNames.PRODUCT_CATENTRY_ID, SolrFieldNames.PRODUCT_CATENTRY_ID);
    solrDocument.setField(SolrFieldNames.STORE_ID, SolrFieldNames.STORE_ID);
    solrDocument.setField(SolrFieldNames.CREATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.UPDATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.OFFER_PRICE, Collections.singletonList(SolrFieldNames.OFFER_PRICE));
    solrDocument.setField(SolrFieldNames.LIST_PRICE, Collections.singletonList(SolrFieldNames.LIST_PRICE));
    solrDocument.setField(SolrFieldNames.BUYABLE, Collections.singletonList(SolrFieldNames.BUYABLE));
    solrDocument.setField(SolrFieldNames.DISCOVERABLE, Collections.singletonList(SolrFieldNames.DISCOVERABLE));
    solrDocument.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonList(SolrFieldNames.SALES_CATALOG));
    solrDocument.setField(SolrFieldNames.WHOLESALE_PRICE_ACTIVATED, true);
    solrDocument.setField(SolrFieldNames.TOTAL_SCORE, 90.0);
    solrDocument.setField(SolrFieldNames.PRISTINE_ID, "PRISTINE_ID");
    ProductAndItemSolr productAndItemSolr = CommonUtil.toProductAndItemSolr(solrDocument);
    assertEquals(SolrFieldNames.ID, productAndItemSolr.getId());
    assertEquals(SolrFieldNames.BRAND, productAndItemSolr.getBrand());
    assertEquals(SolrFieldNames.ITEM_CODE, productAndItemSolr.getItemCode());
    assertEquals(90.0, productAndItemSolr.getProductScoreTotal(), 0);
    assertTrue(productAndItemSolr.getWholesalePriceActivated());
  }

  @Test
  public void toOfflineItemIdTest() {
    String result = CommonUtil.toOfflineItemId(ITEM_SKU, PICKUP_POINT_CODE);
    assertEquals(result, ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_CODE);
  }

  @Test
  public void toProductAndItemWithWholesalePriceActivatedNullSolr() {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.setField(SolrFieldNames.ID, SolrFieldNames.ID);
    solrDocument.setField(SolrFieldNames.TICKET_TEMPLATE_CODE, SolrFieldNames.TICKET_TEMPLATE_CODE);
    solrDocument.setField(SolrFieldNames.CNC_ACTIVE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.OFFLINE_PRICES, Collections.singletonList(SolrFieldNames.OFFER_PRICE));
    solrDocument.setField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocument.setField(SolrFieldNames.ITEM_IMAGES, Collections.singletonList(SolrFieldNames.ITEM_IMAGES));
    solrDocument.setField(SolrFieldNames.IS_SYNCHRONIZED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MARK_FOR_DELETE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.IS_LATE_FULFILLMENT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.IS_ARCHIVED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.TRADING_PRODUCT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocument.setField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    solrDocument.setField(SolrFieldNames.PRODUCT_CODE, SolrFieldNames.PRODUCT_CODE);
    solrDocument.setField(SolrFieldNames.ITEM_NAME, SolrFieldNames.ITEM_NAME);
    solrDocument.setField(SolrFieldNames.ITEM_CODE, SolrFieldNames.ITEM_CODE);
    solrDocument.setField(SolrFieldNames.ITEM_SKU, SolrFieldNames.ITEM_SKU);
    solrDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.PROMO_BUNDLING, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MERCHANT_SKU, SolrFieldNames.MERCHANT_SKU);
    solrDocument.setField(SolrFieldNames.PRODUCT_TYPE, SolrFieldNames.PRODUCT_TYPE);
    solrDocument.setField(SolrFieldNames.MERCHANT_CODE, SolrFieldNames.MERCHANT_CODE);
    solrDocument.setField(SolrFieldNames.MASTER_CATALOG, SolrFieldNames.MASTER_CATALOG);
    solrDocument.setField(SolrFieldNames.PICKUP_POINT_CODE, SolrFieldNames.PICKUP_POINT_CODE);
    solrDocument.setField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    solrDocument.setField(SolrFieldNames.PRODUCT_CATENTRY_ID, SolrFieldNames.PRODUCT_CATENTRY_ID);
    solrDocument.setField(SolrFieldNames.STORE_ID, SolrFieldNames.STORE_ID);
    solrDocument.setField(SolrFieldNames.CREATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.UPDATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.OFFER_PRICE, Collections.singletonList(SolrFieldNames.OFFER_PRICE));
    solrDocument.setField(SolrFieldNames.LIST_PRICE, Collections.singletonList(SolrFieldNames.LIST_PRICE));
    solrDocument.setField(SolrFieldNames.BUYABLE, Collections.singletonList(SolrFieldNames.BUYABLE));
    solrDocument.setField(SolrFieldNames.DISCOVERABLE, Collections.singletonList(SolrFieldNames.DISCOVERABLE));
    solrDocument.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonList(SolrFieldNames.SALES_CATALOG));
    solrDocument.setField(SolrFieldNames.WHOLESALE_PRICE_ACTIVATED, null);
    solrDocument.setField(SolrFieldNames.FREE_SAMPLE, Boolean.TRUE);
    ProductAndItemSolr productAndItemSolr = CommonUtil.toProductAndItemSolr(solrDocument);
    assertEquals(SolrFieldNames.ID, productAndItemSolr.getId());
    assertEquals(SolrFieldNames.BRAND, productAndItemSolr.getBrand());
    assertEquals(SolrFieldNames.ITEM_CODE, productAndItemSolr.getItemCode());
    assertNull(productAndItemSolr.getWholesalePriceActivated());
  }

  @Test
  public void toProductAndItemWithAllNullSolr() {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.setField(SolrFieldNames.ID, SolrFieldNames.ID);
    ProductAndItemSolr productAndItemSolr = CommonUtil.toProductAndItemSolr(solrDocument);
    assertEquals(SolrFieldNames.ID, productAndItemSolr.getId());
    assertNull(productAndItemSolr.getWholesalePriceActivated());
  }

  @Test
  public void toSolrInputDocumentFromSolrDocument() {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.setField(SolrFieldNames.ID, SolrFieldNames.ID);
    solrDocument.setField(SolrFieldNames.TICKET_TEMPLATE_CODE, SolrFieldNames.TICKET_TEMPLATE_CODE);
    solrDocument.setField(SolrFieldNames.CNC_ACTIVE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.OFFLINE_PRICES, Collections.singletonList(SolrFieldNames.OFFER_PRICE));
    solrDocument.setField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocument.setField(SolrFieldNames.ITEM_IMAGES, Collections.singletonList(SolrFieldNames.ITEM_IMAGES));
    solrDocument.setField(SolrFieldNames.IS_SYNCHRONIZED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MARK_FOR_DELETE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.IS_LATE_FULFILLMENT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.IS_ARCHIVED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.TRADING_PRODUCT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocument.setField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    solrDocument.setField(SolrFieldNames.PRODUCT_CODE, SolrFieldNames.PRODUCT_CODE);
    solrDocument.setField(SolrFieldNames.ITEM_NAME, SolrFieldNames.ITEM_NAME);
    solrDocument.setField(SolrFieldNames.ITEM_CODE, SolrFieldNames.ITEM_CODE);
    solrDocument.setField(SolrFieldNames.ITEM_SKU, SolrFieldNames.ITEM_SKU);
    solrDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.PROMO_BUNDLING, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MERCHANT_SKU, SolrFieldNames.MERCHANT_SKU);
    solrDocument.setField(SolrFieldNames.PRODUCT_TYPE, SolrFieldNames.PRODUCT_TYPE);
    solrDocument.setField(SolrFieldNames.MERCHANT_CODE, SolrFieldNames.MERCHANT_CODE);
    solrDocument.setField(SolrFieldNames.MASTER_CATALOG, SolrFieldNames.MASTER_CATALOG);
    solrDocument.setField(SolrFieldNames.PICKUP_POINT_CODE, SolrFieldNames.PICKUP_POINT_CODE);
    solrDocument.setField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    solrDocument.setField(SolrFieldNames.PRODUCT_CATENTRY_ID, SolrFieldNames.PRODUCT_CATENTRY_ID);
    solrDocument.setField(SolrFieldNames.STORE_ID, SolrFieldNames.STORE_ID);
    solrDocument.setField(SolrFieldNames.CREATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.UPDATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.OFFER_PRICE, Collections.singletonList(SolrFieldNames.OFFER_PRICE));
    solrDocument.setField(SolrFieldNames.LIST_PRICE, Collections.singletonList(SolrFieldNames.LIST_PRICE));
    solrDocument.setField(SolrFieldNames.BUYABLE, Collections.singletonList(SolrFieldNames.BUYABLE));
    solrDocument.setField(SolrFieldNames.DISCOVERABLE, Collections.singletonList(SolrFieldNames.DISCOVERABLE));
    solrDocument.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonList(SolrFieldNames.SALES_CATALOG));
    solrDocument.setField(SolrFieldNames.PRISTINE_ID, SolrFieldNames.PRISTINE_ID);
    solrDocument.setField(SolrFieldNames.FREE_SAMPLE, Boolean.TRUE);
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocumentFromSolrDocument(solrDocument);
    assertEquals(SolrFieldNames.ID, solrInputDocument.getFieldValue(SolrFieldNames.ID));
    assertEquals(SolrFieldNames.BRAND, solrInputDocument.getFieldValue(SolrFieldNames.BRAND));
    assertEquals(SolrFieldNames.ITEM_CODE, solrInputDocument.getFieldValue(SolrFieldNames.ITEM_CODE));
  }

  @Test
  public void toSolrInputDocumentFromSolrDocumentWithTotalScoreTest() {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.setField(SolrFieldNames.ID, SolrFieldNames.ID);
    solrDocument.setField(SolrFieldNames.TICKET_TEMPLATE_CODE, SolrFieldNames.TICKET_TEMPLATE_CODE);
    solrDocument.setField(SolrFieldNames.CNC_ACTIVE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.OFFLINE_PRICES, Collections.singletonList(SolrFieldNames.OFFER_PRICE));
    solrDocument.setField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocument.setField(SolrFieldNames.ITEM_IMAGES, Collections.singletonList(SolrFieldNames.ITEM_IMAGES));
    solrDocument.setField(SolrFieldNames.IS_SYNCHRONIZED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MARK_FOR_DELETE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.IS_LATE_FULFILLMENT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.IS_ARCHIVED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.TRADING_PRODUCT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocument.setField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    solrDocument.setField(SolrFieldNames.PRODUCT_CODE, SolrFieldNames.PRODUCT_CODE);
    solrDocument.setField(SolrFieldNames.ITEM_NAME, SolrFieldNames.ITEM_NAME);
    solrDocument.setField(SolrFieldNames.ITEM_CODE, SolrFieldNames.ITEM_CODE);
    solrDocument.setField(SolrFieldNames.ITEM_SKU, SolrFieldNames.ITEM_SKU);
    solrDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.PROMO_BUNDLING, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MERCHANT_SKU, SolrFieldNames.MERCHANT_SKU);
    solrDocument.setField(SolrFieldNames.PRODUCT_TYPE, SolrFieldNames.PRODUCT_TYPE);
    solrDocument.setField(SolrFieldNames.MERCHANT_CODE, SolrFieldNames.MERCHANT_CODE);
    solrDocument.setField(SolrFieldNames.MASTER_CATALOG, SolrFieldNames.MASTER_CATALOG);
    solrDocument.setField(SolrFieldNames.PICKUP_POINT_CODE, SolrFieldNames.PICKUP_POINT_CODE);
    solrDocument.setField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    solrDocument.setField(SolrFieldNames.PRODUCT_CATENTRY_ID, SolrFieldNames.PRODUCT_CATENTRY_ID);
    solrDocument.setField(SolrFieldNames.STORE_ID, SolrFieldNames.STORE_ID);
    solrDocument.setField(SolrFieldNames.CREATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.UPDATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.OFFER_PRICE, Collections.singletonList(SolrFieldNames.OFFER_PRICE));
    solrDocument.setField(SolrFieldNames.LIST_PRICE, Collections.singletonList(SolrFieldNames.LIST_PRICE));
    solrDocument.setField(SolrFieldNames.BUYABLE, Collections.singletonList(SolrFieldNames.BUYABLE));
    solrDocument.setField(SolrFieldNames.DISCOVERABLE, Collections.singletonList(SolrFieldNames.DISCOVERABLE));
    solrDocument.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonList(SolrFieldNames.SALES_CATALOG));
    solrDocument.setField(SolrFieldNames.TOTAL_SCORE, 80.0);
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocumentFromSolrDocument(solrDocument);
    assertEquals(80.0, solrInputDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE));
  }

  @Test
  public void toSolrInputDocumentFromSolrDocumentAllNull() {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.setField(SolrFieldNames.ID, SolrFieldNames.ID);
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocumentFromSolrDocument(solrDocument);
    assertEquals(SolrFieldNames.ID, solrInputDocument.getFieldValue(SolrFieldNames.ID));
    assertNull(solrInputDocument.getFieldValue(SolrFieldNames.BRAND));
  }

  @Test
  public void toSolrInputDocumentFromSolrDocumentL3() {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocument.addField(SolrFieldNames.ITEM_IMAGES, Arrays.asList(ITEM_IMAGE));
    solrDocument.addField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    solrDocument.addField(SolrFieldNames.MASTER_CATALOG, SolrFieldNames.MASTER_CATALOG);
    solrDocument.setField(SolrFieldNames.IS_SYNCHRONIZED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MARK_FOR_DELETE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.PRODUCT_CODE, SolrFieldNames.PRODUCT_CODE);
    solrDocument.setField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    solrDocument.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonList(SolrFieldNames.SALES_CATALOG));
    solrDocument.setField(SolrFieldNames.MERCHANT_CODE, SolrFieldNames.MERCHANT_CODE);
    solrDocument.setField(SolrFieldNames.IS_SUSPENDED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.STORE_ID, SolrFieldNames.STORE_ID);
    solrDocument.setField(SolrFieldNames.CREATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.UPDATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.FREE_SAMPLE, Boolean.FALSE);
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocumentL3(solrDocument, productAndItemsVO);
    assertEquals(SolrFieldNames.BRAND, solrInputDocument.getFieldValue(SolrFieldNames.BRAND));
    assertEquals(SolrFieldNames.SALES_CATALOG, solrInputDocument.getFieldValue(SolrFieldNames.SALES_CATALOG));
    assertEquals(IMAGE, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_MAIN_IMAGE));
    assertTrue(CollectionUtils.isEmpty((List) solrInputDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODES)));
  }

  @Test
  public void toSolrInputDocumentFromSolrDocumentL3WithTotalScore() {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocument.addField(SolrFieldNames.ITEM_IMAGES, Arrays.asList(ITEM_IMAGE));
    solrDocument.addField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    solrDocument.addField(SolrFieldNames.MASTER_CATALOG, SolrFieldNames.MASTER_CATALOG);
    solrDocument.setField(SolrFieldNames.IS_SYNCHRONIZED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MARK_FOR_DELETE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.PRODUCT_CODE, SolrFieldNames.PRODUCT_CODE);
    solrDocument.setField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    solrDocument.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonList(SolrFieldNames.SALES_CATALOG));
    solrDocument.setField(SolrFieldNames.MERCHANT_CODE, SolrFieldNames.MERCHANT_CODE);
    solrDocument.setField(SolrFieldNames.IS_SUSPENDED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.STORE_ID, SolrFieldNames.STORE_ID);
    solrDocument.setField(SolrFieldNames.CREATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.UPDATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.TOTAL_SCORE, 80.0);
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocumentL3(solrDocument, productAndItemsVO);
    assertEquals(80.0, solrInputDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE));
    assertTrue(CollectionUtils.isEmpty((List) solrInputDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODES)));
  }

  @Test
  public void toSolrInputDocumentFromSolrDocumentL3AllNull() {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocumentL3(solrDocument, productAndItemsVO);
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertTrue(CollectionUtils.isEmpty((List) solrInputDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODES)));
  }

  @Test
  public void updateMasterDataWithDPCMasterDataTest() {
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    MasterDataProduct defaultMasterDataProduct = new MasterDataProduct();
    defaultMasterDataProduct.setDescription(DESCRIPTION);
    defaultMasterDataProduct.setLongDescription(LONG_DESCRIPTION);
    CommonUtil.updateMasterDataWithDPCMasterData(masterDataProduct, defaultMasterDataProduct);
    assertEquals(DESCRIPTION, masterDataProduct.getDescription());
    assertEquals(LONG_DESCRIPTION, masterDataProduct.getLongDescription());
  }

  @Test
  public void settingPriceUpdatedDateTest() {
    item.getPrice().forEach(priceL4 -> {
      priceL4.setChannel(DEFAULT_CHANNEL);
      priceL4.setListPrice(1.0);
      priceL4.setOfferPrice(1.0);
    });
    itemPickupPoint.getPrice().forEach(priceL5 -> {
      priceL5.setChannel(DEFAULT_CHANNEL);
      priceL5.setOfferPrice(2.0);
      priceL5.setListPrice(2.0);
    });
    CommonUtil.settingPriceUpdatedDate(item, itemPickupPoint, DEFAULT_CHANNEL);
  }

  @Test
  public void settingPriceUpdatedDateOfferPriceSameTest() {
    item.getPrice().forEach(priceL4 -> {
      priceL4.setChannel(DEFAULT_CHANNEL);
      priceL4.setListPrice(1.0);
      priceL4.setOfferPrice(2.0);
    });
    itemPickupPoint.getPrice().forEach(priceL5 -> {
      priceL5.setChannel(DEFAULT_CHANNEL);
      priceL5.setOfferPrice(2.0);
      priceL5.setListPrice(2.0);
    });
    CommonUtil.settingPriceUpdatedDate(item, itemPickupPoint, DEFAULT_CHANNEL);
  }

  @Test
  public void settingPriceUpdatedDateListPriceSameTest() {
    item.getPrice().forEach(priceL4 -> {
      priceL4.setChannel(DEFAULT_CHANNEL);
      priceL4.setListPrice(2.0);
      priceL4.setOfferPrice(2.0);
    });
    itemPickupPoint.getPrice().forEach(priceL5 -> {
      priceL5.setChannel(DEFAULT_CHANNEL);
      priceL5.setOfferPrice(2.0);
      priceL5.setListPrice(2.0);
    });
    CommonUtil.settingPriceUpdatedDate(item, itemPickupPoint, DEFAULT_CHANNEL);
  }

  @Test
  public void settingPriceUpdatedDateListPriceSameOfferPriceDifferentTest() {
    item.getPrice().forEach(priceL4 -> {
      priceL4.setChannel(DEFAULT_CHANNEL);
      priceL4.setListPrice(2.0);
      priceL4.setOfferPrice(3.0);
    });
    itemPickupPoint.getPrice().forEach(priceL5 -> {
      priceL5.setChannel(DEFAULT_CHANNEL);
      priceL5.setOfferPrice(2.0);
      priceL5.setListPrice(2.0);
    });
    CommonUtil.settingPriceUpdatedDate(item, itemPickupPoint, DEFAULT_CHANNEL);
  }

  @Test
  public void settingPriceUpdatedDateL5OtherChannelTest() {
    item.getPrice().forEach(priceL4 -> {
      priceL4.setChannel(DEFAULT_CHANNEL);
      priceL4.setListPrice(2.0);
      priceL4.setOfferPrice(2.0);
    });
    itemPickupPoint.getPrice().forEach(priceL5 -> {
      priceL5.setChannel(B2B_SELLER_CHANNEL);
      priceL5.setOfferPrice(2.0);
      priceL5.setListPrice(2.0);
    });
    CommonUtil.settingPriceUpdatedDate(item, itemPickupPoint, DEFAULT_CHANNEL);
  }

  @Test
  public void settingPriceUpdatedDateL4OtherChannelTest() {
    item.getPrice().forEach(priceL4 -> {
      priceL4.setChannel(B2B_SELLER_CHANNEL);
      priceL4.setListPrice(2.0);
      priceL4.setOfferPrice(2.0);
    });
    itemPickupPoint.getPrice().forEach(priceL5 -> {
      priceL5.setChannel(B2B_SELLER_CHANNEL);
      priceL5.setOfferPrice(2.0);
      priceL5.setListPrice(2.0);
    });
    CommonUtil.settingPriceUpdatedDate(item, itemPickupPoint, DEFAULT_CHANNEL);
  }

  @Test
  public void getSolrInputDocumentForAtomicUpdateOnToggleArchiveItemAction() {
    SolrInputDocument solrInputFields =
        CommonUtil.getSolrInputDocumentForAtomicUpdateOnToggleArchiveItemAction(item, STRING_SOLR_DELIMITER);
    assertNotNull(solrInputFields);
    assertNotNull(solrInputFields.get(SolrFieldNames.IS_ARCHIVED));
    assertNotNull(solrInputFields.getFieldValues(SolrFieldNames.BUYABLE));
    assertNotNull(solrInputFields.getFieldValues(SolrFieldNames.DISCOVERABLE));
    assertNotNull(solrInputFields.getFieldValues(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE));
  }

  @Test
  public void getSolrInputDocumentOnMerchantPromoDiscountActivatedChangeTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(1))));
    itemPickupPoint.getPrice().stream().findFirst().get().setMerchantPromoDiscountPrice(discountPrice);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentOnMerchantPromoDiscountActivatedChange(Arrays.asList(itemPickupPoint));
    assertNotNull(solrInputDocument);
  }

  @Test
  public void getSolrInputDocumentOnMerchantPromoDiscountActivatedExpriedChangeTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(2))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    itemPickupPoint.getPrice().stream().findFirst().get().setMerchantPromoDiscountPrice(discountPrice);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentOnMerchantPromoDiscountActivatedChange(Arrays.asList(itemPickupPoint));
    assertNotNull(solrInputDocument);
  }

  @Test
  public void getSolrInputDocumentOnMerchantPromoDiscountActivatedFalseChangeTest() {
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentOnMerchantPromoDiscountActivatedChange(new ArrayList<>());
    assertNull(solrInputDocument);
  }

  @Test
  public void getSolrInputDocumentForAtomicUpdateOnItemViewConfigChanges() {
    SolrInputDocument solrInputFields =
        CommonUtil.getSolrInputDocumentForAtomicUpdateOnToggleArchiveItemAction(item, STRING_SOLR_DELIMITER);
    assertNotNull(solrInputFields);
    assertNotNull(solrInputFields.getFieldValues(SolrFieldNames.BUYABLE));
    assertNotNull(solrInputFields.getFieldValues(SolrFieldNames.DISCOVERABLE));
  }

  @Test
  public void getSolrInputDocumentForAtomicUpdateOnToggleArchiveItemAction1() {
    item.setArchived(false);
    SolrInputDocument solrInputFields =
        CommonUtil.getSolrInputDocumentForAtomicUpdateOnToggleArchiveItemAction(item, STRING_SOLR_DELIMITER);
    assertNotNull(solrInputFields);
    assertNotNull(solrInputFields.get(SolrFieldNames.IS_ARCHIVED));
    assertNull(solrInputFields.getFieldValues(SolrFieldNames.BUYABLE));
    assertNull(solrInputFields.getFieldValues(SolrFieldNames.DISCOVERABLE));
  }

  @Test
  public void getSolrDocumentListForAtomicUpdateOnUnsyncAction() {
    List<SolrInputDocument> solrInputDocuments =
        CommonUtil.getSolrDocumentListForAtomicUpdateOnSyncUnsyncAction(productAndItemsVO);
    assertNotNull(solrInputDocuments);
    assertNotNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.IS_SYNCHRONIZED));
    assertNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PRODUCT_NAME));
  }

  @Test
  public void getSolrDocumentListForAtomicUpdateOnUnsyncAction_Null() {
    productAndItemsVO.setItems(new ArrayList<>());
    List<SolrInputDocument> solrInputDocuments =
        CommonUtil.getSolrDocumentListForAtomicUpdateOnSyncUnsyncAction(productAndItemsVO);
    assertNull(solrInputDocuments);
  }

  @Test
  public void getSolrDocumentListForAtomicUpdateOnSyncAction() {
    item.setSynchronized(true);
    item.setPristineDataItem(new PristineDataItem());
    productAndItemsVO.setItems(Arrays.asList(item));
    List<SolrInputDocument> solrInputDocuments =
        CommonUtil.getSolrDocumentListForAtomicUpdateOnSyncUnsyncAction(productAndItemsVO);
    assertNotNull(solrInputDocuments);
    assertNotNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.IS_SYNCHRONIZED));
    assertNotNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PRODUCT_NAME));
  }

  @Test
  public void getSolrDocumentListForAtomicUpdateOnSyncActionWithProductScore() {
    item.setSynchronized(true);
    product.setProductScore(productScore);
    productAndItemsVO.setItems(Arrays.asList(item));
    productAndItemsVO.setProduct(product);
    List<SolrInputDocument> solrInputDocuments =
        CommonUtil.getSolrDocumentListForAtomicUpdateOnSyncUnsyncAction(productAndItemsVO);
    assertNotNull(solrInputDocuments);
    assertNotNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.IS_SYNCHRONIZED));
    assertNotNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.TOTAL_SCORE));
  }

  @Test
  public void getSolrInputDocumentForAtomicUpdateOnPriceChanges() {
    Price price = new Price();
    price.setChannel("DEFAULT");
    price.setOfferPrice(10);
    price.setListPrice(10);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(1000);
    price.setMerchantPromoDiscountPrice(discountPrice);
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    List<SolrInputDocument> solrInputFields =
        CommonUtil.getSolrInputDocumentForAtomicUpdateOnPriceChanges(Arrays.asList(item), STRING_SOLR_DELIMITER);
    assertNotNull(solrInputFields.get(0).get(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED));
    assertNotNull(solrInputFields.get(0).getFieldValues(SolrFieldNames.OFFER_PRICE));
    assertNotNull(solrInputFields.get(0).getFieldValues(SolrFieldNames.LIST_PRICE));
  }

  @Test
  public void getSolrInputDocumentForAtomicUpdateOnPriceChangesNullDiscountPrice() {
    Price price = new Price();
    price.setChannel("DEFAULT");
    price.setOfferPrice(10);
    price.setListPrice(10);
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    List<SolrInputDocument> solrInputFields =
        CommonUtil.getSolrInputDocumentForAtomicUpdateOnPriceChanges(Arrays.asList(item), STRING_SOLR_DELIMITER);
    assertNotNull(solrInputFields.get(0).get(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED));
    assertNotNull(solrInputFields.get(0).getFieldValues(SolrFieldNames.OFFER_PRICE));
    assertNotNull(solrInputFields.get(0).getFieldValues(SolrFieldNames.LIST_PRICE));
  }

  @Test
  public void getSolrInputDocumentsForAtomicUpdateOnMasterCatalogChanges() {
    List<SolrInputDocument> solrInputFields =
        CommonUtil.getSolrInputDocumentsForAtomicUpdateOnMasterCatalogChanges(productAndItemsVO.getProduct(),
            Arrays.asList(item), STRING_SOLR_DELIMITER);
    assertNotNull(solrInputFields);
    assertNotNull(solrInputFields.get(0).getFieldValue(SolrFieldNames.MASTER_CATALOG));
    assertNull(solrInputFields.get(0).getFieldValue(SolrFieldNames.SALES_CATALOG));
    assertNull(solrInputFields.get(0).getFieldValue(SolrFieldNames.MERCHANT_CODE));
    assertEquals(1, solrInputFields.size());
  }

  @Test
  public void getSolrInputDocumentsForAtomicUpdateOnMasterCatalogChangesTest() {
    Product product = productAndItemsVO.getProduct();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setMasterCatalog(product.getMasterCatalog());
    product.setMasterCatalog(null);
    product.setMasterDataProduct(masterDataProduct);
    List<SolrInputDocument> solrInputFields =
        CommonUtil.getSolrInputDocumentsForAtomicUpdateOnMasterCatalogChanges(productAndItemsVO.getProduct(),
            Arrays.asList(item), STRING_SOLR_DELIMITER);
    assertNotNull(solrInputFields);
    assertNotNull(solrInputFields.get(0).getFieldValue(SolrFieldNames.MASTER_CATALOG));
    assertNull(solrInputFields.get(0).getFieldValue(SolrFieldNames.SALES_CATALOG));
    assertEquals(1, solrInputFields.size());
  }

  @Test
  public void getSolrInputDocumentsForAtomicUpdateOnSalesCatalogChanges() {
    List<SolrInputDocument> solrInputFields =
        CommonUtil.getSolrInputDocumentsForAtomicUpdateOnSalesCatalogChanges(productAndItemsVO.getProduct(),
            Arrays.asList(item), STRING_SOLR_DELIMITER);
    assertNotNull(solrInputFields);
    assertNotNull(solrInputFields.get(0).getFieldValue(SolrFieldNames.SALES_CATALOG));
    assertNull(solrInputFields.get(0).getFieldValue(SolrFieldNames.MASTER_CATALOG));
    assertEquals(1, solrInputFields.size());
  }

  @Test
  public void checkForDisableUnSyncUpdateTest() {
    boolean response = CommonUtil.checkForDisableUnSyncUpdate(CATEGORY_CODE, CATEGORY_CODE);
    assertTrue(response);
  }

  @Test
  public void checkForDisableUnSyncUpdateTest_emptySystemParameterValue() {
    boolean response = CommonUtil.checkForDisableUnSyncUpdate(StringUtils.EMPTY, CATEGORY_CODE);
    assertFalse(response);
  }

  @Test
  public void checkForDisableUnSyncUpdateTest_UnSyncableCategory() {
    boolean response = CommonUtil.checkForDisableUnSyncUpdate(CATEGORY_CODE, CATEGORY_CODE_2);
    assertFalse(response);
  }

  @Test
  public void toProductSolrTest() {
    solrDocumentL3.setField(SolrFieldNames.PICKUP_POINT_CODES, Arrays.asList(PICKUP_POINT_CODE));
    solrDocumentL3.setField(SolrFieldNames.FREE_SAMPLE, Boolean.TRUE);
    solrDocumentL3.setField(SolrFieldNames.BUNDLE_PRODUCT, Boolean.TRUE);
    solrDocumentL3.setField(SolrFieldNames.SIZE_CHART_CODE, SIZE_CHART_CODE);
    ProductSolr productSolr = CommonUtil.toProductSolr(solrDocumentL3);
    assertEquals(SolrFieldNames.PRODUCT_NAME, productSolr.getProductName());
    assertEquals(SolrFieldNames.PRODUCT_SKU, productSolr.getProductSku());
    assertEquals(SolrFieldNames.PRODUCT_CODE, productSolr.getProductCode());
    assertEquals(SolrFieldNames.BRAND, productSolr.getBrand());
    assertEquals(SolrFieldNames.MERCHANT_CODE, productSolr.getMerchantCode());
    assertFalse(productSolr.isMarkForDelete());
    assertFalse(productSolr.isSynchronized());
    assertEquals(1000.0, productSolr.getMaximumSellingPrice().doubleValue(), 0);
    assertEquals(1000.0, productSolr.getMinimumSellingPrice().doubleValue(), 0);
    assertEquals(1000.0, productSolr.getMaximumListPrice().doubleValue(), 0);
    assertEquals(1000.0, productSolr.getMinimumListPrice().doubleValue(), 0);
    assertEquals(1000.0, productSolr.getMinimumListPrice().doubleValue(), 0);
    assertFalse(productSolr.getIsArchived().booleanValue());
    assertFalse(productSolr.getOff2OnChannelActive().booleanValue());
    assertEquals(ITEM_SKU, productSolr.getPromoItemSkus().get(0));
    assertTrue(productSolr.getInStock().booleanValue());
    assertEquals(ITEM_SKU, productSolr.getWholesaleItemSkus().get(0));
    assertEquals(5, productSolr.getVariantCount().intValue());
    assertEquals(5, productSolr.getL5Count());
    assertTrue(productSolr.getIsPreOrderActive().booleanValue());
    assertTrue(productSolr.isFreeSample());
    assertTrue(productSolr.isBundleProduct());
  }

  @Test
  public void toProductSolrAllNullTest() {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.setField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    ProductSolr productSolr = CommonUtil.toProductSolr(solrDocument);
    assertEquals(SolrFieldNames.PRODUCT_SKU, productSolr.getProductSku());
  }

  @Test
  public void toSolrInputDocumentTest() throws Exception {
    this.productSolr.setTradingProduct(true);
    productSolr.setMaximumSellingPrice(1000.0);
    productSolr.setMinimumSellingPrice(200.0);
    productSolr.setMaximumListPrice(1000.0);
    productSolr.setMinimumListPrice(200.0);
    productSolr.setIsPreOrderActive(false);
    productSolr.setInStock(true);
    productSolr.setOff2OnChannelActive(true);
    productSolr.setPickupPointCodes(Arrays.asList(PICKUP_POINT_CODE, PICKUP_POINT_CODE_1));
    productSolr.setSizeChartCode(SIZE_CHART_CODE);
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocument(productSolr);
    assertEquals(PRODUCT_SKU, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(80.0, solrInputDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE));
    assertEquals((new SimpleDateFormat("yyyy/MM/dd").parse("2021/01/20")),
        solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE));
    assertTrue(solrInputDocument.containsKey(SolrFieldNames.TRADING_PRODUCT));
    assertEquals(SIZE_CHART_CODE, solrInputDocument.getFieldValue(SolrFieldNames.SIZE_CHART_CODE));
  }

  @Test
  public void toSolrInputDocumentWithTotalScoreTest() {
    productSolr.setProductScoreTotal(80.0);
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocument(productSolr);
    assertEquals(80.0, solrInputDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE));
  }

  @Test
  public void getFieldsAndValuesForMasterCatalogAtomicUpdateL3CollectionTest() {
    Map<String, Object> fieldsAndValues =
        CommonUtil.getFieldsAndValuesForMasterCatalogAtomicUpdateL3Collection(productAndItemsVO.getProduct(),
            STRING_SOLR_DELIMITER);
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.MASTER_CATALOG)).get(SolrConstants.SET_CLAUSE),
        CODE + STRING_SOLR_DELIMITER + CODE);
  }

  @Test
  public void getFieldsAndValuesForMasterCatalogAtomicUpdateL3CollectionMasterDataNullTest() {
    productAndItemsVO.getProduct().setMasterDataProduct(new MasterDataProduct());
    productAndItemsVO.getProduct().getMasterDataProduct()
        .setMasterCatalog(productAndItemsVO.getProduct().getMasterCatalog());
    productAndItemsVO.getProduct().setMasterCatalog(null);
    Map<String, Object> fieldsAndValues =
        CommonUtil.getFieldsAndValuesForMasterCatalogAtomicUpdateL3Collection(productAndItemsVO.getProduct(),
            STRING_SOLR_DELIMITER);
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.MASTER_CATALOG)).get(SolrConstants.SET_CLAUSE),
        CODE + STRING_SOLR_DELIMITER + CODE);
  }

  @Test
  public void getSolrInputDocumentForMasterCatalogAtomicUpdateL3CollectionTest() {
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentForMasterCatalogAtomicUpdateL3Collection(productAndItemsVO.getProduct(),
            STRING_SOLR_DELIMITER);
    assertEquals(solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU), PRODUCT_SKU);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.MASTER_CATALOG)).get(SolrConstants.SET_CLAUSE),
        CODE + STRING_SOLR_DELIMITER + CODE);
  }

  @Test
  public void getSolrInputDocumentForProductAndItemSolrFlagsUpdateTest() {
    Map<String, Boolean> archiveItemsMap = new HashedMap();
    productAndItemSolr.setItemSku(ITEM_SKU);
    productAndItemSolr.setMerchantCode(MERCHANT_CODE);
    archiveItemsMap.put(ITEM_SKU, true);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentForProductAndItemSolrFlagsUpdate(productAndItemSolr, Boolean.TRUE,
            archiveItemsMap);
    assertEquals(solrInputDocument.getFieldValue(SolrFieldNames.ID), ITEM_SKU);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_SUSPENDED)).get(SolrConstants.SET_CLAUSE),
        true);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED)).get(SolrConstants.SET_CLAUSE),
        true);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.MARK_FOR_DELETE)).get(SolrConstants.SET_CLAUSE),
        true);
    assertEquals(solrInputDocument.getFieldValue(SolrFieldNames.MERCHANT_CODE), MERCHANT_CODE);
  }

  @Test
  public void getSolrInputDocumentForProductSolrFlagsUpdateTest() {
    productSolr.setProductSku(PRODUCT_SKU);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentForProductSolrFlagsUpdate(productSolr, true, true);
    assertEquals(solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU), PRODUCT_SKU);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_SUSPENDED)).get(SolrConstants.SET_CLAUSE),
        true);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.MARK_FOR_DELETE)).get(SolrConstants.SET_CLAUSE),
        true);
  }

  @Test
  public void getFieldsAndValuesForProductSolrFlagsUpdateTest() {
    Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesForProductSolrFlagsUpdate(true, true);
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.IS_SUSPENDED)).get(SolrConstants.SET_CLAUSE), true);
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.MARK_FOR_DELETE)).get(SolrConstants.SET_CLAUSE), true);
  }

  @Test
  public void getSolrInputDocumentsForAtomicUpdateOnProductSalesCatalogChangesTest() throws Exception {
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentsForAtomicUpdateOnProductSalesCatalogChanges(productAndItemsVO.getProduct(),
            STRING_SOLR_DELIMITER);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE)).get(
        SolrConstants.SET_CLAUSE), (new SimpleDateFormat("yyyy/MM/dd").parse("2021/01/20")));
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.SALES_CATALOG)).get(SolrConstants.SET_CLAUSE),
        Arrays.asList(SALES_CATALOG + STRING_SOLR_DELIMITER + CODE));
  }

  @Test
  public void getFieldsAndValuesForAtomicUpdateOnProductSalesCatalogChangesTest() throws Exception {
    Map<String, Object> fieldsAndValues =
        CommonUtil.getFieldsAndValuesForAtomicUpdateOnProductSalesCatalogChanges(productAndItemsVO.getProduct(),
            STRING_SOLR_DELIMITER);
    assertEquals(fieldsAndValues.get(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE),
        new SimpleDateFormat("yyyy/MM/dd").parse("2021/01/20"));
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.SALES_CATALOG)).get(SolrConstants.SET_CLAUSE),
        Arrays.asList(SALES_CATALOG + STRING_SOLR_DELIMITER + CODE));
  }

  @Test
  public void getFieldsAndValuesForProductAtomicUpdateOnSyncUnsyncActionTestSyncTrueAndProductScore() {
    productAndItemsVO.getProduct().setSynchronized(true);
    productAndItemsVO.getProduct().setProductScore(productScore);
    Map<String, Object> fieldsAndValues =
        CommonUtil.getFieldsAndValuesForProductAtomicUpdateOnSyncUnsyncAction(productAndItemsVO.getProduct());
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.IS_SYNCHRONIZED)).get(SolrConstants.SET_CLAUSE), true);
    assertNotNull(fieldsAndValues.get(SolrFieldNames.TOTAL_SCORE));
  }

  @Test
  public void getFieldsAndValuesForProductAtomicUpdateOnSyncUnsyncActionTestSyncTrue() {
    productAndItemsVO.getProduct().setSynchronized(true);
    Map<String, Object> fieldsAndValues =
        CommonUtil.getFieldsAndValuesForProductAtomicUpdateOnSyncUnsyncAction(productAndItemsVO.getProduct());
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.IS_SYNCHRONIZED)).get(SolrConstants.SET_CLAUSE), true);
  }

  @Test
  public void getFieldsAndValuesForProductAtomicUpdateOnSyncUnsyncActionTest() {
    Map<String, Object> fieldsAndValues =
        CommonUtil.getFieldsAndValuesForProductAtomicUpdateOnSyncUnsyncAction(productAndItemsVO.getProduct());
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.IS_SYNCHRONIZED)).get(SolrConstants.SET_CLAUSE), false);
  }

  @Test
  public void getSolrDocumentListForProductAtomicUpdateOnSyncUnsyncActionTest() {
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrDocumentListForProductAtomicUpdateOnSyncUnsyncAction(productAndItemsVO.getProduct());
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_SYNCHRONIZED)).get(SolrConstants.SET_CLAUSE),
        false);
  }

  @Test
  public void getSolrDocumentListForProductAtomicUpdateOnSyncUnsyncActionTestSyncTrue() {
    productAndItemsVO.getProduct().setSynchronized(true);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrDocumentListForProductAtomicUpdateOnSyncUnsyncAction(productAndItemsVO.getProduct());
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_SYNCHRONIZED)).get(SolrConstants.SET_CLAUSE),
        true);
  }

  @Test
  public void getSolrDocumentListForProductAtomicUpdateOnSyncUnsyncActionTestSyncTrueAndProductScore() {
    productAndItemsVO.getProduct().setSynchronized(true);
    productAndItemsVO.getProduct().setProductScore(productScore);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrDocumentListForProductAtomicUpdateOnSyncUnsyncAction(productAndItemsVO.getProduct());
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_SYNCHRONIZED)).get(SolrConstants.SET_CLAUSE),
        true);
    assertNotNull(solrInputDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE));
  }

  @Test
  public void toMerchantPromoDiscountChangeEventTest() throws Exception {
    MerchantPromoDiscountChangeEvent merchantPromoDiscountChangeEvent =
        CommonUtil.toMerchantPromoDiscountChangeEvent(item1);
    assertEquals(ITEM_SKU, merchantPromoDiscountChangeEvent.getItemSku());
    assertEquals(MERCHANT_CODE, merchantPromoDiscountChangeEvent.getMerchantCode());
    assertEquals(PRODUCT_SKU, merchantPromoDiscountChangeEvent.getProductSku());
  }

  @Test
  public void constructMasterCatalogTest() {
    MasterCatalog masterCatalog = CommonUtil.constructMasterCatalog(productCategoryResponse);
    assertEquals(MASTER_CATALOG, masterCatalog.getCatalogCode());
    assertEquals(CATEGORY_CODE, masterCatalog.getCategory().getCategoryCode());
    assertEquals(CATEGORY_CODE, masterCatalog.getCategory().getCatgroupId());
  }

  @Test
  public void checkProductsForForceReviewTest() {
    CommonUtil.checkProductsForForceReview(Arrays.asList(product));
  }

  @Test
  public void checkItemsForForceReviewTest() {
    CommonUtil.checkItemsForForceReview(Arrays.asList(item));
  }

  @Test
  public void checkProductsForForceReview_expectException() {
    product.setForceReview(true);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> CommonUtil.checkProductsForForceReview(Arrays.asList(product)));
  }

  @Test
  public void checkItemsForForceReview_expectException() {
    item.setForceReview(true);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> CommonUtil.checkItemsForForceReview(Arrays.asList(item)));
  }

  @Test
  public void generateAttributeScoreRequestsFromMasterDataAndSpecialAttributesTest() {
    setProductAttributeResponses(productDetailResponse);
    product.setProductSpecialAttributes(Collections.singletonList(productSpecialAttribute));
    List<AttributeScoreRequest> attributeScoreRequestList =
        CommonUtil.generateAttributeScoreRequestsFromMasterDataAndSpecialAttributes(productDetailResponse, product);
    assertEquals(ATTRIBUTE_CODE, attributeScoreRequestList.get(0).getAttributeCode());
    assertEquals(ATTRIBUTE_CODE_1, attributeScoreRequestList.get(1).getAttributeCode());
    assertEquals(ATTRIBUTE_CODE_2, attributeScoreRequestList.get(2).getAttributeCode());
    assertEquals(DESCRIPTIVE_ATTRIBUTE_VALUE, attributeScoreRequestList.get(0).getValues().get(0));
    assertEquals(PREDEFINED_ATTRIBUTE_VALUE, attributeScoreRequestList.get(1).getValues().get(0));
    assertEquals(ALLOWED_ATTRIBUTE_VALUE, attributeScoreRequestList.get(2).getValues().get(0));
  }


  @Test
  public void generateAttributeScoreRequestsFromMasterDataAndSpecialAttributesTestWithNullValue() {
    setProductAttributeResponses(productDetailResponse);
    productDetailResponse.getProductAttributeResponses().get(1).getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(null);
    product.setProductSpecialAttributes(Collections.singletonList(productSpecialAttribute));
    List<AttributeScoreRequest> attributeScoreRequestList =
        CommonUtil.generateAttributeScoreRequestsFromMasterDataAndSpecialAttributes(productDetailResponse, product);
    assertEquals(ATTRIBUTE_CODE, attributeScoreRequestList.get(0).getAttributeCode());
    assertEquals(ATTRIBUTE_CODE_1, attributeScoreRequestList.get(1).getAttributeCode());
    assertEquals(ATTRIBUTE_CODE_2, attributeScoreRequestList.get(2).getAttributeCode());
    assertEquals(DESCRIPTIVE_ATTRIBUTE_VALUE, attributeScoreRequestList.get(0).getValues().get(0));
    assertEquals(0, attributeScoreRequestList.get(1).getValues().size());
    assertEquals(ALLOWED_ATTRIBUTE_VALUE, attributeScoreRequestList.get(2).getValues().get(0));
  }

  private void setProductAttributeResponses(ProductDetailResponse productDetailResponse) {
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();

    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    attributeResponse.setName(ATTRIBUTE_CODE);
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValueResponse.setDescriptiveAttributeValueType(
        com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE);

    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse));
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);

    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    AttributeResponse attributeResponse1 = new AttributeResponse();
    ProductAttributeValueResponse productAttributeValueResponse1 = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();

    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse1.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse1.setName(ATTRIBUTE_CODE_1);
    predefinedAllowedAttributeValueResponse.setValue(PREDEFINED_ATTRIBUTE_VALUE);
    productAttributeValueResponse1.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse1.setDescriptiveAttributeValueType(
        com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.PREDEFINED);

    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse1));
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);

    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    AttributeResponse attributeResponse2 = new AttributeResponse();
    ProductAttributeValueResponse productAttributeValueResponse2 = new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();

    attributeResponse2.setAttributeCode(ATTRIBUTE_CODE_2);
    attributeResponse2.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE.name());
    attributeResponse2.setName(ATTRIBUTE_CODE_2);
    allowedAttributeValueResponse.setValue(ALLOWED_ATTRIBUTE_VALUE);
    productAttributeValueResponse2.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse2.setDescriptiveAttributeValueType(
        com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.NONE);

    productAttributeResponse2.setAttribute(attributeResponse2);
    productAttributeResponse2.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse2));
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse2);
  }

  @Test
  public void generateItemScoreRequestsWithoutImagesTest() {
    List<ItemScoreRequest> itemScoreRequestList =
        CommonUtil.generateItemScoreRequestsWithoutImages(Collections.singletonList(item));
    assertEquals(UPC_CODE, itemScoreRequestList.get(0).getUpcCode());
  }

  @Test
  public void generateItemScoreRequestsWithoutImagesTest_nullMasterData() {
    item.setMasterDataItem(null);
    List<ItemScoreRequest> itemScoreRequestList =
        CommonUtil.generateItemScoreRequestsWithoutImages(Collections.singletonList(item));
    assertTrue(CollectionUtils.isEmpty(itemScoreRequestList));
  }

  @Test
  public void generateAttributeScoreRequestsForUnsyncL3Test() {
    product.setMasterDataProduct(new MasterDataProduct());
    product.setProductSpecialAttributes(Collections.singletonList(productSpecialAttribute));
    setMasterDataAttributes(product.getMasterDataProduct(), DESCRIPTIVE_ATTRIBUTE_VALUE, PREDEFINED_ATTRIBUTE_VALUE,
        ALLOWED_ATTRIBUTE_VALUE);
    List<AttributeScoreRequest> attributeScoreRequestList =
        CommonUtil.generateAttributeScoreRequestsForUnsyncL3(product);
    assertEquals(ATTRIBUTE_CODE, attributeScoreRequestList.get(0).getAttributeCode());
    assertEquals(ATTRIBUTE_CODE_1, attributeScoreRequestList.get(1).getAttributeCode());
    assertEquals(ATTRIBUTE_CODE_2, attributeScoreRequestList.get(2).getAttributeCode());
    assertEquals(DESCRIPTIVE_ATTRIBUTE_VALUE, attributeScoreRequestList.get(0).getValues().get(0));
    assertEquals(PREDEFINED_ATTRIBUTE_VALUE, attributeScoreRequestList.get(1).getValues().get(0));
    assertEquals(ALLOWED_ATTRIBUTE_VALUE, attributeScoreRequestList.get(2).getValues().get(0));
  }

  private void setMasterDataAttributes(MasterDataProduct masterDataProduct, String descriptiveAttributeValue,
      String predefinedAttributeValue, String allowedAttributeValue) {
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());

    MasterDataProductAttribute masterDataProductAttribute = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();

    masterDataAttribute.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttribute.setAttributeName(ATTRIBUTE_CODE);
    masterDataProductAttributeValue.setDescriptiveAttributeValue(descriptiveAttributeValue);
    masterDataProductAttributeValue.setDescriptiveAttributeValueType(
        com.gdn.x.product.enums.DescriptiveAttributeValueType.SINGLE);

    masterDataProductAttribute.setMasterDataAttribute(masterDataAttribute);
    masterDataProductAttribute.setMasterDataProductAttributeValues(
        Collections.singletonList(masterDataProductAttributeValue));
    masterDataProduct.getMasterDataProductAttributes().add(masterDataProductAttribute);

    MasterDataProductAttribute masterDataProductAttribute1 = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute1 = new MasterDataAttribute();
    MasterDataProductAttributeValue masterDataProductAttributeValue1 = new MasterDataProductAttributeValue();
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();

    masterDataAttribute1.setAttributeCode(ATTRIBUTE_CODE_1);
    masterDataAttribute1.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    masterDataAttribute1.setAttributeName(ATTRIBUTE_CODE_1);
    predefinedAllowedAttributeValue.setValue(predefinedAttributeValue);
    masterDataProductAttributeValue1.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    masterDataProductAttributeValue1.setDescriptiveAttributeValueType(
        com.gdn.x.product.enums.DescriptiveAttributeValueType.PREDEFINED);

    masterDataProductAttribute1.setMasterDataAttribute(masterDataAttribute1);
    masterDataProductAttribute1.setMasterDataProductAttributeValues(
        Collections.singletonList(masterDataProductAttributeValue1));
    masterDataProduct.getMasterDataProductAttributes().add(masterDataProductAttribute1);

    MasterDataProductAttribute masterDataProductAttribute2 = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute2 = new MasterDataAttribute();
    MasterDataProductAttributeValue masterDataProductAttributeValue2 = new MasterDataProductAttributeValue();
    MasterDataAllowedAttributeValue masterDataAllowedAttributeValue2 = new MasterDataAllowedAttributeValue();

    masterDataAttribute2.setAttributeCode(ATTRIBUTE_CODE_2);
    masterDataAttribute2.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataAttribute2.setAttributeName(ATTRIBUTE_CODE_2);
    masterDataAllowedAttributeValue2.setValue(allowedAttributeValue);
    masterDataProductAttributeValue2.setAllowedAttributeValue(masterDataAllowedAttributeValue2);

    masterDataProductAttribute2.setMasterDataAttribute(masterDataAttribute2);
    masterDataProductAttribute2.setMasterDataProductAttributeValues(
        Collections.singletonList(masterDataProductAttributeValue2));
    masterDataProduct.getMasterDataProductAttributes().add(masterDataProductAttribute2);
  }

  @Test
  public void toMasterDataProductImageDTOTest() {
    List<MasterDataProductImageDTO> imageDTOS =
        CommonUtil.toMasterDataProductImageDTO(PRODUCT_CODE, Collections.singletonList(image));
    assertEquals(PRODUCT_CODE, imageDTOS.get(0).getProductCode());
    assertEquals(LOCATION_PATH, imageDTOS.get(0).getLocationPath());
    assertEquals(0, imageDTOS.get(0).getSequence());
    assertTrue(imageDTOS.get(0).isMainImage());
  }

  @Test
  public void toMasterDataProductImageActiveTrueDTOTest() {
    image.setActive(true);
    image.setEdited(true);
    image.setOriginalImage(true);
    List<MasterDataProductImageDTO> imageDTOS =
        CommonUtil.toMasterDataProductImageDTO(PRODUCT_CODE, Collections.singletonList(image));
    assertEquals(PRODUCT_CODE, imageDTOS.get(0).getProductCode());
    assertEquals(LOCATION_PATH, imageDTOS.get(0).getLocationPath());
    assertEquals(0, imageDTOS.get(0).getSequence());
    assertTrue(imageDTOS.get(0).isMainImage());
  }

  @Test
  public void toMasterDataProductImageActiveFalseDTOTest() {
    image.setActive(false);
    image.setEdited(true);
    image.setOriginalImage(true);
    List<MasterDataProductImageDTO> imageDTOS =
        CommonUtil.toMasterDataProductImageDTO(PRODUCT_CODE, Collections.singletonList(image));
    assertEquals(0, imageDTOS.size());
  }

  @Test
  public void toMasterDataProductImageOINullDTOTest() {
    image.setOriginalImage(null);
    List<MasterDataProductImageDTO> imageDTOS =
        CommonUtil.toMasterDataProductImageDTO(PRODUCT_CODE, Collections.singletonList(image));
    assertEquals(PRODUCT_CODE, imageDTOS.get(0).getProductCode());
    assertEquals(LOCATION_PATH, imageDTOS.get(0).getLocationPath());
    assertEquals(0, imageDTOS.get(0).getSequence());
    assertTrue(imageDTOS.get(0).isMainImage());
  }

  @Test
  public void toMasterDataProductImageOINonNullDTOTest() {
    image.setOriginalImage(false);
    List<MasterDataProductImageDTO> imageDTOS =
        CommonUtil.toMasterDataProductImageDTO(PRODUCT_CODE, Collections.singletonList(image));
    assertEquals(PRODUCT_CODE, imageDTOS.get(0).getProductCode());
    assertEquals(LOCATION_PATH, imageDTOS.get(0).getLocationPath());
    assertEquals(0, imageDTOS.get(0).getSequence());
    assertTrue(imageDTOS.get(0).isMainImage());
  }

  @Test
  public void toMasterDataProductImageOITrueDTOTest() {
    image.setOriginalImage(true);
    List<MasterDataProductImageDTO> imageDTOS =
        CommonUtil.toMasterDataProductImageDTO(PRODUCT_CODE, Collections.singletonList(image));
    assertEquals(0, imageDTOS.size());
  }

  @Test
  public void toMasterDataProductImageEmptyImagesDTOTest() {
    List<MasterDataProductImageDTO> imageDTOS =
        CommonUtil.toMasterDataProductImageDTO(PRODUCT_CODE, Collections.emptyList());
    assertEquals(0, imageDTOS.size());
  }

  @Test
  public void convertItemImagesToMasterDataProductImageDTOTest() {
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setLocationPath(LOCATION_PATH);
    List<MasterDataProductImageDTO> imageDTOS =
        CommonUtil.convertItemImagesToMasterDataProductImageDTO(Collections.singletonList(masterDataItemImage));
    assertEquals(LOCATION_PATH, imageDTOS.get(0).getLocationPath());
  }

  @Test
  public void convertItemImagesToMasterDataProductImageDTOEmptyListTest() {
    List<MasterDataProductImageDTO> imageDTOS =
        CommonUtil.convertItemImagesToMasterDataProductImageDTO(Collections.emptyList());
    assertEquals(0, imageDTOS.size());
  }

  @Test
  public void getProductScoreRequestTest() throws Exception {
    product.setProductCode(PRODUCT_CODE);
    product.setProductSpecialAttributes(new ArrayList<>());
    product.getProductSpecialAttributes().add(new ProductSpecialAttribute(ATTRIBUTE_CODE, PRODUCT_NAME, "value"));
    ProductScoreRequest productScoreRequest = CommonUtil.getProductScoreRequest(product, productDetailResponse);
    assertNotNull(productScoreRequest);
  }

  @Test
  public void getProductScoreRequestImageEditedTrueTest() throws Exception {
    product.setProductCode(PRODUCT_CODE);
    product.setProductSpecialAttributes(new ArrayList<>());
    productDetailResponse.getProductItemResponses().stream().findFirst().get().getImages().get(0).setEdited(true);
    product.getProductSpecialAttributes().add(new ProductSpecialAttribute(ATTRIBUTE_CODE, PRODUCT_NAME, "value"));
    ProductScoreRequest productScoreRequest = CommonUtil.getProductScoreRequest(product, productDetailResponse);
    assertNotNull(productScoreRequest);
  }

  @Test
  public void getProductScoreRequestNullPredefinedValueTest() throws Exception {
    product.setProductCode(PRODUCT_CODE);
    product.setProductSpecialAttributes(new ArrayList<>());
    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    AttributeResponse attributeResponse1 = new AttributeResponse();
    ProductAttributeValueResponse productAttributeValueResponse1 = new ProductAttributeValueResponse();
    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse1.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse1.setName(ATTRIBUTE_CODE_1);
    productAttributeValueResponse1.setPredefinedAllowedAttributeValue(null);
    productAttributeValueResponse1.setDescriptiveAttributeValueType(
        com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.PREDEFINED);
    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse1));
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);
    product.getProductSpecialAttributes().add(new ProductSpecialAttribute(ATTRIBUTE_CODE, PRODUCT_NAME, "value"));
    ProductScoreRequest productScoreRequest = CommonUtil.getProductScoreRequest(product, productDetailResponse);
    assertNotNull(productScoreRequest);
  }

  @Test
  public void getProductScoreRequest_emptyCategoryTest() throws Exception {
    product.setProductCode(PRODUCT_CODE);
    product.setProductSpecialAttributes(new ArrayList<>());
    product.getProductSpecialAttributes().add(new ProductSpecialAttribute(ATTRIBUTE_CODE, PRODUCT_NAME, "value"));
    productDetailResponse.setProductCategoryResponses(new ArrayList<>());
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> CommonUtil.getProductScoreRequest(product, productDetailResponse));
  }

  @Test
  public void isEqualListTest() {
    List<String> list1 = Arrays.asList(BRAND, CODE, DESCRIPTION);
    boolean result = CommonUtil.isEqualList(list1, Collections.EMPTY_LIST);
    assertFalse(result);
  }

  @Test
  public void isEqualListTest2() {
    List<String> list1 = Arrays.asList(BRAND, CODE, DESCRIPTION);
    List<String> list2 = Arrays.asList(CODE, DESCRIPTION, BRAND);
    boolean result = CommonUtil.isEqualList(list1, list2);
    assertTrue(result);
  }

  @Test
  public void isEqualListTest3() {
    boolean result = CommonUtil.isEqualList(Collections.EMPTY_LIST, Collections.EMPTY_LIST);
    assertTrue(result);
  }

  @Test
  public void isEqualListTest4() {
    List<String> list1 = Arrays.asList(BRAND, CODE, DESCRIPTION);
    List<String> list2 = Arrays.asList(CODE, DESCRIPTION);
    boolean result = CommonUtil.isEqualList(list1, list2);
    assertFalse(result);
  }

  @Test
  public void getSolrInputDocumentFromProductSolrForAtomicUpdateOnProductPublishTest() {
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentFromProductSolrForAtomicUpdateOnProductPublish(productSolr, fieldValueObjectMap);
    assertEquals(solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU), PRODUCT_SKU);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.BRAND)).get(SolrConstants.SET_CLAUSE),
        NEW_BRAND);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_MAIN_IMAGE)).get(SolrConstants.SET_CLAUSE),
        NEW_PRODUCT_MAIN_IMAGE);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME)).get(SolrConstants.SET_CLAUSE),
        NEW_PRODUCT_NAME);
    assertNotNull(solrInputDocument.getFieldValue(SolrFieldNames.SALES_CATALOG));
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.MASTER_CATALOG)).get(SolrConstants.SET_CLAUSE),
        NEW_MASTER_CATALOG);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE)).get(SolrConstants.SET_CLAUSE),
        20.0);
  }

  @Test
  public void getSolrInputDocumentFromProductAnsItemSolrForAtomicUpdateOnProductPublishTest() {
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentFromProductAndItemSolrForAtomicUpdateOnProductPublish(productAndItemSolr,
            fieldValueObjectMap);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.BRAND)).get(SolrConstants.SET_CLAUSE),
        NEW_BRAND);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME)).get(SolrConstants.SET_CLAUSE),
        NEW_PRODUCT_NAME);
    assertNotNull(solrInputDocument.getFieldValue(SolrFieldNames.SALES_CATALOG));
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.MASTER_CATALOG)).get(SolrConstants.SET_CLAUSE),
        NEW_MASTER_CATALOG);
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE)).get(SolrConstants.SET_CLAUSE),
        20.0);
  }

  @Test
  public void checkIfSolrUpdateNeededForProductSolrTest() {
    Map<String, FieldValueObject> result =
        CommonUtil.checkIfSolrUpdateNeededForProductSolr(productSolr, getProductSolr());
    assertTrue(result.size() == 6);
    assertEquals(NEW_BRAND, result.get(SolrFieldNames.BRAND).getNewValue());
    assertEquals(NEW_PRODUCT_NAME, result.get(SolrFieldNames.PRODUCT_NAME).getNewValue());
    assertEquals(NEW_MASTER_CATALOG, result.get(SolrFieldNames.MASTER_CATALOG).getNewValue());
    assertEquals(NEW_SALES_CATALOG, result.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(0));
    assertEquals(NEW_PRODUCT_MAIN_IMAGE, result.get(SolrFieldNames.PRODUCT_MAIN_IMAGE).getNewValue());
    assertEquals(NEW_PRODUCT_SCORE, Double.parseDouble(result.get(SolrFieldNames.TOTAL_SCORE).getNewValue()), 0.0);
  }


  private ProductSolr getProductSolr() {
    ProductSolr productSolr = new ProductSolr();
    productSolr.setProductName(NEW_PRODUCT_NAME);
    productSolr.setBrand(NEW_BRAND);
    productSolr.setMasterCatalog(NEW_MASTER_CATALOG);
    productSolr.setSalesCatalog(Arrays.asList(NEW_SALES_CATALOG));
    productSolr.setProductMainImage(NEW_PRODUCT_MAIN_IMAGE);
    productSolr.setProductScoreTotal(NEW_PRODUCT_SCORE);
    return productSolr;
  }

  private ProductAndItemSolr getProductAndItemSolr() {
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setProductName(NEW_PRODUCT_NAME);
    productAndItemSolr.setItemName(NEW_ITEM_NAME);
    productAndItemSolr.setBrand(NEW_BRAND);
    productAndItemSolr.setProductType(NEW_PRODUCT_TYPE);
    productAndItemSolr.setMasterCatalog(NEW_MASTER_CATALOG);
    productAndItemSolr.setSalesCatalog(Arrays.asList(NEW_SALES_CATALOG));
    productAndItemSolr.setItemImages(Arrays.asList(NEW_ITEM_IMAGE));
    productAndItemSolr.setProductScoreTotal(NEW_PRODUCT_SCORE);
    return productAndItemSolr;
  }

  @Test
  public void checkIfSolrUpdateNeededForProductAndItemSolrTest() {
    Map<String, FieldValueObject> result =
        CommonUtil.checkIfSolrUpdateNeededForProductAndItemSolr(productAndItemSolr, getProductAndItemSolr());
    assertTrue(result.size() == 8);
    assertEquals(NEW_BRAND, result.get(SolrFieldNames.BRAND).getNewValue());
    assertEquals(NEW_PRODUCT_NAME, result.get(SolrFieldNames.PRODUCT_NAME).getNewValue());
    assertEquals(NEW_MASTER_CATALOG, result.get(SolrFieldNames.MASTER_CATALOG).getNewValue());
    assertEquals(NEW_SALES_CATALOG, result.get(SolrFieldNames.SALES_CATALOG).getNewListValues().get(0));
    assertEquals(NEW_ITEM_IMAGE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
    assertEquals(NEW_ITEM_NAME, result.get(SolrFieldNames.ITEM_NAME).getNewValue());
    assertEquals(NEW_PRODUCT_TYPE, result.get(SolrFieldNames.PRODUCT_TYPE).getNewValue());
    assertEquals(NEW_PRODUCT_SCORE, Double.parseDouble(result.get(SolrFieldNames.TOTAL_SCORE).getNewValue()), 0.0);
  }

  @Test
  public void getSolrInputDocumentFromProductAndItemSolrForAtomicUpdateOnProductScoreUpdatePublishTest() {
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentFromProductAndItemSolrForAtomicUpdateOnProductScoreUpdatePublish(
            productAndItemSolr, fieldValueObjectMap);
    Assertions.assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE)).get(SolrConstants.SET_CLAUSE), 20.0);
  }

  @Test
  public void getSolrInputDocumentFromProductAndItemSolrForAtomicUpdateOnProductScoreUpdatePublishNullTest() {
    fieldValueObjectMap.put(SolrFieldNames.TOTAL_SCORE, null);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentFromProductAndItemSolrForAtomicUpdateOnProductScoreUpdatePublish(
            productAndItemSolr, fieldValueObjectMap);
  }

  @Test
  public void getSolrInputDocumentFromProductSolrForAtomicUpdateOnProductScoreUpdateTest() {
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentFromProductSolrForAtomicUpdateOnProductScoreUpdate(productSolr,
            fieldValueObjectMap);
    assertEquals(solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU), PRODUCT_SKU);
    Assertions.assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE)).get(SolrConstants.SET_CLAUSE), 20.0);
  }

  @Test
  public void getSolrInputDocumentFromProductSolrForAtomicUpdateOnProductScoreUpdateNullTest() {
    fieldValueObjectMap.put(SolrFieldNames.TOTAL_SCORE, null);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentFromProductSolrForAtomicUpdateOnProductScoreUpdate(productSolr,
            fieldValueObjectMap);
    assertEquals(solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU), PRODUCT_SKU);
  }

  @Test
  public void generateAttributeScoreRequestsFromMasterDataAndSpecialAttributesTest_emptyMasterAttributes() {
    productDetailResponse.setProductAttributeResponses(Collections.EMPTY_LIST);
    product.setProductSpecialAttributes(Collections.EMPTY_LIST);
    List<AttributeScoreRequest> attributeScoreRequestList =
        CommonUtil.generateAttributeScoreRequestsFromMasterDataAndSpecialAttributes(productDetailResponse, product);
    assertTrue(CollectionUtils.isEmpty(attributeScoreRequestList));
  }

  @Test
  public void generateAttributeScoreRequestsFromMasterDataAndSpecialAttributesTest_emptyAndUnrecognizedAttributeType() {
    setProductAttributeResponses(productDetailResponse);
    productDetailResponse.getProductAttributeResponses().get(1).getAttribute().setAttributeType(StringUtils.EMPTY);
    productDetailResponse.getProductAttributeResponses().get(2).getAttribute().setAttributeType(ATTRIBUTE_CODE);
    product.setProductSpecialAttributes(Collections.singletonList(productSpecialAttribute));
    List<AttributeScoreRequest> attributeScoreRequestList =
        CommonUtil.generateAttributeScoreRequestsFromMasterDataAndSpecialAttributes(productDetailResponse, product);
    assertEquals(ATTRIBUTE_CODE, attributeScoreRequestList.get(0).getAttributeCode());
    assertEquals(DESCRIPTIVE_ATTRIBUTE_VALUE, attributeScoreRequestList.get(0).getValues().get(0));
  }

  @Test
  public void generateAttributeScoreRequestsForUnsyncL3Test_nullMasterData() {
    product.setMasterDataProduct(null);
    List<AttributeScoreRequest> attributeScoreRequestList =
        CommonUtil.generateAttributeScoreRequestsForUnsyncL3(product);
    assertTrue(CollectionUtils.isEmpty(attributeScoreRequestList));
  }

  @Test
  public void generateAttributeScoreRequestsForUnsyncL3Test_emptyAttributesList() {
    product.setMasterDataProduct(new MasterDataProduct());
    product.getMasterDataProduct().setMasterDataProductAttributes(Collections.EMPTY_LIST);
    List<AttributeScoreRequest> attributeScoreRequestList =
        CommonUtil.generateAttributeScoreRequestsForUnsyncL3(product);
    assertTrue(CollectionUtils.isEmpty(attributeScoreRequestList));
  }

  @Test
  public void generateAttributeScoreRequestsForUnsyncL3Test_emptyAttributeType() {
    product.setMasterDataProduct(new MasterDataProduct());
    product.setProductSpecialAttributes(Collections.singletonList(productSpecialAttribute));
    setMasterDataAttributes(product.getMasterDataProduct(), DESCRIPTIVE_ATTRIBUTE_VALUE, PREDEFINED_ATTRIBUTE_VALUE,
        ALLOWED_ATTRIBUTE_VALUE);
    product.getMasterDataProduct().getMasterDataProductAttributes().get(2).getMasterDataAttribute()
        .setAttributeType(null);
    List<AttributeScoreRequest> attributeScoreRequestList =
        CommonUtil.generateAttributeScoreRequestsForUnsyncL3(product);
    assertEquals(ATTRIBUTE_CODE, attributeScoreRequestList.get(0).getAttributeCode());
    assertEquals(ATTRIBUTE_CODE_1, attributeScoreRequestList.get(1).getAttributeCode());
    assertEquals(ATTRIBUTE_CODE_2, attributeScoreRequestList.get(2).getAttributeCode());
    assertEquals(DESCRIPTIVE_ATTRIBUTE_VALUE, attributeScoreRequestList.get(0).getValues().get(0));
    assertEquals(PREDEFINED_ATTRIBUTE_VALUE, attributeScoreRequestList.get(1).getValues().get(0));
  }

  @Test
  public void checkIfSolrUpdateNeededForProductAndItemSolrTest_updateNotRequired() {
    Map<String, FieldValueObject> result =
        CommonUtil.checkIfSolrUpdateNeededForProductAndItemSolr(productAndItemSolr, productAndItemSolr);
    assertTrue(result.size() == 0);
  }

  @Test
  public void checkIfSolrUpdateNeededForProductSolrTest_updateNotRequired() {
    Map<String, FieldValueObject> result = CommonUtil.checkIfSolrUpdateNeededForProductSolr(productSolr, productSolr);
    assertTrue(result.size() == 0);
  }

  @Test
  public void generateAttributeScoreRequestsForUnsyncL3Test_isSkuValueTrue() {
    product.setMasterDataProduct(new MasterDataProduct());
    product.setProductSpecialAttributes(Collections.singletonList(productSpecialAttribute));
    setMasterDataAttributes(product.getMasterDataProduct(), DESCRIPTIVE_ATTRIBUTE_VALUE, PREDEFINED_ATTRIBUTE_VALUE,
        ALLOWED_ATTRIBUTE_VALUE);
    product.getMasterDataProduct().getMasterDataProductAttributes().get(2).getMasterDataAttribute().setSkuValue(true);
    List<AttributeScoreRequest> attributeScoreRequestList =
        CommonUtil.generateAttributeScoreRequestsForUnsyncL3(product);
    assertEquals(ATTRIBUTE_CODE, attributeScoreRequestList.get(0).getAttributeCode());
    assertEquals(ATTRIBUTE_CODE_1, attributeScoreRequestList.get(1).getAttributeCode());
    assertEquals(DESCRIPTIVE_ATTRIBUTE_VALUE, attributeScoreRequestList.get(0).getValues().get(0));
    assertEquals(PREDEFINED_ATTRIBUTE_VALUE, attributeScoreRequestList.get(1).getValues().get(0));
  }

  @Test
  public void generateAttributeScoreRequestsForUnsyncL3Test_isSkuValueTrueNullPredefinedAttribute() {
    product.setMasterDataProduct(new MasterDataProduct());
    product.setProductSpecialAttributes(Collections.singletonList(productSpecialAttribute));
    setMasterDataAttributes(product.getMasterDataProduct(), DESCRIPTIVE_ATTRIBUTE_VALUE, PREDEFINED_ATTRIBUTE_VALUE,
        ALLOWED_ATTRIBUTE_VALUE);
    product.getMasterDataProduct().getMasterDataProductAttributes().get(1).getMasterDataProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(null);
    product.getMasterDataProduct().getMasterDataProductAttributes().get(2).getMasterDataAttribute().setSkuValue(true);
    List<AttributeScoreRequest> attributeScoreRequestList =
        CommonUtil.generateAttributeScoreRequestsForUnsyncL3(product);
    assertEquals(ATTRIBUTE_CODE, attributeScoreRequestList.get(0).getAttributeCode());
    assertEquals(ATTRIBUTE_CODE_1, attributeScoreRequestList.get(1).getAttributeCode());
    assertEquals(DESCRIPTIVE_ATTRIBUTE_VALUE, attributeScoreRequestList.get(0).getValues().get(0));
    assertEquals(0, attributeScoreRequestList.get(1).getValues().size());
  }

  @Test
  public void setItemRequestsForNullProductCodeL4Test() {
    item.getMasterDataItem().setMasterDataItemImages(Collections.singletonList(masterDataItemImage));
    List<ItemScoreRequest> itemScoreRequestList =
        CommonUtil.setItemRequestsForNullProductCodeL4(Collections.singletonList(item));
    assertEquals(LOCATION_PATH, itemScoreRequestList.get(0).getItemImages().get(0).getLocationPath());
    assertEquals(0, itemScoreRequestList.get(0).getItemImages().get(0).getSequence());
  }

  @Test
  public void setItemRequestsForNullProductCodeL4Test_emptyMasterImages() {
    List<ItemScoreRequest> itemScoreRequestList =
        CommonUtil.setItemRequestsForNullProductCodeL4(Collections.singletonList(item));
    assertTrue(CollectionUtils.isEmpty(itemScoreRequestList.get(0).getItemImages()));
  }

  @Test
  public void toProductCenterHistoryResponseTest() {
    ProductCenterHistoryResponse productCenterHistoryResponse =
        CommonUtil.toProductCenterHistoryResponse(productCenterHistory);
    assertEquals(PRODUCT_CENTER_ACTIVITY, productCenterHistoryResponse.getActivity());
    assertEquals(PRODUCT_CENTER_DESCRIPTION, productCenterHistoryResponse.getDescription());
    assertEquals(PRODUCT_SKU, productCenterHistoryResponse.getProductSku());
    assertEquals(PRODUCT_CENTER_UPDATEDBY, productCenterHistoryResponse.getUpdatedBy());
  }

  @Test
  public void toProductSkuSummaryResponseListTest() {
    productSolr.setMasterCatalog(STORE_ID + solrStringDelimiter + MASTER_CATALOG);
    org.apache.commons.lang3.tuple.Pair<List<String>, List<ProductSkuSummaryResponse>> summaryResponseList =
        CommonUtil.toProductSkuSummaryResponseList(Arrays.asList(productSolr), solrStringDelimiter);
    assertEquals(BRAND, summaryResponseList.getValue().get(0).getBrand());
    assertEquals(PRODUCT_SKU, summaryResponseList.getValue().get(0).getProductSku());
    assertEquals(PRODUCT_NAME, summaryResponseList.getValue().get(0).getProductName());
    assertEquals(MASTER_CATALOG, summaryResponseList.getValue().get(0).getCategoryCode());
    assertEquals(PRODUCT_MAIN_IMAGE, summaryResponseList.getValue().get(0).getProductImage());
    assertFalse(summaryResponseList.getValue().get(0).isArchived());
  }

  @Test
  public void toProductSkuSummaryResponseList_archivedProductTest() {
    productSolr.setIsArchived(Boolean.TRUE);
    productSolr.setMasterCatalog(STORE_ID + solrStringDelimiter + MASTER_CATALOG);
    org.apache.commons.lang3.tuple.Pair<List<String>, List<ProductSkuSummaryResponse>> summaryResponseList =
        CommonUtil.toProductSkuSummaryResponseList(Arrays.asList(productSolr), solrStringDelimiter);
    assertEquals(BRAND, summaryResponseList.getValue().get(0).getBrand());
    assertEquals(PRODUCT_SKU, summaryResponseList.getValue().get(0).getProductSku());
    assertEquals(PRODUCT_NAME, summaryResponseList.getValue().get(0).getProductName());
    assertEquals(MASTER_CATALOG, summaryResponseList.getValue().get(0).getCategoryCode());
    assertEquals(PRODUCT_MAIN_IMAGE, summaryResponseList.getValue().get(0).getProductImage());
    assertTrue(summaryResponseList.getValue().get(0).isArchived());
  }

  @Test
  public void toProductSkuSummaryResponseList_ReindexNeededProductTest() {
    productSolr2.setMasterCatalog(STORE_ID + solrStringDelimiter + MASTER_CATALOG);
    productSolr.setMasterCatalog(null);
    org.apache.commons.lang3.tuple.Pair<List<String>, List<ProductSkuSummaryResponse>> summaryResponseList =
        CommonUtil.toProductSkuSummaryResponseList(Arrays.asList(productSolr, productSolr2), solrStringDelimiter);
    assertEquals(BRAND, summaryResponseList.getValue().get(0).getBrand());
    assertEquals(PRODUCT_SKU_1, summaryResponseList.getValue().get(0).getProductSku());
    assertEquals(PRODUCT_NAME, summaryResponseList.getValue().get(0).getProductName());
    assertEquals(MASTER_CATALOG, summaryResponseList.getValue().get(0).getCategoryCode());
    assertEquals(PRODUCT_MAIN_IMAGE, summaryResponseList.getValue().get(0).getProductImage());
    assertTrue(CollectionUtils.isNotEmpty(summaryResponseList.getKey()));
  }


  @Test
  public void getSolrInputDocumentsForListingUpdateTest() {
    Price price1 = new Price();
    price1.setOfferPrice(1000.0);
    price1.setListPrice(1000.0);
    price1.setMerchantPromoDiscountPrice(new DiscountPrice());
    price1.setChannel("DEFAULT");
    Price price2 = new Price();
    price2.setOfferPrice(1000.0);
    price2.setListPrice(1000.0);
    item.setPrice(Stream.of(price1, price2).collect(Collectors.toSet()));
    List<SolrInputDocument> solrInputDocuments =
        CommonUtil.getSolrInputDocumentsForListingUpdate(Arrays.asList(item), ProductType.REGULAR, solrStringDelimiter);
    assertEquals(item.getItemSku(), solrInputDocuments.get(0).getFieldValue(SolrFieldNames.ID));
  }

  @Test
  public void setMinAndMaxPriceFieldsVariants2Test() {
    Price price1 = new Price();
    price1.setOfferPrice(2000.0);
    price1.setListPrice(2000.0);
    price1.setChannel("DEFAULT");
    Price price2 = new Price();
    price2.setOfferPrice(1000.0);
    price2.setListPrice(1000.0);
    price2.setChannel("DEFAULT1");
    Price price3 = new Price();
    price3.setOfferPrice(2000.0);
    price3.setListPrice(2000.0);
    item.setPrice(Stream.of(price1, price2).collect(Collectors.toSet()));
    productAndItemSolr.setWholesalePriceActivated(true);
    productAndItemSolr.setPickupPointCode("pickup");
    productSolr.setVariantCount(2);
    productSolr.setMaximumSellingPrice(1000.0);
    productSolr.setMinimumSellingPrice(200.0);
    productSolr.setMaximumListPrice(1000.0);
    productSolr.setMinimumListPrice(200.0);
    SolrInputDocument solrInputDocuments =
        CommonUtil.getL3SolrInputDocumentsForListingUpdate(PRODUCT_SKU, Arrays.asList(item), true);
    assertEquals(PRODUCT_SKU, solrInputDocuments.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, 2000.0),
        solrInputDocuments.getFieldValue(SolrFieldNames.MAXIMUM_SELLING_PRICE));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, 2000.0),
        solrInputDocuments.getFieldValue(SolrFieldNames.MAXIMUM_LIST_PRICE));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, true),
        solrInputDocuments.getFieldValue(SolrFieldNames.IS_ARCHIVED));
  }

  @Test
  public void setMinAndMaxPriceFieldsVariants2MinPricesTest() {
    Price price1 = new Price();
    price1.setOfferPrice(2000.0);
    price1.setListPrice(2000.0);
    price1.setChannel("DEFAULT");
    Price price2 = new Price();
    price2.setOfferPrice(1000.0);
    price2.setListPrice(1000.0);
    price2.setChannel("DEFAULT1");
    Price price3 = new Price();
    price3.setOfferPrice(2000.0);
    price3.setListPrice(2000.0);
    item.setPrice(Stream.of(price1, price2).collect(Collectors.toSet()));
    productAndItemSolr.setWholesalePriceActivated(true);
    productAndItemSolr.setPickupPointCode("pickup");
    productSolr.setVariantCount(2);
    productSolr.setMaximumSellingPrice(5000.0);
    productSolr.setMinimumSellingPrice(2000.0);
    productSolr.setMaximumListPrice(5000.0);
    productSolr.setMinimumListPrice(2000.0);
    SolrInputDocument solrInputDocuments =
        CommonUtil.getL3SolrInputDocumentsForListingUpdate(PRODUCT_SKU, Arrays.asList(item), false);
    assertEquals(PRODUCT_SKU, solrInputDocuments.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, 1000.0),
        solrInputDocuments.getFieldValue(SolrFieldNames.MINIMUM_SELLING_PRICE));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, 1000.0),
        solrInputDocuments.getFieldValue(SolrFieldNames.MINIMUM_LIST_PRICE));
  }

  @Test
  public void setMinAndMaxPriceFieldsVariants1Test() {
    Price price1 = new Price();
    price1.setOfferPrice(2000.0);
    price1.setListPrice(2000.0);
    price1.setChannel("DEFAULT");
    Price price2 = new Price();
    price2.setOfferPrice(1000.0);
    price2.setListPrice(1000.0);
    price2.setChannel("DEFAULT1");
    Price price3 = new Price();
    price3.setOfferPrice(2000.0);
    price3.setListPrice(2000.0);
    item.setPrice(Stream.of(price1, price2).collect(Collectors.toSet()));
    productAndItemSolr.setWholesalePriceActivated(true);
    productAndItemSolr.setPickupPointCode("pickup");
    productSolr.setVariantCount(1);
    productSolr.setMaximumSellingPrice(1000.0);
    productSolr.setMinimumSellingPrice(200.0);
    productSolr.setMaximumListPrice(1000.0);
    productSolr.setMinimumListPrice(200.0);
    SolrInputDocument solrInputDocuments =
        CommonUtil.getL3SolrInputDocumentsForListingUpdate(PRODUCT_SKU, Arrays.asList(item), false);
    assertEquals(PRODUCT_SKU, solrInputDocuments.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, 2000.0),
        solrInputDocuments.getFieldValue(SolrFieldNames.MAXIMUM_SELLING_PRICE));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, 2000.0),
        solrInputDocuments.getFieldValue(SolrFieldNames.MAXIMUM_LIST_PRICE));
  }

  @Test
  public void setMinAndMaxPriceFieldsVariantsViaEventTest() {
    Price price1 = new Price();
    price1.setOfferPrice(2000.0);
    price1.setListPrice(2000.0);
    price1.setChannel("DEFAULT");
    Price price2 = new Price();
    price2.setOfferPrice(1000.0);
    price2.setListPrice(1000.0);
    price2.setChannel("DEFAULT1");
    Price price3 = new Price();
    price3.setOfferPrice(2000.0);
    price3.setListPrice(2000.0);
    item.setPrice(Stream.of(price1, price2).collect(Collectors.toSet()));
    productAndItemSolr.setWholesalePriceActivated(true);
    productAndItemSolr.setPickupPointCode("pickup");
    productSolr.setVariantCount(1);
    productSolr.setMaximumSellingPrice(1000.0);
    productSolr.setMinimumSellingPrice(200.0);
    productSolr.setMaximumListPrice(1000.0);
    productSolr.setMinimumListPrice(200.0);
    Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesForListingUpdate(Arrays.asList(item), false);
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, 2000.0),
        fieldsAndValues.get(SolrFieldNames.MAXIMUM_SELLING_PRICE));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, 2000.0),
        fieldsAndValues.get(SolrFieldNames.MAXIMUM_LIST_PRICE));
  }

  @Test
  public void getpickupPointCodeAtomicUpdateInputDocumentTest() {
    item.setPickupPointCode(PICKUP_POINT_CODE);
    SolrInputDocument solrInputDocument = CommonUtil.getpickupPointCodeAtomicUpdateInputDocument(item);
    assertEquals(item.getItemSku(), solrInputDocument.getFieldValue(SolrFieldNames.ID));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, PICKUP_POINT_CODE),
        solrInputDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODE));
  }

  @Test
  public void getpickupPointCodeAtomicUpdateInputDocumentL3() {
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);
    item.setPickupPointCode(PICKUP_POINT_CODE);
    SolrInputDocument solrInputDocument = CommonUtil.getpickupPointCodeAtomicUpdateInputDocumentL3(Arrays.asList(item));
    Assertions.assertEquals(item.getProductSku(), solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, Arrays.asList(PICKUP_POINT_CODE)),
        solrInputDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODES));
  }

  @Test
  public void toSolrInputDocumentL3AtomicUpdateTest() {
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocumentL3AtomicUpdate(productSolr);
    assertEquals(PRODUCT_SKU, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getProductCode()),
        solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getProductName()),
        solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getMerchantCode()),
        solrInputDocument.getFieldValue(SolrFieldNames.MERCHANT_CODE));
  }

  @Test
  public void toSolrInputDocumentL3AtomicUpdateNullTest() {
    ProductSolr productSolr = new ProductSolr();
    productSolr.setProductSku(PRODUCT_SKU);
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocumentL3AtomicUpdate(productSolr);
    assertEquals(PRODUCT_SKU, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
  }


  @Test
  public void generateL3AggregateDataFromItemsTest() {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    price.setOfferPrice(1000);
    price.setListPrice(2000);
    item.setPrice(Collections.singleton(price));
    CommonUtil.generateL3AggregateDataFromItems(product, Arrays.asList(item), solrInputDocument, itemPickupPointList,
        false, false, false);
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    assertFalse(CollectionUtils.isEmpty(solrInputDocument.getFieldValues(SolrFieldNames.PICKUP_POINT_CODES)));
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
  }

  @Test
  public void generateL3AggregateDataFromItemsZeroVariantsTest() {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    price.setOfferPrice(1000);
    price.setListPrice(2000);
    item.setPrice(Collections.singleton(price));
    itemPickupPoint.setMarkForDelete(true);
    CommonUtil.generateL3AggregateDataFromItems(product, null, solrInputDocument, Arrays.asList(itemPickupPoint), true,
        false, false);
    assertEquals(0, (double) solrInputDocument.getFieldValue(SolrFieldNames.VARIANT_COUNT), 1);
  }

  @Test
  public void generateL3AggregateDataFromItemsItemNullTest() {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    price.setOfferPrice(1000);
    price.setListPrice(2000);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPointList.add(itemPickupPoint);
    item.setPrice(Collections.singleton(price));
    CommonUtil.generateL3AggregateDataFromItems(product, null, solrInputDocument, itemPickupPointList, false,
        false, false);
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    assertFalse(CollectionUtils.isEmpty(solrInputDocument.getFieldValues(SolrFieldNames.PICKUP_POINT_CODES)));
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    assertEquals((double) solrInputDocument.getFieldValue(SolrFieldNames.VARIANT_COUNT), 1, 0);
  }

  @Test
  public void generateL3AggregateDataFromItems_nonEmptyProductCollectionTest() {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    price.setOfferPrice(1000);
    price.setListPrice(2000);
    item.setPrice(Collections.singleton(price));
    product.setPickupPointCodes(new HashSet<>(Arrays.asList(PICKUP_POINT_CODE, PICKUP_POINT_CODE_1)));
    CommonUtil.generateL3AggregateDataFromItems(product, Arrays.asList(item), solrInputDocument, itemPickupPointList,
        false, false, false);
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    assertEquals(Arrays.asList(PICKUP_POINT_CODE_1, PICKUP_POINT_CODE),
        solrInputDocument.get(SolrFieldNames.PICKUP_POINT_CODES).getValue());
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
  }

  @Test
  public void generateL3AggregateDataFromItems_inStockWebTest() {
    inventoryStockInfoDTO.setWebTotalAvailableStock(1);
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    price.setOfferPrice(1000);
    price.setListPrice(2000);
    item.setPrice(Collections.singleton(price));
    CommonUtil.generateL3AggregateDataFromItems(product, Arrays.asList(item), solrInputDocument, itemPickupPointList,
        false, false, false);
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    assertFalse(CollectionUtils.isEmpty(solrInputDocument.getFieldValues(SolrFieldNames.PICKUP_POINT_CODES)));
  }

  @Test
  public void generateL3AggregateDataFromItems_inStockWarehouseTest() {
    inventoryStockInfoDTO.setWarehouseTotalAvailableStock(1);
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    price.setOfferPrice(1000);
    price.setListPrice(2000);
    item.setPrice(Collections.singleton(price));
    CommonUtil.generateL3AggregateDataFromItems(product, Arrays.asList(item), solrInputDocument, itemPickupPointList,
        false, false, false);
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    assertFalse(CollectionUtils.isEmpty(solrInputDocument.getFieldValues(SolrFieldNames.PICKUP_POINT_CODES)));
  }

  @Test
  public void generateL3AggregateDataFromItems_emptyPricesTest() {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    CommonUtil.generateL3AggregateDataFromItems(product, Arrays.asList(item), solrInputDocument, itemPickupPointList,
        false, false, false);
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    assertFalse(CollectionUtils.isEmpty(solrInputDocument.getFieldValues(SolrFieldNames.PICKUP_POINT_CODES)));
  }

  @Test
  public void generateL3AggregateDataFromItems_nullPreOrder() {
    product.setPreOrder(null);
    price.setMerchantPromoDiscountPrice(null);
    item.setActivePromoBundlings(new HashSet<>());
    item.setPrice(ImmutableSet.of(price));
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    CommonUtil.generateL3AggregateDataFromItems(product, Arrays.asList(item), solrInputDocument, itemPickupPointList,
        true, false, false);
    assertNull(solrInputDocument.getFieldValue(SolrFieldNames.IS_PRE_ORDER_ACTIVE));
    assertNull(solrInputDocument.getFieldValue(SolrFieldNames.WHOLESALE_ITEM_SKUS));
    assertNull(solrInputDocument.getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS));
  }

  @Test
  public void generateL3AggregateDataFromItems_PreOrderActiveFlagNull() {
    product.setPreOrder(new PreOrder());
    price.setMerchantPromoDiscountPrice(new DiscountPrice());
    item.setMerchantPromoDiscount(false);
    item.setActivePromoBundlings(ImmutableSet.of(PromoType.PROMO_DISCOUNT.getDescription()));
    item.setPrice(ImmutableSet.of(price));
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    CommonUtil.generateL3AggregateDataFromItems(product, Arrays.asList(item), solrInputDocument, itemPickupPointList,
        false, false, false);
    assertNull(solrInputDocument.getFieldValue(SolrFieldNames.IS_PRE_ORDER_ACTIVE));
    assertNull(solrInputDocument.getFieldValue(SolrFieldNames.WHOLESALE_ITEM_SKUS));
    assertNull(solrInputDocument.getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS));
  }

  @Test
  public void generateL3AggregateDataFromItems_PreOrderActiveTrue() {
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    product.setPreOrder(preOrder);
    price.setMerchantPromoDiscountPrice(new DiscountPrice());
    item.setMerchantPromoDiscount(true);
    item.setWholesalePriceExists(true);
    item.setActivePromoBundlings(ImmutableSet.of(Constants.WHOLESALE_PRICE));
    item.setPrice(ImmutableSet.of(price));
    itemPickupPointList.get(0).getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    itemPickupPointList.get(0).setWholesalePriceExists(true);
    itemPickupPointList.get(0).setPromoBundling(true);
    itemPickupPointList.get(0).setItemSku(item.getItemSku());
    itemPickupPointList.get(0).setDelivery(true);
    itemPickupPointList.add(new ItemPickupPoint());
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    CommonUtil.generateL3AggregateDataFromItems(product, Arrays.asList(item), solrInputDocument, itemPickupPointList,
        false, false, false);
    assertTrue((Boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_PRE_ORDER_ACTIVE));
    assertTrue(solrInputDocument.getFieldValues(SolrFieldNames.WHOLESALE_ITEM_SKUS).contains(item.getItemSku()));
    assertTrue(solrInputDocument.getFieldValues(SolrFieldNames.PROMO_ITEM_SKUS).contains(item.getItemSku()));
  }

  @Test
  public void getSolrInputDocumentForNewFieldsL3SolrTest() {
    item.setPrice(Collections.singleton(price));
    item.setPickupPointCode(PICKUP_POINT_CODE);
    product.setArchived(true);
    product.setOff2OnChannelActive(true);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentForNewFieldsL3Solr(PRODUCT_SKU, product, Arrays.asList(item), false);
    assertEquals(PRODUCT_SKU, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODES)).get(SolrConstants.SET_CLAUSE),
        Collections.singletonList(PICKUP_POINT_CODE));
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.VARIANT_COUNT)).get(SolrConstants.SET_CLAUSE),
        1);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MAXIMUM_SELLING_PRICE)).get(SolrConstants.SET_CLAUSE),
        0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MINIMUM_SELLING_PRICE)).get(SolrConstants.SET_CLAUSE),
        0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MAXIMUM_LIST_PRICE)).get(SolrConstants.SET_CLAUSE), 0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MINIMUM_LIST_PRICE)).get(SolrConstants.SET_CLAUSE), 0.0);
    assertFalse(
        (boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_IN_STOCK)).get(SolrConstants.SET_CLAUSE));
    assertTrue(
        (boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED)).get(SolrConstants.SET_CLAUSE));
    assertTrue((boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE)).get(
        SolrConstants.SET_CLAUSE));
  }

  @Test
  public void getSolrInputDocumentForNewFieldsL3Solr_inStockWebTest() {
    product.setPreOrder(new PreOrder(true, null, null, null));
    item.setWholesalePriceExists(true);
    item.setActivePromoBundlings(ImmutableSet.of(Constants.WHOLESALE_PRICE));
    inventoryStockInfoDTO.setWebTotalAvailableStock(1);
    item.setPrice(Collections.singleton(price));
    item.setPickupPointCode(PICKUP_POINT_CODE);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentForNewFieldsL3Solr(PRODUCT_SKU, product, Arrays.asList(item), true);
    assertEquals(PRODUCT_SKU, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODES)).get(SolrConstants.SET_CLAUSE),
        Collections.singletonList(PICKUP_POINT_CODE));
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.VARIANT_COUNT)).get(SolrConstants.SET_CLAUSE),
        1);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MAXIMUM_SELLING_PRICE)).get(SolrConstants.SET_CLAUSE),
        0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MINIMUM_SELLING_PRICE)).get(SolrConstants.SET_CLAUSE),
        0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MAXIMUM_LIST_PRICE)).get(SolrConstants.SET_CLAUSE), 0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MINIMUM_LIST_PRICE)).get(SolrConstants.SET_CLAUSE), 0.0);
    assertTrue(
        (boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_IN_STOCK)).get(SolrConstants.SET_CLAUSE));
    assertFalse(
        (boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED)).get(SolrConstants.SET_CLAUSE));
    assertFalse((boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE)).get(
        SolrConstants.SET_CLAUSE));
  }

  @Test
  public void getSolrInputDocumentForNewFieldsL3Solr_inStockWarehouseTest() throws ParseException {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(CAMPAIGN_CODE);
    discountPrice.setStartDateTime(new SimpleDateFormat("dd-MM-yyyy").parse("01-07-2021"));
    discountPrice.setEndDateTime(new SimpleDateFormat("dd-MM-yyyy").parse("31-07-2021"));
    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    inventoryStockInfoDTO.setWarehouseTotalAvailableStock(1);
    item.setMerchantPromoDiscount(true);
    item.setPromoBundling(true);
    item.setPrice(ImmutableSet.of(price));
    item.setPrice(Collections.singleton(price));
    item.setPickupPointCode(PICKUP_POINT_CODE);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentForNewFieldsL3Solr(PRODUCT_SKU, product, Arrays.asList(item), true);
    assertEquals(PRODUCT_SKU, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODES)).get(SolrConstants.SET_CLAUSE),
        Collections.singletonList(PICKUP_POINT_CODE));
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.VARIANT_COUNT)).get(SolrConstants.SET_CLAUSE),
        1);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MAXIMUM_SELLING_PRICE)).get(SolrConstants.SET_CLAUSE),
        0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MINIMUM_SELLING_PRICE)).get(SolrConstants.SET_CLAUSE),
        0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MAXIMUM_LIST_PRICE)).get(SolrConstants.SET_CLAUSE), 0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MINIMUM_LIST_PRICE)).get(SolrConstants.SET_CLAUSE), 0.0);
    assertTrue(
        (boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_IN_STOCK)).get(SolrConstants.SET_CLAUSE));
    assertFalse(
        (boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED)).get(SolrConstants.SET_CLAUSE));
    assertFalse((boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE)).get(
        SolrConstants.SET_CLAUSE));
  }

  @Test
  public void getPromoTypesByItemTest() {
    ItemPickupPoint item = new ItemPickupPoint();
    item.setPromoBundling(true);
    item.setMerchantPromoDiscount(true);
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    List<String> promoTypesByItem = CommonUtil.getPromoTypesByItem(item);
    assertEquals(3, promoTypesByItem.size());
  }

  @Test
  public void getPromoTypesByItemTestNoPrice() {
    ItemPickupPoint item = new ItemPickupPoint();
    item.setPromoBundling(false);
    item.setMerchantPromoDiscount(false);
    Price price = new Price();
    price.setOfferPrice(100);
    List<String> promoTypesByItem = CommonUtil.getPromoTypesByItem(item);
    assertEquals(0, promoTypesByItem.size());
  }

  @Test
  public void getPromoTypesByItemNoPromo() {
    ItemPickupPoint item = new ItemPickupPoint();
    item.setPromoBundling(false);
    item.setMerchantPromoDiscount(false);
    Price price = new Price();
    price.setOfferPrice(100);
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    List<String> promoTypesByItem = CommonUtil.getPromoTypesByItem(item);
    assertEquals(0, promoTypesByItem.size());
  }

  @Test
  public void getPromoTypesByItemTestUpcomingPromo() {
    ItemPickupPoint item = new ItemPickupPoint();
    item.setPromoBundling(true);
    item.setMerchantPromoDiscount(true);
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(+1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    List<String> promoTypesByItem = CommonUtil.getPromoTypesByItem(item);
    assertEquals(2, promoTypesByItem.size());
  }

  @Test
  public void getPromoTypesByItemTestExpiredPromo() {
    ItemPickupPoint item = new ItemPickupPoint();
    item.setPromoBundling(true);
    item.setMerchantPromoDiscount(true);
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(2))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    List<String> promoTypesByItem = CommonUtil.getPromoTypesByItem(item);
    assertEquals(2, promoTypesByItem.size());
  }

  @Test
  public void getSolrInputDocumentForL3SolrTest() {
    product.setProductSku(PRODUCT_SKU);
    product.setArchived(true);
    product.setOff2OnChannelActive(true);
    product.setMerchantCode(MERCHANT_CODE);
    SolrInputDocument solrInputDocument = CommonUtil.getSolrInputDocumentForL3Solr(product);
    assertEquals(PRODUCT_SKU, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(MERCHANT_CODE, solrInputDocument.getFieldValue(SolrFieldNames.MERCHANT_CODE));
    assertTrue(
        (boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED)).get(SolrConstants.SET_CLAUSE));
    assertTrue((boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE)).get(
        SolrConstants.SET_CLAUSE));
  }

  @Test
  public void getFieldsAndValuesForPromoUpdatesTest() {
    item.setPromoBundling(true);
    item.setProductSku(PRODUCT_SKU);
    Map<String, Map<String, Object>> productAndPromoDetails =
        CommonUtil.getFieldsAndValuesForPromoUpdates(Arrays.asList(item), true);
    assertEquals(1, productAndPromoDetails.size());
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.ADD_CLAUSE), Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getFieldsAndValuesForPromoUpdatesOnlyMPDTest() {
    item.setMerchantPromoDiscount(true);
    item.setProductSku(PRODUCT_SKU);
    Map<String, Map<String, Object>> productAndPromoDetails =
        CommonUtil.getFieldsAndValuesForPromoUpdates(Arrays.asList(item), true);
    assertEquals(1, productAndPromoDetails.size());
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.ADD_CLAUSE), Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getFieldsAndValuesForPromoUpdatesMPDAndPromoTest() {
    item.setMerchantPromoDiscount(true);
    item.setPromoBundling(true);
    item.setProductSku(PRODUCT_SKU);
    Map<String, Map<String, Object>> productAndPromoDetails =
        CommonUtil.getFieldsAndValuesForPromoUpdates(Arrays.asList(item), true);
    assertEquals(1, productAndPromoDetails.size());
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.ADD_CLAUSE), Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getFieldsAndValuesForPromoUpdatesCampaignTest() {
    item.setProductSku(PRODUCT_SKU);
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    Map<String, Map<String, Object>> productAndPromoDetails =
        CommonUtil.getFieldsAndValuesForPromoUpdates(Arrays.asList(item), true);
    assertEquals(1, productAndPromoDetails.size());
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.ADD_CLAUSE), Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getFieldsAndValuesForPromoUpdatesMPDPromoAndCampaignTest() {
    item.setMerchantPromoDiscount(true);
    item.setPromoBundling(true);
    item.setProductSku(PRODUCT_SKU);
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    Map<String, Map<String, Object>> productAndPromoDetails =
        CommonUtil.getFieldsAndValuesForPromoUpdates(Arrays.asList(item), true);
    assertEquals(1, productAndPromoDetails.size());
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.ADD_CLAUSE), Arrays.asList(item.getItemSku()));
    assertNull(productAndPromoDetails.get(SolrFieldNames.WHOLESALE_ITEM_SKUS));

  }

  @Test
  public void getFieldsAndValuesForPromoUpdatesWithWholesaleTest() {
    item.setMerchantPromoDiscount(true);
    item.setPromoBundling(true);
    item.setProductSku(PRODUCT_SKU);
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    Map<String, Map<String, Object>> productAndPromoDetails =
        CommonUtil.getFieldsAndValuesForPromoUpdates(Arrays.asList(item), false);
    assertEquals(1, productAndPromoDetails.size());
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.ADD_CLAUSE), Arrays.asList(item.getItemSku()));
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getFieldsAndValuesForPromoUpdatesMultipleItemsTest() {
    item.setMerchantPromoDiscount(true);
    item.setPromoBundling(true);
    item.setProductSku(PRODUCT_SKU);
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setActivePromoBundlings(new HashSet<>());
    Item item2 = new Item();
    item2.setPromoBundling(false);
    item2.setProductSku(PRODUCT_SKU);
    item2.setItemSku(PRODUCT_SKU);
    item2.setWholesalePriceExists(true);
    Map<String, Map<String, Object>> productAndPromoDetails =
        CommonUtil.getFieldsAndValuesForPromoUpdates(Arrays.asList(item, item2), false);
    assertEquals(1, productAndPromoDetails.size());
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.ADD_CLAUSE), Arrays.asList(item.getItemSku()));
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item2.getItemSku()));
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item.getItemSku(), item2.getItemSku()));
  }

  @Test
  public void getFieldsAndValuesForPromoUpdatesMultipleItemsWholesaleTrueTest() {
    item.setMerchantPromoDiscount(false);
    item.setPromoBundling(false);
    item.setProductSku(PRODUCT_SKU);
    item.setWholesalePriceExists(true);
    item.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    Item item2 = new Item();
    item2.setPromoBundling(false);
    item2.setProductSku(PRODUCT_SKU);
    item2.setItemSku(PRODUCT_SKU);
    item2.setWholesalePriceExists(true);
    item2.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    Map<String, Map<String, Object>> productAndPromoDetails =
        CommonUtil.getFieldsAndValuesForPromoUpdates(Arrays.asList(item, item2), false);
    assertEquals(1, productAndPromoDetails.size());
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item.getItemSku(), item2.getItemSku()));
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(
        SolrConstants.ADD_CLAUSE), Arrays.asList(item.getItemSku(), item2.getItemSku()));

  }

  @Test
  public void getFieldsAndValuesForPromoUpdatesMultipleItemsWholesaleTrueAndFalseTest() {
    item.setMerchantPromoDiscount(false);
    item.setPromoBundling(false);
    item.setProductSku(PRODUCT_SKU);
    item.setWholesalePriceExists(true);
    item.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    Item item2 = new Item();
    item2.setPromoBundling(false);
    item2.setProductSku(PRODUCT_SKU);
    item2.setItemSku(PRODUCT_SKU);
    item2.setWholesalePriceExists(true);
    item2.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE)));
    Map<String, Map<String, Object>> productAndPromoDetails =
        CommonUtil.getFieldsAndValuesForPromoUpdates(Arrays.asList(item, item2), false);
    assertEquals(1, productAndPromoDetails.size());
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item.getItemSku(), item2.getItemSku()));
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(
        SolrConstants.ADD_CLAUSE), Arrays.asList(item.getItemSku()));
    assertEquals(((Map) productAndPromoDetails.get(PRODUCT_SKU).get(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item2.getItemSku()));

  }

  @Test
  public void getSolrInputDocsForPromoUpdatesOnlyPromo() {
    item.setPromoBundling(true);
    item.setProductSku(PRODUCT_SKU);
    List<SolrInputDocument> solrInputDocuments = CommonUtil.getSolrInputDocsForPromoUpdates(Arrays.asList(item), true);
    assertEquals(1, solrInputDocuments.size());
    assertEquals(
        ((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS)).get(SolrConstants.ADD_CLAUSE),
        Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getSolrInputDocsForPromoUpdatesOnlyMPD() {
    item.setMerchantPromoDiscount(true);
    item.setProductSku(PRODUCT_SKU);
    List<SolrInputDocument> solrInputDocuments = CommonUtil.getSolrInputDocsForPromoUpdates(Arrays.asList(item), true);
    assertEquals(1, solrInputDocuments.size());
    assertEquals(
        ((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS)).get(SolrConstants.ADD_CLAUSE),
        Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getSolrInputDocsForPromoUpdatesMPDAndPromo() {
    item.setMerchantPromoDiscount(true);
    item.setPromoBundling(true);
    item.setProductSku(PRODUCT_SKU);
    List<SolrInputDocument> solrInputDocuments = CommonUtil.getSolrInputDocsForPromoUpdates(Arrays.asList(item), true);
    assertEquals(1, solrInputDocuments.size());
    assertEquals(
        ((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS)).get(SolrConstants.ADD_CLAUSE),
        Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getSolrInputDocsForPromoUpdatesCampaign() {
    item.setProductSku(PRODUCT_SKU);
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    List<SolrInputDocument> solrInputDocuments = CommonUtil.getSolrInputDocsForPromoUpdates(Arrays.asList(item), true);
    assertEquals(1, solrInputDocuments.size());
    assertEquals(
        ((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS)).get(SolrConstants.ADD_CLAUSE),
        Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getSolrInputDocsForPromoUpdatesMPDPromoAndCampaign() {
    item.setMerchantPromoDiscount(true);
    item.setPromoBundling(true);
    item.setProductSku(PRODUCT_SKU);
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    List<SolrInputDocument> solrInputDocuments = CommonUtil.getSolrInputDocsForPromoUpdates(Arrays.asList(item), true);
    assertEquals(1, solrInputDocuments.size());
    assertEquals(
        ((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS)).get(SolrConstants.ADD_CLAUSE),
        Arrays.asList(item.getItemSku()));
    assertNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.WHOLESALE_ITEM_SKUS));

  }

  @Test
  public void getSolrInputDocsForPromoUpdatesWithWholesale() {
    item.setMerchantPromoDiscount(true);
    item.setPromoBundling(true);
    item.setProductSku(PRODUCT_SKU);
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    List<SolrInputDocument> solrInputDocuments = CommonUtil.getSolrInputDocsForPromoUpdates(Arrays.asList(item), false);
    assertEquals(1, solrInputDocuments.size());
    assertEquals(
        ((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS)).get(SolrConstants.ADD_CLAUSE),
        Arrays.asList(item.getItemSku()));
    assertEquals(((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getSolrInputDocsForPromoUpdatesMultipleItems() {
    item.setMerchantPromoDiscount(true);
    item.setPromoBundling(true);
    item.setProductSku(PRODUCT_SKU);
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setActivePromoBundlings(new HashSet<>());
    Item item2 = new Item();
    item2.setPromoBundling(false);
    item2.setProductSku(PRODUCT_SKU);
    item2.setItemSku(PRODUCT_SKU);
    item2.setWholesalePriceExists(true);
    List<SolrInputDocument> solrInputDocuments =
        CommonUtil.getSolrInputDocsForPromoUpdates(Arrays.asList(item, item2), false);
    assertEquals(1, solrInputDocuments.size());
    assertEquals(
        ((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS)).get(SolrConstants.ADD_CLAUSE),
        Arrays.asList(item.getItemSku()));
    assertEquals(((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item2.getItemSku()));
    assertEquals(((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item.getItemSku(), item2.getItemSku()));
  }

  @Test
  public void getSolrInputDocsForPromoUpdatesMultipleItemsWholesaleTrue() {
    item.setMerchantPromoDiscount(false);
    item.setPromoBundling(false);
    item.setProductSku(PRODUCT_SKU);
    item.setWholesalePriceExists(true);
    item.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    Item item2 = new Item();
    item2.setPromoBundling(false);
    item2.setProductSku(PRODUCT_SKU);
    item2.setItemSku(PRODUCT_SKU);
    item2.setWholesalePriceExists(true);
    item2.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    List<SolrInputDocument> solrInputDocuments =
        CommonUtil.getSolrInputDocsForPromoUpdates(Arrays.asList(item, item2), false);
    assertEquals(1, solrInputDocuments.size());
    assertEquals(((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item.getItemSku(), item2.getItemSku()));
    assertEquals(((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(
        SolrConstants.ADD_CLAUSE), Arrays.asList(item.getItemSku(), item2.getItemSku()));

  }

  @Test
  public void getSolrInputDocsForPromoUpdatesMultipleItemsWholesaleTrueAndFalse() {
    item.setMerchantPromoDiscount(false);
    item.setPromoBundling(false);
    item.setProductSku(PRODUCT_SKU);
    item.setWholesalePriceExists(true);
    item.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    Item item2 = new Item();
    item2.setPromoBundling(false);
    item2.setProductSku(PRODUCT_SKU);
    item2.setItemSku(PRODUCT_SKU);
    item2.setWholesalePriceExists(true);
    item2.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE)));
    List<SolrInputDocument> solrInputDocuments =
        CommonUtil.getSolrInputDocsForPromoUpdates(Arrays.asList(item, item2), false);
    assertEquals(1, solrInputDocuments.size());
    assertEquals(((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item.getItemSku(), item2.getItemSku()));
    assertEquals(((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(
        SolrConstants.ADD_CLAUSE), Arrays.asList(item.getItemSku()));
    assertEquals(((Map) solrInputDocuments.get(0).getFieldValue(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(
        SolrConstants.REMOVE_CLAUSE), Arrays.asList(item2.getItemSku()));

  }

  @Test
  public void getFieldsAndValuesForPriceAtomicUpdateL3Test() {
    item.setWholesalePriceExists(true);
    item.setActivePromoBundlings(ImmutableSet.of(Constants.WHOLESALE_PRICE));
    productSolr.setVariantCount(1);
    List<Item> items = new ArrayList<>();
    items.add(item);
    Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesForPriceAtomicUpdateL3(item, items);
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(SolrConstants.ADD_CLAUSE),
        Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getFieldsAndValuesForPriceAtomicUpdateL3RemoveClauseTest() {
    item.setWholesalePriceExists(false);
    productSolr.setVariantCount(1);
    List<Item> items = new ArrayList<>();
    items.add(item);
    Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesForPriceAtomicUpdateL3(item, items);
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(SolrConstants.REMOVE_CLAUSE),
        Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getSolrInputDocumentForPriceAtomicUpdateL3Test() {
    item.setWholesalePriceExists(true);
    item.setActivePromoBundlings(ImmutableSet.of(Constants.WHOLESALE_PRICE));
    productSolr.setVariantCount(1);
    List<Item> items = new ArrayList<>();
    items.add(item);
    SolrInputDocument solrInputDocument = CommonUtil.getSolrInputDocumentForPriceAtomicUpdateL3(item, items);
    assertEquals(item.getProductSku(), solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(SolrConstants.ADD_CLAUSE),
        Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getSolrInputDocumentForPriceAtomicUpdateL3RemoveClauseTest() {
    item.setWholesalePriceExists(false);
    productSolr.setVariantCount(1);
    List<Item> items = new ArrayList<>();
    items.add(item);
    SolrInputDocument solrInputDocument = CommonUtil.getSolrInputDocumentForPriceAtomicUpdateL3(item, items);
    assertEquals(item.getProductSku(), solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.WHOLESALE_ITEM_SKUS)).get(SolrConstants.REMOVE_CLAUSE),
        Arrays.asList(item.getItemSku()));
  }

  @Test
  public void getPriceEditDisabledReasonsTest() throws ParseException {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(CAMPAIGN_CODE);
    discountPrice.setStartDateTime(new SimpleDateFormat("dd-MM-yyyy").parse("01-07-2021"));
    discountPrice.setEndDateTime(new SimpleDateFormat("dd-MM-yyyy").parse("31-07-2099"));
    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    item.setMerchantPromoDiscount(true);
    item.setPrice(ImmutableSet.of(price));
    Set<String> result = CommonUtil.getPriceEditDisabledReasons(item);
    assertTrue(result.contains(PromoType.CAMPAIGN.getDescription()));
    assertTrue(result.contains(PromoType.PROMO_DISCOUNT.getDescription()));
  }

  @Test
  public void getPriceEditDisabledReasonsFalseTest() throws ParseException {
    Price price = new Price();
    item.setPrice(ImmutableSet.of(price));
    Set<String> result = CommonUtil.getPriceEditDisabledReasons(item);
    assertTrue(!result.contains(PromoType.CAMPAIGN.getDescription()));
    assertTrue(!result.contains(PromoType.PROMO_DISCOUNT.getDescription()));
  }

  @Test
  public void getPriceEditDisabledReasonsEmptyPriceTest() throws ParseException {
    item.setPrice(new HashSet<>());
    Set<String> result = CommonUtil.getPriceEditDisabledReasons(item);
    assertTrue(result.isEmpty());
  }

  @Test
  public void setItemCatalogsTest() {
    CommonUtil.setItemCatalogs(product,
        Collections.singletonMap(CATEGORY_CODE, Collections.singletonList(itemCatalogVO)), Arrays.asList(CATEGORY_CODE),
        false);
    assertEquals(itemCatalogVO, product.getItemCatalogs().get(0));
  }

  @Test
  public void removeInactiveSalesCategoriesTest() {
    CommonUtil.removeInactiveSalesCategories(product, Collections.singletonList(itemCatalogVO), false);
    CommonUtil.removeInactiveSalesCategories(product, Collections.singletonList(itemCatalogVO), true);
    ItemCategoryVO itemCategoryVO = new ItemCategoryVO();
    itemCatalogVO.setItemCategories(Collections.singletonList(itemCategoryVO));
    itemCatalogVO.getItemCategories().get(0).setActivated(true);
    CommonUtil.removeInactiveSalesCategories(product, Collections.singletonList(itemCatalogVO), true);
    itemCatalogVO.getItemCategories().get(0).setActivated(false);
    itemCatalogVO.getItemCategories().get(0).setProductCategoryCode(PRODUCT_CODE);
    CommonUtil.removeInactiveSalesCategories(product, Collections.singletonList(itemCatalogVO), true);
    SalesCatalog salesCatalog = new SalesCatalog();
    salesCatalog.setCatalogCode(Constants.SALES_CATEGORY_CATALOG_CODE);
    product.setSalesCatalogs(Collections.singletonList(salesCatalog));
    CommonUtil.removeInactiveSalesCategories(product, Collections.singletonList(itemCatalogVO), true);
    Category category = new Category();
    category.setCategoryCode(PICKUP_POINT_CODE);
    product.setSalesCatalogs(Collections.singletonList(salesCatalog));
    product.getSalesCatalogs().get(0).setListOfCategories(Collections.singletonList(category));
    CommonUtil.removeInactiveSalesCategories(product, Collections.singletonList(itemCatalogVO), true);
    product.setSalesCatalogs(Collections.singletonList(salesCatalog));
    product.getSalesCatalogs().get(0).getListOfCategories().get(0).setCategoryCode(PRODUCT_CODE);
    product.getSalesCatalogs().get(0).setListOfCategories(new ArrayList<>(Collections.singletonList(category)));
    CommonUtil.removeInactiveSalesCategories(product, new ArrayList<>(Collections.singletonList(itemCatalogVO)), true);
  }

  @Test
  public void fetchCategoryCodeListTest() {
    Set<String> categoryCodeList = new HashSet<>();
    Map<String, List<String>> productToCategoryCodeList = new HashMap<>();
    CommonUtil.fetchCategoryCodeList(product, categoryCodeList, productToCategoryCodeList, Constants.ALL);
    CommonUtil.fetchCategoryCodeList(product1, categoryCodeList, productToCategoryCodeList, Constants.ALL);
    assertEquals(2, categoryCodeList.size());
    assertEquals(Arrays.asList(CATEGORY_CODE), productToCategoryCodeList.get(PRODUCT_SKU));
    assertEquals(Arrays.asList(CATEGORY_CODE, SALES_CATALOG), productToCategoryCodeList.get(PRODUCT_SKU_1));
  }

  @Test
  public void fetchCategoryCodeListIgnoreSaleTest() {
    Set<String> categoryCodeList = new HashSet<>();
    Map<String, List<String>> productToCategoryCodeList = new HashMap<>();
    CommonUtil.fetchCategoryCodeList(product1, categoryCodeList, productToCategoryCodeList, Constants.MASTER);
    assertEquals(1, categoryCodeList.size());
    assertEquals(List.of(CATEGORY_CODE), productToCategoryCodeList.get(PRODUCT_SKU_1));
  }

  @Test
  public void fetchCategoryCodeListIgnoreMasterTest() {
    Set<String> categoryCodeList = new HashSet<>();
    Map<String, List<String>> productToCategoryCodeList = new HashMap<>();
    CommonUtil.fetchCategoryCodeList(product1, categoryCodeList, productToCategoryCodeList, Constants.SALES);
    assertEquals(1, categoryCodeList.size());
    assertEquals(List.of(SALES_CATALOG), productToCategoryCodeList.get(PRODUCT_SKU_1));
  }

  @Test
  public void fetchCategoryCodeList_nullMasterCatalogTest() {
    Set<String> categoryCodeList = new HashSet<>();
    Map<String, List<String>> productToCategoryCodeList = new HashMap<>();
    product.setMasterCatalog(null);
    item.setCategoryCode(CATEGORY_CODE);
    CommonUtil.fetchCategoryCodeList(product, categoryCodeList, productToCategoryCodeList, Constants.ALL);
    assertEquals(Arrays.asList(CATEGORY_CODE), productToCategoryCodeList.get(PRODUCT_SKU));
  }

  @Test
  public void fetchCategoryCodeList_nullMasterCatalogMasterDataTest() {
    Set<String> categoryCodeList = new HashSet<>();
    Map<String, List<String>> productToCategoryCodeList = new HashMap<>();
    product.setMasterCatalog(null);
    product.setMasterDataProduct(null);
    CommonUtil.fetchCategoryCodeList(product, categoryCodeList, productToCategoryCodeList, Constants.ALL);
    assertTrue(CollectionUtils.isEmpty(categoryCodeList));
  }

  @Test
  public void getItemSkusNotFoundInFetchTest() {
    List<String> result =
        CommonUtil.getItemSkusNotFoundInFetch(Arrays.asList(item), Arrays.asList(SolrFieldNames.ITEM_SKU, ITEM_NAME));
    assertEquals(ITEM_NAME, result.get(0));
  }

  @Test
  public void getWholeSalePriceActivatedTest() {
    item.setWholesalePriceExists(true);
    item.setActivePromoBundlings(ImmutableSet.of(Constants.WHOLESALE_PRICE));
    Boolean b = CommonUtil.getWholeSalePriceActivated(item);
    assertTrue(b.booleanValue());
  }

  @Test
  public void getWholeSalePriceActivatedFalseTest() {
    item.setWholesalePriceExists(true);
    item.setActivePromoBundlings(new HashSet<>());
    Boolean b = CommonUtil.getWholeSalePriceActivated(item);
    assertFalse(b.booleanValue());
  }

  @Test
  public void getWholeSalePriceActivatedNullPromoBundlingTest() {
    item.setWholesalePriceExists(true);
    item.setActivePromoBundlings(null);
    Boolean b = CommonUtil.getWholeSalePriceActivated(item);
    assertFalse(b.booleanValue());
  }

  @Test
  public void getWholeSalePriceActivatedWholeSalePriceExistsFalseTest() {
    item.setWholesalePriceExists(false);
    item.setActivePromoBundlings(null);
    Boolean b = CommonUtil.getWholeSalePriceActivated(item);
    assertNull(b);
  }

  @Test
  public void getWholeSalePriceActivatedL5Test() {
    itemPickupPoint.setWholesalePriceExists(true);
    itemPickupPoint.setActivePromoBundlings(ImmutableSet.of(Constants.WHOLESALE_PRICE));
    Boolean b = CommonUtil.getWholeSalePriceActivated(itemPickupPoint);
    assertTrue(b.booleanValue());
  }

  @Test
  public void getWholeSalePriceActivatedFalseL5Test() {
    itemPickupPoint.setWholesalePriceExists(true);
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    Boolean b = CommonUtil.getWholeSalePriceActivated(itemPickupPoint);
    assertFalse(b.booleanValue());
  }

  @Test
  public void getWholeSalePriceActivatedNullPromoBundlingL5Test() {
    itemPickupPoint.setWholesalePriceExists(true);
    itemPickupPoint.setActivePromoBundlings(null);
    Boolean b = CommonUtil.getWholeSalePriceActivated(itemPickupPoint);
    assertFalse(b.booleanValue());
  }

  @Test
  public void getWholeSalePriceActivatedWholeSalePriceExistsFalseL5Test() {
    itemPickupPoint.setWholesalePriceExists(false);
    itemPickupPoint.setActivePromoBundlings(null);
    Boolean b = CommonUtil.getWholeSalePriceActivated(itemPickupPoint);
    assertNull(b);
  }

  @Test
  public void setStatusOnFailure() {
    ProductRetryEventPublish productRetryEventPublish = ProductRetryEventPublish.builder().retryCount(4).build();
    CommonUtil.setStatusOnFailure(productRetryEventPublish, new ArrayList<>(), 3);
  }

  @Test
  public void getOfflinePriceAndCncUpdateSolrInputDocumentTest() {
    SolrInputDocument input =
        CommonUtil.getOfflinePriceAndCncUpdateSolrInputDocument(Arrays.asList(OFFLINE_PRICE_REMOVE),
            Arrays.asList(OFFLINE_PRICE_REMOVE), item);
    assertEquals(item.getItemSku(), input.getFieldValue(SolrFieldNames.ID));
    assertFalse((Boolean) input.getFieldValue(SolrFieldNames.CNC_ACTIVE));
  }

  @Test
  public void getOfflinePriceAndCncUpdateSolrInputDocument_emptyAddPricessTest() {
    SolrInputDocument input = CommonUtil.getOfflinePriceAndCncUpdateSolrInputDocument(Collections.EMPTY_LIST,
        Arrays.asList(OFFLINE_PRICE_REMOVE), item);
    assertEquals(item.getItemSku(), input.getFieldValue(SolrFieldNames.ID));
    assertFalse((Boolean) input.getFieldValue(SolrFieldNames.CNC_ACTIVE));
  }

  @Test
  public void getOfflinePriceAndCncUpdateSolrInputDocument_emptyRemovePricessTest() {
    SolrInputDocument input =
        CommonUtil.getOfflinePriceAndCncUpdateSolrInputDocument(Arrays.asList(OFFLINE_PRICE_REMOVE),
            Collections.EMPTY_LIST, item);
    assertEquals(item.getItemSku(), input.getFieldValue(SolrFieldNames.ID));
    assertFalse((Boolean) input.getFieldValue(SolrFieldNames.CNC_ACTIVE));
  }

  @Test
  public void checkIfUsernameIsExcludedEmptyTest() {
    boolean isExcluded = CommonUtil.checkIfUsernameIsExcluded(USERNAME, StringUtils.EMPTY);
    assertFalse(isExcluded);
  }

  @Test
  public void checkIfUsernameIsUserNameNullTest() {
    assertFalse(CommonUtil.checkIfUsernameIsExcluded(null, StringUtils.EMPTY));
  }

  @Test
  public void checkIfUsernameIsUserNameEmptyTest() {
    assertFalse(CommonUtil.checkIfUsernameIsExcluded(StringUtils.EMPTY, StringUtils.EMPTY));
  }

  @Test
  public void checkIfUsernameIsExcludedNonEmptyTest() {
    boolean isExcluded = CommonUtil.checkIfUsernameIsExcluded(USERNAME, Constants.ALL);
    assertTrue(isExcluded);
  }

  @Test
  public void checkIfUsernameIsExcludedNonEmptyNoneTest() {
    boolean isExcluded = CommonUtil.checkIfUsernameIsExcluded(USERNAME, Constants.NONE);
    assertFalse(isExcluded);
  }

  @Test
  public void checkIfUsernameIsExcludedNonEmptySpecificTest() {
    boolean isExcluded = CommonUtil.checkIfUsernameIsExcluded(USERNAME, Constants.DEFAULT_USERNAME);
    assertFalse(isExcluded);
  }

  @Test
  public void checkIfUsernameIsExcludedNonEmptySpecificWithCommaTest() {
    boolean isExcluded = CommonUtil.checkIfUsernameIsExcluded(USERNAME, X_PRODUCT_INTEGRATOR_SYSTEM_USERNAME);
    assertTrue(isExcluded);
  }

  @Test
  public void checkIfUsernameIsExcludedNonEmptySpecificWithCommaIntegratorTest() {
    boolean isExcluded =
        CommonUtil.checkIfUsernameIsExcluded(X_PRODUCT_INTEGRATOR, X_PRODUCT_INTEGRATOR_SYSTEM_USERNAME);
    assertTrue(isExcluded);
  }

  @Test
  public void toProductL3SolrReindexStatuses() {
    List<ProductL3SolrReindexStatus> list =
        CommonUtil.toProductL3SolrReindexStatuses(new HashSet(Arrays.asList(PRODUCT_SKU)), STORE_ID,
            ProductReindexStatus.REINDEX_PENDING_L3);
    assertEquals(list.size(), 1);
    assertEquals(ProductReindexStatus.REINDEX_PENDING_L3, list.get(0).getProductReindexStatus());
  }

  @Test
  public void setActiveProductDetailVoTest() {
    ProductSolr productSolr1 = new ProductSolr();
    productSolr1.setProductSku(PRODUCT_SKU);
    ProductSolr productSolr2 = new ProductSolr();
    productSolr2.setProductSku(PRODUCT_SKU_1);
    Map<String, List<ItemNameSkuVO>> itemSkuMap =
        ImmutableMap.of(PRODUCT_SKU, Arrays.asList(new ItemNameSkuVO(null, ITEM_SKU)));
    Map<String, List<ItemPickupPoint>> itemCountMap =
        ImmutableMap.of(PRODUCT_SKU, Arrays.asList(new ItemPickupPoint()));
    List<ActiveProductDetailVo> activeProductDetailVos = new ArrayList<>();

    CommonUtil.setActiveProductDetailVo(activeProductDetailVos, Arrays.asList(productSolr1, productSolr2), itemSkuMap,
        itemCountMap, new HashMap<>(), solrStringDelimiter);

    assertEquals(PRODUCT_SKU, activeProductDetailVos.get(0).getProductSku());
    assertEquals(PRODUCT_SKU_1, activeProductDetailVos.get(1).getProductSku());
    assertEquals(ITEM_SKU, activeProductDetailVos.get(0).getItemDetailVOList().get(0).getItemSku());
    assertEquals(1, activeProductDetailVos.get(0).getItemCount().intValue());
    assertTrue(activeProductDetailVos.get(1).getItemDetailVOList().isEmpty());
    assertEquals(0, activeProductDetailVos.get(1).getItemCount().intValue());
  }

  @Test
  public void setActiveProductDetailVoSingleVariantAndSingleL5Test() {
    ProductSolr productSolr1 = new ProductSolr();
    productSolr1.setProductSku(PRODUCT_SKU);
    productSolr1.setL5Count(1);
    ProductSolr productSolr2 = new ProductSolr();
    productSolr2.setProductSku(PRODUCT_SKU_1);
    productSolr2.setL5Count(2);
    Map<String, List<ItemNameSkuVO>> itemSkuMap =
        ImmutableMap.of(PRODUCT_SKU, Arrays.asList(new ItemNameSkuVO(null, ITEM_SKU)));
    Map<String, List<ItemPickupPoint>> itemCountMap =
        ImmutableMap.of(PRODUCT_SKU, Arrays.asList(new ItemPickupPoint()));
    List<ActiveProductDetailVo> activeProductDetailVos = new ArrayList<>();

    CommonUtil.setActiveProductDetailVo(activeProductDetailVos, Arrays.asList(productSolr1, productSolr2), itemSkuMap,
        itemCountMap, ImmutableMap.of(PRODUCT_SKU, item), solrStringDelimiter);

    assertEquals(PRODUCT_SKU, activeProductDetailVos.get(0).getProductSku());
    assertEquals(PRODUCT_SKU_1, activeProductDetailVos.get(1).getProductSku());
    assertEquals(ITEM_SKU, activeProductDetailVos.get(0).getItemDetailVOList().get(0).getItemSku());
    assertEquals(1, activeProductDetailVos.get(0).getItemCount().intValue());
    assertTrue(activeProductDetailVos.get(1).getItemDetailVOList().isEmpty());
    assertEquals(0, activeProductDetailVos.get(1).getItemCount().intValue());
  }

  @Test
  public void setActiveProductDetailVoSingleVariantAndSingleL5Test2() {
    ProductSolr productSolr1 = new ProductSolr();
    productSolr1.setProductSku(PRODUCT_SKU);
    productSolr1.setL5Count(1);
    ProductSolr productSolr2 = new ProductSolr();
    productSolr2.setProductSku(PRODUCT_SKU_1);
    productSolr2.setL5Count(2);
    Map<String, List<ItemNameSkuVO>> itemSkuMap =
        ImmutableMap.of(PRODUCT_SKU, Arrays.asList(new ItemNameSkuVO(null, ITEM_SKU)));
    Map<String, List<ItemPickupPoint>> itemCountMap =
        ImmutableMap.of(PRODUCT_SKU_1, Arrays.asList(new ItemPickupPoint()));
    List<ActiveProductDetailVo> activeProductDetailVos = new ArrayList<>();

    CommonUtil.setActiveProductDetailVo(activeProductDetailVos, Arrays.asList(productSolr1, productSolr2), itemSkuMap,
        itemCountMap, ImmutableMap.of(PRODUCT_SKU, item), solrStringDelimiter);

    assertEquals(PRODUCT_SKU, activeProductDetailVos.get(0).getProductSku());
    assertEquals(PRODUCT_SKU_1, activeProductDetailVos.get(1).getProductSku());
    assertEquals(ITEM_SKU, activeProductDetailVos.get(0).getItemDetailVOList().get(0).getItemSku());
    assertTrue(activeProductDetailVos.get(1).getItemDetailVOList().isEmpty());
  }

  @Test
  public void toItemPickupPointListTest() {
    item.setPickupPointCode(PICKUP_POINT_CODE);
    List<ItemPickupPoint> result = CommonUtil.toItemPickupPointList(Collections.singletonList(item));
    assertNotNull(result.get(0));
    assertEquals(SolrFieldNames.ITEM_SKU, result.get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, result.get(0).getPickupPointCode());
    assertFalse(result.get(0).isCncActive());
    assertTrue(result.get(0).isDelivery());
  }

  @Test
  public void toItemPickupPointList_emptyPickupPointCodeTest() {
    List<ItemPickupPoint> result = CommonUtil.toItemPickupPointList(Collections.singletonList(item));
    assertNotNull(result.get(0));
    assertEquals(SolrFieldNames.ITEM_SKU, result.get(0).getOfflineItemId());
    assertFalse(result.get(0).isCncActive());
    assertTrue(result.get(0).isDelivery());
  }

  @Test
  public void toItemPickupPointOnConflictTest() {
    ItemPickupPoint itemPickupPointOnConflict = CommonUtil.toItemPickupPointOnConflict(item, new ItemPickupPoint());
    assertNotNull(itemPickupPointOnConflict);
    assertTrue(itemPickupPointOnConflict.isDelivery());
    assertFalse(itemPickupPointOnConflict.getItemViewConfig().stream().findFirst().get().isBuyable());
    assertFalse(itemPickupPointOnConflict.getItemViewConfig().stream().findFirst().get().isDiscoverable());
  }

  @Test
  public void toItemPickupPointOnConflict_itemPriceLowerTest() {
    item.setPrice(Collections.singleton(itemPrice));
    ItemPickupPoint itemPickupPointOnConflict = CommonUtil.toItemPickupPointOnConflict(item, itemPickupPoint);
    assertNotNull(itemPickupPointOnConflict);
    assertTrue(itemPickupPointOnConflict.isDelivery());
    assertFalse(itemPickupPointOnConflict.getItemViewConfig().stream().findFirst().get().isBuyable());
    assertFalse(itemPickupPointOnConflict.getItemViewConfig().stream().findFirst().get().isDiscoverable());
    assertEquals(itemPickupPointOnConflict.getPrice().stream().findFirst().get().getOfferPrice(),
        itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice(), 0);
    assertEquals(itemPickupPointOnConflict.getPrice().stream().findFirst().get().getListPrice(),
        itemPickupPoint.getPrice().stream().findFirst().get().getListPrice(), 0);
  }

  @Test
  public void toItemPickupPointOnConflict_itemPriceOfferLowerTest() {
    itemPrice.setListPrice(10000);
    item.setPrice(Collections.singleton(itemPrice));
    ItemPickupPoint itemPickupPointOnConflict = CommonUtil.toItemPickupPointOnConflict(item, itemPickupPoint);
    assertNotNull(itemPickupPointOnConflict);
    assertTrue(itemPickupPointOnConflict.isDelivery());
    assertFalse(itemPickupPointOnConflict.getItemViewConfig().stream().findFirst().get().isBuyable());
    assertFalse(itemPickupPointOnConflict.getItemViewConfig().stream().findFirst().get().isDiscoverable());
    assertEquals(itemPickupPointOnConflict.getPrice().stream().findFirst().get().getOfferPrice(),
        itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice(), 0);
    assertEquals(itemPickupPointOnConflict.getPrice().stream().findFirst().get().getListPrice(),
        itemPickupPoint.getPrice().stream().findFirst().get().getListPrice(), 0);
  }

  @Test
  public void toItemPickupPointOnConflict_emptyL5PriceTest() {
    item.setPrice(Collections.singleton(itemPrice));
    ItemPickupPoint itemPickupPointOnConflict = CommonUtil.toItemPickupPointOnConflict(item, new ItemPickupPoint());
    assertNotNull(itemPickupPointOnConflict);
    assertTrue(itemPickupPointOnConflict.isDelivery());
    assertFalse(itemPickupPointOnConflict.getItemViewConfig().stream().findFirst().get().isBuyable());
    assertFalse(itemPickupPointOnConflict.getItemViewConfig().stream().findFirst().get().isDiscoverable());
  }

  @Test
  public void toItemPickupPointOnConflict_itemPriceHigherTest() {
    itemPrice.setOfferPrice(100000);
    itemPrice.setListPrice(100000);
    item.setPrice(Collections.singleton(itemPrice));
    ItemPickupPoint itemPickupPointOnConflict = CommonUtil.toItemPickupPointOnConflict(item, itemPickupPoint);
    assertNotNull(itemPickupPointOnConflict);
    assertTrue(itemPickupPointOnConflict.isDelivery());
    assertFalse(itemPickupPointOnConflict.getItemViewConfig().stream().findFirst().get().isBuyable());
    assertFalse(itemPickupPointOnConflict.getItemViewConfig().stream().findFirst().get().isDiscoverable());
    assertEquals(itemPickupPointOnConflict.getPrice().stream().findFirst().get().getOfferPrice(),
        itemPrice.getOfferPrice(), 0);
    assertEquals(itemPickupPointOnConflict.getPrice().stream().findFirst().get().getListPrice(),
        itemPrice.getListPrice(), 0);
  }

  @Test
  public void setItemChangeToItemPickupPointTest() {
    CommonUtil.setItemChangeToItemPickupPoint(itemChange, itemPickupPoint);
    assertEquals(itemPickupPoint.getPrice().stream().findFirst().get().getListPrice(), 1, 0);
    assertEquals(itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice(), 1, 0);
    assertFalse(itemPickupPoint.isWholesalePriceExists());
    assertFalse(itemPickupPoint.isPromoBundling());
    assertFalse(itemPickupPoint.isMerchantPromoDiscount());
    assertFalse(itemPickupPoint.isFlashSaleActive());
  }

  @Test
  public void getOfflineItemsByItemPickupPointTest() {
    List<OfflineItem> response = CommonUtil.getOfflineItemsByItemPickupPoint(itemPickupPointList, false, null);
    assertEquals(itemPickupPointList.size(), response.size());
    assertEquals(itemPickupPointList.get(0).getPickupPointCode(), response.get(0).getPickupPointCode());
  }

  @Test
  public void constructItemSummaryResponseTest() {
    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build();
    itemPickupPoint.setCncActive(true);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ITEM_SKU);
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);

    ItemSummaryResponseVO itemSummaryResponseVO =
        CommonUtil.constructItemSummaryResponse(ImmutableMap.of(ITEM_SKU, new HashSet<>()),
            ImmutableMap.of(ITEM_SKU, 1.0), Collections.emptyMap(),
            ImmutableMap.of(ITEM_SKU, item), productAndItemSolr, itemPickupPoint);

    assertNotNull(itemSummaryResponseVO);
    assertTrue(itemSummaryResponseVO.isCncActive());
  }

  @Test
  public void constructItemSummaryResponse_PreOrderActive_Test() {
    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build();
    itemPickupPoint.setCncActive(true);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ITEM_SKU);
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);
    productAndItemSolr.setProductScoreTotal(1.0);
    productAndItemSolr.setPristineId(SolrFieldNames.PRISTINE_ID);
    productAndItemSolr.setProductType(ProductType.REGULAR.name());
    productAndItemSolr.setProductSku(ITEM_SKU);

    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderDate(DateUtils.addDays(new Date(), 2));

    ItemSummaryResponseVO itemSummaryResponseVO =
        CommonUtil.constructItemSummaryResponse(ImmutableMap.of(ITEM_SKU, new HashSet<>()),
            ImmutableMap.of(), ImmutableMap.of(ITEM_SKU, preOrder), ImmutableMap.of(ITEM_SKU, item),
            productAndItemSolr, itemPickupPoint);

    assertNotNull(itemSummaryResponseVO);
    assertTrue(itemSummaryResponseVO.isPreOrder());
    assertNotNull(itemSummaryResponseVO.getPreOrderDate());
  }

  @Test
  public void constructItemSummaryResponse_PreOrderInActive_Test() {
    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build();
    itemPickupPoint.setCncActive(true);
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ITEM_SKU);
    productAndItemSolr.setPickupPointCode(PICKUP_POINT_CODE);
    productAndItemSolr.setProductType(ProductType.REGULAR.name());
    productAndItemSolr.setProductSku(ITEM_SKU);

    Map<String, PreOrder> preOrderDetails = new HashMap<>(1);
    preOrderDetails.put(ITEM_SKU, null);

    ItemSummaryResponseVO itemSummaryResponseVO =
        CommonUtil.constructItemSummaryResponse(ImmutableMap.of(ITEM_SKU, new HashSet<>()),
            ImmutableMap.of(ITEM_SKU, 1.0), preOrderDetails, ImmutableMap.of(ITEM_SKU, item),
            productAndItemSolr, itemPickupPoint);

    assertNotNull(itemSummaryResponseVO);
    assertFalse(itemSummaryResponseVO.isPreOrder());
    assertNull(itemSummaryResponseVO.getPreOrderDate());
  }

  @Test
  public void getPriceEditDisabledReasonsItemPickupPointTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(CAMPAIGN_CODE);
    discountPrice.setStartDateTime(DateUtils.addDays(new Date(), -1));
    discountPrice.setEndDateTime(DateUtils.addDays(new Date(), 2));

    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(true);
    itemPickupPoint.setPrice(ImmutableSet.of(price));

    Set<String> promos = CommonUtil.getPriceEditDisabledReasons(itemPickupPoint);

    assertTrue(promos.containsAll(
        Arrays.asList(PromoType.PROMO_DISCOUNT.getDescription(), PromoType.CAMPAIGN.getDescription())));
  }

  @Test
  public void getPriceEditDisabledReasonsItemPickupPointInvalidEndDateTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(CAMPAIGN_CODE);
    discountPrice.setStartDateTime(DateUtils.addDays(new Date(), -1));
    discountPrice.setEndDateTime(DateUtils.addDays(new Date(), -1));

    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(true);
    itemPickupPoint.setPrice(ImmutableSet.of(price));

    Set<String> promos = CommonUtil.getPriceEditDisabledReasons(itemPickupPoint);

    assertTrue(promos.containsAll(Arrays.asList(PromoType.PROMO_DISCOUNT.getDescription())));
  }

  @Test
  public void getPriceEditDisabledReasonsItemPickupPointInvalidStartDateTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(CAMPAIGN_CODE);
    discountPrice.setStartDateTime(DateUtils.addDays(new Date(), 2));
    discountPrice.setEndDateTime(DateUtils.addDays(new Date(), 2));

    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(true);
    itemPickupPoint.setPrice(ImmutableSet.of(price));

    Set<String> promos = CommonUtil.getPriceEditDisabledReasons(itemPickupPoint);

    assertTrue(promos.containsAll(Arrays.asList(PromoType.PROMO_DISCOUNT.getDescription())));
  }


  @Test
  public void getPriceEditDisabledReasonsItemPickupPointCampaignCodeNullTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setStartDateTime(DateUtils.addDays(new Date(), -1));
    discountPrice.setEndDateTime(DateUtils.addDays(new Date(), 2));

    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(true);
    itemPickupPoint.setPrice(ImmutableSet.of(price));

    Set<String> promos = CommonUtil.getPriceEditDisabledReasons(itemPickupPoint);

    assertTrue(promos.containsAll(Arrays.asList(PromoType.PROMO_DISCOUNT.getDescription())));
  }

  @Test
  public void getPriceEditDisabledReasonsItemPickupPointMerchantPromoFalseTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setStartDateTime(DateUtils.addDays(new Date(), -1));
    discountPrice.setEndDateTime(DateUtils.addDays(new Date(), 2));

    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(ImmutableSet.of(price));

    Set<String> promos = CommonUtil.getPriceEditDisabledReasons(itemPickupPoint);

    assertTrue(promos.isEmpty());
  }

  @Test
  public void getPriceEditDisabledReasonsItemPickupPointNoPromoTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();

    Set<String> promos = CommonUtil.getPriceEditDisabledReasons(itemPickupPoint);

    assertTrue(promos.isEmpty());
  }

  @Test
  public void generateOfflineItemIdTest() {
    String offlineItemId = CommonUtil.generateOfflineItemId(ITEM_SKU, PICKUP_POINT_CODE);
    assertEquals(ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_CODE, offlineItemId);
  }

  @Test
  public void convertToOfflineItemChangeWithHistoryTest() {
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setOfflineItemHistoryDetail(new OfflineItemHistoryDetailVO());
    Map<String, Item> itemMap = ImmutableMap.of(itemPickupPoint.getItemSku(), item);

    OfflineItemChange offlineItemChange = CommonUtil.convertToOfflineItemChange(itemPickupPoint, itemMap);
    assertEquals(MERCHANT_CODE, offlineItemChange.getMerchantCode());
  }

  @Test
  public void convertToOfflineItemChangeWithoutHistoryTest() {
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setOfflineItemHistoryDetail(null);
    Map<String, Item> itemMap = ImmutableMap.of(itemPickupPoint.getItemSku(), item);

    OfflineItemChange offlineItemChange = CommonUtil.convertToOfflineItemChange(itemPickupPoint, itemMap);
    assertEquals(MERCHANT_CODE, offlineItemChange.getMerchantCode());
  }

  @Test
  public void getSolrInputDocumentForAtomicUpdateOnItemViewConfigChangesTest() {
    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemSku(ITEM_SKU).itemViewConfig(ImmutableSet.of(new ItemViewConfig())).build();
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentForAtomicUpdateOnItemViewConfigChanges(itemPickupPoint, solrStringDelimiter);
    assertEquals(ITEM_SKU, solrInputDocument.getFieldValue(SolrFieldNames.ID));
  }

  @Test
  public void checkItemChangedItemDataChangedTrueTest() {
    boolean result = CommonUtil.checkItemChanged(true, false, false, false, false);
    assertTrue(result);
  }

  @Test
  public void checkItemChangedShippingChangedTrueTest() {
    boolean result = CommonUtil.checkItemChanged(false, true, false, false, false);
    assertTrue(result);
  }

  @Test
  public void checkItemChangedOff2OnChangedTrueTest() {
    boolean result = CommonUtil.checkItemChanged(false, false, true, false, false);
    assertTrue(result);
  }

  @Test
  public void checkItemChangedPriceChangedTrueTest() {
    boolean result = CommonUtil.checkItemChanged(false, false, false, true, false);
    assertTrue(result);
  }

  @Test
  public void checkItemChangedPromoBundlingTrueTest() {
    boolean result = CommonUtil.checkItemChanged(false, false, false, false, true);
    assertTrue(result);
  }

  @Test
  public void checkItemChanedNoChangeTest() {
    boolean result = CommonUtil.checkItemChanged(false, false, false, false, false);
    assertFalse(result);
  }

  @Test
  public void getL3FieldsAndValuesForUpdatedCNCActivaAndPickupPointAndVariantTest() {
    Map<String, Object> fieldsAndValues =
        CommonUtil.getFieldsAndValuesForUpdatedCNCActivaAndPickupPointAndVariant(itemPickupPointList);
    assertEquals(((Map) (fieldsAndValues.get(SolrFieldNames.L5_COUNT))).get(SolrConstants.SET_CLAUSE),
        itemPickupPointList.size());
  }

  @Test
  public void getL3SolrInputDocumentsForUpdatedTest() {
    CommonUtil.getL3SolrInputDocumentsForUpdatedCNCActivaAndPickupPointAndVariant(PRODUCT_SKU, Arrays.asList(item),
        itemPickupPointList);
  }

  @Test
  public void getDistinctPickupPointCodesTest() {
    Set<String> result = CommonUtil.getDistinctPickupPointCodes(Collections.singletonList(itemPickupPoint));
    assertTrue(result.contains(PICKUP_POINT_CODE));
  }

  @Test
  public void getDistinctPickupPointCodes_nullTest() {
    Set<String> result = CommonUtil.getDistinctPickupPointCodes(null);
    assertTrue(CollectionUtils.isEmpty(result));
  }

  @Test
  public void isDefiningOrVariantCreatingTest() {
    productAttributeDomainEventModel.setAttribute(null);
    boolean result = CommonUtil.isDefiningOrVariantCreating(productAttributeDomainEventModel);
    assertFalse(result);
  }

  @Test
  public void isDefiningOrVariantCreatingNotDefiningOrVariantCreatingTest() {
    boolean result = CommonUtil.isDefiningOrVariantCreating(productAttributeDomainEventModel);
    assertFalse(result);
  }

  @Test
  public void isDefiningOrVariantCreatingDefiningAttributeTest() {
    productAttributeDomainEventModel.getAttribute().setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE.name());
    boolean result = CommonUtil.isDefiningOrVariantCreating(productAttributeDomainEventModel);
    assertTrue(result);
  }

  @Test
  public void isDefiningOrVariantCreatingAttributeTest() {
    productAttributeDomainEventModel.getAttribute().setVariantCreation(true);
    boolean result = CommonUtil.isDefiningOrVariantCreating(productAttributeDomainEventModel);
    assertTrue(result);
  }

  @Test
  public void isDefiningOrVariantCreatingTest1() {
    productItemAttributeValueResponse.setAttributeResponse(null);
    boolean result = CommonUtil.isDefiningOrVariantCreating(productItemAttributeValueResponse);
    assertFalse(result);
  }

  @Test
  public void isDefiningOrVariantCreatingNotDefiningOrVariantCreating1Test() {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    boolean result = CommonUtil.isDefiningOrVariantCreating(productItemAttributeValueResponse);
    assertFalse(result);
  }

  @Test
  public void isDefiningOrVariantCreatingDefiningAttribute1Test() {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    boolean result = CommonUtil.isDefiningOrVariantCreating(productItemAttributeValueResponse);
    assertTrue(result);
  }

  @Test
  public void isDefiningOrVariantCreatingAttribute1Test() {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse.setVariantCreation(true);
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    boolean result = CommonUtil.isDefiningOrVariantCreating(productItemAttributeValueResponse);
    assertTrue(result);
  }

  @Test
  public void toItemViewConfigWithPickupPointCodeEventTest() {
    ItemViewConfigWithArchivedChangeEvent itemViewConfigWithArchivedChangeEvent =
        CommonUtil.toItemViewConfigWithArchivedChangeEvent(item, itemPickupPoint, false);
    assertEquals(ITEM_SKU, itemViewConfigWithArchivedChangeEvent.getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemViewConfigWithArchivedChangeEvent.getPickupPointCode());
    assertTrue(itemViewConfigWithArchivedChangeEvent.isArchived());
  }

  @Test
  public void toItemViewConfigWithArchivedChangeEvent_cncForWarehouseTrueTest() {
    itemPickupPointVo.setItemSku(ITEM_SKU);
    itemPickupPointVo.setPickupPointCode(PICKUP_POINT_CODE);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel("CNC");
    itemPickupPointVo.setItemViewConfig(Collections.singleton(itemViewConfig));
    ItemViewConfigWithArchivedChangeEvent itemViewConfigWithArchivedChangeEvent =
        CommonUtil.toItemViewConfigWithArchivedChangeEvent(itemVo, itemPickupPointVo, true);
    Assertions.assertEquals(ITEM_SKU, itemViewConfigWithArchivedChangeEvent.getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemViewConfigWithArchivedChangeEvent.getPickupPointCode());
    Assertions.assertEquals(1, itemViewConfigWithArchivedChangeEvent.getItemViewConfigs().size());
  }

  @Test
  public void convertToItemPriceChangeEventModelTest() {
    ParentCategory parentCategory = new ParentCategory(CATEGORY_CODE, CATEGORY_CODE);
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, item);
    List<ItemPriceChangeEventModel> priceChangeEventModels =
        CommonUtil.convertToItemPriceChangeEventModel(PRODUCT_CODE, parentCategory, product,
            Arrays.asList(itemPickupPoint), itemMap);
    assertEquals(parentCategory, priceChangeEventModels.get(0).getParentCategory());
    assertEquals(PRODUCT_CODE, priceChangeEventModels.get(0).getProductCode());
    assertEquals(MERCHANT_CODE, priceChangeEventModels.get(0).getMerchantCode());
    assertEquals(ITEM_SKU, priceChangeEventModels.get(0).getItemSku());
    assertNotNull(priceChangeEventModels.get(0).getPristineId());
  }

  @Test
  public void convertToItemPriceChangeEventModelPristineIdTest() {
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    item.setPristineDataItem(pristineDataItem);
    ParentCategory parentCategory = new ParentCategory(CATEGORY_CODE, CATEGORY_CODE);
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, item);
    List<ItemPriceChangeEventModel> priceChangeEventModels =
        CommonUtil.convertToItemPriceChangeEventModel(PRODUCT_CODE, parentCategory, product,
            Arrays.asList(itemPickupPoint), itemMap);
    assertEquals(parentCategory, priceChangeEventModels.get(0).getParentCategory());
    assertEquals(PRODUCT_CODE, priceChangeEventModels.get(0).getProductCode());
    assertEquals(MERCHANT_CODE, priceChangeEventModels.get(0).getMerchantCode());
    assertEquals(ITEM_SKU, priceChangeEventModels.get(0).getItemSku());
    assertEquals(PRISTINE_ID, priceChangeEventModels.get(0).getPristineId());
  }

  @Test
  public void convertToItemPriceChangeEventModelPristineIdNullTest() {
    ParentCategory parentCategory = new ParentCategory(CATEGORY_CODE, CATEGORY_CODE);
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, item);
    List<ItemPriceChangeEventModel> priceChangeEventModels =
        CommonUtil.convertToItemPriceChangeEventModel(PRODUCT_CODE, parentCategory, product,
            Arrays.asList(itemPickupPoint), itemMap);
    assertEquals(parentCategory, priceChangeEventModels.get(0).getParentCategory());
    assertEquals(PRODUCT_CODE, priceChangeEventModels.get(0).getProductCode());
    assertEquals(MERCHANT_CODE, priceChangeEventModels.get(0).getMerchantCode());
    assertEquals(ITEM_SKU, priceChangeEventModels.get(0).getItemSku());
    assertEquals(StringUtils.EMPTY, priceChangeEventModels.get(0).getPristineId());
  }

  @Test
  public void toMerchantPromoDiscountChangeEventWithPickupPointTest() throws Exception {
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setPrice(new HashSet<>(Arrays.asList(new Price())));
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    MerchantPromoDiscountChangeEvent merchantPromoDiscountChangeEvent =
        CommonUtil.toMerchantPromoDiscountChangeEventWithPickupPoint(itemPickupPoint);
    assertEquals(ITEM_SKU, merchantPromoDiscountChangeEvent.getItemSku());
    assertEquals(MERCHANT_CODE, merchantPromoDiscountChangeEvent.getMerchantCode());
    assertEquals(PRODUCT_SKU, merchantPromoDiscountChangeEvent.getProductSku());
    assertEquals(PICKUP_POINT_CODE, merchantPromoDiscountChangeEvent.getPickupPointCode());
  }

  @Test
  public void ItemPickupPointViewConfigChangeEventTest() {
    ItemPickupPoint item = new ItemPickupPoint();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule());
    itemViewConfig.setItemDiscoverableSchedules(new ItemDiscoverableSchedule());
    item.setItemViewConfig(Collections.singleton(itemViewConfig));
    ItemPickupPointViewConfigChangeEvent event = CommonUtil.toItemPickupPointViewConfigChangeEvent(item, false);
    assertTrue(event.getItemViewConfigs().stream().findFirst()
        .map(com.gdn.x.product.domain.event.model.ItemViewConfig::isBuyable).orElse(false));
    assertTrue(event.getItemViewConfigs().stream().findFirst()
        .map(com.gdn.x.product.domain.event.model.ItemViewConfig::isDiscoverable).orElse(false));
  }

  @Test
  public void toItemPickupPointRequestVos() {
    List<ItemPickupPointRequestVo> itemPickupPointRequestVos = CommonUtil.toItemPickupPointRequestVos(
        Arrays.asList(new ItemInfo(ITEM_SKU, PICKUP_POINT_CODE, PICKUP_POINT_CODE_1)));
    assertEquals(itemPickupPointRequestVos.size(), 1);
    assertEquals(itemPickupPointRequestVos.get(0).getItemSku(), ITEM_SKU);
  }

  @Test
  public void getFieldsAndValuesL3ForPromoUpdateTest() {
    Map<String, Object> fieldsAndValues =
        CommonUtil.getFieldsAndValuesL3ForPromoUpdate(new HashMap<>(), Arrays.asList(ITEM_SKU),
            false, false);
    assertNotNull(fieldsAndValues);
  }

  @Test
  public void getFieldsAndValuesL3ForPromoUpdateNonEmptyItemsTest() {
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setPromoBundling(true);
    map.put(PRODUCT_SKU, Arrays.asList(itemPickupPoint));
    Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesL3ForPromoUpdate(map, new ArrayList<>(),
        false, false);
    assertNotNull(fieldsAndValues);
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.PROMO_ITEM_SKUS)).size(), 1);
  }

  @Test
  public void getFieldsAndValuesL3ForPromoUpdateNonEmptyItemsNonPromoTest() {
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setPromoBundling(false);
    map.put(PRODUCT_SKU, Arrays.asList(itemPickupPoint));
    Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesL3ForPromoUpdate(map, new ArrayList<>(),
        false, false);
    assertNotNull(fieldsAndValues);
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.PROMO_ITEM_SKUS)).size(), 1);
  }

  @Test
  public void getSolrInputDocumentL3ForPromoUpdateEmptyItemsTest() {
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentL3ForPromoUpdate(new HashMap<>(), Arrays.asList(ITEM_SKU), PRODUCT_SKU,
            false, false);
    assertNotNull(solrInputDocument);
    assertEquals(solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU), PRODUCT_SKU);
  }

  @Test
  public void getSolrInputDocumentL3ForPromoUpdateNonEmptyItemsTest() {
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setPromoBundling(true);
    map.put(PRODUCT_SKU, Arrays.asList(itemPickupPoint));
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentL3ForPromoUpdate(map, new ArrayList<>(), PRODUCT_SKU,
            false, false);
    assertNotNull(solrInputDocument);
    assertEquals((solrInputDocument.getFieldValues(SolrFieldNames.PROMO_ITEM_SKUS)).size(), 1);
  }

  @Test
  public void getSolrInputDocumentL3ForPromoUpdateNonEmptyItemsNonPromoTest() {
    Map<String, List<ItemPickupPoint>> map = new HashMap<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setPromoBundling(false);
    map.put(PRODUCT_SKU, Arrays.asList(itemPickupPoint));
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentL3ForPromoUpdate(map, new ArrayList<>(), PRODUCT_SKU,
            false, false);
    assertNotNull(solrInputDocument);
    assertEquals((solrInputDocument.getFieldValues(SolrFieldNames.PROMO_ITEM_SKUS)).size(), 1);
  }

  @Test
  public void getSolrInputDocumentL3ForPromoAndWholesaleUpdateTest() {
    List<ItemPickupPoint> map = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setPromoBundling(false);
    map.add(itemPickupPoint);
    SolrInputDocument solrInputDocument = CommonUtil.getSolrInputDocumentL3ForPromoAndWholesaleUpdate(map, PRODUCT_SKU,
        false, false);
    assertNotNull(solrInputDocument);
    assertEquals((solrInputDocument.getFieldValues(SolrFieldNames.PROMO_ITEM_SKUS)).size(), 1);
  }

  @Test
  public void getFieldsAndValuesL3ForPromoAndWholesaleUpdateNullTest() {
    List<ItemPickupPoint> map = new ArrayList<>();
    Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesL3ForPromoAndWholesaleUpdate(map,
        false, false);
    assertEquals(fieldsAndValues.size(), 0);
  }

  @Test
  public void getFieldsAndValuesL3ForPromoAndWholesaleUpdateTest() {
    List<ItemPickupPoint> map = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setPromoBundling(false);
    itemPickupPoint.setMerchantPromoDiscount(false);
    map.add(itemPickupPoint);
    Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesL3ForPromoAndWholesaleUpdate(map,
        false, false);
    assertNotNull(fieldsAndValues);
    assertEquals(((Map) fieldsAndValues.get(SolrFieldNames.PROMO_ITEM_SKUS)).size(), 1);
  }

  @Test
  public void getSolrInputDocumentL3ForPromoAndWholesaleUpdateTrueTest() {
    List<ItemPickupPoint> map = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setWholesalePriceExists(true);
    itemPickupPoint.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    map.add(itemPickupPoint);
    SolrInputDocument solrInputDocument = CommonUtil.getSolrInputDocumentL3ForPromoAndWholesaleUpdate(map, PRODUCT_SKU,
        false, false);
    assertNotNull(solrInputDocument);
    assertEquals((solrInputDocument.getFieldValues(SolrFieldNames.PROMO_ITEM_SKUS)).size(), 1);
  }

  @Test
  public void isItemAddedToWholesalePriceTest() {
    boolean itemAddedToWholesalePrice = CommonUtil.isItemAddedToWholesalePrice(itemPickupPoint);
    assertFalse(itemAddedToWholesalePrice);
  }

  @Test
  public void isItemAddedToWholesalePriceFalseTest() {
    itemPickupPoint.setWholesalePriceExists(true);
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    boolean itemAddedToWholesalePrice = CommonUtil.isItemAddedToWholesalePrice(itemPickupPoint);
    assertFalse(itemAddedToWholesalePrice);
  }

  @Test
  public void isItemAddedToWholesalePriceTrueTest() {
    itemPickupPoint.setWholesalePriceExists(true);
    itemPickupPoint.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    boolean itemAddedToWholesalePrice = CommonUtil.isItemAddedToWholesalePrice(itemPickupPoint);
    assertTrue(itemAddedToWholesalePrice);
  }

  @Test
  public void getSolrInputDocumentL3ForPromoAndWholesaleUpdateEmptyTest() {
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentL3ForPromoAndWholesaleUpdate(new ArrayList<>(), PRODUCT_SKU,
            false, false);
    assertNull(solrInputDocument);
  }

  @Test
  public void getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTOs() {
    List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> responses =
        CommonUtil.getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTOs(Arrays.asList(itemPickupPoint));
    assertEquals(responses.size(), 1);
    assertEquals(responses.get(0).getWebItemSku(), itemPickupPoint.getItemSku());
  }

  @Test
  public void toMasterDataItemImageDTOTest() {
    List<MasterDataItemImageDTO> response = CommonUtil.toMasterDataItemImageDTO(NEW_PRODUCT_MAIN_IMAGE);
    assertTrue(response.get(0).isMainImage());
    assertEquals(NEW_PRODUCT_MAIN_IMAGE, response.get(0).getLocationPath());
  }

  @Test
  public void getSolrDocumentListForAtomicUpdateOnPromoFlagChangeByItemSkus() {
    List<SolrInputDocument> solrInputDocuments =
        CommonUtil.getSolrDocumentListForAtomicUpdateOnPromoFlagChangeByItemSkus(Arrays.asList(ITEM_SKU), true,
            SolrFieldNames.PROMO_BUNDLING, MERCHANT_CODE);
    assertNotNull(solrInputDocuments);
    assertEquals(solrInputDocuments.size(), 1);
  }

  @Test
  public void isPureCncStatusChangeFalseTest() {
    ItemViewConfig request = new ItemViewConfig();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    boolean result = CommonUtil.isPureCNCStatusChange(request, itemViewConfig, false, false);
    assertFalse(result);
  }

  @Test
  public void isPureCncStatusChangeTrueTest() {
    ItemViewConfig request = new ItemViewConfig();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    boolean result = CommonUtil.isPureCNCStatusChange(request, itemViewConfig, true, false);
    assertTrue(result);
  }

  @Test
  public void isPureCncStatusChangeFalse1() {
    ItemViewConfig request = new ItemViewConfig();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    boolean result = CommonUtil.isPureCNCStatusChange(request, itemViewConfig, false, false);
    assertFalse(result);
  }

  @Test
  public void isPureCncStatusChangeTrue1() {
    ItemViewConfig request = new ItemViewConfig();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    boolean result = CommonUtil.isPureCNCStatusChange(request, itemViewConfig, true, false);
    assertTrue(result);
  }

  @Test
  public void isPureCncStatusChangeTrue2() {
    ItemViewConfig request = new ItemViewConfig();
    request.setDiscoverable(false);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(true);
    boolean result = CommonUtil.isPureCNCStatusChange(request, itemViewConfig, true, true);
    assertTrue(result);
  }

  @Test
  public void isPureCncStatusChangefalse2() {
    ItemViewConfig request = new ItemViewConfig();
    request.setDiscoverable(false);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(false);
    boolean result = CommonUtil.isPureCNCStatusChange(request, itemViewConfig, true, true);
    assertFalse(result);
  }

  @Test
  public void getBuyableFromConfigTrueTest() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemBuyableSchedule.setEndDateTime(NEXT_DATE);
    itemBuyableSchedule.setBuyable(true);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    boolean result = CommonUtil.getBuyableFromConfig(itemViewConfig);
    assertTrue(result);
  }

  @Test
  public void getBuyableFromConfigTrueNullTest() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemBuyableSchedule.setEndDateTime(NEXT_DATE);
    itemViewConfig.setItemBuyableSchedules(null);
    boolean result = CommonUtil.getBuyableFromConfig(itemViewConfig);
    assertFalse(result);
  }

  @Test
  public void getBuyableFromConfigTrueTest1() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(false);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemBuyableSchedule.setEndDateTime(NEXT_DATE);
    itemBuyableSchedule.setBuyable(true);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    boolean result = CommonUtil.getBuyableFromConfig(itemViewConfig);
    assertTrue(result);
  }

  @Test
  public void getBuyableFromConfigTrueTest2() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemBuyableSchedule.setEndDateTime(NEXT_DATE);
    itemBuyableSchedule.setBuyable(false);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    boolean result = CommonUtil.getBuyableFromConfig(itemViewConfig);
    assertTrue(result);
  }

  @Test
  public void getBuyableFromConfigTrueTest3() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setStartDateTime(NEXT_DATE);
    itemBuyableSchedule.setEndDateTime(PREVIOUS_DATE);
    itemBuyableSchedule.setBuyable(true);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    boolean result = CommonUtil.getBuyableFromConfig(itemViewConfig);
    assertTrue(result);
  }

  @Test
  public void getBuyableFromConfigFalseTest() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(false);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemBuyableSchedule.setEndDateTime(NEXT_DATE);
    itemBuyableSchedule.setBuyable(false);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    boolean result = CommonUtil.getBuyableFromConfig(itemViewConfig);
    assertFalse(result);
  }

  @Test
  public void getBuyableFromConfigFalseTest1() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setStartDateTime(NEXT_DATE);
    itemBuyableSchedule.setEndDateTime(PREVIOUS_DATE);
    itemBuyableSchedule.setBuyable(true);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    boolean result = CommonUtil.getBuyableFromConfig(itemViewConfig);
    assertTrue(result);
  }

  @Test
  public void getBuyableFromConfigFalseTest2() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(false);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setStartDateTime(NEXT_DATE);
    itemBuyableSchedule.setEndDateTime(PREVIOUS_DATE);
    itemBuyableSchedule.setBuyable(false);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    boolean result = CommonUtil.getBuyableFromConfig(itemViewConfig);
    assertFalse(result);
  }

  @Test
  public void getBuyableFromConfigFalseTest3() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(false);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setStartDateTime(NEXT_DATE);
    itemBuyableSchedule.setEndDateTime(PREVIOUS_DATE);
    itemBuyableSchedule.setBuyable(true);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    boolean result = CommonUtil.getBuyableFromConfig(itemViewConfig);
    assertFalse(result);
  }

  @Test
  public void getBuyableFromConfigFalseTest4() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(false);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemBuyableSchedule.setEndDateTime(PREVIOUS_DATE);
    itemBuyableSchedule.setBuyable(true);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    boolean result = CommonUtil.getBuyableFromConfig(itemViewConfig);
    assertFalse(result);
  }

  @Test
  public void getDiscoverableFromConfigTrueTest() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(true);
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemDiscoverableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemDiscoverableSchedule.setEndDateTime(NEXT_DATE);
    itemDiscoverableSchedule.setDiscoverable(true);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    boolean result = CommonUtil.getDiscoverableFromConfig(itemViewConfig);
    assertTrue(result);
  }

  @Test
  public void getDiscoverableFromConfigTrueNullTest() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemDiscoverableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemDiscoverableSchedule.setEndDateTime(NEXT_DATE);
    itemViewConfig.setItemDiscoverableSchedules(null);
    boolean result = CommonUtil.getDiscoverableFromConfig(itemViewConfig);
    assertFalse(result);
  }

  @Test
  public void getDiscoverableFromConfigTrueTest1() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(false);
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemDiscoverableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemDiscoverableSchedule.setEndDateTime(NEXT_DATE);
    itemDiscoverableSchedule.setDiscoverable(true);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    boolean result = CommonUtil.getDiscoverableFromConfig(itemViewConfig);
    assertTrue(result);
  }

  @Test
  public void getDiscoverableFromConfigTrueTest2() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(true);
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemDiscoverableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemDiscoverableSchedule.setEndDateTime(NEXT_DATE);
    itemDiscoverableSchedule.setDiscoverable(false);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    boolean result = CommonUtil.getDiscoverableFromConfig(itemViewConfig);
    assertTrue(result);
  }

  @Test
  public void getDiscoverableFromConfigTrueTest3() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(true);
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemDiscoverableSchedule.setStartDateTime(NEXT_DATE);
    itemDiscoverableSchedule.setEndDateTime(PREVIOUS_DATE);
    itemDiscoverableSchedule.setDiscoverable(true);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    boolean result = CommonUtil.getDiscoverableFromConfig(itemViewConfig);
    assertTrue(result);
  }

  @Test
  public void getDiscoverableFromConfigFalseTest() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(false);
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemDiscoverableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemDiscoverableSchedule.setEndDateTime(NEXT_DATE);
    itemDiscoverableSchedule.setDiscoverable(false);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    boolean result = CommonUtil.getDiscoverableFromConfig(itemViewConfig);
    assertFalse(result);
  }

  @Test
  public void getDiscoverableFromConfigFalseTest1() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(true);
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemDiscoverableSchedule.setStartDateTime(NEXT_DATE);
    itemDiscoverableSchedule.setEndDateTime(PREVIOUS_DATE);
    itemDiscoverableSchedule.setDiscoverable(true);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    boolean result = CommonUtil.getDiscoverableFromConfig(itemViewConfig);
    assertTrue(result);
  }

  @Test
  public void getDiscoverableFromConfigFalseTest2() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(false);
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemDiscoverableSchedule.setStartDateTime(NEXT_DATE);
    itemDiscoverableSchedule.setEndDateTime(PREVIOUS_DATE);
    itemDiscoverableSchedule.setDiscoverable(false);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    boolean result = CommonUtil.getDiscoverableFromConfig(itemViewConfig);
    assertFalse(result);
  }

  @Test
  public void getDiscoverableFromConfigFalseTest3() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(false);
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemDiscoverableSchedule.setStartDateTime(NEXT_DATE);
    itemDiscoverableSchedule.setEndDateTime(PREVIOUS_DATE);
    itemDiscoverableSchedule.setDiscoverable(true);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    boolean result = CommonUtil.getDiscoverableFromConfig(itemViewConfig);
    assertFalse(result);
  }

  @Test
  public void getDiscoverableFromConfigFalseTest4() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(false);
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    itemDiscoverableSchedule.setStartDateTime(PREVIOUS_DATE);
    itemDiscoverableSchedule.setEndDateTime(PREVIOUS_DATE);
    itemDiscoverableSchedule.setDiscoverable(true);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    boolean result = CommonUtil.getDiscoverableFromConfig(itemViewConfig);
    assertFalse(result);
  }

  @Test
  public void toItemPickupPointMapTest() {
    Map<String, ItemPickupPoint> map = CommonUtil.toItemPickupPointMap(itemPickupPointList);
    assertNotNull(map);
  }

  @Test
  public void isDimensionUpdatedTest() {
    assertFalse(CommonUtil.isDimensionUpdated(productDetailResponse, item));
  }

  @Test
  public void isDimensionUpdatedLengthTest() {
    productDetailResponse.setLength(1.0);
    assertTrue(CommonUtil.isDimensionUpdated(productDetailResponse, item));
  }

  @Test
  public void isDimensionUpdatedWidthTest() {
    item.setLength(1.0);
    productDetailResponse.setLength(1.0);
    productDetailResponse.setWidth(1.0);
    assertTrue(CommonUtil.isDimensionUpdated(productDetailResponse, item));
  }

  @Test
  public void isDimensionUpdatedHeightTest() {
    productDetailResponse.setHeight(1.0);
    assertTrue(CommonUtil.isDimensionUpdated(productDetailResponse, item));
  }

  @Test
  public void isDimensionUpdatedWeightTest() {
    productDetailResponse.setWeight(1.0);
    assertTrue(CommonUtil.isDimensionUpdated(productDetailResponse, item));
  }

  @Test
  public void isDimensionUpdatedShippingWeightTest() {
    item.setLength(1.0);
    item.setWidth(1.0);
    item.setHeight(1.0);
    item.setWeight(1.0);
    productDetailResponse.setLength(1.0);
    productDetailResponse.setWidth(1.0);
    productDetailResponse.setHeight(1.0);
    productDetailResponse.setWeight(1.0);
    productDetailResponse.setShippingWeight(1.0);
    assertTrue(CommonUtil.isDimensionUpdated(productDetailResponse, item));
  }

  @Test
  public void isDimensionUpdatedShippingWeightEqualTest() {
    item.setLength(1.0);
    item.setWidth(1.0);
    item.setHeight(1.0);
    item.setWeight(1.0);
    item.setShippingWeight(1.0);
    productDetailResponse.setLength(1.0);
    productDetailResponse.setWidth(1.0);
    productDetailResponse.setHeight(1.0);
    productDetailResponse.setWeight(1.0);
    productDetailResponse.setShippingWeight(1.0);
    assertFalse(CommonUtil.isDimensionUpdated(productDetailResponse, item));
  }

  @Test
  public void toFieldUpdateRequestVosTest() {
    List<FieldUpdateRequestVo> fieldUpdateRequestVos =
        CommonUtil.toFieldUpdateRequestVos(ProductFieldNames.ACTIVE_PROMO_BUNDLINGS, Collections.emptyList());
    assertEquals(ProductFieldNames.ACTIVE_PROMO_BUNDLINGS, fieldUpdateRequestVos.get(0).getField());
  }

  @Test
  public void toPickupPointUpdateRequestTest() {
    ItemPickupPointListingUpdateRequestVo updateRequestVo = new ItemPickupPointListingUpdateRequestVo();
    updateRequestVo.setProductSku(PRODUCT_SKU);
    PickupPointUpdateRequest response = CommonUtil.toPickupPointUpdateRequest(updateRequestVo);
    assertEquals(PRODUCT_SKU, updateRequestVo.getProductSku());
  }

  @Test
  public void deletePickupPointsPromoAndDiscountDetailsTest() {
    itemPickupPoint.getPrice().stream().findFirst().get().setListOfDiscountPrices(Arrays.asList(new DiscountPrice()));
    CommonUtil.deletePickupPointsPromoAndDiscountDetails(Arrays.asList(itemPickupPoint));
    assertEquals(itemPickupPoint.getPrice().stream().findFirst().get().getListOfDiscountPrices(), new ArrayList<>());
  }

  @Test
  public void getItemPickupPointIdsTest() {
    String id = UUID.randomUUID().toString();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setId(id);

    Set<String> ids = CommonUtil.getItemPickupPointIds(Arrays.asList(itemPickupPoint));

    assertEquals(id, ids.iterator().next());
  }

  @Test
  public void combineListOfUpdatedAndNotUpdatedPickupPointTest() {
    ItemPickupPoint itemPickupPoint1 = ItemPickupPoint.builder().itemSku(ITEM_SKU).build();
    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    ItemPickupPoint itemPickupPoint3 = new ItemPickupPoint();
    itemPickupPoint3.setMarkForDelete(true);

    Set<ItemPickupPoint> itemPickupPointList =
        CommonUtil.combineListOfUpdatedAndNotUpdatedPickupPoint(Arrays.asList(itemPickupPoint1),
            Arrays.asList(itemPickupPoint2, itemPickupPoint3));
    assertEquals(2, itemPickupPointList.size());
  }

  @Test
  public void combineListOfUpdatedAndNotUpdatedPickupPointEmptyTest() {
    Set<ItemPickupPoint> itemPickupPointList =
        CommonUtil.combineListOfUpdatedAndNotUpdatedPickupPoint(new ArrayList<>(), new ArrayList<>());
    assertEquals(0, itemPickupPointList.size());
  }

  @Test
  public void isProductRejectedFalseTest() {
    product.setMarkForDelete(false);
    boolean productRejected = CommonUtil.isProductRejected(product);
    assertFalse(productRejected);
  }

  @Test
  public void isProductRejectedFalseSuspendedTest() {
    product.setMarkForDelete(true);
    product.setSuspended(true);
    boolean productRejected = CommonUtil.isProductRejected(product);
    assertFalse(productRejected);
  }

  @Test
  public void isPriceChanged() {
    boolean result = CommonUtil.isPriceChanged(99.0, 100.0, 89.0, 89.0);
    assertTrue(result);
  }

  @Test
  public void isPriceChanged1() {
    boolean result = CommonUtil.isPriceChanged(99.0, 99.0, 89.0, 99.0);
    assertTrue(result);
  }

  @Test
  public void isPriceChanged2() {
    boolean result = CommonUtil.isPriceChanged(99.0, 99.0, 89.0, 89.0);
    assertFalse(result);
  }

  @Test
  public void isPriceChanged3() {
    boolean result = CommonUtil.isPriceChanged(99.0, 98.0, 89.0, 99.0);
    assertTrue(result);
  }

  @Test
  public void isBasePriceChanged_BothNull_ReturnsFalse() {
    assertFalse(CommonUtil.isBasePriceChanged(null, null));
  }

  @Test
  public void isBasePriceChanged_OneNull_ReturnsTrue() {
    assertTrue(CommonUtil.isBasePriceChanged(99.0, null));
    assertTrue(CommonUtil.isBasePriceChanged(null, 99.0));
  }

  @Test
  public void isBasePriceChanged_DifferentValues_ReturnsTrue() {
    assertTrue(CommonUtil.isBasePriceChanged(99.0, 100.0));
  }

  @Test
  public void isBasePriceChanged_SameValues_ReturnsFalse() {
    assertFalse(CommonUtil.isBasePriceChanged(99.0, 99.0));
  }


  @Test
  public void isManagedFlagChangedTest() {
    boolean result = CommonUtil.isManagedChanged(false, true);
    assertTrue(result);
  }

  @Test
  public void isManagedFlagNotChangedTest() {
    boolean result = CommonUtil.isManagedChanged(false, false);
    assertFalse(result);
  }

  @Test
  public void isB2bBuyableNotChangedTest() {
    boolean result = CommonUtil.isB2bBuyableChanged(false, false);
    assertFalse(result);
  }

  @Test
  public void isB2bBuyableChangedTest() {
    boolean result = CommonUtil.isB2bBuyableChanged(false, true);
    assertTrue(result);
  }

  @Test
  public void isB2bDiscoverableChangedTest() {
    boolean result = CommonUtil.isB2bDiscoverableChanged(false, true);
    assertTrue(result);
  }

  @Test
  public void isB2bDiscoverableNotChangedTest() {
    boolean result = CommonUtil.isB2bDiscoverableChanged(false, false);
    assertFalse(result);
  }

  @Test
  public void isSalesChannleChangedTest() {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    CommonUtil.isSalesChannelChanged(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL, product, auditTrailDtoList);
    assertEquals(1, auditTrailDtoList.size());
  }

  @Test
  public void isSalesChannleChangedTest1() {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    CommonUtil.isSalesChannelChanged(B2B_SELLER_CHANNEL, B2B_SELLER_CHANNEL, product, auditTrailDtoList);
    assertEquals(0, auditTrailDtoList.size());
  }


  @Test
  public void getSalesChannelTest() {
    product.setB2bActivated(true);
    product.setB2cActivated(true);
    String result = CommonUtil.getSalesChannelOfProductForMultiChannelSeller(product, businessPartner);
    assertEquals(SalesChannel.COMBINED_CHANNEL.getName(), result);
  }

  @Test
  public void getSalesChannelTest1() {
    product.setB2cActivated(false);
    product.setB2bActivated(true);
    String result = CommonUtil.getSalesChannelOfProductForMultiChannelSeller(product, businessPartner);
    assertEquals(SalesChannel.B2B_CHANNEL.getName(), result);
  }

  @Test
  public void getSalesChannelTest2() {
    product.setB2cActivated(true);
    product.setB2bActivated(false);
    String result = CommonUtil.getSalesChannelOfProductForMultiChannelSeller(product, businessPartner);
    assertEquals(SalesChannel.B2C_CHANNEL.getName(), result);
  }

  @Test
  public void getSalesChannelTest3() {
    product.setB2cActivated(false);
    product.setB2bActivated(false);
    String result = CommonUtil.getSalesChannelOfProductForMultiChannelSeller(product, businessPartner);
    assertEquals(SalesChannel.NO_CHANNEL.getName(), result);
  }

  @Test
  public void getSalesChannelTestNonMultiChannelSeller() {
    product.setB2cActivated(true);
    product.setB2bActivated(true);
    String result = CommonUtil.getSalesChannelOfProductForMultiChannelSeller(product, new BusinessPartner());
    assertEquals(SalesChannel.NO_CHANNEL.getName(), result);
  }

  @Test
  public void isB2bFieldsGotUpdated() {
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    b2bFieldsVo.setB2bItemViewConfigs(new HashSet<>(Arrays.asList(new ItemViewConfig())));
    B2bFields b2bFields = new B2bFields();
    itemPickupPoint.setB2bFields(b2bFields);
    boolean result = CommonUtil.isB2bFieldsGotUpdated(true, true, true, true, new ArrayList<>(), item, itemPickupPoint,
        ItemPickupPointListingUpdateRequestVo.builder().b2bFieldsVo(b2bFieldsVo).build());
    assertTrue(result);
  }

  @Test
  public void isB2bFieldsNotGotUpdated() {
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    b2bFieldsVo.setB2bItemViewConfigs(new HashSet<>(Arrays.asList(new ItemViewConfig())));
    B2bFields b2bFields = new B2bFields();
    itemPickupPoint.setB2bFields(b2bFields);
    boolean result =
        CommonUtil.isB2bFieldsGotUpdated(false, false, false, false, new ArrayList<>(), item, itemPickupPoint,
            ItemPickupPointListingUpdateRequestVo.builder().b2bFieldsVo(b2bFieldsVo).build());
    assertFalse(result);
  }

  @Test
  public void isB2bFieldsGotUpdated1() {
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    b2bFieldsVo.setB2bItemViewConfigs(new HashSet<>(Arrays.asList(new ItemViewConfig())));
    B2bFields b2bFields = new B2bFields();
    itemPickupPoint.setB2bFields(b2bFields);
    boolean result =
        CommonUtil.isB2bFieldsGotUpdated(true, false, false, false, new ArrayList<>(), item, itemPickupPoint,
            ItemPickupPointListingUpdateRequestVo.builder().b2bFieldsVo(b2bFieldsVo).build());
    assertTrue(result);
  }

  @Test
  public void isB2bFieldsGotUpdated2() {
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    b2bFieldsVo.setB2bItemViewConfigs(new HashSet<>(Arrays.asList(new ItemViewConfig())));
    B2bFields b2bFields = new B2bFields();
    itemPickupPoint.setB2bFields(b2bFields);
    boolean result =
        CommonUtil.isB2bFieldsGotUpdated(false, true, false, false, new ArrayList<>(), item, itemPickupPoint,
            ItemPickupPointListingUpdateRequestVo.builder().b2bFieldsVo(b2bFieldsVo).build());
    assertTrue(result);
  }

  @Test
  public void isB2bFieldsGotUpdated3() {
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    b2bFieldsVo.setB2bItemViewConfigs(new HashSet<>(Arrays.asList(new ItemViewConfig())));
    B2bFields b2bFields = new B2bFields();
    itemPickupPoint.setB2bFields(b2bFields);
    boolean result =
        CommonUtil.isB2bFieldsGotUpdated(false, false, true, false, new ArrayList<>(), item, itemPickupPoint,
            ItemPickupPointListingUpdateRequestVo.builder().b2bFieldsVo(b2bFieldsVo).build());
    assertTrue(result);
  }

  @Test
  public void isB2bFieldsGotUpdated4() {
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    b2bFieldsVo.setB2bItemViewConfigs(new HashSet<>(Arrays.asList(new ItemViewConfig())));
    B2bFields b2bFields = new B2bFields();
    itemPickupPoint.setB2bFields(b2bFields);
    boolean result =
        CommonUtil.isB2bFieldsGotUpdated(false, false, false, true, new ArrayList<>(), item, itemPickupPoint,
            ItemPickupPointListingUpdateRequestVo.builder().b2bFieldsVo(b2bFieldsVo).build());
    assertTrue(result);
  }

  @Test
  public void isB2bFlagUpdatedAtL3LevelTest() {
    boolean result = CommonUtil.isB2bFlagUpdatedAtL3Level(product, true, true);
    assertTrue(result);
  }

  @Test
  public void isB2bFlagNotUpdatedAtL3LevelTest() {
    boolean result = CommonUtil.isB2bFlagUpdatedAtL3Level(product, false, true);
    assertTrue(result);
  }

  @Test
  public void isB2bFlagNotUpdatedAtL3LevelTest1() {
    boolean result = CommonUtil.isB2bFlagUpdatedAtL3Level(product, true, false);
    assertTrue(result);
  }

  @Test
  public void isB2bFlagNotUpdatedAtL3LevelTest3() {
    boolean result = CommonUtil.isB2bFlagUpdatedAtL3Level(product, false, false);
    assertFalse(result);
  }

  @Test
  public void isB2bFlagNotUpdatedAtL3LevelTest2() {
    product.setB2bActivated(true);
    boolean result = CommonUtil.isB2bFlagUpdatedAtL3Level(product, true, true);
    assertFalse(result);
  }

  @Test
  public void isProductRejectedFalseForceReviewTest() {
    product.setMarkForDelete(true);
    product.setSuspended(false);
    product.setForceReview(true);
    boolean productRejected = CommonUtil.isProductRejected(product);
    assertFalse(productRejected);
  }

  @Test
  public void isProductRejectedTrueTest() {
    product.setMarkForDelete(true);
    product.setSuspended(false);
    product.setForceReview(false);
    boolean productRejected = CommonUtil.isProductRejected(product);
    assertTrue(productRejected);
  }


  @Test
  public void getSortedDefiningAttributesTest() {
    List<SortedDefiningAttributeDTO> list = new ArrayList<>();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList = new ArrayList<>();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(new ArrayList<>());
    masterDataProductAttributeDTO.setSequence(10);
    masterDataProductAttributeDTOList.add(masterDataProductAttributeDTO);
    masterDataProductDTO.setMasterDataProductAttributes(masterDataProductAttributeDTOList);
    CommonUtil.getSortedDefiningAttributes(masterDataProductDTO);
  }

  @Test
  public void getSortedDefiningAttributesTest2() {
    List<SortedDefiningAttributeDTO> list = new ArrayList<>();
    MasterDataAttributeDTO masterDataAttributeDTO = new MasterDataAttributeDTO();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS = new ArrayList<>();
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    masterDataProductAttributeValueDTO.setSequence(10);
    masterDataProductAttributeValueDTOS.add(masterDataProductAttributeValueDTO);
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList = new ArrayList<>();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(new ArrayList<>());
    masterDataProductAttributeDTO.setSequence(10);
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    masterDataAttributeDTO.setAttributeName(ATTRIBUTE_NAME);
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    masterDataProductAttributeDTOList.add(masterDataProductAttributeDTO);
    masterDataProductDTO.setMasterDataProductAttributes(masterDataProductAttributeDTOList);
    CommonUtil.getSortedDefiningAttributes(masterDataProductDTO);
    SortedDefiningAttributeDTO sortedDefiningAttributeDTO =
        CommonUtil.getSortedDefiningAttributeFromMasterDataProductAttributeDto(masterDataProductAttributeDTO);
    list.add(sortedDefiningAttributeDTO);
  }

  @Test
  public void getSortedDefiningAttributesTest3() {
    List<SortedDefiningAttributeDTO> list = new ArrayList<>();
    MasterDataAttributeDTO masterDataAttributeDTO = new MasterDataAttributeDTO();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS = new ArrayList<>();
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    masterDataProductAttributeValueDTO.setSequence(10);
    masterDataProductAttributeValueDTOS.add(masterDataProductAttributeValueDTO);
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList = new ArrayList<>();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(new ArrayList<>());
    masterDataProductAttributeDTO.setSequence(10);
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    masterDataAttributeDTO.setAttributeName(ATTRIBUTE_NAME);
    masterDataAttributeDTO.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    masterDataProductAttributeDTOList.add(masterDataProductAttributeDTO);
    masterDataProductDTO.setMasterDataProductAttributes(masterDataProductAttributeDTOList);
    CommonUtil.getSortedDefiningAttributes(masterDataProductDTO);
    SortedDefiningAttributeDTO sortedDefiningAttributeDTO =
        CommonUtil.getSortedDefiningAttributeFromMasterDataProductAttributeDto(masterDataProductAttributeDTO);
    list.add(sortedDefiningAttributeDTO);
  }

  @Test
  public void getProductSkuPickUpPointList() {
    List<ProductSkuPickupPointResponseV2> result =
      CommonUtil.getProductSkuPickUpPointList(itemPickupPointList);
    assertEquals(1, result.size());
    assertEquals(itemPickupPointList.get(0).getPickupPointCode(), result.get(0).getPickupPointCode());
    assertEquals(1, result.get(0).getItemViewConfig().size());
    assertEquals("DEFAULT", result.get(0).getItemViewConfig().iterator().next().getChannel());
  }

  @Test
  public void getItemSkuAndPickupPointCodeResponseTest() {
    Date date1 = new Date();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setStoreId(STORE_ID);
    itemPickupPoint.setCreatedDate(date1);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setMarkForDelete(true);
    itemPickupPoint.setUpdatedDate(date1);
    ItemSkuAndPickupPointCodeResponse itemSkuAndPickupPointCodeResponse =
        CommonUtil.getItemSkuAndPickupPointCodeResponse(itemPickupPoint);
    assertEquals(ITEM_SKU, itemSkuAndPickupPointCodeResponse.getItemSku());
    assertEquals(PICKUP_POINT_CODE, itemSkuAndPickupPointCodeResponse.getPickupPointCode());
    assertEquals(STORE_ID, itemSkuAndPickupPointCodeResponse.getStoreId());
    assertEquals(date1, itemSkuAndPickupPointCodeResponse.getCreatedDate());
    assertEquals(date1, itemSkuAndPickupPointCodeResponse.getUpdatedDate());
    assertTrue(itemSkuAndPickupPointCodeResponse.isCncActive());
    assertTrue(itemSkuAndPickupPointCodeResponse.isMarkForDelete());
  }

  @Test
  public void toPriceUpdatedInTimeRangeL5ResponseTest() {
    Date date1 = new Date();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setStoreId(STORE_ID);
    itemPickupPoint.setCreatedDate(date1);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setMarkForDelete(true);
    itemPickupPoint.setUpdatedDate(date1);
    itemPickupPoint.setPriceUpdatedDate(date1);
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(Constants.DEFAULT_CHANNEL);
    itemViewConfigs.add(itemViewConfig);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setMarkForDelete(false);
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    PriceUpdatedInTimeRangeL5Response priceUpdatedInTimeRangeL5Response =
        CommonUtil.toPriceUpdatedInTimeRangeL5Response(itemPickupPoint);
    assertEquals(ITEM_SKU, priceUpdatedInTimeRangeL5Response.getItemSku());
    assertEquals(PICKUP_POINT_CODE, priceUpdatedInTimeRangeL5Response.getPickupPointCode());
    assertEquals(STORE_ID, priceUpdatedInTimeRangeL5Response.getStoreId());
    assertEquals(date1, priceUpdatedInTimeRangeL5Response.getCreatedDate());
    assertEquals(date1, priceUpdatedInTimeRangeL5Response.getUpdatedDate());
    assertEquals(date1, priceUpdatedInTimeRangeL5Response.getPriceUpdatedDate());
    assertTrue(priceUpdatedInTimeRangeL5Response.isBuyable());
    assertTrue(priceUpdatedInTimeRangeL5Response.isDiscoverable());
    assertTrue(priceUpdatedInTimeRangeL5Response.isCncActive());
    assertFalse(priceUpdatedInTimeRangeL5Response.isMarkForDelete());
  }

  @Test
  public void toDateFromTimestampObjectTest() {
    Date response = CommonUtil.toDateFromTimestampObject(date.getTime());
    assertEquals(date, response);
  }

  @Test
  public void toDateFromTimestampObjectNullTest() {
    Date response = CommonUtil.toDateFromTimestampObject(null);
    assertNull(response);
  }

  @Test
  public void isDateFieldTrue() {
    boolean isDateField = CommonUtil.isDateField(PRODUCT_CENTER_UPDATED_DATE);
    assertTrue(isDateField);
  }

  @Test
  public void isDateFieldFalseTest() {
    boolean isDateField = CommonUtil.isDateField(PRODUCT_SKU);
    assertFalse(isDateField);
  }

  @Test
  public void getSolrInputDocumentForTotalStockTest() {
    inventoryStockInfoDTO.setTotalStock(10);
    inventoryStockInfoDTO.setWarehouseTotalAvailableStock(0);
    inventoryStockInfoDTO.setWebTotalAvailableStock(0);
    item.setPrice(Collections.singleton(price));
    item.setPickupPointCode(PICKUP_POINT_CODE);
    product.setArchived(true);
    product.setOff2OnChannelActive(true);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentForNewFieldsL3Solr(PRODUCT_SKU, product, Arrays.asList(item), true);
    assertEquals(PRODUCT_SKU, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODES)).get(SolrConstants.SET_CLAUSE),
        Collections.singletonList(PICKUP_POINT_CODE));
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.VARIANT_COUNT)).get(SolrConstants.SET_CLAUSE),
        1);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MAXIMUM_SELLING_PRICE)).get(SolrConstants.SET_CLAUSE),
        0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MINIMUM_SELLING_PRICE)).get(SolrConstants.SET_CLAUSE),
        0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MAXIMUM_LIST_PRICE)).get(SolrConstants.SET_CLAUSE), 0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MINIMUM_LIST_PRICE)).get(SolrConstants.SET_CLAUSE), 0.0);
    assertTrue(
        (boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_IN_STOCK)).get(SolrConstants.SET_CLAUSE));
    assertTrue(
        (boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED)).get(SolrConstants.SET_CLAUSE));
    assertTrue((boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE)).get(
        SolrConstants.SET_CLAUSE));
  }

  @Test
  public void getSolrInputDocumentForNullTotalStockTest() {
    inventoryStockInfoDTO.setTotalStock(null);
    inventoryStockInfoDTO.setWarehouseTotalAvailableStock(0);
    inventoryStockInfoDTO.setWebTotalAvailableStock(0);
    item.setPrice(Collections.singleton(price));
    item.setPickupPointCode(PICKUP_POINT_CODE);
    product.setArchived(true);
    product.setOff2OnChannelActive(true);
    SolrInputDocument solrInputDocument =
        CommonUtil.getSolrInputDocumentForNewFieldsL3Solr(PRODUCT_SKU, product, Arrays.asList(item), false);
    assertEquals(PRODUCT_SKU, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODES)).get(SolrConstants.SET_CLAUSE),
        Collections.singletonList(PICKUP_POINT_CODE));
    assertEquals(((Map) solrInputDocument.getFieldValue(SolrFieldNames.VARIANT_COUNT)).get(SolrConstants.SET_CLAUSE),
        1);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MAXIMUM_SELLING_PRICE)).get(SolrConstants.SET_CLAUSE),
        0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MINIMUM_SELLING_PRICE)).get(SolrConstants.SET_CLAUSE),
        0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MAXIMUM_LIST_PRICE)).get(SolrConstants.SET_CLAUSE), 0.0);
    assertEquals(
        ((Map) solrInputDocument.getFieldValue(SolrFieldNames.MINIMUM_LIST_PRICE)).get(SolrConstants.SET_CLAUSE), 0.0);
    assertFalse(
        (boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_IN_STOCK)).get(SolrConstants.SET_CLAUSE));
    assertTrue(
        (boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED)).get(SolrConstants.SET_CLAUSE));
    assertTrue((boolean) ((Map) solrInputDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE)).get(
        SolrConstants.SET_CLAUSE));
  }

  @Test
  public void generateL3AggregateDataFromItemsTotalStockTest() {
    inventoryStockInfoDTO.setTotalStock(10);
    inventoryStockInfoDTO.setWarehouseTotalAvailableStock(0);
    inventoryStockInfoDTO.setWebTotalAvailableStock(0);
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    price.setOfferPrice(1000);
    price.setListPrice(2000);
    item.setPrice(Collections.singleton(price));
    CommonUtil.generateL3AggregateDataFromItems(product, Arrays.asList(item), solrInputDocument, itemPickupPointList,
        true, false, false);
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    assertFalse(CollectionUtils.isEmpty(solrInputDocument.getFieldValues(SolrFieldNames.PICKUP_POINT_CODES)));
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    assertTrue((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_IN_STOCK));
  }

  @Test
  public void generateL3AggregateDataFromItemsTotalStockNullTest() {
    inventoryStockInfoDTO.setTotalStock(null);
    inventoryStockInfoDTO.setWarehouseTotalAvailableStock(0);
    inventoryStockInfoDTO.setWebTotalAvailableStock(0);
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    price.setOfferPrice(1000);
    price.setListPrice(2000);
    item.setPrice(Collections.singleton(price));
    CommonUtil.generateL3AggregateDataFromItems(product, Arrays.asList(item), solrInputDocument, itemPickupPointList,
        false, false, false);
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    assertFalse(CollectionUtils.isEmpty(solrInputDocument.getFieldValues(SolrFieldNames.PICKUP_POINT_CODES)));
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    assertFalse((boolean) solrInputDocument.getFieldValue(SolrFieldNames.IS_IN_STOCK));
  }

  @Test
  public void checkArgumentApiIncorrectInputDataExceptionThrowErrorTest() {
    Assertions.assertThrows(ApiIncorrectInputDataException.class,
        () -> CommonUtil.checkArgumentApiIncorrectInputDataException(false, "", ""));
  }

  @Test
  public void checkArgumentApiIncorrectInputDataExceptionTest() {
    CommonUtil.checkArgumentApiIncorrectInputDataException(true, "", "");
  }

  @Test
  public void checkEligibilityForB2cAndB2bActivatedProfileResponseNullTest() {
    product.setB2bActivated(true);
    product.setB2cActivated(true);
    itemVo.setItemPickupPointVoList(Arrays.asList(itemPickupPointVo));
    CommonUtil.checkEligibilityForB2cAndB2bActivated(product, Arrays.asList(itemVo), null);
    assertFalse(product.isB2bActivated());
    assertFalse(product.getB2cActivated());
  }

  @Test
  public void checkEligibilityForB2cAndB2bActivatedProfileResponseCompanyNullTest() {
    product.setB2bActivated(true);
    product.setB2cActivated(true);
    itemVo.setItemPickupPointVoList(Arrays.asList(itemPickupPointVo));
    CommonUtil.checkEligibilityForB2cAndB2bActivated(product, Arrays.asList(itemVo), new BusinessPartner());
    assertFalse(product.isB2bActivated());
    assertFalse(product.getB2cActivated());
  }

  @Test
  public void checkEligibilityForB2cAndB2bActivatedSalesChannelNullTest() {
    product.setB2bActivated(true);
    product.setB2cActivated(true);
    profileResponse.getCompany().setSalesChannel(null);
    itemVo.setItemPickupPointVoList(Arrays.asList(itemPickupPointVo));
    defaultItemViewConfig.setDiscoverable(true);
    b2bItemViewConfig.setBuyable(true);
    itemPickupPointVo.setItemViewConfig(new HashSet<>(Arrays.asList(defaultItemViewConfig, b2bItemViewConfig)));
    CommonUtil.checkEligibilityForB2cAndB2bActivated(product, Arrays.asList(itemVo), new BusinessPartner());
    assertFalse(product.isB2bActivated());
    assertFalse(product.getB2cActivated());
    assertFalse(defaultItemViewConfig.isDiscoverable());
    assertEquals(itemPickupPointVo.getAllItemViewConfigs().size(), 1);
  }

  @Test
  public void checkEligibilityForB2cAndB2bActivatedTest() {
    product.setB2bActivated(true);
    product.setB2cActivated(true);
    BusinessPartner businessPartner = new BusinessPartner();
    businessPartner.setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    defaultItemViewConfig.setDiscoverable(true);
    b2bItemViewConfig.setBuyable(true);
    itemPickupPointVo.setItemViewConfig(new HashSet<>(Arrays.asList(defaultItemViewConfig, b2bItemViewConfig)));
    itemVo.setItemPickupPointVoList(Arrays.asList(itemPickupPointVo));
    CommonUtil.checkEligibilityForB2cAndB2bActivated(product, Arrays.asList(itemVo), businessPartner);
    assertTrue(product.isB2bActivated());
    assertTrue(product.getB2cActivated());
    assertTrue(defaultItemViewConfig.isDiscoverable());
    assertTrue(b2bItemViewConfig.isBuyable());
    assertEquals(itemPickupPointVo.getAllItemViewConfigs().size(), 2);
  }

  @Test
  public void setItemViewConfigFromRequestTest() {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT_CHANNEL);
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setChannel(Constants.B2B);
    ItemViewConfig itemViewConfig3 = new ItemViewConfig();
    itemViewConfig3.setChannel(Constants.B2B);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemViewConfig(ImmutableSet.of(itemViewConfig1, itemViewConfig2));

    assertEquals(itemPickupPoint, CommonUtil.setItemViewConfigFromRequest(itemPickupPoint, itemViewConfig3, false));

    itemPickupPoint.setItemViewConfig(new HashSet<>());
    assertNull(CommonUtil.setItemViewConfigFromRequest(itemPickupPoint, itemViewConfig3, false));
  }

  @Test
  public void setItemViewConfigFromRequestTest_cnc_1p() {
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT_CHANNEL);
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setChannel(Constants.B2B);
    ItemViewConfig itemViewConfig3 = new ItemViewConfig();
    itemViewConfig3.setChannel(Constants.CNC);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Set<ItemViewConfig> itemViewConfig = new HashSet<>();
    itemViewConfig.add(itemViewConfig1);
    itemViewConfig.add(itemViewConfig2);
    itemPickupPoint.setItemViewConfig(itemViewConfig);

    Assertions.assertEquals(3,
        CommonUtil.setItemViewConfigFromRequest(itemPickupPoint, itemViewConfig3, true).getAllItemViewConfigs().size());
  }

  @Test
  public void generateSellerChannelForL5EventTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    ProfileResponse profileResponse1 = new ProfileResponse();
    ProfileResponse profileResponse2 = null;
    CompanyDTO companyDTO = new CompanyDTO();
    List<String> sellerChannel = new ArrayList<>();
    sellerChannel.add(Constants.B2B_SELLER_CHANNEL);
    sellerChannel.add(Constants.B2C_SELLER_CHANNEL);
    companyDTO.setSalesChannel(sellerChannel);
    CompanyDTO companyDTO1 = new CompanyDTO();
    companyDTO1.setName(PRODUCT_NAME);
    profileResponse1.setCompany(companyDTO1);
    profileResponse.setCompany(companyDTO);
    List<String> sellerChannelRequest = new ArrayList<>();
    List<String> response = CommonUtil.generateSellerChannelForL5Event(profileResponse, sellerChannelRequest);
    assertEquals(response.size(), 2);
    assertEquals(response.get(0), Constants.B2B);
    assertEquals(response.get(1), Constants.RETAIL);
    CommonUtil.generateSellerChannelForL5Event(new ProfileResponse(), sellerChannelRequest);
    CommonUtil.generateSellerChannelForL5Event(profileResponse1, sellerChannelRequest);
    CommonUtil.generateSellerChannelForL5Event(profileResponse2, sellerChannelRequest);
    profileResponse.getCompany().setSalesChannel(new ArrayList<>(Collections.singleton(Constants.B2B_SELLER_CHANNEL)));
    CommonUtil.generateSellerChannelForL5Event(profileResponse, sellerChannelRequest);
    profileResponse.getCompany().setSalesChannel(new ArrayList<>(Collections.singleton(Constants.B2C_SELLER_CHANNEL)));
    CommonUtil.generateSellerChannelForL5Event(profileResponse, sellerChannelRequest);
  }

  @Test
  public void isProductMasterDataDetailChangedTest() {
    Product product = new Product();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE_2);
    productCategoryResponse.setCategory(categoryResponse);
    product.setProductName(PRODUCT_NAME);
    productDetailResponse.setName(PRODUCT_NAME_2);
    product.setBrand(PRODUCT_BRAND);
    productDetailResponse.setBrand(PRODUCT_BRAND_2);
    product.setCategoryCode(CATEGORY_CODE);
    productDetailResponse.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    assertTrue(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));

    productDetailResponse.setBrand(PRODUCT_BRAND);
    assertTrue(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));


    categoryResponse.setCategoryCode(CATEGORY_CODE);
    assertTrue(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));

    productDetailResponse.setName(PRODUCT_NAME);
    assertTrue(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, true));
    assertFalse(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));

    productDetailResponse.setUrl(URL);
    assertTrue(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    product.setUrl(URL);
    assertFalse(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    productDetailResponse.setVideoDTO(new VideoDTO());
    assertFalse(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    product.setVideo(new Video());
    product.setVideo(null);
    assertFalse(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    productDetailResponse.setVideoDTO(new VideoDTO());
    product.setVideo(new Video());
    assertFalse(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    VideoDTO videoDTO = new VideoDTO();
    videoDTO.setVideoId(VIDEO_ID);
    productDetailResponse.setVideoDTO(videoDTO);
    assertTrue(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    Video video = new Video();
    video.setVideoId(VIDEO_ID);
    product.setVideo(video);
    assertFalse(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    videoDTO.setFinalUrl(FINAL_URL);
    productDetailResponse.setVideoDTO(videoDTO);
    assertTrue(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    video.setFinalUrl(FINAL_URL);
    product.setVideo(video);
    assertFalse(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    videoDTO.setVideoName(VIDEO_NAME);
    productDetailResponse.setVideoDTO(videoDTO);
    assertTrue(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    video.setVideoName(VIDEO_NAME);
    product.setVideo(video);
    assertFalse(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    videoDTO.setCoverImagePath(COVER_IMAGE);
    productDetailResponse.setVideoDTO(videoDTO);
    assertTrue(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    video.setCoverImagePath(COVER_IMAGE);
    product.setVideo(video);
    assertFalse(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    videoDTO.setSourceUrl(SOURCE_IMAGE);
    productDetailResponse.setVideoDTO(videoDTO);
    assertTrue(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
    video.setSourceUrl(SOURCE_IMAGE);
    product.setVideo(video);
    assertFalse(CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, false));
  }

  @Test
  public void isItemMasterDataDetailChangedTest() {
    Image image1 = new Image();
    image1.setMarkForDelete(true);
    Image image2 = new Image();
    image2.setOriginalImage(true);
    Image image3 = new Image();
    image3.setOriginalImage(false);
    Image image4 = new Image();
    image4.setMainImages(true);
    image4.setLocationPath(LOCATION_PATH_2);
    Item item = new Item();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductItemResponse productItemResponse = new ProductItemResponse();
    item.setLength(10.0);
    productDetailResponse.setLength(20.0);
    item.setGeneratedItemName(PRODUCT_NAME);
    productItemResponse.setGeneratedItemName(PRODUCT_NAME_2);
    item.setDangerousLevel(1);
    productItemResponse.setDangerousGoodsLevel(2);
    item.setMainImageUrl(LOCATION_PATH);
    productItemResponse.setImages(Arrays.asList(image1, image2, image3, image4));

    assertTrue(CommonUtil.isItemMasterDataDetailChanged(item, productDetailResponse, productItemResponse));

    productDetailResponse.setLength(10.0);
    assertTrue(CommonUtil.isItemMasterDataDetailChanged(item, productDetailResponse, productItemResponse));

    productItemResponse.setGeneratedItemName(PRODUCT_NAME);
    assertTrue(CommonUtil.isItemMasterDataDetailChanged(item, productDetailResponse, productItemResponse));

    productItemResponse.setDangerousGoodsLevel(1);
    assertTrue(CommonUtil.isItemMasterDataDetailChanged(item, productDetailResponse, productItemResponse));

    image4.setLocationPath(LOCATION_PATH);
    assertFalse(CommonUtil.isItemMasterDataDetailChanged(item, productDetailResponse, productItemResponse));

  }

  @Test
  public void generateDefaultSellerChannelForL5EventTest() {
    List<String> sellerChannel = new ArrayList<>();
    String defaultSellerChannelValue = Constants.B2C_SELLER_CHANNEL;
    List<String> response = CommonUtil.generateDefaultSellerChannelForL5Event(defaultSellerChannelValue, sellerChannel);
    assertEquals(response.size(), 1);
    assertEquals(response.get(0), Constants.RETAIL);
    defaultSellerChannelValue = Constants.B2B_SELLER_CHANNEL;
    sellerChannel.clear();
    CommonUtil.generateDefaultSellerChannelForL5Event(defaultSellerChannelValue, sellerChannel);
    assertEquals(response.size(), 0);
  }

  @Test
  public void generateSellerChannelFromBusinessPartnerTest() {
    BusinessPartner businessPartner = new BusinessPartner();
    businessPartner.setBusinessPartnerCode(MERCHANT_CODE);
    List<String> response = CommonUtil.generateSellerChannelFromBusinessPartner(null);
    assertEquals(response.size(), 0);
    CommonUtil.generateSellerChannelFromBusinessPartner(businessPartner);
    assertEquals(response.size(), 0);
    businessPartner.setSalesChannel(new ArrayList<>(Collections.singleton(Constants.B2B_SELLER_CHANNEL)));
    CommonUtil.generateSellerChannelFromBusinessPartner(businessPartner);
    businessPartner.setSalesChannel(new ArrayList<>(Collections.singleton(Constants.B2C_SELLER_CHANNEL)));
    CommonUtil.generateSellerChannelFromBusinessPartner(businessPartner);
  }

  @Test
  public void isItemPriceChangeTest() {
    Item item = new Item();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(1000);
    price.setChannel(DEFAULT_CHANNEL);
    itemPrices.add(price);
    item.setPrice(itemPrices);
    boolean result = CommonUtil.isItemPriceChange(item, prices);
    assertTrue(result);
  }

  @Test
  public void isItemPriceChange_whenPriceNotChangeTest() {
    Item item = new Item();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(800);
    price.setListPrice(1000);
    price.setChannel(DEFAULT_CHANNEL);
    itemPrices.add(price);
    item.setPrice(itemPrices);
    boolean result = CommonUtil.isItemPriceChange(item, prices);
    assertFalse(result);
  }

  @Test
  public void isItemPriceChange_whenChannelNotMatchTest() {
    Item item = new Item();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(1000);
    price.setChannel(OTHER_CHANNEL);
    itemPrices.add(price);
    item.setPrice(itemPrices);
    boolean result = CommonUtil.isItemPriceChange(item, prices);
    assertFalse(result);
  }

  @Test
  public void isItemPriceChange_whenListPriceChangeTest() {
    Item item = new Item();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(900);
    price.setChannel(DEFAULT_CHANNEL);
    itemPrices.add(price);
    item.setPrice(itemPrices);
    boolean result = CommonUtil.isItemPriceChange(item, prices);
    assertTrue(result);
  }

  @Test
  public void isItemPriceChange_whenEmptyPriceSetTest() {
    Item item = new Item();
    Set<Price> itemPrices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(700);
    price.setListPrice(900);
    price.setChannel(DEFAULT_CHANNEL);
    itemPrices.add(price);
    item.setPrice(itemPrices);
    boolean result = CommonUtil.isItemPriceChange(item, new HashSet<Price>());
    assertFalse(result);
  }

  @Test
  public void isItemPriceChange_WhenMerchantPromoDiscountTrueAndNoPriceChangeTest() {
    Item item = new Item();
    item.setMerchantPromoDiscount(true);
    Set<Price> itemPrices = new HashSet<>();
    Price existingPrice = new Price();
    existingPrice.setOfferPrice(800);
    existingPrice.setListPrice(1000);
    existingPrice.setMerchantPromoDiscountPrice(new DiscountPrice());
    existingPrice.getMerchantPromoDiscountPrice().setDiscountPrice(800);
    existingPrice.setChannel(DEFAULT_CHANNEL);
    itemPrices.add(existingPrice);
    item.setPrice(itemPrices);
    boolean result = CommonUtil.isItemPriceChange(item, prices);
    assertFalse(result);
  }

  @Test
  public void isItemPriceChange_WhenMerchantPromoDiscountTrueAndPriceChangedTest() {
    Item item = new Item();
    item.setMerchantPromoDiscount(true);
    Set<Price> itemPrices = new HashSet<>();
    Price existingPrice = new Price();
    existingPrice.setOfferPrice(700);
    existingPrice.setListPrice(1000);
    existingPrice.setMerchantPromoDiscountPrice(new DiscountPrice());
    existingPrice.getMerchantPromoDiscountPrice().setDiscountPrice(900);
    existingPrice.setChannel(DEFAULT_CHANNEL);
    itemPrices.add(existingPrice);
    item.setPrice(itemPrices);
    boolean result = CommonUtil.isItemPriceChange(item, prices);
    assertTrue(result);
  }

  @Test
  public void isItemPriceChange_WhenMerchantPromoDiscountTrueAndUpcomingPromoPriceChangedTest() {
    Item item = new Item();
    item.setMerchantPromoDiscount(true);
    Set<Price> itemPrices = new HashSet<>();
    Price existingPrice = new Price();
    existingPrice.setOfferPrice(700);
    existingPrice.setListPrice(1000);
    existingPrice.setChannel(DEFAULT_CHANNEL);
    itemPrices.add(existingPrice);
    item.setPrice(itemPrices);
    boolean result = CommonUtil.isItemPriceChange(item, prices);
    assertTrue(result);
  }

  @Test
  public void isItemPriceChange_WhenMerchantPromoDiscountTrueAndUpcomingPromoNoPriceChangeTest() {
    Item item = new Item();
    item.setMerchantPromoDiscount(true);
    Set<Price> itemPrices = new HashSet<>();
    Price existingPrice = new Price();
    existingPrice.setOfferPrice(800);
    existingPrice.setListPrice(1000);
    existingPrice.setChannel(DEFAULT_CHANNEL);
    itemPrices.add(existingPrice);
    item.setPrice(itemPrices);
    boolean result = CommonUtil.isItemPriceChange(item, prices);
    assertFalse(result);
  }

  @Test
  public void isEligibleForL5UpdateAllTrueTest() {
    assertTrue(CommonUtil.isEligibleForL5Update(true, true, true, true, true, true, false));
  }

  @Test
  public void isEligibleForL5UpdateDataChangeIsPriceUpdatedTest() {
    assertTrue(CommonUtil.isEligibleForL5Update(true, false, false, false, false, false, false));
  }

  @Test
  public void isEligibleForL5UpdateDataChangeIsItemDataUpdatedTest() {
    assertTrue(CommonUtil.isEligibleForL5Update(false, true, false, false, false, false, false));
  }

  @Test
  public void isEligibleForL5UpdateDataChangeWholeSalePriceUpdatedTest() {
    assertTrue(CommonUtil.isEligibleForL5Update(false, false, true, false, false, false, false));
  }

  @Test
  public void isEligibleForL5UpdateDataChangeCncFlagUpdatedTest() {
    assertTrue(CommonUtil.isEligibleForL5Update(false, false, false, true, false, false, false));
  }

  @Test
  public void isEligibleForL5UpdateDataChangeB2BFieldsUpdatedTest() {
    assertTrue(CommonUtil.isEligibleForL5Update(false, false, false, false, true, false, false));
  }

  @Test
  public void isEligibleForL5UpdateDataChangeItemPickupPointUpdatedTest() {
    assertTrue(CommonUtil.isEligibleForL5Update(false, false, false, false, false, true, false));
  }


  @Test
  public void isEligibleForL5UpdateDataChangeAllFalseTest() {
    assertFalse(CommonUtil.isEligibleForL5Update(false, false, false, false, false, false, false));
  }

  @Test
  public void isValidItemPickupPointSummaryRequestAllFieldValidationTest() {
    itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setMerchantCode(MERCHANT_CODE);

    assertTrue(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));
    itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setItemSku(ITEM_SKU);
    assertTrue(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));
    itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setKeyword(ITEM_SKU);

    assertTrue(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));
    itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    assertFalse(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));
  }

  @Test
  public void isValidItemPickupPointSummaryRequestItemCodeValidationTest() {
    itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    Set<String> itemCodeSet = new HashSet<>();
    itemCodeSet.add(null);
    itemPickupPointSummaryRequest.setItemCodes(itemCodeSet);
    assertFalse(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));

    itemCodeSet = new HashSet<>();
    itemCodeSet.add(ITEM_SKU);
    itemCodeSet.add(null);
    itemPickupPointSummaryRequest.setItemCodes(itemCodeSet);
    assertTrue(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));
  }

  @Test
  public void isValidItemPickupPointSummaryRequestProductSkuValidationTest() {
    itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    List<String> productSkuList = new ArrayList<>();
    productSkuList.add(null);
    productSkuList.add(PRODUCT_SKU);
    itemPickupPointSummaryRequest.setProductSkuList(productSkuList);
    assertTrue(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));

    itemPickupPointSummaryRequest.setProductSkuList(List.of(PRODUCT_SKU_1));
    assertTrue(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));
  }

  @Test
  public void isValidItemPickupPointSummaryRequestPpCodeValidationTest() {
    itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    List<String> ppCodeList = new ArrayList<>();
    ppCodeList.add(null);
    ppCodeList.add(PRODUCT_SKU);
    itemPickupPointSummaryRequest.setPickupPointCodes(ppCodeList);
    assertTrue(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));

    itemPickupPointSummaryRequest.setPickupPointCodes(List.of(PICKUP_POINT_CODE));
    assertTrue(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));
  }

  @Test
  public void isValidItemPickupPointSummaryRequestItemPickupPointCodeValidationTest() {
    itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    List<ItemPickupPointRequest> pickupPointRequestList = new ArrayList<>();
    pickupPointRequestList.add(null);
    itemPickupPointSummaryRequest.setItemPickupPointCode(pickupPointRequestList);
    assertFalse(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));

    pickupPointRequestList = new ArrayList<>();
    pickupPointRequest.setItemSku(ITEM_SKU);
    pickupPointRequest.setPickupPointCode(null);
    pickupPointRequestList.add(pickupPointRequest);
    itemPickupPointSummaryRequest.setItemPickupPointCode(pickupPointRequestList);
    assertFalse(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));


    pickupPointRequestList = new ArrayList<>();
    pickupPointRequest.setItemSku(null);
    pickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointRequestList.add(pickupPointRequest);
    itemPickupPointSummaryRequest.setItemPickupPointCode(pickupPointRequestList);
    assertFalse(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));

    pickupPointRequestList = new ArrayList<>();
    pickupPointRequest.setItemSku(ITEM_SKU);
    pickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointRequestList.add(pickupPointRequest);
    itemPickupPointSummaryRequest.setItemPickupPointCode(pickupPointRequestList);
    assertTrue(CommonUtil.isValidItemPickupPointSummaryRequest(itemPickupPointSummaryRequest));
  }

  @Test
  public void updateCurationStatusOfProductNullCategory() {
    productCategoryResponse.setCategory(null);
    CommonUtil.updateCurationStatusOfProduct(product, productCategoryResponse);
    assertNull(product.getCurationStatus());
  }

  @Test
  public void updateCurationStatusOfProductCurationStatusNoneTest() {
    productCategoryResponse.getCategory().setHalalCategory(false);
    product.setCurationStatus(CurationStatus.NONE);
    boolean categoyUpdate = CommonUtil.updateCurationStatusOfProduct(product, productCategoryResponse);
    assertEquals(product.getCurationStatus(), CurationStatus.NONE);
    assertFalse(categoyUpdate);
  }

  @Test
  public void updateCurationStatusOfProductCurationStatusNullTest() {
    productCategoryResponse.getCategory().setHalalCategory(false);
    CommonUtil.updateCurationStatusOfProduct(product, productCategoryResponse);
    assertEquals(product.getCurationStatus(), null);
  }

  @Test
  public void getCurationStatusTest() {
    Product product = new Product();
    product.setCurationStatus(CurationStatus.APPROVED);
    int curationStatus = CommonUtil.getCurationStatus(product);
    assertEquals(CurationStatus.APPROVED.getValue(), curationStatus);
  }

  @Test
  public void getCurationStatusNullTest() {
    Product product = new Product();
    int curationStatus = CommonUtil.getCurationStatus(product);
    assertEquals(CurationStatus.NONE.getValue(), curationStatus);
  }

  @Test
  public void updateCurationStatusOfProductCurationStatusActiveTest() {
    productCategoryResponse.getCategory().setHalalCategory(false);
    product.setCurationStatus(CurationStatus.APPROVED);
    boolean categoyUpdate = CommonUtil.updateCurationStatusOfProduct(product, productCategoryResponse);
    assertTrue(categoyUpdate);
    assertEquals(product.getCurationStatus(), CurationStatus.NONE);
  }

  @Test
  public void updateCurationStatusOfProductCurationStatusNeedCurationFromNullTest() {
    productCategoryResponse.getCategory().setHalalCategory(true);
    CommonUtil.updateCurationStatusOfProduct(product, productCategoryResponse);
    assertEquals(product.getCurationStatus(), CurationStatus.NEED_CURATION);
  }

  @Test
  public void updateCurationStatusOfProductCurationStatusNeedCurationFromNoneTest() {
    productCategoryResponse.getCategory().setHalalCategory(true);
    product.setCurationStatus(CurationStatus.NONE);
    CommonUtil.updateCurationStatusOfProduct(product, productCategoryResponse);
    assertEquals(product.getCurationStatus(), CurationStatus.NEED_CURATION);
  }

  @Test
  public void updateCurationStatusOfProductCurationStatusApprovedTest() {
    productCategoryResponse.getCategory().setHalalCategory(true);
    product.setCurationStatus(CurationStatus.APPROVED);
    CommonUtil.updateCurationStatusOfProduct(product, productCategoryResponse);
    assertEquals(product.getCurationStatus(), CurationStatus.APPROVED);
  }

  @Test
  public void masterCategoryFromProductDetailResponseNullProductCategoryResponseTest() {
    productDetailResponse.setProductCategoryResponses(null);
    ProductCategoryResponse productCategoryResponse =
        CommonUtil.masterCategoryFromProductDetailResponse(productDetailResponse, SALES_CATALOG);
    assertEquals(productCategoryResponse, new ProductCategoryResponse());
  }

  @Test
  public void masterCategoryFromProductDetailResponseWithSalesCategoryTest() {
    productDetailResponse.getProductCategoryResponses().get(0).getCategory().getCatalog().setCatalogCode(SALES_CATALOG);
    ProductCategoryResponse productCategoryResponse =
        CommonUtil.masterCategoryFromProductDetailResponse(productDetailResponse, SALES_CATALOG);
    assertEquals(productCategoryResponse, new ProductCategoryResponse());
  }

  @Test
  public void masterCategoryFromProductDetailResponseWithEmptyCatalogCodeTest() {
    productDetailResponse.getProductCategoryResponses().get(0).getCategory().getCatalog().setCatalogCode("");
    ProductCategoryResponse productCategoryResponse =
        CommonUtil.masterCategoryFromProductDetailResponse(productDetailResponse, SALES_CATALOG);
    assertEquals(productCategoryResponse, new ProductCategoryResponse());
  }

  @Test
  public void masterCategoryFromProductDetailResponseTest() {
    productDetailResponse.getProductCategoryResponses().get(0).getCategory().getCatalog()
        .setCatalogCode(MASTER_CATALOG);
    ProductCategoryResponse productCategoryResponse =
        CommonUtil.masterCategoryFromProductDetailResponse(productDetailResponse, SALES_CATALOG);
    assertEquals(productCategoryResponse, productDetailResponse.getProductCategoryResponses().get(0));
  }

  @Test
  public void setUpdatedItemSkuAndItemMapTest() {
    CommonUtil.setUpdatedItemSkuAndItemMap(Arrays.asList(item), editProductDetailDTO);
    assertEquals(editProductDetailDTO.getAllItemMap().size(), 1);
    assertEquals(editProductDetailDTO.getUpdatedItemMap().size(), 1);
  }

  @Test
  public void setOfflineItemIdToItemPickupPointMapTest() {
    CommonUtil.setOfflineItemIdToItemPickupPointMap(Arrays.asList(itemPickupPoint), editProductDetailDTO);
    assertEquals(1, editProductDetailDTO.getAllItemPickupPointMap().size());
    assertEquals(1, editProductDetailDTO.getUpdatedItemPickupPointMap().size());
  }

  @Test
  public void getItemPickupPointFromDTOTest() {
    editProductDetailDTO.getAllItemPickupPointMap()
        .put(ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_CODE, itemPickupPoint);
    Optional<ItemPickupPoint> itemPickupPointOptional =
        CommonUtil.getItemPickupPointFromDTO(editProductDetailDTO, ITEM_SKU, PICKUP_POINT_CODE);
    assertNotNull(itemPickupPointOptional.get());
  }

  @Test
  public void getItemPickupPointFromDTONullTest() {
    Optional<ItemPickupPoint> itemPickupPointOptional =
        CommonUtil.getItemPickupPointFromDTO(editProductDetailDTO, ITEM_SKU, PICKUP_POINT_CODE);
    assertFalse(itemPickupPointOptional.isPresent());
  }

  @Test()
  public void getItemFromDTOTest() {
    ApiIncorrectInputDataException apiException = null;
    try {
      Item item = CommonUtil.getItemFromDTO(ITEM_SKU, editProductDetailDTO);
    } catch (ApiIncorrectInputDataException e) {
      apiException = e;
    } finally {
      assertNotNull(apiException);
      assertEquals(ErrorMessages.NO_ITEM_FOUND, apiException.getErrorMessage());
      assertEquals(ErrorCategory.DATA_NOT_FOUND.getCode(), apiException.getErrorCode());
    }
  }

  @Test()
  public void getItemFromDTOMfdTrueTest() {
    this.item.setMarkForDelete(true);
    editProductDetailDTO.getAllItemMap().put(ITEM_SKU, this.item);
    ApiIncorrectInputDataException apiException = null;
    try {
      Item item = CommonUtil.getItemFromDTO(ITEM_SKU, editProductDetailDTO);
    } catch (ApiIncorrectInputDataException e) {
      apiException = e;
    } finally {
      assertNotNull(apiException);
      assertEquals(ErrorMessages.NO_ITEM_FOUND, apiException.getErrorMessage());
      assertEquals(ErrorCategory.DATA_NOT_FOUND.getCode(), apiException.getErrorCode());
    }
  }

  @Test()
  public void getItemFromDTOMfdFalseTest() {
    this.item.setMarkForDelete(false);
    editProductDetailDTO.getAllItemMap().put(ITEM_SKU, this.item);
    Item item = CommonUtil.getItemFromDTO(ITEM_SKU, editProductDetailDTO);
    assertNotNull(item);
  }

  @Test()
  public void isProductUpdatedForCombinedEditTest() {
    editProductDetailDTO.setProductUpdated(false);
    boolean response1 = CommonUtil.isProductUpdatedForCombinedEdit(false, editProductDetailDTO, true);
    assertFalse(response1);
    boolean response2 = CommonUtil.isProductUpdatedForCombinedEdit(true, editProductDetailDTO, false);
    assertFalse(response2);
    boolean response3 = CommonUtil.isProductUpdatedForCombinedEdit(true, editProductDetailDTO, true);
    assertTrue(response3);
    editProductDetailDTO.setProductUpdated(true);
    boolean response4 = CommonUtil.isProductUpdatedForCombinedEdit(true, editProductDetailDTO, true);
    assertTrue(response4);
  }

  @Test
  public void formInvRequestForPickupPointDeleteTest() {
    deleteOfflineItemVO = new DeleteOfflineItemVO();
    deleteOfflineItemVO1 = new DeleteOfflineItemVO();
    deleteOfflineItemVO.setItemSku(ITEM_SKU);
    deleteOfflineItemVO.setPickupPointCode(PICKUP_POINT_CODE);
    deleteOfflineItemVO.setSuccess(true);
    deleteOfflineItemVO1.setSuccess(false);
    List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> result =
        CommonUtil.formInvRequestForPickupPointDelete(Arrays.asList(deleteOfflineItemVO, deleteOfflineItemVO1),
            MERCHANT_CODE);
    assertEquals(result.get(0).getWebItemSku(), ITEM_SKU);
    assertEquals(result.get(0).getPickupPointCode(), PICKUP_POINT_CODE);
    assertEquals(result.get(0).getWebMerchantCode(), MERCHANT_CODE);
  }

  @Test
  public void populateProductTypeNullTest() {
    assertNull(CommonUtil.populateProductType(product, productChange).getProductType());
  }

  @Test
  public void populateProductTypeBigProductTest() {
    product.setProductType(ProductType.BIG_PRODUCT);
    ProductChange result = CommonUtil.populateProductType(product, productChange);
    assertEquals(result.getProductType().getCode(), ProductType.BIG_PRODUCT.getCode());
  }

  @Test
  public void populateProductTypeBopisProductTest() {
    product.setProductType(ProductType.BOPIS);
    ProductChange result = CommonUtil.populateProductType(product, productChange);
    assertEquals(result.getProductType().getCode(), ProductType.BOPIS.getCode());
  }

  @Test
  public void populateProductTypeRegularProductTest() {
    product.setProductType(ProductType.REGULAR);
    ProductChange result = CommonUtil.populateProductType(product, productChange);
    assertEquals(result.getProductType().getCode(), ProductType.REGULAR.getCode());
  }

  @Test
  public void isOnlyContentUpdatedTest() {
    editProductDetailDTO.setEditChangeType(EditChangeType.ITEM_PICKUP_POINT);
    boolean result1 = CommonUtil.isOnlyContentUpdated(false, editProductDetailDTO);
    assertFalse(result1);
    boolean result2 = CommonUtil.isOnlyContentUpdated(true, editProductDetailDTO);
    assertFalse(result2);
    editProductDetailDTO.setEditChangeType(EditChangeType.CONTENT);
    boolean result3 = CommonUtil.isOnlyContentUpdated(true, editProductDetailDTO);
    assertTrue(result3);
  }

  @Test
  public void isItemUpdatedTest() {
    List<Item> items = new ArrayList<>();
    items.add(item1);
    boolean result1 = CommonUtil.isItemUpdated(new ArrayList<>(), null);
    assertFalse(result1);
    boolean result2 = CommonUtil.isItemUpdated(items, null);
    assertTrue(result2);
    editProductDetailDTO.setUpdatedItemMap(new HashMap<>());
    boolean result3 = CommonUtil.isItemUpdated(new ArrayList<>(), editProductDetailDTO);
    assertFalse(result3);
    editProductDetailDTO.setUpdatedItemMap(Map.of(ITEM_SKU, item));
    boolean result4 = CommonUtil.isItemUpdated(new ArrayList<>(), editProductDetailDTO);
    assertTrue(result4);
  }

  @Test
  public void shouldCreateMasterSkuReturnsTrueWhenCategoriesMatch() {
    assertTrue(CommonUtil.shouldCreateMasterSku(categoryCodesHierarchy, categoriesToGenerateMasterSku));
  }

  @Test
  public void shouldCreateMasterSkuReturnsFalseWhenCategoryCodesHierarchyIsNull() {
    assertFalse(CommonUtil.shouldCreateMasterSku(null, categoriesToGenerateMasterSku));
  }

  @Test
  public void shouldCreateMasterSkuReturnsFalseWhenCategoryCodesHierarchyIsEmpty() {
    assertFalse(CommonUtil.shouldCreateMasterSku(Collections.emptyList(), categoriesToGenerateMasterSku));
  }

  @Test
  public void shouldCreateMasterSkuReturnsFalseWhenCategoriesToGenerateIsNull() {
    assertFalse(CommonUtil.shouldCreateMasterSku(categoryCodesHierarchy, null));
  }

  @Test
  public void shouldCreateMasterSkuReturnsFalseWhenCategoriesToGenerateIsBlank() {
    assertFalse(CommonUtil.shouldCreateMasterSku(categoryCodesHierarchy, ""));
  }

  @Test
  public void shouldCreateMasterSkuReturnsFalseWhenNoMatchingCategories() {
    assertFalse(CommonUtil.shouldCreateMasterSku(categoryCodesHierarchy, categoriesToGenerateMasterSku1));
  }

  @Test
  public void findMasterSkuInSourceItemsReturnsNullWhenSourceItemsIsNull() {
    assertEquals(StringUtils.EMPTY, CommonUtil.findMasterSkuFromSourceItems(null));
  }

  @Test
  public void findMasterSkuInSourceItemsReturnsNullWhenSourceItemsIsEmpty() {
    assertEquals(StringUtils.EMPTY, CommonUtil.findMasterSkuFromSourceItems(new ArrayList<>()));
  }

  @Test
  public void findMasterSkuInSourceItemsReturnsNullWhenNoItemHasMasterSku() {
    assertEquals(StringUtils.EMPTY, CommonUtil.findMasterSkuFromSourceItems(List.of(new Item())));
  }

  @Test
  public void findMasterSkuInSourceItemsReturnsNullWhenAllMasterSkusAreNull() {
    item.setMasterSku(null);
    assertEquals(StringUtils.EMPTY, CommonUtil.findMasterSkuFromSourceItems(List.of(item)));
  }

  @Test
  public void findMasterSkuInSourceItemsReturnsFirstNonNullMasterSku() {
    item.setMasterSku(masterSku);
    assertEquals(masterSku, CommonUtil.findMasterSkuFromSourceItems(List.of(item)));
  }

  @Test
  public void isDefiningOrVariantCreatingMasterDataItemAttributeTest() {
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    MasterDataItemAttributeValue masterDataItemAttributeValue = new MasterDataItemAttributeValue();
    masterDataItemAttributeValue.setMasterDataAttribute(masterDataAttribute);

    assertFalse(CommonUtil.isDefiningOrVariantCreating(masterDataItemAttributeValue));

    masterDataAttribute.setVariantCreation(true);
    assertTrue(CommonUtil.isDefiningOrVariantCreating(masterDataItemAttributeValue));

    masterDataAttribute.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    assertTrue(CommonUtil.isDefiningOrVariantCreating(masterDataItemAttributeValue));
  }

  @Test
  public void isMerchantPromoDiscountActiveAndMerchantPromoDiscountPriceIsNullTest() {
    itemPickupPoint.setMerchantPromoDiscount(true);
    boolean merchantPromoDiscount = CommonUtil.isMerchantPromoDiscountActive(itemPickupPoint, true);
    assertTrue(merchantPromoDiscount);
  }

  @Test
  public void isMerchantPromoDiscountActiveAndMerchantPromoDiscountPriceIsNonNullTest() {
    itemPickupPoint.setMerchantPromoDiscount(true);
    price.setMerchantPromoDiscountPrice(new DiscountPrice());
    itemPickupPoint.setPrice(new HashSet<>());
    itemPickupPoint.getPrice().add(price);
    boolean merchantPromoDiscount = CommonUtil.isMerchantPromoDiscountActive(itemPickupPoint, true);
    assertFalse(merchantPromoDiscount);
  }

  @Test
  public void isMerchantPromoDiscountActiveAndMerchantPromoDiscountPriceIsNullButIgnoreFlagFalseTest() {
    itemPickupPoint.setMerchantPromoDiscount(true);
    boolean merchantPromoDiscount = CommonUtil.isMerchantPromoDiscountActive(itemPickupPoint, false);
    assertFalse(merchantPromoDiscount);
  }

  @Test
  public void isMerchantPromoDiscountActivatedFalseTest() {
    itemPickupPoint.setMerchantPromoDiscount(false);
    boolean isMerchantPromoDiscountActivated = CommonUtil.isMerchantPromoDiscountActivated(itemPickupPoint);
    assertFalse(isMerchantPromoDiscountActivated);
  }

  @Test
  public void isMerchantPromoDiscountActivatedUpcomingPromoTest() {
    itemPickupPoint.setMerchantPromoDiscount(true);
    boolean isMerchantPromoDiscountActivated = CommonUtil.isMerchantPromoDiscountActivated(itemPickupPoint);
    assertFalse(isMerchantPromoDiscountActivated);
  }

  @Test
  public void isMerchantPromoDiscountActivatedOngoingPromoTest() {
    itemPickupPoint.setMerchantPromoDiscount(true);
    price.setMerchantPromoDiscountPrice(new DiscountPrice());
    itemPickupPoint.setPrice(new HashSet<>());
    itemPickupPoint.getPrice().add(price);
    boolean isMerchantPromoDiscountActivated = CommonUtil.isMerchantPromoDiscountActivated(itemPickupPoint);
    assertTrue(isMerchantPromoDiscountActivated);
  }

  @Test
  public void transformSolrFullReindexInputDocumentToSolrAtomicUpdateInputDocumentTest() {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, PRODUCT_SKU);
    solrInputDocument.setField(SolrFieldNames.IS_IN_STOCK, true);
    solrInputDocument.setField(SolrFieldNames.BRAND, BRAND);
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE, PREVIOUS_DATE);

    SolrInputDocument atomicUpdateDocument =
        CommonUtil.transformSolrFullReindexInputDocumentToSolrAtomicUpdateInputDocument(solrInputDocument, true);

    assertEquals(4, atomicUpdateDocument.size());
    assertEquals(PRODUCT_SKU, atomicUpdateDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, true),
        atomicUpdateDocument.getFieldValue(SolrFieldNames.IS_IN_STOCK));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, BRAND),
        atomicUpdateDocument.getFieldValue(SolrFieldNames.BRAND));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, PREVIOUS_DATE),
        atomicUpdateDocument.getFieldValue(SolrFieldNames.UPDATED_DATE));

  }

  @Test
  public void transformSolrFullReindexInputDocumentToSolrAtomicUpdateInputDocumentTest_StockNotUpdated() {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, PRODUCT_SKU);
    solrInputDocument.setField(SolrFieldNames.IS_IN_STOCK, false);
    solrInputDocument.setField(SolrFieldNames.BRAND, BRAND);

    SolrInputDocument atomicUpdateDocument =
        CommonUtil.transformSolrFullReindexInputDocumentToSolrAtomicUpdateInputDocument(solrInputDocument, null);

    assertEquals(2, atomicUpdateDocument.size());
    assertEquals(PRODUCT_SKU, atomicUpdateDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    assertNull(atomicUpdateDocument.getFieldValue(SolrFieldNames.IS_IN_STOCK));
    assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, BRAND),
        atomicUpdateDocument.getFieldValue(SolrFieldNames.BRAND));
  }

  @Test
  public void testRemoveNewlyAddedEventTypesForL5DataChange() {
    List<ItemPickupPointChangeEventType> eventTypes =
        Arrays.asList(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE,
            ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE,
          ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE);
    String newlyAddedEventTypes = "SOME_EVENT_TYPE,ANOTHER_EVENT_TYPE";
    List<ItemPickupPointChangeEventType> result =
        CommonUtil.removeNewlyAddedEventTypesForL5DataChange(eventTypes, newlyAddedEventTypes);
    assertEquals(3, result.size());
    assertFalse(result.contains(null));
    List<ItemPickupPointChangeEventType> nullList = null;
    List<ItemPickupPointChangeEventType> resultNullList =
        CommonUtil.removeNewlyAddedEventTypesForL5DataChange(nullList, newlyAddedEventTypes);
    assertNotNull(resultNullList);
    assertTrue(resultNullList.isEmpty());
    String nullNewlyAddedEventTypes = "";
    List<ItemPickupPointChangeEventType> resultNullNewlyAddedTypes =
        CommonUtil.removeNewlyAddedEventTypesForL5DataChange(eventTypes, nullNewlyAddedEventTypes);
    assertNotNull(resultNullNewlyAddedTypes);
    String eventTypeContainedInNewlyAdded = "BUYABLE_SCHEDULE_CHANGE";
    List<ItemPickupPointChangeEventType> eventTypesWithContainedType =
        Arrays.asList(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE,
            ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE, null);
    List<ItemPickupPointChangeEventType> resultWithContainedType =
        CommonUtil.removeNewlyAddedEventTypesForL5DataChange(eventTypesWithContainedType,
            eventTypeContainedInNewlyAdded);
    assertEquals(1, resultWithContainedType.size());
    assertTrue(resultWithContainedType.contains(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE));
  }

  @Test
  public void testRemoveNewlyAddedEventTypesForL5DataChangeForCncDiscoverable() {
    List<ItemPickupPointChangeEventType> eventTypes =
      Arrays.asList(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE,
        ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE,
        ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE);
    String newlyAddedEventTypes =
      String.valueOf(ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE)
        .concat(Constants.COMMA)
        .concat(String.valueOf(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE));
    List<ItemPickupPointChangeEventType> result =
      CommonUtil.removeNewlyAddedEventTypesForL5DataChange(eventTypes, newlyAddedEventTypes);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(
      result.stream().map(ItemPickupPointChangeEventType::name).filter(name -> name.equals(
          String.valueOf(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE)))
        .collect(Collectors.toList())));
  }

  @Test
  public void updateUpdatedFieldsIfCategoryIsUpdatedTest() {
    CommonUtil.updateFieldsForCategoryBrandOrNameChange(Arrays.asList(item), false, false, false);
    assertEquals(0, item.getUpdatedFields().size());
  }

  @Test
  public void updateUpdatedFieldsIfCategoryIsUpdatedUpdateCategoryTrueTest() {
    CommonUtil.updateFieldsForCategoryBrandOrNameChange(Arrays.asList(item), true, false, false);
    assertEquals(1, item.getUpdatedFields().size());
  }

  @Test
  public void addUpdatedFieldsInItemTest() {
    CommonUtil.addUpdatedFieldsInItem(Arrays.asList(item), new ArrayList<>());
    assertEquals(0, item.getUpdatedFields().size());
  }

  @Test
  public void updateUpdatedFieldsBrandAndNameUpdatedTest() {
    CommonUtil.updateFieldsForCategoryBrandOrNameChange(Arrays.asList(item), false, true, true);
    assertEquals(2, item.getUpdatedFields().size());
    assertEquals(Set.of(UpdatedFields.NAME_UPDATE.name(), UpdatedFields.BRAND_UPDATE.name()), item.getUpdatedFields());
  }

  @Test
  public void getLateFulfillmentFromProductTypeTest() {
    assertFalse(CommonUtil.getLateFulfillmentFromProductType(null, false, false));
    assertFalse(CommonUtil.getLateFulfillmentFromProductType(null, true, false));
    assertFalse(CommonUtil.getLateFulfillmentFromProductType(ProductType.REGULAR, true, true));
    assertTrue(CommonUtil.getLateFulfillmentFromProductType(ProductType.BIG_PRODUCT, true, true));
    assertTrue(CommonUtil.getLateFulfillmentFromProductType(ProductType.BOPIS, true, true));
  }

  @Test
  public void toBundleRecipesTest() {
    ProductBundleCreationRequest productBundleCreationRequest =
        new ProductBundleCreationRequest(List.of(new BundleRecipeVo(ITEM_SKU, 1)));
    Set<BundleRecipe> bundleRecipes = CommonUtil.toBundleRecipes(productBundleCreationRequest);
    assertEquals(ITEM_SKU, bundleRecipes.stream().findFirst().get().getItemSku());
    assertEquals(1, bundleRecipes.stream().findFirst().get().getQuantity());
  }

  @Test
  public void toCreateUpdateBillOfMaterialRecipeRequestTest() {
    ProductBundleCreationRequest productBundleCreationRequest =
        new ProductBundleCreationRequest(List.of(new BundleRecipeVo(ITEM_SKU, 1), new BundleRecipeVo(ITEM_SKU_2, 2)));
    Item item = new Item();
    item.setItemCode(ITEM_CODE_1);

    CreateUpdateBillOfMaterialRecipeCommandRequest createUpdateBillOfMaterialRecipeRequest =
        CommonUtil.toCreateUpdateBillOfMaterialRecipeRequest(item, productBundleCreationRequest,
            Map.of(ITEM_SKU, ITEM_CODE_1, ITEM_SKU_2, ITEM_CODE_2));

    assertEquals(ITEM_CODE_1, createUpdateBillOfMaterialRecipeRequest.getItemCode());
    assertEquals(ITEM_CODE_1, createUpdateBillOfMaterialRecipeRequest.getBillOfMaterialSetup().get(0).getItemCode());
    assertEquals(1, createUpdateBillOfMaterialRecipeRequest.getBillOfMaterialSetup().get(0).getQuantity().intValue());
    assertEquals(ITEM_CODE_2, createUpdateBillOfMaterialRecipeRequest.getBillOfMaterialSetup().get(1).getItemCode());
    assertEquals(2, createUpdateBillOfMaterialRecipeRequest.getBillOfMaterialSetup().get(1).getQuantity().intValue());
  }

  @Test
  public void getProductTypeTest() {
    assertNull(CommonUtil.getProductType(StringUtils.EMPTY));
    assertEquals(ProductType.BOPIS, CommonUtil.getProductType("bopis"));
    assertNull(CommonUtil.getProductType(PRODUCT_CODE));
  }

  @Test
  public void validateItemTest() {
    CommonUtil.validateItem(item.getItemSku(), item);
    try {
      CommonUtil.validateItem(item.getItemSku(), null);
    } catch (ApiIncorrectInputDataException exception) {
      assertEquals(ErrorMessages.NO_ITEM_FOUND, exception.getErrorMessage());
    }
  }

  @Test
  public void isBuyableAndDiscoverableScheduleChangedTest() {
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo =
        new ItemPickupPointListingUpdateRequestVo();
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo1 =
        itemPickupPointListingUpdateRequestVo;
    itemPickupPointListingUpdateRequestVo1.setScheduleRemoval(true);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    assertFalse(
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, itemPickupPointListingUpdateRequestVo, eventTypes,
            itemPickupPoint, false));
    assertTrue(
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(true, itemPickupPointListingUpdateRequestVo1, eventTypes,
            itemPickupPoint, false));
    assertTrue(
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(true, itemPickupPointListingUpdateRequestVo1, eventTypes,
            itemPickupPoint, true));
    eventTypes.add(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE);
    eventTypes.add(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE);
    assertFalse(
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, itemPickupPointListingUpdateRequestVo1, eventTypes,
            itemPickupPoint, true));
  }

  @Test
  public void isBuyableAndDiscoverableScheduleChangedSwitchOffTest() {
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo =
        new ItemPickupPointListingUpdateRequestVo();
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo1 =
        itemPickupPointListingUpdateRequestVo;
    itemPickupPointListingUpdateRequestVo1.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    assertFalse(
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, itemPickupPointListingUpdateRequestVo, eventTypes,
            itemPickupPoint, false));
    assertTrue(
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(true, itemPickupPointListingUpdateRequestVo1, eventTypes,
            itemPickupPoint, false));
    assertTrue(
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(true, itemPickupPointListingUpdateRequestVo1, eventTypes,
            itemPickupPoint, true));
    eventTypes.add(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE);
    eventTypes.add(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE);
    assertFalse(
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, itemPickupPointListingUpdateRequestVo1, eventTypes,
            itemPickupPoint, true));
  }

  @Test
  public void testScheduleRemoval() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    requestVo.setScheduleRemoval(true);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint itemPickupPointWithSchedule = createItemPickupPointWithSchedules();

    boolean result =
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, itemPickupPointWithSchedule,
            true);

    assertTrue(result);
  }

  @Test
  public void testNoScheduleChangeWhenNotModified() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint itemPickupPointWithSchedule = createItemPickupPointWithSchedules();

    CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, itemPickupPointWithSchedule, true);

    assertTrue(eventTypes.isEmpty());
  }

  @Test
  public void testNoScheduleChangeWhenNotAdded() {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    requestVo.setBuyableScheduleVo(new BuyableScheduleVo());
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint l5 = new ItemPickupPoint();

    boolean result = CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, l5, true);

    assertTrue(result);
    assertFalse(eventTypes.isEmpty());
  }


  @Test
  public void testBuyableScheduleModification() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Date startDate = sdf.parse("2024-01-10T00:00:00");
    Date endDate = sdf.parse("2024-09-30T23:59:59");
    requestVo.setBuyableScheduleVo(createBuyableSchedule(startDate, endDate));
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint pickupPointWithSchedules = createItemPickupPointWithSchedules();

    boolean result =
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, pickupPointWithSchedules,
            true);

    assertTrue(result);
    assertTrue(eventTypes.contains(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE));
  }

  @Test
  public void testBuyableScheduleModificationWithNoExistingSchedule() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Date startDate = sdf.parse("2024-01-10T00:00:00");
    Date endDate = sdf.parse("2024-09-30T23:59:59");
    requestVo.setBuyableScheduleVo(createBuyableSchedule(startDate, endDate));
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint pickupPoint = new ItemPickupPoint();

    boolean result =
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, pickupPoint, true);

    assertTrue(result);
    assertTrue(eventTypes.contains(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE));
  }


  @Test
  public void testDiscoverableScheduleNoTimeModification() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Date startDate = sdf.parse("2024-01-01T00:00:00");
    Date endDate = sdf.parse("2024-12-31T23:59:59");
    requestVo.setDiscoverableScheduleVo(createDiscoverableSchedule(startDate, endDate));
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint itemPickupPointWithSchedules = createItemPickupPointWithSchedules();

    boolean result =
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, itemPickupPointWithSchedules,
            true);

    assertFalse(eventTypes.contains(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE));
  }

  @Test
  public void testDiscoverableScheduleStartTimeModification() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Date startDate = sdf.parse("2024-08-01T00:00:00");
    Date endDate = sdf.parse("2024-12-31T23:59:59");
    requestVo.setDiscoverableScheduleVo(createDiscoverableSchedule(startDate, endDate));
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint withSchedules = createItemPickupPointWithSchedules();

    boolean result =
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, withSchedules, true);

    assertTrue(result);
    assertTrue(eventTypes.contains(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE));
  }

  @Test
  public void testDiscoverableScheduleTimeModification() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Date startDate = sdf.parse("2024-01-01T00:00:00");
    Date endDate = sdf.parse("2025-12-31T23:59:59");
    requestVo.setDiscoverableScheduleVo(createDiscoverableSchedule(startDate, endDate));
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint pickupPointWithSchedules = createItemPickupPointWithSchedules();

    boolean result =
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, pickupPointWithSchedules,
            true);

    assertTrue(result);
    assertTrue(eventTypes.contains(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE));
  }

  @Test
  public void testDiscoverableScheduleModificationWithNoExistingSchedule() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Date startDate = sdf.parse("2024-01-01T00:00:00");
    Date endDate = sdf.parse("2024-12-31T23:59:59");
    requestVo.setDiscoverableScheduleVo(createDiscoverableSchedule(startDate, endDate));
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint pickupPoint = new ItemPickupPoint();

    boolean result =
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, pickupPoint, true);

    assertTrue(result);
    assertTrue(eventTypes.contains(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE));
  }

  @Test
  public void testBuyableScheduleModificationSchedule() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Date startDate = sdf.parse("2024-08-01T00:00:00");
    Date endDate = sdf.parse("2024-12-31T23:59:59");
    requestVo.setBuyableScheduleVo(createBuyableSchedule(startDate, endDate));
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint itemPickupPointWithSchedules = createItemPickupPointWithSchedules();
    boolean result =
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, itemPickupPointWithSchedules,
            true);

    assertTrue(result);
    assertTrue(eventTypes.contains(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE));
  }

  @Test
  public void testBothSchedulesModification() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Date startDate = sdf.parse("2024-01-01T00:00:00");
    Date endDate = sdf.parse("2024-12-31T23:59:59");
    requestVo.setBuyableScheduleVo(createBuyableSchedule(startDate, endDate));
    requestVo.setDiscoverableScheduleVo(createDiscoverableSchedule(startDate, endDate));
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint withSchedules = createItemPickupPointWithSchedules();

    boolean result =
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, withSchedules, true);

    assertFalse(result);
    assertFalse(eventTypes.contains(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE));
    assertFalse(eventTypes.contains(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE));
  }

  @Test
  public void testBothSchedulesModificationOnlyEndDateChange() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Date startDate = sdf.parse("2024-01-01T00:00:00");
    Date endDate = sdf.parse("2025-12-31T23:59:59");
    requestVo.setBuyableScheduleVo(createBuyableSchedule(startDate, endDate));
    requestVo.setDiscoverableScheduleVo(createDiscoverableSchedule(startDate, endDate));
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = createItemPickupPointWithSchedules();

    boolean result =
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, itemPickupPoint, true);

    assertTrue(result);
    assertTrue(eventTypes.contains(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE));
    assertTrue(eventTypes.contains(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE));
  }

  @Test
  public void testBothSchedulesModification2() throws ParseException {
    ItemPickupPointListingUpdateRequestVo requestVo = new ItemPickupPointListingUpdateRequestVo();
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Date startDate = sdf.parse("2024-08-01T00:00:00");
    Date endDate = sdf.parse("2025-12-31T23:59:59");
    requestVo.setBuyableScheduleVo(createBuyableSchedule(startDate, endDate));
    requestVo.setDiscoverableScheduleVo(createDiscoverableSchedule(startDate, endDate));
    requestVo.setScheduleRemoval(false);
    List<ItemPickupPointChangeEventType> eventTypes = new ArrayList<>();
    ItemPickupPoint pickupPointWithSchedules = createItemPickupPointWithSchedules();

    boolean result =
        CommonUtil.isBuyableAndDiscoverableScheduleChanged(false, requestVo, eventTypes, pickupPointWithSchedules,
            true);

    assertTrue(result);
    assertTrue(eventTypes.contains(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE));
    assertTrue(eventTypes.contains(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE));
  }

  // Helper methods for creating test data
  private ItemPickupPoint createItemPickupPointWithSchedules() throws ParseException {
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Date startDate = sdf.parse("2024-01-01T00:00:00");
    Date endDate = sdf.parse("2024-12-31T23:59:59");
    ItemPickupPoint l5 = new ItemPickupPoint();
    ItemViewConfig config = new ItemViewConfig();
    BuyableScheduleVo buyableSchedule = createBuyableSchedule(startDate, endDate);
    DiscoverableScheduleVo discoverableSchedule = createDiscoverableSchedule(startDate, endDate);
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    BeanUtils.copyProperties(buyableSchedule, itemBuyableSchedule);
    BeanUtils.copyProperties(discoverableSchedule, itemDiscoverableSchedule);
    config.setItemBuyableSchedules(itemBuyableSchedule);
    config.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    config.setChannel(Constants.DEFAULT_CHANNEL);
    l5.setItemViewConfig(Collections.singleton(config));
    return l5;
  }

  private BuyableScheduleVo createBuyableSchedule(Date start, Date end) {
    BuyableScheduleVo schedule = new BuyableScheduleVo();
    schedule.setStartDateTime(start);
    schedule.setEndDateTime(end);
    schedule.setBuyable(true);
    return schedule;
  }

  private DiscoverableScheduleVo createDiscoverableSchedule(Date start, Date end) {
    DiscoverableScheduleVo schedule = new DiscoverableScheduleVo();
    schedule.setStartDateTime(start);
    schedule.setEndDateTime(end);
    schedule.setDiscoverable(true);
    return schedule;
  }

  @Test
  public void setSchedulesForAddPickupPointTest() {
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemViewConfig(new HashSet<>());
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo =
        new ItemPickupPointListingUpdateRequestVo();
    itemPickupPointListingUpdateRequestVo.setBuyableScheduleVo(new BuyableScheduleVo());
    CommonUtil.setSchedulesForAddPickupPoint(true, itemPickupPointListingUpdateRequestVo, itemPickupPoint1);
  }

  @Test
  public void setSchedulesForAddPickupPointDiscoverableTest() {
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemViewConfig(new HashSet<>());
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo =
        new ItemPickupPointListingUpdateRequestVo();
    itemPickupPointListingUpdateRequestVo.setDiscoverableScheduleVo(new DiscoverableScheduleVo());
    CommonUtil.setSchedulesForAddPickupPoint(true, itemPickupPointListingUpdateRequestVo, itemPickupPoint1);
  }

  @Test
  public void setSchedulesForAddPickupPointDiscoverableScheduleNotEnabledTest() {
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemViewConfig(new HashSet<>());
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo =
        new ItemPickupPointListingUpdateRequestVo();
    itemPickupPointListingUpdateRequestVo.setBuyableScheduleVo(new BuyableScheduleVo());
    CommonUtil.setSchedulesForAddPickupPoint(false, itemPickupPointListingUpdateRequestVo, itemPickupPoint1);
  }

  @Test
  public void clearSchedulesTest() {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    CommonUtil.clearSchedules(auditTrailDtoList, item, itemPickupPoint);
    assertEquals(new ArrayList<>(), auditTrailDtoList);
  }

  @Test
  public void clearBuyableAndDiscoverableSchedulesTest() {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    defaultItemViewConfig.setItemDiscoverableSchedules(new ItemDiscoverableSchedule());
    defaultItemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule());
    itemPickupPoint.setItemViewConfig(new HashSet<>(Arrays.asList(defaultItemViewConfig)));
    CommonUtil.clearSchedules(auditTrailDtoList, item, itemPickupPoint);
    assertEquals(2, auditTrailDtoList.size());
    assertNull(defaultItemViewConfig.getItemBuyableSchedules());
    assertNull(defaultItemViewConfig.getItemDiscoverableSchedules());
  }

  @Test
  public void clearScheduleTest() {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypeList = new ArrayList<>();
    CommonUtil.clearSchedule(auditTrailDtoList, item, itemPickupPoint, itemPickupPointChangeEventTypeList);
    assertEquals(new ArrayList<>(), auditTrailDtoList);
    assertEquals(new ArrayList<>(), itemPickupPointChangeEventTypeList);
  }

  @Test
  public void clearBuyableAndDiscoverableScheduleTest() {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypeList = new ArrayList<>();
    defaultItemViewConfig.setItemDiscoverableSchedules(new ItemDiscoverableSchedule());
    defaultItemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule());
    itemPickupPoint.setItemViewConfig(new HashSet<>(Arrays.asList(defaultItemViewConfig)));
    CommonUtil.clearSchedule(auditTrailDtoList, item, itemPickupPoint, itemPickupPointChangeEventTypeList);
    assertEquals(2, auditTrailDtoList.size());
    assertNull(defaultItemViewConfig.getItemBuyableSchedules());
    assertNull(defaultItemViewConfig.getItemDiscoverableSchedules());
    assertEquals(2, itemPickupPointChangeEventTypeList.size());
  }

  @Test
  public void isScheduleCompletedNullTest() {
    boolean scheduleCompleted = CommonUtil.isScheduleCompleted(null);
    assertTrue(scheduleCompleted);
  }

  @Test
  public void isScheduleCompletedTrueTest() {
    Date endDate = Date.from(Instant.now().minus(Duration.ofDays(1)));
    boolean scheduleCompleted = CommonUtil.isScheduleCompleted(endDate);
    assertTrue(scheduleCompleted);
  }

  @Test
  public void isScheduleCompletedFlaseTest() {
    Date endDate = Date.from(Instant.now().plus(Duration.ofDays(1)));
    boolean scheduleCompleted = CommonUtil.isScheduleCompleted(endDate);
    assertFalse(scheduleCompleted);
  }

  @Test
  public void filterBusinessPartnerPickupPointBasedOnMerchantCodeTest() {
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setBusinessPartnerCode("BPCODE_1");
    BusinessPartnerPickupPoint businessPartnerPickupPoint1 = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint1.setBusinessPartnerCode("BPCODE_2");
    List<BusinessPartnerPickupPoint> businessPartnerPickupPoints =
        CommonUtil.filterBusinessPartnerPickupPointBasedOnMerchantCode("BPCODE_1",
            Arrays.asList(businessPartnerPickupPoint, businessPartnerPickupPoint1), true);
    assertEquals(1, businessPartnerPickupPoints.size());
  }

  @Test
  public void filterBusinessPartnerPickupPointBasedOnMerchantCodeWithSecuritySwitchOff() {
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setBusinessPartnerCode("BPCODE_1");
    BusinessPartnerPickupPoint businessPartnerPickupPoint1 = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint1.setBusinessPartnerCode("BPCODE_2");
    List<BusinessPartnerPickupPoint> businessPartnerPickupPoints =
        CommonUtil.filterBusinessPartnerPickupPointBasedOnMerchantCode("BPCODE_1",
            Arrays.asList(businessPartnerPickupPoint, businessPartnerPickupPoint1), false);
    assertEquals(2, businessPartnerPickupPoints.size());
  }

  @Test
  public void filterBusinessPartnerPickupPointBasedOnMerchantCodeWithMerchantCodeNull() {
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setBusinessPartnerCode("BPCODE_1");
    BusinessPartnerPickupPoint businessPartnerPickupPoint1 = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint1.setBusinessPartnerCode("BPCODE_2");
    List<BusinessPartnerPickupPoint> businessPartnerPickupPoints =
        CommonUtil.filterBusinessPartnerPickupPointBasedOnMerchantCode(null,
            Arrays.asList(businessPartnerPickupPoint, businessPartnerPickupPoint1), true);
    assertEquals(2, businessPartnerPickupPoints.size());
  }

  @Test
  public void clearSchedulesNeedRevisionTest() {
    defaultItemViewConfig.setItemDiscoverableSchedules(new ItemDiscoverableSchedule());
    defaultItemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule());
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes = new ArrayList<>();
    itemPickupPoint.setItemViewConfig(new HashSet<>(List.of(defaultItemViewConfig)));
    CommonUtil.clearSchedules(itemPickupPoint, itemPickupPointChangeEventTypes);
    assertNull(defaultItemViewConfig.getItemBuyableSchedules());
    assertNull(defaultItemViewConfig.getItemDiscoverableSchedules());
    assertEquals(2, itemPickupPointChangeEventTypes.size());
  }

  @Test
  public void clearSchedulesDiscoverableTest() {
    defaultItemViewConfig.setItemDiscoverableSchedules(new ItemDiscoverableSchedule());
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes = new ArrayList<>();
    itemPickupPoint.setItemViewConfig(new HashSet<>(List.of(defaultItemViewConfig)));
    CommonUtil.clearSchedules(itemPickupPoint, itemPickupPointChangeEventTypes);
    assertNull(defaultItemViewConfig.getItemDiscoverableSchedules());
    assertEquals(1, itemPickupPointChangeEventTypes.size());
  }

  @Test
  public void clearSchedulesBuyableTest() {
    defaultItemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule());
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes = new ArrayList<>();
    itemPickupPoint.setItemViewConfig(new HashSet<>(List.of(defaultItemViewConfig)));
    CommonUtil.clearSchedules(itemPickupPoint, itemPickupPointChangeEventTypes);
    assertNull(defaultItemViewConfig.getItemBuyableSchedules());
    assertEquals(1, itemPickupPointChangeEventTypes.size());
  }

  @Test
  public void validateForBopisCategoryRestrictionTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType("CM");
    String merchantTypesForBopisCategoryValidation = "CM,CC";
    boolean bopisNotEligibleForCategory = true;
    boolean result =
        CommonUtil.validateForBopisCategoryRestriction(profileResponse, merchantTypesForBopisCategoryValidation,
            bopisNotEligibleForCategory);
    assertTrue(result);
    bopisNotEligibleForCategory = false;
    result = CommonUtil.validateForBopisCategoryRestriction(profileResponse, merchantTypesForBopisCategoryValidation,
        bopisNotEligibleForCategory);
    assertFalse(result);
    profileResponse.getCompany().setMerchantType("TypeC");
    result = CommonUtil.validateForBopisCategoryRestriction(profileResponse, merchantTypesForBopisCategoryValidation,
        bopisNotEligibleForCategory);
    assertFalse(result);
  }

  @Test
  public void fetchDimensionsMissingForBopisProductTest() {
    Item item = new Item();
    item.setHeight(0.0);
    item.setLength(0.0);
    item.setWidth(0.0);
    item.setWeight(0.0);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType("CM");
    String merchantTypesForBopisCategoryValidation = "CM,CC";
    boolean bopisNotEligibleForCategory = true;
    boolean result =
        CommonUtil.fetchDimensionsMissingForBopisProduct(item, profileResponse, merchantTypesForBopisCategoryValidation,
            bopisNotEligibleForCategory);
    assertTrue(result);
    bopisNotEligibleForCategory = false;
    result =
        CommonUtil.fetchDimensionsMissingForBopisProduct(item, profileResponse, merchantTypesForBopisCategoryValidation,
            bopisNotEligibleForCategory);
    assertFalse(result);
    item.setHeight(1.0);
    result =
        CommonUtil.fetchDimensionsMissingForBopisProduct(item, profileResponse, merchantTypesForBopisCategoryValidation,
            bopisNotEligibleForCategory);
    assertFalse(result);
  }

  @Test
  public void isAllDimensionsAreZeroTest() {
    Item item = new Item();
    item.setHeight(0.0);
    item.setLength(0.0);
    item.setWidth(0.0);
    item.setWeight(0.0);
    boolean result = CommonUtil.isAllDimensionsAreZero(item);
    assertTrue(result);
    item.setHeight(1.0);
    result = CommonUtil.isAllDimensionsAreZero(item);
    assertFalse(result);
    result = CommonUtil.isAllDimensionsAreZero(null);
    assertFalse(result);
  }

  @Test
  public void toProductSkuSizeChartCodeTest() {
    solrDocumentL3.addField(SolrFieldNames.SIZE_CHART_CODE, SolrFieldNames.SIZE_CHART_CODE);
    CommonUtil.toProductSkuSizeChartCode(solrDocumentL3);
    solrDocumentL3.remove(SolrFieldNames.PRODUCT_SKU);
    solrDocumentL3.remove(SolrFieldNames.SIZE_CHART_CODE);
    ProductSkuSizeChartResponse productSkuSizeChartCode = CommonUtil.toProductSkuSizeChartCode(solrDocumentL3);
    Assertions.assertTrue(Objects.nonNull(productSkuSizeChartCode));
  }

  @Test
  public void isProductUpdatedTest() {
    CommonUtil.isProductUpdated(false, false, false);
    CommonUtil.isProductUpdated(false, false, true);
    CommonUtil.isProductUpdated(false, true, true);
    Assertions.assertTrue(CommonUtil.isProductUpdated(true, true, true));
  }

  @Test
  public void updateCncAtL5Test() {
    ProductAndL5MigrationRequest productAndL5MigrationRequest = new ProductAndL5MigrationRequest();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    CommonUtil.updateCncAtL5(productAndL5MigrationRequest, itemPickupPoint);
    productAndL5MigrationRequest.setCncActiveAtL5(true);
    CommonUtil.updateCncAtL5(productAndL5MigrationRequest, itemPickupPoint);
    productAndL5MigrationRequest.setCncActiveAtL5(false);
    itemPickupPoint.setCncActive(true);
    Assertions.assertTrue(CommonUtil.updateCncAtL5(productAndL5MigrationRequest, itemPickupPoint));
    productAndL5MigrationRequest.setCncActiveAtL5(true);
    itemPickupPoint.setCncActive(true);
    Assertions.assertFalse(CommonUtil.updateCncAtL5(productAndL5MigrationRequest, itemPickupPoint));
  }

  @Test
  public void checkAndUpdateStatusInItemViewConfigTest() {
    productAndL5MigrationRequest.setDiscoverable(false);
    productAndL5MigrationRequest.setBuyable(false);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    CommonUtil.checkAndUpdateStatusInItemViewConfig(productAndL5MigrationRequest, false, new HashSet<>(),
        itemViewConfig);
    CommonUtil.checkAndUpdateStatusInItemViewConfig(productAndL5MigrationRequest, false, new HashSet<>(), null);
    productAndL5MigrationRequest.setDiscoverable(true);
    CommonUtil.checkAndUpdateStatusInItemViewConfig(productAndL5MigrationRequest, false, new HashSet<>(),
        itemViewConfig);
    productAndL5MigrationRequest.setDiscoverable(false);
    productAndL5MigrationRequest.setBuyable(true);
    itemViewConfig.setDiscoverable(false);
    CommonUtil.checkAndUpdateStatusInItemViewConfig(productAndL5MigrationRequest, false, new HashSet<>(),
        itemViewConfig);
    ItemBuyableSchedule itemBuyableSchedules = new ItemBuyableSchedule();
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    LocalDateTime tomorrow = LocalDateTime.now().plusDays(1);
    Date endDate = Date.from(tomorrow.atZone(ZoneId.systemDefault()).toInstant());
    itemBuyableSchedules.setEndDateTime(endDate);
    itemDiscoverableSchedule.setEndDateTime(endDate);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedules);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    CommonUtil.checkAndUpdateStatusInItemViewConfig(productAndL5MigrationRequest, false, new HashSet<>(),
        itemViewConfig);
  }

  @Test
  public void checkAndUpdateStatusInItemViewConfigTest2() {
    productAndL5MigrationRequest.setDiscoverable(false);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    productAndL5MigrationRequest.setDiscoverable(false);
    productAndL5MigrationRequest.setBuyable(true);
    itemViewConfig.setDiscoverable(false);
    ItemBuyableSchedule itemBuyableSchedules = new ItemBuyableSchedule();
    ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
    LocalDateTime tomorrow = LocalDateTime.now().plusDays(1);
    Date endDate = Date.from(tomorrow.atZone(ZoneId.systemDefault()).toInstant());
    itemBuyableSchedules.setEndDateTime(endDate);
    itemDiscoverableSchedule.setEndDateTime(endDate);
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedules);
    itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    CommonUtil.checkAndUpdateStatusInItemViewConfig(productAndL5MigrationRequest, false, new HashSet<>(),
        itemViewConfig);
  }

  @Test
  public void checkAndSetDimensionsMissingTest() {
    productAndL5MigrationRequest.setDimensionsMissing(null);
    CommonUtil.checkAndSetDimensionsMissing(product, productAndL5MigrationRequest);
    productAndL5MigrationRequest.setDimensionsMissing(true);
    product.setDimensionsMissing(true);
    CommonUtil.checkAndSetDimensionsMissing(product, productAndL5MigrationRequest);
    product.setDimensionsMissing(false);
    CommonUtil.checkAndSetDimensionsMissing(product, productAndL5MigrationRequest);
  }

  @Test
  public void checkAndSetProductTypeTest() {
    product.setProductType(null);
    CommonUtil.checkAndSetProductType(product, productAndL5MigrationRequest);
    product.setProductType(ProductType.REGULAR);
    productAndL5MigrationRequest.setProductType(ProductType.REGULAR);
    CommonUtil.checkAndSetProductType(product, productAndL5MigrationRequest);
    productAndL5MigrationRequest.setProductType(ProductType.BIG_PRODUCT);
    CommonUtil.checkAndSetProductType(product, productAndL5MigrationRequest);
  }

  @Test
  public void checkAndSetCncActivatedAtL3Test() {
    productAndL5MigrationRequest.setCncActiveAtL3(null);
    CommonUtil.checkAndSetCncActivatedAtL3(product, productAndL5MigrationRequest);
    productAndL5MigrationRequest.setCncActiveAtL3(true);
    product.setCncActivated(true);
    CommonUtil.checkAndSetCncActivatedAtL3(product, productAndL5MigrationRequest);
    product.setCncActivated(false);
    CommonUtil.checkAndSetCncActivatedAtL3(product, productAndL5MigrationRequest);
  }

  @Test
  public void checkAndSetLateFulfillmentTest() {
    productAndL5MigrationRequest.setProductType(null);
    CommonUtil.checkAndSetLateFulfillment(Collections.singletonList(item1), productAndL5MigrationRequest);
    productAndL5MigrationRequest.setProductType(ProductType.BIG_PRODUCT);
    CommonUtil.checkAndSetLateFulfillment(Collections.singletonList(item1), productAndL5MigrationRequest);
    productAndL5MigrationRequest.setProductType(ProductType.REGULAR);
    CommonUtil.checkAndSetLateFulfillment(Collections.singletonList(item1), productAndL5MigrationRequest);
  }

  @Test
  public void getProductAndL5MigrationRequestTest() {
    ProductAndL5MigrationRequest productAndL5MigrationRequest1 = CommonUtil.getProductAndL5MigrationRequest(product);
    Assertions.assertTrue(Objects.nonNull(productAndL5MigrationRequest1));
  }

  @Test
  public void toEanUpcPickupPointCodeResponseTest() {
    EanUpcPickupPointCodeResponse eanUpcPickupPointCodeResponse =
        CommonUtil.toEanUpcPickupPointCodeResponse(itemPickupPoint, new HashMap<>());
    Assertions.assertTrue(Objects.nonNull(eanUpcPickupPointCodeResponse));
  }

  @Test
  public void processRemovalOfSchedulesTest() {
    boolean bol = CommonUtil.processRemovalOfSchedules(true, new ArrayList<>(), itemPickupPoint);
    Assertions.assertTrue(bol);
  }

  @Test
  public void testMissingFieldsWhenDimensionsAreZero() {
    Product productData = new Product();
    productData.setOff2OnChannelActive(true);
    productData.setB2cActivated(false);
    productDetailResponse.setLength(ZERO_DIMENSIONS);
    productDetailResponse.setWeight(ZERO_DIMENSIONS);
    productDetailResponse.setWidth(ZERO_DIMENSIONS);
    productDetailResponse.setShippingWeight(ZERO_DIMENSIONS);
    productDetailResponse.setDescription(new byte[8]);
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse, productData);
    assertTrue(productData.getMissingFields().contains(Constants.DIMENSIONS_MISSING));
  }

  @Test
  public void testProductNull() {
    Product productData = new Product();
    productData.setOff2OnChannelActive(true);
    productData.setB2cActivated(false);
    productDetailResponse.setLength(ZERO_DIMENSIONS);
    productDetailResponse.setWeight(ZERO_DIMENSIONS);
    productDetailResponse.setWidth(ZERO_DIMENSIONS);
    productDetailResponse.setShippingWeight(ZERO_DIMENSIONS);
    productDetailResponse.setDescription(new byte[8]);
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse, null);
  }

  @Test
  public void testMissingFieldsWhenDimensionsAreZeroAndProductTypeRegular() {
    Product productData = new Product();
    productData.setOff2OnChannelActive(true);
    productData.setB2cActivated(false);
    productData.setProductType(ProductType.REGULAR);
    productDetailResponse.setLength(ZERO_DIMENSIONS);
    productDetailResponse.setWeight(ZERO_DIMENSIONS);
    productDetailResponse.setWidth(ZERO_DIMENSIONS);
    productDetailResponse.setShippingWeight(ZERO_DIMENSIONS);
    productDetailResponse.setDescription(new byte[8]);
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse, productData);
    assertTrue(productData.getMissingFields().contains(Constants.DIMENSIONS_MISSING));
  }

  @Test
  public void testMissingFieldsWhenDimensionsAreZeroAndProductTypeBopis() {
    Product productData = new Product();
    productData.setOff2OnChannelActive(true);
    productData.setB2cActivated(false);
    productData.setProductType(ProductType.BOPIS);
    productDetailResponse.setLength(ZERO_DIMENSIONS);
    productDetailResponse.setWeight(ZERO_DIMENSIONS);
    productDetailResponse.setWidth(ZERO_DIMENSIONS);
    productDetailResponse.setShippingWeight(ZERO_DIMENSIONS);
    productDetailResponse.setDescription(new byte[8]);
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse, productData);
    assertFalse(productData.getMissingFields().contains(Constants.DIMENSIONS_MISSING));
  }

  @Test
  public void testMissingFieldsWhenDescriptionIsBlank() {
    Product productData = new Product();
    productData.setOff2OnChannelActive(true);
    productDetailResponse.setLength(1.0);
    productDetailResponse.setWeight(1.0);
    productDetailResponse.setWidth(1.0);
    productDetailResponse.setShippingWeight(1.0);
    productDetailResponse.setDescription(null);
    productData.setB2cActivated(false);
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse, productData);
    assertTrue(productData.getMissingFields().contains(Constants.DESCRIPTION_MISSING));
  }

  @Test
  public void testNoMissingFieldsWhenProductIsOff2OnChannelInactive() {
    product.setOff2OnChannelActive(false);
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse, product);
    assertTrue(product.getMissingFields().contains(Constants.DIMENSIONS_MISSING));
    assertFalse(product.getMissingFields().contains(Constants.DESCRIPTION_MISSING));
  }

  @Test
  public void testNoMissingFieldsWhenInstoreNewFlowDisabled() {
    product.setOff2OnChannelActive(true);
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse, product);
    assertTrue(product.getMissingFields().contains(Constants.DIMENSIONS_MISSING));
    assertFalse(product.getMissingFields().contains(Constants.DESCRIPTION_MISSING));
  }

  @Test
  public void testNoMissingFieldsWhenDimensionsAndDescriptionAreValid() {
    product.setOff2OnChannelActive(true);
    productDetailResponse.setLength(1.0);
    productDetailResponse.setWeight(1.0);
    productDetailResponse.setWidth(1.0);
    productDetailResponse.setShippingWeight(1.0);
    productDetailResponse.setDescription(new byte[90]);
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse, product);
    assertFalse(product.getMissingFields().contains(Constants.DIMENSIONS_MISSING));
    assertFalse(product.getMissingFields().contains(Constants.DESCRIPTION_MISSING));
  }

  @Test
  public void testNoMissingFieldsWhenDimensionsAndDescriptionAreValidAndProductTypeRegular() {
    product.setOff2OnChannelActive(true);
    product.setProductType(ProductType.REGULAR);
    productDetailResponse.setLength(1.0);
    productDetailResponse.setWeight(1.0);
    productDetailResponse.setWidth(1.0);
    productDetailResponse.setShippingWeight(1.0);
    productDetailResponse.setDescription(new byte[90]);
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse, product);
    assertFalse(product.getMissingFields().contains(Constants.DIMENSIONS_MISSING));
    assertFalse(product.getMissingFields().contains(Constants.DESCRIPTION_MISSING));
  }

  @Test
  public void testNoMissingFieldsWhenDimensionsAndDescriptionAreValidAndProductTypeBopis() {
    product.setOff2OnChannelActive(true);
    product.setProductType(ProductType.BOPIS);
    productDetailResponse.setLength(1.0);
    productDetailResponse.setWeight(1.0);
    productDetailResponse.setWidth(1.0);
    productDetailResponse.setShippingWeight(1.0);
    productDetailResponse.setDescription(new byte[90]);
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse, product);
    assertFalse(product.getMissingFields().contains(Constants.DIMENSIONS_MISSING));
    assertFalse(product.getMissingFields().contains(Constants.DESCRIPTION_MISSING));
  }

  @Test
  public void isCncActivatedItemTest() {
    Assertions.assertFalse(CommonUtil.isNotCncActivatedItem(null));
    item.setCncActivated(true);
    Assertions.assertFalse(CommonUtil.isNotCncActivatedItem(item));
    item.setCncActivated(false);
    Assertions.assertTrue(CommonUtil.isNotCncActivatedItem(item));
  }

  @Test
  public void isL5StatusIsOnlineTest() {
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo =
        new ItemPickupPointListingUpdateRequestVo();
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(new HashSet<>());
    Assertions.assertFalse(CommonUtil.isL5StatusIsOnline(itemPickupPointListingUpdateRequestVo, false));
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(false);
    itemViewConfig.setBuyable(false);
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(Collections.singleton(itemViewConfig));
    Assertions.assertFalse(CommonUtil.isL5StatusIsOnline(itemPickupPointListingUpdateRequestVo, false));
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setBuyable(false);
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(Collections.singleton(itemViewConfig));
    Assertions.assertTrue(CommonUtil.isL5StatusIsOnline(itemPickupPointListingUpdateRequestVo, false));
    itemViewConfig.setBuyable(true);
    Assertions.assertTrue(CommonUtil.isL5StatusIsOnline(itemPickupPointListingUpdateRequestVo, false));
  }

  @Test
  public void publishProductFlagChangeHistoryEventTest() {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    CommonUtil.publishProductFlagChangeHistoryEvent(product, true, false, true, Constants.ACTION_KEY, auditTrailDtoList);
    Assertions.assertEquals(1, auditTrailDtoList.size());
    Assertions.assertEquals(product.getProductSku(), auditTrailDtoList.getFirst().getProductSku());
    Assertions.assertEquals(Constants.ACTION_KEY, auditTrailDtoList.getFirst().getActionKey());

    auditTrailDtoList = new ArrayList<>();
    CommonUtil.publishProductFlagChangeHistoryEvent(product, false, false, true, Constants.ACTION_KEY, auditTrailDtoList);
    Assertions.assertTrue(auditTrailDtoList.isEmpty());
  }

  @Test
  void testSetBrandAndCategoryCodeAndPreOrderForOdooTest() {
    Product product = product1;
    product.setBrand("Nike");
    product.setCategoryCode("Shoes");
    ProductChange productChange = new ProductChange();
    CommonUtil.setBrandAndCategoryCodeAndPreOrderForOdoo(product, productChange);
    assertEquals("Nike", productChange.getBrand());
    assertEquals("Shoes", productChange.getCategoryCode());
  }

  @Test
  void convertVideoDTOToVideoTest() {
    VideoDTO videoDTO = new VideoDTO();
    videoDTO.setVideoId("12345");
    videoDTO.setSourceUrl("http://example.com");
    Video video = CommonUtil.convertVideoDTOToVideo(videoDTO);
    assertEquals("12345", video.getVideoId());
  }

  @Test
  void convertVideoDTOToVideoTest_null() {
    Video video = CommonUtil.convertVideoDTOToVideo(null);
    assertNull(video);
  }

  @Test
  public void isItemAddedToPromoTest() {
    Assertions.assertFalse(CommonUtil.isItemAddedToPromo(itemPickupPoint, false, false));
    Assertions.assertFalse(CommonUtil.isItemAddedToPromo(itemPickupPoint, true, false));
    itemPickupPoint.setPromoBundling(true);
    Assertions.assertTrue(CommonUtil.isItemAddedToPromo(itemPickupPoint, true, false));
    itemPickupPoint.setPromoBundling(false);
    itemPickupPoint.setMerchantPromoDiscount(true);
    Assertions.assertTrue(CommonUtil.isItemAddedToPromo(itemPickupPoint, true, false));
    itemPickupPoint.setMerchantPromoDiscount(false);
    itemPickupPoint.setPromoBundling(false);
    Assertions.assertFalse(CommonUtil.isItemAddedToPromo(itemPickupPoint, true, false));
    Assertions.assertFalse(CommonUtil.isItemAddedToPromo(itemPickupPoint, false, false));
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(CAMPAIGN_CODE);
    discountPrice.setEndDateTime(DateUtils.addDays(new Date(), 10));
    discountPrice.setStartDateTime(DateUtils.addDays(new Date(), -10));
    itemPickupPoint.getPrice().iterator().next().setListOfDiscountPrices(Collections.singletonList(discountPrice));
    Assertions.assertTrue(CommonUtil.isItemAddedToPromo(itemPickupPoint, true, false));
    Assertions.assertTrue(CommonUtil.isItemAddedToPromo(itemPickupPoint, false, false));
  }

  @Test
  public void isPartOfPwpPromo() {
    Assertions.assertFalse(CommonUtil.isItemAddedToPromo(itemPickupPoint, false, true));
    itemPickupPoint.setActivePromoBundlings(Collections.singleton(Constants.PWP_ADDITIONAL_ACTIVE));
    Assertions.assertTrue(CommonUtil.isItemAddedToPromo(itemPickupPoint, false, true));
  }

  @Test
  public void setPromoLabelsAtL4Test() {
    ItemL4SummaryResponse itemL4SummaryResponse = new ItemL4SummaryResponse();
    CommonUtil.setPromoLabelsAtL4(productSolr, itemPickupPoint, item, itemL4SummaryResponse, false, false);
    itemPickupPoint.setMerchantPromoDiscount(true);
    CommonUtil.setPromoLabelsAtL4(productSolr, itemPickupPoint, item, itemL4SummaryResponse, false, false);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(itemL4SummaryResponse.getPromoLabels()));
  }

  @Test
  public void isMerchantCampaignActiveOrUpcomingTest() {
    Assertions.assertFalse(CommonUtil.isMerchantCampaignActiveOrUpcoming(itemPickupPoint));
    itemPickupPoint.getPrice().iterator().next().setListOfDiscountPrices(new ArrayList<>());
    Assertions.assertFalse(CommonUtil.isMerchantCampaignActiveOrUpcoming(itemPickupPoint));
    DiscountPrice discountPrice = new DiscountPrice();
    itemPickupPoint.getPrice().iterator().next().setListOfDiscountPrices(Collections.singletonList(discountPrice));
    Assertions.assertFalse(CommonUtil.isMerchantCampaignActiveOrUpcoming(itemPickupPoint));
    itemPickupPoint.getPrice().iterator().next().getListOfDiscountPrices().get(0).setCampaignCode(CAMPAIGN_CODE);
    itemPickupPoint.getPrice().iterator().next().getListOfDiscountPrices().get(0)
        .setEndDateTime(DateUtils.addDays(new Date(), -10));
    Assertions.assertFalse(CommonUtil.isMerchantCampaignActiveOrUpcoming(itemPickupPoint));
    itemPickupPoint.getPrice().iterator().next().getListOfDiscountPrices().get(0)
        .setEndDateTime(DateUtils.addDays(new Date(), 10));
    Assertions.assertTrue(CommonUtil.isMerchantCampaignActiveOrUpcoming(itemPickupPoint));
  }

  @Test
  void testGetProductBasicInfoResponse_WithAllFields() {
    Product product2 = product;
    product2.setUrl("https://test-url.com");
    product2.setProductType(ProductType.REGULAR);
    List<Image> imageBasicInfoResponsesList = new ArrayList<>();
    Image imageBasicInfoResponse = new Image();
    imageBasicInfoResponse.setLocationPath("path/to/image1.jpg");
    imageBasicInfoResponse.setMainImages(true);
    imageBasicInfoResponsesList.add(imageBasicInfoResponse);
    basicInfoProductResponse.setCommonImageList(imageBasicInfoResponsesList);
    product2.setCategoryCode("CAT001");
    product2.setOff2OnChannelActive(true);
    ProductBasicInfoResponse response = CommonUtil.getProductBasicInfoResponse(product2, basicInfoProductResponse);

    assertNotNull(response);
    assertEquals("PRODUCT_SKU", response.getProductSku());
    assertEquals(1, response.getInstore());
    assertEquals("CAT001", response.getCategoryCode());
    assertEquals("https://test-url.com", response.getVideoUrl());

    assertNotNull(response.getCommonImageList());
    assertEquals(1, response.getCommonImageList().size());

    ImageBasicInfoResponse firstImage = response.getCommonImageList().get(0);
    assertTrue(firstImage.isMainImage());
    assertEquals("path/to/image1.jpg", firstImage.getLocationPath());
  }

  @Test
  void testGetProductBasicInfoResponse_WithVideoFromVideoObject() {
    Video video = new Video();
    video.setFinalUrl("https://final-url.com");
    product.setUrl(null);
    product.setVideo(video);
    ProductBasicInfoResponse response = CommonUtil.getProductBasicInfoResponse(product, basicInfoProductResponse);
    assertNotNull(response);
    assertEquals("https://final-url.com", response.getVideoUrl());
  }

  @Test
  void testGetProductBasicInfoResponse_WithBigProductType() {
    product.setProductType(ProductType.BIG_PRODUCT);
    ProductBasicInfoResponse response = CommonUtil.getProductBasicInfoResponse(product, basicInfoProductResponse);
    assertNotNull(response);
    assertEquals("Dikirim oleh seller", response.getShippingType());
  }

  @Test
  void testGetProductBasicInfoResponse_WithBOPISProductType() {
    product.setProductType(ProductType.BOPIS);
    ProductBasicInfoResponse response = CommonUtil.getProductBasicInfoResponse(product, basicInfoProductResponse);
    assertNotNull(response);
    assertEquals("Produk non-fisik", response.getShippingType());
  }

  @Test
  void testGetProductBasicInfoResponse_WithOff2OnChannelInactive() {
    product.setOff2OnChannelActive(false);
    ProductBasicInfoResponse response = CommonUtil.getProductBasicInfoResponse(product, basicInfoProductResponse);
    assertNotNull(response);
    assertEquals(0, response.getInstore());
  }

  @Test
  void testGetProductBasicInfoResponse_WithNullProductType() {
    product.setProductType(null);
    ProductBasicInfoResponse response = CommonUtil.getProductBasicInfoResponse(product, basicInfoProductResponse);
    assertNotNull(response);
    assertNull(response.getShippingType());
  }

  @Test
  void testGetProductBasicInfoResponse_WithEmptyImageList() {
    basicInfoProductResponse.setCommonImageList(new ArrayList<>());
    ProductBasicInfoResponse response = CommonUtil.getProductBasicInfoResponse(product, basicInfoProductResponse);
    assertNotNull(response);
    assertNotNull(response.getCommonImageList());
    assertTrue(response.getCommonImageList().isEmpty());
  }

  @Test
  public void testIsCncActivatedFalse_NoCncChannel() {
    ItemPickupPoint ipp = new ItemPickupPoint();
    ItemViewConfig config = new ItemViewConfig();
    config.setChannel(Constants.DEFAULT_CHANNEL);
    config.setBuyable(true);
    config.setDiscoverable(true);
    ipp.setItemViewConfig(Set.of(config));
    assertTrue(CommonUtil.isCncActivatedFalse(ipp));
  }

  @Test
  public void testIsCncActivatedFalse_CncChannelNotBuyableNotDiscoverable() {
    ItemPickupPoint ipp = new ItemPickupPoint();
    ItemViewConfig config = new ItemViewConfig();
    config.setChannel(Constants.CNC);
    config.setBuyable(false);
    config.setDiscoverable(false);
    ipp.setItemViewConfig(Set.of(config));
    assertTrue(CommonUtil.isCncActivatedFalse(ipp));
  }

  @Test
  public void testIsCncActivatedFalse_CncChannelNotBuyableAndDiscoverableTrue() {
    ItemPickupPoint ipp = new ItemPickupPoint();
    ItemViewConfig config = new ItemViewConfig();
    config.setChannel(Constants.CNC);
    config.setBuyable(false);
    config.setDiscoverable(true);
    ipp.setItemViewConfig(Set.of(config));
    assertFalse(CommonUtil.isCncActivatedFalse(ipp));
  }

  @Test
  public void testIsCncActivatedFalse_CncChannelBuyableTrue() {
    ItemPickupPoint ipp = new ItemPickupPoint();
    ItemViewConfig config = new ItemViewConfig();
    config.setChannel(Constants.CNC);
    config.setBuyable(true);
    config.setDiscoverable(false);
    ipp.setItemViewConfig(Set.of(config));
    assertFalse(CommonUtil.isCncActivatedFalse(ipp));
  }

  @Test
  public void setRejectedChangeTypeForItemDataChangeEvent_WhenProductRejected_ShouldAddRejectedChangeType() {
    ItemDataChange itemDataChange = new ItemDataChange();
    itemDataChange.setItemChangeEventTypes(new ArrayList<>(Arrays.asList(
      ItemChangeEventType.ITEM_DATA_CHANGE,
      ItemChangeEventType.ITEM_PRICE_CHANGE
    )));
    CommonUtil.setRejectedChangeTypeForItemDataChangeEvent(itemDataChange, true);
    assertEquals(2,
      itemDataChange.getItemChangeEventTypes().size());
    assertEquals(1, itemDataChange.getItemChangeEventTypesV2().size());
  }

  @Test
  public void setRejectedChangeTypeForItemDataChangeEvent_WhenProductNotRejected_ShouldNotAddRejectedChangeType() {
    ItemDataChange itemDataChange = new ItemDataChange();
    itemDataChange.setItemChangeEventTypes(new ArrayList<>(Arrays.asList(
      ItemChangeEventType.ITEM_DATA_CHANGE,
      ItemChangeEventType.ITEM_PRICE_CHANGE
    )));
    CommonUtil.setRejectedChangeTypeForItemDataChangeEvent(itemDataChange, false);
    assertEquals(2,
      itemDataChange.getItemChangeEventTypes().size());
  }

  @Test
  public void setRejectedChangeTypeForItemDataChangeEvent_WithNullChangeTypes_ShouldHandleGracefully() {
    ItemDataChange itemDataChange = new ItemDataChange();
    itemDataChange.setItemChangeEventTypesV2(null);
    CommonUtil.setRejectedChangeTypeForItemDataChangeEvent(itemDataChange, true);
    assertNotNull(itemDataChange.getItemChangeEventTypesV2());
  }

  @Test
  public void setRejectedChangeTypeForItemDataChangeEvent_WithEmptyChangeTypes_ShouldHandleGracefully() {
    // Arrange
    ItemDataChange itemDataChange = new ItemDataChange();
    itemDataChange.setItemChangeEventTypes(new ArrayList<>());

    // Act
    CommonUtil.setRejectedChangeTypeForItemDataChangeEvent(itemDataChange, true);

    // Assert
    assertNotNull(itemDataChange.getItemChangeEventTypesV2());
    assertEquals(1, itemDataChange.getItemChangeEventTypesV2().size());
  }

  @Test
  public void setRejectedChangeTypeForL5DataChangeEventModelList_WhenProductRejected_ShouldAddRejectedChangeType() {
    List<ItemPickupPointDataChangeEventModel> eventModels = new ArrayList<>();
    ItemPickupPointDataChangeEventModel model1 = new ItemPickupPointDataChangeEventModel();
    model1.setItemPickupPointChangeEventTypes(new ArrayList<>(Arrays.asList(
      ItemPickupPointChangeEventType.PRICE_CHANGE,
      ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE
    )));
    ItemPickupPointDataChangeEventModel model2 = new ItemPickupPointDataChangeEventModel();
    model2.setItemPickupPointChangeEventTypes(new ArrayList<>(Arrays.asList(
      ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE
    )));
    eventModels.add(model1);
    eventModels.add(model2);

    // Act
    CommonUtil.setRejectedChangeTypeForL5DataChangeEventModelList(eventModels, true);
    assertEquals(2,
      eventModels.get(0).getItemPickupPointChangeEventTypes().size());
    assertEquals(1, eventModels.get(0).getItemPickupPointChangeEventTypesV2().size());
  }

  @Test
  public void setRejectedChangeTypeForL5DataChangeEventModelList_WhenProductNotRejected_ShouldNotAddRejectedChangeType() {
    List<ItemPickupPointDataChangeEventModel> eventModels = new ArrayList<>();
    ItemPickupPointDataChangeEventModel model = new ItemPickupPointDataChangeEventModel();
    model.setItemPickupPointChangeEventTypes(new ArrayList<>(Arrays.asList(
      ItemPickupPointChangeEventType.PRICE_CHANGE,
      ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE
    )));
    eventModels.add(model);

    // Act
    CommonUtil.setRejectedChangeTypeForL5DataChangeEventModelList(eventModels, false);

    assertEquals(2, eventModels.get(0).getItemPickupPointChangeEventTypes().size());
  }


  @Test
  public void setRejectedChangeTypeForL5DataChangeEventModelList_WithNullChangeTypes() {
    List<ItemPickupPointDataChangeEventModel> eventModels = new ArrayList<>();
    ItemPickupPointDataChangeEventModel model = new ItemPickupPointDataChangeEventModel();
    model.setItemPickupPointChangeEventTypesV2(null);
    eventModels.add(model);
    CommonUtil.setRejectedChangeTypeForL5DataChangeEventModelList(eventModels, true);
    assertNotNull(
      eventModels.get(0).getItemPickupPointChangeEventTypesV2());
    assertEquals(1,
      eventModels.get(0).getItemPickupPointChangeEventTypesV2().size());
  }


  @Test
  public void setRejectedChangeTypeForL5DataChangeEventModelList_WithEmptyChangeTypes_ShouldHandleGracefully() {
    // Arrange
    List<ItemPickupPointDataChangeEventModel> eventModels = new ArrayList<>();
    ItemPickupPointDataChangeEventModel model = new ItemPickupPointDataChangeEventModel();
    model.setItemPickupPointChangeEventTypes(new ArrayList<>());
    eventModels.add(model);

    // Act
    CommonUtil.setRejectedChangeTypeForL5DataChangeEventModelList(eventModels, true);

    // Assert
    assertNotNull(eventModels.get(0).getItemPickupPointChangeEventTypesV2());
    assertEquals(1, eventModels.get(0).getItemPickupPointChangeEventTypesV2().size());
  }

  @Test
  public void setRejectedChangeTypeForL5DataChangeEventModelList_WithEmptyList_ShouldHandleGracefully() {
    // Arrange
    List<ItemPickupPointDataChangeEventModel> eventModels = new ArrayList<>();
    CommonUtil.setRejectedChangeTypeForL5DataChangeEventModelList(eventModels, true);
    Assertions.assertNotNull(eventModels);
  }

  @Test
  public void setSharedProduct_WhenSingleProduct_ShouldSetSharedProductToFalse() {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    List<Product> products = List.of(product);
    CommonUtil.setSharedProduct(products);
    Assertions.assertFalse(product.isSharedProduct());
  }

  @Test
  public void setSharedProduct_WhenMultipleProductsWithDifferentCodes_ShouldSetSharedProductToFalse() {
    // Given
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE);
    product1.setProductSku(PRODUCT_SKU);

    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE.concat(PRODUCT_CODE));
    product2.setProductSku(PRODUCT_SKU_1);

    List<Product> products = Arrays.asList(product1, product2);
    CommonUtil.setSharedProduct(products);
    Assertions.assertFalse(product1.isSharedProduct());
    Assertions.assertFalse(product2.isSharedProduct());
  }

  @Test
  public void setSharedProduct_WhenTwoProductsWithSameCode_ShouldSetSharedProductToTrue() {
    // Given
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE);
    product1.setProductSku(PRODUCT_SKU);

    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE);
    product2.setProductSku(PRODUCT_SKU_1);

    List<Product> products = Arrays.asList(product1, product2);
    CommonUtil.setSharedProduct(products);
    Assertions.assertTrue(product1.isSharedProduct());
    Assertions.assertTrue(product2.isSharedProduct());
  }

  @Test
  public void setSharedProduct_WhenThreeProductsWithSameCode_ShouldSetSharedProductToTrue() {
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE);
    product1.setProductSku(PRODUCT_SKU);

    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE);
    product2.setProductSku(PRODUCT_SKU);

    Product product3 = new Product();
    product3.setProductCode(PRODUCT_CODE);
    product3.setProductSku(PRODUCT_SKU_1);

    List<Product> products = Arrays.asList(product1, product2, product3);
    CommonUtil.setSharedProduct(products);
    Assertions.assertTrue(product1.isSharedProduct());
    Assertions.assertTrue(product2.isSharedProduct());
    Assertions.assertTrue(product3.isSharedProduct());
  }

  @Test
  void testIsItemSummaryCacheEligible() {
    String validClient = "content-api";
    List<String> allowedClients = Arrays.asList(validClient, "other-client");
    List<ItemPickupPointRequest> requests = new ArrayList<>();
    ItemPickupPointRequest req = new ItemPickupPointRequest();
    req.setItemSku("SKU1");
    req.setPickupPointCode("P1");
    requests.add(req);
    assertFalse(
      CommonUtil.isItemSummaryCacheEligible(requests, "unknown-client", allowedClients, 10));
    List<ItemPickupPointRequest> tooManyRequests = new ArrayList<>();
    for (int i = 0; i < 15; i++) {
      ItemPickupPointRequest r = new ItemPickupPointRequest();
      r.setItemSku("SKU" + i);
      r.setPickupPointCode("P" + i);
      tooManyRequests.add(r);
    }
    assertFalse(
      CommonUtil.isItemSummaryCacheEligible(tooManyRequests, validClient, allowedClients,
        10));
    assertTrue(
      CommonUtil.isItemSummaryCacheEligible(requests, validClient, allowedClients, 10));
    assertTrue(CommonUtil.isItemSummaryCacheEligible(Collections.emptyList(), validClient,
      allowedClients, 10));
  }
  @Test
  void testToItemViewConfigDTOs_withValidConfigs() {
    com.gdn.x.product.model.entity.ItemViewConfig entity = new com.gdn.x.product.model.entity.ItemViewConfig();
    entity.setChannel(DEFAULT_CHANNEL);
    entity.setBuyable(true);
    entity.setDiscoverable(false);
    Set<com.gdn.x.product.model.entity.ItemViewConfig> entities = new HashSet<>();
    entities.add(entity);
    Set<ItemViewConfigDTO> dtos = CommonUtil.toItemViewConfigDTOs(entities);
    assertNotNull(dtos, "DTO set should not be null");
    assertEquals(1, dtos.size(), "DTO set should contain one element");
    ItemViewConfigDTO dto = dtos.iterator().next();
    assertEquals(DEFAULT_CHANNEL, dto.getChannel());
    assertTrue(dto.isBuyable());
    assertFalse(dto.isDiscoverable());
  }

  @Test
  void testToItemViewConfigDTOs_withValidConfigsScheds() {
    com.gdn.x.product.model.entity.ItemViewConfig entity = new com.gdn.x.product.model.entity.ItemViewConfig();
    entity.setChannel(DEFAULT_CHANNEL);
    entity.setBuyable(true);
    entity.setDiscoverable(false);
    entity.setItemDiscoverableSchedules(new ItemDiscoverableSchedule());
    entity.setItemBuyableSchedules(new ItemBuyableSchedule());
    Set<com.gdn.x.product.model.entity.ItemViewConfig> entities = new HashSet<>();
    entities.add(entity);
    Set<ItemViewConfigDTO> dtos = CommonUtil.toItemViewConfigDTOs(entities);
    assertNotNull(dtos, "DTO set should not be null");
    assertEquals(1, dtos.size(), "DTO set should contain one element");
    ItemViewConfigDTO dto = dtos.iterator().next();
    assertEquals(DEFAULT_CHANNEL, dto.getChannel());
    assertTrue(dto.isBuyable());
    assertFalse(dto.isDiscoverable());
    assertNotNull(dto.getItemBuyableSchedules());
    assertNotNull(dto.getItemDiscoverableSchedules());
  }

  @Test
  void testToItemViewConfigDTOs_withEmptyInput() {
    Set<com.gdn.x.product.model.entity.ItemViewConfig> emptySet = Collections.emptySet();
    Set<ItemViewConfigDTO> dtos = CommonUtil.toItemViewConfigDTOs(emptySet);
    assertNotNull(dtos, "DTO set should not be null");
    assertTrue(dtos.isEmpty(), "DTO set should be empty when input is empty");
  }

  @Test
  void testToItemViewConfigDTOs_withMultipleEntities() {
    com.gdn.x.product.model.entity.ItemViewConfig entity1 = new com.gdn.x.product.model.entity.ItemViewConfig();
    entity1.setChannel(DEFAULT_CHANNEL);
    entity1.setBuyable(true);
    entity1.setDiscoverable(true);

    com.gdn.x.product.model.entity.ItemViewConfig entity2 = new com.gdn.x.product.model.entity.ItemViewConfig();
    entity2.setChannel(B2B_SELLER_CHANNEL);
    entity2.setBuyable(false);
    entity2.setDiscoverable(true);

    Set<com.gdn.x.product.model.entity.ItemViewConfig> entities = new HashSet<>();
    entities.add(entity1);
    entities.add(entity2);
    Set<ItemViewConfigDTO> dtos = CommonUtil.toItemViewConfigDTOs(entities);
    assertTrue(dtos.stream().anyMatch(d -> DEFAULT_CHANNEL.equals(d.getChannel())));
    assertTrue(dtos.stream().anyMatch(d -> B2B_SELLER_CHANNEL.equals(d.getChannel())));
  }

  @Test
  void testToItemViewConfigDTOs_withNullFields() {
    com.gdn.x.product.model.entity.ItemViewConfig entity = new com.gdn.x.product.model.entity.ItemViewConfig();
    Set<com.gdn.x.product.model.entity.ItemViewConfig> entities = new HashSet<>();
    entities.add(entity);
    Set<ItemViewConfigDTO> dtos = CommonUtil.toItemViewConfigDTOs(entities);
    assertEquals(1, dtos.size(), "DTO set should still contain one element");
    ItemViewConfigDTO dto = dtos.iterator().next();
    assertNull(dto.getChannel(), "Channel should be null since entity channel was null");
  }

  @Test
  void testGenerateItemSummaryCacheKey() {
    String storeId = "STORE1";
    ItemPickupPointRequest req1 = new ItemPickupPointRequest();
    req1.setItemSku("B");
    req1.setPickupPointCode("2");
    ItemPickupPointRequest req2 = new ItemPickupPointRequest();
    req2.setItemSku("A");
    req2.setPickupPointCode("1");
    List<ItemPickupPointRequest> unordered = Arrays.asList(req1, req2);
    String key1 = CommonUtil.generateItemSummaryCacheKey(storeId, unordered, false);
    String key2 = CommonUtil.generateItemSummaryCacheKey(storeId, Arrays.asList(req2, req1), false);
    assertEquals(key1, key2);
    List<ItemPickupPointRequest> single = Collections.singletonList(req1);
    String keySingle = CommonUtil.generateItemSummaryCacheKey(storeId, single, false);
    assertTrue(keySingle.startsWith(storeId + "-"));
    String emptyKey = CommonUtil.generateItemSummaryCacheKey(storeId, Collections.emptyList(), false);
    assertEquals(storeId + "-" + false + "-0", emptyKey);
  }

  @Test
  void isSharedProductFalseTest() {
    product1.setMerchantCode(MERCHANT_CODE);
    Assertions.assertFalse(
      CommonUtil.isSharedProduct(Collections.singletonList(product1), MERCHANT_CODE));
  }

  @Test
  void isSharedProductMerchantCodeFalseTest() {
    product1.setMerchantCode("merchant");
    Assertions.assertTrue(
      CommonUtil.isSharedProduct(Collections.singletonList(product1), MERCHANT_CODE));
  }

  @Test
  void isSharedProductMultipleTrueTest() {
    Assertions.assertTrue(
      CommonUtil.isSharedProduct(List.of(product1,product), MERCHANT_CODE));
  }

  @Test
  public void computeDistributionFlag_Pure_Distribution_Test() {
    ItemVo itemVo = new ItemVo();
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    itemPickupPointVo.setDistribution(true);
    itemVo.setItemPickupPointVoList(List.of(itemPickupPointVo));
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    CommonUtil.computeDistributionFlag(List.of(itemVo), Set.of(MERCHANT_CODE), MERCHANT_CODE,
        product);
    assertEquals(product.getDistributionStatus(), DistributionStatus.PURE_DISTRIBUTION);
  }

  @Test
  public void computeDistributionFlag_Distribution_Test() {
    ItemVo itemVo = new ItemVo();
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    ItemPickupPointVo itemPickupPointVo2 = new ItemPickupPointVo();
    itemPickupPointVo.setDistribution(true);
    itemVo.setItemPickupPointVoList(List.of(itemPickupPointVo, itemPickupPointVo2));
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    CommonUtil.computeDistributionFlag(List.of(itemVo), Set.of(MERCHANT_CODE), MERCHANT_CODE,
        product);
    assertEquals(product.getDistributionStatus(), DistributionStatus.DISTRIBUTION);
  }

  @Test
  public void computeDistributionFlag_Non_Distribution_Test() {
    ItemVo itemVo = new ItemVo();
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    ItemPickupPointVo itemPickupPointVo2 = new ItemPickupPointVo();
    itemPickupPointVo.setDistribution(false);
    itemVo.setItemPickupPointVoList(List.of(itemPickupPointVo, itemPickupPointVo2));
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    CommonUtil.computeDistributionFlag(List.of(itemVo), Set.of(MERCHANT_CODE), MERCHANT_CODE,
        product);
    assertEquals(product.getDistributionStatus(), DistributionStatus.NON_DISTRIBUTION);
  }

  @Test
  public void computeDistributionFlag_Random_Seller_Test() {
    ItemVo itemVo = new ItemVo();
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    itemPickupPointVo.setDistribution(true);
    itemVo.setItemPickupPointVoList(List.of(itemPickupPointVo));
    CommonUtil.computeDistributionFlag(List.of(itemVo), Set.of(MERCHANT_CODE), "MERCHANT_CODE",
        product);
    assertEquals(product.getDistributionStatus(), DistributionStatus.NON_DISTRIBUTION);
  }

  @Test
  public void distributionSellerAndAddOrDeletePerformed_TrueTest() {
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo =
        new ItemPickupPointUpdateRequestVo();
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    addDeleteVariantRequestVo.setAddVariantsList(List.of(addVariantRequestVo));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    Assertions.assertTrue(
        CommonUtil.distributionSellerAndAddOrDeletePerformed(Set.of(MERCHANT_CODE),
            itemPickupPointUpdateRequestVo, MERCHANT_CODE));
  }

  @Test
  public void distributionSellerAndAddOrDeletePerformed_True_Delete_Variant_Test() {
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo =
        new ItemPickupPointUpdateRequestVo();
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    addDeleteVariantRequestVo.setDeleteVariantsList(List.of(ITEM_SKU));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    Assertions.assertTrue(
        CommonUtil.distributionSellerAndAddOrDeletePerformed(Set.of(MERCHANT_CODE),
            itemPickupPointUpdateRequestVo, MERCHANT_CODE));
  }

  @Test
  public void non_distributionSellerAndAddOrDeletePerformed_False_Test() {
    Assertions.assertFalse(
        CommonUtil.distributionSellerAndAddOrDeletePerformed(Set.of(MERCHANT_CODE),
            new ItemPickupPointUpdateRequestVo(), "MERCHANT_CODE"));
  }

  @Test
  public void non_distributionSellerAndAddOrDeletePerformed_Empty_Request_False_Test() {
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo =
        new ItemPickupPointUpdateRequestVo();
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    Assertions.assertFalse(
        CommonUtil.distributionSellerAndAddOrDeletePerformed(Set.of(MERCHANT_CODE),
            itemPickupPointUpdateRequestVo, MERCHANT_CODE));
  }

  @Test
  public void distributionSellerAndAddOrDeletePerformed_True_Add_PP_Test() {
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo =
        new ItemPickupPointUpdateRequestVo();
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo =
        new ItemPickupPointListingUpdateRequestVo();
    itemPickupPointListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(
        List.of(itemPickupPointListingUpdateRequestVo));
    Assertions.assertTrue(
        CommonUtil.distributionSellerAndAddOrDeletePerformed(Set.of(MERCHANT_CODE),
            itemPickupPointUpdateRequestVo, MERCHANT_CODE));
  }

  @Test
  public void distributionSellerAndAddOrDeletePerformed_True_Del_PP_Test() {
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo =
        new ItemPickupPointUpdateRequestVo();
    ItemPickupPointDeleteRequestVo deleteRequestVo =
        new ItemPickupPointDeleteRequestVo(ITEM_SKU, PICKUP_POINT_CODE);
    itemPickupPointUpdateRequestVo.setDeletePickupPointRequests(List.of(deleteRequestVo));
    Assertions.assertTrue(
        CommonUtil.distributionSellerAndAddOrDeletePerformed(Set.of(MERCHANT_CODE),
            itemPickupPointUpdateRequestVo, MERCHANT_CODE));
  }

  @Test
  public void distributionSellerAndAddOrDeletePerformed_False_Test() {
    Assertions.assertFalse(
        CommonUtil.distributionSellerAndAddOrDeletePerformed(Set.of(MERCHANT_CODE),
            new ItemPickupPointUpdateRequestVo(), MERCHANT_CODE));
  }

  @Test
  public void setDistributionStatusBasedOnL5FlagsTest() {
    CommonUtil.setDistributionStatus(product, false, new ArrayList<>(), new HashSet<>());
    product.setMerchantCode(MERCHANT_CODE);
    CommonUtil.setDistributionStatus(product, false, new ArrayList<>(), Collections.singleton(MERCHANT_CODE));
    CommonUtil.setDistributionStatus(product, true, new ArrayList<>(), Collections.singleton(MERCHANT_CODE));
    Assertions.assertNotNull(catalogResponse);
  }

  @Test
  void testNoChangesInSku_shouldReturnEmptyAuditList() {
    Product product = getSampleProduct();
    Map<String, String> existingMap = Map.of("ITEM1", "SKU1");
    Map<String, String> updatedMap = Map.of("ITEM1", "SKU1");
    Map<String, String> itemNames = Map.of("ITEM1", "Item One");
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    Item item = new Item();
    item.setItemCode("ITEM1");
    item.setItemSku("SKU1");
    item.setGeneratedItemName("Item One");
    List<AuditTrailDto> result =
        CommonUtil.getHistoryListForOmniChannelSkuUpdate(product, existingMap, updatedMap,
            List.of(item), auditTrailDtoList);

    assertEquals(0, result.size());
  }

  @Test
  void testSingleSkuUpdate_shouldCreateOneAuditEntry() {
    Product product = getSampleProduct();
    Map<String, String> existingMap = Map.of("ITEM1", "SKU1");
    Map<String, String> updatedMap = Map.of("ITEM1", "SKU2");
    Map<String, String> itemNames = Map.of("ITEM1", "Item One");
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    Item item = new Item();
    item.setItemCode("ITEM1");
    item.setItemSku("SKU1");
    item.setGeneratedItemName("Item One");

    List<AuditTrailDto> result =
        CommonUtil.getHistoryListForOmniChannelSkuUpdate(product, existingMap, updatedMap,
            List.of(item), auditTrailDtoList);

    assertEquals(1, result.size());
    AuditTrailDto audit = result.get(0);
    assertEquals("SKU1", audit.getOldValue());
    assertEquals("SKU2", audit.getNewValue());
    assertEquals("Item One", audit.getName());
    assertEquals(product.getMerchantCode(), audit.getBusinessPartnerCode());
  }

  @Test
  void testMultipleSkuUpdates_shouldCreateMultipleAuditEntries() {
    Product product = getSampleProduct();
    Map<String, String> existingMap = Map.of("ITEM1", "SKU1", "ITEM2", "SKU2");
    Map<String, String> updatedMap = Map.of("ITEM1", "SKU1_UPDATED", "ITEM2", "SKU2_UPDATED");
    Map<String, String> itemNames = Map.of("ITEM1", "Item One", "ITEM2", "Item Two");
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    Item item = new Item();
    item.setItemCode("ITEM1");
    item.setItemSku("SKU1");
    item.setGeneratedItemName("Item One");

    List<AuditTrailDto> result =
        CommonUtil.getHistoryListForOmniChannelSkuUpdate(product, existingMap, updatedMap,
            List.of(item), auditTrailDtoList);

    assertEquals(2, result.size());
  }

  @Test
  void testOnlyNewEntriesAdded_shouldCreateAuditEntry() {
    Product product = getSampleProduct();
    Map<String, String> existingMap = Map.of();
    Map<String, String> updatedMap = Map.of("ITEM1", "SKU_NEW");
    Map<String, String> itemNames = Map.of("ITEM1", "Item One");
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    Item item = new Item();
    item.setItemCode("ITEM1");
    item.setItemSku("SKU1");
    item.setGeneratedItemName("Item One");

    List<AuditTrailDto> result =
        CommonUtil.getHistoryListForOmniChannelSkuUpdate(product, existingMap, updatedMap,
            List.of(item), auditTrailDtoList);

    assertEquals(1, result.size());
  }

  @Test
  void testEmptyMaps_shouldReturnEmptyAuditList() {
    Product product = getSampleProduct();
    Map<String, String> existingMap = new HashMap<>();
    Map<String, String> updatedMap = new HashMap<>();
    Map<String, String> itemNames = new HashMap<>();
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    Item item = new Item();
    item.setItemCode("ITEM1");
    item.setItemSku("SKU1");
    item.setGeneratedItemName("Item One");
    List<AuditTrailDto> result =
        CommonUtil.getHistoryListForOmniChannelSkuUpdate(product, existingMap, updatedMap,
            List.of(item), auditTrailDtoList);

    assertEquals(0, result.size());
  }

  @Test
  void testPrePopulatedAuditList_shouldAppendNewEntries() {
    Product product = getSampleProduct();
    Map<String, String> existingMap = Map.of("ITEM1", "SKU1");
    Map<String, String> updatedMap = Map.of("ITEM1", "SKU2");
    Map<String, String> itemNames = Map.of("ITEM1", "Item One");
    Item item = new Item();
    item.setItemCode("ITEM1");
    item.setItemSku("SKU1");
    item.setGeneratedItemName("Item One");

    Item item2 = new Item();
    item2.setItemCode("ITEM1");
    item2.setItemSku("SKU1");
    item2.setGeneratedItemName("Item One");

    AuditTrailDto existingAudit = new AuditTrailDto();
    existingAudit.setName("Existing Entry");
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    auditTrailDtoList.add(existingAudit);

    List<AuditTrailDto> result =
        CommonUtil.getHistoryListForOmniChannelSkuUpdate(product, existingMap, updatedMap,
            List.of(item, item2), auditTrailDtoList);

    assertEquals(2, result.size());
    assertEquals("Existing Entry", result.get(0).getName());
    assertEquals("Item One", result.get(1).getName());
  }

  private Product getSampleProduct() {
    Product product = new Product();
    product.setMerchantCode("MERCHANT123");
    product.setOnline(true);
    product.setProductSku("PROD123");
    return product;
  }

  @Test
  public void testGetAttributeValueWithNullProductAttributeResponses() {
    ProductAndAttributeDetailResponse response = new ProductAndAttributeDetailResponse();
    response.setProductAttributeResponses(null);
    List<String> attributeCodes = Arrays.asList(ATTRIBUTE_CODE);
    
    String result = CommonUtil.getAttributeValue(response, attributeCodes);
    
    assertNull(result);
  }

  @Test
  public void testGetAttributeValueWithEmptyProductAttributeResponses() {
    ProductAndAttributeDetailResponse response = new ProductAndAttributeDetailResponse();
    response.setProductAttributeResponses(new ArrayList<>());
    List<String> attributeCodes = Arrays.asList(ATTRIBUTE_CODE);
    
    String result = CommonUtil.getAttributeValue(response, attributeCodes);
    
    assertNull(result);
  }

  @Test
  public void testGetAttributeValueWithNoMatchingAttributeCode() {
    ProductAndAttributeDetailResponse response = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeResponse.setAttribute(attribute);
    response.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    List<String> attributeCodes = Arrays.asList(ATTRIBUTE_CODE_3);
    
    String result = CommonUtil.getAttributeValue(response, attributeCodes);
    
    assertNull(result);
  }

  @Test
  public void testGetAttributeValueWithNullProductAttributeValues() {
    ProductAndAttributeDetailResponse response = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeResponse.setAttribute(attribute);
    productAttributeResponse.setProductAttributeValues(null);
    response.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    List<String> attributeCodes = Arrays.asList(ATTRIBUTE_CODE);
    
    String result = CommonUtil.getAttributeValue(response, attributeCodes);
    
    assertNull(result);
  }

  @Test
  public void testGetAttributeValueWithEmptyProductAttributeValues() {
    ProductAndAttributeDetailResponse response = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeResponse.setAttribute(attribute);
    productAttributeResponse.setProductAttributeValues(new ArrayList<>());
    response.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    List<String> attributeCodes = Arrays.asList(ATTRIBUTE_CODE);
    
    String result = CommonUtil.getAttributeValue(response, attributeCodes);
    
    assertNull(result);
  }

  @Test
  public void testGetAttributeValueWithDescriptiveAttributeType() {
    ProductAndAttributeDetailResponse response = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    productAttributeResponse.setAttribute(attribute);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_VALUE);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    response.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    List<String> attributeCodes = Arrays.asList(ATTRIBUTE_CODE);
    
    String result = CommonUtil.getAttributeValue(response, attributeCodes);
    
    assertEquals(DESCRIPTIVE_VALUE, result);
  }

  @Test
  public void testGetAttributeValueWithPredefinedAttributeTypeWithValue() {
    ProductAndAttributeDetailResponse response = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType("PREDEFINED_ATTRIBUTE");
    productAttributeResponse.setAttribute(attribute);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedValue = new PredefinedAllowedAttributeValueResponse();
    predefinedValue.setValue(PREDEFINED_VALUE);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedValue);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    response.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    List<String> attributeCodes = Arrays.asList(ATTRIBUTE_CODE);
    
    String result = CommonUtil.getAttributeValue(response, attributeCodes);
    
    assertEquals(PREDEFINED_VALUE, result);
  }

  @Test
  public void testGetAttributeValueWithPredefinedAttributeTypeWithNullValue() {
    ProductAndAttributeDetailResponse response = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType("PREDEFINED_ATTRIBUTE");
    productAttributeResponse.setAttribute(attribute);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(null);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    response.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    List<String> attributeCodes = Arrays.asList(ATTRIBUTE_CODE);
    
    String result = CommonUtil.getAttributeValue(response, attributeCodes);
    
    assertNull(result);
  }

  @Test
  public void testGetAttributeValueWithOtherAttributeType() {
    ProductAndAttributeDetailResponse response = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(OTHER_ATTRIBUTE_TYPE);
    productAttributeResponse.setAttribute(attribute);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_VALUE);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    response.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    List<String> attributeCodes = Arrays.asList(ATTRIBUTE_CODE);
    
    String result = CommonUtil.getAttributeValue(response, attributeCodes);
    
    assertNull(result);
  }

  @Test
  public void testGetAttributeValueWithNullAttributeType() {
    ProductAndAttributeDetailResponse response = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(null);
    productAttributeResponse.setAttribute(attribute);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_VALUE);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    response.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    List<String> attributeCodes = Arrays.asList(ATTRIBUTE_CODE);
    
    String result = CommonUtil.getAttributeValue(response, attributeCodes);
    response.getProductAttributeResponses().get(0).setAttribute(null);
    CommonUtil.getAttributeValue(response, attributeCodes);
    assertNull(result);
  }

  @Test
  public void testOverridePreOrderDateToJKT_WhenAllConditionsMet_ShouldSubtract7Hours() {
    Date originalDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(originalDate);
    calendar.add(Calendar.HOUR_OF_DAY, 7);
    Date dateWith7Hours = calendar.getTime();

    PreOrderDTO preOrder = PreOrderDTO.builder()
        .preOrderDate(dateWith7Hours)
        .convertToJKT(true)
        .build();
    ProductRequest productRequest = new ProductRequest();
    productRequest.setPreOrder(preOrder);
    ProductRequest result = CommonUtil.overridePreOrderDateToJKT(productRequest);
    assertNotNull(result);
    assertNotNull(result.getPreOrder());
    assertNotNull(result.getPreOrder().getPreOrderDate());
    Calendar resultCalendar = Calendar.getInstance();
    resultCalendar.setTime(result.getPreOrder().getPreOrderDate());
    Calendar expectedCalendar = Calendar.getInstance();
    expectedCalendar.setTime(originalDate);
    long timeDifference = Math.abs(resultCalendar.getTimeInMillis() - expectedCalendar.getTimeInMillis());
    assertTrue(timeDifference < 1000, "Date should be approximately 7 hours earlier");
  }

  @Test
  public void testOverridePreOrderDateToJKT_WhenPreOrderIsNull_ShouldReturnUnchanged() {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setPreOrder(null);
    ProductRequest result = CommonUtil.overridePreOrderDateToJKT(productRequest);
    assertNotNull(result);
    assertNull(result.getPreOrder());
  }

  @Test
  public void testOverridePreOrderDateToJKT_WhenPreOrderDateIsNull_ShouldReturnUnchanged() {
    PreOrderDTO preOrder = PreOrderDTO.builder()
        .preOrderDate(null)
        .convertToJKT(true)
        .build();
    ProductRequest productRequest = new ProductRequest();
    productRequest.setPreOrder(preOrder);
    ProductRequest result = CommonUtil.overridePreOrderDateToJKT(productRequest);
    assertNotNull(result);
    assertNotNull(result.getPreOrder());
    assertNull(result.getPreOrder().getPreOrderDate());
  }

  @Test
  public void testOverridePreOrderDateToJKT_WhenConvertToJKTIsFalse_ShouldReturnUnchanged() {
    Date originalDate = new Date();
    PreOrderDTO preOrder = PreOrderDTO.builder()
        .preOrderDate(originalDate)
        .convertToJKT(false)
        .build();
    ProductRequest productRequest = new ProductRequest();
    productRequest.setPreOrder(preOrder);
    ProductRequest result = CommonUtil.overridePreOrderDateToJKT(productRequest);
    assertNotNull(result);
    assertNotNull(result.getPreOrder());
    assertEquals(originalDate, result.getPreOrder().getPreOrderDate());
  }

  @Test
  public void testOverridePreOrderDateToJKT_WhenPreOrderAndDateAreNull_ShouldReturnUnchanged() {
    PreOrderDTO preOrder = PreOrderDTO.builder()
        .preOrderDate(null)
        .convertToJKT(true)
        .build();
    ProductRequest productRequest = new ProductRequest();
    productRequest.setPreOrder(preOrder);
    ProductRequest result = CommonUtil.overridePreOrderDateToJKT(productRequest);
    assertNotNull(result);
    assertNotNull(result.getPreOrder());
    assertNull(result.getPreOrder().getPreOrderDate());
  }

  @Test
  public void testOverridePreOrderDateToJKT_WhenAllNullConditions_ShouldReturnUnchanged() {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setPreOrder(null);
    ProductRequest result = CommonUtil.overridePreOrderDateToJKT(productRequest);
    assertNotNull(result);
    assertNull(result.getPreOrder());
  }

  @Test
  public void testOverridePreOrderDateToJKT_VerifyExact7HoursSubtraction() {
    Calendar calendar = Calendar.getInstance();
    calendar.set(2024, Calendar.JANUARY, 15, 14, 30, 0);
    calendar.set(Calendar.MILLISECOND, 0);
    Date originalDate = calendar.getTime();
    PreOrderDTO preOrder = PreOrderDTO.builder()
        .preOrderDate(originalDate)
        .convertToJKT(true)
        .build();
    ProductRequest productRequest = new ProductRequest();
    productRequest.setPreOrder(preOrder);
    ProductRequest result = CommonUtil.overridePreOrderDateToJKT(productRequest);
    assertNotNull(result);
    assertNotNull(result.getPreOrder());
    assertNotNull(result.getPreOrder().getPreOrderDate());

    Calendar resultCalendar = Calendar.getInstance();
    resultCalendar.setTime(result.getPreOrder().getPreOrderDate());
    Calendar expectedCalendar = Calendar.getInstance();
    expectedCalendar.setTime(originalDate);
    expectedCalendar.add(Calendar.HOUR_OF_DAY, -7);
    assertEquals(expectedCalendar.get(Calendar.YEAR), resultCalendar.get(Calendar.YEAR));
    assertEquals(expectedCalendar.get(Calendar.MONTH), resultCalendar.get(Calendar.MONTH));
    assertEquals(expectedCalendar.get(Calendar.DAY_OF_MONTH), resultCalendar.get(Calendar.DAY_OF_MONTH));
    assertEquals(expectedCalendar.get(Calendar.HOUR_OF_DAY), resultCalendar.get(Calendar.HOUR_OF_DAY));
    assertEquals(expectedCalendar.get(Calendar.MINUTE), resultCalendar.get(Calendar.MINUTE));
  }
}
